//! A CLI tool to generate HTML documentation of DreamMaker codebases.
#![forbid(unsafe_code)]
extern crate dreammaker as dm;
extern crate docstrings;
extern crate pulldown_cmark;
extern crate tera;
extern crate git2;
#[macro_use] extern crate serde_derive;

mod template;

use std::collections::{BTreeMap, BTreeSet};
use std::cell::RefCell;
use std::io::{self, Write};
use std::fs::{self, File};
use std::path::{Path, PathBuf};

use docstrings::{DocBlock, parse_md_docblock};

// ----------------------------------------------------------------------------
// Driver

thread_local! {
    static ALL_TYPE_NAMES: RefCell<BTreeSet<String>> = Default::default();
}

fn main() -> Result<(), Box<std::error::Error>> {
    // TODO: command-line args
    let output_path: &Path = "docs".as_ref();

    // load tera templates
    println!("loading templates");
    let mut tera = template::builtin()?;

    // register tera extensions
    tera.register_filter("md", |input, opts| match input {
        tera::Value::String(s) => Ok(tera::Value::String(render_markdown(&s, opts.contains_key("teaser")))),
        _ => Err("md() input must be string".into()),
    });
    tera.register_filter("linkify_type", |input, _opts| match input {
        tera::Value::String(s) => {
            let mut output = String::new();
            let mut all_progress = String::new();
            let mut progress = String::new();
            for bit in s.split("/").skip_while(|b| b.is_empty()) {
                all_progress.push_str("/");
                all_progress.push_str(bit);
                progress.push_str("/");
                progress.push_str(bit);
                if ALL_TYPE_NAMES.with(|t| t.borrow().contains(&all_progress)) {
                    use std::fmt::Write;
                    let _ = write!(output, r#"/<a href="{}.html">{}</a>"#, &all_progress[1..], &progress[1..]);
                    progress.clear();
                }
            }
            output.push_str(&progress);
            Ok(tera::Value::String(output))
        }
        _ => Err("linkify_type() input must be string".into()),
    });
    tera.register_filter("length", |input, _opts| match input {
        tera::Value::String(s) => Ok(s.len().into()),
        tera::Value::Array(a) => Ok(a.len().into()),
        tera::Value::Object(o) => Ok(o.len().into()),
        _ => Ok(0 .into()),
    });
    tera.register_filter("substring", |input, opts| match input {
        tera::Value::String(s) => {
            let start = opts.get("start").and_then(|v| v.as_u64()).unwrap_or(0) as usize;
            let mut end = opts.get("end").and_then(|v| v.as_u64()).map(|s| s as usize).unwrap_or(s.len());
            if end > s.len() {
                end = s.len();
            }
            Ok(s[start..end].into())
        }
        _ => Err("substring() input must be string".into()),
    });

    // parse environment
    let environment = match dm::detect_environment("tgstation.dme")? {
        Some(env) => env,
        None => {
            eprintln!("Unable to find a .dme file in this directory");
            return Ok(());
        }
    };
    println!("parsing {}", environment.display());

    let mut context = dm::Context::default();
    context.set_print_severity(Some(dm::Severity::Error));
    let mut pp = dm::preprocessor::Preprocessor::new(&context, environment.clone())?;
    let (module_docs, objtree);
    {
        let indents = dm::indents::IndentProcessor::new(&context, &mut pp);
        let mut parser = dm::parser::Parser::new(&context, indents);
        parser.run();
        module_docs = parser.take_module_docs();
        objtree = parser.finalize_object_tree();
    }
    pp.finalize();

    println!("collating documented types");
    let mut types_with_docs = BTreeMap::new();
    let mut progress = Progress::default();

    // collate modules which have docs
    let mut modules = BTreeMap::<PathBuf, Module>::new();
    let mut macro_count = 0;
    for (file, comment_vec) in module_docs {
        let file_path = context.file_path(file);
        progress.update(&file_path.display().to_string());
        let module = modules.entry(file_path).or_default();
        for (line, doc) in comment_vec {
            module.items_wip.push((line, ModuleItem::DocComment(doc)));
        }
    }

    // if macros have docs, that counts as a module too
    for (range, (name, define)) in pp.history().iter() {
        progress.update(&format!("#define {}", name));

        let (docs, has_params, params, is_variadic);
        match define {
            dm::preprocessor::Define::Constant { docs: Some(dc), .. } => {
                docs = dc;
                has_params = false;
                params = &[][..];
                is_variadic = false;
            }
            dm::preprocessor::Define::Function { docs: Some(dc), params: macro_params, variadic, .. } => {
                docs = dc;
                has_params = true;
                params = macro_params;
                is_variadic = *variadic;
            }
            _ => continue,
        }
        let docs = match parse_md_docblock(&docs.text) {
            Ok(block) => block,
            Err(e) => {
                progress.println(&format!("#define {}: {}", name, e));
                continue;
            }
        };

        let module = modules.entry(context.file_path(range.start.file)).or_default();
        module.items_wip.push((range.start.line, ModuleItem::Define { name, teaser: docs.teaser.clone() }));
        module.defines.insert(name, Define { docs, has_params, params, is_variadic });
        macro_count += 1;
    }

    // collate types which have docs
    let mut count = 0;
    objtree.root().recurse(&mut |ty| {
        count += 1;
        progress.update(&ty.path);

        let mut parsed_type = ParsedType::default();
        parsed_type.name = ty.get().vars.get("name")
            .and_then(|v| v.value.constant.as_ref())
            .and_then(|c| c.as_str())
            .unwrap_or("");

        let mut anything = false;
        if let Some(ref docs) = ty.docs {
            match parse_md_docblock(&docs.text) {
                Ok(block) => {
                    if let Some(module) = modules.get_mut(&context.file_path(ty.location.file)) {
                        module.items_wip.push((ty.location.line, ModuleItem::Type { path: ty.get().pretty_path(), teaser: block.teaser.clone() }));
                    }
                    parsed_type.docs = Some(block);
                    anything = true;
                }
                Err(e) => progress.println(&format!("{}: {}", ty.path, e)),
            }
        }

        for (name, var) in ty.get().vars.iter() {
            if let Some(ref docs) = var.value.docs {
                match parse_md_docblock(&docs.text) {
                    Ok(block) => {
                        let path = match ty.get_declaration(name) {
                            Some(decl) => format_type_path(&decl.var_type.type_path),
                            _ => String::new(),
                        };
                        parsed_type.vars.insert(name, Var {
                            docs: block,
                            type_path: path,
                            decl: if var.declaration.is_some() { "var" } else { "" },
                        });
                        anything = true;
                    }
                    Err(e) => progress.println(&format!("{}/var/{}: {}", ty.path, name, e)),
                }
            }
        }

        for (name, proc) in ty.get().procs.iter() {
            let proc_value = proc.value.last().unwrap();
            if let Some(ref docs) = proc_value.docs {
                match parse_md_docblock(&docs.text) {
                    Ok(block) => {
                        parsed_type.procs.insert(name, Proc {
                            docs: block,
                            params: proc_value.parameters.iter().map(|p| Param {
                                name: p.name.clone(),
                                type_path: format_type_path(&p.path),
                            }).collect(),
                            decl: match proc.declaration {
                                Some(ref decl) => if decl.is_verb { "verb" } else { "proc" },
                                None => "",
                            },
                        });
                        anything = true;
                    }
                    Err(e) => progress.println(&format!("{}/proc/{}: {}", ty.path, name, e)),
                }
            }
        }

        if anything {
            if ty.is_root() {
                parsed_type.htmlname = "global";
            } else {
                parsed_type.htmlname = &ty.get().path[1..];
            }
            types_with_docs.insert(ty.get().pretty_path(), parsed_type);
        }
    });

    // finalize modules
    for (path, module) in modules.iter_mut() {
        module.htmlname = path.with_extension("").display().to_string().replace("\\", "/");
        module.items_wip.sort_by_key(|&(line, _)| line);

        let mut docs: Option<dm::docs::DocComment> = None;
        let mut last_line = 0;
        for (line, item) in module.items_wip.drain(..) {
            match item {
                ModuleItem::DocComment(doc) => {
                    if line > last_line + 1 {
                        if let Some(ref mut docs) = docs {
                            docs.text.push_str("\n");
                        }
                    }
                    doc.merge_into(&mut docs);
                    last_line = line;
                },
                other => {
                    if let Some(doc) = docs.take() {
                        module.items.push(ModuleItem::Docs(doc.text));
                    }
                    module.items.push(other);
                }
            }
        }
        if let Some(doc) = docs.take() {
            module.items.push(ModuleItem::Docs(doc.text));
        }
    }

    drop(progress);
    print!("documenting {} modules, {} macros, ", modules.len(), macro_count);
    if count == 0 {
        println!("0 types");
    } else {
        println!("{}/{} types ({}%)", types_with_docs.len(), count, (types_with_docs.len() * 100 / count));
    }

    ALL_TYPE_NAMES.with(|all| {
        all.borrow_mut().extend(types_with_docs.keys().map(|&t| t.to_owned()));
    });

    // render
    println!("saving static resources");
    progress = Progress::default();
    for (name, contents) in template::RESOURCES {
        progress.update(name);
        create(&output_path.join(name))?.write_all(contents)?;
    }
    drop(progress);

    let env_filename = environment.display().to_string();
    let mut env = Environment {
        dmdoc: DmDoc {
            version: env!("CARGO_PKG_VERSION"),
        },
        filename: &env_filename,
        world_name: objtree.find("/world")
            .and_then(|w| w.get().vars.get("name"))
            .and_then(|v| v.value.constant.as_ref())
            .and_then(|c| c.as_str())
            .unwrap_or(""),
        git: Default::default(),
    };
    if let Err(e) = git_info(&mut env.git) {
        println!("incomplete git info: {}", e);
    }
    let env = &env;

    progress = Progress::default();
    progress.println("rendering html");
    {
        #[derive(Serialize)]
        struct Index<'a> {
            env: &'a Environment<'a>,
            types: &'a BTreeMap<&'a str, ParsedType<'a>>,
            modules: &'a BTreeMap<PathBuf, Module<'a>>,
        }

        progress.update("index.html");
        let mut index = create(&output_path.join("index.html"))?;
        index.write_all(tera.render("dm_index.html", &Index {
            env,
            types: &types_with_docs,
            modules: &modules,
        })?.as_bytes())?;
    }

    for (path, details) in types_with_docs.iter() {
        #[derive(Serialize)]
        struct Type<'a> {
            env: &'a Environment<'a>,
            base_href: &'a str,
            path: &'a str,
            details: &'a ParsedType<'a>,
            types: &'a BTreeMap<&'a str, ParsedType<'a>>,
        }

        let fname = format!("{}.html", details.htmlname);
        progress.update(&fname);

        let mut base = String::new();
        for _ in fname.chars().filter(|&x| x == '/') {
            base.push_str("../");
        }

        let mut f = create(&output_path.join(&fname))?;
        f.write_all(tera.render("dm_type.html", &Type {
            env,
            base_href: &base,
            path,
            details,
            types: &types_with_docs,
        })?.as_bytes())?;
    }

    for (path, details) in modules.iter() {
        #[derive(Serialize)]
        struct ModuleArgs<'a> {
            env: &'a Environment<'a>,
            base_href: &'a str,
            path: &'a Path,
            details: &'a Module<'a>,
        }

        let fname = format!("{}.html", details.htmlname);
        progress.update(&fname);

        let mut base = String::new();
        for _ in fname.chars().filter(|&x| x == '/') {
            base.push_str("../");
        }

        let mut f = create(&output_path.join(&fname))?;
        f.write_all(tera.render("dm_module.html", &ModuleArgs {
            env,
            base_href: &base,
            path,
            details,
        })?.as_bytes())?;
    }
    drop(progress);

    Ok(())
}

// ----------------------------------------------------------------------------
// Helpers

fn format_type_path(vec: &[String]) -> String {
    if vec.is_empty() {
        String::new()
    } else {
        format!("/{}", vec.join("/"))
    }
}

/// Create the parent dirs of a file and then itself.
fn create(path: &Path) -> io::Result<File> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    File::create(path)
}

fn render_markdown(markdown: &str, summary: bool) -> String {
    let mut buf = String::new();
    let mut parser = pulldown_cmark::Parser::new(markdown).peekable();
    match (summary, parser.peek()) {
        (true, Some(&pulldown_cmark::Event::Start(pulldown_cmark::Tag::Paragraph))) => {
            // Skip the opening <p>
            parser.next();
            // Parse everything
            let mut rest: Vec<_> = parser.collect();
            // Drop the closing </p>
            if let Some(&pulldown_cmark::Event::End(pulldown_cmark::Tag::Paragraph)) = rest.last() {
                let len = rest.len() - 1;
                rest.truncate(len);
            }
            pulldown_cmark::html::push_html(&mut buf, rest.into_iter());
        },
        _ => pulldown_cmark::html::push_html(&mut buf, parser),
    }
    let len = buf.trim_right().len();
    buf.truncate(len);
    buf
}

fn git_info(git: &mut Git) -> Result<(), git2::Error> {
    macro_rules! req {
        ($e:expr) => { match $e { Some(x) => x, None => {
            println!("incomplete git info: malformed or non-utf8 name");
            return Ok(());
        }}}
    }

    // get the revision
    let repo = git2::Repository::open_from_env()?;
    let head = repo.head()?;
    let head_oid = head.peel_to_commit()?.id();
    git.revision = head_oid.to_string();

    if !head.is_branch() {
        println!("incomplete git info: HEAD is not a branch");
        return Ok(());
    }

    // check that the current revision is an ancestor of its remote
    let branch = repo.find_branch(req!(head.shorthand()), git2::BranchType::Local)?;
    if let Ok(Some(name)) = branch.name() {
        git.branch = name.to_owned();
    }
    let upstream = branch.upstream()?;
    let upstream_oid = upstream.get().peel_to_commit()?.id();
    let upstream_name = req!(upstream.name()?);
    if repo.merge_base(head_oid, upstream_oid)? != head_oid {
        println!("incomplete git info: HEAD is not an ancestor of {}", upstream_name);
        return Ok(());
    }

    // figure out the remote URL, convert from SSH to HTTPS
    let mut iter = upstream_name.splitn(2, "/");
    let remote_name = req!(iter.next());
    if let Some(name) = iter.next() {
        git.remote_branch = name.to_owned();
    }

    let remote = repo.find_remote(remote_name)?;
    let mut url = req!(remote.url());
    if url.ends_with("/") {
        url = &url[..url.len() - 1];
    }
    if url.ends_with(".git") {
        url = &url[..url.len() - 4];
        if url.ends_with("/") {
            url = &url[..url.len() - 1];
        }
    }
    if url.starts_with("https://") || url.starts_with("http://") {
        git.web_url = url.to_owned();
    } else if url.starts_with("ssh://") {
        git.web_url = url.replace("ssh://", "https://");
    } else {
        let at = req!(url.find("@"));
        let colon = req!(url.find(":"));
        if colon >= at {
            git.web_url = format!("https://{}/{}", &url[at+1..colon], &url[colon+1..]);
        } else {
            println!("incomplete git info: weird SSH path: {}", url);
        }
    }
    Ok(())
}

/// Helper for printing progress information.
#[derive(Default)]
struct Progress {
    last_len: usize,
}

impl Progress {
    fn update(&mut self, msg: &str) {
        print!("\r{}", msg);
        for _ in msg.len() .. self.last_len {
            print!(" ");
        }
        self.last_len = msg.len();
    }

    fn println(&mut self, msg: &str) {
        print!("\r");
        for _ in 0..self.last_len {
            print!(" ");
        }
        println!("\r{}", msg);
    }
}

impl Drop for Progress {
    fn drop(&mut self) {
        self.update("");
        print!("\r");
    }
}

// ----------------------------------------------------------------------------
// Templating structs

#[derive(Serialize)]
struct Environment<'a> {
    dmdoc: DmDoc,
    filename: &'a str,
    world_name: &'a str,
    git: Git,
}

#[derive(Serialize)]
struct DmDoc {
    version: &'static str,
}

#[derive(Serialize, Default)]
struct Git {
    revision: String,
    branch: String,
    remote_branch: String,
    web_url: String,
}

/// A parsed documented type.
#[derive(Default, Serialize)]
struct ParsedType<'a> {
    name: &'a str,
    docs: Option<DocBlock>,
    vars: BTreeMap<&'a str, Var>,
    procs: BTreeMap<&'a str, Proc>,
    htmlname: &'a str,
}

#[derive(Serialize)]
struct Var {
    docs: DocBlock,
    decl: &'static str,
    type_path: String,
}

#[derive(Serialize)]
struct Proc {
    docs: DocBlock,
    decl: &'static str,
    params: Vec<Param>,
}

#[derive(Serialize)]
struct Param {
    name: String,
    type_path: String,
}

#[derive(Default, Serialize)]
struct Module<'a> {
    htmlname: String,
    name: &'a str,
    teaser: String,
    items: Vec<ModuleItem<'a>>,
    items_wip: Vec<(u32, ModuleItem<'a>)>,
    defines: BTreeMap<&'a str, Define<'a>>,
}

#[derive(Serialize)]
struct Define<'a> {
    docs: DocBlock,
    has_params: bool,
    params: &'a [String],
    is_variadic: bool,
}

#[derive(Serialize)]
enum ModuleItem<'a> {
    // preparation
    #[serde(skip)]
    DocComment(dm::docs::DocComment),

    // rendering
    #[serde(rename="docs")]
    Docs(String),
    #[serde(rename="define")]
    Define {
        name: &'a str,
        teaser: String,
    },
    #[serde(rename="type")]
    Type {
        path: &'a str,
        teaser: String,
    },
}
