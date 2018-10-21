//! A CLI tool to generate HTML documentation of DreamMaker codebases.
#![forbid(unsafe_code)]
extern crate dreammaker as dm;
extern crate git2;
extern crate pulldown_cmark;
extern crate tera;
extern crate walkdir;
#[macro_use]
extern crate serde_derive;

mod markdown;
mod template;

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use dm::docs::*;

use markdown::DocBlock;

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
    tera.register_filter("linkify_type", |input, _opts| match input {
        tera::Value::String(s) => {
            Ok(linkify_type(s.split("/").skip_while(|b| b.is_empty())).into())
        }
        tera::Value::Array(a) => Ok(linkify_type(a.iter().filter_map(|v| v.as_str())).into()),
        _ => Err("linkify_type() input must be string".into()),
    });
    tera.register_filter("length", |input, _opts| match input {
        tera::Value::String(s) => Ok(s.len().into()),
        tera::Value::Array(a) => Ok(a.len().into()),
        tera::Value::Object(o) => Ok(o.len().into()),
        _ => Ok(0.into()),
    });
    tera.register_filter("substring", |input, opts| match input {
        tera::Value::String(s) => {
            let start = opts.get("start").and_then(|v| v.as_u64()).unwrap_or(0) as usize;
            let mut end = opts
                .get("end")
                .and_then(|v| v.as_u64())
                .map(|s| s as usize)
                .unwrap_or(s.len());
            if end > s.len() {
                end = s.len();
            }
            Ok(s[start..end].into())
        }
        _ => Err("substring() input must be string".into()),
    });

    // parse environment
    let environment = match dm::detect_environment_default()? {
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
    let mut modules = BTreeMap::new();
    let mut macro_count = 0;
    for (file, comment_vec) in module_docs {
        let file_path = context.file_path(file);
        progress.update(&file_path.display().to_string());
        let module = module_entry(&mut modules, &file_path);
        for (line, doc) in comment_vec {
            module.items_wip.push((line, ModuleItem::DocComment(doc)));
        }
    }

    // if macros have docs, that counts as a module too
    for (range, (name, define)) in pp.history().iter() {
        progress.update(&format!("#define {}", name));

        let (docs, has_params, params, is_variadic);
        match define {
            dm::preprocessor::Define::Constant { docs: dc, .. } => {
                docs = dc;
                has_params = false;
                params = &[][..];
                is_variadic = false;
            }
            dm::preprocessor::Define::Function {
                docs: dc,
                params: macro_params,
                variadic,
                ..
            } => {
                docs = dc;
                has_params = true;
                params = macro_params;
                is_variadic = *variadic;
            }
        }
        if docs.is_empty() {
            continue;
        }
        let docs = DocBlock::parse(&docs.text());
        let module = module_entry(&mut modules, &context.file_path(range.start.file));
        module.items_wip.push((
            range.start.line,
            ModuleItem::Define {
                name,
                teaser: docs.teaser().to_owned(),
            },
        ));
        module.defines.insert(
            name,
            Define {
                docs,
                has_params,
                params,
                is_variadic,
                line: range.start.line,
            },
        );
        macro_count += 1;
    }

    // search the code tree for Markdown files
    // TODO: don't hardcode this?
    let mut index_docs = None;
    for entry in walkdir::WalkDir::new("code")
        .into_iter()
        .filter_entry(is_visible)
    {
        use std::io::Read;

        let entry = entry?;
        let path = entry.path();
        if path.extension() != Some("md".as_ref()) {
            continue;
        }
        progress.update(&path.display().to_string());

        let mut buf = String::new();
        File::open(path)?.read_to_string(&mut buf)?;
        if path == Path::new("code/README.md") {
            index_docs = Some(DocBlock::parse_with_title(&buf));
        } else {
            let module = module_entry(&mut modules, &path);
            module.items_wip.push((
                0,
                ModuleItem::DocComment(DocComment {
                    kind: CommentKind::Block,
                    target: DocTarget::EnclosingItem,
                    text: buf,
                }),
            ));
        }
    }

    // collate types which have docs
    let mut count = 0;
    let mut substance_count = 0;
    objtree.root().recurse(&mut |ty| {
        count += 1;
        progress.update(&ty.path);

        let mut parsed_type = ParsedType::default();
        parsed_type.name = ty
            .get()
            .vars
            .get("name")
            .and_then(|v| v.value.constant.as_ref())
            .and_then(|c| c.as_str())
            .unwrap_or("")
            .into();

        let mut anything = false;
        let mut substance = false;
        if !ty.docs.is_empty() {
            let (title, block) = DocBlock::parse_with_title(&ty.docs.text());
            if let Some(title) = title {
                parsed_type.name = title.into();
            }
            anything = true;
            substance = block.has_description;
            parsed_type.docs = Some(block);
        }

        let parent_type = ty.parent_type();
        if parent_type != ty.parent_path() {
            if let Some(parent) = parent_type {
                parsed_type.parent_type = Some(&parent.get().path);
            }
        }

        for (name, var) in ty.get().vars.iter() {
            if !var.value.docs.is_empty() {
                let block = DocBlock::parse(&var.value.docs.text());
                // `type` is pulled from the parent if necessary
                let type_ = ty.get_declaration(name).map(|decl| VarType {
                    is_static: decl.var_type.is_static,
                    is_const: decl.var_type.is_const,
                    is_tmp: decl.var_type.is_tmp,
                    path: &decl.var_type.type_path,
                });
                parsed_type.vars.insert(
                    name,
                    Var {
                        docs: block,
                        type_,
                        // but `decl` is only used if it's on this type
                        decl: if var.declaration.is_some() { "var" } else { "" },
                        file: context.file_path(var.value.location.file),
                        line: var.value.location.line,
                    },
                );
                anything = true;
                substance = true;
            }
        }

        for (name, proc) in ty.get().procs.iter() {
            let proc_value = proc.value.last().unwrap();
            if !proc_value.docs.is_empty() {
                let block = DocBlock::parse(&proc_value.docs.text());
                parsed_type.procs.insert(
                    name,
                    Proc {
                        docs: block,
                        params: proc_value
                            .parameters
                            .iter()
                            .map(|p| Param {
                                name: p.name.clone(),
                                type_path: format_type_path(&p.path),
                            }).collect(),
                        decl: match proc.declaration {
                            Some(ref decl) => if decl.is_verb {
                                "verb"
                            } else {
                                "proc"
                            },
                            None => "",
                        },
                        file: context.file_path(proc_value.location.file),
                        line: proc_value.location.line,
                    },
                );
                anything = true;
                substance = true;
            }
        }

        // file the type under its module as well
        if let Some(ref block) = parsed_type.docs {
            if let Some(module) =
                modules.get_mut(&module_path(&context.file_path(ty.location.file)))
            {
                module.items_wip.push((
                    ty.location.line,
                    ModuleItem::Type {
                        path: ty.get().pretty_path(),
                        teaser: block.teaser().to_owned(),
                        substance: substance,
                    },
                ));
            }
        }

        if anything {
            parsed_type.file = context.file_path(ty.location.file);
            parsed_type.line = ty.location.line;
            parsed_type.substance = substance;
            if substance {
                if ty.is_root() {
                    parsed_type.htmlname = "global";
                } else {
                    parsed_type.htmlname = &ty.get().path[1..];
                }
            }
            types_with_docs.insert(ty.get().pretty_path(), parsed_type);
            if substance {
                substance_count += 1;
            }
        }
    });

    // finalize modules
    for (_, module) in modules.iter_mut() {
        module.items_wip.sort_by_key(|&(line, _)| line);

        let mut docs = DocCollection::default();
        let mut _first = true;
        macro_rules! push_docs {
            () => {
                // oof
                if !docs.is_empty() {
                    let doc = ::std::mem::replace(&mut docs, Default::default());
                    if _first {
                        _first = false;
                        let (title, block) = DocBlock::parse_with_title(&doc.text());
                        module.name = title;
                        module.teaser = block.teaser().to_owned();
                        module.items.push(ModuleItem::Docs(block.html));
                    } else {
                        module
                            .items
                            .push(ModuleItem::Docs(markdown::render(&doc.text())));
                    }
                }
            };
        }

        let mut last_line = 0;
        for (line, item) in module.items_wip.drain(..) {
            match item {
                ModuleItem::DocComment(doc) => {
                    if line > last_line + 1 {
                        docs.push(DocComment::new(CommentKind::Line, DocTarget::EnclosingItem));
                    }
                    docs.push(doc);
                    last_line = line;
                }
                other => {
                    push_docs!();
                    module.items.push(other);
                }
            }
        }
        push_docs!();
    }

    drop(progress);
    print!(
        "documenting {} modules, {} macros, ",
        modules.len(),
        macro_count
    );
    if count == 0 {
        println!("0 types");
    } else {
        println!(
            "{}/{}/{} types ({}%)",
            substance_count,
            types_with_docs.len(),
            count,
            (types_with_docs.len() * 100 / count)
        );
    }

    ALL_TYPE_NAMES.with(|all| {
        all.borrow_mut().extend(
            types_with_docs
                .iter()
                .filter(|(_, v)| v.substance)
                .map(|(&t, _)| t.to_owned()),
        );
    });

    // render
    println!("saving static resources");
    fs::create_dir_all(output_path)?;
    template::save_resources(output_path)?;

    let env_filename = environment.display().to_string();
    let world_name = objtree
        .find("/world")
        .and_then(|w| w.get().vars.get("name"))
        .and_then(|v| v.value.constant.as_ref())
        .and_then(|c| c.as_str())
        .unwrap_or("");
    let title = index_docs
        .as_ref()
        .and_then(|(title, _)| title.as_ref())
        .map(|s| &s[..])
        .unwrap_or(world_name);
    let mut env = Environment {
        dmdoc: DmDoc {
            version: env!("CARGO_PKG_VERSION"),
        },
        filename: &env_filename,
        world_name,
        title,
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
            html: Option<&'a str>,
            modules: Vec<IndexTree<'a>>,
            types: Vec<IndexTree<'a>>,
        }

        progress.update("index.html");
        let mut index = create(&output_path.join("index.html"))?;
        index.write_all(
            tera.render(
                "dm_index.html",
                &Index {
                    env,
                    html: index_docs.as_ref().map(|(_, docs)| &docs.html[..]),
                    modules: build_index_tree(modules.iter().map(|(_path, module)| IndexTree {
                        htmlname: &module.htmlname,
                        full_name: &module.orig_filename,
                        self_name: match module.name {
                            None => last_element(&module.htmlname),
                            Some(ref t) => t.as_str(),
                        },
                        teaser: &module.teaser,
                        no_substance: false,
                        children: Vec::new(),
                    })),
                    types: build_index_tree(types_with_docs.iter().map(|(path, ty)| IndexTree {
                        htmlname: &ty.htmlname,
                        full_name: path,
                        self_name: if ty.name.is_empty() {
                            last_element(path)
                        } else {
                            &ty.name
                        },
                        teaser: ty.docs.as_ref().map_or("", |d| d.teaser()),
                        no_substance: !ty.substance,
                        children: Vec::new(),
                    })),
                },
            )?.as_bytes(),
        )?;
    }

    for (path, details) in modules.iter() {
        #[derive(Serialize)]
        struct ModuleArgs<'a> {
            env: &'a Environment<'a>,
            base_href: &'a str,
            path: &'a str,
            details: &'a Module<'a>,
        }

        let fname = format!("{}.html", details.htmlname);
        progress.update(&fname);

        let mut base = String::new();
        for _ in fname.chars().filter(|&x| x == '/') {
            base.push_str("../");
        }

        let mut f = create(&output_path.join(&fname))?;
        f.write_all(
            tera.render(
                "dm_module.html",
                &ModuleArgs {
                    env,
                    base_href: &base,
                    path,
                    details,
                },
            )?.as_bytes(),
        )?;
    }

    for (path, details) in types_with_docs.iter() {
        if !details.substance {
            continue;
        }

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
        f.write_all(
            tera.render(
                "dm_type.html",
                &Type {
                    env,
                    base_href: &base,
                    path,
                    details,
                    types: &types_with_docs,
                },
            )?.as_bytes(),
        )?;
    }
    drop(progress);

    Ok(())
}

// ----------------------------------------------------------------------------
// Helpers

fn module_path(path: &Path) -> String {
    path.with_extension("")
        .display()
        .to_string()
        .replace("\\", "/")
}

fn module_entry<'a, 'b>(
    modules: &'a mut BTreeMap<String, Module<'b>>,
    path: &Path,
) -> &'a mut Module<'b> {
    modules.entry(module_path(path)).or_insert_with(|| {
        let mut module = Module::default();
        module.htmlname = module_path(path);
        module.orig_filename = path.display().to_string().replace("\\", "/");
        module
    })
}

fn is_visible(entry: &walkdir::DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|s| !s.starts_with("."))
        .unwrap_or(true)
}

fn format_type_path(vec: &[String]) -> String {
    if vec.is_empty() {
        String::new()
    } else {
        format!("/{}", vec.join("/"))
    }
}

fn linkify_type<'a, I: Iterator<Item = &'a str>>(iter: I) -> String {
    let mut output = String::new();
    let mut all_progress = String::new();
    let mut progress = String::new();
    for bit in iter {
        all_progress.push_str("/");
        all_progress.push_str(bit);
        progress.push_str("/");
        progress.push_str(bit);
        if ALL_TYPE_NAMES.with(|t| t.borrow().contains(&all_progress)) {
            use std::fmt::Write;
            let _ = write!(
                output,
                r#"/<a href="{}.html">{}</a>"#,
                &all_progress[1..],
                &progress[1..]
            );
            progress.clear();
        }
    }
    output.push_str(&progress);
    output
}

// (normalized, reference) -> (href, tooltip)
fn handle_crosslink(_: &str, reference: &str) -> Option<(String, String)> {
    // TODO: allow performing relative searches, find vars and procs too
    let mut progress = String::new();
    let mut best = String::new();
    for bit in reference.split("/").skip_while(|s| s.is_empty()) {
        progress.push_str("/");
        progress.push_str(bit);
        if ALL_TYPE_NAMES.with(|t| t.borrow().contains(&progress)) {
            best = progress.clone();
        }
    }
    if !best.is_empty() {
        Some((format!("{}.html", &best[1..]), best))
    } else {
        None
    }
}

/// Create the parent dirs of a file and then itself.
fn create(path: &Path) -> io::Result<File> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    File::create(path)
}

fn git_info(git: &mut Git) -> Result<(), git2::Error> {
    macro_rules! req {
        ($e:expr) => {
            match $e {
                Some(x) => x,
                None => {
                    println!("incomplete git info: malformed or non-utf8 name");
                    return Ok(());
                }
            }
        };
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
        println!(
            "incomplete git info: HEAD is not an ancestor of {}",
            upstream_name
        );
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
            git.web_url = format!("https://{}/{}", &url[at + 1..colon], &url[colon + 1..]);
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
        for _ in msg.len()..self.last_len {
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
// Tree stuff

#[derive(Serialize)]
struct IndexTree<'a> {
    htmlname: &'a str, // href="{{htmlname}}.html"
    full_name: &'a str,
    self_name: &'a str,
    teaser: &'a str,
    no_substance: bool,
    children: Vec<IndexTree<'a>>,
}

fn build_index_tree<'a, I>(iter: I) -> Vec<IndexTree<'a>>
where
    I: IntoIterator<Item = IndexTree<'a>>,
{
    let mut stack = vec![IndexTree {
        htmlname: "",
        full_name: "",
        self_name: "",
        teaser: "",
        no_substance: false,
        children: Vec::new(),
    }];
    for each in iter {
        // don't speak to me or my poorly-thought-out son ever again
        {
            let mut i = 1;
            let mut len = 0;
            let mut bits = each.full_name.split("/").peekable();
            if bits.peek() == Some(&"") {
                bits.next();
                len += 1;
            }
            // determine common position
            while i < stack.len() {
                {
                    let bit = match bits.peek() {
                        Some(bit) => bit,
                        None => break,
                    };
                    if stack[i].full_name != &each.full_name[..len + bit.len()] {
                        break;
                    }
                    len += 1 + bit.len();
                }
                i += 1;
                bits.next();
            }
            // pop everything below our common parent
            combine(&mut stack, i);
            // push ancestors between stack and ourselves
            while let Some(bit) = bits.next() {
                if bits.peek().is_none() {
                    break;
                }
                stack.push(IndexTree {
                    htmlname: "",
                    full_name: &each.full_name[..len + bit.len()],
                    self_name: bit,
                    teaser: "",
                    no_substance: true,
                    children: Vec::new(),
                });
                len += 1 + bit.len();
            }
        }
        // push ourselves
        stack.push(each);
    }
    combine(&mut stack, 1);
    stack.remove(0).children
}

fn combine(stack: &mut Vec<IndexTree>, to: usize) {
    while to < stack.len() {
        let popped = stack.pop().unwrap();
        let last = stack.last_mut().expect("last_mut");
        last.no_substance = last.no_substance && popped.no_substance;
        last.children.push(popped);
    }
}

fn last_element(path: &str) -> &str {
    path.split("/").last().unwrap_or("")
}

// ----------------------------------------------------------------------------
// Templating structs

#[derive(Serialize)]
struct Environment<'a> {
    dmdoc: DmDoc,
    filename: &'a str,
    world_name: &'a str,
    title: &'a str,
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
    name: std::borrow::Cow<'a, str>,
    parent_type: Option<&'a str>,
    docs: Option<DocBlock>,
    substance: bool,
    vars: BTreeMap<&'a str, Var<'a>>,
    procs: BTreeMap<&'a str, Proc>,
    htmlname: &'a str,
    file: PathBuf,
    line: u32,
}

#[derive(Serialize)]
struct Var<'a> {
    docs: DocBlock,
    decl: &'static str,
    #[serde(rename = "type")]
    type_: Option<VarType<'a>>,
    file: PathBuf,
    line: u32,
}

#[derive(Serialize)]
struct VarType<'a> {
    is_static: bool,
    is_const: bool,
    is_tmp: bool,
    path: &'a [String],
}

#[derive(Serialize)]
struct Proc {
    docs: DocBlock,
    decl: &'static str,
    params: Vec<Param>,
    file: PathBuf,
    line: u32,
}

#[derive(Serialize)]
struct Param {
    name: String,
    type_path: String,
}

#[derive(Default, Serialize)]
struct Module<'a> {
    htmlname: String,
    orig_filename: String,
    name: Option<String>,
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
    line: u32,
}

#[derive(Serialize)]
enum ModuleItem<'a> {
    // preparation
    #[serde(skip)]
    DocComment(DocComment),

    // rendering
    #[serde(rename = "docs")]
    Docs(String),
    #[serde(rename = "define")]
    Define { name: &'a str, teaser: String },
    #[serde(rename = "type")]
    Type {
        path: &'a str,
        teaser: String,
        substance: bool,
    },
}
