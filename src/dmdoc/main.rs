//! A CLI tool to generate HTML documentation of DreamMaker codebases.
#![forbid(unsafe_code)]
extern crate dreammaker as dm;
extern crate docstrings;
extern crate pulldown_cmark;
extern crate tera;
#[macro_use] extern crate serde_derive;

mod template;

use std::collections::{BTreeMap, BTreeSet};
use std::cell::RefCell;
use std::io::{self, Write};
use std::fs::{self, File};
use std::path::Path;

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

    // parse environment
    let context = dm::Context::default();
    let environment = match dm::detect_environment("tgstation.dme")? {
        Some(env) => env,
        None => {
            eprintln!("Unable to find a .dme file in this directory");
            return Ok(());
        }
    };
    println!("parsing {}", environment.display());
    let objtree = context.parse_environment(&environment)?;

    // collate types which have docs
    println!("collating documented types");
    let mut types_with_docs = BTreeMap::new();
    let mut progress = Progress::default();
    let mut count = 0;
    objtree.root().recurse(&mut |ty| {
        count += 1;
        progress.update(&ty.path);

        let mut parsed_type = ParsedType::default();
        let mut anything = false;
        if let Some(ref docs) = ty.docs {
            match parse_md_docblock(&docs.text) {
                Ok(block) => {
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
                        });
                        anything = true;
                    }
                    Err(e) => progress.println(&format!("{}/proc/{}: {}", ty.path, name, e)),
                }
            }
        }

        if anything {
            if ty.is_root() {
                parsed_type.filename = "global";
            } else {
                parsed_type.filename = &ty.get().path[1..];
            }
            types_with_docs.insert(ty.get().pretty_path(), parsed_type);
        }
    });
    drop(progress);
    if count == 0 {
        println!("none of {} types have documentation", count);
        return Ok(());
    } else {
        println!("documenting {}/{} types ({}%)", types_with_docs.len(), count, (types_with_docs.len() * 100 / count));
    }

    ALL_TYPE_NAMES.with(|all| {
        all.borrow_mut().extend(types_with_docs.keys().map(|&t| t.to_owned()));
    });

    println!("saving static resources");
    progress = Progress::default();
    for (name, contents) in template::RESOURCES {
        progress.update(name);
        create(&output_path.join(name))?.write_all(contents.as_bytes())?;
    }

    progress.println("rendering html");
    let env_filename = environment.display().to_string();
    let env = &Environment {
        filename: &env_filename,
        world_name: objtree.find("/world")
            .and_then(|w| w.get().vars.get("name"))
            .and_then(|v| v.value.constant.as_ref())
            .and_then(|c| c.as_str())
            .unwrap_or(""),
        dmdoc_version: env!("CARGO_PKG_VERSION"),
    };

    {
        #[derive(Serialize)]
        struct Index<'a> {
            env: &'a Environment<'a>,
            types: &'a BTreeMap<&'a str, ParsedType<'a>>,
        }

        progress.update("index.html");
        let mut index = create(&output_path.join("index.html"))?;
        index.write_all(tera.render("dm_index.html", &Index {
            env,
            types: &types_with_docs,
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

        let fname = format!("{}.html", details.filename);
        progress.update(&fname);

        let mut base = String::new();
        for _ in  fname.chars().filter(|&x| x == '/') {
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
    filename: &'a str,
    world_name: &'a str,
    dmdoc_version: &'a str,
}

/// A parsed documented type.
#[derive(Default, Serialize)]
struct ParsedType<'a> {
    docs: Option<DocBlock>,
    vars: BTreeMap<&'a str, Var>,
    procs: BTreeMap<&'a str, Proc>,
    filename: &'a str,
}

#[derive(Serialize)]
struct Var {
    docs: DocBlock,
    type_path: String,
}

#[derive(Serialize)]
struct Proc {
    docs: DocBlock,
    params: Vec<Param>,
}

#[derive(Serialize)]
struct Param {
    name: String,
    type_path: String,
}
