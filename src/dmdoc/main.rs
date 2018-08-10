//! A CLI tool to generate HTML documentation of DreamMaker codebases.
#![forbid(unsafe_code)]
extern crate dreammaker as dm;
extern crate docstrings;
extern crate pulldown_cmark;
extern crate tera;
#[macro_use] extern crate serde_derive;

mod template;

use std::collections::BTreeMap;
use std::io::{self, Write};
use std::fs::{self, File};
use std::path::Path;

use docstrings::{DocBlock, parse_md_docblock};

fn main() -> Result<(), Box<std::error::Error>> {
    // TODO: command-line args
    let output_path: &Path = "docs".as_ref();

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
                    parsed_type.own = Some(block);
                    anything = true;
                }
                Err(e) => progress.println(&format!("{}: {}", ty.path, e)),
            }
        }

        for (name, var) in ty.get().vars.iter() {
            if let Some(ref docs) = var.value.docs {
                match parse_md_docblock(&docs.text) {
                    Ok(block) => {
                        parsed_type.vars.insert(name, block);
                        anything = true;
                    }
                    Err(e) => progress.println(&format!("{}/var/{}: {}", ty.path, name, e)),
                }
            }
        }

        for (name, proc) in ty.get().procs.iter() {
            if let Some(ref docs) = proc.value.last().unwrap().docs {
                match parse_md_docblock(&docs.text) {
                    Ok(block) => {
                        parsed_type.procs.insert(name, block);
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

    println!("loading templates");
    let tera = template::builtin()?;

    println!("saving static resources");
    progress = Progress::default();
    progress.update("dmdoc.css");
    create(&output_path.join("dmdoc.css"))?.write_all(include_bytes!("dmdoc.css"))?;
    progress.update("dmdoc.js");
    create(&output_path.join("dmdoc.js"))?.write_all(include_bytes!("dmdoc.js"))?;

    progress.println("rendering html");
    {
        #[derive(Serialize)]
        struct Index<'a> {
            environment: &'a str,
            types: &'a BTreeMap<&'a str, ParsedType<'a>>,
        }

        progress.update("index.html");
        let mut index = create(&output_path.join("index.html"))?;
        index.write_all(tera.render("dm_index.html", &Index {
            environment: &environment.display().to_string(),
            types: &types_with_docs,
        })?.as_bytes())?;
    }

    for (path, details) in types_with_docs.iter() {
        #[derive(Serialize)]
        struct Type<'a> {
            base_href: &'a str,
            path: &'a str,
            details: &'a ParsedType<'a>,
        }

        let fname = format!("{}.html", details.filename);
        progress.update(&fname);

        let mut base = String::new();
        for _ in  fname.chars().filter(|&x| x == '/') {
            base.push_str("../");
        }

        let mut f = create(&output_path.join(&fname))?;
        f.write_all(tera.render("dm_type.html", &Type {
            base_href: &base,
            path,
            details,
        })?.as_bytes())?;
    }
    drop(progress);

    Ok(())
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

/// A parsed documented type.
#[derive(Default, Serialize)]
struct ParsedType<'a> {
    own: Option<DocBlock>,
    vars: BTreeMap<&'a str, DocBlock>,
    procs: BTreeMap<&'a str, DocBlock>,
    filename: &'a str,
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
