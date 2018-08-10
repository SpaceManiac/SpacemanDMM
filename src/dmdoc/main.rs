//! A CLI tool to generate HTML documentation of DreamMaker codebases.
#![forbid(unsafe_code)]
extern crate dreammaker as dm;
extern crate docstrings;

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

    {
        println!("creating index.html");
        let mut index = create(&output_path.join("index.html"))?;
        writeln!(index, "<h1>{}</h1>", environment.display())?;
        writeln!(index, "<ul>")?;
        for (typath, details) in types_with_docs.iter() {
            write!(index, r#"<li><a href="{fname}.html">{path}</a>"#, path=typath, fname=details.filename)?;
            if let Some(ref own) = details.own {
                write!(index, " - {}", own.teaser)?;
            }
            writeln!(index)?;
        }
        writeln!(index, "</ul>")?;
    }

    progress = Progress::default();
    for (typath, details) in types_with_docs.iter() {
        let fname = format!("{}.html", details.filename);
        progress.update(&format!("creating {}", fname));
        let mut f = create(&output_path.join(&fname))?;
        writeln!(f, "<h1>{}</h1>", typath)?;
        if let Some(ref own) = details.own {
            writeln!(f, "<pre>{:#?}</pre>", own)?;
        }
        if !details.vars.is_empty() {
            writeln!(f, "<h2>Vars</h2>")?;
            writeln!(f, "<table>")?;
            for (name, var) in details.vars.iter() {
                writeln!(f, "<tr><th valign=top>{}</th><td><pre>{:#?}</pre></td></tr>", name, var)?;
            }
            writeln!(f, "</table>")?;
        }
        if !details.procs.is_empty() {
            writeln!(f, "<h2>Procs</h2>")?;
            writeln!(f, "<table>")?;
            for (name, proc) in details.procs.iter() {
                writeln!(f, "<tr><th valign=top>{}</th><td><pre>{:#?}</pre></td></tr>", name, proc)?;
            }
            writeln!(f, "</table>")?;
        }
    }

    Ok(())
}

/// Create the parent dirs of a file and then itself.
fn create(path: &Path) -> io::Result<File> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    File::create(path)
}

/// A parsed documented type.
#[derive(Default)]
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
