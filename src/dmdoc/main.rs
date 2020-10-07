//! A CLI tool to generate HTML documentation of DreamMaker codebases.
#![forbid(unsafe_code)]
extern crate dreammaker as dm;
extern crate pulldown_cmark;
extern crate tera;
extern crate git2;
extern crate walkdir;
#[macro_use] extern crate serde_derive;

mod markdown;
mod template;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::io::{self, Write};
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use tera::Value;

use dm::docs::*;

use markdown::DocBlock;

const BUILD_INFO: &str = concat!(
    "dmdoc ", env!("CARGO_PKG_VERSION"), "  Copyright (C) 2017-2020  Tad Hardesty\n",
    include_str!(concat!(env!("OUT_DIR"), "/build-info.txt")), "\n",
    "This program comes with ABSOLUTELY NO WARRANTY. This is free software,\n",
    "and you are welcome to redistribute it under the conditions of the GNU\n",
    "General Public License version 3.",
);

// ----------------------------------------------------------------------------
// Driver

fn main() {
    if let Err(e) = main2() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn main2() -> Result<(), Box<dyn std::error::Error>> {
    // command-line args
    let mut environment = None;
    let mut output_path = "dmdoc".to_owned();
    let mut index_path = None;
    let mut dry_run = false;

    let mut args = std::env::args();
    let _ = args.next();  // skip executable name
    while let Some(arg) = args.next() {
        if arg == "-V" || arg == "--version" {
            println!("{}", BUILD_INFO);
            return Ok(());
        } else if arg == "-e" {
            environment = Some(args.next().expect("must specify a value for -e"));
        } else if arg == "--output" {
            output_path = args.next().expect("must specify a value for --output");
        } else if arg == "--index" {
            index_path = Some(args.next().expect("must specify a value for --index"));
        } else if arg == "--dry-run" {
            dry_run = true;
        } else {
            return Err(format!("unknown argument: {}", arg).into());
        }
    }

    let output_path: &Path = output_path.as_ref();

    // parse environment
    let environment = match environment {
        Some(e) => e.into(),
        None => match dm::detect_environment_default()? {
            Some(env) => env,
            None => {
                return Err("Unable to find a .dme file in this directory".into());
            }
        }
    };
    println!("parsing {}", environment.display());

    let mut context = dm::Context::default();
    context.autodetect_config(&environment);
    context.set_print_severity(Some(dm::Severity::Error));
    let mut pp = dm::preprocessor::Preprocessor::new(&context, environment.clone())?;
    let (objtree, module_docs) = {
        let indents = dm::indents::IndentProcessor::new(&context, &mut pp);
        let parser = dm::parser::Parser::new(&context, indents);
        parser.parse_with_module_docs()
    };
    let define_history = pp.finalize();

    println!("collating documented types");

    // Any top-level directory which is `#include`d in the `.dme` (most
    // importantly "code", but also "_maps", "interface", and any downstream
    // modular folders) will be searched for `.md` files to include in the docs.
    let mut code_directories = std::collections::HashSet::new();
    context.file_list().for_each(|path| {
        if let Some(std::path::Component::Normal(first)) = path.components().next() {
            code_directories.insert(first.to_owned());
        }
    });

    // get a read on which types *have* docs
    let mut types_with_docs = BTreeMap::new();
    objtree.root().recurse(&mut |ty| {
        // TODO: it would be nice if this was not a duplicate of below
        let mut own_docs = false;
        if !ty.docs.is_empty() {
            own_docs = DocBlock::parse_with_title(&ty.docs.text(), None).1.has_description;
        }

        let mut var_docs = BTreeSet::new();
        for (name, var) in ty.get().vars.iter() {
            if !var.value.docs.is_empty() {
                var_docs.insert(name.as_str());
            }
        }

        let mut proc_docs = BTreeSet::new();
        for (name, proc) in ty.get().procs.iter() {
            // TODO: integrate docs from non-main procs
            if !proc.main_value().docs.is_empty() {
                proc_docs.insert(name.as_str());
            }
        }

        if own_docs || !var_docs.is_empty() || !proc_docs.is_empty() {
            types_with_docs.insert(ty.get().path.as_str(), TypeHasDocs {
                var_docs,
                proc_docs,
            });
        }
    });

    // collate modules which have docs
    let mut modules1 = BTreeMap::new();
    let mut macro_count = 0;
    let mut macros_all = 0;
    for (file, comment_vec) in module_docs {
        let file_path = context.file_path(file);
        let module = module_entry(&mut modules1, &file_path);
        for (line, doc) in comment_vec {
            module.items_wip.push((line, ModuleItem::DocComment(doc)));
        }
    }
    let mut modules_which_exist: BTreeSet<_> = modules1.keys().cloned().collect();

    // build a map of macro names to the module.html files in which they appear
    let mut macro_exists = BTreeSet::new();
    let mut macro_to_module_map = BTreeMap::new();
    for (range, (name, define)) in define_history.iter() {
        macro_exists.insert(name.as_str());
        if !define.docs().is_empty() {
            let mod_path = module_path(&context.file_path(range.start.file));
            modules_which_exist.insert(mod_path.clone());
            macro_to_module_map.insert(name.as_str(), mod_path);
        }
    }

    // set up crosslink error reporting
    let diagnostic_count: std::cell::Cell<i32> = Default::default();
    let error_entity: std::cell::Cell<Option<String>> = Default::default();
    let error_entity_put = |string: String| error_entity.set(Some(string));
    let error_entity_print = || {
        diagnostic_count.set(diagnostic_count.get() + 1);
        if let Some(name) = error_entity.take() {
            eprintln!("{}:", name);
        }
    };

    // (normalized, reference) -> (href, tooltip)
    let broken_link_callback = &|_: &str, reference: &str| -> Option<(String, String)> {
        // macros
        if let Some(module) = macro_to_module_map.get(reference) {
            return Some((format!("{}.html#define/{}", module, reference), reference.to_owned()));
        } else if macro_exists.contains(reference) {
            error_entity_print();
            eprintln!("    [{}]: macro not documented", reference);
            return None;
        } else if reference.ends_with(".dm") || reference.ends_with(".txt") || reference.ends_with(".md") {
            let mod_path = module_path(reference.as_ref());
            if modules_which_exist.contains(&mod_path) {
                return Some((format!("{}.html", mod_path), reference.to_owned()));
            }
            error_entity_print();
            eprintln!("    [{}]: module {}", reference, if Path::new(reference).exists() { "not documented" } else { "does not exist" });
            return None;
        }

        // parse "proc" or "var" reference out
        let mut ty_path = reference;
        let mut proc_name = None;
        let mut var_name = None;
        let mut entity_exists = false;
        if let Some(idx) = reference.find("/proc/") {
            // `[/ty/proc/procname]`
            let name = &reference[idx + "/proc/".len()..];
            proc_name = Some(name);
            ty_path = &reference[..idx];
            if let Some(ty) = objtree.find(ty_path) {
                entity_exists = ty.procs.contains_key(name);
            }
        } else if let Some(idx) = reference.find("/verb/") {
            // `[/ty/verb/procname]`
            let name = &reference[idx + "/verb/".len()..];
            proc_name = Some(name);
            ty_path = &reference[..idx];
            if let Some(ty) = objtree.find(ty_path) {
                entity_exists = ty.procs.contains_key(name);
            }
        } else if let Some(idx) = reference.find("/var/") {
            // `[/ty/var/varname]`
            let name = &reference[idx + "/var/".len()..];
            var_name = Some(name);
            ty_path = &reference[..idx];
            if let Some(ty) = objtree.find(ty_path) {
                entity_exists = ty.vars.contains_key(name);
            }
        } else if let Some(_) = objtree.find(reference) {
            entity_exists = true;
        } else if let Some(idx) = reference.rfind('/') {
            let (parent, rest) = (&reference[..idx], &reference[idx + 1..]);
            if let Some(ty) = objtree.find(parent) {
                if ty.procs.contains_key(rest) && !ty.vars.contains_key(rest) {
                    // correct `[/ty/procname]` to `[/ty/proc/procname]`
                    proc_name = Some(rest);
                    ty_path = parent;
                    error_entity_print();
                    eprintln!("    [{}]: correcting to [{}/proc/{}]", reference, parent, rest);
                    entity_exists = true;
                } else if ty.vars.contains_key(rest) {
                    // correct `[/ty/varname]` to `[/ty/var/varname]`
                    var_name = Some(rest);
                    ty_path = parent;
                    error_entity_print();
                    eprintln!("    [{}]: correcting to [{}/var/{}]", reference, parent, rest);
                    entity_exists = true;
                }
            }
        } else if objtree.root().vars.contains_key(reference) {
            ty_path = "";
            var_name = Some(reference);
            error_entity_print();
            eprintln!("    [{0}]: correcting to [/var/{0}]", reference);
            entity_exists = true;
        }
        // else `[/ty]`

        let mut progress = String::new();
        let mut best = 0;

        if ty_path.is_empty() {
            progress.push_str("/global");
            if let Some(info) = types_with_docs.get("") {
                if let Some(proc_name) = proc_name {
                    if info.proc_docs.contains(proc_name) {
                        best = progress.len();
                    }
                } else if let Some(var_name) = var_name {
                    if info.var_docs.contains(var_name) {
                        best = progress.len();
                    }
                } else {
                    best = progress.len();
                }
            }
        } else {
            for bit in ty_path.trim_start_matches('/').split('/') {
                progress.push_str("/");
                progress.push_str(bit);
                if let Some(info) = types_with_docs.get(progress.as_str()) {
                    if let Some(proc_name) = proc_name {
                        if info.proc_docs.contains(proc_name) {
                            best = progress.len();
                        }
                    } else if let Some(var_name) = var_name {
                        if info.var_docs.contains(var_name) {
                            best = progress.len();
                        }
                    } else {
                        best = progress.len();
                    }
                }
            }
        }

        if best > 0 {
            use std::fmt::Write;

            if best < progress.len() {
                error_entity_print();
                if entity_exists {
                    eprint!("    [{}]: not documented, guessing [{}", reference, &progress[..best]);
                } else {
                    eprint!("    [{}]: unknown crosslink, guessing [{}", reference, &progress[..best]);
                }
                if let Some(proc_name) = proc_name {
                    let _ = eprint!("/proc/{}", proc_name);
                } else if let Some(var_name) = var_name {
                    let _ = eprint!("/var/{}", var_name);
                }
                eprintln!("]");
                progress.truncate(best);
            }

            let mut href = format!("{}.html", &progress[1..]);
            if let Some(proc_name) = proc_name {
                let _ = write!(href, "#proc/{}", proc_name);
            } else if let Some(var_name) = var_name {
                let _ = write!(href, "#var/{}", var_name);
            }
            Some((href, progress))
        } else {
            error_entity_print();
            if entity_exists {
                eprintln!("    [{}]: not documented", reference);
            } else {
                eprintln!("    [{}]: unknown crosslink", reference);
            }
            None
        }
    };

    // if macros have docs, that counts as a module too
    for (range, (name, define)) in define_history.iter() {
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
        macros_all += 1;
        if docs.is_empty() {
            continue;
        }
        error_entity_put(format!("#define {}", name));
        let docs = DocBlock::parse(&docs.text(), Some(broken_link_callback));
        let module = module_entry(&mut modules1, &context.file_path(range.start.file));
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
    for modules_path in code_directories {
        for entry in walkdir::WalkDir::new(modules_path).into_iter().filter_entry(is_visible) {
            let entry = entry?;
            let path = entry.path();

            if let Some(buf) = read_as_markdown(path)? {
                if Some(path) != index_path.as_ref().map(Path::new) {
                    let module = module_entry(&mut modules1, &path);
                    module.items_wip.push((0, ModuleItem::DocComment(DocComment {
                        kind: CommentKind::Block,
                        target: DocTarget::EnclosingItem,
                        text: buf,
                    })));
                }
            }
        }
    }

    // Incorporate the index file if requested.
    let mut index_docs = None;
    if let Some(index_path) = index_path {
        let buf = read_as_markdown(index_path.as_ref())?.expect("file for --index must be .md or .txt");
        error_entity_put(index_path.to_owned());
        index_docs = Some(DocBlock::parse_with_title(&buf, Some(broken_link_callback)));
    }

    // collate types which have docs
    let mut count = 0;
    let mut substance_count = 0;
    let mut type_docs = BTreeMap::new();
    objtree.root().recurse(&mut |ty| {
        count += 1;

        let mut parsed_type = ParsedType::default();
        if context.config().dmdoc.use_typepath_names {
            parsed_type.name = ty
                .get()
                .name
                .as_str()
                .into();
        } else {
            parsed_type.name = ty
                .get()
                .vars
                .get("name")
                .and_then(|v| v.value.constant.as_ref())
                .and_then(|c| c.as_str())
                .map(strip_propriety)
                .unwrap_or("")
                .into();
        }

        let mut anything = false;
        let mut substance = false;
        if !ty.docs.is_empty() {
            error_entity_put(ty.path.to_owned());
            let (title, block) = DocBlock::parse_with_title(&ty.docs.text(), Some(broken_link_callback));
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
                // determine if there is a documented parent we can link to
                let mut parent = None;
                let mut next = ty.parent_type();
                while let Some(current) = next {
                    if let Some(entry) = current.vars.get(name) {
                        if !entry.value.docs.is_empty() {
                            parent = Some(current.path[1..].to_owned());
                            break;
                        }
                    }
                    next = current.parent_type();
                }

                error_entity_put(format!("{}/var/{}", ty.path, name));
                let block = DocBlock::parse(&var.value.docs.text(), Some(broken_link_callback));
                // `type` is pulled from the parent if necessary
                let type_ = ty.get_var_declaration(name).map(|decl| VarType {
                    is_static: decl.var_type.flags.is_static(),
                    is_const: decl.var_type.flags.is_const(),
                    is_tmp: decl.var_type.flags.is_tmp(),
                    is_final: decl.var_type.flags.is_final(),
                    is_private: decl.var_type.flags.is_private(),
                    is_protected: decl.var_type.flags.is_protected(),
                    path: &decl.var_type.type_path,
                });
                parsed_type.vars.insert(name, Var {
                    docs: block,
                    type_,
                    // but `decl` is only used if it's on this type
                    decl: if var.declaration.is_some() { "var" } else { "" },
                    file: context.file_path(var.value.location.file),
                    line: var.value.location.line,
                    parent,
                });
                anything = true;
                substance = true;
            }
        }

        for (name, proc) in ty.get().procs.iter() {
            // TODO: integrate docs from non-main procs
            let proc_value = proc.main_value();
            if !proc_value.docs.is_empty() {
                // determine if there is a documented parent we can link to
                let mut parent = None;
                let mut next = ty.parent_type();
                while let Some(current) = next {
                    if let Some(entry) = current.procs.get(name) {
                        if !entry.main_value().docs.is_empty() {
                            parent = Some(current.path[1..].to_owned());
                            break;
                        }
                    }
                    next = current.parent_type();
                }

                error_entity_put(format!("{}/proc/{}", ty.path, name));
                let block = DocBlock::parse(&proc_value.docs.text(), Some(broken_link_callback));
                parsed_type.procs.insert(name, Proc {
                    docs: block,
                    params: proc_value.parameters.iter().map(|p| Param {
                        name: p.name.clone(),
                        type_path: format_type_path(&p.var_type.type_path),
                    }).collect(),
                    decl: match proc.declaration {
                        Some(ref decl) => decl.kind.name(),
                        None => "",
                    },
                    file: context.file_path(proc_value.location.file),
                    line: proc_value.location.line,
                    parent,
                });
                anything = true;
                substance = true;
            }
        }

        // file the type under its module as well
        if let Some(ref block) = parsed_type.docs {
            if let Some(module) = modules1.get_mut(&module_path(&context.file_path(ty.location.file))) {
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
            type_docs.insert(ty.get().pretty_path(), parsed_type);
            if substance {
                substance_count += 1;
            }
        }
    });

    // collate all hrefable entities to use in autolinking
    let all_type_names: Arc<BTreeSet<_>> = Arc::new(type_docs.iter()
        .filter(|(_, v)| v.substance)
        .map(|(&t, _)| t.to_owned())
        .collect());

    // finalize modules
    let modules: BTreeMap<_, _> = modules1.into_iter().map(|(key, module1)| {
        let Module1 {
            htmlname,
            orig_filename,
            name,
            teaser,
            mut items_wip,
            defines,
        } = module1;
        let mut module = Module {
            htmlname,
            orig_filename,
            name,
            teaser,
            items: Vec::new(),
            defines,
        };

        let mut docs = DocCollection::default();
        let mut _first = true;
        macro_rules! push_docs { () => {  // oof
            if !docs.is_empty() {
                let doc = std::mem::replace(&mut docs, Default::default());
                if _first {
                    _first = false;
                    let (title, block) = DocBlock::parse_with_title(&doc.text(), Some(broken_link_callback));
                    module.name = title;
                    module.teaser = block.teaser().to_owned();
                    module.items.push(ModuleItem::Docs(block.html));
                } else {
                    module.items.push(ModuleItem::Docs(markdown::render(&doc.text(), Some(broken_link_callback))));
                }
            }
        }}

        error_entity_put(module.orig_filename.to_owned());
        let mut last_line = 0;
        items_wip.sort_by_key(|&(line, _)| line);
        for (line, item) in items_wip.drain(..) {
            match item {
                ModuleItem::DocComment(doc) => {
                    if line > last_line + 1 {
                        docs.push(DocComment::new(CommentKind::Line, DocTarget::EnclosingItem));
                    }
                    docs.push(doc);
                    last_line = line;
                },
                other => {
                    push_docs!();
                    module.items.push(other);
                }
            }
        }
        push_docs!();
        (key, module)
    }).collect();

    print!("documenting {} modules, ", modules.len());
    if macros_all == 0 {
        print!("0 macros, ");
    } else {
        print!(
            "{}/{} macros ({}%), ",
            macro_count,
            macros_all,
            (macro_count * 1000 / macros_all) as f32 / 10.
        );
    }
    if count == 0 {
        println!("0 types");
    } else {
        println!(
            "{}/{}/{} types ({}%)",
            substance_count,
            type_docs.len(),
            count,
            (type_docs.len() * 1000 / count) as f32 / 10.
        );
    }

    {
        // Ensure the diagnostic count is not increased after this point.
        let exit_code = diagnostic_count.into_inner();
        if dry_run {
            std::process::exit(exit_code);
        }
    }

    // load tera templates
    println!("loading templates");
    let mut tera = template::builtin()?;

    // register tera extensions
    let linkify_typenames = all_type_names.clone();
    tera.register_filter("linkify_type", move |value: &Value, _: &HashMap<String, Value>| {
        match *value {
            tera::Value::String(ref s) => Ok(linkify_type(&linkify_typenames, s.split("/").skip_while(|b| b.is_empty())).into()),
            tera::Value::Array(ref a) => Ok(linkify_type(&linkify_typenames, a.iter().filter_map(|v| v.as_str())).into()),
            _ => Err("linkify_type() input must be string".into()),
        }
    });
    tera.register_filter("length", |value: &Value, _: &HashMap<String, Value>| {
        match *value {
            tera::Value::String(ref s) => Ok(s.len().into()),
            tera::Value::Array(ref a) => Ok(a.len().into()),
            tera::Value::Object(ref o) => Ok(o.len().into()),
            _ => Ok(0.into()),
        }
    });
    tera.register_filter("substring", |value: &Value, opts: &HashMap<String, Value>| {
        match *value {
            tera::Value::String(ref s) => {
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
        }
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
    let mut git = Default::default();
    if let Err(e) = git_info(&mut git) {
        println!("incomplete git info: {}", e);
    }
    let env = &Environment {
        dmdoc: DmDoc {
            version: env!("CARGO_PKG_VERSION"),
            url: env!("CARGO_PKG_HOMEPAGE"),
            build_info: BUILD_INFO,
        },
        filename: &env_filename,
        world_name,
        title,
        coverage: Coverage {
            modules: modules.len(),
            macros_documented: macro_count,
            macros_all,
            types_detailed: substance_count,
            types_documented: type_docs.len(),
            types_all: count,
        },
        git,
    };

    println!("rendering html");
    {
        #[derive(Serialize)]
        struct Index<'a> {
            env: &'a Environment<'a>,
            html: Option<&'a str>,
            modules: Vec<IndexTree<'a>>,
            types: Vec<IndexTree<'a>>,
        }

        let mut index = create(&output_path.join("index.html"))?;
        index.write_all(tera.render("dm_index.html", &tera::Context::from_serialize(Index {
            env,
            html: index_docs.as_ref().map(|(_, docs)| &docs.html[..]),
            modules: build_index_tree(modules.iter().map(|(_path, module)| IndexTree {
                htmlname: &module.htmlname,
                full_name: &module.htmlname,
                self_name: match module.name {
                    None => last_element(&module.htmlname),
                    Some(ref t) => t.as_str(),
                },
                teaser: &module.teaser,
                no_substance: false,
                children: Vec::new(),
            })),
            types: build_index_tree(type_docs.iter().map(|(path, ty)| IndexTree {
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
        })?)?.as_bytes())?;
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

        let mut base = String::new();
        for _ in fname.chars().filter(|&x| x == '/') {
            base.push_str("../");
        }

        let mut f = create(&output_path.join(&fname))?;
        f.write_all(tera.render("dm_module.html", &tera::Context::from_serialize(ModuleArgs {
            env,
            base_href: &base,
            path,
            details,
        })?)?.as_bytes())?;
    }

    for (path, details) in type_docs.iter() {
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

        let mut base = String::new();
        for _ in fname.chars().filter(|&x| x == '/') {
            base.push_str("../");
        }

        let mut f = create(&output_path.join(&fname))?;
        f.write_all(tera.render("dm_type.html", &tera::Context::from_serialize(Type {
            env,
            base_href: &base,
            path,
            details,
            types: &type_docs,
        })?)?.as_bytes())?;
    }

    Ok(())
}

// ----------------------------------------------------------------------------
// Helpers

fn module_path(path: &Path) -> String {
    let mut path = path.with_extension("");
    if path.file_name().map_or(false, |x| x.to_string_lossy().eq_ignore_ascii_case("README")) {
        path.pop();
    }
    path.display().to_string().replace("\\", "/")
}

fn module_entry<'a, 'b>(modules: &'a mut BTreeMap<String, Module1<'b>>, path: &Path) -> &'a mut Module1<'b> {
    modules.entry(module_path(path)).or_insert_with(|| {
        let mut module = Module1::default();
        module.htmlname = module_path(path);
        module.orig_filename = path.display().to_string().replace("\\", "/");
        module
    })
}

fn is_visible(entry: &walkdir::DirEntry) -> bool {
    entry.file_name()
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

fn linkify_type<'a, I: Iterator<Item=&'a str>>(all_type_names: &BTreeSet<String>, iter: I) -> String {
    let mut output = String::new();
    let mut all_progress = String::new();
    let mut progress = String::new();
    for bit in iter {
        all_progress.push_str("/");
        all_progress.push_str(bit);
        progress.push_str("/");
        progress.push_str(bit);
        if all_type_names.contains(&all_progress) {
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
            git.web_url = format!("https://{}/{}", &url[at + 1..colon], &url[colon + 1..]);
        } else {
            println!("incomplete git info: weird SSH path: {}", url);
        }
    }
    Ok(())
}

fn read_as_markdown(path: &Path) -> std::io::Result<Option<String>> {
    use std::io::Read;

    let ext = path.extension();
    let is_md = ext == Some("md".as_ref());
    let is_txt = ext == Some("txt".as_ref());
    if is_md || is_txt {
        let mut buf = String::new();
        if is_txt {
            buf.push_str("```\n");
        }
        File::open(path)?.read_to_string(&mut buf)?;
        if is_txt {
            buf.push_str("```");
        }
        Ok(Some(buf))
    } else {
        Ok(None)
    }
}

fn strip_propriety(name: &str) -> &str {
    name
        .trim_start_matches("\\proper")
        .trim_start_matches("\\improper")
        .trim_start()
}

// ----------------------------------------------------------------------------
// Tree stuff

#[derive(Serialize)]
struct IndexTree<'a> {
    htmlname: &'a str,  // href="{{htmlname}}.html"
    full_name: &'a str,
    self_name: &'a str,
    teaser: &'a str,
    no_substance: bool,
    children: Vec<IndexTree<'a>>,
}

fn build_index_tree<'a, I>(iter: I) -> Vec<IndexTree<'a>>
where
    I: IntoIterator<Item=IndexTree<'a>>,
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
// Pre-templating helper structs

#[derive(Default)]
struct TypeHasDocs<'a> {
    var_docs: BTreeSet<&'a str>,
    proc_docs: BTreeSet<&'a str>,
}

/// In-construction Module step 1.
#[derive(Default)]
struct Module1<'a> {
    htmlname: String,
    orig_filename: String,
    name: Option<String>,
    teaser: String,
    items_wip: Vec<(u32, ModuleItem<'a>)>,
    defines: BTreeMap<&'a str, Define<'a>>,
}

// ----------------------------------------------------------------------------
// Templating structs

#[derive(Serialize)]
struct Environment<'a> {
    dmdoc: DmDoc,
    filename: &'a str,
    world_name: &'a str,
    title: &'a str,
    coverage: Coverage,
    git: Git,
}

#[derive(Serialize)]
struct DmDoc {
    version: &'static str,
    url: &'static str,
    build_info: &'static str,
}

#[derive(Serialize)]
struct Coverage {
    modules: usize,
    macros_documented: usize,
    macros_all: usize,
    types_detailed: usize,
    types_documented: usize,
    types_all: usize,
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
    #[serde(rename="type")]
    type_: Option<VarType<'a>>,
    file: PathBuf,
    line: u32,
    parent: Option<String>,
}

#[derive(Serialize)]
struct VarType<'a> {
    is_static: bool,
    is_const: bool,
    is_tmp: bool,
    is_final: bool,
    is_private: bool,
    is_protected: bool,
    path: &'a [String],
}

#[derive(Serialize)]
struct Proc {
    docs: DocBlock,
    decl: &'static str,
    params: Vec<Param>,
    file: PathBuf,
    line: u32,
    parent: Option<String>,
}

#[derive(Serialize)]
struct Param {
    name: String,
    type_path: String,
}

/// Module struct exposed to templates.
#[derive(Default, Serialize)]
struct Module<'a> {
    htmlname: String,
    orig_filename: String,
    name: Option<String>,
    teaser: String,
    items: Vec<ModuleItem<'a>>,
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
        substance: bool,
    },
}
