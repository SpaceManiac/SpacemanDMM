//! DreamChecker, a robust static analysis and typechecking engine for
//! DreamMaker.

extern crate dreammaker as dm;
extern crate dreamchecker;

use dm::Context;
use dm::objtree::Code;

use dreamchecker::*;
use dm::config::*;

// ----------------------------------------------------------------------------
// Command-line interface

fn main() {
    // command-line args
    let mut environment = None;
    let mut config_file = None;

    let mut args = std::env::args();
    let _ = args.next();  // skip executable name
    while let Some(arg) = args.next() {
        if arg == "-V" || arg == "--version" {
            println!(
                "dreamchecker {}  Copyright (C) 2017-2019  Tad Hardesty",
                env!("CARGO_PKG_VERSION")
            );
            println!("{}", include_str!(concat!(env!("OUT_DIR"), "/build-info.txt")));
            println!("This program comes with ABSOLUTELY NO WARRANTY. This is free software,");
            println!("and you are welcome to redistribute it under the conditions of the GNU");
            println!("General Public License version 3.");
            return;
        } else if arg == "-e" {
            environment = Some(args.next().expect("must specify a value for -e"));
        } else if arg == "-c" {
            config_file = Some(args.next().expect("must specify a file for -c"));
        } else {
            eprintln!("unknown argument: {}", arg);
            return;
        }
    }

    let dme = environment
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| dm::detect_environment_default()
            .expect("error detecting .dme")
            .expect("no .dme found"));

    const PRINT_SEVERITY: dm::Severity = dm::Severity::Info;

    let mut context = Context::default();
    if let Some(filepath) = config_file {
        let config = read_config_toml(filepath);
        context.register_filter(config.warnings);
    }
    context.set_print_severity(Some(PRINT_SEVERITY));
    println!("============================================================");
    println!("Parsing {}...\n", dme.display());
    let pp = dm::preprocessor::Preprocessor::new(&context, dme)
        .expect("i/o error opening .dme");
    let indents = dm::indents::IndentProcessor::new(&context, pp);
    let mut parser = dm::parser::Parser::new(&context, indents);
    parser.enable_procs();
    let tree = parser.parse_object_tree();

    check_var_defs(&tree, &context);

    let mut present = 0;
    let mut invalid = 0;
    let mut builtin = 0;

    let mut analyzer = AnalyzeObjectTree::new(&context, &tree);

    println!("============================================================");
    println!("Gathering proc settings...\n");
    tree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            if let Code::Present(ref code) = proc.get().code {
                analyzer.gather_settings(proc, code);
            }
        }
    });

    println!("============================================================");
    println!("Analyzing proc bodies...\n");
    tree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            match proc.get().code {
                Code::Present(ref code) => {
                    present += 1;
                    analyzer.check_proc(proc, code);
                }
                Code::Invalid(_) => invalid += 1,
                Code::Builtin => builtin += 1,
                Code::Disabled => panic!("proc parsing was enabled, but also disabled. this is a bug"),
            }
        }
    });

    println!("Procs analyzed: {}. Errored: {}. Builtins: {}.\n", present, invalid, builtin);

    println!("============================================================");
    println!("Analyzing proc override validity...\n");
    tree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            analyzer.check_kwargs(proc);
        }
    });
    analyzer.finish_check_kwargs();

    println!("============================================================");
    let errors = context.errors().iter().filter(|each| each.severity() <= PRINT_SEVERITY).count();
    println!("Found {} diagnostics", errors);
    std::process::exit(if errors > 0 { 1 } else { 0 });
}
