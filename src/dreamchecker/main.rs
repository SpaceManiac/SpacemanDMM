//! DreamChecker, a robust static analysis and typechecking engine for
//! DreamMaker.
#![allow(dead_code, unused_variables)]

extern crate dreammaker as dm;
extern crate dreamchecker;

use dm::Context;
use dm::objtree::Code;

use dreamchecker::*;

// ----------------------------------------------------------------------------
// Command-line interface

fn main() {
    const PRINT_SEVERITY: dm::Severity = dm::Severity::Info;

    let mut context = Context::default();
    context.set_print_severity(Some(PRINT_SEVERITY));
    let dme = dm::detect_environment_default()
        .expect("error detecting .dme")
        .expect("no .dme found");
    println!("============================================================");
    println!("Parsing {}...\n", dme.display());
    let pp = dm::preprocessor::Preprocessor::new(&context, dme)
        .expect("i/o error opening .dme");
    let indents = dm::indents::IndentProcessor::new(&context, pp);
    let mut parser = dm::parser::Parser::new(&context, indents);
    parser.enable_procs();
    let tree = parser.parse_object_tree();

    let mut present = 0;
    let mut invalid = 0;
    let mut builtin = 0;

    let mut env = AnalyzeObjectTree::default();

    // First pass: analyze all proc bodies
    println!("============================================================");
    println!("Analyzing proc bodies...\n");
    tree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            match proc.code {
                Code::Present(ref code) => {
                    present += 1;
                    AnalyzeProc::new(&mut env, &context, &tree, proc).run(code);
                }
                Code::Invalid(_) => invalid += 1,
                Code::Builtin => builtin += 1,
                Code::Disabled => panic!("proc parsing was enabled, but also disabled. this is a bug"),
            }
        }
    });

    println!("Procs analyzed: {}. Errored: {}. Builtins: {}.\n", present, invalid, builtin);

    // Second pass: warn about procs which are missing kwargs their parents have
    println!("============================================================");
    println!("Analyzing proc override validity...\n");
    tree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            env.check_kwargs(&context, proc);
        }
    });
    env.finish_check_kwargs(&context);

    println!("============================================================");
    let errors = context.errors().iter().filter(|each| each.severity() <= PRINT_SEVERITY).count();
    println!("Found {} diagnostics", errors);
    std::process::exit(if errors > 0 { 1 } else { 0 });
}
