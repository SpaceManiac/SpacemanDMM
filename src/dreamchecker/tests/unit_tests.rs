
extern crate dreammaker as dm;
use dm::Context;
use dm::objtree::Code;
use std::borrow::Cow;
use std::slice::Iter;

use dreamchecker::*;

pub fn parse_a_file_for_test<S: Into<Cow<'static, str>>>(buffer: S) -> Context {

    let context = Context::unit_test_default();

    let pp = dm::preprocessor::Preprocessor::from_buffer(&context, "unit_tests.rs".into(), buffer);

    let indents = dm::indents::IndentProcessor::new(&context, pp);

    let mut parser = dm::parser::Parser::new(&context, indents);
    parser.enable_procs();
    let tree = parser.parse_object_tree();

    check_var_defs(&tree, &context);

    let mut analyzer = AnalyzeObjectTree::new(&context, &tree);

    tree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            if let Code::Present(ref code) = proc.get().code {
                analyzer.gather_settings(proc, code);
            }
        }
    });

    tree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            match proc.get().code {
                Code::Present(ref code) => {
                    analyzer.check_proc(proc, code);
                }
                Code::Invalid(_) => {},
                Code::Builtin => {},
                Code::Disabled => panic!("proc parsing was enabled, but also disabled. this is a bug"),
            }
        }
    });

    tree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            analyzer.check_kwargs(proc);
        }
    });
    analyzer.finish_check_kwargs();

    context
}

pub fn check_errors_match(mut iter: Iter<'_, dm::DMError>, errorlist: &[(u32, u16, &str)]) {
    for (line, column, desc) in errorlist {
        let nexterror = iter.next().unwrap();
        if nexterror.location().line != *line || nexterror.location().column != *column || nexterror.description() != *desc {
            panic!("possible feature regression in dreamchecker");
        }
    }
    if iter.next().is_some() {
        panic!("found more errors than was expected");
    }
}

pub const IN_AMBIG_ERRORS: &[(u32, u16, &str)] = &[
    (2, 7, "ambiguous `!` on left side of an `in`"),
    (6, 7, "ambiguous `&&` on left side of an `in`"),
    (11, 7, "ambiguous `=` on left side of an `in`"),
    // TODO: Fix this, https://github.com/SpaceManiac/SpacemanDMM/issues/122
    //(13, 7, "ambiguous ternary on left side of an `in`"),
];

#[test]
fn in_ambig() {
    let code = r##"
/proc/test()
    if(!1 in list())
        return
    if(!(1 in list()))
        return
    if(1 && 1 in list())
        return
    if(1 && (1 in list()))
        return
    var/i
    if(i = 1 in list())
        return
    if(i = (1 in list()))
        return
    if(i ? 1 : 2 in list())
        return
    if((i ? 1 : 2) in list())
        return
"##.trim();
    let context = parse_a_file_for_test(code);
    let errors = context.errors();
    let iter = errors.iter();
    check_errors_match(iter, IN_AMBIG_ERRORS);
}

pub const OP_OVERLOAD_ERRORS: &[(u32, u16, &str)] = &[
    (6, 5, "Attempting ++ on a /mob which does not overload operator++"),
];

#[test]
fn operator_overload() {
    let code = r##"
/mob/test/operator++()
    return

/proc/test()
    var/mob/M = new
    M++
    var/mob/test/T = new
    T++
"##.trim();
    let context = parse_a_file_for_test(code);
    let errors = context.errors();
    let iter = errors.iter();
    check_errors_match(iter, OP_OVERLOAD_ERRORS);
}
