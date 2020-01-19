
extern crate dreammaker as dm;
use dm::Context;
use dm::objtree::Code;
use std::borrow::Cow;

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
"##.trim();
    let context = parse_a_file_for_test(code);
    let errors = context.errors();
    let mut iter = errors.iter();
    let mut nexterror = iter.next().unwrap();
    if nexterror.location().line != 2 || nexterror.location().column != 7 || nexterror.description() != "ambiguous `!` on left side of an `in`" {
        panic!("");
    }
    nexterror = iter.next().unwrap();
    if nexterror.location().line != 6 || nexterror.location().column != 7 || nexterror.description() != "ambiguous `&&` on left side of an `in`" {
        panic!("");
    }
    if iter.next().is_some() {
        panic!("");
    }
}
