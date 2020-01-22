use dm::Context;
use dm::objtree::Code;
use std::borrow::Cow;

use crate::{AnalyzeObjectTree, check_var_defs};

pub const NO_ERRORS: &[(u32, u16, &str)] = &[];

pub fn parse_a_file_for_test<S: Into<Cow<'static, str>>>(buffer: S) -> Context {

    let context = Context::default();

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

pub fn check_errors_match<S: Into<Cow<'static, str>>>(buffer: S, errorlist: &[(u32, u16, &str)]) {
    let context = parse_a_file_for_test(buffer);
    let errors = context.errors();
    let mut iter = errors.iter();
    for (line, column, desc) in errorlist {
        let nexterror = iter.next().unwrap();
        if nexterror.location().line != *line || nexterror.location().column != *column || nexterror.description() != *desc {
            panic!(format!("possible feature regression in dreamchecker, expected {}:{}:{}, found {}:{}:{}",
                *line, *column, *desc,
                nexterror.location().line, nexterror.location().column, nexterror.description()));
        }
    }
    if iter.next().is_some() {
        panic!("found more errors than was expected");
    }
}
