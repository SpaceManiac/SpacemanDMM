use dm::Context;
use std::borrow::Cow;

use crate::{run_inner};

pub const NO_ERRORS: &[(u32, u16, &str)] = &[];

pub fn parse_a_file_for_test<S: Into<Cow<'static, str>>>(buffer: S, config: String) -> Context {
    let context = Context::default();

    context.set_config(config);

    let pp = dm::preprocessor::Preprocessor::from_buffer(&context, "unit_tests.rs".into(), buffer.into());

    let indents = dm::indents::IndentProcessor::new(&context, pp);

    let mut parser = dm::parser::Parser::new(&context, indents);
    parser.enable_procs();
    let tree = parser.parse_object_tree();

    run_inner(&context, &tree, false);

    context
}

pub fn check_errors_match<S: Into<Cow<'static, str>>>(buffer: S, errorlist: &[(u32, u16, &str)]) {
    check_errors_match_with_config(buffer, errorlist, String::new());
}

pub fn check_errors_match_with_config<S: Into<Cow<'static, str>>>(buffer: S, errorlist: &[(u32, u16, &str)], config: String) {
    let context = parse_a_file_for_test(buffer, config);
    let errors = context.errors();
    let mut iter = errors.iter();
    for (line, column, desc) in errorlist {
        let nexterror = iter.next().unwrap();
        if nexterror.location().line != *line
            || nexterror.location().column != *column
            || nexterror.description() != *desc
        {
            panic!(
                "possible feature regression in dreamchecker, expected {}:{}:{}, found {}:{}:{}",
                *line,
                *column,
                *desc,
                nexterror.location().line,
                nexterror.location().column,
                nexterror.description()
            );
        }
    }
    if iter.next().is_some() {
        panic!("found more errors than was expected");
    }
}
