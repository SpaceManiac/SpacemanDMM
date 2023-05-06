use dm::Context;
use std::borrow::Cow;

use crate::{run_inner};

pub const NO_ERRORS: &[(u32, u16, &str)] = &[];

pub fn parse_a_file_for_test<S: Into<Cow<'static, str>>>(buffer: S) -> Context {
    let context = Context::default();

    let pp = dm::preprocessor::Preprocessor::from_buffer(&context, "unit_tests.rs".into(), buffer.into());

    let indents = dm::indents::IndentProcessor::new(&context, pp);

    let mut parser = dm::parser::Parser::new(&context, indents);
    parser.enable_procs();
    let tree = parser.parse_object_tree();

    run_inner(&context, &tree, false);

    context
}

pub fn check_errors_match<S: Into<Cow<'static, str>>>(buffer: S, errorlist: &[(u32, u16, &str)]) {
    let context = parse_a_file_for_test(buffer);
    let errors = context.errors();
    let mut iter = errors.iter();
    for (line, column, desc) in errorlist {
        let nexterror_option = iter.next();
        match nexterror_option {
            Some(nexterror) => {
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
            },
            None => {
                panic!(
                    "possible feature regression in dreamchecker, expected {}:{}:{}, found no additional errors!",
                    *line,
                    *column,
                    *desc
                );
            }
        }
    }
    if let Some(error) = iter.next() {
        let error_loc = error.location();
        panic!("found more errors than was expected: {}:{}:{}", error_loc.line, error_loc.column, error.description());
    }
}
