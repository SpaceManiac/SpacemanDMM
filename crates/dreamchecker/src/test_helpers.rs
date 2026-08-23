use dm::Context;
use std::borrow::Cow;

use crate::run_inner;

pub fn parse_a_file_for_test<S: Into<Cow<'static, str>>>(buffer: S) -> Context {
    let context = Context::default();
    let pp = dm::Preprocessor::from_buffer(&context, "unit_tests.rs".into(), buffer.into());
    let mut parser = dm::Parser::new(&context, pp);
    parser.enable_procs();
    let tree = parser.parse_object_tree();

    run_inner(&context, &tree, false);

    context
}

pub fn check_errors_match<S: Into<Cow<'static, str>>>(buffer: S, errorlist: &[(u32, u16, &str)]) {
    let context = parse_a_file_for_test(buffer);
    let errors = context.errors();
    let mut iter = errors.iter();
    for &(line, column, desc) in errorlist {
        let nexterror_option = iter.next();
        match nexterror_option {
            Some(nexterror) => {
                if nexterror.location().line != line
                    || nexterror.location().column != column
                    || nexterror.description() != desc
                {
                    panic!(
                        "possible feature regression in dreamchecker, expected:\n- {}:{}:{}\nfound:\n- {}:{}:{}",
                        line,
                        column,
                        desc,
                        nexterror.location().line,
                        nexterror.location().column,
                        nexterror.description()
                    );
                }
            },
            None => {
                panic!(
                    "possible feature regression in dreamchecker, expected:\n- {}:{}:{}\nfound no additional errors!",
                    line, column, desc
                );
            },
        }
    }
    if let Some(error) = iter.next() {
        let error_loc = error.location();
        panic!(
            "found more errors than was expected:\n- {}:{}:{}",
            error_loc.line,
            error_loc.column,
            error.description()
        );
    }
}
