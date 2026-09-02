//! A very simple CLI binary which finds a `.dme` file in the current directory
//! and prints all non-hint diagnostics from parsing the object tree and procs.

use std::path::PathBuf;

use dreammaker::{Context, Parser, Preprocessor, Severity, detect_environment_default};

fn main() {
    let env = std::env::args_os().nth(1).map_or_else(
        || {
            detect_environment_default()
                .expect("error detecting .dme")
                .expect("no .dme found")
        },
        PathBuf::from,
    );

    let mut context = Context::default();
    context.set_print_severity(Some(Severity::Info));
    let pp = context.unwrap(Preprocessor::new(&context, env.clone()));
    let mut parser = Parser::new(&context, pp);
    parser.enable_procs();
    parser.parse_object_tree();
}
