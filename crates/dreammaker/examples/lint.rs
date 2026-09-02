//! A very simple CLI binary which finds a `.dme` file in the current directory
//! and prints all non-hint diagnostics from parsing the object tree and procs.

use dreammaker::{Context, Parser, Preprocessor, Severity};

fn main() {
    let mut context = Context::default();
    context.set_print_severity(Some(Severity::Info));
    let env = context.configure_cli(std::env::args_os().nth(1));
    let pp = context.unwrap(Preprocessor::new(&context, env.clone()));
    let mut parser = Parser::new(&context, pp);
    parser.enable_procs();
    parser.parse_object_tree();
}
