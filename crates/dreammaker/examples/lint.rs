//! A very simple CLI binary which finds a `.dme` file in the current directory
//! and prints all non-hint diagnostics from parsing the object tree and procs.

extern crate dreammaker as dm;

fn main() {
    let mut context = dm::Context::default();
    context.set_print_severity(Some(dm::Severity::Info));
    let env = dm::detect_environment_default()
        .expect("error detecting .dme")
        .expect("no .dme found");
    let pp = dm::preprocessor::Preprocessor::new(&context, env)
        .expect("i/o error opening .dme");
    let indents = dm::indents::IndentProcessor::new(&context, pp);
    let mut parser = dm::parser::Parser::new(&context, indents);
    parser.enable_procs();
    parser.parse_object_tree();
}
