extern crate dreammaker as dm;

fn main() {
    let mut context = dm::Context::default();
    context.set_print_severity(Some(dm::Severity::Info));
    let env = dm::detect_environment_default()
        .expect("error detecting .dme")
        .expect("no .dme found");
    let pp = context.unwrap(dm::Preprocessor::new(&context, env));
    println!(
        "{}",
        dm::pretty_print(dm::IndentProcessor::new(&context, pp), false)
    );
}
