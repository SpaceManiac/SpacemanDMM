extern crate dreammaker as dm;

use std::path::PathBuf;

use dm::{Context, Parser, Preprocessor};

fn with_test_dme<F: FnOnce(Preprocessor)>(context: &Context, f: F) {
    let dme = match std::env::var_os("TEST_DME") {
        Some(dme) => dme,
        None => {
            println!("Set TEST_DME to check full pipeline");
            return;
        },
    };
    f(Preprocessor::new(context, PathBuf::from(dme)).expect("failed to open test file"))
}

#[test]
fn check_preprocessor() {
    let context = Context::default();
    with_test_dme(&context, |preprocessor| {
        preprocessor.count();
        context.assert_success();
    });
}

#[test]
fn check_indentor() {
    let context = Context::default();
    with_test_dme(&context, |preprocessor| {
        dm::IndentProcessor::new(&context, preprocessor).count();
        context.assert_success();
    });
}

#[test]
fn check_parser() {
    let context = Context::default();
    with_test_dme(&context, |mut preprocessor| {
        let mut parser = Parser::new(&context, &mut preprocessor);
        parser.enable_procs();
        let _tree = parser.parse_object_tree();
        context.assert_success();

        println!("\n--------\nSuccess!\n--------");
    });
}
