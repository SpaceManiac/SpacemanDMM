extern crate dreammaker as dm;

use std::path::PathBuf;

use dm::*;
use dm::preprocessor::Preprocessor;

fn with_test_dme<F: FnOnce(Preprocessor)>(context: &Context, f: F) {
    let dme = match std::env::var_os("TEST_DME") {
        Some(dme) => dme,
        None => {
            println!("Set TEST_DME to check full pipeline");
            return;
        }
    };
    f(Preprocessor::new(context, PathBuf::from(dme)).expect("failed to open test file"))
}

#[test]
fn check_preprocessor() {
    let context = Context::default();
    with_test_dme(&context, |mut preprocessor| {
        let mut string = Vec::new();
        pretty_print(&mut string, preprocessor.by_ref().map(|t| t.token), true).unwrap();
        context.assert_success();
    });
}

#[test]
fn check_indentor() {
    let context = Context::default();
    with_test_dme(&context, |mut preprocessor| {
        let mut string = Vec::new();
        pretty_print(
            &mut string,
            indents::IndentProcessor::new(&context, &mut preprocessor).map(|t| t.token),
            true,
        ).unwrap();
        context.assert_success();
    });
}

#[test]
fn check_parser() {
    let context = Context::default();
    with_test_dme(&context, |mut preprocessor| {
        let _tree = parser::parse(&context, indents::IndentProcessor::new(&context, &mut preprocessor));
        context.assert_success();

        println!("\n--------\nSuccess!\n--------");
    });
}
