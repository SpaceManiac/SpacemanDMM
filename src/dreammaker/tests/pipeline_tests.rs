extern crate dreammaker as dm;

use std::io;
use std::path::PathBuf;

use dm::*;

const TEST_FILE: &str = "D:/projects/tgstation/tgstation.dme";

fn open_test_file(context: &Context) -> preprocessor::Preprocessor {
    preprocessor::Preprocessor::new(context, PathBuf::from(TEST_FILE))
        .expect("failed to open test file")
}

#[test]
fn check_preprocessor() {
    let context = Context::default();
    let mut preprocessor = open_test_file(&context);

    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    pretty_print(&mut stdout, preprocessor.by_ref().map(|t| t.token), true).unwrap();
    assert!(context.print_all_errors());
}

#[test]
fn check_indentor() {
    let context = Context::default();
    let mut preprocessor = open_test_file(&context);

    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    pretty_print(&mut stdout, indents::IndentProcessor::new(&context, &mut preprocessor).map(|t| t.token), true).unwrap();
    assert!(context.print_all_errors());
}

#[test]
fn check_parser() {
    let context = Context::default();
    let mut preprocessor = open_test_file(&context);
    let _tree = parser::parse(&context, indents::IndentProcessor::new(&context, &mut preprocessor).map(Ok));
    assert!(context.print_all_errors());

    println!("\n--------\nSuccess!\n--------");
    #[cfg(feature="xml")] {
        _tree.to_xml("objtree.xml".as_ref()).unwrap();
    }
}
