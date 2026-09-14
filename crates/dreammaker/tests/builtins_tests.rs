extern crate dreammaker as dm;

use dm::{objtree::ObjectTree, preprocessor::DefineMap};

#[test]
fn check_builtin_types() {
    let ot = ObjectTree::with_builtins();
    assert!(ot.iter_types().count() >= 28);
    for each in ot.iter_types() {
        eprintln!("{:?}", each);
    }
}

#[test]
fn check_builtin_macros() {
    let ot = DefineMap::with_builtins();
    assert!(ot.len() >= 100);
}
