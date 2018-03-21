extern crate dreammaker as dm;

use dm::objtree::ObjectTree;

#[test]
fn check_builtins() {
    let _ = ObjectTree::default();
    println!("{:?}", ObjectTree::with_builtins());
}
