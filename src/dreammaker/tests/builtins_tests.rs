extern crate dreammaker as dm;

use dm::objtree::ObjectTree;

#[test]
fn check_builtins() {
    println!("{:?}", ObjectTree::with_builtins());
}
