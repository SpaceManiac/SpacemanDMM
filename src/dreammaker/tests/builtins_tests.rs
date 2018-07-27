extern crate dreammaker as dm;

use dm::objtree::ObjectTree;

#[test]
fn check_builtins() {
    let mut tree = ObjectTree::default();
    tree.register_builtins();
    println!("{:?}", tree);
}
