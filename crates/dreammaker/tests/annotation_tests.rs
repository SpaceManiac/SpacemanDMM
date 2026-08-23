extern crate dreammaker as dm;

use dm::annotation::AnnotationTree;
use dm::{FileId, Lexer, Location, Parser};

#[test]
fn annotation_basic() {
    let code = r#"
/var/foo = bar
/datum/globals
    var/number = 7 + 5
    var/string = foo("Hello [ "world" ]")

    var/baz
    baz = "neat"

    proc/Init()
        world.log << new/obj()
"#
    .trim();

    let context = Default::default();
    let lexer = Lexer::new(&context, FileId::INVALID, code.as_bytes());
    let mut annotations = AnnotationTree::default();
    Parser::new(&context, lexer).parse_annotations_only(&mut annotations);
    context.assert_success();
    println!("len: {}", annotations.len());
    for each in annotations.get_location(Location {
        file: FileId::INVALID,
        line: 9,
        column: 14,
    }) {
        println!("{each:?}");
        for each in annotations.get_range_raw(each.0) {
            println!("    {:?}", each.1);
        }
    }
}
