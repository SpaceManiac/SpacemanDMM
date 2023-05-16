extern crate dreammaker as dm;

use dm::{objtree::ObjectTree, Context, preprocessor::Preprocessor, indents, ast::{SyntaxTree, TreeEntryData, Expression, Term}, parser, incremental_reparse, Location, FileId};
use interval_tree::{range, RangeInclusive};

fn with_reparse<
    F1: FnOnce(&Context, &SyntaxTree, ObjectTree, FileId) -> RangeInclusive<Location>,
    F2: FnOnce(&Context, SyntaxTree, ObjectTree)>(
        code: &'static str,
        f1: F1,
        code2: &'static str,
        f2: F2) {
    let context = Context::default();
    let path = std::path::PathBuf::from(r"test.dm");
    let mut pp = Preprocessor::from_buffer(&context, path, code.trim());
    let indents = indents::IndentProcessor::new(&context, &mut pp);
    let mut parser = parser::Parser::new(&context, indents);
    parser.enable_procs();
    let mut syntax_tree = parser.parse();
    syntax_tree.with_define_history(pp.finalize());
    let mut objtree = syntax_tree.object_tree_without_builtins();

    let range = f1(&context, &syntax_tree, objtree, context.get_file(std::path::PathBuf::from("test.dm").as_path()).unwrap());

    let result = incremental_reparse(&context, syntax_tree, range, code2, None);

    assert!(result.is_ok());
    let (parser_error, syntax_tree_2) = result.unwrap();
    assert!(!parser_error);

    objtree = syntax_tree_2.object_tree_without_builtins();

    f2(&context, syntax_tree_2, objtree);
}


#[test]
fn test_basic_reparse() {
    with_reparse("
/datum/one

/datum/two

/datum/three
".trim(), |context: &Context, _, _, file|{
    context.assert_success();

    let start_range = Location {
        file,
        line: 3,
        column: 1
    };
    let end_range = Location {
        file,
        line: 3,
        column: 11,
    };

    range(start_range, end_range)
},
"/datum/one

/datum/four // <- see what I did there?

/datum/three
".trim(), |context, syn, _| {
    context.assert_success();
    let root_entries = &syn.root().entries;
    assert_eq!(3, root_entries.len());
    let new_entry = &root_entries[1];

    assert!(new_entry.absolute);
    assert_eq!(2, new_entry.leading_path.len());
    assert_eq!("datum", new_entry.leading_path[0]);
    assert_eq!("four", new_entry.leading_path[1]);

    if let TreeEntryData::Decl = new_entry.data {}
    else {
        panic!("Expected declaration!");
    }
    });
}

#[test]
fn test_macro_redefine(){
    with_reparse("
#define SOME_MACRO 47

var/test = SOME_MACRO
".trim(), |context, _, _, file|{
    context.assert_success();

    range(Location {
        file,
        line: 1,
        column: 5,
    }, Location {
        file,
        line: 2,
        column: 1 })
}, "
#define SOME_MACRO 25

var/test = SOME_MACRO
".trim(), |context, syn, _|{
    context.assert_success();
    assert_eq!(1, syn.root().entries.len());
    let entry = &syn.root().entries[0];
    assert!(entry.absolute);

    if let TreeEntryData::Var(var, name) = &entry.data {
        assert_eq!("test", name);
        if let Expression::Base { term, follow } = var.value.expression.as_ref().unwrap() {
            assert_eq!(0, follow.len());
            assert!(term.elem.is_truthy().unwrap());
            if let Term::Int(number) = term.elem {
                assert_eq!(25, number);
            } else {
                panic!("Expected Int!");
            }
        } else {
            panic!("Expected expression!");
        }
    } else {
        panic!("Expected var!");
    }
});
}

#[test]
fn test_noop_macro_redefine(){
    with_reparse("
#define SOME_MACRO 47

var/test = SOME_MACRO
".trim(), |context, _, _, file|{
    context.assert_success();

    range(Location {
        file,
        line: 1,
        column: 5,
    }, Location {
        file,
        line: 2,
        column: 1 })
}, "
#define SOME_MACRO 25

#undef SOME_MACRO

#define SOME_MACRO 24
#define SOME_MACRO 47

var/test = SOME_MACRO
".trim(), |_, syn, _|{
    // macro redef warning
    // context.assert_success();
    assert_eq!(1, syn.root().entries.len());
    let entry = &syn.root().entries[0];

    if let TreeEntryData::Var(var, name) = &entry.data {
        assert_eq!("test", name);
        if let Expression::Base { term, follow } = var.value.expression.as_ref().unwrap() {
            assert_eq!(0, follow.len());
            assert!(term.elem.is_truthy().unwrap());
            if let Term::Int(number) = term.elem {
                assert_eq!(47, number);
            } else {
                panic!("Expected Int!");
            }
        } else {
            panic!("Expected expression!");
        }
    } else {
        panic!("Expected var!");
    }
});
}
