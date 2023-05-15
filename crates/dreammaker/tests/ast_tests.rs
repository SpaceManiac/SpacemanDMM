extern crate dreammaker as dm;

use dm::*;
use dm::ast::{SyntaxTree, TreeEntryData, Expression, Term};
use dm::preprocessor::Preprocessor;
use dm::objtree::ObjectTree;

fn with_code<F: FnOnce(&Context, SyntaxTree, ObjectTree)>(code: &'static str, f: F) {
    let context = Context::default();
    let path = std::path::PathBuf::from(r"test.dm");
    let pp = Preprocessor::from_buffer(&context, path, code.trim());
    let indents = indents::IndentProcessor::new(&context, pp);
    let mut parser = parser::Parser::new(&context, indents);
    parser.enable_procs();
    let syntax_tree = parser.parse();
    let _tree = syntax_tree.object_tree_without_builtins();

    f(&context, syntax_tree, _tree)
}

#[test]
fn check_semicolon_in_proc_parameters() {
    with_code("
#define DEF1 0x01;
#define DEF2 \"asdf\" as text;

/proc/darn(foo = DEF1, bar = DEF2, anotherarg = 1)
", |context, _, _| {
        let errors = context.errors();
        assert_eq!(errors.len(), 2);

        for error in errors.into_iter() {
            assert_eq!(error.errortype().expect("No errortype set!"), "semicolon_in_proc_parameter");
        }
    });
}

#[test]
fn check_absolut_type_path_decl() {
    with_code("
/datum/config_entry/flag/forbid_all_profiling

/datum
    var/asdf = 4

/datum/config_entry
    asdf = 5
", |_, syn, _|{
        assert_eq!(3, syn.root().entries.len());
        let entry = &syn.root().entries[0];
        assert!(entry.absolute);
        if let TreeEntryData::Decl = entry.data {}
        else {
            panic!("Expected declaration!");
        }
        assert_eq!(4, entry.leading_path.len());
        assert_eq!("datum", entry.leading_path[0]);
        assert_eq!("config_entry", entry.leading_path[1]);
        assert_eq!("flag", entry.leading_path[2]);
        assert_eq!("forbid_all_profiling", entry.leading_path[3]);

        let entry2 = &syn.root().entries[1];
        assert!(entry2.absolute);
        assert_eq!(1, entry2.leading_path.len());
        assert_eq!("datum", entry2.leading_path[0]);
        if let TreeEntryData::Block(block) = &entry2.data {
            assert_eq!(1, block.entries.len());
            let subentry = &block.entries[0];
            assert_eq!(0, subentry.leading_path.len());
            if let TreeEntryData::Var(var, name) = &subentry.data {
                assert_eq!("asdf", name);
                assert!(var.declaration.is_some());
                if let Expression::Base { term, follow } = var.value.expression.as_ref().unwrap() {
                    assert_eq!(0, follow.len());
                    assert!(term.elem.is_truthy().unwrap());
                    if let Term::Int(number) = term.elem {
                        assert_eq!(4, number);
                    } else {
                        panic!("Expected Int!");
                    }
                } else {
                    panic!("Expected expression!");
                }
            } else {
                panic!("Expected var!");
            }
        } else {
            panic!("Expected block!");
        }

        let entry3 = &syn.root().entries[2];
        assert!(entry3.absolute);
        assert_eq!(2, entry3.leading_path.len());
        assert_eq!("datum", entry3.leading_path[0]);
        assert_eq!("config_entry", entry3.leading_path[1]);
        if let TreeEntryData::Block(block) = &entry3.data {
            assert_eq!(1, block.entries.len());
            let subentry = &block.entries[0];
            assert_eq!(0, subentry.leading_path.len());
            if let TreeEntryData::Var(var, name) = &subentry.data {
                assert_eq!("asdf", name);
                assert!(var.declaration.is_none());
                if let Expression::Base { term, follow } = var.value.expression.as_ref().unwrap() {
                    assert_eq!(0, follow.len());
                    assert!(term.elem.is_truthy().unwrap());
                    if let Term::Int(number) = term.elem {
                        assert_eq!(5, number);
                    } else {
                        panic!("Expected Int!");
                    }
                } else {
                    panic!("Expected expression!");
                }
            } else {
                panic!("Expected var!");
            }
        } else {
            panic!("Expected block!");
        }
    });
}

#[test]
fn random_regression_test() {
    with_code("
#define DEFINE_TYPE(_flags) /datum/bitfield/new_type { \
    flags = ##_flags;\
}

DEFINE_TYPE(list())

/datum/bitfield
    var/flags
", |context, syn, _|{
    context.assert_success();
    let entry = &syn.root().entries[1];
    if let TreeEntryData::Block(block) = &entry.data {
        let subentry = &block.entries[0];
        assert_eq!(0, subentry.leading_path.len());
    } else {
        panic!("Expected block!");
    }
});
}
