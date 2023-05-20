extern crate dreammaker as dm;

use dm::{objtree::ObjectTree, Context, preprocessor::{Preprocessor, FileProvider}, indents, ast::{SyntaxTree, TreeEntryData, Expression, Term, SyntaxEq}, parser, incremental_reparse, Location, FileId, Component};
use interval_tree::{range, RangeInclusive};

struct BufferedFileProvider {
    code: &'static str,
}

const TEST_FILE_PATH: &str = "test.dme";

impl FileProvider for BufferedFileProvider {
    fn open_file(&mut self, _: &std::path::Path) -> Result<Box<dyn std::io::Read>, std::io::Error> {
        Ok(Box::new(self.code.as_bytes()))
    }
}

fn with_reparse<
    F1: FnOnce(&Context, &SyntaxTree, Option<ObjectTree>, FileId) -> RangeInclusive<Location>,
    F2: FnOnce(&Context, SyntaxTree, Option<ObjectTree>, FileId)>(
        code: &'static str,
        f1: F1,
        mut code2: &'static str,
        f2: F2) {
    let context = Context::default();
    let path = std::path::PathBuf::from(TEST_FILE_PATH);
    let mut pp = Preprocessor::from_buffer(&context, path, code.trim());
    let indents = indents::IndentProcessor::new(&context, &mut pp);
    let mut parser = parser::Parser::new(&context, indents);
    parser.enable_procs();
    let mut syntax_tree = parser.parse();
    syntax_tree.with_define_history(pp.finalize());
    let objtree = syntax_tree.object_tree_without_builtins();

    let file = context.get_file(std::path::PathBuf::from(TEST_FILE_PATH).as_path()).unwrap();
    let range = f1(&context, &syntax_tree, Some(objtree), file);

    context.clear_errors(Some(&range), Some(Component::Parser));
    context.clear_errors(None, Some(Component::ObjectTree));
    let original_syntax_tree = syntax_tree.root().clone();

    code2 = code2.trim();
    let mut file_provider = BufferedFileProvider{
        code: code2,
    };

    let result = incremental_reparse(
        &context,
        syntax_tree,
        range,
        code2,
        &mut file_provider,
        None);

    let (parser_error, syntax_tree_2) = result;


    let obj = if parser_error {
        // parser errors should not update the previous syntax tree
        assert!(original_syntax_tree.syntax_eq(syntax_tree_2.root()));
        None
    } else {
        // for everything we reparse successfully, we want to fully parse it and syntax compare it as well to assert there are no differences with the updated syntax tree
        let context2 = Context::default();
        let path2 = std::path::PathBuf::from(TEST_FILE_PATH);
        let mut pp2 = Preprocessor::from_buffer(&context2, path2, code2.trim());
        let indents2 = indents::IndentProcessor::new(&context2, &mut pp2);
        let mut parser2 = parser::Parser::new(&context2, indents2);
        parser2.enable_procs();
        let syntax_tree_2_full = parser2.parse();

        assert!(syntax_tree_2.syntax_eq(&syntax_tree_2_full));
        Some(syntax_tree_2.object_tree_without_builtins())
    };

    f2(&context, syntax_tree_2, obj, file);
}

fn with_double_reparse<
    F1: FnOnce(&Context, &SyntaxTree, Option<ObjectTree>, FileId) -> RangeInclusive<Location>,
    F2: FnOnce(&Context, &SyntaxTree, Option<ObjectTree>, FileId) -> RangeInclusive<Location>,
    F3: FnOnce(&Context, SyntaxTree, Option<ObjectTree>, FileId)>(
        code: &'static str,
        f1: F1,
        code2: &'static str,
        f2: F2,
        mut code3: &'static str,
        f3: F3) {
    with_reparse(code, f1, code2, |context,syn,obj,file|{
        let second_reparse_range: RangeInclusive<Location> = f2(context, &syn, obj, file);
        context.clear_errors(Some(&second_reparse_range), Some(Component::Parser));
        context.clear_errors(None, Some(Component::ObjectTree));

        code3 = code3.trim();
        let mut file_provider = BufferedFileProvider{
            code: code3,
        };
        let result = incremental_reparse(
            &context,
            syn,
            second_reparse_range,
            code3,
            &mut file_provider,
            None);

        let (parser_error, syn2) = result;

        // for everything we reparse, we want to fully parse it and syntax compare it as well to assert there are no differences
        let context2 = Context::default();
        let path2 = std::path::PathBuf::from(r"test.dm");
        let mut pp2 = Preprocessor::from_buffer(&context2, path2, code3.trim());
        let indents2 = indents::IndentProcessor::new(&context2, &mut pp2);
        let mut parser2 = parser::Parser::new(&context2, indents2);
        parser2.enable_procs();
        let syntax_tree_2_full = parser2.parse();

        assert!(syn2.syntax_eq(&syntax_tree_2_full));

        let obj = if parser_error {
            None
        } else {
            Some(syn2.object_tree_without_builtins())
        };

        f3(context, syn2, obj, file)
    });
}

#[test]
fn test_basic_reparse() {
    with_reparse("
/datum/one

/datum/two

/datum/three
", |context: &Context, _, _, file|{
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
", |context, syn, _, _| {
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
", |context, _, _, file|{
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
", |context, syn, _, _|{
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
", |context, _, _, file|{
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
", |_, syn, _, _|{
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

#[test]
fn test_destructive_non_fatal_reparse_into_fix() {
    with_double_reparse("
/datum/asdf
    var/list/foo = list()

/datum/asdf/override
    foo = 4
", |context, syn, _, file|{
    context.assert_success();

    assert_eq!(2, syn.root().entries.len());

    range(Location {
        file,
        line: 1,
        column: 7,
    }, Location {
        file,
        line: 1,
        column: 8 })
}, "
/datumasdf
    var/list/foo = list()

/datum/asdf/override
    foo = 4
", |context, syn, _, file|{
    // expecting errors ofc
    assert_ne!(0, context.errors().len());
    // but now reparse it a second time and undo the damage
    assert_eq!(2, syn.root().entries.len());
    let bad_entry = &syn.root().entries[0];
    assert_eq!(1, bad_entry.start.line);
    assert_eq!(1, bad_entry.start.column);

    range(Location {
        file,
        line: 1,
        column: 7,
    }, Location {
        file,
        line: 1,
        column: 7 })
    },"
/datum/asdf
    var/list/foo = list()

/datum/asdf/override
    foo = 4
", |context, syn2, _, _|{
    assert_eq!(2, syn2.root().entries.len());
    let entry = &syn2.root().entries[0];

    assert_eq!(2, entry.leading_path.len());
    assert_eq!("datum", entry.leading_path[0]);
    assert_eq!("asdf", entry.leading_path[1]);
    if let TreeEntryData::Block(block) = &entry.data {
        assert_eq!(1, block.entries.len());
        // etc... etc..
        context.assert_success();
    } else {
        panic!("Expected block!");
    }
});
}

#[test]
fn test_fatal_open_brace_reparse_into_fix() {
    with_double_reparse("
/datum/braced{
	var/a;
}

/proc/foo()
    return
", |context, syn, _, file| {
    context.assert_success();
    assert_eq!(2, syn.root().entries.len());
    range(Location {
        file,
        line: 3,
        column: 1,
    }, Location {
        file,
        line: 3,
        column: 2,
    })
}, "
/datum/braced{
    var/a;


/proc/foo()
    return
", |context, _, obj, file|{
    assert_ne!(0, context.errors().len());
    assert!(obj.is_none());

    range(Location {
        file,
        line: 3,
        column: 1,
    }, Location {
        file,
        line: 3,
        column: 2,
    })
}, "
/datum/braced{
    var/a
}

/proc/foo()
    return
", |context, syn, obj, _|{
    assert!(obj.is_some());
    context.assert_success();
    assert_eq!(2, syn.root().entries.len());
})
}

#[test]
fn test_macro_multi_range_reparse(){
    with_reparse("
/datum/sub

#define DATUM(X) /datum/##X

DATUM(affected)

/datum/maybe_affected

/datum/definitely_not_affected

DATUM(second_affected)
", |ctx, _, _, file|{
    ctx.assert_success();

    range(Location{
        file,
        line: 3,
        column: 25
    },
    Location{
        file,
        line: 3,
        column: 25
    })
}, "
/datum/sub

#define DATUM(X) /datum/sub/##X

DATUM(affected)

/datum/maybe_affected

/datum/definitely_not_affected

DATUM(second_affected)
", |ctx, _, _, _| ctx.assert_success());
}
