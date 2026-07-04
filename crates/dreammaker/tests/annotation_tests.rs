extern crate dreammaker as dm;

use dm::annotation::*;
use dm::indents::IndentProcessor;
use dm::lexer::*;
use dm::parser::Parser;
use dm::Location;

fn parse_annotations(code: &str) -> AnnotationTree {
    parse_annotations_with_success_check(code, true)
}

fn parse_annotations_allow_errors(code: &str) -> AnnotationTree {
    parse_annotations_with_success_check(code, false)
}

fn parse_annotations_with_success_check(code: &str, assert_success: bool) -> AnnotationTree {
    let context = Default::default();
    let lexer = Lexer::new(&context, Default::default(), code.trim().as_bytes());
    let indent = IndentProcessor::new(&context, lexer);
    let mut annotations = AnnotationTree::default();
    Parser::new(&context, indent).parse_annotations_only(&mut annotations);
    if assert_success {
        context.assert_success();
    }
    annotations
}

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

    let annotations = parse_annotations(code);
    println!("len: {}", annotations.len());
    for each in annotations.get_location(Location {
        file: Default::default(),
        line: 9,
        column: 14,
    }) {
        println!("{each:?}");
        for each in annotations.get_range_raw(each.0) {
            println!("    {:?}", each.1);
        }
    }
}

#[test]
fn static_scoped_var_annotation() {
    let annotations = parse_annotations(
        r#"
/datum/foo
    var/bar
/proc/test()
    /datum/foo::bar
"#,
    );

    let mut saw_static_scoped_var = false;
    for (_, annotation) in annotations.get_location(Location {
        file: Default::default(),
        line: 4,
        column: 18,
    }) {
        if let Annotation::StaticScopedVar(priors, var_name) = annotation {
            saw_static_scoped_var = priors
                .iter()
                .map(|ident| ident.as_str())
                .collect::<Vec<_>>()
                == ["datum", "foo"]
                && var_name.as_str() == "bar";
        }
    }

    assert!(
        saw_static_scoped_var,
        "expected /datum/foo::bar to annotate bar as StaticScopedVar"
    );
}

#[test]
fn static_scoped_missing_ident_annotation() {
    let annotations = parse_annotations_allow_errors(
        r#"
/datum/foo
    var/bar
/proc/test()
    /datum/foo::
"#,
    );

    let mut saw_static_scoped_missing_ident = false;
    for (_, annotation) in annotations.iter() {
        if let Annotation::StaticScopedMissingIdent(priors) = annotation {
            saw_static_scoped_missing_ident = priors
                .iter()
                .map(|ident| ident.as_str())
                .collect::<Vec<_>>()
                == ["datum", "foo"];
        }
    }

    assert!(
        saw_static_scoped_missing_ident,
        "expected /datum/foo:: to annotate completion as StaticScopedMissingIdent"
    );
}
