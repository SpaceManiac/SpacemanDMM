extern crate dreammaker as dm;

use dm::Token;
use dm::lexer::Token::{Ident, Int};
use dm::preprocessor::Preprocessor;

fn process(source: &'static str) -> Vec<dm::lexer::Token> {
    let ctx = dm::Context::default();
    let pp = Preprocessor::from_buffer(&ctx, "macro_tests.rs".into(), source);

    // collect tokens, strip leading and trailing newlines
    let mut tokens: Vec<_> = pp
        .map(|loctok| loctok.token)
        .skip_while(|tok| *tok == Token!['\n'])
        .collect();
    ctx.assert_success();
    while let Some(&Token!['\n'] | &Token![' ']) = tokens.last() {
        tokens.pop();
    }
    tokens
}

#[test]
fn clamp_inside_clamp() {
    // check that both the inner and outer "CLAMP" calls became "clamp" calls
    assert_eq!(
        process(
            r#"
#define CLAMP(VAL, MIN, MAX) clamp(VAL, MIN, MAX)

CLAMP(alpha - CLAMP(beta - 2, 0, beta), 3, alpha)
"#
        ),
        &[
            Ident("clamp".into(), false),
            Token!['('],
            Ident("alpha".into(), true),
            Token![-],
            Ident("clamp".into(), false),
            Token!['('],
            Ident("beta".into(), true),
            Token![-],
            Int(2),
            Token![,],
            Int(0),
            Token![,],
            Ident("beta".into(), false),
            Token![')'],
            Token![,],
            Int(3),
            Token![,],
            Ident("alpha".into(), false),
            Token![')'],
        ]
    );
}

#[test]
fn variadic_token_pasting() {
    // Per https://www.byond.com/docs/ref/#/DM/preprocessor/define:
    // > If you use this with the last argument in a variadic macro, any preceding spaces and a comma (if found) will be removed if the replacement is empty.
    // Actually, the same applies to any empty argument.
    assert_eq!(
        process(
            r#"
#define MACRO_COMMA(x, y...) x, ##y
#define MACRO_SLASH(x, y...) x/ ##y
#define MANY(w, x, y, z) w ## x ## y ## z
MACRO_COMMA(1)
MACRO_COMMA(1, 2)
MACRO_SLASH(1)
MACRO_SLASH(1, 2)
MANY(w,,,z)
            "#
        )
        .split(|t| *t == Token!['\n'])
        .collect::<Vec<_>>(),
        &[
            &[Int(1)][..],
            &[Int(1), Token![,], Int(2)],
            &[Int(1), Token![/]],
            &[Int(1), Token![/], Int(2)],
            &[Ident("wz".into(), false)]
        ]
    );
}

#[test]
fn defined_function() {
    assert_eq!(
        process(
            r#"
#define FOO
#if defined(FOO)
ok1
#endif
"#
        ),
        &[Ident("ok1".into(), false),]
    );

    assert_eq!(
        process(
            r#"
#define A multiple.tokens()
#if defined(C) || defined(A) && !defined(B)
ok2
#endif
"#
        ),
        &[Ident("ok2".into(), false),]
    );
}

#[test]
fn fexists_function() {
    assert_eq!(
        process(
            r#"
#if fexists("README.md")
exists
#endif
"#
        ),
        &[Ident("exists".into(), false),]
    );

    assert_eq!(
        process(
            r#"
#if fexists("this file does not exist")
exists
#endif
"#
        ),
        &[]
    );
}
