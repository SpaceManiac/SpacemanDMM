extern crate dreammaker as dm;

use dm::preprocessor::*;
use dm::lexer::Token::*;
use dm::lexer::Punctuation::*;

fn process(source: &'static str) -> Vec<dm::lexer::Token> {
    let ctx = dm::Context::default();
    let pp = Preprocessor::from_buffer(&ctx, "macro_tests.rs".into(), source);

    // collect tokens, strip leading and trailing newlines
    let mut tokens: Vec<_> = pp.map(|loctok| loctok.token)
        .skip_while(|tok| *tok == Punct(Newline))
        .collect();
    ctx.assert_success();
    while let Some(&Punct(Newline)) = tokens.last() {
        tokens.pop();
    }
    tokens
}

#[test]
fn clamp_inside_clamp() {
    // check that both the inner and outer "CLAMP" calls became "clamp" calls
    assert_eq!(process(r#"
#define CLAMP(VAL, MIN, MAX) clamp(VAL, MIN, MAX)

CLAMP(alpha - CLAMP(beta - 2, 0, beta), 3, alpha)
"#), &[
        Ident("clamp".into(), false),
        Punct(LParen),
            Ident("alpha".into(), true),
            Punct(Sub),
            Ident("clamp".into(), false),
            Punct(LParen),
                Ident("beta".into(), true),
                Punct(Sub),
                Int(2),
                Punct(Comma),
                Int(0),
                Punct(Comma),
                Ident("beta".into(), false),
            Punct(RParen),
            Punct(Comma),
            Int(3),
            Punct(Comma),
            Ident("alpha".into(), false),
        Punct(RParen),
    ]);
}

#[test]
fn defined_function() {
    assert_eq!(process(r#"
#define FOO
#if defined(FOO)
ok1
#endif
"#), &[
        Ident("ok1".into(), false),
    ]);

    assert_eq!(process(r#"
#define A multiple.tokens()
#if defined(C) || defined(A) && !defined(B)
ok2
#endif
"#), &[
        Ident("ok2".into(), false),
    ]);
}
