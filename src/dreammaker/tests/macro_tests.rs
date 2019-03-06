extern crate dreammaker as dm;

use dm::preprocessor::*;
use dm::lexer::Token::*;
use dm::lexer::Punctuation::*;

#[test]
fn clamp_inside_clamp() {
    let ctx = dm::Context::default();
    let pp = Preprocessor::from_buffer(&ctx, "macro_tests.rs".into(), r#"
#define CLAMP(VAL, MIN, MAX) clamp(VAL, MIN, MAX)

CLAMP(alpha - CLAMP(beta - 2, 0, beta), 3, alpha)
"#);

    // collect tokens, strip leading and trailing newlines
    let tokens: Vec<_> = pp.map(|loctok| loctok.token).collect();
    let mut subset = &tokens[..];
    while let Some((&Punct(Newline), rest)) = subset.split_first() {
        subset = rest;
    }
    while let Some((&Punct(Newline), rest)) = subset.split_last() {
        subset = rest;
    }

    // check that both the inner and outer "CLAMP" calls became "clamp" calls
    assert_eq!(subset, &[
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
