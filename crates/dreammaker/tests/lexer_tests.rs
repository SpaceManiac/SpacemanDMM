extern crate dreammaker as dm;

use dm::lexer::Punctuation::*;
use dm::lexer::Token::*;
use dm::lexer::*;

fn lex(f: &str) -> Vec<Token> {
    let context = Default::default();
    let result = Lexer::new(&context, Default::default(), f.as_bytes())
        .map(|t| t.token)
        .collect();
    context.assert_success();
    result
}

fn one_token(f: &str) -> Token {
    let mut v = lex(f);
    assert_eq!(v.len(), 2, "not one token: {f:?} -> {v:?}");
    assert_eq!(v[1], Punct(Newline));
    v.remove(0)
}

fn float(f: &str) -> f32 {
    match one_token(f) {
        Token::Float(f) => f,
        other => panic!("{f:?}: expected float, got {other:?}"),
    }
}

#[test]
fn number_literals() {
    assert_eq!(lex("0.08"), vec![Float(0.08), Punct(Newline)]);
    assert_eq!(lex("0xABCDE"), vec![Int(703710), Punct(Newline)]);
    assert_eq!(lex("1e4"), vec![Float(10000.0), Punct(Newline)]);

    let f = float("1.#INF"); assert!(f.is_infinite() && f > 0.);
    let f = float("1.#IND"); assert!(f.is_nan());
    let f = float("1#INF"); assert!(f.is_infinite() && f > 0.);
    let f = float("1#IND"); assert!(f.is_nan());
}

#[test]
fn nested_interpolation() {
    assert_eq!(
        lex(r#""A[B"C"D]E""#),
        vec![
            InterpStringBegin("A".into()),
            Ident("B".into(), false),
            String("C".into()),
            Ident("D".into(), false),
            InterpStringEnd("E".into()),
            Punct(Newline),
        ]
    );
}

#[test]
fn empty_block_comment() {
    // This is legal. It should not do either of the following:
    // - Error with "still skipping comments at end of file"
    // - Yield a DocComment { text: "", .. }
    assert_eq!(
        lex(r#"/**/"#),
        vec![Punct(Newline)]
    )
}

#[test]
fn raw_strings() {
    let desired = Token::String("content".to_owned());
    let stuff = lex(r#"
@"content"
@xcontentx
@/content/
@(x)contentx
@(EOD)contentEOD
@(very long terminator)contentvery long terminator
@{"content"}
@{content{
"#);
    for each in stuff.iter() {
        if each == &Punct(Newline) { continue }
        assert_eq!(each, &desired);
    }
}

#[test]
fn heredoc_with_quotes() {
    // 1-3 quotes in the middle of ordinary characters
    assert_eq!(
        lex(r#"{"foo"bar"}"#),
        vec![
            Token::String(r#"foo"bar"#.to_owned()),
            Token::Punct(Punctuation::Newline),
        ]
    );
    assert_eq!(
        lex(r#"{"foo""bar"}"#),
        vec![
            Token::String(r#"foo""bar"#.to_owned()),
            Token::Punct(Punctuation::Newline),
        ]
    );
    assert_eq!(
        lex(r#"{"foo"""bar"}"#),
        vec![
            Token::String(r#"foo"""bar"#.to_owned()),
            Token::Punct(Punctuation::Newline),
        ]
    );

    // 0-5 quotes at the start/end
    assert_eq!(
        lex(r#"{""}"#),
        vec![
            Token::String(r#""#.to_owned()),
            Token::Punct(Punctuation::Newline),
        ]
    );
    assert_eq!(
        lex(r#"{"""}"#),
        vec![
            Token::String(r#"""#.to_owned()),
            Token::Punct(Punctuation::Newline),
        ]
    );
    assert_eq!(
        lex(r#"{""""}"#),
        vec![
            Token::String(r#""""#.to_owned()),
            Token::Punct(Punctuation::Newline),
        ]
    );
    assert_eq!(
        lex(r#"{"""""}"#),
        vec![
            Token::String(r#"""""#.to_owned()),
            Token::Punct(Punctuation::Newline),
        ]
    );
}
