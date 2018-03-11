extern crate dreammaker as dm;

use dm::lexer::*;
use dm::lexer::Token::*;
use dm::lexer::Punctuation::*;

#[cfg(test)]
fn lex(f: &str) -> Vec<Token> {
    let context = Default::default();
    let result = Lexer::new(&context, Default::default(), f.bytes().map(Ok))
        .map(|t| t.token)
        .collect();
    assert!(context.errors().is_empty());
    result
}

#[test]
fn floats() {
    assert_eq!(lex("0.08"), vec![Float(0.08), Punct(Newline)]);
}

#[test]
fn nested_interpolation() {
    assert_eq!(lex(r#""A[B"C"D]E""#), vec![
        InterpStringBegin("A".into()),
        Ident("B".into(), false),
        String("C".into()),
        Ident("D".into(), false),
        InterpStringEnd("E".into()),
        Punct(Newline),
    ]);
}
