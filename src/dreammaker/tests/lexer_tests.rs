extern crate dreammaker as dm;

use dm::lexer::*;
use dm::lexer::Token::*;
use dm::lexer::Punctuation::*;

fn lex(f: &str) -> Vec<Token> {
    let context = Default::default();
    let result = Lexer::new(&context, Default::default(), f.bytes().map(Ok))
        .map(|t| t.token)
        .collect();
    context.assert_success();
    result
}

#[test]
fn floats() {
    assert_eq!(lex("0.08"), vec![Float(0.08), Punct(Newline)]);
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
    let stuff = lex(r###"
@"content"
@xcontentx
@/content/
@(x)contentx
@(EOD)contentEOD
@(very long terminator)contentvery long terminator
@{"content"}
@{content{
"###);
    for each in stuff.iter() {
        if each == &Punct(Newline) { continue }
        assert_eq!(each, &desired);
    }
}
