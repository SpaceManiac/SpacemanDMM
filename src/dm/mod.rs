//
#![allow(dead_code)]

use std::path::Path;
use std::io;

macro_rules! try_iter {
    ($e:expr) => {
        match $e {
            Ok(x) => x,
            Err(e) => return Some(Err(From::from(e))),
        }
    }
}
macro_rules! try_opt {
    ($e:expr) => {
        match $e {
            Some(x) => x,
            None => return None,
        }
    }
}

// roughly in order of stage
pub mod lexer;
pub mod preprocessor;
pub mod indents;

#[derive(Debug)]
pub struct DMError;

#[allow(unused_variables)]
impl DMError {
    fn new<S: Into<String>>(line: usize, col: usize, desc: S) -> DMError {
        // TODO
        panic!("{}:{}: {}", line, col, desc.into());
    }

    fn with_cause<S, E>(line: usize, col: usize, desc: S, cause: E) -> DMError
        where S: Into<String>, E: ::std::error::Error + 'static
    {
        // TODO
        panic!("{}:{}: {}\n{}\n{:?}", line, col, desc.into(), cause, cause);
    }
}

impl From<io::Error> for DMError {
    fn from(e: io::Error) -> DMError {
        DMError::with_cause(0, 0, "i/o error", e)
    }
}

fn parse(path: &Path) {
    let mut was_newline = true;
    for token in indents::IndentProcessor::new(preprocessor::Preprocessor::new(path.to_owned()).unwrap()) {
        let t = token.unwrap();
        print!("{} ", t.token);
        match t.token {
            lexer::Token::Punct(lexer::Punctuation::LBrace) |
            lexer::Token::Punct(lexer::Punctuation::RBrace) |
            lexer::Token::Punct(lexer::Punctuation::Semicolon) => {
                println!();
            }
            _ => {}
        }
    }
}

#[test]
fn tgstation() {
    parse("D:/projects/tgstation/tgstation.dme".as_ref());
}
