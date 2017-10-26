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

// roughly in order of stage
pub mod lexer;
pub mod preprocessor;
pub mod indents;

#[derive(Debug)]
pub struct DMError {
    location: Location,
    desc: String,
}

#[allow(unused_variables)]
impl DMError {
    fn new<S: Into<String>>(location: Location, desc: S) -> DMError {
        DMError {
            location,
            desc: desc.into(),
        }
    }

    fn with_cause<S, E>(location: Location, desc: S, _cause: E) -> DMError
        where S: Into<String>, E: ::std::error::Error + 'static
    {
        Self::new(location, desc) // TODO
    }
}

impl From<io::Error> for DMError {
    fn from(e: io::Error) -> DMError {
        DMError::with_cause(Location::default(), "i/o error", e)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Location {
    pub file: u32,
    pub line: u32,
    pub column: u32,
}

pub trait HasLocation {
    fn location(&self) -> Location;

    #[inline]
    fn error<S: Into<String>>(&self, message: S) -> DMError {
        DMError::new(self.location(), message)
    }
}
impl<'a, T: HasLocation> HasLocation for &'a T {
    fn location(&self) -> Location { (**self).location() }
}
impl<'a, T: HasLocation> HasLocation for &'a mut T {
    fn location(&self) -> Location { (**self).location() }
}

fn parse(path: &Path) {
    let mut preprocessor = preprocessor::Preprocessor::new(path.to_owned()).unwrap();
    let mut indents = 0;
    let mut needs_newline = false;
    let mut error = None;
    let mut prev = None;
    for thing in indents::IndentProcessor::new(&mut preprocessor) {
        let thing = match thing {
            Ok(t) => t,
            Err(e) => {
                error = Some(e);
                break;
            }
        };
        match thing.token {
            lexer::Token::Punct(lexer::Punctuation::LBrace) => {
                indents += 1;
                needs_newline = true;
            }
            lexer::Token::Punct(lexer::Punctuation::RBrace) => {
                indents -= 1;
                needs_newline = true;
            }
            lexer::Token::Punct(lexer::Punctuation::Semicolon) => {
                needs_newline = true;
            }
            other => {
                if needs_newline {
                    print!("\n{}", &"                                "[..2*indents]);
                    needs_newline = false;
                } else if let Some(prev) = prev {
                    if other.separate_from(&prev) {
                        print!(" ");
                    }
                }
                print!("{}", other);
                prev = Some(other);
            }
        }
    }
    if let Some(error) = error {
        println!("\n{}, line {}, column {}:",
            preprocessor.file_path(error.location.file).display(),
            error.location.line,
            error.location.column);
        println!("{}\n", error.desc);
    }
}

#[test]
fn tgstation() {
    parse("D:/projects/tgstation/tgstation.dme".as_ref());
}
