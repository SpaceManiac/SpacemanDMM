//! Parsing suite for DreamMaker, the language of the BYOND game engine.
#[cfg(feature="xml-rs")] extern crate xml;
extern crate petgraph;
extern crate linked_hash_map;

use std::io;
use std::path::Path;

macro_rules! try_iter {
    ($e:expr) => {
        match $e {
            Ok(x) => x,
            Err(e) => return Some(Err(From::from(e))),
        }
    }
}

mod error;
pub use error::*;

// roughly in order of stage
pub mod lexer;
pub mod preprocessor;
pub mod indents;
pub mod parser;
pub mod ast;
pub mod objtree;
mod builtins;
pub mod constants;

impl Context {
    /// Run the parsing suite on a given `.dme` file, producing an object tree.
    ///
    /// Errors are automatically pretty-printed to stderr before they are returned.
    pub fn parse_environment(&mut self, dme: &Path) -> Result<objtree::ObjectTree, DMError> {
        let start = self.errors().len();
        let result = parser::parse(self,
            indents::IndentProcessor::new(self,
                preprocessor::Preprocessor::new(self, dme.to_owned())?.map(Ok)
            )
        );

        let errors = self.errors();
        let stderr = io::stderr();
        let stderr = &mut stderr.lock();
        for err in &errors[start..] {
            self.pretty_print_error(stderr, &err).expect("error writing to stderr");
        }
        if let Err(ref err) = result {
            self.pretty_print_error(stderr, &err).expect("error writing to stderr");
        }

        result
    }
}

// ----------------------------------------------------------------------------
// Pretty printing

/// Pretty-print a series of tokens to the given output.
///
/// If `show_ws` is true, braces and semicolons are included directly in the
/// output rather than only being implied by the indentation.
pub fn pretty_print<W, I>(w: &mut W, input: I, show_ws: bool) -> Result<(), DMError> where
    W: io::Write,
    I: IntoIterator<Item=Result<lexer::Token, DMError>>
{
    let mut indents = 0;
    let mut needs_newline = false;
    let mut prev = None;
    for token in input {
        match token? {
            lexer::Token::Punct(lexer::Punctuation::LBrace) => {
                indents += 1;
                needs_newline = true;
                if show_ws { write!(w, "{{")?; }
            }
            lexer::Token::Punct(lexer::Punctuation::RBrace) => {
                indents -= 1;
                needs_newline = true;
                if show_ws { write!(w, "}}")?; }
            }
            lexer::Token::Punct(lexer::Punctuation::Semicolon) |
            lexer::Token::Punct(lexer::Punctuation::Newline) => {
                needs_newline = true;
                if show_ws { write!(w, ";")?; }
            }
            other => {
                if needs_newline {
                    const SPACES: &str = "                                ";
                    let spaces = 2 * indents;
                    writeln!(w)?;
                    for _ in 0..(spaces / SPACES.len()) {
                        write!(w, "{}", SPACES)?;
                    }
                    write!(w, "{}", &SPACES[..spaces % SPACES.len()])?;
                    needs_newline = false;
                } else if let Some(prev) = prev {
                    if other.separate_from(&prev) {
                        write!(w, " ")?;
                    }
                }
                write!(w, "{}", other)?;
                prev = Some(other);
            }
        }
    }
    if needs_newline {
        writeln!(w)?;
    }
    Ok(())
}

// ----------------------------------------------------------------------------
// Tests

#[cfg(test)]
mod test {
    use super::*;
    use std::path::PathBuf;

    const TEST_FILE: &str = "D:/projects/tgstation/tgstation.dme";

    #[test]
    fn check_preprocessor() {
        let stdout = io::stdout();
        let mut stdout = stdout.lock();
        let mut preprocessor = preprocessor::Preprocessor::new(PathBuf::from(TEST_FILE)).unwrap();
        match pretty_print(&mut stdout, preprocessor.by_ref().map(|t| t.map(|t| t.token)), true) {
            Ok(()) => {}
            Err(e) => pretty_print_error(&mut stdout, &preprocessor, &e).unwrap(),
        }
    }

    #[test]
    fn check_indentor() {
        let stdout = io::stdout();
        let mut stdout = stdout.lock();
        let mut preprocessor = preprocessor::Preprocessor::new(PathBuf::from(TEST_FILE)).unwrap();
        match pretty_print(&mut stdout, indents::IndentProcessor::new(&mut preprocessor).map(|t| t.map(|t| t.token)), true) {
            Ok(()) => {}
            Err(e) => pretty_print_error(&mut stdout, &preprocessor, &e).unwrap(),
        }
    }

    #[test]
    fn check_parser() {
        let mut preprocessor = preprocessor::Preprocessor::new(PathBuf::from(TEST_FILE)).unwrap();
        let tree = parser::parse(indents::IndentProcessor::new(&mut preprocessor));
        match tree {
            Ok(v) => {
                println!("\n--------\nSuccess!\n--------");
                v.to_xml("objtree.xml".as_ref()).unwrap();
            }
            Err(e) => pretty_print_error(&mut io::stdout(), &preprocessor, &e).unwrap(),
        }
    }
}
