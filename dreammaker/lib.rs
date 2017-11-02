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

// roughly in order of stage
pub mod lexer;
pub mod preprocessor;
pub mod indents;
pub mod parser;
pub mod ast;
pub mod objtree;
mod builtins;
pub mod constants;

/// Run the parsing suite on a given `.dme` file, producing an object tree.
///
/// Errors are automatically pretty-printed to stdout before they are returned.
pub fn parse_environment(dme: &Path) -> Result<objtree::ObjectTree, DMError> {
    let mut preprocessor = preprocessor::Preprocessor::new(dme.to_owned()).unwrap();
    parser::parse(indents::IndentProcessor::new(&mut preprocessor)).map_err(|e| {
        pretty_print_error(&mut io::stdout(), &preprocessor, &e).unwrap();
        e
    })
}

// ----------------------------------------------------------------------------
// Error handling

/// An error produced during DM parsing, with location information.
#[derive(Debug)]
pub struct DMError {
    location: Location,
    desc: String,
}

#[allow(unused_variables)]
impl DMError {
    #[doc(hidden)]
    pub fn new<S: Into<String>>(location: Location, desc: S) -> DMError {
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

    /// Get the location in the code at which this error was observed.
    pub fn location(&self) -> Location {
        self.location
    }

    /// Get the description associated with this error.
    pub fn description(&self) -> &str {
        &self.desc
    }
}

impl From<io::Error> for DMError {
    fn from(e: io::Error) -> DMError {
        DMError::with_cause(Location::default(), "i/o error", e)
    }
}

// ----------------------------------------------------------------------------
// Location handling

/// File, line, and column information for an error.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Location {
    /// The index into the file table.
    pub file: u32,
    /// The line number, starting at 1.
    pub line: u32,
    /// The column number, starting at 1.
    pub column: u32,
}

/// A trait for types which may yield location information.
pub trait HasLocation {
    /// Get the current location of this parsing stage.
    fn location(&self) -> Location;

    #[inline]
    #[doc(hidden)]
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

/// Pretty-print a `DMError` to the given output.
pub fn pretty_print_error<W: io::Write>(w: &mut W, pp: &preprocessor::Preprocessor, error: &DMError) -> io::Result<()> {
    writeln!(w, "\n{}, line {}, column {}:",
        pp.file_path(error.location.file).display(),
        error.location.line,
        error.location.column)?;
    writeln!(w, "{}\n", error.desc)
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
