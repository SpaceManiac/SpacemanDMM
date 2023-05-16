//! Parsing suite for DreamMaker, the language of the BYOND game engine.
#![forbid(unsafe_code)]

extern crate indexmap;
extern crate interval_tree;
extern crate lodepng;
#[macro_use] extern crate bitflags;
#[macro_use] extern crate guard;
extern crate termcolor;
extern crate ordered_float;
extern crate serde;
extern crate serde_derive;
extern crate toml;

use std::path::Path;
use std::borrow::Cow;

use annotation::AnnotationTree;
pub use error::*;
use ast::SyntaxTree;
use interval_tree::RangeInclusive;
use lexer::LocatedToken;

use crate::indents::IndentProcessor;
use crate::parser::Parser;

#[allow(unused_macros)]
macro_rules! try_iter {
    ($e:expr) => {
        match $e {
            Ok(x) => x,
            Err(e) => return Some(Err(From::from(e))),
        }
    };
}

mod error;
// roughly in order of stage
pub mod docs;
pub mod lexer;
pub mod preprocessor;
pub mod indents;
pub mod parser;
pub mod annotation;
pub mod ast;
pub mod objtree;
mod builtins;
pub mod constants;
pub mod dmi;
pub mod config;

impl Context {
    /// Run the parsing suite on a given `.dme` file, producing an object tree.
    ///
    /// Will only return failure on an `io::Error`. Compilation failures will
    /// return a best-effort parse. Call `print_all_errors` to pretty-print
    /// errors to standard error.
    pub fn parse_environment(&self, dme: &Path) -> Result<SyntaxTree, DMError> {
        Ok(parser::parse(self,
            indents::IndentProcessor::new(self,
                preprocessor::Preprocessor::new(self, dme.to_owned())?
            )
        ))
    }
}

// ----------------------------------------------------------------------------
// Pretty printing

/// Pretty-print a series of tokens to the given output.
///
/// If `show_ws` is true, braces and semicolons are included directly in the
/// output rather than only being implied by the indentation.
pub fn pretty_print<W, I>(w: &mut W, input: I, show_ws: bool) -> std::fmt::Result where
    W: std::fmt::Write,
    I: IntoIterator<Item=lexer::Token>
{
    let mut indents = 0;
    let mut needs_newline = false;
    let mut prev = None;
    for token in input {
        match token {
            lexer::Token::Punct(lexer::Punctuation::LBrace) => {
                indents += 1;
                needs_newline = true;
                if show_ws {
                    write!(w, "{{")?;
                }
            }
            lexer::Token::Punct(lexer::Punctuation::RBrace) => {
                indents -= 1;
                needs_newline = true;
                if show_ws {
                    write!(w, "}}")?;
                }
            }
            lexer::Token::Punct(lexer::Punctuation::Semicolon) |
            lexer::Token::Punct(lexer::Punctuation::Newline) => {
                needs_newline = true;
                if show_ws {
                    write!(w, ";")?;
                }
            }
            lexer::Token::DocComment(_) => {}
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
// Utilities

/// Attempt to case-correct the last component of the given path.
///
/// On Windows, this is a no-op.
#[cfg(windows)]
#[inline(always)]
pub fn fix_case(path: &Path) -> Cow<Path> {
    Cow::Borrowed(path)
}

/// Attempt to case-correct the last component of the given path.
///
/// On non-Windows platforms, the parent of the given path is searched for a
/// file with the same name but a different case.
#[cfg(not(windows))]
pub fn fix_case(path: &Path) -> Cow<Path> {
    if path.exists() {
        return Cow::Borrowed(path);
    }

    let parent = match path.parent() {
        Some(x) => x,
        None => return Cow::Borrowed(path),
    };

    for entry in match parent.read_dir() {
        Ok(x) => x,
        Err(_) => return Cow::Borrowed(path),
    } {
        let entry = match entry {
            Ok(x) => x,
            Err(_) => return Cow::Borrowed(path),
        };
        let epath = entry.path();
        let epath_str = epath.display().to_string();
        let path_str = path.display().to_string();
        if epath_str.eq_ignore_ascii_case(&path_str) {
            return Cow::Owned(epath);
        }
    }
    Cow::Borrowed(path)
}

pub const DEFAULT_ENV: &str = "tgstation.dme";

/// Autodetect any `.dme` file in the current folder, or fall back to default.
///
/// If multiple environments exist, the first non-default is preferred.
pub fn detect_environment(root: &Path, default: &str) -> std::io::Result<Option<std::path::PathBuf>> {
    let mut result = None;
    for entry in std::fs::read_dir(root)?.flatten() {
        let name = entry.file_name();
        let (dme, default) = {
            let utf8_name = name.to_string_lossy();
            (utf8_name.ends_with(".dme"), utf8_name == default)
        };
        if dme {
            result = Some(entry.path());
            if !default {
                break;
            }
        }
    }
    Ok(result)
}

pub fn detect_environment_default() -> std::io::Result<Option<std::path::PathBuf>> {
    // Return a path in the current directory `.` ...
    detect_environment(".".as_ref(), DEFAULT_ENV).map(|o| o.map(|path| {
        // ... but without `./` preceding it.
        path.strip_prefix(".").map(|p| p.to_owned()).unwrap_or(path)
    }))
}

pub fn incremental_reparse<'ctx, S: Into<Cow<'ctx, str>>>(
    context: &'ctx Context,
    syntax_tree: SyntaxTree<'ctx>,
    range: RangeInclusive<Location>,
    file_buffer: S,
    annotations_opt: Option<&mut AnnotationTree>) -> Result<(bool, SyntaxTree<'ctx>), DMError> {

    // incremental parsing across files is not supported, do one at a time
    // adding an #include line is still a one file change
    assert_eq!(range.start.file, range.end.file);
    let defines_option = syntax_tree.defines();
    if defines_option.is_none() {
        return Err(DMError::new(Location::builtins(), "SyntaxTree does not have define history!"));
    }

    // get our reference point define maps
    let existing_defines = defines_option.unwrap();
    let mut preprocessor = existing_defines.branch_at(range.start, context);
    let existing_defines_end = existing_defines.branch_at(Location {
            file: range.end.file,
            line: range.end.line + 1, // should be gucchi
            column: 1,
        }, context).finalize();

    let path = context.file_list().get_path(range.start.file);

    // Should not fail unless file_buffer is scuffed
    preprocessor.push_buffer(path, file_buffer)?;

    let indents = IndentProcessor::new(context, &mut preprocessor);

    // preprocess before continuing
    let tokens: Vec<LocatedToken> = indents.into_iter().collect();

    let new_defines_end = preprocessor.finalize();

    if !new_defines_end.currently_equals(&existing_defines_end) {
        // Uncommon, but slow AF, we have to re-preprocess everything after this point
        todo!();
    }

    let mut parser = Parser::new(context, tokens);
    if let Some(annotations) = annotations_opt {
        parser.annotate_to(annotations);
    }

    Ok(parser.reparse_2(syntax_tree, range))
}
