//! Rendering a preprocessed token stream back into our source text,
//! for the `experimental/dreammaker/expandMacros` request.

use std::fmt::Write;

use dm::Token;
use dm::lexer::{Lexer, LocatedToken, Token};
use dm::{Context, FileId};

/// Find the lines of `source` which contain actual code
fn code_lines(source: &str, line_count: usize) -> Vec<bool> {
    let mut code = vec![false; line_count];
    let context = Context::default();

    let mut directive = false;

    for token in Lexer::new(&context, FileId::UNKNOWN, source.as_bytes()) {
        // Directive stays in effect for the whole line (lexer eats \ continuations)
        if token.token == Token!['\n'] {
            directive = false;
            continue;
        }
        // comments are kept
        if token.token.is_whitespace() || matches!(token.token, Token::DocComment(_)) {
            continue;
        }
        // `#` starts a directive, which runs to the end of the line
        directive |= token.token == Token![#];

        if let Some(line) = code.get_mut(token.start.line as usize - 1) {
            *line = !directive;
        }
    }

    code
}

/// Rebuild `source` with its macros expanded. Akin to [dm::pretty_print]
///
/// A line that produced no tokens keeps the original text, unless it held code that was
/// swallowed by a multi-line macro invocation or skipped by an `#if`.
pub fn render<I>(source: &str, file: FileId, tokens: I) -> String
where
    I: IntoIterator<Item = LocatedToken>,
{
    let source_lines: Vec<&str> = source.lines().collect();
    let code = code_lines(source, source_lines.len());
    let mut rendered = vec![String::new(); source_lines.len()];

    let mut prev: Option<(usize, Token)> = None;

    for LocatedToken { start, token, .. } in tokens {
        // Drop whitespace and copy indentation from the original line
        if start.file != file || token.is_whitespace() || matches!(token, Token::DocComment(_)) {
            continue;
        }
        let index = start.line as usize - 1;
        let Some(line) = rendered.get_mut(index) else {
            continue;
        };

        if let Some((prev_index, prev_token)) = &prev
            && *prev_index == index
            && token.separate_from(prev_token)
        {
            line.push(' ');
        }

        let _ = write!(line, "{token}");
        prev = Some((index, token));
    }

    // Create the result
    let mut output = String::with_capacity(source.len());
    for (index, source_line) in source_lines.iter().enumerate() {
        let line = rendered[index].trim_end();
        if !line.is_empty() {
            // Reuse the original indentation
            let indent = source_line.len() - source_line.trim_start().len();
            output.push_str(&source_line[..indent]);
            output.push_str(line);
        } else if !code[index] {
            output.push_str(source_line);
        } else {
            continue;
        }
        output.push('\n');
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use dm::preprocessor::Preprocessor;

    fn expand(source: &str) -> String {
        let context = Context::default();
        let mut pp = Preprocessor::from_buffer(&context, "test.dm".into(), source.to_owned());
        let file = context.files().get_id("test.dm".as_ref()).unwrap();
        let tokens: Vec<LocatedToken> = (&mut pp).collect();
        pp.finalize();
        render(source, file, tokens)
    }

    #[test]
    fn expands_macro_in_place() {
        let expanded = expand(
            "#define IS_TYPE(A, L) (A && length(L))\n\
             /proc/foo(obj)\n\
             \tif(IS_TYPE(obj, list))\n\
             \t\treturn\n",
        );
        assert_eq!(
            expanded,
            "#define IS_TYPE(A, L) (A && length(L))\n\
             /proc/foo(obj)\n\
             \tif((obj && length(list)))\n\
             \t\treturn\n"
        );
    }

    #[test]
    fn collapses_multi_line_invocation() {
        let expanded = expand(
            "#define PAIR(A, B) A + B\n\
             /proc/foo()\n\
             \tvar/x = PAIR(PAIR(1,\n\
             \t\t2),\n\
             \t\t3)\n\
             \treturn x\n",
        );
        assert_eq!(
            expanded,
            "#define PAIR(A, B) A + B\n\
             /proc/foo()\n\
             \tvar/x = 1 + 2 + 3\n\
             \treturn x\n"
        );
    }

    #[test]
    fn keeps_comments_and_drops_disabled_code() {
        let expanded = expand(
            "// leading comment\n\
             #if 0\n\
             /proc/gone()\n\
             #endif\n\
             \n\
             /proc/kept()\n\
             \t// indented comment\n\
             \treturn\n",
        );
        assert_eq!(
            expanded,
            "// leading comment\n\
             #if 0\n\
             #endif\n\
             \n\
             /proc/kept()\n\
             \t// indented comment\n\
             \treturn\n"
        );
    }
}
