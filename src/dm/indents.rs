//! The indentation processor.
use std::collections::VecDeque;

use super::DMError;
use super::lexer::{LocatedToken, Token, Punctuation};

/// Eliminates blank lines, parses and validates indentation, braces, and semicolons.
///
/// After processing, no Newline, Tab, or Space tokens remain.
pub struct IndentProcessor<I> {
    inner: I,
    pasting: Option<(usize, usize, VecDeque<Token>)>,

    // If we're indented, the number of spaces per indent and the number of indents.
    current: Option<(usize, usize)>,
    // The number of spaces/tabs accumulated on the current line. None when not at line head.
    current_spaces: Option<usize>,
    parentheses: usize,
}

impl<I: Iterator<Item=Result<LocatedToken, DMError>>> IndentProcessor<I> {
    pub fn new<J: IntoIterator<Item=Result<LocatedToken, DMError>, IntoIter=I>>(inner: J) -> Self {
        IndentProcessor {
            inner: inner.into_iter(),
            pasting: None,
            current: None,
            current_spaces: None,
            parentheses: 0,
        }
    }

    #[inline]
    fn inner_next(&mut self) -> Option<Result<LocatedToken, DMError>> {
        self.inner.next()
    }

    fn paste_one(&mut self, current: LocatedToken) {
        debug_assert!(self.pasting.is_none());
        self.pasting = Some((current.line, current.column, vec![current.token].into_iter().collect()));
    }

    fn real_next(&mut self, current: LocatedToken) -> Option<Result<LocatedToken, DMError>> {
        let (line, column) = (current.line, current.column);
        let locate = |t| LocatedToken::new(line, column, t);

        // handle whitespace
        match current.token {
            Token::Punct(Punctuation::Newline) => {
                if self.parentheses != 0 {
                    return None
                }
                let is_none = self.current_spaces.is_none();
                self.current_spaces = Some(0);
                if is_none {
                    // only generate a semicolon if there was anything on this line
                    return Some(Ok(locate(Token::Punct(Punctuation::Semicolon))))
                } else {
                    return None
                }
            }
            Token::Punct(Punctuation::Tab) |
            Token::Punct(Punctuation::Space) => {
                if let Some(spaces) = self.current_spaces.as_mut() {
                    *spaces += 1;
                }
                return None
            }
            _ => {}
        }

        match current.token {
            Token::Punct(Punctuation::LBrace) |
            Token::Punct(Punctuation::RBrace) => {
                self.current_spaces = None;
            }
            _ => {}
        }

        // handle indentation
        if let Some(spaces) = self.current_spaces {
            self.current_spaces = None;

            let (indents, new_indents);
            match self.current {
                None => {
                    indents = 0;
                    if spaces == 0 {
                        new_indents = 0;
                        self.current = None;
                    } else {
                        new_indents = 1;
                        self.current = Some((spaces, 1));
                    }
                }
                Some((spaces_per_indent, indents_)) => {
                    indents = indents_;
                    if spaces == 0 {
                        self.current = None;
                        new_indents = 0;
                    } else {
                        if spaces % spaces_per_indent != 0 {
                            return Some(Err(DMError::new(current.line, current.column, "inconsistent indentation")));
                        }
                        new_indents = spaces / spaces_per_indent;
                        self.current = Some((spaces_per_indent, new_indents));
                    }
                }
            }

            if indents + 1 == new_indents {
                // single indent
                self.paste_one(locate(Token::Punct(Punctuation::LBrace)));
            } else if indents < new_indents {
                // multiple indent is an error
                return Some(Err(DMError::new(current.line, current.column, "inconsistent indentation")))
            } else if indents == new_indents + 1 {
                // single unindent
                self.paste_one(locate(Token::Punct(Punctuation::RBrace)));
            } else if indents > new_indents {
                // multiple unindent
                let v: VecDeque<_> = vec![Token::Punct(Punctuation::RBrace); indents - new_indents].into_iter().collect();
                self.pasting = Some((current.line, current.column, v));
            } else {
                // equal
            }
        }

        // handle non-whitespace
        match current.token {
            Token::Punct(Punctuation::LBrace) => {
                self.current = match self.current {
                    None => Some((1, 1)),
                    Some((x, y)) => Some((x, y + 1)),
                };
            }
            Token::Punct(Punctuation::RBrace) => {
                self.current = match self.current {
                    None => return Some(Err(DMError::new(current.line, current.column, "unmatched right brace"))),
                    Some((_, 1)) => None,
                    Some((x, y)) => Some((x, y - 1)),
                };
            }
            Token::Punct(Punctuation::LParen) => {
                self.parentheses += 1;
            }
            Token::Punct(Punctuation::RParen) => {
                self.parentheses -= 1;
            }
            _ => {}
        }
        Some(Ok(current))
    }
}

impl<I: Iterator<Item=Result<LocatedToken, DMError>>> Iterator for IndentProcessor<I> {
    type Item = Result<LocatedToken, DMError>;

    fn next(&mut self) -> Option<Result<LocatedToken, DMError>> {
        loop {
            if let Some(&mut (line, column, ref mut pasting)) = self.pasting.as_mut() {
                if let Some(tok) = pasting.pop_front() {
                    return Some(Ok(LocatedToken::new(line, column, tok)));
                }
            }
            self.pasting = None;
            if let Some(tok) = self.inner_next() {
                if let Some(out) = self.real_next(try_iter!(tok)) {
                    match self.pasting {
                        Some((_, _, ref mut pasting)) => pasting.push_back(try_iter!(out).token),
                        None => return Some(out)
                    }
                }
            } else {
                return None;
            }
        }
    }
}
