//! The lexer/tokenizer.
use std::io::{Read, Bytes};
use std::str::FromStr;
use std::fmt;

use super::{DMError, Location, HasLocation};

macro_rules! table {
    ($(#[$attr:meta])* table $tabname:ident: $repr:ty => $enum_:ident; $($literal:expr, $name:ident;)*) => {
        $(#[$attr])*
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
        pub enum $enum_ {
            $($name,)*
        }

        impl $enum_ {
            #[allow(dead_code)]
            fn value(self) -> $repr {
                match self {
                    $($enum_::$name => $literal,)*
                }
            }
        }

        const $tabname: &'static [($repr, $enum_)] = &[
            $(($literal, $enum_::$name),)*
        ];
    }
}

// (paren) {brace} [bracket]
table! {
    /// A punctuation token recognized by the language.
    table PUNCT_TABLE: &'static [u8] => Punctuation;
    // Order is significant; see read_punct below.
    b"\t",  Tab;
    b"\n",  Newline;
    b" ",   Space;
    b"!",	Not;
    b"!=",	NotEq;
    b"\"",  DoubleQuote;
    b"#",   Hash;
    b"##",  TokenPaste;
    b"%",	Mod;
    b"&",	BitAnd;
    b"&&",	And;
    b"&=",	BitAndAssign;
    b"'",   SingleQuote;
    b"(",	LParen;
    b")",	RParen;
    b"*",	Mul;
    b"**",	Pow;
    b"*=",	MulAssign;
    b"+",	Add;
    b"++",  PlusPlus;
    b"+=",	AddAssign;
    b",",	Comma;
    b"-",	Sub;
    b"--",  MinusMinus;
    b"-=",	SubAssign;
    b".",	Dot;
    b"..",  Super;
    b"...", Ellipsis;
    b"/",	Slash;
    b"/*",	BlockComment;
    b"//",	LineComment;
    b"/=",	DivAssign;
    b":",	Colon;
    b";",	Semicolon;
    b"<",	Less;
    b"<<",	LShift;
    b"<<=",	LShiftAssign;
    b"<=",	LessEq;
    b"<>",	LessGreater;
    b"=",	Assign;
    b"==",	Eq;
    b">",	Greater;
    b">=",	GreaterEq;
    b">>",	RShift;
    b">>=",	RShiftAssign;
    b"?",   QuestionMark;
    b"[",	LBracket;
    b"]",	RBracket;
    b"^",	BitXor;
    b"^=",	BitXorAssign;
    b"{",	LBrace;
    b"{\"", BlockString;
    b"|",	BitOr;
    b"|=",	BitOrAssign;
    b"||",	Or;
    b"}",	RBrace;
    b"~",	BitNot;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// An end-of-file. Exists as a convenience and is not emitted by the lexer.
    Eof,
    /// A punctuation symbol.
    Punct(Punctuation),
    /// A raw identifier or keyword. Indicates whether it is followed by whitespace.
    Ident(String, bool),
    /// A string literal with no interpolation.
    String(String),
    /// Interpolation markers. Strings and expressions in-between are combined.
    InterpStringBegin(String),
    InterpStringPart(String),
    InterpStringEnd(String),
    /// A resource literal, referring to a filename.
    Resource(String),
    /// An integer literal.
    Int(i32),
    /// A floating-point literal.
    Float(f32),
}

impl Token {
    /// Check whether this token should be separated from the previous one when
    /// pretty-printing.
    pub fn separate_from(&self, prev: &Token) -> bool {
        use self::Punctuation::*;
        // space-surrounded tokens
        for &each in &[self, prev] {
            let p = match *each {
                Token::Punct(p) => p,
                _ => continue,
            };
            match p {
                Eq |
                NotEq |
                Mod |
                And |
                BitAndAssign |
                Mul |
                Pow |
                MulAssign |
                Add |
                AddAssign |
                Sub |
                SubAssign |
                DivAssign |
                Colon |
                Less |
                LShift |
                LShiftAssign |
                LessEq |
                LessGreater |
                Assign |
                Greater |
                GreaterEq |
                RShift |
                RShiftAssign |
                QuestionMark |
                BitXorAssign |
                BitOrAssign |
                Or => return true,
                _ => {}
            }
        }

        // space
        match (prev, self) {
            (&Token::Ident(_, true), _) |
            (&Token::Punct(Comma), _) => true,
            (&Token::Ident(_, _), &Token::Punct(_)) |
            (&Token::Ident(_, _), &Token::InterpStringEnd(_)) |
            (&Token::Ident(_, _), &Token::InterpStringPart(_)) => false,
            (&Token::Ident(_, _), _) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;
        match *self {
            Eof => f.write_str("__EOF__"),
            Punct(Punctuation::Tab) => f.write_str("    "),
            Punct(p) => f.write_str(::std::str::from_utf8(p.value()).unwrap()),
            Ident(ref i, _) => f.write_str(i),
            String(ref i) => write!(f, "\"{}\"", i),
            InterpStringBegin(ref i) => write!(f, "\"{}[", i),
            InterpStringPart(ref i) => write!(f, "]{}[", i),
            InterpStringEnd(ref i) => write!(f, "]{}\"", i),
            Resource(ref i) => write!(f, "'{}'", i),
            Int(i) => write!(f, "{}", i),
            Float(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LocatedToken {
    pub location: Location,
    pub token: Token,
}

impl LocatedToken {
    #[inline]
    pub fn new(location: Location, token: Token) -> LocatedToken {
        LocatedToken { location, token }
    }
}

fn is_digit(ch: u8) -> bool {
    ch >= b'0' && ch <= b'9'
}

fn is_ident(ch: u8) -> bool {
    (ch >= b'a' && ch <= b'z') || (ch >= b'A' && ch <= b'Z') || ch == b'_'
}

// Used to track nested string interpolations and know when they end.
struct Interpolation {
    end: &'static [u8],
    bracket_depth: usize,
}

/// The lexer, which serves as a source of tokens through iteration.
pub struct Lexer<R: Read> {
    next: Option<u8>,
    input: Bytes<R>,
    location: Location,
    at_line_head: bool,
    interp_stack: Vec<Interpolation>,
}

impl<R: Read> HasLocation for Lexer<R> {
    fn location(&self) -> Location {
        self.location
    }
}

impl<R: Read> Lexer<R> {
    /// Create a new lexer from a byte stream.
    pub fn new(file_number: u32, source: R) -> Lexer<R> {
        Lexer {
            next: None,
            input: source.bytes(),
            location: Location {
                file: file_number,
                line: 1,
                column: 1,
            },
            at_line_head: true,
            interp_stack: Vec::new(),
        }
    }

    fn next(&mut self) -> Result<Option<u8>, DMError> {
        if let Some(next) = self.next.take() {
            return Ok(Some(next));
        }
        match self.input.next() {
            Some(Ok(ch)) => {
                if ch == b'\n' {
                    self.location.line += 1;
                    self.location.column = 1;
                    self.at_line_head = true;
                } else {
                    if ch != b'\t' && ch != b' ' && self.at_line_head {
                        self.at_line_head = false;
                    }
                    self.location.column += 1;
                }

                Ok(Some(ch))
            }
            Some(Err(_)) => Err(self.error("i/o error")), // TODO
            None => Ok(None),
        }
    }

    fn put_back(&mut self, val: Option<u8>) {
        if self.next.is_some() {
            panic!("cannot put_back twice");
        }
        self.next = val;
    }

    fn skip_until(&mut self, end: &[u8]) -> Result<(), DMError> {
        let mut idx = 0;
        while let Some(ch) = self.next()? {
            if ch == end[idx] {
                idx += 1;
                if idx == end.len() {
                    break
                }
            } else {
                idx = 0;
            }
        }
        Ok(())
    }

    fn read_number_inner(&mut self, first: u8) -> Result<(bool, u32, String), DMError> {
        let mut integer = true;
        let mut exponent = false;
        let mut radix = 10;
        let mut buf = String::new();
        buf.push(first as char);

        if first == b'.' {
            integer = false;
        } else if first == b'0' {
            radix = 8; // hate. let me tell you...
            match self.next()? {
                Some(b'x') => radix = 16,
                ch => self.put_back(ch),
            }
        }
        loop {
            match self.next()? {
                Some(b'_') => {},
                Some(ch) if ch == b'.' || ch == b'e' => {
                    integer = false;
                    exponent |= ch == b'e';
                    buf.push(ch as char);
                }
                Some(ch) if (ch == b'+' || ch == b'-') && exponent => {
                    buf.push(ch as char);
                }
                Some(b'#') => {
                    let (i, n, f) = (self.next()?, self.next()?, self.next()?);
                    if i == Some(b'I') && n == Some(b'N') && f == Some(b'F') {
                        return Ok((false, 10, "inf".to_owned()));
                    } else {
                        return Err(self.error("expected INF"));
                    }
                }
                Some(ch) if is_digit(ch) => buf.push(ch as char),
                ch => { self.put_back(ch); return Ok((integer, radix, buf)) }
            }
        }
    }

    fn read_number(&mut self, first: u8) -> Result<Token, DMError> {
        let (integer, radix, buf) = self.read_number_inner(first)?;
        if integer {
            match i32::from_str_radix(&buf, radix) {
                Ok(val) => Ok(Token::Int(val)),
                Err(_) => Err(self.error(format!("bad base-{} integer: {}", radix, buf))), // TODO
            }
        } else {
            // ignore radix
            match f32::from_str(&buf) {
                Ok(val) => Ok(Token::Float(val)),
                Err(_) => Err(self.error(format!("bad float: {}", buf))), // TODO
            }
        }
    }

    fn read_ident(&mut self, first: u8) -> Result<String, DMError> {
        let mut ident = vec![first];
        loop {
            match self.next()? {
                Some(ch) if is_ident(ch) || is_digit(ch) => ident.push(ch),
                ch => { self.put_back(ch); break }
            }
        }
        String::from_utf8(ident).map_err(|_| self.error("non-utf8 identifier")) // TODO
    }

    fn next_string(&mut self) -> Result<u8, DMError> {
        match self.next() {
            Ok(Some(ch)) => Ok(ch),
            Ok(None) => Err(self.error("unterminated string")),
            Err(e) => Err(e),
        }
    }

    fn read_resource(&mut self) -> Result<String, DMError> {
        let mut buf = Vec::new();

        loop {
            let ch = self.next_string()?;
            if ch == b'\'' {
                break;
            } else {
                buf.push(ch);
            }
        }

        String::from_utf8(buf).map_err(|_| self.error("non-utf8 string")) // TODO
    }

    fn read_string(&mut self, end: &'static [u8], interp_closed: bool) -> Result<Token, DMError> {
        let mut buf = Vec::new();
        let mut backslash = false;
        let mut idx = 0;
        let mut interp_opened = false;

        loop {
            let ch = self.next_string()?;
            if ch == end[idx] && !backslash {
                idx += 1;
                if idx == end.len() {
                    break
                }
                continue
            } else {
                buf.extend_from_slice(&end[..idx]);
                idx = 0;
            }
            match ch {
                b'\r' | b'\n' if backslash => {
                    backslash = false;
                    let next = self.skip_ws(true)?;
                    self.put_back(next)
                },
                /*b'"' | b'\'' | b'\\' | b'[' | b']' if backslash => {
                    backslash = false;
                    buf.push(ch);
                }*/
                ch if backslash => {
                    // escape sequence handling happens at a later stage
                    backslash = false;
                    buf.push(b'\\');
                    buf.push(ch);
                }
                // `backslash` is false hereafter
                b'[' => {
                    self.interp_stack.push(Interpolation {
                        end: end,
                        bracket_depth: 1,
                    });
                    interp_opened = true;
                    break;
                }
                b'\\' => backslash = true,
                ch => buf.push(ch),
            }
        }
        let string = match String::from_utf8(buf) {
            Ok(s) => s,
            Err(_) => return Err(self.error("non-utf8 string")), // TODO
        };
        Ok(match (interp_opened, interp_closed) {
            (true, true) => Token::InterpStringPart(string),
            (true, false) => Token::InterpStringBegin(string),
            (false, true) => Token::InterpStringEnd(string),
            (false, false) => Token::String(string),
        })
    }

    fn read_punct(&mut self, first: u8) -> Result<Option<Punctuation>, DMError> {
        // requires that PUNCT_TABLE be ordered, shorter entries be first,
        // and all entries with >1 character also have their prefix in the table
        let mut items: Vec<_> = PUNCT_TABLE.iter()
            .skip_while(|&&(tok, _)| tok[0] != first)
            .take_while(|&&(tok, _)| tok[0] == first)
            .collect();
        if items.len() == 0 {
            return Ok(None)
        }

        let mut candidate;
        let mut needle = vec![first];
        loop {
            candidate = Some(items[0].1);
            if items.len() == 1 {
                return Ok(candidate)
            }
            match self.next()? {
                Some(b) => needle.push(b),
                None => return Ok(candidate), // EOF
            }
            items.retain(|&&(tok, _)| tok.starts_with(&needle));
            if items.len() == 0 {
                self.put_back(needle.last().cloned());
                return Ok(candidate)
            }
        }
    }

    fn skip_ws(&mut self, skip_newlines: bool) -> Result<Option<u8>, DMError> {
        let mut skip_newlines = if skip_newlines { 2 } else { 0 };
        loop {
            match self.next()? {
                Some(b'\r') => {},
                Some(b' ') |
                Some(b'\t') if !self.at_line_head || skip_newlines > 0 => {},
                Some(b'\n') if skip_newlines == 2 => { skip_newlines = 1; },
                ch => return Ok(ch)
            }
        }
    }
}

impl<R: Read> Iterator for Lexer<R> {
    type Item = Result<LocatedToken, DMError>;

    fn next(&mut self) -> Option<Result<LocatedToken, DMError>> {
        use self::Token::*;
        use self::Punctuation::*;
        let mut skip_newlines = false;
        loop {
            let first = match self.skip_ws(skip_newlines) {
                Ok(Some(t)) => t,
                Ok(None) => {
                    // always end with a newline
                    if !self.at_line_head {
                        self.at_line_head = true;
                        return Some(Ok(LocatedToken {
                            location: self.location(),
                            token: Token::Punct(Punctuation::Newline),
                        }))
                    } else {
                        return None;
                    }
                }
                Err(e) => return Some(Err(e)),
            };
            skip_newlines = false;

            let loc = self.location;
            let locate = |token| LocatedToken::new(loc, token);

            let punct = try_iter!(self.read_punct(first));
            return match punct {
                Some(BlockComment) => {
                    try_iter!(self.skip_until(b"*/"));
                    continue;
                }
                Some(LineComment) => {
                    try_iter!(self.skip_until(b"\n"));
                    Some(Ok(locate(Punct(Newline))))
                }
                Some(SingleQuote) => Some(self.read_resource().map(|s| locate(Resource(s)))),
                Some(DoubleQuote) => Some(self.read_string(b"\"", false).map(locate)),
                Some(BlockString) => Some(self.read_string(b"\"}", false).map(locate)),
                Some(LBracket) => {
                    if let Some(interp) = self.interp_stack.last_mut() {
                        interp.bracket_depth += 1;
                    }
                    Some(Ok(locate(Punct(LBracket))))
                }
                Some(RBracket) => {
                    if let Some(mut interp) = self.interp_stack.pop() {
                        interp.bracket_depth -= 1;
                        if interp.bracket_depth == 0 {
                            return Some(self.read_string(interp.end, true).map(locate));
                        }
                        self.interp_stack.push(interp);
                    }
                    Some(Ok(locate(Punct(RBracket))))
                }
                Some(v) => Some(Ok(locate(Punct(v)))),
                None => match first {
                    b'0'...b'9' => Some(self.read_number(first).map(|n| locate(n))),
                    b'_' | b'a'...b'z' | b'A'...b'Z' => {
                        let ident = match self.read_ident(first) {
                            Ok(ident) => ident,
                            Err(e) => return Some(Err(e)),
                        };
                        let next = try_iter!(self.next());
                        self.put_back(next);
                        let ws = next == Some(b' ') || next == Some(b'\t');
                        Some(Ok(locate(Ident(ident, ws))))
                    }
                    b'\\' => {
                        self.at_line_head = false;
                        skip_newlines = true;
                        continue;
                    }
                    _ => Some(Err(self.error(format!("illegal byte '{}' (0x{:x})", first as char, first))))
                }
            }
        }
    }
}

#[cfg(test)]
fn lex(f: &str) -> Vec<Token> {
    Lexer::new(0, f.as_bytes()).map(|x| x.map(|y| y.token)).collect::<Result<Vec<_>, _>>().unwrap()
}

#[test]
fn floats() {
    assert_eq!(lex("0.08"), vec![Token::Float(0.08)]);
}

#[test]
fn nested_interpolation() {
    println!("{:?}", lex(r#""A[B"C"D]E""#));
}
