use std::io::{Read, Bytes};
use std::str::FromStr;

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
    /// A raw identifier or keyword.
    Ident(String),
    /// A string literal.
    String(String),
    /// A resource literal, referring to a filename.
    Resource(String),
    /// An integer literal.
    Int(i32),
    /// A floating-point literal.
    Float(f32),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LocatedToken {
    pub line: usize,
    pub column: usize,
    pub token: Token,
}

impl LocatedToken {
    #[inline]
    fn new(line: usize, column: usize, token: Token) -> LocatedToken {
        LocatedToken { line, column, token }
    }
}

fn is_digit(ch: u8) -> bool {
    ch >= b'0' && ch <= b'9'
}

fn is_ident(ch: u8) -> bool {
    (ch >= b'a' && ch <= b'z') || (ch >= b'A' && ch <= b'Z') || ch == b'_'
}

/// The lexer, which serves as a source of tokens through iteration.
pub struct Lexer<R: Read> {
    next: Option<u8>,
    input: Bytes<R>,
    line: usize,
    column: usize,
    at_line_head: bool,
}

impl<R: Read> Lexer<R> {
    /// Create a new lexer from a byte stream.
    pub fn new(source: R) -> Lexer<R> {
        Lexer {
            next: None,
            input: source.bytes(),
            line: 1,
            column: 1,
            at_line_head: true,
        }
    }

    fn next(&mut self) -> Result<Option<u8>, DMError> {
        if let Some(next) = self.next.take() {
            return Ok(Some(next));
        }
        match self.input.next() {
            Some(Ok(ch)) => {
                if ch == b'\n' {
                    self.line += 1;
                    self.column = 1;
                    self.at_line_head = true;
                } else {
                    if ch != b'\t' && ch != b' ' && self.at_line_head {
                        self.at_line_head = false;
                    }
                    self.column += 1;
                }

                Ok(Some(ch))
            }
            Some(Err(e)) => Err(DMError::with_cause(self.line, self.column, "i/o error", e)),
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
        let mut radix = 10;
        let mut buf = String::new();
        buf.push(first as char);

        if first == b'.' {
            integer = false;
        } else if first == b'0' {
            radix = 8; // hate. let me tell you...
            match self.next()? {
                Some(b'x') => radix = 16,
                Some(ch) if is_digit(ch) => buf.push(ch as char),
                ch => { self.put_back(ch); return Ok((integer, radix, buf)) }
            }
        }
        loop {
            match self.next()? {
                Some(b'_') => {},
                Some(ch) if ch == b'.' || ch == b'e' => {
                    integer = false;
                    buf.push(ch as char);
                },
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
                Err(e) => Err(DMError::with_cause(self.line, self.column, format!("bad base-{} integer: {}", radix, buf), e)),
            }
        } else {
            if radix != 10 {
                return Err(DMError::new(self.line, self.column, format!("base {} floats not supported", radix)));
            }
            match f32::from_str(&buf) {
                Ok(val) => Ok(Token::Float(val)),
                Err(e) => Err(DMError::with_cause(self.line, self.column, format!("bad float: {}", buf), e)),
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
        String::from_utf8(ident).map_err(|e| DMError::with_cause(self.line, self.column, "non-utf8 identifier", e))
    }

    fn next_string(&mut self) -> Result<u8, DMError> {
        match self.next() {
            Ok(Some(ch)) => Ok(ch),
            Ok(None) => Err(DMError::new(self.line, self.column, "unterminated string")),
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

        String::from_utf8(buf).map_err(|e| DMError::with_cause(self.line, self.column, "non-utf8 string", e))
    }

    fn read_string(&mut self, end: &[u8]) -> Result<String, DMError> {
        let mut buf = Vec::new();
        let mut backslash = false;
        let mut idx = 0;

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
                    let next = self.skip_ws()?;
                    self.put_back(next)
                },
                b'"' | b'\'' | b'\\' if backslash => {
                    backslash = false;
                    buf.push(ch);
                }
                ch if backslash => {
                    // escape sequence handling happens at a later stage
                    backslash = false;
                    buf.push(b'\\');
                    buf.push(ch);
                }
                // `backslash` is false hereafter
                b'\\' => backslash = true,
                ch => buf.push(ch),
            }
        }
        String::from_utf8(buf).map_err(|e| DMError::with_cause(self.line, self.column, "non-utf8 string", e))
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

    fn skip_ws(&mut self) -> Result<Option<u8>, DMError> {
        loop {
            match self.next()? {
                Some(b'\r') => continue,
                Some(b' ') |
                Some(b'\t') |
                Some(b'\n') if !self.at_line_head => continue,
                ch => return Ok(ch)
            }
        }
    }
}

macro_rules! iter_try {
    ($e:expr) => {
        match $e {
            Ok(x) => x,
            Err(e) => return Some(Err(e)),
        }
    }
}

impl<R: Read> Iterator for Lexer<R> {
    type Item = Result<LocatedToken, DMError>;

    fn next(&mut self) -> Option<Result<LocatedToken, DMError>> {
        use self::Token::*;
        use self::Punctuation::*;
        loop {
            let first = match self.skip_ws() {
                Ok(Some(t)) => t,
                Ok(None) => return None,
                Err(e) => return Some(Err(e)),
            };

            let (line, column) = (self.line, self.column);
            let punct = iter_try!(self.read_punct(first));
            return match punct {
                Some(BlockComment) => {
                    iter_try!(self.skip_until(b"*/"));
                    continue;
                }
                Some(LineComment) => {
                    iter_try!(self.skip_until(b"\n"));
                    Some(Ok(LocatedToken::new(line, column, Punct(Newline))))
                }
                Some(SingleQuote) => Some(self.read_resource().map(|s| LocatedToken::new(line, column, Resource(s)))),
                Some(DoubleQuote) => Some(self.read_string(b"\"").map(|s| LocatedToken::new(line, column, String(s)))),
                Some(BlockString) => Some(self.read_string(b"\"}").map(|s| LocatedToken::new(line, column, String(s)))),
                Some(v) => Some(Ok(LocatedToken::new(line, column, Punct(v)))),
                None => match first {
                    b'0'...b'9' => Some(self.read_number(first).map(|n| LocatedToken::new(line, column, n))),
                    b'_' | b'a'...b'z' | b'A'...b'Z' => {
                        let ident = match self.read_ident(first) {
                            Ok(ident) => ident,
                            Err(e) => return Some(Err(e)),
                        };
                        /*let token = match KEYWORD_TABLE.iter().find(|&&(keywd, _)| keywd == &ident) {
                            Some(&(_, keywd)) => Keyword(keywd),
                            None => Ident(ident)
                        };*/
                        let token = Ident(ident);
                        Some(Ok(LocatedToken::new(line, column, token)))
                    }
                    _ => Some(Err(DMError::new(line, column, format!("illegal byte '{}' (0x{:x})", first as char, first))))
                }
            }
        }
    }
}
