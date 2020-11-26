//! The lexer/tokenizer.
use std::io::Read;
use std::str::FromStr;
use std::fmt;
use std::borrow::Cow;

use super::{DMError, Location, HasLocation, FileId, Context, Severity};
use super::docs::*;
use super::ast::Ident;

macro_rules! table {
    (
        $(#[$attr:meta])* table $tabname:ident: $repr:ty => $enum_:ident;
        $($literal:expr, $name:ident $(-> $close:ident)*;)*
    ) => {
        $(#[$attr])*
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
        pub enum $enum_ {
            $(
                $name,
                $($close,)*
            )*
        }

        impl $enum_ {
            fn value(self) -> $repr {
                match self {
                    $(
                        $enum_::$name => $literal,
                        $($enum_::$close => $literal,)*
                    )*
                }
            }

            pub fn single_quoted(self) -> &'static str {
                match self {
                    $(
                        $enum_::$name => concat!("'", $literal, "'"),
                        $($enum_::$close => concat!("'", $literal, "'"),)*
                    )*
                }
            }

            fn close(self) -> Self {
                match self {
                    $(
                        $($enum_::$name => $enum_::$close,)*
                    )*
                    _ => self,
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
    ///
    /// Not all punctuation types will actually appear in the lexer's output;
    /// some (such as comments) are handled internally.
    table PUNCT_TABLE: &'static str => Punctuation;
    // Order is significant; see filter_punct below.
    "\t",  Tab;
    "\n",  Newline;
    " ",   Space;
    "!",   Not;
    "!=",  NotEq;
    "\"",  DoubleQuote;
    "#",   Hash;
    "##",  TokenPaste;
    "%",   Mod;
    "%=",  ModAssign;
    "&",   BitAnd;
    "&&",  And;
    "&=",  BitAndAssign;
    "'",   SingleQuote;
    "(",   LParen;
    ")",   RParen;
    "*",   Mul;
    "**",  Pow;
    "*=",  MulAssign;
    "+",   Add;
    "++",  PlusPlus;
    "+=",  AddAssign;
    ",",   Comma;
    "-",   Sub;
    "--",  MinusMinus;
    "-=",  SubAssign;
    ".",   Dot;
    "..",  Super;
    "...", Ellipsis;
    "/",   Slash;
    "/*",  BlockComment;
    "//",  LineComment;
    "/=",  DivAssign;
    ":",   Colon -> CloseColon;
    ";",   Semicolon;
    "<",   Less;
    "<<",  LShift;
    "<<=", LShiftAssign;
    "<=",  LessEq;
    "<>",  LessGreater;
    "=",   Assign;
    "==",  Eq;
    ">",   Greater;
    ">=",  GreaterEq;
    ">>",  RShift;
    ">>=", RShiftAssign;
    "?",   QuestionMark;
    "?.",  SafeDot;
    "?:",  SafeColon;
    "[",   LBracket;
    "]",   RBracket;
    "^",   BitXor;
    "^=",  BitXorAssign;
    "{",   LBrace;
    "{\"", BlockString;
    "|",   BitOr;
    "|=",  BitOrAssign;
    "||",  Or;
    "}",   RBrace;
    "~",   BitNot;
    "~!",  NotEquiv;
    "~=",  Equiv;
    // Keywords - not checked by read_punct
    "in",  In;
}

impl fmt::Display for Punctuation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.value())
    }
}

/// This lookup table is used to keep `read_punct`, called for essentially each
/// character in the input, blazing fast. The code to generate it is contained
/// in the following test.
static SPEEDY_TABLE: [(usize, usize); 127] = [
    (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 1), (1, 2), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
    (2, 3), (3, 5), (5, 6), (6, 8), (0, 0), (8, 10), (10, 13), (13, 14),
    (14, 15), (15, 16), (16, 19), (19, 22), (22, 23), (23, 26), (26, 29), (29, 33),
    (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (33, 34), (34, 35), (35, 40), (40, 42), (42, 46), (46, 49),
    (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (0, 0), (49, 50), (0, 0), (50, 51), (51, 53), (0, 0),
    (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (0, 0), (53, 55), (55, 58), (58, 59), (59, 62)];

#[test]
fn make_speedy_table() {
    let everything: Vec<&str> = PUNCT_TABLE
        .iter()
        .map(|p| p.0)
        .filter(|s| !s.chars().any(|c| c.is_alphanumeric()))
        .collect();
    for each in everything.iter() {
        assert!(
            each.len() == 1 || everything.contains(&&each[..each.len() - 1]),
            "no prefix: {}",
            each
        );
    }

    let mut table = vec![];
    for (i, (each, _)) in PUNCT_TABLE.iter().enumerate() {
        if each.chars().any(|c| c.is_alphanumeric()) {
            continue;
        }

        let b = each.as_bytes()[0] as usize;
        if b >= table.len() {
            table.resize(b + 1, (0, 0));
        }
        if table[b] == (0, 0) {
            table[b].0 = i;
            table[b].1 = i + 1;
        } else {
            assert!(i >= table[b].0);
            assert_eq!(i, table[b].1, "{}", each);
            table[b].1 = i + 1;
        }
    }

    if &SPEEDY_TABLE[..] != &table[..] {
        panic!(
            "\n\nSpeedy table outdated, replace with:\n\nstatic SPEEDY_TABLE: [(usize, usize); {}] = {:?};\n\n",
            table.len(),
            table
        );
    }
}

#[inline]
fn filter_punct_table(filter: u8) -> &'static [(&'static str, Punctuation)] {
    let &(start, end) = SPEEDY_TABLE.get(filter as usize).unwrap_or(&(0, 0));
    &PUNCT_TABLE[start..end]
}

#[inline]
fn filter_punct<'a>(input: &'a [(&'static str, Punctuation)], filter: &[u8]) -> &'a [(&'static str, Punctuation)] {
    // requires that PUNCT_TABLE be ordered, shorter entries be first,
    // and all entries with >1 character also have their prefix in the table
    let mut start = 0;
    while start < input.len() && !input[start].0.as_bytes().starts_with(filter) {
        start += 1;
    }
    let mut end = start;
    while end < input.len() && input[end].0.as_bytes().starts_with(filter) {
        end += 1;
    }
    //println!("{:?} -> {:?}", filter, &input[start..end]);
    &input[start..end]
}

/// A single DM token.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// An end-of-file. Exists as a convenience and is not emitted by the lexer.
    Eof,
    /// A punctuation symbol.
    Punct(Punctuation),
    /// A raw identifier or keyword. Indicates whether it is followed by whitespace.
    Ident(Ident, bool),
    /// A string literal with no interpolation.
    String(String),
    /// The opening portion of an interpolated string. Followed by an expression.
    InterpStringBegin(String),
    /// An internal portion of an interpolated string. Preceded and followed by an expression.
    InterpStringPart(String),
    /// The closing portion of an interpolated string. Preceded by an expression.
    InterpStringEnd(String),
    /// A resource literal, referring to a filename.
    Resource(String),
    /// An integer literal.
    Int(i32),
    /// A floating-point literal.
    Float(f32),
    /// A documentation comment.
    DocComment(DocComment),
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
                In |
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
            (&Token::Ident(..), &Token::Punct(_)) |
            (&Token::Ident(..), &Token::InterpStringEnd(_)) |
            (&Token::Ident(..), &Token::InterpStringPart(_)) |
            (&Token::Punct(_), &Token::Ident(..)) |
            (&Token::InterpStringBegin(_), &Token::Ident(..)) |
            (&Token::InterpStringPart(_), &Token::Ident(..)) => false,
            (&Token::Ident(..), _) |
            (_, &Token::Ident(..)) => true,
            _ => false,
        }
    }

    /// Check whether this token is whitespace.
    pub fn is_whitespace(&self) -> bool {
        matches!(*self,
            Token::Punct(Punctuation::Tab)
            | Token::Punct(Punctuation::Newline)
            | Token::Punct(Punctuation::Space)
            | Token::Eof
        )
    }

    /// Check whether this token matches a given identifier.
    pub fn is_ident(&self, ident: &str) -> bool {
        match *self {
            Token::Ident(ref i, _) => i == ident,
            _ => false,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;
        match *self {
            Eof => f.write_str("__EOF__"),
            Punct(p) => write!(f, "{}", p),
            Ident(ref i, _) => f.write_str(i),
            String(ref i) => Quote(i).fmt(f),
            InterpStringBegin(ref i) => write!(f, "\"{}[", i),
            InterpStringPart(ref i) => write!(f, "]{}[", i),
            InterpStringEnd(ref i) => write!(f, "]{}\"", i),
            Resource(ref i) => write!(f, "'{}'", i),
            Int(i) => FormatFloat(i as f32).fmt(f),
            Float(i) => FormatFloat(i).fmt(f),
            DocComment(ref c) => write!(f, "{}", c),
        }
    }
}

/// Formatting helper to quote a string according to DM's rules.
///
/// Assumes that escapes within the string have NOT been parsed
pub struct Quote<'a>(pub &'a str);

impl<'a> fmt::Display for Quote<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self.0;
        if s.contains("\"}") {
            write!(f, "@@{}@", s)
        } else if s.contains('"') || s.contains('\n') {
            write!(f, "{{\"{}\"}}", s)
        } else {
            write!(f, "\"{}\"", s)
        }
    }
}

/// Formatting helper to format a float according to DM's rules.
pub struct FormatFloat(pub f32);

impl fmt::Display for FormatFloat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n = self.0;
        if n.is_nan() {
            if n.is_sign_negative() {
                f.write_str("-1.#IND")
            } else {
                f.write_str("1.#IND")
            }
        } else if n.is_infinite() {
            if n.is_sign_negative() {
                f.write_str("-1.#INF")
            } else {
                f.write_str("1.#INF")
            }
        } else if n == 0.0 {
            f.write_str("0")
        } else {
            let exp = n.abs().log10().floor();
            let factor = 10.0f32.powf(5.0 - exp);
            if exp >= 6.0 || exp <= -5.0 {
                let n2 = (n * factor).round() * 1.0e-5;
                let mut precision = 0;
                while precision < 5 && (n2 * 10.0f32.powi(precision)) != (n2 * 10.0f32.powi(precision)).round() {
                    precision += 1;
                }
                write!(f, "{:.*}e{:+04}", precision as usize, n2, exp)
            } else {
                let n2 = (n * factor).round() / factor;
                write!(f, "{}", n2)
            }
        }
    }
}

/// A token with a location attached.
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

fn from_latin1(bytes: &[u8]) -> String {
    let non_ascii = bytes.iter().filter(|&&i| i > 0x7f).count();
    let mut output = String::with_capacity(bytes.len() + non_ascii);
    for &byte in bytes.iter() {
        output.push(byte as char);
    }
    output
}

/// Convert the input bytes to a `String` attempting UTF-8 or falling back to Latin-1.
pub fn from_utf8_or_latin1(bytes: Vec<u8>) -> String {
    match String::from_utf8(bytes) {
        Ok(v) => v,
        Err(e) => from_latin1(e.as_bytes()),
    }
}

/// Convert the input bytes to a `String` attempting UTF-8 or falling back to Latin-1.
pub fn from_utf8_or_latin1_borrowed(bytes: &[u8]) -> Cow<str> {
    match std::str::from_utf8(bytes) {
        Ok(v) => Cow::Borrowed(v),
        Err(_) => Cow::Owned(from_latin1(bytes)),
    }
}

// Used to track nested string interpolations and know when they end.
#[derive(Debug)]
struct Interpolation {
    end: &'static [u8],
    bracket_depth: usize,
}

// Used to track specially-lexed preprocessor directives like #warn
#[derive(Debug, PartialEq, Copy, Clone)]
enum Directive {
    None,
    Hash,
    Ordinary,
    Stringy,
}

fn buffer_read<R: Read>(file: FileId, mut read: R) -> Result<Vec<u8>, DMError> {
    let mut buffer = Vec::new();

    if let Err(error) = read.read_to_end(&mut buffer) {
        let mut tracker = LocationTracker::new(file, buffer.as_slice().into());
        tracker.by_ref().count();
        return Err(DMError::new(tracker.location(), "i/o error reading file").with_cause(error));
    }

    Ok(buffer)
}

/// Attempt to read an entire file into memory, returning a line and column if
/// an I/O error occurs.
pub fn buffer_file(file: FileId, path: &std::path::Path) -> Result<Vec<u8>, DMError> {
    let mut buffer = match std::fs::metadata(path) {
        Ok(metadata) => Vec::with_capacity(metadata.len() as usize),
        Err(_) => Vec::new(),
    };

    let mut read = match std::fs::File::open(path) {
        Ok(read) => read,
        Err(error) => return Err(DMError::new(Location { file, line: 1, column: 1 }, "i/o error opening file").with_cause(error)),
    };

    if let Err(error) = read.read_to_end(&mut buffer) {
        let mut tracker = LocationTracker::new(file, buffer.as_slice().into());
        tracker.by_ref().count();
        return Err(DMError::new(tracker.location(), "i/o error reading file").with_cause(error));
    }

    Ok(buffer)
}

/// A wrapper for an input stream which tracks line and column numbers.
///
/// All characters, including tabs, are considered to occupy one column
/// regardless of position.
///
/// `io::Error`s are converted to `DMError`s which include the location.
pub struct LocationTracker<'a> {
    inner: Cow<'a, [u8]>,
    offset: usize,
    /// The location of the last character returned by `next()`.
    location: Location,
    at_line_end: bool,
}

impl<'a> LocationTracker<'a> {
    pub fn new(file: FileId, inner: Cow<'a, [u8]>) -> LocationTracker<'a> {
        LocationTracker {
            inner,
            offset: 0,
            location: Location {
                file,
                line: 0,
                column: 0,
            },
            at_line_end: true,
        }
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn remaining(&self) -> &[u8] {
        &self.inner[self.offset..]
    }
}

impl<'a> fmt::Debug for LocationTracker<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("LocationTracker")
            // inner omitted
            .field("offset", &self.offset)
            .field("location", &self.location)
            .field("at_line_end", &self.at_line_end)
            .finish()
    }
}

impl<'a> Iterator for LocationTracker<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        if self.at_line_end {
            self.at_line_end = false;
            match self.location.line.checked_add(1) {
                Some(new) => self.location.line = new,
                None => panic!("per-file line limit of {} exceeded", self.location.line),
            }
            self.location.column = 0;
        }

        let ch = match self.inner.get(self.offset) {
            Some(&ch) => ch,
            None => return None,
        };
        self.offset += 1;

        if ch == b'\n' {
            self.at_line_end = true;
        }
        match self.location.column.checked_add(1) {
            Some(new) => self.location.column = new,
            None => panic!("per-line column limit of {} exceeded", self.location.column),
        }
        Some(ch)
    }
}

/// The lexer, which serves as a source of tokens through iteration.
pub struct Lexer<'ctx> {
    context: &'ctx Context,
    input: LocationTracker<'ctx>,
    next: Option<u8>,
    final_newline: bool,
    at_line_head: bool,
    close_allowed: bool,
    directive: Directive,
    interp_stack: Vec<Interpolation>,
}

impl<'ctx> fmt::Debug for Lexer<'ctx> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Lexer")
            .field("context", self.context)
            .field("input", &self.input)
            .field("next", &self.next)
            .field("final_newline", &self.final_newline)
            .field("at_line_head", &self.at_line_head)
            .field("directive", &self.directive)
            .field("interp_stack", &self.interp_stack)
            .finish()
    }
}

impl<'ctx> HasLocation for Lexer<'ctx> {
    #[inline]
    fn location(&self) -> Location {
        self.input.location
    }
}

impl<'ctx> Lexer<'ctx> {
    /// Create a new lexer from a byte stream.
    pub fn new<I: Into<Cow<'ctx, [u8]>>>(context: &'ctx Context, file_number: FileId, input: I) -> Self {
        Lexer {
            context,
            input: LocationTracker::new(file_number, input.into()),
            next: None,
            final_newline: false,
            at_line_head: true,
            close_allowed: true,
            directive: Directive::None,
            interp_stack: Vec::new(),
        }
    }

    /// Create a new lexer from a reader.
    pub fn from_read<R: Read>(context: &'ctx Context, file: FileId, read: R) -> Result<Self, DMError> {
        Ok(Lexer::new(context, file, buffer_read(file, read)?))
    }

    /// Create a new lexer from a reader.
    pub fn from_file(context: &'ctx Context, file: FileId, path: &std::path::Path) -> Result<Self, DMError> {
        Ok(Lexer::new(context, file, buffer_file(file, path)?))
    }

    pub fn remaining(&self) -> &[u8] {
        self.input.remaining()
    }

    fn next(&mut self) -> Option<u8> {
        if let Some(next) = self.next.take() {
            return Some(next);
        }

        let previous_loc = self.location();
        let result = self.input.next();
        if self.location().line > previous_loc.line {
            self.at_line_head = true;
            self.directive = Directive::None;
        }
        match result {
            None => None,
            Some(ch) => {
                if ch != b'\t' && ch != b' ' {
                    self.at_line_head = false;
                }
                Some(ch)
            }
        }
    }

    fn put_back(&mut self, val: Option<u8>) {
        if self.next.is_some() {
            panic!("cannot put_back twice");
        }
        self.next = val;
    }

    fn skip_block_comments(&mut self) -> Option<Token> {
        let mut depth = 1;
        let mut buffer = [0, 0];

        // read the first character and check for being a comment
        let mut comment = None;
        match self.next() {
            // '*' must be tracked to accurately end the block comment, and
            // will be stripped by the documentation parser.
            Some(b'*') => {
                comment = Some(DocComment::new(CommentKind::Block, DocTarget::FollowingItem));
                buffer[1] = b'*';
            }
            // '!' will not be skipped by the documentation parser, and is not
            // important to checking when the block comment has ended.
            Some(b'!') => comment = Some(DocComment::new(CommentKind::Block, DocTarget::EnclosingItem)),
            Some(other) => buffer[1] = other,
            None => {}
        }

        loop {
            // read one character
            buffer[0] = buffer[1];
            match self.next() {
                Some(val) => buffer[1] = val,
                None => {
                    self.context.register_error(self.error("still skipping comments at end of file"));
                    break;
                }
            }

            if buffer == *b"/*" {
                depth += 1;
            } else if buffer == *b"*/" {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }

            if buffer[0] != 0 {
                if let Some(ref mut comment) = comment {
                    comment.text.push(buffer[0] as char);
                }
            }
        }

        comment.filter(|c| !c.text.is_empty()).map(Token::DocComment)
    }

    fn skip_line_comment(&mut self) -> Option<Token> {
        let mut backslash = false;

        // read the first character and check for being a comment
        let mut comment = None;
        match self.next() {
            Some(b'/') => comment = Some(DocComment::new(CommentKind::Line, DocTarget::FollowingItem)),
            Some(b'!') => comment = Some(DocComment::new(CommentKind::Line, DocTarget::EnclosingItem)),
            Some(b'\n') => {
                self.put_back(Some(b'\n'));
                return None;
            }
            Some(b'\\') => backslash = true,
            _ => {}
        }

        while let Some(ch) = self.next() {
            if ch != b'\r' && ch != b'\n' {
                if let Some(ref mut comment) = comment {
                    comment.text.push(ch as char);
                }
            }

            if ch == b'\r' {
                // not listening
            } else if backslash {
                if ch == b'\n' {
                    self.error("backslash in line comment may be commenting out the following line")
                        .set_severity(Severity::Warning)
                        .register(self.context);
                }
                backslash = false;
            } else if ch == b'\n' {
                self.put_back(Some(ch));
                break;
            } else if ch == b'\\' {
                backslash = true;
            }
        }

        comment.map(Token::DocComment)
    }

    fn read_number_inner(&mut self, first: u8) -> (bool, u32, Cow<'static, str>) {
        let mut integer = true;
        let mut exponent = false;
        let mut radix = 10;
        let mut buf = String::new();
        buf.push(first as char);

        if first == b'.' {
            integer = false;
        } else if first == b'0' {
            radix = 8;  // hate. let me tell you...
            match self.next() {
                Some(b'x') => radix = 16,
                ch => self.put_back(ch),
            }
        }
        loop {
            match self.next() {
                Some(b'_') => {},
                Some(ch) if ch == b'.' || ch == b'e' || ch == b'E' => {
                    // E is used for scientific notation
                    // UNLESS we're parsing a hexadecimal literal
                    if radix != 16 {
                        integer = false;
                        exponent |= ch == b'e' || ch == b'E';
                    }
                    buf.push(ch as char);
                }
                Some(ch) if (ch == b'+' || ch == b'-') && exponent => {
                    buf.push(ch as char);
                }
                Some(b'#') => {
                    buf.push('#');  // Keep pushing to `buf` in case of error.
                    let start = buf.len();
                    for _ in 0..3 {
                        if let Some(ch) = self.next() {
                            buf.push(ch as char);
                            if ["I", "IN", "INF", "IND"].contains(&&buf[start..]) {
                                continue;
                            }
                        }
                        // Not what we expected, throw it up the line so that
                        // f32::from_str will error.
                        return (false, 10, buf.into());
                    }
                    if &buf[start..] == "INF" {
                        // Got "1.#INF", change it to "inf" for read_number.
                        return (false, 10, "inf".into());
                    } else {
                        // Got "1.#IND", change it to "NaN" for read_number.
                        return (false, 10, "NaN".into());
                    }
                }
                Some(ch) if (ch as char).is_digit(::std::cmp::max(radix, 10)) => {
                    exponent = false;
                    buf.push(ch as char);
                }
                ch => {
                    self.put_back(ch);
                    return (integer, radix, buf.into());
                }
            }
        }
    }

    fn read_number(&mut self, first: u8) -> Token {
        let (integer, radix, buf) = self.read_number_inner(first);
        if integer {
            let original_error = match i32::from_str_radix(&buf, radix) {
                Ok(val) => return Token::Int(val),
                Err(e) => e,
            };
            // Try to parse it as a float instead - this will catch numbers
            // that are formatted like integers but are out of the range of our
            // integer type.
            if radix == 10 {
                if let Ok(val) = f32::from_str(&buf) {
                    let val_str = val.to_string();
                    if val_str != buf {
                        self.error(format!("precision loss of integer constant: \"{}\" to {}", buf, val))
                            .set_severity(Severity::Warning)
                            .with_errortype("integer_precision_loss")
                            .register(self.context);
                    }
                    return Token::Float(val)
                }
            }
            self.context.register_error(self.error(
                format!("bad base-{} integer \"{}\": {}", radix, buf, original_error)));
            Token::Int(0)  // fallback
        } else {
            // ignore radix
            match f32::from_str(&buf) {
                Ok(val) => Token::Float(val),
                Err(e) => {
                    self.context.register_error(self.error(
                        format!("bad float \"{}\": {}", buf, e)));
                    Token::Float(0.0)  // fallback
                }
            }
        }
    }

    fn read_ident(&mut self, first: u8) -> String {
        // 12 is ~89% of idents, 24 is ~99.5%, 48 is ~100%
        let mut ident = Vec::with_capacity(12);
        ident.push(first);
        loop {
            match self.next() {
                Some(ch) if is_ident(ch) || is_digit(ch) => ident.push(ch),
                ch => {
                    self.put_back(ch);
                    break;
                }
            }
        }
        from_utf8_or_latin1(ident)
    }

    fn read_resource(&mut self) -> String {
        let start_loc = self.location();
        let mut buf = Vec::new();
        let mut backslash = false;
        loop {
            match self.next() {
                Some(ch) if backslash => {
                    backslash = false;
                    buf.push(ch);
                },
                Some(b'\\') => backslash = true,
                Some(b'\'') => break,
                Some(ch) => buf.push(ch),
                None => {
                    self.context.register_error(DMError::new(start_loc, "unterminated resource literal"));
                    break;
                }
            }
        }
        from_utf8_or_latin1(buf)
    }

    fn read_string(&mut self, end: &'static [u8], interp_closed: bool) -> Token {
        let start_loc = self.location();
        let mut buf = Vec::new();
        let mut backslash = false;
        let mut idx = 0;
        let mut interp_opened = false;

        loop {
            let ch = match self.next() {
                Some(ch) => ch,
                None => {
                    self.context.register_error(DMError::new(start_loc, "unterminated string literal"));
                    break;
                }
            };
            if ch == end[idx] && !backslash {
                idx += 1;
                if idx == end.len() {
                    break;
                }
                continue;
            } else if ch == end[0] && !backslash {
                // TODO: this is a hack to fix the '""}' situation
                buf.extend_from_slice(&end[..idx]);
                idx = 1;
            } else {
                buf.extend_from_slice(&end[..idx]);
                idx = 0;
            }
            match ch {
                b'\r' | b'\n' if backslash => {
                    backslash = false;
                    let next = self.skip_ws(true);
                    self.put_back(next);
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
                        end,
                        bracket_depth: 1,
                    });
                    interp_opened = true;
                    break;
                }
                b'\\' => backslash = true,
                ch => buf.push(ch),
            }
        }

        let string = from_utf8_or_latin1(buf);
        match (interp_opened, interp_closed) {
            (true, true) => Token::InterpStringPart(string),
            (true, false) => Token::InterpStringBegin(string),
            (false, true) => Token::InterpStringEnd(string),
            (false, false) => Token::String(string),
        }
    }

    fn read_raw_string_inner(&mut self, terminator: &[u8]) -> Token {
        let start_loc = self.location();
        let mut buf = Vec::new();
        loop {
            match self.next() {
                Some(ch) => buf.push(ch),
                None => {
                    DMError::new(start_loc, "unterminated raw string")
                        .register(self.context);
                    break;
                }
            }
            if buf.ends_with(terminator) {
                let len = buf.len() - terminator.len();
                buf.truncate(len);
                break;
            }
        }
        Token::String(from_utf8_or_latin1(buf))
    }

    fn read_raw_string(&mut self) -> Token {
        // We just got the '@'. Let's see what the next character is.
        match self.next() {
            // @<LF> - error
            Some(b'\n') |
            None => {
                self.error("unterminated raw string").register(self.context);
                Token::String(String::new())
            },
            // @(<terminator string>)<string><terminator string> - no LF in contents
            Some(b'(') => {
                // build terminator until ), then read until that terminator
                let mut terminator = Vec::new();
                loop {
                    match self.next() {
                        Some(b')') => break,
                        Some(ch) => terminator.push(ch),
                        None => {
                            self.error("unterminated raw string terminator").register(self.context);
                            return Token::String(String::new())
                        }
                    }
                }
                if terminator.is_empty() {
                    self.error("empty raw string terminator").register(self.context);
                    return Token::String(String::new())
                }
                self.read_raw_string_inner(&terminator)
            },
            Some(b'{') => match self.next() {
                // @{"<string>"} - LF allowed in contents
                Some(b'"') => self.read_raw_string_inner(b"\"}"),
                // @{<not ">{ -
                other => {
                    self.put_back(other);
                    self.read_raw_string_inner(b"{")
                }
            },
            // @<terminator char><string><terminator char>
            Some(terminator) => self.read_raw_string_inner(&[terminator]),
        }
    }

    fn read_punct(&mut self, first: u8) -> Option<Punctuation> {
        let mut needle = [first, 0, 0, 0, 0, 0, 0, 0];  // poor man's StackVec
        let mut needle_idx = 1;

        let mut items = filter_punct_table(first);
        let mut candidate = None;
        while !items.is_empty() {
            if items[0].0.as_bytes() == &needle[..needle_idx] {
                candidate = Some(items[0].1);
            }
            if items.len() == 1 {
                return candidate;
            }

            match self.next() {
                Some(b) => {
                    needle[needle_idx] = b;
                    needle_idx += 1;
                },
                None => return candidate,  // EOF
            }
            items = filter_punct(items, &needle[..needle_idx]);
        }
        if needle_idx > 1 {
            self.put_back(needle[..needle_idx].last().cloned());
        }
        candidate
    }

    fn check_close(&mut self, mut punct: Punctuation) -> Punctuation {
        let close = punct.close();
        if punct != close {
            let next = self.next();
            match next {
                Some(b'\r') |
                Some(b' ') |
                Some(b'\t') |
                Some(b'\n') => {}
                _ => punct = close,
            }
            self.put_back(next);
        }
        punct
    }

    fn skip_ws(&mut self, skip_newlines: bool) -> Option<u8> {
        let mut skip_newlines = if skip_newlines { 2 } else { 0 };
        loop {
            match self.next() {
                Some(b'\r') => {},
                Some(b' ') |
                Some(b'\t') if !self.at_line_head || skip_newlines > 0 => { self.close_allowed = false; },
                Some(b'\n') if skip_newlines == 2 => { skip_newlines = 1; self.close_allowed = true; },
                ch => return ch
            }
        }
    }
}

impl<'ctx> Iterator for Lexer<'ctx> {
    type Item = LocatedToken;

    fn next(&mut self) -> Option<LocatedToken> {
        use self::Token::*;
        use self::Punctuation::*;
        let mut skip_newlines = false;
        let mut found_illegal = false;
        loop {
            let first = match self.skip_ws(skip_newlines) {
                Some(t) => t,
                None => {
                    // always end with a newline
                    if !self.final_newline {
                        self.final_newline = true;
                        let mut location = self.location();
                        location.column += 1;
                        return Some(LocatedToken {
                            location,
                            token: Token::Punct(Punctuation::Newline),
                        });
                    } else {
                        return None;
                    }
                }
            };
            skip_newlines = false;

            let loc = self.location();
            let locate = |token| LocatedToken::new(loc, token);

            if self.directive == Directive::Stringy {
                self.directive = Directive::None;
                self.put_back(Some(first));
                return Some(locate(self.read_string(b"\n", false)));
            }

            let mut punct = self.read_punct(first);
            if self.close_allowed {
                punct = punct.map(|p| self.check_close(p));
            }
            return match punct {
                Some(Hash) if self.directive == Directive::None => {
                    self.directive = Directive::Hash;
                    Some(locate(Punct(Hash)))
                }
                Some(BlockComment) => {
                    if let Some(t) = self.skip_block_comments() {
                        return Some(locate(t));
                    }
                    continue;
                }
                Some(LineComment) => {
                    if let Some(t) = self.skip_line_comment() {
                        return Some(locate(t));
                    }
                    continue;
                }
                Some(SingleQuote) => Some(locate(Resource(self.read_resource()))),
                Some(DoubleQuote) => Some(locate(self.read_string(b"\"", false))),
                Some(BlockString) => Some(locate(self.read_string(b"\"}", false))),
                Some(LBracket) => {
                    if let Some(interp) = self.interp_stack.last_mut() {
                        interp.bracket_depth += 1;
                    }
                    Some(locate(Punct(LBracket)))
                }
                Some(RBracket) => {
                    if let Some(mut interp) = self.interp_stack.pop() {
                        interp.bracket_depth -= 1;
                        if interp.bracket_depth == 0 {
                            return Some(locate(self.read_string(interp.end, true)));
                        }
                        self.interp_stack.push(interp);
                    }
                    self.close_allowed = true;
                    Some(locate(Punct(RBracket)))
                }
                Some(RParen) => {
                    self.close_allowed = true;
                    Some(locate(Punct(RParen)))
                }
                Some(v) => Some(locate(Punct(v))),
                None => match first {
                    b'0'..=b'9' => Some(locate(self.read_number(first))),
                    b'_' | b'a'..=b'z' | b'A'..=b'Z' => {
                        let ident = self.read_ident(first);
                        let next = self.next();
                        self.put_back(next);
                        let ws = next == Some(b' ') || next == Some(b'\t');
                        if self.directive == Directive::Hash {
                            if ident == "warn" || ident == "error" {
                                self.directive = Directive::Stringy;
                            } else {
                                self.directive = Directive::Ordinary;
                            }
                        }
                        // check keywords
                        for &(name, value) in PUNCT_TABLE.iter() {
                            if name == ident {
                                return Some(locate(Punct(value)));
                            }
                        }
                        self.close_allowed = true;
                        Some(locate(Ident(ident, ws)))
                    }
                    b'\\' => {
                        self.at_line_head = false;
                        skip_newlines = true;
                        continue;
                    }
                    b'@' => Some(locate(self.read_raw_string())),
                    _ => {
                        if !found_illegal {
                            let mut msg = format!("illegal byte 0x{:x}", first);
                            if first >= b' ' && first <= b'~' {
                                use std::fmt::Write;
                                let _ = write!(msg, " ({:?})", first as char);
                            }
                            self.context.register_error(self.error(msg));
                            found_illegal = true;
                        }
                        continue;
                    }
                },
            };
        }
    }
}
