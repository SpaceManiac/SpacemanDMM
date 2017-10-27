//! Minimalist parser which turns a token stream into an object tree.

use super::{DMError, Location, HasLocation};
use super::lexer::{LocatedToken, Token, Punctuation};
use super::objtree::ObjectTree;

pub fn parse<I>(iter: I) -> Result<ObjectTree, DMError> where
    I: IntoIterator<Item=Result<LocatedToken, DMError>>,
    I::IntoIter: HasLocation
{
    let mut parser = Parser::new(iter.into_iter());
    match parser.root() {
        Ok(Some(())) => return Ok(parser.tree),
        _ => {}
    }

    let expected = parser.expected.join(", ");
    let got = parser.next("");
    Err(DMError::new(parser.location(), format!("got {:?}, expected one of: {}", got, expected)))
}

type Ident = String;

// ----------------------------------------------------------------------------
// Error handling

type Status<T> = Result<Option<T>, DMError>;

#[inline]
fn success<T>(t: T) -> Status<T> {
    Ok(Some(t))
}

const SUCCESS: Status<()> = Ok(Some(()));

macro_rules! require {
    ($self:ident.$($rest:tt)*) => {{
        let v = $self.$($rest)*;
        $self.require(v)?
    }}
}

macro_rules! leading {
    ($e:expr) => {
        match $e {
            Some(x) => x,
            None => return Ok(None),
        }
    }
}

// ----------------------------------------------------------------------------
// Path stack and iterator over parts so far

#[derive(Debug, Copy, Clone)]
struct PathStack<'a> {
    parent: Option<&'a PathStack<'a>>,
    parts: &'a [String],
}

impl<'a> PathStack<'a> {
    fn iter(&self) -> PathStackIter<'a> {
        let mut rest = Vec::new();
        let mut current = Some(self);
        while let Some(c) = current {
            rest.push(c.parts);
            current = c.parent;
        }
        PathStackIter {
            current: &[],
            rest,
        }
    }
}

struct PathStackIter<'a> {
    current: &'a [String],
    rest: Vec<&'a [String]>,
}

impl<'a> Iterator for PathStackIter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        loop {
            match {self.current}.split_first() {
                Some((first, rest)) => {
                    self.current = rest;
                    return Some(first);
                }
                None => match self.rest.pop() {
                    Some(c) => self.current = c,
                    None => return None,
                }
            }
        }
    }
}

// ----------------------------------------------------------------------------
// The parser

struct Parser<I> {
    tree: ObjectTree,

    input: I,
    eof: bool,
    next: Option<Token>,
    location: Location,
    expected: Vec<String>,
}

impl<I: HasLocation> HasLocation for Parser<I> {
    fn location(&self) -> Location {
        self.location
    }
}

impl<I> Parser<I> where
    I: Iterator<Item=Result<LocatedToken, DMError>> + HasLocation
{
    fn new(input: I) -> Parser<I> {
        Parser {
            tree: Default::default(),

            input,
            eof: false,
            next: None,
            location: Default::default(),
            expected: Vec::new(),
        }
    }

    // ------------------------------------------------------------------------
    // Basic setup

    fn parse_error<T>(&mut self) -> Result<T, DMError> {
        let expected = self.expected.join(", ");
        let got = self.next("");
        let message;
        if let Ok(got) = got {
            message = format!("got {:?}, expected one of: {}", got, expected);
            self.put_back(got);
        } else {
            message = format!("i/o error, expected one of: {}", expected);
        }
        Err(self.error(message))
    }

    fn require<T>(&mut self, t: Result<Option<T>, DMError>) -> Result<T, DMError> {
        match t {
            Ok(Some(v)) => Ok(v),
            Ok(None) => self.parse_error(),
            Err(e) => Err(e)
        }
    }

    fn next<S: Into<String>>(&mut self, expected: S) -> Result<Token, DMError> {
        let tok = self.next.take().map_or_else(|| match self.input.next() {
            Some(Ok(token)) => {
                self.expected.clear();
                self.location = token.location;
                Ok(token.token)
            }
            Some(Err(e)) => Err(e),
            None => {
                if !self.eof {
                    self.eof = true;
                    Ok(Token::Eof)
                } else {
                    Err(DMError::new(self.location, "read-after-EOF"))
                }
            }
        }, Ok);
        self.expected.push(expected.into());
        tok
    }

	fn put_back(&mut self, tok: Token) {
		if self.next.is_some() {
			panic!("cannot put_back twice")
		}
		self.next = Some(tok);
	}

	fn try_another<T>(&mut self, tok: Token) -> Status<T> {
		self.put_back(tok);
        Ok(None)
	}

	fn exact(&mut self, tok: Token) -> Status<()> {
		let next = self.next(format!("{}", tok))?;
		if next == tok {
            SUCCESS
		} else {
			self.try_another(next)
		}
	}

	fn ident(&mut self) -> Status<Ident> {
		match self.next("identifier")? {
			Token::Ident(i, _) => Ok(Some(i)),
			other => self.try_another(other),
		}
	}

    // ------------------------------------------------------------------------
    // Object tree

    fn path(&mut self) -> Status<(bool, Vec<Ident>)> {
        // path :: '/'? ident (path_sep ident?)*
        // path_sep :: '/' | '.'
        let mut absolute = false;
        let mut parts = Vec::new();

        // handle leading slash
        if let Some(_) = self.exact(Token::Punct(Punctuation::Slash))? {
            absolute = true;
        }

        // expect at least one ident
        parts.push(match self.ident() {
            Ok(Some(i)) => i,
            Ok(None) if !absolute => return Ok(None),
            _ => return self.parse_error(),
        });
        // followed by ('/' ident)*
        loop {
            match self.next("path separator")? {
                Token::Punct(Punctuation::Slash) => {}
                //Token::Punct(Punctuation::Dot) => {}
                //Token::Punct(Punctuation::Colon) => {}
                t => { self.put_back(t); break; }
            }
            if let Some(i) = self.ident()? {
                parts.push(i);
            }
        }

        success((absolute, parts))
    }

    fn tree_entry(&mut self, parent: PathStack) -> Status<()> {
        // tree_entry :: path ';'
        // tree_entry :: path tree_block
        // tree_entry :: path '=' expression ';'
        // tree_entry :: path '(' argument_list ')' ';'
        // tree_entry :: path '(' argument_list ')' code_block

        use super::lexer::Token::*;
        use super::lexer::Punctuation::*;

        // read and calculate the current path
        let (absolute, path) = leading!(self.path()?);
        if absolute && parent.parent.is_some() {
            println!("WARNING: {:?} inside {:?}", path, parent);
        }
        let new_stack = PathStack {
            parent: if absolute { None } else { Some(&parent) },
            parts: &path
        };

        // discard list size declaration
        match self.next("[")? {
            t @ Punct(LBracket) => {
                self.put_back(t);
                require!(self.ignore_group(LBracket, RBracket));
            }
            t => self.put_back(t),
        }

        // read the contents for real
        match self.next("contents")? {
            t @ Punct(LBrace) => {
                self.tree.add_entry(self.location, new_stack.iter())?;
                self.put_back(t);
                require!(self.tree_block(new_stack));
                SUCCESS
            }
            Punct(Assign) => {
                // TODO: read an expression properly
                let mut value = Vec::new();
                loop {
                    match self.next("expression part")? {
                        Punct(Semicolon) => break,
                        other => {
                            self.put_back(other);
                            require!(self.read_any_tt(&mut value));
                        }
                    }
                }
                self.tree.add_var(self.location, new_stack.iter(), value)?;
                SUCCESS
            }
            t @ Punct(LParen) => {
                self.tree.add_proc(self.location, new_stack.iter())?;
                self.put_back(t);
                require!(self.ignore_group(LParen, RParen));
                match self.next("contents2")? {
                    t @ Punct(LBrace) => {
                        self.put_back(t);
                        require!(self.ignore_group(LBrace, RBrace));
                        SUCCESS
                    }
                    t => { self.put_back(t); SUCCESS }
                }
            }
            other => {
                self.tree.add_entry(self.location, new_stack.iter())?;
                self.put_back(other);
                SUCCESS
            }
        }
    }

    fn tree_entries(&mut self, parent: PathStack, terminator: Token) -> Status<()> {
        loop {
            let next = self.next(format!("newline or {:?}", terminator))?;
            if next == terminator || next == Token::Eof {
                break
            } else if next == Token::Punct(Punctuation::Semicolon) {
                continue
            }
            self.put_back(next);
            /*push*/ require!(self.tree_entry(parent));
        }
        SUCCESS
    }

    fn tree_block(&mut self, parent: PathStack) -> Status<()> {
        leading!(self.exact(Token::Punct(Punctuation::LBrace))?);
        Ok(Some(require!(self.tree_entries(parent, Token::Punct(Punctuation::RBrace)))))
    }

    fn root(&mut self) -> Status<()> {
        let root = PathStack { parent: None, parts: &[] };
        self.tree_entries(root, Token::Eof)
    }

    // ------------------------------------------------------------------------
    // Expressions

    // ------------------------------------------------------------------------
    // Procs

    fn read_any_tt(&mut self, target: &mut Vec<Token>) -> Status<()> {
        // read a single arbitrary "token tree", either a group or a single token
        let start = self.next("anything")?;
        let end = match start {
            Token::Punct(Punctuation::LParen) => Punctuation::RParen,
            Token::Punct(Punctuation::LBrace) => Punctuation::RBrace,
            Token::Punct(Punctuation::LBracket) => Punctuation::RBracket,
            other => { target.push(other); return SUCCESS; }
        };
        target.push(start);
        loop {
            match self.next("anything")? {
                Token::Punct(p) if p == end => {
                    target.push(Token::Punct(p));
                    return SUCCESS;
                }
                other => {
                    self.put_back(other);
                    require!(self.read_any_tt(target));
                }
            }
        }
    }

    fn ignore_group(&mut self, left: Punctuation, right: Punctuation) -> Status<()> {
        leading!(self.exact(Token::Punct(left))?);
        let mut depth = 1;
        while depth > 0 {
            let n = self.next("anything")?;
            match n {
                Token::Punct(p) if p == left => depth += 1,
                Token::Punct(p) if p == right => depth -= 1,
                _ => {}
            }
        }
        SUCCESS
    }
}
