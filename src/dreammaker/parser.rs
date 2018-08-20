//! Minimalist parser which turns a token stream into an object tree.

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::ops::Range;
use std::fmt;

use linked_hash_map::LinkedHashMap;

use super::{DMError, Location, HasLocation, Context, Severity, FileId};
use super::lexer::{LocatedToken, Token, Punctuation};
use super::objtree::ObjectTree;
use super::annotation::*;
use super::ast::*;
use super::docs::*;

/// Parse a token stream, in the form emitted by the indent processor, into
/// an object tree.
///
/// Compilation failures will return a best-effort parse, and diagnostics will
/// be registered with the provided `Context`.
pub fn parse<I>(context: &Context, iter: I) -> ObjectTree where
    I: IntoIterator<Item=LocatedToken>
{
    Parser::new(context, iter.into_iter()).parse_object_tree()
}

type Ident = String;

// ----------------------------------------------------------------------------
// Error handling

type Status<T> = Result<Option<T>, DMError>;
// Ok(Some(x)) = the thing was parsed successfully
// Ok(None) = there isn't one of those here, nothing was read, try another
// Err(e) = an unrecoverable error

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
        match $e? {
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

    fn contains(&self, keyword: &str) -> bool {
        self.iter().any(|x| x == keyword)
    }

    fn len(&self) -> usize {
        self.parts.len() + self.parent.as_ref().map_or(0, |p| p.len())
    }

    fn to_vec(&self) -> Vec<String> {
        self.iter().map(|t| t.to_owned()).collect()
    }
}

impl<'a> fmt::Display for PathStack<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref parent) = self.parent {
            write!(fmt, "{}", parent)?;
        }
        for part in self.parts {
            fmt.write_str("/")?;
            fmt.write_str(part)?;
        }
        Ok(())
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
// Operator precedence table

#[derive(Debug, Clone, Copy)]
struct OpInfo {
    strength: Strength,
    token: Punctuation,
    oper: Op,
}

impl OpInfo {
    fn matches(&self, token: &Token) -> bool {
        match *token {
            Token::Punct(p) => self.token == p,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Op {
    BinaryOp(BinaryOp),
    AssignOp(AssignOp),
}

impl Op {
    fn build(self, lhs: Box<Expression>, rhs: Box<Expression>) -> Expression {
        match self {
            Op::BinaryOp(op) => Expression::BinaryOp { op: op, lhs: lhs, rhs: rhs },
            Op::AssignOp(op) => Expression::AssignOp { op: op, lhs: lhs, rhs: rhs },
        }
    }
}

macro_rules! oper_table {
    (@elem ($strength:ident) ($kind:ident, $op:ident)) => {
        OpInfo {
            strength: Strength::$strength,
            token: Punctuation::$op,
            oper: Op::$kind($kind::$op),
        }
    };
    (@elem ($strength:ident) ($kind:ident, $op:ident = $punct:ident)) => {
        OpInfo {
            strength: Strength::$strength,
            token: Punctuation::$punct,
            oper: Op::$kind($kind::$op),
        }
    };
    ($name:ident; $($strength:ident {$($child:tt,)*})*) => {
        #[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
        enum Strength {
            $($strength),*
        }

        const $name: &'static [OpInfo] = &[
            $($(
                oper_table!(@elem ($strength) $child),
            )*)*
        ];
    }
}

// Highest precedence is first in the list, to match the reference.
oper_table! { BINARY_OPS;
    // () . : /        // here . : / are path operators
    // []
    // . : ?. :
    // ~ ! - ++ --     // unary operators
    // **
    Pow {
        (BinaryOp, Pow),
    }
    // * / %
    Mul {
        (BinaryOp, Mul), //
        (BinaryOp, Div = Slash), //
        (BinaryOp, Mod),
    }
    // + -
    Add {
        (BinaryOp, Add),
        (BinaryOp, Sub),
    }
    // < <= > >=
    Compare {
        (BinaryOp, Less),
        (BinaryOp, Greater),
        (BinaryOp, LessEq),
        (BinaryOp, GreaterEq),
    }
    // << >>
    Shift {
        (BinaryOp, LShift),
        (BinaryOp, RShift),
    }
    // == != <> ~= ~!
    Equality {
        (BinaryOp, Eq),
        (BinaryOp, NotEq), //
        (BinaryOp, NotEq = LessGreater),
        (BinaryOp, Equiv),
        (BinaryOp, NotEquiv),
    }
    // & ^ |
    Bitwise {
        (BinaryOp, BitAnd),
        (BinaryOp, BitXor),
        (BinaryOp, BitOr),
    }
    // &&
    And {
        (BinaryOp, And),
    }
    // ||
    Or {
        (BinaryOp, Or),
    }
    // ?               // ternary a ? b : c
    // = += -= -= *= /= %= &= |= ^= <<= >>=
    Assign {
        (AssignOp, Assign),
        (AssignOp, AddAssign),
        (AssignOp, SubAssign),
        (AssignOp, MulAssign),
        (AssignOp, DivAssign),
        (AssignOp, ModAssign),
        (AssignOp, BitAndAssign),
        (AssignOp, BitOrAssign),
        (AssignOp, BitXorAssign),
        (AssignOp, LShiftAssign),
        (AssignOp, RShiftAssign),
    }
    // "in" is special and has different precedence in different contexts
    In {
        // N.B. the RHS of "in" is evaluated before the LHS
        (BinaryOp, In),
    }
}

impl Strength {
    fn right_binding(self) -> bool {
        match self {
            Strength::Assign => true,
            _ => false,
        }
    }
}

// ----------------------------------------------------------------------------
// Token-tree-based skip and recovery handling

#[derive(Debug, Copy, Clone)]
enum TTKind {
    Paren,   // ()
    Brace,   // {}
    Bracket, // []
}

impl TTKind {
    fn from_token(token: &Token) -> Option<TTKind> {
        match *token {
            Token::Punct(Punctuation::LParen) => Some(TTKind::Paren),
            Token::Punct(Punctuation::LBrace) => Some(TTKind::Brace),
            Token::Punct(Punctuation::LBracket) => Some(TTKind::Bracket),
            _ => None,
        }
    }

    fn is_end(&self, token: &Token) -> bool {
        match (self, token) {
            (&TTKind::Paren, &Token::Punct(Punctuation::RParen)) => true,
            (&TTKind::Brace, &Token::Punct(Punctuation::RBrace)) => true,
            (&TTKind::Bracket, &Token::Punct(Punctuation::RBracket)) => true,
            _ => false,
        }
    }
}

// ----------------------------------------------------------------------------
// The parser

#[derive(Debug)]
enum LoopContext {
    None,
    ForLoop,
    ForList,
    ForRange,
    While,
    DoWhile,
}

/// A single-lookahead, recursive-descent DM parser.
///
/// Results are accumulated into an inner `ObjectTree`. To parse an entire
/// environment, use the `parse` or `parse_environment` functions.
pub struct Parser<'ctx, 'an, I> {
    context: &'ctx Context,
    annotations: Option<&'an mut AnnotationTree>,
    tree: ObjectTree,

    input: I,
    eof: bool,
    next: Option<Token>,
    location: Location,
    expected: Vec<Cow<'static, str>>,

    docs_following: DocCollection,
    docs_enclosing: DocCollection,
    module_docs: BTreeMap<FileId, Vec<(u32, DocComment)>>,
    in_docs: usize,

    procs: bool,
    procs_bad: u64,
    procs_good: u64,
}

impl<'ctx, 'an, I> HasLocation for Parser<'ctx, 'an, I> {
    fn location(&self) -> Location {
        self.location
    }
}

impl<'ctx, 'an, I> Parser<'ctx, 'an, I> where
    I: Iterator<Item=LocatedToken>
{
    /// Construct a new parser using the given input stream.
    pub fn new(context: &'ctx Context, input: I) -> Parser<I> {
        Parser {
            context,
            annotations: None,
            tree: ObjectTree::default(),

            input,
            eof: false,
            next: None,
            location: Default::default(),
            expected: Vec::new(),

            docs_following: Default::default(),
            docs_enclosing: Default::default(),
            module_docs: Default::default(),
            in_docs: 0,

            procs: false,
            procs_bad: 0,
            procs_good: 0,
        }
    }

    pub fn enable_procs(&mut self) {
        self.procs = true;
    }

    pub fn annotate_to(&mut self, annotations: &'an mut AnnotationTree) {
        self.annotations = Some(annotations);
        self.procs = true;
    }

    pub fn set_fallback_location(&mut self, fallback: Location) {
        assert!(self.location == Default::default());
        self.location = fallback;
    }

    pub fn parse_object_tree(mut self) -> ObjectTree {
        self.run();
        self.finalize_object_tree()
    }

    pub fn run(&mut self) {
        self.tree.register_builtins();
        let root = self.root();
        if let Err(e) = self.require(root) {
            self.context.register_error(e);
        }
    }

    pub fn take_module_docs(&mut self) -> BTreeMap<FileId, Vec<(u32, DocComment)>> {
        ::std::mem::replace(&mut self.module_docs, Default::default())
    }

    pub fn finalize_object_tree(mut self) -> ObjectTree {
        let procs_total = self.procs_good + self.procs_bad;
        if procs_total > 0 {
            eprintln!("parsed {}/{} proc bodies ({}%)", self.procs_good, procs_total, (self.procs_good * 100 / procs_total));
        }

        let sloppy = self.context.errors().iter().any(|p| p.severity() == Severity::Error);
        self.tree.finalize(self.context, sloppy);
        self.tree
    }

    // ------------------------------------------------------------------------
    // Basic setup

    // Call this to get a DMError in the event of an entry point returning None
    fn describe_parse_error(&mut self) -> DMError {
        let expected = self.expected.join(", ");
        if self.eof {
            return self.error(format!("got EOF, expected one of: {}", expected));
        }
        match self.next("") {
            Ok(got) => {
                let message = format!("got '{}', expected one of: {}", got, expected);
                self.put_back(got);
                self.error(message)
            },
            Err(err) => {
                self.error(format!("i/o error, expected one of: {}", expected)).set_cause(err)
            }
        }
    }

    fn parse_error<T>(&mut self) -> Result<T, DMError> {
        Err(self.describe_parse_error())
    }

    pub fn require<T>(&mut self, t: Result<Option<T>, DMError>) -> Result<T, DMError> {
        match t {
            Ok(Some(v)) => Ok(v),
            Ok(None) => self.parse_error(),
            Err(e) => Err(e)
        }
    }

    fn next<S: Into<Cow<'static, str>>>(&mut self, expected: S) -> Result<Token, DMError> {
        let tok = loop {
            if let Some(next) = self.next.take() {
                break Ok(next);
            }
            match self.input.next() {
                Some(LocatedToken { location, token: Token::DocComment(dc) }) => {
                    match dc.target {
                        DocTarget::EnclosingItem if self.in_docs == 0 => {
                            self.module_docs.entry(location.file).or_default().push((location.line, dc));
                        },
                        DocTarget::EnclosingItem => self.docs_enclosing.push(dc),
                        DocTarget::FollowingItem => self.docs_following.push(dc),
                    }
                }
                Some(token) => {
                    self.expected.clear();
                    self.location = token.location;
                    break Ok(token.token);
                }
                None => {
                    if !self.eof {
                        self.eof = true;
                        break Ok(Token::Eof);
                    } else {
                        break self.parse_error();
                    }
                }
            }
        };
        let what = expected.into();
        if !what.is_empty() && !self.expected.contains(&what) {
            self.expected.push(what);
        }
        tok
    }

    fn put_back(&mut self, tok: Token) {
        if self.next.is_some() {
            panic!("cannot put_back twice")
        }
        self.next = Some(tok);
    }

    fn updated_location(&mut self) -> Location {
        if let Ok(token) = self.next("") {
            self.put_back(token);
        }
        self.location
    }

    #[inline]
    fn annotate<F: FnOnce() -> Annotation>(&mut self, start: Location, f: F) {
        let end = self.updated_location();
        self.annotate_precise(start..end, f);
    }

    fn annotate_precise<F: FnOnce() -> Annotation>(&mut self, range: Range<Location>, f: F) {
        if let Some(dest) = self.annotations.as_mut() {
            dest.insert(range, f());
        }
    }

    fn try_another<T>(&mut self, tok: Token) -> Status<T> {
        self.put_back(tok);
        Ok(None)
    }

    fn exact(&mut self, tok: Token) -> Status<()> {
        let message = if tok == Token::Eof {
            "EOF".to_owned()
        } else {
            format!("'{}'", tok)
        };
        let next = self.next(message)?;
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

    fn ident_in_seq(&mut self, idx: usize) -> Status<Ident> {
        let start = self.updated_location();
        match self.next("identifier")? {
            Token::Ident(i, _) => {
                self.annotate(start, || Annotation::InSequence(idx));
                Ok(Some(i))
            },
            other => self.try_another(other),
        }
    }

    fn exact_ident(&mut self, ident: &'static str) -> Status<()> {
        match self.next(ident)? {
            Token::Ident(ref i, _) if i == ident => SUCCESS,
            other => self.try_another(other),
        }
    }

    // ------------------------------------------------------------------------
    // Doc comment tracking

    fn doc_comment<R, F: FnOnce(&mut Self) -> Status<R>>(&mut self, f: F) -> Status<(DocCollection, R)> {
        use std::mem::replace;

        let enclosing = replace(&mut self.docs_enclosing, Default::default());
        let mut docs = replace(&mut self.docs_following, Default::default());
        self.in_docs += 1;
        let result = f(self);
        self.in_docs -= 1;
        docs.extend(replace(&mut self.docs_enclosing, enclosing));
        match result {
            Ok(Some(found)) => Ok(Some((docs, found))),
            Ok(None) => Ok(None),
            Err(err) => Err(err),
        }
    }

    // ------------------------------------------------------------------------
    // Object tree

    fn tree_path(&mut self) -> Status<(bool, Vec<Ident>)> {
        // path :: '/'? ident ('/' ident?)*
        let mut absolute = false;
        let mut spurious_lead = false;
        let mut parts = Vec::new();
        let start = self.updated_location();

        // handle leading slash
        match self.next("'/'")? {
            Token::Punct(Punctuation::Slash) => absolute = true,
            Token::Punct(p @ Punctuation::Dot) |
            Token::Punct(p @ Punctuation::Colon) => {
                spurious_lead = true;
                self.context.register_error(self.error(format!("path started by '{}', should be unprefixed", p))
                    .set_severity(Severity::Warning));
            }
            t => { self.put_back(t); }
        }
        let mut slash_loc = self.location;

        // expect at least one ident
        match self.ident_in_seq(parts.len())? {
            Some(i) => parts.push(i),
            None if !(absolute || spurious_lead) => return Ok(None),
            None => {
                slash_loc.column += 1;
                self.annotate_precise(slash_loc..slash_loc, || Annotation::IncompleteTreePath(absolute, parts.clone()));
                self.context.register_error(self.error("path has no effect"));
                return success((absolute, Vec::new()));
            }
        }
        // followed by ('/' ident)*
        loop {
            match self.next("'/'")? {
                Token::Punct(Punctuation::Slash) => {}
                Token::Punct(p @ Punctuation::Dot) |
                Token::Punct(p @ Punctuation::Colon) => {
                    self.context.register_error(self.error(format!("path separated by '{}', should be '/'", p))
                        .set_severity(Severity::Warning));
                }
                t => { self.put_back(t); break; }
            }
            let mut slash_loc = self.location;
            if let Some(i) = self.ident_in_seq(parts.len())? {
                parts.push(i);
            } else {
                slash_loc.column += 1;
                self.annotate_precise(slash_loc..slash_loc, || Annotation::IncompleteTreePath(absolute, parts.clone()));
            }
        }

        self.annotate(start, || Annotation::TreePath(absolute, parts.clone()));
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

        let entry_start = self.updated_location();

        // read and calculate the current path
        let (absolute, path) = leading!(self.tree_path());
        let new_stack = PathStack {
            parent: if absolute { None } else { Some(&parent) },
            parts: &path
        };
        if absolute && parent.parent.is_some() {
            self.context.register_error(self.error(format!("nested absolute path: {} inside {}", new_stack, parent))
                .set_severity(Severity::Warning));
        }

        require!(self.var_annotations());

        // read the contents for real
        match self.next("contents")? {
            t @ Punct(LBrace) => {
                // `thing{` - block
                if let Err(e) = self.tree.add_entry(self.location, new_stack.iter(), new_stack.len(), Default::default()) {
                    self.context.register_error(e);
                }
                self.put_back(t);
                let start = self.updated_location();
                let (comment, ()) = require!(self.doc_comment(|this| this.tree_block(new_stack)));
                // TODO: make this duplicate less work?
                if !comment.is_empty() {
                    let _ = self.tree.add_entry(self.location, new_stack.iter(), new_stack.len(), comment);
                }
                self.annotate(start, || Annotation::TreeBlock(new_stack.to_vec()));
                SUCCESS
            }
            Punct(Assign) => {
                // `something=` - var
                let location = self.location;
                // kind of goofy, but allows "enclosing" doc comments at the end of the line
                let (comment, expr) = require!(self.doc_comment(|this| {
                    let expr = require!(this.expression());
                    let _ = require!(this.input_specifier());
                    require!(this.statement_terminator());
                    success(expr)
                }));
                if let Err(e) = self.tree.add_var(location, new_stack.iter(), new_stack.len(), expr, comment) {
                    self.context.register_error(e);
                }
                self.annotate(entry_start, || Annotation::Variable(new_stack.to_vec()));
                SUCCESS
            }
            Punct(LParen) => {
                // `something(` - proc
                let location = self.location;
                let parameters = require!(self.separated(Comma, RParen, None, Parser::proc_parameter));

                // split off a subparser so we can keep parsing the objtree
                // even when the proc body doesn't parse
                let mut body_start = self.location;
                let mut body_tt = Vec::new();
                // check that it doesn't end immediately (empty body)
                let (comment, ()) = require!(self.doc_comment(|this| {
                    body_start = this.updated_location();
                    if let Some(()) = this.statement_terminator()? {
                        body_tt.push(LocatedToken::new(this.location, Punct(Semicolon)));
                    } else {
                        // read an initial token tree
                        require!(this.read_any_tt(&mut body_tt));
                        // if the first token is not an LBrace, it's on one line
                        if body_tt[0].token != Punct(LBrace) {
                            while this.statement_terminator()?.is_none() {
                                require!(this.read_any_tt(&mut body_tt));
                            }
                            body_tt.push(LocatedToken::new(this.location, Punct(Semicolon)));
                        }
                    }
                    SUCCESS
                }));

                match self.tree.add_proc(location, new_stack.iter(), new_stack.len(), parameters) {
                    Ok((idx, proc)) => {
                        proc.docs.extend(comment);
                        // manually performed for borrowck reasons
                        if let Some(dest) = self.annotations.as_mut() {
                            dest.insert(entry_start..body_start, Annotation::ProcHeader(new_stack.to_vec(), idx));
                            dest.insert(body_start..self.location, Annotation::ProcBody(new_stack.to_vec(), idx));
                        }
                    }
                    Err(e) => self.context.register_error(e),
                };

                if self.procs {
                    let result = {
                        let mut subparser: Parser<'ctx, '_, _> = Parser::new(self.context, body_tt.iter().cloned());
                        if let Some(a) = self.annotations.as_mut() {
                            subparser.annotations = Some(&mut *a);
                        }
                        let block = subparser.block(&LoopContext::None);
                        subparser.require(block)
                    };
                    if result.is_ok() {
                        self.procs_good += 1;
                    } else {
                        self.procs_bad += 1;
                    }
                    if let Err(err) = result {
                        self.context.register_error(err);
                    }
                }
                SUCCESS
            }
            other => {
                // usually `thing;` - a contentless declaration
                // TODO: allow enclosing-targeting docs here somehow?
                let comment = ::std::mem::replace(&mut self.docs_following, Default::default());
                if let Err(e) = self.tree.add_entry(self.location, new_stack.iter(), new_stack.len(), comment) {
                    self.context.register_error(e);
                }
                self.put_back(other);
                if new_stack.contains("var") {
                    self.annotate(entry_start, || Annotation::Variable(new_stack.to_vec()));
                }
                SUCCESS
            }
        }
    }

    fn proc_parameter(&mut self) -> Status<Parameter> {
        use super::lexer::Token::*;
        use super::lexer::Punctuation::*;

        if let Some(()) = self.exact(Punct(Ellipsis))? {
            return success(Parameter {
                name: "...".to_owned(),
                location: self.location,
                .. Default::default()
            });
        }

        // `name` or `obj/name` or `var/obj/name` or ...
        let leading_loc = self.updated_location();
        let (_absolute, mut path) = leading!(self.tree_path());
        let name = match path.pop() {
            Some(name) => name,
            None => {
                self.context.register_error(self.describe_parse_error());
                "".to_owned()
            }
        };
        if path.first().map_or(false, |i| i == "var") {
            path.remove(0);
            self.context.register_error(DMError::new(leading_loc, "'var/' is unnecessary here")
                .set_severity(Severity::Hint));
        }
        let location = self.location;
        require!(self.var_annotations());
        // = <expr>
        let default = if let Some(()) = self.exact(Punct(Assign))? {
            Some(require!(self.expression()))
        } else {
            None
        };
        let (input_type, in_list) = require!(self.input_specifier());
        success(Parameter {
            path, name, default, input_type, in_list, location
        })
    }

    fn tree_entries(&mut self, parent: PathStack, terminator: Token) -> Status<()> {
        loop {
            let message = if terminator == Token::Eof {
                "newline".to_owned()
            } else {
                format!("newline, '{}'", terminator)
            };
            let next = self.next(message)?;
            if next == terminator || next == Token::Eof {
                break
            } else if next == Token::Punct(Punctuation::Semicolon) {
                continue
            }
            self.put_back(next);
            require!(self.tree_entry(parent));
        }
        SUCCESS
    }

    fn tree_block(&mut self, parent: PathStack) -> Status<()> {
        leading!(self.exact(Token::Punct(Punctuation::LBrace)));
        Ok(Some(require!(self.tree_entries(parent, Token::Punct(Punctuation::RBrace)))))
    }

    fn root(&mut self) -> Status<()> {
        let root = PathStack { parent: None, parts: &[] };
        self.tree_entries(root, Token::Eof)
    }

    // ------------------------------------------------------------------------
    // Statements

    /// Parse list size declarations.
    fn var_annotations(&mut self) -> Status<()> {
        use super::lexer::Token::Punct;
        use super::lexer::Punctuation::*;
        // TODO: parse the declarations as expressions rather than giving up
        while let Some(()) = self.exact(Punct(LBracket))? {
            self.put_back(Punct(LBracket));
            require!(self.ignore_group(LBracket, RBracket));
        }
        SUCCESS
    }

    /// Parse an optional 'as' input_type and 'in' expression pair.
    fn input_specifier(&mut self) -> Status<(InputType, Option<Expression>)> {
        // as obj|turf
        let input_type = if let Some(()) = self.exact_ident("as")? {
            require!(self.input_type())
        } else {
            InputType::default()
        };
        // `in view(7)` or `in list("a", "b")` or ...
        let in_list = if let Some(()) = self.exact(Token::Punct(Punctuation::In))? {
            Some(require!(self.expression()))
        } else {
            None
        };
        success((input_type, in_list))
    }

    /// Parse a verb input type. Used by proc params and the input() form.
    fn input_type(&mut self) -> Status<InputType> {
        let ident = leading!(self.ident());
        let mut as_what = match InputType::from_str(&ident) {
            Some(what) => what,
            None => {
                self.context.register_error(self.error(format!("bad input type: '{}'", ident)));
                InputType::default()
            }
        };
        while let Some(()) = self.exact(Token::Punct(Punctuation::BitOr))? {
            let ident = require!(self.ident());
            match InputType::from_str(&ident) {
                Some(what) => as_what |= what,
                None => {
                    self.context.register_error(self.error(format!("bad input type: '{}'", ident)));
                }
            }
        }
        success(as_what)
    }

    /// Parse a block
    fn block(&mut self, loop_ctx: &LoopContext) -> Status<Vec<Statement>> {
        let mut vars = Vec::new();
        let result = if let Some(()) = self.exact(Token::Punct(Punctuation::LBrace))? {
            let mut statements = Vec::new();
            loop {
                if let Some(()) = self.exact(Token::Punct(Punctuation::RBrace))? {
                    break;
                } else if let Some(()) = self.exact(Token::Punct(Punctuation::Semicolon))? {
                    continue;
                } else {
                    statements.push(require!(self.statement(loop_ctx, &mut vars)));
                }
            }
            statements
        } else if let Some(()) = self.statement_terminator()? {
            // empty blocks: proc/foo();
            Vec::new()
        } else {
            // and one-line blocks: if(1) neat();
            let statement = require!(self.statement(loop_ctx, &mut vars));
            vec![statement]
        };
        for (loc, var_type, name) in vars {
            self.annotate(loc, || Annotation::LocalVarScope(var_type, name));
        }
        success(result)
    }

    fn statement(&mut self, loop_ctx: &LoopContext, vars: &mut Vec<(Location, VarType, String)>) -> Status<Statement> {
        // BLOCK STATEMENTS
        if let Some(()) = self.exact_ident("if")? {
            // statement :: 'if' '(' expression ')' block ('else' 'if' '(' expression ')' block)* ('else' block)?
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let expr = require!(self.expression());
            require!(self.exact(Token::Punct(Punctuation::RParen)));
            let block = require!(self.block(loop_ctx));
            let mut arms = vec![(expr, block)];

            let mut else_arm = None;
            self.skip_phantom_semicolons()?;
            while let Some(()) = self.exact_ident("else")? {
                if let Some(()) = self.exact_ident("if")? {
                    require!(self.exact(Token::Punct(Punctuation::LParen)));
                    let expr = require!(self.expression());
                    require!(self.exact(Token::Punct(Punctuation::RParen)));
                    let block = require!(self.block(loop_ctx));
                    arms.push((expr, block));
                } else {
                    else_arm = Some(require!(self.block(loop_ctx)));
                    break
                }
                self.skip_phantom_semicolons()?;
            }

            success(Statement::If(arms, else_arm))
        } else if let Some(()) = self.exact_ident("while")? {
            // statement :: 'while' '(' expression ')' block
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let expr = require!(self.expression());
            require!(self.exact(Token::Punct(Punctuation::RParen)));
            success(Statement::While(expr, require!(self.block(&LoopContext::While))))
        } else if let Some(()) = self.exact_ident("do")? {
            // statement :: 'do' block 'while' '(' expression ')' ';'
            let block = require!(self.block(&LoopContext::DoWhile));
            self.skip_phantom_semicolons()?;
            require!(self.exact_ident("while"));
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let expr = require!(self.expression());
            require!(self.exact(Token::Punct(Punctuation::RParen)));
            require!(self.statement_terminator());
            success(Statement::DoWhile(block, expr))
        } else if let Some(()) = self.exact_ident("for")? {
            // for (Var [as Type] [in List]) Statement
            // for (Init, Test, Inc) Statement
            // for (Var in Low to High)
            // for (Var = Low to High)
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let init = self.simple_statement(true, vars)?;
            if let Some(()) = self.comma_or_semicolon()? {
                // three-pronged loop form ("for loop")
                let test = self.expression()?;
                require!(self.comma_or_semicolon());
                let inc = self.simple_statement(false, vars)?;
                require!(self.exact(Token::Punct(Punctuation::RParen)));
                success(Statement::ForLoop {
                    init: init.map(Box::new),
                    test,
                    inc: inc.map(Box::new),
                    block: require!(self.block(&LoopContext::ForLoop)),
                })
            } else if let Some(init) = init {
                // in-list form ("for list")
                let (var_type, name) = match init {
                    // this is a really terrible way to do this
                    Statement::Var(VarStatement { var_type, name, value: Some(value) }) => {
                        // for(var/a = 1 to
                        require!(self.exact_ident("to"));
                        let rhs = require!(self.expression());
                        return success(require!(self.for_range(Some(var_type), name, value, rhs)));
                    },
                    Statement::Var(VarStatement { var_type, name, value: None }) => { (Some(var_type), name) },
                    Statement::Expr(Expression::BinaryOp { op: BinaryOp::In, lhs, rhs }) => {
                        let name = match lhs.into_term() {
                            Some(Term::Ident(name)) => name,
                            _ => return Err(self.error("for-list must start with variable")),
                        };
                        // Explicit move is necessary because rustc becomes
                        // confused when matching on the *rhs lvalue, thinking
                        // moving the LHS also moves the RHS. This fails:
                        //   let a: Box<(NonCopy, NonCopy)>;
                        //   let (b, c) = *a;
                        match {*rhs} {
                            Expression::BinaryOp { op: BinaryOp::To, lhs, rhs } => {
                                return success(require!(self.for_range(None, name, *lhs, *rhs)));
                            },
                            rhs => {
                                // I love code duplication, don't you?
                                require!(self.exact(Token::Punct(Punctuation::RParen)));
                                return success(Statement::ForList {
                                    var_type: None,
                                    name,
                                    input_type: InputType::default(),
                                    in_list: Some(rhs),
                                    block: require!(self.block(&LoopContext::ForList)),
                                });
                            }
                        }
                    },
                    Statement::Expr(expr) => match expr.into_term() {
                        Some(Term::Ident(name)) => (None, name),
                        _ => return Err(self.error("for-list must start with variable")),
                    },
                    _ => return Err(self.error("for-list must start with variable")),
                };

                let input_type = if let Some(()) = self.exact_ident("as")? {
                    // for(var/a as obj
                    require!(self.input_type())
                } else {
                    InputType::default()
                };

                let in_list = if let Some(()) = self.exact(Token::Punct(Punctuation::In))? {
                    let value = require!(self.expression());
                    if let Some(()) = self.exact_ident("to")? {
                        let rhs = require!(self.expression());
                        return success(require!(self.for_range(var_type, name, value, rhs)));
                    }
                    Some(value)
                } else {
                    None
                };

                require!(self.exact(Token::Punct(Punctuation::RParen)));
                success(Statement::ForList {
                    var_type,
                    name,
                    input_type,
                    in_list,
                    block: require!(self.block(&LoopContext::ForList)),
                })
            } else {
                Err(self.error("for-in-list must start with variable"))
            }
        } else if let Some(()) = self.exact_ident("spawn")? {
            let expr;
            if let Some(()) = self.exact(Token::Punct(Punctuation::LParen))? {
                expr = self.expression()?;
                require!(self.exact(Token::Punct(Punctuation::RParen)));
            } else {
                expr = None;
            }
            success(Statement::Spawn(expr, require!(self.block(&LoopContext::None))))
        } else if let Some(()) = self.exact_ident("switch")? {
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let expr = require!(self.expression());
            require!(self.exact(Token::Punct(Punctuation::RParen)));
            require!(self.exact(Token::Punct(Punctuation::LBrace)));
            let mut cases = Vec::new();
            while let Some(()) = self.exact_ident("if")? {
                require!(self.exact(Token::Punct(Punctuation::LParen)));
                let what = require!(self.separated(Punctuation::Comma, Punctuation::RParen, None, Parser::case));
                if what.is_empty() {
                    self.context.register_error(self.error("switch case cannot be empty"));
                }
                let block = require!(self.block(loop_ctx));
                cases.push((what, block));
            }
            let default = if let Some(()) = self.exact_ident("else")? {
                Some(require!(self.block(loop_ctx)))
            } else {
                None
            };
            require!(self.exact(Token::Punct(Punctuation::RBrace)));
            success(Statement::Switch(expr, cases, default))
        } else if let Some(()) = self.exact_ident("try")? {
            let try_block = require!(self.block(loop_ctx));
            self.skip_phantom_semicolons()?;
            require!(self.exact_ident("catch"));
            let catch_params;
            if let Some(()) = self.exact(Token::Punct(Punctuation::LParen))? {
                catch_params = require!(self.separated(Punctuation::Comma, Punctuation::RParen, None, |this| {
                    // TODO: improve upon this cheap approximation
                    success(leading!(this.tree_path()).1)
                }));
            } else {
                catch_params = Vec::new();
            }
            let catch_block = require!(self.block(loop_ctx));
            success(Statement::TryCatch { try_block, catch_params, catch_block })
        // SINGLE-LINE STATEMENTS
        } else if let Some(()) = self.exact_ident("set")? {
            let name = require!(self.ident());
            let mode = if let Some(()) = self.exact(Token::Punct(Punctuation::Assign))? {
                SettingMode::Assign
            } else if let Some(()) = self.exact(Token::Punct(Punctuation::In))? {
                SettingMode::In
            } else {
                return self.parse_error();
            };
            let value = require!(self.expression());
            require!(self.statement_terminator());
            // TODO: warn on weird values for these
            success(Statement::Setting(name, mode, value))
        } else if let Some(()) = self.exact_ident("break")? {
            let label = self.ident()?;
            require!(self.statement_terminator());
            success(Statement::Break(label))
        } else if let Some(()) = self.exact_ident("continue")? {
            let label = self.ident()?;
            require!(self.statement_terminator());
            success(Statement::Continue(label))
        } else {
            let result = leading!(self.simple_statement(false, vars));

            // check for a label `ident:`
            if let Statement::Expr(ref expr) = result {
                if let Some(Term::Ident(ref name)) = expr.as_term() {
                    if let Some(()) = self.exact(Token::Punct(Punctuation::Colon))? {
                        // it's a label! check for a block
                        return success(Statement::Label(name.to_owned(), require!(self.block(loop_ctx))));
                    }
                }
            }

            require!(self.statement_terminator());
            success(result)
        }
    }

    // Handle if(1){a=1;b=2} without a trailing semicolon
    fn statement_terminator(&mut self) -> Status<()> {
        match self.next("';'")? {
            Token::Punct(Punctuation::Semicolon) => SUCCESS,
            p @ Token::Punct(Punctuation::RBrace) => {
                //eprintln!("{:?} instead of semicolon, rbrace (soft)", self.location);
                self.put_back(p);
                SUCCESS
            }
            other => {
                //eprintln!("{:?} instead of semicolon, {:?}", self.location, other);
                self.try_another(other)
            }
        }
    }

    fn skip_phantom_semicolons(&mut self) -> Result<(), DMError> {
        // Indent processor inserts these semicolons which should be ignored:
        //   if(cond){block}  ;
        //   else if(cond){block}  ;
        //   else {block}  ;
        // In other situations, we really want those semicolons:
        //   thing1 = /obj{name="prefab"}  ;
        //   thing2 = "foo"  ;
        // So it's easiest to just ignore them when it makes sense.
        while let Some(()) = self.exact(Token::Punct(Punctuation::Semicolon))? {}
        Ok(())
    }

    // Single-line statements. Can appear in for loops. Followed by a semicolon.
    fn simple_statement(&mut self, in_for: bool, vars: &mut Vec<(Location, VarType, String)>) -> Status<Statement> {
        if let Some(()) = self.exact_ident("var")? {
            // statement :: 'var' type_path name ('=' value)
            let mut var_stmts = Vec::new();
            loop {
                let type_path_start = self.location();
                let (_, mut tree_path) = require!(self.tree_path());
                let name = match tree_path.pop() {
                    Some(name) => name,
                    None => return Err(self.error("'var' must be followed by a name"))
                };

                require!(self.var_annotations());

                let var_type = tree_path.into_iter().collect::<VarType>();
                if var_type.is_tmp {
                    self.context.register_error(DMError::new(type_path_start, "var/tmp has no effect here")
                        .set_severity(Severity::Warning));
                }

                if self.annotations.is_some() {
                    vars.push((self.location, var_type.clone(), name.clone()));
                }

                let value = if let Some(()) = self.exact(Token::Punct(Punctuation::Assign))? {
                    Some(require!(self.expression()))
                } else {
                    None
                };
                let (input_types, in_list) = if !in_for {
                    require!(self.input_specifier())
                } else {
                    (InputType::default(), None)
                };
                if !input_types.is_empty() || in_list.is_some() {
                    self.context.register_error(self.error("input specifier has no effect here")
                        .set_severity(Severity::Warning));
                }

                var_stmts.push(VarStatement { var_type, name, value });
                if in_for || self.exact(Token::Punct(Punctuation::Comma))?.is_none() {
                    break
                }
            }
            if var_stmts.len() == 1 {
                success(Statement::Var(var_stmts.remove(0)))
            } else {
                success(Statement::Vars(var_stmts))
            }
        } else if let Some(()) = self.exact_ident("return")? {
            // statement :: 'return' expression ';'
            let expression = self.expression()?;
            success(Statement::Return(expression))
        } else if let Some(()) = self.exact_ident("throw")? {
            // statement :: 'throw' expression ';'
            let expression = require!(self.expression());
            success(Statement::Throw(expression))
        // EXPRESSION STATEMENTS
        } else {
            // statement :: expression ';'
            let expr = leading!(self.expression());
            success(Statement::Expr(expr))
        }
    }

    // for(var/a = 1 to 20
    // for(var/a in 1 to 20
    fn for_range(&mut self, var_type: Option<VarType>, name: String, start: Expression, end: Expression) -> Status<Statement> {
        // step 2
        let step = if let Some(()) = self.exact_ident("step")? {
            Some(require!(self.expression()))
        } else {
            None
        };
        // )
        require!(self.exact(Token::Punct(Punctuation::RParen)));
        // {...}
        success(Statement::ForRange {
            var_type,
            name,
            start,
            end,
            step,
            block: require!(self.block(&LoopContext::ForRange)),
        })
    }

    fn comma_or_semicolon(&mut self) -> Status<()> {
        if let Some(()) = self.exact(Token::Punct(Punctuation::Comma))? {
            SUCCESS
        } else if let Some(()) = self.exact(Token::Punct(Punctuation::Semicolon))? {
            SUCCESS
        } else {
            Ok(None)
        }
    }

    fn case(&mut self) -> Status<Case> {
        let first = require!(self.expression());
        if let Some(()) = self.exact_ident("to")? {
            success(Case::Range(first, require!(self.expression())))
        } else {
            success(Case::Exact(first))
        }
    }

    // ------------------------------------------------------------------------
    // Expressions

    fn path_separator(&mut self) -> Status<PathOp> {
        success(match self.next("path separator")? {
            Token::Punct(Punctuation::Slash) => PathOp::Slash,
            Token::Punct(Punctuation::Dot) => PathOp::Dot,
            Token::Punct(Punctuation::CloseColon) => PathOp::Colon,
            other => return self.try_another(other),
        })
    }

    // distinct from a tree_path, path must begin with a path separator and can
    // use any path separator rather than just slash, AND can be followed by vars
    fn prefab(&mut self) -> Status<Prefab> {
        self.prefab_ex(Vec::new())
    }

    fn prefab_ex(&mut self, mut parts: TypePath) -> Status<Prefab> {
        // path :: path_sep ident (path_sep ident?)*
        // path_sep :: '/' | '.' | ':'
        let start = self.updated_location();

        // expect at least one path element
        let sep = match self.path_separator()? {
            Some(sep) => sep,
            None if !parts.is_empty() => return Ok(Some(Prefab::from(parts))),
            None => return Ok(None),
        };
        let mut separator_loc = self.location;
        if let Some(ident) = self.ident_in_seq(parts.len())? {
            parts.push((sep, ident));
        } else {
            separator_loc.column += 1;
            self.annotate_precise(separator_loc..separator_loc, || Annotation::IncompleteTypePath(parts.clone(), sep));
        }

        // followed by more path elements, empty ones ignored
        while let Some(sep) = self.path_separator()? {
            let mut separator_loc = self.location;
            if let Some(ident) = self.ident_in_seq(parts.len())? {
                parts.push((sep, ident));
            } else {
                separator_loc.column += 1;
                self.annotate_precise(separator_loc..separator_loc, || Annotation::IncompleteTypePath(parts.clone(), sep));
            }
        }

        // avoid problems with returning an empty Vec
        if parts.is_empty() {
            parts.push((PathOp::Slash, "PARSE_ERROR".to_owned()));
        }

        self.annotate(start, || Annotation::TypePath(parts.clone()));

        // parse vars if we find them
        let mut vars = LinkedHashMap::default();
        if let Some(()) = self.exact(Token::Punct(Punctuation::LBrace))? {
            self.separated(Punctuation::Semicolon, Punctuation::RBrace, Some(()), |this| {
                let key = require!(this.ident());
                require!(this.exact(Token::Punct(Punctuation::Assign)));
                let value = require!(this.expression());
                vars.insert(key, value);
                SUCCESS
            })?;
        }

        success(Prefab { path: parts, vars })
    }

    /// Parse an expression at the current position.
    pub fn expression(&mut self) -> Status<Expression> {
        self.expression_ex(false)
    }

    fn expression_ex(&mut self, in_ternary: bool) -> Status<Expression> {
        let mut expr = leading!(self.group(in_ternary));
        loop {
            // try to read the next operator
            let next = self.next("operator")?;
            let &info = match BINARY_OPS.iter().find(|op| op.matches(&next)) {
                Some(info) => info,
                None => {
                    self.put_back(next);
                    break;
                }
            };

            // trampoline high-strength expression parts as the lhs of the newly found op
            expr = require!(self.expression_part(expr, info, in_ternary));
        }

        // TODO: this needs to be worked into the precedence table somehow
        if let Some(()) = self.exact(Token::Punct(Punctuation::QuestionMark))? {
            let if_ = require!(self.expression_ex(true));
            match self.next("':'")? {
                Token::Punct(Punctuation::Colon) |
                Token::Punct(Punctuation::CloseColon) => {}
                _ => return self.parse_error(),
            }
            let else_ = require!(self.expression());
            expr = Expression::TernaryOp {
                cond: Box::new(expr),
                if_: Box::new(if_),
                else_: Box::new(else_),
            };
        }

        success(expr)
    }

    fn expression_part(&mut self, lhs: Expression, prev_op: OpInfo, in_ternary: bool) -> Status<Expression> {
        use std::cmp::Ordering;

        let mut bits = vec![lhs];
        let mut ops = vec![prev_op.oper];
        let mut rhs = require!(self.group(in_ternary));
        loop {
            // try to read the next operator...
            let next = self.next("operator")?;
            let &info = match BINARY_OPS.iter().find(|op| op.matches(&next)) {
                Some(info) => info,
                None => {
                    self.put_back(next);
                    break;
                }
            };

            // Strength is in reverse order: A < B means A binds tighter
            match info.strength.cmp(&prev_op.strength) {
                Ordering::Less => {
                    // the operator is stronger than us... recurse down
                    rhs = require!(self.expression_part(rhs, info, in_ternary));
                }
                Ordering::Greater => {
                    // the operator is weaker than us... return up
                    self.put_back(Token::Punct(info.token));
                    break;
                }
                Ordering::Equal => {
                    // the same strength... push it to the list
                    ops.push(info.oper);
                    bits.push(rhs);
                    rhs = require!(self.group(in_ternary));
                }
            }
        }

        // Handle ternary ops... they should have their own precedence or else.
        // TODO: A?B:C should probably be handled here as well.
        if prev_op.token == Punctuation::In {
            // "in" is optionally ternary: (x in 1 to 5)
            if let Some(()) = self.exact_ident("to")? {
                rhs = Expression::BinaryOp {
                    op: BinaryOp::To,
                    lhs: Box::new(rhs),
                    rhs: Box::new(require!(self.expression_ex(in_ternary))),
                };
                // "step" could appear here but doesn't actually do anything.
                // In for statements it is parsed by `for_range`.
            }
        }

        // everything in 'ops' should be the same strength
        success(if prev_op.strength.right_binding() {
            let mut result = rhs;
            for (op, bit) in ops.into_iter().zip(bits.into_iter()).rev() {
                result = op.build(Box::new(bit), Box::new(result));
            }
            result
        } else {
            let mut iter = bits.into_iter();
            let mut ops_iter = ops.into_iter();
            let mut result = iter.next().unwrap();
            for (item, op) in iter.zip(&mut ops_iter) {
                result = op.build(Box::new(result), Box::new(item));
            }
            ops_iter.next().unwrap().build(Box::new(result), Box::new(rhs))
        })
    }

    // parse an Expression::Base (unary ops, term, follows)
    fn group(&mut self, in_ternary: bool) -> Status<Expression> {
        // read unary ops
        let mut unary_ops = Vec::new();
        loop {
            match self.next("operator")? {
                Token::Punct(Punctuation::Sub) => unary_ops.push(UnaryOp::Neg),
                Token::Punct(Punctuation::Not) => unary_ops.push(UnaryOp::Not),
                Token::Punct(Punctuation::BitNot) => unary_ops.push(UnaryOp::BitNot),
                Token::Punct(Punctuation::PlusPlus) => unary_ops.push(UnaryOp::PreIncr),
                Token::Punct(Punctuation::MinusMinus) => unary_ops.push(UnaryOp::PreDecr),
                other => { self.put_back(other); break }
            }
        }

        let mut belongs_to = Vec::new();
        let term = if unary_ops.len() > 0 {
            require!(self.term(&mut belongs_to))
        } else {
            leading!(self.term(&mut belongs_to))
        };

        // Read follows
        let mut follow = Vec::new();
        loop {
            match self.next("operator")? {
                Token::Punct(Punctuation::PlusPlus) => unary_ops.push(UnaryOp::PostIncr),
                Token::Punct(Punctuation::MinusMinus) => unary_ops.push(UnaryOp::PostDecr),
                other => {
                    self.put_back(other);
                    match self.follow(&mut belongs_to, in_ternary)? {
                        Some(f) => follow.push(f),
                        None => break,
                    }
                }
            }
        }

        // This has the effect of stripping unnecessary parentheses, which
        // simplifies later logic.
        if unary_ops.is_empty() && follow.is_empty() {
            if let Term::Expr(expr) = term {
                return success(*expr);
            }
        }

        success(Expression::Base {
            unary: unary_ops,
            term: term,
            follow: follow,
        })
    }

    fn term(&mut self, belongs_to: &mut Vec<String>) -> Status<Term> {
        use super::lexer::Punctuation::*;

        let start = self.updated_location();
        success(match self.next("term")? {
            // term :: 'new' (ident | abs-path)? arglist?
            Token::Ident(ref i, _) if i == "new" => {
                // try to read an ident or path
                let t = if let Some(ident) = self.ident()? {
                    NewType::Ident(ident)
                } else if let Some(path) = self.prefab()? {
                    NewType::Prefab(path)
                } else {
                    NewType::Implicit
                };

                // try to read an arglist
                // TODO: communicate what type is being new'd somehow
                let a = self.arguments(&[], "New")?;

                Term::New {
                    type_: t,
                    args: a,
                }
            },

            // term :: 'list' list_lit
            // TODO: list arguments are actually subtly different, but
            // we're going to pretend they're not to make code simpler, and
            // anyone relying on the difference needs to fix their garbage
            Token::Ident(ref i, _) if i == "list" => match self.arguments(&[], "list")? {
                Some(args) => Term::List(args),
                None => Term::Ident(i.to_owned()),
            },

            // term :: 'call' arglist arglist
            Token::Ident(ref i, _) if i == "call" => {
                Term::DynamicCall(require!(self.arguments(&[], "call")), require!(self.arguments(&[], "call*")))
            },

            // term :: 'input' arglist input_specifier
            Token::Ident(ref i, _) if i == "input" => match self.arguments(&[], "input")? {
                Some(args) => {
                    let (input_type, in_list) = require!(self.input_specifier());
                    Term::Input { args, input_type, in_list: in_list.map(Box::new) }
                }
                None => Term::Ident(i.to_owned())
            },

            // term :: 'locate' arglist ('in' expression)?
            Token::Ident(ref i, _) if i == "locate" => match self.arguments(&[], "locate")? {
                Some(args) => {
                    // warn against this mistake
                    if let Some(&Expression::BinaryOp { op: BinaryOp::In, .. } ) = args.get(0) {
                        self.context.register_error(self.error("bad 'locate(in)', should be 'locate() in'")
                            .set_severity(Severity::Warning));
                    }

                    // read "in" clause
                    let in_list = if let Some(()) = self.exact(Token::Punct(Punctuation::In))? {
                        Some(Box::new(require!(self.expression())))
                    } else {
                        None
                    };
                    Term::Locate { args, in_list }
                }
                None => Term::Ident(i.to_owned())
            },

            // term :: 'pick' pick_arglist
            Token::Ident(ref i, _) if i == "pick" => match self.pick_arguments()? {
                Some(args) => Term::Pick(args),
                None => Term::Ident(i.to_owned())
            },

            // term :: ident arglist | ident
            Token::Ident(i, _) => {
                let first_token = self.updated_location();
                match self.arguments(&[], &i)? {
                    Some(args) => {
                        self.annotate_precise(start..first_token, || Annotation::UnscopedCall(i.clone()));
                        Term::Call(i, args)
                    },
                    None => {
                        belongs_to.push(i.clone());
                        self.annotate(start, || Annotation::UnscopedVar(i.clone()));
                        Term::Ident(i)
                    },
                }
            },

            // term :: '..' arglist
            Token::Punct(Punctuation::Super) => {
                self.annotate(start, || Annotation::ParentCall);
                Term::ParentCall(require!(self.arguments(&[], "..")))
            },

            // term :: '.'
            Token::Punct(Punctuation::Dot) => {
                let mut dot_loc = self.location;
                if let Some(ident) = self.ident()? {
                    // prefab
                    // TODO: arrange for this ident to end up in the prefab's annotation
                    Term::Prefab(require!(self.prefab_ex(vec![(PathOp::Dot, ident)])))
                } else if let Some(args) = self.arguments(&[], ".")? {
                    // .() call
                    Term::SelfCall(args)
                } else {
                    // bare dot
                    dot_loc.column += 1;
                    self.annotate_precise(dot_loc..dot_loc, || Annotation::IncompleteTypePath(Vec::new(), PathOp::Dot));
                    self.annotate(start, || Annotation::ReturnVal);
                    Term::Ident(".".to_owned())
                }
            },
            // term :: path_lit
            t @ Token::Punct(Punctuation::Slash) |
            t @ Token::Punct(Punctuation::CloseColon) => {
                self.put_back(t);
                Term::Prefab(require!(self.prefab()))
            },

            // term :: str_lit | num_lit
            Token::String(val) => Term::String(val),
            Token::Resource(val) => Term::Resource(val),
            Token::Int(val) => Term::Int(val),
            Token::Float(val) => Term::Float(val),

            // term :: '(' expression ')'
            Token::Punct(LParen) => {
                let expr = require!(self.expression());
                require!(self.exact(Token::Punct(Punctuation::RParen)));
                Term::Expr(Box::new(expr))
            },

            Token::InterpStringBegin(begin) => {
                let mut parts = Vec::new();
                loop {
                    let expr = self.expression()?;
                    match self.next("']'")? {
                        Token::InterpStringPart(part) => {
                            parts.push((expr, part));
                        },
                        Token::InterpStringEnd(end) => {
                            parts.push((expr, end));
                            break;
                        },
                        _ => return self.parse_error(),
                    }
                }
                Term::InterpString(begin, parts)
            },

            other => return self.try_another(other),
        })
    }

    fn follow(&mut self, belongs_to: &mut Vec<String>, in_ternary: bool) -> Status<Follow> {
        match self.next("field access")? {
            // follow :: '[' expression ']'
            Token::Punct(Punctuation::LBracket) => {
                belongs_to.clear();
                let expr = require!(self.expression());
                require!(self.exact(Token::Punct(Punctuation::RBracket)));
                success(Follow::Index(Box::new(expr)))
            },

            // follow :: '.' ident arglist?
            // TODO: only apply these rules if there is no whitespace around the punctuation
            Token::Punct(Punctuation::Dot) => self.follow_index(IndexKind::Dot, belongs_to),
            Token::Punct(Punctuation::CloseColon)
                if !belongs_to.is_empty() || !in_ternary
                => self.follow_index(IndexKind::Colon, belongs_to),
            Token::Punct(Punctuation::SafeDot) => self.follow_index(IndexKind::SafeDot, belongs_to),
            Token::Punct(Punctuation::SafeColon) => self.follow_index(IndexKind::SafeColon, belongs_to),

            other => return self.try_another(other),
        }
    }

    fn follow_index(&mut self, kind: IndexKind, belongs_to: &mut Vec<String>) -> Status<Follow> {
        let mut index_op_loc = self.location;
        let start = self.updated_location();
        let ident = match self.ident()? {
            Some(ident) => ident,
            None => {
                index_op_loc.column += kind.len() as u16;
                self.annotate_precise(index_op_loc..index_op_loc, || Annotation::ScopedMissingIdent(belongs_to.clone()));
                // register the parse error, but keep going
                self.context.register_error(self.describe_parse_error());
                String::new()
            }
        };
        let end = self.updated_location();

        success(match self.arguments(belongs_to, &ident)? {
            Some(args) => {
                if !belongs_to.is_empty() {
                    let past = ::std::mem::replace(belongs_to, Vec::new());
                    self.annotate_precise(start..end, || Annotation::ScopedCall(past, ident.clone()));
                }
                Follow::Call(kind, ident, args)
            },
            None => {
                if !belongs_to.is_empty() {
                    self.annotate_precise(start..end, || Annotation::ScopedVar(belongs_to.clone(), ident.clone()));
                    belongs_to.push(ident.clone());
                }
                Follow::Field(kind, ident)
            },
        })
    }

    /// a parenthesized, comma-separated list of expressions
    fn arguments(&mut self, parents: &[String], proc: &str) -> Status<Vec<Expression>> {
        leading!(self.exact(Token::Punct(Punctuation::LParen)));
        let start = self.location;

        let mut arguments = Vec::new();
        // TODO: account for implicit nulls again
        let result = self.separated(Punctuation::Comma, Punctuation::RParen, Some(()), |this| {
            let arg_start = this.location;
            let result = this.expression();
            this.annotate(arg_start, || Annotation::ProcArgument(arguments.len()));
            match result {
                Ok(Some(expr)) => { arguments.push(expr); SUCCESS },
                Ok(None) => Ok(None),
                Err(e) => Err(e),
            }
        });
        let end = self.location;  // location of the closing parenthesis
        self.annotate_precise(start..end, || Annotation::ProcArguments(parents.to_owned(), proc.to_owned(), arguments.len()));
        match result {
            Ok(Some(_)) => success(arguments),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn pick_arguments(&mut self) -> Status<Vec<(Option<Expression>, Expression)>> {
        leading!(self.exact(Token::Punct(Punctuation::LParen)));
        success(require!(self.separated(Punctuation::Comma, Punctuation::RParen, None, |this| {
            let expr = leading!(this.expression());
            if let Some(()) = this.exact(Token::Punct(Punctuation::Semicolon))? {
                success((Some(expr), require!(this.expression())))
            } else {
                success((None, expr))
            }
        })))
    }

    fn separated<R: Clone, F: FnMut(&mut Self) -> Status<R>>(&mut self, sep: Punctuation, terminator: Punctuation, allow_empty: Option<R>, mut f: F) -> Status<Vec<R>> {
        let mut comma_legal = false;
        let mut elems = Vec::new();
        loop {
            if let Some(()) = self.exact(Token::Punct(terminator))? {
                return success(elems);
            } else if let Some(()) = self.exact(Token::Punct(sep))? {
                if comma_legal {
                    comma_legal = false;
                } else if let Some(empty) = allow_empty.clone() {
                    elems.push(empty);
                } else {
                    return self.parse_error();
                }
            } else if !comma_legal {
                let v = f(self);
                elems.push(self.require(v)?);
                comma_legal = true;
            } else {
                return self.parse_error();
            }
        }
    }

    // ------------------------------------------------------------------------
    // Procs

    fn read_any_tt(&mut self, target: &mut Vec<LocatedToken>) -> Status<()> {
        // read a single arbitrary "token tree", either a group or a single token
        let start = self.next("anything")?;
        let kind = TTKind::from_token(&start);
        target.push(LocatedToken::new(self.location(), start));
        let kind = match kind {
            Some(k) => k,
            None => return SUCCESS,
        };
        loop {
            let token = self.next("anything")?;
            if kind.is_end(&token) {
                target.push(LocatedToken::new(self.location(), token));
                return SUCCESS;
            } else {
                self.put_back(token);
                require!(self.read_any_tt(target));
            }
        }
    }

    fn ignore_group(&mut self, left: Punctuation, right: Punctuation) -> Status<()> {
        leading!(self.exact(Token::Punct(left)));
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
