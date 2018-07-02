//! Minimalist parser which turns a token stream into an object tree.

use linked_hash_map::LinkedHashMap;

use super::{DMError, Location, HasLocation, Context, Severity};
use super::lexer::{LocatedToken, Token, Punctuation};
use super::objtree::ObjectTree;
use super::annotation::*;
use super::ast::*;

/// Parse a token stream, in the form emitted by the indent processor, into
/// an object tree.
///
/// Compilation failures will return a best-effort parse, and diagnostics will
/// be registered with the provided `Context`.
pub fn parse<I>(context: &Context, iter: I) -> ObjectTree where
    I: IntoIterator<Item=LocatedToken>
{
    let mut parser = Parser::new(context, iter.into_iter());
    match parser.root() {
        Ok(Some(())) => {}
        Ok(None) => context.register_error(parser.describe_parse_error()),
        Err(err) => context.register_error(err),
    }

    let procs_total = parser.procs_good + parser.procs_bad;
    if procs_total > 0 {
        eprintln!("parsed {}/{} proc bodies ({}%)", parser.procs_good, procs_total, (parser.procs_good * 100 / procs_total));
    }

    let sloppy = context.errors().iter().any(|p| p.severity() == Severity::Error);
    parser.tree.finalize(context, sloppy);
    parser.tree
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
        (AssignOp, BitAndAssign),
        (AssignOp, BitOrAssign),
        (AssignOp, BitXorAssign),
        (AssignOp, LShiftAssign),
        (AssignOp, RShiftAssign),
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
// The parser

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
    expected: Vec<String>,

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
            tree: ObjectTree::with_builtins(),

            input,
            eof: false,
            next: None,
            location: Default::default(),
            expected: Vec::new(),

            procs_bad: 0,
            procs_good: 0,
        }
    }

    pub fn run(mut self) {
        match self.root() {
            Ok(Some(())) => {}
            Ok(None) => self.context.register_error(self.describe_parse_error()),
            Err(err) => self.context.register_error(err),
        }
    }

    pub fn annotate_to(&mut self, annotations: &'an mut AnnotationTree) {
        self.annotations = Some(annotations);
    }

    pub fn set_fallback_location(&mut self, fallback: Location) {
        assert!(self.location == Default::default());
        self.location = fallback;
    }

    // ------------------------------------------------------------------------
    // Basic setup

    // Call this to get a DMError in the event of an entry point returning None
    pub fn describe_parse_error(&mut self) -> DMError {
        let expected = self.expected.join(", ");
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

    fn require<T>(&mut self, t: Result<Option<T>, DMError>) -> Result<T, DMError> {
        match t {
            Ok(Some(v)) => Ok(v),
            Ok(None) => self.parse_error(),
            Err(e) => Err(e)
        }
    }

    fn next<S: Into<String>>(&mut self, expected: S) -> Result<Token, DMError> {
        let tok = self.next.take().map_or_else(|| match self.input.next() {
            Some(token) => {
                self.expected.clear();
                self.location = token.location;
                Ok(token.token)
            }
            None => {
                if !self.eof {
                    self.eof = true;
                    Ok(Token::Eof)
                } else {
                    Err(DMError::new(self.location, "read-after-EOF"))
                }
            }
        }, Ok);
        let what = expected.into();
        if !what.is_empty() {
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

    fn annotate<F: FnOnce() -> Annotation>(&mut self, start: Location, f: F) {
        let end = self.updated_location();
        if let Some(dest) = self.annotations.as_mut() {
            dest.insert(start..end, f());
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
        let start = self.updated_location();
        match self.next("identifier")? {
            Token::Ident(i, _) => {
                self.annotate(start, || Annotation::Ident(i.clone()));
                Ok(Some(i))
            },
            other => self.try_another(other),
        }
    }

    fn exact_ident(&mut self, ident: &str) -> Status<()> {
        match self.next(ident)? {
            Token::Ident(ref i, _) if i == ident => SUCCESS,
            other => self.try_another(other),
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

        // expect at least one ident
        parts.push(match self.ident()? {
            Some(i) => i,
            None if !(absolute || spurious_lead) => return Ok(None),
            None => {
                self.context.register_error(self.error("path has no effect"));
                return success((absolute, Vec::new()));
            }
        });
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
            if let Some(i) = self.ident()? {
                parts.push(i);
            }
        }

        self.annotate(start, || Annotation::TreePath(parts.clone()));
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
        if absolute && parent.parent.is_some() {
            self.context.register_error(self.error(format!("nested absolute path: {:?} inside {:?}", path, parent))
                .set_severity(Severity::Warning));
        }
        let new_stack = PathStack {
            parent: if absolute { None } else { Some(&parent) },
            parts: &path
        };

        require!(self.var_annotations());

        // read the contents for real
        match self.next("contents")? {
            t @ Punct(LBrace) => {
                if let Err(e) = self.tree.add_entry(self.location, new_stack.iter(), new_stack.len()) {
                    self.context.register_error(e);
                }
                self.put_back(t);
                let start = self.updated_location();
                require!(self.tree_block(new_stack));
                self.annotate(start, || Annotation::TreeBlock(new_stack.to_vec()));
                SUCCESS
            }
            Punct(Assign) => {
                let location = self.location;
                let expr = require!(self.expression());
                require!(self.exact(Punct(Semicolon)));
                if let Err(e) = self.tree.add_var(location, new_stack.iter(), new_stack.len(), expr) {
                    self.context.register_error(e);
                }
                self.annotate(entry_start, || Annotation::Variable(new_stack.to_vec()));
                SUCCESS
            }
            Punct(LParen) => {
                let location = self.location;
                let parameters = require!(self.separated(Comma, RParen, None, Parser::proc_parameter));
                if let Err(e) = self.tree.add_proc(location, new_stack.iter(), new_stack.len(), parameters) {
                    self.context.register_error(e);
                }

                // split off a subparser so we can keep parsing the objtree
                // even when the proc body doesn't parse
                self.annotate(entry_start, || Annotation::ProcHeader(new_stack.to_vec()));
                let start = self.updated_location();
                let mut body_tt = Vec::new();
                require!(self.read_any_tt(&mut body_tt));
                while body_tt[0].token != Punct(LBrace) && body_tt[body_tt.len() - 1].token != Punct(Semicolon) {
                    // read repeatedly until it's a block or ends with a newline
                    require!(self.read_any_tt(&mut body_tt));
                }
                self.annotate(start, || Annotation::ProcBody(new_stack.to_vec()));
                let mut subparser = Parser::new(self.context, body_tt.iter().cloned());
                let result = subparser.block();
                if result.is_ok() {
                    self.procs_good += 1;
                } else {
                    self.procs_bad += 1;
                }
                self.annotate(start, || Annotation::ProcBodyDetails(result.map(|ok| ok.unwrap_or_default())));
                SUCCESS
            }
            other => {
                if let Err(e) = self.tree.add_entry(self.location, new_stack.iter(), new_stack.len()) {
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
                .. Default::default()
            });
        }

        // `name` or `obj/name` or `var/obj/name` or ...
        let (_absolute, mut path) = leading!(self.tree_path());
        let name = path.pop().unwrap();
        if path.first().map_or(false, |i| i == "var") {
            path.remove(0);
        }
        require!(self.var_annotations());
        // = <expr>
        let default = if let Some(()) = self.exact(Punct(Assign))? {
            Some(require!(self.expression()))
        } else {
            None
        };
        // as obj|turf
        let as_types = if let Some(()) = self.exact_ident("as")? {
            let mut as_what = vec![require!(self.ident())];
            while let Some(()) = self.exact(Punct(BitOr))? {
                as_what.push(require!(self.ident()));
            }
            Some(as_what)
        } else {
            None
        };
        // `in view(7)` or `in list("a", "b")` or ...
        let in_list = if let Some(()) = self.exact_ident("in")? {
            Some(require!(self.expression()))
        } else {
            None
        };
        success(Parameter {
            path, name, default, as_types, in_list
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
            /*push*/ require!(self.tree_entry(parent));
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

    /// Parse a block
    fn block(&mut self) -> Status<Vec<Statement>> {
        // empty blocks e.g. proc/foo();
        if let Some(()) = self.exact(Token::Punct(Punctuation::Semicolon))? {
            return success(Vec::new());
        }

        require!(self.exact(Token::Punct(Punctuation::LBrace)));
        let mut statements = Vec::new();
        loop {
            if let Some(()) = self.exact(Token::Punct(Punctuation::RBrace))? {
                break;
            } else if let Some(()) = self.exact(Token::Punct(Punctuation::Semicolon))? {
                continue;
            } else {
                statements.push(require!(self.statement()));
            }
        }
        success(statements)
    }

    fn statement(&mut self) -> Status<Statement> {
        // BLOCK STATEMENTS
        if let Some(()) = self.exact_ident("if")? {
            // statement :: 'if' '(' expression ')' block ('else' 'if' '(' expression ')' block)* ('else' block)?
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let expr = require!(self.expression());
            require!(self.exact(Token::Punct(Punctuation::RParen)));
            let block = require!(self.block());
            let mut arms = vec![(expr, block)];

            let mut else_arm = None;
            while let Some(()) = self.exact_ident("else")? {
                if let Some(()) = self.exact_ident("if")? {
                    require!(self.exact(Token::Punct(Punctuation::LParen)));
                    let expr = require!(self.expression());
                    require!(self.exact(Token::Punct(Punctuation::RParen)));
                    let block = require!(self.block());
                    arms.push((expr, block));
                } else {
                    else_arm = Some(require!(self.block()));
                    break
                }
            }

            success(Statement::If(arms, else_arm))
        } else if let Some(()) = self.exact_ident("while")? {
            // statement :: 'while' '(' expression ')' block
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let expr = require!(self.expression());
            require!(self.exact(Token::Punct(Punctuation::RParen)));
            success(Statement::While(expr, require!(self.block())))
        } else if let Some(()) = self.exact_ident("do")? {
            // statement :: 'do' block 'while' '(' expression ')' ';'
            let block = require!(self.block());
            require!(self.exact_ident("while"));
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let expr = require!(self.expression());
            require!(self.exact(Token::Punct(Punctuation::RParen)));
            require!(self.exact(Token::Punct(Punctuation::Semicolon)));
            success(Statement::DoWhile(block, expr))
        // SINGLE-LINE STATEMENTS
        } else if let Some(()) = self.exact_ident("return")? {
            // statement :: 'return' expression ';'
            let expression = self.expression()?;
            require!(self.exact(Token::Punct(Punctuation::Semicolon)));
            success(Statement::Return(expression))
        // EXPRESSION STATEMENTS
        } else {
            // statement :: expression ';'
            let expr = require!(self.expression());
            require!(self.exact(Token::Punct(Punctuation::Semicolon)));
            success(Statement::Expr(expr))
        }
    }

    // ------------------------------------------------------------------------
    // Expressions

    fn path_separator(&mut self) -> Status<PathOp> {
        success(match self.next("path separator")? {
            Token::Punct(Punctuation::Slash) => PathOp::Slash,
            Token::Punct(Punctuation::Dot) => PathOp::Dot,
            Token::Punct(Punctuation::Colon) => PathOp::Colon,
            other => { self.put_back(other); return Ok(None); }
        })
    }

    // distinct from a tree_path, path must begin with a path separator and can
    // use any path separator rather than just slash, AND can be followed by vars
    fn prefab(&mut self) -> Status<Prefab> {
        // path :: path_sep ident (path_sep ident?)*
        // path_sep :: '/' | '.' | ':'

        // expect at least one path element
        let mut parts = Vec::new();
        parts.push((
            leading!(self.path_separator()),
            require!(self.ident()),
        ));

        // followed by more path elements, empty ones ignored
        loop {
            if let Some(sep) = self.path_separator()? {
                if let Some(ident) = self.ident()? {
                    parts.push((sep, ident));
                }
            } else {
                break;
            }
        }

        // parse vars if we find them
        let mut vars = LinkedHashMap::default();
        if self.exact(Token::Punct(Punctuation::LBrace))?.is_some() {
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
        let mut expr = leading!(self.group());
        loop {
            // try to read the next operator
            let next = self.next("binary operator")?;
            let &info = match BINARY_OPS.iter().find(|op| op.matches(&next)) {
                Some(info) => info,
                None => {
                    self.put_back(next);
                    break;
                }
            };

            // trampoline high-strength expression parts as the lhs of the newly found op
            expr = require!(self.expression_part(expr, info));
        }

        if let Some(()) = self.exact(Token::Punct(Punctuation::QuestionMark))? {
            let if_ = require!(self.expression());
            require!(self.exact(Token::Punct(Punctuation::Colon)));
            let else_ = require!(self.expression());
            expr = Expression::TernaryOp {
                cond: Box::new(expr),
                if_: Box::new(if_),
                else_: Box::new(else_),
            };
        }

        success(expr)
    }

    fn expression_part(&mut self, lhs: Expression, prev_op: OpInfo) -> Status<Expression> {
        use std::cmp::Ordering;

        let mut bits = vec![lhs];
        let mut ops = vec![prev_op.oper];
        let mut rhs = require!(self.group());
        loop {
            // try to read the next operator...
            let next = self.next("binary operator")?;
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
                    rhs = require!(self.expression_part(rhs, info));
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
                    rhs = require!(self.group());
                }
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

    fn group(&mut self) -> Status<Expression> {
        let mut unary_ops = Vec::new();
        loop {
            match self.next("unary operator")? {
                Token::Punct(Punctuation::Sub) => unary_ops.push(UnaryOp::Neg),
                Token::Punct(Punctuation::Not) => unary_ops.push(UnaryOp::Not),
                Token::Punct(Punctuation::BitNot) => unary_ops.push(UnaryOp::BitNot),
                other => { self.put_back(other); break }
            }
        }

        let term = if unary_ops.len() > 0 {
            require!(self.term())
        } else {
            leading!(self.term())
        };

        let mut follow = Vec::new();
        loop {
            if let Some(()) = self.exact(Token::Punct(Punctuation::PlusPlus))? {
                unary_ops.push(UnaryOp::PostIncr);
            } else if let Some(()) = self.exact(Token::Punct(Punctuation::MinusMinus))? {
                unary_ops.push(UnaryOp::PostDecr);
            } else {
                match self.follow()? {
                    Some(f) => follow.push(f),
                    None => break,
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

    fn term(&mut self) -> Status<Term> {
        use super::lexer::Punctuation::*;

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
                let a = self.arguments()?;

                Term::New {
                    type_: t,
                    args: a,
                }
            },

            // term :: 'list' list_lit
            Token::Ident(ref i, _) if i == "list" => {
                // TODO: list arguments are actually subtly different, but
                // we're going to pretend they're not to make code simpler, and
                // anyone relying on the difference needs to fix their garbage
                match self.arguments()? {
                    Some(args) => Term::List(args),
                    None => Term::Ident("list".to_owned()),
                }
            },

            // term :: 'call' arglist arglist
            Token::Ident(ref i, _) if i == "call" => {
                Term::DynamicCall(require!(self.arguments()), require!(self.arguments()))
            },

            // term :: '.'
            Token::Punct(Punctuation::Dot) => {
                if let Some(ident) = self.ident()? {
                    // prefab
                    if let Some(mut prefab) = self.prefab()? {
                        prefab.path.insert(0, (PathOp::Dot, ident));
                        Term::Prefab(prefab)
                    } else {
                        Term::Prefab(Prefab {
                            path: vec![(PathOp::Dot, ident)],
                            vars: Default::default(),
                        })
                    }
                } else {
                    // bare dot
                    Term::Ident(".".to_owned())
                }
            },
            // term :: path_lit
            t @ Token::Punct(Punctuation::Slash) |
            t @ Token::Punct(Punctuation::Colon) => {
                self.put_back(t);
                Term::Prefab(require!(self.prefab()))
            },

            // term :: ident | str_lit | num_lit
            Token::Ident(val, _) => {
                match self.arguments()? {
                    Some(args) => Term::Call(val, args),
                    None => Term::Ident(val),
                }
            },
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
                    let expr = require!(self.expression());
                    match self.next("interpolated string part")? {
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

    fn follow(&mut self) -> Status<Follow> {
        success(match self.next("field, index, or function call")? {
            // follow :: '.' ident
            Token::Punct(Punctuation::Dot) => {
                let ident = require!(self.ident());
                match self.arguments()? {
                    Some(args) => Follow::Call(ident, args),
                    None => Follow::Field(ident),
                }
            }
            // follow :: '[' expression ']'
            Token::Punct(Punctuation::LBracket) => {
                let expr = require!(self.expression());
                require!(self.exact(Token::Punct(Punctuation::RBracket)));
                Follow::Index(Box::new(expr))
            },
            // follow :: 'as' ident
            Token::Ident(ref ident, _) if ident == "as" => {
                let cast = require!(self.ident());
                Follow::Cast(cast)
            }
            other => return self.try_another(other)
        })
    }

    /// a parenthesized, comma-separated list of expressions
    fn arguments(&mut self) -> Status<Vec<Expression>> {
        leading!(self.exact(Token::Punct(Punctuation::LParen)));
        success(require!(self.separated(Punctuation::Comma, Punctuation::RParen, Some(Expression::from(Term::Null)), Parser::expression)))
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
        let end = match start {
            Token::Punct(Punctuation::LParen) => Punctuation::RParen,
            Token::Punct(Punctuation::LBrace) => Punctuation::RBrace,
            Token::Punct(Punctuation::LBracket) => Punctuation::RBracket,
            other => { target.push(LocatedToken::new(self.location(), other)); return SUCCESS; }
        };
        target.push(LocatedToken::new(self.location(), start));
        loop {
            match self.next("anything")? {
                Token::Punct(p) if p == end => {
                    target.push(LocatedToken::new(self.location(), Token::Punct(p)));
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
