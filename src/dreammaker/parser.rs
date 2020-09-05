//! Minimalist parser which turns a token stream into an object tree.

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::ops::Range;

use linked_hash_map::LinkedHashMap;

use super::{DMError, Location, HasLocation, Context, Severity, FileId};
use super::lexer::{LocatedToken, Token, Punctuation};
use super::objtree::{ObjectTree, NodeIndex};
use super::annotation::*;
use super::ast::*;
use super::docs::*;

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
    };
}

// ----------------------------------------------------------------------------
// Convenience functions

/// Parse a token stream, in the form emitted by the indent processor, into
/// an object tree.
///
/// Compilation failures will return a best-effort parse, and diagnostics will
/// be registered with the provided `Context`.
pub fn parse<I>(context: &Context, iter: I) -> ObjectTree
where
    I: IntoIterator<Item=LocatedToken>,
{
    Parser::new(context, iter.into_iter()).parse_object_tree()
}

/// Parse a token stream into an expression.
///
/// Fatal errors will be directly returned and miscellaneous diagnostics will
/// be registered with the provided `Context`.
pub fn parse_expression<I>(context: &Context, location: Location, iter: I) -> Result<Expression, DMError>
where
    I: IntoIterator<Item=LocatedToken>,
{
    let mut parser = Parser::new(context, iter.into_iter());
    parser.location = location;
    Ok(require!(parser.expression()))
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
    fn matches(self, token: &Token) -> bool {
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
    TernaryOp(TernaryOp),
}

impl Op {
    fn build(self, lhs: Box<Expression>, rhs: Box<Expression>) -> Expression {
        match self {
            Op::BinaryOp(op) => Expression::BinaryOp { op, lhs, rhs },
            Op::AssignOp(op) => Expression::AssignOp { op, lhs, rhs },
            Op::TernaryOp(_) => unreachable!(),
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
    // &
    BitAnd {
        (BinaryOp, BitAnd),
    }
    // ^
    BitXor {
        (BinaryOp, BitXor),
    }
    // |
    BitOr {
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
    Conditional {
        (TernaryOp, Conditional = QuestionMark),
    }
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

    fn is_end(self, token: &Token) -> bool {
        match (self, token) {
            (TTKind::Paren, &Token::Punct(Punctuation::RParen)) => true,
            (TTKind::Brace, &Token::Punct(Punctuation::RBrace)) => true,
            (TTKind::Bracket, &Token::Punct(Punctuation::RBracket)) => true,
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
pub struct Parser<'ctx, 'an, 'inp> {
    context: &'ctx Context,
    annotations: Option<&'an mut AnnotationTree>,
    tree: ObjectTree,
    fatal_errored: bool,

    input: Box<dyn Iterator<Item=LocatedToken> + 'inp>,
    eof: bool,
    possible_indentation_error: bool,
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

impl<'ctx, 'an, 'inp> HasLocation for Parser<'ctx, 'an, 'inp> {
    fn location(&self) -> Location {
        self.location
    }
}

impl<'ctx, 'an, 'inp> Parser<'ctx, 'an, 'inp> {
    /// Construct a new parser using the given input stream.
    pub fn new<I: IntoIterator<Item=LocatedToken> + 'inp>(context: &'ctx Context, input: I) -> Self {
        Parser {
            context,
            annotations: None,
            tree: ObjectTree::default(),
            fatal_errored: false,

            input: Box::new(input.into_iter()),
            eof: false,
            possible_indentation_error: false,
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

    pub fn parse_object_tree(mut self) -> ObjectTree {
        self.tree.register_builtins();
        self.run();
        self.finalize_object_tree()
    }

    pub fn parse_object_tree_2(mut self) -> (bool, ObjectTree) {
        self.tree.register_builtins();
        self.run();
        (self.fatal_errored, self.finalize_object_tree())
    }

    #[doc(hidden)]
    pub fn parse_object_tree_without_builtins(mut self) -> ObjectTree {
        self.run();
        self.tree
    }

    pub fn parse_with_module_docs(mut self) -> (ObjectTree, BTreeMap<FileId, Vec<(u32, DocComment)>>) {
        self.tree.register_builtins();
        self.run();
        let docs = std::mem::replace(&mut self.module_docs, Default::default());
        (self.finalize_object_tree(), docs)
    }

    pub fn parse_annotations_only(mut self, annotations: &'an mut AnnotationTree) {
        self.annotate_to(annotations);
        self.run();
    }

    fn run(&mut self) {
        let root = self.root();
        if let Err(mut e) = self.require(root) {
            let loc = e.location();
            e = e.set_severity(Severity::Error);
            e.add_note(loc, "fatal error: the parser cannot continue");
            e.add_note(loc, "constant evaluation will be skipped");
            self.fatal_errored = true;
            self.context.register_error(e);
        }
    }

    fn finalize_object_tree(mut self) -> ObjectTree {
        let procs_total = self.procs_good + self.procs_bad;
        if self.procs_bad > 0 {
            eprintln!(
                "parsed {}/{} proc bodies ({}%)",
                self.procs_good,
                procs_total,
                (self.procs_good * 100 / procs_total)
            );
        }

        self.tree.finalize(self.context, self.fatal_errored);
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
                let mut e = self.error(message);
                if self.possible_indentation_error {
                    let mut loc = e.location();
                    loc.line += 1;
                    loc.column = 1;
                    e.add_note(loc, "check for extra indentation at the start of the next line");
                    self.possible_indentation_error = false;
                }
                e
            }
            Err(err) => self
                .error(format!("i/o error, expected one of: {}", expected))
                .with_cause(err),
        }
    }

    fn parse_error<T>(&mut self) -> Result<T, DMError> {
        Err(self.describe_parse_error())
    }

    fn require<T>(&mut self, t: Result<Option<T>, DMError>) -> Result<T, DMError> {
        match t {
            Ok(Some(v)) => Ok(v),
            Ok(None) => self.parse_error(),
            Err(e) => Err(e),
        }
    }

    fn next<S: Into<Cow<'static, str>>>(&mut self, expected: S) -> Result<Token, DMError> {
        let tok = loop {
            if let Some(next) = self.next.take() {
                break Ok(next);
            }
            match self.input.next() {
                Some(LocatedToken {
                    location,
                    token: Token::DocComment(dc),
                }) => match dc.target {
                    DocTarget::EnclosingItem if self.in_docs == 0 => {
                        self.module_docs
                            .entry(location.file)
                            .or_default()
                            .push((location.line, dc));
                    }
                    DocTarget::EnclosingItem => self.docs_enclosing.push(dc),
                    DocTarget::FollowingItem => self.docs_following.push(dc),
                },
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
        let message: Cow<'static, str> = match tok {
            Token::Eof => "EOF".into(),
            Token::Punct(p) => p.single_quoted().into(),
            ref other => format!("'{}'", other).into(),
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

    fn dot(&mut self) -> Status<()> {
        match self.next("'.'")? {
            Token::Punct(Punctuation::Dot) => Ok(Some(())),
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
    // Object tree - root

    fn root(&mut self) -> Status<()> {
        self.tree_entries(self.tree.root().index(), None, None, Token::Eof)
    }

    fn tree_block(&mut self, current: NodeIndex, proc_kind: Option<ProcDeclKind>, var_type: Option<VarType>) -> Status<()> {
        leading!(self.exact(Token::Punct(Punctuation::LBrace)));
        Ok(Some(require!(
            self.tree_entries(current, proc_kind, var_type, Token::Punct(Punctuation::RBrace))
        )))
    }

    fn tree_entries(&mut self, current: NodeIndex, proc_kind: Option<ProcDeclKind>, var_type: Option<VarType>, terminator: Token) -> Status<()> {
        loop {
            let message: Cow<'static, str> = match terminator {
                Token::Eof => "newline".into(),
                ref other => format!("newline, '{}'", other).into(),
            };
            let next = self.next(message)?;
            if next == terminator || next == Token::Eof {
                break;
            } else if next == Token::Punct(Punctuation::Semicolon) {
                continue;
            }
            self.put_back(next);
            require!(self.tree_entry(current, proc_kind, var_type.clone()));
        }
        SUCCESS
    }

    // ------------------------------------------------------------------------
    // Object tree - types

    fn tree_path(&mut self, always_absolute: bool) -> Status<(bool, Vec<Ident>)> {
        // path :: '/'? ident ('/' ident?)*

        // handle leading slash
        let start = self.updated_location();
        let (absolute, spurious_lead) = self.possible_leading_slash()?;
        let mut slash_loc = self.location;

        // 2 is ~66.0%, 4 is ~83.4%, 8 is ~99.9%
        let mut parts = Vec::with_capacity(2);
        // expect at least one ident
        match self.ident_in_seq(parts.len())? {
            Some(i) => parts.push(i),
            None if !(absolute || spurious_lead) => return Ok(None),
            None => {
                slash_loc.column += 1;
                self.annotate_precise(slash_loc..slash_loc, || {
                    Annotation::IncompleteTreePath(absolute, parts.clone())
                });
                self.context.register_error(self.error("path has no effect"));
                return success((absolute, Vec::new()));
            }
        }
        // followed by ('/' ident)*
        loop {
            if self.slash()?.is_none() {
                break;
            }
            let mut slash_loc = self.location;
            if let Some(i) = self.ident_in_seq(parts.len())? {
                parts.push(i);
            } else {
                slash_loc.column += 1;
                self.annotate_precise(slash_loc..slash_loc, || {
                    Annotation::IncompleteTreePath(absolute || always_absolute, parts.clone())
                });
            }
        }

        self.annotate(start, || Annotation::TreePath(absolute || always_absolute, parts.clone()));
        success((absolute, parts))
    }

    fn possible_leading_slash(&mut self) -> Result<(bool, bool), DMError> {
        match self.next("'/'")? {
            Token::Punct(Punctuation::Slash) => Ok((true, false)),
            Token::Punct(p @ Punctuation::Dot) |
            Token::Punct(p @ Punctuation::CloseColon) |
            Token::Punct(p @ Punctuation::Colon) => {
                self.error(format!("path started by '{}', should be unprefixed", p))
                    .set_severity(Severity::Warning)
                    .register(self.context);
                Ok((false, true))
            }
            t => {
                self.put_back(t);
                Ok((false, false))
            }
        }
    }

    // Look for a `/`, and complain but continue if we see a `.` or `:`.
    fn slash(&mut self) -> Status<()> {
        match self.next("'/'")? {
            Token::Punct(Punctuation::Slash) => SUCCESS,
            Token::Punct(p @ Punctuation::Dot) |
            Token::Punct(p @ Punctuation::CloseColon) |
            Token::Punct(p @ Punctuation::Colon) => {
                self.error(format!("path separated by '{}', should be '/'", p))
                    .set_severity(Severity::Warning)
                    .register(self.context);
                SUCCESS
            }
            t => { self.put_back(t); Ok(None) }
        }
    }

    fn tree_entry(&mut self, mut current: NodeIndex, mut proc_kind: Option<ProcDeclKind>, mut var_type: Option<VarType>) -> Status<()> {
        // tree_entry :: path ';'
        // tree_entry :: path tree_block
        // tree_entry :: path '=' expression ';'
        // tree_entry :: path '(' argument_list ')' ';'
        // tree_entry :: path '(' argument_list ')' code_block

        use super::lexer::Token::*;
        use super::lexer::Punctuation::*;

        let entry_start = self.updated_location();

        // read and calculate the current path
        let (absolute, mut path) = leading!(self.tree_path(false));

        if absolute && current != self.tree.root().index() {
            DMError::new(entry_start, format!("nested absolute path inside {}", &self.tree[current].path))
                .set_severity(Severity::Warning)
                .register(self.context);
            current = self.tree.root().index();
        }

        let path_len = path.len();
        let (last_part, traverse) = match path.split_last_mut() {
            Some(x) => x,
            None => {
                self.error("what?")
                    .register(self.context);
                return SUCCESS;
            }
        };

        macro_rules! traverse_tree {
            ($what:expr) => {
                let each = $what;
                if each == "var" {
                    var_type = Some(VarType::default());
                } else if let Some(var_type) = var_type.as_mut() {
                    if let Some(flag) = VarTypeFlags::from_name(each) {
                        var_type.flags |= flag;
                    } else {
                        var_type.type_path.push(each.to_owned());
                    }
                } else if let Some(kind) = ProcDeclKind::from_name(each) {
                    proc_kind = Some(kind);
                } else if proc_kind.is_some() {
                    self.error("cannot have sub-blocks of `proc/` block")
                        .register(self.context);
                } else {
                    let len = self.tree[current].path.chars().filter(|&c| c == '/').count() + path_len;
                    current = self.tree.subtype_or_add(self.location, current, each, len);
                }
            }
        }

        for each in traverse {
            traverse_tree!(each);
        }

        // parse operator overloading definitions
        if last_part == "operator" {
            self.try_read_operator_name(last_part)?;
        }

        let var_suffix = if var_type.is_some() {
            require!(self.var_suffix())
        } else {
            Default::default()
        };

        // read the contents for real
        match self.next("contents")? {
            t @ Punct(LBrace) => {
                // `thing{` - block
                self.put_back(t);
                traverse_tree!(last_part);
                let start = self.updated_location();

                if proc_kind.is_some() || var_type.is_some() {
                    // Can't apply docs to `var/` or `proc/` blocks.
                    require!(self.tree_block(current, proc_kind, var_type.clone()));
                } else {
                    let (comment, ()) = require!(self.doc_comment(|this| this.tree_block(current, proc_kind, var_type.clone())));
                    self.tree[current].docs.extend(comment);
                }

                let node = self.tree[current].path.to_owned();
                self.annotate(start, || Annotation::TreeBlock(reconstruct_path(&node, proc_kind, var_type.as_ref(), "")));
                SUCCESS
            }
            Punct(Assign) => {
                // `something=` - var
                let location = self.location;

                // kind of goofy, but allows "enclosing" doc comments at the end of the line
                // translators note: this allows comments of the form ``//! blah`` at the end of the line
                let (docs, expression) = require!(self.doc_comment(|this| {
                    let expr = require!(this.expression());
                    let _ = require!(this.input_specifier());

                    // We have to annotate prior to consuming the statement terminator, as we
                    // will otherwise consume following whitespace resulting in a bad annotation range
                    let node = this.tree[current].path.to_owned();
                    this.annotate(entry_start, || Annotation::Variable(reconstruct_path(&node, proc_kind, var_type.as_ref(), last_part)));

                    require!(this.statement_terminator());
                    success(expr)
                }));

                if let Some(mut var_type) = var_type {
                    var_type.suffix(&var_suffix);
                    self.tree.declare_var(current, last_part, location, docs, var_type, Some(expression));
                } else {
                    self.tree.override_var(current, last_part, location, docs, expression);
                }

                SUCCESS
            }
            t @ Punct(LParen) => {
                // `something(` - proc
                self.put_back(t);
                require!(self.proc_params_and_body(current, proc_kind, last_part, entry_start, absolute));
                SUCCESS
            }
            other => {
                // usually `thing;` - a contentless declaration
                // TODO: allow enclosing-targeting docs here somehow?
                self.put_back(other);

                if last_part == "var" {
                    self.error("`var;` item has no effect")
                        .set_severity(Severity::Warning)
                        .register(self.context);
                } else if let Some(mut var_type) = var_type.take() {
                    if VarTypeFlags::from_name(last_part).is_some() {
                        self.error(format!("`var/{};` item has no effect", last_part))
                            .set_severity(Severity::Warning)
                            .register(self.context);
                    } else {
                        let docs = std::mem::take(&mut self.docs_following);
                        var_type.suffix(&var_suffix);
                        let node = self.tree[current].path.to_owned();
                        self.annotate(entry_start, || Annotation::Variable(reconstruct_path(&node, proc_kind, Some(&var_type), last_part)));
                        self.tree.declare_var(current, last_part, self.location, docs, var_type, var_suffix.into_initializer());
                    }
                } else if ProcDeclKind::from_name(last_part).is_some() {
                    self.error("`proc;` item has no effect")
                        .set_severity(Severity::Warning)
                        .register(self.context);
                } else if proc_kind.is_some() {
                    self.error("child of `proc/` without body")
                        .register(self.context);
                } else {
                    let docs = std::mem::take(&mut self.docs_following);
                    let len = self.tree[current].path.chars().filter(|&c| c == '/').count() + path_len;
                    current = self.tree.subtype_or_add(self.location, current, last_part, len);
                    self.tree[current].docs.extend(docs);
                }

                SUCCESS
            }
        }
    }

    // ------------------------------------------------------------------------
    // Object tree - Vars

    // ------------------------------------------------------------------------
    // Object tree - Procs

    fn try_read_operator_name(&mut self, last_part: &mut String) -> Status<()> {
        use super::lexer::Token::Punct;
        use super::lexer::Punctuation::*;

        if self.exact(Punct(Mod))?.is_some() {
            last_part.push('%');
        } else if self.exact(Punct(ModAssign))?.is_some() {
            last_part.push_str("%=");
        } else if self.exact(Punct(BitAnd))?.is_some() {
            last_part.push('&');
        } else if self.exact(Punct(BitAndAssign))?.is_some() {
            last_part.push_str("&=");
        } else if self.exact(Punct(Mul))?.is_some() {
            last_part.push('*');
        } else if self.exact(Punct(Pow))?.is_some() {
            last_part.push_str("**");
        } else if self.exact(Punct(MulAssign))?.is_some() {
            last_part.push_str("*=");
        } else if self.exact(Punct(Add))?.is_some() {
            last_part.push('+');
        } else if self.exact(Punct(PlusPlus))?.is_some() {
            last_part.push_str("++");
        } else if self.exact(Punct(AddAssign))?.is_some() {
            last_part.push_str("+=");
        } else if self.exact(Punct(Sub))?.is_some() {
            last_part.push('-');
        } else if self.exact(Punct(MinusMinus))?.is_some() {
            last_part.push_str("--");
        } else if self.exact(Punct(SubAssign))?.is_some() {
            last_part.push_str("-=");
        } else if self.exact(Punct(Less))?.is_some() {
            last_part.push('<');
        } else if self.exact(Punct(LShift))?.is_some() {
            last_part.push_str("<<");
        } else if self.exact(Punct(LShiftAssign))?.is_some() {
            last_part.push_str("<<=");
        } else if self.exact(Punct(LessEq))?.is_some() {
            last_part.push_str("<=")
        } else if self.exact(Punct(Greater))?.is_some() {
            last_part.push('>');
        } else if self.exact(Punct(GreaterEq))?.is_some() {
            last_part.push_str(">=");
        } else if self.exact(Punct(RShift))?.is_some() {
            last_part.push_str(">>");
        } else if self.exact(Punct(RShiftAssign))?.is_some() {
            last_part.push_str(">>=");
        } else if self.exact(Punct(BitXor))?.is_some() {
            last_part.push('^');
        } else if self.exact(Punct(BitXorAssign))?.is_some() {
            last_part.push_str("^=");
        } else if self.exact(Punct(BitOr))?.is_some() {
            last_part.push('|');
        } else if self.exact(Punct(BitOrAssign))?.is_some() {
            last_part.push_str("|=");
        } else if self.exact(Punct(BitNot))?.is_some() {
            last_part.push('~');
        } else if self.exact(Punct(Equiv))?.is_some() {
            last_part.push_str("~=");
        } else if self.exact(Punct(LBracket))?.is_some() {
            require!(self.exact(Punct(RBracket)));
            if self.exact(Punct(Assign))?.is_some() {
                last_part.push_str("[]=");
            } else {
                last_part.push_str("[]");
            }
        }
        SUCCESS
    }

    fn proc_params_and_body(&mut self, current: NodeIndex, proc_kind: Option<ProcDeclKind>, name: &str, entry_start: Location, absolute: bool) -> Status<()> {
        use super::lexer::Token::*;
        use super::lexer::Punctuation::*;
        use super::objtree::Code;

        leading!(self.exact(Punct(LParen)));

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

        let code = if self.procs {
            let result = {
                let mut subparser: Parser<'ctx, '_, '_> = Parser::new(self.context, body_tt);
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
            match result {
                Err(err) => {
                    let err2 = err.clone();
                    self.context.register_error(err);
                    Code::Invalid(err2)
                },
                Ok(code) => {
                    Code::Present(code)
                }
            }
        } else {
            Code::Disabled
        };

        match self.tree.register_proc(self.context, location, current, name, proc_kind, parameters, code) {
            Ok((idx, proc)) => {
                proc.docs.extend(comment);
                // manually performed for borrowck reasons
                if let Some(dest) = self.annotations.as_mut() {
                    let new_stack = reconstruct_path(&self.tree[current].path, proc_kind, None, name);
                    dest.insert(entry_start..body_start, Annotation::ProcHeader(new_stack.to_vec(), idx));
                    dest.insert(body_start..self.location, Annotation::ProcBody(new_stack.to_vec(), idx));
                }
                if !absolute && self.context.config().code_standards.disallow_relative_proc_definitions {
                    DMError::new(location, "relatively pathed proc defined here")
                        .set_severity(Severity::Warning)
                        .register(self.context);
                }
            }
            Err(e) => self.context.register_error(e),
        };

        SUCCESS
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
        let (_absolute, mut path) = leading!(self.tree_path(true));
        let name = match path.pop() {
            Some(name) => name,
            None => {
                self.describe_parse_error().register(self.context);
                "".to_owned()
            }
        };
        if path.first().map_or(false, |i| i == "var") {
            path.remove(0);
            DMError::new(leading_loc, "'var/' is unnecessary here")
                .set_severity(Severity::Hint)
                .with_errortype("var_in_proc_paramater")
                .register(self.context);
        }
        let mut var_type: VarType = path.into_iter().collect();
        if var_type.flags.is_static() {
            DMError::new(leading_loc, "'static/' has no effect here")
                .set_severity(Severity::Warning)
                .with_errortype("static_in_proc_parameter")
                .register(self.context);
        }
        let location = self.location;
        // In parameters, the expression within the annotation is ignored.
        var_type.suffix(&require!(self.var_suffix()));
        // = <expr>
        let default = if let Some(()) = self.exact(Punct(Assign))? {
            Some(require!(self.expression()))
        } else {
            None
        };
        let (input_type, in_list) = require!(self.input_specifier());

        // Allow a trailing `;` since BYOND accepts it, but this is dumb
        if let Some(()) = self.exact(Punct(Semicolon))? {
            DMError::new(self.updated_location(), "Extraneous ';' in proc parameter")
                .set_severity(Severity::Warning)
                .with_errortype("semicolon_in_proc_parameter")
                .register(self.context);
        }

        success(Parameter {
            var_type,
            name,
            default,
            input_type,
            in_list,
            location,
        })
    }

    // ------------------------------------------------------------------------
    // Statements

    /// Parse list size declarations.
    fn var_suffix(&mut self) -> Status<VarSuffix> {
        use super::lexer::Token::Punct;
        use super::lexer::Punctuation::*;

        let mut list = Vec::new();
        while let Some(()) = self.exact(Punct(LBracket))? {
            list.push(self.expression()?);
            require!(self.exact(Punct(RBracket)));
        }
        success(VarSuffix { list })
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
        let in_list;
        if let Some(()) = self.exact(Token::Punct(Punctuation::In))? {
            in_list = Some(require!(self.expression()));
            // in case it is out of order
            if let Some(()) = self.exact_ident("as")? {
                self.error("'as' clause should precede 'in' clause, and is being ignored")
                    .with_errortype("in_precedes_as")
                    .set_severity(Severity::Warning)
                    .register(self.context);
                let _ = require!(self.input_type());
            }
        } else {
            in_list = None;
        }
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
    fn block(&mut self, loop_ctx: &LoopContext) -> Status<Block> {
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

    fn statement(&mut self, loop_ctx: &LoopContext, vars: &mut Vec<(Location, VarType, String)>) -> Status<Spanned<Statement>> {
        let start = self.location();
        let spanned = |v| success(Spanned::new(start, v));

        // BLOCK STATEMENTS
        if let Some(()) = self.exact_ident("if")? {
            // statement :: 'if' '(' expression ')' block ('else' 'if' '(' expression ')' block)* ('else' block)?
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let expr = Spanned::new(self.location(), require!(self.expression()));
            require!(self.exact(Token::Punct(Punctuation::RParen)));
            let block = require!(self.block(loop_ctx));
            let mut arms = vec![(expr, block)];

            let mut else_arm = None;
            self.skip_phantom_semicolons()?;
            while let Some(()) = self.exact_ident("else")? {
                if let Some(()) = self.exact_ident("if")? {
                    require!(self.exact(Token::Punct(Punctuation::LParen)));
                    let expr = Spanned::new(self.location(), require!(self.expression()));
                    require!(self.exact(Token::Punct(Punctuation::RParen)));
                    let block = require!(self.block(loop_ctx));
                    arms.push((expr, block));
                } else {
                    else_arm = Some(require!(self.block(loop_ctx)));
                    break;
                }
                self.skip_phantom_semicolons()?;
            }

            spanned(Statement::If { arms, else_arm })
        } else if let Some(()) = self.exact_ident("while")? {
            // statement :: 'while' '(' expression ')' block
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let condition = require!(self.expression());
            require!(self.exact(Token::Punct(Punctuation::RParen)));
            let block = require!(self.block(&LoopContext::While));
            spanned(Statement::While { condition, block })
        } else if let Some(()) = self.exact_ident("do")? {
            // statement :: 'do' block 'while' '(' expression ')' ';'
            let block = require!(self.block(&LoopContext::DoWhile));
            self.skip_phantom_semicolons()?;
            require!(self.exact_ident("while"));
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let condition = Spanned::new(self.location(), require!(self.expression()));
            require!(self.exact(Token::Punct(Punctuation::RParen)));
            require!(self.statement_terminator());
            spanned(Statement::DoWhile { block, condition })
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
                let inc = match self.comma_or_semicolon()? {
                    Some(()) => self.simple_statement(false, vars)?,
                    None => None,
                };
                require!(self.exact(Token::Punct(Punctuation::RParen)));
                spanned(Statement::ForLoop {
                    init: init.map(Box::new),
                    test,
                    inc: inc.map(Box::new),
                    block: require!(self.block(&LoopContext::ForLoop)),
                })
            } else if let Some(init) = init {
                // in-list form ("for list")
                let (var_type, name) = match init {
                    // this is a really terrible way to do this
                    Statement::Var(VarStatement {
                        var_type,
                        name,
                        value: Some(value),
                    }) => {
                        // for(var/a = 1 to
                        require!(self.exact_ident("to"));
                        let rhs = require!(self.expression());
                        return spanned(require!(self.for_range(Some(var_type), name, value, rhs)));
                    }
                    Statement::Expr(Expression::AssignOp {
                        op: AssignOp::Assign,
                        lhs,
                        rhs,
                    }) => {
                        // for(a = 1 to
                        let name = match lhs.into_term() {
                            Some(Term::Ident(name)) => name,
                            _ => return Err(self.error("for-list must start with variable")),
                        };
                        require!(self.exact_ident("to"));
                        let to_rhs = require!(self.expression());
                        return spanned(require!(self.for_range(None, name, *rhs, to_rhs)));
                    }
                    Statement::Var(VarStatement {
                        var_type,
                        name,
                        value: None,
                    }) => (Some(var_type), name),
                    Statement::Expr(Expression::BinaryOp {
                        op: BinaryOp::In,
                        lhs,
                        rhs,
                    }) => {
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
                                return spanned(require!(self.for_range(None, name, *lhs, *rhs)));
                            },
                            rhs => {
                                // I love code duplication, don't you?
                                require!(self.exact(Token::Punct(Punctuation::RParen)));
                                return spanned(Statement::ForList {
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
                        return spanned(require!(self.for_range(var_type, name, value, rhs)));
                    }
                    Some(value)
                } else {
                    None
                };

                require!(self.exact(Token::Punct(Punctuation::RParen)));
                spanned(Statement::ForList {
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
            spanned(Statement::Spawn {
                delay: expr,
                block: require!(self.block(&LoopContext::None))
            })
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
                cases.push((Spanned::new(self.location(), what), block));
            }
            let default = if let Some(()) = self.exact_ident("else")? {
                Some(require!(self.block(loop_ctx)))
            } else {
                None
            };
            require!(self.exact(Token::Punct(Punctuation::RBrace)));
            spanned(Statement::Switch { input: expr, cases, default })
        } else if let Some(()) = self.exact_ident("try")? {
            let try_block = require!(self.block(loop_ctx));
            self.skip_phantom_semicolons()?;
            require!(self.exact_ident("catch"));
            let catch_params;
            if let Some(()) = self.exact(Token::Punct(Punctuation::LParen))? {
                catch_params = require!(self.separated(Punctuation::Comma, Punctuation::RParen, None, |this| {
                    // TODO: improve upon this cheap approximation
                    success(leading!(this.tree_path(true)).1)
                }));
            } else {
                catch_params = Vec::new();
            }
            let catch_block = require!(self.block(loop_ctx));
            spanned(Statement::TryCatch {
                try_block,
                catch_params,
                catch_block,
            })
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
            spanned(Statement::Setting { name, mode, value })
        } else if let Some(()) = self.exact_ident("break")? {
            let label = self.ident()?;
            require!(self.statement_terminator());
            spanned(Statement::Break(label))
        } else if let Some(()) = self.exact_ident("continue")? {
            let label = self.ident()?;
            require!(self.statement_terminator());
            spanned(Statement::Continue(label))
        } else if let Some(()) = self.exact_ident("del")? {
            let expr = require!(self.expression());
            require!(self.statement_terminator());
            spanned(Statement::Del(expr))
        } else {
            let result = leading!(self.simple_statement(false, vars));

            // check for a label `ident:`
            if let Statement::Expr(ref expr) = result {
                if let Some(Term::Ident(ref name)) = expr.as_term() {
                    if let Some(()) = self.exact(Token::Punct(Punctuation::Colon))? {
                        // it's a label! check for a block
                        return spanned(Statement::Label {
                            name: name.to_owned(),
                            block: require!(self.block(loop_ctx)),
                        });
                    }
                }
            }

            require!(self.statement_terminator());
            spanned(result)
        }
    }

    // Handle if(1){a=1;b=2} without a trailing semicolon
    fn statement_terminator(&mut self) -> Status<()> {
        match self.next("';'")? {
            Token::Punct(Punctuation::Semicolon) => SUCCESS,
            p @ Token::Punct(Punctuation::RBrace) => {
                self.put_back(p);
                SUCCESS
            }
            p @ Token::Punct(Punctuation::LBrace) => {
                self.possible_indentation_error = true;
                self.try_another(p)
            }
            other => {
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
                let (_, mut tree_path) = require!(self.tree_path(true));
                let name = match tree_path.pop() {
                    Some(name) => name,
                    None => return Err(self.error("'var' must be followed by a name")),
                };

                let mut var_type = tree_path.into_iter().collect::<VarType>();
                if var_type.flags.is_tmp() {
                    DMError::new(type_path_start, "var/tmp has no effect here")
                        .set_severity(Severity::Warning)
                        .with_errortype("tmp_no_effect")
                        .register(self.context);
                }
                if var_type.flags.is_final() {
                    DMError::new(type_path_start, "var/final has no effect here")
                        .set_severity(Severity::Warning)
                        .with_errortype("final_no_effect")
                        .register(self.context);
                }
                if var_type.flags.is_private() {
                    DMError::new(type_path_start, "var/SpacemanDMM_private has no effect here")
                        .with_errortype("private_var")
                        .set_severity(Severity::Warning)
                        .register(self.context);
                }
                if var_type.flags.is_protected() {
                    DMError::new(type_path_start, "var/SpacemanDMM_protected has no effect here")
                        .with_errortype("protected_var")
                        .set_severity(Severity::Warning)
                        .register(self.context);
                }
                let var_suffix = require!(self.var_suffix());
                var_type.suffix(&var_suffix);

                if self.annotations.is_some() {
                    vars.push((self.location, var_type.clone(), name.clone()));
                }

                let value = if let Some(()) = self.exact(Token::Punct(Punctuation::Assign))? {
                    Some(require!(self.expression()))
                } else {
                    var_suffix.into_initializer()
                };
                let (input_types, in_list) = if !in_for {
                    require!(self.input_specifier())
                } else {
                    (InputType::default(), None)
                };
                if !input_types.is_empty() || in_list.is_some() {
                    self.error("'as' clause has no effect on local variables")
                        .set_severity(Severity::Warning)
                        .with_errortype("as_local_var")
                        .register(self.context);
                }

                var_stmts.push(VarStatement { var_type, name, value });
                if in_for || self.exact(Token::Punct(Punctuation::Comma))?.is_none() {
                    break;
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
        } else if let Some(()) = self.exact_ident("CRASH")? {
            require!(self.exact(Token::Punct(Punctuation::LParen)));
            let expression = require!(self.expression());
            require!(self.exact(Token::Punct(Punctuation::RParen)));
            success(Statement::Crash(expression))
        } else if let Some(()) = self.exact_ident("throw")? {
            // statement :: 'throw' expression ';'
            let expression = require!(self.expression());
            success(Statement::Throw(expression))
        } else if let Some(()) = self.exact_ident("goto")? {
            // statement :: 'goto' ident ';'
            let label_name = require!(self.ident());
            success(Statement::Goto(label_name))
        // EXPRESSION STATEMENTS
        } else {
            // statement :: expression ';'
            let expr = leading!(self.expression());
            success(Statement::Expr(expr))
        }
    }

    // for(var/a = 1 to 20
    // for(var/a in 1 to 20
    fn for_range(
        &mut self,
        var_type: Option<VarType>,
        name: String,
        start: Expression,
        end: Expression,
    ) -> Status<Statement> {
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
            self.annotate_precise(separator_loc..separator_loc, || {
                Annotation::IncompleteTypePath(parts.clone(), sep)
            });
        }

        // followed by more path elements, empty ones ignored
        while let Some(sep) = self.path_separator()? {
            let mut separator_loc = self.location;
            if let Some(ident) = self.ident_in_seq(parts.len())? {
                parts.push((sep, ident));
            } else {
                separator_loc.column += 1;
                self.annotate_precise(separator_loc..separator_loc, || {
                    Annotation::IncompleteTypePath(parts.clone(), sep)
                });
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

    fn expression(&mut self) -> Status<Expression> {
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
            expr = require!(self.expression_part(expr, info,
                in_ternary || info.strength == Strength::Conditional));
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
                    rhs = require!(self.expression_part(rhs, info,
                        in_ternary || info.strength == Strength::Conditional));
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
        if prev_op.strength == Strength::In {
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
        } else if prev_op.strength == Strength::Conditional {
            // This is essentially a special associativity category.
            let mut result = rhs;
            while let Some(lhs) = bits.pop() {
                // Ensure that the next thing we see is a ':' by now.
                match self.next("':'")? {
                    Token::Punct(Punctuation::Colon) |
                    Token::Punct(Punctuation::CloseColon) => {}
                    _ => return self.parse_error(),
                }
                // Read the else branch.
                let else_ = match self.expression()? {
                    Some(else_) => else_,
                    None => {
                        self.error("missing else arm of conditional operator should be replaced with 'null'")
                            .set_severity(Severity::Warning)
                            .register(self.context);
                        Expression::from(Term::Null)
                    }
                };
                // Compose the result.
                result = Expression::TernaryOp {
                    cond: Box::new(lhs),
                    if_: Box::new(result),
                    else_: Box::new(else_),
                }
            }
            return success(result);
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
                other => {
                    self.put_back(other);
                    break;
                }
            }
        }

        let mut belongs_to = Vec::new();
        let term = if unary_ops.is_empty() {
            leading!(self.term(&mut belongs_to))
        } else {
            require!(self.term(&mut belongs_to))
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
        /*if unary_ops.is_empty() && follow.is_empty() {
            if let Term::Expr(expr) = term.elem {
                return success(*expr);
            }
        }*/

        success(Expression::Base {
            unary: unary_ops,
            term,
            follow,
        })
    }

    fn term(&mut self, belongs_to: &mut Vec<String>) -> Status<Spanned<Term>> {
        use super::lexer::Punctuation::*;

        let start = self.updated_location();
        let term = match self.next("term")? {
            // term :: 'new' (prefab | (ident (index | field)*))? arglist?
            Token::Ident(ref i, _) if i == "new" => {
                // It's not entirely clear what is supposed to be valid here.
                // Some things definitely are:
                //   * new()
                //   * new /obj()
                //   * new /obj{name = "foo"}()
                //   * new .relative/path()
                //   * new some_list[0]()
                //   * new various.field.accesses()
                // But some things definitely aren't:
                //   * new some_proc()() - first parens belong to the 'new'
                //   * new /path[0]() - DM gives "expected expression"
                //   * new 2 + 2() - DM gives "expected end of statement"
                // The following is what seems a reasonable approximation.

                // try to read an ident or path
                let t = if self.dot()?.is_some() {
                    if let Some(ident) = self.ident()? {
                        // prefab
                        // TODO: arrange for this ident to end up in the prefab's annotation
                        NewType::Prefab(require!(self.prefab_ex(vec![(PathOp::Dot, ident)])))
                    } else {
                        // bare dot
                        let fields = Vec::new();
                        let ident = ".".to_owned();
                        NewType::MiniExpr { ident, fields }
                    }
                } else if let Some(ident) = self.ident()? {
                    let mut fields = Vec::new();
                    let mut belongs_to = vec![ident.clone()];
                    while let Some(item) = self.index_or_field(&mut belongs_to, false)? {
                        fields.push(item);
                    }
                    NewType::MiniExpr { ident, fields }
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
            Token::Ident(ref i, _) if i == "call" => Term::DynamicCall(
                require!(self.arguments(&[], "call")),
                require!(self.arguments(&[], "call*")),
            ),

            // term :: 'input' arglist input_specifier
            Token::Ident(ref i, _) if i == "input" => match self.arguments(&[], "input")? {
                Some(args) => {
                    let (input_type, in_list) = require!(self.input_specifier());
                    Term::Input {
                        args,
                        input_type,
                        in_list: in_list.map(Box::new),
                    }
                }
                None => Term::Ident(i.to_owned()),
            },

            // term :: 'locate' arglist ('in' expression)?
            Token::Ident(ref i, _) if i == "locate" => match self.arguments(&[], "locate")? {
                Some(args) => {
                    // warn against this mistake
                    if let Some(&Expression::BinaryOp { op: BinaryOp::In, .. } ) = args.get(0) {
                        self.error("bad `locate(X in Y)`, should be `locate(X) in Y`")
                            .set_severity(Severity::Warning)
                            .register(self.context);
                    }

                    // read "in" clause
                    let in_list = if let Some(()) = self.exact(Token::Punct(Punctuation::In))? {
                        if args.len() > 1 {
                            DMError::new(start, "bad 'locate(x, y, z) in'")
                                .set_severity(Severity::Warning)
                                .register(self.context);
                        }
                        Some(Box::new(require!(self.expression())))
                    } else {
                        None
                    };
                    Term::Locate { args, in_list }
                }
                None => Term::Ident(i.to_owned()),
            },

            // term :: 'pick' pick_arglist
            Token::Ident(ref i, _) if i == "pick" => match self.pick_arguments()? {
                Some(args) => Term::Pick(args),
                None => Term::Ident(i.to_owned()),
            },

            Token::Ident(ref i, _) if i == "null" => Term::Null,

            // term :: 'as' '(' input_type ')'
            Token::Ident(ref i, _) if i == "as" => {
                require!(self.exact(Token::Punct(Punctuation::LParen)));
                let input_type = self.input_type()?.unwrap_or_default();
                require!(self.exact(Token::Punct(Punctuation::RParen)));
                Term::As(input_type)
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
                    self.annotate_precise(dot_loc..dot_loc, || {
                        Annotation::IncompleteTypePath(Vec::new(), PathOp::Dot)
                    });
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
                if let Some(()) = self.exact(Token::Punct(Punctuation::RParen))? {
                    self.error("'()' should be replaced with 'null'")
                        .set_severity(Severity::Warning)
                        .register(self.context);
                    Term::Null
                } else {
                    let expr = require!(self.expression());
                    require!(self.exact(Token::Punct(Punctuation::RParen)));
                    Term::Expr(Box::new(expr))
                }
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
        };
        success(Spanned::new(start, term))
    }

    fn follow(&mut self, belongs_to: &mut Vec<String>, in_ternary: bool) -> Status<Spanned<Follow>> {
        let first_location = self.updated_location();
        let kind = match self.next("field access")? {
            // follow :: '[' expression ']'
            Token::Punct(Punctuation::LBracket) => {
                belongs_to.clear();
                let expr = require!(self.expression());
                require!(self.exact(Token::Punct(Punctuation::RBracket)));
                return success(Spanned::new(first_location, Follow::Index(Box::new(expr))))
            }

            // follow :: '.' ident arglist?
            // TODO: only apply these rules if there is no whitespace around the punctuation
            Token::Punct(Punctuation::Dot) => IndexKind::Dot,
            Token::Punct(Punctuation::CloseColon) if !belongs_to.is_empty() || !in_ternary => IndexKind::Colon,
            Token::Punct(Punctuation::SafeDot) => IndexKind::SafeDot,
            Token::Punct(Punctuation::SafeColon) => IndexKind::SafeColon,

            other => return self.try_another(other),
        };

        let mut index_op_loc = self.location;
        let start = self.updated_location();
        let ident = match self.ident()? {
            Some(ident) => ident,
            None => {
                index_op_loc.column += kind.name().len() as u16;
                self.annotate_precise(index_op_loc..index_op_loc, || {
                    Annotation::ScopedMissingIdent(belongs_to.clone())
                });
                // register the parse error, but keep going
                self.context.register_error(self.describe_parse_error());
                String::new()
            }
        };
        let end = self.updated_location();

        let follow = match self.arguments(belongs_to, &ident)? {
            Some(args) => {
                if !belongs_to.is_empty() {
                    let past = std::mem::replace(belongs_to, Vec::new());
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
        };
        success(Spanned::new(first_location, follow))
    }

    // TODO: somehow fix the fact that this is basically copy-pasted from
    // follow() above, except for the very end.
    fn index_or_field(&mut self, belongs_to: &mut Vec<String>, in_ternary: bool) -> Status<IndexOrField> {
        let kind = match self.next("field access")? {
            // follow :: '[' expression ']'
            Token::Punct(Punctuation::LBracket) => {
                belongs_to.clear();
                let expr = require!(self.expression());
                require!(self.exact(Token::Punct(Punctuation::RBracket)));
                return success(IndexOrField::Index(Box::new(expr)))
            }

            // follow :: '.' ident
            // TODO: only apply these rules if there is no whitespace around the punctuation
            Token::Punct(Punctuation::Dot) => IndexKind::Dot,
            Token::Punct(Punctuation::CloseColon) if !belongs_to.is_empty() || !in_ternary => IndexKind::Colon,
            Token::Punct(Punctuation::SafeDot) => IndexKind::SafeDot,
            Token::Punct(Punctuation::SafeColon) => IndexKind::SafeColon,

            other => return self.try_another(other),
        };

        let mut index_op_loc = self.location;
        let start = self.updated_location();
        let ident = match self.ident()? {
            Some(ident) => ident,
            None => {
                index_op_loc.column += kind.name().len() as u16;
                self.annotate_precise(index_op_loc..index_op_loc, || {
                    Annotation::ScopedMissingIdent(belongs_to.clone())
                });
                // register the parse error, but keep going
                self.context.register_error(self.describe_parse_error());
                String::new()
            }
        };
        let end = self.updated_location();

        if !belongs_to.is_empty() {
            self.annotate_precise(start..end, || Annotation::ScopedVar(belongs_to.clone(), ident.clone()));
            belongs_to.push(ident.clone());
        }
        success(IndexOrField::Field(kind, ident))
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
                Ok(Some(expr)) => {
                    arguments.push(expr);
                    SUCCESS
                }
                Ok(None) => Ok(None),
                Err(e) => Err(e),
            }
        });
        let end = self.location;  // location of the closing parenthesis
        self.annotate_precise(start..end, || {
            Annotation::ProcArguments(parents.to_owned(), proc.to_owned(), arguments.len())
        });
        match result {
            Ok(Some(_)) => success(arguments),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn pick_arguments(&mut self) -> Status<Vec<(Option<Expression>, Expression)>> {
        leading!(self.exact(Token::Punct(Punctuation::LParen)));
        success(require!(self.separated(
            Punctuation::Comma,
            Punctuation::RParen,
            None,
            |this| {
                let expr = leading!(this.expression());
                if let Some(()) = this.exact(Token::Punct(Punctuation::Semicolon))? {
                    success((Some(expr), require!(this.expression())))
                } else {
                    success((None, expr))
                }
            }
        )))
    }

    fn separated<R: Clone, F: FnMut(&mut Self) -> Status<R>>(
        &mut self,
        sep: Punctuation,
        terminator: Punctuation,
        allow_empty: Option<R>,
        mut f: F,
    ) -> Status<Vec<R>> {
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
}

fn reconstruct_path(node: &str, proc_kind: Option<ProcDeclKind>, var_type: Option<&VarType>, last: &str) -> Vec<String> {
    let mut result = Vec::new();
    for entry in node.split('/').skip(1) {
        result.push(entry.to_owned());
    }
    if let Some(kind) = proc_kind {
        result.push(kind.to_string());
    }
    if let Some(var) = var_type {
        result.extend(var.flags.to_vec().into_iter().map(ToOwned::to_owned));
        result.extend(var.type_path.iter().cloned());
    }
    if !last.is_empty() {
        result.push(last.to_owned());
    }
    result
}
