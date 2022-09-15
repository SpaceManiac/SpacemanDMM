//! The DM abstract syntax tree.
//!
//! Most AST types can be pretty-printed using the `Display` trait.
use phf::phf_map;
use std::fmt;
use std::iter::FromIterator;

use crate::error::Location;

/// Arguments for [`Term::Pick`]
pub type PickArgs = [(Option<Expression>, Expression)];

/// Cases for [`Term::Switch`]
pub type SwitchCases = [(Spanned<Vec<Case>>, Block)];

// ----------------------------------------------------------------------------
// Simple enums

/// The unary operators, both prefix and postfix.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
    PreIncr,
    PostIncr,
    PreDecr,
    PostDecr,
}

impl UnaryOp {
    /// Prepare to display this unary operator around (to the left or right of)
    /// its operand.
    pub fn around<T: fmt::Display + ?Sized>(self, expr: &'_ T) -> impl fmt::Display + '_ {
        /// A formatting wrapper created by `UnaryOp::around`.
        struct Around<'a, T: 'a + ?Sized> {
            op: UnaryOp,
            expr: &'a T,
        }

        impl<'a, T: fmt::Display + ?Sized> fmt::Display for Around<'a, T> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                use self::UnaryOp::*;
                match self.op {
                    Neg => write!(f, "-{}", self.expr),
                    Not => write!(f, "!{}", self.expr),
                    BitNot => write!(f, "~{}", self.expr),
                    PreIncr => write!(f, "++{}", self.expr),
                    PostIncr => write!(f, "{}++", self.expr),
                    PreDecr => write!(f, "--{}", self.expr),
                    PostDecr => write!(f, "{}--", self.expr),
                }
            }
        }

        Around { op: self, expr }
    }

    /// Get a human-readable name for this unary operator. May be ambiguous.
    pub fn name(self) -> &'static str {
        use self::UnaryOp::*;
        match self {
            Neg => "-",
            Not => "!",
            BitNot => "~",
            PreIncr | PostIncr => "++",
            PreDecr | PostDecr => "--",
        }
    }
}

/// The DM path operators.
///
/// Which path operator is used typically only matters at the start of a path.
#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum PathOp {
    /// `/` for absolute pathing.
    Slash,
    /// `.` for checked relative pathing.
    Dot,
    /// `:` for unchecked relative pathing.
    Colon,
}

impl PathOp {
    pub fn name(self) -> &'static str {
        match self {
            PathOp::Slash => "/",
            PathOp::Dot => ".",
            PathOp::Colon => ":",
        }
    }
}

impl fmt::Display for PathOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// The binary operators.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Eq,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Equiv,
    NotEquiv,
    BitAnd,
    BitXor,
    BitOr,
    LShift,
    RShift,
    And,
    Or,
    In,
    To, // only appears in RHS of `In`
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::BinaryOp::*;
        fmt.write_str(match *self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Pow => "**",
            Mod => "%",
            Eq => "==",
            NotEq => "!=",
            Less => "<",
            Greater => ">",
            LessEq => "<=",
            GreaterEq => ">=",
            Equiv => "~=",
            NotEquiv => "~!",
            BitAnd => "&",
            BitXor => "^",
            BitOr => "|",
            LShift => "<<",
            RShift => ">>",
            And => "&&",
            Or => "||",
            In => "in",
            To => "to",
        })
    }
}

/// The assignment operators, including augmented assignment.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AssignInto,
    BitAndAssign,
    AndAssign,
    BitOrAssign,
    OrAssign,
    BitXorAssign,
    LShiftAssign,
    RShiftAssign,
}

impl fmt::Display for AssignOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::AssignOp::*;
        fmt.write_str(match *self {
            Assign => "=",
            AddAssign => "+=",
            SubAssign => "-=",
            MulAssign => "*=",
            DivAssign => "/=",
            ModAssign => "%=",
            AssignInto => ":=",
            BitAndAssign => "&=",
            AndAssign => "&&=",
            BitXorAssign => "^=",
            BitOrAssign => "|=",
            OrAssign => "||=",
            LShiftAssign => "<<=",
            RShiftAssign => ">>=",
        })
    }
}

macro_rules! augmented {
    ($($bin:ident = $aug:ident;)*) => {
        impl BinaryOp {
            /// Get the corresponding augmented assignment operator, if available.
            pub fn assign_op(self) -> Option<AssignOp> {
                match self {
                    $(BinaryOp::$bin => Some(AssignOp::$aug),)*
                    _ => None,
                }
            }
        }

        impl AssignOp {
            /// Get the corresponding binary operator, if available.
            pub fn binary_op(self) -> Option<BinaryOp> {
                match self {
                    $(AssignOp::$aug => Some(BinaryOp::$bin),)*
                    _ => None,
                }
            }
        }
    }
}
augmented! {
    Add = AddAssign;
    Sub = SubAssign;
    Mul = MulAssign;
    Div = DivAssign;
    Mod = ModAssign;
    BitAnd = BitAndAssign;
    BitOr = BitOrAssign;
    BitXor = BitXorAssign;
    LShift = LShiftAssign;
    RShift = RShiftAssign;
    And = AndAssign;
    Or = OrAssign;
}

/// The ternary operator, represented uniformly for convenience.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum TernaryOp {
    Conditional,
}

/// The possible kinds of access operators for lists
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ListAccessKind {
    /// `[]`
    Normal,
    /// `?[]`
    Safe,
}

/// The possible kinds of index operators, for both fields and methods.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PropertyAccessKind {
    /// `a.b`
    Dot,
    /// `a:b`
    Colon,
    /// `a?.b`
    SafeDot,
    /// `a?:b`
    SafeColon,
}

impl PropertyAccessKind {
    pub fn name(self) -> &'static str {
        match self {
            PropertyAccessKind::Dot => ".",
            PropertyAccessKind::Colon => ":",
            PropertyAccessKind::SafeDot => "?.",
            PropertyAccessKind::SafeColon => "?:",
        }
    }
}

impl fmt::Display for PropertyAccessKind {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(self.name())
    }
}

/// The proc declaration kind, either `proc` or `verb`.
///
/// DM requires referencing proc paths to include whether the target is
/// declared as a proc or verb, even though the two modes are functionally
/// identical in many other respects.
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum ProcDeclKind {
    Proc,
    Verb,
}

impl ProcDeclKind {
    /// Attempt to convert a string to a declaration kind.
    pub fn from_name(name: &str) -> Option<ProcDeclKind> {
        match name {
            "proc" => Some(ProcDeclKind::Proc),
            "verb" => Some(ProcDeclKind::Verb),
            _ => None,
        }
    }

    /// Return whether `self` is `ProcDeclKind::Verb`.
    pub fn is_verb(self) -> bool {
        self == ProcDeclKind::Verb
    }

    /// Return the string representation of this declaration kind.
    pub fn name(self) -> &'static str {
        match self {
            ProcDeclKind::Proc => "proc",
            ProcDeclKind::Verb => "verb",
        }
    }
}

impl fmt::Display for ProcDeclKind {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(self.name())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SettingMode {
    /// As in `set name = "Use"`.
    Assign,
    /// As in `set src in usr`.
    In,
}

impl SettingMode {
    /// Return the string representation of this setting mode.
    pub fn name(self) -> &'static str {
        match self {
            SettingMode::Assign => "=",
            SettingMode::In => "in",
        }
    }
}

impl fmt::Display for SettingMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.name())
    }
}

// ----------------------------------------------------------------------------
// Bitflags

macro_rules! type_table {
    ($(#[$attr:meta])* pub struct $name:ident; $($txt:expr, $i:ident, $val:expr;)*) => {
        bitflags! {
            $(#[$attr])*
            /// A type specifier for verb arguments and input() calls.
            pub struct $name: u32 {
                $(const $i = $val;)*
            }
        }

        impl std::str::FromStr for $name {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $(
                        $txt => Ok($name::$i),
                    )*
                    _ => Err(()),
                }
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                let mut first = true;
                $(
                    if self.contains($name::$i) {
                        write!(fmt, "{}{}", if first { "" } else { "|" }, $txt)?;
                        first = false;
                    }
                )*
                if first {
                    fmt.write_str("()")?;
                }
                Ok(())
            }
        }
    }
}

type_table! {
    /// A type specifier for verb arguments and input() calls.
    pub struct InputType;

    // These values can be known with an invocation such as:
    //     src << as(command_text)
    "mob",          MOB,          1 << 0;
    "obj",          OBJ,          1 << 1;
    "text",         TEXT,         1 << 2;
    "num",          NUM,          1 << 3;
    "file",         FILE,         1 << 4;
    "turf",         TURF,         1 << 5;
    "key",          KEY,          1 << 6;
    "null",         NULL,         1 << 7;
    "area",         AREA,         1 << 8;
    "icon",         ICON,         1 << 9;
    "sound",        SOUND,        1 << 10;
    "message",      MESSAGE,      1 << 11;
    "anything",     ANYTHING,     1 << 12;
    "password",     PASSWORD,     1 << 15;
    "command_text", COMMAND_TEXT, 1 << 16;
    "color",        COLOR,        1 << 17;
}

bitflags! {
    #[derive(Default)]
    pub struct VarTypeFlags: u8 {
        // DM flags
        const STATIC = 1 << 0;
        const CONST = 1 << 2;
        const TMP = 1 << 3;
        // SpacemanDMM flags
        const FINAL = 1 << 4;
        const PRIVATE = 1 << 5;
        const PROTECTED = 1 << 6;
    }
}

impl VarTypeFlags {
    pub fn from_name(name: &str) -> Option<VarTypeFlags> {
        match name {
            // DM flags
            "global" | "static" => Some(VarTypeFlags::STATIC),
            "const" => Some(VarTypeFlags::CONST),
            "tmp" => Some(VarTypeFlags::TMP),
            // SpacemanDMM flags
            "SpacemanDMM_final" => Some(VarTypeFlags::FINAL),
            "SpacemanDMM_private" => Some(VarTypeFlags::PRIVATE),
            "SpacemanDMM_protected" => Some(VarTypeFlags::PROTECTED),
            // Fallback
            _ => None,
        }
    }

    #[inline]
    pub fn is_static(&self) -> bool {
        self.contains(VarTypeFlags::STATIC)
    }

    #[inline]
    pub fn is_const(&self) -> bool {
        self.contains(VarTypeFlags::CONST)
    }

    #[inline]
    pub fn is_tmp(&self) -> bool {
        self.contains(VarTypeFlags::TMP)
    }

    #[inline]
    pub fn is_final(&self) -> bool {
        self.contains(VarTypeFlags::FINAL)
    }

    #[inline]
    pub fn is_private(&self) -> bool {
        self.contains(VarTypeFlags::PRIVATE)
    }

    #[inline]
    pub fn is_protected(&self) -> bool {
        self.contains(VarTypeFlags::PROTECTED)
    }

    #[inline]
    pub fn is_const_evaluable(&self) -> bool {
        self.contains(VarTypeFlags::CONST)
            || !self.intersects(VarTypeFlags::STATIC | VarTypeFlags::PROTECTED)
    }

    #[inline]
    pub fn is_normal(&self) -> bool {
        !self.intersects(VarTypeFlags::CONST | VarTypeFlags::STATIC | VarTypeFlags::PROTECTED)
    }

    pub fn to_vec(&self) -> Vec<&'static str> {
        let mut v = Vec::new();
        if self.is_static() {
            v.push("static");
        }
        if self.is_const() {
            v.push("const");
        }
        if self.is_tmp() {
            v.push("tmp");
        }
        if self.is_final() {
            v.push("SpacemanDMM_final");
        }
        if self.is_private() {
            v.push("SpacemanDMM_private");
        }
        if self.is_protected() {
            v.push("SpacemanDMM_protected");
        }
        v
    }
}

impl fmt::Display for VarTypeFlags {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.is_static() {
            fmt.write_str("static/")?;
        }
        if self.is_const() {
            fmt.write_str("const/")?;
        }
        if self.is_tmp() {
            fmt.write_str("tmp/")?;
        }
        if self.is_final() {
            fmt.write_str("SpacemanDMM_final/")?;
        }
        if self.is_private() {
            fmt.write_str("SpacemanDMM_private/")?;
        }
        if self.is_protected() {
            fmt.write_str("SpacemanDMM_protected/")?;
        }
        Ok(())
    }
}

// ----------------------------------------------------------------------------
// Helper types

// Original `Ident` is an alias for `String`.
pub type Ident = String;

// Ident2 is an opaque type which promises a limited interface.
// It's a `Box<str>` for now (smaller than `Ident` by 8 bytes),
// but could be replaced by interning later.
#[derive(Clone, Eq, PartialEq)]
pub struct Ident2 {
    inner: Box<str>,
}

impl Ident2 {
    pub fn as_str(&self) -> &str {
        &*self.inner
    }
}

impl PartialEq<str> for Ident2 {
    fn eq(&self, other: &str) -> bool {
        &*self.inner == other
    }
}

impl<'a> From<&'a str> for Ident2 {
    fn from(v: &'a str) -> Self {
        Ident2 { inner: v.into() }
    }
}

impl From<String> for Ident2 {
    fn from(v: String) -> Self {
        Ident2 { inner: v.into() }
    }
}

impl From<Ident2> for String {
    fn from(v: Ident2) -> Self {
        v.inner.into()
    }
}

impl std::ops::Deref for Ident2 {
    type Target = str;
    fn deref(&self) -> &str {
        &*self.inner
    }
}

impl fmt::Display for Ident2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl fmt::Debug for Ident2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

/// An AST element with an additional location attached.
#[derive(Copy, Clone, Eq, Debug)]
pub struct Spanned<T> {
    // TODO: add a Span type and use it here
    pub location: Location,
    pub elem: T,
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        // Skips the location: allows easy recursive Eq checks
        self.elem == other.elem
    }
}

impl<T> Spanned<T> {
    pub fn new(location: Location, elem: T) -> Spanned<T> {
        Spanned { location, elem }
    }
}

/// A (typically absolute) tree path where the path operator is irrelevant.
pub type TreePath = Box<[Ident]>;

pub struct FormatTreePath<'a, T>(pub &'a [T]);

impl<'a, T: fmt::Display> fmt::Display for FormatTreePath<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for each in self.0.iter() {
            write!(f, "/{}", each)?;
        }
        Ok(())
    }
}

/// A series of identifiers separated by path operators.
pub type TypePath = Vec<(PathOp, Ident)>;

pub struct FormatTypePath<'a>(pub &'a [(PathOp, Ident)]);

impl<'a> fmt::Display for FormatTypePath<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for each in self.0.iter() {
            write!(f, "{}{}", each.0, each.1)?;
        }
        Ok(())
    }
}

// ----------------------------------------------------------------------------
// Terms and Expressions

/// A typepath optionally followed by a set of variables.
#[derive(Clone, PartialEq, Debug)]
pub struct Prefab {
    pub path: TypePath,
    pub vars: Box<[(Ident2, Expression)]>,
}

impl From<TypePath> for Prefab {
    fn from(path: TypePath) -> Self {
        Prefab {
            path,
            vars: Default::default(),
        }
    }
}

/// Formatting helper for variable arrays.
pub struct FormatVars<'a, T>(pub &'a T);

impl<'a, T, K, V> fmt::Display for FormatVars<'a, T>
where
    &'a T: IntoIterator<Item = (K, V)>,
    K: fmt::Display,
    V: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for (k, v) in self.0 {
            write!(f, "{}{} = {}", if first { " {" } else { "; " }, k, v)?;
            first = false;
        }
        if !first {
            f.write_str("}")?;
        }
        Ok(())
    }
}

/// The structure of an expression, a tree of terms and operators.
#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    /// An expression containing a term directly. The term is evaluated first,
    /// then its follows, then its unary operators in reverse order.
    Base {
        /// The term of the expression.
        term: Box<Spanned<Term>>,
        /// The follow operations applied to this value.
        follow: Box<[Spanned<Follow>]>,
    },
    /// A binary operation.
    BinaryOp {
        /// The binary operation.
        op: BinaryOp,
        /// The left-hand side of the operation.
        lhs: Box<Expression>,
        /// The right-hand side of the operation.
        rhs: Box<Expression>,
    },
    /// An assignment operation.
    AssignOp {
        /// The assignment operation.
        op: AssignOp,
        /// The left-hand side of the assignment.
        lhs: Box<Expression>,
        /// The right-hand side of the assignment.
        rhs: Box<Expression>,
    },
    /// A ternary operation.
    TernaryOp {
        /// The condition.
        cond: Box<Expression>,
        /// The value if the condition is truthy.
        if_: Box<Expression>,
        /// The value otherwise.
        else_: Box<Expression>,
    },
}

impl Expression {
    /// If this expression consists of a single term, return it.
    pub fn as_term(&self) -> Option<&Term> {
        match self {
            &Expression::Base {
                ref term,
                ref follow,
            } if follow.is_empty() => Some(&term.elem),
            _ => None,
        }
    }

    /// If this expression consists of a single term, return it.
    pub fn into_term(self) -> Option<Term> {
        match self {
            Expression::Base { term, follow } => {
                if follow.is_empty() {
                    Some(term.elem)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn is_const_eval(&self) -> bool {
        match self {
            Expression::BinaryOp { op, lhs, rhs } => {
                guard!(let Some(lhterm) = lhs.as_term() else {
                    return false
                });
                guard!(let Some(rhterm) = rhs.as_term() else {
                    return false
                });
                if !lhterm.is_static() {
                    return false;
                }
                if !rhterm.is_static() {
                    return false;
                }
                matches!(
                    op,
                    BinaryOp::Eq
                        | BinaryOp::NotEq
                        | BinaryOp::Less
                        | BinaryOp::Greater
                        | BinaryOp::LessEq
                        | BinaryOp::GreaterEq
                        | BinaryOp::And
                        | BinaryOp::Or
                )
            }
            _ => false,
        }
    }

    pub fn is_truthy(&self) -> Option<bool> {
        match self {
            Expression::Base { term, follow } => {
                guard!(let Some(mut truthy) = term.elem.is_truthy() else {
                    return None;
                });
                for follow in follow.iter() {
                    match follow.elem {
                        Follow::Unary(UnaryOp::Not) => truthy = !truthy,
                        _ => return None,
                    }
                }
                Some(truthy)
            }
            Expression::BinaryOp { op, lhs, rhs } => {
                guard!(let Some(lhtruth) = lhs.is_truthy() else {
                    return None
                });
                guard!(let Some(rhtruth) = rhs.is_truthy() else {
                    return None
                });
                match op {
                    BinaryOp::And => Some(lhtruth && rhtruth),
                    BinaryOp::Or => Some(lhtruth || rhtruth),
                    _ => None,
                }
            }
            Expression::AssignOp { op, lhs: _, rhs } => {
                if let AssignOp::Assign = op {
                    return match rhs.as_term() {
                        Some(term) => term.is_truthy(),
                        _ => None,
                    };
                } else {
                    None
                }
            }
            Expression::TernaryOp { cond, if_, else_ } => {
                guard!(let Some(condtruth) = cond.is_truthy() else {
                    return None
                });
                if condtruth {
                    if_.is_truthy()
                } else {
                    else_.is_truthy()
                }
            }
        }
    }
}

impl From<Term> for Expression {
    fn from(term: Term) -> Expression {
        match term {
            Term::Expr(expr) => *expr,
            term => Expression::Base {
                term: Box::new(Spanned::new(Default::default(), term)),
                follow: Default::default(),
            },
        }
    }
}

/// The structure of a term, the basic building block of the AST.
#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    // Terms with no recursive contents ---------------------------------------
    /// The literal `null`.
    Null,
    /// An integer literal.
    Int(i32),
    /// A floating-point literal.
    Float(f32),
    /// An identifier.
    Ident(Ident),
    /// A string literal.
    String(String),
    /// A resource literal.
    Resource(String),
    /// An `as()` call, with an input type. Undocumented.
    As(InputType),

    // Non-function calls with recursive contents -----------------------------
    /// An expression contained in a term.
    Expr(Box<Expression>),
    /// A prefab literal (path + vars).
    Prefab(Box<Prefab>),
    /// An interpolated string, alternating string/expr/string/expr.
    InterpString(Ident2, Box<[(Option<Expression>, Box<str>)]>),

    // Function calls with recursive contents ---------------------------------
    /// An unscoped function call.
    Call(Ident2, Box<[Expression]>),
    /// A `.()` call.
    SelfCall(Box<[Expression]>),
    /// A `..()` call. If arguments is empty, the proc's arguments are passed.
    ParentCall(Box<[Expression]>),
    /// A `new()` call.
    NewImplicit {
        /// The list of arguments to pass to the `New()` proc.
        args: Option<Box<[Expression]>>,
    },
    /// A `new /type()` call.
    NewPrefab {
        /// The type to be instantiated.
        prefab: Box<Prefab>,
        /// The list of arguments to pass to the `New()` proc.
        args: Option<Box<[Expression]>>,
    },
    /// A `new foo.bar()` call.
    NewMiniExpr {
        /// The miniature expression.
        expr: Box<MiniExpr>,
        /// The list of arguments to pass to the `New()` proc.
        args: Option<Box<[Expression]>>,
    },
    /// A `list` call. Associations are represented by assignment expressions.
    List(Box<[Expression]>),
    /// An `input` call.
    Input {
        args: Box<[Expression]>,
        input_type: Option<InputType>,    // as
        in_list: Option<Box<Expression>>, // in
    },
    /// A `locate` call.
    Locate {
        args: Box<[Expression]>,
        in_list: Option<Box<Expression>>, // in
    },
    /// A `pick` call, possibly with weights.
    Pick(Box<PickArgs>),
    /// A use of the `call()()` primitive.
    DynamicCall(Box<[Expression]>, Box<[Expression]>),
}

impl Term {
    pub fn is_static(&self) -> bool {
        matches!(
            self,
            Term::Null | Term::Int(_) | Term::Float(_) | Term::String(_) | Term::Prefab(_)
        )
    }

    pub fn is_truthy(&self) -> Option<bool> {
        return match self {
            // `null`, `0`, and empty strings are falsey.
            Term::Null => Some(false),
            Term::Int(i) => Some(*i != 0),
            Term::Float(i) => Some(*i != 0f32),
            Term::String(s) => Some(!s.is_empty()),

            // Paths/prefabs are truthy.
            Term::Prefab(_) => Some(true),
            // `new()` and `list()` return the newly-created reference.
            Term::NewImplicit { .. } => Some(true),
            Term::NewPrefab { .. } => Some(true),
            Term::NewMiniExpr { .. } => Some(true),
            Term::List(_) => Some(true),

            // Truthy if any of the literal parts are non-empty.
            // Otherwise, don't try to determine the content of the parts.
            Term::InterpString(first, parts) => {
                if !first.is_empty() || parts.iter().any(|(_, p)| !p.is_empty()) {
                    Some(true)
                } else {
                    None
                }
            }

            // Recurse.
            Term::Expr(e) => e.is_truthy(),

            _ => None,
        };
    }

    pub fn valid_for_range(&self, other: &Term, step: Option<&Expression>) -> Option<bool> {
        if let Term::Int(i) = *self {
            if let Term::Int(o) = *other {
                // edge case
                if i == 0 && o == 0 {
                    return Some(false);
                }
                if let Some(stepexp) = step {
                    if let Some(stepterm) = stepexp.as_term() {
                        if let Term::Int(_s) = stepterm {
                            return Some(true);
                        }
                    } else {
                        return Some(true);
                    }
                }
                return Some(i <= o);
            }
        }
        None
    }
}

impl From<Expression> for Term {
    fn from(expr: Expression) -> Term {
        match expr {
            Expression::Base { term, follow } => {
                if follow.is_empty() {
                    match term.elem {
                        Term::Expr(expr) => Term::from(*expr),
                        other => other,
                    }
                } else {
                    Term::Expr(Box::new(Expression::Base { term, follow }))
                }
            }
            other => Term::Expr(Box::new(other)),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct MiniExpr {
    pub ident: Ident2,
    pub fields: Box<[Field]>,
}

/// An expression part which is applied to a term or another follow.
#[derive(Debug, Clone, PartialEq)]
pub enum Follow {
    /// Index the value by an expression.
    Index(ListAccessKind, Box<Expression>),
    /// Access a field of the value.
    Field(PropertyAccessKind, Ident2),
    /// Call a method of the value.
    Call(PropertyAccessKind, Ident2, Box<[Expression]>),
    /// Apply a unary operator to the value.
    Unary(UnaryOp),
}

/// Like a `Follow` but only supports field accesses.
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub kind: PropertyAccessKind,
    pub ident: Ident2,
}

impl From<Field> for Follow {
    fn from(field: Field) -> Follow {
        Follow::Field(field.kind, field.ident)
    }
}

/// A parameter declaration in the header of a proc.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Parameter {
    pub var_type: VarType,
    pub name: Ident,
    pub default: Option<Expression>,
    pub input_type: Option<InputType>,
    pub in_list: Option<Expression>,
    pub location: Location,
}

impl fmt::Display for Parameter {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}{}", self.var_type, self.name)?;
        if let Some(input_type) = self.input_type {
            write!(fmt, " as {}", input_type)?;
        }
        Ok(())
    }
}

/// A type which may be ascribed to a `var`.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct VarType {
    pub flags: VarTypeFlags,
    pub type_path: TreePath,
}

impl VarType {
    #[inline]
    pub fn is_const_evaluable(&self) -> bool {
        self.flags.is_const_evaluable()
    }

    #[inline]
    pub fn is_normal(&self) -> bool {
        self.flags.is_normal()
    }
}

impl FromIterator<String> for VarType {
    fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
        VarTypeBuilder::from_iter(iter).build()
    }
}

impl fmt::Display for VarType {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.flags.fmt(fmt)?;
        for bit in self.type_path.iter() {
            fmt.write_str(bit)?;
            fmt.write_str("/")?;
        }
        Ok(())
    }
}

/// Builder for `VarType` with useful mutation methods.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct VarTypeBuilder {
    pub flags: VarTypeFlags,
    pub type_path: Vec<Ident>,
}

impl VarTypeBuilder {
    pub fn suffix(&mut self, suffix: &VarSuffix) {
        if !suffix.list.is_empty() {
            self.type_path.insert(0, "list".to_owned());
        }
    }

    pub fn build(self) -> VarType {
        VarType {
            flags: self.flags,
            type_path: self.type_path.into_boxed_slice(),
        }
    }
}

impl FromIterator<String> for VarTypeBuilder {
    fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
        let mut flags = VarTypeFlags::default();
        let type_path = iter
            .into_iter()
            .skip_while(|p| {
                if let Some(flag) = VarTypeFlags::from_name(p) {
                    flags |= flag;
                    true
                } else {
                    false
                }
            })
            .collect();
        VarTypeBuilder { flags, type_path }
    }
}

/// Suffixes which may appear after a variable's name in its declaration.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct VarSuffix {
    // var/L[], var/L[10]
    pub list: Vec<Option<Expression>>,
}

impl VarSuffix {
    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    pub fn into_initializer(self) -> Option<Expression> {
        // `var/L[10]` is equivalent to `var/list/L = new /list(10)`
        // `var/L[2][][3]` is equivalent to `var/list/list/list = new /list(2, 3)`
        let args: Vec<_> = self.list.into_iter().flatten().collect();
        if args.is_empty() {
            None
        } else {
            Some(Expression::from(Term::NewPrefab {
                prefab: Box::new(Prefab::from(vec![(PathOp::Slash, "list".to_owned())])),
                args: Some(args.into_boxed_slice()),
            }))
        }
    }
}

// ----------------------------------------------------------------------------
// Statements

/// A block of statements.
pub type Block = Box<[Spanned<Statement>]>;

/// A statement in a proc body.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expr(Expression),
    Return(Option<Expression>),
    Throw(Expression),
    While {
        condition: Expression,
        block: Block,
    },
    DoWhile {
        block: Block,
        condition: Box<Spanned<Expression>>,
    },
    If {
        arms: Vec<(Spanned<Expression>, Block)>,
        else_arm: Option<Block>,
    },
    ForInfinite {
        block: Block,
    },
    ForLoop {
        init: Option<Box<Statement>>,
        test: Option<Box<Expression>>,
        inc: Option<Box<Statement>>,
        block: Block,
    },
    ForList(Box<ForListStatement>),
    ForRange(Box<ForRangeStatement>),
    Var(Box<VarStatement>),
    Vars(Vec<VarStatement>),
    Setting {
        name: Ident2,
        mode: SettingMode,
        value: Expression,
    },
    Spawn {
        delay: Option<Expression>,
        block: Block,
    },
    Switch {
        input: Box<Expression>,
        cases: Box<SwitchCases>,
        default: Option<Block>,
    },
    TryCatch {
        try_block: Block,
        catch_params: Box<[TreePath]>,
        catch_block: Block,
    },
    Continue(Option<Ident>),
    Break(Option<Ident>),
    Goto(Ident),
    Label {
        name: Ident,
        block: Block,
    },
    Del(Expression),
    Crash(Option<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarStatement {
    pub var_type: VarType,
    pub name: Ident,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Case {
    Exact(Expression),
    Range(Expression, Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForListStatement {
    pub var_type: Option<VarType>,
    pub name: Ident2,
    /// If zero, uses the declared type of the variable.
    pub input_type: Option<InputType>,
    /// Defaults to 'world'.
    pub in_list: Option<Expression>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForRangeStatement {
    pub var_type: Option<VarType>,
    pub name: Ident2,
    pub start: Expression,
    pub end: Expression,
    pub step: Option<Expression>,
    pub block: Block,
}

// ----------------------------------------------------------------------------
// Miscellaneous

pub const KNOWN_SETTING_NAMES: &[&str] = &[
    "name",
    "desc",
    "category",
    "hidden",
    "popup_menu",
    "instant",
    "invisibility",
    "src",
    "background",
    "waitfor",
];

// TODO: maybe put this somewhere more suitable?
pub static VALID_FILTER_TYPES: phf::Map<&'static str, &[&str]> = phf_map! {
    "alpha" => &[ "x", "y", "icon", "render_source", "flags" ],
    "angular_blur" => &[ "x", "y", "size" ],
    "bloom" => &[ "threshold", "size", "offset", "alpha" ],
    "color" => &[ "color", "space" ],
    "displace" => &[ "x", "y", "size", "icon", "render_source" ],
    "drop_shadow" => &[ "x", "y", "size", "offset", "color"],
    "blur" => &[ "size" ],
    "layer" => &[ "x", "y", "icon", "render_source", "flags", "color", "transform", "blend_mode" ],
    "motion_blur" => &[ "x", "y" ],
    "outline" => &[ "size", "color", "flags" ],
    "radial_blur" => &[ "x", "y", "size" ],
    "rays" => &[ "x", "y", "size", "color", "offset", "density", "threshold", "factor", "flags" ],
    "ripple" => &[ "x", "y", "size", "repeat", "radius", "falloff", "flags" ],
    "wave" => &[ "x", "y", "size", "offset", "flags" ],
};

// filter type => (flag field name, exclusive, can_be_0, valid flag values)
pub static VALID_FILTER_FLAGS: phf::Map<&'static str, (&str, bool, bool, &[&str])> = phf_map! {
    "alpha" => ("flags", false, true, &[ "MASK_INVERSE", "MASK_SWAP" ]),
    "color" => ("space", true, false, &[ "FILTER_COLOR_RGB", "FILTER_COLOR_HSV", "FILTER_COLOR_HSL", "FILTER_COLOR_HCY" ]),
    "layer" => ("flags", true, true, &[ "FILTER_OVERLAY", "FILTER_UNDERLAY" ]),
    "rays" => ("flags", false, true, &[ "FILTER_OVERLAY", "FILTER_UNDERLAY" ]),
    "outline" => ("flags", false, true, &[ "OUTLINE_SHARP", "OUTLINE_SQUARE" ]),
    "ripple" => ("flags", false, true, &[ "WAVE_BOUNDED" ]),
    "wave" => ("flags", false, true, &[ "WAVE_SIDEWAYS", "WAVE_BOUNDED" ]),
};
