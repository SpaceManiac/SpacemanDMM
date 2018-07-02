//! The DM abstract syntax tree.
//!
//! Most AST types can be pretty-printed using the `Display` trait.
use std::fmt;

use linked_hash_map::LinkedHashMap;

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
    pub fn around<T: fmt::Display>(self, expr: &T) -> Around<T> {
        Around { op: self, expr }
    }
}

/// A formatting wrapper created by `UnaryOp::around`.
pub struct Around<'a, T: 'a> {
    op: UnaryOp,
    expr: &'a T,
}

impl<'a, T: fmt::Display> fmt::Display for Around<'a, T> {
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

/// The DM path operators.
///
/// Which path operator is used typically only matters at the start of a path.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PathOp {
    /// `/` for absolute pathing.
    Slash,
    /// `.` for checked relative pathing.
    Dot,
    /// `:` for unchecked relative pathing.
    Colon,
}

impl fmt::Display for PathOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PathOp::Slash => f.write_str("/"),
            PathOp::Dot => f.write_str("."),
            PathOp::Colon => f.write_str(":"),
        }
    }
}

/// A series of identifiers separated by path operators.
pub type TypePath = Vec<(PathOp, String)>;

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
    BitAnd,
    BitXor,
    BitOr,
    LShift,
    RShift,
    And,
    Or,
    In,
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
            BitAnd => "&",
            BitXor => "^",
            BitOr => "|",
            LShift => "<<",
            RShift => ">>",
            And => "&&",
            Or => "||",
            In => "in",
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
    BitAndAssign,
    BitOrAssign,
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
            BitAndAssign => "&=",
            BitXorAssign => "^=",
            BitOrAssign => "|=",
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
    BitAnd = BitAndAssign;
    BitOr = BitOrAssign;
    BitXor = BitXorAssign;
    LShift = LShiftAssign;
    RShift = RShiftAssign;
}

/// A path optionally followed by a set of variables.
#[derive(Clone, PartialEq, Debug)]
pub struct Prefab<E=Expression> {
    pub path: TypePath,
    pub vars: LinkedHashMap<String, E>,
}

impl<E: fmt::Display> fmt::Display for Prefab<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for each in self.path.iter() {
            write!(f, "{}{}", each.0, each.1)?;
        }
        if !self.vars.is_empty() {
            write!(f, " {{")?;
            let mut first = true;
            for (k, v) in self.vars.iter() {
                if !first {
                    write!(f, "; ")?;
                }
                first = false;
                write!(f, "{} = {}", k, v)?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

/// The different forms of the `new` command.
#[derive(Clone, PartialEq, Debug)]
pub enum NewType<E=Expression> {
    /// Implicit type, taken from context.
    Implicit,
    /// The name of a variable in which to find the prefab to instantiate.
    Ident(String),
    /// A prefab to be instantiated.
    Prefab(Prefab<E>),
}

impl<E: fmt::Display> fmt::Display for NewType<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NewType::Implicit => Ok(()),
            NewType::Ident(ref name) => write!(f, " {}", name),
            NewType::Prefab(ref prefab) => write!(f, " {}", prefab),
        }
    }
}

/// The structure of an expression, a tree of terms and operators.
#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    /// An expression containing a term directly. The term is evaluated first,
    /// then its follows, then its unary operators in reverse order.
    Base {
        /// The unary operations applied to this value, in reverse order.
        unary: Vec<UnaryOp>,
        /// The term of the expression.
        term: Term,
        /// The follow operations applied to this value.
        follow: Vec<Follow>,
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
    }
}

impl From<Term> for Expression {
    fn from(term: Term) -> Expression {
        match term {
            Term::Expr(expr) => *expr,
            term => Expression::Base {
                unary: vec![],
                follow: vec![],
                term,
            }
        }
    }
}

/// The structure of a term, the basic building block of the AST.
#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    /// The literal `null`.
    Null,
    /// A `new` call.
    New {
        /// The type to be instantiated.
        type_: NewType,
        /// The list of arguments to pass to the `New()` proc.
        args: Option<Vec<Expression>>,
    },
    /// A `list` call. Associations are represented by assignment expressions.
    List(Vec<Expression>),
    /// An unscoped function call.
    Call(String, Vec<Expression>),
    /// A prefab literal (path + vars).
    Prefab(Prefab),
    /// An identifier.
    Ident(String),
    /// A string literal.
    String(String),
    /// A resource literal.
    Resource(String),
    /// An integer literal.
    Int(i32),
    /// A floating-point literal.
    Float(f32),
    /// An expression contained in a term.
    Expr(Box<Expression>),
    /// The current proc's return value (`.`).
    ReturnValue,
    /// A use of the `call()()` primitive.
    DynamicCall(Vec<Expression>, Vec<Expression>),
    /// An interpolated string, alternating string/expr/string/expr.
    InterpString(String, Vec<(Expression, String)>),
}

impl From<Expression> for Term {
    fn from(expr: Expression) -> Term {
        match expr {
            Expression::Base { term, unary, follow } => if unary.is_empty() && follow.is_empty() {
                match term {
                    Term::Expr(expr) => Term::from(*expr),
                    other => other,
                }
            } else {
                Term::Expr(Box::new(Expression::Base { term, unary, follow }))
            },
            other => Term::Expr(Box::new(other))
        }
    }
}

/// An expression part which is applied to a term or another follow.
#[derive(Debug, Clone, PartialEq)]
pub enum Follow {
    /// Access a field of the value.
    Field(String),
    /// Index the value by an expression.
    Index(Box<Expression>),
    /// Call a method of the value.
    Call(String, Vec<Expression>),
    /// Cast the value using the "as" operator.
    Cast(String),
}

/// A parameter declaration in the header of a proc.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Parameter {
    pub path: Vec<String>,
    pub name: String,
    pub default: Option<Expression>,
    pub as_types: Option<Vec<String>>,
    pub in_list: Option<Expression>,
}

impl fmt::Display for Parameter {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for each in self.path.iter() {
            write!(fmt, "{}/", each)?;
        }
        fmt.write_str(&self.name)?;
        if let Some(ref as_types) = self.as_types {
            fmt.write_str(" as ")?;
            let mut first = true;
            for each in as_types.iter() {
                write!(fmt, "{}{}", if first { "" } else { "|" }, each)?;
                first = false;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expr(Expression),
    Return(Option<Expression>),
    While(Expression, Vec<Statement>),
    DoWhile(Vec<Statement>, Expression),
    If(Vec<(Expression, Vec<Statement>)>, Option<Vec<Statement>>),
}
