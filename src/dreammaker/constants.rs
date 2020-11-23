//! The constant folder/evaluator, used by the preprocessor and object tree.
use std::fmt;
use std::ops;
use std::path::Path;

use linked_hash_map::LinkedHashMap;
use ordered_float::OrderedFloat;

use super::{DMError, Location, HasLocation, Context};
use super::objtree::*;
use super::ast::*;
use super::preprocessor::DefineMap;

/// An absolute typepath and optional variables.
///
/// The path may involve `/proc` or `/verb` references.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Pop {
    pub path: TreePath,
    pub vars: LinkedHashMap<Ident, Constant>,
}

impl From<TreePath> for Pop {
    fn from(path: TreePath) -> Self {
        Pop {
            path,
            vars: Default::default(),
        }
    }
}

impl fmt::Display for Pop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", FormatTreePath(&self.path), FormatVars(&self.vars))
    }
}

/// A DM constant, usually a literal or simple combination of other constants.
///
/// This is intended to represent the degree to which constants are evaluated
/// before being displayed in DreamMaker.
#[derive(Debug, Clone)]
pub enum Constant {
    /// The literal `null`.
    Null(Option<TreePath>),
    /// A `new` call.
    New {
        /// The type to be instantiated.
        type_: Option<Pop>,
        /// The list of arugments to pass to the `New()` proc.
        args: Option<Vec<(Constant, Option<Constant>)>>,
    },
    /// A `list` literal. Elements have optional associations.
    List(Vec<(Constant, Option<Constant>)>),
    /// A call to a constant type constructor.
    Call(ConstFn, Vec<(Constant, Option<Constant>)>),
    /// A prefab literal.
    Prefab(Pop),
    /// A string literal.
    String(String),
    /// A resource literal.
    Resource(String),
    /// An integer literal.
    Int(i32),
    /// A floating-point literal.
    Float(f32),
}

// Manual Hash and Eq impls using OrderedFloat, so that we get the desired
// upstream properties without having to wrap/unwrap at all hours of the day.
impl std::hash::Hash for Constant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Constant::Null(p) => p.hash(state),
            Constant::New { type_, args } => (type_, args).hash(state),
            Constant::List(list) => list.hash(state),
            Constant::Call(f, args) => (f, args).hash(state),
            Constant::Prefab(pop) => pop.hash(state),
            Constant::String(s) => s.hash(state),
            Constant::Resource(s) => s.hash(state),
            Constant::Int(i) => i.hash(state),
            Constant::Float(f) => OrderedFloat(*f).hash(state),
        }
    }
}

impl std::cmp::PartialEq for Constant {
    fn eq(&self, other: &Constant) -> bool {
        match (self, other) {
            (Constant::Null(p1), Constant::Null(p2)) => p1 == p2,
            (Constant::New { type_: type1, args: args1 }, Constant::New { type_: type2, args: args2 }) => (type1, args1) == (type2, args2),
            (Constant::List(l1), Constant::List(l2)) => l1 == l2,
            (Constant::Call(f1, args1), Constant::Call(f2, args2)) => (f1, args1) == (f2, args2),
            (Constant::Prefab(pop1), Constant::Prefab(pop2)) => pop1 == pop2,
            (Constant::String(s1), Constant::String(s2)) => s1 == s2,
            (Constant::Resource(s1), Constant::Resource(s2)) => s1 == s2,
            (Constant::Int(i1), Constant::Int(i2)) => i1 == i2,
            (Constant::Float(f1), Constant::Float(f2)) => OrderedFloat(*f1) == OrderedFloat(*f2),
            (Constant::Int(i), Constant::Float(f)) |
            (Constant::Float(f), Constant::Int(i)) => OrderedFloat(*f) == OrderedFloat(*i as f32),
            _ => false,
        }
    }
}

impl std::cmp::Eq for Constant {}

/// The constant functions which are represented as-is.
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum ConstFn {
    /// The `icon()` type constructor.
    Icon,
    /// The `matrix()` type constructor.
    Matrix,
    /// The `newlist()` function, which combines `new` mapped over a `list`.
    Newlist,
    /// The `sound()` type constructor.
    Sound,
    /// The `filter()` type constructor.
    Filter,
    /// The `file()` annotator (marks a string as `isfile`).
    File,
}

/// A constant-evaluation error (usually type mismatch).
pub struct EvalError;

impl Constant {
    // ------------------------------------------------------------------------
    // Constructors

    pub const EMPTY_STRING: &'static Constant = &Constant::String(String::new());

    pub fn null() -> &'static Constant {
        static NULL: Constant = Constant::Null(None);
        &NULL
    }

    #[inline]
    pub fn string<S: Into<String>>(s: S) -> Constant {
        Constant::String(s.into())
    }

    // ------------------------------------------------------------------------
    // Conversions

    #[inline]
    pub fn is_null(&self) -> bool {
        matches!(*self, Constant::Null(_))
    }

    pub fn to_bool(&self) -> bool {
        match *self {
            Constant::Null(_) => false,
            Constant::Int(i) => i != 0,
            Constant::Float(f) => f != 0.,
            Constant::String(ref s) => !s.is_empty(),
            _ => true,
        }
    }

    pub fn to_float(&self) -> Option<f32> {
        match *self {
            Constant::Int(i) => Some(i as f32),
            Constant::Float(f) => Some(f),
            _ => None,
        }
    }

    pub fn to_int(&self) -> Option<i32> {
        match *self {
            Constant::Int(i) => Some(i),
            Constant::Float(f) => Some(f as i32),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match *self {
            Constant::String(ref s) => Some(s.as_ref()),
            _ => None,
        }
    }

    pub fn as_path_str(&self) -> Option<&str> {
        match *self {
            Constant::String(ref s) |
            Constant::Resource(ref s) => Some(s),
            _ => None,
        }
    }

    #[inline]
    pub fn as_path(&self) -> Option<&Path> {
        self.as_path_str().map(Path::new)
    }

    // ------------------------------------------------------------------------
    // Comparisons

    pub fn eq_string(&self, string: &str) -> bool {
        match *self {
            Constant::String(ref s) => s == string,
            _ => false,
        }
    }

    pub fn eq_resource(&self, resource: &str) -> bool {
        match self {
            Constant::String(ref s) |
            Constant::Resource(ref s) => s == resource,
            _ => false,
        }
    }

    // ------------------------------------------------------------------------
    // Operations

    pub fn contains_key(&self, key: &Constant) -> bool {
        if let Constant::List(ref elements) = *self {
            for &(ref k, _) in elements {
                if key == k {
                    return true;
                }
            }
        }
        false
    }

    pub fn index(&self, key: &Constant) -> Option<&Constant> {
        match (self, key) {
            (&Constant::List(ref elements), &Constant::Int(i)) => return elements.get(i as usize).map(|&(ref k, _)| k),
            (&Constant::List(ref elements), key) => for &(ref k, ref v) in elements {
                if key == k {
                    return v.as_ref();
                }
            },
            _ => {}
        }
        None
    }

    pub fn negate(&self) -> Result<Constant, EvalError> {
        Ok(match *self {
            Constant::Int(i) => Constant::Int(-i),
            Constant::Float(i) => Constant::Float(-i),
            _ => return Err(EvalError),
        })
    }
}

impl Default for Constant {
    fn default() -> Self {
        Constant::Null(None)
    }
}

impl From<i32> for Constant {
    fn from(value: i32) -> Constant {
        Constant::Int(value)
    }
}

impl From<f32> for Constant {
    fn from(value: f32) -> Constant {
        Constant::Float(value)
    }
}

impl From<bool> for Constant {
    fn from(value: bool) -> Constant {
        match value {
            true => Constant::Int(1),
            false => Constant::Int(0),
        }
    }
}

impl PartialEq<str> for Constant {
    fn eq(&self, other: &str) -> bool {
        match self {
            Constant::String(ref s) |
            Constant::Resource(ref s) => s == other,
            _ => false,
        }
    }
}

impl ops::Not for Constant {
    type Output = Constant;

    fn not(self) -> Constant {
        <&Constant>::not(&self)
    }
}

impl<'a> ops::Not for &'a Constant {
    type Output = Constant;

    fn not(self) -> Constant {
        Constant::from(!self.to_bool())
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Constant::Null(_) => f.write_str("null"),
            Constant::New { ref type_, ref args } => {
                f.write_str("new")?;
                if let Some(prefab) = type_ {
                    write!(f, " {}", prefab)?;
                }
                if let Some(args) = args.as_ref() {
                    write!(f, "(")?;
                    let mut first = true;
                    for each in args.iter() {
                        if !first {
                            write!(f, ", ")?;
                        }
                        first = false;
                        write!(f, "{}", each.0)?;
                        if let Some(val) = each.1.as_ref() {
                            write!(f, " = {}", val)?;
                        }
                    }
                    write!(f, ")")?;
                }
                Ok(())
            },
            Constant::List(ref list) => {
                write!(f, "list(")?;
                let mut first = true;
                let mut previous_assoc = false;
                for &(ref key, ref val) in list.iter() {
                    if !first {
                        write!(f, ",")?;
                        if previous_assoc {
                            write!(f, " ")?;
                        }
                    }
                    first = false;
                    previous_assoc = false;
                    write!(f, "{}", key)?;
                    if let Some(val) = val.as_ref() {
                        write!(f, " = {}", val)?;
                        previous_assoc = true;
                    }
                }
                write!(f, ")")
            },
            Constant::Call(const_fn, ref list) => {
                write!(f, "{}(", const_fn)?;
                let mut first = true;
                for (key, val) in list.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{}", key)?;
                    if let Some(val) = val {
                        write!(f, " = {}", val)?;
                    }
                }
                write!(f, ")")
            },
            Constant::Prefab(ref val) => write!(f, "{}", val),
            Constant::String(ref val) => crate::lexer::Quote(val).fmt(f),
            Constant::Resource(ref val) => write!(f, "'{}'", val),
            Constant::Int(val) => crate::lexer::FormatFloat(val as f32).fmt(f),
            Constant::Float(val) => crate::lexer::FormatFloat(val).fmt(f),
        }
    }
}

impl fmt::Display for ConstFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match *self {
            ConstFn::Icon => "icon",
            ConstFn::Matrix => "matrix",
            ConstFn::Newlist => "newlist",
            ConstFn::Sound => "sound",
            ConstFn::Filter => "filter",
            ConstFn::File => "file",
        })
    }
}

// ----------------------------------------------------------------------------
// The constant evaluator

pub fn evaluate_str(location: Location, input: &[u8]) -> Result<Constant, DMError> {
    use super::lexer::{Lexer, from_utf8_or_latin1_borrowed};

    let ctx = Context::default();
    let mut lexer = Lexer::new(&ctx, location.file, input);
    let expr = crate::parser::parse_expression(&ctx, location, &mut lexer)?;
    let leftover = lexer.remaining();
    if !leftover.is_empty() {
        return Err(DMError::new(location, format!("leftover: {:?} {}", from_utf8_or_latin1_borrowed(&input), leftover.len())));
    }
    expr.simple_evaluate(location)
}

impl Expression {
    /// Evaluate this expression in the absence of any surrounding context.
    pub fn simple_evaluate(self, location: Location) -> Result<Constant, DMError> {
        ConstantFolder {
            tree: None,
            location,
            ty: NodeIndex::new(0),
            defines: None,
        }.expr(self, None)
    }
}

/// Evaluate an expression in the preprocessor, with `defined()` available.
pub fn preprocessor_evaluate(location: Location, expr: Expression, defines: &DefineMap) -> Result<Constant, DMError> {
    ConstantFolder {
        tree: None,
        location,
        ty: NodeIndex::new(0),
        defines: Some(defines),
    }.expr(expr, None)
}

/// Evaluate all the type-level variables in an object tree into constants.
pub(crate) fn evaluate_all(context: &Context, tree: &mut ObjectTree) {
    for ty in tree.node_indices() {
        let keys: Vec<String> = tree[ty].vars.keys().cloned().collect();
        for key in keys {
            if !tree[ty]
                .get_var_declaration(&key, tree)
                .map_or(true, |x| {
                    x.var_type.is_const_evaluable() && (x.var_type.flags.is_const() || ty != NodeIndex::new(0))
                })
            {
                continue;  // skip non-constant-evaluable vars
            }
            match constant_ident_lookup(tree, ty, &key, false) {
                Err(err) => context.register_error(err),
                Ok(ConstLookup::Found(_, _)) => {}
                Ok(ConstLookup::Continue(_)) => {
                    context.register_error(DMError::new(
                        tree[ty].vars[&key].value.location,
                        format!(
                            "undefined var '{}' on type '{}'",
                            key,
                            tree[ty].path,
                        ),
                    ));
                }
            }
        }
    }
}

enum ConstLookup {
    Found(TreePath, Constant),
    Continue(Option<NodeIndex>),
}

fn constant_ident_lookup(
    tree: &mut ObjectTree,
    ty: NodeIndex,
    ident: &str,
    must_be_const: bool,
) -> Result<ConstLookup, DMError> {
    // try to read the currently-set value if we can and
    // substitute that in, otherwise try to evaluate it.
    let (location, type_hint, expr) = {
        let decl = match tree[ty]
            .get_var_declaration(ident, tree)
            .cloned()
        {
            Some(decl) => decl,
            None => return Ok(ConstLookup::Continue(None)),  // definitely doesn't exist
        };

        let type_ = &mut tree[ty];
        let parent = type_.parent_type_index();
        match type_.vars.get_mut(ident) {
            None => return Ok(ConstLookup::Continue(parent)),
            Some(var) => match var.value.constant.clone() {
                Some(constant) => return Ok(ConstLookup::Found(decl.var_type.type_path.clone(), constant)),
                None => match var.value.expression.clone() {
                    Some(expr) => {
                        if var.value.being_evaluated {
                            return Err(DMError::new(
                                var.value.location,
                                format!("recursive constant reference: {}", ident),
                            ));
                        } else if !decl.var_type.is_const_evaluable() {
                            return Err(DMError::new(
                                var.value.location,
                                format!("non-const-evaluable variable: {}", ident),
                            ));
                        } else if !decl.var_type.flags.is_const() && must_be_const {
                            return Err(DMError::new(
                                var.value.location,
                                format!("non-const variable: {}", ident),
                            ));
                        }
                        var.value.being_evaluated = true;
                        (var.value.location, decl.var_type.type_path, expr)
                    }
                    None => {
                        let c = Constant::Null(Some(decl.var_type.type_path.clone()));
                        var.value.constant = Some(c.clone());
                        return Ok(ConstLookup::Found(decl.var_type.type_path, c));
                    }
                },
            },
        }
    };
    // evaluate full_value
    let value = ConstantFolder {
        tree: Some(tree),
        defines: None,
        location,
        ty,
    }.expr(expr, if type_hint.is_empty() { None } else { Some(&type_hint) })?;
    // and store it into 'value', then return it
    let var = tree[ty].vars.get_mut(ident).unwrap();
    var.value.constant = Some(value.clone());
    var.value.being_evaluated = false;
    Ok(ConstLookup::Found(type_hint, value))
}

struct ConstantFolder<'a> {
    tree: Option<&'a mut ObjectTree>,
    defines: Option<&'a DefineMap>,
    location: Location,
    ty: NodeIndex,
}

impl<'a> HasLocation for ConstantFolder<'a> {
    fn location(&self) -> Location {
        self.location
    }
}

impl<'a> ConstantFolder<'a> {
    fn expr(&mut self, expression: Expression, type_hint: Option<&TreePath>) -> Result<Constant, DMError> {
        Ok(match expression {
            Expression::Base { unary, term, follow } => {
                let base_type_hint = if follow.is_empty() && unary.is_empty() {
                    type_hint
                } else {
                    None
                };
                let mut term = self.term(term.elem, base_type_hint)?;
                for each in follow {
                    term = self.follow(term, each.elem)?;
                }
                for each in unary.into_iter().rev() {
                    term = self.unary(term, each)?;
                }
                term
            },
            Expression::BinaryOp { op, lhs, rhs } => {
                let lhs = self.expr(*lhs, None)?;
                let rhs = self.expr(*rhs, None)?;
                self.binary(lhs, rhs, op)?
            },
            Expression::TernaryOp { cond, if_, else_ } => {
                match self.expr(*cond, None)?.to_bool() {
                    true => self.expr(*if_, type_hint)?,
                    false => self.expr(*else_, type_hint)?,
                }
            },
            Expression::AssignOp { .. } => return Err(self.error("non-constant assignment")),
        })
    }

    /// list of expressions, keyword arguments disallowed
    #[allow(dead_code)]
    fn expr_vec(&mut self, v: Vec<Expression>) -> Result<Vec<Constant>, DMError> {
        let mut out = Vec::new();
        for each in v {
            out.push(self.expr(each, None)?);
        }
        Ok(out)
    }

    /// arguments or keyword arguments
    fn arguments(&mut self, v: Vec<Expression>) -> Result<Vec<(Constant, Option<Constant>)>, DMError> {
        let mut out = Vec::new();
        for each in v {
            out.push(match each {
                // handle associations
                Expression::AssignOp {
                    op: AssignOp::Assign,
                    lhs,
                    rhs,
                } => {
                    let key = match Term::from(*lhs) {
                        Term::Ident(ident) => Constant::String(ident),
                        other => self.term(other, None)?,
                    };
                    (key, Some(self.expr(*rhs, None)?))
                },
                key => (self.expr(key, None)?, None),
            });
        }
        Ok(out)
    }

    fn follow(&mut self, term: Constant, follow: Follow) -> Result<Constant, DMError> {
        match (term, follow) {
            // Meant to handle the GLOB.SCI_FREQ case:
            //     /datum/globals/var/const/SCI_FREQ = 1351
            //     /var/datum/globals/GLOB = null
            //     /obj/var/freq = GLOB.SCI_FREQ   // initial() is 1351
            // If it's a reference to a type-hinted value, look up the field in
            // its const variables (but not non-const variables).
            (Constant::Null(Some(type_hint)), Follow::Field(_, field_name)) => {
                let mut full_path = String::new();
                for each in type_hint {
                    full_path.push('/');
                    full_path.push_str(&each);
                }
                match self.tree.as_mut().and_then(|t| t.find(&full_path)).map(|t| t.index()) {
                    Some(idx) => self.recursive_lookup(idx, &field_name, true),
                    None => Err(self.error(format!("unknown typepath {}", full_path))),
                }
            }
            (term, follow) => Err(self.error(format!("non-constant expression follower: {} {:?}", term, follow))),
        }
    }

    fn unary(&mut self, term: Constant, op: UnaryOp) -> Result<Constant, DMError> {
        use self::Constant::*;

        Ok(match (op, term) {
            // int ops
            (UnaryOp::Neg, Int(i)) => Int(-i),
            (UnaryOp::BitNot, Int(i)) => Int(!i),
            (UnaryOp::Not, Int(i)) => Int(if i != 0 { 0 } else { 1 }),
            // float ops
            (UnaryOp::Neg, Float(i)) => Float(-i),
            // unsupported
            (op, term) => return Err(self.error(format!("non-constant unary operation: {}", op.around(&term)))),
        })
    }

    fn binary(&mut self, mut lhs: Constant, mut rhs: Constant, op: BinaryOp) -> Result<Constant, DMError> {
        use self::Constant::*;

        macro_rules! numeric {
            ($name:ident $oper:tt) => {
                match (op, lhs, rhs) {
                    (BinaryOp::$name, Int(lhs), Int(rhs)) => return Ok(Constant::from(lhs $oper rhs)),
                    (BinaryOp::$name, Int(lhs), Float(rhs)) => return Ok(Constant::from((lhs as f32) $oper rhs)),
                    (BinaryOp::$name, Float(lhs), Int(rhs)) => return Ok(Constant::from(lhs $oper (rhs as f32))),
                    (BinaryOp::$name, Float(lhs), Float(rhs)) => return Ok(Constant::from(lhs $oper rhs)),
                    (_, lhs_, rhs_) => { lhs = lhs_; rhs = rhs_; }
                }
            }
        }
        numeric!(Add +);
        numeric!(Sub -);
        numeric!(Mul *);
        numeric!(Div /);
        numeric!(Mod %);
        numeric!(Less <);
        numeric!(LessEq <=);
        numeric!(Greater >);
        numeric!(GreaterEq >=);
        match (op, lhs, rhs) {
            (BinaryOp::Pow, Int(lhs), Int(rhs)) => {
                use std::convert::TryFrom;
                if let Ok(rhs2) = u32::try_from(rhs) {
                    if let Some(result) = lhs.checked_pow(rhs2) {
                        return Ok(Constant::from(result));
                    }
                }
                return Ok(Constant::from((lhs as f32).powf(rhs as f32)));
            }
            (BinaryOp::Pow, Int(lhs), Float(rhs)) => return Ok(Constant::from((lhs as f32).powf(rhs))),
            (BinaryOp::Pow, Float(lhs), Int(rhs)) => return Ok(Constant::from(lhs.powi(rhs))),
            (BinaryOp::Pow, Float(lhs), Float(rhs)) => return Ok(Constant::from(lhs.powf(rhs))),
            (_, lhs_, rhs_) => {
                lhs = lhs_;
                rhs = rhs_;
            }
        }

        macro_rules! integer {
            ($name:ident $oper:tt) => {
                match (op, lhs, rhs) {
                    (BinaryOp::$name, Int(lhs), Int(rhs)) => return Ok(Int(lhs $oper rhs)),
                    (_, lhs_, rhs_) => { lhs = lhs_; rhs = rhs_; }
                }
            }
        }
        integer!(BitOr |);
        integer!(BitAnd &);
        integer!(BitXor ^);
        integer!(LShift <<);
        integer!(RShift >>);

        match (op, lhs, rhs) {
            (BinaryOp::Add, String(lhs), String(rhs)) => Ok(String(lhs + &rhs)),
            (BinaryOp::Eq, lhs, rhs) => Ok(Constant::from(lhs == rhs)),
            (BinaryOp::NotEq, lhs, rhs) => Ok(Constant::from(lhs != rhs)),
            (BinaryOp::And, lhs, rhs) => Ok(if lhs.to_bool() { rhs } else { lhs }),
            (BinaryOp::Or, lhs, rhs) => Ok(if lhs.to_bool() { lhs } else { rhs }),
            (op, lhs, rhs) => Err(self.error(format!("non-constant {:?}: {} {} {}", op, lhs, op, rhs))),
        }
    }

    fn term(&mut self, term: Term, type_hint: Option<&TreePath>) -> Result<Constant, DMError> {
        Ok(match term {
            Term::Null => Constant::Null(type_hint.cloned()),
            Term::New { type_, args } => Constant::New {
                type_: match type_ {
                    NewType::Prefab(e) => Some(self.prefab(*e)?),
                    NewType::Implicit => None,
                    NewType::MiniExpr { .. } => return Err(self.error("non-constant new expression")),
                },
                args: match args {
                    Some(args) => Some(self.arguments(args)?),
                    None => None,
                },
            },
            Term::List(vec) => Constant::List(self.arguments(vec)?),
            Term::Call(ident, args) => match &*ident {
                // constructors which remain as they are
                "matrix" => Constant::Call(ConstFn::Matrix, self.arguments(args)?),
                "newlist" => Constant::Call(ConstFn::Newlist, self.arguments(args)?),
                "icon" => Constant::Call(ConstFn::Icon, self.arguments(args)?),
                "sound" => Constant::Call(ConstFn::Sound, self.arguments(args)?),
                "file" => Constant::Call(ConstFn::File, self.arguments(args)?),
                // constant-evaluatable functions
                "sin" => self.trig_op(args, f32::sin)?,
                "cos" => self.trig_op(args, f32::cos)?,
                "arcsin" => self.trig_op(args, f32::asin)?,
                "arccos" => self.trig_op(args, f32::acos)?,
                "rgb" => {
                    use std::fmt::Write;
                    if args.len() != 3 && args.len() != 4 {
                        return Err(self.error(format!("malformed rgb() call, must have 3 or 4 arguments and instead has {}", args.len())));
                    }
                    let mut result = String::with_capacity(7);
                    result.push('#');
                    for each in args {
                        if let Some(i) = self.expr(each, None)?.to_int() {
                            let clamped = std::cmp::max(::std::cmp::min(i, 255), 0);
                            let _ = write!(result, "{:02x}", clamped);
                        } else {
                            return Err(self.error("malformed rgb() call, argument wasn't an int"));
                        }
                    }
                    Constant::String(result)
                },
                "defined" if self.defines.is_some() => {
                    let defines = self.defines.unwrap();  // annoying, but keeps the match clean
                    if args.len() != 1 {
                        return Err(self.error(format!("malformed defined() call, must have 1 argument and instead has {}", args.len())));
                    }
                    match args[0].as_term() {
                        Some(Term::Ident(ref ident)) => {
                            Constant::Int(if defines.contains_key(ident) { 1 } else { 0 })
                        },
                        _ => return Err(self.error("malformed defined() call, argument given isn't an Ident.")),
                    }
                }
                // other functions are no-goes
                _ => return Err(self.error(format!("non-constant function call: {}", ident))),
            },
            Term::Prefab(prefab) => Constant::Prefab(self.prefab(*prefab)?),
            Term::Ident(ident) => self.ident(ident, false)?,
            Term::String(v) => Constant::String(v),
            Term::Resource(v) => Constant::Resource(v),
            Term::Int(v) => Constant::Int(v),
            Term::Float(v) => Constant::from(v),
            Term::Expr(expr) => self.expr(*expr, type_hint)?,
            _ => return Err(self.error("non-constant expression".to_owned())),
        })
    }

    fn trig_op(&mut self, mut args: Vec<Expression>, op: fn(f32) -> f32) -> Result<Constant, DMError> {
        if args.len() != 1 {
            Err(self.error(format!("trig function requires exactly 1 argument, instead found {}", args.len())))
        } else if let Some(f) = self.expr(args.remove(0), None)?.to_float() {
            Ok(Constant::Float(op(f)))
        } else {
            Err(self.error("trig function requires numeric argument"))
        }
    }

    fn prefab(&mut self, prefab: Prefab) -> Result<Pop, DMError> {
        let vars = self.vars(prefab.vars)?;

        // If the path is all slashes, it's absolute, and doesn't need to be
        // further resolved.
        if prefab.path.iter().all(|&(op, _)| op == PathOp::Slash) {
            let path: TreePath = prefab.path.iter().map(|&(_, ref name)| name.to_owned()).collect();
            return Ok(Pop { path, vars })
        }

        // Otherwise, resolve it against our object tree, then stringify it.
        let tree = match self.tree.as_ref() {
            Some(tree) => tree,
            None => return Err(self.error(format!(
                "cannot resolve relative type path without an object tree: {}",
                FormatTypePath(&prefab.path)))),
        };

        let relative_to = TypeRef::new(tree, self.ty);
        let found = match relative_to.navigate_path(&prefab.path) {
            Some(found) => found,
            None => return Err(self.error(format!("could not resolve {} relative to {}",
                FormatTypePath(&prefab.path), relative_to))),
        };

        let path = found.to_path();
        Ok(Pop { path, vars })
    }

    fn vars(&mut self, input: LinkedHashMap<Ident, Expression>) -> Result<LinkedHashMap<Ident, Constant>, DMError> {
        // Visit the vars recursively.
        let mut vars = LinkedHashMap::new();
        for (k, v) in input {
            // TODO: find a type annotation by looking up 'k' on the prefab's type
            vars.insert(k, self.expr(v, None)?);
        }
        Ok(vars)
    }

    fn ident(&mut self, ident: Ident, must_be_const: bool) -> Result<Constant, DMError> {
        let ty = self.ty;
        self.recursive_lookup(ty, &ident, must_be_const)
    }

    fn recursive_lookup(&mut self, ty: NodeIndex, ident: &str, must_be_const: bool) -> Result<Constant, DMError> {
        let mut idx = Some(ty);
        while let Some(ty) = idx {
            let location = self.location;
            if self.tree.is_none() {
                return Err(self.error("cannot reference variables in this context"));
            }
            let tree = self.tree.as_mut().unwrap();
            match constant_ident_lookup(tree, ty, &ident, must_be_const)
                .map_err(|e| e.with_location(location))?
            {
                ConstLookup::Found(_, v) => return Ok(v),
                ConstLookup::Continue(i) => idx = i,
            }
        }
        Err(self.error(format!("unknown variable: {}", ident)))
    }
}
