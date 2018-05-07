//! The constant folder/evaluator, used by the preprocessor and object tree.
use std::fmt;
use std::path::Path;

use linked_hash_map::LinkedHashMap;

use super::{DMError, Location, HasLocation, Context};
use super::objtree::*;
use super::ast::*;

/// A DM constant, usually a literal or simple combination of other constants.
///
/// This is intended to represent the degree to which constants are evaluated
/// before being displayed in DreamMaker.
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    /// The literal `null`.
    Null(Option<TypePath>),
    /// A `new` call.
    New {
        /// The type to be instantiated.
        type_: NewType<Constant>,
        /// The list of arugments to pass to the `New()` proc.
        args: Option<Vec<(Constant, Option<Constant>)>>,
    },
    /// A `list` literal. Elements have optional associations.
    List(Vec<(Constant, Option<Constant>)>),
    /// A call to a constant type constructor.
    Call(ConstFn, Vec<Constant>),
    /// A prefab literal.
    Prefab(Prefab<Constant>),
    /// A string literal.
    String(String),
    /// A resource literal.
    Resource(String),
    /// An integer literal.
    Int(i32),
    /// A floating-point literal.
    Float(f32),
}

/// The constant functions which are represented as-is.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ConstFn {
    /// The `icon()` type constructor.
    Icon,
    /// The `matrix()` type constructor.
    Matrix,
    /// The `newlist()` function, which combines `new` mapped over a `list`.
    Newlist,
    /// The `sound()` type constructor.
    Sound,
}

impl Constant {
    #[inline]
    pub fn string<S: Into<String>>(s: S) -> Constant {
        Constant::String(s.into())
    }

    pub fn contains_key(&self, key: &Constant) -> bool {
        match self {
            &Constant::List(ref elements) => for &(ref k, _) in elements {
                if key == k {
                    return true;
                }
            }
            _ => {}
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
            }
            _ => {}
        }
        None
    }

    pub fn to_bool(&self) -> bool {
        match self {
            &Constant::Null(_) => false,
            &Constant::Int(i) => i != 0,
            &Constant::Float(f) => f != 0.,
            &Constant::String(ref s) => !s.is_empty(),
            _ => true,
        }
    }

    pub fn to_float(&self) -> Option<f32> {
        match self {
            &Constant::Int(i) => Some(i as f32),
            &Constant::Float(f) => Some(f),
            _ => None,
        }
    }

    pub fn to_int(&self) -> Option<i32> {
        match self {
            &Constant::Int(i) => Some(i),
            &Constant::Float(f) => Some(f as i32),
            _ => None,
        }
    }

    pub fn eq_string(&self, string: &str) -> bool {
        match self {
            &Constant::String(ref s) => s == string,
            _ => false,
        }
    }

    pub fn eq_resource(&self, resource: &str) -> bool {
        match self {
            &Constant::String(ref s) |
            &Constant::Resource(ref s) => s == resource,
            _ => false,
        }
    }

    pub fn as_path(&self) -> Option<&Path> {
        match self {
            &Constant::String(ref s) |
            &Constant::Resource(ref s) => Some(s.as_ref()),
            _ => None,
        }
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

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Constant::Null(_) => f.write_str("null"),
            Constant::New { ref type_, ref args } => {
                write!(f, "new{}", type_)?;
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
                for &(ref key, ref val) in list.iter() {
                    if !first {
                        write!(f, ",")?;
                    }
                    first = false;
                    write!(f, "{}", key)?;
                    if let Some(val) = val.as_ref() {
                        write!(f, " = {}", val)?;
                    }
                }
                write!(f, ")")
            },
            Constant::Call(const_fn, ref list) => {
                write!(f, "{}(", const_fn)?;
                let mut first = true;
                for each in list.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{}", each)?;
                }
                write!(f, ")")
            },
            Constant::Prefab(ref val) => write!(f, "{}", val),
            Constant::String(ref val) => write!(f, "\"{}\"", val),
            Constant::Resource(ref val) => write!(f, "'{}'", val),
            Constant::Int(val) => write!(f, "{}", val),
            Constant::Float(val) => write!(f, "{}", val),
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
        })
    }
}

// ----------------------------------------------------------------------------
// The constant evaluator

/// Evaluate all the type-level variables in an object tree into constants.
pub(crate) fn evaluate_all(context: &Context, tree: &mut ObjectTree) {
    for ty in tree.graph.node_indices() {
        let keys: Vec<String> = tree.graph.node_weight(ty).unwrap().vars.keys().cloned().collect();
        for key in keys {
            if !tree.graph.node_weight(ty).unwrap().get_declaration(&key, tree).map_or(true, |x| x.is_const_evaluable() && (x.is_const || ty != NodeIndex::new(0))) {
                continue  // skip non-constant-evaluable vars
            }
            match constant_ident_lookup(tree, ty, &key, false) {
                Err(err) => context.register_error(err),
                Ok(ConstLookup::Found(_, _)) => {}
                Ok(ConstLookup::Continue(_)) => {
                    context.register_error(DMError::new(tree.graph.node_weight(ty).unwrap().vars[&key].value.location,
                        format!("undefined var '{}' on type '{}'", key, tree.graph.node_weight(ty).unwrap().path)));
                }
            }
        }
    }
}

/// Evaluate an expression in the absence of any surrounding context.
pub fn simple_evaluate(location: Location, expr: Expression) -> Result<Constant, DMError> {
    ConstantFolder { tree: None, location, ty: NodeIndex::new(0) }.expr(expr, None)
}

enum ConstLookup {
    Found(TypePath, Constant),
    Continue(Option<NodeIndex>),
}

fn constant_ident_lookup(tree: &mut ObjectTree, ty: NodeIndex, ident: &str, must_be_static: bool) -> Result<ConstLookup, DMError> {
    // try to read the currently-set value if we can and
    // substitute that in, otherwise try to evaluate it.
    let (location, type_hint, expr) = {
        let decl = match tree.graph.node_weight(ty).unwrap().get_declaration(ident, tree).cloned() {
            Some(decl) => decl,
            None => return Ok(ConstLookup::Continue(None))  // definitely doesn't exist
        };

        let type_ = tree.graph.node_weight_mut(ty).unwrap();
        let parent = type_.parent_type();
        match type_.vars.get_mut(ident) {
            None => { return Ok(ConstLookup::Continue(parent)); }
            Some(var) => match var.value.constant.clone() {
                Some(constant) => { return Ok(ConstLookup::Found(decl.type_path.clone(), constant)); }
                None => match var.value.expression.clone() {
                    Some(expr) => {
                        if var.value.being_evaluated {
                            return Err(DMError::new(var.value.location, format!("recursive constant reference: {}", ident)));
                        } else if !decl.is_const_evaluable() {
                            return Err(DMError::new(var.value.location, format!("non-const variable: {}", ident)));
                        } else if !decl.is_static && must_be_static {
                            return Err(DMError::new(var.value.location, format!("non-static variable: {}", ident)));
                        }
                        var.value.being_evaluated = true;
                        (var.value.location, decl.type_path, expr)
                    }
                    None => {
                        let c = Constant::Null(Some(decl.type_path.clone()));
                        var.value.constant = Some(c.clone());
                        return Ok(ConstLookup::Found(decl.type_path, c));
                    }
                }
            }
        }
    };
    // evaluate full_value
    let value = ConstantFolder { tree: Some(tree), location, ty }
        .expr(expr, if type_hint.is_empty() { None } else { Some(&type_hint) })?;
    // and store it into 'value', then return it
    let var = tree.graph.node_weight_mut(ty).unwrap().vars.get_mut(ident).unwrap();
    var.value.constant = Some(value.clone());
    var.value.being_evaluated = false;
    Ok(ConstLookup::Found(type_hint, value))
}

struct ConstantFolder<'a> {
    tree: Option<&'a mut ObjectTree>,
    location: Location,
    ty: NodeIndex,
}

impl<'a> HasLocation for ConstantFolder<'a> {
    fn location(&self) -> Location {
        self.location
    }
}

impl<'a> ConstantFolder<'a> {
    fn expr(&mut self, expression: Expression, type_hint: Option<&TypePath>) -> Result<Constant, DMError> {
        Ok(match expression {
            Expression::Base { unary, term, follow } => {
                let base_type_hint = if follow.is_empty() && unary.is_empty() { type_hint } else { None };
                let mut term = self.term(term, base_type_hint)?;
                for each in follow {
                    term = self.follow(term, each)?;
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
                Expression::AssignOp { op: AssignOp::Assign, lhs, rhs } => {
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
            // Meant to handle the GLOB.SCI_FREQ case.
            // If it's a reference to a type-hinted value, look up the field in
            // its static variables (but not non-static variables).
            (Constant::Null(Some(type_hint)), Follow::Field(field_name)) => {
                let mut full_path = String::new();
                for each in type_hint {
                    full_path.push('/');  // TODO: use path ops here?
                    full_path.push_str(&each.1);
                }
                match self.tree.as_mut().and_then(|t| t.types.get(&full_path)) {
                    Some(&idx) => self.recursive_lookup(idx, &field_name, true),
                    None => Err(self.error(format!("unknown typepath {}", full_path))),
                }
            }
            (Constant::String(string), Follow::Cast(cast)) => {
                if cast != "text" {
                    return Err(self.error(format!("cannot cast string to {:?}", cast)));
                }
                Ok(Constant::String(string))
            }
            (term, follow) => Err(self.error(format!("non-constant expression follower: {} {:?}", term, follow)))
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
        numeric!(Less <);
        numeric!(LessEq <=);
        numeric!(Greater >);
        numeric!(GreaterEq >=);

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
        integer!(LShift <<);
        integer!(RShift >>);

        match (op, lhs, rhs) {
            (BinaryOp::Add, String(lhs), String(rhs)) => Ok(String(lhs + &rhs)),
            (BinaryOp::Eq, lhs, rhs) => Ok(Constant::from(lhs == rhs)),
            (BinaryOp::NotEq, lhs, rhs) => Ok(Constant::from(lhs != rhs)),
            (op, lhs, rhs) => Err(self.error(format!("non-constant {:?}: {} {} {}", op, lhs, op, rhs)))
        }
    }

    fn term(&mut self, term: Term, type_hint: Option<&TypePath>) -> Result<Constant, DMError> {
        Ok(match term {
            Term::Null => Constant::Null(type_hint.cloned()),
            Term::New { type_, args } => {
                Constant::New {
                    type_: match type_ {
                        NewType::Prefab(e) => NewType::Prefab(self.prefab(e)?),
                        NewType::Implicit => NewType::Implicit,
                        NewType::Ident(_) => return Err(self.error("non-constant new expression")),
                    },
                    args: match args {
                        Some(args) => Some(self.arguments(args)?),
                        None => None,
                    },
                }
            },
            Term::List(vec) => {
                Constant::List(self.arguments(vec)?)
            },
            Term::Call(ident, args) => match &*ident {
                // constructors which remain as they are
                "matrix" => Constant::Call(ConstFn::Matrix, self.expr_vec(args)?),
                "newlist" => Constant::Call(ConstFn::Newlist, self.expr_vec(args)?),
                "icon" => Constant::Call(ConstFn::Icon, self.expr_vec(args)?),
                "sound" => Constant::Call(ConstFn::Sound, self.expr_vec(args)?),
                // constant-evaluatable functions
                "rgb" => {
                    use std::fmt::Write;
                    if args.len() != 3 && args.len() != 4 {
                        return Err(self.error("malformed rgb() call"));
                    }
                    let mut result = "#".to_owned();
                    for each in args {
                        if let Constant::Int(i) = self.expr(each, None)? {
                            let clamped = ::std::cmp::max(::std::cmp::min(i, 255), 0);
                            let _ = write!(result, "{:02x}", clamped);
                        } else {
                            return Err(self.error("malformed rgb() call"));
                        }
                    }
                    Constant::String(result)
                },
                // other functions are no-goes
                _ => return Err(self.error(format!("non-constant function call: {}", ident)))
            },
            Term::Prefab(prefab) => Constant::Prefab(self.prefab(prefab)?),
            Term::Ident(ident) => self.ident(ident, type_hint, false)?,
            Term::String(v) => Constant::String(v),
            Term::Resource(v) => Constant::Resource(v),
            Term::Int(v) => Constant::Int(v),
            Term::Float(v) => Constant::Float(v),
            Term::Expr(expr) => self.expr(*expr, type_hint)?,
            _ => return Err(self.error(format!("non-constant expression")))
        })
    }

    fn prefab(&mut self, prefab: Prefab) -> Result<Prefab<Constant>, DMError> {
        let mut vars = LinkedHashMap::new();
        for (k, v) in prefab.vars {
            // TODO: find a type annotation by looking up 'k' on the prefab's type
            vars.insert(k, self.expr(v, None)?);
        }
        Ok(Prefab { path: prefab.path, vars })
    }

    fn ident(&mut self, ident: String, type_hint: Option<&TypePath>, must_be_static: bool) -> Result<Constant, DMError> {
        if ident == "null" {
            Ok(Constant::Null(type_hint.cloned()))
        } else {
            let ty = self.ty;
            self.recursive_lookup(ty, &ident, must_be_static)
        }
    }

    fn recursive_lookup(&mut self, ty: NodeIndex, ident: &str, must_be_static: bool) -> Result<Constant, DMError> {
        let mut idx = Some(ty);
        while let Some(ty) = idx {
            let location = self.location;
            if self.tree.is_none() {
                return Err(self.error("cannot reference variables in this context"));
            }
            let tree = self.tree.as_mut().unwrap();
            match constant_ident_lookup(tree, ty, &ident, must_be_static).map_err(|e| DMError::new(location, e.into_description()))? {
                ConstLookup::Found(_, v) => return Ok(v),
                ConstLookup::Continue(i) => idx = i,
            }
        }
        Err(self.error(format!("unknown variable: {}", ident)))
    }
}
