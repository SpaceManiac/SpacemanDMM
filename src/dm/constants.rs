//! The constant folder/evaluator, used by the preprocessor and object tree.

use linked_hash_map::LinkedHashMap;

use super::{DMError, Location, HasLocation};
use super::lexer::{Token, LocatedToken};
use super::objtree::*;
use super::ast::*;

pub fn evaluate_all(tree: &mut ObjectTree) -> Result<(), DMError> {
    for ty in tree.graph.node_indices() {
        let keys: Vec<String> = tree.graph.node_weight(ty).unwrap().vars.keys().cloned().collect();
        println!("type #{} has these variables: {:?}", ty.index(), keys);
        for key in keys {
            if !tree.graph.node_weight(ty).unwrap().vars[&key].is_const_evaluable() {
                continue
            }
            match constant_ident_lookup(tree, ty, &key)? {
                ConstLookup::Found(_) => {}
                ConstLookup::Continue(_) => return Err(DMError::new(Location::default(), "oh no")),
            }
        }
    }
    Ok(())
}

enum ConstLookup {
    Found(Constant),
    Continue(Option<NodeIndex>),
}

fn constant_ident_lookup(tree: &mut ObjectTree, ty: NodeIndex, ident: &str) -> Result<ConstLookup, DMError> {
    // try to read the currently-set value if we can and
    // substitute that in, otherwise try to evaluate it.
    let (location, full_value) = {
        let type_ = tree.graph.node_weight_mut(ty).unwrap();
        let parent = type_.parent_type();
        match type_.vars.get_mut(ident) {
            None => { return Ok(ConstLookup::Continue(parent)); }
            Some(var) => match var.value.clone() {
                Some(value) => { return Ok(ConstLookup::Found(value)); }
                None => match var.full_value.clone() {
                    Some(full_value) => {
                        if var.being_evaluated {
                            return Err(DMError::new(var.location, format!("recursive constant reference: {}", ident)));
                        } else if !var.is_const_evaluable() {
                            return Err(DMError::new(var.location, format!("non-const variable: {}", ident)));
                        }
                        var.being_evaluated = true;
                        (var.location, full_value)
                    }
                    None => {
                        // basically means "null"
                        //return Err(DMError::new(var.location, format!("undefined constant reference: {}", ident)));
                        (var.location, vec![Token::Ident("null".to_owned(), false)])
                    }
                }
            }
        }
    };
    // evaluate full_value
    let value = evaluate(tree, ty, location, full_value)?;
    // and store it into 'value', then return it
    let var = tree.graph.node_weight_mut(ty).unwrap().vars.get_mut(ident).unwrap();
    var.value = Some(value.clone());
    var.being_evaluated = false;
    Ok(ConstLookup::Found(value))
}

pub fn evaluate(tree: &mut ObjectTree, ty: NodeIndex, location: Location, value: Vec<Token>) -> Result<Constant, DMError> {
    println!("{:?}", value);
    {let so = ::std::io::stdout();
    super::pretty_print(&mut so.lock(), value.iter().cloned().map(Ok), false)?;}
    println!();

    let mut parser = super::parser::Parser::new(value.into_iter().map(|i| Ok(LocatedToken::new(location, i))));
    let v = match parser.expression(false)? {
        Some(v) => v,
        None => return Err(DMError::new(location, "not an expression")),
    };
    println!("{:?}", v);
    let v = ConstantFolder { tree, location, ty }.expr(v);
    println!("{:?}", v);
    println!();

    v
}

struct ConstantFolder<'a> {
    tree: &'a mut ObjectTree,
    location: Location,
    ty: NodeIndex,
}

impl<'a> HasLocation for ConstantFolder<'a> {
    fn location(&self) -> Location {
        self.location
    }
}

impl<'a> ConstantFolder<'a> {
    fn expr(&mut self, expression: Expression) -> Result<Constant, DMError> {
        Ok(match expression {
            Expression::Base { unary, term, follow } => {
                let mut term = self.term(term)?;
                for each in follow {
                    term = self.follow(term, each)?;
                }
                for each in unary.into_iter().rev() {
                    term = self.unary(term, each)?;
                }
                term
            },
            Expression::BinaryOp { op, lhs, rhs } => {
                let lhs = self.expr(*lhs)?;
                let rhs = self.expr(*rhs)?;
                self.binary(lhs, rhs, op)?
            },
            _ => return Err(self.error("non-constant augmented assignment")),
        })
    }

    fn expr_vec(&mut self, v: Vec<Expression>) -> Result<Vec<Constant>, DMError> {
        let mut out = Vec::new();
        for each in v {
            out.push(self.expr(each)?);
        }
        Ok(out)
    }

    fn follow(&mut self, _: Constant, follow: Follow) -> Result<Constant, DMError> {
        Err(self.error(format!("non-constant expression followers: {:?}", follow)))
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
            (op, term) => return Err(self.error(format!("non-constant unary operation: {:?} {:?}", op, term))),
        })
    }

    fn binary(&mut self, mut lhs: Constant, mut rhs: Constant, op: BinaryOp) -> Result<Constant, DMError> {
        use self::Constant::*;

        macro_rules! numeric {
            ($name:ident $oper:tt) => {
                match (op, lhs, rhs) {
                    (BinaryOp::$name, Int(lhs), Int(rhs)) => return Ok(Int(lhs $oper rhs)),
                    (BinaryOp::$name, Int(lhs), Float(rhs)) => return Ok(Float(lhs as f32 $oper rhs)),
                    (BinaryOp::$name, Float(lhs), Int(rhs)) => return Ok(Float(lhs $oper rhs as f32)),
                    (BinaryOp::$name, Float(lhs), Float(rhs)) => return Ok(Float(lhs $oper rhs)),
                    (_, lhs_, rhs_) => { lhs = lhs_; rhs = rhs_; }
                }
            }
        }
        numeric!(Add +);
        numeric!(Sub -);
        numeric!(Mul *);
        numeric!(Div /);

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
            (op, lhs, rhs) => Err(self.error(format!("non-constant binary operation: {:?} {:?} {:?}", lhs, op, rhs)))
        }
    }

    fn term(&mut self, term: Term) -> Result<Constant, DMError> {
        Ok(match term {
            Term::Null => Constant::Null,
            Term::New { type_, args } => {
                Constant::New {
                    type_: match type_ {
                        NewType::Implicit => NewType::Implicit,
                        NewType::Ident(_) => return Err(self.error("non-constant new expression")),
                        NewType::Prefab(e) => NewType::Prefab(self.prefab(e)?),
                    },
                    args: self.expr_vec(args)?,
                }
            },
            Term::List(vec) => {
                let mut out = Vec::new();
                for each in vec {
                    out.push(match each {
                        (key, Some(val)) => {
                            let key = match Term::from(key) {
                                Term::Ident(ref ident) => {
                                    println!("WARNING: ident used as list key: {}", ident);
                                    Constant::String(ident.clone())
                                },
                                other => self.term(other)?,
                            };
                            (key, Some(self.expr(val)?))
                        },
                        (key, None) => (self.expr(key)?, None),
                    });
                }
                Constant::List(out)
            },
            Term::Call(ident, args) => match &*ident {
                // constructors which remain as they are
                "matrix" => Constant::Call(ConstFn::Matrix, self.expr_vec(args)?),
                "newlist" => Constant::Call(ConstFn::Newlist, self.expr_vec(args)?),
                "icon" => Constant::Call(ConstFn::Icon, self.expr_vec(args)?),
                // constant-evaluatable functions
                "rgb" => {
                    use std::fmt::Write;
                    if args.len() != 3 && args.len() != 4 {
                        return Err(self.error("malformed rgb() call"));
                    }
                    let mut result = "#".to_owned();
                    for each in args {
                        if let Constant::Int(i) = self.expr(each)? {
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
            Term::Ident(ident) => self.ident(ident)?,
            Term::String(v) => Constant::String(v),
            Term::Resource(v) => Constant::Resource(v),
            Term::Int(v) => Constant::Int(v),
            Term::Float(v) => Constant::Float(v),
            Term::Expr(expr) => self.expr(*expr)?,
        })
    }

    fn prefab(&mut self, prefab: Prefab) -> Result<Prefab<Constant>, DMError> {
        let mut vars = LinkedHashMap::new();
        for (k, v) in prefab.vars {
            vars.insert(k, self.expr(v)?);
        }
        Ok(Prefab { path: prefab.path, vars })
    }

    fn ident(&mut self, ident: String) -> Result<Constant, DMError> {
        if ident == "null" {
            Ok(Constant::Null)
        } else {
            let mut idx = Some(self.ty);
            while let Some(ty) = idx {
                println!("searching type #{}", ty.index());
                match constant_ident_lookup(self.tree, ty, &ident).map_err(|e| DMError::new(self.location, e.desc))? {
                    ConstLookup::Found(v) => return Ok(v),
                    ConstLookup::Continue(i) => idx = i,
                }
            }
            Err(self.error(format!("unknown variable: {}", ident)))
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ConstFn {
    Icon,
    Matrix,
    Newlist,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    /// The literal `null`.
    Null,
    New {
        type_: NewType<Constant>,
        args: Vec<Constant>,
    },
    List(Vec<(Constant, Option<Constant>)>),
    Call(ConstFn, Vec<Constant>),
    Prefab(Prefab<Constant>),
    String(String),
    Resource(String),
    Int(i32),
    Float(f32),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedConstant {
    pub type_: String,
    pub constant: Constant,
}
