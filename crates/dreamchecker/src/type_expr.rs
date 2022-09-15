//! Support for "type expressions", used in evaluating dynamic/generic return
//! types.

use std::collections::HashMap;

use dm::ast::*;
use dm::constants::Constant;
use dm::objtree::{ObjectTree, ProcRef};
use dm::{DMError, Location};

use ahash::RandomState;

use crate::{Analysis, StaticType};

pub struct TypeExprContext<'o, 't> {
    pub objtree: &'o ObjectTree,
    pub param_name_map: HashMap<&'t str, Analysis<'o>, RandomState>,
    pub param_idx_map: HashMap<usize, Analysis<'o>, RandomState>,
}

impl<'o, 't> TypeExprContext<'o, 't> {
    fn get(&self, name: &str, idx: usize) -> Option<&Analysis<'o>> {
        if let Some(a) = self.param_name_map.get(name) {
            Some(a)
        } else if let Some(a) = self.param_idx_map.get(&idx) {
            Some(a)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr<'o> {
    // Static type literal (`null` => `None` included).
    Static(StaticType<'o>),

    // The value of a parameter, if it is a typepath.
    ParamTypepath {
        name: String,
        p_idx: usize,
        // Number of /list to strip.
        index_ct: usize,
    },

    // The static type of a parameter.
    ParamStaticType {
        name: String,
        p_idx: usize,
        // Number of /list to strip.
        index_ct: usize,
    },

    // from `&&`, `||`, and `?:`
    Condition {
        cond: Box<TypeExpr<'o>>,
        if_: Box<TypeExpr<'o>>,
        else_: Box<TypeExpr<'o>>,
    },
}

impl<'o> TypeExpr<'o> {
    pub fn compile(
        proc: ProcRef<'o>,
        location: Location,
        expression: &Expression,
    ) -> Result<TypeExpr<'o>, DMError> {
        TypeExprCompiler {
            objtree: proc.tree(),
            proc,
        }
        .visit_expression(location, expression)
    }

    pub fn evaluate(
        &self,
        location: Location,
        ec: &TypeExprContext<'o, '_>,
    ) -> Result<StaticType<'o>, DMError> {
        match self {
            TypeExpr::Static(st) => Ok(st.clone()),

            TypeExpr::Condition { cond, if_, else_ } => {
                if cond.evaluate(location, ec)?.is_truthy() {
                    if_.evaluate(location, ec)
                } else {
                    else_.evaluate(location, ec)
                }
            }

            TypeExpr::ParamTypepath {
                name,
                p_idx,
                index_ct: _,
            } => {
                if let Some(analysis) = ec.get(name, *p_idx) {
                    if let Some(Constant::Prefab(ref pop)) = analysis.value {
                        crate::static_type(ec.objtree, location, &pop.path)
                    } else {
                        Ok(StaticType::None)
                    }
                } else {
                    Ok(StaticType::None)
                }
            }

            TypeExpr::ParamStaticType {
                name,
                p_idx,
                index_ct,
            } => {
                if let Some(analysis) = ec.get(name, *p_idx) {
                    Ok(analysis.static_ty.clone().strip_lists(*index_ct))
                } else {
                    Ok(StaticType::None)
                }
            }
        }
    }
}

impl<'o> From<StaticType<'o>> for TypeExpr<'o> {
    fn from(static_type: StaticType<'o>) -> TypeExpr<'o> {
        TypeExpr::Static(static_type)
    }
}

struct TypeExprCompiler<'o> {
    objtree: &'o ObjectTree,
    proc: ProcRef<'o>,
}

impl<'o> TypeExprCompiler<'o> {
    fn visit_expression(
        &mut self,
        location: Location,
        expr: &Expression,
    ) -> Result<TypeExpr<'o>, DMError> {
        match expr {
            Expression::Base { term, follow } => {
                let mut ty = self.visit_term(term.location, &term.elem)?;
                for each in follow.iter() {
                    ty = self.visit_follow(each.location, ty, &each.elem)?;
                }
                Ok(ty)
            }
            Expression::BinaryOp {
                op: BinaryOp::Or,
                lhs,
                rhs,
            } => {
                // `A || B` => `A ? A : B`
                let lty = self.visit_expression(location, lhs)?;
                let rty = self.visit_expression(location, rhs)?;
                Ok(TypeExpr::Condition {
                    cond: Box::new(lty.clone()),
                    if_: Box::new(lty),
                    else_: Box::new(rty),
                })
            }
            Expression::BinaryOp {
                op: BinaryOp::And,
                lhs,
                rhs,
            } => {
                // `A && B` => `A ? B : A`
                let lty = self.visit_expression(location, lhs)?;
                let rty = self.visit_expression(location, rhs)?;
                Ok(TypeExpr::Condition {
                    cond: Box::new(lty.clone()),
                    if_: Box::new(rty),
                    else_: Box::new(lty),
                })
            }
            Expression::TernaryOp { cond, if_, else_ } => Ok(TypeExpr::Condition {
                cond: Box::new(self.visit_expression(location, cond)?),
                if_: Box::new(self.visit_expression(location, if_)?),
                else_: Box::new(self.visit_expression(location, else_)?),
            }),
            _ => Err(DMError::new(location, "type expr: bad expression node")),
        }
    }

    fn visit_term(&mut self, location: Location, term: &Term) -> Result<TypeExpr<'o>, DMError> {
        match term {
            Term::Null => Ok(TypeExpr::from(StaticType::None)),

            Term::Ident(unscoped_name) => {
                for (i, param) in self.proc.parameters.iter().enumerate() {
                    if *unscoped_name == param.name {
                        return Ok(TypeExpr::ParamTypepath {
                            name: unscoped_name.to_owned(),
                            p_idx: i,
                            index_ct: 0,
                        });
                    }
                }
                Err(DMError::new(
                    location,
                    format!("type expr: no such parameter {:?}", unscoped_name),
                ))
            }

            Term::Expr(expr) => self.visit_expression(location, expr),

            Term::Prefab(fab) => {
                let bits: Vec<_> = fab.path.iter().map(|(_, name)| name.to_owned()).collect();
                let ty = crate::static_type(self.objtree, location, &bits)?;
                Ok(TypeExpr::from(ty))
            }

            _ => Err(DMError::new(location, "type expr: bad term node")),
        }
    }

    fn visit_follow(
        &mut self,
        location: Location,
        lhs: TypeExpr<'o>,
        rhs: &Follow,
    ) -> Result<TypeExpr<'o>, DMError> {
        match rhs {
            // X[_] => static type of argument X with one /list stripped
            Follow::Index(_, expr) => match expr.as_term() {
                Some(Term::Ident(name)) if name == "_" => match lhs {
                    TypeExpr::ParamTypepath {
                        name,
                        p_idx,
                        index_ct,
                    } => Ok(TypeExpr::ParamTypepath {
                        name,
                        p_idx,
                        index_ct: index_ct + 1,
                    }),
                    _ => Err(DMError::new(
                        location,
                        "type expr: cannot index non-parameters",
                    )),
                },
                _ => Err(DMError::new(
                    location,
                    "type expr: cannot index by anything but `_`",
                )),
            },

            // X.type => static type of argument X
            Follow::Field(_, name) if name == "type" => match lhs {
                TypeExpr::ParamTypepath {
                    name,
                    p_idx,
                    index_ct,
                } => Ok(TypeExpr::ParamStaticType {
                    name,
                    p_idx,
                    index_ct,
                }),
                _ => Err(DMError::new(
                    location,
                    "type expr: cannot take .type of non-parameters",
                )),
            },

            _ => Err(DMError::new(
                location,
                format!("type expr: bad follow node {:?}", rhs),
            )),
        }
    }
}
