//! The constant folder/evaluator, used by the preprocessor and object tree.

use super::{DMError, Location};
use super::lexer::{Token, LocatedToken};
use super::ast::*;

pub fn evaluate(location: Location, value: &[Token]) -> Result<(), DMError> {
    println!("{:?}", value);
    {let so = ::std::io::stdout();
    super::pretty_print(&mut so.lock(), value.iter().cloned().map(Ok), false)?;}
    println!();

    let mut iter = value.iter();
    let mut parser = super::parser::Parser::new(iter.by_ref().map(|i| Ok(LocatedToken::new(location, i.clone()))));
    let v = match parser.expression(false)? {
        Some(v) => v,
        None => return Err(DMError::new(location, "not an expression")),
    };
    println!("{:?}", v);
    println!("{:?}", fold_expr(v).unwrap());
    println!();

    Ok(())
}

fn fold_expr(expression: Expression) -> Result<Term, NonConstant> {
    Ok(match expression {
        Expression::Base { unary, term, follow } => {
            let mut term = fold_term(term)?;
            for each in follow {
                term = fold_follow(term, each)?;
            }
            for each in unary.into_iter().rev() {
                term = fold_unary(term, each)?;
            }
            term
        },
        Expression::BinaryOp { op, lhs, rhs } => {
            let lhs = fold_expr(*lhs)?;
            let rhs = fold_expr(*rhs)?;
            fold_binary(lhs, rhs, op)?
        },
        _ => return Err(NonConstant::AssignOp),
    })
}

fn fold_expr_vec(v: Vec<Expression>) -> Result<Vec<Expression>, NonConstant> {
    let mut out = Vec::new();
    for each in v {
        out.push(Expression::from(fold_expr(each)?));
    }
    Ok(out)
}

fn fold_follow(_: Term, follow: Follow) -> Result<Term, NonConstant> {
    Err(NonConstant::Follow(follow))
}

fn fold_unary(term: Term, op: UnaryOp) -> Result<Term, NonConstant> {
    Ok(match (op, term) {
        // int ops
        (UnaryOp::Neg, Term::Int(i)) => Term::Int(-i),
        (UnaryOp::BitNot, Term::Int(i)) => Term::Int(!i),
        (UnaryOp::Not, Term::Int(i)) => Term::Int(if i != 0 { 0 } else { 1 }),
        // float ops
        (UnaryOp::Neg, Term::Float(i)) => Term::Float(-i),
        // unsupported
        (op, _) => return Err(NonConstant::UnaryOp(op)),
    })
}

fn fold_binary(mut lhs: Term, mut rhs: Term, op: BinaryOp) -> Result<Term, NonConstant> {
    macro_rules! numeric {
        ($name:ident $oper:tt) => {
            match (op, lhs, rhs) {
                (BinaryOp::$name, Term::Int(lhs), Term::Int(rhs)) => return Ok(Term::Int(lhs $oper rhs)),
                (BinaryOp::$name, Term::Int(lhs), Term::Float(rhs)) => return Ok(Term::Float(lhs as f32 $oper rhs)),
                (BinaryOp::$name, Term::Float(lhs), Term::Int(rhs)) => return Ok(Term::Float(lhs $oper rhs as f32)),
                (BinaryOp::$name, Term::Float(lhs), Term::Float(rhs)) => return Ok(Term::Float(lhs $oper rhs)),
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
                (BinaryOp::$name, Term::Int(lhs), Term::Int(rhs)) => return Ok(Term::Int(lhs $oper rhs)),
                (_, lhs_, rhs_) => { lhs = lhs_; rhs = rhs_; }
            }
        }
    }
    integer!(BitOr |);
    integer!(BitAnd &);
    integer!(LShift <<);
    integer!(RShift >>);

    match (op, lhs, rhs) {
        (BinaryOp::Add, Term::String(lhs), Term::String(rhs)) => Ok(Term::String(lhs + &rhs)),
        _ => Err(NonConstant::BinaryOp(op))
    }
}

fn fold_term(term: Term) -> Result<Term, NonConstant> {
    Ok(match term {
        Term::Null => Term::Null,
        Term::New { type_, args } => {
            Term::New { type_, args: fold_expr_vec(args)? }
        },
        Term::List(vec) => {
            let mut out = Vec::new();
            for each in vec {
                out.push(match each {
                    (key, Some(val)) => {
                        let key = match key {
                            Expression::Base { term: Term::Ident(ref ident), ref unary, ref follow }
                            if unary.is_empty() && follow.is_empty() => {
                                println!("WARNING: ident used as list key: {}", ident);
                                Term::String(ident.clone()).into()
                            },
                            other => fold_expr(other)?.into(),
                        };
                        (key, Some(fold_expr(val)?.into()))
                    },
                    (key, None) => (fold_expr(key)?.into(), None),
                });
            }
            Term::List(out)
        },
        Term::Call(ident, args) => match &*ident {
            // constructors which remain as they are
            "matrix" | "newlist" | "icon" => Term::Call(ident, fold_expr_vec(args)?),
            // constant-evaluatable functions
            "rgb" => {
                use std::fmt::Write;
                if args.len() != 3 && args.len() != 4 {
                    return Err(NonConstant::BadFunctionCall(ident));
                }
                let mut result = "#".to_owned();
                for each in args {
                    if let Term::Int(i) = fold_expr(each)? {
                        let clamped = ::std::cmp::max(::std::cmp::min(i, 255), 0);
                        let _ = write!(result, "{:02x}", clamped);
                    } else {
                        return Err(NonConstant::BadFunctionCall(ident));
                    }
                }
                Term::String(result)
            },
            // other functions are no-goes
            _ => return Err(NonConstant::Function(ident))
        },
        Term::Expr(expr) => fold_expr(*expr)?,
        Term::Ident(ref ident) if ident == "null" => Term::Null,
        Term::Ident(ident) => return Err(NonConstant::Ident(ident)),
        other => other,
    })
}

#[derive(Debug)]
enum NonConstant {
    AssignOp,
    Function(String),
    BadFunctionCall(String),
    Follow(Follow),
    Ident(String),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
}
