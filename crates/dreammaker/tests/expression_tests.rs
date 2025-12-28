extern crate dreammaker as dm;

use dm::ast::*;
use dm::lexer::Lexer;
use dm::parser::*;

fn parse_expr(f: &str) -> Expression {
    let context = Default::default();
    let lexer = Lexer::new(&context, Default::default(), f.as_bytes());
    let result =
        parse_expression(&context, Default::default(), lexer).expect("failed to parse expression");
    context.assert_success();
    result
}

#[test]
fn ternary_precedence() {
    // (foo = ((1 + 2) ? (3 + 4) : (5 + 6))
    assert_eq!(
        parse_expr("foo = 1 + 2 ? 3 + 4 : 5 + 6"),
        Expression::AssignOp {
            op: AssignOp::Assign,
            lhs: Box::new(Expression::from(Term::Ident("foo".into()))),
            rhs: Box::new(Expression::TernaryOp {
                cond: Box::new(Expression::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Expression::from(Term::Int(1))),
                    rhs: Box::new(Expression::from(Term::Int(2))),
                }),
                if_: Box::new(Expression::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Expression::from(Term::Int(3))),
                    rhs: Box::new(Expression::from(Term::Int(4))),
                }),
                else_: Box::new(Expression::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Expression::from(Term::Int(5))),
                    rhs: Box::new(Expression::from(Term::Int(6))),
                }),
            }),
        }
    )
}

#[test]
fn in_after_ternary() {
    // ((foo = (1 ? 2 : 3)) in 4)
    assert_eq!(
        parse_expr("foo = 1 ? 2 : 3 in 4"),
        Expression::BinaryOp {
            op: BinaryOp::In,
            lhs: Box::new(Expression::AssignOp {
                op: AssignOp::Assign,
                lhs: Box::new(Expression::from(Term::Ident("foo".into()))),
                rhs: Box::new(Expression::TernaryOp {
                    cond: Box::new(Expression::from(Term::Int(1))),
                    if_: Box::new(Expression::from(Term::Int(2))),
                    else_: Box::new(Expression::from(Term::Int(3))),
                }),
            }),
            rhs: Box::new(Expression::from(Term::Int(4))),
        }
    )
}

#[test]
fn ternary_nesting() {
    // 1 ? 2 : (3 ? 4 : 5)
    assert_eq!(
        parse_expr("1 ? 2 : 3 ? 4 : 5"),
        Expression::TernaryOp {
            cond: Box::new(Expression::from(Term::Int(1))),
            if_: Box::new(Expression::from(Term::Int(2))),
            else_: Box::new(Expression::TernaryOp {
                cond: Box::new(Expression::from(Term::Int(3))),
                if_: Box::new(Expression::from(Term::Int(4))),
                else_: Box::new(Expression::from(Term::Int(5))),
            }),
        }
    );
    // 1 ? (2 ? 3 : 4) : 5
    assert_eq!(
        parse_expr("1 ? 2 ? 3 : 4 : 5"),
        Expression::TernaryOp {
            cond: Box::new(Expression::from(Term::Int(1))),
            if_: Box::new(Expression::TernaryOp {
                cond: Box::new(Expression::from(Term::Int(2))),
                if_: Box::new(Expression::from(Term::Int(3))),
                else_: Box::new(Expression::from(Term::Int(4))),
            }),
            else_: Box::new(Expression::from(Term::Int(5))),
        }
    );
}

#[test]
fn ternary_without_spaces() {
    parse_expr(r#"listkey = set_keyword ? "[set_keyword] [locname]":"[locname]""#);
    parse_expr(r#"pump_direction?("release"):("siphon")"#);
}

#[test]
fn bitop_precedence() {
    // 1 | (6 & 2)
    assert_eq!(
        parse_expr("1 | 6 & 2"),
        Expression::BinaryOp {
            op: BinaryOp::BitOr,
            lhs: Box::new(Expression::from(Term::Int(1))),
            rhs: Box::new(Expression::BinaryOp {
                op: BinaryOp::BitAnd,
                lhs: Box::new(Expression::from(Term::Int(6))),
                rhs: Box::new(Expression::from(Term::Int(2))),
            }),
        }
    );
}

#[test]
fn pointer_ops() {
    assert_eq!(
        parse_expr("*&1"),
        Expression::Base {
            term: Box::new(Spanned::new(Default::default(), Term::Int(1))),
            follow: vec![
                Spanned::new(Default::default(), Follow::Unary(UnaryOp::Reference)),
                Spanned::new(Default::default(), Follow::Unary(UnaryOp::Dereference)),
            ]
            .into_boxed_slice(),
        }
    )
}

#[test]
fn call_ext() {
    assert_eq!(
        parse_expr("call_ext(\"cat.dll\", \"meow\")(1, 2, 3)"),
        Expression::Base {
            term: Box::new(Spanned::new(
                Default::default(),
                Term::ExternalCall {
                    library: Some(Box::new(Expression::from(Term::String(
                        "cat.dll".to_owned()
                    )))),
                    function: Box::new(Expression::from(Term::String("meow".to_owned()))),
                    args: Box::new([
                        Expression::Base {
                            term: Box::new(Spanned::new(Default::default(), Term::Int(1))),
                            follow: Box::new([])
                        },
                        Expression::Base {
                            term: Box::new(Spanned::new(Default::default(), Term::Int(2))),
                            follow: Box::new([])
                        },
                        Expression::Base {
                            term: Box::new(Spanned::new(Default::default(), Term::Int(3))),
                            follow: Box::new([])
                        }
                    ])
                }
            )),
            follow: Box::new([]),
        }
    )
}

#[test]
fn loaded_call_ext() {
    assert_eq!(
        parse_expr("call_ext(loaded_cat_meow)(1, 2, 3)"),
        Expression::Base {
            term: Box::new(Spanned::new(
                Default::default(),
                Term::ExternalCall {
                    library: None,
                    function: Box::new(Expression::from(Term::Ident("loaded_cat_meow".into()))),
                    args: Box::new([
                        Expression::Base {
                            term: Box::new(Spanned::new(Default::default(), Term::Int(1))),
                            follow: Box::new([])
                        },
                        Expression::Base {
                            term: Box::new(Spanned::new(Default::default(), Term::Int(2))),
                            follow: Box::new([])
                        },
                        Expression::Base {
                            term: Box::new(Spanned::new(Default::default(), Term::Int(3))),
                            follow: Box::new([])
                        }
                    ])
                }
            )),
            follow: Box::new([]),
        }
    )
}
