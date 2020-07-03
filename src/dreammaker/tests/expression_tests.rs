extern crate dreammaker as dm;

use dm::lexer::Lexer;
use dm::parser::*;
use dm::ast::*;

fn parse_expr(f: &str) -> Expression {
    let context = Default::default();
    let lexer = Lexer::new(&context, Default::default(), f.as_bytes());
    let result = parse_expression(&context, Default::default(), lexer).expect("failed to parse expression");
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
            lhs: Box::new(Expression::from(Term::Ident("foo".to_owned()))),
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
