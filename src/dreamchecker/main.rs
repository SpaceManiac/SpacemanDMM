//! DreamChecker, a robust static analysis and typechecking engine for
//! DreamMaker.
#![allow(dead_code, unused_variables)]

extern crate dreammaker as dm;
use dm::objtree::{ProcValue, Code};
use dm::constants::{Constant, ConstFn};
use dm::ast::*;

// ----------------------------------------------------------------------------
// Helper structures

enum Type {
    Any,
    Null,
    String,
    Resource,
    Number,
    List,
    Instance(String),
    Typepath(String),
}

impl Type {
    fn from_constant(constant: &Constant) -> Type {
        match constant {
            Constant::Null(_) => Type::Null,
            Constant::String(_) => Type::String,
            Constant::Resource(_) => Type::Resource,
            Constant::Int(_) => Type::Number,
            Constant::Float(_) => Type::Number,
            Constant::List(_) => Type::List,
            Constant::Call(func, _) => match func {
                ConstFn::Icon => Type::Instance("/icon".to_owned()),
                ConstFn::Matrix => Type::Instance("/matrix".to_owned()),
                ConstFn::Newlist => Type::List,
                ConstFn::Sound => Type::Instance("/sound".to_owned()),
            },
            // TODO: New => Instance, Prefab => Typepath
            _ => Type::Any,
        }
    }
}

/// An 'atom' in the type analysis. A type/set of possible types, as well as a
/// known constant value if available.
struct Analysis {
    ty: Type,
    value: Option<Constant>,
}

impl From<Type> for Analysis {
    fn from(ty: Type) -> Analysis {
        Analysis { ty, value: None }
    }
}

impl From<Constant> for Analysis {
    fn from(value: Constant) -> Analysis {
        Analysis {
            ty: Type::from_constant(&value),
            value: Some(value),
        }
    }
}

// ----------------------------------------------------------------------------
// Procedure analyzer

struct ProcAnalyzer {
}

impl ProcAnalyzer {
    fn visit_block(&mut self, block: &[Statement]) {
        for stmt in block.iter() {
            self.visit_statement(stmt);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Expr(expr) => { self.visit_expression(expr); },
            Statement::Return(Some(expr)) => { self.visit_expression(expr); },
            Statement::Return(None) => {},
            Statement::Throw(expr) => { self.visit_expression(expr); },
            Statement::While { condition, block } => {
                self.visit_expression(condition);
                self.visit_block(block);
            },
            Statement::DoWhile { block, condition } => {
                self.visit_block(block);
                self.visit_expression(condition);
            },
            Statement::If { arms, else_arm } => {
                for &(ref condition, ref block) in arms.iter() {
                    self.visit_expression(condition);
                    self.visit_block(block);
                }
                if let Some(else_arm) = else_arm {
                    self.visit_block(else_arm);
                }
            },
            Statement::ForLoop { init, test, inc, block } => {
                if let Some(init) = init {
                    self.visit_statement(init);
                }
                if let Some(test) = test {
                    self.visit_expression(test);
                }
                if let Some(inc) = inc {
                    self.visit_statement(inc);
                }
                self.visit_block(block);
            },
            Statement::ForList { in_list, block, .. } => {
                if let Some(in_list) = in_list {
                    self.visit_expression(in_list);
                }
                self.visit_block(block);
            },
            Statement::ForRange { start, end, step, block, .. } => {
                self.visit_expression(start);
                self.visit_expression(end);
                if let Some(step) = step {
                    self.visit_expression(step);
                }
                self.visit_block(block);
            },
            Statement::Var(var) => self.visit_var(var),
            Statement::Vars(vars) => {
                for each in vars.iter() {
                    self.visit_var(each);
                }
            },
            Statement::Setting { .. } => {},
            Statement::Spawn { delay, block } => {
                if let Some(delay) = delay {
                    self.visit_expression(delay);
                }
                self.visit_block(block);
            },
            Statement::Switch { input, cases, default } => {
                self.visit_expression(input);
                for &(ref case, ref block) in cases.iter() {
                    for case_part in case.iter() {
                        match case_part {
                            dm::ast::Case::Exact(expr) => { self.visit_expression(expr); },
                            dm::ast::Case::Range(start, end) => {
                                self.visit_expression(start);
                                self.visit_expression(end);
                            }
                        }
                    }
                    self.visit_block(block);
                }
                if let Some(default) = default {
                    self.visit_block(default);
                }
            },
            Statement::TryCatch { try_block, catch_block, .. } => {
                self.visit_block(try_block);
                self.visit_block(catch_block);
            },
            Statement::Continue(_) => {},
            Statement::Break(_) => {},
            Statement::Label { name: _, block } => self.visit_block(block),
            Statement::Del(expr) => { self.visit_expression(expr); },
        }
    }

    fn visit_var(&mut self, var: &VarStatement) {
    }

    fn visit_expression(&mut self, expression: &Expression) -> Analysis {
        match expression {
            Expression::Base { unary, term, follow } => {
                let mut ty = self.visit_term(term);
                for each in follow.iter() {
                    ty = self.visit_follow(ty, each);
                }
                for each in unary.iter().rev() {
                    ty = self.visit_unary(ty, each);
                }
                ty
            },
            Expression::BinaryOp { lhs, rhs, .. } => {
                let lty = self.visit_expression(lhs);
                let rty = self.visit_expression(rhs);
                unimplemented!()
            },
            Expression::AssignOp { lhs, rhs, .. } => {
                self.visit_expression(rhs)
                // TODO: visit LHS?
            },
            Expression::TernaryOp { cond, if_, else_ } => {
                // TODO: be sensible
                self.visit_expression(cond);
                let ty = self.visit_expression(if_);
                self.visit_expression(else_);
                ty
            }
        }
    }

    fn visit_term(&mut self, term: &Term) -> Analysis {
        match term {
            Term::Null => Analysis::from(Type::Null),
            Term::New { type_, .. } => match type_ {
                NewType::Implicit => Analysis::from(Type::Any),  // TODO: type hinting
                NewType::Ident(_) => Analysis::from(Type::Any),  // TODO: lookup
                // TODO: evaluate path operators
                NewType::Prefab(prefab) => Analysis::from(Type::Instance(format!("{:?}", prefab.path))),
            },
            Term::List(_) => Analysis::from(Type::List),
            _ => Analysis::from(Type::Any),
        }
    }

    fn visit_follow(&mut self, lhs: Analysis, rhs: &Follow) -> Analysis {
        unimplemented!()
    }

    fn visit_unary(&mut self, rhs: Analysis, op: &UnaryOp) -> Analysis {
        unimplemented!()
    }
}

fn some_analysis(func: &ProcValue, code: &[Statement]) {
    println!("{:?}", func.parameters);
    println!("{:#?}", code);
    ProcAnalyzer{}.visit_block(code);
}

fn main() {
    let mut context = dm::Context::default();
    context.set_print_severity(Some(dm::Severity::Error));
    let env = dm::detect_environment_default()
        .expect("error detecting .dme")
        .expect("no .dme found");
    let pp = dm::preprocessor::Preprocessor::new(&context, env)
        .expect("i/o error opening .dme");
    let indents = dm::indents::IndentProcessor::new(&context, pp);
    let mut parser = dm::parser::Parser::new(&context, indents);
    parser.enable_procs();
    let tree = parser.parse_object_tree();

    let mut present = 0;
    let mut invalid = 0;
    let mut builtin = 0;
    let mut disabled = 0;

    tree.root().recurse(&mut |ty| {
        for proc in ty.procs.values() {
            for value in proc.value.iter() {
                match value.code {
                    Code::Present(ref code) => {
                        present += 1;
                        some_analysis(&value, code);
                        std::process::exit(0);
                    }
                    Code::Invalid(_) => invalid += 1,
                    Code::Builtin => builtin += 1,
                    Code::Disabled => disabled += 1,
                }
            }
        }
    });

    println!("{:?}", (present, invalid, builtin, disabled));
}
