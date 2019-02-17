//! DreamChecker, a robust static analysis and typechecking engine for
//! DreamMaker.
#![allow(dead_code, unused_variables)]

extern crate dreammaker as dm;
use dm::Context;
use dm::objtree::{ProcValue, Code, ObjectTree, TypeRef};
use dm::constants::{Constant, ConstFn};
use dm::ast::*;

// ----------------------------------------------------------------------------
// Helper structures

#[derive(Copy, Clone, Debug)]
enum Type<'o> {
    Any,
    Null,
    String,
    Resource,
    Number,
    List(Option<TypeRef<'o>>),
    Instance(TypeRef<'o>),
    Typepath(TypeRef<'o>),
}

impl<'o> Type<'o> {
    fn from_constant(objtree: &'o ObjectTree, constant: &Constant) -> Type<'o> {
        match constant {
            Constant::Null(_) => Type::Null,
            Constant::String(_) => Type::String,
            Constant::Resource(_) => Type::Resource,
            Constant::Int(_) => Type::Number,
            Constant::Float(_) => Type::Number,
            Constant::List(_) => Type::List(None),
            Constant::Call(func, _) => match func {
                ConstFn::Icon => Type::Instance(objtree.find("/icon").unwrap()),
                ConstFn::Matrix => Type::Instance(objtree.find("/matrix").unwrap()),
                ConstFn::Newlist => Type::List(None),
                ConstFn::Sound => Type::Instance(objtree.find("/sound").unwrap()),
            },
            // TODO: New => Instance, Prefab => Typepath
            _ => Type::Any,
        }
    }
}

/// An 'atom' in the type analysis. A type/set of possible types, as well as a
/// known constant value if available.
struct Analysis<'o> {
    ty: Type<'o>,
    value: Option<Constant>,
}

impl<'o> From<Type<'o>> for Analysis<'o> {
    fn from(ty: Type<'o>) -> Analysis<'o> {
        Analysis { ty, value: None }
    }
}

impl<'o> Analysis<'o> {
    fn empty() -> Analysis<'o> {
        Type::Any.into()
    }

    fn from_value(objtree: &'o ObjectTree, value: Constant) -> Analysis<'o> {
        Analysis {
            ty: Type::from_constant(objtree, &value),
            value: Some(value),
        }
    }
}

// ----------------------------------------------------------------------------
// Procedure analyzer

struct ProcAnalyzer<'o> {
    context: &'o Context,
    objtree: &'o ObjectTree,
    ty: TypeRef<'o>,
}

impl<'o> ProcAnalyzer<'o> {
    fn visit_block(&mut self, block: &[Statement]) {
        for stmt in block.iter() {
            self.visit_statement(stmt);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Expr(expr) => { self.visit_expression(expr, None); },
            Statement::Return(Some(expr)) => { self.visit_expression(expr, None); },
            Statement::Return(None) => {},
            Statement::Throw(expr) => { self.visit_expression(expr, None); },
            Statement::While { condition, block } => {
                self.visit_expression(condition, None);
                self.visit_block(block);
            },
            Statement::DoWhile { block, condition } => {
                self.visit_block(block);
                self.visit_expression(condition, None);
            },
            Statement::If { arms, else_arm } => {
                for &(ref condition, ref block) in arms.iter() {
                    self.visit_expression(condition, None);
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
                    self.visit_expression(test, None);
                }
                if let Some(inc) = inc {
                    self.visit_statement(inc);
                }
                self.visit_block(block);
            },
            Statement::ForList { in_list, block, .. } => {
                if let Some(in_list) = in_list {
                    self.visit_expression(in_list, None);
                }
                self.visit_block(block);
            },
            Statement::ForRange { start, end, step, block, .. } => {
                self.visit_expression(start, None);
                self.visit_expression(end, None);
                if let Some(step) = step {
                    self.visit_expression(step, None);
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
                    self.visit_expression(delay, None);
                }
                self.visit_block(block);
            },
            Statement::Switch { input, cases, default } => {
                self.visit_expression(input, None);
                for &(ref case, ref block) in cases.iter() {
                    for case_part in case.iter() {
                        match case_part {
                            dm::ast::Case::Exact(expr) => { self.visit_expression(expr, None); },
                            dm::ast::Case::Range(start, end) => {
                                self.visit_expression(start, None);
                                self.visit_expression(end, None);
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
            Statement::Del(expr) => { self.visit_expression(expr, None); },
        }
    }

    fn visit_var(&mut self, var: &VarStatement) {
    }

    fn visit_expression(&mut self, expression: &Expression, type_hint: Option<TypeRef<'o>>) -> Analysis<'o> {
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
                let lty = self.visit_expression(lhs, None);
                let rty = self.visit_expression(rhs, None);
                //unimplemented!()
                Analysis::empty()
            },
            Expression::AssignOp { lhs, rhs, .. } => {
                self.visit_expression(rhs, type_hint)
                // TODO: visit LHS?
            },
            Expression::TernaryOp { cond, if_, else_ } => {
                // TODO: be sensible
                self.visit_expression(cond, None);
                let ty = self.visit_expression(if_, type_hint);
                self.visit_expression(else_, type_hint);
                ty
            }
        }
    }

    fn visit_term(&mut self, term: &Term) -> Analysis<'o> {
        match term {
            Term::Null => Type::Null.into(),
            Term::New { type_, .. } => match type_ {
                NewType::Implicit => Type::Any.into(),  // TODO: type hinting
                NewType::Ident(_) => Type::Any.into(),  // TODO: lookup
                NewType::Prefab(prefab) => {
                    if let Some(ty) = self.ty.navigate_path(&prefab.path) {
                        Type::Instance(ty).into()
                    } else {
                        Type::Any.into()  // TODO: report lookup error
                    }
                },
            },
            Term::List(_) => Type::List(None).into(),
            _ => Type::Any.into(),
        }
    }

    fn visit_follow(&mut self, lhs: Analysis<'o>, rhs: &Follow) -> Analysis<'o> {
        match rhs {
            Follow::Index(expr) => {
                Type::Any.into()
            },
            Follow::Field(kind, name) => {
                Type::Any.into()
            },
            Follow::Call(kind, name, arguments) => {
                // TODO: checking
                Type::Any.into()
            },
        }
    }

    fn visit_unary(&mut self, rhs: Analysis<'o>, op: &UnaryOp) -> Analysis<'o> {
        match (op, rhs.ty) {
            (UnaryOp::Not, _) => Type::Number.into(),
            (UnaryOp::Neg, Type::Number) => Type::Number.into(),
            (UnaryOp::BitNot, Type::Number) => Type::Number.into(),
            (UnaryOp::PreIncr, Type::Number) => Type::Number.into(),
            (UnaryOp::PostIncr, Type::Number) => Type::Number.into(),
            (UnaryOp::PreDecr, Type::Number) => Type::Number.into(),
            (UnaryOp::PostDecr, Type::Number) => Type::Number.into(),
            _ => Type::Any.into(),
        }
    }
}

fn some_analysis(context: &Context, objtree: &ObjectTree, ty: TypeRef, func: &ProcValue, code: &[Statement]) {
    println!("{:?}", func.parameters);
    println!("{:#?}", code);
    ProcAnalyzer { context, objtree, ty }.visit_block(code);
}

fn main() {
    let mut context = Context::default();
    context.set_print_severity(Some(dm::Severity::Info));
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
                        some_analysis(&context, &tree, ty, &value, code);
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
