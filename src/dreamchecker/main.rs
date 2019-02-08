//! DreamChecker, a robust static analysis and typechecking engine for
//! DreamMaker.
#![allow(dead_code)]

extern crate dreammaker as dm;
use dm::objtree::{ProcValue, Code};
use dm::ast::{Statement, Expression, VarStatement};

use std::collections::HashMap;

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
            Statement::Expr(expr) => self.visit_expression(expr),
            Statement::Return(Some(expr)) => self.visit_expression(expr),
            Statement::Return(None) => {},
            Statement::Throw(expr) => self.visit_expression(expr),
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
            Statement::Switch { .. } => {}  // TODO
            Statement::TryCatch { .. } => {}  // TODO
            Statement::Continue(_) => {},
            Statement::Break(_) => {},
            Statement::Label { name: _, block } => self.visit_block(block),
            Statement::Del(expr) => self.visit_expression(expr),
        }
    }

    fn visit_expression(&mut self, expression: &Expression) {
    }

    fn visit_var(&mut self, var: &VarStatement) {
    }
}

fn some_analysis(func: &ProcValue, code: &[Statement]) {
    println!("{:?}", func.parameters);
    println!("{:#?}", code);
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
