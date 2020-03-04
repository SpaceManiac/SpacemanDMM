//! The symbol table used for "Find References" support.

use dm::Location;
use dm::ast::*;

pub fn extract(block: &[Spanned<Statement>]) -> Vec<String> {
    let mut output = Vec::new();
    WalkProc { local_vars: &mut output }.visit_block(block);
    output
}

struct WalkProc<'o> {
    local_vars: &'o mut Vec<String>,
}

impl<'o> WalkProc<'o> {
    fn visit_block(&mut self, block: &'o [Spanned<Statement>]) {
        for stmt in block.iter() {
            self.visit_statement(stmt.location, &stmt.elem);
        }
    }

    fn visit_statement(&mut self, location: Location, statement: &'o Statement) {
        match statement {
            Statement::Expr(_) => {},
            Statement::Return(_) => {},
            Statement::Crash(_) => {},
            Statement::Throw(_) => {},
            Statement::While { block, .. } => self.visit_block(block),
            Statement::DoWhile { block, .. } => self.visit_block(block),
            Statement::If { arms, else_arm } => {
                for &(_, ref block) in arms.iter() {
                    self.visit_block(block);
                }
                if let Some(else_arm) = else_arm {
                    self.visit_block(else_arm);
                }
            },
            Statement::ForLoop { init, test: _, inc, block } => {
                if let Some(init) = init {
                    self.visit_statement(location, init);
                }
                if let Some(inc) = inc {
                    self.visit_statement(location, inc);
                }
                self.visit_block(block);
            },
            Statement::ForList { block, var_type, name, .. } => {
                if let Some(var_type) = var_type {
                    self.visit_var(location, var_type, name, None);
                }
                self.visit_block(block);
            },
            Statement::ForRange { var_type, name, start, end: _, step: _, block } => {
                if let Some(var_type) = var_type {
                    self.visit_var(location, var_type, name, Some(start));
                }
                self.visit_block(block);
            },
            Statement::Var(var) => self.visit_var_stmt(location, var),
            Statement::Vars(vars) => {
                for each in vars.iter() {
                    self.visit_var_stmt(location, each);
                }
            },
            Statement::Setting { .. } => {},
            Statement::Spawn { delay: _, block } => {
                self.visit_block(block);
            },
            Statement::Switch { input: _, cases, default } => {
                for &(_, ref block) in cases.iter() {
                    self.visit_block(block);
                }
                if let Some(default) = default {
                    self.visit_block(default);
                }
            },
            Statement::TryCatch { try_block, catch_params, catch_block } => {
                self.visit_block(try_block);
                for caught in catch_params.iter() {
                    let (var_name, mut type_path) = match caught.split_last() {
                        Some(x) => x,
                        None => continue
                    };
                    match type_path.split_first() {
                        Some((first, rest)) if first == "var" => type_path = rest,
                        _ => {}
                    }
                    let var_type: VarType = type_path.iter().map(ToOwned::to_owned).collect();
                    self.visit_var(location, &var_type, var_name, None);
                }
                self.visit_block(catch_block);
            },
            Statement::Continue(_) => {},
            Statement::Break(_) => {},
            Statement::Goto(_) => {},
            Statement::Label { name: _, block } => self.visit_block(block),
            Statement::Del(_) => {},
        }
    }

    fn visit_var_stmt(&mut self, location: Location, var: &'o VarStatement) {
        self.visit_var(location, &var.var_type, &var.name, var.value.as_ref())
    }

    fn visit_var(&mut self, _location: Location, var_type: &VarType, name: &str, _value: Option<&'o Expression>) {
        // static and const vars do not exist in the stack frame
        if !var_type.is_static && !var_type.is_const {
            self.local_vars.push(name.to_owned());
        }
    }
}
