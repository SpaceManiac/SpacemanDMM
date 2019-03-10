//! DreamChecker, a robust static analysis and typechecking engine for
//! DreamMaker.
#![allow(dead_code, unused_variables)]

extern crate dreammaker as dm;
use dm::{Context, DMError, Location};
use dm::objtree::{Code, ObjectTree, TypeRef, ProcRef};
use dm::constants::{Constant, ConstFn};
use dm::ast::*;

use std::collections::{BTreeMap, HashMap, HashSet};

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
    Global,
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
                ConstFn::Icon => Type::Instance(objtree.expect("/icon")),
                ConstFn::Matrix => Type::Instance(objtree.expect("/matrix")),
                ConstFn::Newlist => Type::List(None),
                ConstFn::Sound => Type::Instance(objtree.expect("/sound")),
            },
            // TODO: New => Instance, Prefab => Typepath
            _ => Type::Any,
        }
    }
}

/// An 'atom' in the type analysis. A type/set of possible types, as well as a
/// known constant value if available.
#[derive(Debug, Clone)]
struct Analysis<'o> {
    static_ty: Option<TypeRef<'o>>,
    ty: Type<'o>,
    value: Option<Constant>,
}

impl<'o> From<Type<'o>> for Analysis<'o> {
    fn from(ty: Type<'o>) -> Analysis<'o> {
        Analysis { static_ty: None, ty, value: None }
    }
}

impl<'o> Analysis<'o> {
    fn empty() -> Analysis<'o> {
        Type::Any.into()
    }

    fn null() -> Analysis<'o> {
        Analysis {
            static_ty: None,
            ty: Type::Null,
            value: Some(Constant::Null(None)),
        }
    }

    fn from_static_type(ty: TypeRef<'o>) -> Analysis<'o> {
        Analysis {
            static_ty: Some(ty),
            ty: Type::Instance(ty),
            value: None,
        }
    }

    fn from_value(objtree: &'o ObjectTree, value: Constant) -> Analysis<'o> {
        Analysis {
            static_ty: None,
            ty: Type::from_constant(objtree, &value),
            value: Some(value),
        }
    }
}

// ----------------------------------------------------------------------------
// Analysis environment

struct BadOverride {
    missing: Vec<String>,
    location: Location,
}

struct CalledAt {
    location: Location,
    others: u32,
}

#[derive(Default)]
struct KwargInfo {
    location: Location,
    // kwarg name -> location that the proc is called with that arg
    called_at: BTreeMap<String, CalledAt>,
    // Debug(ProcRef) -> its definition location
    bad_overrides_at: BTreeMap<String, BadOverride>,
}

#[derive(Default)]
struct Env {
    // Debug(ProcRef) -> KwargInfo
    used_kwargs: BTreeMap<String, KwargInfo>,
}

impl Env {
    fn check_kwargs(&mut self, context: &Context, proc: ProcRef) {
        let param_names: HashSet<&String> = proc.parameters.iter().map(|p| &p.name).collect();

        // Start at the parent - calls which immediately resolve to bad kwargs
        // error earlier in the process.
        let mut next = proc.parent_proc();
        while let Some(current) = next {
            if let Some(kwargs) = self.used_kwargs.get_mut(&current.to_string()) {
                let mut missing = Vec::new();

                for (keyword, location) in kwargs.called_at.iter() {
                    if !param_names.contains(keyword) {
                        missing.push(keyword.to_owned());
                    }
                }

                if !missing.is_empty() {
                    kwargs.bad_overrides_at.insert(
                        proc.ty().path.to_owned(),
                        BadOverride { missing, location: proc.location });
                }
            }
            next = current.parent_proc();
        }
    }

    fn finish_check_kwargs(&self, context: &Context) {
        for (base_procname, kwarg_info) in self.used_kwargs.iter() {
            if kwarg_info.bad_overrides_at.is_empty() {
                continue
            }

            // List out the child procs that are missing overrides.
            let mut error = DMError::new(kwarg_info.location, format!("overrides of {} are missing keyword args", base_procname));
            let mut missing = HashSet::new();
            for (child_procname, bad_override) in kwarg_info.bad_overrides_at.iter() {
                error.add_note(bad_override.location, format!("{} is missing \"{}\"",
                    child_procname,
                    bad_override.missing.join("\", \"")));
                missing.extend(bad_override.missing.iter());
            }

            // List call sites. If nobody ever calls these as kwargs, then
            // there's not gonna be a problem.
            for (arg_name, called_at) in kwarg_info.called_at.iter() {
                if !missing.contains(arg_name) {
                    continue
                }

                if called_at.others > 0 {
                    error.add_note(called_at.location, format!("called with {:?} here, and {} other places",
                        arg_name, called_at.others));
                } else {
                    error.add_note(called_at.location, format!("called with {:?} here", arg_name));
                }
            }

            error.register(context);
        }
    }
}

// ----------------------------------------------------------------------------
// Procedure analyzer

struct ProcAnalyzer<'o> {
    env: &'o mut Env,
    context: &'o Context,
    objtree: &'o ObjectTree,
    ty: TypeRef<'o>,
    proc_ref: ProcRef<'o>,
    local_vars: HashMap<String, Analysis<'o>>,
    messages: Vec<String>,
}

impl<'o> ProcAnalyzer<'o> {
    fn new(env: &'o mut Env, context: &'o Context, objtree: &'o ObjectTree, proc_ref: ProcRef<'o>) -> Self {
        let ty = proc_ref.ty();

        let mut local_vars = HashMap::new();
        local_vars.insert(".".to_owned(), Analysis::empty());
        local_vars.insert("args".to_owned(), Analysis::from_static_type(objtree.expect("/list")));
        local_vars.insert("usr".to_owned(), Analysis::from_static_type(objtree.expect("/mob")));
        if !ty.is_root() {
            local_vars.insert("src".to_owned(), Analysis::from_static_type(ty));
        }
        local_vars.insert("global".to_owned(), Analysis {
            static_ty: Some(objtree.root()),
            ty: Type::Global,
            value: None,
        });

        ProcAnalyzer {
            env,
            context,
            objtree,
            ty,
            proc_ref,
            local_vars,
            messages: Vec::new(),
        }
    }

    fn run(&mut self, block: &'o [Spanned<Statement>]) {
        for param in self.proc_ref.get().parameters.iter() {
            let analysis = self.static_type(&param.var_type.type_path);
            self.local_vars.insert(param.name.to_owned(), analysis);
        }
        self.visit_block(block);

        // TODO: maybe actually have span information on AST elements, so that
        // the locations here can be more precise.
        let mut error = DMError::new(self.proc_ref.location, format!("problems in {}", self.proc_ref));
        for message in self.messages.drain(..) {
            error.add_note(self.proc_ref.location, message);
        }
        if !error.notes().is_empty() {
            error.register(self.context);
        }
    }

    fn error<S: Into<String>>(&mut self, message: S) {
        self.messages.push(message.into())
    }

    fn visit_block(&mut self, block: &'o [Spanned<Statement>]) {
        for stmt in block.iter() {
            self.visit_statement(&stmt.elem);
        }
    }

    fn visit_statement(&mut self, statement: &'o Statement) {
        match statement {
            Statement::Expr(expr) => { self.visit_expression(expr, None); },
            Statement::Return(Some(expr)) => {
                // TODO: factor in the previous return type if there was one
                let return_type = self.visit_expression(expr, None);
                self.local_vars.insert(".".to_owned(), return_type);
                // TODO: break out of the analysis for this branch?
            },
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
            Statement::ForList { in_list, block, var_type, name, .. } => {
                if let Some(in_list) = in_list {
                    self.visit_expression(in_list, None);
                }
                if let Some(var_type) = var_type {
                    self.visit_var(var_type, name, None);
                }
                self.visit_block(block);
            },
            Statement::ForRange { var_type, name, start, end, step, block } => {
                self.visit_expression(end, None);
                if let Some(step) = step {
                    self.visit_expression(step, None);
                }
                if let Some(var_type) = var_type {
                    self.visit_var(var_type, name, Some(start));
                }
                self.visit_block(block);
            },
            Statement::Var(var) => self.visit_var_stmt(var),
            Statement::Vars(vars) => {
                for each in vars.iter() {
                    self.visit_var_stmt(each);
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

    fn visit_var_stmt(&mut self, var: &'o VarStatement) {
        self.visit_var(&var.var_type, &var.name, var.value.as_ref())
    }

    fn visit_var(&mut self, var_type: &VarType, name: &str, value: Option<&'o Expression>) {
        // Calculate type hint
        let type_hint;
        if var_type.type_path.is_empty() {
            type_hint = None;
        } else {
            type_hint = self.objtree.type_by_path(&var_type.type_path);
            if type_hint.is_none() {
                self.error(format!("undefined type: {}", FormatTreePath(&var_type.type_path)));
            }
        };

        // Visit the expression if it's there
        let mut analysis = match value {
            Some(ref expr) => self.visit_expression(expr, type_hint),
            None => Analysis::null(),
        };
        analysis.static_ty = type_hint;

        // Save var to locals
        self.local_vars.insert(name.to_owned(), analysis);
    }

    fn visit_expression(&mut self, expression: &'o Expression, type_hint: Option<TypeRef<'o>>) -> Analysis<'o> {
        match expression {
            Expression::Base { unary, term, follow } => {
                let base_type_hint = if follow.is_empty() && unary.is_empty() {
                    type_hint
                } else {
                    None
                };
                let mut ty = self.visit_term(term, base_type_hint);
                for each in follow.iter() {
                    ty = self.visit_follow(ty, each);
                }
                for each in unary.iter().rev() {
                    ty = self.visit_unary(ty, each);
                }
                ty
            },
            Expression::BinaryOp { op: BinaryOp::Or, lhs, rhs } => {
                // It appears that DM does this in more cases than this, but
                // this is the only case I've seen it used in the wild.
                // ex: var/datum/cache_entry/E = cache[key] || new
                let lty = self.visit_expression(lhs, type_hint);
                let rty = self.visit_expression(rhs, type_hint);
                self.visit_binary(lty, rty, BinaryOp::Or)
            },
            Expression::BinaryOp { op, lhs, rhs } => {
                let lty = self.visit_expression(lhs, None);
                let rty = self.visit_expression(rhs, None);
                self.visit_binary(lty, rty, *op)
            },
            Expression::AssignOp { lhs, rhs, .. } => {
                let lhs = self.visit_expression(lhs, None);
                self.visit_expression(rhs, lhs.static_ty)
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

    fn visit_term(&mut self, term: &'o Term, type_hint: Option<TypeRef<'o>>) -> Analysis<'o> {
        match term {
            Term::Null => Analysis::null(),
            Term::New { type_, args } => {
                // determine the type being new'd
                let typepath = match type_ {
                    NewType::Implicit => if let Some(hint) = type_hint {
                        Some(hint)
                    } else {
                        self.error("no type hint available on implicit new()");
                        None
                    },
                    NewType::Prefab(prefab) => {
                        if let Some(nav) = self.ty.navigate_path(&prefab.path) {
                            // TODO: handle proc/verb paths here
                            Some(nav.ty())
                        } else {
                            self.error(format!("failed to resolve path {}", FormatTypePath(&prefab.path)));
                            None
                        }
                    },
                    NewType::MiniExpr { .. } => None,  // TODO: evaluate
                };

                // call to the New() method
                if let Some(typepath) = typepath {
                    self.visit_call(
                        typepath,
                        typepath.get_proc("New").expect("couldn't find New proc"),
                        args.as_ref().map_or(&[], |v| &v[..]),
                        // New calls are exact: `new /datum()` will always call
                        // `/datum/New()` and never an override.
                        true);
                    Type::Instance(typepath).into()
                } else {
                    Analysis::empty()
                }
            },
            Term::List(_) => Type::List(None).into(),
            Term::Prefab(prefab) => {
                if let Some(nav) = self.ty.navigate_path(&prefab.path) {
                    // TODO: handle proc/verb paths here
                    Type::Typepath(nav.ty()).into()
                } else {
                    self.error(format!("failed to resolve path {}", FormatTypePath(&prefab.path)));
                    Analysis::empty()
                }
            },
            Term::String(text) => Analysis::from_value(self.objtree, Constant::String(text.to_owned())),
            Term::Resource(text) => Analysis::from_value(self.objtree, Constant::Resource(text.to_owned())),
            Term::Int(number) => Analysis::from_value(self.objtree, Constant::from(*number)),
            Term::Float(number) => Analysis::from_value(self.objtree, Constant::from(*number)),
            Term::Expr(expr) => self.visit_expression(expr, type_hint),
            Term::InterpString(..) => Type::String.into(),
            Term::Call(unscoped_name, args) => {
                let src = self.ty;
                if let Some(proc) = self.ty.get_proc(unscoped_name) {
                    self.visit_call(src, proc, args, false)
                } else {
                    let msg = format!("undefined proc: {:?} on {}", unscoped_name, self.ty);
                    self.error(msg);
                    Analysis::empty()
                }
            },
            Term::Ident(unscoped_name) => {
                if let Some(var) = self.local_vars.get(unscoped_name) {
                    return var.clone()
                }
                if let Some(decl) = self.ty.get_var_declaration(unscoped_name) {
                    self.static_type(&decl.var_type.type_path)
                } else {
                    self.error(format!("undefined var: {:?}", unscoped_name));
                    Analysis::empty()
                }
            },
            Term::ParentCall(args) => {
                if let Some(proc) = self.proc_ref.parent_proc() {
                    // TODO: if args are empty, call w/ same args
                    let src = self.ty;
                    // Parent calls are exact, and won't ever call an override.
                    self.visit_call(src, proc, args, true)
                } else {
                    let msg = format!("proc has no parent: {:?}", self.proc_ref);
                    self.error(msg);
                    Analysis::empty()
                }
            },
            Term::SelfCall(args) => {
                let src = self.ty;
                let proc = self.proc_ref;
                // Self calls are exact, and won't ever call an override.
                self.visit_call(src, proc, args, true)
            },
            Term::Locate { args, .. } => {
                // TODO: deal with in_list
                if args.len() == 3 {  // X,Y,Z - it's gotta be a turf
                    Type::Instance(self.objtree.expect("/turf")).into()
                } else {
                    Analysis::empty()
                }
            },
            Term::Input { args, input_type, .. } => {
                // TODO: deal with in_list
                let without_null = *input_type - InputType::NULL;
                if input_type.contains(InputType::ANYTHING) {
                    Analysis::empty()
                } else if without_null == InputType::MOB {
                    Type::Instance(self.objtree.expect("/mob")).into()
                } else if without_null == InputType::OBJ {
                    Type::Instance(self.objtree.expect("/obj")).into()
                } else if without_null == InputType::AREA {
                    Type::Instance(self.objtree.expect("/area")).into()
                } else if without_null == InputType::TURF {
                    Type::Instance(self.objtree.expect("/turf")).into()
                } else if without_null == InputType::TEXT || without_null == InputType::MESSAGE || without_null == InputType::KEY || without_null == InputType::PASSWORD || without_null == InputType::COLOR || without_null.is_empty() {
                    Type::String.into()
                } else if without_null == InputType::NUM {
                    Type::Number.into()
                } else if without_null == InputType::ICON {
                    Type::Instance(self.objtree.expect("/icon")).into()
                } else if without_null == InputType::SOUND {
                    Type::Instance(self.objtree.expect("/sound")).into()
                } else if without_null == InputType::FILE {
                    // TODO: it's not clear that this is correct
                    Type::Resource.into()
                } else {
                    //self.error(format!("visit_term: weird input() type: {:?}", input_type));
                    Analysis::empty()
                }
            },
            Term::DynamicCall(lhs_args, rhs_args) => {
                Analysis::empty()  // TODO
            },
            Term::Pick(choices) => {
                if choices.len() == 1 {
                    match self.visit_expression(&choices[0].1, None).ty {
                        Type::List(Some(inst)) => Type::Instance(inst).into(),
                        _ => Analysis::empty(),
                    }
                } else {
                    // TODO: common superset of all choices
                    Analysis::empty()
                }
            },
        }
    }

    fn visit_follow(&mut self, lhs: Analysis<'o>, rhs: &'o Follow) -> Analysis<'o> {
        match rhs {
            Follow::Field(IndexKind::Colon, _) => Analysis::empty(),
            Follow::Field(IndexKind::SafeColon, _) => Analysis::empty(),
            Follow::Call(IndexKind::Colon, _, _) => Analysis::empty(),
            Follow::Call(IndexKind::SafeColon, _, _) => Analysis::empty(),

            Follow::Index(expr) => {
                if let Type::List(lty) = lhs.ty {
                    self.visit_expression(expr, None);
                    if let Some(ty) = lty {
                        Type::Instance(ty).into()
                    } else {
                        Analysis::empty()
                    }
                } else if lhs.static_ty == Some(self.objtree.expect("/list")) {
                    // TODO: keep track of what /list was declared
                    Analysis::empty()
                } else {
                    //self.show_header();
                    //println!("visit_follow: can't index {:?}", lhs);
                    Analysis::empty()
                }
            },
            Follow::Field(kind, name) => {
                if let Some(ty) = lhs.static_ty {
                    if let Some(decl) = ty.get_var_declaration(name) {
                        self.static_type(&decl.var_type.type_path)
                    } else {
                        self.error(format!("undefined field: {:?} on {}", name, ty));
                        Analysis::empty()
                    }
                } else {
                    self.error(format!("field access requires static type: {:?}", name));
                    Analysis::empty()
                }
            },
            Follow::Call(kind, name, arguments) => {
                if let Some(ty) = lhs.static_ty {
                    if let Some(proc) = ty.get_proc(name) {
                        self.visit_call(ty, proc, arguments, false)
                    } else {
                        self.error(format!("undefined proc: {:?} on {}", name, ty));
                        Analysis::empty()
                    }
                } else {
                    self.error(format!("proc call require static type: {:?} on {:?}", name, lhs));
                    Analysis::empty()
                }
            },
        }
    }

    fn visit_unary(&mut self, rhs: Analysis<'o>, op: &UnaryOp) -> Analysis<'o> {
        match (op, rhs.ty) {
            // !x just evaluates the "truthiness" of x and negates it, returning 1 or 0
            (UnaryOp::Not, _) => Type::Number.into(),
            (UnaryOp::Neg, Type::Number) => Type::Number.into(),
            (UnaryOp::BitNot, Type::Number) => Type::Number.into(),
            (UnaryOp::PreIncr, Type::Number) => Type::Number.into(),
            (UnaryOp::PostIncr, Type::Number) => Type::Number.into(),
            (UnaryOp::PreDecr, Type::Number) => Type::Number.into(),
            (UnaryOp::PostDecr, Type::Number) => Type::Number.into(),
            (_, Type::Any) => Analysis::empty(),
            _ => {
                //self.error(format!("visit_unary: don't know how to {:?} {:?}", op, rhs.ty));
                Analysis::empty()
            }
        }
    }

    fn visit_binary(&mut self, lhs: Analysis<'o>, rhs: Analysis<'o>, op: BinaryOp) -> Analysis<'o> {
        //println!("visit_binary: don't know anything about {}", op);
        Analysis::empty()
    }

    fn visit_call(&mut self, src: TypeRef<'o>, proc: ProcRef, args: &'o [Expression], is_exact: bool) -> Analysis<'o> {
        // identify and register kwargs used
        let mut any_kwargs_yet = false;
        for arg in args {
            let mut argument_value = arg;
            let mut this_kwarg = false;
            if let Expression::AssignOp { op: AssignOp::Assign, lhs, rhs } = arg {
                match lhs.as_term() {
                    Some(Term::Ident(name)) |
                    Some(Term::String(name)) => {
                        // Don't visit_expression the kwarg key.
                        any_kwargs_yet = true;
                        this_kwarg = true;
                        argument_value = rhs;

                        // Check that that kwarg actually exists.
                        if !proc.parameters.iter().any(|p| p.name == *name) {
                            // Search for a child proc that does have this keyword argument.
                            let mut error = DMError::new(self.proc_ref.location,
                                format!("bad keyword argument {:?} to {}", name, proc));
                            proc.recurse_children(&mut |child_proc| {
                                if child_proc.ty() == proc.ty() { return }
                                if child_proc.parameters.iter().any(|p| p.name == *name) {
                                    error.add_note(child_proc.location, format!("an override has this parameter: {}", child_proc));
                                }
                            });
                            error.register(self.context);
                        } else if !is_exact {
                            // If it does, mark it as "used".
                            // Format with src/proc/foo here, rather than the
                            // type the proc actually appears on, so that
                            // calling /datum/foo() on a /datum/A won't
                            // complain about /datum/B/foo().
                            self.env.used_kwargs.entry(format!("{}/proc/{}", src, proc.name()))
                                .or_insert_with(|| KwargInfo {
                                    location: proc.location,
                                    .. Default::default()
                                })
                                .called_at
                                // TODO: use a more accurate location
                                .entry(name.clone())
                                .and_modify(|ca| ca.others += 1)
                                .or_insert(CalledAt {
                                    location: self.proc_ref.location,
                                    others: 0,
                                });
                        }
                    }
                    _ => {}
                }
            }

            if any_kwargs_yet && !this_kwarg && !(proc.ty().is_root() && proc.name() == "animate") {
                // TODO: don't hardcode the animate() exception
                self.error(format!("proc called with non-kwargs after kwargs: {}()", proc.name()));
            }

            self.visit_expression(argument_value, None);
        }

        Analysis::empty()
    }

    fn static_type(&mut self, of: &Vec<String>) -> Analysis<'o> {
        if of.is_empty() {
            Analysis::empty()
        } else if of[0] == "list" {
            let mut analysis = Analysis::from_static_type(self.objtree.expect("/list"));
            analysis.ty = Type::List(self.objtree.type_by_path(&of[1..]));
            analysis
        } else if let Some(ty) = self.objtree.type_by_path(of) {
            Analysis::from_static_type(ty)
        } else {
            self.error(format!("undefined type: {}", FormatTreePath(of)));
            Analysis::empty()
        }
    }
}

fn main() {
    const PRINT_SEVERITY: dm::Severity = dm::Severity::Info;

    let mut context = Context::default();
    context.set_print_severity(Some(PRINT_SEVERITY));
    let dme = dm::detect_environment_default()
        .expect("error detecting .dme")
        .expect("no .dme found");
    println!("============================================================");
    println!("Parsing {}...\n", dme.display());
    let pp = dm::preprocessor::Preprocessor::new(&context, dme)
        .expect("i/o error opening .dme");
    let indents = dm::indents::IndentProcessor::new(&context, pp);
    let mut parser = dm::parser::Parser::new(&context, indents);
    parser.enable_procs();
    let tree = parser.parse_object_tree();

    let mut present = 0;
    let mut invalid = 0;
    let mut builtin = 0;

    let mut env = Env::default();

    // First pass: analyze all proc bodies
    println!("============================================================");
    println!("Analyzing proc bodies...\n");
    tree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            match proc.code {
                Code::Present(ref code) => {
                    present += 1;
                    ProcAnalyzer::new(&mut env, &context, &tree, proc).run(code);
                }
                Code::Invalid(_) => invalid += 1,
                Code::Builtin => builtin += 1,
                Code::Disabled => panic!("proc parsing was enabled, but also disabled. this is a bug"),
            }
        }
    });

    println!("Procs analyzed: {}. Errored: {}. Builtins: {}.\n", present, invalid, builtin);

    // Second pass: warn about procs which are missing kwargs their parents have
    println!("============================================================");
    println!("Analyzing proc override validity...\n");
    tree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            env.check_kwargs(&context, proc);
        }
    });
    env.finish_check_kwargs(&context);

    println!("============================================================");
    let errors = context.errors().iter().filter(|each| each.severity() <= PRINT_SEVERITY).count();
    println!("Found {} diagnostics", errors);
    std::process::exit(if errors > 0 { 1 } else { 0 });
}
