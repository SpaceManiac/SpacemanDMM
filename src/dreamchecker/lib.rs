//! DreamChecker, a robust static analysis and typechecking engine for
//! DreamMaker.
#![allow(dead_code, unused_variables)]

extern crate dreammaker as dm;
use dm::{Context, DMError, Location, Severity};
use dm::objtree::{ObjectTree, TypeRef, ProcRef};
use dm::constants::{Constant, ConstFn};
use dm::ast::*;

use std::collections::{BTreeMap, HashMap, HashSet};

// ----------------------------------------------------------------------------
// Helper structures

#[derive(Debug, Clone)]
enum StaticType<'o> {
    None,
    Type(TypeRef<'o>),
    List {
        list: TypeRef<'o>,
        keys: Box<StaticType<'o>>,
    },
}

impl<'o> StaticType<'o> {
    fn basic_type(&self) -> Option<TypeRef<'o>> {
        match *self {
            StaticType::None => None,
            StaticType::Type(t) => Some(t),
            StaticType::List { list, .. } => Some(list),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum Assumption<'o> {
    Truthy(bool),
    IsNull(bool),
    IsText(bool),
    IsNum(bool),
    IsType(bool, TypeRef<'o>),
    IsPath(bool, TypeRef<'o>),
}

impl<'o> Assumption<'o> {
    fn oneway_conflict(&self, other: &Assumption) -> bool {
        use Assumption::*;
        match (self, other) {
            // trivial conflicts
            (Truthy(a), Truthy(b)) |
            (IsNull(a), IsNull(b)) |
            (IsText(a), IsText(b)) |
            (IsNum(a), IsNum(b)) => a != b,
            // null is always false
            (Truthy(true), IsNull(true)) => true,
            // can only be one of null, text, num
            (IsNum(true), IsText(true)) => true,
            (IsNum(true), IsNull(true)) => true,
            (IsText(true), IsNull(true)) => true,
            // no conflict after all
            _ => false
        }
    }
}

#[derive(Clone, Debug, Default)]
struct AssumptionSet<'o> {
    set: HashSet<Assumption<'o>>,
}

macro_rules! assumption_set {
    ($($v:expr),* $(,)*) => {
        AssumptionSet {
            set: vec![$($v),*].into_iter().collect(),
        }
    }
}

impl<'o> AssumptionSet<'o> {
    fn from_constant(objtree: &'o ObjectTree, constant: &Constant, type_hint: Option<TypeRef<'o>>) -> AssumptionSet<'o> {
        match constant {
            Constant::Null(_) => assumption_set![Assumption::IsNull(true), Assumption::Truthy(false)],
            Constant::String(val) => assumption_set![Assumption::IsText(true), Assumption::Truthy(!val.is_empty())],
            Constant::Resource(_) => assumption_set![Assumption::Truthy(true)],
            Constant::Int(val) => assumption_set![Assumption::IsNum(true), Assumption::Truthy(*val != 0)],
            Constant::Float(val) => assumption_set![Assumption::IsNum(true), Assumption::Truthy(*val != 0.0)],
            Constant::List(_) => AssumptionSet::from_valid_instance(objtree.expect("/list")),
            Constant::Call(func, _) => match func {
                ConstFn::Icon => AssumptionSet::from_valid_instance(objtree.expect("/icon")),
                ConstFn::Matrix => AssumptionSet::from_valid_instance(objtree.expect("/matrix")),
                ConstFn::Newlist => AssumptionSet::from_valid_instance(objtree.expect("/list")),
                ConstFn::Sound => AssumptionSet::from_valid_instance(objtree.expect("/sound")),
                ConstFn::Filter => AssumptionSet::default(),
            },
            Constant::New { type_, args: _ } => {
                if let Some(pop) = type_.as_ref() {
                    if let Some(ty) = objtree.type_by_path(&pop.path) {
                        AssumptionSet::from_valid_instance(ty)
                    } else {
                        AssumptionSet::default()
                    }
                } else if let Some(hint) = type_hint {
                    AssumptionSet::from_valid_instance(hint)
                } else {
                    AssumptionSet::default()
                }
            },
            Constant::Prefab(pop) => {
                if let Some(ty) = objtree.type_by_path(&pop.path) {
                    assumption_set![Assumption::Truthy(false), Assumption::IsPath(true, ty)]
                } else {
                    AssumptionSet::default()
                }
            },
        }
    }

    fn from_valid_instance(ty: TypeRef<'o>) -> AssumptionSet<'o> {
        assumption_set![Assumption::Truthy(true), Assumption::IsNull(false), Assumption::IsType(true, ty)]
    }

    fn conflicts_with(&self, new: &Assumption) -> Option<&Assumption> {
        for each in self.set.iter() {
            if each.oneway_conflict(new) || new.oneway_conflict(each) {
                return Some(each);
            }
        }
        None
    }
}

/// Single atomic type.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
enum Type<'o> {
    Any,  // TODO: this doesn't belong here?

    // Primitives -------------------------------------------------------------
    Null,  // Only thing that isnull().
    EmptyString,
    String,
    Zero,
    Number,
    Resource,  // Only thing that isfile(), file() returns this.

    // Types ------------------------------------------------------------------
    Instance(TypeRef<'o>),  // Only thing that istype() what it is;
                            // isloc/isarea/ismob/isobj/isturf are this.
    Typepath(TypeRef<'o>),  // Only thing that ispath() what it is.

    // Special Types ----------------------------------------------------------
    // These are not nameable and have no isX() proc, but are "something".
    Global,
    AbstractFilter,
    ProcPath,
}

impl<'o> Type<'o> {
    fn from_constant(objtree: &'o ObjectTree, constant: &Constant) -> Type<'o> {
        match constant {
            Constant::Null(_) => Type::Null,
            Constant::String(_) => Type::String,
            Constant::Resource(_) => Type::Resource,
            Constant::Int(_) => Type::Number,
            Constant::Float(_) => Type::Number,
            Constant::List(_) => Type::Instance(objtree.expect("/list")),
            Constant::Call(func, _) => match func {
                ConstFn::Icon => Type::Instance(objtree.expect("/icon")),
                ConstFn::Matrix => Type::Instance(objtree.expect("/matrix")),
                ConstFn::Newlist => Type::Instance(objtree.expect("/list")),
                ConstFn::Sound => Type::Instance(objtree.expect("/sound")),
                ConstFn::Filter => Type::AbstractFilter,
            },
            Constant::New { type_, args: _ } => {
                if let Some(pop) = type_.as_ref() {
                    if let Some(ty) = objtree.type_by_path(&pop.path) {
                        Type::Instance(ty)
                    } else {
                        Type::Any
                    }
                } else {
                    // TODO: receive type hint here
                    Type::Any
                }
            },
            Constant::Prefab(pop) => {
                if let Some(ty) = objtree.type_by_path(&pop.path) {
                    Type::Instance(ty)
                } else {
                    Type::Any
                }
            },
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Type::Null => false,
            Type::EmptyString => false,
            Type::Zero => false,
            _ => true,
        }
    }

    fn isnull(&self) -> bool {
        match self {
            Type::Null => true,
            _ => false,
        }
    }

    fn istext(&self) -> bool {
        match self {
            Type::String => true,
            Type::EmptyString => true,
            _ => false,
        }
    }

    fn isnum(&self) -> bool {
        match self {
            Type::Number => true,
            Type::Zero => true,
            _ => false,
        }
    }

    fn isfile(&self) -> bool {
        match self {
            Type::Resource => true,
            _ => false,
        }
    }

    fn isloc(&self) -> bool {
        match self {
            Type::Instance(ty) => ty.is_subtype_of(&ty.tree().expect("/atom")),
            _ => false,
        }
    }

    fn isarea(&self) -> bool {
        match self {
            Type::Instance(ty) => ty.is_subtype_of(&ty.tree().expect("/area")),
            _ => false,
        }
    }

    fn ismob(&self) -> bool {
        match self {
            Type::Instance(ty) => ty.is_subtype_of(&ty.tree().expect("/mob")),
            _ => false,
        }
    }

    fn isobj(&self) -> bool {
        // Tricky: actually any /atom/movable which is not /mob.
        match self {
            Type::Instance(ty) => {
                ty.is_subtype_of(&ty.tree().expect("/atom/movable"))
                    && !ty.is_subtype_of(&ty.tree().expect("/mob"))
            },
            _ => false,
        }
    }

    fn isturf(&self) -> bool {
        // Tricky: actually any /atom which is not /area or /atom/movable.
        match self {
            Type::Instance(ty) => {
                ty.is_subtype_of(&ty.tree().expect("/atom"))
                    && !ty.is_subtype_of(&ty.tree().expect("/area"))
                    && !ty.is_subtype_of(&ty.tree().expect("/atom/movable"))
            },
            _ => false,
        }
    }

    fn istype(&self, other: TypeRef<'o>) -> bool {
        // The "tricky" bits above do not apply here.
        match self {
            Type::Instance(ty) => ty.is_subtype_of(&other),
            _ => false,
        }
    }

    fn ispath(&self, other: TypeRef<'o>) -> bool {
        match self {
            Type::Typepath(ty) => ty.is_subtype_of(&other),
            _ => false,
        }
    }
}

/// An 'atom' in the type analysis. A type/set of possible types, as well as a
/// known constant value if available.
#[derive(Debug, Clone)]
struct Analysis<'o> {
    static_ty: StaticType<'o>,
    aset: AssumptionSet<'o>,
    value: Option<Constant>,
}

impl<'o> Analysis<'o> {
    fn empty() -> Analysis<'o> {
        Type::Any.into()
    }

    fn null() -> Analysis<'o> {
        Analysis {
            static_ty: StaticType::None,
            aset: assumption_set![Assumption::IsNull(true)],
            value: Some(Constant::Null(None)),
        }
    }

    fn from_static_type(ty: TypeRef<'o>) -> Analysis<'o> {
        Analysis::from(StaticType::Type(ty))
    }

    fn from_value(objtree: &'o ObjectTree, value: Constant, type_hint: Option<TypeRef<'o>>) -> Analysis<'o> {
        Analysis {
            static_ty: StaticType::None,
            aset: AssumptionSet::from_constant(objtree, &value, type_hint),
            value: Some(value),
        }
    }
}

/// Build an analysis with a single type as its element.
impl<'o> From<Type<'o>> for Analysis<'o> {
    fn from(ty: Type<'o>) -> Analysis<'o> {
        unimplemented!()
        /*Analysis {
            static_ty: StaticType::None,
            ty: Some(ty).into_iter().collect(),
            value: None,
        }*/
    }
}

/// Optimistic: assumes static type is true and non-null.
impl<'o> From<StaticType<'o>> for Analysis<'o> {
    fn from(static_ty: StaticType<'o>) -> Analysis<'o> {
        let mut aset = AssumptionSet::default();
        if let Some(ty) = static_ty.basic_type() {
            aset.set.insert(Assumption::IsType(true, ty));
        }

        Analysis {
            aset,
            static_ty: static_ty,
            value: None,
        }
    }
}

// ----------------------------------------------------------------------------
// Shortcut entry point

pub fn run(context: &Context, objtree: &ObjectTree) {
    let mut analyzer = AnalyzeObjectTree::new(context, objtree);

    objtree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            if let dm::objtree::Code::Present(ref code) = proc.get().code {
                analyzer.gather_settings(proc, code);
            }
        }
    });

    objtree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            if let dm::objtree::Code::Present(ref code) = proc.get().code {
                analyzer.check_proc(proc, code);
            }
        }
    });

    objtree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            analyzer.check_kwargs(proc);
        }
    });

    analyzer.finish_check_kwargs();
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

pub struct AnalyzeObjectTree<'o> {
    context: &'o Context,
    objtree: &'o ObjectTree,

    return_type: HashMap<ProcRef<'o>, StaticType<'o>>,
    // Debug(ProcRef) -> KwargInfo
    used_kwargs: BTreeMap<String, KwargInfo>,
}

impl<'o> AnalyzeObjectTree<'o> {
    pub fn new(context: &'o Context, objtree: &'o ObjectTree) -> Self {
        AnalyzeObjectTree {
            context,
            objtree,
            return_type: Default::default(),
            used_kwargs: Default::default(),
        }
    }

    pub fn check_proc(&mut self, proc: ProcRef<'o>, code: &'o [Spanned<Statement>]) {
        AnalyzeProc::new(self, self.context, self.objtree, proc).run(code)
    }

    pub fn gather_settings(&mut self, proc: ProcRef<'o>, code: &'o [Spanned<Statement>]) {
        for statement in code.iter() {
            if let Statement::Setting { ref name, ref value, .. } = statement.elem {
                if name == "spacemandmm_return_type" {
                    if let Some(Term::Prefab(fab)) = value.as_term() {
                        let bits: Vec<_> = fab.path.iter().map(|(_, name)| name.to_owned()).collect();
                        let ty = self.static_type(statement.location, &bits);
                        self.return_type.insert(proc, ty);
                    }
                }
            } else {
                break;
            }
        }
    }

    pub fn check_kwargs(&mut self, proc: ProcRef) {
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

    pub fn finish_check_kwargs(&self) {
        for (base_procname, kwarg_info) in self.used_kwargs.iter() {
            if kwarg_info.bad_overrides_at.is_empty() {
                continue
            }

            // List out the child procs that are missing overrides.
            let msg = match kwarg_info.bad_overrides_at.len() {
                1 => format!("an override of {} is missing keyword args", base_procname),
                len => format!("{} overrides of {} are missing keyword args", len, base_procname),
            };
            let mut error = error(kwarg_info.location, msg);
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

            error.register(self.context);
        }
    }

    fn static_type(&mut self, location: Location, mut of: &[String]) -> StaticType<'o> {
        while !of.is_empty() && ["static", "global", "const", "tmp"].contains(&&*of[0]) {
            of = &of[1..];
        }

        if of.is_empty() {
            StaticType::None
        } else if of[0] == "list" {
            let keys = self.static_type(location, &of[1..]);
            StaticType::List {
                list: self.objtree.expect("/list"),
                keys: Box::new(keys),
            }
        } else if let Some(ty) = self.objtree.type_by_path(of) {
            StaticType::Type(ty)
        } else {
            error(location, format!("undefined type: {}", FormatTreePath(of)))
                .register(self.context);
            StaticType::None
        }
    }
}

fn error<S: Into<String>>(location: Location, desc: S) -> DMError {
    DMError::new(location, desc).with_component(dm::Component::DreamChecker)
}

// ----------------------------------------------------------------------------
// Procedure analyzer

struct AnalyzeProc<'o, 's> {
    env: &'s mut AnalyzeObjectTree<'o>,
    context: &'o Context,
    objtree: &'o ObjectTree,
    ty: TypeRef<'o>,
    proc_ref: ProcRef<'o>,
    local_vars: HashMap<String, Analysis<'o>>,
}

impl<'o, 's> AnalyzeProc<'o, 's> {
    fn new(env: &'s mut AnalyzeObjectTree<'o>, context: &'o Context, objtree: &'o ObjectTree, proc_ref: ProcRef<'o>) -> Self {
        let ty = proc_ref.ty();

        let mut local_vars = HashMap::new();
        local_vars.insert(".".to_owned(), Analysis::empty());
        local_vars.insert("args".to_owned(), Analysis::from_static_type(objtree.expect("/list")));
        local_vars.insert("usr".to_owned(), Analysis::from_static_type(objtree.expect("/mob")));
        if !ty.is_root() {
            local_vars.insert("src".to_owned(), Analysis::from_static_type(ty));
        }
        local_vars.insert("global".to_owned(), Analysis {
            static_ty: StaticType::Type(objtree.root()),
            aset: assumption_set![Assumption::IsNull(false)],
            value: None,
        });

        AnalyzeProc {
            env,
            context,
            objtree,
            ty,
            proc_ref,
            local_vars,
        }
    }

    pub fn run(&mut self, block: &'o [Spanned<Statement>]) {
        for param in self.proc_ref.get().parameters.iter() {
            let analysis = self.static_type(param.location, &param.var_type.type_path);
            self.local_vars.insert(param.name.to_owned(), analysis);
        }
        self.visit_block(block);
    }

    fn visit_block(&mut self, block: &'o [Spanned<Statement>]) {
        for stmt in block.iter() {
            self.visit_statement(stmt.location, &stmt.elem);
        }
    }

    fn visit_statement(&mut self, location: Location, statement: &'o Statement) {
        match statement {
            Statement::Expr(expr) => { self.visit_expression(location, expr, None); },
            Statement::Return(Some(expr)) => {
                // TODO: factor in the previous return type if there was one
                let return_type = self.visit_expression(location, expr, None);
                self.local_vars.insert(".".to_owned(), return_type);
                // TODO: break out of the analysis for this branch?
            },
            Statement::Return(None) => {},
            Statement::Throw(expr) => { self.visit_expression(location, expr, None); },
            Statement::While { condition, block } => {
                self.visit_expression(location, condition, None);
                self.visit_block(block);
            },
            Statement::DoWhile { block, condition } => {
                self.visit_block(block);
                self.visit_expression(location, condition, None);
            },
            Statement::If { arms, else_arm } => {
                for &(ref condition, ref block) in arms.iter() {
                    self.visit_expression(location, condition, None);
                    self.visit_block(block);
                }
                if let Some(else_arm) = else_arm {
                    self.visit_block(else_arm);
                }
            },
            Statement::ForLoop { init, test, inc, block } => {
                if let Some(init) = init {
                    self.visit_statement(location, init);
                }
                if let Some(test) = test {
                    self.visit_expression(location, test, None);
                }
                if let Some(inc) = inc {
                    self.visit_statement(location, inc);
                }
                self.visit_block(block);
            },
            Statement::ForList { in_list, block, var_type, name, .. } => {
                if let Some(in_list) = in_list {
                    self.visit_expression(location, in_list, None);
                }
                if let Some(var_type) = var_type {
                    self.visit_var(location, var_type, name, None);
                }
                self.visit_block(block);
            },
            Statement::ForRange { var_type, name, start, end, step, block } => {
                self.visit_expression(location, end, None);
                if let Some(step) = step {
                    self.visit_expression(location, step, None);
                }
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
            Statement::Spawn { delay, block } => {
                if let Some(delay) = delay {
                    self.visit_expression(location, delay, None);
                }
                self.visit_block(block);
            },
            Statement::Switch { input, cases, default } => {
                self.visit_expression(location, input, None);
                for &(ref case, ref block) in cases.iter() {
                    for case_part in case.iter() {
                        match case_part {
                            dm::ast::Case::Exact(expr) => { self.visit_expression(location, expr, None); },
                            dm::ast::Case::Range(start, end) => {
                                self.visit_expression(location, start, None);
                                self.visit_expression(location, end, None);
                            }
                        }
                    }
                    self.visit_block(block);
                }
                if let Some(default) = default {
                    self.visit_block(default);
                }
            },
            Statement::TryCatch { try_block, catch_params, catch_block } => {
                self.visit_block(try_block);
                if catch_params.len() > 1 {
                    error(location, format!("Expected 0 or 1 catch parameters, got {}", catch_params.len()))
                        .set_severity(Severity::Warning)
                        .register(self.context);
                }
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
            Statement::Del(expr) => { self.visit_expression(location, expr, None); },
        }
    }

    fn visit_var_stmt(&mut self, location: Location, var: &'o VarStatement) {
        self.visit_var(location, &var.var_type, &var.name, var.value.as_ref())
    }

    fn visit_var(&mut self, location: Location, var_type: &VarType, name: &str, value: Option<&'o Expression>) {
        // Calculate type hint
        let static_type = self.env.static_type(location, &var_type.type_path);

        // Visit the expression if it's there
        let mut analysis = match value {
            Some(ref expr) => self.visit_expression(location, expr, static_type.basic_type()),
            None => Analysis::null(),
        };
        analysis.static_ty = static_type;

        // Save var to locals
        self.local_vars.insert(name.to_owned(), analysis);
    }

    fn visit_expression(&mut self, location: Location, expression: &'o Expression, type_hint: Option<TypeRef<'o>>) -> Analysis<'o> {
        match expression {
            Expression::Base { unary, term, follow } => {
                let base_type_hint = if follow.is_empty() && unary.is_empty() {
                    type_hint
                } else {
                    None
                };
                let mut ty = self.visit_term(term.location, &term.elem, base_type_hint);
                for each in follow.iter() {
                    ty = self.visit_follow(each.location, ty, &each.elem);
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
                let lty = self.visit_expression(location, lhs, type_hint);
                let rty = self.visit_expression(location, rhs, type_hint);
                self.visit_binary(lty, rty, BinaryOp::Or)
            },
            Expression::BinaryOp { op, lhs, rhs } => {
                let lty = self.visit_expression(location, lhs, None);
                let rty = self.visit_expression(location, rhs, None);
                self.visit_binary(lty, rty, *op)
            },
            Expression::AssignOp { lhs, rhs, .. } => {
                let lhs = self.visit_expression(location, lhs, None);
                self.visit_expression(location, rhs, lhs.static_ty.basic_type())
            },
            Expression::TernaryOp { cond, if_, else_ } => {
                // TODO: be sensible
                self.visit_expression(location, cond, None);
                let ty = self.visit_expression(location, if_, type_hint);
                self.visit_expression(location, else_, type_hint);
                ty
            }
        }
    }

    fn visit_term(&mut self, location: Location, term: &'o Term, type_hint: Option<TypeRef<'o>>) -> Analysis<'o> {
        match term {
            Term::Null => Analysis::null(),
            Term::Int(number) => Analysis::from_value(self.objtree, Constant::from(*number), type_hint),
            Term::Float(number) => Analysis::from_value(self.objtree, Constant::from(*number), type_hint),
            Term::String(text) => Analysis::from_value(self.objtree, Constant::String(text.to_owned()), type_hint),
            Term::Resource(text) => Analysis::from_value(self.objtree, Constant::Resource(text.to_owned()), type_hint),
            Term::As(_) => Type::Number.into(),

            Term::Ident(unscoped_name) => {
                if let Some(var) = self.local_vars.get(unscoped_name) {
                    return var.clone()
                }
                if let Some(decl) = self.ty.get_var_declaration(unscoped_name) {
                    self.static_type(location, &decl.var_type.type_path)
                } else {
                    error(location, format!("undefined var: {:?}", unscoped_name))
                        .register(self.context);
                    Analysis::empty()
                }
            },

            Term::Expr(expr) => self.visit_expression(location, expr, type_hint),
            Term::Prefab(prefab) => {
                if let Some(nav) = self.ty.navigate_path(&prefab.path) {
                    // TODO: handle proc/verb paths here
                    Type::Typepath(nav.ty()).into()
                } else {
                    error(location, format!("failed to resolve path {}", FormatTypePath(&prefab.path)))
                        .register(self.context);
                    Analysis::empty()
                }
            },
            Term::InterpString(_, parts) => {
                for (ref expr, _) in parts.iter() {
                    if let Some(expr) = expr {
                        self.visit_expression(location, expr, None);
                    }
                }
                Type::String.into()
            },

            Term::Call(unscoped_name, args) => {
                let src = self.ty;
                if let Some(proc) = self.ty.get_proc(unscoped_name) {
                    self.visit_call(location, src, proc, args, false)
                } else {
                    error(location, format!("undefined proc: {:?} on {}", unscoped_name, self.ty))
                        .register(self.context);
                    Analysis::empty()
                }
            },
            Term::SelfCall(args) => {
                let src = self.ty;
                let proc = self.proc_ref;
                // Self calls are exact, and won't ever call an override.
                self.visit_call(location, src, proc, args, true)
            },
            Term::ParentCall(args) => {
                if let Some(proc) = self.proc_ref.parent_proc() {
                    // TODO: if args are empty, call w/ same args
                    let src = self.ty;
                    // Parent calls are exact, and won't ever call an override.
                    self.visit_call(location, src, proc, args, true)
                } else {
                    error(location, format!("proc has no parent: {}", self.proc_ref))
                        .register(self.context);
                    Analysis::empty()
                }
            },

            Term::New { type_, args } => {
                // determine the type being new'd
                let typepath = match type_ {
                    NewType::Implicit => if let Some(hint) = type_hint {
                        Some(hint)
                    } else {
                        error(location, "no type hint available on implicit new()")
                            .register(self.context);
                        None
                    },
                    NewType::Prefab(prefab) => {
                        if let Some(nav) = self.ty.navigate_path(&prefab.path) {
                            // TODO: handle proc/verb paths here
                            Some(nav.ty())
                        } else {
                            error(location, format!("failed to resolve path {}", FormatTypePath(&prefab.path)))
                                .register(self.context);
                            None
                        }
                    },
                    NewType::MiniExpr { .. } => None,  // TODO: evaluate
                };

                // call to the New() method
                if let Some(typepath) = typepath {
                    self.visit_call(
                        location,
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
            Term::List(args) => {
                self.visit_arguments(location, args);
                Type::Instance(self.objtree.expect("/list")).into()
            },
            Term::Input { args, input_type, in_list } => {
                // TODO: deal with in_list
                self.visit_arguments(location, args);
                if let Some(ref expr) = in_list {
                    self.visit_expression(location, expr, None);
                }

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
            Term::Locate { args, in_list } => {
                self.visit_arguments(location, args);
                if let Some(ref expr) = in_list {
                    self.visit_expression(location, expr, None);
                }

                if args.len() == 3 {  // X,Y,Z - it's gotta be a turf
                    Type::Instance(self.objtree.expect("/turf")).into()
                } else {
                    Analysis::empty()
                }
            },
            Term::Pick(choices) => {
                for (weight, choice) in choices.iter() {
                    if let Some(ref weight) = weight {
                        self.visit_expression(location, weight, None);
                    }
                    self.visit_expression(location, choice, None);
                }

                // TODO: common superset of all choices
                Analysis::empty()
            },
            Term::DynamicCall(lhs_args, rhs_args) => {
                self.visit_arguments(location, lhs_args);
                self.visit_arguments(location, rhs_args);
                Analysis::empty()  // TODO
            },
        }
    }

    fn visit_follow(&mut self, location: Location, lhs: Analysis<'o>, rhs: &'o Follow) -> Analysis<'o> {
        match rhs {
            Follow::Field(IndexKind::Colon, _) => Analysis::empty(),
            Follow::Field(IndexKind::SafeColon, _) => Analysis::empty(),
            Follow::Call(IndexKind::Colon, _, args) |
            Follow::Call(IndexKind::SafeColon, _, args) => {
                // No analysis yet, but be sure to visit the arguments
                for arg in args {
                    let mut argument_value = arg;
                    if let Expression::AssignOp { op: AssignOp::Assign, lhs, rhs } = arg {
                        match lhs.as_term() {
                            Some(Term::Ident(name)) |
                            Some(Term::String(name)) => {
                                // Don't visit_expression the kwarg key.
                                argument_value = rhs;
                            },
                            _ => {},
                        }
                    }
                    self.visit_expression(location, argument_value, None);
                }
                Analysis::empty()
            },

            Follow::Index(expr) => {
                self.visit_expression(location, expr, None);
                // TODO: differentiate between L[1] and L[non_numeric_key]
                match lhs.static_ty {
                    StaticType::List { keys, .. } => Analysis::from(*keys),
                    _ => Analysis::empty(),
                }
            },
            Follow::Field(kind, name) => {
                if let Some(ty) = lhs.static_ty.basic_type() {
                    if let Some(decl) = ty.get_var_declaration(name) {
                        self.static_type(location, &decl.var_type.type_path)
                    } else {
                        error(location, format!("undefined field: {:?} on {}", name, ty))
                            .register(self.context);
                        Analysis::empty()
                    }
                } else {
                    error(location, format!("field access requires static type: {:?}", name))
                        .register(self.context);
                    Analysis::empty()
                }
            },
            Follow::Call(kind, name, arguments) => {
                if let Some(ty) = lhs.static_ty.basic_type() {
                    if let Some(proc) = ty.get_proc(name) {
                        self.visit_call(location, ty, proc, arguments, false)
                    } else {
                        error(location, format!("undefined proc: {:?} on {}", name, ty))
                            .register(self.context);
                        Analysis::empty()
                    }
                } else {
                    error(location, format!("proc call require static type: {:?}", name))
                        .register(self.context);
                    Analysis::empty()
                }
            },
        }
    }

    fn visit_unary(&mut self, rhs: Analysis<'o>, op: &UnaryOp) -> Analysis<'o> {
        match op {
            // !x just evaluates the "truthiness" of x and negates it, returning 1 or 0
            UnaryOp::Not => Type::Number.into(),
            /*
            (UnaryOp::Neg, Type::Number) => Type::Number.into(),
            (UnaryOp::BitNot, Type::Number) => Type::Number.into(),
            (UnaryOp::PreIncr, Type::Number) => Type::Number.into(),
            (UnaryOp::PostIncr, Type::Number) => Type::Number.into(),
            (UnaryOp::PreDecr, Type::Number) => Type::Number.into(),
            (UnaryOp::PostDecr, Type::Number) => Type::Number.into(),
            */
            _ => Analysis::empty(),
        }
    }

    fn visit_binary(&mut self, lhs: Analysis<'o>, rhs: Analysis<'o>, op: BinaryOp) -> Analysis<'o> {
        //println!("visit_binary: don't know anything about {}", op);
        Analysis::empty()
    }

    fn visit_call(&mut self, location: Location, src: TypeRef<'o>, proc: ProcRef, args: &'o [Expression], is_exact: bool) -> Analysis<'o> {
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
                            let mut error = error(location,
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
                                    location: location,
                                    others: 0,
                                });
                        }
                    }
                    _ => {}
                }
            }

            if any_kwargs_yet && !this_kwarg && !(proc.ty().is_root() && proc.name() == "animate") {
                // TODO: don't hardcode the animate() exception
                error(location, format!("proc called with non-kwargs after kwargs: {}()", proc.name()))
                    .register(self.context);
            }

            self.visit_expression(location, argument_value, None);
        }

        if let Some(return_type) = self.env.return_type.get(&proc) {
            Analysis::from(return_type.clone())
        } else {
            Analysis::empty()
        }
    }

    fn visit_arguments(&mut self, location: Location, args: &'o [Expression]) {
        for arg in args {
            let mut argument_value = arg;
            if let Expression::AssignOp { op: AssignOp::Assign, lhs, rhs } = arg {
                match lhs.as_term() {
                    Some(Term::Ident(_name)) |
                    Some(Term::String(_name)) => {
                        // Don't visit_expression the kwarg key.
                        argument_value = rhs;
                    }
                    _ => {}
                }
            }

            self.visit_expression(location, argument_value, None);
        }
    }

    fn static_type(&mut self, location: Location, of: &[String]) -> Analysis<'o> {
        Analysis::from(self.env.static_type(location, of))
    }
}
