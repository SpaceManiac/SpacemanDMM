//! DreamChecker, a robust static analysis and typechecking engine for
//! DreamMaker.
#![allow(dead_code, unused_variables)]
#[macro_use] extern crate guard;

extern crate dreammaker as dm;
use dm::{Context, DMError, Location, Severity};
use dm::objtree::{ObjectTree, TypeRef, ProcRef};
use dm::constants::{Constant, ConstFn};
use dm::ast::*;

use std::collections::{BTreeMap, HashMap, HashSet};

mod type_expr;
use type_expr::TypeExpr;

#[doc(hidden)]  // Intended for the tests only.
pub mod test_helpers;

// ----------------------------------------------------------------------------
// Helper structures

#[derive(Debug, PartialEq, Clone)]
pub enum StaticType<'o> {
    None,
    Type(TypeRef<'o>),
    List {
        list: TypeRef<'o>,
        keys: Box<StaticType<'o>>,
    },
}

impl<'o> StaticType<'o> {
    fn is_truthy(&self) -> bool {
        match *self {
            StaticType::None => false,
            _ => true,
        }
    }

    fn basic_type(&self) -> Option<TypeRef<'o>> {
        match *self {
            StaticType::None => None,
            StaticType::Type(t) => Some(t),
            StaticType::List { list, .. } => Some(list),
        }
    }

    fn strip_list(self) -> StaticType<'o> {
        if let StaticType::List { keys, .. } = self {
            *keys
        } else {
            StaticType::None
        }
    }

    fn strip_lists(mut self, n: usize) -> StaticType<'o> {
        for _ in 0..n {
            self = self.strip_list();
        }
        self
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
            // types and paths are truthy
            (IsType(true, _), Truthy(false)) |
            (IsType(true, _), IsNull(true)) |
            (IsPath(true, _), Truthy(false)) |
            (IsPath(true, _), Truthy(true)) => true,
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
            Constant::Flag(_) => assumption_set![Assumption::IsNum(true), Assumption::Truthy(true)],
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
                ConstFn::File => AssumptionSet::default(),
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

/// An 'atom' in the type analysis. A type/set of possible types, as well as a
/// known constant value if available.
#[derive(Debug, Clone)]
pub struct Analysis<'o> {
    static_ty: StaticType<'o>,
    aset: AssumptionSet<'o>,
    value: Option<Constant>,
    fix_hint: Option<(Location, String)>,
}

impl<'o> Analysis<'o> {
    fn empty() -> Analysis<'o> {
        Analysis {
            static_ty: StaticType::None,
            aset: AssumptionSet::default(),
            value: None,
            fix_hint: None,
        }
    }

    fn null() -> Analysis<'o> {
        Analysis {
            static_ty: StaticType::None,
            aset: assumption_set![Assumption::IsNull(true)],
            value: Some(Constant::Null(None)),
            fix_hint: None,
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
            fix_hint: None,
        }
    }

    fn with_fix_hint<S: Into<String>>(mut self, location: Location, desc: S) -> Self {
        if location != Location::default() {
            self.fix_hint = Some((location, desc.into()));
        }
        self
    }
}

trait WithFixHint {
    fn with_fix_hint(self, analysis: &Analysis) -> Self;
}

impl WithFixHint for DMError {
    fn with_fix_hint(mut self, analysis: &Analysis) -> Self {
        if let Some((loc, desc)) = analysis.fix_hint.clone() {
            self.add_note(loc, desc);
        }
        self
    }
}

/// Build an analysis from an assumption set.
impl<'o> From<AssumptionSet<'o>> for Analysis<'o> {
    fn from(aset: AssumptionSet<'o>) -> Analysis<'o> {
        Analysis {
            static_ty: StaticType::None,
            aset,
            value: None,
            fix_hint: None,
        }
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
            fix_hint: None,
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

struct ProcDirective<'o> {
    directive: HashMap<ProcRef<'o>, (bool, Location)>,
    can_be_disabled: bool,
    directive_string: &'static str,
}

impl<'o> ProcDirective<'o> {
    pub fn new(directive_string: &'static str, can_be_disabled: bool) -> ProcDirective<'o> {
        ProcDirective {
            directive: Default::default(),
            directive_string,
            can_be_disabled,
        }
    }

    pub fn insert(&mut self, proc: ProcRef<'o>, enable: bool, location: Location) -> Result<(), DMError> {
        if !enable && !self.can_be_disabled {
            return Err(error(location, format!("{} sets {} false, but it cannot be disabled.", proc, self.directive_string))
                .with_errortype("disabled_directive")
                .set_severity(Severity::Warning))
        }
        if let Some((_, originallocation)) = self.directive.get(&proc) {
            return Err(error(location, format!("{} sets {} twice", proc, self.directive_string))
                .with_note(*originallocation, "first definition here")
                .with_errortype("sets_directive_twice")
                .set_severity(Severity::Warning))
        }
        self.directive.insert(proc, (enable, location));
        Ok(())
    }

    pub fn get(&self, proc: ProcRef<'o>) -> Option<&(bool, Location)> {
        self.directive.get(&proc)
    }

    pub fn get_self_or_parent(&self, proc: ProcRef<'o>) -> Option<(ProcRef<'o>, bool, Location)> {
        let mut next = Some(proc);
        while let Some(current) = next {
            if let Some(&(truthy, location)) = self.get(current) {
                return Some((current, truthy, location))
            }
            next = current.parent_proc();
        }
        None
    }
}

pub fn directive_value_to_truthy(expr: &Expression, location: Location) -> Result<bool, DMError> {
    // Maybe this should be using constant evaluation, but for now accept TRUE and FALSE directly.
    match expr.as_term() {
        Some(Term::Int(0)) => Ok(false),
        Some(Term::Int(1)) => Ok(true),
        Some(Term::Ident(i)) if i == "FALSE" => Ok(false),
        Some(Term::Ident(i)) if i == "TRUE" => Ok(true),
        _ => Err(error(location, format!("invalid value for lint directive {:?}", expr))
        .with_errortype("invalid_lint_directive_value")
        .set_severity(Severity::Warning)),
    }
}

pub struct AnalyzeObjectTree<'o> {
    context: &'o Context,
    objtree: &'o ObjectTree,

    return_type: HashMap<ProcRef<'o>, TypeExpr<'o>>,
    must_call_parent: ProcDirective<'o>,
    must_not_override: ProcDirective<'o>,
    // Debug(ProcRef) -> KwargInfo
    used_kwargs: BTreeMap<String, KwargInfo>,
}

impl<'o> AnalyzeObjectTree<'o> {
    pub fn new(context: &'o Context, objtree: &'o ObjectTree) -> Self {
        let mut return_type = HashMap::default();
        return_type.insert(objtree.root().get_proc("get_step").unwrap(), StaticType::Type(objtree.expect("/turf")).into());

        AnalyzeObjectTree {
            context,
            objtree,
            return_type,
            must_call_parent: ProcDirective::new("SpacemanDMM_should_call_parent", true),
            must_not_override: ProcDirective::new("SpacemanDMM_should_not_override", false),
            used_kwargs: Default::default(),
        }
    }

    pub fn check_proc(&mut self, proc: ProcRef<'o>, code: &'o [Spanned<Statement>]) {
        AnalyzeProc::new(self, self.context, self.objtree, proc).run(code)
    }

    #[inline]
    fn add_directive_or_error(&mut self, proc: ProcRef<'o>, directive: &str, expr: &Expression, location: Location) {
        let procdirective = match directive {
            "SpacemanDMM_should_not_override" => &mut self.must_not_override,
            "SpacemanDMM_should_call_parent" => &mut self.must_call_parent,
            other => {
                error(location, format!("unknown linter setting {:?}", directive))
                    .with_errortype("unknown_linter_setting")
                    .set_severity(Severity::Warning)
                    .register(self.context);
                return
            }
        };
        match directive_value_to_truthy(expr, location) {
            Ok(truthy) => {
                if let Err(error) = procdirective.insert(proc, truthy, location) {
                    self.context.register_error(error);
                }
            },
            Err(error) => self.context.register_error(error),
        }
    }

    pub fn gather_settings(&mut self, proc: ProcRef<'o>, code: &'o [Spanned<Statement>]) {
        for statement in code.iter() {
            if let Statement::Setting { ref name, ref value, .. } = statement.elem {
                if name == "SpacemanDMM_return_type" {
                    if let Some(Term::Prefab(fab)) = value.as_term() {
                        let bits: Vec<_> = fab.path.iter().map(|(_, name)| name.to_owned()).collect();
                        let ty = self.static_type(statement.location, &bits);
                        self.return_type.insert(proc, TypeExpr::from(ty));
                    } else {
                        match TypeExpr::compile(proc, statement.location, value) {
                            Ok(expr) => { self.return_type.insert(proc, expr); },
                            Err(error) => error
                                .with_component(dm::Component::DreamChecker)
                                .register(self.context),
                        }
                    }
                } else if name.starts_with("SpacemanDMM_") {
                    self.add_directive_or_error(proc, &name.as_str(), value, statement.location);
                } else if !KNOWN_SETTING_NAMES.contains(&name.as_str()) {
                    error(statement.location, format!("unknown setting {:?}", name))
                        .set_severity(Severity::Warning)
                        .register(self.context);
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
            let mut error = error(kwarg_info.location, msg)
                .with_errortype("override_missing_keyword_arg");
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

    fn static_type(&mut self, location: Location, of: &[String]) -> StaticType<'o> {
        match static_type(self.objtree, location, of) {
            Ok(s) => s,
            Err(e) => {
                e.register(self.context);
                StaticType::None
            }
        }
    }
}

fn static_type<'o>(objtree: &'o ObjectTree, location: Location, mut of: &[String]) -> Result<StaticType<'o>, DMError> {
    while !of.is_empty() && ["static", "global", "const", "tmp", "SpacemanDMM_final"].contains(&&*of[0]) {
        of = &of[1..];
    }

    if of.is_empty() {
        Ok(StaticType::None)
    } else if of[0] == "list" {
        let keys = static_type(objtree, location, &of[1..])?;
        Ok(StaticType::List {
            list: objtree.expect("/list"),
            keys: Box::new(keys),
        })
    } else if let Some(ty) = objtree.type_by_path(of) {
        Ok(StaticType::Type(ty))
    } else {
        Err(error(location, format!("undefined type: {}", FormatTreePath(of))))
    }
}

fn error<S: Into<String>>(location: Location, desc: S) -> DMError {
    DMError::new(location, desc).with_component(dm::Component::DreamChecker)
}

// ----------------------------------------------------------------------------
// Variable analyzer

pub fn check_var_defs(objtree: &ObjectTree, context: &Context) {
    for (path, _) in objtree.types.iter() {
        guard!(let Some(typeref) = objtree.find(path)
            else { continue });

        for parent in typeref.iter_parent_types() {
            if parent.is_root() {
                break;
            }

            if &parent.path == path {
                continue;
            }

            for (varname, typevar) in typeref.vars.iter() {
                if varname == "vars" {
                    continue;
                }
                if path == "/client" && varname == "parent_type" {
                    continue;
                }

                guard!(let Some(parentvar) = parent.vars.get(varname)
                    else { continue });

                guard!(let Some(decl) = &parentvar.declaration
                    else { continue });

                if let Some(mydecl) = &typevar.declaration {
                    if typevar.value.location.is_builtins() {
                        continue;
                    }
                    DMError::new(mydecl.location, format!("{} redeclares var {:?}", path, varname))
                        .with_note(decl.location, format!("declared on {} here", parent.path))
                        .register(context);
                }

                if decl.var_type.is_final {
                    DMError::new(typevar.value.location, format!("{} overrides final var {:?}", path, varname))
                        .with_errortype("final_var")
                        .with_note(decl.location, format!("declared final on {} here", parent.path))
                        .register(context);
                }
            }
        }
    }
}

// ----------------------------------------------------------------------------
// Procedure analyzer

struct LocalVar<'o> {
    location: Location,
    analysis: Analysis<'o>,
}

impl<'o> From<Analysis<'o>> for LocalVar<'o> {
    fn from(analysis: Analysis<'o>) -> Self {
        LocalVar { location: Location::default(), analysis }
    }
}

struct AnalyzeProc<'o, 's> {
    env: &'s mut AnalyzeObjectTree<'o>,
    context: &'o Context,
    objtree: &'o ObjectTree,
    ty: TypeRef<'o>,
    proc_ref: ProcRef<'o>,
    local_vars: HashMap<String, LocalVar<'o>>,
    calls_parent: bool,
}

impl<'o, 's> AnalyzeProc<'o, 's> {
    fn new(env: &'s mut AnalyzeObjectTree<'o>, context: &'o Context, objtree: &'o ObjectTree, proc_ref: ProcRef<'o>) -> Self {
        let ty = proc_ref.ty();

        let mut local_vars = HashMap::<String, LocalVar>::new();
        local_vars.insert(".".to_owned(), Analysis::empty().into());
        local_vars.insert("args".to_owned(), Analysis::from_static_type(objtree.expect("/list")).into());
        local_vars.insert("usr".to_owned(), Analysis::from_static_type(objtree.expect("/mob")).into());
        if !ty.is_root() {
            local_vars.insert("src".to_owned(), Analysis::from_static_type(ty).into());
        }
        local_vars.insert("global".to_owned(), Analysis {
            static_ty: StaticType::Type(objtree.root()),
            aset: assumption_set![Assumption::IsNull(false)],
            value: None,
            fix_hint: None,
        }.into());

        AnalyzeProc {
            env,
            context,
            objtree,
            ty,
            proc_ref,
            local_vars,
            calls_parent: false,
        }
    }

    pub fn run(&mut self, block: &'o [Spanned<Statement>]) {
        for param in self.proc_ref.get().parameters.iter() {
            let analysis = self.static_type(param.location, &param.var_type.type_path);
            self.local_vars.insert(param.name.to_owned(), LocalVar {
                location: self.proc_ref.location,
                analysis,
            });
        }
        self.visit_block(block);

        if self.proc_ref.parent_proc().is_some() {
            if let Some((proc, must_not, location)) = self.env.must_not_override.get_self_or_parent(self.proc_ref) {
                if must_not && proc != self.proc_ref {
                    error(self.proc_ref.location, format!("proc overrides parent, prohibited by {}", proc))
                        .with_note(location, "prohibited by this must_not_override annotation")
                        .with_errortype("must_not_override")
                        .register(self.context);
                }
            }
            if !self.calls_parent {
                if let Some((proc, must, location)) = self.env.must_call_parent.get_self_or_parent(self.proc_ref) {
                    if must {
                        error(self.proc_ref.location, format!("proc never calls parent, required by {}", proc))
                            .with_note(location, "required by this must_call_parent annotation")
                            .with_errortype("must_call_parent")
                            .register(self.context);
                    }
                }
            }
        }
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
                self.local_vars.get_mut(".").unwrap().analysis = return_type;
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
                for (condition, ref block) in arms.iter() {
                    self.visit_expression(condition.location, &condition.elem, None);
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
        self.local_vars.insert(name.to_owned(), LocalVar { location, analysis });
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
                    ty = self.visit_unary(ty, each, location);
                }
                ty
            },
            Expression::BinaryOp { op: BinaryOp::In, lhs, rhs } => {
                // check for incorrect/ambiguous in statements
                match &**lhs {
                    Expression::Base { unary, term, follow } => {
                        if unary.len() > 0 {
                            error(location, format!("ambiguous `{}` on left side of an `in`", unary[0].name()))
                                .set_severity(Severity::Warning)
                                .with_errortype("ambiguous_in_lhs")
                                .with_note(location, format!("add parentheses to fix: `{}`", unary[0].around("(a in b)")))
                                .with_note(location, format!("add parentheses to disambiguate: `({}) in b`", unary[0].around("a")))
                                .register(self.context);
                        }
                    },
                    Expression::BinaryOp { op, lhs, rhs } => {
                        error(location, format!("ambiguous `{}` on left side of an `in`", op))
                            .set_severity(Severity::Warning)
                            .with_errortype("ambiguous_in_lhs")
                            .with_note(location, format!("add parentheses to fix: `a {} (b in c)`", op))
                            .with_note(location, format!("add parentheses to disambiguate: `(a {} b) in c`", op))
                            .register(self.context);
                    },
                    Expression::AssignOp { op, lhs, rhs } => {
                        error(location, format!("ambiguous `{}` on left side of an `in`", op))
                            .set_severity(Severity::Warning)
                            .with_errortype("ambiguous_in_lhs")
                            .with_note(location, format!("add parentheses to fix: `a {} (b in c)`", op))
                            .with_note(location, format!("add parentheses to disambiguate: `(a {} b) in c`", op))
                            .register(self.context);
                    },
                    Expression::TernaryOp { cond, if_, else_ } => {
                        error(location, format!("ambiguous ternary on left side of an `in`"))
                            .set_severity(Severity::Warning)
                            .with_errortype("ambiguous_in_lhs")
                            .with_note(location, "add parentheses to fix: `a ? b : (c in d)`")
                            .with_note(location, "add parentheses to disambiguate: `(a ? b : c) in d`")
                            .register(self.context);
                    },
                };
                let lty = self.visit_expression(location, lhs, None);
                let rty = self.visit_expression(location, rhs, None);
                self.visit_binary(lty, rty, BinaryOp::In)
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
            Term::Flag(name) => Analysis::from_value(self.objtree, Constant::Flag(name.to_owned()), type_hint),
            Term::Resource(text) => Analysis::from_value(self.objtree, Constant::Resource(text.to_owned()), type_hint),
            Term::As(_) => assumption_set![Assumption::IsNum(true)].into(),

            Term::Ident(unscoped_name) => {
                if let Some(var) = self.local_vars.get(unscoped_name) {
                    return var.analysis.clone()
                        .with_fix_hint(var.location, "add additional type info here")
                }
                if let Some(decl) = self.ty.get_var_declaration(unscoped_name) {
                    self.static_type(location, &decl.var_type.type_path)
                        .with_fix_hint(decl.location, "add additional type info here")
                } else {
                    error(location, format!("undefined var: {:?}", unscoped_name))
                        .register(self.context);
                    Analysis::empty()
                }
            },

            Term::Expr(expr) => self.visit_expression(location, expr, type_hint),
            Term::Prefab(prefab) => {
                if let Some(nav) = self.ty.navigate_path(&prefab.path) {
                    let ty = nav.ty();  // TODO: handle proc/verb paths here
                    let pop = dm::constants::Pop::from(ty.path.split("/").skip(1).map(ToOwned::to_owned).collect::<Vec<_>>());
                    Analysis {
                        static_ty: StaticType::None,
                        aset: assumption_set![Assumption::IsPath(true, nav.ty())].into(),
                        value: Some(Constant::Prefab(pop)),
                        fix_hint: None,
                    }
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
                assumption_set![Assumption::IsText(true)].into()
            },

            Term::Call(unscoped_name, args) => {
                let src = self.ty;
                if let Some(proc) = self.ty.get_proc(unscoped_name) {
                    self.visit_call(location, src, proc, args, false)
                } else if unscoped_name == "SpacemanDMM_unlint" {
                    // Escape hatch for cases like `src` in macros used in
                    // global procs.
                    Analysis::empty()
                } else if unscoped_name == "SpacemanDMM_debug" {
                    eprintln!("SpacemanDMM_debug:");
                    for arg in args {
                        eprintln!("    {:?}", self.visit_expression(location, arg, None));
                    }
                    Analysis::empty()
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
                self.calls_parent = true;
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
                            .with_errortype("no_typehint_implicit_new")
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
                    if let Some(new_proc) = typepath.get_proc("New") {
                        self.visit_call(
                            location,
                            typepath,
                            new_proc,
                            args.as_ref().map_or(&[], |v| &v[..]),
                            // New calls are exact: `new /datum()` will always call
                            // `/datum/New()` and never an override.
                            true);
                    } else if typepath.path != "/list" {
                        error(location, format!("couldn't find {}/proc/New", typepath.path))
                            .register(self.context);
                    }
                    assumption_set![Assumption::IsType(true, typepath)].into()
                } else {
                    Analysis::empty()
                }
            },
            Term::List(args) => {
                self.visit_arguments(location, args);
                assumption_set![Assumption::IsType(true, self.objtree.expect("/list"))].into()
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
                    assumption_set![Assumption::IsType(true, self.objtree.expect("/mob"))].into()
                } else if without_null == InputType::OBJ {
                    assumption_set![Assumption::IsType(true, self.objtree.expect("/obj"))].into()
                } else if without_null == InputType::AREA {
                    assumption_set![Assumption::IsType(true, self.objtree.expect("/area"))].into()
                } else if without_null == InputType::TURF {
                    assumption_set![Assumption::IsType(true, self.objtree.expect("/turf"))].into()
                } else if without_null == InputType::TEXT || without_null == InputType::MESSAGE || without_null == InputType::KEY || without_null == InputType::PASSWORD || without_null == InputType::COLOR || without_null.is_empty() {
                    assumption_set![Assumption::IsText(true)].into()
                } else if without_null == InputType::NUM {
                    assumption_set![Assumption::IsNum(true)].into()
                } else if without_null == InputType::ICON {
                    assumption_set![Assumption::IsType(true, self.objtree.expect("/icon"))].into()
                } else if without_null == InputType::SOUND {
                    assumption_set![Assumption::IsType(true, self.objtree.expect("/sound"))].into()
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
                    assumption_set![Assumption::IsType(true, self.objtree.expect("/turf"))].into()
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
                    StaticType::List { keys, .. } => {
                        let mut res = Analysis::from(*keys);
                        if let Some((loc, _)) = lhs.fix_hint {
                            res.fix_hint = Some((loc, "add a type annotation after /list here".to_owned()))
                        }
                        res
                    },
                    _ => lhs.clone()  // carry through fix_hint
                }
            },
            Follow::Field(kind, name) => {
                if let Some(ty) = lhs.static_ty.basic_type() {
                    if let Some(decl) = ty.get_var_declaration(name) {
                        self.static_type(location, &decl.var_type.type_path)
                            .with_fix_hint(decl.location, "add additional type info here")
                    } else {
                        error(location, format!("undefined field: {:?} on {}", name, ty))
                            .register(self.context);
                        Analysis::empty()
                    }
                } else {
                    error(location, format!("field access requires static type: {:?}", name))
                        .set_severity(Severity::Warning)
                        .with_errortype("field_access_static_type")
                        .with_fix_hint(&lhs)
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
                    error(location, format!("proc call requires static type: {:?}", name))
                        .set_severity(Severity::Warning)
                        .with_errortype("proc_call_static_type")
                        .with_fix_hint(&lhs)
                        .register(self.context);
                    Analysis::empty()
                }
            },
        }
    }

    // checks operatorX overloads on types
    fn check_operator_overload(&mut self, rhs: Analysis<'o>, location: Location, operator: &str) -> Analysis<'o> {
        let typeerror;
        match rhs.static_ty {
            StaticType::None => {
                return Analysis::empty()
            },
            StaticType::Type(typeref) => {
                // Its been overloaded, assume they really know they want to do this
                if let Some(proc) = typeref.get_proc(&format!("operator{}",operator)) {
                    return self.visit_call(location, typeref, proc, &[], true)
                }
                typeerror = typeref.get().pretty_path();
            },
            StaticType::List { list, .. } => {
                typeerror = "list";
            },
        };
        error(location, format!("Attempting {} on a {} which does not overload operator{}", operator, typeerror, operator))
            .with_errortype("no_operator_overload")
            .register(self.context);
        return Analysis::empty()
    }

    fn visit_unary(&mut self, rhs: Analysis<'o>, op: &UnaryOp, location: Location) -> Analysis<'o> {
        match op {
            // !x just evaluates the "truthiness" of x and negates it, returning 1 or 0
            UnaryOp::Not => Analysis::from(assumption_set![Assumption::IsNum(true)]),
            UnaryOp::PreIncr | UnaryOp::PostIncr => self.check_operator_overload(rhs, location, "++"),
            UnaryOp::PreDecr | UnaryOp::PostDecr => self.check_operator_overload(rhs, location, "--"),
            /*
            (UnaryOp::Neg, Type::Number) => Type::Number.into(),
            (UnaryOp::BitNot, Type::Number) => Type::Number.into(),
            */
            _ => Analysis::empty(),
        }
    }

    fn visit_binary(&mut self, lhs: Analysis<'o>, rhs: Analysis<'o>, op: BinaryOp) -> Analysis<'o> {
        //println!("visit_binary: don't know anything about {}", op);
        Analysis::empty()
    }

    fn check_filter_flag(&mut self, term: &'o Term, can_be_zero: bool, location: Location, typevalue: &str, valid_flags: &[&str], flagfieldname: &str) {
        match term {
            Term::Flag(flagname) => {
                if valid_flags.iter().position(|&x| x == flagname).is_none() {
                    error(location, format!("filter(type=\"{}\") called with invalid '{}' flag '{}'", typevalue, flagfieldname, flagname))
                        .register(self.context);
                }
            },
            Term::Int(0) if can_be_zero => {},
            other => {
                error(location, format!("filter(type=\"{}\") called with invalid '{}' value '{:?}'", typevalue, flagfieldname, other))
                    .register(self.context);
            },
        }
    }

    fn visit_call(&mut self, location: Location, src: TypeRef<'o>, proc: ProcRef, args: &'o [Expression], is_exact: bool) -> Analysis<'o> {
        // identify and register kwargs used
        let mut any_kwargs_yet = false;

        let mut param_name_map = HashMap::new();
        let mut param_idx_map = HashMap::new();
        let mut param_expr_map = HashMap::new();
        let mut param_idx = 0;

        for arg in args {
            let mut argument_value = arg;
            let mut this_kwarg = None;
            if let Expression::AssignOp { op: AssignOp::Assign, lhs, rhs } = arg {
                match lhs.as_term() {
                    Some(Term::Ident(name)) |
                    Some(Term::String(name)) => {
                        // Don't visit_expression the kwarg key.
                        any_kwargs_yet = true;
                        this_kwarg = Some(name);
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

            if any_kwargs_yet && this_kwarg.is_none() && !(proc.ty().is_root() && proc.name() == "animate") {
                // TODO: don't hardcode the animate() exception
                error(location, format!("proc called with non-kwargs after kwargs: {}()", proc.name()))
                    .register(self.context);
            }
            //println!("{:#?}", argument_value);
            let analysis = self.visit_expression(location, argument_value, None);
            if let Some(kw) = this_kwarg {
                param_expr_map.insert(kw.as_str(), argument_value);
                param_name_map.insert(kw.as_str(), analysis);
            } else {
                param_idx_map.insert(param_idx, analysis);
                param_idx += 1;
            }
        }

        // filter call checking
        // TODO: check flags for valid values
        //  eg "wave" type "flags" param only works with WAVE_SIDEWAYS, WAVE_BOUND
        // also some filters have limits for their numerical params
        //  eg "rays" type "threshold" param defaults to 0.5, can be 0 to 1
        if proc.ty().is_root() && proc.name() == "filter" {
            //println!("{:#?}", param_name_map);
            guard!(let Some(typename) = param_name_map.get("type") else {
                error(location, "filter() called without mandatory keyword parameter 'type'")
                    .register(self.context);
                return Analysis::empty()
            });
            guard!(let Some(Constant::String(typevalue)) = &typename.value else {
                error(location, format!("filter() called with non-string type keyword parameter value '{:?}'", typename.value))
                    .register(self.context);
                return Analysis::empty()
            });
            guard!(let Some(arglist) = VALID_FILTER_TYPES.get(typevalue.as_str()) else {
                error(location, format!("filter() called with invalid type keyword parameter value '{}'", typevalue))
                    .register(self.context);
                return Analysis::empty()
            });
            for arg in param_name_map.keys() {
                if *arg != "type" && arglist.iter().position(|&x| x == *arg).is_none() {
                    error(location, format!("filter(type=\"{}\") called with invalid keyword parameter '{}'", typevalue, arg))
                        // luckily lummox has made the anchor url match the type= value for each filter
                        .with_note(location, format!("See: http://www.byond.com/docs/ref/#/{{notes}}/filters/{} for the permitted arguments", typevalue))
                        .register(self.context);
                }
            }
            if let Some((flagfieldname, exclusive, can_be_zero, valid_flags)) = VALID_FILTER_FLAGS.get(typevalue.as_str()) {
                if let Some(flagsvalue) = param_expr_map.get(flagfieldname) {
                    match flagsvalue {
                        Expression::BinaryOp{ op: BinaryOp::BitOr, lhs, rhs } => {
                            if *exclusive {
                                error(location, format!("filter(type=\"{}\") '{}' parameter must have one value, found bitwise OR", typevalue, flagfieldname))
                                    .register(self.context);
                                return Analysis::empty()
                            }
                            guard!(let Some(lhsterm) = lhs.as_term() else {
                                error(location, "filter() flag fields cannot have more than two bitwise OR'd flags")
                                    .register(self.context);
                                return Analysis::empty()
                            });
                            guard!(let Some(rhsterm) = rhs.as_term() else {
                                error(location, "filter() flag fields cannot have more than two bitwise OR'd flags")
                                    .register(self.context);
                                return Analysis::empty()
                            });
                            self.check_filter_flag(lhsterm, *can_be_zero, location, typevalue, valid_flags, flagfieldname);
                            self.check_filter_flag(rhsterm, *can_be_zero, location, typevalue, valid_flags, flagfieldname);
                        },
                        Expression::Base{ unary, term, follow: _ } => {
                            if unary.len() > 0 {
                                error(location, "filter() flag fields cannot have unary ops")
                                    .register(self.context);
                            }
                            self.check_filter_flag(&term.elem, *can_be_zero, location, typevalue, valid_flags, flagfieldname);
                        },
                        _ => {
                            error(location, format!("filter(type=\"{}\"), extremely invalid value passed to '{}' field", typevalue, flagfieldname))
                                .register(self.context);
                        }
                    }
                }
            }
        }

        if let Some(return_type) = self.env.return_type.get(&proc) {
            let ec = type_expr::TypeExprContext {
                objtree: self.objtree,
                param_name_map,
                param_idx_map,
            };
            match return_type.evaluate(location, &ec) {
                Ok(st) => {
                    let hint = format!("return type evaluated to {:?}", st);
                    Analysis::from(st).with_fix_hint(location, hint)
                },
                Err(err) => {
                    err.with_component(dm::Component::DreamChecker)
                        .register(self.context);
                    Analysis::empty()
                }
            }
        } else {
            Analysis::empty()
                .with_fix_hint(proc.location, format!("add a return type annotation to {}", proc))
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
