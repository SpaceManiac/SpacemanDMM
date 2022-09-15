//! DreamChecker, a robust static analysis and typechecking engine for
//! DreamMaker.
#![allow(dead_code, unused_variables)]
#[macro_use]
extern crate guard;

extern crate dreammaker as dm;
use dm::ast::*;
use dm::constants::{ConstFn, Constant};
use dm::objtree::{ObjectTree, ProcRef, TypeRef};
use dm::{Context, DMError, Location, Severity};

use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};

use ahash::RandomState;

mod type_expr;
use type_expr::TypeExpr;
mod switch_rand_range;
use switch_rand_range::check_switch_rand_range;

#[doc(hidden)] // Intended for the tests only.
pub mod test_helpers;

// ----------------------------------------------------------------------------
// Helper structures

/// Analysis result for checking type safety
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
        !matches!(*self, StaticType::None)
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

    fn plain_list(tree: &'o ObjectTree) -> StaticType<'o> {
        StaticType::List {
            list: tree.expect("/list"),
            keys: Box::new(StaticType::None),
        }
    }

    fn list_of_type(tree: &'o ObjectTree, of: &str) -> StaticType<'o> {
        StaticType::List {
            list: tree.expect("/list"),
            keys: Box::new(StaticType::Type(tree.expect(of))),
        }
    }

    fn is_list(&self) -> bool {
        match *self {
            StaticType::None => false,
            StaticType::Type(ty) => ty.path == "/list",
            StaticType::List { .. } => true,
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
    IsTypeVar(bool, TypeRef<'o>),
}

impl<'o> Assumption<'o> {
    fn oneway_conflict(&self, other: &Assumption) -> bool {
        use Assumption::*;
        match (self, other) {
            // trivial conflicts
            (Truthy(a), Truthy(b))
            | (IsNull(a), IsNull(b))
            | (IsText(a), IsText(b))
            | (IsNum(a), IsNum(b)) => a != b,
            // null is always false
            (Truthy(true), IsNull(true)) => true,
            // can only be one of null, text, num
            (IsNum(true), IsText(true)) => true,
            (IsNum(true), IsNull(true)) => true,
            (IsText(true), IsNull(true)) => true,
            // types and paths are truthy
            (IsType(true, _), Truthy(false))
            | (IsType(true, _), IsNull(true))
            | (IsPath(true, _), Truthy(false))
            | (IsPath(true, _), Truthy(true)) => true,
            // no conflict after all
            _ => false,
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
    fn from_constant(
        objtree: &'o ObjectTree,
        constant: &Constant,
        type_hint: Option<TypeRef<'o>>,
    ) -> AssumptionSet<'o> {
        match constant {
            Constant::Null(_) => {
                assumption_set![Assumption::IsNull(true), Assumption::Truthy(false)]
            }
            Constant::String(val) => assumption_set![
                Assumption::IsText(true),
                Assumption::Truthy(!val.is_empty())
            ],
            Constant::Resource(_) => assumption_set![Assumption::Truthy(true)],
            Constant::Float(val) => {
                assumption_set![Assumption::IsNum(true), Assumption::Truthy(*val != 0.0)]
            }
            Constant::List(_) => AssumptionSet::from_valid_instance(objtree.expect("/list")),
            Constant::Call(func, _) => match func {
                ConstFn::Icon => AssumptionSet::from_valid_instance(objtree.expect("/icon")),
                ConstFn::Matrix => AssumptionSet::from_valid_instance(objtree.expect("/matrix")),
                ConstFn::Newlist => AssumptionSet::from_valid_instance(objtree.expect("/list")),
                ConstFn::Sound => AssumptionSet::from_valid_instance(objtree.expect("/sound")),
                ConstFn::Generator => {
                    AssumptionSet::from_valid_instance(objtree.expect("/generator"))
                }
                ConstFn::Filter => AssumptionSet::default(),
                ConstFn::File => AssumptionSet::default(),
            },
            Constant::New { type_, args: _ } => {
                if let Some(pop) = type_.as_ref() {
                    if let Some(ty) = objtree.type_by_path(pop.path.iter()) {
                        AssumptionSet::from_valid_instance(ty)
                    } else {
                        AssumptionSet::default()
                    }
                } else if let Some(hint) = type_hint {
                    AssumptionSet::from_valid_instance(hint)
                } else {
                    AssumptionSet::default()
                }
            }
            Constant::Prefab(pop) => {
                if let Some(ty) = objtree.type_by_path(pop.path.iter()) {
                    assumption_set![Assumption::Truthy(false), Assumption::IsPath(true, ty)]
                } else {
                    AssumptionSet::default()
                }
            }
        }
    }

    fn from_valid_instance(ty: TypeRef<'o>) -> AssumptionSet<'o> {
        assumption_set![
            Assumption::Truthy(true),
            Assumption::IsNull(false),
            Assumption::IsType(true, ty)
        ]
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
    is_impure: Option<bool>,
}

impl<'o> Analysis<'o> {
    fn empty() -> Analysis<'o> {
        Analysis {
            static_ty: StaticType::None,
            aset: AssumptionSet::default(),
            value: None,
            fix_hint: None,
            is_impure: None,
        }
    }

    fn null() -> Analysis<'o> {
        Analysis {
            static_ty: StaticType::None,
            aset: assumption_set![Assumption::IsNull(true)],
            value: Some(Constant::Null(None)),
            fix_hint: None,
            is_impure: None,
        }
    }

    fn from_static_type(ty: TypeRef<'o>) -> Analysis<'o> {
        Analysis::from(StaticType::Type(ty))
    }

    fn from_static_type_impure(ty: TypeRef<'o>) -> Analysis<'o> {
        let mut analysis = Analysis::from(StaticType::Type(ty));
        analysis.is_impure = Some(true);
        analysis
    }

    fn from_value(
        objtree: &'o ObjectTree,
        value: Constant,
        type_hint: Option<TypeRef<'o>>,
    ) -> Analysis<'o> {
        Analysis {
            static_ty: StaticType::None,
            aset: AssumptionSet::from_constant(objtree, &value, type_hint),
            value: Some(value),
            fix_hint: None,
            is_impure: None,
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
            if !loc.is_builtins() {
                // Don't try to tell people to edit the builtins.
                self.add_note(loc, desc);
            }
        }
        self
    }
}

trait WithFilterArgs {
    fn with_filter_args(self, loc: Location, filtertype: &str) -> Self;
}

impl WithFilterArgs for DMError {
    fn with_filter_args(mut self, loc: Location, filtertype: &str) -> Self {
        // luckily lummox has made the anchor url match the type= value for each filter
        self.add_note(loc, format!("See: http://www.byond.com/docs/ref/#/{{notes}}/filters/{} for the permitted arguments", filtertype));
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
            is_impure: None,
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
            static_ty,
            fix_hint: None,
            value: None,
            is_impure: None,
        }
    }
}

// ----------------------------------------------------------------------------
// Entry points

/// Run DreamChecker, registering diagnostics to the context.
pub fn run(context: &Context, objtree: &ObjectTree) {
    run_inner(context, objtree, false)
}

/// Run DreamChecker, registering diagnostics and printing progress to stdout.
pub fn run_cli(context: &Context, objtree: &ObjectTree) {
    run_inner(context, objtree, true)
}

fn run_inner(context: &Context, objtree: &ObjectTree, cli: bool) {
    macro_rules! cli_println {
        ($($rest:tt)*) => {
            if cli { println!($($rest)*) }
        }
    }

    cli_println!("============================================================");
    cli_println!("Analyzing variables...\n");

    check_var_defs(objtree, context);

    let mut analyzer = AnalyzeObjectTree::new(context, objtree);

    cli_println!("============================================================");
    cli_println!("Gathering proc settings...\n");
    objtree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            if let Some(ref code) = proc.get().code {
                analyzer.gather_settings(proc, code);
            }
        }
    });

    cli_println!("============================================================");
    cli_println!("Analyzing proc bodies...\n");
    objtree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            if let Some(ref code) = proc.get().code {
                analyzer.check_proc(proc, code);
            }
        }
    });

    cli_println!("============================================================");
    cli_println!("Analyzing proc override validity...\n");
    objtree.root().recurse(&mut |ty| {
        for proc in ty.iter_self_procs() {
            analyzer.check_kwargs(proc);
            analyzer.propagate_violations(proc);
        }
    });

    analyzer.finish_check_kwargs();

    cli_println!("============================================================");
    cli_println!("Analyzing proc call tree...\n");
    analyzer.check_proc_call_tree();
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

/// Struct for SpacemanDMM_* directives
struct ProcDirective<'o> {
    directive: HashMap<ProcRef<'o>, (bool, Location)>,
    can_be_disabled: bool,
    set_at_definition: bool,
    can_be_global: bool,
    directive_string: &'static str,
}

impl<'o> ProcDirective<'o> {
    pub fn new(
        directive_string: &'static str,
        can_be_disabled: bool,
        set_at_definition: bool,
        can_be_global: bool,
    ) -> ProcDirective<'o> {
        ProcDirective {
            directive: Default::default(),
            directive_string,
            can_be_disabled,
            set_at_definition,
            can_be_global,
        }
    }

    pub fn insert(
        &mut self,
        proc: ProcRef<'o>,
        enable: bool,
        location: Location,
    ) -> Result<(), DMError> {
        if proc.ty().is_root() && !self.can_be_global {
            return Err(error(
                location,
                format!(
                    "{} sets {}, which cannot be set on global procs",
                    proc, self.directive_string
                ),
            )
            .with_errortype("incompatible_directive"));
        }
        if !enable && !self.can_be_disabled {
            return Err(error(
                location,
                format!(
                    "{} sets {} false, but it cannot be disabled.",
                    proc, self.directive_string
                ),
            )
            .with_errortype("disabled_directive")
            .set_severity(Severity::Warning));
        }
        if let Some((_, originallocation)) = self.directive.get(&proc) {
            return Err(error(
                location,
                format!("{} sets {} twice", proc, self.directive_string),
            )
            .with_note(*originallocation, "first definition here")
            .with_errortype("sets_directive_twice")
            .set_severity(Severity::Warning));
        }
        self.directive.insert(proc, (enable, location));
        Ok(())
    }

    /// Check if this specific ProcRef has this directive
    pub fn get(&self, proc: ProcRef<'o>) -> Option<&(bool, Location)> {
        self.directive.get(&proc)
    }

    /// Check if this ProcRef or its parents have this directive
    pub fn get_self_or_parent(&self, proc: ProcRef<'o>) -> Option<(ProcRef<'o>, bool, Location)> {
        let mut next = Some(proc);
        while let Some(current) = next {
            if let Some(&(truthy, location)) = self.get(current) {
                return Some((current, truthy, location));
            }
            next = current.parent_proc();
        }
        None
    }

    fn try_copy_from_parent(&mut self, proc: ProcRef<'o>) {
        if self.directive.get(&proc).is_none() {
            if let Some(parent) = proc.parent_proc() {
                if let Some((_, true, location)) = self.get_self_or_parent(parent) {
                    let _ = self.insert(proc, true, location);
                }
            }
        }
    }
}

/// Helper evaluation for directive true/false setting
pub fn directive_value_to_truthy(expr: &Expression, location: Location) -> Result<bool, DMError> {
    // Maybe this should be using constant evaluation, but for now accept TRUE and FALSE directly.
    match expr.as_term() {
        Some(Term::Int(0)) => Ok(false),
        Some(Term::Int(1)) => Ok(true),
        Some(Term::Ident(i)) if i == "FALSE" => Ok(false),
        Some(Term::Ident(i)) if i == "TRUE" => Ok(true),
        _ => Err(error(location, format!("invalid value for set {:?}", expr))
            .set_severity(Severity::Warning)),
    }
}

/// An ordered chain of ProcRef calls with their location and whether or not they are in a new context
#[derive(Default, Clone)]
pub struct CallStack<'o> {
    call_stack: VecDeque<(ProcRef<'o>, Location, bool)>,
}

impl<'o> CallStack<'o> {
    pub fn add_step(&mut self, proc: ProcRef<'o>, location: Location, new_context: bool) {
        self.call_stack.push_back((proc, location, new_context));
    }
}

trait DMErrorExt {
    fn with_callstack(self, stack: &CallStack) -> Self;
    fn with_blocking_builtins(self, blockers: &[(String, Location)]) -> Self;
    fn with_impure_operations(self, impures: &[(String, Location)]) -> Self;
}

impl DMErrorExt for DMError {
    fn with_callstack(mut self, stack: &CallStack) -> DMError {
        for (procref, location, new_context) in stack.call_stack.iter() {
            self.add_note(*location, format!("{}() called here", procref));
        }
        self
    }

    fn with_blocking_builtins(mut self, blockers: &[(String, Location)]) -> DMError {
        for (procname, location) in blockers.iter() {
            self.add_note(*location, format!("{}() called here", procname));
        }
        self
    }

    fn with_impure_operations(mut self, impures: &[(String, Location)]) -> DMError {
        for (impure, location) in impures.iter() {
            self.add_note(*location, format!("{} happens here", impure));
        }
        self
    }
}

#[derive(Default)]
pub struct ViolatingProcs<'o> {
    violators: HashMap<ProcRef<'o>, Vec<(String, Location)>>,
}

impl<'o> ViolatingProcs<'o> {
    pub fn insert_violator(&mut self, proc: ProcRef<'o>, builtin: &str, location: Location) {
        self.violators
            .entry(proc)
            .or_default()
            .push((builtin.to_string(), location));
    }

    pub fn get_violators(&self, proc: ProcRef<'o>) -> Option<&Vec<(String, Location)>> {
        self.violators.get(&proc)
    }
}

#[derive(Default, Debug)]
pub struct ViolatingOverrides<'o> {
    overrides: HashMap<ProcRef<'o>, Vec<ProcRef<'o>>>,
}

impl<'o> ViolatingOverrides<'o> {
    pub fn insert_override(&mut self, proc: ProcRef<'o>, child: ProcRef<'o>) {
        self.overrides.entry(proc).or_default().push(child);
    }

    pub fn get_override_violators(&self, proc: ProcRef<'o>) -> Option<&Vec<ProcRef<'o>>> {
        self.overrides.get(&proc)
    }
}

/// A deeper analysis of an ObjectTree
pub struct AnalyzeObjectTree<'o> {
    context: &'o Context,
    objtree: &'o ObjectTree,

    return_type: HashMap<ProcRef<'o>, TypeExpr<'o>>,
    must_call_parent: ProcDirective<'o>,
    must_not_override: ProcDirective<'o>,
    private: ProcDirective<'o>,
    protected: ProcDirective<'o>,
    must_not_sleep: ProcDirective<'o>,
    sleep_exempt: ProcDirective<'o>,
    must_be_pure: ProcDirective<'o>,
    can_be_redefined: ProcDirective<'o>,
    // Debug(ProcRef) -> KwargInfo
    used_kwargs: BTreeMap<String, KwargInfo>,

    call_tree: HashMap<ProcRef<'o>, Vec<(ProcRef<'o>, Location, bool)>>,

    sleeping_procs: ViolatingProcs<'o>,
    impure_procs: ViolatingProcs<'o>,
    waitfor_procs: HashSet<ProcRef<'o>>,

    sleeping_overrides: ViolatingOverrides<'o>,
    impure_overrides: ViolatingOverrides<'o>,
}

impl<'o> AnalyzeObjectTree<'o> {
    pub fn new(context: &'o Context, objtree: &'o ObjectTree) -> Self {
        let mut return_type = HashMap::default();
        return_type.insert(
            objtree.root().get_proc("get_step").unwrap(),
            StaticType::Type(objtree.expect("/turf")).into(),
        );

        AnalyzeObjectTree {
            context,
            objtree,
            return_type,
            must_call_parent: ProcDirective::new(
                "SpacemanDMM_should_call_parent",
                true,
                false,
                false,
            ),
            must_not_override: ProcDirective::new(
                "SpacemanDMM_should_not_override",
                false,
                false,
                false,
            ),
            private: ProcDirective::new("SpacemanDMM_private_proc", false, true, false),
            protected: ProcDirective::new("SpacemanDMM_protected_proc", false, true, false),
            must_not_sleep: ProcDirective::new("SpacemanDMM_should_not_sleep", false, true, true),
            sleep_exempt: ProcDirective::new("SpacemanDMM_allowed_to_sleep", false, true, true),
            must_be_pure: ProcDirective::new("SpacemanDMM_should_be_pure", false, true, true),
            can_be_redefined: ProcDirective::new(
                "SpacemanDMM_can_be_redefined",
                false,
                false,
                false,
            ),
            used_kwargs: Default::default(),
            call_tree: Default::default(),
            sleeping_procs: Default::default(),
            impure_procs: Default::default(),
            waitfor_procs: Default::default(),
            sleeping_overrides: Default::default(),
            impure_overrides: Default::default(),
        }
    }

    /// Analyze a specific proc
    pub fn check_proc(&mut self, proc: ProcRef<'o>, code: &'o [Spanned<Statement>]) {
        self.must_not_sleep.try_copy_from_parent(proc);
        self.must_be_pure.try_copy_from_parent(proc);

        AnalyzeProc::new(self, self.context, self.objtree, proc).run(code)
    }

    #[inline]
    fn add_directive_or_error(
        &mut self,
        proc: ProcRef<'o>,
        directive: &str,
        expr: &Expression,
        location: Location,
    ) {
        let procdirective = match directive {
            "SpacemanDMM_should_not_override" => &mut self.must_not_override,
            "SpacemanDMM_should_call_parent" => &mut self.must_call_parent,
            "SpacemanDMM_private_proc" => &mut self.private,
            "SpacemanDMM_protected_proc" => &mut self.protected,
            "SpacemanDMM_should_not_sleep" => &mut self.must_not_sleep,
            "SpacemanDMM_allowed_to_sleep" => &mut self.sleep_exempt,
            "SpacemanDMM_should_be_pure" => &mut self.must_be_pure,
            "SpacemanDMM_can_be_redefined" => &mut self.can_be_redefined,
            other => {
                error(location, format!("unknown linter setting {:?}", directive))
                    .with_errortype("unknown_linter_setting")
                    .set_severity(Severity::Warning)
                    .register(self.context);
                return;
            }
        };

        if procdirective.set_at_definition {
            if let Some(procdef) = &mut proc.get_declaration() {
                if procdef.location != proc.get().location {
                    error(
                        location,
                        format!(
                            "Can't define procs {} outside their initial definition",
                            directive
                        ),
                    )
                    .set_severity(Severity::Warning)
                    .register(self.context);
                    return;
                }
            }
        }

        match directive_value_to_truthy(expr, location) {
            Ok(truthy) => {
                if let Err(error) = procdirective.insert(proc, truthy, location) {
                    self.context.register_error(error);
                }
            }
            Err(error) => self
                .context
                .register_error(error.with_errortype("invalid_lint_directive_value")),
        }
    }

    pub fn check_proc_call_tree(&mut self) {
        for (procref, &(_, location)) in self.must_not_sleep.directive.iter() {
            if let Some(sleepvec) = self.sleeping_procs.get_violators(*procref) {
                error(
                    procref.get().location,
                    format!(
                        "{} sets SpacemanDMM_should_not_sleep but calls blocking built-in(s)",
                        procref
                    ),
                )
                .with_note(location, "SpacemanDMM_should_not_sleep set here")
                .with_errortype("must_not_sleep")
                .with_blocking_builtins(sleepvec)
                .register(self.context)
            }
            let mut visited = HashSet::<ProcRef<'o>>::new();
            let mut to_visit = VecDeque::<(ProcRef<'o>, CallStack, bool)>::new();
            if let Some(procscalled) = self.call_tree.get(procref) {
                for (proccalled, location, new_context) in procscalled {
                    let mut callstack = CallStack::default();
                    callstack.add_step(*proccalled, *location, *new_context);
                    to_visit.push_back((*proccalled, callstack, *new_context));
                }
            }
            while let Some((nextproc, callstack, new_context)) = to_visit.pop_front() {
                if !visited.insert(nextproc) {
                    continue;
                }
                if self.waitfor_procs.get(&nextproc).is_some() {
                    continue;
                }
                if self.sleep_exempt.get(nextproc).is_some() {
                    continue;
                }
                if new_context {
                    continue;
                }
                if let Some(sleepvec) = self.sleeping_procs.get_violators(nextproc) {
                    error(
                        procref.get().location,
                        format!(
                            "{} sets SpacemanDMM_should_not_sleep but calls blocking proc {}",
                            procref, nextproc
                        ),
                    )
                    .with_note(location, "SpacemanDMM_should_not_sleep set here")
                    .with_errortype("must_not_sleep")
                    .with_callstack(&callstack)
                    .with_blocking_builtins(sleepvec)
                    .register(self.context)
                } else if let Some(overridesleep) =
                    self.sleeping_overrides.get_override_violators(nextproc)
                {
                    for child_violator in overridesleep {
                        if procref.ty().is_subtype_of(&nextproc.ty())
                            && !child_violator.ty().is_subtype_of(&procref.ty())
                        {
                            continue;
                        }
                        error(
                            procref.get().location,
                            format!(
                                "{} calls {} which has override child proc that sleeps {}",
                                procref, nextproc, child_violator
                            ),
                        )
                        .with_note(location, "SpacemanDMM_should_not_sleep set here")
                        .with_errortype("must_not_sleep")
                        .with_callstack(&callstack)
                        .with_blocking_builtins(
                            self.sleeping_procs.get_violators(*child_violator).unwrap(),
                        )
                        .register(self.context)
                    }
                }
                if let Some(calledvec) = self.call_tree.get(&nextproc) {
                    for (proccalled, location, new_context) in calledvec.iter() {
                        let mut newstack = callstack.clone();
                        newstack.add_step(*proccalled, *location, *new_context);
                        to_visit.push_back((*proccalled, newstack, *new_context));
                    }
                }
            }
        }

        for (procref, (_, location)) in self.must_be_pure.directive.iter() {
            if let Some(impurevec) = self.impure_procs.get_violators(*procref) {
                error(
                    procref.get().location,
                    format!("{} does impure operations", procref),
                )
                .with_errortype("must_be_pure")
                .with_note(*location, "SpacemanDMM_should_be_pure set here")
                .with_impure_operations(impurevec)
                .register(self.context)
            }
            let mut visited = HashSet::<ProcRef<'o>>::new();
            let mut to_visit = VecDeque::<(ProcRef<'o>, CallStack)>::new();
            if let Some(procscalled) = self.call_tree.get(procref) {
                for (proccalled, location, new_context) in procscalled {
                    let mut callstack = CallStack::default();
                    callstack.add_step(*proccalled, *location, *new_context);
                    to_visit.push_back((*proccalled, callstack));
                }
            }
            while let Some((nextproc, callstack)) = to_visit.pop_front() {
                if !visited.insert(nextproc) {
                    continue;
                }
                if let Some(impurevec) = self.impure_procs.get_violators(nextproc) {
                    error(procref.get().location, format!("{} sets SpacemanDMM_should_be_pure but calls a {} that does impure operations", procref, nextproc))
                        .with_note(*location, "SpacemanDMM_should_be_pure set here")
                        .with_errortype("must_be_pure")
                        .with_callstack(&callstack)
                        .with_impure_operations(impurevec)
                        .register(self.context)
                } else if let Some(overrideimpure) =
                    self.impure_overrides.get_override_violators(nextproc)
                {
                    for child_violator in overrideimpure {
                        if procref.ty().is_subtype_of(&nextproc.ty())
                            && !child_violator.ty().is_subtype_of(&procref.ty())
                        {
                            continue;
                        }
                        error(procref.get().location, format!("{} calls {} which has override child proc that does impure operations {}", procref, nextproc, child_violator))
                            .with_note(*location, "SpacemanDMM_should_not_pure set here")
                            .with_errortype("must_be_pure")
                            .with_callstack(&callstack)
                            .with_blocking_builtins(self.impure_procs.get_violators(*child_violator).unwrap())
                            .register(self.context)
                    }
                }
                if let Some(calledvec) = self.call_tree.get(&nextproc) {
                    for (proccalled, location, new_context) in calledvec.iter() {
                        let mut newstack = callstack.clone();
                        newstack.add_step(*proccalled, *location, *new_context);
                        to_visit.push_back((*proccalled, newstack));
                    }
                }
            }
        }
    }

    /// Gather and store set directives for the given proc using the provided code body
    pub fn gather_settings(&mut self, proc: ProcRef<'o>, code: &'o [Spanned<Statement>]) {
        for statement in code.iter() {
            if let Statement::Setting {
                ref name,
                ref value,
                ..
            } = statement.elem
            {
                if name == "SpacemanDMM_return_type" {
                    if let Some(Term::Prefab(fab)) = value.as_term() {
                        let bits: Vec<_> =
                            fab.path.iter().map(|(_, name)| name.to_owned()).collect();
                        let ty = self.static_type(statement.location, &bits);
                        self.return_type.insert(proc, TypeExpr::from(ty));
                    } else {
                        match TypeExpr::compile(proc, statement.location, value) {
                            Ok(expr) => {
                                self.return_type.insert(proc, expr);
                            }
                            Err(error) => error
                                .with_component(dm::Component::DreamChecker)
                                .register(self.context),
                        }
                    }
                } else if name.starts_with("SpacemanDMM_") {
                    self.add_directive_or_error(proc, name.as_str(), value, statement.location);
                } else if !KNOWN_SETTING_NAMES.contains(&name.as_str()) {
                    error(statement.location, format!("unknown setting {:?}", name))
                        .set_severity(Severity::Warning)
                        .register(self.context);
                } else {
                    match name.as_str() {
                        "background" | "waitfor" | "hidden" | "instant" | "popup_menu" => {
                            if directive_value_to_truthy(value, statement.location).is_err() {
                                error(
                                    statement.location,
                                    format!("set {} must be 0/1/TRUE/FALSE", name.as_str()),
                                )
                                .set_severity(Severity::Warning)
                                .with_errortype("invalid_set_value")
                                .register(self.context);
                            }
                        }
                        "name" | "category" | "desc" => {
                            if let Some(term) = value.as_term() {
                                match term {
                                    // TODO: detect procs-as-verbs here
                                    Term::String(_) | Term::InterpString(_, _) => {}
                                    // category can be set null to hide it
                                    Term::Null if name.as_str() == "category" => {}
                                    other => {
                                        error(
                                            statement.location,
                                            format!(
                                                "set {} must have a string value",
                                                name.as_str()
                                            ),
                                        )
                                        .set_severity(Severity::Warning)
                                        .with_errortype("invalid_set_value")
                                        .register(self.context);
                                    }
                                }
                            }
                        }
                        "invisibility" => {
                            if let Some(Term::Int(i)) = value.as_term() {
                                if *i >= 0 && *i <= 100 {
                                    continue;
                                }
                            }
                            error(statement.location, "set invisibility must be 0-100")
                                .set_severity(Severity::Warning)
                                .with_errortype("invalid_set_value")
                                .register(self.context);
                        }
                        _ => {}
                    }
                }
            } else {
                break;
            }
        }
    }

    /// Propagate violations make up the inheritence graph
    pub fn propagate_violations(&mut self, proc: ProcRef<'o>) {
        if proc.name() == "New" {
            // New() propogates via ..() and causes weirdness
            return;
        }
        if self.sleeping_procs.get_violators(proc).is_some() {
            let mut next = proc.parent_proc();
            while let Some(current) = next {
                self.sleeping_overrides.insert_override(current, proc);
                next = current.parent_proc();
            }
        }
        if self.impure_procs.get_violators(proc).is_some() {
            let mut next = proc.parent_proc();
            while let Some(current) = next {
                self.impure_overrides.insert_override(current, proc);
                next = current.parent_proc();
            }
        }
    }

    /// Check and build a list of bad overrides of kwargs for a ProcRef
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
                        BadOverride {
                            missing,
                            location: proc.location,
                        },
                    );
                }
            }
            next = current.parent_proc();
        }
    }

    /// Finish analyzing kwargs for missing overrides
    pub fn finish_check_kwargs(&self) {
        for (base_procname, kwarg_info) in self.used_kwargs.iter() {
            if kwarg_info.bad_overrides_at.is_empty() {
                continue;
            }

            // List out the child procs that are missing overrides.
            let msg = match kwarg_info.bad_overrides_at.len() {
                1 => format!("an override of {} is missing keyword args", base_procname),
                len => format!(
                    "{} overrides of {} are missing keyword args",
                    len, base_procname
                ),
            };
            let mut error =
                error(kwarg_info.location, msg).with_errortype("override_missing_keyword_arg");
            let mut missing = HashSet::new();
            for (child_procname, bad_override) in kwarg_info.bad_overrides_at.iter() {
                error.add_note(
                    bad_override.location,
                    format!(
                        "{} is missing \"{}\"",
                        child_procname,
                        bad_override.missing.join("\", \"")
                    ),
                );
                missing.extend(bad_override.missing.iter());
            }

            // List call sites. If nobody ever calls these as kwargs, then
            // there's not gonna be a problem.
            for (arg_name, called_at) in kwarg_info.called_at.iter() {
                if !missing.contains(arg_name) {
                    continue;
                }

                if called_at.others > 0 {
                    error.add_note(
                        called_at.location,
                        format!(
                            "called with {:?} here, and {} other places",
                            arg_name, called_at.others
                        ),
                    );
                } else {
                    error.add_note(
                        called_at.location,
                        format!("called with {:?} here", arg_name),
                    );
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

fn static_type<'o>(
    objtree: &'o ObjectTree,
    location: Location,
    mut of: &[String],
) -> Result<StaticType<'o>, DMError> {
    while !of.is_empty()
        && [
            "static",
            "global",
            "const",
            "tmp",
            "SpacemanDMM_final",
            "SpacemanDMM_private",
            "SpacemanDMM_protected",
        ]
        .contains(&&*of[0])
    {
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
        Err(error(
            location,
            format!("undefined type: {}", FormatTreePath(of)),
        ))
    }
}

fn error<S: Into<String>>(location: Location, desc: S) -> DMError {
    DMError::new(location, desc).with_component(dm::Component::DreamChecker)
}

// ----------------------------------------------------------------------------
// Variable analyzer

/// Examines an ObjectTree for var definitions that are invalid
pub fn check_var_defs(objtree: &ObjectTree, context: &Context) {
    for typeref in objtree.iter_types() {
        let path = &typeref.path;

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
                    DMError::new(
                        mydecl.location,
                        format!("{} redeclares var {:?}", path, varname),
                    )
                    .with_note(decl.location, format!("declared on {} here", parent.path))
                    .register(context);
                }

                if decl.var_type.flags.is_final() {
                    DMError::new(
                        typevar.value.location,
                        format!("{} overrides final var {:?}", path, varname),
                    )
                    .with_errortype("final_var")
                    .with_note(
                        decl.location,
                        format!("declared final on {} here", parent.path),
                    )
                    .register(context);
                }

                if decl.var_type.flags.is_private() {
                    DMError::new(
                        typevar.value.location,
                        format!("{} overrides private var {:?}", path, varname),
                    )
                    .with_errortype("private_var")
                    .with_note(
                        decl.location,
                        format!("declared private on {} here", parent.path),
                    )
                    .register(context);
                }
            }
        }
    }
}

// ----------------------------------------------------------------------------
// Procedure analyzer
#[derive(Debug)]
pub struct ControlFlow {
    pub returns: bool,
    pub continues: bool,
    pub breaks: bool,
    pub fuzzy: bool,
}

impl ControlFlow {
    pub fn alltrue() -> ControlFlow {
        ControlFlow {
            returns: true,
            continues: true,
            breaks: true,
            fuzzy: false,
        }
    }
    pub fn allfalse() -> ControlFlow {
        ControlFlow {
            returns: false,
            continues: false,
            breaks: false,
            fuzzy: false,
        }
    }
    pub fn terminates(&self) -> bool {
        !self.fuzzy && (self.returns || self.continues || self.breaks)
    }

    pub fn terminates_loop(&self) -> bool {
        !self.fuzzy && (self.returns || self.breaks)
    }

    pub fn no_else(&mut self) {
        self.returns = false;
        self.continues = false;
        self.breaks = false;
    }

    pub fn merge(&mut self, other: ControlFlow) {
        if !self.fuzzy && other.returns {
            self.returns = true;
        }
        if other.fuzzy {
            self.returns = false;
        }
        if other.continues {
            self.continues = true;
        }
        if other.breaks {
            self.breaks = true;
        }
        if other.fuzzy {
            self.fuzzy = true;
        }
    }

    pub fn merge_false(&mut self, other: ControlFlow) {
        if !other.returns {
            self.returns = false;
        }
        if !other.continues {
            self.continues = false;
        }
        if !other.breaks {
            self.breaks = false;
        }
        if other.fuzzy {
            self.fuzzy = true;
        }
    }

    pub fn finalize(&mut self) {
        if self.returns || self.breaks || self.continues {
            self.fuzzy = false;
        }
    }

    pub fn end_loop(&mut self) {
        self.returns = false;
        self.continues = false;
        self.breaks = false;
        self.fuzzy = false;
    }
}

#[derive(Debug, Clone)]
struct LocalVar<'o> {
    location: Location,
    analysis: Analysis<'o>,
}

impl<'o> From<Analysis<'o>> for LocalVar<'o> {
    fn from(analysis: Analysis<'o>) -> Self {
        LocalVar {
            location: Location::default(),
            analysis,
        }
    }
}

struct AnalyzeProc<'o, 's> {
    env: &'s mut AnalyzeObjectTree<'o>,
    context: &'o Context,
    objtree: &'o ObjectTree,
    ty: TypeRef<'o>,
    proc_ref: ProcRef<'o>,
    calls_parent: bool,
    inside_newcontext: u32,
}

impl<'o, 's> AnalyzeProc<'o, 's> {
    fn new(
        env: &'s mut AnalyzeObjectTree<'o>,
        context: &'o Context,
        objtree: &'o ObjectTree,
        proc_ref: ProcRef<'o>,
    ) -> Self {
        let ty = proc_ref.ty();

        AnalyzeProc {
            env,
            context,
            objtree,
            ty,
            proc_ref,
            calls_parent: false,
            inside_newcontext: 0,
        }
    }

    pub fn run(&mut self, block: &'o [Spanned<Statement>]) {
        let mut local_vars =
            HashMap::<String, LocalVar, RandomState>::with_hasher(RandomState::default());
        local_vars.insert(".".to_owned(), Analysis::empty().into());
        local_vars.insert(
            "args".to_owned(),
            Analysis::from_static_type_impure(self.objtree.expect("/list")).into(),
        );
        local_vars.insert(
            "usr".to_owned(),
            Analysis::from_static_type(self.objtree.expect("/mob")).into(),
        );
        if !self.ty.is_root() {
            local_vars.insert("src".to_owned(), Analysis::from_static_type(self.ty).into());
        }
        local_vars.insert(
            "global".to_owned(),
            Analysis {
                static_ty: StaticType::Type(self.objtree.root()),
                aset: assumption_set![Assumption::IsNull(false)],
                value: None,
                fix_hint: None,
                is_impure: Some(true),
            }
            .into(),
        );

        for param in self.proc_ref.get().parameters.iter() {
            let mut analysis = self.static_type(param.location, &param.var_type.type_path);
            analysis.is_impure = Some(true); // all params are impure
            local_vars.insert(
                param.name.to_owned(),
                LocalVar {
                    location: self.proc_ref.location,
                    analysis,
                },
            );
            //println!("adding parameters {:#?}", self.local_vars);
        }

        self.visit_block(block, &mut local_vars);

        //println!("purity {}", self.is_pure);

        if let Some(parent) = self.proc_ref.parent_proc() {
            if let Some((proc, true, location)) = self.env.private.get_self_or_parent(self.proc_ref)
            {
                if proc != self.proc_ref {
                    error(
                        self.proc_ref.location,
                        format!("proc overrides private parent, prohibited by {}", proc),
                    )
                    .with_note(location, "prohibited by this private_proc annotation")
                    .with_errortype("private_proc")
                    .register(self.context);
                }
            }
            if let Some((proc, true, location)) =
                self.env.must_not_override.get_self_or_parent(self.proc_ref)
            {
                if proc != self.proc_ref {
                    error(
                        self.proc_ref.location,
                        format!("proc overrides parent, prohibited by {}", proc),
                    )
                    .with_note(location, "prohibited by this must_not_override annotation")
                    .with_errortype("must_not_override")
                    .register(self.context);
                }
            }
            if !self.calls_parent {
                if let Some((proc, true, location)) =
                    self.env.must_call_parent.get_self_or_parent(self.proc_ref)
                {
                    error(
                        self.proc_ref.location,
                        format!("proc never calls parent, required by {}", proc),
                    )
                    .with_note(location, "required by this must_call_parent annotation")
                    .with_errortype("must_call_parent")
                    .register(self.context);
                }
            }
            if !parent.is_builtin()
                && self.proc_ref.ty() == parent.ty()
                && self
                    .env
                    .can_be_redefined
                    .get_self_or_parent(self.proc_ref)
                    .is_none()
            {
                error(
                    self.proc_ref.location,
                    format!("redefining proc {}/{}", self.ty, self.proc_ref.name()),
                )
                .with_errortype("redefined_proc")
                .with_note(parent.location, "previous definition is here")
                .set_severity(Severity::Hint)
                .register(self.context);
            }
        }
    }

    fn visit_block(
        &mut self,
        block: &'o [Spanned<Statement>],
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) -> ControlFlow {
        let mut term = ControlFlow::allfalse();
        for stmt in block.iter() {
            if term.terminates() {
                error(stmt.location, "possible unreachable code here")
                    .with_errortype("unreachable_code")
                    .register(self.context);
                return term; // stop evaluating
            }
            let state = self.visit_statement(stmt.location, &stmt.elem, local_vars);
            term.merge(state);
        }
        term
    }

    fn loop_condition_check(&mut self, location: Location, expression: &'o Expression) {
        match expression.is_truthy() {
            Some(true) => {
                error(location, "loop condition is always true")
                    .with_errortype("loop_condition_determinate")
                    .register(self.context);
            }
            Some(false) => {
                error(location, "loop condition is always false")
                    .with_errortype("loop_condition_determinate")
                    .register(self.context);
            }
            _ => (),
        };
    }

    fn visit_control_condition(&mut self, location: Location, expression: &'o Expression) {
        if expression.is_const_eval() {
            error(location, "control flow condition is a constant evalutation")
                .with_errortype("control_condition_static")
                .register(self.context);
        } else if let Some(term) = expression.as_term() {
            if term.is_static() {
                error(location, "control flow condition is a static term")
                    .with_errortype("control_condition_static")
                    .register(self.context);
            }
        }
    }

    fn visit_statement(
        &mut self,
        location: Location,
        statement: &'o Statement,
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) -> ControlFlow {
        match statement {
            Statement::Expr(expr) => {
                match expr {
                    Expression::Base { term, follow } => {
                        if let Term::Call(call, vec) = &term.elem {
                            if !follow.iter().any(|f| matches!(f.elem, Follow::Call(..))) {
                                if let Some(proc) = self.ty.get_proc(call) {
                                    if let Some((_, _, loc)) =
                                        self.env.must_be_pure.get_self_or_parent(proc)
                                    {
                                        error(
                                            location,
                                            format!(
                                                "call to pure proc {} discards return value",
                                                call
                                            ),
                                        )
                                        .with_note(
                                            loc,
                                            "prohibited by this must_be_pure annotation",
                                        )
                                        .register(self.context);
                                    }
                                }
                            }
                        }
                    }
                    Expression::BinaryOp {
                        op: BinaryOp::LShift,
                        lhs,
                        rhs,
                    } => {
                        let lhsanalysis = self.visit_expression(location, lhs, None, local_vars);
                        if let Some(impurity) = lhsanalysis.is_impure {
                            if impurity {
                                self.env.impure_procs.insert_violator(
                                    self.proc_ref,
                                    "purity breaking << on expression",
                                    location,
                                );
                            }
                        }
                    }
                    _ => {}
                }
                self.visit_expression(location, expr, None, local_vars);
            }
            Statement::Return(Some(expr)) => {
                // TODO: factor in the previous return type if there was one
                if self.inside_newcontext > 0 {
                    error(location, "returning a value in a spawn has no effect")
                        .set_severity(Severity::Warning)
                        .register(self.context);
                }
                let return_type = self.visit_expression(location, expr, None, local_vars);
                local_vars.get_mut(".").unwrap().analysis = return_type;
                return ControlFlow {
                    returns: true,
                    continues: false,
                    breaks: false,
                    fuzzy: false,
                };
            }
            Statement::Return(None) => {
                return ControlFlow {
                    returns: true,
                    continues: false,
                    breaks: false,
                    fuzzy: false,
                }
            }
            Statement::Crash(expr) => {
                if let Some(expr) = expr {
                    self.visit_expression(location, expr, None, local_vars);
                }
                return ControlFlow {
                    returns: true,
                    continues: false,
                    breaks: false,
                    fuzzy: false,
                };
            }
            Statement::Throw(expr) => {
                self.visit_expression(location, expr, None, local_vars);
            }
            Statement::While { condition, block } => {
                let mut scoped_locals = local_vars.clone();
                // We don't check for static/determine conditions because while(TRUE) is so common.
                self.visit_expression(location, condition, None, &mut scoped_locals);
                let mut state = self.visit_block(block, &mut scoped_locals);
                state.end_loop();
                return state;
            }
            Statement::DoWhile { block, condition } => {
                let mut scoped_locals = local_vars.clone();
                let mut state = self.visit_block(block, &mut scoped_locals);
                if state.terminates_loop() {
                    error(
                        location,
                        "do while terminates without ever reaching condition",
                    )
                    .register(self.context);
                    return state;
                }
                self.visit_expression(
                    condition.location,
                    &condition.elem,
                    None,
                    &mut scoped_locals,
                );

                state.end_loop();
                return state;
            }
            Statement::If { arms, else_arm } => {
                let mut allterm = ControlFlow::alltrue();
                let mut alwaystrue = false;
                for (condition, ref block) in arms.iter() {
                    let mut scoped_locals = local_vars.clone();
                    self.visit_control_condition(condition.location, &condition.elem);
                    if alwaystrue {
                        error(condition.location,"unreachable if block, preceeding if/elseif condition(s) are always true")
                            .with_errortype("unreachable_code")
                            .register(self.context);
                    }
                    self.visit_expression(
                        condition.location,
                        &condition.elem,
                        None,
                        &mut scoped_locals,
                    );
                    let state = self.visit_block(block, &mut scoped_locals);
                    match condition.elem.is_truthy() {
                        Some(true) => {
                            error(condition.location, "if condition is always true")
                                .with_errortype("if_condition_determinate")
                                .register(self.context);
                            allterm.merge_false(state);
                            alwaystrue = true;
                        }
                        Some(false) => {
                            error(condition.location, "if condition is always false")
                                .with_errortype("if_condition_determinate")
                                .register(self.context);
                        }
                        None => allterm.merge_false(state),
                    };
                }
                if let Some(else_arm) = else_arm {
                    if alwaystrue {
                        if let Some(else_expr) = else_arm.first() {
                            error(else_expr.location ,"unreachable else block, preceeding if/elseif condition(s) are always true")
                                .with_errortype("unreachable_code")
                                .register(self.context);
                        }
                    }
                    let state = self.visit_block(else_arm, &mut local_vars.clone());
                    allterm.merge_false(state);
                } else {
                    allterm.no_else();
                    return allterm;
                }
                allterm.finalize();
                return allterm;
            }
            Statement::ForInfinite { block } => {
                let mut scoped_locals = local_vars.clone();
                let mut state = self.visit_block(block, &mut scoped_locals);
                state.end_loop();
                return state;
            }
            Statement::ForLoop {
                init,
                test,
                inc,
                block,
            } => {
                let mut scoped_locals = local_vars.clone();
                if let Some(init) = init {
                    self.visit_statement(location, init, &mut scoped_locals);
                }
                if let Some(test) = test {
                    self.loop_condition_check(location, test);
                    self.visit_control_condition(location, test);
                    self.visit_expression(location, test, None, &mut scoped_locals);
                }
                if let Some(inc) = inc {
                    self.visit_statement(location, inc, &mut scoped_locals);
                }
                let mut state = self.visit_block(block, &mut scoped_locals);
                state.end_loop();
                return state;
            }
            Statement::ForList(for_list) => {
                let ForListStatement {
                    var_type,
                    name,
                    input_type,
                    in_list,
                    block,
                } = &**for_list;
                let mut scoped_locals = local_vars.clone();
                if let Some(in_list) = in_list {
                    let list = self.visit_expression(location, in_list, None, &mut scoped_locals);
                    match list.static_ty {
                        StaticType::None => {
                            // Occurs extremely often due to DM not complaining about this, with
                            // over 800 detections on /tg/. Maybe a future lint.
                        }
                        StaticType::List { .. } => { /* OK */ }
                        StaticType::Type(ty) => {
                            if ty != self.objtree.expect("/world")
                                && ty != self.objtree.expect("/list")
                            {
                                let atom = self.objtree.expect("/atom");
                                if ty.is_subtype_of(&atom) {
                                    // Fine.
                                } else if atom.is_subtype_of(&ty) {
                                    // Iffy conceptually, but the only detections on /tg/ are false positives in the
                                    // component system, where we loop over `var/datum/parent` that is known to be an
                                    // atom in a way that's hard for Dreamchecker to capture.
                                    error(
                                        location,
                                        "iterating over a /datum which might not be an /atom",
                                    )
                                    .set_severity(Severity::Hint)
                                    .register(self.context);
                                } else {
                                    // The type is a /datum/foo subtype that definitely can't be looped over.
                                    error(
                                        location,
                                        format!(
                                            "iterating over a {} which cannot be iterated",
                                            ty.path
                                        ),
                                    )
                                    .register(self.context);
                                }
                            }
                        }
                    }
                }
                if let Some(var_type) = var_type {
                    self.visit_var(location, var_type, name, None, &mut scoped_locals);
                }
                let mut state = self.visit_block(block, &mut scoped_locals);
                state.end_loop();
                return state;
            }
            Statement::ForRange(for_range) => {
                let ForRangeStatement {
                    var_type,
                    name,
                    start,
                    end,
                    step,
                    block,
                } = &**for_range;
                let mut scoped_locals = local_vars.clone();
                self.visit_expression(location, end, None, &mut scoped_locals);
                if let Some(step) = step {
                    self.visit_expression(location, step, None, &mut scoped_locals);
                }
                if let Some(var_type) = var_type {
                    self.visit_var(location, var_type, name, Some(start), &mut scoped_locals);
                }
                let mut state = self.visit_block(block, &mut scoped_locals);
                if let Some(startterm) = start.as_term() {
                    if let Some(endterm) = end.as_term() {
                        if let Some(validity) = startterm.valid_for_range(endterm, step.as_ref()) {
                            if !validity {
                                error(
                                    location,
                                    "for range loop body is never reached due to invalid range",
                                )
                                .register(self.context);
                            } else {
                                return state;
                            }
                        }
                    }
                }
                state.end_loop();
                return state;
            }
            Statement::Var(var) => self.visit_var_stmt(location, var, local_vars),
            Statement::Vars(vars) => {
                for each in vars.iter() {
                    self.visit_var_stmt(location, each, local_vars);
                }
            }
            Statement::Setting {
                name,
                mode: SettingMode::Assign,
                value,
            } => {
                if name != "waitfor" {
                    return ControlFlow::allfalse();
                }
                match match value.as_term() {
                    Some(Term::Int(0)) => Some(true),
                    Some(Term::Ident(i)) if i == "FALSE" => Some(true),
                    _ => None,
                } {
                    Some(_) => {
                        self.env.waitfor_procs.insert(self.proc_ref);
                    }
                    None => (),
                }
            }
            Statement::Setting { .. } => {}
            Statement::Spawn { delay, block } => {
                self.inside_newcontext = self.inside_newcontext.wrapping_add(1);
                let mut scoped_locals = local_vars.clone();
                if let Some(delay) = delay {
                    self.visit_expression(location, delay, None, &mut scoped_locals);
                }
                self.visit_block(block, &mut scoped_locals);
                self.inside_newcontext = self.inside_newcontext.wrapping_sub(1);
            }
            Statement::Switch {
                input,
                cases,
                default,
            } => {
                check_switch_rand_range(input, cases, default, location, self.context);
                let mut allterm = ControlFlow::alltrue();
                self.visit_control_condition(location, input);
                self.visit_expression(location, input, None, local_vars);
                for (case, ref block) in cases.iter() {
                    let mut scoped_locals = local_vars.clone();
                    if let [dm::ast::Case::Exact(Expression::BinaryOp {
                        op: BinaryOp::Or, ..
                    })] = case.elem[..]
                    {
                        error(case.location, "Elements in a switch-case branch separated by ||, this is likely in error and should be replaced by a comma")
                            .set_severity(Severity::Warning)
                            .register(self.context);
                    }
                    for case_part in case.elem.iter() {
                        match case_part {
                            dm::ast::Case::Exact(expr) => {
                                self.visit_expression(
                                    case.location,
                                    expr,
                                    None,
                                    &mut scoped_locals,
                                );
                            }
                            dm::ast::Case::Range(start, end) => {
                                self.visit_expression(
                                    case.location,
                                    start,
                                    None,
                                    &mut scoped_locals,
                                );
                                self.visit_expression(case.location, end, None, &mut scoped_locals);
                            }
                        }
                    }
                    let state = self.visit_block(block, &mut scoped_locals);
                    allterm.merge_false(state);
                }
                if let Some(default) = default {
                    let state = self.visit_block(default, &mut local_vars.clone());
                    allterm.merge_false(state);
                } else {
                    allterm.no_else();
                    return allterm;
                }
                allterm.finalize();
                return allterm;
            }
            Statement::TryCatch {
                try_block,
                catch_params,
                catch_block,
            } => {
                self.visit_block(try_block, &mut local_vars.clone());
                if catch_params.len() > 1 {
                    error(
                        location,
                        format!(
                            "Expected 0 or 1 catch parameters, got {}",
                            catch_params.len()
                        ),
                    )
                    .set_severity(Severity::Warning)
                    .register(self.context);
                }
                let mut catch_locals = local_vars.clone();
                for caught in catch_params.iter() {
                    let (var_name, mut type_path) = match caught.split_last() {
                        Some(x) => x,
                        None => continue,
                    };
                    match type_path.split_first() {
                        Some((first, rest)) if first == "var" => type_path = rest,
                        _ => {}
                    }
                    let var_type: VarType = type_path.iter().map(ToOwned::to_owned).collect();
                    self.visit_var(location, &var_type, var_name, None, &mut catch_locals);
                }
                self.visit_block(catch_block, &mut catch_locals);
            }
            Statement::Continue(_) => {
                return ControlFlow {
                    returns: false,
                    continues: true,
                    breaks: false,
                    fuzzy: true,
                }
            }
            Statement::Break(_) => {
                return ControlFlow {
                    returns: false,
                    continues: false,
                    breaks: true,
                    fuzzy: true,
                }
            }
            Statement::Goto(_) => {}
            Statement::Label { name: _, block } => {
                self.visit_block(block, &mut local_vars.clone());
            }
            Statement::Del(expr) => {
                self.visit_expression(location, expr, None, local_vars);
            }
        }
        ControlFlow::allfalse()
    }

    fn visit_var_stmt(
        &mut self,
        location: Location,
        var: &'o VarStatement,
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) {
        self.visit_var(
            location,
            &var.var_type,
            &var.name,
            var.value.as_ref(),
            local_vars,
        )
    }

    fn visit_var(
        &mut self,
        location: Location,
        var_type: &VarType,
        name: &str,
        value: Option<&'o Expression>,
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) {
        // Calculate type hint
        let static_type = self.env.static_type(location, &var_type.type_path);
        // Visit the expression if it's there
        let mut analysis = match value {
            Some(expr) => {
                self.visit_expression(location, expr, static_type.basic_type(), local_vars)
            }
            None => Analysis::null(),
        };
        analysis.static_ty = static_type;

        // Save var to locals
        local_vars.insert(name.to_owned(), LocalVar { location, analysis });
    }

    fn visit_expression(
        &mut self,
        location: Location,
        expression: &'o Expression,
        type_hint: Option<TypeRef<'o>>,
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) -> Analysis<'o> {
        match expression {
            Expression::Base { term, follow } => {
                let base_type_hint = if follow.is_empty() { type_hint } else { None };
                let mut ty = self.visit_term(term.location, &term.elem, base_type_hint, local_vars);
                for each in follow.iter() {
                    ty = self.visit_follow(each.location, ty, &each.elem, local_vars);
                }
                ty
            }
            Expression::BinaryOp {
                op: BinaryOp::LShift,
                lhs,
                rhs,
            } => {
                let lty = self.visit_expression(location, lhs, None, local_vars);

                if lty.static_ty == StaticType::Type(self.objtree.expect("/mob")) {
                    self.env.impure_procs.insert_violator(
                        self.proc_ref,
                        "LShift onto mob",
                        location,
                    );
                } else if lty.static_ty == StaticType::Type(self.objtree.expect("/savefile")) {
                    self.env.impure_procs.insert_violator(
                        self.proc_ref,
                        "LShift onto savefile",
                        location,
                    );
                } else if lty.static_ty == StaticType::Type(self.objtree.expect("/list")) {
                    self.env.impure_procs.insert_violator(
                        self.proc_ref,
                        "LShift onto list",
                        location,
                    );
                }

                let rty = self.visit_expression(location, rhs, None, local_vars);
                self.visit_binary(lty, rty, BinaryOp::LShift)
            }
            Expression::BinaryOp {
                op: BinaryOp::In,
                lhs,
                rhs,
            } => {
                // check for incorrect/ambiguous in statements
                match &**lhs {
                    Expression::Base { term, follow } => {
                        for each in follow.iter() {
                            if let Follow::Unary(unary) = each.elem {
                                error(
                                    location,
                                    format!("ambiguous `{}` on left side of an `in`", unary.name()),
                                )
                                .set_severity(Severity::Warning)
                                .with_errortype("ambiguous_in_lhs")
                                .with_note(
                                    location,
                                    format!(
                                        "add parentheses to fix: `{}`",
                                        unary.around("(a in b)")
                                    ),
                                )
                                .with_note(
                                    location,
                                    format!(
                                        "add parentheses to disambiguate: `({}) in b`",
                                        unary.around("a")
                                    ),
                                )
                                .register(self.context);
                                break;
                            }
                        }
                    }
                    Expression::BinaryOp { op, lhs, rhs } => {
                        error(
                            location,
                            format!("ambiguous `{}` on left side of an `in`", op),
                        )
                        .set_severity(Severity::Warning)
                        .with_errortype("ambiguous_in_lhs")
                        .with_note(
                            location,
                            format!("add parentheses to fix: `a {} (b in c)`", op),
                        )
                        .with_note(
                            location,
                            format!("add parentheses to disambiguate: `(a {} b) in c`", op),
                        )
                        .register(self.context);
                    }
                    Expression::AssignOp { op, lhs, rhs } => {
                        error(
                            location,
                            format!("ambiguous `{}` on left side of an `in`", op),
                        )
                        .set_severity(Severity::Warning)
                        .with_errortype("ambiguous_in_lhs")
                        .with_note(
                            location,
                            format!("add parentheses to fix: `a {} (b in c)`", op),
                        )
                        .with_note(
                            location,
                            format!("add parentheses to disambiguate: `(a {} b) in c`", op),
                        )
                        .register(self.context);
                    }
                    Expression::TernaryOp { cond, if_, else_ } => {
                        error(
                            location,
                            "ambiguous ternary on left side of an `in`".to_string(),
                        )
                        .set_severity(Severity::Warning)
                        .with_errortype("ambiguous_in_lhs")
                        .with_note(location, "add parentheses to fix: `a ? b : (c in d)`")
                        .with_note(
                            location,
                            "add parentheses to disambiguate: `(a ? b : c) in d`",
                        )
                        .register(self.context);
                    }
                };
                let lty = self.visit_expression(location, lhs, None, local_vars);
                let rty = self.visit_expression(location, rhs, None, local_vars);
                self.visit_binary(lty, rty, BinaryOp::In)
            }
            Expression::BinaryOp {
                op: BinaryOp::Or,
                lhs,
                rhs,
            } => {
                // It appears that DM does this in more cases than this, but
                // this is the only case I've seen it used in the wild.
                // ex: var/datum/cache_entry/E = cache[key] || new
                let lty = self.visit_expression(location, lhs, type_hint, local_vars);
                let rty = self.visit_expression(location, rhs, type_hint, local_vars);
                self.visit_binary(lty, rty, BinaryOp::Or)
            }
            Expression::BinaryOp { op, lhs, rhs } => {
                let lty = self.visit_expression(location, lhs, None, local_vars);
                let rty = self.visit_expression(location, rhs, None, local_vars);
                match op {
                    BinaryOp::BitAnd => {
                        self.check_negated_bitwise(lhs, location, BinaryOp::BitAnd, BinaryOp::And)
                    }
                    BinaryOp::BitOr => {
                        self.check_negated_bitwise(lhs, location, BinaryOp::BitOr, BinaryOp::Or)
                    }
                    BinaryOp::BitXor => {
                        self.check_negated_bitwise(lhs, location, BinaryOp::BitXor, BinaryOp::NotEq)
                    }
                    _ => {}
                }
                self.visit_binary(lty, rty, *op)
            }
            Expression::AssignOp { lhs, rhs, .. } => {
                let lhs = self.visit_expression(location, lhs, None, local_vars);
                if let Some(true) = lhs.is_impure {
                    self.env.impure_procs.insert_violator(
                        self.proc_ref,
                        "Assignment on purity breaking expression",
                        location,
                    );
                }
                self.visit_expression(location, rhs, lhs.static_ty.basic_type(), local_vars)
            }
            Expression::TernaryOp { cond, if_, else_ } => {
                // TODO: be sensible
                self.visit_expression(location, cond, None, local_vars);
                let ty = self.visit_expression(location, if_, type_hint, local_vars);
                self.visit_expression(location, else_, type_hint, local_vars);
                ty
            }
        }
    }

    fn visit_term(
        &mut self,
        location: Location,
        term: &'o Term,
        type_hint: Option<TypeRef<'o>>,
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) -> Analysis<'o> {
        match term {
            Term::Null => Analysis::null(),
            Term::Int(number) => {
                Analysis::from_value(self.objtree, Constant::from(*number), type_hint)
            }
            Term::Float(number) => {
                Analysis::from_value(self.objtree, Constant::from(*number), type_hint)
            }
            Term::String(text) => Analysis::from_value(
                self.objtree,
                Constant::String(text.as_str().into()),
                type_hint,
            ),
            Term::Resource(text) => Analysis::from_value(
                self.objtree,
                Constant::Resource(text.as_str().into()),
                type_hint,
            ),
            Term::As(_) => assumption_set![Assumption::IsNum(true)].into(),

            Term::Ident(unscoped_name) => {
                if let Some(var) = local_vars.get(unscoped_name) {
                    return var
                        .analysis
                        .clone()
                        .with_fix_hint(var.location, "add additional type info here");
                }
                if let Some(decl) = self.ty.get_var_declaration(unscoped_name) {
                    //println!("found type var");
                    let mut ana = self
                        .static_type(location, &decl.var_type.type_path)
                        .with_fix_hint(decl.location, "add additional type info here");
                    ana.is_impure = Some(true);
                    ana
                } else {
                    error(location, format!("undefined var: {:?}", unscoped_name))
                        .register(self.context);
                    Analysis::empty()
                }
            }

            Term::Expr(expr) => self.visit_expression(location, expr, type_hint, local_vars),
            Term::Prefab(prefab) => {
                if let Some(nav) = self.ty.navigate_path(&prefab.path) {
                    let ty = nav.ty(); // TODO: handle proc/verb paths here
                    let pop = dm::constants::Pop::from(
                        ty.path
                            .split('/')
                            .skip(1)
                            .map(ToOwned::to_owned)
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                    );
                    Analysis {
                        static_ty: StaticType::None,
                        aset: assumption_set![Assumption::IsPath(true, nav.ty())],
                        value: Some(Constant::Prefab(Box::new(pop))),
                        fix_hint: None,
                        is_impure: None,
                    }
                } else {
                    error(
                        location,
                        format!("failed to resolve path {}", FormatTypePath(&prefab.path)),
                    )
                    .register(self.context);
                    Analysis::empty()
                }
            }
            Term::InterpString(_, parts) => {
                for (ref expr, _) in parts.iter() {
                    if let Some(expr) = expr {
                        self.visit_expression(location, expr, None, local_vars);
                    }
                }
                assumption_set![Assumption::IsText(true)].into()
            }

            Term::Call(unscoped_name, args) => {
                if self.inside_newcontext == 0
                    && matches!(
                        unscoped_name.as_str(),
                        "sleep" | "alert" | "shell" | "winexists" | "winget"
                    )
                {
                    self.env
                        .sleeping_procs
                        .insert_violator(self.proc_ref, unscoped_name, location);
                }
                self.check_type_sleepers(self.ty, location, unscoped_name);
                let src = self.ty;
                if let Some(proc) = self.ty.get_proc(unscoped_name) {
                    self.visit_call(location, src, proc, args, false, local_vars)
                } else if unscoped_name == "SpacemanDMM_unlint" {
                    // Escape hatch for cases like `src` in macros used in
                    // global procs.
                    Analysis::empty()
                } else if unscoped_name == "SpacemanDMM_debug" {
                    eprintln!("SpacemanDMM_debug:");
                    for arg in args.iter() {
                        eprintln!(
                            "    {:?}",
                            self.visit_expression(location, arg, None, local_vars)
                        );
                    }
                    Analysis::empty()
                } else {
                    error(
                        location,
                        format!("undefined proc: {:?} on {}", unscoped_name, self.ty),
                    )
                    .register(self.context);
                    Analysis::empty()
                }
            }
            Term::SelfCall(args) => {
                let src = self.ty;
                let proc = self.proc_ref;
                // Self calls are exact, and won't ever call an override.
                self.visit_call(location, src, proc, args, true, local_vars)
            }
            Term::ParentCall(args) => {
                self.calls_parent = true;
                if let Some(proc) = self.proc_ref.parent_proc() {
                    // TODO: if args are empty, call w/ same args
                    let src = self.ty;
                    // Parent calls are exact, and won't ever call an override.
                    self.visit_call(location, src, proc, args, true, local_vars)
                } else {
                    error(location, format!("proc has no parent: {}", self.proc_ref))
                        .with_errortype("proc_has_no_parent")
                        .register(self.context);
                    Analysis::empty()
                }
            }

            Term::NewImplicit { args } => {
                if let Some(hint) = type_hint {
                    self.visit_new(location, hint, args, local_vars)
                } else {
                    error(location, "no type hint available on implicit new()")
                        .with_errortype("no_typehint_implicit_new")
                        .register(self.context);
                    Analysis::empty()
                }
            }
            Term::NewPrefab { prefab, args } => {
                if let Some(nav) = self.ty.navigate_path(&prefab.path) {
                    // TODO: handle proc/verb paths here
                    self.visit_new(location, nav.ty(), args, local_vars)
                } else {
                    error(
                        location,
                        format!("failed to resolve path {}", FormatTypePath(&prefab.path)),
                    )
                    .register(self.context);
                    Analysis::empty()
                }
            }
            Term::NewMiniExpr { .. } => {
                // TODO: evaluate
                Analysis::empty()
            }

            Term::List(args) => {
                self.visit_arguments(location, args, local_vars);
                Analysis::from_static_type(self.objtree.expect("/list"))
            }
            Term::Input {
                args,
                input_type,
                in_list,
            } => {
                if self.inside_newcontext == 0 {
                    self.env
                        .sleeping_procs
                        .insert_violator(self.proc_ref, "input", location);
                }
                // TODO: deal with in_list
                self.visit_arguments(location, args, local_vars);
                if let Some(ref expr) = in_list {
                    self.visit_expression(location, expr, None, local_vars);
                }

                let input_type = input_type.unwrap_or_else(InputType::empty);
                let without_null = input_type - InputType::NULL;
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
                } else if without_null == InputType::TEXT
                    || without_null == InputType::MESSAGE
                    || without_null == InputType::KEY
                    || without_null == InputType::PASSWORD
                    || without_null == InputType::COLOR
                    || without_null.is_empty()
                {
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
            }
            Term::Locate { args, in_list } => {
                self.visit_arguments(location, args, local_vars);
                if let Some(ref expr) = in_list {
                    self.visit_expression(location, expr, None, local_vars);
                }

                if args.len() == 3 {
                    // X,Y,Z - it's gotta be a turf
                    assumption_set![Assumption::IsType(true, self.objtree.expect("/turf"))].into()
                } else {
                    Analysis::empty()
                }
            }
            Term::Pick(choices) => {
                for (weight, choice) in choices.iter() {
                    if let Some(ref weight) = weight {
                        self.visit_expression(location, weight, None, local_vars);
                    }
                    self.visit_expression(location, choice, None, local_vars);
                }

                // TODO: common superset of all choices
                Analysis::empty()
            }
            Term::DynamicCall(lhs_args, rhs_args) => {
                self.visit_arguments(location, lhs_args, local_vars);
                self.visit_arguments(location, rhs_args, local_vars);
                Analysis::empty() // TODO
            }
        }
    }

    fn visit_new(
        &mut self,
        location: Location,
        typepath: TypeRef<'o>,
        args: &'o Option<Box<[Expression]>>,
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) -> Analysis<'o> {
        if let Some(new_proc) = typepath.get_proc("New") {
            self.visit_call(
                location,
                typepath,
                new_proc,
                args.as_ref().map_or(&[], |v| &v[..]),
                // New calls are exact: `new /datum()` will always call
                // `/datum/New()` and never an override.
                true,
                local_vars,
            );
        } else if typepath.path != "/list" {
            error(
                location,
                format!("couldn't find {}/proc/New", typepath.path),
            )
            .register(self.context);
        }
        assumption_set![Assumption::IsType(true, typepath)].into()
    }

    fn check_type_sleepers(&mut self, ty: TypeRef<'o>, location: Location, unscoped_name: &str) {
        match ty.get().path.as_str() {
            "/client" => {
                if self.inside_newcontext == 0
                    && matches!(unscoped_name, "SoundQuery" | "MeasureText")
                {
                    self.env.sleeping_procs.insert_violator(
                        self.proc_ref,
                        format!("client.{}", unscoped_name).as_str(),
                        location,
                    );
                }
            }
            "/world" => {
                if self.inside_newcontext == 0 && matches!(unscoped_name, "Import" | "Export") {
                    self.env.sleeping_procs.insert_violator(
                        self.proc_ref,
                        format!("world.{}", unscoped_name).as_str(),
                        location,
                    );
                }
            }
            _ => {}
        }
    }

    fn visit_follow(
        &mut self,
        location: Location,
        lhs: Analysis<'o>,
        rhs: &'o Follow,
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) -> Analysis<'o> {
        match rhs {
            Follow::Unary(op) => self.visit_unary(lhs, op, location, local_vars),

            Follow::Field(PropertyAccessKind::Colon, _) => Analysis::empty(),
            Follow::Field(PropertyAccessKind::SafeColon, _) => Analysis::empty(),
            Follow::Call(PropertyAccessKind::Colon, _, args)
            | Follow::Call(PropertyAccessKind::SafeColon, _, args) => {
                // No analysis yet, but be sure to visit the arguments
                for arg in args.iter() {
                    let mut argument_value = arg;
                    if let Expression::AssignOp {
                        op: AssignOp::Assign,
                        lhs,
                        rhs,
                    } = arg
                    {
                        match lhs.as_term() {
                            Some(Term::Ident(name)) | Some(Term::String(name)) => {
                                // Don't visit_expression the kwarg key.
                                argument_value = rhs;
                            }
                            _ => {}
                        }
                    }
                    self.visit_expression(location, argument_value, None, local_vars);
                }
                Analysis::empty()
            }

            Follow::Index(_, expr) => {
                self.visit_expression(location, expr, None, local_vars);
                // TODO: differentiate between L[1] and L[non_numeric_key]
                match lhs.static_ty {
                    StaticType::List { keys, .. } => {
                        let mut res = Analysis::from(*keys);
                        if let Some((loc, _)) = lhs.fix_hint {
                            res.fix_hint =
                                Some((loc, "add a type annotation after /list here".to_owned()))
                        }
                        res
                    }
                    _ => lhs.clone(), // carry through fix_hint
                }
            }
            Follow::Field(kind, name) => {
                if let Some(ty) = lhs.static_ty.basic_type() {
                    if let Some(decl) = ty.get_var_declaration(name) {
                        if ty != self.ty && decl.var_type.flags.is_private() {
                            error(
                                location,
                                format!("field {:?} on {} is declared as private", name, ty),
                            )
                            .with_errortype("private_var")
                            .set_severity(Severity::Warning)
                            .with_note(decl.location, "definition is here")
                            .register(self.context);
                        } else if !self.ty.is_subtype_of(ty.get())
                            && decl.var_type.flags.is_protected()
                        {
                            error(
                                location,
                                format!("field {:?} on {} is declared as protected", name, ty),
                            )
                            .with_errortype("protected_var")
                            .set_severity(Severity::Warning)
                            .with_note(decl.location, "definition is here")
                            .register(self.context);
                        }
                        self.static_type(location, &decl.var_type.type_path)
                            .with_fix_hint(decl.location, "add additional type info here")
                    } else {
                        error(location, format!("undefined field: {:?} on {}", name, ty))
                            .register(self.context);
                        Analysis::empty()
                    }
                } else {
                    error(
                        location,
                        format!("field access requires static type: {:?}", name),
                    )
                    .set_severity(Severity::Warning)
                    .with_errortype("field_access_static_type")
                    .with_fix_hint(&lhs)
                    .register(self.context);
                    Analysis::empty()
                }
            }
            Follow::Call(kind, name, arguments) => {
                if let Some(ty) = lhs.static_ty.basic_type() {
                    self.check_type_sleepers(ty, location, name);
                    if let Some(proc) = ty.get_proc(name) {
                        if let Some((privateproc, true, decllocation)) =
                            self.env.private.get_self_or_parent(proc)
                        {
                            if ty != privateproc.ty() {
                                error(
                                    location,
                                    format!(
                                        "{} attempting to call private proc {}, types do not match",
                                        self.proc_ref, privateproc
                                    ),
                                )
                                .with_errortype("private_proc")
                                .with_note(
                                    decllocation,
                                    "prohibited by this private_proc annotation",
                                )
                                .register(self.context);
                                return Analysis::empty(); // dont double up with visit_call()
                            }
                        }
                        if let Some((protectedproc, true, decllocation)) =
                            self.env.protected.get_self_or_parent(proc)
                        {
                            if !self.ty.is_subtype_of(protectedproc.ty().get()) {
                                error(
                                    location,
                                    format!(
                                        "{} attempting to call protected proc {}",
                                        self.proc_ref, protectedproc
                                    ),
                                )
                                .with_errortype("protected_proc")
                                .with_note(
                                    decllocation,
                                    "prohibited by this protected_proc annotation",
                                )
                                .register(self.context);
                            }
                        }
                        self.visit_call(location, ty, proc, arguments, false, local_vars)
                    } else {
                        error(location, format!("undefined proc: {:?} on {}", name, ty))
                            .register(self.context);
                        Analysis::empty()
                    }
                } else {
                    error(
                        location,
                        format!("proc call requires static type: {:?}", name),
                    )
                    .set_severity(Severity::Warning)
                    .with_errortype("proc_call_static_type")
                    .with_fix_hint(&lhs)
                    .register(self.context);
                    Analysis::empty()
                }
            }
        }
    }

    // checks operatorX overloads on types
    fn check_operator_overload(
        &mut self,
        rhs: Analysis<'o>,
        location: Location,
        operator: &str,
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) -> Analysis<'o> {
        if let Some(impurity) = rhs.is_impure {
            if impurity {
                self.env.impure_procs.insert_violator(
                    self.proc_ref,
                    &format!("{} done on non-local var", operator),
                    location,
                );
            }
        }
        let typeerror;
        match rhs.static_ty {
            StaticType::None => return Analysis::empty(),
            StaticType::Type(typeref) => {
                // Its been overloaded, assume they really know they want to do this
                if let Some(proc) = typeref.get_proc(operator) {
                    return self.visit_call(location, typeref, proc, &[], true, local_vars);
                }
                typeerror = typeref.get().pretty_path();
            }
            StaticType::List { list, .. } => {
                typeerror = "list";
            }
        };
        error(
            location,
            format!(
                "Attempting {} on a {} which does not overload {}",
                operator, typeerror, operator
            ),
        )
        .register(self.context);
        Analysis::empty()
    }

    fn visit_unary(
        &mut self,
        rhs: Analysis<'o>,
        op: &UnaryOp,
        location: Location,
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) -> Analysis<'o> {
        match op {
            // !x just evaluates the "truthiness" of x and negates it, returning 1 or 0
            UnaryOp::Not => Analysis::from(assumption_set![Assumption::IsNum(true)]),
            UnaryOp::PreIncr | UnaryOp::PostIncr => {
                self.check_operator_overload(rhs, location, "operator++", local_vars)
            }
            UnaryOp::PreDecr | UnaryOp::PostDecr => {
                self.check_operator_overload(rhs, location, "operator--", local_vars)
            }
            /*
            (UnaryOp::Neg, Type::Number) => Type::Number.into(),
            (UnaryOp::BitNot, Type::Number) => Type::Number.into(),
            */
            _ => Analysis::empty(),
        }
    }

    // checks for bitwise operations on a negated LHS
    fn check_negated_bitwise(
        &mut self,
        lhs: &dm::ast::Expression,
        location: Location,
        bit_op: BinaryOp,
        bool_op: BinaryOp,
    ) {
        guard!(let Expression::Base { follow, .. } = lhs else { return });
        let any_not = follow
            .iter()
            .any(|f| matches!(f.elem, Follow::Unary(UnaryOp::Not)));
        if any_not {
            error(
                location,
                format!(
                    "Ambiguous `!` on left side of bitwise `{}` operator",
                    bit_op
                ),
            )
            .with_errortype("ambiguous_not_bitwise")
            .set_severity(Severity::Warning)
            .with_note(location, format!("Did you mean `!(x {} y)`?", bit_op))
            .with_note(location, format!("Did you mean `!x {} y`?", bool_op))
            .with_note(location, format!("Did you mean `~x {} y`?", bit_op))
            .register(self.context);
        }
    }

    fn visit_binary(&mut self, lhs: Analysis<'o>, rhs: Analysis<'o>, op: BinaryOp) -> Analysis<'o> {
        //println!("visit_binary: don't know anything about {}", op);
        if lhs.static_ty.is_list() {
            // If the LHS of these operators is a list, so is the result.
            match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::BitOr
                | BinaryOp::BitAnd
                | BinaryOp::BitXor => return lhs.static_ty.into(),
                _ => {}
            }
        }
        Analysis::empty()
    }

    // It's fine.
    #[allow(clippy::too_many_arguments)]
    fn check_filter_flag(
        &mut self,
        expr: &'o Expression,
        can_be_zero: bool,
        location: Location,
        typevalue: &str,
        valid_flags: &[&str],
        flagfieldname: &str,
        exclusive: bool,
    ) {
        match expr {
            Expression::BinaryOp {
                op: BinaryOp::BitOr,
                lhs,
                rhs,
            } => {
                if exclusive {
                    error(location, format!("filter(type=\"{}\") '{}' parameter must have one value, found bitwise OR", typevalue, flagfieldname))
                        .with_filter_args(location, typevalue)
                        .register(self.context);
                    return;
                }
                // recurse
                self.check_filter_flag(
                    lhs,
                    can_be_zero,
                    location,
                    typevalue,
                    valid_flags,
                    flagfieldname,
                    exclusive,
                );
                self.check_filter_flag(
                    rhs,
                    can_be_zero,
                    location,
                    typevalue,
                    valid_flags,
                    flagfieldname,
                    exclusive,
                );
            }
            Expression::Base { term, follow } => {
                if follow.len() > 0 {
                    error(
                        location,
                        "filter() flag fields cannot have unary ops or field accesses",
                    )
                    .register(self.context);
                    return;
                }
                match &term.elem {
                    Term::Ident(flagname) => {
                        if !valid_flags.iter().any(|&x| x == flagname) {
                            error(
                                location,
                                format!(
                                    "filter(type=\"{}\") called with invalid '{}' flag '{}'",
                                    typevalue, flagfieldname, flagname
                                ),
                            )
                            .with_filter_args(location, typevalue)
                            .register(self.context);
                        }
                    }
                    Term::Int(0) if can_be_zero => {}
                    other => {
                        error(
                            location,
                            format!(
                                "filter(type=\"{}\") called with invalid '{}' value '{:?}'",
                                typevalue, flagfieldname, other
                            ),
                        )
                        .with_filter_args(location, typevalue)
                        .register(self.context);
                    }
                }
            }
            _ => {
                error(
                    location,
                    format!(
                        "filter(type=\"{}\"), extremely invalid value passed to '{}' field",
                        typevalue, flagfieldname
                    ),
                )
                .with_filter_args(location, typevalue)
                .register(self.context);
            }
        }
    }

    fn visit_call(
        &mut self,
        location: Location,
        src: TypeRef<'o>,
        proc: ProcRef<'o>,
        args: &'o [Expression],
        is_exact: bool,
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) -> Analysis<'o> {
        self.env.call_tree.entry(self.proc_ref).or_default().push((
            proc,
            location,
            self.inside_newcontext != 0,
        ));
        if let Some((privateproc, true, decllocation)) = self.env.private.get_self_or_parent(proc) {
            if self.ty != privateproc.ty() {
                error(
                    location,
                    format!(
                        "{} attempting to call private proc {}, types do not match",
                        self.proc_ref, privateproc
                    ),
                )
                .with_errortype("private_proc")
                .with_note(decllocation, "prohibited by this private_proc annotation")
                .register(self.context);
            }
        }

        // identify and register kwargs used
        let mut any_kwargs_yet = false;

        let mut param_name_map = HashMap::with_hasher(RandomState::default());
        let mut param_expr_map = HashMap::with_hasher(RandomState::default());
        let mut param_idx_map = HashMap::with_hasher(RandomState::default());
        let mut param_idx = 0;
        let mut arglist_used = false;

        for arg in args {
            let mut argument_value = arg;
            let mut this_kwarg = None;
            match arg {
                Expression::AssignOp {
                    op: AssignOp::Assign,
                    lhs,
                    rhs,
                } => {
                    match lhs.as_term() {
                        Some(Term::Ident(name)) | Some(Term::String(name)) => {
                            // Don't visit_expression the kwarg key.
                            any_kwargs_yet = true;
                            this_kwarg = Some(name);
                            argument_value = rhs;

                            // Check that that kwarg actually exists.
                            if !proc.parameters.iter().any(|p| p.name == *name) {
                                // Search for a child proc that does have this keyword argument.
                                let mut error = error(
                                    location,
                                    format!("bad keyword argument {:?} to {}", name, proc),
                                );
                                proc.recurse_children(&mut |child_proc| {
                                    if child_proc.ty() == proc.ty() {
                                        return;
                                    }
                                    if child_proc.parameters.iter().any(|p| p.name == *name) {
                                        error.add_note(
                                            child_proc.location,
                                            format!(
                                                "an override has this parameter: {}",
                                                child_proc
                                            ),
                                        );
                                    }
                                });
                                error.register(self.context);
                            } else if !is_exact {
                                // If it does, mark it as "used".
                                // Format with src/proc/foo here, rather than the
                                // type the proc actually appears on, so that
                                // calling /datum/foo() on a /datum/A won't
                                // complain about /datum/B/foo().
                                self.env
                                    .used_kwargs
                                    .entry(format!("{}/proc/{}", src, proc.name()))
                                    .or_insert_with(|| KwargInfo {
                                        location: proc.location,
                                        ..Default::default()
                                    })
                                    .called_at
                                    // TODO: use a more accurate location
                                    .entry(name.clone())
                                    .and_modify(|ca| ca.others += 1)
                                    .or_insert(CalledAt {
                                        location,
                                        others: 0,
                                    });
                            }
                        }
                        _ => {}
                    }
                }
                expr => {
                    if let Some(Term::Call(callname, _)) = expr.as_term() {
                        // only interested in the first expression being arglist
                        if callname.as_str() == "arglist"
                            && param_name_map.is_empty()
                            && param_idx == 0
                        {
                            arglist_used = true;
                        }
                    }
                }
            }

            if any_kwargs_yet
                && this_kwarg.is_none()
                && !(proc.ty().is_root() && proc.name() == "animate")
            {
                // TODO: don't hardcode the animate() exception
                error(
                    location,
                    format!(
                        "proc called with non-kwargs after kwargs: {}()",
                        proc.name()
                    ),
                )
                .register(self.context);
            }

            let analysis = self.visit_expression(location, argument_value, None, local_vars);
            if let Some(kw) = this_kwarg {
                param_name_map.insert(kw.as_str(), analysis);
                param_expr_map.insert(kw.as_str(), argument_value);
            } else {
                param_idx_map.insert(param_idx, analysis);
                param_idx += 1;
            }
        }

        // filter call checking
        // TODO: some filters have limits for their numerical params
        //  eg "rays" type "threshold" param defaults to 0.5, can be 0 to 1
        if proc.ty().is_root() && proc.name() == "filter" {
            guard!(let Some(typename) = param_name_map.get("type") else {
                if !arglist_used {
                    error(location, "filter() called without mandatory keyword parameter 'type'")
                        .register(self.context);
                } // regardless, we're done here
                return Analysis::empty()
            });
            guard!(let Some(Constant::String(typevalue)) = &typename.value else {
                error(location, format!("filter() called with non-string type keyword parameter value '{:?}'", typename.value))
                    .register(self.context);
                return Analysis::empty()
            });
            guard!(let Some(arglist) = VALID_FILTER_TYPES.get(typevalue) else {
                error(location, format!("filter() called with invalid type keyword parameter value '{}'", typevalue))
                    .register(self.context);
                return Analysis::empty()
            });
            for arg in param_name_map.keys() {
                if *arg != "type" && !arglist.iter().any(|&x| x == *arg) {
                    error(
                        location,
                        format!(
                            "filter(type=\"{}\") called with invalid keyword parameter '{}'",
                            typevalue, arg
                        ),
                    )
                    .with_filter_args(location, typevalue)
                    .register(self.context);
                }
            }
            if let Some((flagfieldname, exclusive, can_be_zero, valid_flags)) =
                VALID_FILTER_FLAGS.get(typevalue)
            {
                if let Some(flagsvalue) = param_expr_map.get(flagfieldname) {
                    self.check_filter_flag(
                        flagsvalue,
                        *can_be_zero,
                        location,
                        typevalue,
                        valid_flags,
                        flagfieldname,
                        *exclusive,
                    );
                }
            }
        }

        if proc.ty().is_root() && proc.is_builtin() {
            Analysis::from(self.global_builtin_returntype(proc))
        } else if let Some(return_type) = self.env.return_type.get(&proc) {
            let ec = type_expr::TypeExprContext {
                objtree: self.objtree,
                param_name_map,
                param_idx_map,
            };
            match return_type.evaluate(location, &ec) {
                Ok(st) => {
                    let hint = format!("return type evaluated to {:?}", st);
                    Analysis::from(st).with_fix_hint(location, hint)
                }
                Err(err) => {
                    err.with_component(dm::Component::DreamChecker)
                        .register(self.context);
                    Analysis::empty()
                }
            }
        } else {
            Analysis::empty().with_fix_hint(
                proc.location,
                format!("add a return type annotation to {}", proc),
            )
        }
    }

    fn visit_arguments(
        &mut self,
        location: Location,
        args: &'o [Expression],
        local_vars: &mut HashMap<String, LocalVar<'o>, RandomState>,
    ) {
        for arg in args {
            let mut argument_value = arg;
            if let Expression::AssignOp {
                op: AssignOp::Assign,
                lhs,
                rhs,
            } = arg
            {
                match lhs.as_term() {
                    Some(Term::Ident(_name)) | Some(Term::String(_name)) => {
                        // Don't visit_expression the kwarg key.
                        argument_value = rhs;
                    }
                    _ => {}
                }
            }

            self.visit_expression(location, argument_value, None, local_vars);
        }
    }

    fn static_type(&mut self, location: Location, of: &[String]) -> Analysis<'o> {
        Analysis::from(self.env.static_type(location, of))
    }

    fn global_builtin_returntype(&mut self, proc: ProcRef) -> StaticType<'o> {
        match proc.name() {
            "argslist" => StaticType::plain_list(self.objtree),
            "block" => StaticType::list_of_type(self.objtree, "/turf"),
            "bounds" => StaticType::list_of_type(self.objtree, "/atom"),
            "flist" => StaticType::plain_list(self.objtree),
            "get_step" => StaticType::Type(self.objtree.expect("/turf")),
            "hearers" => StaticType::list_of_type(self.objtree, "/mob"),
            "icon" => StaticType::Type(self.objtree.expect("/icon")),
            "icon_states" => StaticType::plain_list(self.objtree),
            "matrix" => StaticType::Type(self.objtree.expect("/matrix")),
            "obounds" => StaticType::list_of_type(self.objtree, "/atom"),
            "ohearers" => StaticType::list_of_type(self.objtree, "/mob"),
            "orange" => StaticType::list_of_type(self.objtree, "/atom"),
            "oview" => StaticType::list_of_type(self.objtree, "/atom"),
            "oviewers" => StaticType::list_of_type(self.objtree, "/mob"),
            "range" => StaticType::list_of_type(self.objtree, "/atom"),
            "regex" => StaticType::Type(self.objtree.expect("/regex")),
            "splittext" => StaticType::plain_list(self.objtree),
            "typesof" => StaticType::plain_list(self.objtree),
            "view" => StaticType::list_of_type(self.objtree, "/atom"),
            "viewers" => StaticType::list_of_type(self.objtree, "/mob"),
            _ => StaticType::None,
        }
    }
}
