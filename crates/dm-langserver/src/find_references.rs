//! The symbol table used for "Find References" support.

use foldhash::{HashMap, HashMapExt};

use dm::ast::*;
use dm::objtree::*;
use dm::Location;

pub struct ReferencesTable {
    uses: HashMap<SymbolId, References>,
    symbols: SymbolIdSource,
}

#[derive(Default)]
struct References {
    references: Vec<Location>,
    implementations: Vec<Location>,
}

impl ReferencesTable {
    pub fn new(objtree: &ObjectTree) -> Self {
        let mut tab = ReferencesTable {
            uses: HashMap::new(),
            symbols: SymbolIdSource::new(SymbolIdCategory::LocalVars),
        };

        // Insert the "definition" locations for the types and such
        objtree.root().recurse(&mut |ty| {
            tab.uses.insert(ty.id, References {
                references: vec![],
                implementations: vec![ty.location],
            });
            for (name, var) in ty.vars.iter() {
                if let Some(decl) = ty.get_var_declaration(name) {
                    tab.impl_symbol(decl.id, var.value.location);
                }
            }
            for (name, proc) in ty.procs.iter() {
                if let Some(decl) = ty.get_proc_declaration(name) {
                    tab.impl_symbol(decl.id, proc.value.first().unwrap().location);
                }
            }
        });

        objtree.root().recurse(&mut |ty| {
            for (name, var) in ty.vars.iter() {
                if let Some(ref expr) = var.value.expression {
                    let mut walk = WalkProc::from_ty(&mut tab, objtree, ty);
                    let type_hint = match ty.get_var_declaration(name) {
                        Some(decl) => walk.static_type(decl.location, &decl.var_type.type_path).basic_type(),
                        None => None,
                    };
                    walk.visit_expression(var.value.location, expr, type_hint);
                }
            }

            for proc in ty.iter_self_procs() {
                if let Some(ref code) = proc.code {
                    WalkProc::from_proc(&mut tab, objtree, proc).run(proc, code);
                }
            }
        });

        // Sublime Text client does not sort these itself, so sort them here.
        for value in tab.uses.values_mut() {
            value.references.sort();
            value.implementations.sort();
        }

        tab
    }

    pub fn find_references(&self, symbol: SymbolId, _declaration: bool) -> &[Location] {
        match self.uses.get(&symbol) {
            None => &[],
            Some(list) => &list.references,
        }
    }

    pub fn find_implementations(&self, symbol: SymbolId) -> &[Location] {
        match self.uses.get(&symbol) {
            None => &[],
            Some(list) => &list.implementations,
        }
    }

    fn new_symbol(&mut self, location: Location) -> SymbolId {
        let id = self.symbols.allocate();
        self.uses.insert(id, References {
            references: vec![location],
            implementations: vec![],
        });
        id
    }

    fn use_symbol(&mut self, symbol: SymbolId, location: Location) {
        self.uses.entry(symbol).or_default().references.push(location);
    }

    fn impl_symbol(&mut self, symbol: SymbolId, location: Location) {
        self.uses.entry(symbol).or_default().implementations.push(location);
    }
}

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

struct Local<'o> {
    ty: StaticType<'o>,
    symbol: SymbolId,
}

struct WalkProc<'o> {
    tab: &'o mut ReferencesTable,
    objtree: &'o ObjectTree,
    ty: TypeRef<'o>,
    proc: Option<ProcRef<'o>>,
    local_vars: HashMap<String, Local<'o>>,
}

impl<'o> WalkProc<'o> {
    fn from_proc(tab: &'o mut ReferencesTable, objtree: &'o ObjectTree, proc: ProcRef<'o>) -> Self {
        let mut local_vars = HashMap::new();
        local_vars.insert("global".to_owned(), Local {
            ty: StaticType::Type(objtree.root()),
            symbol: objtree.root().id,
        });
        local_vars.insert(".".to_owned(), Local {
            ty: StaticType::None,
            symbol: tab.new_symbol(proc.location),
        });
        local_vars.insert("args".to_owned(), Local {
            ty: StaticType::Type(objtree.expect("/list")),
            symbol: tab.new_symbol(proc.location),
        });
        local_vars.insert("usr".to_owned(), Local {
            ty: StaticType::Type(objtree.expect("/mob")),
            symbol: tab.new_symbol(proc.location),
        });

        let ty = proc.ty();
        if !ty.is_root() {
            local_vars.insert("src".to_owned(), Local {
                ty: StaticType::Type(ty),
                symbol: tab.new_symbol(proc.location),
            });
        }

        WalkProc {
            tab,
            objtree,
            ty: proc.ty(),
            proc: Some(proc),
            local_vars
        }
    }

    fn from_ty(tab: &'o mut ReferencesTable, objtree: &'o ObjectTree, ty: TypeRef<'o>) -> Self {
        let mut local_vars = HashMap::new();
        local_vars.insert("global".to_owned(), Local {
            ty: StaticType::Type(objtree.root()),
            symbol: objtree.root().id,
        });

        WalkProc {
            tab,
            objtree,
            ty,
            proc: None,
            local_vars
        }
    }

    pub fn run(&mut self, proc: ProcRef<'o>, block: &'o [Spanned<Statement>]) {
        for param in proc.get().parameters.iter() {
            let ty = self.static_type(param.location, &param.var_type.type_path);
            self.use_type(param.location, &ty);
            if let Some(expr) = &param.default {
                self.visit_expression(param.location, expr, None);
            }
            self.local_vars.insert(param.name.to_owned(), Local {
                ty,
                symbol: self.tab.new_symbol(param.location)
            });
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
            Statement::Return(expr) => {
                let dot = self.local_vars.get(".").unwrap().symbol;
                self.tab.use_symbol(dot, location);
                if let Some(expr) = expr {
                    self.visit_expression(location, expr, None);
                }
            },
            Statement::Throw(expr) => { self.visit_expression(location, expr, None); },
            Statement::While { condition, block } => {
                self.visit_expression(location, condition, None);
                self.visit_block(block);
            },
            Statement::DoWhile { block, condition } => {
                self.visit_block(block);
                self.visit_expression(condition.location, &condition.elem, None);
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
            Statement::ForInfinite { block } => {
                self.visit_block(block);
            }
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
            Statement::ForList(for_list) => {
                let ForListStatement { var_type, name, in_list, block, .. } = &**for_list;
                if let Some(in_list) = in_list {
                    self.visit_expression(location, in_list, None);
                }
                if let Some(var_type) = var_type {
                    self.visit_var(location, var_type, name, None);
                }
                self.visit_block(block);
            },
            Statement::ForRange(for_range) => {
                let ForRangeStatement { var_type, name, start, end, step, block } = &**for_range;
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
                for (case, ref block) in cases.iter() {
                    for case_part in case.elem.iter() {
                        match case_part {
                            dm::ast::Case::Exact(expr) => { self.visit_expression(case.location, expr, None); },
                            dm::ast::Case::Range(start, end) => {
                                self.visit_expression(case.location, start, None);
                                self.visit_expression(case.location, end, None);
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
            Statement::Crash(_) => {},
            Statement::Label { name: _, block } => self.visit_block(block),
            Statement::Del(expr) => { self.visit_expression(location, expr, None); },
            Statement::ForKeyValue(for_key_value) => {
                let ForKeyValueStatement { var_type, key, value, in_list, block } = &**for_key_value;
                if let Some(in_list) = in_list {
                    self.visit_expression(location, in_list, None);
                }
                if let Some(var_type) = var_type {
                    self.visit_var(location, var_type, key, None);
                }
                // the "v" in a DM for (var/k, v) statement is essentially typeless.
                // There is currently no way to change that.
                let var_type_value = VarType {
                    flags: VarTypeFlags::from_bits_truncate(0),
                    type_path: Box::new([]),
                    input_type: InputType::from_bits_truncate(0),
                };
                self.visit_var(location, &var_type_value, value, None);
                self.visit_block(block);
            },
        }
    }

    fn visit_var_stmt(&mut self, location: Location, var: &'o VarStatement) {
        self.visit_var(location, &var.var_type, &var.name, var.value.as_ref())
    }

    fn visit_var(&mut self, location: Location, var_type: &VarType, name: &str, value: Option<&'o Expression>) {
        let ty = self.static_type(location, &var_type.type_path);
        self.use_type(location, &ty);
        if let Some(expr) = value {
            self.visit_expression(location, expr, ty.basic_type());
        }
        self.local_vars.insert(name.to_owned(), Local {
            ty,
            symbol: self.tab.new_symbol(location),
        });
    }

    fn use_type(&mut self, location: Location, ty: &StaticType<'o>) {
        match ty {
            StaticType::None => {},
            StaticType::Type(ty) => self.tab.use_symbol(ty.id, location),
            StaticType::List { list, keys } => {
                self.tab.use_symbol(list.id, location);
                self.use_type(location, keys);
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn visit_expression(&mut self, location: Location, expression: &'o Expression, type_hint: Option<TypeRef<'o>>) -> StaticType<'o> {
        match expression {
            Expression::Base { term, follow } => {
                let base_type_hint = if follow.is_empty() {
                    type_hint
                } else {
                    None
                };
                let mut ty = self.visit_term(term.location, &term.elem, base_type_hint);
                for each in follow.iter() {
                    ty = self.visit_follow(each.location, ty, &each.elem);
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
                self.visit_expression(location, rhs, lhs.basic_type())
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

    fn visit_ident(&mut self, location: Location, unscoped_name: &'o str) -> StaticType<'o> {
        if let Some(var) = self.local_vars.get(unscoped_name) {
            self.tab.use_symbol(var.symbol, location);
            return var.ty.clone();
        }
        if let Some(decl) = self.ty.get_var_declaration(unscoped_name) {
            self.tab.use_symbol(decl.id, location);
            self.static_type(location, &decl.var_type.type_path)
        } else {
            StaticType::None
        }
    }

    fn visit_term(&mut self, location: Location, term: &'o Term, type_hint: Option<TypeRef<'o>>) -> StaticType<'o> {
        match term {
            Term::Null => StaticType::None,
            Term::Int(_) => StaticType::None,
            Term::Float(_) => StaticType::None,
            Term::String(_) => StaticType::None,
            Term::Resource(_) => StaticType::None,
            Term::As(_) => StaticType::None,

            Term::Expr(expr) => self.visit_expression(location, expr, type_hint),
            Term::Prefab(prefab) => {
                self.visit_prefab(location, prefab);
                StaticType::None
            },
            Term::InterpString(_, parts) => {
                for (ref expr, _) in parts.iter() {
                    if let Some(expr) = expr {
                        self.visit_expression(location, expr, None);
                    }
                }
                StaticType::None
            },

            Term::Ident(unscoped_name) => self.visit_ident(location, unscoped_name),
            Term::Call(unscoped_name, args) => {
                let src = self.ty;
                if let Some(proc) = self.ty.get_proc(unscoped_name) {
                    self.visit_call(location, src, proc, args, false)
                } else {
                    StaticType::None
                }
            },
            Term::SelfCall(args) => {
                if let Some(proc) = self.proc {
                    let src = self.ty;
                    // Self calls are exact, and won't ever call an override.
                    self.visit_call(location, src, proc, args, true)
                } else {
                    StaticType::None
                }
            },
            Term::ParentCall(args) => {
                if let Some(proc) = self.proc.and_then(ProcRef::parent_proc) {
                    // TODO: if args are empty, call w/ same args
                    let src = self.ty;
                    // Parent calls are exact, and won't ever call an override.
                    self.visit_call(location, src, proc, args, true)
                } else {
                    StaticType::None
                }
            },

            Term::NewImplicit { args } => {
                self.visit_new(location, type_hint, args)
            },
            Term::NewPrefab { prefab, args } => {
                let typepath = self.visit_prefab(location, prefab);
                self.visit_new(location, typepath, args)
            },
            Term::NewMiniExpr { expr, .. } => {
                let mut current = self.visit_ident(location, &expr.ident);
                for field in expr.fields.iter() {
                    current = self.visit_field(location, current, &field.ident);
                }
                // The whole point is it's dynamic, so don't try anything
                StaticType::None
            },

            Term::List(args) => {
                // TODO: use /proc/list
                self.visit_arguments(location, args);
                StaticType::List {
                    list: self.objtree.expect("/list"),
                    keys: Box::new(StaticType::None),
                }
            },
            Term::Locate { args, in_list } => {
                // TODO: use /proc/locate
                self.visit_arguments(location, args);
                if let Some(ref expr) = in_list {
                    self.visit_expression(location, expr, None);
                }
                StaticType::None
            },
            Term::Input { args, input_type: _, in_list } => {
                // TODO: use /proc/input
                self.visit_arguments(location, args);
                if let Some(ref expr) = in_list {
                    self.visit_expression(location, expr, None);
                }
                StaticType::None
            },
            Term::Pick(args) => {
                // TODO: use /proc/pick
                for (weight, value) in args.iter() {
                    if let Some(ref weight) = weight {
                        self.visit_expression(location, weight, None);
                    }
                    self.visit_expression(location, value, None);
                }
                StaticType::None
            },
            Term::DynamicCall(args_1, args_2) => {
                // TODO: use /proc/call
                self.visit_arguments(location, args_1);
                self.visit_arguments(location, args_2);
                StaticType::None
            },
            Term::ExternalCall { library, function, args } => {
                if let Some(library) = library {
                    self.visit_expression(location, library, None);
                }
                self.visit_expression(location, function, None);
                self.visit_arguments(location, args);
                StaticType::None
            },

            Term::GlobalCall(name, args) => {
                if let Some(proc) = self.objtree.root().get_proc(name) {
                    self.visit_call(location, self.objtree.root(), proc, args, false)
                } else {
                    self.visit_arguments(location, args);
                    StaticType::None
                }
            },
            Term::GlobalIdent(name) => {
                if let Some(decl) = self.objtree.root().get_var_declaration(name) {
                    self.tab.use_symbol(decl.id, location);
                    self.static_type(location, &decl.var_type.type_path)
                } else {
                    StaticType::None
                }
            },
            Term::__TYPE__ => {
                self.tab.use_symbol(self.ty.id, location);
                StaticType::None
            },
            Term::__PROC__ => {
                let Some(proc) = self.proc else {
                    return StaticType::None
                };
                if let Some(decl) = self.ty.get_proc_declaration(proc.name()) {
                    self.tab.use_symbol(decl.id, location);
                }
                StaticType::None
            }
            Term::__IMPLIED_TYPE__ => {
                let Some(implied_type) = type_hint else {
                    return StaticType::None
                };
                self.tab.use_symbol(implied_type.id, location);
                StaticType::Type(implied_type)
            },
        }
    }

    fn visit_new(&mut self, location: Location, typepath: Option<TypeRef<'o>>, args: &'o Option<Box<[Expression]>>) -> StaticType<'o> {
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
            }
            // If we had a diagnostic context here, we'd error for
            // types other than `/list`, which has no `New()`.
            StaticType::Type(typepath)
        } else {
            StaticType::None
        }
    }

    fn visit_prefab(&mut self, location: Location, prefab: &'o Prefab) -> Option<TypeRef<'o>> {
        if let Some(nav) = self.ty.navigate_path(&prefab.path) {
            // Use the proc if there was one of those
            if let NavigatePathResult::ProcPath(proc, _) = nav {
                if let Some(decl) = nav.ty().get_proc_declaration(proc.name()) {
                    self.tab.use_symbol(decl.id, location);
                }
            } else {
                // Use the type
                self.tab.use_symbol(nav.ty().id, location);
                // Use the prefab's vars
                for (key, expr) in prefab.vars.iter() {
                    let mut type_hint = None;
                    if let Some(decl) = nav.ty().get_var_declaration(key) {
                        self.tab.use_symbol(decl.id, location);
                        type_hint = self.static_type(location, &decl.var_type.type_path).basic_type();
                    }
                    self.visit_expression(location, expr, type_hint);
                }
            }
            Some(nav.ty())
        } else {
            None
        }
    }

    fn visit_field(&mut self, location: Location, lhs: StaticType<'o>, name: &'o str) -> StaticType<'o> {
        if let Some(ty) = lhs.basic_type() {
            if let Some(decl) = ty.get_var_declaration(name) {
                self.tab.use_symbol(decl.id, location);
                self.static_type(location, &decl.var_type.type_path)
            } else {
                StaticType::None
            }
        } else {
            StaticType::None
        }
    }

    fn visit_follow(&mut self, location: Location, lhs: StaticType<'o>, rhs: &'o Follow) -> StaticType<'o> {
        match rhs {
            Follow::Unary(op) => self.visit_unary(lhs, *op),
            Follow::Index(_, expr) => {
                self.visit_expression(location, expr, None);
                // TODO: call operator[] or operator[]=
                // TODO: differentiate between L[1] and L[non_numeric_key]
                match lhs {
                    StaticType::List { keys, .. } => *keys,
                    _ => StaticType::None,
                }
            },
            Follow::Field(_, name) => self.visit_field(location, lhs, name),
            Follow::StaticField(name) => self.visit_field(location, lhs, name),
            Follow::Call(_, name, arguments) => {
                if let Some(ty) = lhs.basic_type() {
                    if let Some(proc) = ty.get_proc(name) {
                        self.visit_call(location, ty, proc, arguments, false)
                    } else {
                        self.visit_arguments(location, arguments);
                        StaticType::None
                    }
                } else {
                    self.visit_arguments(location, arguments);
                    StaticType::None
                }
            },
            Follow::ProcReference(name) => {
                if let Some(ty) = lhs.basic_type() {
                    if let Some(decl) = ty.get_proc_declaration(name) {
                        self.tab.use_symbol(decl.id, location);
                    }
                }
                StaticType::None
            },
        }
    }

    fn visit_unary(&mut self, _rhs: StaticType<'o>, _op: UnaryOp) -> StaticType<'o> {
        // TODO: mark usage of operatorX procs
        StaticType::None
    }

    fn visit_binary(&mut self, _lhs: StaticType<'o>, _rhs: StaticType<'o>, _op: BinaryOp) -> StaticType<'o> {
        // TODO: mark usage of operatorX procs
        StaticType::None
    }

    fn visit_call(&mut self, location: Location, src: TypeRef<'o>, proc: ProcRef, args: &'o [Expression], is_exact: bool) -> StaticType<'o> {
        // register use of symbol
        if !is_exact {
            // Only include uses of the symbol by name, not `.()` or `..()`
            // or `new /datum()`.
            if let Some(decl) = src.get_proc_declaration(proc.name()) {
                self.tab.use_symbol(decl.id, location);
            }
        }

        // identify and register kwargs used
        for arg in args {
            let mut argument_value = arg;
            if let Expression::AssignOp { op: AssignOp::Assign, lhs, rhs } = arg {
                match lhs.as_term() {
                    Some(Term::Ident(_name)) |
                    Some(Term::String(_name)) => {
                        // Don't visit_expression the kwarg key.
                        argument_value = rhs;

                        // TODO: register a usage of the kwarg symbol here.
                        // Recurse to children too?
                    }
                    _ => {}
                }
            }

            self.visit_expression(location, argument_value, None);
        }

        StaticType::None
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

    #[allow(clippy::only_used_in_recursion)]
    fn static_type(&mut self, location: Location, mut of: &[String]) -> StaticType<'o> {
        while !of.is_empty() && ["static", "global", "const", "tmp", "final", "SpacemanDMM_final", "SpacemanDMM_private", "SpacemanDMM_protected"].contains(&&*of[0]) {
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
            StaticType::None
        }
    }
}
