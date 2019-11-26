//! The object tree representation, used as a parsing target.

use std::collections::BTreeMap;
use std::fmt;

pub use petgraph::graph::NodeIndex;
use petgraph::graph::Graph;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use linked_hash_map::LinkedHashMap;

use super::ast::{Expression, VarType, VarSuffix, PathOp, Parameter, Block, ProcDeclKind};
use super::constants::{Constant, Pop};
use super::docs::DocCollection;
use super::{DMError, Location, Context};

// ----------------------------------------------------------------------------
// Symbol IDs

/// An identifier referring to a symbol in the object tree.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct SymbolId(u32);

#[derive(Debug)]
pub struct SymbolIdSource(SymbolId);

#[derive(Copy, Clone, Debug)]
pub enum SymbolIdCategory {
    ObjectTree,
    Preprocessor,
    LocalVars,
}

const SYMBOL_ID_BITS: u32 = 32 - 2;

impl SymbolIdSource {
    pub fn new(category: SymbolIdCategory) -> SymbolIdSource {
        SymbolIdSource(SymbolId((category as u32) << SYMBOL_ID_BITS))
    }

    pub fn allocate(&mut self) -> SymbolId {
        let prev = self.0;
        (self.0).0 += 1;
        prev
    }
}

// ----------------------------------------------------------------------------
// Variables

pub type Vars = LinkedHashMap<String, Constant>;

#[derive(Debug, Clone)]
pub struct VarDeclaration {
    pub var_type: VarType,
    pub location: Location,
    pub id: SymbolId,
}

#[derive(Debug, Clone)]
pub struct VarValue {
    pub location: Location,
    /// Syntactic value, as specified in the source.
    pub expression: Option<Expression>,
    /// Evaluated value for non-static and non-tmp vars.
    pub constant: Option<Constant>,
    pub being_evaluated: bool,
    pub docs: DocCollection,
}

#[derive(Debug, Clone)]
pub struct TypeVar {
    pub value: VarValue,
    pub location: Location,
    pub declaration: Option<VarDeclaration>,
}

#[derive(Debug, Clone)]
pub struct ProcDeclaration {
    pub location: Location,
    pub kind: ProcDeclKind,
    pub id: SymbolId,
}

#[derive(Debug, Clone)]
pub struct ProcValue {
    pub location: Location,
    pub parameters: Vec<Parameter>,
    pub docs: DocCollection,
    pub code: Code,
}

#[derive(Debug, Clone)]
pub enum Code {
    Present(Block),
    Invalid(DMError),
    Builtin,
    Disabled,
}

#[derive(Debug, Clone, Default)]
pub struct TypeProc {
    pub value: Vec<ProcValue>,
    pub declaration: Option<ProcDeclaration>,
}

impl TypeProc {
    #[inline]
    pub fn main_value(&self) -> &ProcValue {
        self.value.last().expect("TypeProc::value is empty")
    }
}

// ----------------------------------------------------------------------------
// Types

const BAD_NODE_INDEX: usize = ::std::usize::MAX;

#[derive(Debug)]
pub struct Type {
    pub name: String,
    pub path: String,
    pub location: Location,
    location_specificity: usize,
    /// Variables which this type has declarations or overrides for.
    pub vars: LinkedHashMap<String, TypeVar>,
    /// Procs and verbs which this type has declarations or overrides for.
    pub procs: LinkedHashMap<String, TypeProc>,
    parent_type: NodeIndex,
    pub docs: DocCollection,
    pub id: SymbolId,
}

impl Type {
    pub fn parent_type(&self) -> Option<NodeIndex> {
        if self.parent_type == NodeIndex::new(BAD_NODE_INDEX) {
            None
        } else {
            Some(self.parent_type)
        }
    }

    /// Checks whether this node is the root node, on which global vars and
    /// procs reside.
    #[inline]
    pub fn is_root(&self) -> bool {
        self.path.is_empty()
    }

    pub fn pretty_path(&self) -> &str {
        if self.is_root() {
            "(global)"
        } else {
            &self.path
        }
    }

    /// Checks whether this type's path is a subpath of the given path.
    #[inline]
    pub fn is_subpath_of(&self, parent: &str) -> bool {
        subpath(&self.path, parent)
    }

    // Used in the constant evaluator which holds an &mut ObjectTree and thus
    // can't be used with TypeRef.
    pub(crate) fn get_value<'a>(&'a self, name: &str, objtree: &'a ObjectTree) -> Option<&'a VarValue> {
        let mut current = Some(self);
        while let Some(ty) = current {
            if let Some(var) = ty.vars.get(name) {
                return Some(&var.value);
            }
            current = objtree.parent_of(ty);
        }
        None
    }

    pub(crate) fn get_var_declaration<'a>(&'a self, name: &str, objtree: &'a ObjectTree) -> Option<&'a VarDeclaration> {
        let mut current = Some(self);
        while let Some(ty) = current {
            if let Some(var) = ty.vars.get(name) {
                if let Some(ref decl) = var.declaration {
                    return Some(decl);
                }
            }
            current = objtree.parent_of(ty);
        }
        None
    }
}

#[inline]
pub fn subpath(path: &str, parent: &str) -> bool {
    debug_assert!(path.starts_with('/') && parent.starts_with('/') && parent.ends_with('/'));
    path == &parent[..parent.len() - 1] || path.starts_with(parent)
}

// ----------------------------------------------------------------------------
// Type references

#[derive(Copy, Clone)]
pub struct TypeRef<'a> {
    tree: &'a ObjectTree,
    idx: NodeIndex,
}

impl<'a> TypeRef<'a> {
    #[inline]
    pub(crate) fn new(tree: &'a ObjectTree, idx: NodeIndex) -> TypeRef<'a> {
        TypeRef { tree, idx }
    }

    #[inline]
    pub fn get(self) -> &'a Type {
        self.tree.graph.node_weight(self.idx).unwrap()
    }

    pub fn tree(self) -> &'a ObjectTree {
        self.tree
    }

    /// Find the parent **path**, without taking `parent_type` into account.
    pub fn parent_path(&self) -> Option<TypeRef<'a>> {
        self.tree
            .graph
            .neighbors_directed(self.idx, Direction::Incoming)
            .next()
            .map(|i| TypeRef::new(self.tree, i))
    }

    /// Find the parent **type** based on `parent_type` var, or parent path if unspecified.
    pub fn parent_type(&self) -> Option<TypeRef<'a>> {
        let idx = self.parent_type;
        self.tree.graph.node_weight(idx).map(|_| TypeRef::new(self.tree, idx))
    }

    /// Find a child **path** with the given name, if it exists.
    pub fn child(&self, name: &str) -> Option<TypeRef<'a>> {
        for idx in self.tree.graph.neighbors(self.idx) {
            let ty = self.tree.graph.node_weight(idx).unwrap();
            if ty.name == name {
                return Some(TypeRef::new(self.tree, idx));
            }
        }
        None
    }

    /// Iterate over all child **paths**.
    pub fn children(self) -> impl Iterator<Item=TypeRef<'a>> + 'a {
        self.tree.graph.neighbors(self.idx).map(move |idx| TypeRef::new(self.tree, idx))
    }

    /// Recursively visit this and all child **paths**.
    pub fn recurse<F: FnMut(TypeRef<'a>)>(&self, f: &mut F) {
        f(*self);
        for child in self.children() {
            child.recurse(f);
        }
    }

    /// Recursively visit this and all parent **types**.
    pub fn visit_parent_types<F: FnMut(TypeRef<'a>)>(&self, f: &mut F) {
        let mut next = Some(*self);
        while let Some(current) = next {
            f(current);
            next = current.parent_type();
        }
    }

    /// Recursively visit this and all parent **paths**.
    pub fn visit_parent_paths<F: FnMut(TypeRef<'a>)>(&self, f: &mut F) {
        let mut next = Some(*self);
        while let Some(current) = next {
            f(current);
            next = current.parent_path();
        }
    }

    /// Navigate the tree according to the given path operator.
    pub fn navigate(self, op: PathOp, name: &str) -> Option<TypeRef<'a>> {
        match op {
            // '/' always looks for a direct child
            PathOp::Slash => self.child(name),
            // '.' looks for a child of us or of any of our parents
            PathOp::Dot => {
                let mut next = Some(self);
                while let Some(current) = next {
                    if let Some(child) = current.child(name) {
                        return Some(child);
                    }
                    next = current.parent_path();
                }
                None
            },
            // ':' looks for a child of us or of any of our children
            PathOp::Colon => {
                if let Some(child) = self.child(name) {
                    return Some(child);
                }
                for idx in self.tree.graph.neighbors(self.idx) {
                    if let Some(child) = TypeRef::new(self.tree, idx).navigate(PathOp::Colon, name) {
                        // Yes, simply returning the first thing that matches
                        // is the correct behavior.
                        return Some(child);
                    }
                }
                None
            },
        }
    }

    /// Find another type relative to this type.
    pub fn navigate_path<S: AsRef<str>>(self, pieces: &'a [(PathOp, S)]) -> Option<NavigatePathResult<'a>> {
        let mut next = Some(self);
        if let Some(&(PathOp::Slash, _)) = pieces.first() {
            next = Some(self.tree.root());
        }

        let mut iter = pieces.iter();
        while let Some(&(op, ref s)) = iter.next() {
            let name = s.as_ref();
            if let Some(current) = next {
                // Check if it's `proc` or `verb` in the path.
                // Note that this doesn't catch this confusing corner case:
                //    /proc/foo()
                //    /proc/bar()
                //        return .foo
                // It also doesn't yet handle the difference between `:proc`,
                // `.proc`, and `/proc`, treating everything as `.proc`.
                if let Some(kind) = ProcDeclKind::from_name(s.as_ref()) {
                    if let Some((_, name)) = iter.next() {
                        if let Some(proc_ref) = current.get_proc(name.as_ref()) {
                            return Some(NavigatePathResult::ProcPath(proc_ref, kind));
                        } else {
                            // The proc doesn't exist, so lookup fails.
                            return None;
                        }
                    }
                    return Some(NavigatePathResult::ProcGroup(current, kind));
                }

                // Otherwise keep navigating as normal.
                next = current.navigate(op, name.as_ref());
            } else {
                return None;
            }
        }
        next.map(NavigatePathResult::Type)
    }

    /// Checks whether this type is a subtype of the given type.
    pub fn is_subtype_of(self, parent: &Type) -> bool {
        let mut current = Some(self);
        while let Some(ty) = current.take() {
            if ::std::ptr::eq(ty.get(), parent) {
                return true;
            }
            current = ty.parent_type();
        }
        false
    }

    #[inline]
    pub fn get_value(self, name: &str) -> Option<&'a VarValue> {
        self.get().get_value(name, self.tree)
    }

    #[inline]
    pub fn get_var_declaration(self, name: &str) -> Option<&'a VarDeclaration> {
        self.get().get_var_declaration(name, self.tree)
    }

    pub fn get_proc(self, name: &'a str) -> Option<ProcRef<'a>> {
        let mut current: Option<TypeRef<'a>> = Some(self);
        while let Some(ty) = current {
            if let Some(proc) = ty.get().procs.get(name) {
                return Some(ProcRef {
                    ty,
                    list: &proc.value,
                    name,
                    idx: proc.value.len() - 1,
                });
            }
            current = ty.parent_type();
        }
        None
    }

    pub fn get_proc_declaration(self, name: &str) -> Option<&'a ProcDeclaration> {
        let mut current: Option<TypeRef<'a>> = Some(self);
        while let Some(ty) = current {
            if let Some(proc) = ty.get().procs.get(name) {
                if let Some(decl) = proc.declaration.as_ref() {
                    return Some(decl);
                }
            }
            current = ty.parent_type();
        }
        None
    }

    pub fn iter_self_procs(self) -> impl Iterator<Item=ProcRef<'a>> {
        self.get().procs.iter().flat_map(move |(name, type_proc)| {
            let list = &type_proc.value;
            (0..list.len()).map(move |idx| ProcRef {
                ty: self,
                list,
                name,
                idx,
            })
        })
    }
}

impl<'a> ::std::ops::Deref for TypeRef<'a> {
    type Target = Type;
    fn deref(&self) -> &Type {
        self.get()
    }
}

impl<'a> fmt::Debug for TypeRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.path, self.idx.index())
    }
}

impl<'a> fmt::Display for TypeRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.pretty_path())
    }
}

impl<'a> ::std::cmp::PartialEq for TypeRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        ::std::ptr::eq(self.tree, other.tree) && self.idx == other.idx
    }
}

impl<'a> ::std::cmp::Eq for TypeRef<'a> {}

impl<'a> ::std::hash::Hash for TypeRef<'a> {
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        self.idx.hash(state);
    }
}

#[derive(Debug, Copy, Clone)]
pub enum NavigatePathResult<'o> {
    Type(TypeRef<'o>),
    ProcGroup(TypeRef<'o>, ProcDeclKind),
    ProcPath(ProcRef<'o>, ProcDeclKind),
}

impl<'o> NavigatePathResult<'o> {
    pub fn ty(self) -> TypeRef<'o> {
        match self {
            NavigatePathResult::Type(ty) => ty,
            NavigatePathResult::ProcGroup(ty, _) => ty,
            NavigatePathResult::ProcPath(proc, _) => proc.ty(),
        }
    }

    pub fn to_path(self) -> Vec<String> {
        let mut path: Vec<String> = self.ty().path.split('/').skip(1).map(ToOwned::to_owned).collect();
        match self {
            NavigatePathResult::Type(_) => {},
            NavigatePathResult::ProcGroup(_, kind) => path.push(kind.to_string()),
            NavigatePathResult::ProcPath(proc, kind) => {
                path.push(kind.to_string());
                path.push(proc.name().to_owned());
            }
        }
        path
    }
}

// ----------------------------------------------------------------------------
// Proc references

#[derive(Clone, Copy)]
pub struct ProcRef<'a> {
    ty: TypeRef<'a>,
    list: &'a [ProcValue],
    name: &'a str,
    idx: usize,
}

impl<'a> ProcRef<'a> {
    pub fn get(self) -> &'a ProcValue {
        &self.list[self.idx]
    }

    pub fn ty(self) -> TypeRef<'a> {
        self.ty
    }

    pub fn name(&self) -> &str {
        self.name
    }

    pub fn index(self) -> usize {
        self.idx
    }

    pub fn tree(self) -> &'a ObjectTree {
        self.ty.tree()
    }

    /// Look up the immediate parent, `..()`.
    pub fn parent_proc(self) -> Option<ProcRef<'a>> {
        if let Some(idx) = self.idx.checked_sub(1) {
            Some(ProcRef {
                ty: self.ty,
                list: self.list,
                name: self.name,
                idx,
            })
        } else {
            self.ty.parent_type().and_then(|ty| ty.get_proc(self.name))
        }
    }

    /// Returns whether this is the public-facing version (final override) of this proc.
    pub fn is_externally_visible(self) -> bool {
        self.idx + 1 == self.list.len()
    }

    /// Check whether this proc is indicated to be varargs.
    pub fn is_varargs(self) -> bool {
        self.parameters.iter().any(|p| p.name == "...")
    }

    /// Get the declaration corresponding to this proc reference.
    pub fn get_declaration(self) -> Option<&'a ProcDeclaration> {
        self.ty.get_proc_declaration(self.name)
    }

    /// Recursively visit this and all public-facing procs which override it.
    pub fn recurse_children<F: FnMut(ProcRef<'a>)>(self, f: &mut F) {
        self.ty.recurse(&mut move |ty| {
            if let Some(proc) = ty.get().procs.get(self.name) {
                f(ProcRef {
                    ty,
                    list: &proc.value,
                    name: self.name,
                    idx: proc.value.len() - 1,
                });
            }
        });
    }
}

impl<'a> ::std::ops::Deref for ProcRef<'a> {
    type Target = ProcValue;
    fn deref(&self) -> &ProcValue {
        self.get()
    }
}

impl<'a> fmt::Debug for ProcRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}/proc/{}[{}/{}]", self.ty, self.name, self.idx, self.list.len())
    }
}

impl<'a> fmt::Display for ProcRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/proc/{}", self.ty.path, self.name)?;
        if self.list.len() > 1 {
            write!(f, "[{}/{}]", self.idx, self.list.len())?;
        }
        Ok(())
    }
}

impl<'a> ::std::cmp::PartialEq for ProcRef<'a> {
    fn eq(&self, other: &ProcRef<'a>) -> bool {
        self.ty == other.ty && self.name == other.name && self.idx == other.idx
    }
}

impl<'a> std::cmp::Eq for ProcRef<'a> {}

impl<'a> ::std::hash::Hash for ProcRef<'a> {
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        self.ty.hash(state);
        self.name.hash(state);
        self.idx.hash(state);
    }
}

// ----------------------------------------------------------------------------
// The object tree itself

#[derive(Debug)]
pub struct ObjectTree {
    pub graph: Graph<Type, ()>,
    pub types: BTreeMap<String, NodeIndex>,
    pub symbols: SymbolIdSource,
}

impl Default for ObjectTree {
    fn default() -> Self {
        let mut tree = ObjectTree {
            graph: Default::default(),
            types: Default::default(),
            symbols: SymbolIdSource::new(SymbolIdCategory::ObjectTree),
        };
        tree.graph.add_node(Type {
            name: String::new(),
            path: String::new(),
            location: Default::default(),
            location_specificity: 0,
            vars: Default::default(),
            procs: Default::default(),
            parent_type: NodeIndex::new(BAD_NODE_INDEX),
            docs: Default::default(),
            id: tree.symbols.allocate(),
        });
        tree
    }
}

impl ObjectTree {
    pub fn register_builtins(&mut self) {
        super::builtins::register_builtins(self).expect("register_builtins failed");
    }

    // ------------------------------------------------------------------------
    // Access

    pub fn root(&self) -> TypeRef {
        TypeRef::new(self, NodeIndex::new(0))
    }

    pub fn find(&self, path: &str) -> Option<TypeRef> {
        self.types.get(path).map(|&ix| TypeRef::new(self, ix))
    }

    pub fn expect(&self, path: &str) -> TypeRef {
        match self.types.get(path) {
            Some(&ix) => TypeRef::new(self, ix),
            None => panic!("type not found: {:?}", path),
        }
    }

    pub fn parent_of(&self, type_: &Type) -> Option<&Type> {
        self.graph.node_weight(type_.parent_type)
    }

    pub fn type_by_path<I>(&self, path: I) -> Option<TypeRef>
    where
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        let (exact, ty) = self.type_by_path_approx(path);
        if exact {
            Some(ty)
        } else {
            None
        }
    }

    pub fn type_by_path_approx<I>(&self, path: I) -> (bool, TypeRef)
    where
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        let mut current = NodeIndex::new(0);
        let mut first = true;
        'outer: for each in path {
            let each = each.as_ref();

            for edge in self.graph.edges(current) {
                let target = edge.target();
                if self.graph.node_weight(target).unwrap().name == each {
                    current = target;
                    if each == "list" && first {
                        // any lookup under list/ is list/
                        break 'outer;
                    }
                    first = false;
                    continue 'outer;
                }
            }
            return (false, TypeRef::new(self, current));
    }
        (true, TypeRef::new(self, current))
    }

    pub fn type_by_constant(&self, constant: &Constant) -> Option<TypeRef> {
        match *constant {
            Constant::String(ref string_path) => self.find(string_path),
            Constant::Prefab(Pop { ref path, .. }) => self.type_by_path(path),
            _ => None,
        }
    }

    // ------------------------------------------------------------------------
    // Finalization

    pub(crate) fn finalize(&mut self, context: &Context, parser_fatal_errored: bool) {
        self.assign_parent_types(context);
        if !parser_fatal_errored {
            super::constants::evaluate_all(context, self);
        }
    }

    fn assign_parent_types(&mut self, context: &Context) {
        for (path, &type_idx) in self.types.iter() {
            let mut location = self.graph.node_weight(type_idx).unwrap().location;
            let idx = if path == "/datum" || path == "/list" || path == "/savefile" || path == "/world" {
                // These types have no parent and cannot have one added. In the official compiler:
                // - setting list or savefile/parent_type is denied with the same error as setting something's parent type to them;
                // - setting datum/parent_type infinite loops the compiler;
                // - setting world/parent_type compiles but has no runtime effect.

                // Here, let's try to error if anything is set.
                if let Some(var) = self.graph.node_weight(type_idx).unwrap().vars.get("parent_type") {
                    // This check won't catch invalid redeclarations like `/datum/var/parent_type`, but that's fine for now.
                    if var.value.expression.is_some() {
                        context.register_error(DMError::new(
                            var.value.location,
                            format!("not allowed to change {}/parent_type", path),
                        ));
                    }
                }

                NodeIndex::new(0)
            } else {
                let mut parent_type_buf;
                let parent_type = if path == "/atom" {
                    "/datum"
                } else if path == "/turf" || path == "/area" {
                    "/atom"
                } else if path == "/obj" || path == "/mob" {
                    "/atom/movable"
                } else {
                    let mut parent_type = match path.rfind('/').unwrap() {
                        0 if path == "/client" => "",
                        0 => "/datum",
                        idx => &path[..idx],
                    };
                    if let Some(var) = self.graph.node_weight(type_idx).unwrap().vars.get("parent_type") {
                        location = var.value.location;
                        if let Some(expr) = var.value.expression.clone() {
                            match expr.simple_evaluate(var.value.location) {
                                Ok(Constant::String(s)) => {
                                    parent_type_buf = s;
                                    parent_type = &parent_type_buf;
                                }
                                Ok(Constant::Prefab(Pop { ref path, ref vars })) if vars.is_empty() => {
                                    parent_type_buf = String::new();
                                    for piece in path.iter() {
                                        parent_type_buf.push('/');
                                        parent_type_buf.push_str(&piece);
                                    }
                                    parent_type = &parent_type_buf;
                                }
                                Ok(other) => {
                                    context.register_error(DMError::new(location, format!("bad parent_type: {}", other)));
                                }
                                Err(e) => {
                                    context.register_error(e);
                                }
                            }
                        }
                    }
                    parent_type
                };

                if path == "/client" && parent_type == "" {
                    // client has no parent by default, but can be safely reparented to /datum
                    NodeIndex::new(0)
                } else if let Some(&idx) = self.types.get(parent_type) {
                    idx
                } else {
                    context.register_error(DMError::new(
                        location,
                        format!("bad parent type for {}: {}", path, parent_type),
                    ));
                    NodeIndex::new(0)  // on bad parent_type, fall back to the root
                }
            };

            self.graph.node_weight_mut(type_idx)
                .unwrap()
                .parent_type = idx;
        }
    }

    // ------------------------------------------------------------------------
    // Parsing

    fn subtype_or_add(&mut self, location: Location, parent: NodeIndex, child: &str, len: usize) -> NodeIndex {
        let mut neighbors = self.graph.neighbors(parent).detach();
        while let Some(target) = neighbors.next_node(&self.graph) {
            let node = self.graph.node_weight_mut(target).unwrap();
            if node.name == child {
                if node.location_specificity > len {
                    node.location_specificity = len;
                    node.location = location;
                }
                return target;
            }
        }

        // time to add a new child
        let path = format!("{}/{}", self.graph.node_weight(parent).unwrap().path, child);
        let node = self.graph.add_node(Type {
            name: child.to_owned(),
            path: path.clone(),
            vars: Default::default(),
            procs: Default::default(),
            location,
            location_specificity: len,
            parent_type: NodeIndex::new(BAD_NODE_INDEX),
            docs: Default::default(),
            id: self.symbols.allocate(),
        });
        self.graph.add_edge(parent, node, ());
        self.types.insert(path, node);
        node
    }

    fn get_from_path<'a, I: Iterator<Item=&'a str>>(
        &mut self,
        location: Location,
        mut path: I,
        len: usize,
    ) -> Result<(NodeIndex, &'a str), DMError> {
        let mut current = NodeIndex::new(0);
        let mut last = match path.next() {
            Some(name) => name,
            None => return Err(DMError::new(location, "cannot register root path")),
        };
        if is_decl(last) {
            return Ok((current, last));
        }
        for each in path {
            current = self.subtype_or_add(location, current, last, len);
            last = each;
            if is_decl(last) {
                break;
            }
        }

        Ok((current, last))
    }

    fn register_var<'a, I>(
        &mut self,
        location: Location,
        parent: NodeIndex,
        mut prev: &'a str,
        mut rest: I,
        comment: DocCollection,
        suffix: VarSuffix,
    ) -> Result<Option<&mut TypeVar>, DMError>
    where
        I: Iterator<Item=&'a str>,
    {
        let (mut is_declaration, mut is_static, mut is_const, mut is_tmp, mut is_final) = (false, false, false, false, false);

        let mut varname = prev;

        if is_var_decl(prev) {
            is_declaration = true;
            prev = match rest.next() {
                Some(name) => name,
                None => return Ok(None), // var{} block, children will be real vars
            };
            while prev == "global" || prev == "static" || prev == "tmp" || prev == "const" {
                if let Some(name) = rest.next() {
                    is_static |= prev == "global" || prev == "static";
                    is_const |= prev == "const";
                    is_tmp |= prev == "tmp";
                    is_final |= prev == "final";
                    prev = name;
                } else {
                    return Ok(None); // var/const{} block, children will be real vars
                }
            }
            varname = prev;
        } else if is_proc_decl(prev) {
            return Err(DMError::new(location, "proc looks like a var"));
        }

        let mut type_path = Vec::new();
        for each in rest {
            type_path.push(prev.to_owned());
            prev = each;
        }
        let mut var_type = VarType {
            is_static,
            is_const,
            is_tmp,
            is_final,
            type_path,
        };
        var_type.suffix(&suffix);

        let symbolid = self.symbols.allocate();
        {
            let node = self.graph.node_weight(parent).unwrap();
            
            if !is_declaration {
                if let Some(decl) = node.get_var_declaration(varname, self) {
                    if decl.var_type.is_final {
                        return Err(DMError::new(location, format!("override of final var {}", varname))
                        .with_note(decl.location, "previous definition"))
                    }
                }
            } else if is_final {
                for (_typepath, _nodeindex) in self.types.iter() {
                    let child = self.graph.node_weight(*_nodeindex).unwrap();
                    if let Some(varoverride) = child.vars.get(varname) {
                        return Err(DMError::new(varoverride.location, format!("override of final var {}", varname))
                        .with_note(location, "previous definition"))
                    }
                }



            }
        }
        let node = self.graph.node_weight_mut(parent).unwrap();

        // TODO: warn and merge docs for repeats
        Ok(Some(node.vars.entry(prev.to_owned()).or_insert_with(|| TypeVar {
            value: VarValue {
                location,
                expression: suffix.into_initializer(),
                constant: None,
                being_evaluated: false,
                docs: comment,
            },
            location: location,
            declaration: if is_declaration {
                Some(VarDeclaration {
                    var_type,
                    location,
                    id: symbolid,
                })
            } else {
                None
            },
        })))
    }

    fn register_proc(
        &mut self,
        context: &Context,
        location: Location,
        parent: NodeIndex,
        name: &str,
        declaration: Option<ProcDeclKind>,
        parameters: Vec<Parameter>,
        code: Code,
    ) -> Result<(usize, &mut ProcValue), DMError> {
        let node = self.graph.node_weight_mut(parent).unwrap();
        let proc = node.procs.entry(name.to_owned()).or_insert_with(Default::default);
        if let Some(kind) = declaration {
            if let Some(ref decl) = proc.declaration {
                DMError::new(location, format!("duplicate definition of {}/{}", kind, name))
                    .with_note(decl.location, "previous definition")
                    .register(context);
            } else {
                proc.declaration = Some(ProcDeclaration {
                    location,
                    kind,
                    id: self.symbols.allocate(),
                });
            }
        }

        let value = ProcValue {
            location,
            parameters,
            docs: Default::default(),
            code
        };

        // TODO: resolve the "procedure override precedes definition" problem.
        // DM really does reorder the declaration to appear before the override,
        // but only when a `/proc` block appeared somewhere prior to the
        // override. This logic is too much of a corner case to bother
        // implementing, so abusers of it will have to put up with some
        // incorrect analyses for now. http://www.byond.com/forum/post/2441385
        let len = proc.value.len();
        proc.value.push(value);
        Ok((len, proc.value.last_mut().unwrap()))
    }

    // an entry which may be anything depending on the path
    pub fn add_entry<'a, I: Iterator<Item = &'a str>>(
        &mut self,
        location: Location,
        mut path: I,
        len: usize,
        comment: DocCollection,
        suffix: VarSuffix,
    ) -> Result<(), DMError> {
        let (parent, child) = self.get_from_path(location, &mut path, len)?;
        if is_var_decl(child) {
            self.register_var(location, parent, "var", path, comment, suffix)?;
        } else if is_proc_decl(child) {
            // proc{} block, children will be procs
        } else {
            let idx = self.subtype_or_add(location, parent, child, len);
            self.graph.node_weight_mut(idx).unwrap().docs.extend(comment);
        }
        Ok(())
    }

    // an entry which is definitely a var because a value is specified
    pub fn add_var<'a, I: Iterator<Item = &'a str>>(
        &mut self,
        location: Location,
        mut path: I,
        len: usize,
        expr: Expression,
        comment: DocCollection,
        suffix: VarSuffix,
    ) -> Result<(), DMError> {
        let (parent, initial) = self.get_from_path(location, &mut path, len)?;
        if let Some(type_var) = self.register_var(location, parent, initial, path, comment, suffix)? {
            type_var.value.location = location;
            type_var.value.expression = Some(expr);
            Ok(())
        } else {
            Err(DMError::new(location, "var must have a name"))
        }
    }

    // an entry which is definitely a proc because an argument list is specified
    pub fn add_proc<'a, I: Iterator<Item = &'a str>>(
        &mut self,
        context: &Context,
        location: Location,
        mut path: I,
        len: usize,
        parameters: Vec<Parameter>,
        code: Code,
    ) -> Result<(usize, &mut ProcValue), DMError> {
        let (parent, mut proc_name) = self.get_from_path(location, &mut path, len)?;
        let mut declaration = None;
        if let Some(kind) = ProcDeclKind::from_name(proc_name) {
            declaration = Some(kind);
            proc_name = match path.next() {
                Some(name) => name,
                None => return Err(DMError::new(location, "proc must have a name")),
            };
        } else if is_var_decl(proc_name) {
            return Err(DMError::new(location, "var looks like a proc"));
        }
        if let Some(other) = path.next() {
            return Err(DMError::new(
                location,
                format!("proc name must be a single identifier (spurious {:?})", other),
            ));
        }

        self.register_proc(context, location, parent, proc_name, declaration, parameters, code)
    }

    /// Drop all code ASTs to attempt to reduce memory usage.
    pub fn drop_code(&mut self) {
        for node in self.graph.node_weights_mut() {
            for (_, typroc) in node.procs.iter_mut() {
                for proc in typroc.value.iter_mut() {
                    proc.code = Code::Disabled;
                }
            }
        }
    }
}

#[inline]
fn is_var_decl(s: &str) -> bool {
    s == "var"
}

#[inline]
fn is_proc_decl(s: &str) -> bool {
    s == "proc" || s == "verb"
}

#[inline]
fn is_decl(s: &str) -> bool {
    is_var_decl(s) || is_proc_decl(s)
}
