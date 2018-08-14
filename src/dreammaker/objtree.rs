//! The object tree representation, used as a parsing target.

use std::collections::BTreeMap;
use std::fmt;

pub use petgraph::graph::NodeIndex;
use petgraph::graph::Graph;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use linked_hash_map::LinkedHashMap;

use super::ast::{Expression, VarType, PathOp, Prefab, Parameter};
use super::constants::Constant;
use super::docs::DocCollection;
use super::{DMError, Location, Context};

// ----------------------------------------------------------------------------
// Variables

pub type Vars = LinkedHashMap<String, Constant>;

#[derive(Debug, Clone)]
pub struct VarDeclaration {
    pub var_type: VarType,
    pub location: Location,
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
    pub declaration: Option<VarDeclaration>,
}

#[derive(Debug, Clone)]
pub struct ProcDeclaration {
    pub location: Location,
    pub is_verb: bool,
}

#[derive(Debug, Clone)]
pub struct ProcValue {
    pub location: Location,
    pub parameters: Vec<Parameter>,
    pub docs: DocCollection,
}

#[derive(Debug, Clone, Default)]
pub struct TypeProc {
    pub value: Vec<ProcValue>,
    pub declaration: Option<ProcDeclaration>,
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
    pub vars: LinkedHashMap<String, TypeVar>,
    pub procs: LinkedHashMap<String, TypeProc>,
    parent_type: NodeIndex,
    pub docs: DocCollection,
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

    pub(crate) fn get_declaration<'a>(&'a self, name: &str, objtree: &'a ObjectTree) -> Option<&'a VarDeclaration> {
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
    debug_assert!(path.starts_with("/") && parent.starts_with("/") && parent.ends_with("/"));
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
    fn new(tree: &'a ObjectTree, idx: NodeIndex) -> TypeRef<'a> {
        TypeRef { tree, idx }
    }

    #[inline]
    pub fn get(self) -> &'a Type {
        self.tree.graph.node_weight(self.idx).unwrap()
    }

    /// Find the parent **path**, without taking `parent_type` into account.
    pub fn parent_path(&self) -> Option<TypeRef<'a>> {
        self.tree.graph.neighbors_directed(self.idx, Direction::Incoming).next().map(|i| TypeRef::new(self.tree, i))
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
    pub fn children(&self) -> Vec<TypeRef<'a>> {
        let mut output = Vec::new();
        for idx in self.tree.graph.neighbors(self.idx) {
            output.push(TypeRef::new(self.tree, idx));
        }
        output
    }

    /// Recursively visit this and all child **paths**.
    pub fn recurse<F: FnMut(TypeRef<'a>)>(&self, f: &mut F) {
        f(*self);
        for child in self.children() {
            child.recurse(f);
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

    /// Checks whether this type is a subtype of the given type.
    pub fn is_subtype_of(self, parent: &Type) -> bool {
        let mut current = Some(self);
        while let Some(ty) = current.take() {
            if ::std::ptr::eq(ty.get(), parent) { return true }
            current = ty.parent_type();
        }
        false
    }

    #[inline]
    pub fn get_value(self, name: &str) -> Option<&'a VarValue> {
        self.get().get_value(name, self.tree)
    }

    #[inline]
    pub fn get_declaration(self, name: &str) -> Option<&'a VarDeclaration> {
        self.get().get_declaration(name, self.tree)
    }

    pub fn get_proc(self, name: &str) -> Option<&'a ProcValue> {
        let mut current: Option<TypeRef<'a>> = Some(self);
        while let Some(ty) = current {
            if let Some(proc) = ty.get().procs.get(name) {
                return proc.value.last();
            }
            current = ty.parent_type();
        }
        None
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

// ----------------------------------------------------------------------------
// The object tree itself

#[derive(Debug)]
pub struct ObjectTree {
    pub graph: Graph<Type, ()>,
    pub types: BTreeMap<String, NodeIndex>,
}

impl Default for ObjectTree {
    fn default() -> Self {
        let mut tree = ObjectTree {
            graph: Default::default(),
            types: Default::default(),
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

    pub fn parent_of(&self, type_: &Type) -> Option<&Type> {
        self.graph.node_weight(type_.parent_type)
    }

    pub fn type_by_path<I>(&self, path: I) -> Option<TypeRef>
        where I: IntoIterator, I::Item: AsRef<str>
    {
        let (exact, ty) = self.type_by_path_approx(path);
        if exact {
            Some(ty)
        } else {
            None
        }
    }

    pub fn type_by_path_approx<I>(&self, path: I) -> (bool, TypeRef)
        where I: IntoIterator, I::Item: AsRef<str>
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
        return (true, TypeRef::new(self, current));
    }

    pub fn type_by_constant(&self, constant: &Constant) -> Option<TypeRef> {
        match constant {
            &Constant::String(ref string_path) => self.find(string_path),
            &Constant::Prefab(Prefab { ref path, .. }) => self.type_by_path(path.iter().map(|(_, item)| item)),
            _ => None,
        }
    }

    // ------------------------------------------------------------------------
    // Finalization

    pub(crate) fn finalize(&mut self, context: &Context, sloppy: bool) {
        self.assign_parent_types(context);
        super::constants::evaluate_all(context, self, sloppy);
    }

    fn assign_parent_types(&mut self, context: &Context) {
        for (path, &type_idx) in self.types.iter() {
            let mut location = self.graph.node_weight(type_idx).unwrap().location;
            let idx = if path == "/datum" {
                NodeIndex::new(0)
            } else {
                let mut parent_type_buf;
                let parent_type = if path == "/atom" {
                    "/datum"
                } else if path == "/turf" {
                    "/atom"
                } else if path == "/area" {
                    "/atom"
                } else if path == "/obj" {
                    "/atom/movable"
                } else if path == "/mob" {
                    "/atom/movable"
                } else {
                    let mut parent_type = match path.rfind("/").unwrap() {
                        0 => "/datum",
                        idx => &path[..idx],
                    };
                    if let Some(name) = self.graph.node_weight(type_idx).unwrap().vars.get("parent_type") {
                        location = name.value.location;
                        if let Some(expr) = name.value.expression.clone() {
                            match ::constants::simple_evaluate(name.value.location, expr) {
                                Ok(Constant::String(s)) => {
                                    parent_type_buf = s;
                                    parent_type = &parent_type_buf;
                                }
                                Ok(Constant::Prefab(Prefab { ref path, ref vars })) if vars.is_empty() => {
                                    parent_type_buf = String::new();
                                    for &(_, ref piece) in path.iter() {
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

                if let Some(&idx) = self.types.get(parent_type) {
                    idx
                } else {
                    context.register_error(DMError::new(location, format!("bad parent type for {}: {}", path, parent_type)));
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
            location: location,
            location_specificity: len,
            parent_type: NodeIndex::new(BAD_NODE_INDEX),
            docs: Default::default(),
        });
        self.graph.add_edge(parent, node, ());
        self.types.insert(path, node);
        node
    }

    fn get_from_path<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, mut path: I, len: usize) -> Result<(NodeIndex, &'a str), DMError> {
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

    fn register_var<'a, I>(&mut self,
        location: Location,
        parent: NodeIndex,
        mut prev: &'a str,
        mut rest: I,
        comment: DocCollection,
    ) -> Result<Option<&mut TypeVar>, DMError> where
        I: Iterator<Item=&'a str>
    {
        let (mut is_declaration, mut is_static, mut is_const, mut is_tmp) = (false, false, false, false);

        if is_var_decl(prev) {
            is_declaration = true;
            prev = match rest.next() {
                Some(name) => name,
                None => return Ok(None) // var{} block, children will be real vars
            };
            while prev == "global" || prev == "static" || prev == "tmp" || prev == "const" {
                if let Some(name) = rest.next() {
                    is_static |= prev == "global" || prev == "static";
                    is_const |= prev == "const";
                    is_tmp |= prev == "tmp";
                    prev = name;
                } else {
                    return Ok(None) // var/const{} block, children will be real vars
                }
            }
        } else if is_proc_decl(prev) {
            return Err(DMError::new(location, "proc looks like a var"))
        }

        let mut type_path = Vec::new();
        for each in rest {
            type_path.push(prev.to_owned());
            prev = each;
        }
        let node = self.graph.node_weight_mut(parent).unwrap();
        // TODO: warn and merge docs for repeats
        Ok(Some(node.vars.entry(prev.to_owned()).or_insert_with(|| TypeVar {
            value: VarValue {
                location,
                expression: None,
                constant: None,
                being_evaluated: false,
                docs: comment,
            },
            declaration: if is_declaration {
                Some(VarDeclaration {
                    var_type: VarType {
                        is_static,
                        is_const,
                        is_tmp,
                        type_path,
                    },
                    location,
                })
            } else {
                None
            },
        })))
    }

    fn register_proc(&mut self,
        location: Location,
        parent: NodeIndex,
        name: &str,
        is_verb: Option<bool>,
        parameters: Vec<Parameter>,
    ) -> Result<(usize, &mut ProcValue), DMError> {
        let node = self.graph.node_weight_mut(parent).unwrap();
        let proc = node.procs.entry(name.to_owned()).or_insert_with(Default::default);
        proc.declaration = is_verb.map(|is_verb| ProcDeclaration {
            location,
            is_verb,
        });

        let len = proc.value.len();
        proc.value.push(ProcValue {
            location,
            parameters,
            docs: Default::default(),
        });
        Ok((len, proc.value.last_mut().unwrap()))
    }

    // an entry which may be anything depending on the path
    pub fn add_entry<'a, I: Iterator<Item=&'a str>>(&mut self,
        location: Location,
        mut path: I,
        len: usize,
        comment: DocCollection,
    ) -> Result<(), DMError> {
        let (parent, child) = self.get_from_path(location, &mut path, len)?;
        if is_var_decl(child) {
            self.register_var(location, parent, "var", path, comment)?;
        } else if is_proc_decl(child) {
            // proc{} block, children will be procs
        } else {
            let idx = self.subtype_or_add(location, parent, child, len);
            self.graph.node_weight_mut(idx).unwrap().docs.extend(comment);
        }
        Ok(())
    }

    // an entry which is definitely a var because a value is specified
    pub fn add_var<'a, I: Iterator<Item=&'a str>>(&mut self,
        location: Location,
        mut path: I,
        len: usize,
        expr: Expression,
        comment: DocCollection,
    ) -> Result<(), DMError> {
        let (parent, initial) = self.get_from_path(location, &mut path, len)?;
        if let Some(type_var) = self.register_var(location, parent, initial, path, comment)? {
            type_var.value.location = location;
            type_var.value.expression = Some(expr);
            Ok(())
        } else {
            Err(DMError::new(location, "var must have a name"))
        }
    }

    // an entry which is definitely a proc because an argument list is specified
    pub fn add_proc<'a, I: Iterator<Item=&'a str>>(&mut self,
        location: Location,
        mut path: I,
        len: usize,
        parameters: Vec<Parameter>,
    ) -> Result<(usize, &mut ProcValue), DMError> {
        let (parent, mut proc_name) = self.get_from_path(location, &mut path, len)?;
        let mut is_verb = None;
        if is_proc_decl(proc_name) {
            is_verb = Some(proc_name == "verb");
            proc_name = match path.next() {
                Some(name) => name,
                None => return Err(DMError::new(location, "proc must have a name"))
            };
        } else if is_var_decl(proc_name) {
            return Err(DMError::new(location, "var looks like a proc"))
        }
        if let Some(other) = path.next() {
            return Err(DMError::new(location, format!("proc name must be a single identifier (spurious {:?})", other)))
        }

        self.register_proc(location, parent, proc_name, is_verb, parameters)
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
