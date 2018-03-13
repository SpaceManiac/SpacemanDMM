//! The object tree representation, used as a parsing target.

use std::collections::BTreeMap;

pub use petgraph::graph::NodeIndex;
use petgraph::graph::Graph;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use linked_hash_map::LinkedHashMap;

use super::ast::{Expression, TypePath, PathOp, Prefab};
use super::constants::Constant;
use super::{DMError, Location, Context};

// ----------------------------------------------------------------------------
// Variables

pub type Vars = LinkedHashMap<String, Constant>;

#[derive(Debug, Clone)]
pub struct VarDeclaration {
    pub is_static: bool,
    pub is_const: bool,
    pub is_tmp: bool,
    pub type_path: TypePath,
    pub location: Location,
}

impl VarDeclaration {
    #[inline]
    pub fn is_const_evaluable(&self) -> bool {
        self.is_const || (!self.is_static && !self.is_tmp)
    }
}

#[derive(Debug, Clone)]
pub struct VarValue {
    pub location: Location,
    /// Syntactic value, as specified in the source.
    pub expression: Option<Expression>,
    /// Evaluated value for non-static and non-tmp vars.
    pub constant: Option<Constant>,
    pub being_evaluated: bool,
}

#[derive(Debug, Clone)]
pub struct TypeVar {
    pub value: VarValue,
    pub declaration: Option<VarDeclaration>,
}

// ----------------------------------------------------------------------------
// Types

const BAD_NODE_INDEX: usize = ::std::usize::MAX;

#[derive(Debug, Default)]
pub struct Type {
    pub name: String,
    pub path: String,
    pub location: Location,
    pub vars: LinkedHashMap<String, TypeVar>,
    parent_type: NodeIndex,
}

impl Type {
    pub fn parent_type(&self) -> Option<NodeIndex> {
        if self.parent_type == NodeIndex::new(BAD_NODE_INDEX) {
            None
        } else {
            Some(self.parent_type)
        }
    }

    /// Checks whether this type's path is a subpath of the given path.
    #[inline]
    pub fn is_subpath_of(&self, parent: &str) -> bool {
        subpath(&self.path, parent)
    }

    /// Checks whether this type is a subtype of the given type.
    pub fn is_subtype_of(&self, parent: &Type, objtree: &ObjectTree) -> bool {
        let mut current = Some(self);
        while let Some(ty) = current.take() {
            if ::std::ptr::eq(ty, parent) { return true }
            current = objtree.parent_of(ty);
        }
        false
    }

    /// Checks whether this type is a supertype (inclusive) of the given type.
    #[inline]
    pub fn is_supertype_of(&self, child: &Type, objtree: &ObjectTree) -> bool {
        child.is_subtype_of(self, objtree)
    }

    pub fn get_declaration<'a>(&'a self, name: &str, objtree: &'a ObjectTree) -> Option<&'a VarDeclaration> {
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

    // Intended to be used by the constant evaluator only
    #[doc(hidden)]
    pub fn get_value_mut(&mut self, name: &str, objtree: &ObjectTree)
        -> Option<(&mut VarValue, VarDeclaration)>
    {
        match {
            let mut current = &*self;
            loop {
                if let Some(var) = current.vars.get(name) {
                    if let Some(decl) = var.declaration.as_ref() {
                        break Some(decl.clone());
                    }
                }
                match objtree.graph.node_weight(current.parent_type) {
                    None => break None,
                    Some(x) => current = x,
                }
            }
        } {
            None => None,
            Some(decl) => match self.vars.get_mut(name) {
                Some(var) => Some((&mut var.value, decl)),
                None => None,
            }
        }
    }
}

#[inline]
pub fn subpath(path: &str, parent: &str) -> bool {
    debug_assert!(path.starts_with("/") && parent.starts_with("/") && parent.ends_with("/"));
    path == &parent[..parent.len() - 1] || path.starts_with(parent)
}

// ----------------------------------------------------------------------------
// Type references

#[derive(Debug, Copy, Clone)]
pub struct TypeRef<'a> {
    idx: NodeIndex,
    ty: &'a Type,
}

impl<'a> TypeRef<'a> {
    fn new(tree: &'a ObjectTree, idx: NodeIndex) -> TypeRef<'a> {
        TypeRef {
            idx,
            ty: tree.graph.node_weight(idx).unwrap(),
        }
    }

    pub fn parent(&self, objtree: &'a ObjectTree) -> Option<TypeRef<'a>> {
        objtree.graph.neighbors_directed(self.idx, Direction::Incoming).next().map(|i| TypeRef::new(objtree, i))
    }

    pub fn parent_type(&self, objtree: &'a ObjectTree) -> Option<TypeRef<'a>> {
        let idx = self.ty.parent_type;
        objtree.graph.node_weight(idx).map(|ty| TypeRef { idx, ty })
    }

    pub fn child(&self, name: &str, objtree: &'a ObjectTree) -> Option<TypeRef<'a>> {
        for idx in objtree.graph.neighbors(self.idx) {
            let ty = objtree.graph.node_weight(idx).unwrap();
            if ty.name == name {
                return Some(TypeRef { idx, ty });
            }
        }
        None
    }

    pub fn children(&self, objtree: &'a ObjectTree) -> Vec<TypeRef<'a>> {
        let mut output = Vec::new();
        for idx in objtree.graph.neighbors(self.idx) {
            output.push(TypeRef::new(objtree, idx));
        }
        output
    }

    pub fn get(&self) -> &'a Type {
        self.ty
    }
}

impl<'a> ::std::ops::Deref for TypeRef<'a> {
    type Target = Type;
    fn deref(&self) -> &Type {
        self.ty
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
            vars: Default::default(),
            parent_type: NodeIndex::new(BAD_NODE_INDEX),
        });
        tree
    }
}

impl ObjectTree {
    pub fn with_builtins() -> Self {
        let mut tree = Self::default();
        super::builtins::register_builtins(&mut tree).unwrap();
        tree
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

    pub fn type_by_path(&self, path: &TypePath) -> Option<&Type> {
        let mut current = NodeIndex::new(0);
        'outer: for &(_, ref each) in path {
            for edge in self.graph.edges(current) {
                let target = edge.target();
                if self.graph.node_weight(target).unwrap().name == *each {
                    current = target;
                    continue 'outer;
                }
            }
            return None;
        }
        Some(self.graph.node_weight(current).unwrap())
    }

    pub fn type_by_constant(&self, constant: &Constant) -> Option<&Type> {
        match constant {
            &Constant::String(ref string_path) => self.find(string_path).map(|tr| tr.get()),
            &Constant::Prefab(Prefab { ref path, .. }) => self.type_by_path(path),
            _ => None,
        }
    }

    // ------------------------------------------------------------------------
    // Finalization

    pub(crate) fn finalize(&mut self, context: &Context) {
        self.assign_parent_types(context);
        super::constants::evaluate_all(context, self);
    }

    fn assign_parent_types(&mut self, context: &Context) {
        for (path, &type_idx) in self.types.iter() {
            let idx = if path == "/datum" {
                NodeIndex::new(0)
            } else {
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
                    // TODO
                    /*match type_.vars.get("parent_type") {
                        Some(name) => name,
                        None => */match path.rfind("/").unwrap() {
                            0 => "/datum",
                            idx => &path[..idx],
                        }
                    //}
                };

                if let Some(&idx) = self.types.get(parent_type) {
                    idx
                } else {
                    context.register_error(DMError::new(Location::default(), format!("bad parent_type for {}: {}", path, parent_type)));
                    continue;
                }
            };

            self.graph.node_weight_mut(type_idx)
                .unwrap()
                .parent_type = idx;
        }
    }

    // ------------------------------------------------------------------------
    // XML Output

    #[cfg(feature="xml-rs")]
    pub fn to_xml(&self, path: &::std::path::Path) -> ::xml::writer::Result<()> {
        use xml::writer::events::XmlEvent;

        let mut out = ::xml::EventWriter::new_with_config(::std::fs::File::create(path)?, ::xml::EmitterConfig {
            perform_indent: true,
            .. Default::default()
        });
        out.write(XmlEvent::StartDocument {
            version: ::xml::common::XmlVersion::Version10,
            encoding: None,
            standalone: None,
        })?;
        self.to_xml_ty(&mut out, NodeIndex::new(0))
    }

    #[cfg(feature="xml-rs")]
    fn to_xml_ty(&self, out: &mut ::xml::EventWriter<::std::fs::File>, ty: NodeIndex) -> ::xml::writer::Result<()> {
        use xml::writer::events::XmlEvent;
        out.write(XmlEvent::start_element(::xml::name::Name::local("object")))?;
        {
            let node = self.graph.node_weight(ty).unwrap();
            out.write(&*node.name)?;

            for (key, val) in node.vars.iter() {
                out.write(XmlEvent::start_element(::xml::name::Name::local("var")))?;
                out.write(&**key)?;
                if let Some(ref value) = val.value.constant {
                    out.write(XmlEvent::start_element(::xml::name::Name::local("val")))?;
                    out.write(&*value.to_string())?;
                    out.write(XmlEvent::end_element())?;
                }
                out.write(XmlEvent::end_element())?;
            }
        }
        let mut neighbors = self.graph.neighbors(ty).collect::<Vec<_>>();
        neighbors.sort();
        for child in neighbors {
            self.to_xml_ty(out, child)?;
        }
        out.write(XmlEvent::end_element())?;
        Ok(())
    }

    // ------------------------------------------------------------------------
    // Parsing

    fn subtype_or_add(&mut self, location: Location, parent: NodeIndex, child: &str) -> NodeIndex {
        for edge in self.graph.edges(parent) {
            let target = edge.target();
            if self.graph.node_weight(target).unwrap().name == child {
                return target;
            }
        }

        // time to add a new child
        let path = format!("{}/{}", self.graph.node_weight(parent).unwrap().path, child);
        let node = self.graph.add_node(Type {
            name: child.to_owned(),
            path: path.clone(),
            vars: Default::default(),
            location: location,
            parent_type: NodeIndex::new(BAD_NODE_INDEX),
        });
        self.graph.add_edge(parent, node, ());
        self.types.insert(path, node);
        node
    }

    fn get_from_path<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, mut path: I) -> Result<(NodeIndex, &'a str), DMError> {
        let mut current = NodeIndex::new(0);
        let mut last = match path.next() {
            Some(name) => name,
            None => return Err(DMError::new(location, "cannot register root path")),
        };
        if is_decl(last) {
            return Ok((current, last));
        }
        for each in path {
            current = self.subtype_or_add(location, current, last);
            last = each;
            if is_decl(last) {
                break;
            }
        }

        Ok((current, last))
    }

    fn register_var<'a, I>(&mut self, location: Location, parent: NodeIndex, mut prev: &'a str, mut rest: I) -> Result<Option<&mut TypeVar>, DMError> where
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
                // TODO: store this information
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
            type_path.push((PathOp::Slash, prev.to_owned()));
            prev = each;
        }
        // TODO: track the type path
        let node = self.graph.node_weight_mut(parent).unwrap();
        Ok(Some(node.vars.entry(prev.to_owned()).or_insert_with(|| TypeVar {
            value: VarValue {
                location,
                expression: None,
                constant: None,
                being_evaluated: false,
            },
            declaration: if is_declaration {
                Some(VarDeclaration {
                    is_static,
                    is_const,
                    is_tmp,
                    type_path,
                    location,
                })
            } else {
                None
            },
        })))
    }

    // an entry which may be anything depending on the path
    pub fn add_entry<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, mut path: I) -> Result<(), DMError> {
        let (parent, child) = self.get_from_path(location, &mut path)?;
        if is_var_decl(child) {
            self.register_var(location, parent, "var", path)?;
        } else if is_proc_decl(child) {
            // proc{} block, children will be procs
        } else {
            self.subtype_or_add(location, parent, child);
        }
        Ok(())
    }

    // an entry which is definitely a var because a value is specified
    pub fn add_var<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, mut path: I, expr: Expression) -> Result<(), DMError> {
        let (parent, initial) = self.get_from_path(location, &mut path)?;
        if let Some(type_var) = self.register_var(location, parent, initial, path)? {
            type_var.value.location = location;
            type_var.value.expression = Some(expr);
            Ok(())
        } else {
            Err(DMError::new(location, "var must have a name"))
        }
    }

    // an entry which is definitely a proc because an argument list is specified
    pub fn add_proc<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, mut path: I) -> Result<(), DMError> {
        let (parent, mut proc_name) = self.get_from_path(location, &mut path)?;
        if is_proc_decl(proc_name) {
            proc_name = match path.next() {
                Some(name) => name,
                None => return Err(DMError::new(location, "proc must have a name"))
            };
        } else if is_var_decl(proc_name) {
            return Err(DMError::new(location, "var looks like a proc"))
        }
        if path.next().is_some() {
            return Err(DMError::new(location, "proc name must be a single identifier"))
        }
        // TODO: keep track of procs
        let _ = (parent, proc_name);
        Ok(())
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
