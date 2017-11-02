//! The object tree representation, used as a parsing target.

use std::collections::BTreeMap;
use std::cell::Cell;

pub use petgraph::graph::NodeIndex;
use petgraph::graph::Graph;
use petgraph::visit::EdgeRef;
use linked_hash_map::LinkedHashMap;

use super::ast::{Expression, TypePath, PathOp, Prefab};
use super::constants::Constant;
use super::{DMError, Location};

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

#[derive(Debug, Default)]
pub struct Type {
    name: String,
    pub path: String,
    pub vars: LinkedHashMap<String, TypeVar>,
    parent_type: Cell<NodeIndex>,
}

impl Type {
    pub fn parent_type(&self) -> Option<NodeIndex> {
        let idx = self.parent_type.get();
        if idx == NodeIndex::new(::std::usize::MAX) {
            None
        } else {
            Some(idx)
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
                match objtree.graph.node_weight(current.parent_type.get()) {
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
            vars: Default::default(),
            parent_type: Cell::new(NodeIndex::new(::std::usize::MAX)),
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

    pub fn find(&self, path: &str) -> Option<&Type> {
        self.types.get(path).and_then(|&ix| self.graph.node_weight(ix))
    }

    pub fn parent_of(&self, type_: &Type) -> Option<&Type> {
        self.graph.node_weight(type_.parent_type.get())
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
            &Constant::String(ref string_path) => self.find(string_path),
            &Constant::Prefab(Prefab { ref path, .. }) => self.type_by_path(path),
            _ => None,
        }
    }

    // ------------------------------------------------------------------------
    // Finalization

    pub fn finalize(&mut self) -> Result<(), DMError> {
        self.assign_parent_types()?;
        super::constants::evaluate_all(self)?;
        Ok(())
    }

    fn assign_parent_types(&mut self) -> Result<(), DMError> {
        for (path, &type_idx) in self.types.iter() {
            let type_ = self.graph.node_weight(type_idx).unwrap();

            let parent_type = if path == "/datum" {
                // parented to the root type
                type_.parent_type.set(NodeIndex::new(0));
                continue;
            } else if path == "/atom" {
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

            type_.parent_type.set(match self.types.get(parent_type) {
                Some(&v) => v,
                None => return Err(DMError::new(Location::default(), format!("bad parent_type for {}: {}", path, parent_type)))
            });
        }
        Ok(())
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

    fn subtype_or_add(&mut self, parent: NodeIndex, child: &str) -> NodeIndex {
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
            parent_type: Cell::new(NodeIndex::new(::std::usize::MAX)),
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
            current = self.subtype_or_add(current, last);
            last = each;
            if is_decl(last) {
                break;
            }
        }

        Ok((current, last))
    }

    fn register_var<'a, I>(&mut self, location: Location, parent: NodeIndex, mut prev: &'a str, mut rest: I) -> Result<&mut TypeVar, DMError> where
        I: Iterator<Item=&'a str>
    {
        let (mut is_declaration, mut is_static, mut is_const, mut is_tmp) = (false, false, false, false);

        if is_var_decl(prev) {
            is_declaration = true;
            prev = match rest.next() {
                Some(name) => name,
                None => return Err(DMError::new(location, "var must have a name"))
            };
            while prev == "global" || prev == "static" || prev == "tmp" || prev == "const" {
                // TODO: store this information
                if let Some(name) = rest.next() {
                    is_static |= prev == "global" || prev == "static";
                    is_const |= prev == "const";
                    is_tmp |= prev == "tmp";
                    prev = name;
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
        Ok(node.vars.entry(prev.to_owned()).or_insert_with(|| TypeVar {
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
        }))
    }

    // an entry which may be anything depending on the path
    pub fn add_entry<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, mut path: I) -> Result<(), DMError> {
        let (parent, child) = self.get_from_path(location, &mut path)?;
        if is_var_decl(child) {
            self.register_var(location, parent, "var", path)?;
        } else if is_proc_decl(child) {
            return Err(DMError::new(location, "proc looks like a generic entry"))
        } else {
            self.subtype_or_add(parent, child);
        }
        Ok(())
    }

    // an entry which is definitely a var because a value is specified
    pub fn add_var<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, mut path: I, expr: Expression) -> Result<(), DMError> {
        let (parent, initial) = self.get_from_path(location, &mut path)?;
        let var = &mut self.register_var(location, parent, initial, path)?.value;
        var.location = location;
        var.expression = Some(expr);
        Ok(())
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
