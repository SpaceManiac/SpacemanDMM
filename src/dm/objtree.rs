//! The object tree representation, used as a parsing target.

use std::collections::BTreeMap;
use std::cell::Cell;

pub use petgraph::graph::NodeIndex;
use petgraph::graph::Graph;
use petgraph::visit::EdgeRef;

use super::lexer::Token;
use super::ast::{TypePath, PathOp};
use super::constants::Constant;
use super::{DMError, Location};

pub type Vars = ::linked_hash_map::LinkedHashMap<String, VarValue>;

#[derive(Debug, Clone)]
pub struct VarValue {
    pub is_static: bool,
    pub is_const: bool,
    pub is_tmp: bool,

    pub type_path: TypePath,

    pub location: Location,
    /// Evaluated value for non-static and non-tmp vars.
    pub value: Option<Constant>,
    /// Syntactic value, as specified in the source.
    pub full_value: Option<Vec<Token>>,

    pub being_evaluated: bool,
}

impl VarValue {
    fn set_value(&mut self, location: Location, value: Vec<Token>) -> Result<(), DMError> {
        // TODO warn if self.full_value.is_some()
        self.location = location;
        self.full_value = Some(value);
        Ok(())
    }

    pub fn is_const_evaluable(&self) -> bool {
        self.is_const || (!self.is_static && !self.is_tmp)
    }
}

#[derive(Debug, Default)]
pub struct Type {
    name: String,
    pub path: String,
    pub vars: Vars,
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
}

struct ParentIterMut<'a> {
    graph: &'a mut Graph<Type, ()>,
    current: Option<NodeIndex>,
}

impl<'a> ParentIterMut<'a> {
    pub fn next(&mut self) -> Option<&mut Type> {
        let i = match self.current.take() {
            None => return None,
            Some(i) => i,
        };
        let ty = match self.graph.node_weight_mut(i) {
            None => return None,
            Some(ty) => ty,
        };
        self.current = Some(ty.parent_type.get());
        Some(ty)
    }
}

#[derive(Debug)]
pub struct ObjectTree {
    pub graph: Graph<Type, ()>,
    pub types: BTreeMap<String, NodeIndex>,
    blank_vars: Vars,
    const_fns: Vec<Token>,
}

impl Default for ObjectTree {
    fn default() -> Self {
        let mut tree = ObjectTree {
            graph: Default::default(),
            types: Default::default(),
            blank_vars: Default::default(),
            const_fns: Default::default(),
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
    // ------------------------------------------------------------------------
    // Access

    pub fn find(&self, path: &str) -> Option<&Type> {
        self.types.get(path).and_then(|&ix| self.graph.node_weight(ix))
    }

    pub fn parent_of(&self, type_: &Type) -> Option<&Type> {
        self.graph.node_weight(type_.parent_type.get())
    }

    pub fn parent_of_mut(&mut self, type_: &Type) -> Option<&mut Type> {
        self.graph.node_weight_mut(type_.parent_type.get())
    }

    pub fn blank_vars(&self) -> &Vars {
        &self.blank_vars
    }

    // ------------------------------------------------------------------------
    // Finalization

    pub fn finalize(&mut self) -> Result<(), DMError> {
        self.assign_parent_types()?;
        self.evaluate_constants()?;
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
                    None =>*/ match path.rfind("/").unwrap() {
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

    fn evaluate_constants(&mut self) -> Result<(), DMError> {
        super::constants::evaluate_all(self)
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
        //println!("registered: {}", path);
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

    fn register_var<'a, I>(&mut self, location: Location, parent: NodeIndex, mut prev: &'a str, mut rest: I) -> Result<&mut VarValue, DMError> where
        I: Iterator<Item=&'a str>
    {
        let (mut is_static, mut is_const, mut is_tmp) = (false, false, false);

        if is_var_decl(prev) {
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
        Ok(node.vars.entry(prev.to_owned()).or_insert_with(|| VarValue {
            is_static,
            is_const,
            is_tmp,
            location,
            type_path,
            full_value: None,
            value: None,
            being_evaluated: false,
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
    pub fn add_var<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, mut path: I, value: Vec<Token>) -> Result<(), DMError> {
        let (parent, initial) = self.get_from_path(location, &mut path)?;
        self.register_var(location, parent, initial, path)?.set_value(location, value)?;
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
