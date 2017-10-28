//! The object tree representation, used as a parsing target.

use std::collections::BTreeMap;
use std::cell::Cell;

use petgraph::graph::{Graph, NodeIndex};
use petgraph::visit::EdgeRef;

use super::lexer::Token;
use super::{DMError, Location};

pub type Vars = ::linked_hash_map::LinkedHashMap<String, VarValue>;

#[derive(Debug)]
pub struct VarValue {
    pub is_static: bool,
    pub is_const: bool,
    pub is_tmp: bool,
    pub value: Option<Vec<Token>>,
}

impl VarValue {
    fn set_value(&mut self, location: Location, value: Vec<Token>) -> Result<(), DMError> {
        if !self.is_static {
            super::constants::evaluate(location, &value)?;
        }
        self.value = Some(value);
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct Type {
    name: String,
    pub path: String,
    pub vars: Vars,
    parent_type: Cell<NodeIndex>,
}

#[derive(Debug)]
pub struct ObjectTree {
    graph: Graph<Type, ()>,
    types: BTreeMap<String, NodeIndex>,
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

    pub fn blank_vars(&self) -> &Vars {
        &self.blank_vars
    }

    // ------------------------------------------------------------------------
    // Parsing

    pub fn assign_parent_types(&mut self) -> Result<(), DMError> {
        for (path, &type_idx) in self.types.iter() {
            let type_ = self.graph.node_weight(type_idx).unwrap();

            let parent_type = if path == "/datum" {
                continue; // parent is 0
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

            type_.parent_type.set(self.types[parent_type]);
        }
        Ok(())
    }

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
            if prev == "global" || prev == "static" || prev == "tmp" || prev == "const" {
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

        let mut type_path = String::new();
        for each in rest {
            type_path.push('/');
            type_path.push_str(prev);
            prev = each;
        }

        // TODO: track the type path
        let node = self.graph.node_weight_mut(parent).unwrap();
        Ok(node.vars.entry(prev.to_owned()).or_insert_with(|| VarValue {
            is_static,
            is_const,
            is_tmp,
            value: None
        }))
    }

    // an entry which may be anything depending on the path
    pub fn add_entry<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, mut path: I) -> Result<(), DMError> {
        let (parent, child) = self.get_from_path(location, &mut path)?;
        if child == "var" {
            self.register_var(location, parent, "var", path)?;
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
