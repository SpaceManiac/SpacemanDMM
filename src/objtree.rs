use std::io;
use std::path::Path;
use std::collections::BTreeMap;
use std::cell::Cell;

use petgraph::graph::{Graph, NodeIndex};
use xml::EventReader;
use xml::reader::XmlEvent;

pub type Vars = BTreeMap<String, String>;

#[derive(Debug)]
pub struct ObjectTree {
    graph: Graph<Type, ()>,
    types: BTreeMap<String, NodeIndex>,
    blank_vars: Vars,
}

#[derive(Debug, Default)]
pub struct Type {
    name: String,
    pub path: String,
    pub vars: Vars,
    parent_type: Cell<NodeIndex>,
}

impl ObjectTree {
    pub fn from_file(path: &Path) -> io::Result<ObjectTree> {
        flame!("ObjectTree::from_file");
        ObjectTree::parse(io::BufReader::new(::std::fs::File::open(path)?))
    }

    pub fn parse<R: io::BufRead>(mut file: R) -> io::Result<ObjectTree> {
        use std::io::Read;

        let mut buf = String::new();
        file.read_line(&mut buf)?;
        while buf.starts_with("loading ") {
            buf.clear();
            file.read_line(&mut buf)?;
        }
        let mut chain = io::Cursor::new(buf.into_bytes()).chain(file);
        let mut er = EventReader::new(&mut chain as &mut io::Read);

        match er.next().unwrap() {
            XmlEvent::StartDocument { .. } => {},
            e => panic!("{:?}", e),
        }
        match er.next().unwrap() {
            XmlEvent::StartElement { name, .. } => assert_eq!(name.local_name, "dm"),
            e => panic!("{:?}", e),
        }

        let mut tree = ObjectTree {
            graph: Graph::new(),
            types: BTreeMap::new(),
            blank_vars: BTreeMap::new(),
        };
        let root = tree.graph.add_node(Type {
            name: String::new(),
            path: String::new(),
            vars: Default::default(),
            parent_type: Cell::new(NodeIndex::new(::std::usize::MAX)),
        });
        loop {
            let next = er.next().unwrap();
            match next { // TODO: error handling
                XmlEvent::Whitespace(_) => {},
                XmlEvent::StartElement { name, .. } => parse_tag(&mut tree, root, &mut er, &name.local_name)?,
                XmlEvent::EndElement { .. } => break,
                e => panic!("{:?}", e)
            }
        }

        loop {
            match er.next().unwrap() {
                XmlEvent::Whitespace(_) => {}
                XmlEvent::EndDocument { .. } => break,
                e => panic!("{:?}", e),
            }
        }

        // assign parent_type
        for (path, &type_idx) in tree.types.iter() {
            let type_ = tree.graph.node_weight(type_idx).unwrap();

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
                match type_.vars.get("parent_type") {
                    Some(name) => name,
                    None => match path.rfind("/").unwrap() {
                        0 => "/datum",
                        idx => &path[..idx],
                    }
                }
            };

            type_.parent_type.set(tree.types[parent_type]);
        }

        Ok(tree)
    }

    pub fn find(&self, path: &str) -> Option<&Type> {
        self.types.get(path).and_then(|&ix| self.graph.node_weight(ix))
    }

    pub fn parent_of(&self, type_: &Type) -> Option<&Type> {
        self.graph.node_weight(type_.parent_type.get())
    }

    pub fn blank_vars(&self) -> &Vars {
        &self.blank_vars
    }
}

fn parse_tag(tree: &mut ObjectTree, parent: NodeIndex, er: &mut EventReader<&mut io::Read>, name: &str) -> io::Result<()> {
    if name == "object" || name == "area" || name == "turf" || name == "obj" || name == "mob" {
        parse_type(tree, parent, er)
    } else if name == "var" {
        parse_var(tree, parent, er)
    } else if name == "proc" || name == "verb" {
        parse_nothing(er)
    } else {
        panic!("{}", name)
    }
}

fn parse_nothing(er: &mut EventReader<&mut io::Read>) -> io::Result<()> {
    loop {
        match er.next().unwrap() {
            XmlEvent::StartElement { .. } => parse_nothing(er)?,
            XmlEvent::EndElement { .. } => break,
            _ => {}
        }
    }
    Ok(())
}

fn parse_type(tree: &mut ObjectTree, parent: NodeIndex, er: &mut EventReader<&mut io::Read>) -> io::Result<()> {
    let mut me = None;

    loop {
        match er.next().unwrap() {
            XmlEvent::Whitespace(_) => {},
            XmlEvent::Characters(content) => {
                let name = content.trim_right().to_owned();
                assert!(!name.is_empty());
                let path = format!("{}/{}", tree.graph[parent].path, name);
                let added = tree.graph.add_node(Type {
                    name,
                    path: path.clone(),
                    vars: Default::default(),
                    parent_type: Default::default(),
                });
                tree.graph.add_edge(parent, added, ());
                tree.types.insert(path, added);
                me = Some(added);
            }
            XmlEvent::StartElement { name, .. } => {
                let me = me.unwrap(); // all types should be named
                parse_tag(tree, me, er, &name.local_name)?;
            }
            XmlEvent::EndElement { .. } => break,
            e => panic!("{:?}", e)
        }
    }
    Ok(())
}

fn parse_var(tree: &mut ObjectTree, parent: NodeIndex, er: &mut EventReader<&mut io::Read>) -> io::Result<()> {
    let mut key = None;

    loop {
        match er.next().unwrap() {
            XmlEvent::Whitespace(_) => {},
            XmlEvent::Characters(content) => {
                let name = content.trim_right().to_owned();
                assert!(!name.is_empty());
                key = Some(name);
            }
            XmlEvent::StartElement { name, .. } => {
                let key = key.take().unwrap();
                assert_eq!(name.local_name, "val");
                parse_val(tree, parent, er, key)?;
            }
            XmlEvent::EndElement { .. } => break,
            e => panic!("{:?}", e)
        }
    }

    if let Some(key) = key {
        tree.graph[parent].vars.insert(key, String::new());
    }
    Ok(())
}

fn parse_val(tree: &mut ObjectTree, parent: NodeIndex, er: &mut EventReader<&mut io::Read>, key: String) -> io::Result<()> {
    let mut key = Some(key);

    loop {
        match er.next().unwrap() {
            XmlEvent::Whitespace(_) => {},
            XmlEvent::Characters(content) => {
                let value = content.trim_right().to_owned();
                tree.graph[parent].vars.insert(key.take().unwrap(), value);
            }
            XmlEvent::StartElement { name, .. } => {
                if name.local_name == "list" {
                    parse_nothing(er)?; // TODO: list parsing
                } else if name.local_name == "var" {
                    parse_nothing(er)?; // this is trash output, ignore it
                } else {
                    panic!("{}", name.local_name);
                }
            }
            XmlEvent::EndElement { .. } => break,
            e => panic!("{:?}", e)
        }
    }
    Ok(())
}
