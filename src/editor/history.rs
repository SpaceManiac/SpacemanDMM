//! Reified undo/redo history tree.
#![allow(dead_code)]  // WIP

use petgraph::Direction;
use petgraph::graph::{NodeIndex, Graph};

pub struct History<T> {
    current: T,
    idx: NodeIndex,
    graph: Graph<Entry<T>, ()>,
}

struct Entry<T> {
    desc: String,
    snapshot: Option<T>,
    edit: Box<Fn(&mut T)>,
}

impl<T: Clone> History<T> {
    pub fn new(desc: String, current: T) -> Self {
        let mut graph = Graph::default();
        let idx = graph.add_node(Entry {
            desc,
            snapshot: Some(current.clone()),
            edit: Box::new(|_| {}),
        });
        History {
            current,
            idx,
            graph,
        }
    }

    pub fn current(&self) -> &T {
        &self.current
    }

    pub fn edit<F: Fn(&mut T) + 'static>(&mut self, desc: String, f: F) {
        // perform the edit immediately
        f(&mut self.current);

        // save the edit to the history
        let new_idx = self.graph.add_node(Entry {
            desc,
            snapshot: None,
            edit: Box::new(f),
        });
        self.graph.add_edge(self.idx, new_idx, ());
        self.idx = new_idx;
    }

    fn parent(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.graph.neighbors_directed(idx, Direction::Incoming).next()
    }

    fn get_snapshot(&mut self, idx: NodeIndex) -> &T {
        if self.graph.node_weight(idx).unwrap().snapshot.is_none() {
            if let Some(parent) = self.parent(idx) {
                let mut snapshot = self.get_snapshot(parent).clone();
                let node = self.graph.node_weight_mut(idx).unwrap();
                (node.edit)(&mut snapshot);
                node.snapshot = Some(snapshot);
                return node.snapshot.as_ref().unwrap();
            }
            panic!("root of history had no snapshot")
        }
        self.graph.node_weight(idx).unwrap().snapshot.as_ref().unwrap()
    }

    pub fn can_undo(&self) -> bool {
        self.parent(self.idx).is_some()
    }

    pub fn undo(&mut self) {
        if let Some(parent) = self.parent(self.idx) {
            self.current = self.get_snapshot(parent).clone();
        }
    }

    pub fn can_redo(&self) -> bool {
        self.graph.neighbors_directed(self.idx, Direction::Outgoing).next().is_some()
    }

    pub fn redo(&mut self) {
        if let Some(child) = self.graph.neighbors_directed(self.idx, Direction::Outgoing).last() {
            self.current = self.get_snapshot(child).clone();
        }
    }
}
