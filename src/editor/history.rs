//! Reified undo/redo history tree.
#![allow(dead_code)]  // WIP

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

    pub fn undo(&mut self) {
        unimplemented!()
    }

    pub fn redo(&mut self) {
        unimplemented!()
    }
}
