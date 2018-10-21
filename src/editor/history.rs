//! Reified undo/redo history tree.

use std::cell::Cell;

use petgraph::Direction;
use petgraph::graph::{NodeIndex, Graph};
use petgraph::visit::EdgeRef;

pub struct History<T, E> {
    current: T,
    idx: NodeIndex,
    clean_idx: Cell<NodeIndex>,
    graph: Graph<Entry, Edit<T, E>>,
}

struct Entry {
    desc: String,
}

struct Edit<T, E> {
    redo: Box<Fn(&E, &mut T) -> Box<Fn(&E, &mut T)>>,
    undo: Box<Fn(&E, &mut T)>,
}

impl<T, E> History<T, E> {
    pub fn new(desc: String, current: T) -> Self {
        let mut graph = Graph::default();
        let idx = graph.add_node(Entry { desc });
        History {
            current,
            idx,
            clean_idx: Cell::new(idx),
            graph,
        }
    }

    pub fn current(&self) -> &T {
        &self.current
    }

    pub fn replace_current(&mut self, new: T) {
        // TODO: somehow verify that the new value is semantically equivalent
        // to what we currently have.
        self.current = new;
    }

    pub fn is_dirty(&self) -> bool {
        self.clean_idx.get() != self.idx
    }

    pub fn mark_clean(&self) {
        self.clean_idx.set(self.idx);
    }

    pub fn edit<F: 'static + Fn(&E, &mut T) -> Box<Fn(&E, &mut T)>>(&mut self, env: &E, desc: String, f: F) {
        // perform the edit immediately
        let undo = f(env, &mut self.current);

        // save the edit to the history
        let new_idx = self.graph.add_node(Entry {
            desc,
        });
        self.graph.add_edge(self.idx, new_idx, Edit {
            redo: Box::new(f),
            undo,
        });
        self.idx = new_idx;
    }

    fn parent(&self, idx: NodeIndex) -> Option<NodeIndex> {
        self.graph.neighbors_directed(idx, Direction::Incoming).next()
    }

    pub fn can_undo(&self) -> bool {
        self.parent(self.idx).is_some()
    }

    pub fn undo(&mut self, env: &E) {
        if let Some(edge) = self.graph.edges_directed(self.idx, Direction::Incoming).last() {
            (edge.weight().undo)(env, &mut self.current);
            self.idx = edge.source();
        }
    }

    pub fn can_redo(&self) -> bool {
        self.graph
            .neighbors_directed(self.idx, Direction::Outgoing)
            .next()
            .is_some()
    }

    pub fn redo(&mut self, env: &E) {
        let mut undo = None;
        if let Some(edge) = self.graph.edges_directed(self.idx, Direction::Outgoing).last() {
            undo = Some((edge.id(), (edge.weight().redo)(env, &mut self.current)));
            self.idx = edge.target();
        }
        if let Some((id, undo)) = undo {
            self.graph.edge_weight_mut(id).unwrap().undo = undo;
        }
    }
}
