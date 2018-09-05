//! Reified undo/redo history tree.
#![allow(dead_code)]  // WIP

pub struct History<T> {
    current: T,
}

struct Entry<T> {
    desc: String,
    edit: Box<Fn(&mut T) -> bool>,
    snapshot: Option<T>,
}

impl<T> History<T> {
    pub fn new(desc: String, current: T) -> Self {
        let _ = desc; // TODO
        History { current }
    }

    pub fn current(&self) -> &T {
        &self.current
    }

    pub fn edit<F: Fn(&mut T)>(&mut self, desc: String, f: F) {
        let _ = desc;  // TODO
        f(&mut self.current);
    }

    pub fn undo(&mut self) {
        unimplemented!()
    }

    pub fn redo(&mut self) {
        unimplemented!()
    }
}

