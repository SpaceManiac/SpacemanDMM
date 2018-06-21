//! Data structures for the parser to output mappings from input ranges to AST
//! elements at those positions.

use interval_tree::{IntervalTree, RangePairIter, RangeInclusive, range};
use super::Location;

pub type Iter<'a> = RangePairIter<'a, Location, Annotation>;

#[derive(Debug)]
pub enum Annotation {
    TreeBlock(Vec<String>),
    TreePath(Vec<String>),
    ProcBody(Vec<String>),
    Ident(String),
}

pub struct AnnotationTree {
    tree: IntervalTree<Location, Annotation>,
    len: usize,
}

impl Default for AnnotationTree {
    fn default() -> Self {
        AnnotationTree {
            tree: IntervalTree::new(),
            len: 0,
        }
    }
}

impl AnnotationTree {
    pub fn insert(&mut self, place: ::std::ops::Range<Location>, value: Annotation) {
        self.tree.insert(range(place.start, place.end.pred()), value);
        self.len += 1;
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn iter(&self) -> Iter {
        self.tree.iter()
    }

    pub fn get_location(&self, loc: Location) -> Iter {
        self.tree.range(range(loc.pred(), loc))
    }

    pub fn get_range(&self, place: ::std::ops::Range<Location>) -> Iter {
        self.tree.range(range(place.start, place.end.pred()))
    }

    pub fn get_range_raw(&self, place: RangeInclusive<Location>) -> Iter {
        self.tree.range(place)
    }
}
