use std::collections::Bound;
use std::vec;
use crate::{IntervalTree, RangeInclusive};
use crate::node::Node;

#[derive(Debug, Clone)]
enum Visiting {
    Left,
    Center,
    Middle(usize),
    Right,
}

/// An iterator over those members of an `IntervalTree` which intersect some range.
#[derive(Debug)]
pub struct RangePairIter<'a, K: 'a, V: 'a> {
    start: Bound<K>,
    end: Bound<K>,
    stack: Vec<(&'a Node<K, V>, Visiting)>,
}

impl<'a, K: Ord, V> RangePairIter<'a, K, V> {
    pub(crate) fn new(tree: &'a IntervalTree<K, V>, start: Bound<K>, end: Bound<K>) -> Self {
        let mut stack = Vec::with_capacity(tree.height());
        if let Some(ref root) = tree.root {
            stack.push((&**root, Visiting::Left));
        }
        RangePairIter { start, end, stack }
    }

    fn visit_left(&mut self, node: &'a Node<K, V>) {
        match node.left {
            Some(ref lsucc) => {
                self.stack.push((node, Visiting::Center));
                if match self.start {  // left_subtree_relevant
                    Bound::Included(ref start) => node.max >= *start,
                    Bound::Excluded(ref start) => node.max > *start,
                    Bound::Unbounded => true,
                } {
                    self.stack.push((&**lsucc, Visiting::Left))
                }
            },
            None => self.stack.push((node, Visiting::Center))
        }
    }

    fn visit_right(&mut self, node: &'a Node<K, V>) {
        if !match self.end {  // right_subtree_relevant
            Bound::Included(ref end) => *end >= node.key.start,
            Bound::Excluded(ref end) => *end > node.key.start,
            Bound::Unbounded => true,
        } { return }
        if let Some(ref rsucc) = node.right {
            self.stack.push((&**rsucc, Visiting::Left));
        }
    }

    fn visit_center(&mut self, node: &'a Node<K, V>) {
        if intersect(&self.start, &self.end, &node.key) {
            self.stack.push((node, Visiting::Middle(0)));
        } else {
            self.stack.push((node, Visiting::Right));
        }
    }

    fn get_next_node(&mut self) -> Option<(&'a Node<K, V>, usize)> {
        while let Some((node, state)) = self.stack.pop() {
            match state {
                Visiting::Left => self.visit_left(node),
                Visiting::Right => self.visit_right(node),
                Visiting::Center => self.visit_center(node),
                Visiting::Middle(i) => {
                    if i >= node.data.len() {
                        self.stack.push((node, Visiting::Right));
                    } else {
                        self.stack.push((node, Visiting::Middle(i + 1)));
                        return Some((node, i));
                    }
                }
            }
        }
        None
    }
}

impl<'a, K: Ord + Clone, V> Iterator for RangePairIter<'a, K, V> {
    type Item = (RangeInclusive<K>, &'a V);

    fn next(&mut self) -> Option<(RangeInclusive<K>, &'a V)> {
        self.get_next_node().map(|(n, i)| (n.key.clone(), &n.data[i]))
    }
}

impl<'a, K: Clone, V> Clone for RangePairIter<'a, K, V> {
    fn clone(&self) -> Self {
        RangePairIter {
            start: self.start.clone(),
            end: self.end.clone(),
            stack: self.stack.clone(),
        }
    }
}

fn intersect<T: Ord>(start: &Bound<T>, end: &Bound<T>, range: &RangeInclusive<T>) -> bool {
    (match *start {
        Bound::Included(ref start) => range.end >= *start,
        Bound::Excluded(ref start) => range.end > *start,
        Bound::Unbounded => true,
    }) && match *end {
        Bound::Included(ref end) => range.start <= *end,
        Bound::Excluded(ref end) => range.start < *end,
        Bound::Unbounded => true,
    }
}

pub struct IntoIter<K, V> {
    stack: Vec<Node<K, V>>,
    current: Option<(RangeInclusive<K>, vec::IntoIter<V>)>,
}

impl<K, V> IntoIter<K, V> {
    pub(crate) fn new(tree: IntervalTree<K, V>) -> Self {
        let mut this = IntoIter {
            stack: Vec::new(),
            current: None,
        };
        this.push_node(tree.root);
        this
    }

    fn push_node(&mut self, mut node: Option<Box<Node<K, V>>>) {
        while let Some(mut current) = node {
            node = current.left.take();
            self.stack.push(*current);
        }
    }
}

impl<K: Clone, V> Iterator for IntoIter<K, V> {
    type Item = (RangeInclusive<K>, V);

    fn next(&mut self) -> Option<(RangeInclusive<K>, V)> {
        loop {
            if let Some((key, mut iter)) = self.current.take() {
                if let Some(value) = iter.next() {
                    self.current = Some((key.clone(), iter));
                    return Some((key, value));
                }
            }

            if self.current.is_none() {
                let mut node = match self.stack.pop() {
                    Some(node) => node,
                    None => return None,
                };
                self.push_node(node.right.take());
                self.current = Some((node.key, node.data.into_iter()));
            }
        }
    }
}

#[test]
fn test_iterators() {
    let mut tree = IntervalTree::<u64, i32>::new();
    tree.insert(RangeInclusive::new(18,18), 1337);
    tree.insert(RangeInclusive::new(13,13), 1338);
    tree.insert(RangeInclusive::new(17,17), 1339);
    tree.insert(RangeInclusive::new(10,10), 1321);
    tree.insert(RangeInclusive::new(1 ,1), 1321);
    tree.insert(RangeInclusive::new(3 ,3), 1322);

    let iter = RangePairIter::new(&tree, Bound::Included(0), Bound::Included(1000));

    for (k,v) in iter {
        println!("{:?} {}",k,v);
    }

    let mut iter = RangePairIter::new(&tree, Bound::Included(0), Bound::Included(1000));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(1 ,1 ));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(3 ,3 ));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(10,10));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(13,13));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(17,17));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(18,18));
    assert!(iter.next().is_none());

    let mut iter = RangePairIter::new(&tree, Bound::Included(3), Bound::Included(17));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(3 ,3 ));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(10,10));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(13,13));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(17,17));
    assert!(iter.next().is_none());
}

#[test]
fn test_into_iter() {
    let mut tree = IntervalTree::<u64, i32>::new();
    tree.insert(RangeInclusive::new(18,18), 1337);
    tree.insert(RangeInclusive::new(13,13), 1338);
    tree.insert(RangeInclusive::new(17,17), 1339);
    tree.insert(RangeInclusive::new(10,10), 1321);
    tree.insert(RangeInclusive::new(1 ,1), 1321);
    tree.insert(RangeInclusive::new(1 ,1), 1350);
    tree.insert(RangeInclusive::new(3 ,3), 1322);

    let values: Vec<_> = tree.into_iter().collect();
    assert_eq!(values, vec![
        (RangeInclusive::new(1, 1), 1321),
        (RangeInclusive::new(1, 1), 1350),
        (RangeInclusive::new(3, 3), 1322),
        (RangeInclusive::new(10, 10), 1321),
        (RangeInclusive::new(13, 13), 1338),
        (RangeInclusive::new(17, 17), 1339),
        (RangeInclusive::new(18, 18), 1337),
    ]);
}
