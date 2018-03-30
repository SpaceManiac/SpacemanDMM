use {IntervalTree, RangeInclusive};
use node::Node;

#[derive(Debug, Clone)]
enum Visiting {
    Left,
    Center,
    Right,
}

/// An iterator over those members of an `IntervalTree` which intersect some range.
#[derive(Debug, Clone)]
pub struct RangePairIter<'a, K: 'a, V: 'a> {
    range: RangeInclusive<K>,
    stack: Vec<(&'a Node<K, V>, Visiting)>
}

impl<'a, K: Ord, V> RangePairIter<'a, K, V> {
    pub(crate) fn new(tree: &'a IntervalTree<K, V>, range: RangeInclusive<K>) -> Self {
        let mut stack = Vec::with_capacity(tree.height());
        if let Some(ref root) = tree.root {
            stack.push((&**root, Visiting::Left));
        }
        RangePairIter{ range, stack }
    }

    fn visit_left(&mut self, node: &'a Node<K, V>) {
        //println!("left {:?}", node.key);
        match node.left {
            Some(ref lsucc) => {
                self.stack.push((node, Visiting::Center));
                if node.left_subtree_relevant(&self.range) {
                    self.stack.push((&**lsucc, Visiting::Left))
                }
            },
            None => self.stack.push((node, Visiting::Center))
        }
    }

    fn visit_right(&mut self, node: &'a Node<K, V>) {
        if !node.right_subtree_relevant(&self.range) { return }
        if let Some(ref rsucc) = node.right {
            self.stack.push((&**rsucc, Visiting::Left));
        }
    }

    fn visit_center(&mut self, node: &'a Node<K, V>) -> Option<&'a Node<K, V>>{
        self.stack.push((node, Visiting::Right));
        if ::range::intersect(&node.key, &self.range) {
            Some(node)
        } else {
            None
        }
    }

    fn get_next_node(&mut self) -> Option<&'a Node<K, V>>{
        while let Some((node, state)) = self.stack.pop() {
            match state {
                Visiting::Left => self.visit_left(node),
                Visiting::Right => self.visit_right(node),
                Visiting::Center => if let Some(node) = self.visit_center(node) {
                    return Some(node)
                }
            }
        }
        None
    }
}

impl<'a, K: Ord + Clone, V> Iterator for RangePairIter<'a, K, V> {
    type Item = (RangeInclusive<K>, &'a V);

    fn next(&mut self) -> Option<(RangeInclusive<K>, &'a V)> {
        self.get_next_node().map(|n| (n.key.clone(), &n.data))
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

    let iter = RangePairIter::new(&tree, RangeInclusive::new(0, 1000));

    for (k,v) in iter {
        println!("{:?} {}",k,v);
    }

    let mut iter = RangePairIter::new(&tree, RangeInclusive::new(0, 1000));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(1 ,1 ));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(3 ,3 ));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(10,10));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(13,13));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(17,17));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(18,18));
    assert!(iter.next().is_none());

    let mut iter = RangePairIter::new(&tree, RangeInclusive::new(3, 17));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(3 ,3 ));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(10,10));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(13,13));
    assert_eq!(iter.next().expect("should have a few values").0, RangeInclusive::new(17,17));
    assert!(iter.next().is_none());
}
