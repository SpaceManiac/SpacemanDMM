use crate::iterators::{IntoIter, RangePairIter};
use crate::node::{height, Node};
use crate::range::RangeInclusive;
use std::collections::Bound;

/// An interval tree.
#[derive(Debug, Clone)]
pub struct IntervalTree<K, V> {
    pub(crate) root: Option<Box<Node<K, V>>>,
}

impl<K, V> Default for IntervalTree<K, V> {
    fn default() -> Self {
        IntervalTree { root: None }
    }
}

impl<K, V> IntervalTree<K, V> {
    /// This function will construct a new empty IntervalTree.
    /// # Examples
    /// ```
    /// extern crate interval_tree;
    /// let mut t=interval_tree::IntervalTree::<u64, i32>::new();
    /// ```
    pub fn new() -> Self {
        IntervalTree { root: None }
    }

    /// This function will return true if the tree is empty, false otherwise.
    /// # Examples
    /// ```
    /// extern crate interval_tree;
    ///
    /// let mut t=interval_tree::IntervalTree::<u64, i32>::new();
    /// assert!(t.is_empty());
    /// t.insert(interval_tree::range(2,2),25);
    /// assert!(!t.is_empty());
    ///
    /// ```
    pub fn is_empty(&self) -> bool {
        self.root.is_none()
    }

    /// This function will return the hieght of the tree. An empty tree hash height 0, one with only
    /// one elemente has height 1 etc.
    /// # Examples
    /// ```
    /// extern crate interval_tree;
    ///
    /// let mut t=interval_tree::IntervalTree::<u64, i32>::new();
    /// assert_eq!(t.height(), 0);
    /// t.insert(interval_tree::range(2,2),3);
    /// assert_eq!(t.height(), 1);
    ///
    /// ```
    pub fn height(&self) -> usize {
        height(&self.root) as usize
    }
}

impl<K: Ord + Clone, V> IntervalTree<K, V> {
    /// This function will insert the key,value pair into the tree, appending to
    /// the old data if the key is already part of the tree.
    ///
    /// # Examples
    /// ```
    /// extern crate interval_tree;
    ///
    /// let mut t=interval_tree::IntervalTree::<u64, i32>::new();
    /// t.insert(interval_tree::range(2,2),25);
    /// assert_eq!(t.get(interval_tree::range(2,2)), Some(&[25][..]));
    /// t.insert(interval_tree::range(2,2),30);
    /// assert_eq!(t.get(interval_tree::range(2,2)), Some(&[25, 30][..]));
    /// ```
    pub fn insert(&mut self, key: RangeInclusive<K>, data: V) {
        self.root = Some(match self.root.take() {
            Some(box_to_node) => box_to_node.insert(key, data),
            None => Box::new(Node::new(key, data)),
        });
    }

    /// This function will remove the key,value pair from the tree, doing nothing if the key is not
    /// part of the tree.
    /// # Examples
    /// ```
    /// extern crate interval_tree;
    ///
    /// let mut t=interval_tree::IntervalTree::<u64, i32>::new();
    /// t.insert(interval_tree::range(2,2),25);
    /// t.remove(interval_tree::range(2,2));
    /// assert!(t.is_empty());
    /// // deleting nonexistant keys doesn't do anything
    /// t.remove(interval_tree::range(3,3));
    /// assert!(t.is_empty());
    /// ```
    pub fn remove(&mut self, key: RangeInclusive<K>) {
        if let Some(box_to_node) = self.root.take() {
            self.root = box_to_node.delete(key);
        }
    }

    /// This function will return the Some(data) stored under the given key or None if the key is not
    /// known.
    /// # Examples
    /// ```
    /// extern crate interval_tree;
    ///
    /// let mut t=interval_tree::IntervalTree::<u64, i32>::new();
    /// t.insert(interval_tree::range(2,2),25);
    /// assert_eq!(t.get(interval_tree::range(2,2)), Some(&[25][..]));
    /// assert_eq!(t.get(interval_tree::range(3,3)), None);
    ///
    /// ```
    pub fn get(&self, key: RangeInclusive<K>) -> Option<&[V]> {
        match self.root {
            Some(ref box_to_node) => box_to_node.search(&key),
            None => None,
        }
    }

    /// This function will return the data stored under the given key or the default if the key is not
    /// known.
    /// # Examples
    /// ```
    /// extern crate interval_tree;
    ///
    /// let mut t=interval_tree::IntervalTree::<u64, i32>::new();
    /// t.insert(interval_tree::range(2,2),25);
    /// assert_eq!(t.get_or(interval_tree::range(2,2),&[2000]), &[25]);
    /// assert_eq!(t.get_or(interval_tree::range(3,3),&[2000]), &[2000]);
    ///
    /// ```
    pub fn get_or<'a>(&'a self, key: RangeInclusive<K>, default: &'a [V]) -> &'a [V] {
        self.get(key).unwrap_or(default)
    }

    /// This function will return true if the tree contains the given key, false otherwise
    /// # Examples
    /// ```
    /// extern crate interval_tree;
    ///
    /// let mut t=interval_tree::IntervalTree::<u64, i32>::new();
    /// t.insert(interval_tree::range(2,2),25);
    /// assert!(!t.contains(interval_tree::range(3,3)));
    /// assert!(t.contains(interval_tree::range(2,2)));
    ///
    /// ```
    pub fn contains(&self, key: RangeInclusive<K>) -> bool {
        self.get(key).is_some()
    }

    /// This function will return the key/value pair with the smallest key in the tree, or None if the
    /// tree is empty.
    /// # Examples
    /// ```
    /// extern crate interval_tree;
    ///
    /// let mut t=interval_tree::IntervalTree::<u64, u64>::new();
    /// t.insert(interval_tree::range(2,2),25);
    /// t.insert(interval_tree::range(3,3),50);
    /// assert_eq!(t.min().unwrap().0, &interval_tree::range(2,2));
    /// assert_eq!(t.min().unwrap().1, &[25]);
    ///
    /// ```
    pub fn min(&self) -> Option<(&'_ RangeInclusive<K>, &'_ [V])> {
        self.root.as_ref().map(|n| n.min_pair())
    }

    /// This function will return the key/value pair with the biggest key in the tree, or None if the
    /// tree is empty.
    /// # Examples
    /// ```
    /// extern crate interval_tree;
    ///
    /// let mut t=interval_tree::IntervalTree::<u64, i32>::new();
    /// t.insert(interval_tree::range(2,2),25);
    /// t.insert(interval_tree::range(3,3),50);
    /// assert_eq!(t.max().unwrap().0, &interval_tree::range(3,3));
    /// assert_eq!(t.max().unwrap().1, &[50]);
    ///
    /// ```
    pub fn max(&self) -> Option<(&'_ RangeInclusive<K>, &'_ [V])> {
        self.root.as_ref().map(|n| n.max_pair())
    }

    /// Merge all (key, value) pairs from another tree into this one, consuming it.
    pub fn merge(&mut self, other: IntervalTree<K, V>) {
        for (k, v) in other.into_iter() {
            self.insert(k, v);
        }
    }
}

impl<K: Ord + Clone, V> IntoIterator for IntervalTree<K, V> {
    type Item = <IntoIter<K, V> as Iterator>::Item;

    type IntoIter = IntoIter<K, V>;

    /// Return an iterator for all (key,value) pairs in the tree, consuming the tree in the process.
    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter::new(self)
    }
}

impl<K: Ord, V> IntervalTree<K, V> {
    /// This function will return a read only iterator for all (key,value) pairs between the two
    /// bounds.
    /// # Examples
    /// ```
    /// //[...]
    /// # let mut t=interval_tree::IntervalTree::<u64, i32>::new();
    /// for (key,val) in t.range(interval_tree::range(9, 100)) {
    ///     println!("{:?} -> {}",key,val)
    /// }
    ///
    /// ```
    pub fn range(&self, range: RangeInclusive<K>) -> RangePairIter<'_, K, V> {
        RangePairIter::new(
            self,
            Bound::Included(range.start),
            Bound::Included(range.end),
        )
    }

    /// This function will return a read only iterator for all (key,value) pairs in the tree.
    /// # Examples
    /// ```
    /// # let mut t=interval_tree::IntervalTree::<u64, i32>::new();
    /// for (key,val) in t.iter() {
    ///     println!("{:?} -> {}",key,val)
    /// }
    ///
    /// ```
    pub fn iter(&self) -> RangePairIter<'_, K, V> {
        RangePairIter::new(self, Bound::Unbounded, Bound::Unbounded)
    }
}

#[cfg(test)]
mod tests {
    extern crate rand;
    use crate::node::tests::is_interval_tree;
    use crate::range::RangeInclusive;

    type IntervalTree<V> = super::IntervalTree<u64, V>;

    fn random_range() -> RangeInclusive<u64> {
        let offset = rand::random::<u64>() % 50;
        let len: u64 = rand::random::<u64>() % 50;
        crate::range(offset, offset + len)
    }

    #[test]
    fn test_fuzz() {
        let mut t = IntervalTree::<i32>::new();
        for _ in 1..5000 {
            let decision = rand::random::<bool>();
            let range = random_range();
            if decision {
                t.insert(range, 1337);
                assert!(t.contains(range));
                assert!(is_interval_tree(&t.root));
            } else {
                t.remove(range);
                assert!(!t.contains(range));
                assert!(is_interval_tree(&t.root));
            };
        }
    }
}
