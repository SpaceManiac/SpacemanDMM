use std::cmp::{self, Ordering};
use crate::range::RangeInclusive;

#[derive(Debug, Clone)]
pub struct Node<K, V> {
    pub key: RangeInclusive<K>,
    pub data: Vec<V>,
    height: u32,
    pub max: K,
    pub left: Option<Box<Node<K, V>>>,
    pub right: Option<Box<Node<K, V>>>,
}

impl<K: Ord + Clone, V> Node<K, V> {
    pub fn new(key: RangeInclusive<K>, data: V) -> Self {
        Node {
            max: key.end.clone(),
            key,
            data: vec![data],
            height: 1,
            left: None,
            right: None,
        }
    }

    fn diff_of_successors_height(&self) -> i32 {
        let l = height(&self.left);
        let r = height(&self.right);
        (l as i32) - (r as i32)
    }

    /// update the cached height of root. To call this function make sure that the cached values of
    /// both children of root ar up to date.
    fn update_height(&mut self) {
        self.height = cmp::max(height(&self.left), height(&self.right)) + 1;
        self.max.clone_from(&self.key.end);
        if let Some(ref left) = self.left {
            if left.max > self.max {
                self.max.clone_from(&left.max);
            }
        }
        if let Some(ref right) = self.right {
            if right.max > self.max {
                self.max.clone_from(&right.max);
            }
        }
    }

    ///returns the minimal key,value pair within this tree
    pub fn min_pair(&self) -> (&RangeInclusive<K>, &[V]) {
        self.left.as_ref().map_or((&self.key, &self.data), |n| n.min_pair())
    }

    ///returns the maximal key,value pair within this tree
    pub fn max_pair(&self) -> (&RangeInclusive<K>, &[V]) {
        self.right.as_ref().map_or((&self.key, &self.data), |n| n.max_pair())
    }

    /// Perform a single right rotation on this (sub) tree
    fn rotate_right(mut self: Box<Self>) -> Box<Self> {
        let mut new_root_box = self.left.take().expect("Avl broken");
        self.left = new_root_box.right.take();
        self.update_height();
        new_root_box.right = Some(self);
        new_root_box.update_height();
        new_root_box
    }

    /// Perform a single left rotation on this (sub) tree
    fn rotate_left(mut self: Box<Self>) -> Box<Self> {
        let mut new_root_box = self.right.take().expect("Avl broken");
        self.right = new_root_box.left.take();
        self.update_height();
        new_root_box.left = Some(self);
        new_root_box.update_height();
        new_root_box
    }

    /// Performs a rotation that counteracts the fact that the left successor is too high
    fn rotate_left_successor(mut self: Box<Self>) -> Box<Self> {
        let left = self.left.take().expect("Interval broken");
        if height(&left.left) < height(&left.right) {
            let rotated = left.rotate_left();
            self.left = Some(rotated);
            self.update_height();
        } else {
            self.left = Some(left);
        }
        self.rotate_right()
    }

    /// Performs a rotation that counteracts the fact that the right successor is too high
    fn rotate_right_successor(mut self: Box<Self>) -> Box<Self> {
        let right = self.right.take().expect("Interval broken");
        if height(&right.left) > height(&right.right) {
            let rotated = right.rotate_right();
            self.right = Some(rotated);
            self.update_height();
        } else {
            self.right = Some(right)
        }
        self.rotate_left()
    }

    /// Apply all necessary rotations on root.
    fn rotate_if_necessary(self: Box<Self>) -> Box<Self> {
        match self.diff_of_successors_height() {
            2 => self.rotate_left_successor(),
            1 | 0 | -1 => self,
            -2 => self.rotate_right_successor(),
            _ => unreachable!(),
        }
    }

    //will update_heights and rotate the node if necessary, returns the rotated node
    fn updated_node(mut self: Box<Self>) -> Box<Self> {
        self.update_height();
        self.rotate_if_necessary()
    }

    //Return a new Interval tree, where the root has been removed
    fn delete_root(mut self) -> Option<Box<Self>> {
        match (self.left.take(), self.right.take()) {
            (None,    None)    => None,
            (Some(l), None)    => Some(l),
            (None,    Some(r)) => Some(r),
            (Some(l), Some(r)) => Some(Node::combine_two_subtrees(l, r))
        }
    }

    //Performs recursive `drop_and_get_min` if a left  since a successor is available
    fn drop_min_from_left(mut root: Box<Self>, left: Box<Self>) -> (Option<Box<Self>>, Box<Self>) {
        let (new_left, min) = left.drop_min();
        root.left = new_left;
        (Some(root.updated_node()), min)
    }

    //Finds the minimal value below root and returns a new (optional) tree where the minimal value has been
    //removed and the (optional) minimal node as tuple (new_tree, min);
    fn drop_min(mut self: Box<Self>) -> (Option<Box<Self>>, Box<Self>) {
        match self.left.take() {
            Some(left) => Node::drop_min_from_left(self, left),
            None => (self.right.take(), self),
        }
    }

    //Return a new Interval tree, as the combination of two subtrees with max(l) <= min(r)
    fn combine_two_subtrees(l: Box<Self>, r: Box<Self>) -> Box<Self> {
        let (remaining_tree, min) = r.drop_min();
        let mut new_root = min;
        new_root.left = Some(l);
        new_root.right = remaining_tree;
        new_root.updated_node()
    }

    // will delete `key` from the tree `root`. Returns either `Some` tree or if the resilting tree is
    // empty: None.
    //
    //
    pub fn delete(mut self: Box<Self>, key: RangeInclusive<K>) -> Option<Box<Self>> {
        match self.key.cmp(&key) {
            Ordering::Equal => return self.delete_root(),
            Ordering::Less => {
                if let Some(succ) = self.right.take() {
                    self.right = succ.delete(key);
                    return Some(self.updated_node());
                }
            },
            Ordering::Greater => {
                if let Some(succ) = self.left.take() {
                    self.left = succ.delete(key);
                    return Some(self.updated_node());
                }
            }
        }
        Some(self)
    }

    /// returns a read only reference to the data stored under key in the tree given by root
    pub fn search(&self, key: &RangeInclusive<K>) -> Option<&[V]> {
        self.search_pair(key).map(|(_, v)| v)
    }

    /// returns a read only reference paie to the data stored under key in the tree given by root
    pub fn search_pair(&self, key: &RangeInclusive<K>) -> Option<(&RangeInclusive<K>, &[V])> {
        match self.key.cmp(key) {
            Ordering::Equal => Some((&self.key, &self.data)),
            Ordering::Less => self.right.as_ref().and_then(|succ| succ.search_pair(key)),
            Ordering::Greater => self.left.as_ref().and_then(|succ| succ.search_pair(key)),
        }
    }

    /// Inserts the given data under the key in the tree root. It will replace old data stored
    /// under this key if it was allready used in the tree. The resulting tree will be returned (its
    /// root may now differ due to rotations, thus the old root is moved into the function)
    pub fn insert(mut self: Box<Self>, key: RangeInclusive<K>, data: V) -> Box<Self> {
        match self.key.cmp(&key) {
            Ordering::Equal => { self.data.push(data); return self },
            Ordering::Less =>    self.right = Node::insert_in_successor(self.right.take(), key, data),
            Ordering::Greater => self.left  = Node::insert_in_successor(self.left.take(),  key, data)
        }
        self.update_height();
        self.rotate_if_necessary()
    }

    /// recursively insert the (key,data) pair into the given optional succesor and return its new
    /// value
    fn insert_in_successor(succ: Option<Box<Self>>, key: RangeInclusive<K>, data: V) -> Option<Box<Self>> {
        Some(match succ {
            Some(succ) => succ.insert(key, data),
            None => Box::new(Node::new(key, data)),
        })
    }
}

pub fn height<K, V>(node: &Option<Box<Node<K, V>>>) -> u32 {
    node.as_ref().map_or(0, |succ| succ.height)
}

#[cfg(test)]
pub mod tests {
use super::*;
type Node<V> = super::Node<u64, V>;

/// returns true iff key is stored in the tree given by root
pub fn contains<V>(key: &RangeInclusive<u64>, root: &Box<Node<V>>) -> bool {
    root.search(key).is_some()
}

fn subtree_max<V>(node: &Option<Box<Node<V>>>) -> u64 {
    node.as_ref().map_or(0, |succ| succ.max)
}

///returns the smallest key and value after the given key.
pub fn min_after<'a,V>(key: &RangeInclusive<u64>, root: &'a Box<Node<V>>) -> Option<(&'a RangeInclusive<u64>,&'a [V])> {
    match root.key.cmp(key){
        Ordering::Equal =>  root.right.as_ref().map_or(None, |succ| Some(succ.min_pair())),
        Ordering::Less =>   root.right.as_ref().map_or(None, |succ| min_after(key, succ)),
        Ordering::Greater => {
            match root.left {
                Some(ref succ) => min_after(key, &succ).or( Some((&root.key,&root.data)) ),
                None => Some((&root.key, &root.data))
            }
        }
    }
}

///returns the minimal value within this tree
pub fn min<V>(root: &Box<Node<V>>) -> &[V] {
    root.left.as_ref().map_or(&root.data, min)
}

///returns the minimal value within this tree
pub fn max<V>(root: &Box<Node<V>>) -> &[V] {
    root.right.as_ref().map_or(&root.data, max)
}


fn simple_tree(size: i32) -> Box<Node<i32>> {
    let mut t = Box::new(Node::<i32>{key: RangeInclusive::new(1,1), data: vec![1337], height: 0, max: 1, left:None, right: None});
    for x in 2..size+1 {
        t = t.insert(RangeInclusive::new(x as u64, x as u64 ),1337+x-1)
    }
    t
}
fn is_sorted_left<V>(node: &Box<Node<V>>) -> bool {
    node.left.as_ref().map_or(true, |succ| succ.key < node.key)
}
fn is_sorted_right<V>(node: &Box<Node<V>>) -> bool {
    node.right.as_ref().map_or(true, |succ| succ.key > node.key)
}
fn is_interval_node<V>(node: &Box<Node<V>>) -> bool {
    let sorted = is_sorted_left(node) && is_sorted_right(node);
    let balanced = node.height == cmp::max(height(&node.left),height(&node.right))+1;
    let proper_max = node.max == cmp::max(subtree_max(&node.left), cmp::max(subtree_max(&node.right), node.key.end));
    return sorted && balanced && proper_max;
}

pub fn is_interval_tree<V>(root: &Option<Box<Node<V>>>) -> bool {
    (*root).as_ref().map_or(true, is_interval_node)
}

#[test]
fn simple_tree_operations() {
    let mut t = Box::new(Node::<i32>{key: RangeInclusive::new(3,3), data: vec![4], max:3, height: 2,
        left: Some(Box::new(Node::<i32>{key: RangeInclusive::new(2,2), data: vec![5], height:1, max: 2, left: None, right: None})),
        right: None});
    assert!(is_interval_node(&t));
    assert!( contains::<i32>(&RangeInclusive::new(3,3),&t) );
    assert!( contains::<i32>(&RangeInclusive::new(2,2),&t) );
    assert!( !contains::<i32>(&RangeInclusive::new(6,6),&t) );
    assert!( !contains::<i32>(&RangeInclusive::new(4,4),&t) );
    t = t.insert(RangeInclusive::new(4,4),7);
    t = t.insert(RangeInclusive::new(5,5),7);
    t = t.insert(RangeInclusive::new(6,6),8);
    assert!(  contains::<i32>(&RangeInclusive::new(4,4),&t) );
    assert!(  contains::<i32>(&RangeInclusive::new(6,6),&t) );
    assert!( !contains::<i32>(&RangeInclusive::new(7,7),&t) );
}

#[test]
fn rotations_on_tree(){
    let mut t = Box::new(Node::<i32>{key: RangeInclusive::new(1,1), data: vec![1337], height: 1, max: 1, left: None, right: None});
    for i in 2..255 {
        t = t.insert(RangeInclusive::new(i,i),1337);
        assert!(is_interval_node(&t));
    }
    //check that the tree is indeed balanced
    assert!(height(&Some(t)) <= 8);
}

#[test]
fn test_drop_min(){
    let mut t = simple_tree(3);
    let (maybe_tree,min) = t.drop_min();
    t = maybe_tree.expect("failure to get tree for first min delete");
    assert!(is_interval_node(&t));
    assert!( min.key == RangeInclusive::new(1,1));
    assert!(!contains::<i32>(&RangeInclusive::new(1,1),&t));
    assert!( contains::<i32>(&RangeInclusive::new(2,2),&t));
    assert!( contains::<i32>(&RangeInclusive::new(3,3),&t));

    let (maybe_tree,min) = t.drop_min();
    t = maybe_tree.expect("failure to get tree for second min delete");
    assert!(is_interval_node(&t));
    assert!( min.key == RangeInclusive::new(2,2));
    assert!(!contains::<i32>(&RangeInclusive::new(1,1),&t));
    assert!(!contains::<i32>(&RangeInclusive::new(2,2),&t));
    assert!( contains::<i32>(&RangeInclusive::new(3,3),&t));

    let (maybe_tree,min) = t.drop_min();
    assert!( maybe_tree.is_none() );
    assert!( min.key == RangeInclusive::new(3,3));
}

#[test]
fn test_drop_root(){
    let mut t = simple_tree(3);
    let maybe_tree = t.delete_root();
    t = maybe_tree.expect("failure to get tree for first root drop");
    assert!(is_interval_node(&t));
    assert!( t.height == 2);
    assert!( contains::<i32>(&RangeInclusive::new(1,1),&t));
    assert!(!contains::<i32>(&RangeInclusive::new(2,2),&t));
    assert!( contains::<i32>(&RangeInclusive::new(3,3),&t));

    let maybe_tree = t.delete_root();
    t = maybe_tree.expect("failure to get tree for second root drop");
    assert!(is_interval_node(&t));
    assert!( contains::<i32>(&RangeInclusive::new(1,1),&t));
    assert!(!contains::<i32>(&RangeInclusive::new(2,2),&t));
    assert!(!contains::<i32>(&RangeInclusive::new(3,3),&t));

    let maybe_tree = t.delete_root();
    assert!( maybe_tree.is_none() );
}

#[test]
fn test_delete(){
    let mut t = simple_tree(10);
    for i in 1..10 {
        assert!(contains::<i32>(&RangeInclusive::new(i,i),&t));
        let maybe_tree = t.delete(RangeInclusive::new(i,i));
        t = maybe_tree.expect("failure to get tree for delete");
        assert!(!contains::<i32>(&RangeInclusive::new(i,i),&t));
        assert!(is_interval_node(&t));
    }
    assert!(contains::<i32>(&RangeInclusive::new(10,10),&t));
    let maybe_tree = t.delete(RangeInclusive::new(10,10));
    assert!(maybe_tree.is_none());
}

#[test]
fn test_min_max() {
    let t = simple_tree(50);
    assert_eq!(min(&t),&[1337]);
    assert_eq!(max(&t),&[1337+50-1]);
    assert_eq!(t.max_pair().0,&RangeInclusive::new(50,50));
    assert_eq!(t.min_pair().0,&RangeInclusive::new(1,1));
}

#[test]
fn test_min_after(){
    let t = simple_tree(50);
    for old_key in 0..55 {
        println!("trying value: {}", old_key);
        match min_after(&RangeInclusive::new(old_key,old_key),&t) {
            Some((k,_d)) => assert_eq!(k, &(RangeInclusive::new(old_key+1,old_key+1))),
            None => assert!(old_key >= 50)
        }
    }
}
}
