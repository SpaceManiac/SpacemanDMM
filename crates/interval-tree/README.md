IntervalTree
============

**Based on [theban_interval_tree](https://github.com/theban/interval-tree), used under the GPL.**

A simple crate that implements a interval tree datastructure. An `IntervalTree` maps ranges of `u64` to any value. We can than use the tree to perform querys such as "what key/value pairs are intersecting the range (x,y)?" does "does the tree contain the range (X,Y)?". Insertion, deletion and lookup are in O(log(n)). Iterating over all m solutions to a query is in O(m*log(n)).

```rust
extern crate interval_tree;
extern crate rand;
extern crate time;

use interval_tree::{IntervalTree, range};

fn main(){
    let data = 4221;
    let mut t = IntervalTree::<u64, i32>::new();

    assert!(t.empty());
    assert!(t.min().is_none());

    t.insert(range(1,1), data);
    t.insert(range(2,2), data+1);
    t.insert(range(3,3), data+2);

    assert_eq!(t.min().expect("get min"),(&range(1,1),&data));

    assert!(!t.empty());
    assert!(t.get_or(range(1,1), &0) == &data);
    assert!(!t.contains(range(0,0)));

    t.delete(range(1,1));

    assert!(!t.contains(range(1,1)));

    for (i,pair) in t.iter().enumerate() {
        //[...]
    }

    for (i,pair) in t.range(34, 36).enumerate() {
        //[...]
    }
}
```
