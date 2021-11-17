use std::cmp;
use std::collections::BTreeSet;
use crate::RangeInclusive;

type IntervalTree<V> = crate::IntervalTree<u64, V>;

#[test]
fn test_getters(){
    let data = 1337;
    let mut t = IntervalTree::<i32>::new();
    t.insert(RangeInclusive::new(1,1), data);
    t.insert(RangeInclusive::new(2,2), data+1);
    t.insert(RangeInclusive::new(3,3), data+2);
    assert_eq!(t.get_or(RangeInclusive::new(1,1), &[0]), &[data]);
    assert_eq!(t.get_or(RangeInclusive::new(2,2), &[0]), &[data+1]);
    assert_eq!(t.get_or(RangeInclusive::new(3,3), &[0]), &[data+2]);
    assert_eq!(t.get_or(RangeInclusive::new(4,4), &[0]), &[0]);
    assert_eq!(t.get(RangeInclusive::new(4,4)), None);
}

#[test]
fn test_contains(){
    let data = 1337;
    let mut t = IntervalTree::<i32>::new();
    t.insert(RangeInclusive::new(1,1), data);
    t.insert(RangeInclusive::new(2,2), data+1);
    t.insert(RangeInclusive::new(3,3), data+2);
    assert!(!t.contains(RangeInclusive::new(0,0)));
    assert!( t.contains(RangeInclusive::new(1,1)));
    assert!( t.contains(RangeInclusive::new(2,2)));
    assert!( t.contains(RangeInclusive::new(3,3)));
    assert!(!t.contains(RangeInclusive::new(4,4)));
}

#[test]
fn test_empty(){
    let data = 1337;
    let mut t = IntervalTree::<i32>::new();
    assert!(t.is_empty());
    t.insert(RangeInclusive::new(1,1), data);
    t.insert(RangeInclusive::new(2,2), data+1);
    t.insert(RangeInclusive::new(3,3), data+2);
    assert!(!t.is_empty());
}

#[test]
fn test_remove(){
    let data = 1337;
    let mut t = IntervalTree::<i32>::new();
    t.insert(RangeInclusive::new(1,1), data);
    t.insert(RangeInclusive::new(2,2), data+1);
    t.insert(RangeInclusive::new(3,3), data+2);
    t.remove(RangeInclusive::new(1,1));
    assert!(!t.contains(RangeInclusive::new(1,1)));
    assert!( t.contains(RangeInclusive::new(2,2)));
    assert!( t.contains(RangeInclusive::new(3,3)));
    t.remove(RangeInclusive::new(2,2));
    assert!(!t.contains(RangeInclusive::new(1,1)));
    assert!(!t.contains(RangeInclusive::new(2,2)));
    assert!( t.contains(RangeInclusive::new(3,3)));
    t.remove(RangeInclusive::new(3,3));
    assert!(!t.contains(RangeInclusive::new(1,1)));
    assert!(!t.contains(RangeInclusive::new(2,2)));
    assert!(!t.contains(RangeInclusive::new(3,3)));
    assert!(t.is_empty());
}

#[test]
fn test_min(){
    let mut t = IntervalTree::<i32>::new();
    assert!{t.min().is_none()};
    t.insert(RangeInclusive::new(50,50), 1337);
    assert_eq!{t.min().expect("get 1 min"),(&RangeInclusive::new(50,50),&[1337][..])};
    t.insert(RangeInclusive::new(49,49),1338);
    assert_eq!{t.min().expect("get 2 min"),(&RangeInclusive::new(49,49),&[1338][..])};
    t.insert(RangeInclusive::new(47,47),1339);
    assert_eq!{t.min().expect("get 3 min"),(&RangeInclusive::new(47,47),&[1339][..])};
    t.insert(RangeInclusive::new(48,48),1340);
    assert_eq!{t.min().expect("get 4 min"),(&RangeInclusive::new(47,47),&[1339][..])};
}

#[test]
fn test_iter(){
    let mut t = IntervalTree::<i32>::new();
    t.insert(RangeInclusive::new(32,32),1337);
    t.insert(RangeInclusive::new(34,34),1338);
    t.insert(RangeInclusive::new(36,36),1339);
    t.insert(RangeInclusive::new(38,38),1340);
    for (i,pair) in t.iter().enumerate() {
        let (k,v) = pair;
        println!("{:?}, {}",k,v);
        let key = (i as u64)*2 +32;
        assert_eq!(k,RangeInclusive::new(key,key));
        assert_eq!(v,&((i as i32)+1337));
    }

}

#[test]
fn test_range_iter(){
    let mut t = IntervalTree::<i32>::new();
    t.insert(RangeInclusive::new(32,32),1337);
    t.insert(RangeInclusive::new(34,34),1338);
    t.insert(RangeInclusive::new(36,36),1339);
    t.insert(RangeInclusive::new(38,38),1340);
    for (i,pair) in t.range(RangeInclusive::new(34, 36)).enumerate() {
        let (k,v) = pair;
        println!("{:?}, {}",k,v);
        let key = (i as u64)*2 +34;
        assert_eq!(k,RangeInclusive::new(key,key));
        assert_eq!(v,&((i as i32)+1338));
        assert!(i<2);
    }

}

#[test]
fn test_range_iter_non_pointwise(){
    let mut t = IntervalTree::<i32>::new();
    t.insert(RangeInclusive::new(3,8),1337);
    t.insert(RangeInclusive::new(6,10),1338);
    t.insert(RangeInclusive::new(12,36),1339);
    t.insert(RangeInclusive::new(32,40),1340);
    assert_eq!(t.range(RangeInclusive::new(9,14)).map(|(k,_)| k.start).collect::<Vec<u64>>(), vec![6,12])
}

fn random_range() -> RangeInclusive<u64> {
    let offset = rand::random::<u64>();
    let len: u64;
    if rand::random::<bool>() {
        len = cmp::min(rand::random::<u64>()%500, 0xff_ff_ff_ff_ff_ff_ff_ff - offset)
    } else {
        len = rand::random::<u64>()%(0xff_ff_ff_ff_ff_ff_ff_ff - offset)
    }

    return RangeInclusive::new(offset, offset+len)
}

#[test]
fn test_range_iter_nontrivial(){
    let mut set = BTreeSet::<RangeInclusive<u64>>::new();
    let mut t = IntervalTree::<i32>::new();
    for _ in 1..5000 {
        let decision = rand::random::<bool>();
        let range = random_range();
        if  decision {
            set.insert(range);
            t.insert(range, 1337);
            assert!(t.contains(range));
            //assert!(t.test_theban_interval_tree());
        } else {
            set.remove(&range);
            t.remove(range);
            assert!(!t.contains(range));
            //assert!(t.test_theban_interval_tree());
        };
    let query = random_range();
    let should = set.iter().filter(|r| crate::range::intersect(&query, r)).map(|r| r.clone()).collect::<Vec<RangeInclusive<u64>>>();
    let is = t.range(query).map(|(r,_)| r).collect::<Vec<RangeInclusive<u64>>>();
    assert_eq!(should, is);
    };
}
