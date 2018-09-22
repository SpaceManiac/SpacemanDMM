//! Representation of a map as a collection of atoms rather than a grid.

use std::rc::{Rc, Weak};
use weak_table::WeakKeyHashMap;

use dmm_tools::dmm::{Map, Prefab};
use dm::objtree::ObjectTree;

use dmi::IconCache;
use map_renderer::{RenderPop, Vertex};

#[derive(Debug, Clone)]
pub struct AtomMap {
    pub size: (u32, u32),
    pub pops: WeakKeyHashMap<Weak<Prefab>, RenderPop>,
    pub levels: Vec<AtomZ>,
}

#[derive(Debug, Default, Clone)]
pub struct AtomZ {
    // no sorting invariants, could be in any order whatsoever
    pub instances: DualPool<Instance, [Vertex; 4]>,
    // plane+layer sorting is maintained
    pub sorted_order: Vec<usize>,
    index_buffer: Vec<[u32; 6]>,
    index_buffer_dirty: bool,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Instance {
    pub x: u32,
    pub y: u32,
    pub pop: Rc<Prefab>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct InstanceId {
    z: u32,
    idx: usize,
}

#[derive(Debug)]
pub struct Defer<'a> {
    map: &'a mut AtomMap,
    z: u32,
}

impl AtomMap {
    pub fn new(map: &Map, icons: &IconCache, objtree: &ObjectTree) -> AtomMap {
        let (dim_x, dim_y, dim_z) = map.dim_xyz();
        let mut atom_map = AtomMap {
            size: (dim_x as u32, dim_y as u32),
            pops: Default::default(),
            levels: Default::default(),
        };

        for z in 0..dim_z {
            atom_map.levels.push(AtomZ::default());
            atom_map.defer_sort(z as u32, |defer| {
                for ((y, x), key) in map.z_level(z).indexed_iter() {
                    for fab in map.dictionary[key].iter() {
                        let pop = defer.add_pop(fab, icons, objtree);
                        defer.add_instance((x as u32, y as u32), pop);
                    }
                }
            });
        }
        atom_map
    }

    pub fn save(&self, merge_base: Option<&Map>) -> Map {
        unimplemented!()
    }

    pub fn dim_xyz(&self) -> (u32, u32, u32) {
        (self.size.0, self.size.1, self.levels.len() as u32)
    }

    pub fn refresh_pops(&mut self, icons: &IconCache, objtree: &ObjectTree) {
        for (prefab, rpop) in self.pops.iter_mut() {
            *rpop = RenderPop::from_prefab(icons, objtree, &prefab).unwrap_or_default();
        }
    }

    pub fn add_pop(&mut self, prefab: &Prefab, icons: &IconCache, objtree: &ObjectTree) -> Rc<Prefab> {
        if let Some(key) = self.pops.get_key(&prefab) {
            key
        } else {
            let rc = Rc::new(prefab.to_owned());
            self.pops.insert(rc.clone(), RenderPop::from_prefab(icons, objtree, &prefab).unwrap_or_default());
            rc
        }
    }

    pub fn defer_sort<F: FnOnce(&mut Defer)>(&mut self, z: u32, f: F) {
        f(&mut Defer { map: self, z });
        self.sort_again(z);
    }

    fn add_instance_unsorted(&mut self, (x, y, z): (u32, u32, u32), prefab: Rc<Prefab>) -> InstanceId {
        let level = &mut self.levels[z as usize];
        let new_instance = level.prep_instance(&mut self.pops, (x, y), prefab);
        level.sorted_order.push(new_instance);
        level.index_buffer.push(indices(new_instance));
        InstanceId { z, idx: new_instance }
    }

    pub fn add_instance(&mut self, (x, y, z): (u32, u32, u32), prefab: Rc<Prefab>) -> InstanceId {
        let pops = &mut self.pops;
        let level = &mut self.levels[z as usize];
        let new_instance = level.prep_instance(pops, (x, y), prefab);
        let sorted_order = &mut level.sorted_order;
        let instances = &mut level.instances;

        let sort_key = |&idx| {
            let inst = instances.get_key(idx);
            let rpop = pops.get(&inst.pop).expect("instance with missing pop");
            rpop.sort_key()
        };
        let pos = match sorted_order.binary_search_by_key(&sort_key(&new_instance), sort_key) {
            Ok(found) => found,  // TODO: add 1? add more than 1?
            Err(dest) => dest,
        };
        sorted_order.insert(pos, new_instance);
        level.index_buffer.insert(pos, indices(new_instance));
        InstanceId { z, idx: new_instance }
    }

    pub fn get_instance(&self, id: InstanceId) -> Option<&Instance> {
        let level = &self.levels[id.z as usize];
        if id.idx >= level.instances.len() || level.instances.freelist.contains(&id.idx) {
            // TODO: the contains() call is probably not fast
            None
        } else {
            Some(level.instances.get_key(id.idx))
        }
    }

    pub fn sort_again(&mut self, z: u32) {
        let pops = &self.pops;
        let level = &mut self.levels[z as usize];
        let sorted_order = &mut level.sorted_order;
        let instances = &mut level.instances;

        sorted_order.sort_by_key(|&idx| {
            let inst = instances.get_key(idx);
            let rpop = pops.get(&inst.pop).expect("instance with missing pop");
            rpop.sort_key()
        });
        level.index_buffer_dirty = true;
    }

    #[inline]
    pub fn vertex_buffer(&self, z: u32) -> &[[Vertex; 4]] {
        self.levels[z as usize].instances.values()
    }

    pub fn index_buffer(&mut self, z: u32) -> &[[u32; 6]] {
        let level = &mut self.levels[z as usize];
        if level.index_buffer_dirty {
            level.index_buffer_dirty = false;
            level.index_buffer.clear();
            for &inst in level.sorted_order.iter() {
                level.index_buffer.push(indices(inst));
            }
        }
        &level.index_buffer[..]
    }
}

impl AtomZ {
    fn prep_instance(&mut self, pops: &mut WeakKeyHashMap<Weak<Prefab>, RenderPop>, (x, y): (u32, u32), prefab: Rc<Prefab>) -> usize {
        let vertices = pops.get(&prefab)
            .map_or_else(|| [Vertex::default(); 4], |rpop| rpop.instance((x, y)));
        self.instances.push(Instance { x, y, pop: prefab }, vertices)
    }
}

impl<'a> Defer<'a> {
    #[inline]
    pub fn add_pop(&mut self, prefab: &Prefab, icons: &IconCache, objtree: &ObjectTree) -> Rc<Prefab> {
        self.map.add_pop(prefab, icons, objtree)
    }

    pub fn add_instance(&mut self, (x, y): (u32, u32), prefab: Rc<Prefab>) -> InstanceId {
        self.map.add_instance_unsorted((x, y, self.z), prefab)
    }
}

/// 2-tuple struct of arrays storage with freelist.
#[derive(Debug, Clone)]
pub struct DualPool<K, V> {
    keys: Vec<K>,
    vals: Vec<V>,
    freelist: Vec<usize>,
}

impl<K, V> Default for DualPool<K, V> {
    fn default() -> Self {
        DualPool {
            keys: Default::default(),
            vals: Default::default(),
            freelist: Default::default(),
        }
    }
}

impl<K, V> DualPool<K, V> {
    /// Get a slice of all values in the pool, including dead values.
    pub fn values(&self) -> &[V] {
        &self.vals[..]
    }

    pub fn push(&mut self, key: K, value: V) -> usize {
        if let Some(idx) = self.freelist.pop() {
            // we have free slots, stick it in the middle
            self.keys[idx] = key;
            self.vals[idx] = value;
            idx
        } else {
            // we do not have free slots, push it on the end
            let idx = self.keys.len();
            debug_assert!(idx == self.vals.len());
            self.keys.push(key);
            self.vals.push(value);
            idx
        }
    }

    pub fn free(&mut self, idx: usize) {
        debug_assert!(!self.freelist.contains(&idx));
        self.freelist.push(idx);
    }

    pub fn get_key(&self, idx: usize) -> &K {
        &self.keys[idx]
    }

    pub fn len(&self) -> usize {
        self.keys.len()
    }
}

fn indices(start: usize) -> [u32; 6] {
    let start = start as u32;
    [start, start + 1, start + 3, start + 1, start + 2, start + 3]
}
