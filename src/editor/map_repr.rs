//! Representation of a map as a collection of atoms rather than a grid.
#![allow(dead_code)]  // WIP

use std::sync::{Arc, Weak};
use std::cell::{Cell, RefCell, Ref};
use weak_table::WeakKeyHashMap;

use dmm_tools::dmm::{Map, Prefab};
use dm::objtree::ObjectTree;

use dmi::IconCache;
use map_renderer::{RenderPop, Vertex, DrawCall};

#[derive(Debug, Clone)]
pub struct AtomMap {
    pub size: (u32, u32),
    pub pops: WeakKeyHashMap<Weak<Prefab>, RenderPop>,
    pub levels: Vec<AtomZ>,
    pub duration: ::std::time::Duration,
}

#[derive(Debug, Default, Clone)]
pub struct AtomZ {
    // no sorting invariants, could be in any order whatsoever
    pub instances: DualPool<Instance, [Vertex; 4]>,

    // plane+layer sorting is maintained
    pub sorted_order: Vec<usize>,
    index_buffer: RefCell<Vec<[u32; 6]>>,
    index_buffer_dirty: Cell<bool>,

    // matches sorted_order with combined calls
    pub draw_calls: Vec<DrawCall>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Instance {
    pub x: u32,
    pub y: u32,
    pub pop: Arc<Prefab>,
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
        let start = ::std::time::Instant::now();
        let (dim_x, dim_y, dim_z) = map.dim_xyz();
        let mut atom_map = AtomMap {
            size: (dim_x as u32, dim_y as u32),
            pops: Default::default(),
            levels: Default::default(),
            duration: Default::default(),
        };

        for z in 0..dim_z {
            atom_map.levels.push(AtomZ::default());
            atom_map.defer_sort(z as u32, |defer| {
                for ((y, x), key) in map.z_level(z).indexed_iter() {
                    for fab in map.dictionary[key].iter() {
                        let pop = defer.add_pop(fab, icons, objtree);
                        defer.add_instance((x as u32, (dim_y - 1 - y) as u32), pop);
                    }
                }
            });
        }
        atom_map.duration = ::std::time::Instant::now() - start;
        atom_map
    }

    pub fn save(&self, _merge_base: Option<&Map>) -> Map {
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

    pub fn add_pop(&mut self, prefab: &Prefab, icons: &IconCache, objtree: &ObjectTree) -> Arc<Prefab> {
        if let Some(key) = self.pops.get_key(&prefab) {
            key
        } else {
            let rc = Arc::new(prefab.to_owned());
            self.pops.insert(rc.clone(), RenderPop::from_prefab(icons, objtree, &prefab).unwrap_or_default());
            rc
        }
    }

    pub fn defer_sort<F: FnOnce(&mut Defer)>(&mut self, z: u32, f: F) {
        f(&mut Defer { map: self, z });
        self.sort_again(z);
    }

    fn add_instance_unsorted(&mut self, (x, y, z): (u32, u32, u32), prefab: Arc<Prefab>) -> InstanceId {
        let level = &mut self.levels[z as usize];
        let new_instance = level.prep_instance(&mut self.pops, (x, y), prefab);
        level.sorted_order.push(new_instance);
        level.index_buffer.get_mut().push(indices(new_instance));
        InstanceId { z, idx: new_instance }
    }

    pub fn add_instance(&mut self, (x, y, z): (u32, u32, u32), prefab: Arc<Prefab>) -> InstanceId {
        let pops = &mut self.pops;
        let level = &mut self.levels[z as usize];
        let new_instance = level.prep_instance(pops, (x, y), prefab);
        let sorted_order = &mut level.sorted_order;
        let instances = &mut level.instances;
        let draw_calls = &mut level.draw_calls;

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
        level.index_buffer.get_mut().insert(pos, indices(new_instance));

        // find the draw call which "should" contain the new index
        let pos = 6 * (pos as u32);
        let mut start = 0;
        let mut draw_call = draw_calls.len();
        for (i, call) in draw_calls.iter_mut().enumerate() {
            if pos >= start && pos < start + call.len {
                draw_call = i;
                break;
            }
            start += call.len;
        }

        // extend the draw call or insert a new one
        let rpop = pops.get(&instances.get_key(new_instance).pop).expect("instance with missing pop");
        if pos == start {
            // in between two calls
            if draw_call > 0 && draw_calls[draw_call - 1].can_contain(rpop) {
                // left can fit us
                draw_calls[draw_call - 1].len += 6;
            } else if draw_call < draw_calls.len() && draw_calls[draw_call].can_contain(rpop) {
                // right can fit us
                draw_calls[draw_call].len += 6;
            } else {
                // neither can, insert
                draw_calls.insert(draw_call, DrawCall {
                    category: rpop.category,
                    texture: rpop.texture,
                    len: 6,
                });
            }
        } else {
            // in the middle of a call
            if draw_calls[draw_call].can_contain(rpop) {
                // increase len
                draw_calls[draw_call].len += 6;
            } else {
                // split
                let mut clone = draw_calls[draw_call].clone();
                clone.len = pos - start;
                draw_calls[draw_call].len -= clone.len;
                draw_calls.insert(draw_call, DrawCall {
                    category: rpop.category,
                    texture: rpop.texture,
                    len: 6,
                });
                draw_calls.insert(draw_call, clone);
            }
        }

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

    pub fn remove_instance(&mut self, id: InstanceId) {
        let level = &mut self.levels[id.z as usize];
        let draw_calls = &mut level.draw_calls;
        level.instances.free(id.idx);

        if let Some(pos) = level.sorted_order.iter().position(|&idx| idx == id.idx) {
            level.sorted_order.remove(pos);
            level.index_buffer.get_mut().remove(pos);

            // find the draw call which previously contained the index
            let pos = 6 * (pos as u32);
            let mut start = 0;
            let mut draw_call = draw_calls.len();
            for (i, call) in draw_calls.iter_mut().enumerate() {
                if pos >= start && pos < start + call.len {
                    draw_call = i;
                    break;
                }
                start += call.len;
            }

            draw_calls[draw_call].len -= 6;
            if draw_calls[draw_call].len == 0 {
                draw_calls.remove(draw_call);
            }
        }
    }

    pub fn iter_instances<'a>(&'a self, (x, y, z): (u32, u32, u32)) -> impl Iterator<Item=(InstanceId, &'a Prefab)> + 'a {
        let level = &self.levels[z as usize];
        level.sorted_order.iter().rev().filter_map(move |&idx| {
            let inst = &level.instances.get_key(idx);
            if inst.x == x && inst.y == y {
                Some((InstanceId { z, idx }, &*inst.pop))
            } else {
                None
            }
        })
    }

    pub fn sort_again(&mut self, z: u32) {
        let pops = &self.pops;
        let level = &mut self.levels[z as usize];
        let sorted_order = &mut level.sorted_order;
        let draw_calls = &mut level.draw_calls;
        let instances = &level.instances;

        sorted_order.sort_by_key(|&idx| {
            let inst = instances.get_key(idx);
            let rpop = pops.get(&inst.pop).expect("instance with missing pop");
            rpop.sort_key()
        });
        level.index_buffer_dirty.set(true);

        draw_calls.clear();
        for &inst in sorted_order.iter() {
            let pop = &instances.get_key(inst).pop;
            let rpop = match pops.get(pop) {
                Some(rpop) => rpop,
                None => continue,
            };
            if let Some(call) = draw_calls.last_mut() {
                if call.can_contain(rpop) {
                    call.len += 6;
                    continue;
                }
            }
            draw_calls.push(DrawCall {
                category: rpop.category,
                texture: rpop.texture,
                len: 6,
            });
        }
    }

    #[inline]
    pub fn vertex_buffer(&self, z: u32) -> &[[Vertex; 4]] {
        self.levels[z as usize].instances.values()
    }

    pub fn index_buffer(&self, z: u32) -> Ref<[[u32; 6]]> {
        let level = &self.levels[z as usize];
        if level.index_buffer_dirty.get() {
            level.index_buffer_dirty.set(false);
            let mut ib = level.index_buffer.borrow_mut();
            ib.clear();
            for &inst in level.sorted_order.iter() {
                ib.push(indices(inst));
            }
        }
        Ref::map(level.index_buffer.borrow(), |v| &v[..])
    }
}

impl AtomZ {
    fn prep_instance(&mut self, pops: &mut WeakKeyHashMap<Weak<Prefab>, RenderPop>, (x, y): (u32, u32), prefab: Arc<Prefab>) -> usize {
        let vertices = pops.get(&prefab)
            .map_or_else(|| [Vertex::default(); 4], |rpop| rpop.instance((x, y)));
        self.instances.push(Instance { x, y, pop: prefab }, vertices)
    }
}

impl<'a> Defer<'a> {
    #[inline]
    pub fn add_pop(&mut self, prefab: &Prefab, icons: &IconCache, objtree: &ObjectTree) -> Arc<Prefab> {
        self.map.add_pop(prefab, icons, objtree)
    }

    pub fn add_instance(&mut self, (x, y): (u32, u32), prefab: Arc<Prefab>) -> InstanceId {
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

fn indices(inst: usize) -> [u32; 6] {
    let start = (inst * 4) as u32;
    [start, start + 1, start + 3, start + 1, start + 2, start + 3]
}
