use std::collections::BTreeMap;
use std::fmt;
use std::fs::File;
use std::io;
use std::path::Path;

use foldhash::fast::RandomState;
use indexmap::IndexMap;
use ndarray::{self, Array3, Axis};

use crate::dmi::Dir;
use dm::constants::Constant;
use dm::DMError;

mod read;
mod save_tgm;

const MAX_KEY_LENGTH: u8 = 3;

/// BYOND is currently limited to 65534 keys.
/// https://www.byond.com/forum/?post=2340796#comment23770802
type KeyType = u16;

/// An opaque map key.
#[derive(Copy, Clone, Debug, Hash, Ord, Eq, PartialOrd, PartialEq, Default)]
pub struct Key(KeyType);

/// An XY coordinate pair in the BYOND coordinate system.
///
/// The lower-left corner is `{ x: 1, y: 1 }`.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Coord2 {
    pub x: i32,
    pub y: i32,
}

impl Coord2 {
    #[inline]
    pub fn new(x: i32, y: i32) -> Coord2 {
        Coord2 { x, y }
    }

    #[inline]
    pub fn z(self, z: i32) -> Coord3 {
        Coord3 { x: self.x, y: self.y, z }
    }

    fn to_raw(self, (dim_y, dim_x): (usize, usize)) -> (usize, usize) {
        assert!(self.x >= 1 && self.x <= dim_x as i32, "x={} not in [1, {}]", self.x, dim_x);
        assert!(self.y >= 1 && self.y <= dim_y as i32, "y={} not in [1, {}]", self.y, dim_y);
        (dim_y - self.y as usize, self.x as usize - 1)
    }

    fn from_raw((y, x): (usize, usize), (dim_y, _dim_x): (usize, usize)) -> Coord2 {
        Coord2 { x: x as i32 + 1, y: (dim_y - y) as i32 }
    }
}

impl std::ops::Add<Dir> for Coord2 {
    type Output = Coord2;

    fn add(self, rhs: Dir) -> Coord2 {
        let (x, y) = rhs.offset();
        Coord2 { x: self.x + x, y: self.y + y }
    }
}

/// An XYZ coordinate triple in the BYOND coordinate system.
///
/// The lower-left corner of the first level is `{ x: 1, y: 1, z: 1 }`.
///
/// Note that BYOND by default considers "UP" to be Z+1, but this does not
/// necessarily apply to a given game's logic.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Coord3 {
    pub x: i32,
    pub y: i32,
    pub z: i32,
}

impl Coord3 {
    #[inline]
    pub fn new(x: i32, y: i32, z: i32) -> Coord3 {
        Coord3 { x, y, z }
    }

    #[inline]
    pub fn xy(self) -> Coord2 {
        Coord2 { x: self.x, y: self.y }
    }

    fn to_raw(self, (dim_z, dim_y, dim_x): (usize, usize, usize)) -> (usize, usize, usize) {
        assert!(self.x >= 1 && self.x <= dim_x as i32, "x={} not in [1, {}]", self.x, dim_x);
        assert!(self.y >= 1 && self.y <= dim_y as i32, "y={} not in [1, {}]", self.y, dim_y);
        assert!(self.z >= 1 && self.z <= dim_z as i32, "y={} not in [1, {}]", self.z, dim_z);
        (self.z as usize - 1, dim_y - self.y as usize, self.x as usize - 1)
    }

    #[allow(dead_code)]
    fn from_raw((z, y, x): (usize, usize, usize), (_dim_z, dim_y, _dim_x): (usize, usize, usize)) -> Coord3 {
        Coord3 { x: x as i32 + 1, y: (dim_y - y) as i32, z: z as i32 + 1 }
    }
}

/// A BYOND map, structured similarly to its serialized form.
#[derive(Clone)]
pub struct Map {
    key_length: u8,
    /// The map's dictionary keys in sorted order.
    pub dictionary: BTreeMap<Key, Vec<Prefab>>,
    /// The map's grid of keys in Z/Y/X order.
    pub grid: Array3<Key>,
}

/// A slice referencing one z-level of a `Map`.
#[derive(Clone, Copy)]
pub struct ZLevel<'a> {
    pub grid: ndarray::ArrayView<'a, Key, ndarray::Dim<[usize; 2]>>,
}

#[derive(Debug, Default, Clone)]
pub struct Prefab {
    pub path: String,
    // insertion order, sort of most of the time alphabetical but not quite
    pub vars: IndexMap<String, Constant, RandomState>,
}

impl PartialEq for Prefab {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path && self.vars == other.vars
    }
}

impl Eq for Prefab {}

impl std::hash::Hash for Prefab {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.hash(state);
        let mut items: Vec<_> = self.vars.iter().collect();
        items.sort_by_key(|&(k, _)| k);
        for kvp in items {
            kvp.hash(state);
        }
    }
}

impl Map {
    pub fn new(x: usize, y: usize, z: usize, turf: String, area: String) -> Map {
        assert!(x > 0 && y > 0 && z > 0, "({x}, {y}, {z})");

        let mut dictionary = BTreeMap::new();
        dictionary.insert(Key(0), vec![
            Prefab::from_path(turf),
            Prefab::from_path(area),
        ]);

        let grid = Array3::default((z, y, x));  // default = 0

        Map {
            key_length: 1,
            dictionary,
            grid,
        }
    }

    pub fn with_empty_dictionary(x: usize, y: usize, z: usize) -> Map {
        Map {
            key_length: 1,
            dictionary: BTreeMap::new(),
            grid: Array3::default((z, y, x)),
        }
    }

    pub fn from_file(path: &Path) -> Result<Map, DMError> {
        let mut map = Map {
            key_length: 0,
            dictionary: Default::default(),
            grid: Array3::default((1, 1, 1)),
        };
        read::parse_map(&mut map, path)?;
        Ok(map)
    }

    pub fn to_writer(&self, writer: &mut impl std::io::Write) -> io::Result<()> {
        save_tgm::save_tgm(self, writer)
    }

    pub fn to_file(&self, path: &Path) -> io::Result<()> {
        self.to_writer(&mut File::create(path)?)
    }

    pub fn key_length(&self) -> u8 {
        self.key_length
    }

    pub fn adjust_key_length(&mut self) {
        if let Some(max_key) = self.dictionary.keys().max() {
            let max_key = max_key.0;
            if max_key >= 2704 {
                self.key_length = 3;
            } else if max_key >= 52 {
                self.key_length = 2;
            } else {
                self.key_length = 1;
            }
        }
    }

    #[inline]
    pub fn dim_xyz(&self) -> (usize, usize, usize) {
        let dim = self.grid.dim();
        (dim.2, dim.1, dim.0)
    }

    #[inline]
    pub fn dim_z(&self) -> usize {
        self.grid.dim().0
    }

    #[inline]
    pub fn z_level(&self, z: usize) -> ZLevel<'_> {
        ZLevel { grid: self.grid.index_axis(Axis(0), z) }
    }

    pub fn iter_levels(&self) -> impl Iterator<Item=(i32, ZLevel<'_>)> + '_ {
        self.grid.axis_iter(Axis(0)).enumerate().map(|(i, grid)| (i as i32 + 1, ZLevel { grid }))
    }

    #[inline]
    pub fn format_key(&self, key: Key) -> impl std::fmt::Display {
        FormatKey(self.key_length, key)
    }
}

impl std::ops::Index<Coord3> for Map {
    type Output = Key;

    #[inline]
    fn index(&self, coord: Coord3) -> &Key {
        &self.grid[coord.to_raw(self.grid.dim())]
    }
}

impl<'a> ZLevel<'a> {
    /// Iterate over the z-level in row-major order starting at the top-left.
    pub fn iter_top_down(&self) -> impl Iterator<Item=(Coord2, Key)> + '_ {
        let dim = self.grid.dim();
        self.grid.indexed_iter().map(move |(c, k)| (Coord2::from_raw(c, dim), *k))
    }
}

impl<'a> std::ops::Index<Coord2> for ZLevel<'a> {
    type Output = Key;

    #[inline]
    fn index(&self, coord: Coord2) -> &Key {
        &self.grid[coord.to_raw(self.grid.dim())]
    }
}

impl Prefab {
    pub fn from_path<S: Into<String>>(path: S) -> Prefab {
        Prefab {
            path: path.into(),
            vars: Default::default(),
        }
    }
}

impl fmt::Display for Prefab {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.path)?;
        if !self.vars.is_empty() {
            write!(f, " {{")?;
            let mut first = true;
            for (k, v) in self.vars.iter() {
                if !first {
                    write!(f, "; ")?;
                }
                first = false;
                if f.alternate() {
                    f.write_str("\n    ")?;
                }
                write!(f, "{k} = {v}")?;
            }
            if f.alternate() {
                f.write_str("\n")?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

impl fmt::Display for Coord2 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl fmt::Display for Coord3 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {}, {})", self.x, self.y, self.z)
    }
}

impl Key {
    pub fn invalid() -> Key {
        Key(KeyType::MAX)
    }

    pub fn next(self) -> Key {
        Key(self.0 + 1)
    }
}

#[derive(Copy, Clone)]
struct FormatKey(u8, Key);

impl fmt::Display for FormatKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Write;
        let FormatKey(key_length, Key(key)) = *self;

        if key_length < MAX_KEY_LENGTH && key >= 52u16.pow(key_length as u32) {
            panic!("Attempted to format an out-of-range key");
        }

        let mut current = 52usize.pow(key_length as u32 - 1);
        for _ in 0..key_length {
            f.write_char(BASE_52[(key as usize / current) % 52] as char)?;
            current /= 52;
        }

        Ok(())
    }
}

const BASE_52: &[u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

fn base_52_reverse(ch: u8) -> Result<KeyType, String> {
    if ch.is_ascii_lowercase() {
        Ok(ch as KeyType - b'a' as KeyType)
    } else if ch.is_ascii_uppercase() {
        Ok(26 + ch as KeyType - b'A' as KeyType)
    } else {
        Err(format!("Not a base-52 character: {:?}", ch as char))
    }
}

fn advance_key(current: KeyType, next_digit: KeyType) -> Result<KeyType, &'static str> {
    current.checked_mul(52).and_then(|b| b.checked_add(next_digit)).ok_or({
        // https://www.byond.com/forum/?post=2340796#comment23770802
        "Key overflow, max is 'ymo'"
    })
}
