use std::collections::BTreeMap;
use std::path::Path;
use std::fs::File;
use std::io;
use std::fmt;

use ndarray::{self, Array3, Axis};
use linked_hash_map::LinkedHashMap;

use dm::{DMError, Location};
use dm::constants::Constant;

mod read;
mod save_tgm;

const MAX_KEY_LENGTH: u8 = 3;

/// BYOND is currently limited to 65534 keys.
/// https://secure.byond.com/forum/?post=2340796#comment23770802
type KeyType = u16;

/// An opaque map key.
#[derive(Copy, Clone, Debug, Hash, Ord, Eq, PartialOrd, PartialEq, Default)]
pub struct Key(KeyType);

/// An XY coordinate pair in the BYOND coordinate system.
///
/// The lower-left corner is `{ x: 1, y: 1 }`.
pub struct Coord2 {
    pub x: i32,
    pub y: i32,
}

impl Coord2 {
    #[inline]
    pub fn new(x: i32, y: i32) -> Coord2 {
        Coord2 { x, y }
    }
}

/// An XYZ coordinate triple in the BYOND coordinate system.
///
/// The lower-left corner of the first level is `{ x: 1, y: 1, z: 1 }`.
///
/// Note that BYOND by default considers "UP" to be Z+1, but this does not
/// necessarily apply to a given game's logic.
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
}

#[derive(Debug, Clone)]
pub struct Map {
    pub key_length: u8,
    // sorted order
    pub dictionary: BTreeMap<Key, Vec<Prefab>>,
    pub grid: Array3<Key>, // Z/Y/X order
}

pub type Grid<'a> = ndarray::ArrayBase<ndarray::ViewRepr<&'a Key>, ndarray::Dim<[usize; 2]>>;

// TODO: port to ast::Prefab<Constant>
#[derive(Debug, Default, Hash, Eq, PartialEq, Clone)]
pub struct Prefab {
    pub path: String,
    // insertion order, sort of most of the time alphabetical but not quite
    pub vars: LinkedHashMap<String, Constant>,
}

impl Map {
    pub fn from_file(path: &Path) -> Result<Map, DMError> {
        let mut map = Map {
            key_length: 0,
            dictionary: Default::default(),
            grid: Array3::default((1, 1, 1)),
        };
        read::parse_map(&mut map, File::open(path).map_err(|e| {
            DMError::new(Location::default(), "i/o error").set_cause(e)
        })?)?;
        Ok(map)
    }

    pub fn new(x: usize, y: usize, z: usize, turf: String, area: String) -> Map {
        assert!(x > 0 && y > 0 && z > 0, "({}, {}, {})", x, y, z);

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

    pub fn to_file(&self, path: &Path) -> io::Result<()> {
        // DMM saver later
        save_tgm::save_tgm(self, File::create(path)?)
    }

    pub fn adjust_key_length(&mut self) {
        if self.dictionary.len() > 2704 {
            self.key_length = 3;
        } else if self.dictionary.len() > 52 {
            self.key_length = 2;
        } else {
            self.key_length = 1;
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
    pub fn z_level(&self, z: usize) -> Grid {
        self.grid.index_axis(Axis(0), z)
    }

    #[inline]
    pub fn format_key(&self, key: Key) -> impl std::fmt::Display {
        FormatKey(self.key_length, key)
    }

    pub fn zero_to_one(&self, (x, y, z): (usize, usize, usize)) -> (usize, usize, usize) {
        (x + 1, self.grid.dim().1 - y, z + 1)
    }

    pub fn one_to_zero(&self, (x, y, z): (usize, usize, usize)) -> (usize, usize, usize) {
        (x - 1, self.grid.dim().1 - y, z - 1)
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
                write!(f, "{} = {}", k, v)?;
            }
            if f.alternate() {
                f.write_str("\n")?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

impl Key {
    pub fn invalid() -> Key {
        Key(KeyType::max_value())
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
    if ch >= b'a' && ch <= b'z' {
        Ok(ch as KeyType - b'a' as KeyType)
    } else if ch >= b'A' && ch <= b'Z' {
        Ok(26 + ch as KeyType - b'A' as KeyType)
    } else {
        Err(format!("Not a base-52 character: {:?}", ch as char))
    }
}

fn advance_key(current: KeyType, next_digit: KeyType) -> Result<KeyType, &'static str> {
    current.checked_mul(52).and_then(|b| b.checked_add(next_digit)).ok_or_else(|| {
        // https://secure.byond.com/forum/?post=2340796#comment23770802
        "Key overflow, max is 'ymo'"
    })
}
