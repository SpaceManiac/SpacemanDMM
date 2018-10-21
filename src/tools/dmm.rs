use std::collections::BTreeMap;
use std::path::Path;
use std::fs::File;
use std::io::{self, BufReader, BufWriter};
use std::fmt;

use ndarray::{self, Array3, Axis};
use linked_hash_map::LinkedHashMap;

use dm::{DMError, Location, HasLocation};
use dm::lexer::{LocationTracker, from_latin1, from_latin1_borrowed};
use dm::constants::Constant;

const MAX_KEY_LENGTH: u8 = 3;

/// BYOND is currently limited to 65534 keys.
/// https://secure.byond.com/forum/?post=2340796#comment23770802
type KeyType = u16;

/// An opaque map key.
#[derive(Copy, Clone, Debug, Hash, Ord, Eq, PartialOrd, PartialEq, Default)]
pub struct Key(u16);

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
        parse_map(&mut map, File::open(path).map_err(|e| {
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
        save_tgm(self, File::create(path)?)
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
        self.grid.subview(Axis(0), z)
    }

    #[inline]
    pub fn format_key(&self, key: Key) -> FormatKey {
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

// ----------------------------------------------------------------------------
// Map Writer

#[derive(Copy, Clone)]
pub struct FormatKey(u8, Key);

impl FormatKey {
    #[inline]
    pub fn new(key_length: u8, key: Key) -> FormatKey {
        FormatKey(key_length, key)
    }
}

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

const TGM_HEADER: &str = "//MAP CONVERTED BY dmm2tgm.py THIS HEADER COMMENT PREVENTS RECONVERSION, DO NOT REMOVE";

fn save_tgm(map: &Map, f: File) -> io::Result<()> {
    use std::io::Write;

    let mut f = BufWriter::new(f);
    write!(f, "{}\n", TGM_HEADER)?;

    // dictionary
    for (&key, prefabs) in map.dictionary.iter() {
        write!(f, "\"{}\" = (\n", map.format_key(key))?;
        for (i, fab) in prefabs.iter().enumerate() {
            write!(f, "{}", fab.path)?;
            if !fab.vars.is_empty() {
                write!(f, "{{")?;
                for (i, (var, value)) in fab.vars.iter().enumerate() {
                    write!(f, "\n\t{} = {}", var, value)?;
                    if i + 1 != fab.vars.len() {
                        write!(f, ";")?;
                    }
                }
                write!(f, "\n\t}}")?;
            }
            if i + 1 != prefabs.len() {
                write!(f, ",\n")?;
            }
        }
        write!(f, ")\n")?;
    }

    // grid in Y-major
    for (z, z_grid) in map.grid.axis_iter(Axis(0)).enumerate() {
        write!(f, "\n")?;
        for (x, x_col) in z_grid.axis_iter(Axis(1)).enumerate() {
            write!(f, "({},1,{}) = {{\"\n", x + 1, z + 1)?;
            for &elem in x_col.iter() {
                write!(f, "{}\n", map.format_key(elem))?;
            }
            write!(f, "\"}}\n")?;
        }
    }

    Ok(())
}

// ----------------------------------------------------------------------------
// Map Parser

#[inline]
fn take<T: Default>(t: &mut T) -> T {
    ::std::mem::replace(t, T::default())
}

fn parse_map(map: &mut Map, f: File) -> Result<(), DMError> {
    use std::io::Read;
    use std::cmp::max;

    let mut chars = LocationTracker::new(Default::default(), BufReader::new(f).bytes());

    let mut in_comment_line = false;
    let mut comment_trigger = false;

    // dictionary
    let mut curr_data = Vec::new();
    let mut curr_prefab = Prefab::default();
    let mut curr_var = Vec::new();
    let mut curr_datum = Vec::new();
    let mut curr_key = 0;
    let mut curr_key_length = 0;

    let mut in_quote_block = false;
    let mut in_key_block = false;
    let mut in_data_block = false;
    let mut in_varedit_block = false;
    let mut after_data_block = false;
    let mut escaping = false;
    let mut skip_whitespace = false;

    while let Some(ch) = chars.next() {
        let ch = ch?;
        if ch == b'\n' || ch == b'\r' {
            in_comment_line = false;
            comment_trigger = false;
            continue;
        } else if in_comment_line {
            continue;
        } else if ch == b'\t' {
            continue;
        }

        if ch == b'/' && !in_quote_block {
            if comment_trigger {
                in_comment_line = true;
                continue;
            } else {
                comment_trigger = true;
            }
        } else {
            comment_trigger = false;
        }

        if in_data_block {
            if in_varedit_block {
                if in_quote_block {
                    if ch == b'\\' {
                        curr_datum.push(ch);
                        escaping = true;
                    } else if escaping {
                        curr_datum.push(ch);
                        escaping = false;
                    } else if ch == b'"' {
                        curr_datum.push(ch);
                        in_quote_block = false;
                    } else {
                        curr_datum.push(ch);
                    }
                } else { // in_quote_block
                    if skip_whitespace && ch == b' ' {
                        skip_whitespace = false;
                        continue;
                    }
                    skip_whitespace = false;

                    if ch == b'"' {
                        curr_datum.push(ch);
                        in_quote_block = true;
                    } else if ch == b'=' && curr_var.is_empty() {
                        curr_var = take(&mut curr_datum);
                        let mut length = curr_var.len();
                        while length > 0 && (curr_var[length - 1] as char).is_whitespace() {
                            length -= 1;
                        }
                        curr_var.truncate(length);
                        skip_whitespace = true;
                    } else if ch == b';' {
                        curr_prefab.vars.insert(
                            from_latin1(take(&mut curr_var)),
                            parse_constant(chars.location(), take(&mut curr_datum))?,
                        );
                        skip_whitespace = true;
                    } else if ch == b'}' {
                        if !curr_var.is_empty() {
                            curr_prefab.vars.insert(
                                from_latin1(take(&mut curr_var)),
                                parse_constant(chars.location(), take(&mut curr_datum))?,
                            );
                        }
                        in_varedit_block = false;
                    } else {
                        curr_datum.push(ch);
                    }
                }
            } else if ch == b'{' {
                curr_prefab.path = from_latin1(take(&mut curr_datum));
                in_varedit_block = true;
            } else if ch == b',' {
                if curr_prefab.path.is_empty() && !curr_datum.is_empty() {
                    curr_prefab.path = from_latin1(take(&mut curr_datum));
                }
                curr_data.push(take(&mut curr_prefab));
            } else if ch == b')' {
                if curr_prefab.path.is_empty() && !curr_datum.is_empty() {
                    curr_prefab.path = from_latin1(take(&mut curr_datum));
                }
                curr_data.push(take(&mut curr_prefab));
                let key = take(&mut curr_key);
                let data = take(&mut curr_data);
                curr_key_length = 0;
                map.dictionary.insert(Key(key), data);
                in_data_block = false;
                after_data_block = true;
            } else {
                curr_datum.push(ch);
            }
        } else if in_key_block {
            if ch == b'"' {
                in_key_block = false;
                assert!(map.key_length == 0 || map.key_length == curr_key_length);
                map.key_length = curr_key_length;
            } else {
                curr_key = advance_key(curr_key, base_52_reverse(ch)?)?;
                curr_key_length += 1;
            }
        } else if ch == b'"' {
            in_key_block = true;
            after_data_block = false;
        } else if ch == b'(' {
            if after_data_block {
                curr_key = 0;
                curr_key_length = 0;
                break; // go to grid parsing
            } else {
                in_data_block = true;
                after_data_block = false;
            }
        }
    }

    // grid
    #[derive(PartialEq, Debug)]
    enum Coord {
        X,
        Y,
        Z,
    }

    let mut grid = BTreeMap::new();
    let mut reading_coord = Coord::X;
    let (mut curr_x, mut curr_y, mut curr_z) = (0, 0, 0);
    let (mut max_x, mut max_y, mut max_z) = (0, 0, 0);
    let mut curr_num = 0;
    let mut iter_x = 0;

    let mut in_coord_block = true;
    let mut in_map_string = false;
    let mut adjust_y = true;

    while let Some(ch) = chars.next() {
        let ch = ch?;
        if in_coord_block {
            if ch == b',' {
                if reading_coord == Coord::X {
                    curr_x = take(&mut curr_num);
                    max_x = max(max_x, curr_x);
                    iter_x = 0;
                    reading_coord = Coord::Y;
                } else if reading_coord == Coord::Y {
                    curr_y = take(&mut curr_num);
                    max_y = max(max_y, curr_y);
                    reading_coord = Coord::Z;
                } else {
                    return Err(DMError::new(Location::default(), "Incorrect number of coordinates"));
                }
            } else if ch == b')' {
                assert_eq!(reading_coord, Coord::Z);
                curr_z = take(&mut curr_num);
                max_z = max(max_z, curr_z);
                in_coord_block = false;
                reading_coord = Coord::X;
            } else {
                match (ch as char).to_digit(10) {
                    Some(x) => curr_num = 10 * curr_num + x as usize,
                    None => return Err(DMError::new(Location::default(), "bad digit in map coordinate")),
                }
            }
        } else if in_map_string {
            if ch == b'"' {
                in_map_string = false;
                adjust_y = true;
                curr_y -= 1;
            } else if ch == b'\r' {
                // nothing
            } else if ch == b'\n' {
                if adjust_y {
                    adjust_y = false;
                } else {
                    curr_y += 1;
                }
                max_x = max(max_x, curr_x);
                if iter_x > 1 {
                    curr_x = 1;
                }
                iter_x = 0;
            } else {
                curr_key = advance_key(curr_key, base_52_reverse(ch)?)?;
                curr_key_length += 1;
                if curr_key_length == map.key_length {
                    iter_x += 1;
                    if iter_x > 1 {
                        curr_x += 1;
                    }
                    let key = take(&mut curr_key);
                    curr_key_length = 0;
                    grid.insert((curr_x, curr_y, curr_z), Key(key));
                }
            }
        } else if ch == b'(' {
            in_coord_block = true;
        } else if ch == b'"' {
            in_map_string = true;
        }
    }
    max_y = max(max_y, curr_y);

    map.grid = Array3::from_shape_fn((max_z, max_y, max_x), |(z, y, x)| {
        grid[&(x + 1, y + 1, z + 1)]
    });

    Ok(())
}

fn base_52_reverse(ch: u8) -> Result<KeyType, DMError> {
    if ch >= b'a' && ch <= b'z' {
        Ok(ch as KeyType - b'a' as KeyType)
    } else if ch >= b'A' && ch <= b'Z' {
        Ok(26 + ch as KeyType - b'A' as KeyType)
    } else {
        Err(DMError::new(Location::default(), format!("Not a base-52 character: {:?}", ch as char)))
    }
}

fn advance_key(current: KeyType, next_digit: KeyType) -> Result<KeyType, DMError> {
    current.checked_mul(52).and_then(|b| b.checked_add(next_digit)).ok_or_else(|| {
        // https://secure.byond.com/forum/?post=2340796#comment23770802
        DMError::new(Location::default(), "Key overflow, max is 'ymo'")
    })
}

fn parse_constant(location: Location, input: Vec<u8>) -> Result<Constant, DMError> {
    use dm::Context;
    use dm::lexer::Lexer;
    use dm::parser::Parser;

    let mut bytes = input.iter().map(|&x| Ok(x));
    let ctx = Context::default();
    let expr = match Parser::new(&ctx, Lexer::new(&ctx, Default::default(), &mut bytes)).expression()? {
        Some(expr) => expr,
        None => return Err(DMError::new(location, format!("not an expression: {}", from_latin1_borrowed(&input)))),
    };
    if bytes.next().is_some() {
        return Err(DMError::new(location, format!("leftover: {:?} {}", from_latin1_borrowed(&input), bytes.len())));
    }
    ::dm::constants::simple_evaluate(location, expr)
}
