use std::collections::BTreeMap;
use std::path::Path;
use std::fs::File;
use std::io::{self, BufReader};

use ndarray::{self, Array3, Axis};

#[derive(Debug)]
pub struct Map {
    pub key_length: u8,
    pub dictionary: BTreeMap<u32, Vec<Prefab>>,
    pub grid: Array3<u32>, // Z/Y/X order
}

pub type Grid<'a> = ndarray::ArrayBase<ndarray::ViewRepr<&'a u32>, ndarray::Dim<[usize; 2]>>;

#[derive(Debug, Default)]
pub struct Prefab {
    pub path: String,
    // important that this is a BTreeMap, so it's ordered alphabetically
    pub vars: BTreeMap<String, String>,
}

impl Map {
    pub fn from_file(path: &Path) -> io::Result<Map> {
        flame!("Map::from_file");
        let mut map = Map {
            key_length: 0,
            dictionary: BTreeMap::new(),
            grid: Array3::default((1, 255, 255)),
        };
        parse_map(&mut map, File::open(path)?)?;
        Ok(map)
    }

    #[inline]
    pub fn dim_z(&self) -> usize {
        self.grid.dim().0
    }

    #[inline]
    pub fn z_level(&self, z: usize) -> Grid {
        self.grid.subview(Axis(0), z)
    }
}

// ----------------------------------------------------------------------------
// Map Parser

#[inline]
fn take<T: Default>(t: &mut T) -> T {
    ::std::mem::replace(t, T::default())
}

fn parse_map(map: &mut Map, f: File) -> io::Result<()> {
    use std::cmp::max;

    let mut chars = ::utils::Chars::new(BufReader::new(f));

    let mut in_comment_line = false;
    let mut comment_trigger = false;

    // dictionary
    let mut curr_data = Vec::new();
    let mut curr_prefab = Prefab::default();
    let mut curr_var = String::new();
    let mut curr_datum = String::new();
    let mut curr_key = 0;
    let mut curr_key_length = 0;

    let mut in_quote_block = false;
    let mut in_key_block = false;
    let mut in_data_block = false;
    let mut in_varedit_block = false;
    let mut after_data_block = false;
    let mut escaping = false;
    let mut skip_whitespace = false;

    for ch in chars.by_ref() {
        let ch = ch?;

        if ch == '\n' || ch == '\r' {
            in_comment_line = false;
            comment_trigger = false;
            continue
        } else if in_comment_line {
            continue
        } else if ch == '\t' {
            continue
        }

        if ch == '/' && !in_quote_block {
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
                    if ch == '\\' {
                        curr_datum.push(ch);
                        escaping = true;
                    } else if escaping {
                        curr_datum.push(ch);
                        escaping = false;
                    } else if ch == '"' {
                        curr_datum.push(ch);
                        in_quote_block = false;
                    } else {
                        curr_datum.push(ch);
                    }
                } else { // in_quote_block
                    if skip_whitespace && ch == ' ' {
                        skip_whitespace = false;
                        continue;
                    }
                    skip_whitespace = false;

                    if ch == '"' {
                        curr_datum.push(ch);
                        in_quote_block = true;
                    } else if ch == '=' {
                        curr_var = take(&mut curr_datum);
                        let length = curr_var.trim_right().len();
                        curr_var.truncate(length);
                        skip_whitespace = true;
                    } else if ch == ';' {
                        curr_prefab.vars.insert(take(&mut curr_var), take(&mut curr_datum));
                        skip_whitespace = true;
                    } else if ch == '}' {
                        curr_prefab.vars.insert(take(&mut curr_var), take(&mut curr_datum));
                        in_varedit_block = false;
                    } else {
                        curr_datum.push(ch);
                    }
                }
            } else if ch == '{' {
                curr_prefab.path = take(&mut curr_datum);
                in_varedit_block = true;
            } else if ch == ',' {
                if curr_prefab.path.is_empty() && !curr_datum.is_empty() {
                    curr_prefab.path = take(&mut curr_datum);
                }
                curr_data.push(take(&mut curr_prefab));
            } else if ch == ')' {
                if curr_prefab.path.is_empty() && !curr_datum.is_empty() {
                    curr_prefab.path = take(&mut curr_datum);
                }
                curr_data.push(take(&mut curr_prefab));
                let key = take(&mut curr_key);
                let data = take(&mut curr_data);
                curr_key_length = 0;
                map.dictionary.insert(key, data);
                in_data_block = false;
                after_data_block = true;
            } else {
                curr_datum.push(ch);
            }
        } else if in_key_block {
            if ch == '"' {
                in_key_block = false;
                assert!(map.key_length == 0 || map.key_length == curr_key_length);
                map.key_length = curr_key_length;
            } else {
                curr_key = 52 * curr_key + base_52_reverse(ch).unwrap();
                curr_key_length += 1;
            }
        } else if ch == '"' {
            in_key_block = true;
            after_data_block = false;
        } else if ch == '(' {
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
        X, Y, Z
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

    for ch in chars {
        let ch = ch?;
        if in_coord_block {
            if ch == ',' {
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
                    unimplemented!() // error
                }
            } else if ch == ')' {
                assert_eq!(reading_coord, Coord::Z);
                curr_z = take(&mut curr_num);
                max_z = max(max_z, curr_z);
                in_coord_block = false;
                reading_coord = Coord::X;
            } else {
                match ch.to_digit(10) {
                    Some(x) => curr_num = 10 * curr_num + x as usize,
                    None => panic!("bad digit")
                }
            }
        } else if in_map_string {
            if ch == '"' {
                in_map_string = false;
                adjust_y = true;
                curr_y -= 1;
            } else if ch == '\r' {
                // nothing
            } else if ch == '\n' {
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
                curr_key = 52 * curr_key + base_52_reverse(ch).unwrap();
                curr_key_length += 1;
                if curr_key_length == map.key_length {
                    iter_x += 1;
                    if iter_x > 1 {
                        curr_x += 1;
                    }
                    let key = take(&mut curr_key);
                    curr_key_length = 0;
                    grid.insert((curr_x, curr_y, curr_z), key);
                }
            }
        } else if ch == '(' {
            in_coord_block = true;
        } else if ch == '"' {
            in_map_string = true;
        }
    }
    max_y = max(max_y, curr_y);

    map.grid = Array3::from_shape_fn((max_z, max_y, max_x), |(z, y, x)| {
        grid[&(x + 1, y + 1, z + 1)]
    });

    Ok(())
}

#[inline]
fn base_52_reverse(ch: char) -> Option<u32> {
    if ch >= 'a' && ch <= 'z' {
        Some(ch as u32 - b'a' as u32)
    } else if ch >= 'A' && ch <= 'Z' {
        Some(26 + ch as u32 - b'A' as u32)
    } else {
        None
    }
}

// ----------------------------------------------------------------------------
// Tests

#[cfg(test)]
mod test {
    extern crate walkdir;
    use self::walkdir::{DirEntry, WalkDir, WalkDirIterator};
    use super::*;

    #[test]
    fn parse_all_dmm() {
        fn is_visible(entry: &DirEntry) -> bool {
            entry.path()
                .file_name()
                .unwrap_or("".as_ref())
                .to_str()
                .map(|s| !s.starts_with("."))
                .unwrap_or(true)
        }

        for entry in WalkDir::new("../../")
            .into_iter()
            .filter_entry(is_visible)
        {
            let entry = entry.unwrap();
            if entry.file_type().is_file() && entry.path().extension() == Some("dmm".as_ref()) {
                println!("{:?}", entry.path());
                Map::from_file(entry.path()).unwrap();
            }
        }
    }
}