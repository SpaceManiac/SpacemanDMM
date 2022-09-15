//! Map parser, supporting standard DMM or TGM-format files.
use std::cmp::max;
use std::collections::BTreeMap;
use std::mem::take;

use ndarray::Array3;

use dm::lexer::{from_utf8_or_latin1, LocationTracker};
use dm::{DMError, Location};

use super::{Key, KeyType, Map, Prefab};

pub fn parse_map(map: &mut Map, path: &std::path::Path) -> Result<(), DMError> {
    let file_id = Default::default();
    let mut chars = LocationTracker::new(file_id, dm::lexer::buffer_file(file_id, path)?.into());

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

    let mut curr_key_start_location = Location::default();

    let mut curr_datum_start_location = Location::default();
    macro_rules! set_curr_datum_start_location {
        () => {
            if curr_datum.is_empty() {
                curr_datum_start_location = chars.location();
            }
        };
    }

    macro_rules! insert_current_var {
        () => {
            curr_prefab.vars.insert(
                from_utf8_or_latin1(take(&mut curr_var)),
                dm::constants::evaluate_str(curr_datum_start_location, &take(&mut curr_datum))
                    .map_err(|e| {
                        e.with_note(
                            curr_key_start_location,
                            format!(
                                "within key: \"{}\"",
                                super::FormatKey(curr_key_length, super::Key(curr_key))
                            ),
                        )
                    })?,
            );
        };
    }

    while let Some(ch) = chars.next() {
        // Readability, simple elif chain isn't duplicate code
        #[allow(clippy::if_same_then_else)]
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
                        set_curr_datum_start_location!();
                        curr_datum.push(ch);
                        escaping = true;
                    } else if escaping {
                        set_curr_datum_start_location!();
                        curr_datum.push(ch);
                        escaping = false;
                    } else if ch == b'"' {
                        set_curr_datum_start_location!();
                        curr_datum.push(ch);
                        in_quote_block = false;
                    } else {
                        set_curr_datum_start_location!();
                        curr_datum.push(ch);
                    }
                } else {
                    // in_quote_block
                    if skip_whitespace && ch == b' ' {
                        skip_whitespace = false;
                        continue;
                    }
                    skip_whitespace = false;

                    if ch == b'"' {
                        set_curr_datum_start_location!();
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
                        insert_current_var!();
                        skip_whitespace = true;
                    } else if ch == b'}' {
                        if !curr_var.is_empty() {
                            insert_current_var!();
                        }
                        in_varedit_block = false;
                    } else {
                        set_curr_datum_start_location!();
                        curr_datum.push(ch);
                    }
                }
            } else if ch == b'{' {
                curr_prefab.path = from_utf8_or_latin1(take(&mut curr_datum));
                in_varedit_block = true;
            } else if ch == b',' {
                if curr_prefab.path.is_empty() && !curr_datum.is_empty() {
                    curr_prefab.path = from_utf8_or_latin1(take(&mut curr_datum));
                }
                curr_data.push(take(&mut curr_prefab));
            } else if ch == b')' {
                if curr_prefab.path.is_empty() && !curr_datum.is_empty() {
                    curr_prefab.path = from_utf8_or_latin1(take(&mut curr_datum));
                }
                curr_data.push(take(&mut curr_prefab));
                let key = take(&mut curr_key);
                let data = take(&mut curr_data);
                curr_key_length = 0;
                map.dictionary.insert(Key(key), data);
                in_data_block = false;
                after_data_block = true;
            } else {
                set_curr_datum_start_location!();
                curr_datum.push(ch);
            }
        } else if in_key_block {
            if ch == b'"' {
                in_key_block = false;
                assert!(map.key_length == 0 || map.key_length == curr_key_length);
                map.key_length = curr_key_length;
            } else {
                if curr_key == 0 {
                    curr_key_start_location = chars.location();
                }
                curr_key = advance_key(chars.location(), curr_key, ch)?;
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
    let mut base_x = 0;

    let mut in_coord_block = true;
    let mut in_map_string = false;
    let mut adjust_y = true;

    while let Some(ch) = chars.next() {
        if in_coord_block {
            if ch == b',' {
                if reading_coord == Coord::X {
                    curr_x = take(&mut curr_num);
                    max_x = max(max_x, curr_x);
                    base_x = curr_x;
                    reading_coord = Coord::Y;
                } else if reading_coord == Coord::Y {
                    curr_y = take(&mut curr_num);
                    max_y = max(max_y, curr_y);
                    reading_coord = Coord::Z;
                } else {
                    return Err(DMError::new(
                        chars.location(),
                        "Incorrect number of coordinates",
                    ));
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
                    None => {
                        return Err(DMError::new(
                            chars.location(),
                            format!("bad digit {:?} in map coordinate", ch),
                        ))
                    }
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
                curr_x = base_x;
            } else {
                curr_key = advance_key(chars.location(), curr_key, ch)?;
                curr_key_length += 1;
                if curr_key_length == map.key_length {
                    let key = take(&mut curr_key);
                    curr_key_length = 0;
                    if grid.insert((curr_x, curr_y, curr_z), Key(key)).is_some() {
                        return Err(DMError::new(
                            chars.location(),
                            format!("multiple entries for ({}, {}, {})", curr_x, curr_y, curr_z),
                        ));
                    }
                    max_x = max(max_x, curr_x);
                    curr_x += 1;
                }
            }
        } else if ch == b'(' {
            in_coord_block = true;
        } else if ch == b'"' {
            in_map_string = true;
        }
    }
    max_y = max(max_y, curr_y);

    let mut result = Ok(());
    map.grid = Array3::from_shape_fn((max_z, max_y, max_x), |(z, y, x)| {
        if let Some(&tile) = grid.get(&(x + 1, y + 1, z + 1)) {
            tile
        } else {
            result = Err(DMError::new(
                chars.location(),
                format!("no value for tile ({}, {}, {})", x + 1, y + 1, z + 1),
            ));
            Key(0)
        }
    });

    result
}

fn advance_key(loc: Location, curr_key: KeyType, ch: u8) -> Result<KeyType, DMError> {
    match super::base_52_reverse(ch) {
        Err(err) => Err(DMError::new(loc, err)),
        Ok(single) => match super::advance_key(curr_key, single) {
            Err(err) => Err(DMError::new(loc, err)),
            Ok(key) => Ok(key),
        },
    }
}
