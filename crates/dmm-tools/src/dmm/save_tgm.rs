//! TGM map writer.
use std::fs::File;
use std::io::{self, BufWriter, Write};

use ndarray::Axis;

use super::Map;

const TGM_HEADER: &str =
    "//MAP CONVERTED BY dmm2tgm.py THIS HEADER COMMENT PREVENTS RECONVERSION, DO NOT REMOVE";

// Note: writeln! currently (2022-04-30) writes the \n character alone on all platforms
// If that changes, this will break.
pub fn save_tgm(map: &Map, f: File) -> io::Result<()> {
    let mut f = BufWriter::new(f);
    writeln!(f, "{}", TGM_HEADER)?;

    // dictionary
    for (&key, prefabs) in map.dictionary.iter() {
        writeln!(f, "\"{}\" = (", map.format_key(key))?;
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
                writeln!(f, ",")?;
            }
        }
        writeln!(f, ")")?;
    }

    // grid in Y-major
    for (z, z_grid) in map.grid.axis_iter(Axis(0)).enumerate() {
        writeln!(f)?;
        for (x, x_col) in z_grid.axis_iter(Axis(1)).enumerate() {
            writeln!(f, "({},1,{}) = {{\"", x + 1, z + 1)?;
            for &elem in x_col.iter() {
                writeln!(f, "{}", map.format_key(elem))?;
            }
            writeln!(f, "\"}}")?;
        }
    }

    Ok(())
}
