//! TGM map writer.
use std::fs::File;
use std::io::{self, Write, BufWriter};

use ndarray::Axis;

use super::Map;

const TGM_HEADER: &str = "//MAP CONVERTED BY dmm2tgm.py THIS HEADER COMMENT PREVENTS RECONVERSION, DO NOT REMOVE";

pub fn save_tgm(map: &Map, f: File) -> io::Result<()> {
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
