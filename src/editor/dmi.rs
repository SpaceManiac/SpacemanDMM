#![allow(dead_code)]

use std::io;
use std::path::{Path, PathBuf};
use std::collections::{btree_map, BTreeMap};

use lodepng::{self, RGBA};
use lodepng::ffi::{State as PngState, ColorType};

pub const NORTH: i32 = 1;
pub const SOUTH: i32 = 2;
pub const EAST: i32 = 4;
pub const WEST: i32 = 8;
pub const NORTHEAST: i32 = 5;
pub const NORTHWEST: i32 = 9;
pub const SOUTHEAST: i32 = 6;
pub const SOUTHWEST: i32 = 10;

type Rect = (u32, u32, u32, u32);

pub use dm::dmi::*;

// ----------------------------------------------------------------------------
// Icon file and metadata handling

#[derive(Default)]
pub struct IconCache {
    map: BTreeMap<PathBuf, Option<IconFile>>,
}

impl IconCache {
    pub fn retrieve(&mut self, path: &Path) -> Option<&IconFile> {
        match self.map.entry(path.to_owned()) {
            btree_map::Entry::Occupied(entry) => entry.into_mut().as_ref(),
            btree_map::Entry::Vacant(entry) => entry.insert(load(path)).as_ref(),
        }
    }
}

fn load(path: &Path) -> Option<IconFile> {
    match IconFile::from_file(path) {
        Ok(loaded) => Some(loaded),
        Err(err) => {
            eprintln!("error loading icon: {}\n  {}", path.display(), err);
            None
        }
    }
}

// ----------------------------------------------------------------------------
// Icon file and metadata handling

pub struct IconFile {
    pub metadata: Metadata,
    pub width: u32,
    pub texture: (),
}

impl IconFile {
    pub fn from_file(path: &Path) -> io::Result<IconFile> {
        let path = &::dm::fix_case(path);
        let mut decoder = PngState::new();
        decoder.info_raw.colortype = ColorType::RGBA;
        decoder.info_raw.set_bitdepth(8);
        decoder.remember_unknown_chunks(false);
        let bitmap = match decoder.decode_file(path) {
            Ok(::lodepng::Image::RGBA(bitmap)) => bitmap,
            Ok(_) => return Err(io::Error::new(io::ErrorKind::InvalidData, "not RGBA")),
            Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidData, e)),
        };

        let mut metadata = Metadata {
            width: bitmap.width as u32,
            height: bitmap.height as u32,
            states: Vec::new(),
            state_names: BTreeMap::new(),
        };
        for (key, value) in decoder.info_png().text_keys_cstr() {
            if key.to_str() == Ok("Description") {
                if let Ok(value) = value.to_str() {
                    metadata = Metadata::from_str(value);
                }
                break;
            }
        }

        Ok(IconFile {
            metadata: metadata,
            width: bitmap.width as u32,
            texture: load_texture(bitmap),
        })
    }

    pub fn rect_of(&self, icon_state: &str, dir: i32) -> Option<Rect> {
        let state_index = match self.metadata.state_names.get(icon_state) {
            Some(&i) => i,
            None => return None
        };
        let state = &self.metadata.states[state_index];

        let dir_idx = match (state.dirs, dir) {
            (Dirs::One, _) => 0,
            (Dirs::Eight, NORTHWEST) => 7,
            (Dirs::Eight, NORTHEAST) => 6,
            (Dirs::Eight, SOUTHWEST) => 5,
            (Dirs::Eight, SOUTHEAST) => 4,
            (_, WEST) => 3,
            (_, EAST) => 2,
            (_, NORTH) => 1,
            (_, _) => 0,
        };

        let icon_index = state.offset as u32 + dir_idx;
        let icon_count = self.width / self.metadata.width;
        let (icon_x, icon_y) = (icon_index % icon_count, icon_index / icon_count);
        Some((icon_x * self.metadata.width, icon_y * self.metadata.height,
            self.metadata.width, self.metadata.height))
    }
}

pub fn load_texture(_bitmap: lodepng::Bitmap<RGBA>) {
    // TODO
}
