#![allow(dead_code)]

use std::io;
use std::path::{Path, PathBuf};
use std::collections::hash_map::{HashMap, Entry};
use std::collections::BTreeMap;

use lodepng::{self, RGBA};
use lodepng::ffi::{State as PngState, ColorType};

use gfx::{self, Factory as FactoryTrait};
use {Factory, Texture};

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

pub struct IconCache {
    base_path: PathBuf,
    icons: Vec<IconFile>,
    map: HashMap<PathBuf, Option<usize>>,
}

impl IconCache {
    pub fn new(base_path: &Path) -> IconCache {
        IconCache {
            base_path: base_path.to_owned(),
            icons: Default::default(),
            map: Default::default(),
        }
    }

    pub fn retrieve(&mut self, relative_file_path: &Path) -> Option<&mut IconFile> {
        match self.map.entry(relative_file_path.to_owned()) {
            Entry::Occupied(entry) => match entry.into_mut().as_ref() {
                Some(&i) => self.icons.get_mut(i),
                None => None,
            },
            Entry::Vacant(entry) => {
                match load(&self.base_path.join(relative_file_path)) {
                    Some(icon) => {
                        let i = self.icons.len();
                        self.icons.push(icon);
                        entry.insert(Some(i));
                        self.icons.last_mut()
                    },
                    None => {
                        entry.insert(None);
                        None
                    },
                }
            }
        }
    }

    pub fn get_id(&self, relative_file_path: &Path) -> Option<usize> {
        self.map.get(relative_file_path).and_then(|&x| x)
    }

    pub fn get_by_id(&mut self, id: usize) -> Option<&mut IconFile> {
        self.icons.get_mut(id)
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn clear(&mut self) {
        self.map.clear();
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
    pub height: u32,
    texture: MaybeTexture,
}

enum MaybeTexture {
    Invalid,
    Bitmap(lodepng::Bitmap<RGBA>),
    Texture(Texture),
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
            height: bitmap.height as u32,
            texture: MaybeTexture::Bitmap(bitmap),
        })
    }

    pub fn texture(&mut self, factory: &mut Factory) -> &Texture {
        self.texture = match ::std::mem::replace(&mut self.texture, MaybeTexture::Invalid) {
            MaybeTexture::Bitmap(bitmap) => MaybeTexture::Texture(load_texture(factory, bitmap)),
            other => other,
        };
        if let MaybeTexture::Texture(ref t) = self.texture {
            t
        } else {
            panic!("IconFile::texture in invalid state")
        }
    }

    pub fn uv_of(&self, icon_state: &str, dir: i32) -> Option<(f32, f32, f32, f32)> {
        self.rect_of(icon_state, dir).map(|(x1, y1, w, h)| (
            x1 as f32 / self.width as f32,
            y1 as f32 / self.height as f32,
            (x1 + w) as f32 / self.width as f32,
            (y1 + h) as f32 / self.height as f32,
        ))
    }

    pub fn rect_of(&self, icon_state: &str, dir: i32) -> Option<Rect> {
        if self.metadata.states.is_empty() {
            return Some((0, 0, self.metadata.width, self.metadata.height));
        }
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

pub fn texture_from_bytes(factory: &mut Factory, bytes: &[u8]) -> io::Result<Texture> {
    let mut decoder = PngState::new();
    decoder.info_raw.colortype = ColorType::RGBA;
    decoder.info_raw.set_bitdepth(8);
    decoder.remember_unknown_chunks(false);
    let bitmap = match decoder.decode(bytes) {
        Ok(::lodepng::Image::RGBA(bitmap)) => bitmap,
        Ok(_) => return Err(io::Error::new(io::ErrorKind::InvalidData, "not RGBA")),
        Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidData, e)),
    };
    Ok(load_texture(factory, bitmap))
}

pub fn load_texture(factory: &mut Factory, bitmap: lodepng::Bitmap<RGBA>) -> Texture {
    let width = bitmap.width;
    let height = bitmap.height;
    let mut new_buffer = Vec::with_capacity(4 * width * height);
    for pixel in bitmap.buffer {
        new_buffer.push(pixel.r);
        new_buffer.push(pixel.g);
        new_buffer.push(pixel.b);
        new_buffer.push(pixel.a);
    }

    let kind = gfx::texture::Kind::D2(width as u16, height as u16, gfx::texture::AaMode::Single);
    let (_, view) = factory.create_texture_immutable_u8::<::ColorFormat>(
        kind,
        gfx::texture::Mipmap::Provided,
        &[&new_buffer[..]]
    ).expect("create_texture_immutable_u8");
    view
}
