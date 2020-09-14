//! Editor-environment specific DMI (texture) handling.

use std::io;
use std::path::{Path, PathBuf};
use std::collections::hash_map::HashMap;
use std::sync::{Arc, RwLock};

use lodepng::{self, RGBA, Decoder, ColorType};

use gfx::{self, Factory as FactoryTrait};
use crate::{Factory, Texture};

type Rect = (u32, u32, u32, u32);

pub use dm::dmi::*;

// ----------------------------------------------------------------------------
// Icon file and metadata handling

pub struct IconCache {
    base_path: PathBuf,
    lock: RwLock<IconCacheInner>,
}

#[derive(Default)]
pub struct TextureCache {
    textures: Vec<Option<Texture>>,
}

#[derive(Default)]
struct IconCacheInner {
    icons: Vec<Arc<IconFile>>,
    paths: HashMap<PathBuf, Option<usize>>,
}

impl IconCache {
    pub fn new(base_path: &Path) -> IconCache {
        IconCache {
            base_path: base_path.to_owned(),
            lock: Default::default(),
        }
    }

    pub fn get_index(&self, relative_file_path: &Path) -> Option<usize> {
        let existing = self
            .lock
            .read()
            .expect("IconCache poisoned")
            .paths
            .get(relative_file_path)
            .cloned();
        // shouldn't be inlined or the lifetime of the lock will be extended
        match existing {
            // inner None = failure to load, don't keep trying every time
            Some(existing) => existing,
            None => match load(&self.base_path.join(relative_file_path)) {
                Some(icon) => {
                    let arc = Arc::new(icon);
                    let mut lock = self.lock.write().expect("IconCache poisoned");
                    let i = lock.icons.len();
                    lock.icons.push(arc);
                    lock.paths.insert(relative_file_path.to_owned(), Some(i));
                    Some(i)
                }
                None => {
                    let mut lock = self.lock.write().expect("IconCache poisoned");
                    lock.paths.insert(relative_file_path.to_owned(), None);
                    None
                }
            },
        }
    }

    pub fn get_icon(&self, id: usize) -> Arc<IconFile> {
        self.lock.read().expect("IconCache poisoned").icons[id].clone()
    }

    pub fn len(&self) -> usize {
        self.lock.read().expect("IconCache poisoned").icons.len()
    }

    /*pub fn clear(&mut self) {
        self.map.clear();
    }*/
}

impl TextureCache {
    pub fn retrieve(&mut self, factory: &mut Factory, icons: &IconCache, id: usize) -> &Texture {
        use crate::Fulfill;
        if id >= self.textures.len() {
            self.textures.resize(id + 1, None);
        }
        self.textures[id].fulfill(|| load_texture(factory, &icons.get_icon(id).bitmap))
    }

    pub fn clear(&mut self) {
        self.textures.clear();
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
    bitmap: lodepng::Bitmap<RGBA>,
}

impl IconFile {
    pub fn from_file(path: &Path) -> io::Result<IconFile> {
        let (bitmap, metadata) = Metadata::from_file(path)?;
        Ok(IconFile {
            metadata,
            width: bitmap.width as u32,
            height: bitmap.height as u32,
            bitmap,
        })
    }

    pub fn uv_of(&self, icon_state: &str, dir: Dir) -> Option<[f32; 4]> {
        self.rect_of(icon_state, dir).map(|(x1, y1, w, h)| [
            x1 as f32 / self.width as f32,
            y1 as f32 / self.height as f32,
            (x1 + w) as f32 / self.width as f32,
            (y1 + h) as f32 / self.height as f32,
        ])
    }

    #[inline]
    pub fn rect_of(&self, icon_state: &str, dir: Dir) -> Option<Rect> {
        self.metadata.rect_of(self.width, icon_state, dir, 0)
    }
}

pub fn texture_from_bytes(factory: &mut Factory, bytes: &[u8]) -> io::Result<Texture> {
    let mut decoder = Decoder::new();
    decoder.info_raw_mut().colortype = ColorType::RGBA;
    decoder.info_raw_mut().set_bitdepth(8);
    decoder.remember_unknown_chunks(false);
    let bitmap = match decoder.decode(bytes) {
        Ok(::lodepng::Image::RGBA(bitmap)) => bitmap,
        Ok(_) => return Err(io::Error::new(io::ErrorKind::InvalidData, "not RGBA")),
        Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidData, e)),
    };
    Ok(load_texture(factory, &bitmap))
}

pub fn load_texture(factory: &mut Factory, bitmap: &lodepng::Bitmap<RGBA>) -> Texture {
    let width = bitmap.width;
    let height = bitmap.height;
    let mut new_buffer = Vec::with_capacity(4 * width * height);
    for pixel in &bitmap.buffer {
        new_buffer.push(pixel.r);
        new_buffer.push(pixel.g);
        new_buffer.push(pixel.b);
        new_buffer.push(pixel.a);
    }

    let kind = gfx::texture::Kind::D2(width as u16, height as u16, gfx::texture::AaMode::Single);
    let (_, view) = factory.create_texture_immutable_u8::<crate::ColorFormat>(
        kind,
        gfx::texture::Mipmap::Provided,
        &[&new_buffer[..]]
    ).expect("create_texture_immutable_u8");
    view
}
