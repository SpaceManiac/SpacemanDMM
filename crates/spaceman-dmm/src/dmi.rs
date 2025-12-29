//! Editor-environment specific DMI (texture) handling.

use std::collections::hash_map::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use lodepng::{self, ColorType, Decoder, RGBA};
use sdl3::pixels::{PixelFormat, PixelFormatEnum};
use sdl3::render::Texture;
use sdl3::surface::Surface;

use crate::support::TextureCreator;

type Rect = (u32, u32, u32, u32);

pub use dreammaker::dmi::*;

// ----------------------------------------------------------------------------
// Icon file and metadata handling

pub struct IconCache {
    base_path: PathBuf,
    lock: RwLock<IconCacheInner>,
}

#[derive(Default)]
pub struct TextureCache<'r> {
    textures: Vec<Option<Texture<'r>>>,
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
                },
                None => {
                    let mut lock = self.lock.write().expect("IconCache poisoned");
                    lock.paths.insert(relative_file_path.to_owned(), None);
                    None
                },
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

impl<'r> TextureCache<'r> {
    pub fn retrieve(
        &mut self,
        factory: &'r TextureCreator,
        icons: &IconCache,
        id: usize,
    ) -> &Texture {
        if id >= self.textures.len() {
            self.textures.resize_with(id + 1, Default::default);
        }
        self.textures[id].get_or_insert_with(|| load_texture(factory, &icons.get_icon(id).bitmap))
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
        },
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
        self.rect_of(icon_state, dir).map(|(x1, y1, w, h)| {
            [
                x1 as f32 / self.width as f32,
                y1 as f32 / self.height as f32,
                (x1 + w) as f32 / self.width as f32,
                (y1 + h) as f32 / self.height as f32,
            ]
        })
    }

    #[inline]
    pub fn rect_of(&self, icon_state: &str, dir: Dir) -> Option<Rect> {
        self.metadata
            .rect_of(self.width, &StateIndex::from(icon_state), dir, 0)
    }
}

pub fn texture_from_bytes<'r>(
    factory: &'r TextureCreator,
    bytes: &[u8],
) -> io::Result<Texture<'r>> {
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

pub fn load_texture<'r>(
    factory: &'r TextureCreator,
    bitmap: &lodepng::Bitmap<RGBA>,
) -> Texture<'r> {
    let width = bitmap.width;
    let height = bitmap.height;

    let mut surface = Surface::new(
        width as u32,
        height as u32,
        PixelFormat::from(PixelFormatEnum::RGBA8888),
    )
    .expect("Surface::new");
    surface.with_lock_mut(|dest| {
        let mut dest = dest.iter_mut();
        for pixel in &bitmap.buffer {
            *dest.next().unwrap() = pixel.r;
            *dest.next().unwrap() = pixel.g;
            *dest.next().unwrap() = pixel.b;
            *dest.next().unwrap() = pixel.a;
        }
    });

    factory
        .create_texture_from_surface(surface)
        .expect("create_texture_from_surface")
}
