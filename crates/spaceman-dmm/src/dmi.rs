//! Editor-environment specific DMI (texture) handling.

use lodepng::{self, ColorType, Decoder, RGBA};
use sdl3::gpu::{
    Device, Texture, TextureCreateInfo, TextureRegion, TextureTransferInfo, TextureUsage,
    TransferBufferUsage,
};
use std::collections::hash_map::HashMap;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

type Rect = (u32, u32, u32, u32);

pub use dreammaker::dmi::*;

// ----------------------------------------------------------------------------
// Icon file and metadata handling

pub struct IconCache {
    base_path: PathBuf,
    lock: RwLock<IconCacheInner>,
}

#[derive(Default)]
pub struct TextureCache {
    textures: Vec<Option<Texture<'static>>>,
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

impl TextureCache {
    pub fn retrieve(&mut self, device: &Device, icons: &IconCache, id: usize) -> &Texture {
        if id >= self.textures.len() {
            self.textures.resize_with(id + 1, Default::default);
        }
        self.textures[id].get_or_insert_with(|| load_texture(device, &icons.get_icon(id).bitmap))
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

pub fn texture_from_bytes<'r>(device: &Device, bytes: &[u8]) -> io::Result<Texture<'r>> {
    let mut decoder = Decoder::new();
    decoder.info_raw_mut().colortype = ColorType::RGBA;
    decoder.info_raw_mut().set_bitdepth(8);
    decoder.remember_unknown_chunks(false);
    let bitmap = match decoder.decode(bytes) {
        Ok(::lodepng::Image::RGBA(bitmap)) => bitmap,
        Ok(_) => return Err(io::Error::new(io::ErrorKind::InvalidData, "not RGBA")),
        Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidData, e)),
    };
    Ok(load_texture(device, &bitmap))
}

pub fn load_texture<'r>(device: &Device, bitmap: &lodepng::Bitmap<RGBA>) -> Texture<'static> {
    let width = bitmap.width as u32;
    let height = bitmap.height as u32;

    let texture = device
        .create_texture(
            TextureCreateInfo::new()
                .with_type(sdl3::gpu::TextureType::_2D)
                .with_format(sdl3::gpu::TextureFormat::R8g8b8a8Unorm)
                .with_usage(TextureUsage::SAMPLER)
                .with_width(width)
                .with_height(height)
                .with_layer_count_or_depth(1)
                .with_num_levels(1),
        )
        .expect("create_texture");

    let transfer_buffer = device
        .create_transfer_buffer()
        .with_usage(TransferBufferUsage::UPLOAD)
        .with_size(width * height * 4)
        .build()
        .expect("create_transfer_buffer");

    let mut mem = transfer_buffer.map::<u8>(device, true);
    let mut dest = mem.mem_mut();
    for pixel in &bitmap.buffer {
        dest.write_all(&[pixel.a, pixel.b, pixel.g, pixel.r]);
    }
    mem.unmap();

    let source = TextureTransferInfo::new()
        .with_transfer_buffer(&transfer_buffer)
        .with_offset(0);
    let destination = TextureRegion::new()
        .with_texture(&texture)
        .with_width(width)
        .with_height(height)
        .with_depth(1);

    let command_buffer = device
        .acquire_command_buffer()
        .expect("acquire_command_buffer");
    let copy_pass = device
        .begin_copy_pass(&command_buffer)
        .expect("begin_copy_pass");

    copy_pass.upload_to_gpu_texture(source, destination, false);
    device.end_copy_pass(copy_pass);
    command_buffer.submit().expect("CommandBuffer::submit");

    texture
}
