use std::io;
use std::path::Path;
use std::collections::BTreeMap;

use ndarray::Array3;
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

pub struct IconFile {
    pub metadata: Metadata,
    pub image: Image,
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
            image: Image::from_rgba(bitmap),
        })
    }

    pub fn rect_of(&self, icon_state: &str, dir: i32) -> Option<Rect> {
        let state_index = match self.metadata.state_names.get(icon_state) {
            Some(&i) => i,
            None if icon_state == "" => 0,
            None => return None,
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
        let icon_count = self.image.width / self.metadata.width;
        let (icon_x, icon_y) = (icon_index % icon_count, icon_index / icon_count);
        Some((
            icon_x * self.metadata.width,
            icon_y * self.metadata.height,
            self.metadata.width,
            self.metadata.height,
        ))
    }
}

// ----------------------------------------------------------------------------
// Image manipulation

pub struct Image {
    pub width: u32,
    pub height: u32,
    pub data: Array3<u8>,
}

impl Image {
    pub fn new_rgba(width: u32, height: u32) -> Image {
        Image {
            width,
            height,
            data: Array3::zeros((height as usize, width as usize, 4)),
        }
    }

    fn from_rgba(bitmap: lodepng::Bitmap<RGBA>) -> Image {
        Image {
            width: bitmap.width as u32,
            height: bitmap.height as u32,
            data: Array3::from_shape_fn((bitmap.height, bitmap.width, 4), |(y, x, c)| {
                let rgba = bitmap.buffer[y * bitmap.width + x];
                match c {
                    0 => rgba.r,
                    1 => rgba.g,
                    2 => rgba.b,
                    3 => rgba.a,
                    _ => unreachable!(),
                }
            }),
        }
    }

    /// Read an `Image` from a file.
    ///
    /// Prefer to call `IconFile::from_file`, which can read both metadata and
    /// image contents at one time.
    pub fn from_file(path: &Path) -> io::Result<Image> {
        let path = &::dm::fix_case(path);
        let mut decoder = PngState::new();
        decoder.info_raw.colortype = ColorType::RGBA;
        decoder.info_raw.set_bitdepth(8);
        decoder.read_text_chunks(false);
        decoder.remember_unknown_chunks(false);
        let bitmap = match decoder.decode_file(path) {
            Ok(::lodepng::Image::RGBA(bitmap)) => bitmap,
            Ok(_) => return Err(io::Error::new(io::ErrorKind::InvalidData, "not RGBA")),
            Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidData, e)),
        };

        Ok(Image::from_rgba(bitmap))
    }

    #[cfg(feature="png")]
    pub fn to_file(&self, path: &Path) -> io::Result<()> {
        use std::fs::File;
        use png::{Encoder, HasParameters};

        let mut encoder = Encoder::new(File::create(path)?, self.width, self.height);
        encoder.set(::png::ColorType::RGBA);
        encoder.set(::png::BitDepth::Eight);
        let mut writer = encoder.write_header()?;
        // TODO: metadata with write_chunk()

        writer.write_image_data(self.data.as_slice().unwrap())?;
        Ok(())
    }

    pub fn composite(&mut self, other: &Image, pos: (u32, u32), crop: Rect, color: [u8; 4]) {
        use ndarray::Axis;

        let mut destination = self.data.slice_mut(s![
            pos.1 as isize..(pos.1 + crop.3) as isize,
            pos.0 as isize..(pos.0 + crop.2) as isize,
            ..
        ]);
        let source = other.data.slice(s![
            crop.1 as isize..(crop.1 + crop.3) as isize,
            crop.0 as isize..(crop.0 + crop.2) as isize,
            ..
        ]);

        // loop over each [r, g, b, a] available in the relevant area
        for (mut dest, orig_src) in destination.lanes_mut(Axis(2)).into_iter().zip(source.lanes(Axis(2))) {
            macro_rules! tint {
                ($i:expr) => {
                    mul255(
                        *orig_src.get($i).unwrap_or(&255),
                        *color.get($i).unwrap_or(&255),
                    )
                };
            }
            let src = [tint!(0), tint!(1), tint!(2), tint!(3)];

            // out_A = src_A + dst_A (1 - src_A)
            // out_RGB = (src_RGB src_A + dst_RGB dst_A (1 - src_A)) / out_A
            let out_a = src[3] + mul255(dest[3], 255 - src[3]);
            if out_a != 0 {
                for i in 0..3 {
                    dest[i] = ((src[i] as u32 * src[3] as u32
                        + dest[i] as u32 * dest[3] as u32 * (255 - src[3] as u32) / 255)
                        / out_a as u32) as u8;
                }
            } else {
                for i in 0..3 {
                    dest[i] = 0;
                }
            }
            dest[3] = out_a as u8;
        }
    }
}

#[inline]
fn mul255(x: u8, y: u8) -> u8 {
    (x as u16 * y as u16 / 255) as u8
}
