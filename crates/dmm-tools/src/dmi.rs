//! DMI metadata and image composition.
//!
//! Includes re-exports from `dreammaker::dmi`.

use bytemuck::Pod;
use std::io;
use std::path::Path;

use lodepng::{self, ColorType, Decoder, RGBA};
use ndarray::Array2;

pub use dm::dmi::*;
use std::ops::{Index, IndexMut};

/// Absolute x and y.
pub type Coordinate = (u32, u32);
/// Start x, Start y, End x, End y - relative to Coordinate.
pub type Rect = (u32, u32, u32, u32);

// ----------------------------------------------------------------------------
// Icon file and metadata handling

#[cfg(all(feature = "png", feature = "gif"))]
pub mod render;

/// An image with associated DMI metadata.
#[derive(Debug)]
pub struct IconFile {
    /// The icon's metadata.
    pub metadata: Metadata,
    /// The icon's image.
    pub image: Image,
}

impl IconFile {
    pub fn from_file(path: &Path) -> io::Result<IconFile> {
        Self::from_bytes(&std::fs::read(path)?)
    }

    pub fn from_bytes(data: &[u8]) -> io::Result<IconFile> {
        let (bitmap, metadata) = Metadata::from_bytes(data)?;
        Ok(IconFile {
            metadata,
            image: Image::from_rgba(bitmap),
        })
    }

    #[inline]
    pub fn rect_of(&self, icon_state: &StateIndex, dir: Dir) -> Option<Rect> {
        self.metadata.rect_of(self.image.width, icon_state, dir, 0)
    }

    pub fn rect_of_index(&self, icon_index: u32) -> Rect {
        let icon_count = self.image.width / self.metadata.width;
        let (icon_x, icon_y) = (icon_index % icon_count, icon_index / icon_count);
        (
            icon_x * self.metadata.width,
            icon_y * self.metadata.height,
            self.metadata.width,
            self.metadata.height,
        )
    }

    pub fn get_icon_state(&self, icon_state: &StateIndex) -> io::Result<&State> {
        self.metadata.get_icon_state(icon_state).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("icon_state {} not found", icon_state),
            )
        })
    }
}

#[derive(Default, Debug, Clone, Copy, Pod, Zeroable, Eq, PartialEq)]
#[repr(C)]
pub struct Rgba8 {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Rgba8 {
    pub fn new(r: u8, g: u8, b: u8, a: u8) -> Rgba8 {
        Rgba8 { r, g, b, a }
    }

    pub fn as_bytes(&self) -> &[u8; 4] {
        bytemuck::cast_ref(self)
    }

    pub fn as_bytes_mut(&mut self) -> &mut [u8; 4] {
        bytemuck::cast_mut(self)
    }
}

impl Index<u8> for Rgba8 {
    type Output = u8;

    fn index(&self, index: u8) -> &Self::Output {
        &self.as_bytes()[index as usize]
    }
}

impl IndexMut<u8> for Rgba8 {
    fn index_mut(&mut self, index: u8) -> &mut Self::Output {
        &mut self.as_bytes_mut()[index as usize]
    }
}

// ----------------------------------------------------------------------------
// Image manipulation

/// A two-dimensional RGBA image.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Image {
    pub width: u32,
    pub height: u32,
    pub data: Array2<Rgba8>,
}

impl Image {
    pub fn new_rgba(width: u32, height: u32) -> Image {
        Image {
            width,
            height,
            data: { Array2::default((width as usize, height as usize)) },
        }
    }

    fn from_rgba(bitmap: lodepng::Bitmap<RGBA>) -> Image {
        Image {
            width: bitmap.width as u32,
            height: bitmap.height as u32,
            data: {
                let cast_input = bytemuck::cast_slice(bitmap.buffer.as_slice());
                let mut arr = Array2::default((bitmap.width, bitmap.height));
                arr.as_slice_mut().unwrap().copy_from_slice(cast_input);
                arr
            },
        }
    }

    /// Read an `Image` from a [u8] array.
    ///
    /// Prefer to call `IconFile::from_bytes`, which can read both metadata and
    /// image contents at one time.
    pub fn from_bytes(data: &[u8]) -> io::Result<Image> {
        let mut decoder = Decoder::new();
        decoder.info_raw_mut().colortype = ColorType::RGBA;
        decoder.info_raw_mut().set_bitdepth(8);
        decoder.read_text_chunks(false);
        decoder.remember_unknown_chunks(false);
        let bitmap = match decoder.decode(data) {
            Ok(::lodepng::Image::RGBA(bitmap)) => bitmap,
            Ok(_) => return Err(io::Error::new(io::ErrorKind::InvalidData, "not RGBA")),
            Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidData, e)),
        };

        Ok(Image::from_rgba(bitmap))
    }

    /// Read an `Image` from a file.
    ///
    /// Prefer to call `IconFile::from_file`, which can read both metadata and
    /// image contents at one time.
    pub fn from_file(path: &Path) -> io::Result<Image> {
        let path = &::dm::fix_case(path);
        Self::from_bytes(&std::fs::read(path)?)
    }

    pub fn clear(&mut self) {
        self.data.fill(Default::default())
    }

    #[cfg(feature = "png")]
    pub fn to_write<W: std::io::Write>(&self, writer: W) -> io::Result<()> {
        {
            let mut encoder = png::Encoder::new(writer, self.width, self.height);
            encoder.set_color(::png::ColorType::Rgba);
            encoder.set_depth(::png::BitDepth::Eight);
            let mut writer = encoder.write_header()?;
            // TODO: metadata with write_chunk()
            writer.write_image_data(bytemuck::cast_slice(self.data.as_slice().unwrap()))?;
        }
        Ok(())
    }

    #[cfg(feature = "png")]
    pub fn to_file(&self, path: &Path) -> io::Result<()> {
        self.to_write(std::fs::File::create(path)?)
    }

    #[cfg(feature = "png")]
    pub fn to_bytes(&self) -> io::Result<Vec<u8>> {
        let mut vector = Vec::new();
        self.to_write(&mut vector)?;
        Ok(vector)
    }

    pub fn composite(&mut self, other: &Image, pos: Coordinate, crop: Rect, color: [u8; 4]) {
        let other_dat = other.data.as_slice().unwrap();
        let self_dat = self.data.as_slice_mut().unwrap();
        let mut sy = crop.1;
        for y in pos.1..(pos.1 + crop.3) {
            let mut sx = crop.0;
            for x in pos.0..(pos.0 + crop.2) {
                let src = other_dat[(sy * other.width + sx) as usize];
                macro_rules! tint {
                    ($i:expr) => {
                        mul255(src[$i], color[$i])
                    };
                }
                let mut dst = &mut self_dat[(y * self.width + x) as usize];
                let src_tint = Rgba8::new(tint!(0), tint!(1), tint!(2), tint!(3));

                // out_A = src_A + dst_A (1 - src_A)
                // out_RGB = (src_RGB src_A + dst_RGB dst_A (1 - src_A)) / out_A
                let out_a = src_tint.a + mul255(dst.a, 255 - src_tint.a);
                if out_a != 0 {
                    for i in 0..3 {
                        dst[i] = ((src_tint[i] as u32 * src_tint.a as u32
                            + dst[i] as u32 * dst.a as u32 * (255 - src_tint.a as u32) / 255)
                            / out_a as u32) as u8;
                    }
                } else {
                    for i in 0..3 {
                        dst[i] = 0;
                    }
                }
                dst.a = out_a as u8;

                sx += 1;
            }

            sy += 1;
        }
    }
}

#[inline]
fn mul255(x: u8, y: u8) -> u8 {
    (x as u16 * y as u16 / 255) as u8
}
