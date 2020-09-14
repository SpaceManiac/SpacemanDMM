//! DMI metadata and image composition.
//!
//! Includes re-exports from `dreammaker::dmi`.

use std::io;
use std::path::Path;

use ndarray::Array3;
use lodepng::{self, RGBA, Decoder, ColorType};

pub use dm::dmi::*;

type Rect = (u32, u32, u32, u32);

// ----------------------------------------------------------------------------
// Icon file and metadata handling

/// An image with associated DMI metadata.
pub struct IconFile {
    /// The icon's metadata.
    pub metadata: Metadata,
    /// The icon's image.
    pub image: Image,
}

impl IconFile {
    pub fn from_file(path: &Path) -> io::Result<IconFile> {
        let (bitmap, metadata) = Metadata::from_file(path)?;
        Ok(IconFile {
            metadata,
            image: Image::from_rgba(bitmap),
        })
    }

    #[inline]
    pub fn rect_of(&self, icon_state: &str, dir: Dir) -> Option<Rect> {
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
}

// ----------------------------------------------------------------------------
// Image manipulation

/// A two-dimensional RGBA image.
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
        let mut decoder = Decoder::new();
        decoder.info_raw_mut().colortype = ColorType::RGBA;
        decoder.info_raw_mut().set_bitdepth(8);
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

        let mut encoder = png::Encoder::new(File::create(path)?, self.width, self.height);
        encoder.set_color(::png::ColorType::RGBA);
        encoder.set_depth(::png::BitDepth::Eight);
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
