use std::io;
use std::fs::File;
use std::path::Path;
use std::collections::BTreeMap;

use ndarray::Array3;
use png::OutputInfo;

const TEXT: [u8; 4] = [b't', b'E', b'X', b't'];
const ZTXT: [u8; 4] = [b'z', b'T', b'X', b't'];
const VERSION: &str = "4.0";

pub const NORTH: u32 = 1;
pub const SOUTH: u32 = 2;
pub const EAST: u32 = 4;
pub const WEST: u32 = 8;
pub const NORTHEAST: u32 = 5;
pub const NORTHWEST: u32 = 9;
pub const SOUTHEAST: u32 = 6;
pub const SOUTHWEST: u32 = 10;

type Rect = (u32, u32, u32, u32);

// ----------------------------------------------------------------------------
// Icon file and metadata handling

pub struct IconFile {
    pub metadata: Metadata,
    pub image: Image,
}

impl IconFile {
    pub fn from_file(path: &Path) -> io::Result<IconFile> {
        let path = &::utils::fix_case(path);
        Ok(IconFile {
            metadata: Metadata::from_file(path)?,
            image: Image::from_file(path)?,
        })
    }

    pub fn rect_of(&self, icon_state: &str, dir: u32) -> Option<Rect> {
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
        let icon_count = self.image.info.width / self.metadata.width;
        let (icon_x, icon_y) = (icon_index % icon_count, icon_index / icon_count);
        Some((icon_x * self.metadata.width, icon_y * self.metadata.height,
            self.metadata.width, self.metadata.height))
    }
}

pub fn evaluate_dir(dir: &str) -> u32 {
    match dir {
        "SOUTH" | "" => SOUTH,
        "NORTH" => NORTH,
        "EAST" => EAST,
        "WEST" => WEST,
        "SOUTHEAST" => SOUTHEAST,
        "SOUTHWEST" => SOUTHWEST,
        "NORTHEAST" => NORTHEAST,
        "NORTHWEST" => NORTHWEST,
        other => match other.parse::<u32>() {
            Ok(v) => v,
            Err(_) => SOUTH,
        }
    }
}

#[derive(Debug)]
pub struct Metadata {
    pub width: u32,
    pub height: u32,
    pub states: Vec<State>,
    pub state_names: BTreeMap<String, usize>,
}

#[derive(Debug)]
pub struct State {
    /// Frames before this state starts
    pub offset: usize,
    pub name: String,
    /// 0 for infinite, 1+ for finite
    pub loop_: u32,
    pub rewind: bool,
    pub movement: bool,
    pub dirs: Dirs,
    pub frames: Frames,
}

#[derive(Debug, Clone, Copy)]
pub enum Dirs {
    One,
    Four,
    Eight,
}

#[derive(Debug)]
pub enum Frames {
    /// Without an explicit setting, only one frame
    One,
    /// There are this many frames lasting one tick each
    Count(usize),
    /// Each frame lasts the corresponding number of ticks
    Delays(Vec<f32>),
    // TODO: hotspot support here
}

impl Metadata {
    pub fn from_file(path: &Path) -> io::Result<Metadata> {
        let text = read_metadata(path)?;
        Ok(parse_metadata(&text))
    }
}

impl Dirs {
    pub fn len(&self) -> usize {
        match *self {
            Dirs::One => 1,
            Dirs::Four => 4,
            Dirs::Eight => 8,
        }
    }
}

impl Frames {
    pub fn len(&self) -> usize {
        match *self {
            Frames::One => 1,
            Frames::Count(n) => n,
            Frames::Delays(ref v) => v.len(),
        }
    }

    pub fn delay(&self, idx: usize) -> f32 {
        match *self {
            Frames::One => 1.,
            Frames::Count(_) => 1.,
            Frames::Delays(ref v) => v[idx],
        }
    }
}

// ----------------------------------------------------------------------------
// Metadata parser

// Unfortunately the png library is young enough that we have to do this in two
// shots, or reimplement a ton of stuff, if we want to read the metadata *and*
// the image.

pub fn read_metadata(path: &Path) -> io::Result<String> {
    use std::io::Read;
    use png::{StreamingDecoder, Decoded};

    let mut contents = Vec::new();
    File::open(path)?.read_to_end(&mut contents)?;

    let mut slice = &contents[..];
    let mut decoder = StreamingDecoder::new();
    let mut chunk = Vec::new();

    loop {
        let (size, decoded) = decoder.update(slice)?;
        slice = &slice[size..];
        match decoded {
            Decoded::ChunkBegin(_, _) => chunk.clear(),
            Decoded::PartialChunk(TEXT, data) |
            Decoded::PartialChunk(ZTXT, data) => {
                chunk.extend_from_slice(data);
            }
            Decoded::ChunkComplete(_, TEXT) => {
                let first_zero = chunk.iter().position(|&x| x == 0).unwrap();
                if &chunk[..first_zero] == &b"Description"[..] {
                    return Ok(String::from_utf8_lossy(&chunk[first_zero + 1..]).into_owned());
                }
            }
            Decoded::ChunkComplete(_, ZTXT) => {
                let first_zero = chunk.iter().position(|&x| x == 0).unwrap();
                if &chunk[..first_zero] == &b"Description"[..] {
                    let rest = &chunk[first_zero + 2..];
                    let decompressed = ::inflate::inflate_bytes_zlib(rest).unwrap();
                    return Ok(String::from_utf8_lossy(&decompressed).into_owned());
                }
            }
            Decoded::Nothing | Decoded::ImageEnd => return Ok(String::new()),
            _ => {}
        }
    }
}

pub fn parse_metadata(data: &str) -> Metadata {
    if data.is_empty() {
        return Metadata {
            width: 32,
            height: 32,
            states: Vec::new(),
            state_names: BTreeMap::new(),
        };
    }

    let mut lines = data.lines();
    assert_eq!(lines.next().unwrap(), "# BEGIN DMI");
    assert_eq!(lines.next().unwrap(), &format!("version = {}", VERSION));

    let mut metadata = Metadata {
        width: 0,
        height: 0,
        states: Vec::new(),
        state_names: BTreeMap::new(),
    };
    metadata.state_names.insert(String::new(), 0);
    let mut state: Option<State> = None;
    let mut frames_so_far = 0;

    for line in lines {
        if line.starts_with("# END DMI") {
            break
        }
        let mut split = line.trim().splitn(2, " = ");
        let key = split.next().unwrap();
        let value = split.next().unwrap();
        match key {
            "width" => metadata.width = value.parse().unwrap(),
            "height" => metadata.height = value.parse().unwrap(),
            "state" => {
                if let Some(state) = state.take() {
                    frames_so_far += state.frames.len() * state.dirs.len();
                    metadata.states.push(state);
                }
                let unquoted = value[1..value.len() - 1].to_owned(); // TODO: unquote
                assert!(!unquoted.contains("\\") && !unquoted.contains("\""));
                metadata.state_names.insert(unquoted.clone(), metadata.states.len());

                state = Some(State {
                    offset: frames_so_far,
                    name: unquoted,
                    loop_: 0,
                    rewind: false,
                    movement: false,
                    dirs: Dirs::One,
                    frames: Frames::One,
                });
            }
            "dirs" => {
                let state = state.as_mut().unwrap();
                let n: u8 = value.parse().unwrap();
                state.dirs = match n {
                    1 => Dirs::One,
                    4 => Dirs::Four,
                    8 => Dirs::Eight,
                    _ => panic!(),
                };
            }
            "frames" => {
                let state = state.as_mut().unwrap();
                match state.frames {
                    Frames::One => {},
                    _ => panic!(),
                }
                state.frames = Frames::Count(value.parse().unwrap());
            }
            "delay" => {
                let state = state.as_mut().unwrap();
                let mut vector: Vec<f32> = value.split(",").map(str::parse).collect::<Result<Vec<_>, _>>().unwrap();
                match state.frames {
                    Frames::One => if vector.iter().all(|&n| n == 1.) {
                        state.frames = Frames::Count(vector.len());
                    } else {
                        state.frames = Frames::Delays(vector);
                    },
                    Frames::Count(n) => if !vector.iter().all(|&n| n == 1.) {
                        vector.truncate(n);
                        state.frames = Frames::Delays(vector);
                    },
                    Frames::Delays(_) => panic!()
                }
            }
            "loop" => state.as_mut().unwrap().loop_ = value.parse().unwrap(),
            "rewind" => state.as_mut().unwrap().rewind = value.parse::<u8>().unwrap() != 0,
            "hotspot" => { /* TODO */ }
            "movement" => state.as_mut().unwrap().movement = value.parse::<u8>().unwrap() != 0,
            _ => panic!(),
        }
    }
    metadata.states.extend(state);

    metadata
}

// ----------------------------------------------------------------------------
// Image manipulation

pub struct Image {
    pub info: ::png::OutputInfo,
    pub data: Array3<u8>,
}

impl Image {
    pub fn new_rgba(width: u32, height: u32) -> Image {
        let info = OutputInfo {
            width,
            height,
            color_type: ::png::ColorType::RGBA,
            bit_depth: ::png::BitDepth::Eight,
            line_size: width as usize * 4,
        };
        Image {
            data: Array3::zeros((height as usize, width as usize, 4)),
            info,
        }
    }

    pub fn from_file(path: &Path) -> io::Result<Image> {
        use png::{Decoder, BitDepth};

        let (info, mut reader) = Decoder::new(File::open(path)?).read_info()?;
        let mut buffer = vec![0u8; reader.output_buffer_size()];
        reader.next_frame(&mut buffer)?;

        let mut bytes_per_pixel = info.color_type.samples();
        match info.bit_depth {
            BitDepth::One => bytes_per_pixel /= 8,
            BitDepth::Two => bytes_per_pixel /= 4,
            BitDepth::Four => bytes_per_pixel /= 2,
            BitDepth::Eight => {}
            BitDepth::Sixteen => bytes_per_pixel *= 2,
        }

        Ok(Image {
            data: Array3::from_shape_vec((info.height as usize, info.width as usize, bytes_per_pixel), buffer).unwrap(),
            info,
        })
    }

    pub fn to_file(&self, path: &Path) -> io::Result<()> {
        use png::{Encoder, HasParameters};
        flame!("Image::to_file");

        let mut encoder = Encoder::new(File::create(path)?, self.info.width, self.info.height);
        encoder.set(self.info.bit_depth);
        encoder.set(self.info.color_type);
        let mut writer = encoder.write_header()?;
        // TODO: metadata with write_chunk()

        writer.write_image_data(self.data.as_slice().unwrap())?;
        Ok(())
    }

    pub fn composite(&mut self, other: &Image, pos: (u32, u32), crop: Rect, color: [u8; 4]) {
        use ndarray::Axis;
        flame!("Image::composite");

        let mut destination = self.data.slice_mut(s![
            pos.1 as isize .. (pos.1 + crop.3) as isize,
            pos.0 as isize .. (pos.0 + crop.2) as isize,
            ..]);
        let source = other.data.slice(s![
            crop.1 as isize .. (crop.1 + crop.3) as isize,
            crop.0 as isize .. (crop.0 + crop.2) as isize,
            ..]);

        // loop over each [r, g, b, a] available in the relevant area
        for (mut dest, orig_src) in destination.lanes_mut(Axis(2)).into_iter().zip(source.lanes(Axis(2))) {
            macro_rules! tint { ($i:expr) => { mul255(orig_src[$i], color[$i]) } }
            let src = [tint!(0), tint!(1), tint!(2), tint!(3)];

            // out_A = src_A + dst_A (1 - src_A)
            // out_RGB = (src_RGB src_A + dst_RGB dst_A (1 - src_A)) / out_A
            let out_a = src[3] + mul255(dest[3], 255 - src[3]);
            if out_a != 0 {
                for i in 0..3 {
                    dest[i] = ((src[i] as u32 * src[3] as u32 + dest[i] as u32 * dest[3] as u32 * (255 - src[3] as u32) / 255) / out_a as u32) as u8;
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

// ----------------------------------------------------------------------------
// Tests

#[cfg(test)]
mod test {
    extern crate walkdir;
    use self::walkdir::{DirEntry, WalkDir, WalkDirIterator};
    use super::*;

    #[test]
    fn parse_all_dmi() {
        fn is_visible(entry: &DirEntry) -> bool {
            entry.path()
                .file_name()
                .unwrap_or("".as_ref())
                .to_str()
                .map(|s| !s.starts_with("."))
                .unwrap_or(true)
        }

        for entry in WalkDir::new("../../")
            .into_iter()
            .filter_entry(is_visible)
        {
            let entry = entry.unwrap();
            if entry.file_type().is_file() && entry.path().extension() == Some("dmi".as_ref()) {
                println!("{:?}", entry.path());
                let md = read_metadata(entry.path()).unwrap();
                println!("{}", md);
                parse_metadata(&md);
            }
        }
    }
}
