use std::io;
use std::path::Path;
use std::collections::BTreeMap;

use lodepng;
use lodepng::ffi::{State as PngState};

const VERSION: &str = "4.0";

pub const NORTH: i32 = 1;
pub const SOUTH: i32 = 2;
pub const EAST: i32 = 4;
pub const WEST: i32 = 8;
pub const NORTHEAST: i32 = 5;
pub const NORTHWEST: i32 = 9;
pub const SOUTHEAST: i32 = 6;
pub const SOUTHWEST: i32 = 10;

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
    /// Read the metadata from a given file.
    ///
    /// Prefer to call `IconFile::from_file`, which can read both metadata and
    /// image contents at one time.
    pub fn from_file(path: &Path) -> io::Result<Metadata> {
        let text = read_metadata(path)?;
        Ok(parse_metadata(&text))
    }

    /// Parse metadata from a `Description` string.
    #[inline]
    pub fn from_str(data: &str) -> Metadata {
        parse_metadata(data)
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

fn read_metadata(path: &Path) -> io::Result<String> {
    let path = &::fix_case(path);
    let mut decoder = PngState::new();
    decoder.remember_unknown_chunks(false);
    match decoder.decode_file(path) {
        Ok(_) => {}
        Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidData, e)),
    }

    for (key, value) in decoder.info_png().text_keys_cstr() {
        if key.to_str() == Ok("Description") {
            if let Ok(value) = value.to_str() {
                return Ok(value.to_owned());
            }
        }
    }

    Ok(String::new())
}

fn parse_metadata(data: &str) -> Metadata {
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
