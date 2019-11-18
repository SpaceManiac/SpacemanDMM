//! DMI metadata parsing and representation.

use std::io;
use std::path::Path;
use std::collections::BTreeMap;

use lodepng::ffi::{State as PngState};

const VERSION: &str = "4.0";

/// The two-dimensional facing subset of BYOND's direction type.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Dir {
    North = 1,
    South = 2,
    East = 4,
    West = 8,
    Northeast = 5,
    Northwest = 9,
    Southeast = 6,
    Southwest = 10,
}

impl Dir {
    pub const CARDINALS: &'static [Dir] = &[Dir::North, Dir::South, Dir::East, Dir::West];
    pub const DIAGONALS: &'static [Dir] = &[Dir::Northeast, Dir::Northwest, Dir::Southeast, Dir::Southwest];
    pub const ALL: &'static [Dir] = &[Dir::North, Dir::South, Dir::East, Dir::West, Dir::Northeast, Dir::Northwest, Dir::Southeast, Dir::Southwest];

    /// Attempt to build a direction from its integer representation.
    pub fn from_int(int: i32) -> Option<Dir> {
        Some(match int {
            1 => Dir::North,
            2 => Dir::South,
            4 => Dir::East,
            8 => Dir::West,
            5 => Dir::Northeast,
            9 => Dir::Northwest,
            6 => Dir::Southeast,
            10 => Dir::Southwest,
            _ => return None,
        })
    }

    /// Get this direction's integer representation.
    pub fn to_int(self) -> i32 {
        self as i32
    }

    pub fn contains(self, other: Dir) -> bool {
        self.to_int() & other.to_int() != 0
    }

    pub fn is_diagonal(self) -> bool {
        match self {
            Dir::North |
            Dir::South |
            Dir::East |
            Dir::West => false,
            _ => true
        }
    }

    pub fn flip(self) -> Dir {
        match self {
            Dir::North => Dir::South,
            Dir::South => Dir::North,
            Dir::East => Dir::West,
            Dir::West => Dir::East,
            Dir::Northeast => Dir::Southwest,
            Dir::Northwest => Dir::Southeast,
            Dir::Southeast => Dir::Northwest,
            Dir::Southwest => Dir::Northeast,
        }
    }

    pub fn flip_ns(self) -> Dir {
        match self {
            Dir::North => Dir::South,
            Dir::South => Dir::North,
            Dir::East => Dir::East,
            Dir::West => Dir::West,
            Dir::Northeast => Dir::Southeast,
            Dir::Northwest => Dir::Southwest,
            Dir::Southeast => Dir::Northeast,
            Dir::Southwest => Dir::Northwest,
        }
    }

    pub fn flip_ew(self) -> Dir {
        match self {
            Dir::North => Dir::North,
            Dir::South => Dir::South,
            Dir::East => Dir::West,
            Dir::West => Dir::East,
            Dir::Northeast => Dir::Northwest,
            Dir::Northwest => Dir::Northeast,
            Dir::Southeast => Dir::Southwest,
            Dir::Southwest => Dir::Southeast,
        }
    }

    pub fn clockwise_45(self) -> Dir {
        match self {
            Dir::North => Dir::Northeast,
            Dir::Northeast => Dir::East,
            Dir::East => Dir::Southeast,
            Dir::Southeast => Dir::South,
            Dir::South => Dir::Southwest,
            Dir::Southwest => Dir::West,
            Dir::West => Dir::Northwest,
            Dir::Northwest => Dir::North,
        }
    }

    pub fn counterclockwise_45(self) -> Dir {
        match self {
            Dir::North => Dir::Northwest,
            Dir::Northeast => Dir::North,
            Dir::East => Dir::Northeast,
            Dir::Southeast => Dir::East,
            Dir::South => Dir::Southeast,
            Dir::Southwest => Dir::South,
            Dir::West => Dir::Southwest,
            Dir::Northwest => Dir::West,
        }
    }

    pub fn clockwise_90(self) -> Dir {
        match self {
            Dir::North => Dir::East,
            Dir::South => Dir::West,
            Dir::East => Dir::South,
            Dir::West => Dir::North,
            Dir::Northeast => Dir::Southeast,
            Dir::Northwest => Dir::Northeast,
            Dir::Southeast => Dir::Southwest,
            Dir::Southwest => Dir::Northeast,
        }
    }

    pub fn counterclockwise_90(self) -> Dir {
        match self {
            Dir::North => Dir::West,
            Dir::South => Dir::East,
            Dir::East => Dir::North,
            Dir::West => Dir::South,
            Dir::Southeast => Dir::Northeast,
            Dir::Northeast => Dir::Northwest,
            Dir::Southwest => Dir::Southeast,
            Dir::Northwest => Dir::Southwest,
        }
    }

    /// Get this direction's offset in BYOND's coordinate system.
    pub fn offset(self) -> (i32, i32) {
        match self {
            Dir::North => (0, 1),
            Dir::South => (0, -1),
            Dir::East => (1, 0),
            Dir::West => (-1, 0),
            Dir::Northeast => (1, 1),
            Dir::Northwest => (-1, 1),
            Dir::Southeast => (1, -1),
            Dir::Southwest => (-1, -1),
        }
    }
}

impl Default for Dir {
    fn default() -> Self {
        Dir::South
    }
}

/// Embedded metadata describing a DMI spritesheet's layout.
#[derive(Debug)]
pub struct Metadata {
    /// The width of the icon in pixels.
    pub width: u32,
    /// The height of the icon in pixels.
    pub height: u32,
    /// The list of states in the order they appear in the spritesheet.
    pub states: Vec<State>,
    /// A lookup table from state name to its position in `states`.
    pub state_names: BTreeMap<String, usize>,
}

/// The metadata belonging to a single icon state.
#[derive(Debug)]
pub struct State {
    /// The state's name, corresponding to the `icon_state` var.
    pub name: String,
    /// Whether this is a movement state (shown during gliding).
    pub movement: bool,
    /// The number of frames in the spritesheet before this state's first frame.
    pub offset: usize,
    /// 0 for infinite, 1+ for finite.
    pub loop_: u32,
    pub rewind: bool,
    pub dirs: Dirs,
    pub frames: Frames,
}

/// How many directions a state has.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Dirs {
    One,
    Four,
    Eight,
}

/// How many frames of animation a state has, and their durations.
#[derive(Debug, PartialEq)]
pub enum Frames {
    /// Without an explicit setting, only one frame.
    One,
    /// There are this many frames lasting one tick each.
    Count(usize),
    /// Each frame lasts the corresponding number of ticks.
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

impl State {
    pub fn num_sprites(&self) -> usize {
        self.dirs.len() * self.frames.len()
    }
}

impl Dirs {
    pub fn len(self) -> usize {
        match self {
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
    let mut metadata = Metadata {
        width: 32,
        height: 32,
        states: Vec::new(),
        state_names: BTreeMap::new(),
    };
    if data.is_empty() {
        return metadata;
    }

    let mut lines = data.lines();
    assert_eq!(lines.next().unwrap(), "# BEGIN DMI");
    assert_eq!(lines.next().unwrap(), &format!("version = {}", VERSION));

    let mut state: Option<State> = None;
    let mut frames_so_far = 0;

    for line in lines {
        if line.starts_with("# END DMI") {
            break;
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
                assert!(!unquoted.contains('\\') && !unquoted.contains('"'));
                if !metadata.state_names.contains_key(&unquoted) {
                    metadata.state_names.insert(unquoted.clone(), metadata.states.len());
                }

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
                let mut vector: Vec<f32> = value.split(',').map(str::parse).collect::<Result<Vec<_>, _>>().unwrap();
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
                    Frames::Delays(_) => panic!(),
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
