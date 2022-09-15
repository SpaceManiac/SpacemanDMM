//! DMI metadata parsing and representation.

use std::collections::{BTreeMap, HashMap};
use std::fmt::Display;
use std::io;
use std::path::Path;

use derivative::Derivative;
use lodepng::Decoder;

const EXPECTED_VERSION_LINE: &str = "version = 4.0";

/// Index into the state name table
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct StateIndex(String, u32);

impl Display for StateIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.1 == 0 {
            write!(f, "{}", self.0)
        } else {
            write!(f, "{} ({})", self.0, self.1)
        }
    }
}

impl From<String> for StateIndex {
    fn from(s: String) -> Self {
        StateIndex(s, 0)
    }
}

impl From<&str> for StateIndex {
    fn from(s: &str) -> Self {
        StateIndex(s.to_owned(), 0)
    }
}

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
    pub const DIAGONALS: &'static [Dir] = &[
        Dir::Northeast,
        Dir::Northwest,
        Dir::Southeast,
        Dir::Southwest,
    ];
    pub const ALL: &'static [Dir] = &[
        Dir::North,
        Dir::South,
        Dir::East,
        Dir::West,
        Dir::Northeast,
        Dir::Northwest,
        Dir::Southeast,
        Dir::Southwest,
    ];

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
        !matches!(self, Dir::North | Dir::South | Dir::East | Dir::West)
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
#[derive(Debug, Clone, PartialEq)]
pub struct Metadata {
    /// The width of the icon in pixels.
    pub width: u32,
    /// The height of the icon in pixels.
    pub height: u32,
    /// The list of states in the order they appear in the spritesheet.
    pub states: Vec<State>,
    /// A lookup table from state name to its position in `states`.
    pub state_names: BTreeMap<StateIndex, usize>,
}

/// The metadata belonging to a single icon state.
#[derive(Derivative, Debug, Clone)]
#[derivative(PartialEq)]
pub struct State {
    /// The state's name, corresponding to the `icon_state` var.
    pub name: String,
    /// Whether this is a movement state (shown during gliding).
    pub movement: bool,
    /// The number of frames in the spritesheet before this state's first frame.
    #[derivative(PartialEq = "ignore")]
    pub offset: usize,
    /// 0 for infinite, 1+ for finite.
    pub loop_: u32,
    /// The number of `State`s before this with the same name.
    pub duplicate_index: u32,
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
#[derive(Debug, Clone, PartialEq)]
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
    /// Read the bitmap and DMI metadata from a given file in a single pass.
    pub fn from_file(path: &Path) -> io::Result<(lodepng::Bitmap<lodepng::RGBA>, Metadata)> {
        let path = &crate::fix_case(path);
        Self::from_bytes(&std::fs::read(path)?)
    }

    /// Read a u8 array (raw data of a file) as a DMI into a bitmap and metadata
    pub fn from_bytes(data: &[u8]) -> io::Result<(lodepng::Bitmap<lodepng::RGBA>, Metadata)> {
        let mut decoder = Decoder::new();
        decoder.info_raw_mut().colortype = lodepng::ColorType::RGBA;
        decoder.info_raw_mut().set_bitdepth(8);
        decoder.remember_unknown_chunks(false);
        let bitmap = match decoder.decode(data) {
            Ok(::lodepng::Image::RGBA(bitmap)) => bitmap,
            Ok(_) => return Err(io::Error::new(io::ErrorKind::InvalidData, "not RGBA")),
            Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidData, e)),
        };

        let metadata = Metadata::from_decoder(bitmap.width as u32, bitmap.height as u32, &decoder)?;
        Ok((bitmap, metadata))
    }

    fn from_decoder(width: u32, height: u32, decoder: &Decoder) -> io::Result<Metadata> {
        for (key, value) in decoder.info_png().text_keys() {
            if key == b"Description" {
                if let Ok(value) = std::str::from_utf8(value) {
                    return Metadata::meta_from_str(value);
                }
                break;
            }
        }
        Ok(Metadata {
            width,
            height,
            states: Default::default(),
            state_names: Default::default(),
        })
    }

    /// Parse metadata from a `Description` string.
    #[inline]
    pub fn meta_from_str(data: &str) -> io::Result<Metadata> {
        parse_metadata(data)
    }

    pub fn rect_of(
        &self,
        bitmap_width: u32,
        icon_state: &StateIndex,
        dir: Dir,
        frame: u32,
    ) -> Option<(u32, u32, u32, u32)> {
        if self.states.is_empty() {
            return Some((0, 0, self.width, self.height));
        }
        let state = self.get_icon_state(icon_state)?;
        let icon_index = state.index_of_frame(dir, frame);

        let icon_count = bitmap_width / self.width;
        let (icon_x, icon_y) = (icon_index % icon_count, icon_index / icon_count);
        Some((
            icon_x * self.width,
            icon_y * self.height,
            self.width,
            self.height,
        ))
    }

    pub fn get_icon_state(&self, icon_state: &StateIndex) -> Option<&State> {
        let state_index = match self.state_names.get(icon_state) {
            Some(&i) => i,
            None => return None,
        };
        Some(&self.states[state_index])
    }
}

impl State {
    pub fn is_animated(&self) -> bool {
        match self.frames {
            Frames::One | Frames::Count(1) => false,
            Frames::Count(_) => true,
            Frames::Delays(_) => true,
        }
    }

    pub fn num_sprites(&self) -> usize {
        self.dirs.count() * self.frames.count()
    }

    pub fn index_of_dir(&self, dir: Dir) -> u32 {
        let dir_idx = match (self.dirs, dir) {
            (Dirs::One, _) => 0,
            (Dirs::Eight, Dir::Northwest) => 7,
            (Dirs::Eight, Dir::Northeast) => 6,
            (Dirs::Eight, Dir::Southwest) => 5,
            (Dirs::Eight, Dir::Southeast) => 4,
            (_, Dir::West) => 3,
            (_, Dir::East) => 2,
            (_, Dir::North) => 1,
            (_, _) => 0,
        };

        self.offset as u32 + dir_idx
    }

    #[inline]
    pub fn index_of_frame(&self, dir: Dir, frame: u32) -> u32 {
        self.index_of_dir(dir) + frame * self.dirs.count() as u32
    }

    pub fn get_state_name_index(&self) -> StateIndex {
        StateIndex(self.name.clone(), self.duplicate_index)
    }
}

impl Dirs {
    pub fn count(self) -> usize {
        match self {
            Dirs::One => 1,
            Dirs::Four => 4,
            Dirs::Eight => 8,
        }
    }
}

impl Frames {
    pub fn count(&self) -> usize {
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

fn parse_metadata(data: &str) -> io::Result<Metadata> {
    let mut metadata = Metadata {
        width: 32,
        height: 32,
        states: Vec::new(),
        state_names: BTreeMap::new(),
    };
    if data.is_empty() {
        return Ok(metadata);
    }

    let mut lines = data.lines();
    let header = (lines.next(), lines.next());
    let expected_header = (Some("# BEGIN DMI"), Some(EXPECTED_VERSION_LINE));
    if header != expected_header {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "Wrong dmi metadata header. Expected {:?}, got {:?}",
                expected_header, header
            ),
        ));
    }

    let mut state: Option<State> = None;
    let mut frames_so_far = 0;

    let mut duplicate_map: HashMap<String, u32> = HashMap::new();

    for line in lines {
        if line.starts_with("# END DMI") {
            break;
        }
        let (key, value) = line.trim().split_once(" = ").unwrap();
        match key {
            "width" => metadata.width = value.parse().unwrap(),
            "height" => metadata.height = value.parse().unwrap(),
            "state" => {
                if let Some(state) = state.take() {
                    frames_so_far += state.frames.count() * state.dirs.count();
                    metadata.states.push(state);
                }
                let unquoted = value[1..value.len() - 1].to_owned(); // TODO: unquote
                assert!(!unquoted.contains('\\') && !unquoted.contains('"'));

                let count = duplicate_map.entry(unquoted.clone()).or_insert(0);

                let new_state = State {
                    offset: frames_so_far,
                    name: unquoted,
                    loop_: 0,
                    duplicate_index: *count,
                    rewind: false,
                    movement: false,
                    dirs: Dirs::One,
                    frames: Frames::One,
                };

                let key = new_state.get_state_name_index();

                if let std::collections::btree_map::Entry::Vacant(e) =
                    metadata.state_names.entry(key)
                {
                    e.insert(metadata.states.len());
                }

                state = Some(new_state);

                *count += 1;
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
                    Frames::One => {}
                    _ => panic!(),
                }
                state.frames = Frames::Count(value.parse().unwrap());
            }
            "delay" => {
                let state = state.as_mut().unwrap();
                let mut vector: Vec<f32> = value
                    .split(',')
                    .map(str::parse)
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                match state.frames {
                    Frames::One => {
                        if vector.iter().all(|&n| n == 1.) {
                            state.frames = Frames::Count(vector.len());
                        } else {
                            state.frames = Frames::Delays(vector);
                        }
                    }
                    Frames::Count(n) => {
                        vector.truncate(n);
                        if !vector.iter().all(|&n| n == 1.) {
                            state.frames = Frames::Delays(vector);
                        }
                    }
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

    Ok(metadata)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn duplicate_states() {
        let description = r##"
# BEGIN DMI
version = 4.0
    width = 32
    height = 32
state = "duplicate"
    dirs = 1
    frames = 1
state = "duplicate"
    dirs = 1
    frames = 1
state = "duplicate"
    dirs = 1
    frames = 1
# END DMI
"##
        .trim();

        let metadata = parse_metadata(description).expect("Metadata is valid");
        assert_eq!(metadata.state_names.len(), 3);
        assert_eq!(
            metadata.state_names,
            BTreeMap::from([
                (StateIndex("duplicate".to_owned(), 0), 0),
                (StateIndex("duplicate".to_owned(), 1), 1),
                (StateIndex("duplicate".to_owned(), 2), 2)
            ])
        );
        assert_eq!(metadata.states.len(), 3);

        for (no, state) in metadata.states.iter().enumerate() {
            if no == 0 {
                assert_eq!(state.duplicate_index, 0)
            } else {
                assert_eq!(state.duplicate_index, no as u32);
            }

            // Note: using `no` here only works by virtue of the test data being only composed of duplicates
            assert_eq!(
                no,
                *metadata
                    .state_names
                    .get(&state.get_state_name_index())
                    .unwrap()
            )
        }
    }

    #[test]
    /// Sometimes, Dream Maker just doesn't get rid of extra delay
    /// information when a state has the number of frames edited.
    ///
    /// This means we need to truncate our delay list to the number of frames specified by the frames key.
    ///
    /// This always worked fine- however, we also simplify `delays = 1,1,...` to `Frames::Count(delays.len())`.
    ///
    /// The bug in our code was that we checked if our `delays = 1,1,...` *before* truncating the array
    /// in the truncation case, so we would output `Frames::Delays([1,1])` for this metadata.
    fn delay_overflow_edge_case() {
        let description = r##"
# BEGIN DMI
version = 4.0
    width = 32
    height = 32
state = "one"
    dirs = 1
    frames = 2
    delay = 1,1,0.5,0.5
# END DMI
"##
        .trim();

        let metadata = parse_metadata(description).expect("Metadata is valid");
        let state = metadata
            .get_icon_state(&StateIndex("one".to_owned(), 0))
            .expect("Only one state, named one, should be found");
        assert_eq!(state.frames, Frames::Count(2));
    }
}
