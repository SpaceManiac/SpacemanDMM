// ----------------------------------------------------------------------------
// tgcode is full of case-sensitivity problems
use std::path::Path;
use std::borrow::Cow;

#[cfg(unix)]
pub fn fix_case(path: &Path) -> Cow<Path> {
    use std::ascii::AsciiExt;

    if path.exists() {
        return Cow::Borrowed(path);
    }

    let parent = match path.parent() {
        Some(x) => x,
        None => return Cow::Borrowed(path),
    };

    for entry in match parent.read_dir() {
        Ok(x) => x,
        Err(_) => return Cow::Borrowed(path),
    } {
        let entry = match entry {
            Ok(x) => x,
            Err(_) => return Cow::Borrowed(path),
        };
        let epath = entry.path();
        let epath_str = epath.display().to_string();
        let path_str = path.display().to_string();
        if epath_str.eq_ignore_ascii_case(&path_str) {
            return Cow::Owned(epath);
        }
    }
    Cow::Borrowed(path)
}

#[cfg(windows)]
#[inline(always)]
pub fn fix_case(path: &Path) -> Cow<Path> {
    Cow::Borrowed(path)
}

// ----------------------------------------------------------------------------
// Copy-paste of std::io::Chars for stability
use std::io::{self, Read, ErrorKind};

pub struct Chars<R> {
    inner: R,
}

impl<R> Chars<R> {
    pub fn new(inner: R) -> Chars<R> {
        Chars { inner }
    }
}

impl<R: Read> Iterator for Chars<R> {
    type Item = io::Result<char>;

    fn next(&mut self) -> Option<io::Result<char>> {
        let first_byte = match read_one_byte(&mut self.inner) {
            None => return None,
            Some(Ok(b)) => b,
            Some(Err(e)) => return Some(Err(e)),
        };
        let width = utf8_char_width(first_byte);
        if width == 1 { return Some(Ok(first_byte as char)) }
        if width == 0 { return Some(Err(ErrorKind::InvalidData.into())) }
        let mut buf = [first_byte, 0, 0, 0];
        {
            let mut start = 1;
            while start < width {
                match self.inner.read(&mut buf[start..width]) {
                    Ok(0) => return Some(Err(ErrorKind::InvalidData.into())),
                    Ok(n) => start += n,
                    Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
                    Err(e) => return Some(Err(e)),
                }
            }
        }
        Some(match ::std::str::from_utf8(&buf[..width]).ok() {
            Some(s) => Ok(s.chars().next().unwrap()),
            None => Err(ErrorKind::InvalidData.into()),
        })
    }
}

fn read_one_byte(reader: &mut Read) -> Option<io::Result<u8>> {
    let mut buf = [0];
    loop {
        return match reader.read(&mut buf) {
            Ok(0) => None,
            Ok(..) => Some(Ok(buf[0])),
            Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
            Err(e) => Some(Err(e)),
        };
    }
}

static UTF8_CHAR_WIDTH: [u8; 256] = [
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x1F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x3F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x5F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x7F
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x9F
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xBF
0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, // 0xDF
3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3, // 0xEF
4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0, // 0xFF
];

#[inline]
fn utf8_char_width(b: u8) -> usize {
    return UTF8_CHAR_WIDTH[b as usize] as usize;
}

// ----------------------------------------------------------------------------
// Flamegraphing

#[cfg(feature="flame")]
macro_rules! flame {
    ($e:expr) => {
        let _guard = ::utils::flame_collapse(::flame::start_guard($e));
    }
}

#[cfg(not(feature="flame"))]
macro_rules! flame {
    ($e:expr) => {}
}

#[cfg(feature="flame")]
pub fn flame_collapse(mut g: ::flame::SpanGuard) -> ::flame::SpanGuard {
    {
        let g2 = unsafe { ::std::mem::transmute::<&mut _, &mut (Option<Cow<'static, str>>, bool)>(&mut g) };
        g2.1 = true;
    }
    g
}
