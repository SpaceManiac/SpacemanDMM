//! Error, warning, and other diagnostics handling.

use std::{fmt, error, io};
use std::path::{PathBuf, Path};

/// An identifier referring to a loaded file.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct FileId(u32);

const BAD_FILE_ID: FileId = FileId(::std::u32::MAX);

impl Default for FileId {
    fn default() -> FileId {
        BAD_FILE_ID
    }
}

/// A diagnostics context, tracking loaded files and any observed errors.
#[derive(Debug, Default)]
pub struct Context {
    /// The list of loaded files.
    files: Vec<PathBuf>,
}

impl Context {
    /// Add a new file to the context and return its index.
    pub fn register_file(&mut self, path: PathBuf) -> FileId {
        let len = self.files.len();
        self.files.push(path);
        FileId(len as u32)
    }

    /// Look up a file path by its index returned from `register_file`.
    pub fn file_path(&self, file: FileId) -> &Path {
        let idx = file.0 as usize;
        if idx > self.files.len() {  // includes BAD_FILE_ID
            "(unknown)".as_ref()
        } else {
            &self.files[idx]
        }
    }

    /// Pretty-print a `DMError` to the given output.
    pub fn pretty_print_error<W: io::Write>(&self, w: &mut W, error: &DMError) -> io::Result<()> {
        writeln!(w, "\n{}, line {}, column {}:",
            self.file_path(error.location.file).display(),
            error.location.line,
            error.location.column)?;
        writeln!(w, "{}\n", error.desc)
    }
}

// ----------------------------------------------------------------------------
// Location handling

/// File, line, and column information for an error.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Location {
    /// The index into the file table.
    pub file: FileId,
    /// The line number, starting at 1.
    pub line: u32,
    /// The column number, starting at 1.
    pub column: u32,
}

/// A trait for types which may yield location information.
pub trait HasLocation {
    /// Get the current location of this parsing stage.
    fn location(&self) -> Location;

    #[inline]
    #[doc(hidden)]
    fn error<S: Into<String>>(&self, message: S) -> DMError {
        DMError::new(self.location(), message)
    }
}

impl<'a, T: HasLocation> HasLocation for &'a T {
    fn location(&self) -> Location { (**self).location() }
}

impl<'a, T: HasLocation> HasLocation for &'a mut T {
    fn location(&self) -> Location { (**self).location() }
}

// ----------------------------------------------------------------------------
// Error handling

/// An error produced during DM parsing, with location information.
#[derive(Debug)]
pub struct DMError {
    location: Location,
    desc: String,
    cause: Option<Box<error::Error>>,
}

#[allow(unused_variables)]
impl DMError {
    pub fn new<S: Into<String>>(location: Location, desc: S) -> DMError {
        DMError {
            location,
            desc: desc.into(),
            cause: None,
        }
    }

    pub fn with_cause<S, E>(location: Location, desc: S, cause: E) -> DMError
        where S: Into<String>, E: error::Error + 'static
    {
        DMError {
            location,
            desc: desc.into(),
            cause: Some(Box::new(cause)),
        }
    }

    /// Get the location in the code at which this error was observed.
    pub fn location(&self) -> Location {
        self.location
    }

    /// Get the description associated with this error.
    pub fn description(&self) -> &str {
        &self.desc
    }

    /// Deconstruct this error, returning only the description.
    pub fn into_description(self) -> String {
        self.desc
    }
}

impl From<io::Error> for DMError {
    fn from(e: io::Error) -> DMError {
        DMError::with_cause(Location::default(), "i/o error", e)
    }
}

impl fmt::Display for DMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.location.line, self.location.column, self.desc)
    }
}

impl error::Error for DMError {
    fn description(&self) -> &str {
        &self.desc
    }

    fn cause(&self) -> Option<&error::Error> {
        self.cause.as_ref().map(|x| &**x)
    }
}
