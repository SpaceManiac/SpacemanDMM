//! Error, warning, and other diagnostics handling.

use std::{fmt, error, io};
use std::path::PathBuf;
use std::cell::{RefCell, Ref};

/// An identifier referring to a loaded file.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct FileId(u16);

const FILEID_BUILTINS: FileId = FileId(0x0000);
const FILEID_MIN: FileId = FileId(0x0001);
const FILEID_MAX: FileId = FileId(0xfffe);
const FILEID_BAD: FileId = FileId(0xffff);

impl Default for FileId {
    fn default() -> FileId {
        FILEID_BAD
    }
}

impl FileId {
    #[inline]
    pub fn builtins() -> FileId {
        FILEID_BUILTINS
    }
}

/// A diagnostics context, tracking loaded files and any observed errors.
#[derive(Debug, Default)]
pub struct Context {
    /// The list of loaded files.
    files: RefCell<Vec<PathBuf>>,
    /// A list of errors, warnings, and other diagnostics generated.
    errors: RefCell<Vec<DMError>>,
    /// A list of all preprocessor symbols in the project.
    defines: RefCell<Vec<(String, Location)>>,
}

impl Context {
    /// Add a new file to the context and return its index.
    pub fn register_file(&self, path: PathBuf) -> FileId {
        let mut files = self.files.borrow_mut();
        if files.len() > FILEID_MAX.0 as usize {
            panic!("file limit of {} exceeded", FILEID_MAX.0);
        }
        let len = files.len() as u16;
        files.push(path);
        FileId(len + FILEID_MIN.0)
    }

    /// Look up a file path by its index returned from `register_file`.
    pub fn file_path(&self, file: FileId) -> PathBuf {
        if file == FILEID_BUILTINS {
            return "(builtins)".into();
        }
        let idx = (file.0 - FILEID_MIN.0) as usize;
        let files = self.files.borrow();
        if idx > files.len() {
            "(unknown)".into()
        } else {
            files[idx].to_owned()
        }
    }

    /// Push an error or other diagnostic to the context.
    pub fn register_error(&self, error: DMError) {
        self.errors.borrow_mut().push(error);
    }

    /// Access the list of diagnostics generated so far.
    pub fn errors(&self) -> Ref<[DMError]> {
        Ref::map(self.errors.borrow(), |x| &**x)
    }

    /// Push a preprocessor symbol to the symbol list.
    pub fn register_define(&self, name: String, location: Location) {
        self.defines.borrow_mut().push((name, location));
    }

    /// Access the list of preprocessor symbols.
    pub fn defines(&self) -> Ref<[(String, Location)]> {
        Ref::map(self.defines.borrow(), |x| &**x)
    }

    /// Pretty-print a `DMError` to the given output.
    pub fn pretty_print_error<W: io::Write>(&self, w: &mut W, error: &DMError) -> io::Result<()> {
        writeln!(w, "{}, line {}, column {}:",
            self.file_path(error.location.file).display(),
            error.location.line,
            error.location.column)?;
        writeln!(w, "{}: {}\n", error.severity, error.desc)
    }

    /// Pretty-print all registered diagnostics to standard error.
    ///
    /// Returns `true` if no errors were printed, `false` if any were.
    pub fn print_all_errors(&self) -> bool {
        let stderr = io::stderr();
        let stderr = &mut stderr.lock();
        let errors = self.errors();
        for err in errors.iter() {
            self.pretty_print_error(stderr, &err).expect("error writing to stderr");
        }
        errors.is_empty()
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
    pub column: u16,
}

impl Location {
    /// Pack this Location for use in `u64`-keyed structures.
    pub fn pack(self) -> u64 {
        ((self.file.0 as u64) << 48) | ((self.line as u64) << 16) | (self.column as u64)
    }

    /// Return the predecessor of this `Location`.
    pub fn pred(mut self) -> Location {
        if self.column != 0 {
            self.column -= 1;
        } else if self.line != 0 {
            self.column = !0;
            self.line -= 1;
        } else if self.file.0 != 0 {
            self.column = !0;
            self.line = !0;
            self.file.0 -= 1;
        } else {
            panic!("cannot take pred() of lowest possible Location")
        }
        self
    }
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

/// The possible diagnostic severities available.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Severity {
    Error = 1,
    Warning = 2,
    Info = 3,
    Hint = 4,
}

impl Default for Severity {
    fn default() -> Severity {
        Severity::Error
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Severity::Error => f.write_str("error"),
            Severity::Warning => f.write_str("warning"),
            Severity::Info => f.write_str("info"),
            Severity::Hint => f.write_str("hint"),
        }
    }
}

/// An error produced during DM parsing, with location information.
#[derive(Debug)]
pub struct DMError {
    location: Location,
    severity: Severity,
    desc: String,
    cause: Option<Box<error::Error + Send>>,
}

#[allow(unused_variables)]
impl DMError {
    pub fn new<S: Into<String>>(location: Location, desc: S) -> DMError {
        DMError {
            location,
            severity: Default::default(),
            desc: desc.into(),
            cause: None,
        }
    }

    pub fn set_cause<E: error::Error + Send + 'static>(mut self, cause: E) -> DMError {
        self.cause = Some(Box::new(cause));
        self
    }

    pub fn set_severity(mut self, severity: Severity) -> DMError {
        self.severity = severity;
        self
    }

    /// Get the location in the code at which this error was observed.
    pub fn location(&self) -> Location {
        self.location
    }

    /// Get the severity of this diagnostic.
    pub fn severity(&self) -> Severity {
        self.severity
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
        DMError::new(Location::default(), "i/o error").set_cause(e)
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
        self.cause.as_ref().map(|x| &**x as &error::Error)
    }
}
