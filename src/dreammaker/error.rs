//! Error, warning, and other diagnostics handling.

use std::{fmt, error, io};
use std::path::PathBuf;
use std::cell::{RefCell, Ref};

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
    files: RefCell<Vec<PathBuf>>,
    /// A list of errors, warnings, and other diagnostics generated.
    errors: RefCell<Vec<DMError>>,
}

impl Context {
    /// Add a new file to the context and return its index.
    pub fn register_file(&self, path: PathBuf) -> FileId {
        let mut files = self.files.borrow_mut();
        let len = files.len() as u32;
        files.push(path);
        FileId(len)
    }

    /// Look up a file path by its index returned from `register_file`.
    pub fn file_path(&self, file: FileId) -> PathBuf {
        let files = self.files.borrow();
        let idx = file.0 as usize;
        if idx > files.len() {  // includes BAD_FILE_ID
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

    /// Pretty-print a `DMError` to the given output.
    pub fn pretty_print_error<W: io::Write>(&self, w: &mut W, error: &DMError) -> io::Result<()> {
        writeln!(w, "{}, line {}, column {}:",
            self.file_path(error.location.file).display(),
            error.location.line,
            error.location.column)?;
        writeln!(w, "{}\n", error.desc)
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

/// An error produced during DM parsing, with location information.
#[derive(Debug)]
pub struct DMError {
    location: Location,
    severity: Severity,
    desc: String,
    cause: Option<Box<error::Error>>,
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

    pub fn set_cause<E: error::Error + 'static>(mut self, cause: E) -> DMError {
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
        self.cause.as_ref().map(|x| &**x)
    }
}
