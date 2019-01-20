//! Error, warning, and other diagnostics handling.

use std::{fmt, error, io};
use std::path::{PathBuf, Path};
use std::cell::{RefCell, Ref};
use std::collections::HashMap;

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
    /// Reverse mapping from paths to file numbers.
    reverse_files: RefCell<HashMap<PathBuf, FileId>>,
    /// A list of errors, warnings, and other diagnostics generated.
    errors: RefCell<Vec<DMError>>,
    /// Severity at and above which errors will be printed immediately.
    print_severity: Option<Severity>,
}

impl Context {
    /// Add a new file to the context and return its index.
    pub fn register_file(&self, path: &Path) -> FileId {
        if let Some(id) = self.reverse_files.borrow().get(path).cloned() {
            return id;
        }
        let mut files = self.files.borrow_mut();
        if files.len() > FILEID_MAX.0 as usize {
            panic!("file limit of {} exceeded", FILEID_MAX.0);
        }
        let len = files.len() as u16;
        files.push(path.to_owned());
        let id = FileId(len + FILEID_MIN.0);
        self.reverse_files.borrow_mut().insert(path.to_owned(), id);
        id
    }

    /// Look up a file's ID by its path, without inserting it.
    pub fn get_file(&self, path: &Path) -> Option<FileId> {
        self.reverse_files.borrow().get(path).cloned()
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
        if let Some(severity) = self.print_severity {
            if error.severity <= severity {
                let stderr = io::stderr();
                self.pretty_print_error(&mut stderr.lock(), &error)
                    .expect("error writing to stderr");
            }
        }
        self.errors.borrow_mut().push(error);
    }

    /// Access the list of diagnostics generated so far.
    pub fn errors(&self) -> Ref<[DMError]> {
        Ref::map(self.errors.borrow(), |x| &**x)
    }

    /// Set a severity at and above which errors will be printed immediately.
    pub fn set_print_severity(&mut self, print_severity: Option<Severity>) {
        self.print_severity = print_severity;
    }

    /// Pretty-print a `DMError` to the given output.
    pub fn pretty_print_error<W: io::Write>(&self, w: &mut W, error: &DMError) -> io::Result<()> {
        writeln!(
            w,
            "{}, line {}, column {}:",
            self.file_path(error.location.file).display(),
            error.location.line,
            error.location.column,
        )?;
        writeln!(w, "{}: {}\n", error.severity, error.description)
    }

    /// Pretty-print all registered diagnostics to standard error.
    ///
    /// Returns `true` if no errors were printed, `false` if any were.
    pub fn print_all_errors(&self, min_severity: Severity) -> bool {
        let stderr = io::stderr();
        let stderr = &mut stderr.lock();
        let errors = self.errors();
        let mut printed = false;
        for err in errors.iter() {
            if err.severity <= min_severity {
                self.pretty_print_error(stderr, &err).expect("error writing to stderr");
                printed = true;
            }
        }
        printed
    }

    /// Print messages and panic if there were any errors.
    #[inline]
    #[doc(hidden)]
    pub fn assert_success(&self) {
        if self.print_all_errors(Severity::Info) {
            panic!("there were parse errors");
        }
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
        (u64::from(self.file.0) << 48) | (u64::from(self.line) << 16) | u64::from(self.column)
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

    pub fn is_builtins(self) -> bool {
        self.file == FileId::builtins()
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
    fn location(&self) -> Location {
        (**self).location()
    }
}

impl<'a, T: HasLocation> HasLocation for &'a mut T {
    fn location(&self) -> Location {
        (**self).location()
    }
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
    description: String,
    cause: Option<Box<error::Error + Send + Sync>>,
}

#[allow(unused_variables)]
impl DMError {
    pub fn new<S: Into<String>>(location: Location, desc: S) -> DMError {
        DMError {
            location,
            severity: Default::default(),
            description: desc.into(),
            cause: None,
        }
    }

    pub fn set_cause<E: error::Error + Send + Sync + 'static>(mut self, cause: E) -> DMError {
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
        &self.description
    }

    /// Deconstruct this error, returning only the description.
    pub fn into_description(self) -> String {
        self.description
    }
}

impl fmt::Display for DMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.location.line, self.location.column, self.description)
    }
}

impl error::Error for DMError {
    fn description(&self) -> &str {
        &self.description
    }

    fn cause(&self) -> Option<&error::Error> {
        self.cause.as_ref().map(|x| &**x as &error::Error)
    }
}
