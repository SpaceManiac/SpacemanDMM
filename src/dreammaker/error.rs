//! Error, warning, and other diagnostics handling.

use std::{fmt, error, io};
use std::path::{PathBuf, Path};
use std::cell::{RefCell, Ref, RefMut};
use std::collections::HashMap;

use termcolor::{ColorSpec, Color};
use serde::de::{Deserialize, Deserializer};

use crate::config::{Warnings, WarningLevel};

/// An identifier referring to a loaded file.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
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
    /// Warnings/Errors that should be filtered out
    filter: Warnings,
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

    pub fn register_filter(&mut self, warnings: Warnings) {
        self.filter = warnings;
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
    pub fn register_error(&self, mut error: DMError) {
        // check if the optional errortype is in the list to ignore
        if let Some(errortype) = error.errortype {
            match self.filter.warning_level_for(errortype) {
                WarningLevel::Disabled => { return },
                WarningLevel::Unset => (),
                other => {
                    error.severity = Severity::from(other);
                }
            };
        }
        if let Some(severity) = self.print_severity {
            if error.severity <= severity {
                let stderr = termcolor::StandardStream::stderr(termcolor::ColorChoice::Auto);
                self.pretty_print_error(&mut stderr.lock(), &error)
                    .expect("error writing to stderr");
            }
        }
        // ignore errors with severity above configured level
        if !self.filter.severe_enough(error.severity()) {
            return
        }
        self.errors.borrow_mut().push(error);
    }

    /// Access the list of diagnostics generated so far.
    pub fn errors(&self) -> Ref<[DMError]> {
        Ref::map(self.errors.borrow(), |x| &**x)
    }

    /// Mutably access the diagnostics list. Dangerous.
    pub fn errors_mut(&self) -> RefMut<Vec<DMError>> {
        self.errors.borrow_mut()
    }

    /// Set a severity at and above which errors will be printed immediately.
    pub fn set_print_severity(&mut self, print_severity: Option<Severity>) {
        self.print_severity = print_severity;
    }

    /// Pretty-print a `DMError` to the given output.
    pub fn pretty_print_error<W: termcolor::WriteColor>(&self, w: &mut W, error: &DMError) -> io::Result<()> {
        writeln!(
            w,
            "{}, line {}, column {}:",
            self.file_path(error.location.file).display(),
            error.location.line,
            error.location.column,
        )?;

        w.set_color(&error.severity.style())?;
        write!(w, "{}", error.severity())?;
        w.reset()?;
        writeln!(w, ": {}", error.description())?;

        for note in error.notes().iter() {
            if note.location == error.location {
                writeln!(w, "- {}", note.description, )?;
            } else if note.location.file == error.location.file {
                writeln!(
                    w,
                    "- {}:{}: {}",
                    note.location.line,
                    note.location.column,
                    note.description,
                )?;
            } else {
                writeln!(
                    w,
                    "- {}:{}:{}: {}",
                    self.file_path(note.location.file).display(),
                    note.location.line,
                    note.location.column,
                    note.description,
                )?;
            }
        }
        writeln!(w)
    }

    pub fn pretty_print_error_nocolor<W: io::Write>(&self, w: &mut W, error: &DMError) -> io::Result<()> {
        self.pretty_print_error(&mut termcolor::NoColor::new(w), error)
    }

    /// Pretty-print all registered diagnostics to standard error.
    ///
    /// Returns `true` if no errors were printed, `false` if any were.
    pub fn print_all_errors(&self, min_severity: Severity) -> bool {
        let stderr = termcolor::StandardStream::stderr(termcolor::ColorChoice::Auto);
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

    /// Clone the file list of this Context but not its error list.
    pub fn clone_file_list(&self) -> Context {
        Context {
            files: self.files.clone(),
            reverse_files: self.reverse_files.clone(),
            errors: Default::default(),
            print_severity: Default::default(),
            filter: self.filter.clone(),
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
    pub fn builtins() -> Location {
        Location { file: FILEID_BUILTINS, line: 1, column: 1 }
    }

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

    pub fn add_columns(mut self, num: u16) -> Location {
        self.column += num;
        self
    }

    pub fn is_builtins(self) -> bool {
        self.file == FILEID_BUILTINS
    }
}

/// A trait for types which may yield location information.
pub(crate) trait HasLocation {
    /// Get the current location of this parsing stage.
    fn location(&self) -> Location;

    #[inline]
    #[doc(hidden)]
    fn error<S: Into<String>>(&self, message: S) -> DMError {
        DMError::new(self.location(), message)
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

impl Severity {
    fn style(self) -> ColorSpec {
        let mut spec = ColorSpec::new();
        match self {
            Severity::Error => { spec.set_fg(Some(Color::Red)); }
            Severity::Warning => { spec.set_fg(Some(Color::Yellow)); }
            Severity::Info => { spec.set_fg(Some(Color::White)).set_intense(true); }
            Severity::Hint => {},
        }
        spec
    }
    pub fn default_all() -> Severity {
        Severity::Hint
    }
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

impl<'de> Deserialize<'de> for Severity {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?.to_lowercase();
        let severity = match s.as_str() {
            "error" | "errors" => Ok(Severity::Error),
            "warning" | "warnings" => Ok(Severity::Warning),
            "info" | "infos" => Ok(Severity::Info),
            "hint" | "hints" => Ok(Severity::Hint),
            _other => Ok(Severity::Hint),
        };
        severity
    }
}

/// A component which generated a diagnostic, when separation is desired.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Component {
    Unspecified,
    DreamChecker,
}

impl Component {
    pub fn name(self) -> Option<&'static str> {
        match self {
            Component::Unspecified => None,
            Component::DreamChecker => Some("dreamchecker"),
        }
    }
}

impl Default for Component {
    fn default() -> Component {
        Component::Unspecified
    }
}

impl fmt::Display for Component {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.name() {
            Some(name) => f.write_str(name),
            None => Ok(()),
        }
    }
}

/// An error produced during DM parsing, with location information.
#[derive(Debug)]
#[must_use]
pub struct DMError {
    location: Location,
    severity: Severity,
    component: Component,
    description: String,
    notes: Vec<DiagnosticNote>,
    cause: Option<Box<dyn error::Error + Send + Sync>>,
    errortype: Option<&'static str>,
}

/// An additional note attached to an error, at some other location.
#[derive(Debug, Clone)]
pub struct DiagnosticNote {
    location: Location,
    description: String,
}

#[allow(unused_variables)]
impl DMError {
    pub fn new<S: Into<String>>(location: Location, desc: S) -> DMError {
        DMError {
            location,
            severity: Default::default(),
            component: Default::default(),
            description: desc.into(),
            notes: Vec::new(),
            cause: None,
            errortype: None,
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

    pub fn add_note<S: Into<String>>(&mut self, location: Location, desc: S) {
        self.notes.push(DiagnosticNote {
            location,
            description: desc.into(),
        });
    }

    pub fn with_note<S: Into<String>>(mut self, location: Location, desc: S) -> DMError {
        self.add_note(location, desc);
        self
    }

    pub fn with_component(mut self, component: Component) -> DMError {
        self.component = component;
        self
    }

    pub fn with_errortype(mut self, errortype: &'static str) -> DMError {
        self.errortype = Some(errortype);
        self
    }

    #[inline]
    pub fn register(self, context: &Context) {
        context.register_error(self)
    }

    /// Get the location in the code at which this error was observed.
    pub fn location(&self) -> Location {
        self.location
    }

    /// Get the severity of this diagnostic.
    pub fn severity(&self) -> Severity {
        self.severity
    }

    /// Get the component which generated this diagnostic.
    pub fn component(&self) -> Component {
        self.component
    }

    /// Get the description associated with this error.
    pub fn description(&self) -> &str {
        &self.description
    }

    /// Deconstruct this error, returning only the description.
    pub fn into_description(self) -> String {
        self.description
    }

    /// Get the additional notes associated with this error.
    pub fn notes(&self) -> &[DiagnosticNote] {
        &self.notes
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

    fn cause(&self) -> Option<&dyn error::Error> {
        self.cause.as_ref().map(|x| &**x as &dyn error::Error)
    }
}

impl Clone for DMError {
    fn clone(&self) -> DMError {
        DMError {
            location: self.location,
            severity: self.severity,
            component: self.component,
            description: self.description.clone(),
            notes: self.notes.clone(),
            cause: None,  // not trivially cloneable
            errortype: self.errortype,
        }
    }
}

impl DiagnosticNote {
    /// Get the location in the code at which this error was observed.
    pub fn location(&self) -> Location {
        self.location
    }

    /// Get the description associated with this error.
    pub fn description(&self) -> &str {
        &self.description
    }
}
