//! Error, warning, and other diagnostics handling.

use std::{fmt, error, io};
use std::path::{PathBuf, Path};
use std::cell::{RefCell, Ref, RefMut};
use std::collections::HashMap;

use ahash::RandomState;

use interval_tree::RangeInclusive;
use termcolor::{ColorSpec, Color};

use crate::config::Config;

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

/// A registry mapping between file names and file IDs.
#[derive(Debug, Default, Clone)]
pub struct FileList {
    /// The list of loaded files.
    files: RefCell<Vec<PathBuf>>,
    /// Reverse mapping from paths to file numbers.
    reverse_files: RefCell<HashMap<PathBuf, FileId, RandomState>>,
    /// Mapping of files -> files they include + sorted line numbers.
    included_files: RefCell<Vec<Vec<(FileId, u32)>>>,
    /// Mapping of file -> file that includes them and where.
    reverse_included_files: RefCell<Vec<Option<(FileId, u32)>>>,
}

/// A diagnostics context, tracking loaded files and any observed errors.
#[derive(Debug, Default, Clone)]
pub struct Context {
    files: FileList,
    /// A list of errors, warnings, and other diagnostics generated.
    errors: RefCell<Vec<DMError>>,
    /// Warning config
    config: RefCell<Config>,
    print_severity: Option<Severity>,

    io_time: std::cell::Cell<std::time::Duration>,
}

impl FileList {
    /// Add a new file to the context and return its index.
    pub fn register(&self, path: &Path, include_location: Option<Location>) -> FileId {
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

        self.included_files.borrow_mut().push(Vec::default());
        if let Some(include_loc) = include_location {
            let idx = include_loc.file.0 as usize;
            let includes_vec = &mut self.included_files.borrow_mut()[idx];
            // sorted insert. There can only be one include per line
            let mut inserted = false;
            let new_specifier = (include_loc.file, include_loc.line);
            for index in 0..includes_vec.len(){
                if includes_vec[index].1 > include_loc.line {
                    includes_vec.insert(index, new_specifier);
                    inserted = true;
                    break;
                }
            }

            if !inserted {
                includes_vec.push(new_specifier);
            }

            self.reverse_included_files.borrow_mut().push(Some(new_specifier));
        } else {
            self.reverse_included_files.borrow_mut().push(None);
        }

        id
    }

    // This fn is used a fuck ton by reparsing so it needs to be fast af while still being correct
    pub fn include_aware_gt(&self, lhs: &Location, rhs: &Location) -> bool {
        // Most common case, same file
        if lhs.file == rhs.file {
            return lhs > rhs;
        }

        // Optimization: The second most common case. dm files are included by and only by the .dme
        let reverse_included_files = self.reverse_included_files.borrow();
        let lhs_parent_opt = &reverse_included_files[lhs.file.0 as usize];
        let rhs_parent_opt = &reverse_included_files[rhs.file.0 as usize];

        if let Some(lhs_parent) = lhs_parent_opt {
            if let Some(rhs_parent) = rhs_parent_opt {
                if lhs_parent.0 == rhs_parent.0 {
                    return lhs_parent.1 > rhs_parent.1;
                }

                // super rare case to guard against, rhs was included from lhs
                if rhs_parent.0 == lhs.file {
                    return lhs.line > rhs_parent.1;
                }

                // proceed with true nested compare (slow, but rare)
                // find a common ancestor
                let mut lhs_walk = vec![lhs_parent];
                let mut walker = lhs_parent;
                while let Some(new_lhs_parent) = &reverse_included_files[walker.0.0 as usize] {
                    walker = new_lhs_parent;
                    lhs_walk.push(walker);
                }

                walker = rhs_parent;
                while let Some(new_rhs_parent) = &reverse_included_files[walker.0.0 as usize]  {
                    if let Some(lhs_ancestor) = lhs_walk.iter().find(|lhs_known_include|lhs_known_include.0 == walker.0) {
                        return lhs_ancestor.1 > new_rhs_parent.1;
                    }

                    walker = new_rhs_parent;
                }

                panic!("Files \"{}\" and \"{}\" do not share a common ancestor!", self.get_path(lhs.file).display(), self.get_path(rhs.file).display());
            } else {
                // RHS is in the .dme/root file, walk up to it and compare
                let mut relevant_lhs = lhs_parent;
                while let Some(new_lhs_parent) = &reverse_included_files[relevant_lhs.0.0 as usize]  {
                    relevant_lhs = new_lhs_parent;
                }

                assert_eq!(rhs.file, relevant_lhs.0);

                relevant_lhs.1 > rhs.line
            }
        } else {
            // Same as above case but with LHS
            let mut relevant_rhs = rhs_parent_opt.as_ref().unwrap(); // if this unwrap fails, it means both the lhs and rhs files are rooted but different somehow
            while let Some(new_rhs_parent) = &reverse_included_files[relevant_rhs.0.0 as usize]  {
                relevant_rhs = new_rhs_parent;
            }

            assert_eq!(lhs.file, relevant_rhs.0);
            lhs.line > relevant_rhs.1
        }
    }

    pub fn include_aware_gte(&self, lhs: &Location, rhs: &Location) -> bool {
        lhs == rhs
        || self.include_aware_gt(lhs, rhs)
    }

    /// Look up a file's ID by its path, without inserting it.
    pub fn get_id(&self, path: &Path) -> Option<FileId> {
        self.reverse_files.borrow().get(path).cloned()
    }

    /// Look up a file path by its index returned from `register_file`.
    pub fn get_path(&self, file: FileId) -> PathBuf {
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

    pub fn for_each<F: FnMut(&Path)>(&self, mut f: F) {
        for each in self.files.borrow().iter() {
            f(each);
        }
    }
}

impl Context {
    // ------------------------------------------------------------------------
    // Files

    /// Add a new file to the context and return its index.
    pub fn register_file(&self, path: &Path, parent: Option<Location>) -> FileId {
        self.files.register(path, parent)
    }

    /// Look up a file's ID by its path, without inserting it.
    pub fn get_file(&self, path: &Path) -> Option<FileId> {
        self.files.get_id(path)
    }

    /// Look up a file path by its index returned from `register_file`.
    pub fn file_path(&self, file: FileId) -> PathBuf {
        self.files.get_path(file)
    }

    /// Clone the file list of this Context but not its error list.
    pub fn clone_file_list(&self) -> FileList {
        self.files.clone()
    }

    pub fn file_list(&self) -> &FileList {
        &self.files
    }

    // ------------------------------------------------------------------------
    // Configuration

    pub fn force_config(&self, toml: &Path) {
        match Config::read_toml(toml) {
            Ok(config) => *self.config.borrow_mut() = config,
            Err(io_error) => {
                let file = self.register_file(toml, None);
                let (line, column) = io_error.line_col().unwrap_or((1, 1));
                DMError::new(Location { file, line, column }, "Error reading configuration file", Component::Unspecified)
                    .with_boxed_cause(io_error.into_boxed_error())
                    .register(self);
            }
        }
    }

    pub fn autodetect_config(&self, dme: &Path) {
        let toml = dme.parent().unwrap().join("SpacemanDMM.toml");
        if toml.exists() {
            self.force_config(&toml);
        }
    }

    pub fn config(&self) -> Ref<Config> {
        self.config.borrow()
    }

    /// Set a severity at and above which errors will be printed immediately.
    pub fn set_print_severity(&mut self, print_severity: Option<Severity>) {
        self.print_severity = print_severity;
    }

    // ------------------------------------------------------------------------
    // Additional diagnostics

    pub fn reset_io_time(&self) {
        self.io_time.take();
    }

    pub fn add_io_time(&self, add: std::time::Duration) {
        self.io_time.set(self.io_time.get() + add);
    }

    pub fn get_io_time(&self) -> std::time::Duration {
        self.io_time.get()
    }

    // ------------------------------------------------------------------------
    // Errors

    /// Push an error or other diagnostic to the context.
    pub fn register_error(&self, error: DMError) {
        guard!(let Some(error) = self.config.borrow().set_configured_severity(error) else {
            return // errortype is disabled
        });
        // ignore errors with severity above configured level
        if !self.config.borrow().registerable_error(&error) {
            return
        }
        if let Some(print_severity) = self.print_severity {
            if error.severity() <= print_severity {
                let stderr = termcolor::StandardStream::stderr(termcolor::ColorChoice::Auto);
                self.pretty_print_error(&mut stderr.lock(), &error)
                    .expect("error writing to stderr");
            }
        }
        let mut errors = self.errors.borrow_mut();

        // reparses can duplicate errors, ignore them if they're an exact match
        for existing_error in errors.iter() {
            if error.eq(existing_error) {
                return;
            }
        }

        errors.push(error);
    }

    pub fn clear_errors(&self, range_option: Option<&RangeInclusive<Location>>, component_option: Option<Component>) {
        let mut errors = self.errors.borrow_mut();
        errors.retain(|error|{
            let mut keep = true;
            if let Some(range) = range_option {
                keep &= error.location < range.start || error.location > range.end
            }

            if let Some(component) = component_option {
                keep &= error.component != component
            }

            keep
        });
    }

    /// Access the list of diagnostics generated so far.
    pub fn errors(&self) -> Ref<[DMError]> {
        Ref::map(self.errors.borrow(), |x| &**x)
    }

    /// Mutably access the diagnostics list. Dangerous.
    #[doc(hidden)]
    pub fn errors_mut(&self) -> RefMut<Vec<DMError>> {
        self.errors.borrow_mut()
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
    /// Returns `true` if any errors were printed, `false` if none were.
    fn print_all_errors(&self, min_severity: Severity) -> bool {
        let stderr = termcolor::StandardStream::stderr(termcolor::ColorChoice::Auto);
        let stderr = &mut stderr.lock();
        let errors = self.errors();
        let mut printed = false;
        for err in errors.iter() {
            if err.severity <= min_severity {
                self.pretty_print_error(stderr, err).expect("error writing to stderr");
                printed = true;
            }
        }
        printed
    }

    /// Print messages and panic if there were any errors.
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
        } else if self.file == FILEID_BAD {
            // This file ID generally comes from using Location::default().
            // In that case hopefully it's a test or something, so just let it
            // stay 0:0.
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
    fn error<S: Into<String>>(&self, message: S, component: Component) -> DMError {
        DMError::new(self.location(), message, component)
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

/// A component which generated a diagnostic, when separation is desired.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Component {
    Unspecified,
    Parser,
    ObjectTree,
    DreamChecker,
    DmmTools,
}

impl Component {
    pub fn name(self) -> Option<&'static str> {
        match self {
            Component::Unspecified => None,
            Component::Parser => Some("parser"),
            Component::ObjectTree => Some("object tree"),
            Component::DreamChecker => Some("dreamchecker"),
            Component::DmmTools => Some("dmm-tools")
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
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DiagnosticNote {
    location: Location,
    description: String,
}

#[allow(unused_variables)]
impl DMError {
    pub fn new<S: Into<String>>(location: Location, desc: S, component: Component) -> DMError {
        DMError {
            location,
            severity: Default::default(),
            component: component,
            description: desc.into(),
            notes: Vec::new(),
            cause: None,
            errortype: None,
        }
    }

    fn with_boxed_cause(mut self, cause: Box<dyn error::Error + Send + Sync>) -> DMError {
        self.add_note(self.location, cause.to_string());
        self.cause = Some(cause);
        self
    }

    pub fn with_cause<E: error::Error + Send + Sync + 'static>(self, cause: E) -> DMError {
        self.with_boxed_cause(Box::new(cause))
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

    pub fn with_errortype(mut self, errortype: &'static str) -> DMError {
        self.errortype = Some(errortype);
        self
    }

    pub fn with_location(mut self, location: Location) -> DMError {
        self.location = location;
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

    /// Get the errortype associated with this error.
    pub fn errortype(&self) -> Option<&'static str> {
        self.errortype
    }

    /// Get the additional notes associated with this error.
    pub fn notes(&self) -> &[DiagnosticNote] {
        &self.notes
    }
}

impl fmt::Display for DMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Like `pretty_print_error` above, but without filename information.
        write!(f, "{}:{}: {}: {}", self.location.line, self.location.column, self.severity, self.description)?;
        for note in self.notes.iter() {
            if note.location == self.location {
                write!(f, "\n- {}", note.description, )?;
            } else {
                write!(
                    f,
                    "\n- {}:{}: {}",
                    note.location.line,
                    note.location.column,
                    note.description,
                )?;
            }
        }
        Ok(())
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

impl PartialEq for DMError {
    fn eq(&self, other: &Self) -> bool {
        // ignore causes
        self.location == other.location
        && self.severity == other.severity
        && self.component == other.component
        && self.description == other.description
        && self.notes == other.notes
        && self.errortype == other.errortype
    }
}

impl Eq for DMError{}

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
