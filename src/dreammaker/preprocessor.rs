//! The preprocessor.
use std::collections::{HashMap, VecDeque};
use std::{io, fmt};
use std::fs::File;
use std::path::{Path, PathBuf};
use std::borrow::Cow;

use interval_tree::{IntervalTree, range};

use super::{DMError, Location, HasLocation, FileId, Context, Severity};
use super::lexer::*;
use super::docs::{DocComment, DocTarget, DocCollection};
use super::annotation::*;
use super::ast::Ident;

/// The maximum recursion depth of macro expansion.
const MAX_RECURSION_DEPTH: usize = 32;

// ----------------------------------------------------------------------------
// Macro representation and predefined macros

#[derive(Debug, Clone, PartialEq)]
pub enum Define {
    Constant {
        subst: Vec<Token>,
        docs: DocCollection,
    },
    Function {
        params: Vec<Ident>,
        subst: Vec<Token>,
        variadic: bool,
        docs: DocCollection,
    },
}

impl Define {
    /// Get the documentation associated with this define.
    pub fn docs(&self) -> &DocCollection {
        match self {
            Define::Constant { docs, .. } => docs,
            Define::Function { docs, .. } => docs,
        }
    }

    /// Get this define's substitution. May be empty.
    pub fn substitution(&self) -> &[Token] {
        match self {
            Define::Constant { subst, .. } => subst,
            Define::Function { subst, .. } => subst,
        }
    }

    pub fn display_with_name<'a>(&'a self, name: &'a str) -> impl fmt::Display + 'a {
        NameAndDefine(name, self)
    }
}

struct NameAndDefine<'a>(&'a str, &'a Define);

impl<'a> fmt::Display for NameAndDefine<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "#define {}", self.0)?;

        if let Define::Function { params, .. } = self.1 {
            fmt.write_str("(")?;
            for (i, name) in params.iter().enumerate() {
                if i > 0 {
                    fmt.write_str(", ")?;
                }
                fmt.write_str(name)?;
            }
            fmt.write_str(")")?;
        }

        fmt.write_str("\n")?;

        let subst = self.1.substitution();
        crate::pretty_print(fmt, subst.iter().cloned(), false)
    }
}

type InnerDefineHistory = IntervalTree<Location, (String, Define)>;

/// An interval tree representing historic macro definitions.
#[derive(Debug)]
pub struct DefineHistory {
    env_file: PathBuf,
    last_input_loc: Location,
    tree: InnerDefineHistory,
}

impl DefineHistory {
    /// Branch a child preprocessor from this preprocessor's historic state at
    /// the start of the given file.
    pub fn branch_at_file<'ctx2>(&self, file: FileId, context: &'ctx2 Context) -> Preprocessor<'ctx2> {
        let location = Location { file, line: 0, column: 0 };
        let defines = DefineMap::from_history(self, location);

        Preprocessor {
            context,
            env_file: self.env_file.clone(),
            include_stack: Default::default(),
            include_locations: Default::default(),
            history: Default::default(),  // TODO: support branching a second time
            defines,
            maps: Default::default(),
            skins: Default::default(),
            scripts: Default::default(),
            ifdef_stack: Default::default(),  // should be fine
            ifdef_history: Default::default(),
            last_input_loc: location,
            last_printable_input_loc: location,
            output: Default::default(),
            danger_idents: Default::default(),
            docs_in: Default::default(),
            docs_out: Default::default(),
            in_interp_string: 0,
            annotations: None,
        }
    }

    /// Branch a child preprocessor from this preprocessor's current state.
    pub fn branch_at_end<'ctx2>(&self, context: &'ctx2 Context) -> Preprocessor<'ctx2> {
        Preprocessor {
            context,
            env_file: self.env_file.clone(),
            include_stack: Default::default(),
            include_locations: Default::default(),
            history: Default::default(),  // TODO: support branching a second time
            defines: DefineMap::from_history(self, self.last_input_loc),
            maps: Default::default(),
            skins: Default::default(),
            scripts: Default::default(),
            ifdef_stack: Default::default(),  // should be fine
            ifdef_history: Default::default(),
            last_input_loc: self.last_input_loc,
            last_printable_input_loc: self.last_input_loc,
            output: Default::default(),
            danger_idents: Default::default(),
            docs_in: Default::default(),
            docs_out: Default::default(),
            in_interp_string: 0,
            annotations: None,
        }
    }
}

impl std::ops::Deref for DefineHistory {
    type Target = IntervalTree<Location, (String, Define)>;

    fn deref(&self) -> &Self::Target {
        &self.tree
    }
}

impl std::ops::DerefMut for DefineHistory {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tree
    }
}

/// A map from macro names to their locations and definitions.
///
/// Redefinitions of macros push to a stack, and undefining the macro returns
/// it to the previous entry in the stack, only fully undefining it when the
/// stack is exhausted.
#[derive(Debug, Clone, Default)]
pub struct DefineMap {
    inner: HashMap<String, Vec<(Location, Define)>>,
}

impl DefineMap {
    pub fn with_builtins() -> DefineMap {
        let mut this = Default::default();
        super::builtins::default_defines(&mut this);
        this
    }

    /// Returns the number of elements in the map.
    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns true if the map is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Returns true if the map contains a value for the specified key.
    pub fn contains_key(&self, key: &str) -> bool {
        self.inner.get(key).map_or(false, |v| !v.is_empty())
    }

    /// Returns a reference to the value corresponding to the key.
    pub fn get(&self, key: &str) -> Option<&(Location, Define)> {
        self.inner.get(key).and_then(|v| v.last())
    }

    /// Inserts a key-value pair into the map.
    ///
    /// Returns `None` if the key was not present, or its most recent location
    /// if it was.
    pub fn insert(&mut self, key: String, value: (Location, Define)) -> Option<Location> {
        let stack = self.inner.entry(key).or_insert_with(Default::default);
        let result = stack.last().map(|&(loc, _)| loc);
        stack.push(value);
        result
    }

    /// Removes a key from the map, returning the value at the key if the key
    /// was previously in the map.
    pub fn remove(&mut self, key: &str) -> Option<(Location, Define)> {
        let (remove, result);
        match self.inner.get_mut(key) {
            None => return None,
            Some(stack) => {
                result = stack.pop();
                remove = stack.is_empty();
            }
        }
        if remove {
            self.inner.remove(key);
        }
        result
    }

    /// Cut a DefineMap from the state of a DefineHistory at the given location.
    fn from_history(history: &InnerDefineHistory, location: Location) -> DefineMap {
        let mut map = DefineMap::default();
        for (range, &(ref name, ref define)) in history.range(range(location, location)) {
            map.insert(name.clone(), (range.start, define.clone()));
        }
        map
    }

    /*
    /// Test whether two DefineMaps are equal, ignoring definition locations.
    fn equals(&self, rhs: &DefineMap) -> bool {
        if self.len() != rhs.len() {
            return false;
        }

        self.inner.iter().all(|(key, value)| {
            rhs.inner.get(key).map_or(false, |v| {
                if value.len() != v.len() {
                    return false;
                }
                value.iter().zip(v.iter()).all(|(lhs, rhs)| lhs.1 == rhs.1)
            })
        })
    }
    */
}

// ----------------------------------------------------------------------------
// The stack of currently #included files

#[derive(Debug)]
enum Include<'ctx> {
    File {
        path: PathBuf,
        file: FileId,
        lexer: Lexer<'ctx>,
    },
    Expansion {
        name: Ident,
        location: Location,
        tokens: VecDeque<Token>,
    },
}

impl<'ctx> Include<'ctx> {
    fn from_path(context: &'ctx Context, path: PathBuf) -> Result<Include<'ctx>, DMError> {
        let idx = context.register_file(&path);
        Ok(Include::File {
            file: idx,
            lexer: Lexer::from_file(context, idx, &path)?,
            path,
        })
    }

    fn from_buffer(context: &'ctx Context, path: PathBuf, buffer: Cow<'ctx, [u8]>) -> Include<'ctx> {
        let idx = context.register_file(&path);
        Include::File {
            file: idx,
            lexer: Lexer::new(context, idx, buffer),
            path,
        }
    }
}

#[derive(Debug, Default)]
struct IncludeStack<'ctx> {
    stack: Vec<Include<'ctx>>,
}

impl<'ctx> IncludeStack<'ctx> {
    fn top_file_path(&self) -> &Path {
        for each in self.stack.iter().rev() {
            if let Include::File { ref path, .. } = each {
                return path;
            }
        }
        "".as_ref()
    }

    fn in_expansion(&self) -> bool {
        matches!(self.stack.last(), Some(Include::Expansion { .. }))
    }
}

impl<'ctx> Iterator for IncludeStack<'ctx> {
    type Item = LocatedToken;

    fn next(&mut self) -> Option<LocatedToken> {
        loop {
            match self.stack.last_mut() {
                Some(&mut Include::File { ref mut lexer, .. }) => match lexer.next() {
                    //Some(Err(e)) => return Some(Err(e)),
                    Some(t) => return Some(t),
                    None => {} // fall through
                },
                Some(&mut Include::Expansion {
                    ref mut tokens,
                    location,
                    ..
                }) => match tokens.pop_front() {
                    Some(token) => return Some(LocatedToken { location, token }),
                    None => {} // fall through
                },
                None => return None,
            }
            self.stack.pop();
        }
    }
}

// ----------------------------------------------------------------------------
// The main preprocessor

#[derive(Debug)]
struct Ifdef {
    location: Location,
    active: bool,
    chain_active: bool,
}

impl Ifdef {
    fn new(location: Location, active: bool) -> Ifdef {
        Ifdef {
            location,
            active,
            chain_active: active,
        }
    }
    fn else_(self, location: Location) -> Ifdef {
        Ifdef {
            location,
            active: !self.chain_active,
            chain_active: true,
        }
    }
    fn else_if(self, location: Location, active: bool) -> Ifdef {
        Ifdef {
            location,
            active: !self.chain_active && active,
            chain_active: self.chain_active || active,
        }
    }
}

#[derive(Debug)]
/// C-like preprocessor for DM. Expands directives and macro invocations.
pub struct Preprocessor<'ctx> {
    context: &'ctx Context,
    env_file: PathBuf,

    include_stack: IncludeStack<'ctx>,
    include_locations: HashMap<FileId, Location>,
    last_input_loc: Location,
    output: VecDeque<Token>,
    ifdef_stack: Vec<Ifdef>,
    ifdef_history: IntervalTree<Location, bool>,
    annotations: Option<AnnotationTree>,

    history: InnerDefineHistory,
    defines: DefineMap,
    maps: Vec<PathBuf>,
    skins: Vec<PathBuf>,
    scripts: Vec<PathBuf>,

    last_printable_input_loc: Location,
    danger_idents: HashMap<Ident, Location>,
    in_interp_string: u32,

    docs_in: VecDeque<(Location, DocComment)>,
    docs_out: VecDeque<(Location, DocComment)>,
}

impl<'ctx> HasLocation for Preprocessor<'ctx> {
    fn location(&self) -> Location {
        match self.include_stack.stack.last() {
            Some(&Include::File { ref lexer, .. }) => lexer.location(),
            Some(&Include::Expansion { location, .. }) => location,
            None => Location::default()
        }
    }
}

impl<'ctx> Preprocessor<'ctx> {
    /// Create a new preprocessor from the given Context and environment file.
    pub fn new(context: &'ctx Context, env_file: PathBuf) -> Result<Self, DMError> {
        // Buffer the entire environment file. Large environments take a while
        // to load and locking it for the whole time is somewhat inconvenient.
        let include = Include::from_path(context, env_file.clone())?;

        Ok(Preprocessor {
            context,
            env_file,
            include_stack: IncludeStack { stack: vec![include] },
            include_locations: Default::default(),
            history: Default::default(),
            defines: DefineMap::with_builtins(),
            maps: Default::default(),
            skins: Default::default(),
            scripts: Default::default(),
            ifdef_stack: Default::default(),
            ifdef_history: Default::default(),
            last_input_loc: Default::default(),
            last_printable_input_loc: Default::default(),
            output: Default::default(),
            danger_idents: Default::default(),
            docs_in: Default::default(),
            docs_out: Default::default(),
            in_interp_string: 0,
            annotations: None,
        })
    }

    pub fn from_buffer<S: Into<Cow<'ctx, str>>>(context: &'ctx Context, env_file: PathBuf, buffer: S) -> Self {
        let cow_u8 = match buffer.into() {
            Cow::Borrowed(s) => Cow::Borrowed(s.as_bytes()),
            Cow::Owned(s) => Cow::Owned(s.into_bytes()),
        };
        let include = Include::from_buffer(context, env_file.clone(), cow_u8);
        Preprocessor {
            context,
            env_file,
            include_stack: IncludeStack { stack: vec![include] },
            include_locations: Default::default(),
            history: Default::default(),
            defines: DefineMap::with_builtins(),
            maps: Default::default(),
            skins: Default::default(),
            scripts: Default::default(),
            ifdef_stack: Default::default(),
            ifdef_history: Default::default(),
            last_input_loc: Default::default(),
            last_printable_input_loc: Default::default(),
            output: Default::default(),
            danger_idents: Default::default(),
            docs_in: Default::default(),
            docs_out: Default::default(),
            in_interp_string: 0,
            annotations: None,
        }
    }

    /// Finalize this preprocessor into its complete define history.
    pub fn finalize(mut self) -> DefineHistory {
        let mut i = 0;
        for (name, vector) in self.defines.inner.drain() {
            for (start, define) in vector {
                // Give each define its own end column in order to avoid key
                // collisions in the interval tree.
                i += 1;
                let end = Location {
                    file: FileId::default(),
                    line: !0,
                    column: i,
                };
                self.history.insert(range(start, end), (name.clone(), define));
            }
        }
        DefineHistory {
            env_file: self.env_file,
            last_input_loc: self.last_input_loc,
            tree: self.history,
        }
    }

    /// Access the ifdef history.
    pub fn ifdef_history(&self) -> &IntervalTree<Location, bool> {
        &self.ifdef_history
    }

    /*
    /// Check whether this preprocessor's state as of the end of the given file
    /// matches the given child preprocessor.
    pub fn matches_end_of_file(&self, file: FileId, other: &Preprocessor) -> bool {
        let location = Location { file, line: !0, column: !0 };
        let defines = DefineMap::from_history(&self.history, location);
        defines.equals(&other.defines)
    }
    */

    /// Push a DM file to the top of this preprocessor's stack.
    pub fn push_file<R: io::Read + 'static>(&mut self, path: PathBuf, read: R) -> Result<FileId, DMError> {
        let idx = self.context.register_file(&path);
        self.include_stack.stack.push(Include::File {
            lexer: Lexer::from_read(self.context, idx, read)?,
            file: idx,
            path,
        });
        Ok(idx)
    }

    /// Enable source file annotations.
    pub fn enable_annotations(&mut self) {
        self.annotations = Some(AnnotationTree::default());
    }

    /// Retrieve computer annotations.
    pub fn take_annotations(&mut self) -> Option<AnnotationTree> {
        self.annotations.take()
    }

    // ------------------------------------------------------------------------
    // Macro definition handling

    fn annotate_macro(&mut self, ident: &str, def_loc: Location) {
        if self.include_stack.in_expansion() {
            return;
        }

        if let Some(annotations) = self.annotations.as_mut() {
            annotations.insert(
                self.last_input_loc .. self.last_input_loc.add_columns(ident.len() as u16),
                Annotation::MacroUse(ident.to_owned(), def_loc));
        }
    }

    fn in_environment(&self) -> bool {
        for include in self.include_stack.stack.iter().rev() {
            if let Include::File { ref path, .. } = *include {
                return *path == self.env_file;
            }
        }
        false
    }

    fn is_defined(&self, name: &str) -> bool {
        match name {
            "__MAIN__" => self.in_environment(),
            _ => self.defines.contains_key(name),
        }
    }

    fn move_to_history(&mut self, name: String, previous: (Location, Define)) {
        self.history.insert(range(previous.0, self.last_input_loc), (name, previous.1));
    }

    // ------------------------------------------------------------------------
    // Conditional compilation handling

    fn is_disabled(&self) -> bool {
        self.ifdef_stack.iter().any(|x| !x.active)
    }

    fn pop_ifdef(&mut self) -> Option<Ifdef> {
        self.ifdef_stack.pop().map(|ifdef| {
            self.ifdef_history.insert(range(ifdef.location, self.last_input_loc), ifdef.active);
            ifdef
        })
    }

    fn evaluate_inner(&mut self) -> Result<bool, DMError> {
        // pump real_next to fill output until we get a real newline on input
        let start = self.last_input_loc;
        while let Some(tok) = self.inner_next() {
            self.last_input_loc = tok.location;

            if let Token::Punct(Punctuation::Newline) = tok.token {
                break;
            }

            if let Err(e) = self.real_next(tok.token, true) {
                self.context.register_error(e);
            }
        }

        // If we're inside an inactive #if, don't actually evaluate.
        // #ifdef FOO -- #if FOO == 5 will error otherwise.
        if self.is_disabled() {
            self.output.clear();
            return Ok(false);
        }

        let expr = crate::parser::parse_expression(
            self.context,
            start,
            self.output.drain(..).map(|token| LocatedToken::new(start, token))
        )?;
        Ok(crate::constants::preprocessor_evaluate(start, expr, &self.defines)?.to_bool())
    }

    fn evaluate(&mut self) -> bool {
        // always succeed in order to avoid phantom "unmatched #endif" messages
        match self.evaluate_inner() {
            Ok(value) => value,
            Err(err) => {
                self.context.register_error(err);
                false
            }
        }
    }

    // ------------------------------------------------------------------------
    // Doc comments

    /// Something other than a `#define` was encountered, docs are not for us.
    fn flush_docs(&mut self) {
        self.docs_out.extend(self.docs_in.drain(..));
    }

    // ------------------------------------------------------------------------
    // Internal utilities

    fn prepare_include_file(&mut self, path: PathBuf) -> Result<Include<'ctx>, DMError> {
        // Attempt to open the file.
        let read = io::BufReader::new(File::open(&path).map_err(|e|
            DMError::new(self.last_input_loc, format!("failed to open file: #include {:?}", path))
                .with_cause(e))?);

        // Get the path relative to the environment root, for easy lookup later.
        let register = path.strip_prefix(self.env_file.parent().unwrap()).unwrap_or(&path);

        // Make sure the file hasn't already been included.
        // All DM source is effectively `#pragma once`.
        let file_id = self.context.register_file(&register);
        if let Some(&loc) = self.include_locations.get(&file_id) {
            Err(DMError::new(self.last_input_loc, format!("duplicate #include {:?}", path))
                .set_severity(Severity::Warning)
                .with_note(loc, "previously included here")
                .with_errortype("duplicate_include"))
        } else {
            self.include_locations.insert(file_id, self.last_input_loc);
            Ok(Include::File {
                path,
                file: file_id,
                lexer: Lexer::from_read(&self.context, file_id, read)?,
            })
        }
    }

    fn check_danger_ident(&mut self, name: &str, kind: &str) {
        if let Some(loc) = self.danger_idents.get(name) {
            self.context.register_error(DMError::new(*loc, format!(
                "macro {:?} used immediately before being {}:\n\
                https://secure.byond.com/forum/?post=2072419", name, kind
            )).set_severity(Severity::Warning));
        }
    }

    fn inner_next(&mut self) -> Option<LocatedToken> {
        self.include_stack.next()
    }

    #[allow(unreachable_code)]
    fn real_next(&mut self, read: Token, inside_condition: bool) -> Result<(), DMError> {
        let mut _last_expected_loc = self.last_input_loc;
        macro_rules! next {
            () => {
                match self.inner_next() {
                    Some(x) => {
                        _last_expected_loc = x.location;
                        x.token
                    }
                    None => return Err(self.error("unexpected EOF")),
                }
            };
        }
        macro_rules! expect_token {
            (($($i:ident),*) = $p:pat) => {
                let ($($i,)*) = match next!() {
                    $p => ($($i,)*),
                    other => return Err(self.error(format!("unexpected token {:?}, expecting {}", other, stringify!($p))))
                };
            }
        }

        const ALL_DIRECTIVES: &[&str] = &[
            "if", "ifdef", "ifndef", "elif", "else", "endif",
            "include", "define", "undef", "warn", "error",
        ];
        let disabled = !inside_condition && self.is_disabled();
        match read {
            Token::Punct(Punctuation::Hash) => {
                // preprocessor directive, next thing ought to be an ident
                expect_token!((ident) = Token::Ident(ident, _));
                match &ident[..] {
                    // ifdefs
                    "endif" => {
                        self.pop_ifdef().ok_or_else(||
                            DMError::new(self.last_input_loc, "unmatched #endif"))?;
                    }
                    "else" => {
                        let last = self.pop_ifdef().ok_or_else(||
                            DMError::new(self.last_input_loc, "unmatched #else"))?;
                        self.ifdef_stack.push(last.else_(self.last_input_loc));
                    }
                    "ifdef" => {
                        expect_token!((define_name) = Token::Ident(define_name, _));
                        expect_token!(() = Token::Punct(Punctuation::Newline));
                        let enabled = self.is_defined(&define_name);
                        self.ifdef_stack.push(Ifdef::new(self.last_input_loc, enabled));
                    }
                    "ifndef" => {
                        expect_token!((define_name) = Token::Ident(define_name, _));
                        expect_token!(() = Token::Punct(Punctuation::Newline));
                        let enabled = !self.is_defined(&define_name);
                        self.ifdef_stack.push(Ifdef::new(self.last_input_loc, enabled));
                    }
                    "if" => {
                        let enabled = self.evaluate();
                        self.ifdef_stack.push(Ifdef::new(self.last_input_loc, enabled));
                    }
                    "elif" => {
                        let last = self.pop_ifdef().ok_or_else(||
                            DMError::new(self.last_input_loc, "unmatched #elif"))?;
                        let enabled = self.evaluate();
                        self.ifdef_stack.push(last.else_if(self.last_input_loc, enabled));
                    }
                    // --------------------------------------------------------
                    // anything other than ifdefs may be ifdef'd out
                    // --------------------------------------------------------
                    // include searches relevant paths for files
                    "include" if disabled => {}
                    "include" => {
                        expect_token!((path_str) = Token::String(path_str));
                        let include_loc = _last_expected_loc;
                        expect_token!(() = Token::Punct(Punctuation::Newline));
                        let path = PathBuf::from(path_str.replace("\\", "/"));

                        for candidate in vec![
                            // 1. relative to file in which `#include` appears.
                            self.include_stack.top_file_path().parent().unwrap().join(&path),
                            // 2. relative to root `.dme` file.
                            self.env_file.parent().unwrap().join(&path),
                        ] {
                            if !candidate.exists() {
                                continue;
                            }
                            // Double-match is used to let go of the borrow of
                            // `candidate` so it can be used in the second half.
                            enum FileType {
                                DMM,
                                DMF,
                                DMS,
                                DM,
                            }
                            let file_type = match candidate.extension().and_then(|s| s.to_str()) {
                                Some("dmm") => FileType::DMM,
                                Some("dmf") => FileType::DMF,
                                Some("dms") => FileType::DMS,
                                Some("dm") => FileType::DM,
                                Some("dme") => FileType::DM,
                                Some(ext) => {
                                    self.context.register_error(DMError::new(
                                        self.last_input_loc,
                                        format!("unknown extension {:?}", ext),
                                    ));
                                    return Ok(());
                                }
                                None => {
                                    self.context.register_error(DMError::new(self.last_input_loc, "filename has no extension"));
                                    return Ok(());
                                }
                            };

                            if let Some(annotations) = self.annotations.as_mut() {
                                annotations.insert(
                                    include_loc .. include_loc.add_columns(2 + path_str.len() as u16),
                                    Annotation::Include(candidate.clone()));
                            }

                            match file_type {
                                FileType::DMM => self.maps.push(candidate),
                                FileType::DMF => self.skins.push(candidate),
                                FileType::DMS => self.scripts.push(candidate),
                                FileType::DM => match self.prepare_include_file(candidate) {
                                    Ok(include) => {
                                        // A phantom newline keeps the include
                                        // directive being indented from making
                                        // the first line of the file indented.
                                        self.output.push_back(Token::Punct(Punctuation::Newline));
                                        self.include_stack.stack.push(include);
                                    },
                                    Err(e) => self.context.register_error(e),
                                },
                            }

                            return Ok(());
                        }

                        self.context.register_error(DMError::new(self.last_input_loc, format!("failed to find #include {:?}", path)));
                        return Ok(());
                    }
                    // both constant and function defines
                    "define" if disabled => {
                        // Skip to the end of the line, or else we'll catch
                        // stringify operators `#X` as unknown directives.
                        loop {
                            match next!() {
                                Token::Punct(Punctuation::Newline) => break,
                                _ => {}
                            }
                        }
                    }
                    "define" => {
                        // accumulate just-seen Following doc comments
                        let mut our_docs = Vec::new();
                        while let Some((loc, doc)) = self.docs_in.pop_back() {
                            if doc.target == DocTarget::FollowingItem {
                                our_docs.push(doc);
                            } else {
                                self.docs_in.push_back((loc, doc));
                                break;
                            }
                        }
                        let mut docs = DocCollection::default();
                        for each in our_docs.into_iter().rev() {
                            docs.push(each);
                        }
                        // flush all docs which do not apply to this define
                        self.flush_docs();

                        expect_token!((define_name, ws) = Token::Ident(define_name, ws));
                        let define_name_loc = _last_expected_loc;
                        if let Some(annotations) = self.annotations.as_mut() {
                            annotations.insert(
                                define_name_loc .. define_name_loc.add_columns(define_name.len() as u16),
                                Annotation::MacroDefinition(define_name.to_owned()));
                        }
                        self.check_danger_ident(&define_name, "defined");
                        let mut params = Vec::new();
                        let mut subst = Vec::new();
                        let mut variadic = false;
                        'outer: loop {
                            match next!() {
                                Token::Punct(Punctuation::LParen) if !ws => {
                                    loop {
                                        if variadic {
                                            return Err(self.error("only the last parameter of a macro may be variadic"));
                                        }
                                        match next!() {
                                            Token::Ident(name, _) => params.push(name),
                                            Token::Punct(Punctuation::Ellipsis) => {
                                                params.push("__VA_ARGS__".to_owned());  // default
                                                variadic = true;
                                            }
                                            _ => return Err(self.error("malformed macro parameters, expected name")),
                                        }
                                        match next!() {
                                            Token::Punct(Punctuation::Comma) => {}
                                            Token::Punct(Punctuation::RParen) => break,
                                            Token::Punct(Punctuation::Ellipsis) => {
                                                variadic = true;
                                                match next!() {
                                                    Token::Punct(Punctuation::RParen) => break,
                                                    _ => return Err(self.error("only the last parameter of a macro may be variadic"))
                                                }
                                            }
                                            _ => return Err(self.error("malformed macro parameters, expected comma")),
                                        }
                                    }
                                }
                                Token::Punct(Punctuation::Newline) => break 'outer,
                                Token::DocComment(doc) => docs.push(doc),
                                other => {
                                    subst.push(other);
                                }
                            }
                            loop {
                                match next!() {
                                    Token::Punct(Punctuation::Newline) => break 'outer,
                                    Token::DocComment(doc) => docs.push(doc),
                                    other => subst.push(other),
                                }
                            }
                        }
                        let define = if params.is_empty() {
                            Define::Constant { subst, docs }
                        } else {
                            Define::Function { params, subst, variadic, docs }
                        };
                        // DEBUG can only be defined in the root .dme file
                        if define_name != "DEBUG" || self.in_environment() {
                            if let Some(previous_loc) = self.defines.insert(define_name.clone(), (define_name_loc, define)) {
                                // DM doesn't issue a warning for this, but it's usually a mistake, so let's.
                                // FILE_DIR is handled specially and sometimes makes sense to define multiple times.
                                if define_name != "FILE_DIR" {
                                    DMError::new(define_name_loc, format!("macro redefined: {}", define_name))
                                        .set_severity(Severity::Warning)
                                        .with_note(previous_loc, format!("previous definition of {}", define_name))
                                        .with_errortype("macro_redefined")
                                        .register(self.context);
                                }
                            }
                        }
                    }
                    "undef" if disabled => {}
                    "undef" => {
                        expect_token!((define_name) = Token::Ident(define_name, _));
                        let define_name_loc = _last_expected_loc;
                        self.check_danger_ident(&define_name, "undefined");
                        expect_token!(() = Token::Punct(Punctuation::Newline));
                        if let Some(previous) = self.defines.remove(&define_name) {
                            self.move_to_history(define_name, previous);
                        } else {
                            DMError::new(define_name_loc, format!("macro undefined while not defined: {}", define_name))
                                .with_errortype("macro_undefined_no_definition")
                                .set_severity(Severity::Warning)
                                .register(self.context);
                        }
                    }
                    "warn" if disabled => {}
                    "warn" => {
                        expect_token!((text) = Token::String(text));
                        DMError::new(self.last_input_loc, format!("#{} {}", ident, text))
                            .set_severity(Severity::Warning)
                            .register(self.context);
                    }
                    "error" if disabled => {}
                    "error" => {
                        expect_token!((text) = Token::String(text));
                        self.context.register_error(DMError::new(self.last_input_loc, format!("#{} {}", ident, text)));
                    }
                    // none of this other stuff should even exist
                    other => {
                        let mut meant = "";
                        for each in ALL_DIRECTIVES {
                            if other.starts_with(each) && each.len() > meant.len() {
                                meant = each;
                            }
                        }
                        return Err(DMError::new(self.last_input_loc, format!("unknown directive: #{}{}{}", ident,
                            if !meant.is_empty() { ", did you mean #" } else { "" }, meant)));
                    }
                }
                // yield a newline
                self.output.push_back(Token::Punct(Punctuation::Newline));
                return Ok(());
            }
            // anything other than directives may be ifdef'd out
            _ if disabled => return Ok(()),
            // identifiers may be macros
            Token::Ident(ref ident, whitespace) => {
                self.flush_docs();

                // lint for BYOND bug
                if self.in_interp_string > 0 {
                    self.danger_idents.insert(ident.clone(), self.last_input_loc);
                }

                // substitute special macros
                if ident == "__FILE__" {
                    self.annotate_macro(ident, Location::builtins());
                    for include in self.include_stack.stack.iter().rev() {
                        if let Include::File { ref path, .. } = *include {
                            self.output.push_back(Token::String(path.display().to_string()));
                            return Ok(());
                        }
                    }
                    self.output.push_back(Token::String(String::new()));
                    return Ok(());
                } else if ident == "__LINE__" {
                    self.annotate_macro(ident, Location::builtins());
                    self.output.push_back(Token::Int(self.last_input_loc.line as i32));
                    return Ok(());
                }

                // special case for inside a defined() call
                if let Some(Token::Punct(Punctuation::LParen)) = self.output.back() {
                    if let Some(idx) = self.output.len().checked_sub(2) {
                        if let Some(Token::Ident(identname, _)) = self.output.get(idx) {
                            if identname.as_str() == "defined" {
                                self.output.push_back(Token::Ident(ident.to_owned(), whitespace));
                                return Ok(());
                            }
                        }
                    }
                }

                // if it's a define, perform the substitution
                let mut expansion = self.defines.get(ident).cloned();  // TODO: don't clone?
                if expansion.is_some() && self.include_stack.stack.len() > MAX_RECURSION_DEPTH {
                    self.error(format!("expanding {:?} would exceed max recursion depth of {} levels",
                        ident, MAX_RECURSION_DEPTH)).register(self.context);
                    expansion = None;
                }

                match expansion {
                    Some((location, Define::Constant { subst, docs: _ })) => {
                        self.annotate_macro(ident, location);
                        self.include_stack.stack.push(Include::Expansion {
                            name: ident.to_owned(),
                            tokens: subst.into_iter().collect(),
                            location: self.last_input_loc,
                        });
                        return Ok(());
                    }
                    Some((location, Define::Function { ref params, ref subst, variadic, docs: _ })) => {
                        // if it's not followed by an LParen, it isn't really a function call
                        match next!() {
                            Token::Punct(Punctuation::LParen) => {}
                            other => {
                                self.output.push_back(Token::Ident(ident.to_owned(), false));
                                match other {
                                    Token::InterpStringBegin(_) => self.in_interp_string += 1,
                                    Token::InterpStringEnd(_) => self.in_interp_string -= 1,
                                    _ => {}
                                }
                                self.output.push_back(other);
                                return Ok(());
                            }
                        }

                        self.annotate_macro(ident, location);

                        // read arguments
                        let mut args = Vec::new();
                        let mut this_arg = Vec::new();
                        let mut parens = 0;
                        loop {
                            let token = next!();
                            match token {
                                Token::Punct(Punctuation::LParen) => {
                                    parens += 1;
                                    this_arg.push(token);
                                }
                                Token::Punct(Punctuation::RParen) => {
                                    if parens == 0 {
                                        args.push(this_arg);
                                        break;
                                    }
                                    parens -= 1;
                                    this_arg.push(token);
                                }
                                Token::Punct(Punctuation::Comma) if parens == 0 => {
                                    args.push(this_arg);
                                    this_arg = Vec::new();
                                }
                                _ => this_arg.push(token),
                            }
                        }

                        // check for correct number of arguments
                        if variadic {
                            if args.len() > params.len() {
                                let new_arg = args.split_off(params.len() - 1).join(&Token::Punct(Punctuation::Comma));
                                args.push(new_arg);
                            } else if args.len() + 1 == params.len() {
                                args.push(Vec::new());
                            }
                        }
                        if args.len() != params.len() {
                            return Err(self.error("wrong number of arguments to macro call"));
                        }

                        // paste them into the expansion
                        let mut expansion = VecDeque::new();
                        let mut input = subst.iter().cloned();
                        while let Some(token) = input.next() {
                            match token {
                                // just an ident = expand it
                                Token::Ident(ident, ws) => match params.iter().position(|x| *x == ident) {
                                    Some(i) => expansion.extend(args[i].iter().cloned()),
                                    None => expansion.push_back(Token::Ident(ident, ws)),
                                },
                                // token paste = concat two idents together, if at all possible
                                Token::Punct(Punctuation::TokenPaste) => {
                                    match (expansion.pop_back(), input.next()) {
                                        (Some(Token::Ident(first, ws1)), Some(Token::Ident(param_name, ws))) => {
                                            match params.iter().position(|x| *x == param_name) {
                                                Some(i) => {
                                                    let mut arg = args[i].iter().cloned();
                                                    match arg.next() {
                                                        Some(Token::Ident(param_ident, ws)) => {
                                                            expansion.push_back(Token::Ident(
                                                                format!("{}{}", first, param_ident),
                                                                ws,
                                                            ));
                                                        }
                                                        Some(Token::Int(param_int)) => {
                                                            expansion.push_back(Token::Ident(
                                                                format!("{}{}", first, param_int),
                                                                ws,
                                                            ))
                                                        }
                                                        Some(other) => {
                                                            expansion.push_back(Token::Ident(first, ws1));
                                                            expansion.push_back(other);
                                                        }
                                                        None => {}
                                                    }
                                                    expansion.extend(arg);
                                                }
                                                None => expansion.push_back(Token::Ident(format!("{}{}", first, param_name), ws)),
                                            }
                                        }
                                        (non_ident_first, Some(Token::Ident(second, ws))) => {
                                            expansion.extend(non_ident_first);
                                            match params.iter().position(|x| *x == second) {
                                                Some(i) => expansion.extend(args[i].iter().cloned()),
                                                None => expansion.push_back(Token::Ident(second, ws)),
                                            }
                                        }
                                        (non_ident_first, non_ident_second) => {
                                            expansion.extend(non_ident_first);
                                            expansion.extend(non_ident_second);
                                        }
                                    }
                                    // read the next ident and concat it into the previous ident
                                },
                                // hash = must be followed by a param name, stringify the whole argument
                                Token::Punct(Punctuation::Hash) => {
                                    match input.next() {
                                        Some(Token::Ident(argname, _)) => match params.iter().position(|x| *x == argname) {
                                            Some(i) => {
                                                let mut string = String::new();
                                                for each in &args[i] {
                                                    use std::fmt::Write;
                                                    if !string.is_empty() {
                                                        string.push(' ');
                                                    }
                                                    let _e = write!(string, "{}", each);
                                                    #[cfg(debug_assertions)] {
                                                        _e.unwrap();
                                                    }
                                                }
                                                expansion.push_back(Token::String(string));
                                            }
                                            None => return Err(DMError::new(self.last_input_loc, format!("can't stringify non-argument ident {:?}", argname))),
                                        }
                                        Some(tok) => return Err(DMError::new(self.last_input_loc, format!("can't stringify non-ident '{}'", tok))),
                                        None => return Err(DMError::new(self.last_input_loc, "can't stringify EOF")),
                                    }
                                }
                                _ => expansion.push_back(token),
                            }
                        }
                        self.include_stack.stack.push(Include::Expansion {
                            name: ident.to_owned(),
                            tokens: expansion,
                            location: self.last_input_loc,
                        });
                        return Ok(());
                    }
                    None => {}
                }
            }
            Token::InterpStringBegin(_) => self.in_interp_string += 1,
            Token::InterpStringEnd(_) => self.in_interp_string -= 1,
            // documentation is accumulated, and flushed if no #define follows
            Token::DocComment(doc) => {
                self.docs_in.push_back((self.last_input_loc, doc));
                return Ok(());
            },
            // everything else is itself
            _ => {}
        }
        if !read.is_whitespace() {
            self.flush_docs();
        }
        self.output.push_back(read);
        Ok(())
    }
}

impl<'ctx> Iterator for Preprocessor<'ctx> {
    type Item = LocatedToken;

    fn next(&mut self) -> Option<LocatedToken> {
        loop {
            if let Some((location, doc)) = self.docs_out.pop_front() {
                return Some(LocatedToken {
                    location,
                    token: Token::DocComment(doc),
                });
            }

            if let Some(token) = self.output.pop_front() {
                return Some(LocatedToken {
                    location: self.last_input_loc,
                    token,
                });
            }

            if let Some(tok) = self.inner_next() {
                // linting for https://secure.byond.com/forum/?post=2072419
                if !tok.token.is_whitespace() && tok.token != Token::Punct(Punctuation::Hash) {
                    if tok.location.file != self.last_printable_input_loc.file ||
                        tok.location.line > self.last_printable_input_loc.line
                    {
                        self.danger_idents.clear();
                    }
                    self.last_printable_input_loc = tok.location;
                }

                // update last_input_loc and attempt to process the input token
                self.last_input_loc = tok.location;
                if let Err(e) = self.real_next(tok.token, false) {
                    self.context.register_error(e);
                }
            } else {
                while let Some(ifdef) = self.pop_ifdef() {
                    self.context.register_error(DMError::new(ifdef.location, "unterminated #if/#ifdef"));
                }
                return None;
            }
        }
    }
}
