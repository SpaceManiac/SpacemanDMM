//! The preprocessor.
use std::collections::{HashMap, VecDeque};
use std::io;
use std::fs::File;
use std::path::{Path, PathBuf};

use interval_tree::{IntervalTree, range};

use super::lexer::*;
use super::{DMError, Location, HasLocation, FileId, Context, Severity};

// ----------------------------------------------------------------------------
// Macro representation and predefined macros

#[derive(Debug, Clone, PartialEq)]
pub enum Define {
    Constant {
        subst: Vec<Token>,
    },
    Function {
        params: Vec<String>,
        subst: Vec<Token>,
        variadic: bool,
    },
}

/// An interval tree representing historic macro definitions.
pub type DefineHistory = IntervalTree<Location, (String, Define)>;

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
    /// Returns the number of elements in the map.
    pub fn len(&self) -> usize {
        self.inner.len()
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
    fn from_history(history: &DefineHistory, location: Location) -> DefineMap {
        let mut map = DefineMap::default();
        for (range, &(ref name, ref define)) in history.range(range(location, location)) {
            map.insert(name.clone(), (range.start, define.clone()));
        }
        map
    }

    /// Test whether two DefineMaps are equal, ignoring definition locations.
    fn equals(&self, rhs: &DefineMap) -> bool {
        if self.len() != rhs.len() {
            return false;
        }

        self.inner.iter().all(|(key, value)| rhs.inner.get(key).map_or(false, |v| {
            if value.len() != v.len() {
                return false;
            }
            value.iter().zip(v.iter()).all(|(lhs, rhs)| lhs.1 == rhs.1)
        }))
    }
}

// ----------------------------------------------------------------------------
// The stack of currently #included files

#[derive(Debug)]
enum Include<'ctx> {
    File {
        path: PathBuf,
        file: FileId,
        lexer: Lexer<'ctx, io::Bytes<Box<io::Read>>>,
    },
    Expansion {
        name: String,
        location: Location,
        tokens: VecDeque<Token>,
    }
}

impl<'ctx> Include<'ctx> {
    fn new(context: &'ctx Context, path: PathBuf) -> io::Result<Include> {
        let reader = io::BufReader::new(File::open(&path)?);
        let idx = context.register_file(path.clone());
        Ok(Include::File {
            lexer: Lexer::from_read(context, idx, Box::new(reader)),
            file: idx,
            path: path,
        })
    }
}

impl<'ctx> HasLocation for Include<'ctx> {
    fn location(&self) -> Location {
        match self {
            &Include::File { ref lexer, .. } => lexer.location(),
            &Include::Expansion { location, .. } => location,
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
            if let &Include::File { ref path, .. } = each {
                return path;
            }
        }
        "".as_ref()
    }
    fn top_no_expand(&self) -> &str {
        for each in self.stack.iter().rev() {
            if let &Include::Expansion { ref name, .. } = each {
                return name;
            }
        }
        ""
    }
}

impl<'ctx> HasLocation for IncludeStack<'ctx> {
    fn location(&self) -> Location {
        if let Some(include) = self.stack.last() {
            include.location()
        } else {
            Location::default()
        }
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
                }
                Some(&mut Include::Expansion { ref mut tokens, location, .. }) => match tokens.pop_front() {
                    Some(token) => return Some(LocatedToken { location, token }),
                    None => {} // fall through
                }
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
        Ifdef { location, active, chain_active: active }
    }
    fn else_(self) -> Ifdef {
        Ifdef { location: self.location, active: !self.active, chain_active: true }
    }
    fn else_if(self, active: bool) -> Ifdef {
        Ifdef { location: self.location, active, chain_active: self.chain_active || active }
    }
}

#[derive(Debug)]
/// C-like preprocessor for DM. Expands directives and macro invocations.
pub struct Preprocessor<'ctx> {
    context: &'ctx Context,

    env_file: PathBuf,
    history: DefineHistory,
    defines: DefineMap,
    maps: Vec<PathBuf>,
    skins: Vec<PathBuf>,
    include_stack: IncludeStack<'ctx>,
    ifdef_stack: Vec<Ifdef>,

    last_input_loc: Location,
    output: VecDeque<Token>,
}

impl<'ctx> HasLocation for Preprocessor<'ctx> {
    fn location(&self) -> Location {
        self.include_stack.location()
    }
}

impl<'ctx> Preprocessor<'ctx> {
    /// Create a new preprocessor from the given Context and environment file.
    pub fn new(context: &'ctx Context, env_file: PathBuf) -> io::Result<Self> {
        let mut pp = Preprocessor {
            context,
            env_file: env_file.clone(),
            include_stack: IncludeStack {
                stack: vec![Include::new(context, env_file)?],
            },
            history: Default::default(),
            defines: Default::default(),
            maps: Default::default(),
            skins: Default::default(),
            ifdef_stack: Default::default(),
            last_input_loc: Default::default(),
            output: Default::default(),
        };
        super::builtins::default_defines(&mut pp.defines);
        Ok(pp)
    }

    /// Move all active defines to the define history.
    pub fn finalize(&mut self) {
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
    }

    /// Access the define history. Will be incomplete until finalized.
    pub fn history(&self) -> &DefineHistory {
        &self.history
    }

    /// Branch a child preprocessor from this preprocessor's historic state at
    /// the start of the given file.
    pub fn branch_at_file<'ctx2>(&self, file: FileId, context: &'ctx2 Context) -> Preprocessor<'ctx2> {
        let location = Location { file, line: 0, column: 0 };
        let defines = DefineMap::from_history(&self.history, location);

        Preprocessor {
            context: context,
            env_file: self.env_file.clone(),
            include_stack: Default::default(),
            history: Default::default(),  // TODO: support branching a second time
            defines,
            maps: Default::default(),
            skins: Default::default(),
            ifdef_stack: Default::default(),  // should be fine
            last_input_loc: location,
            output: Default::default(),
        }
    }

    /// Check whether this preprocessor's state as of the end of the given file
    /// matches the given child preprocessor.
    pub fn matches_end_of_file(&self, file: FileId, other: &Preprocessor) -> bool {
        let location = Location { file, line: !0, column: !0 };
        let defines = DefineMap::from_history(&self.history, location);
        defines.equals(&other.defines)
    }

    /// Push a DM file to the top of this preprocessor's stack.
    pub fn push_file<R: io::Read + 'static>(&mut self, path: PathBuf, read: R) -> FileId {
        let idx = self.context.register_file(path.clone());
        self.include_stack.stack.push(Include::File {
            lexer: Lexer::from_read(self.context, idx, Box::new(read)),
            file: idx,
            path,
        });
        idx
    }

    // ------------------------------------------------------------------------
    // Internal utilities

    fn is_disabled(&self) -> bool {
        self.ifdef_stack.iter().any(|x| !x.active)
    }

    fn inner_next(&mut self) -> Option<LocatedToken> {
        self.include_stack.next()
    }

    fn evaluate(&mut self) -> Result<bool, DMError> {
        // TODO: read until Newline and evaluate that expression
        //println!("#if {:?}", self.location());
        Ok(false)
    }

    fn move_to_history(&mut self, name: String, previous: (Location, Define)) {
        self.history.insert(range(previous.0, self.last_input_loc), (name, previous.1));
    }

    #[allow(unreachable_code)]
    fn real_next(&mut self, read: Token) -> Result<(), DMError> {
        let mut _last_expected_loc = self.last_input_loc;
        macro_rules! next {
            () => {
                match self.inner_next() {
                    Some(x) => { _last_expected_loc = x.location; x.token },
                    None => return Err(self.error("unexpected EOF"))
                }
            }
        }
        macro_rules! expect_token {
            (($($i:ident),*) = $p:pat) => {
                let ($($i,)*) = match next!() {
                    $p => ($($i,)*),
                    other => return Err(self.error(format!("unexpected token {:?}, expecting {}", other, stringify!($p))))
                };
            }
        }

        match read {
            Token::Punct(Punctuation::Hash) => {
                // preprocessor directive, next thing ought to be an ident
                expect_token!((ident) = Token::Ident(ident, _));
                match &ident[..] {
                    // ifdefs
                    "endif" => {
                        self.ifdef_stack.pop().ok_or_else(||
                            DMError::new(self.last_input_loc, "unmatched #endif"))?;
                    }
                    "else" => {
                        let last = self.ifdef_stack.pop().ok_or_else(||
                            DMError::new(self.last_input_loc, "unmatched #else"))?;
                        self.ifdef_stack.push(last.else_());
                    }
                    "ifdef" => {
                        expect_token!((define_name) = Token::Ident(define_name, _));
                        expect_token!(() = Token::Punct(Punctuation::Newline));
                        let enabled = self.defines.contains_key(&define_name);
                        self.ifdef_stack.push(Ifdef::new(self.last_input_loc, enabled));
                    }
                    "ifndef" => {
                        expect_token!((define_name) = Token::Ident(define_name, _));
                        expect_token!(() = Token::Punct(Punctuation::Newline));
                        let enabled = !self.defines.contains_key(&define_name);
                        self.ifdef_stack.push(Ifdef::new(self.last_input_loc, enabled));
                    }
                    "if" => {
                        let enabled = self.evaluate()?;
                        self.ifdef_stack.push(Ifdef::new(self.last_input_loc, enabled));
                    }
                    "elseif" => {
                        let last = self.ifdef_stack.pop().ok_or_else(||
                            DMError::new(self.last_input_loc, "unmatched #elseif"))?;
                        let enabled = self.evaluate()?;
                        self.ifdef_stack.push(last.else_if(enabled));
                    }
                    // anything other than ifdefs may be ifdef'd out
                    _ if self.is_disabled() => {}
                    // include searches relevant paths for files
                    "include" => {
                        expect_token!((path) = Token::String(path));
                        expect_token!(() = Token::Punct(Punctuation::Newline));
                        #[cfg(windows)]
                        let path = PathBuf::from(path);
                        #[cfg(unix)]
                        let path = PathBuf::from(path.replace("\\", "/"));

                        for each in vec![
                            self.env_file.parent().unwrap().join(&path),
                            self.include_stack.top_file_path().parent().unwrap().join(&path),
                            path,
                        ].into_iter().rev() {
                            if !each.exists() { continue }
                            // Wacky construct is used to let go of the borrow
                            // of `each` so it can be used in the second half.
                            enum FileType { DMM, DMF, DM }
                            match match each.extension().and_then(|s| s.to_str()) {
                                Some("dmm") => FileType::DMM,
                                Some("dmf") => FileType::DMF,
                                Some("dm") => FileType::DM,
                                Some(ext) => {
                                    self.context.register_error(DMError::new(self.last_input_loc, format!("unknown extension {:?}", ext)));
                                    return Ok(());
                                }
                                None => {
                                    self.context.register_error(DMError::new(self.last_input_loc, "filename"));
                                    return Ok(());
                                }
                            } {
                                FileType::DMM => self.maps.push(each),
                                FileType::DMF => self.skins.push(each),
                                FileType::DM => match Include::new(self.context, each) {
                                    Ok(include) => self.include_stack.stack.push(include),
                                    Err(e) => self.context.register_error(DMError::new(self.last_input_loc,
                                        "failed to open file").set_cause(e)),
                                },
                            }
                            return Ok(());
                        }
                        return Err(self.error("failed to find file"));
                    }
                    // both constant and function defines
                    "define" => {
                        expect_token!((define_name, ws) = Token::Ident(define_name, ws));
                        let define_name_loc = _last_expected_loc;
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
                                            _ => return Err(self.error("malformed macro parameters, expected name"))
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
                                            _ => return Err(self.error("malformed macro parameters, expected comma"))
                                        }
                                    }
                                }
                                Token::Punct(Punctuation::Newline) => break 'outer,
                                other => {
                                    subst.push(other);
                                }
                            }
                            loop {
                                match next!() {
                                    Token::Punct(Punctuation::Newline) => break 'outer,
                                    other => subst.push(other),
                                }
                            }
                        }
                        let define = if params.is_empty() {
                            Define::Constant { subst }
                        } else {
                            Define::Function { params, subst, variadic }
                        };
                        if let Some(previous_loc) = self.defines.insert(define_name.clone(), (define_name_loc, define)) {
                            // DM doesn't issue a warning for this, but it's usually a mistake, so let's
                            self.context.register_error(DMError::new(define_name_loc,
                                format!("macro redefined: {}", define_name)).set_severity(Severity::Warning));
                            self.context.register_error(DMError::new(previous_loc,
                                "previous definition").set_severity(Severity::Info));
                        }
                    }
                    "undef" => {
                        expect_token!((define_name) = Token::Ident(define_name, _));
                        let define_name_loc = _last_expected_loc;
                        expect_token!(() = Token::Punct(Punctuation::Newline));
                        if let Some(previous) = self.defines.remove(&define_name) {
                            self.move_to_history(define_name, previous);
                        } else {
                            self.context.register_error(DMError::new(define_name_loc,
                                format!("macro undefined while not defined: {}", define_name)
                            ).set_severity(Severity::Warning));
                        }
                    }
                    "warning" | "warn" => {
                        expect_token!((text) = Token::String(text));
                        self.context.register_error(DMError::new(self.last_input_loc, format!("#{} {}", ident, text))
                            .set_severity(Severity::Warning));
                    }
                    "error" => {
                        expect_token!((text) = Token::String(text));
                        self.context.register_error(DMError::new(self.last_input_loc, format!("#{} {}", ident, text)));
                    }
                    // none of this other stuff should even exist
                    _ => return Err(DMError::new(self.last_input_loc, format!("unknown directive: #{}", ident)))
                }
                // yield a newline
                self.output.push_back(Token::Punct(Punctuation::Newline));
                return Ok(());
            }
            // anything other than directives may be ifdef'd out
            _ if self.is_disabled() => return Ok(()),
            // identifiers may be macros
            Token::Ident(ref ident, _) if ident != self.include_stack.top_no_expand() => {
                // if it's a define, perform the substitution
                match self.defines.get(ident).cloned() { // TODO
                    Some((_, Define::Constant { subst })) => {
                        let e = Include::Expansion {
                            name: ident.to_owned(),
                            tokens: subst.into_iter().collect(),
                            location: self.last_input_loc,
                        };
                        self.include_stack.stack.push(e);
                        return Ok(());
                    }
                    Some((_, Define::Function { ref params, ref subst, variadic })) => {
                        // if it's not followed by an LParen, it isn't really a function call
                        match next!() {
                            Token::Punct(Punctuation::LParen) => {}
                            other => {
                                self.output.push_back(Token::Ident(ident.to_owned(), false));
                                self.output.push_back(other);
                                return Ok(());
                            }
                        }

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
                            return Err(self.error("wrong number of arguments to macro call"))
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
                                        (Some(Token::Ident(first, ws1)), Some(Token::Ident(second, ws))) => {
                                            match params.iter().position(|x| *x == second) {
                                                Some(i) => {
                                                    let mut arg = args[i].iter().cloned();
                                                    match arg.next() {
                                                        Some(Token::Ident(second, ws)) => {
                                                            expansion.push_back(Token::Ident(format!("{}{}", first, second), ws));
                                                        }
                                                        Some(other) => {
                                                            expansion.push_back(Token::Ident(first, ws1));
                                                            expansion.push_back(other);
                                                        }
                                                        None => {}
                                                    }
                                                    expansion.extend(arg);
                                                }
                                                None => expansion.push_back(Token::Ident(format!("{}{}", first, second), ws)),
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
                        let e = Include::Expansion {
                            name: ident.to_owned(),
                            tokens: expansion,
                            location: self.last_input_loc,
                        };
                        self.include_stack.stack.push(e);
                        return Ok(());
                    }
                    None => {}
                }
            }
            // everything else is itself
            _ => {}
        }
        self.output.push_back(read);
        Ok(())
    }
}

impl<'ctx> Iterator for Preprocessor<'ctx> {
    type Item = LocatedToken;

    fn next(&mut self) -> Option<LocatedToken> {
        loop {
            if let Some(token) = self.output.pop_front() {
                return Some(LocatedToken {
                    location: self.last_input_loc,
                    token: token,
                });
            }

            if let Some(tok) = self.inner_next() {
                self.last_input_loc = tok.location;
                if let Err(e) = self.real_next(tok.token) {
                    self.context.register_error(e);
                }
            } else {
                while let Some(ifdef) = self.ifdef_stack.pop() {
                    self.context.register_error(DMError::new(ifdef.location, "unterminated #if/#ifdef")
                        .set_severity(Severity::Warning));
                }
                return None;
            }
        }
    }
}
