//! The preprocessor.
use std::collections::{HashMap, VecDeque};
use std::io::{self, BufReader};
use std::fs::File;
use std::path::{Path, PathBuf};

use super::lexer::*;
use super::{DMError, Location, HasLocation};

// ----------------------------------------------------------------------------
// Macro representation and predefined macros

#[derive(Debug, Clone)]
enum Define {
    Constant(Vec<Token>),
    Function(Vec<String>, Vec<Token>, bool),
}

fn default_defines(defines: &mut HashMap<String, Define>) {
    use super::lexer::Token::*;
    macro_rules! c {
        ($($i:ident = $($x:expr),*;)*) => {
            $(defines.insert(stringify!($i).into(), Define::Constant(vec![$($x),*]));)*
        }
    }
    c! {
        DM_VERSION = Int(511);

        FALSE = Int(0);
        TRUE = Int(1);

        NORTH = Int(1);
        SOUTH = Int(2);
        EAST = Int(4);
        WEST = Int(8);
        NORTHEAST = Int(5);
        SOUTHEAST = Int(6);
        NORTHWEST = Int(9);
        NORTHEAST = Int(10);

        FLOAT_LAYER = Int(-1);
        AREA_LAYER = Int(1);
        TURF_LAYER = Int(2);
        OBJ_LAYER = Int(3);
        MOB_LAYER = Int(4);
        FLY_LAYER = Int(5);
        EFFECTS_LAYER = Int(5000);
        TOPDOWN_LAYER = Int(10000);
        BACKGROUND_LAYER = Int(20000);

        ICON_ADD = Int(0);
        ICON_SUBTRACT = Int(1);
        ICON_MULTIPLY = Int(2);
        ICON_OVERLAY = Int(3);
        ICON_AND = Int(4);
        ICON_OR = Int(5);
        ICON_UNDERLAY = Int(6);

        BLEND_DEFAULT = Int(0);
        BLEND_OVERLAY = Int(1);
        BLEND_ADD = Int(2);
        BLEND_SUBTRACT = Int(3);
        BLEND_MULTIPLY = Int(4);

        NO_STEPS = Int(0);
        FORWARD_STEPS = Int(1);
        SLIDE_STEPS = Int(2);
        SYNC_STEPS = Int(3);

        BLIND = Int(1);
        SEE_MOBS = Int(4);
        SEE_OBJS = Int(8);
        SEE_TURFS = Int(16);
        SEE_SELF = Int(32);
        SEE_INFRA = Int(64);
        SEE_PIXELS = Int(256);
        SEE_THRU = Int(512);
        SEE_BLACKNESS = Int(1024);

        LONG_GLIDE = Int(1);
        RESET_COLOR = Int(2);
        RESET_ALPHA = Int(4);
        RESET_TRANSFORM = Int(8);
        NO_CLIENT_COLOR = Int(16);
        KEEP_TOGETHER = Int(32);
        KEEP_APART = Int(64);
        PLANE_MASTER = Int(128);
        TILE_BOUND = Int(256);
        PIXEL_SCALE = Int(512);

        TOPDOWN_MAP = Int(0);
        ISOMETRIC_MAP = Int(1);
        SIDE_MAP = Int(2);
        TILED_ICON_MAP = Int(32768);

        CONTROL_FREAK_ALL = Int(1);
        CONTROL_FREAK_SKIN = Int(2);
        CONTROL_FREAK_MACROS = Int(4);

        MOB_PERSPECTIVE = Int(0);
        EYE_PERSPECTIVE = Int(1);
        EDGE_PERSPECTIVE = Int(2);

        MOUSE_ACTIVE_POINTER = Int(1);

        MS_WINDOWS = String("MS Windows".into());
        UNIX = String("UNIX".into());
        MALE = String("male".into());
        FEMALE = String("female".into());
        NEUTER = String("neuter".into());
        PLURAL = String("plural".into());
    }
    // TODO: functions: ASSERT, CRASH, EXCEPTION
}

// ----------------------------------------------------------------------------
// The stack of currently #included files

#[derive(Debug)]
enum Include {
    File {
        path: PathBuf,
        file: u32,
        lexer: Lexer<BufReader<File>>,
    },
    Expansion {
        name: String,
        location: Location,
        tokens: VecDeque<Token>,
    }
}

impl Include {
    fn new(path: PathBuf, idx: u32) -> io::Result<Include> {
        Ok(Include::File {
            lexer: Lexer::new(idx, BufReader::new(File::open(&path)?)),
            file: idx,
            path: path,
        })
    }
}

impl HasLocation for Include {
    fn location(&self) -> Location {
        match self {
            &Include::File { ref lexer, .. } => lexer.location(),
            &Include::Expansion { location, .. } => location,
        }
    }
}

struct IncludeStack {
    stack: Vec<Include>,
}

impl IncludeStack {
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

impl HasLocation for IncludeStack {
    fn location(&self) -> Location {
        if let Some(include) = self.stack.last() {
            include.location()
        } else {
            Location {
                file: 0,
                line: 0,
                column: 0,
            }
        }
    }
}

impl Iterator for IncludeStack {
    type Item = Result<LocatedToken, DMError>;

    fn next(&mut self) -> Option<Result<LocatedToken, DMError>> {
        loop {
            match self.stack.last_mut() {
                Some(&mut Include::File { ref mut lexer, .. }) => match lexer.next() {
                    Some(Err(e)) => return Some(Err(e)),
                    Some(Ok(t)) => return Some(Ok(t)),
                    None => {} // fall through
                }
                Some(&mut Include::Expansion { ref mut tokens, location, .. }) => match tokens.pop_front() {
                    Some(token) => return Some(Ok(LocatedToken { location, token })),
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

struct Ifdef {
    active: bool,
    chain_active: bool,
}

impl Ifdef {
    fn new(active: bool) -> Ifdef {
        Ifdef { active, chain_active: active }
    }
    fn else_(self) -> Ifdef {
        Ifdef { active: !self.active, chain_active: true }
    }
    fn else_if(self, active: bool) -> Ifdef {
        Ifdef { active, chain_active: self.chain_active || active }
    }
}

/// C-like preprocessor for DM. Expands directives and macro invocations.
pub struct Preprocessor {
    env_file: PathBuf,
    defines: HashMap<String, Define>,
    files: Vec<PathBuf>,
    maps: Vec<PathBuf>,
    skins: Vec<PathBuf>,
    include_stack: IncludeStack,
    ifdef_stack: Vec<Ifdef>,

    last_input_loc: Location,
    output: VecDeque<Token>,
}

impl HasLocation for Preprocessor {
    fn location(&self) -> Location {
        self.include_stack.location()
    }
}

impl Preprocessor {
    pub fn new(env_file: PathBuf) -> io::Result<Self> {
        let mut pp = Preprocessor {
            files: vec![env_file.clone()],
            include_stack: IncludeStack {
                stack: vec![Include::new(env_file.clone(), 0)?],
            },
            env_file: env_file,
            defines: Default::default(),
            maps: Default::default(),
            skins: Default::default(),
            ifdef_stack: Default::default(),
            last_input_loc: Default::default(),
            output: Default::default(),
        };
        default_defines(&mut pp.defines);
        Ok(pp)
    }

    pub fn file_path(&self, file: u32) -> &Path {
        &self.files[file as usize]
    }

    fn is_disabled(&self) -> bool {
        self.ifdef_stack.iter().any(|x| !x.active)
    }

    fn inner_next(&mut self) -> Option<Result<LocatedToken, DMError>> {
        self.include_stack.next()
    }

    fn evaluate(&mut self) -> Result<bool, DMError> {
        // TODO: read until Newline and evaluate that expression
        //println!("#if {:?}", self.location());
        Ok(false)
    }

    #[allow(unreachable_code)]
    fn real_next(&mut self, read: Token) -> Result<(), DMError> {
        macro_rules! next {
            () => {
                match self.inner_next() {
                    Some(Ok(x)) => {
                        x.token
                    }
                    Some(Err(e)) => return Err(e),
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
                        let z = self.defines.contains_key(&define_name);
                        self.ifdef_stack.push(Ifdef::new(z));
                    }
                    "ifndef" => {
                        expect_token!((define_name) = Token::Ident(define_name, _));
                        expect_token!(() = Token::Punct(Punctuation::Newline));
                        let z = !self.defines.contains_key(&define_name);
                        self.ifdef_stack.push(Ifdef::new(z));
                    }
                    "if" => {
                        let z = self.evaluate()?;
                        self.ifdef_stack.push(Ifdef::new(z));
                    }
                    "elseif" => {
                        let last = self.ifdef_stack.pop().ok_or_else(||
                            DMError::new(self.last_input_loc, "unmatched #elseif"))?;
                        let z = self.evaluate()?;
                        self.ifdef_stack.push(last.else_if(z));
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
                            enum FileType { DMM, DMF, DM }
                            let len = self.files.len() as u32;
                            self.files.push(each.clone());
                            match match each.extension().and_then(|s| s.to_str()) {
                                Some("dmm") => FileType::DMM,
                                Some("dmf") => FileType::DMF,
                                Some("dm") => FileType::DM,
                                e => return Err(DMError::new(self.last_input_loc, format!("unknown file type {:?}", e))),
                            } {
                                FileType::DMM => self.maps.push(each),
                                FileType::DMF => self.skins.push(each),
                                FileType::DM => self.include_stack.stack.push(Include::new(each, len)?),
                            }
                            return Ok(());
                        }
                        return Err(self.error("failed to find file"));
                    }
                    // both constant and function defines
                    "define" => {
                        expect_token!((define_name, ws) = Token::Ident(define_name, ws));
                        let mut args = Vec::new();
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
                                            Token::Ident(name, _) => args.push(name),
                                            Token::Punct(Punctuation::Ellipsis) => {
                                                args.push("__VA_ARGS__".to_owned());  // default
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
                        if args.is_empty() {
                            self.defines.insert(define_name, Define::Constant(subst));
                        } else {
                            self.defines.insert(define_name, Define::Function(args, subst, variadic));
                        }
                    }
                    "undef" => {
                        expect_token!((define_name) = Token::Ident(define_name, _));
                        expect_token!(() = Token::Punct(Punctuation::Newline));
                        self.defines.remove(&define_name); // TODO: warn if none
                    }
                    "warning" | "warn" | "error" => {
                        // TODO: report warnings as warnings rather than errors
                        expect_token!((text) = Token::String(text));
                        return Err(DMError::new(self.last_input_loc, format!("#{} {}", ident, text)));
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
                    Some(Define::Constant(subst)) => {
                        let e = Include::Expansion {
                            name: ident.to_owned(),
                            tokens: subst.into_iter().collect(),
                            location: self.last_input_loc,
                        };
                        self.include_stack.stack.push(e);
                        return Ok(());
                    }
                    Some(Define::Function(ref params, ref subst, variadic)) => {
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
                                            None => return Err(DMError::new(self.last_input_loc, "can only stringify arguments"))
                                        }
                                        _ => return Err(DMError::new(self.last_input_loc, "can only stringify arguments"))
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

impl Iterator for Preprocessor {
    type Item = Result<LocatedToken, DMError>;

    fn next(&mut self) -> Option<Result<LocatedToken, DMError>> {
        loop {
            if let Some(token) = self.output.pop_front() {
                return Some(Ok(LocatedToken {
                    location: self.last_input_loc,
                    token: token,
                }));
            }

            if let Some(tok) = self.inner_next() {
                let tok = try_iter!(tok);
                self.last_input_loc = tok.location;
                try_iter!(self.real_next(tok.token));
            } else {
                return None;
            }
        }
    }
}
