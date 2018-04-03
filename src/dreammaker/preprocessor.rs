//! The preprocessor.
use std::collections::{HashMap, VecDeque};
use std::io;
use std::fs::File;
use std::path::{Path, PathBuf};

use super::lexer::*;
use super::{DMError, Location, HasLocation, FileId, Context, Severity};

// ----------------------------------------------------------------------------
// Macro representation and predefined macros

#[derive(Debug, Clone)]
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

// ----------------------------------------------------------------------------
// The stack of currently #included files

#[derive(Debug)]
enum Include<'ctx> {
    File {
        path: PathBuf,
        file: FileId,
        lexer: Lexer<'ctx, io::Bytes<io::BufReader<File>>>,
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
            lexer: Lexer::from_read(context, idx, reader),
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

#[derive(Debug)]
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

/// C-like preprocessor for DM. Expands directives and macro invocations.
pub struct Preprocessor<'ctx> {
    context: &'ctx Context,

    env_file: PathBuf,
    defines: HashMap<String, Define>,
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
    pub fn new(context: &'ctx Context, env_file: PathBuf) -> io::Result<Self> {
        let mut pp = Preprocessor {
            context,
            env_file: env_file.clone(),
            include_stack: IncludeStack {
                stack: vec![Include::new(context, env_file)?],
            },
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

    #[allow(unreachable_code)]
    fn real_next(&mut self, read: Token) -> Result<(), DMError> {
        macro_rules! next {
            () => {
                match self.inner_next() {
                    Some(x) => x.token,
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
                        self.context.register_define(define_name.clone(), self.last_input_loc);
                        if params.is_empty() {
                            self.defines.insert(define_name, Define::Constant { subst });
                        } else {
                            self.defines.insert(define_name, Define::Function { params, subst, variadic });
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
                    Some(Define::Constant { subst }) => {
                        let e = Include::Expansion {
                            name: ident.to_owned(),
                            tokens: subst.into_iter().collect(),
                            location: self.last_input_loc,
                        };
                        self.include_stack.stack.push(e);
                        return Ok(());
                    }
                    Some(Define::Function { ref params, ref subst, variadic }) => {
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
