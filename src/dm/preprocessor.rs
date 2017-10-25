//! The preprocessor.
use std::collections::{HashMap, VecDeque};
use std::io::{self, BufReader};
use std::fs::File;
use std::path::{Path, PathBuf};

use super::lexer::*;
use super::DMError;

#[derive(Debug)]
enum Define {
    Constant(Vec<Token>),
    Function(Vec<String>, Vec<Token>),
}

fn default_defines(defines: &mut HashMap<String, Define>) {
    use dm::lexer::Token::*;
    macro_rules! constant {
        ($i:ident [$($x:expr),*]) => {
            defines.insert(stringify!($i).into(), Define::Constant(vec![$($x),*]));
        }
    }

    constant!(DM_VERSION [Int(511)]);

    constant!(FALSE [Int(0)]);
    constant!(TRUE [Int(1)]);

    constant!(TILE_BOUND [Int(256)]);
    constant!(PIXEL_SCALE [Int(512)]);
}

struct Include {
    path: PathBuf,
    lexer: Lexer<BufReader<File>>,
    trailed: bool,
}

impl Include {
    fn new(path: PathBuf) -> io::Result<Include> {
        Ok(Include {
            lexer: Lexer::new(BufReader::new(File::open(&path)?)),
            path: path,
            trailed: false,
        })
    }
}

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
    maps: Vec<PathBuf>,
    skins: Vec<PathBuf>,
    include_stack: Vec<Include>,
    ifdef_stack: Vec<Ifdef>,
    pasting: Option<(usize, usize, VecDeque<Token>)>,
}

impl Preprocessor {
    pub fn new(env_file: PathBuf) -> io::Result<Self> {
        let mut pp = Preprocessor {
            defines: Default::default(),
            maps: Default::default(),
            skins: Default::default(),
            include_stack: vec![Include::new(env_file.clone())?],
            ifdef_stack: Default::default(),
            env_file: env_file,
            pasting: None,
        };
        default_defines(&mut pp.defines);
        Ok(pp)
    }

    pub fn location(&self) -> (&Path, usize, usize) {
        if let Some(include) = self.include_stack.last() {
            let (line, column) = include.lexer.location();
            (&include.path, line, column)
        } else {
            (&self.env_file, 0, 0)
        }
    }

    fn is_disabled(&self) -> bool {
        self.ifdef_stack.iter().any(|x| !x.active)
    }

    fn inner_next(&mut self) -> Option<Result<LocatedToken, DMError>> {
        loop {
            if let Some(include) = self.include_stack.last_mut() {
                match include.lexer.next() {
                    Some(Err(e)) => return Some(Err(e)),
                    Some(Ok(t)) => {
                        include.trailed = match t.token {
                            Token::Punct(Punctuation::Newline) => true,
                            _ => false,
                        };
                        return Some(Ok(t));
                    }
                    None if !include.trailed => {
                        // pretend every file ends with a blank line, so that
                        // stuff that goes looking for newlines works
                        include.trailed = true;
                        return Some(Ok(LocatedToken::new(0, 0, Token::Punct(Punctuation::Newline))));
                    }
                    None => {}
                }
            } else {
                return None;
            }
            self.include_stack.pop();
        }
    }

    fn evaluate(&mut self) -> Result<bool, DMError> {
        // TODO: read until Newline and evaluate that expression
        //println!("#if {:?}", self.location());
        Ok(false)
    }

    #[allow(unreachable_code)]
    fn real_next(&mut self, read: LocatedToken) -> Option<Result<LocatedToken, DMError>> {
        let (mut line, mut column) = (read.line, read.column);

        macro_rules! next {
            () => {
                match self.inner_next() {
                    Some(Ok(x)) => {
                        line = x.line;
                        column = x.column;
                        x.token
                    }
                    Some(Err(e)) => return Some(Err(e)),
                    None => return Some(Err(DMError::new(line, column, "unexpected EOF")))
                }
            }
        }
        macro_rules! expect_token {
            ($($t:tt)*) => {
                guard!(let $($t)* = next!() else {
                    return Some(Err(DMError::new(line, column, "unexpected token")))
                })
            }
        }

        match read.token {
            Token::Punct(Punctuation::Hash) => {
                // preprocessor directive, next thing ought to be an ident
                expect_token!(Token::Ident(ident, _));
                match &ident[..] {
                    // ifdefs
                    "endif" => {
                        self.ifdef_stack.pop().unwrap();
                    }
                    "else" => {
                        let last = self.ifdef_stack.pop().unwrap();
                        self.ifdef_stack.push(last.else_());
                    }
                    "ifdef" => {
                        expect_token!(Token::Ident(define_name, _));
                        expect_token!(Token::Punct(Punctuation::Newline));
                        let z = self.defines.contains_key(&define_name);
                        self.ifdef_stack.push(Ifdef::new(z));
                    }
                    "ifndef" => {
                        expect_token!(Token::Ident(define_name, _));
                        expect_token!(Token::Punct(Punctuation::Newline));
                        let z = !self.defines.contains_key(&define_name);
                        self.ifdef_stack.push(Ifdef::new(z));
                    }
                    "if" => {
                        let z = try_iter!(self.evaluate());
                        self.ifdef_stack.push(Ifdef::new(z));
                    }
                    "elseif" => {
                        let last = self.ifdef_stack.pop().unwrap();
                        let z = try_iter!(self.evaluate());
                        self.ifdef_stack.push(last.else_if(z));
                    }
                    // anything other than ifdefs may be ifdef'd out
                    _ if self.is_disabled() => {}
                    // include searches relevant paths for files
                    "include" => {
                        expect_token!(Token::String(path));
                        expect_token!(Token::Punct(Punctuation::Newline));
                        let path = PathBuf::from(path);

                        for each in vec![
                            self.env_file.parent().unwrap().join(&path),
                            self.include_stack.last().unwrap().path.parent().unwrap().join(&path),
                            path,
                        ].into_iter().rev() {
                            if !each.exists() { continue }
                            enum FileType { DMM, DMF, DM }
                            match match each.extension().and_then(|s| s.to_str()) {
                                Some("dmm") => FileType::DMM,
                                Some("dmf") => FileType::DMF,
                                Some("dm") => FileType::DM,
                                e => unimplemented!("extension {:?}", e),
                            } {
                                FileType::DMM => self.maps.push(each),
                                FileType::DMF => self.skins.push(each),
                                FileType::DM => self.include_stack.push(try_iter!(Include::new(each))),
                            }
                            return None;
                        }
                        return Some(Err(DMError::new(line, column, "failed to find file")));
                    }
                    // both constant and function defines
                    "define" => {
                        expect_token!(Token::Ident(define_name, ws));
                        //let mut args = Vec::new();
                        let mut subst = Vec::new();
                        'outer: loop {
                            match next!() {
                                /*Token::Punct(Punctuation::LParen) if !ws => {
                                    unimplemented!("#define {}()", define_name);
                                }*/
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
                        self.defines.insert(define_name, Define::Constant(subst));
                    }
                    "undef" => {
                        expect_token!(Token::Ident(define_name, _));
                        expect_token!(Token::Punct(Punctuation::Newline));
                        self.defines.remove(&define_name); // TODO: warn if none
                    }
                    "error" => {
                        panic!("#error {}");
                    }
                    // none of this other stuff should even exist
                    _ => unimplemented!("{}", ident),
                }
                // yield a newline
                return Some(Ok(LocatedToken::new(line, column, Token::Punct(Punctuation::Newline))));
            }
            // anything other than directives may be ifdef'd out
            _ if self.is_disabled() => return None,
            // identifiers may be macros
            Token::Ident(ref ident, _) => {
                // if it's a define, perform the substitution
                match self.defines.get(ident) {
                    Some(&Define::Constant(ref subst)) => {
                        assert!(self.pasting.is_none());
                        self.pasting = Some((line, column, subst.iter().cloned().collect()));
                        return None;
                    }
                    Some(&Define::Function(ref params, ref subst)) => unimplemented!("{}()", ident),
                    None => {}
                }
            }
            // everything else is itself
            _ => {}
        }
        Some(Ok(read))
    }
}

impl Iterator for Preprocessor {
    type Item = Result<LocatedToken, DMError>;

    fn next(&mut self) -> Option<Result<LocatedToken, DMError>> {
        loop {
            if let Some(&mut (line, column, ref mut pasting)) = self.pasting.as_mut() {
                if let Some(tok) = pasting.pop_front() {
                    return Some(Ok(LocatedToken::new(line, column, tok)));
                }
            }
            self.pasting = None;
            if let Some(tok) = self.inner_next() {
                if let Some(out) = self.real_next(try_iter!(tok)) {
                    return Some(out);
                }
                continue;
            }
            return None
        }
    }
}
