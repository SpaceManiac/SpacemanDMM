//! The object tree representation, used as a parsing target.

use super::lexer::Token;
use super::{DMError, Location};

#[derive(Debug, Default)]
pub struct ObjectTree;

// path keywords: /var /proc /global /static /tmp

impl ObjectTree {
    // an entry which may be anything depending on the path
    pub fn add_entry<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, path: I) -> Result<(), DMError> {
        for each in path {
            print!("/{}", each);
        }
        println!();
        Ok(())
    }

    // an entry which is definitely a var because a value is specified
    pub fn add_var<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, path: I, value: Vec<Token>) -> Result<(), DMError> {
        for each in path {
            print!("/{}", each);
        }
        print!(" = ");
        {
            let so = ::std::io::stdout();
            super::pretty_print(&mut so.lock(), value.iter().cloned().map(Ok), false)?;
        }
        println!();
        Ok(())
    }

    // an entry which is definitely a proc because an argument list is specified
    pub fn add_proc<'a, I: Iterator<Item=&'a str>>(&mut self, location: Location, path: I) -> Result<(), DMError> {
        /*for each in path {
            print!("/{}", each);
        }
        println!("()");*/
        Ok(())
    }
}
