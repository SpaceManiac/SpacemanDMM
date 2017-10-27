//! The constant folder/evaluator, used by the preprocessor and object tree.

use super::lexer::{Token, LocatedToken};
use super::{DMError, Location};

enum ConstFn {

}

pub fn fold(location: Location, value: &[Token]) -> Result<(), DMError> {
    println!("{:?}", value);
    {let so = ::std::io::stdout();
    super::pretty_print(&mut so.lock(), value.iter().cloned().map(Ok), false)?;}
    println!();

    let mut iter = value.iter();
    let mut parser = super::parser::Parser::new(iter.by_ref().map(|i| Ok(LocatedToken::new(location, i.clone()))));
    match parser.expression() {
        Ok(Some(v)) => println!("{:?}", v),
        Ok(None) => println!("greppable NONE"),
        Err(e) => println!("greppable ERR {:?}", e),
    }
    println!();

    Ok(())
}
