//! The constant folder/evaluator, used by the preprocessor and object tree.

use super::lexer::Token;
use super::{DMError, Location};

enum ConstFn {

}

pub fn fold(value: &[Token]) -> Result<(), DMError> {
    let mut prev = &value[0];
    for each in value.iter() {
        if *each == Token::Punct(super::lexer::Punctuation::LParen) {
            print!(">> {:?} :: ", prev);
            {let so = ::std::io::stdout();
            super::pretty_print(&mut so.lock(), value.iter().cloned().map(Ok), false)?;}
            println!();
        }
        prev = each;
    }
    Ok(())
}
