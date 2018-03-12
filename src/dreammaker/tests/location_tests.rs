extern crate dreammaker as dm;

use dm::lexer::*;

#[test]
fn simple_location_test() {
    let code = r##"
#define islist(thing) istype(thing, /list)

/datum/globals
    var/number = 7 + 5
    var /string = foo("Hello [ "world" ]")

    proc/Init()
        world.log <<  new/   obj()
"##.trim();

    let context = Default::default();
    let located_tokens: Vec<_> = Lexer::new(&context, Default::default(), code.bytes().map(Ok)).collect();

    for token in located_tokens.iter() {
        println!("{}:{}: {:?}", token.location.line, token.location.column, token.token);
    }

    let mut reconstructed = Vec::new();
    for token in located_tokens {
        use std::fmt::Write;

        let line = token.location.line.checked_sub(1).unwrap() as usize;
        let column = token.location.column.checked_sub(1).unwrap() as usize;

        if reconstructed.len() > line + 1 {
            panic!("line numbers went backwards");
        }
        while reconstructed.len() <= line {
            reconstructed.push(String::new());
        }

        let this_line = &mut reconstructed[line];
        if this_line.len() > column {
            panic!("column numbers went backwards: line {}, so far {:?}", line + 1, this_line);
        }
        while this_line.len() < column {
            this_line.push(' ');
        }
        write!(this_line, "{}", token.token).unwrap();
    }

    let reconstructed = reconstructed.join("");
    if reconstructed.trim() != code {
        println!("{}", reconstructed);
        panic!("Some lines differed");
    }
}
