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

/var/foo = bar
"##.trim();

    let context = Default::default();
    let located_tokens: Vec<_> = Lexer::new(&context, Default::default(), code.as_bytes()).collect();
    context.assert_success();

    assert_eq!(located_tokens[0].location.line, 1);
    assert_eq!(located_tokens[0].location.column, 1);

    println!("---- lexer ----");
    for token in located_tokens.iter() {
        println!("{}:{}: {:?}", token.location.line, token.location.column, token.token);
    }

    let reconstructed = reconstruct(&located_tokens, false);
    if reconstructed.trim() != code {
        println!("{}", reconstructed);
        panic!("Some lines differed");
    }

    println!("---- indent processor ----");
    let indented_tokens: Vec<_> = dm::indents::IndentProcessor::new(&context, located_tokens).collect();
    context.assert_success();
    for token in indented_tokens.iter() {
        println!("{}:{}: {:?}", token.location.line, token.location.column, token.token);
    }
    let reconstructed = reconstruct(&indented_tokens, true);
    println!("{}", reconstructed);
}

fn reconstruct(tokens: &[LocatedToken], iffy: bool) -> String {
    let mut reconstructed = Vec::new();
    for token in tokens.iter() {
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
        if this_line.len() > column && !iffy {
            panic!(
                "column numbers went backwards: line {}, so far {:?}",
                line + 1,
                this_line
            );
        }
        while this_line.len() < column {
            this_line.push(' ');
        }
        write!(this_line, "{}", token.token).unwrap();
    }
    for each in reconstructed.iter_mut() {
        if !each.ends_with("\n") {
            each.push('\n');
        }
    }
    reconstructed.join("")
}
