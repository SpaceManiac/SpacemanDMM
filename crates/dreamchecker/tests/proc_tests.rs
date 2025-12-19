extern crate dreamchecker as dc;

use dc::test_helpers::check_errors_match;
use dc::test_helpers::parse_a_file_for_test;

pub const NO_PARENT_ERRORS: &[(u32, u16, &str)] = &[(2, 5, "proc has no parent: /mob/proc/test")];

#[test]
fn no_parent() {
    let code = r##"
/mob/proc/test()
    ..()
    return
"##
    .trim();
    check_errors_match(code, NO_PARENT_ERRORS);
}

#[test]
fn return_type() {
    let code = r##"
/mob/proc/test() as /datum
    return

/mob/proc/test2() as num
    return
"##
    .trim();
    let context = parse_a_file_for_test(code);
    let error_text: Vec<String> = context
        .errors()
        .iter()
        .map(|error| format!("{error}"))
        .collect();
    if !error_text.is_empty() {
        panic!("\n{}", error_text.join("\n"))
    }
}

pub const RETURN_TYPE_FAILURE_ERRORS: &[(u32, u16, &str)] = &[
    (4, 13, "cannot specify a return type for a proc override"),
    (7, 22, "bad input type: 'incorrect'"),
];

#[test]
fn return_type_failure() {
    let code = r##"
/datum/proc/test() as /datum
    return

/mob/test() as /mob
    return

/mob/proc/test2() as incorrect
    return
"##
    .trim();
    check_errors_match(code, RETURN_TYPE_FAILURE_ERRORS);
}
