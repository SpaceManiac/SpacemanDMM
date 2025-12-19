extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const NEW_DOT_ERRORS: &[(u32, u16, &str)] = &[(
    12,
    14,
    "got '(', expected one of: operator, field access, ';'",
)];

#[test]
fn new_dot() {
    let code = r#"
/mob/subtype
/mob/proc/foo()
/mob/proc/test()
    . = /mob
    var/list/L = list()
    new .
    new()
    new /obj()
    new /obj{name = "foo"}()
    new .subtype()
    new src.name
    new foo()()
    new /obj[0]() // TODO: see parser.rs
    new 2 + 2() // TODO: see parser.rs
"#
    .trim();
    check_errors_match(code, NEW_DOT_ERRORS);
}

pub const NEW_PRECEDENCE_ERRORS: &[(u32, u16, &str)] = &[(
    4,
    13,
    "got '(', expected one of: operator, field access, ';'",
)];

#[test]
fn new_precedence() {
    let code = r##"
/mob/subtype
/mob/proc/foo()
/mob/proc/test()
    new L[1]()
"##
    .trim();
    check_errors_match(code, NEW_PRECEDENCE_ERRORS);
}
