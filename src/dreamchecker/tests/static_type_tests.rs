
extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const FIELD_ACCESS_ERRORS: &[(u32, u16, &str)] = &[
    (3, 9, "field access requires static type: \"name\""),
];

#[test]
fn field_access() {
    let code = r##"
/proc/test()
    var/list/L = list()
    L[1].name
"##.trim();
    check_errors_match(code, FIELD_ACCESS_ERRORS);
}

pub const PROC_CALL_ERRORS: &[(u32, u16, &str)] = &[
    (3, 9, "proc call requires static type: \"foo\""),
];

#[test]
fn proc_call() {
    let code = r##"
/proc/test()
    var/list/L = list()
    L[1].foo()
/mob/proc/foo()
"##.trim();
    check_errors_match(code, PROC_CALL_ERRORS);
}
