extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const CALL_EXT_MISSING_ARG_ERRORS: &[(u32, u16, &str)] = &[
    (2, 15, "got ')', expected one of: operator, field access, ','"),
];

#[test]
fn call_ext_missing_arg() {
    let code = r##"
/proc/f()
    call_ext(1)(3, 4, 5)
"##.trim();
    check_errors_match(code, CALL_EXT_MISSING_ARG_ERRORS);
}

pub const CALL_EXT_MISSING_CALL_ERRORS: &[(u32, u16, &str)] = &[
    (2, 19, "got ';', expected one of: '('"),
];

#[test]
fn call_ext_missing_call() {
    let code = r##"
/proc/f()
    call_ext(1, 2)
"##.trim();
    check_errors_match(code, CALL_EXT_MISSING_CALL_ERRORS);
}
