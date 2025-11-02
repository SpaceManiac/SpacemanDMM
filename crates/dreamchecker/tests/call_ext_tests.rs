extern crate dreamchecker as dc;

use dc::test_helpers::*;

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
