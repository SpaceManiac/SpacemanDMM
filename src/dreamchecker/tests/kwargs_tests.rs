
extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const AFTER_KWARG_ERRORS: &[(u32, u16, &str)] = &[
    (3, 5, "proc called with non-kwargs after kwargs: foo()"),
];

#[test]
fn after_kwarg() {
    let code = r##"
/proc/foo(arg1, arg2, arg3)
/proc/test()
    foo(arg2=1, 1)
"##.trim();
    check_errors_match(code, AFTER_KWARG_ERRORS);
}

//TODO: test filter() stuff when that PR is merged
