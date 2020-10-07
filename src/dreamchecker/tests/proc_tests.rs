
extern crate dreamchecker as dc;

use dc::test_helpers::check_errors_match;

pub const NO_PARENT_ERRORS: &[(u32, u16, &str)] = &[
    (2, 5, "proc has no parent: /mob/proc/test"),
];

#[test]
fn no_parent() {
    let code = r##"
/mob/proc/test()
    ..()
    return
"##.trim();
    check_errors_match(code, NO_PARENT_ERRORS);
}
