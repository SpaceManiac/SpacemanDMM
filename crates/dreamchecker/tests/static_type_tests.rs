extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const FIELD_ACCESS_ERRORS: &[(u32, u16, &str)] = &[
    (3, 9, "field access requires static type: \"name\""),
    (4, 10, "field access requires static type: \"name\""),
];

#[test]
fn field_access() {
    let code = r##"
/proc/test()
    var/list/L = list()
    L[1].name
    L?[1].name
    var/atom/movable/particle_holder = new
    particle_holder.particles.height
"##
    .trim();
    check_errors_match(code, FIELD_ACCESS_ERRORS);
}

pub const PROC_CALL_ERRORS: &[(u32, u16, &str)] = &[
    (3, 9, "proc call requires static type: \"foo\""),
    (4, 10, "proc call requires static type: \"foo\""),
];

#[test]
fn proc_call() {
    let code = r##"
/proc/test()
    var/list/L = list()
    L[1].foo()
    L?[1].foo()
/mob/proc/foo()
"##
    .trim();
    check_errors_match(code, PROC_CALL_ERRORS);
}

pub const RETURN_TYPE_ERRORS: &[(u32, u16, &str)] = &[(3, 16, "undefined proc: \"foo\" on /atom")];

#[test]
fn return_type() {
    let code = r##"
/mob/proc/test()
    viewers()[1].foo()
    orange()[1].foo()
/mob/proc/foo()
"##
    .trim();
    check_errors_match(code, RETURN_TYPE_ERRORS);
}
