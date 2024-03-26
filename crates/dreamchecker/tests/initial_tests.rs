
extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const INITIAL_FOLLOW_ERRORS: &[(u32, u16, &str)] = &[
    (7, 17, "initial() called on var/list/bar, but it always returns null for lists"),
    (8, 17, "initial() called on var/turf/T, but it always returns null for types"),
    (11, 13, "initial() called on var/list/bar, but it always returns null for lists"),
    (12, 13, "initial() called on var/turf/T, but it always returns null for types"),
];

#[test]
fn initial_follow() {
    let code = r##"
/datum/foo
    var/list/bar
    var/turf/T

/proc/test()
    var/datum/foo/test = new()
    initial(test.bar)
    initial(test.T)

/datum/foo/proc/test()
    initial(bar)
    initial(T)
"##.trim();
    check_errors_match(code, INITIAL_FOLLOW_ERRORS);
}
