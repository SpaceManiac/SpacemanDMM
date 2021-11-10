
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
"##.trim();
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
"##.trim();
    check_errors_match(code, PROC_CALL_ERRORS);
}

pub const RETURN_TYPE_ERRORS: &[(u32, u16, &str)] = &[
    (3, 16, "undefined proc: \"foo\" on /atom"),
];

#[test]
fn return_type() {
    let code = r##"
/mob/proc/test()
    viewers()[1].foo()
    orange()[1].foo()
/mob/proc/foo()
"##.trim();
    check_errors_match(code, RETURN_TYPE_ERRORS);
}

pub const RELATIVE_PROC_PATHS_ERRORS: &[(u32, u16, &str)] = &[
    (13, 17, "failed to resolve path .fake"),
];

// In particular we'll test the examples documented in SS13's callback.dm. None should error.
#[test]
fn relative_proc_paths() {
    let code = r##"
/proc/global_one()
    return /proc/global_two         // absolute paths should always work

/proc/global_two()
    return .global_one              // global proc while in another global proc

/mob/proc/test()
    return

/mob/proc/other()
    var/proc_path
    proc_path = .test               // proc defined on current(src) object (when in a /proc/ and not an override)
    proc_path = .fake               // Doesn't exist, should error
    return proc_path

/mob/subtype/test()
    . = .proc/foo                   // Proc defined on current object (when in an override not a /proc/)
    return ..()

/mob/subtype/proc/foo()
    var/proc_path
    proc_path = /proc/global_one    // global proc while in a datum proc
    proc_path = .test               // Proc overridden at src
    proc_path = .proc/other         // Proc defined in parent
    proc_path = .proc/test          // Proc defined in parent and also overridden in current.
    return proc_path

/mob/subtype/deepertype/proc/bar()
    return .test              // Proc overridden in parent type (but not current type)
"##.trim();
    check_errors_match(code, RELATIVE_PROC_PATHS_ERRORS);
}
