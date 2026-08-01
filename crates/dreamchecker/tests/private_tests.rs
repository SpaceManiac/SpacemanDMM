extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const PRIVATE_PROC_ERRORS: &[(u32, u16, &str)] = &[
    (5, 21, "proc overrides private parent, prohibited by /mob/proc/private"),
    (11, 5, "/mob/subtype/proc/test2 attempting to call private proc /mob/proc/private2, types do not match"),
    (12, 8, "/mob/subtype/proc/test2 attempting to call private proc /mob/proc/private2, types do not match"),
    (15, 6, "/obj/proc/test attempting to call private proc /mob/proc/private2, types do not match"),
    (17, 6, "/obj/proc/test attempting to call private proc /mob/proc/private, types do not match"),
];

#[test]
fn private_proc() {
    let code = r##"
/mob/proc/private()
    set SpacemanDMM_private_proc = TRUE
/mob/proc/private2()
    set SpacemanDMM_private_proc = TRUE
/mob/subtype/private()
    return
/mob/proc/test()
    private2()
    src.private2()
/mob/subtype/proc/test2()
    private2()
    src.private2()
/obj/proc/test()
    var/mob/M = new
    M.private2()
    var/mob/subtype/S = new
    S.private()
"##
    .trim();
    check_errors_match(code, PRIVATE_PROC_ERRORS);
}

pub const PRIVATE_VAR_ERRORS: &[(u32, u16, &str)] = &[
    (5, 9, "/mob/subtype overrides private var \"foo\""),
    (12, 6, "field \"bar\" on /mob is declared as private"),
    (
        14,
        6,
        "field \"foo\" on /mob/subtype is declared as private",
    ),
];

#[test]
fn private_var() {
    let code = r##"
/mob
    var/SpacemanDMM_private/foo = TRUE
    var/SpacemanDMM_private/bar = TRUE
/mob/subtype
    foo = FALSE
/mob/proc/test()
    foo = FALSE
/mob/subtype/proc/test2()
    bar = FALSE
/obj/proc/test()
    var/mob/M = new
    M.bar = TRUE
    var/mob/subtype/S = new
    S.foo = TRUE
"##
    .trim();
    check_errors_match(code, PRIVATE_VAR_ERRORS);
}

pub const PROTECTED_PROC_ERRORS: &[(u32, u16, &str)] = &[
    (
        15,
        6,
        "/obj/proc/test attempting to call protected proc /mob/proc/protected2",
    ),
    (
        17,
        6,
        "/obj/proc/test attempting to call protected proc /mob/proc/protected",
    ),
];

#[test]
fn protected_proc() {
    let code = r##"
/mob/proc/protected()
    set SpacemanDMM_protected_proc = TRUE
/mob/proc/protected2()
    set SpacemanDMM_protected_proc = TRUE
/mob/subtype/protected()
    return
/mob/proc/test()
    protected()
    src.protected()
/mob/subtype/proc/test2()
    protected2()
    src.protected2()
/obj/proc/test()
    var/mob/M = new
    M.protected2()
    var/mob/subtype/S = new
    S.protected()
"##
    .trim();
    check_errors_match(code, PROTECTED_PROC_ERRORS);
}

pub const PROTECTED_VAR_ERRORS: &[(u32, u16, &str)] = &[
    (12, 6, "field \"bar\" on /mob is declared as protected"),
    (
        14,
        6,
        "field \"foo\" on /mob/subtype is declared as protected",
    ),
];

#[test]
fn protected_var() {
    let code = r##"
/mob
    var/SpacemanDMM_protected/foo = TRUE
    var/SpacemanDMM_protected/bar = TRUE
/mob/subtype
    foo = FALSE
/mob/proc/test()
    foo = FALSE
/mob/subtype/proc/test2()
    bar = FALSE
/obj/proc/test()
    var/mob/M = new
    M.bar = TRUE
    var/mob/subtype/S = new
    S.foo = TRUE
"##
    .trim();
    check_errors_match(code, PROTECTED_VAR_ERRORS);
}
