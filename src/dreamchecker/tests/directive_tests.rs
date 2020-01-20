
extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const TRUE_SUB_ERRORS: &[(u32, u16, &str)] = &[
    (4, 18, "proc never calls parent, required by /mob/proc/test"),
];

#[test]
fn true_substitution() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_call_parent = TRUE
    
/mob/subtype/test()
    return
"##.trim();
    check_errors_match(code, TRUE_SUB_ERRORS);
}

#[test]
fn call_parent() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_call_parent = 1
    
/mob/subtype/test()
    return
"##.trim();
    check_errors_match(code, TRUE_SUB_ERRORS);
}

#[test]
fn call_parent_disable() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_call_parent = 1
    
/mob/subtype/test()
    set SpacemanDMM_should_call_parent = 0
    return
"##.trim();
    check_errors_match(code, NO_ERRORS);
}

pub const NO_OVERRIDE_ERRORS: &[(u32, u16, &str)] = &[
    (4, 18, "proc overrides parent, prohibited by /mob/proc/test"),
];

#[test]
fn no_override() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_not_override = 1
    
/mob/subtype/test()
    return
"##.trim();
    check_errors_match(code, NO_OVERRIDE_ERRORS);
}

pub const NO_OVERRIDE_DISABLE_ERRORS: &[(u32, u16, &str)] = &[
    (5, 5, "/mob/subtype/proc/test sets SpacemanDMM_should_not_override false, but it cannot be disabled."),
    (4, 18, "proc overrides parent, prohibited by /mob/proc/test"),
];

#[test]
fn no_override_disable() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_not_override = 1
    
/mob/subtype/test()
    set SpacemanDMM_should_not_override = 0
    return
"##.trim();
    check_errors_match(code, NO_OVERRIDE_DISABLE_ERRORS);
}
