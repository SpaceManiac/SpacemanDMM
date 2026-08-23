use dreamchecker::test_helpers::*;

#[rustfmt::skip]
const TRUE_SUB_ERRORS: &[(u32, u16, &str)] = &[
    (4, 18, "proc never calls parent, required by /mob/proc/test"),
];

#[test]
fn true_substitution() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_call_parent = TRUE

/mob/subtype/test()
    return
"##
    .trim();
    check_errors_match(code, TRUE_SUB_ERRORS);
}

#[test]
fn call_parent() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_call_parent = 1

/mob/subtype/test()
    return
/mob/anothertype/test()
    ..()
"##
    .trim();
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
"##
    .trim();
    check_errors_match(code, &[]);
}

#[rustfmt::skip]
const NO_OVERRIDE_ERRORS: &[(u32, u16, &str)] = &[
    (4, 18, "proc overrides parent, prohibited by /mob/proc/test"),
];

#[test]
fn no_override() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_not_override = 1

/mob/subtype/test()
    return
"##
    .trim();
    check_errors_match(code, NO_OVERRIDE_ERRORS);
}

#[test]
fn final_proc() {
    let code = r##"
/mob/proc/final/test()
    return

/mob/subtype/test()
    return
"##
    .trim();
    check_errors_match(code, NO_OVERRIDE_ERRORS);
}

#[rustfmt::skip]
const NO_OVERRIDE_DISABLE_ERRORS: &[(u32, u16, &str)] = &[
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
"##
    .trim();
    check_errors_match(code, NO_OVERRIDE_DISABLE_ERRORS);
}

#[test]
fn final_proc_intermix() {
    let code = r##"
/mob/proc/final/test()
    return

/mob/subtype/test()
    set SpacemanDMM_should_not_override = 0
    return
"##
    .trim();
    check_errors_match(code, NO_OVERRIDE_DISABLE_ERRORS);
}

#[test]
fn can_be_redefined() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_can_be_redefined = 1
    return

/mob/test()
    return
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn no_can_be_redefined() {
    let code = r##"
/mob/proc/test()
    return

/mob/test()
    return
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (4, 10, "redefining proc /mob/test"),
    ]);
}

#[test]
fn should_not_call_parent() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_not_call_parent = 1

/mob/subtype/test()
    return ..()
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (4, 18, "proc calls parent, prohibited by /mob/proc/test"),
    ]);
}

#[test]
fn should_not_call_parent_override() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_not_call_parent = 1

/mob/subtype/test()
    set SpacemanDMM_should_not_call_parent = 0
    return ..()
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn should_not_call_parent_grandchild() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_not_call_parent = 1

/mob/subtype/test()
    return 1

/mob/subtype/grandchild/test()
    return ..()
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn should_not_call_parent_grandchild_gap() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_not_call_parent = 1

/mob/subtype/grandchild/test()
    return ..()
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (4, 29, "proc calls parent, prohibited by /mob/proc/test")
    ]);
}
