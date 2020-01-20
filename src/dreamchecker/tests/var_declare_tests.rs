
extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const VAR_DEC_ERRORS: &[(u32, u16, &str)] = &[
    (6, 1, "/mob/subtype redeclares var \"foo\""),
];

#[test]
fn var_redec() {
    let code = r##"
/mob
    var/foo
    
/mob/subtype
    var/foo
"##.trim();
    check_errors_match(code, VAR_DEC_ERRORS);
}

pub const VAR_FINAL_ERRORS: &[(u32, u16, &str)] = &[
    (5, 9, "/mob/subtype overrides final var \"foo\""),
];

#[test]
fn var_final() {
    let code = r##"
/mob
    var/SpacemanDMM_final/foo = 0
    
/mob/subtype
    foo = 1
"##.trim();
    check_errors_match(code, VAR_FINAL_ERRORS);
}
