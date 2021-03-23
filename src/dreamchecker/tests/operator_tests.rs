
extern crate dreamchecker as dc;

use dc::test_helpers::check_errors_match;

pub const IN_AMBIG_ERRORS: &[(u32, u16, &str)] = &[
    (2, 7, "ambiguous `!` on left side of an `in`"),
    (6, 7, "ambiguous `&&` on left side of an `in`"),
    (11, 7, "ambiguous `=` on left side of an `in`"),
    (15, 7, "ambiguous ternary on left side of an `in`"),
];

#[test]
fn in_ambig() {
    let code = r##"
/proc/test()
    if(!1 in list())
        return
    if(!(1 in list()))
        return
    if(1 && 1 in list())
        return
    if(1 && (1 in list()))
        return
    var/i
    if(i = 1 in list())
        return
    if(i = (1 in list()))
        return
    if(i ? 1 : 2 in list())
        return
    if((i ? 1 : 2) in list())
        return
"##.trim();
    check_errors_match(code, IN_AMBIG_ERRORS);
}

pub const TERNARY_IN_AMBIG_ERRORS: &[(u32, u16, &str)] = &[
    (2, 14, "got \'in\', expected one of: operator, field access, \':\'"),
];

#[test]
fn ambig_in_ternary_cond() {
    let code = r##"
/proc/test()
    if(i ? 1 in list() : 2)
        return
"##.trim();
    check_errors_match(code, TERNARY_IN_AMBIG_ERRORS);
}

pub const OP_OVERLOAD_ERRORS: &[(u32, u16, &str)] = &[
    (6, 5, "Attempting operator++ on a /mob which does not overload operator++"),
];

#[test]
fn operator_overload() {
    let code = r##"
/mob/test/operator++()
    return

/proc/test()
    var/mob/M = new
    M++
    var/mob/test/T = new
    T++
"##.trim();
    check_errors_match(code, OP_OVERLOAD_ERRORS);
}
