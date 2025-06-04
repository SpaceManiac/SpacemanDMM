extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const CONST_EVAL_ERRORS: &[(u32, u16, &str)] = &[
    (2, 7, "control flow condition is a static term"),
    (2, 7, "if condition is always true"),
];

#[test]
fn const_eval() {
    let code = r##"
/proc/test()
    if(1)
        return
    return
"##.trim();
    check_errors_match(code, CONST_EVAL_ERRORS);
}

pub const IF_ELSE_ERRORS: &[(u32, u16, &str)] = &[
    (6, 5, "possible unreachable code here"),
];

#[test]
fn if_else() {
    let code = r##"
/proc/test()
    if(prob(50))
        return
    else
        return
    return
"##.trim();
    check_errors_match(code, IF_ELSE_ERRORS);
}

pub const IF_ARMS_ERRORS: &[(u32, u16, &str)] = &[
    (2, 7, "control flow condition is a static term"),
    (2, 7, "if condition is always true"),
    (4, 12, "unreachable if block, preceeding if/elseif condition(s) are always true"),
    // TODO: fix location reporting on this
    (7, 9, "unreachable else block, preceeding if/elseif condition(s) are always true"),
];

#[test]
fn if_arms() {
    let code = r##"
/proc/test()
    if(1)
        return
    else if(prob(50))
        return
    else
        return
"##.trim();
    check_errors_match(code, IF_ARMS_ERRORS);
}

pub const DO_WHILE_ERRORS: &[(u32, u16, &str)] = &[
    (2, 5, "do while terminates without ever reaching condition"),
];

#[test]
fn do_while() {
    let code = r##"
/proc/test()
    do
        return
    while(prob(50))
"##.trim();
    check_errors_match(code, DO_WHILE_ERRORS);
}

pub const FOR_LOOP_CONDITION_ERRORS: &[(u32, u16, &str)] = &[
    (4, 5, "loop condition is always true"),
    (4, 5, "control flow condition is a static term"),
    (6, 5, "control flow condition is a constant evalutation"),
];

#[test]
fn for_loop_condition() {
    let code = r##"
/proc/test()
    for()
        break
    for(var/x = 0; 1; x++)
        break
    for(var/y = 0; 5 <= 7; y++)
        break
    for(var/z = 1; z <= 6; z++) // Legit, should have no error
        break
    return
"##.trim();
    check_errors_match(code, FOR_LOOP_CONDITION_ERRORS);
}

#[test]
fn for_kv_check() {
    let code = r##"
/proc/test()
    var/alist/A = alist()
    for (var/k, v in A)
        world.log << k
        world.log << v

"##.trim();
    check_errors_match(code, NO_ERRORS);
}

pub const FOR_KV_VAR_ERROR: &[(u32, u16, &str)] = &[
    (3, 19, "for (var/key, value) requires a 'var' keyword"),
];

#[test]
fn for_kv_var_check() {
    let code = r##"
/proc/test()
    var/alist/A = alist()
    for (k, v in A)
        world.log << k
        world.log << v

"##.trim();
    check_errors_match(code, FOR_KV_VAR_ERROR);
}

pub const FOR_KV_VALUE_ERROR: &[(u32, u16, &str)] = &[
    (3, 23, "value must be a variable in a for (var/key, value) statement"),
];

#[test]
fn for_kv_value_check() {
    let code = r##"
/proc/test()
    var/alist/A = alist()
    for (var/k, 0 in A)
        world.log << k
"##.trim();
    check_errors_match(code, FOR_KV_VALUE_ERROR);
}

pub const FOR_KV_KEY_ERROR: &[(u32, u16, &str)] = &[
    (3, 27, "cannot assigned a value to var/key in a for(var/key, value) statement"),
];

#[test]
fn for_kv_key_check() {
    let code = r##"
/proc/test()
    var/alist/A = alist()
    for (var/k = 5, v in A)
        world.log << k
"##.trim();
    check_errors_match(code, FOR_KV_KEY_ERROR);
}
