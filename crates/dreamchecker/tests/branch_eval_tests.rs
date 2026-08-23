use dreamchecker::test_helpers::*;

#[test]
fn const_eval() {
    let code = r##"
/proc/test()
    if(1)
        return
    return
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (2, 7, "control flow condition is a static term"),
        (2, 7, "if condition is always true"),
    ]);
}

#[test]
fn if_else() {
    let code = r##"
/proc/test()
    if(prob(50))
        return
    else
        return
    return
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (6, 5, "possible unreachable code here"),
    ]);
}

#[test]
fn if_no_else() {
    let code = r##"
/proc/test()
    if(prob(50))
        return
    return
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn if_empty_else() {
    let code = r##"
/proc/test()
    if(prob(50))
        return
    else
        var/empty
    return
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn if_else_for() {
    let code = r##"
/proc/test()
    for(var/i in list("a"))
        if(prob(50))
            return
        else
            return
        return
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (7, 9, "possible unreachable code here"),
    ]);
}

#[test]
fn if_else_ambiguious_for() {
    let code = r##"
/proc/test()
    for(var/i in list("a"))
        if(prob(50))
            return
        else
            return
    return
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn if_else_for_continue() {
    let code = r##"
/proc/test()
    for(var/i in list("a"))
        if(prob(50))
            continue
        else
            continue
        return
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (7, 9, "possible unreachable code here"),
    ]);
}

#[test]
fn if_else_for_continue_redundant() {
    let code = r##"
/proc/test()
    for(var/i in list("a"))
        if(prob(50))
            continue
        else
            continue
    return
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn guaranteed_for_bleeding() {
    let code = r##"
/proc/test()
    for(var/i in 1 to 2)
        continue
    return
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn guaranteed_for_return() {
    let code = r##"
/proc/test()
    for(var/i in 1 to 2)
        return
    return
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (4, 5, "possible unreachable code here"),
    ]);
}

#[test]
fn unclear_for_return() {
    let code = r##"
/proc/test()
    for(var/i in 1 to 2)
        if(prob(50))
            continue
        return
    return
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn nested_unclear_for_return() {
    let code = r##"
/proc/test()
    for(var/i in 1 to 2)
        if(prob(50))
            if(prob(50))
                continue
        return
    return
"##
    .trim();
    check_errors_match(code, &[]);
}

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
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (2, 7, "control flow condition is a static term"),
        (2, 7, "if condition is always true"),
        (4, 12, "unreachable if block, preceeding if/elseif condition(s) are always true"),
        // TODO: fix location reporting on this
        (7, 9, "unreachable else block, preceeding if/elseif condition(s) are always true"),
    ]);
}

#[test]
fn do_while() {
    let code = r##"
/proc/test()
    do
        return
    while(prob(50))
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (2, 5, "do while terminates without ever reaching condition"),
    ]);
}

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
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (4, 5, "loop condition is always true"),
        (4, 5, "control flow condition is a static term"),
        (6, 5, "control flow condition is a constant evalutation"),
    ]);
}

#[test]
fn for_kv_check() {
    let code = r##"
/proc/test()
    var/alist/A = alist()
    for (var/k, v in A)
        world.log << k
        world.log << v

"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn for_kv_value_check() {
    let code = r##"
/proc/test()
    var/alist/A = alist()
    for (var/k, 0 in A)
        world.log << k
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (3, 23, "value must be a variable in a for (key, value) statement"),
    ]);
}

#[test]
fn for_kv_key_check() {
    let code = r##"
/proc/test()
    var/alist/A = alist()
    for (var/k = 5, v in A)
        world.log << k
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (3, 27, "cannot assign a value to key in a for(key, value) statement"),
    ]);
}
