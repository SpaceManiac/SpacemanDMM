use dreamchecker::test_helpers::*;

#[test]
fn no_parent() {
    let code = r##"
/mob/proc/test()
    ..()
    return
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (2, 5, "proc has no parent: /mob/proc/test"),
    ]);
}

#[test]
fn return_type() {
    let code = r##"
/mob/proc/test() as /datum
    return

/mob/proc/test2() as num
    return
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn return_type_failure() {
    let code = r##"
/datum/proc/test() as /datum
    return

/mob/test() as /mob
    return

/mob/proc/test2() as incorrect
    return
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (4, 13, "cannot specify a return type for a proc override"),
        (7, 22, "bad input type: 'incorrect'"),
    ]);
}

#[test]
fn empty_list_find_warns() {
    let code = r##"
/proc/test()
    var/list/list_foo = list("a", null, "c")
    return list_foo.Find()
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (3, 20, "list.Find() with no arguments searches for null, write Find(null) if that is intended"),
    ]);
}
