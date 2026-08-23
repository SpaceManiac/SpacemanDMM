use dreamchecker::test_helpers::*;

#[test]
fn pure() {
    let code = r##"
/proc/pure()
    return 1
/proc/impure()
    world << "foo"
/proc/foo()
    pure()
/proc/bar()
    impure()
/mob/proc/test()
    set SpacemanDMM_should_be_pure = TRUE
    return foo()
/mob/proc/test2()
    set SpacemanDMM_should_be_pure = TRUE
    bar()
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (12, 16, "/mob/proc/test2 sets SpacemanDMM_should_be_pure but calls a /proc/impure that does impure operations"),
    ]);
}

// these tests are separate because the ordering the errors are reported in isn't determinate and I CBF figuring out why -spookydonut Jan 2020
// TODO: find out why
#[test]
fn pure2() {
    let code = r##"
/mob/proc/test()
    set SpacemanDMM_should_be_pure = TRUE
    return 1
/mob/proc/test2()
    test()
/mob/proc/test3()
    return test()
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (5, 5, "call to pure proc test discards return value"),
    ]);
}
