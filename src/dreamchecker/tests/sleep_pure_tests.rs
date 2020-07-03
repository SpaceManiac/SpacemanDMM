
extern crate dreamchecker as dc;

use dc::test_helpers::check_errors_match;

pub const SLEEP_ERRORS: &[(u32, u16, &str)] = &[
    (16, 16, "/mob/proc/test3 sets SpacemanDMM_should_not_sleep but calls blocking proc /proc/sleepingproc"),
];

#[test]
fn sleep() {
    let code = r##"
/proc/sleepingproc()
    sleep(1)
    input()
/proc/waitforproc()
    set waitfor = 0
    sleep(1)
    input()
    sleepingproc()
/proc/foo()
    sleepingproc()
/proc/bar()
    waitforproc()
/mob/proc/test2()
    set SpacemanDMM_should_not_sleep = TRUE
    waitforproc()
/mob/proc/test3()
    set SpacemanDMM_should_not_sleep = TRUE
    foo()
/mob/proc/test4()
    set SpacemanDMM_should_not_sleep = TRUE
    bar()
/proc/spawnthensleepproc()
    spawn(1)
        sleep(1)
/mob/proc/test5()
    set SpacemanDMM_should_not_sleep = TRUE
    spawn(1)
        sleepingproc()
/mob/proc/test6()
    set SpacemanDMM_should_not_sleep = TRUE
    spawnthensleepproc()
"##.trim();
    check_errors_match(code, SLEEP_ERRORS);
}

pub const PURE_ERRORS: &[(u32, u16, &str)] = &[
    (12, 16, "/mob/proc/test2 sets SpacemanDMM_should_be_pure but calls a /proc/impure that does impure operations"),
];

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
"##.trim();
    check_errors_match(code, PURE_ERRORS);
}

// these tests are separate because the ordering the errors are reported in isn't determinate and I CBF figuring out why -spookydonut Jan 2020
// TODO: find out why
pub const PURE2_ERRORS: &[(u32, u16, &str)] = &[
    (5, 5, "call to pure proc test discards return value"),
];

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
"##.trim();
    check_errors_match(code, PURE2_ERRORS);
}
