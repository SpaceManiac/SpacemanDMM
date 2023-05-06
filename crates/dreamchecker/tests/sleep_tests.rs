
extern crate dreamchecker as dc;

use dc::test_helpers::check_errors_match;

const SLEEP_ERRORS: &[(u32, u16, &str)] = &[
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

const SLEEP_ERRORS2: &[(u32, u16, &str)] = &[
    (8, 21, "/mob/living/proc/bar calls /mob/living/proc/foo which has override child proc that sleeps /mob/living/carbon/proc/foo"),
    (8, 21, "/mob/living/proc/bar calls /mob/proc/thing which has override child proc that sleeps /mob/dead/proc/thing"),
    (8, 21, "/mob/living/proc/bar calls /mob/proc/New which has override child proc that sleeps /mob/dead/proc/New"),
];

#[test]
fn sleep2() {
    let code = r##"
/mob/New()
/mob/proc/foo()
    sleep(1)
/mob/living/foo()
    return TRUE
/mob/living/carbon/foo()
    sleep(1)
/mob/living/proc/bar()
    set SpacemanDMM_should_not_sleep = TRUE
    foo()
    thing()
    new /mob/living()
/mob/living/New()
    . = ..()
/mob/living/carbon/human/foo()
    . = ..()
/mob/dead/New()
    sleep(1)
/mob/proc/thing()
/mob/dead/thing()
    sleep(1)
/mob/living/thing()
    . = ..()
"##.trim();
    check_errors_match(code, SLEEP_ERRORS2);
}

// This test is like sleep2, but checks /atom/movable -> /mob relationship
#[test]
fn sleep3() {
    let code = r##"
/atom/New()
/atom/proc/foo()
    sleep(1)
/atom/movable/foo()
    return TRUE
/mob/foo()
    sleep(1)
/atom/movable/proc/bar()
    set SpacemanDMM_should_not_sleep = TRUE
    foo()
    thing()
    new /atom/movable()
/atom/movable/New()
    . = ..()
/mob/human/foo()
    . = ..()
/atom/dead/New()
    sleep(1)
/atom/proc/thing()
/atom/dead/thing()
    sleep(1)
/atom/movable/thing()
    . = ..()
"##.trim();
    check_errors_match(code, &[
        (8, 23, "/atom/movable/proc/bar calls /atom/movable/proc/foo which has override child proc that sleeps /mob/proc/foo"),
        (8, 23, "/atom/movable/proc/bar calls /atom/proc/thing which has override child proc that sleeps /atom/dead/proc/thing"),
        (8, 23, "/atom/movable/proc/bar calls /atom/proc/New[1/2] which has override child proc that sleeps /atom/dead/proc/New"),
    ]);
}

const SLEEP_ERROR4: &[(u32, u16, &str)] = &[
    (1, 16, "/mob/proc/test1 sets SpacemanDMM_should_not_sleep but calls blocking built-in(s)"),
    (1, 16, "/mob/proc/test1 sets SpacemanDMM_should_not_sleep but calls blocking proc /mob/proc/test2"),
    (1, 16, "/mob/proc/test1 sets SpacemanDMM_should_not_sleep but calls blocking proc /client/proc/checksoundquery"),
    (1, 16, "/mob/proc/test1 sets SpacemanDMM_should_not_sleep but calls blocking proc /client/proc/checkmeasuretext"),
    (1, 16, "/mob/proc/test1 sets SpacemanDMM_should_not_sleep but calls blocking proc /world/proc/checkimport"),
    (1, 16, "/mob/proc/test1 sets SpacemanDMM_should_not_sleep but calls blocking proc /world/proc/checkexport"),
];

#[test]
fn sleep4() {
    let code = r##"
/mob/proc/test1()
    set SpacemanDMM_should_not_sleep = TRUE
    world.Export()
    world.Import()
    var/client/C = new /client
    test2()
    C.checksoundquery()
    C.checkmeasuretext()
    world.checkimport()
    world.checkexport()
/client/proc/checksoundquery()
    SoundQuery()
/client/proc/checkmeasuretext()
    MeasureText()
/world/proc/checkimport()
    Import()
/world/proc/checkexport()
    Export()
/mob/proc/test2()
    var/client/C = new /client
    C.MeasureText()
"##.trim();
    check_errors_match(code, SLEEP_ERROR4);
}

// Test overrides and for regression of issue #267
const SLEEP_ERROR5: &[(u32, u16, &str)] = &[
    (7, 19, "/datum/sub/proc/checker calls /datum/proc/proxy which has override child proc that sleeps /datum/hijack/proc/proxy"),
    (7, 19, "/datum/sub/proc/checker sets SpacemanDMM_should_not_sleep but calls blocking proc /proc/sleeper"),
];

#[test]
fn sleep5() {
    let code = r##"
/datum/proc/checker()
        set SpacemanDMM_should_not_sleep = 1

/datum/proc/proxy()
        sleeper()

/datum/sub/checker()
        proxy()

/proc/sleeper()
        sleep(1)

/datum/hijack/proxy()
        sleep(1)
"##.trim();
    check_errors_match(code, SLEEP_ERROR5);
}

// Test overrides and for regression of issue #355
const SLEEP_ERROR6: &[(u32, u16, &str)] = &[
        (4, 24, "/datum/choiced/proc/is_valid sets SpacemanDMM_should_not_sleep but calls blocking proc /proc/stoplag"),
];

#[test]
fn sleep6() {
    let code = r##"
/datum/proc/is_valid(value)
    set SpacemanDMM_should_not_sleep = 1

/datum/choiced/is_valid(value)
    get_choices()

/datum/choiced/proc/get_choices()
    init_possible_values()

/datum/choiced/proc/init_possible_values()

/datum/choiced/ai_core_display/init_possible_values()
    stoplag()

/proc/stoplag()
    sleep(1)
"##.trim();
    check_errors_match(code, SLEEP_ERROR6);
}