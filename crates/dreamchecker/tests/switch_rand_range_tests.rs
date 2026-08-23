use dreamchecker::test_helpers::*;

#[test]
fn switch_rand_incomplete() {
    let code = r##"
/proc/test()
    switch(rand(1, 3))
        if(0)
            return
        if(1)
            return
        if(2)
            return
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (4, 19, "Case range '0 to 0' will never trigger as it is outside the rand() range 1 to 3"),
        (2, 5, "Switch branches on rand() with range 1 to 3 but no case branch triggers for 3"),
    ]);
}

#[test]
fn switch_rand_with_evaluation() {
    let code = r##"
/proc/test()
    switch(rand(1 + 1, 4 - 1))
        if(3 - 1)
            return
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (2, 5, "Switch branches on rand() with range 2 to 3 but no case branch triggers for 3"),
    ]);
}

#[test]
fn switch_rand_case_ranges() {
    let code = r##"
/proc/test()
    switch(rand(1, 4))
        if(1 to 2)
            return
        if(3, 4)
            return
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn switch_rand_default() {
    let code = r##"
/proc/test()
    switch(rand(1, 4))
        if(5)
            return
        if(2)
            return
        else
            return
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (4, 19, "Case range '5 to 5' will never trigger as it is outside the rand() range 1 to 4"),
    ]);
}

#[test]
fn switch_rand_floats() {
    let code = r##"
/proc/test()
    switch(rand(1, 4))
        if(0.5 to 1.5)
            return
        if(2)
            return
        if(2.5 to 400)
            return
"##
    .trim();
    check_errors_match(code, &[]);
}

#[test]
fn switch_rand_out_of_order() {
    let code = r##"
/proc/test()
    switch(rand(1, 4))
        if(3 to 4)
            return
        if(2)
            return
        if(1)
            return
"##
    .trim();
    check_errors_match(code, &[]);
}
