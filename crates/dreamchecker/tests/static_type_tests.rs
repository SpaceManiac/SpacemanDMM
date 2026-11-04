use dreamchecker::test_helpers::*;

#[test]
fn field_access() {
    let code = r##"
/proc/test()
    var/list/L = list()
    L[1].name
    L?[1].name
    var/atom/movable/particle_holder = new
    particle_holder.particles.height
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (3, 9, "field access requires static type: \"name\""),
        (4, 10, "field access requires static type: \"name\""),
    ]);
}

#[test]
fn proc_call() {
    let code = r##"
/proc/test()
    var/list/L = list()
    L[1].foo()
    L?[1].foo()
/mob/proc/foo()
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (3, 9, "proc call requires static type: \"foo\""),
        (4, 10, "proc call requires static type: \"foo\""),
    ]);
}

#[test]
fn return_type() {
    let code = r##"
/mob/proc/test()
    viewers()[1].foo()
    orange()[1].foo()
/mob/proc/foo()
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (3, 16, "undefined proc: \"foo\" on /atom"),
    ]);
}

#[test]
fn matrix_proc_return_type() {
    let code = r##"
/atom/proc/test()
    transform.Translate(1, 1).Scale(3, 3)
    var/matrix/m = matrix()
    m.Multiply(m).Add(m).Subtract(m).Invert().Turn(45).Scale(3, 3).Translate(1, 1).Interpolate(m, 0.5).Invert()
    m.Translate(1, 1).foo()
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (5, 22, "undefined proc: \"foo\" on /matrix"),
    ]);
}
