use dreamchecker::test_helpers::*;

#[test]
fn local_scope() {
    let code = r##"
/proc/test()
    var/foo
    if(prob(50))
        var/bar
        foo++
    else
        bar++
        foo++
    bar++
    foo++
    alabel:
        var/bar
    bar++
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (7, 9, "undefined var: \"bar\""),
        (9, 5, "undefined var: \"bar\""),
        (13, 5, "undefined var: \"bar\""),
    ]);
}
