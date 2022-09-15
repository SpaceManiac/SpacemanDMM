extern crate dreamchecker as dc;

use dc::test_helpers::check_errors_match;

pub const LOCAL_SCOPE_ERRORS: &[(u32, u16, &str)] = &[
    (7, 9, "undefined var: \"bar\""),
    (9, 5, "undefined var: \"bar\""),
    (13, 5, "undefined var: \"bar\""),
];

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
    check_errors_match(code, LOCAL_SCOPE_ERRORS);
}
