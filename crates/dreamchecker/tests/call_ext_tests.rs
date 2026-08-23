use dreamchecker::test_helpers::*;

#[test]
fn call_ext_missing_call() {
    let code = r##"
/proc/f()
    call_ext(1, 2)
"##
    .trim();
    #[rustfmt::skip]
    check_errors_match(code, &[
        (2, 19, "got ';', expected one of: '('"),
    ]);
}
