
extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const CONST_EVAL_ERRORS: &[(u32, u16, &str)] = &[
    (3, 18, "iterating over turf contents"),
    (5, 18, "iterating over turf contents"),
];

#[test]
fn const_eval() {
    let code = r##"
/proc/test()
    var/turf/T = new /turf
    for(var/a in T)
        world.log << a
    for(var/a in T.contents)
        world.log << a
"##.trim();
    let config = r##"
[code_standards]
disallow_turf_contents_iteration = true
"##.trim();
    check_errors_match_with_config(code, CONST_EVAL_ERRORS, config.to_string());
}

