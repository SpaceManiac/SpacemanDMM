extern crate dreamchecker as dc;

use dc::test_helpers::*;

pub const AFTER_KWARG_ERRORS: &[(u32, u16, &str)] =
    &[(3, 5, "proc called with non-kwargs after kwargs: foo()")];

#[test]
fn after_kwarg() {
    let code = r##"
/proc/foo(arg1, arg2, arg3)
/proc/test()
    foo(arg2=1, 1)
"##
    .trim();
    check_errors_match(code, AFTER_KWARG_ERRORS);
}

pub const FILTER_KWARGS_ERRORS: &[(u32, u16, &str)] = &[
    (
        4,
        5,
        "filter(type=\"color\") called with invalid 'space' value 'Null'",
    ),
    (
        15,
        5,
        "filter(type=\"alpha\") called with invalid keyword parameter 'color'",
    ),
    (
        16,
        5,
        "filter(type=\"blur\") called with invalid keyword parameter 'x'",
    ),
    (
        17,
        5,
        "filter() called with invalid type keyword parameter value 'fakename'",
    ),
    (
        18,
        5,
        "filter() called without mandatory keyword parameter 'type'",
    ),
    (
        19,
        5,
        "filter() called without mandatory keyword parameter 'type'",
    ),
    (
        20,
        5,
        "filter(type=\"wave\") called with invalid keyword parameter 'color'",
    ),
];

#[test]
fn filter_kwarg() {
    let code = r#"
/proc/test()
    filter(type="alpha", x=1, y=2, icon=null, render_source=null, flags=0)
    filter(type="angular_blur", x=1, y=2, size=null)
    filter(type="color", color=null, space=null)
    filter(type="displace", x=1, y=2, size=3, icon=null, render_source=null)
    filter(type="drop_shadow", x=1, y=2, size=3, offset=4, color=5)
    filter(type="blur", size=1)
    filter(type="layer", x=1, y=2, icon=null, render_source=null, flags=0, color=0, transform=null, blend_mode=null)
    filter(type="motion_blur", x=1, y=2)
    filter(type="outline", size=1, color=2)
    filter(type="radial_blur", x=1, y=2, size=3)
    filter(type="rays", x=1, y=2, size=3, color=null, offset=4, density=5, threshold=6, factor=7, flags=0)
    filter(type="ripple", x=1, y=2, size=3, repeat=1, radius=4, falloff=5, flags=0)
    filter(type="wave", x=1, y=2, size=4, offset=5, flags=0)
    filter(type="alpha", color=null)
    filter(type="blur", x=1)
    filter(type="fakename", x=3)
    filter(x=4)
    filter("alpha", x=1, flags=MASK_INVERSE|MASK_INVERSE|MASK_INVERSE|MASK_INVERSE|MASK_INVERSE|MASK_INVERSE)
    filter(type="wave", color=null)
"#.trim();
    check_errors_match(code, FILTER_KWARGS_ERRORS);
}
