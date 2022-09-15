extern crate dreammaker as dm;

use dm::constants::*;

fn eval(code: &str) -> Result<Constant, dm::DMError> {
    dm::constants::evaluate_str(Default::default(), code.as_bytes())
}

#[test]
fn floating_point_rgb() {
    // https://github.com/SpaceManiac/SpacemanDMM/issues/81
    assert_eq!(
        eval("rgb(0.5 * 255, 0.5 * 255, 0.5 * 255)").unwrap(),
        Constant::string("#7f7f7f"),
    );
}

#[test]
fn rgb_base() {
    assert_eq!(eval("rgb(0, 255, 0)").unwrap(), Constant::string("#00ff00"),);
    assert_eq!(
        eval("rgb(50, 50, 50)").unwrap(),
        Constant::string("#323232"),
    );
}

#[test]
fn rgb_range() {
    assert_eq!(
        eval("rgb(0, 300, 0)").unwrap_err().description(),
        "malformed rgb() call, 300 is not within the valid range (0..255)",
    );
    assert_eq!(
        eval("rgb(361, 0, 0, space=1)").unwrap_err().description(),
        "malformed rgb() call, 361 is not within the valid range (0..360)",
    );
}

#[test]
fn rgb_args() {
    assert_eq!(
        eval("rgb(50)").unwrap_err().description(),
        "malformed rgb() call, must have 3, 4, or 5 arguments and instead has 1",
    );
}

#[test]
fn rgb_alpha() {
    assert_eq!(
        eval("rgb(h=0, s=0, v=100, a=50, space=1)").unwrap(),
        Constant::string("#ffffff32"),
    );
    assert_eq!(
        eval("rgb(h=0, s=0, v=100, 50)").unwrap(),
        Constant::string("#ffffff32"),
    );
}

#[test]
fn rgb_hsv() {
    assert_eq!(
        eval("rgb(h=0, s=0, v=100)").unwrap(),
        Constant::string("#ffffff"),
    );
    assert_eq!(
        eval("rgb(h=50, s=50, v=50)").unwrap(),
        Constant::string("#807540"),
    );

    assert_eq!(
        eval("rgb(360, 0, 0, space=1)").unwrap(),
        Constant::string("#000000"),
    );
}

#[test]
fn rgb_hsl() {
    assert_eq!(
        eval("rgb(h=0, s=0, l=100)").unwrap(),
        Constant::string("#ffffff"),
    );
    assert_eq!(
        eval("rgb(h=50, s=50, l=50)").unwrap(),
        Constant::string("#bfaa40"),
    );

    assert_eq!(
        eval("rgb(360, 0, 0, space=2)").unwrap(),
        Constant::string("#000000"),
    );
}

#[test]
fn rgb_hcy() {
    assert_eq!(
        eval("rgb(h=0, c=0, y=100)").unwrap(),
        Constant::string("#ffffff"),
    );
    assert_eq!(
        eval("rgb(h=50, c=50, y=50)").unwrap(),
        Constant::string("#b65f37"),
    );

    assert_eq!(
        eval("rgb(360, 0, 0, space=3)").unwrap(),
        Constant::string("#000000"),
    );
}
