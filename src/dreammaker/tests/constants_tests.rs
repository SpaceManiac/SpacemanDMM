extern crate dreammaker as dm;

use dm::{constants::*};

#[test]
fn floating_point_rgb() {
    // https://github.com/SpaceManiac/SpacemanDMM/issues/81
    let code = "rgb(0.5 * 255, 0.5 * 255, 0.5 * 255)";

    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#7f7f7f".to_owned()),
    );
}

#[test]
fn rgb_base() {
    let code_good = "rgb(0, 255, 0)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_good.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#00ff00".to_owned()),
    );
    let code_good2 = "rgb(50, 50, 50)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_good2.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#323232".to_owned()),
    );
}

#[test]
fn rgb_range() {
    let code_rgb = "rgb(0, 300, 0)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_rgb.as_bytes())
            .unwrap_err().description(),
        "malformed rgb() call, 300 is not within the valid range (0..255)",
    );
    let code_hsv = "rgb(361, 0, 0, space=1)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsv.as_bytes())
            .unwrap_err().description(),
        "malformed rgb() call, 361 is not within the valid range (0..360)",
    );
}

#[test]
fn rgb_args() {
    let code = "rgb(50)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code.as_bytes())
            .unwrap_err().description(),
        "malformed rgb() call, must have 3, 4, or 5 arguments and instead has 1",
    );
}

#[test]
fn rgb_hsv() {
    let code_hsv = "rgb(h=0, s=0, v=100)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsv.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#ffffff".to_owned()),
    );
    let code_hsv2 = "rgb(h=50, s=50, v=50)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsv2.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#807540".to_owned()),
    );

    let code_hsv_space = "rgb(360, 0, 0, space=1)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsv_space.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#000000".to_owned()),
    );
}

#[test]
fn rgb_hsl() {
    let code_hsl = "rgb(h=0, s=0, l=100)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsl.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#ffffff".to_owned()),
    );
    let code_hsl2 = "rgb(h=50, s=50, l=50)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsl2.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#bfaa40".to_owned()),
    );

    let code_hsl_space = "rgb(360, 0, 0, space=2)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsl_space.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#000000".to_owned()),
    );
}


#[test]
fn rgb_hcy() {
    let code_hsl = "rgb(h=0, c=0, y=100)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsl.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#ffffff".to_owned()),
    );
    let code_hsl2 = "rgb(h=50, c=50, y=50)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsl2.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#b65f37".to_owned()),
    );

    let code_hsl_space = "rgb(360, 0, 0, space=3)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsl_space.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#000000".to_owned()),
    );
}
