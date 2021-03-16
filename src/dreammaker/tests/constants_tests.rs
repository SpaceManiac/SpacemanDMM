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

}

#[test]
fn rgb_range() {
    let code_rgb = "rgb(0, 300, 0)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_rgb.as_bytes())
            .unwrap_err().description(),
        "malformed rgb() call, 300 is not within the valid range (0..255)",
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
fn rgb_hsl() {
    let code_hsl = "rgb(h=0, s=0, l=100)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsl.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#ffffff".to_owned()),
    );
    // let code_hsl2 = "rgb(h=50, s=60, l=70)";
    // assert_eq!(
    //     dm::constants::evaluate_str(Default::default(), code_hsl2.as_bytes())
    //         .expect("evaluation failed"),
    //     Constant::String("#e0d185".to_owned()),
    // );

    let code_hsl_space = "rgb(360, 0, 0, space=2)";
    assert_eq!(
        dm::constants::evaluate_str(Default::default(), code_hsl_space.as_bytes())
            .expect("evaluation failed"),
        Constant::String("#000000".to_owned()),
    );
}
