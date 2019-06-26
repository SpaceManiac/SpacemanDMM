extern crate dreammaker as dm;

use dm::constants::*;

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
