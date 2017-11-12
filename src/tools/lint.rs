//! Simplistic linting tools for maps, to automatically fix certain issues.
use std::fmt;

use dm::objtree::*;
use dm::constants::Constant;
use dmm::Map;

macro_rules! lints {
    ($($ident:ident = $desc:expr;)*) => {
        #[derive(Default)]
        pub struct Lints {
            $($ident: u32,)*
        }

        impl Lints {
            pub fn any(&self) -> bool {
                $(self.$ident > 0)||*
            }
        }

        impl fmt::Display for Lints {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                $(if self.$ident > 0 { writeln!(f, concat!("    ", $desc, ": {}"), self.$ident)?; })*
                Ok(())
            }
        }
    }
}

lints! {
    cable_pixels = "cable pixel offsets";
    cable_directions = "cable directions";
    tag_empty = "empty tags";
    tag_icon = "icon tags";
}

#[allow(unused_variables)]
pub fn check(
    objtree: &ObjectTree,
    map: &mut Map,
) -> Lints {
    let mut lints = Lints::default();
    let key_length = map.key_length;

    for (&key, prefabs) in map.dictionary.iter_mut() {
        for fab in prefabs.iter_mut() {
            let tag = fab.vars.get("tag").cloned();
            if let Some(Constant::String(tag)) = tag {
                if tag.is_empty() {
                    lints.tag_empty += 1;
                    fab.vars.remove("tag");
                } else if tag.starts_with("icon-") {
                    lints.tag_icon += 1;
                    fab.vars.remove("tag");
                }
            }

            if subpath(&fab.path, "/obj/structure/cable/") {
                if fab.vars.remove("pixel_y").is_some() | fab.vars.remove("pixel_x").is_some() {
                    lints.cable_pixels += 1;
                }
                if fab.vars.remove("d1").is_some() | fab.vars.remove("d2").is_some() {
                    lints.cable_directions += 1;
                }
            }
        }
    }

    lints
}
