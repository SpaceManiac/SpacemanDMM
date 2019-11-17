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
    stacked_turfs = "stacked turfs";
}

#[allow(unused_variables)]
pub fn check(
    objtree: &ObjectTree,
    map: &mut Map,
) -> Lints {
    let mut lints = Lints::default();
    let key_length = map.key_length();

    for z in 0..map.dim_z() {
        let grid = map.z_level(z);
        for (y, row) in grid.axis_iter(::ndarray::Axis(0)).enumerate() {
            for (x, e) in row.iter().enumerate() {
                let prefabs = &map.dictionary[e];
                let mut found_turf = 0;
                for fab in prefabs {
                    if subpath(&fab.path, "/turf/") {
                        found_turf += 1;
                    }
                }
                if found_turf != 1 {
                    println!("    at {:?}: found {} turfs", map.zero_to_one((x, y, z)), found_turf);
                }
            }
        }
    }

    for (&key, prefabs) in map.dictionary.iter_mut() {
        let mut found_turf = false;
        retain_mut(prefabs, |fab| {
            if subpath(&fab.path, "/turf/") {
                if found_turf {
                    lints.stacked_turfs += 1;
                    return false;
                }
                found_turf = true;
            }

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

            true
        });
    }

    lints
}

pub fn retain_mut<T, F>(v: &mut Vec<T>, mut f: F)
where
    F: FnMut(&mut T) -> bool,
{
    let len = v.len();
    let mut del = 0;
    {
        let v = &mut **v;

        for i in 0..len {
            if !f(&mut v[i]) {
                del += 1;
            } else if del > 0 {
                v.swap(i - del, i);
            }
        }
    }
    if del > 0 {
        v.truncate(len - del);
    }
}
