//! SS13 minimap generation tool
#![deny(unsafe_code)]  // NB deny rather than forbid, ndarray macros use unsafe

extern crate dreammaker as dm;

#[cfg(feature="png")] extern crate png;
extern crate lodepng;
extern crate inflate;

#[macro_use] extern crate ndarray;
extern crate linked_hash_map;
extern crate rand;

pub mod dmm;
pub mod icon_cache;
pub mod minimap;
pub mod render_passes;
pub mod lint;
pub mod dmi;
