//! SS13 minimap generation tool
#![deny(unsafe_code)] // NB deny rather than forbid, ndarray macros use unsafe

extern crate dreammaker as dm;

extern crate inflate;
extern crate lodepng;
#[cfg(feature = "png")]
extern crate png;

#[macro_use]
extern crate ndarray;
extern crate linked_hash_map;
extern crate rand;

#[macro_use]
mod utils;
pub mod dmi;
pub mod dmm;
pub mod icon_cache;
pub mod lint;
pub mod minimap;
pub mod render_passes;
