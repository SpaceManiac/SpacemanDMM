//! SS13 minimap generation tool
#![deny(unsafe_code)] // NB deny rather than forbid, ndarray macros use unsafe

extern crate dreammaker as dm;

extern crate inflate;
extern crate lodepng;
#[cfg(feature = "png")]
extern crate png;

#[macro_use]
extern crate bytemuck;
extern crate bumpalo;
extern crate rand;

#[cfg(feature = "gfx_core")]
extern crate gfx_core;

pub mod dmi;
pub mod dmm;
mod icon_cache;
pub mod minimap;
pub mod render_passes;

pub use icon_cache::IconCache;
