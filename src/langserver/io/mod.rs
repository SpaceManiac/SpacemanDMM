//! Pluggable backends for input/output handling.

pub trait RequestRead {
    fn read(&self) -> Option<String>;
}

pub trait ResponseWrite {
    fn write(&self, output: String);
}

#[cfg_attr(target_arch="wasm32", path="wasm.rs")]
#[cfg_attr(not(target_arch="wasm32"), path="stdio.rs")]
mod system;

pub use self::system::*;
