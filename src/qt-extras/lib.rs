//! Convenient re-exports and additional utilities for Qt support.
extern crate libc;
extern crate glium;
pub extern crate qt_widgets as widgets;
pub extern crate cpp_utils;
#[macro_use] extern crate cpp;

pub mod glium_widget;

pub use widgets::qt_core as core;
pub use widgets::qt_gui as gui;

#[macro_export]
macro_rules! qstr {
    ($e:expr) => {
        &$crate::widgets::qt_core::string::String::from_std_str($e)
    }
}

#[macro_export]
macro_rules! qt_own {
    ($e:expr) => {
        $crate::cpp_utils::static_cast_mut($e.into_raw())
    }
}
