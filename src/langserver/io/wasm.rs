//! I/O backend for WebAssembly target.
//!
//! `main()` creates an engine that will respond to calls to `handle_input()`,
//! which will then call `handle_output()` with output messages.
#![allow(unsafe_code)]

use Engine;
use super::ResponseWrite;

pub fn io_main() {
    let wasmio = Box::leak(Box::new(WasmIo));
    let context = Box::leak(Box::new(Default::default()));
    let engine = Box::new(Engine::new(wasmio, context));
    unsafe {
        ENGINE_PTR = Box::into_raw(engine);
    }
}

static mut ENGINE_PTR: *mut Engine<WasmIo> = 0 as *mut Engine<WasmIo>;

#[no_mangle]
pub unsafe extern fn handle_input(ptr: *const u8, len: usize) {
    assert!(!ENGINE_PTR.is_null());
    let engine = &mut *ENGINE_PTR;
    let slice = std::slice::from_raw_parts(ptr, len);
    let text = std::str::from_utf8(slice).expect("input is not utf-8");
    engine.handle_input(text);
}

struct WasmIo;

impl ResponseWrite for WasmIo {
    fn write(&self, output: String) {
        unsafe {
            handle_output(output.as_ptr(), output.len());
        }
    }
}

extern {
    fn handle_output(ptr: *const u8, len: usize);
}
