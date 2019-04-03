// Based loosely on RLS's input/output code

use std::io::{self, Read, Write};

pub trait RequestRead {
    fn read(&self) -> Option<String>;
}

pub trait ResponseWrite {
    fn write(&self, output: String);
}

pub struct StdIo;

impl RequestRead for StdIo {
    fn read(&self) -> Option<String> {
        macro_rules! check {
            ($exp:expr) => {
                match $exp {
                    Ok(x) => x,
                    Err(e) => {
                        eprintln!("{:?}", e);
                        return None;
                    }
                }
            };
        }

        // read the content-length
        let mut buffer = String::new();
        check!(io::stdin().read_line(&mut buffer));
        if buffer.is_empty() {
            return None;
        }
        let size = {
            let parts: Vec<&str> = buffer.split(' ').collect();
            if parts.len() != 2 {
                return None;
            }
            if !parts[0].eq_ignore_ascii_case("content-length:") {
                return None;
            }
            check!(usize::from_str_radix(parts[1].trim(), 10))
        };

        // skip blank line
        buffer.clear();
        check!(io::stdin().read_line(&mut buffer));

        // read content
        let mut content = vec![0; size];
        check!(io::stdin().read_exact(&mut content));
        Some(check!(String::from_utf8(content)))
    }
}

impl ResponseWrite for StdIo {
    fn write(&self, output: String) {
        let stdout = io::stdout();
        let mut stdout_lock = stdout.lock();
        write!(stdout_lock, "Content-Length: {}\r\n\r\n{}", output.len(), output).unwrap();
        stdout_lock.flush().unwrap();
    }
}

#[cfg(target_arch="wasm32")]
#[allow(unsafe_code)]
pub mod wasm {
    use Engine;
    use super::ResponseWrite;

    extern {
        fn handle_output(ptr: *const u8, len: usize);
    }

    struct WasmIo;

    impl ResponseWrite for WasmIo {
        fn write(&self, output: String) {
            unsafe {
                handle_output(output.as_ptr(), output.len());
            }
        }
    }

    static mut ENGINE_PTR: *mut Engine<WasmIo> = 0 as *mut Engine<WasmIo>;

    pub fn main() {
        let wasmio = Box::leak(Box::new(WasmIo));
        let context = Box::leak(Box::new(Default::default()));
        let engine = Box::new(Engine::new(wasmio, context));
        unsafe {
            ENGINE_PTR = Box::into_raw(engine);
        }
    }

    #[no_mangle]
    pub unsafe extern fn handle_input(ptr: *const u8, len: usize) {
        assert!(!ENGINE_PTR.is_null());
        let engine = &mut *ENGINE_PTR;
        let slice = std::slice::from_raw_parts(ptr, len);
        let text = std::str::from_utf8(slice).expect("input is not utf-8");
        engine.handle_input(text);
    }
}
