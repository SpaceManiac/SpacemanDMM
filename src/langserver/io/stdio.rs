//! I/O backend for standard targets.
//!
//! JSON-RPC over stdin/stdout with Content-Length headers.

use Engine;
use io::{RequestRead, ResponseWrite};
use std::io::{self, Read, Write};

pub fn io_main() {
    let stdio = StdIo;
    let context = Default::default();
    let mut engine = Engine::new(&stdio, &context);
    loop {
        let message = stdio.read().expect("request bad read");
        engine.handle_input(&message);
    }
}

struct StdIo;

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
