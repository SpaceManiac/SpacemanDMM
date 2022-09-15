//! I/O backend for standard targets.
//!
//! JSON-RPC over stdin/stdout with Content-Length headers.

use std::io::{self, BufRead, Write};

pub fn run_until_stdin_eof<F: FnMut(&str)>(mut f: F) {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    while let Some(message) = read(&mut stdin).expect("JSON-RPC read error") {
        f(&message);
    }
}

pub fn run_with_read<R: BufRead, F: FnMut(&str)>(input: &mut R, mut f: F) {
    while let Some(message) = read(input).expect("JSON-RPC read error") {
        f(&message);
    }
}

fn read<R: BufRead>(input: &mut R) -> Result<Option<String>, Box<dyn std::error::Error>> {
    // read the content-length
    let mut buffer = String::new();
    input.read_line(&mut buffer)?;
    if buffer.is_empty() {
        return Ok(None);
    }
    let size = {
        let parts: Vec<&str> = buffer.split(' ').collect();
        if parts.len() != 2 {
            eprintln!("JSON-RPC read error: parts.len() != 2\n{:?}", parts);
            return Ok(None);
        }
        if !parts[0].eq_ignore_ascii_case("content-length:") {
            eprintln!(
                "JSON-RPC read error: !parts[0].eq_ignore_ascii_case(\"content-length:\")\n{:?}",
                parts
            );
            return Ok(None);
        }
        parts[1].trim().parse::<usize>()?
    };

    // skip blank line
    buffer.clear();
    input.read_line(&mut buffer)?;

    // read content
    let mut content = vec![0; size];
    input.read_exact(&mut content)?;
    Ok(Some(String::from_utf8(content)?))
}

pub fn write(output: &str) {
    let stdout = io::stdout();
    let mut stdout_lock = stdout.lock();
    write_to(&mut stdout_lock, output);
}

pub fn write_to<W: Write>(output: &mut W, json: &str) {
    write!(output, "Content-Length: {}\r\n\r\n{}", json.len(), json).expect("JSON-RPC write error");
    output.flush().expect("JSON-RPC write flush error");
}
