//! DreamMaker language server.
//!
//! Based on:
//!
//! * https://langserver.org/
//! * https://github.com/Microsoft/language-server-protocol/blob/master/versions/protocol-2-x.md
//! * https://github.com/rust-lang-nursery/rls

extern crate languageserver_types as ls_types;
extern crate jsonrpc_core as jsonrpc;
extern crate dreammaker as dm;

mod io;

fn main() {
    use io::Input;
    let input = io::StdinInput;

    loop {
        let message = input.read().unwrap();
        eprintln!("{}", message);
    }
}
