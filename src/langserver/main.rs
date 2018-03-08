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
extern crate serde_json;

mod io;

use std::io::Write;
use jsonrpc::types::{Request, Call, Response, Output, MethodCall};

fn main() {
    let stdio = io::StdIo;
    Engine {
        read: &stdio,
        write: &stdio,
        outputs: Vec::new(),
    }.run()
}

const VERSION: Option<jsonrpc::Version> = Some(jsonrpc::Version::V2);

struct Engine<'a, R: 'a, W: 'a> {
    read: &'a R,
    write: &'a W,
    outputs: Vec<Output>,
}

impl<'a, R: io::RequestRead, W: io::ResponseWrite> Engine<'a, R, W> {
    fn run(mut self) {
        let mut debug = std::fs::File::create("debug-output.txt").unwrap();

        loop {
            let message = self.read.read().expect("request read error");
            let request: Request = serde_json::from_str(&message).expect("request invalid json");

            writeln!(debug, "--> {:#?}", request).unwrap();
            match request {
                Request::Single(call) => self.handle_call(call),
                Request::Batch(calls) => for call in calls {
                    self.handle_call(call);
                }
            }

            let response = match self.outputs.len() {
                0 => continue,  // wait for another input
                1 => Response::Single(self.outputs.remove(0)),
                _ => Response::Batch(self.outputs.drain(..).collect()),
            };

            writeln!(debug, "<-- {:#?}", response).unwrap();
            self.write.respond(serde_json::to_string(&response).expect("response invalid json"));
        }
    }

    fn handle_call(&mut self, call: Call) {
        match call {
            Call::Invalid(id) => {
                self.outputs.push(Output::invalid_request(id, VERSION));
            },
            Call::MethodCall(method_call) => self.handle_method_call(method_call),
            Call::Notification(_notification) => {},  // TODO
        }
    }

    fn handle_method_call(&mut self, _call: MethodCall) {
        // TODO
    }
}
