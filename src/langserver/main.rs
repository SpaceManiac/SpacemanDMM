//! DreamMaker language server.
//!
//! Based on:
//!
//! * https://langserver.org/
//! * https://github.com/Microsoft/language-server-protocol/blob/master/versions/protocol-2-x.md
//! * https://github.com/rust-lang-nursery/rls

extern crate serde;
extern crate serde_json;
extern crate languageserver_types as langserver;
extern crate jsonrpc_core as jsonrpc;
extern crate dreammaker as dm;

mod io;

use std::io::Write;
use jsonrpc::{Request, Call, Response, Output};

fn main() {
    let stdio = io::StdIo;
    Engine {
        read: &stdio,
        write: &stdio,
    }.run()
}

const VERSION: Option<jsonrpc::Version> = Some(jsonrpc::Version::V2);

struct Engine<'a, R: 'a, W: 'a> {
    read: &'a R,
    write: &'a W,
}

impl<'a, R: io::RequestRead, W: io::ResponseWrite> Engine<'a, R, W> {
    fn run(mut self) {
        let mut debug = std::fs::File::create("debug-output.txt").unwrap();

        loop {
            let message = self.read.read().expect("request bad read");
            let request: Request = serde_json::from_str(&message).expect("request bad from_str");

            writeln!(debug, "--> {:#?}", request).unwrap();
            let mut outputs: Vec<Output> = match request {
                Request::Single(call) => self.handle_call(call).into_iter().collect(),
                Request::Batch(calls) => calls.into_iter().flat_map(|call| self.handle_call(call)).collect(),
            };

            let response = match outputs.len() {
                0 => continue,  // wait for another input
                1 => Response::Single(outputs.remove(0)),
                _ => Response::Batch(outputs),
            };

            writeln!(debug, "<-- {:#?}", response).unwrap();
            self.write.write(serde_json::to_string(&response).expect("response bad to_string"));
        }
    }

    fn issue_notification<T>(&mut self, params: T::Params) where
        T: langserver::notification::Notification,
        T::Params: serde::Serialize,
    {
        let params = serde_json::to_value(params).expect("notification bad to_value");
        let request = Request::Single(Call::Notification(jsonrpc::Notification {
            jsonrpc: VERSION,
            method: T::METHOD.to_owned(),
            params: Some(match params {
                serde_json::Value::Null => jsonrpc::Params::None,
                serde_json::Value::Array(x) => jsonrpc::Params::Array(x),
                serde_json::Value::Object(x) => jsonrpc::Params::Map(x),
                _ => panic!("notification bad value to params conversion")
            })
        }));
        self.write.write(serde_json::to_string(&request).expect("notification bad to_string"))
    }

    fn handle_call(&mut self, call: Call) -> Option<Output> {
        match call {
            Call::Invalid(id) => {
                Some(Output::invalid_request(id, VERSION))
            },
            Call::MethodCall(method_call) => {
                let id = method_call.id.clone();
                Some(Output::from(self.handle_method_call(method_call), id, VERSION))
            },
            Call::Notification(_notification) => {
                // TODO
                None
            },
        }
    }

    fn handle_method_call(&mut self, call: jsonrpc::MethodCall) -> Result<serde_json::Value, jsonrpc::Error> {
        use langserver::request::*;

        let params_value = match call.params {
            Some(jsonrpc::Params::None) |
            None => serde_json::Value::Null,
            Some(jsonrpc::Params::Array(x)) => serde_json::Value::Array(x),
            Some(jsonrpc::Params::Map(x)) => serde_json::Value::Object(x),
        };

        macro_rules! match_call {
            ($(|$name:ident: $what:ty| $body:block;)*) => (
                $(if call.method == <$what>::METHOD {
                    let $name: <$what as Request>::Params = serde_json::from_value(params_value).expect("blah");
                    let result: <$what as Request>::Result = $body;
                    Ok(serde_json::to_value(result).expect("blah 2"))
                } else)* {
                    Err(jsonrpc::Error {
                        code: jsonrpc::ErrorCode::InternalError,
                        message: "Not yet implemented".to_owned(),
                        data: None,
                    })
                }
            )
        }

        match_call! {
            |_init: Initialize| {
                self.issue_notification::<langserver::notification::ShowMessage>(langserver::ShowMessageParams {
                    typ: langserver::MessageType::Info,
                    message: "Hello, world!".to_owned(),
                });
                Default::default()
            };
        }
    }
}
