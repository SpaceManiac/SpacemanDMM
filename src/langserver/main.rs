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
use langserver::MessageType;

fn main() {
    let stdio = io::StdIo;
    Engine {
        read: &stdio,
        write: &stdio,
        status: InitStatus::Starting,
    }.run()
}

const VERSION: Option<jsonrpc::Version> = Some(jsonrpc::Version::V2);

#[derive(PartialEq)]
enum InitStatus {
    Starting,
    Running,
    ShuttingDown,
}

struct Engine<'a, R: 'a, W: 'a> {
    read: &'a R,
    write: &'a W,
    status: InitStatus,
}

impl<'a, R: io::RequestRead, W: io::ResponseWrite> Engine<'a, R, W> {
    fn run(mut self) {
        let mut debug = std::fs::File::create("debug-output.txt").expect("debug-output failure");

        loop {
            let message = self.read.read().expect("request bad read");

            writeln!(debug, "--> {}", message).expect("debug-output failure");
            let mut outputs: Vec<Output> = match serde_json::from_str(&message) {
                Ok(Request::Single(call)) => self.handle_call(call).into_iter().collect(),
                Ok(Request::Batch(calls)) => calls.into_iter().flat_map(|call| self.handle_call(call)).collect(),
                Err(decode_error) => {
                    vec![Output::Failure(jsonrpc::Failure {
                        jsonrpc: VERSION,
                        error: jsonrpc::Error {
                            code: jsonrpc::ErrorCode::ParseError,
                            message: decode_error.to_string(),
                            data: None,
                        },
                        id: jsonrpc::Id::Null,
                    })]
                }
            };

            let response = match outputs.len() {
                0 => continue,  // wait for another input
                1 => Response::Single(outputs.remove(0)),
                _ => Response::Batch(outputs),
            };

            writeln!(debug, "<-- {:#?}", response).expect("debug-output failure");
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
            params: Some(value_to_params(params)),
        }));
        self.write.write(serde_json::to_string(&request).expect("notification bad to_string"))
    }

    fn show_message<S>(&mut self, typ: langserver::MessageType, message: S) where
        S: Into<String>
    {
        self.issue_notification::<langserver::notification::ShowMessage>(
            langserver::ShowMessageParams { typ, message: message.into() }
        )
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
            Call::Notification(notification) => {
                self.handle_notification(notification);
                None
            },
        }
    }

    fn handle_method_call(&mut self, call: jsonrpc::MethodCall) -> Result<serde_json::Value, jsonrpc::Error> {
        use langserver::request::*;

        // "If the server receives a request... before the initialize request...
        // the response should be an error with code: -32002"
        if call.method != <Initialize>::METHOD && self.status != InitStatus::Running {
            return Err(jsonrpc::Error {
                code: jsonrpc::ErrorCode::from(-32002),
                message: "method call before initialize or after shutdown".to_owned(),
                data: None,
            })
        }

        let params_value = params_to_value(call.params);

        macro_rules! match_call {
            ($(|$name:ident: $what:ty| $body:block;)*) => (
                $(if call.method == <$what>::METHOD {
                    let $name: <$what as Request>::Params = match serde_json::from_value(params_value) {
                        Ok(value) => value,
                        Err(decode_error) => return Err(jsonrpc::Error {
                            code: jsonrpc::ErrorCode::InvalidRequest,
                            message: decode_error.to_string(),
                            data: None,
                        })
                    };
                    let result: <$what as Request>::Result = $body;
                    Ok(serde_json::to_value(result).expect("encode problem"))
                } else)* {
                    self.show_message(MessageType::Warning, format!("Call NYI: {}", call.method));
                    Err(jsonrpc::Error {
                        code: jsonrpc::ErrorCode::InternalError,
                        message: "not yet implemented".to_owned(),
                        data: None,
                    })
                }
            )
        }

        match_call! {
            |_init: Initialize| {
                if self.status != InitStatus::Starting {
                    return Err(jsonrpc::Error {
                        code: jsonrpc::ErrorCode::InvalidRequest,
                        message: "initialize called twice".to_owned(),
                        data: None,
                    })
                }
                self.show_message(MessageType::Info, "Hello, world!");
                self.status = InitStatus::Running;
                Default::default()
            };
            |_empty: Shutdown| {
                self.status = InitStatus::ShuttingDown;
            };
        }
    }

    fn handle_notification(&mut self, notification: jsonrpc::Notification) {
        use langserver::notification::*;

        // "Notifications should be dropped, except for the exit notification"
        if notification.method != <Exit>::METHOD && self.status != InitStatus::Running {
            return
        }

        let params_value = params_to_value(notification.params);

        macro_rules! match_notify {
            ($(|$name:ident: $what:ty| $body:block;)*) => (
                $(if notification.method == <$what>::METHOD {
                    let $name: <$what as Notification>::Params = match serde_json::from_value(params_value) {
                        Ok(value) => value,
                        Err(decode_error) => {
                            self.show_message(MessageType::Error, decode_error.to_string());
                            return;
                        }
                    };
                    $body
                } else)* {
                    self.show_message(MessageType::Warning, format!("Notify NYI: {}", notification.method));
                }
            )
        }

        match_notify! {
            |_empty: Exit| {
                std::process::exit(if self.status == InitStatus::ShuttingDown { 0 } else { 1 });
            };
            |_empty: Initialized| {
                // todo
            };
        }
    }
}

fn params_to_value(params: Option<jsonrpc::Params>) -> serde_json::Value {
    match params {
        None => serde_json::Value::Null,
        Some(jsonrpc::Params::None) => serde_json::Value::Object(Default::default()),
        Some(jsonrpc::Params::Array(x)) => serde_json::Value::Array(x),
        Some(jsonrpc::Params::Map(x)) => serde_json::Value::Object(x),
    }
}

fn value_to_params(value: serde_json::Value) -> jsonrpc::Params {
    match value {
        serde_json::Value::Null => jsonrpc::Params::None,
        serde_json::Value::Array(x) => jsonrpc::Params::Array(x),
        serde_json::Value::Object(x) => jsonrpc::Params::Map(x),
        _ => panic!("bad value to params conversion")
    }
}
