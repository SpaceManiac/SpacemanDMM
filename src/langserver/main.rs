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
use std::path::PathBuf;

use jsonrpc::{Request, Call, Response, Output};
use langserver::MessageType;

fn main() {
    let stdio = io::StdIo;
    Engine::new(&stdio, &stdio).run()
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
    parent_pid: u64,
    root: PathBuf,
    context: dm::Context,

    debug: std::fs::File,
}

impl<'a, R: io::RequestRead, W: io::ResponseWrite> Engine<'a, R, W> {
    fn new(read: &'a R, write: &'a W) -> Self {
        Engine {
            read,
            write,
            status: InitStatus::Starting,
            parent_pid: 0,
            root: Default::default(),
            context: Default::default(),

            debug: std::fs::File::create("debug-output.txt").expect("debug-output failure"),
        }
    }

    // ------------------------------------------------------------------------
    // General input and output utilities

    fn issue_notification<T>(&mut self, params: T::Params) where
        T: langserver::notification::Notification,
        T::Params: serde::Serialize,
    {
        let params = serde_json::to_value(params).expect("notification bad to_value");
        writeln!(self.debug, "<== {}: {:#?}", T::METHOD, params).expect("debug-output failure");
        let request = Request::Single(Call::Notification(jsonrpc::Notification {
            jsonrpc: VERSION,
            method: T::METHOD.to_owned(),
            params: Some(value_to_params(params)),
        }));
        self.write.write(serde_json::to_string(&request).expect("notification bad to_string"))
    }

    fn show_message<S>(&mut self, typ: MessageType, message: S) where
        S: Into<String>
    {
        self.issue_notification::<langserver::notification::ShowMessage>(
            langserver::ShowMessageParams { typ, message: message.into() }
        )
    }

    // ------------------------------------------------------------------------
    // Driver

    fn run(mut self) {
        loop {
            let message = self.read.read().expect("request bad read");

            writeln!(self.debug, "--> ({}) {}", message.len(), message).expect("debug-output failure");
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

            writeln!(self.debug, "<-- {:#?}", response).expect("debug-output failure");
            self.write.write(serde_json::to_string(&response).expect("response bad to_string"));
        }
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
                if let Err(e) = self.handle_notification(notification) {
                    self.show_message(MessageType::Error, e.message);
                }
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
                    let $name: <$what as Request>::Params = serde_json::from_value(params_value)
                        .map_err(invalid_request)?;
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
            |init: Initialize| {
                if self.status != InitStatus::Starting {
                    return Err(invalid_request(""))
                }

                if let Some(id) = init.process_id {
                    self.parent_pid = id;
                }
                if let Some(url) = init.root_uri {
                    if url.scheme() != "file" {
                        return Err(invalid_request("root must be a file:/// URI"));
                    }
                    match url.to_file_path() {
                        Ok(root) => self.root = root,
                        Err(()) => return Err(invalid_request("root must be a valid path"))
                    }
                } else if let Some(path) = init.root_path {
                    self.root = PathBuf::from(path);
                } else {
                    return Err(invalid_request("must provide root_uri or root_path"))
                }
                self.status = InitStatus::Running;
                Default::default()
            };
            |_empty: Shutdown| {
                self.status = InitStatus::ShuttingDown;
            };
        }
    }

    fn handle_notification(&mut self, notification: jsonrpc::Notification) -> Result<(), jsonrpc::Error> {
        use langserver::notification::*;

        // "Notifications should be dropped, except for the exit notification"
        if notification.method != <Exit>::METHOD && self.status != InitStatus::Running {
            return Ok(())
        }

        let params_value = params_to_value(notification.params);

        macro_rules! match_notify {
            ($(|$name:ident: $what:ty| $body:block;)*) => (
                $(if notification.method == <$what>::METHOD {
                    let $name: <$what as Notification>::Params = serde_json::from_value(params_value)
                        .map_err(invalid_request)?;
                    $body
                } else)* {
                    self.show_message(MessageType::Warning, format!("Notify NYI: {}", notification.method));
                }
                Ok(())
            )
        }

        match_notify! {
            |_empty: Exit| {
                std::process::exit(if self.status == InitStatus::ShuttingDown { 0 } else { 1 });
            };
            |_empty: Initialized| {
                eprintln!("root directory: {}", self.root.display());
                let mut environment = None;
                for entry in std::fs::read_dir(&self.root).map_err(invalid_request)? {
                    let entry = entry.map_err(invalid_request)?;
                    let path = entry.path();
                    if path.extension() == Some("dme".as_ref()) {
                        environment = Some(path);
                        break;
                    }
                }
                if let Some(environment) = environment {
                    eprintln!("loading environment: {}", environment.display());
                    if self.context.parse_environment(&environment).is_ok() {
                        self.show_message(MessageType::Info, "Environment loaded.");
                    } else {
                        self.show_message(MessageType::Error, "Error loading environment.");
                    }
                } else {
                    self.show_message(MessageType::Error, "No DME found, language service not available.");
                }
            };
        }
    }
}

// ----------------------------------------------------------------------------
// Helper functions

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

fn invalid_request<S: ToString>(message: S) -> jsonrpc::Error {
    jsonrpc::Error {
        code: jsonrpc::ErrorCode::InvalidRequest,
        message: message.to_string(),
        data: None,
    }
}
