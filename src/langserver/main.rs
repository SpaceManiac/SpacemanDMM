//! DreamMaker language server.
//!
//! Based on:
//!
//! * https://langserver.org/
//! * https://microsoft.github.io/language-server-protocol/specification
//! * https://github.com/rust-lang-nursery/rls

extern crate url;
extern crate serde;
extern crate serde_json;
extern crate petgraph;
extern crate languageserver_types as langserver;
extern crate jsonrpc_core as jsonrpc;
extern crate dreammaker as dm;

mod io;
mod document;

use std::io::Write;
use std::path::PathBuf;
use std::collections::HashMap;

use url::Url;
use jsonrpc::{Request, Call, Response, Output};
use langserver::MessageType;
use petgraph::visit::IntoNodeReferences;

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
    docs: document::DocumentStore,

    status: InitStatus,
    parent_pid: u64,
    root: PathBuf,

    context: dm::Context,
    objtree: dm::objtree::ObjectTree,

    debug: std::fs::File,
}

impl<'a, R: io::RequestRead, W: io::ResponseWrite> Engine<'a, R, W> {
    fn new(read: &'a R, write: &'a W) -> Self {
        Engine {
            read,
            write,
            docs: Default::default(),

            status: InitStatus::Starting,
            parent_pid: 0,
            root: Default::default(),

            context: Default::default(),
            objtree: Default::default(),

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
        let message = message.into();
        eprintln!("{:?}: {}", typ, message);
        self.issue_notification::<langserver::notification::ShowMessage>(
            langserver::ShowMessageParams { typ, message }
        )
    }

    fn file_url(&self, file: dm::FileId) -> Result<Url, jsonrpc::Error> {
        path_to_url(self.root.join(self.context.file_path(file)))
    }

    fn convert_location(&self, loc: dm::Location) -> Result<langserver::Location, jsonrpc::Error> {
        let pos = langserver::Position {
            line: loc.line.saturating_sub(1) as u64,
            character: loc.column.saturating_sub(1) as u64,
        };
        Ok(langserver::Location {
            uri: self.file_url(loc.file)?,
            range: langserver::Range::new(pos, pos),
        })
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
        use langserver::*;
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
                    eprintln!("Call NYI: {} -> {:?}", call.method, params_value);
                    Err(jsonrpc::Error {
                        code: jsonrpc::ErrorCode::InternalError,
                        message: "not yet implemented".to_owned(),
                        data: None,
                    })
                }
            )
        }

        match_call! {
            // ----------------------------------------------------------------
            // basic setup
            |init: Initialize| {
                if self.status != InitStatus::Starting {
                    return Err(invalid_request(""))
                }

                if let Some(id) = init.process_id {
                    self.parent_pid = id;
                }
                if let Some(url) = init.root_uri {
                    self.root = url_to_path(url)?;
                } else if let Some(path) = init.root_path {
                    self.root = PathBuf::from(path);
                } else {
                    return Err(invalid_request("must provide root_uri or root_path"))
                }
                self.status = InitStatus::Running;
                InitializeResult {
                    capabilities: ServerCapabilities {
                        definition_provider: Some(true),
                        workspace_symbol_provider: Some(true),
                        text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                            open_close: Some(true),
                            change: Some(TextDocumentSyncKind::Incremental),
                            .. Default::default()
                        })),
                        .. Default::default()
                    }
                }
            };
            |_empty: Shutdown| {
                self.status = InitStatus::ShuttingDown;
            };
            // ----------------------------------------------------------------
            // actual stuff provision
            |params: GotoDefinition| {
                let path = url_to_path(params.text_document.uri)?;
                let contents = self.docs.get_contents(&path).map_err(invalid_request)?;
                let offset = document::total_offset(&contents, params.position.line, params.position.character)?;
                let word = document::find_word(&contents, offset);
                if word.is_empty() {
                    None
                } else if word == "datum" {
                    Some(GotoDefinitionResponse::Scalar(Location {
                        uri: path_to_url(path)?,
                        range: Range::new(
                            Position::new(0, 0),
                            Position::new(0, 0),
                        ),
                    }))
                } else {
                    None
                }
            };
            |params: WorkspaceSymbol| {
                const MAX_SYMBOLS_PER_TYPE: usize = 15;

                let query = params.query;
                eprintln!("{:?}", query);
                let mut results = Vec::new();
                if query.is_empty() {
                    None
                } else {
                    let upperquery = query.to_uppercase();
                    for &(ref name, location) in self.context.defines().iter() {
                        if name.to_uppercase().contains(&upperquery) {
                            results.push(SymbolInformation {
                                name: name.to_owned(),
                                kind: SymbolKind::Constant,
                                location: self.convert_location(location)?,
                                container_name: None,
                            });
                            if results.len() >= MAX_SYMBOLS_PER_TYPE { break }
                        }
                    }

                    let slash = query.contains("/");
                    for (_idx, ty) in self.objtree.graph.node_references() {
                        if ty.name.starts_with(&query) || (slash && ty.path.contains(&query)) {
                            results.push(SymbolInformation {
                                name: ty.path.clone(),
                                kind: SymbolKind::Class,
                                location: Location {
                                    uri: path_to_url(self.root.clone())?,
                                    range: Range::new(
                                        Position::new(0, 0),
                                        Position::new(0, 0),
                                    ),
                                },
                                container_name: None,
                            });
                            if results.len() >= 2 * MAX_SYMBOLS_PER_TYPE { break }
                        }
                    }
                    Some(results)
                }
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
                    eprintln!("Notify NYI: {} => {:?}", notification.method, params_value);
                }
                Ok(())
            )
        }

        match_notify! {
            // ----------------------------------------------------------------
            // basic setup
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
                    let file_name = environment.file_name().unwrap_or("..".as_ref()).to_string_lossy();
                    eprintln!("loading environment: {}", environment.display());
                    match self.context.parse_environment(&environment) {
                        Ok(objtree) => {
                            self.objtree = objtree;
                            self.show_message(MessageType::Info, format!("Loaded {}", file_name));
                        },
                        Err(err) => {
                            self.show_message(MessageType::Error, format!("Error loading {}", file_name));
                            eprintln!("{:?}", err);
                        }
                    }

                    // initial diagnostics pump
                    let mut map: HashMap<_, Vec<_>> = HashMap::new();
                    for error in self.context.errors().iter() {
                        let loc = error.location();
                        let pos = langserver::Position {
                            line: loc.line.saturating_sub(1) as u64,
                            character: loc.column.saturating_sub(1) as u64,
                        };
                        let diag = langserver::Diagnostic {
                            message: error.description().to_owned(),
                            severity: Some(convert_severity(error.severity())),
                            range: langserver::Range {
                                start: pos,
                                end: pos,
                            },
                            .. Default::default()
                        };
                        map.entry(self.context.file_path(loc.file))
                            .or_insert_with(Default::default)
                            .push(diag);
                    }

                    for (path, diagnostics) in map {
                        let joined_path = self.root.join(path);
                        self.issue_notification::<langserver::notification::PublishDiagnostics>(
                            langserver::PublishDiagnosticsParams {
                                uri: path_to_url(joined_path)?,
                                diagnostics,
                            }
                        );
                    }
                } else {
                    self.show_message(MessageType::Error, "No DME found, language service not available.");
                }
            };
            // ----------------------------------------------------------------
            // document content management
            |params: DidOpenTextDocument| {
                self.docs.open(params.text_document)?;
            };
            |params: DidCloseTextDocument| {
                self.docs.close(params.text_document)?;
            };
            |params: DidChangeTextDocument| {
                self.docs.change(params.text_document, params.content_changes)?;
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

fn url_to_path(url: Url) -> Result<PathBuf, jsonrpc::Error> {
    if url.scheme() != "file" {
        return Err(invalid_request("URI must have 'file' scheme"));
    }
    url.to_file_path().map_err(|_| invalid_request("URI must be a valid path"))
}

fn path_to_url(path: PathBuf) -> Result<Url, jsonrpc::Error> {
    let formatted = path.display().to_string();
    Url::from_file_path(path).map_err(|_| invalid_request(format!(
        "bad file path: {}", formatted,
    )))
}

fn convert_severity(severity: dm::Severity) -> langserver::DiagnosticSeverity {
    match severity {
        dm::Severity::Error => langserver::DiagnosticSeverity::Error,
        dm::Severity::Warning => langserver::DiagnosticSeverity::Warning,
        dm::Severity::Info => langserver::DiagnosticSeverity::Information,
        dm::Severity::Hint => langserver::DiagnosticSeverity::Hint,
    }
}
