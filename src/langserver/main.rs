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
mod symbol_search;

use std::path::PathBuf;
use std::collections::HashMap;

use url::Url;
use jsonrpc::{Request, Call, Response, Output};
use langserver::MessageType;
use petgraph::visit::IntoNodeReferences;

fn main() {
    eprintln!("dm-langserver {}  Copyright (C) 2017-2018  Tad Hardesty", env!("CARGO_PKG_VERSION"));
    eprintln!("This program comes with ABSOLUTELY NO WARRANTY. This is free software,");
    eprintln!("and you are welcome to redistribute it under the conditions of the GNU");
    eprintln!("General Public License version 3.");
    eprintln!();
    match std::env::current_exe() {
        Ok(path) => eprintln!("executable: {}", path.display()),
        Err(e) => eprintln!("exe check failure: {}", e),
    }
    eprint!("{}", include_str!(concat!(env!("OUT_DIR"), "/build-info.txt")));
    match std::env::current_dir() {
        Ok(path) => eprintln!("directory: {}", path.display()),
        Err(e) => eprintln!("dir check failure: {}", e),
    }

    let stdio = io::StdIo;
    let context = Default::default();
    Engine::new(&stdio, &stdio, &context).run()
}

const VERSION: Option<jsonrpc::Version> = Some(jsonrpc::Version::V2);

#[derive(PartialEq)]
enum InitStatus {
    Starting,
    Running,
    ShuttingDown,
}

#[cfg(debug_assertions)]
macro_rules! dbgwriteln {
    ($($t:tt)*) => {{
        use std::io::Write;
        writeln!($($t)*).expect("debug-output failure")
    }}
}
#[cfg(not(debug_assertions))]
macro_rules! dbgwriteln {
    ($($t:tt)*) => {}
}

struct Engine<'a, R: 'a, W: 'a> {
    read: &'a R,
    write: &'a W,
    docs: document::DocumentStore,

    status: InitStatus,
    parent_pid: u64,
    root: PathBuf,

    context: &'a dm::Context,
    preprocessor: Option<dm::preprocessor::Preprocessor<'a>>,
    objtree: dm::objtree::ObjectTree,

    #[cfg(debug_assertions)]
    debug: std::fs::File,
}

impl<'a, R: io::RequestRead, W: io::ResponseWrite> Engine<'a, R, W> {
    fn new(read: &'a R, write: &'a W, context: &'a dm::Context) -> Self {
        Engine {
            read,
            write,
            docs: Default::default(),

            status: InitStatus::Starting,
            parent_pid: 0,
            root: Default::default(),

            context,
            preprocessor: None,
            objtree: Default::default(),

            #[cfg(debug_assertions)]
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
        dbgwriteln!(self.debug, "<== {}: {:#?}", T::METHOD, params);
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

    fn convert_location(&self, loc: dm::Location, one: &str, two: &str, three: &str) -> Result<langserver::Location, jsonrpc::Error> {
        let pos = langserver::Position {
            line: loc.line.saturating_sub(1) as u64,
            character: loc.column.saturating_sub(1) as u64,
        };
        Ok(langserver::Location {
            uri: if loc.file == dm::FileId::builtins() {
                Url::parse(&format!("https://secure.byond.com/docs/ref/info.html#{}{}{}", one, two, three))
                    .map_err(invalid_request)?
            } else {
                self.file_url(loc.file)?
            },
            range: langserver::Range::new(pos, pos),
        })
    }

    // ------------------------------------------------------------------------
    // Environment tracking

    fn parse_environment(&mut self, environment: PathBuf) -> Result<(), jsonrpc::Error> {
        // handle the parsing
        let file_name = environment.file_name().unwrap_or("..".as_ref()).to_string_lossy();
        eprintln!("environment: {}", environment.display());

        let ctx = self.context;
        let mut pp = match dm::preprocessor::Preprocessor::new(ctx, environment.clone()) {
            Ok(pp) => pp,
            Err(err) => {
                self.show_message(MessageType::Error, format!("Error loading {}", file_name));
                eprintln!("{:?}", err);
                return Ok(());
            }
        };

        self.objtree = dm::parser::parse(ctx, dm::indents::IndentProcessor::new(ctx, &mut pp));
        self.preprocessor = Some(pp);
        self.show_message(MessageType::Info, format!("Loaded {}", file_name));

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

        Ok(())
    }

    // ------------------------------------------------------------------------
    // Driver

    fn run(mut self) {
        loop {
            let message = self.read.read().expect("request bad read");

            dbgwriteln!(self.debug, "--> ({}) {}", message.len(), message);
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

            dbgwriteln!(self.debug, "<-- {:#?}", response);
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
                        hover_provider: Some(true),
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
                let query = symbol_search::Query::parse(&params.query);
                eprintln!("{:?} -> {:?}", params.query, query);

                if let Some(query) = query {
                    let mut results = Vec::new();
                    let start = std::time::Instant::now();
                    for &(ref name, location) in self.context.defines().iter() {
                        if query.matches_define(name) {
                            results.push(SymbolInformation {
                                name: name.to_owned(),
                                kind: SymbolKind::Constant,
                                location: self.convert_location(location, "/DM", "/preprocessor/", name)?,
                                container_name: None,
                            });
                        }
                    }

                    for (idx, ty) in self.objtree.graph.node_references() {
                        if query.matches_type(&ty.name, &ty.path) {
                            results.push(SymbolInformation {
                                name: ty.name.clone(),
                                kind: SymbolKind::Class,
                                location: self.convert_location(ty.location, &ty.path, "", "")?,
                                container_name: Some(ty.path[..ty.path.len() - ty.name.len() - 1].to_owned()),
                            });
                        }

                        if !query.matches_on_type(&ty.path) {
                            continue;
                        }
                        for (var_name, tv) in ty.vars.iter() {
                            if let Some(decl) = tv.declaration.as_ref() {
                                if query.matches_var(&var_name) {
                                    results.push(SymbolInformation {
                                        name: var_name.clone(),
                                        kind: SymbolKind::Field,
                                        location: self.convert_location(decl.location, &ty.path, "/var/", var_name)?,
                                        container_name: Some(ty.path.clone()),
                                    });
                                }
                            }
                        }

                        for (proc_name, pv) in ty.procs.iter() {
                            if let Some(decl) = pv.declaration.as_ref() {
                                if query.matches_proc(&proc_name, decl.is_verb) {
                                    results.push(SymbolInformation {
                                        name: proc_name.clone(),
                                        kind: if idx.index() == 0 {
                                            SymbolKind::Function
                                        } else if ["init", "New", "Initialize"].contains(&&**proc_name) {
                                            SymbolKind::Constructor
                                        } else {
                                            SymbolKind::Method
                                        },
                                        location: self.convert_location(decl.location, &ty.path, "/proc/", proc_name)?,
                                        container_name: Some(ty.path.clone()),
                                    });
                                }
                            }
                        }
                    }
                    let elapsed = start.elapsed();
                    eprintln!("    {} results in {}.{:03}s", results.len(), elapsed.as_secs(), elapsed.subsec_nanos() / 1_000_000);
                    #[cfg(debug_assertions)] {
                        // Serializing all these to the debug log is very slow.
                        results.truncate(100);
                    }
                    Some(results)
                } else {
                    None
                }
            };
            |params: HoverRequest| {
                let path = url_to_path(params.text_document.uri)?;
                let contents = self.docs.read(&path).map_err(invalid_request)?;

                let context = Default::default();
                let lexer = dm::lexer::Lexer::from_read(&context, Default::default(), contents);
                let indent = dm::indents::IndentProcessor::new(&context, lexer);
                let mut annotations = dm::annotation::AnnotationTree::default();
                {
                    let mut parser = dm::parser::Parser::new(&context, indent);
                    parser.annotate_to(&mut annotations);
                    parser.run();
                }

                Some(Hover {
                    range: None,
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("```\n{}\n```", annotations.get_location(dm::Location {
                            file: Default::default(),
                            line: params.position.line as u32 + 1,
                            column: params.position.character as u16 + 1,
                        }).map(|(_, x)| format!("{:?}", x)).collect::<Vec<_>>().join("\n")),
                    }),
                })
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
                eprintln!("workspace root: {}", self.root.display());
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
                    self.parse_environment(environment)?;
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
