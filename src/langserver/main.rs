//! DreamMaker language server.
//!
//! Based on:
//!
//! * https://langserver.org/
//! * https://microsoft.github.io/language-server-protocol/specification
//! * https://github.com/rust-lang-nursery/rls
#![forbid(unsafe_code)]

extern crate url;
extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;
extern crate petgraph;
extern crate languageserver_types as langserver;
extern crate jsonrpc_core as jsonrpc;
extern crate dreammaker as dm;

#[macro_use] mod macros;
mod io;
mod document;
mod symbol_search;
mod extras;

use std::path::{PathBuf, Path};
use std::collections::{HashMap, VecDeque};
use std::collections::hash_map::Entry;
use std::rc::Rc;

use url::Url;
use jsonrpc::{Request, Call, Response, Output};
use langserver::MessageType;
use petgraph::visit::IntoNodeReferences;

use dm::FileId;
use dm::annotation::AnnotationTree;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");

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

    annotations: HashMap<PathBuf, (FileId, Rc<AnnotationTree>)>,
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

            annotations: Default::default(),
        }
    }

    // ------------------------------------------------------------------------
    // General input and output utilities

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

    fn show_message<S>(&mut self, typ: MessageType, message: S) where
        S: Into<String>
    {
        let message = message.into();
        eprintln!("{:?}: {}", typ, message);
        self.issue_notification::<langserver::notification::ShowMessage>(
            langserver::ShowMessageParams { typ, message }
        )
    }

    fn show_status<S>(&mut self, message: S) where
        S: Into<String>
    {
        self.issue_notification::<extras::WindowStatus>(extras::WindowStatusParams {
            environment: None,
            tasks: vec![message.into()],
        });
    }

    fn file_url(&self, file: dm::FileId) -> Result<Url, jsonrpc::Error> {
        path_to_url(self.root.join(self.context.file_path(file)))
    }

    fn location_link(&self, loc: dm::Location) -> String {
        if loc.file == dm::FileId::builtins() {
            String::new()
        } else {
            format!("file:{}#{}", self.root.join(self.context.file_path(loc.file)).display().to_string().replace("\\", "/"), loc.line)
        }
    }

    fn convert_location(&self, loc: dm::Location, one: &str, two: &str, three: &str) -> Result<langserver::Location, jsonrpc::Error> {
        let pos = langserver::Position {
            line: loc.line.saturating_sub(1) as u64,
            character: loc.column.saturating_sub(1) as u64,
        };
        Ok(langserver::Location {
            uri: if loc.file == dm::FileId::builtins() {
                Url::parse(&format!("byond-docs://reference/#{}{}{}", one, two, three))
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
        eprintln!("environment: {}", environment.display());
        if let Some(stem) = environment.file_stem() {
            self.issue_notification::<extras::WindowStatus>(extras::WindowStatusParams {
                environment: Some(stem.to_string_lossy().into_owned()),
                tasks: vec!["loading".to_owned()],
            })
        } else {
            self.show_status("loading");
        }

        let ctx = self.context;
        let mut pp = match dm::preprocessor::Preprocessor::new(ctx, environment.clone()) {
            Ok(pp) => pp,
            Err(err) => {
                use std::error::Error;
                self.issue_notification::<langserver::notification::PublishDiagnostics>(
                    langserver::PublishDiagnosticsParams {
                        uri: path_to_url(environment)?,
                        diagnostics: vec![langserver::Diagnostic {
                            message: err.description().to_owned(),
                            .. Default::default()
                        }],
                    }
                );
                eprintln!("{:?}", err);
                return Ok(());
            }
        };

        self.objtree = dm::parser::parse(ctx, dm::indents::IndentProcessor::new(ctx, &mut pp));
        pp.finalize();
        self.preprocessor = Some(pp);
        self.issue_notification::<extras::WindowStatus>(Default::default());

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

    fn get_annotations(&mut self, path: &Path) -> Result<(FileId, Rc<AnnotationTree>), jsonrpc::Error> {
        Ok(match self.annotations.entry(path.to_owned()) {
            Entry::Occupied(o) => o.get().clone(),
            Entry::Vacant(v) => {
                let stripped = match path.strip_prefix(&self.root) {
                    Ok(path) => path,
                    Err(_) => return Err(invalid_request(format!("outside workspace: {}", path.display()))),
                };
                let preprocessor = match self.preprocessor {
                    Some(ref pp) => pp,
                    None => return Err(invalid_request("no preprocessor")),
                };
                let file_id = match self.context.get_file(&stripped) {
                    Some(id) => id,
                    None => return Err(invalid_request(format!("unregistered: {}", stripped.display()))),
                };
                let context = Default::default();
                let mut preprocessor = preprocessor.branch_at_file(file_id, &context);
                let contents = self.docs.read(path).map_err(invalid_request)?;
                let file_id = preprocessor.push_file(stripped.to_owned(), contents);
                let indent = dm::indents::IndentProcessor::new(&context, preprocessor);
                let mut annotations = dm::annotation::AnnotationTree::default();
                {
                    let mut parser = dm::parser::Parser::new(&context, indent);
                    parser.annotate_to(&mut annotations);
                    parser.run();
                }
                v.insert((file_id, Rc::new(annotations))).clone()
            }
        })
    }

    // ------------------------------------------------------------------------
    // Driver

    fn run(mut self) {
        loop {
            let message = self.read.read().expect("request bad read");

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
}

handle_method_call! {
    // ------------------------------------------------------------------------
    // basic setup
    on Initialize(&mut self, init) {
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
    }

    on Shutdown(&mut self, ()) {
        self.status = InitStatus::ShuttingDown;
    }

    // ------------------------------------------------------------------------
    // actual stuff provision
    on WorkspaceSymbol(&mut self, params) {
        let query = symbol_search::Query::parse(&params.query);
        eprintln!("{:?} -> {:?}", params.query, query);

        let query = match query {
            Some(query) => query,
            None => return Ok(None),
        };

        let mut results = Vec::new();
        let start = std::time::Instant::now();
        if let Some(ref preprocessor) = self.preprocessor {
            for (range, &(ref name, _)) in preprocessor.history().iter() {
                if query.matches_define(name) {
                    results.push(SymbolInformation {
                        name: name.to_owned(),
                        kind: SymbolKind::Constant,
                        location: self.convert_location(range.start, "/DM", "/preprocessor/", name)?,
                        container_name: None,
                    });
                }
            }
        }

        for (idx, ty) in self.objtree.graph.node_references() {
            if query.matches_type(&ty.name, &ty.path) && idx.index() != 0 {
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
        Some(results)
    }

    on HoverRequest(&mut self, params) {
        let path = url_to_path(params.text_document.uri)?;
        let (file_id, annotations) = self.get_annotations(&path)?;
        let location = dm::Location {
            file: file_id,
            line: params.position.line as u32 + 1,
            column: params.position.character as u16 + 1,
        };
        let mut results = Vec::new();

        for (_range, annotation) in annotations.get_location(location) {
            use dm::annotation::Annotation;
            #[cfg(debug_assertions)] {
                results.push(format!("{:?}", annotation));
            }
            match annotation {
                Annotation::Variable(path) if !path.is_empty() => {
                    let objtree = &self.objtree;
                    let mut current = objtree.root();
                    let (last, most) = path.split_last().unwrap();
                    for part in most {
                        if part == "var" { break }
                        if let Some(child) = current.child(part) {
                            current = child;
                        } else {
                            break;
                        }
                    }

                    let mut infos = VecDeque::new();
                    let mut next = Some(current);
                    while let Some(current) = next {
                        if let Some(var) = current.vars.get(last) {
                            let constant = if let Some(ref constant) = var.value.constant {
                                format!("  \n= `{}`", constant)
                            } else {
                                String::new()
                            };
                            let path = if current.path.is_empty() {
                                "(global)"
                            } else {
                                &current.path
                            };
                            infos.push_front(format!("[{}]({}){}", path, self.location_link(var.value.location), constant));
                            if let Some(ref decl) = var.declaration {
                                let mut declaration = String::new();
                                declaration.push_str("var");
                                if decl.var_type.is_static {
                                    declaration.push_str("/static");
                                }
                                if decl.var_type.is_const {
                                    declaration.push_str("/const");
                                }
                                if decl.var_type.is_tmp {
                                    declaration.push_str("/tmp");
                                }
                                for (_, bit) in decl.var_type.type_path.iter() {
                                    declaration.push('/');
                                    declaration.push_str(&bit);
                                }
                                declaration.push_str("/**");
                                declaration.push_str(last);
                                declaration.push_str("**");
                                infos.push_front(declaration);
                            }
                        }
                        next = current.parent_type();
                    }
                    if !infos.is_empty() {
                        results.push(infos.into_iter().collect::<Vec<_>>().join("\n\n"));
                    }
                }
                Annotation::ProcHeader(path) if !path.is_empty() => {
                    let objtree = &self.objtree;
                    let mut current = objtree.root();
                    let (last, most) = path.split_last().unwrap();
                    for part in most {
                        if part == "proc" || part == "verb" { break }
                        if let Some(child) = current.child(part) {
                            current = child;
                        } else {
                            break;
                        }
                    }

                    let mut infos = VecDeque::new();
                    let mut next = Some(current);
                    while let Some(current) = next {
                        if let Some(proc) = current.procs.get(last) {
                            let path = if current.path.is_empty() {
                                "(global)"
                            } else {
                                &current.path
                            };
                            let mut message = format!("[{}]({})  \n{}(", path, self.location_link(proc.value.location), last);
                            let mut first = true;
                            for each in proc.value.parameters.iter() {
                                use std::fmt::Write;
                                if first {
                                    first = false;
                                } else {
                                    message.push_str(", ");
                                }
                                let _ = write!(message, "{}", each);
                            }
                            message.push_str(")");
                            infos.push_front(message);
                            if let Some(ref decl) = proc.declaration {
                                let mut declaration = String::new();
                                declaration.push_str(if decl.is_verb { "verb" } else { "proc" });
                                declaration.push_str("/**");
                                declaration.push_str(last);
                                declaration.push_str("**");
                                infos.push_front(declaration);
                            }
                        }
                        next = current.parent_type();
                    }
                    if !infos.is_empty() {
                        results.push(infos.into_iter().collect::<Vec<_>>().join("\n\n"));
                    }
                }
                _ => {}
            }
        }
        if results.is_empty() {
            None
        } else {
            Some(Hover {
                range: None,
                contents: HoverContents::Array(results.into_iter().map(MarkedString::String).collect()),
            })
        }
    }

    on GotoDefinition(&mut self, params) {
        use dm::annotation::Annotation;

        let path = url_to_path(params.text_document.uri)?;
        let (file_id, annotations) = self.get_annotations(&path)?;
        let location = dm::Location {
            file: file_id,
            line: params.position.line as u32 + 1,
            column: params.position.character as u16 + 1,
        };
        let mut results = Vec::new();

        let iter = annotations.get_location(location);
        if_annotation! { Annotation::TreePath(mut absolute, parts) in iter; {
            let mut parts = &parts[..];
            if_annotation! { Annotation::InSequence(idx) in iter; {
                parts = &parts[..idx+1];
            }}
            if let Some(i) = parts.iter().position(|x| x == "var") {
                parts = &parts[i+1..];
                absolute = true;
            }

            let mut prefix_parts = &[][..];
            if !absolute {
                if_annotation! { Annotation::TreeBlock(parts) in iter; {
                    prefix_parts = parts;
                    if let Some(i) = prefix_parts.iter().position(|x| x == "var") {
                        prefix_parts = &prefix_parts[i+1..];
                    }
                }}
            }

            if let Some(ty) = self.objtree.type_by_path(prefix_parts.iter().chain(parts.iter())) {
                results.push(self.convert_location(ty.location, &ty.path, "", "")?);
            }
        }}

        if results.is_empty() {
            None
        } else {
            Some(GotoDefinitionResponse::Array(results))
        }
    }
}

handle_notification! {
    // ------------------------------------------------------------------------
    // basic setup
    on Exit(&mut self, ()) {
        std::process::exit(if self.status == InitStatus::ShuttingDown { 0 } else { 1 });
    }

    on Initialized(&mut self, _) {
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
            self.show_status("no .dme file");
        }
    }

    // ------------------------------------------------------------------------
    // document content management
    on DidOpenTextDocument(&mut self, params) {
        self.docs.open(params.text_document)?;
    }

    on DidCloseTextDocument(&mut self, params) {
        let path = self.docs.close(params.text_document)?;
        self.annotations.remove(&path);
    }

    on DidChangeTextDocument(&mut self, params) {
        let path = self.docs.change(params.text_document, params.content_changes)?;
        self.annotations.remove(&path);
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
