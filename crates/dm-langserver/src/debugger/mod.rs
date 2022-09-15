//! Debug adapter protocol implementation for DreamSeeker.
//!
//! * https://microsoft.github.io/debug-adapter-protocol/
#![allow(dead_code)]
// In BYOND references 0xAA_BBBBBB, A is the the type and B is the instance ID.
#![allow(clippy::unusual_byte_groupings)]

macro_rules! output {
    (in $seq:expr, $fmt:expr) => {
        $seq.println($fmt)
    };
    (in $seq:expr, $fmt:expr, $($rest:tt)*) => {
        $seq.println(format!($fmt, $($rest)*))
    }
}

#[cfg(debug_assertions)]
macro_rules! debug_output {
    (in $seq:expr, $fmt:expr) => {
        $seq.eprintln($fmt)
    };
    (in $seq:expr, $fmt:expr, $($rest:tt)*) => {
        $seq.eprintln(format!($fmt, $($rest)*))
    }
}

#[cfg(not(debug_assertions))]
macro_rules! debug_output {
    ($($rest:tt)*) => {{}};
}

mod auxtools;
mod auxtools_bundle;
mod auxtools_types;
mod evaluate;
mod extools;
mod extools_bundle;
mod extools_types;
mod launched;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::{atomic, Arc, Mutex};

use dm::objtree::ObjectTree;
use dm::FileId;
use dreammaker::config::DebugEngine;

use auxtools::Auxtools;

use ahash::RandomState;

use self::auxtools::AuxtoolsScopes;
use self::extools::ExtoolsHolder;
use self::launched::{EngineParams, Launched};
use crate::jrpc_io;
use dap_types::*;

/// line, path, name, override_id
pub type LineNumber = (i64, String, String, usize);

pub fn start_server(
    engine: DebugEngine,
    dreamseeker_exe: String,
    db: DebugDatabaseBuilder,
) -> std::io::Result<(u16, std::thread::JoinHandle<()>)> {
    use std::net::*;

    let listener = TcpListener::bind((Ipv4Addr::LOCALHOST, 0))?;
    let port = listener.local_addr()?.port();

    let handle = std::thread::Builder::new()
        .name(format!("DAP listener on port {}", port))
        .spawn(move || {
            let (stream, _) = listener.accept().unwrap();
            drop(listener);
            let mut input = std::io::BufReader::new(stream.try_clone().unwrap());
            let mut debugger = Debugger::new(engine, dreamseeker_exe, db, Box::new(stream));
            jrpc_io::run_with_read(&mut input, |message| debugger.handle_input(message));
        })?;

    Ok((port, handle))
}

pub fn debugger_main<I: Iterator<Item = String>>(mut args: I) {
    eprintln!("acting as debug adapter");
    let mut dreamseeker_exe = None;

    while let Some(arg) = args.next() {
        if arg == "--dreamseeker-exe" {
            dreamseeker_exe = Some(
                args.next()
                    .expect("must specify a value for --dreamseeker-exe"),
            );
        } else {
            panic!("unknown argument {:?}", arg);
        }
    }

    let dreamseeker_exe =
        dreamseeker_exe.expect("must provide argument `--dreamseeker-exe path/to/dreamseeker.exe`");
    eprintln!("dreamseeker: {}", dreamseeker_exe);

    // This isn't the preferred way to run the DAP server so it's okay for it
    // to be kind of sloppy.
    let environment = dm::detect_environment_default()
        .expect("detect .dme error")
        .expect("did not detect a .dme");
    let ctx = dm::Context::default();
    ctx.autodetect_config(&environment);
    let mut pp = dm::preprocessor::Preprocessor::new(&ctx, environment).unwrap();
    let objtree = {
        let mut parser =
            dm::parser::Parser::new(&ctx, dm::indents::IndentProcessor::new(&ctx, &mut pp));
        parser.enable_procs();
        Arc::new(parser.parse_object_tree())
    };

    let db = DebugDatabaseBuilder {
        root_dir: Default::default(),
        files: ctx.clone_file_list(),
        objtree,
        extools_dll: None,
        debug_server_dll: None,
    };
    let mut debugger = Debugger::new(
        ctx.config().debugger.engine,
        dreamseeker_exe,
        db,
        Box::new(std::io::stdout()),
    );
    jrpc_io::run_until_stdin_eof(|message| debugger.handle_input(message));
}

pub struct DebugDatabaseBuilder {
    pub root_dir: std::path::PathBuf,
    pub files: dm::FileList,
    pub objtree: Arc<ObjectTree>,
    pub extools_dll: Option<String>,
    pub debug_server_dll: Option<String>,
}

impl DebugDatabaseBuilder {
    fn build(self) -> DebugDatabase {
        let DebugDatabaseBuilder {
            root_dir,
            files,
            objtree,
            extools_dll: _,
            debug_server_dll: _,
        } = self;
        let mut line_numbers: HashMap<dm::FileId, Vec<LineNumber>, RandomState> =
            HashMap::with_hasher(RandomState::default());

        objtree.root().recurse(&mut |ty| {
            for (name, proc) in ty.procs.iter() {
                for (override_id, pv) in proc
                    .value
                    .iter()
                    .skip_while(|pv| {
                        pv.location.is_builtins()
                            && !STDDEF_PROCS.contains(&format!("{}/{}", ty.path, name).as_str())
                    })
                    .enumerate()
                {
                    line_numbers.entry(pv.location.file).or_default().push((
                        pv.location.line.into(),
                        ty.path.to_owned(),
                        name.to_owned(),
                        override_id,
                    ));
                }
            }
        });

        for vec in line_numbers.values_mut() {
            vec.sort();
        }

        DebugDatabase {
            root_dir,
            files,
            objtree,
            line_numbers,
        }
    }
}

pub struct DebugDatabase {
    root_dir: std::path::PathBuf,
    files: dm::FileList,
    objtree: Arc<ObjectTree>,
    line_numbers: HashMap<dm::FileId, Vec<LineNumber>, RandomState>,
}

fn get_proc<'o>(
    objtree: &'o ObjectTree,
    proc_ref: &str,
    override_id: usize,
) -> Option<&'o dm::objtree::ProcValue> {
    let mut bits: Vec<&str> = proc_ref.split('/').collect();
    let procname = bits.pop().unwrap();
    match bits.last() {
        Some(&"proc") | Some(&"verb") => {
            bits.pop();
        }
        _ => {}
    }
    let typename = bits.join("/");

    if let Some(ty) = objtree.find(&typename) {
        if let Some(ty_proc) = ty.get().procs.get(procname) {
            // Don't consider (most) builtins against the override_id count.
            return ty_proc
                .value
                .iter()
                .skip_while(|pv| pv.location.is_builtins() && !STDDEF_PROCS.contains(&proc_ref))
                .nth(override_id);
        }
    }
    None
}

impl DebugDatabase {
    fn get_proc(&self, proc_ref: &str, override_id: usize) -> Option<&dm::objtree::ProcValue> {
        get_proc(&self.objtree, proc_ref, override_id)
    }

    fn file_id(&self, file_path: &str) -> Option<FileId> {
        let path = std::path::Path::new(file_path);
        self.files
            .get_id(path.strip_prefix(&self.root_dir).unwrap_or(path))
    }

    fn location_to_proc_ref(&self, file_id: FileId, line: i64) -> Option<(&str, &str, usize)> {
        if let Some(list) = self.line_numbers.get(&file_id) {
            for (proc_line, type_path, proc_name, override_id) in list.iter().rev() {
                if *proc_line <= line {
                    return Some((type_path, proc_name, *override_id));
                }
            }
        }
        None
    }
}

enum DebugClient {
    Extools(ExtoolsHolder),
    Auxtools(Auxtools),
}

struct Debugger {
    engine: DebugEngine,
    dreamseeker_exe: String,
    extools_dll: Option<String>,
    debug_server_dll: Option<String>,
    db: DebugDatabase,
    launched: Option<Launched>,
    client: DebugClient,

    seq: Arc<SequenceNumber>,
    client_caps: ClientCaps,

    saved_breakpoints: HashMap<FileId, HashSet<(String, usize, i64)>>,
    stddef_dm_info: Option<StddefDmInfo>,
}

impl Debugger {
    fn new(
        engine: DebugEngine,
        dreamseeker_exe: String,
        mut db: DebugDatabaseBuilder,
        stream: OutStream,
    ) -> Self {
        Debugger {
            engine,
            dreamseeker_exe,
            extools_dll: db.extools_dll.take(),
            debug_server_dll: db.debug_server_dll.take(),
            db: db.build(),
            launched: None,
            client: DebugClient::Extools(ExtoolsHolder::default()),

            seq: Arc::new(SequenceNumber::new(stream)),
            client_caps: Default::default(),

            saved_breakpoints: Default::default(),
            stddef_dm_info: None,
        }
    }

    fn handle_input(&mut self, message: &str) {
        // TODO: error handling
        self.handle_input_inner(message)
            .expect("error in handle_input");
    }

    fn handle_input_inner(&mut self, message: &str) -> Result<(), Box<dyn Error>> {
        let protocol_message = serde_json::from_str::<ProtocolMessage>(message)?;
        match protocol_message.type_.as_str() {
            RequestMessage::TYPE => {
                let request = serde_json::from_str::<RequestMessage>(message)?;
                let request_seq = request.protocol_message.seq;
                let command = request.command.clone();

                let handled = match Self::handle_request_table(&request.command) {
                    Some(handler) => {
                        handler(self, request.arguments.unwrap_or(serde_json::Value::Null))
                    }
                    None => Err(format!("Request NYI: {}", request.command).into()),
                };

                let response = ResponseMessage {
                    protocol_message: ProtocolMessage {
                        seq: self.seq.next(),
                        type_: ResponseMessage::TYPE.to_owned(),
                    },
                    request_seq,
                    success: handled.is_ok(),
                    message: handled.as_ref().err().map(|err| err.to_string()),
                    body: match handled {
                        Ok(result) => Some(result),
                        Err(err) => {
                            if command != Evaluate::COMMAND {
                                output!(in self.seq, "[main] Error responding to {:?}: {}", command, err);
                            }
                            debug_output!(in self.seq, " - {}", message);
                            None
                        }
                    },
                    command,
                };
                self.seq
                    .send_raw(&serde_json::to_string(&response).expect("response encode error"))
            }
            other => return Err(format!("unknown `type` field {:?}", other).into()),
        }
        Ok(())
    }

    #[inline]
    fn issue_event<E: Event>(&mut self, event: E) {
        self.seq.issue_event(event);
    }

    fn notify_continue(&mut self) {
        // Called when a Step occurs so we can tell VSC that actually you can't
        // do that on a per-thread basis in DM.
        self.cull_thread_list();
        self.issue_event(dap_types::ContinuedEvent {
            threadId: 0,
            allThreadsContinued: Some(true),
        });
    }

    fn cull_thread_list(&mut self) {
        // Cull threads other than the main thread so that VSC goes back to
        // acting like the application is single-threaded, rather than showing
        // the last-known sleeping stacks every time.

        // An alternative would be to send these in real-time when sleeping
        // threads enter or exit existence.
        match &mut self.client {
            DebugClient::Extools(extools) => {
                let keys: Vec<_> = {
                    guard!(let Ok(extools) = extools.get() else { return });
                    extools
                        .get_all_threads()
                        .keys()
                        .cloned()
                        .filter(|&k| k != 0)
                        .collect()
                };
                for k in keys {
                    self.issue_event(dap_types::ThreadEvent {
                        reason: dap_types::ThreadEvent::REASON_EXITED.to_owned(),
                        threadId: k,
                    });
                }
            }

            DebugClient::Auxtools(auxtools) => {
                for stack in auxtools.get_stacks().unwrap_or_default() {
                    if stack.id == 0 {
                        continue;
                    }
                    self.issue_event(dap_types::ThreadEvent {
                        reason: dap_types::ThreadEvent::REASON_EXITED.to_owned(),
                        threadId: stack.id as i64,
                    });
                }
            }
        }
    }
}

const EXCEPTION_FILTER_RUNTIMES: &str = "runtimes";

handle_request! {
    on Initialize(&mut self, params) {
        // Initialize client caps from request
        self.client_caps = ClientCaps::parse(&params);

        #[cfg(debug_assertions)]
        {
            let debug = format!("{:?}", self.client_caps);
            if let (Some(start), Some(end)) = (debug.find('{'), debug.rfind('}')) {
                debug_output!(in self.seq, "[main] {}", &debug[start + 2..end - 1]);
            } else {
                debug_output!(in self.seq, "[main] {}", debug);
            }
        }

        // ... clientID, clientName, adapterID, locale, pathFormat

        // Tell the client our caps
        Some(Capabilities {
            supportTerminateDebuggee: Some(true),
            supportsExceptionInfoRequest: Some(true),
            supportsConfigurationDoneRequest: Some(true),
            supportsFunctionBreakpoints: Some(true),
            supportsConditionalBreakpoints: Some(true),
            supportsDisassembleRequest: Some(true),
            exceptionBreakpointFilters: Some(vec![
                ExceptionBreakpointsFilter {
                    filter: EXCEPTION_FILTER_RUNTIMES.to_owned(),
                    label: "Runtime errors".to_owned(),
                    default: Some(true),
                }
            ]),
            .. Default::default()
        })
    }

    on LaunchVsc(&mut self, params) {
        // Determine port number to pass if debugging is enabled.
        let debug = !params.base.noDebug.unwrap_or(false);

        let engine_params = if debug {
            Some(match self.engine {
                DebugEngine::Extools => {
                    let (port, extools) = ExtoolsHolder::listen(self.seq.clone())?;
                    self.client = DebugClient::Extools(extools);

                    // Set EXTOOLS_DLL based on configuration or on bundle if available.
                    #[allow(unused_mut)]
                    let mut extools_dll = None;

                    #[cfg(debug_assertions)] {
                        if let Some(dll) = self.extools_dll.as_ref() {
                            debug_output!(in self.seq, "[main] configured override: {:?}", dll);
                            extools_dll = Some(dll.into());
                        }
                    }

                    #[cfg(extools_bundle)] {
                        if extools_dll.is_none() {
                            extools_dll = Some(self::extools_bundle::extract()?);
                        }
                    }

                    EngineParams::Extools {
                        port,
                        dll: extools_dll,
                    }
                }

                DebugEngine::Auxtools => {
                    let (port, auxtools) = Auxtools::listen(self.seq.clone())?;
                    self.client = DebugClient::Auxtools(auxtools);

                    #[allow(unused_mut)]
                    let mut debug_server_dll = None;

                    #[cfg(debug_assertions)] {
                        if let Some(dll) = self.debug_server_dll.as_ref() {
                            debug_output!(in self.seq, "[main] configured override: {:?}", dll);
                            debug_server_dll = Some(dll.into());
                        }
                    }

                    #[cfg(auxtools_bundle)] {
                        if debug_server_dll.is_none() {
                            debug_server_dll = Some(self::auxtools_bundle::extract()?);
                        }
                    }

                    EngineParams::Auxtools {
                        port,
                        dll: debug_server_dll,
                    }
                }
            })
        } else {
            None
        };

        // Launch the subprocess.
        self.launched = Some(Launched::new(self.seq.clone(), &self.dreamseeker_exe, &params.dmb, engine_params)?);
    }

    on AttachVsc(&mut self, params) {
        self.client = match self.engine {
            DebugEngine::Extools => {
                DebugClient::Extools(ExtoolsHolder::attach(self.seq.clone(), params.port.unwrap_or(extools::DEFAULT_PORT))?)
            }

            DebugEngine::Auxtools => {
                DebugClient::Auxtools(Auxtools::connect(self.seq.clone(), params.port)?)
            }
        };
    }

    on Disconnect(&mut self, params) {
        let default_terminate = self.launched.is_some();
        let terminate = params.terminateDebuggee.unwrap_or(default_terminate);

        match &mut self.client {
            DebugClient::Extools(extools) => {
                extools.disconnect();
            }

            DebugClient::Auxtools(auxtools) => {
                auxtools.disconnect();
            }
        }

        if let Some(launched) = self.launched.take() {
            if terminate {
                launched.kill()?;
            } else {
                launched.detach();
            }
        }
    }

    on ConfigurationDone(&mut self, ()) {
        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;

                let text = extools.get_source("stddef.dm".to_owned())?;
                self.stddef_dm_info = Some(StddefDmInfo::new(text));

                extools.configuration_done();
            }

            DebugClient::Auxtools(auxtools) => {
                self.stddef_dm_info = auxtools.get_stddef()?.map(StddefDmInfo::new);
                auxtools.configured()?;
            }
        }
    }

    on Threads(&mut self, ()) {
        match &mut self.client {
            DebugClient::Extools(extools) => {

                let mut threads = Vec::new();

                let extools = extools.get()?;
                for (&k, v) in extools.get_all_threads().iter() {
                    threads.push(Thread {
                        id: k,
                        name: v.call_stack.last().unwrap().proc.clone(),
                    });
                }

                if threads.is_empty() {
                    threads.push(Thread {
                        id: 0,
                        name: "Main".to_owned(),
                    });
                }

                ThreadsResponse {
                    threads,
                }
            },

            DebugClient::Auxtools(auxtools) => {
                let mut threads : Vec<Thread> = auxtools.get_stacks()?.into_iter().map(|x| {
                    Thread {
                        id: x.id as i64,
                        name: x.name,
                    }
                }).collect();

                // If we tell DAP that there are no threads, Pause requests never get passed through!
                if threads.is_empty() {
                    threads.push(
                        Thread {
                            id: 0,
                            name: "Main".to_owned(),
                        }
                    );
                }

                ThreadsResponse {
                    threads,
                }
            }
        }
    }

    on SetBreakpoints(&mut self, params) {
        guard!(let Some(file_path) = params.source.path else {
            return Err(Box::new(GenericError("missing .source.path")));
        });
        guard!(let Some(file_id) = self.db.file_id(&file_path) else {
            return Err(Box::new(GenericError("file is not part of environment")));
        });

        if params.sourceModified.unwrap_or(false) {
            return Err(Box::new(GenericError("cannot update breakpoints in modified source")));
        }

        let inputs = params.breakpoints.unwrap_or_default();
        let saved = self.saved_breakpoints.entry(file_id).or_default();
        let mut keep = HashSet::new();

        match &mut self.client {
            DebugClient::Extools(extools) => {
                let mut breakpoints = Vec::new();

                guard!(let Some(extools) = extools.as_ref() else {
                    for sbp in inputs {
                        breakpoints.push(Breakpoint {
                            message: Some("Debugging hooks not available".to_owned()),
                            line: Some(sbp.line),
                            verified: false,
                            .. Default::default()
                        });
                    }
                    return Ok(SetBreakpointsResponse { breakpoints });
                });

                for sbp in inputs {
                    if let Some((typepath, name, override_id)) = self.db.location_to_proc_ref(file_id, sbp.line) {
                        // TODO: better discipline around format!("{}/{}") and so on
                        let proc = format!("{}/{}", typepath, name);
                        if let Some(offset) = extools.line_to_offset(&proc, override_id, sbp.line) {
                            let tup = (proc, override_id, offset);
                            if saved.insert(tup.clone()) {
                                extools.set_breakpoint(&tup.0, tup.1, tup.2);
                            }
                            keep.insert(tup);
                            breakpoints.push(Breakpoint {
                                line: Some(sbp.line),
                                verified: true,
                                column: Some(0),
                                .. Default::default()
                            });
                        } else {
                            debug_output!(in self.seq,
                                "Couldn't find line {} in the following disassembly:\n{}",
                                sbp.line,
                                Self::format_disassembly(extools.bytecode(&proc, override_id)));

                            breakpoints.push(Breakpoint {
                                message: Some("Unable to determine offset in proc".to_owned()),
                                line: Some(sbp.line),
                                verified: false,
                                .. Default::default()
                            });
                        }
                    } else {
                        breakpoints.push(Breakpoint {
                            message: Some("Unable to determine proc ref".to_owned()),
                            line: Some(sbp.line),
                            verified: false,
                            .. Default::default()
                        });
                    }
                }

                saved.retain(|k| {
                    if !keep.contains(k) {
                        extools.unset_breakpoint(&k.0, k.1, k.2);
                        false
                    } else {
                        true
                    }
                });

                SetBreakpointsResponse { breakpoints }
            }

            DebugClient::Auxtools(auxtools) => {
                let mut breakpoints = vec![];

                for sbp in inputs {
                    if let Some((typepath, name, override_id)) = self.db.location_to_proc_ref(file_id, sbp.line) {
                        // TODO: better discipline around format!("{}/{}") and so on
                        let proc = format!("{}/{}", typepath, name);

                        if let Some(offset) = auxtools.get_offset(proc.as_str(), override_id as u32, sbp.line as u32)? {
                            saved.insert((proc.clone(), override_id, offset as i64));
                            keep.insert((proc.clone(), override_id, offset as i64));

                            let result = auxtools.set_breakpoint(auxtools_types::InstructionRef {
                                proc: auxtools_types::ProcRef {
                                    path: proc,
                                    override_id: override_id as u32
                                },
                                offset
                            }, sbp.condition)?;

                            breakpoints.push(match result {
                                auxtools_types::BreakpointSetResult::Success { line } => {
                                    Breakpoint {
                                        verified: true,
                                        line: line.map(|x| x as i64),
                                        .. Default::default()
                                    }
                                },

                                auxtools_types::BreakpointSetResult::Failed => {
                                    Breakpoint {
                                        verified: false,
                                        .. Default::default()
                                    }
                                }
                            });
                        } else {
                            // debug_output!(in self.seq,
                            //     "Couldn't find line {} in the following disassembly:\n{}",
                            //     sbp.line,
                            //     Self::format_disassembly(extools.bytecode(&proc, override_id)));

                            breakpoints.push(Breakpoint {
                                message: Some("Unable to determine offset in proc".to_owned()),
                                line: Some(sbp.line),
                                verified: false,
                                .. Default::default()
                            });
                        }
                    } else {
                        breakpoints.push(Breakpoint {
                            message: Some("Unable to determine proc ref".to_owned()),
                            line: Some(sbp.line),
                            verified: false,
                            .. Default::default()
                        });
                    }
                }

                saved.retain(|k| {
                    if !keep.contains(k) {
                        let _ = auxtools.unset_breakpoint(&auxtools_types::InstructionRef {
                            proc: auxtools_types::ProcRef {
                                path: k.0.clone(),
                                override_id: k.1 as u32
                            },
                            offset: k.2 as u32,
                        });
                        false
                    } else {
                        true
                    }
                });

                SetBreakpointsResponse { breakpoints }
            }
        }
    }

    on SetFunctionBreakpoints(&mut self, params) {
        let file_id = FileId::default();

        let inputs = params.breakpoints;
        let mut breakpoints = Vec::new();
        let saved = self.saved_breakpoints.entry(file_id).or_default();
        let mut keep = HashSet::new();

        match &mut self.client {
            DebugClient::Extools(extools) => {
                guard!(let Some(extools) = extools.as_ref() else {
                    for _ in inputs {
                        breakpoints.push(Breakpoint {
                            message: Some("Debugging hooks not available".to_owned()),
                            verified: false,
                            .. Default::default()
                        });
                    }
                    return Ok(SetFunctionBreakpointsResponse { breakpoints });
                });

                for sbp in inputs {
                    // parse function reference
                    let mut proc = &sbp.name[..];
                    let mut override_id = 0;
                    if let Some(idx) = sbp.name.find('#') {
                        proc = &sbp.name[..idx];
                        override_id = sbp.name[idx+1..].parse()?;
                    }

                    if let Some(proc_ref) = self.db.get_proc(proc, override_id) {
                        let offset = 0;
                        let tup = (proc.to_owned(), override_id, offset);
                        if saved.insert(tup.clone()) {
                            extools.set_breakpoint(&tup.0, tup.1, tup.2);
                        }
                        keep.insert(tup);
                        breakpoints.push(Breakpoint {
                            line: Some(proc_ref.location.line as i64),
                            verified: true,
                            column: Some(0),
                            .. Default::default()
                        });
                    } else {
                        breakpoints.push(Breakpoint {
                            message: Some(format!("Unknown proc {}#{}", proc, override_id)),
                            verified: false,
                            .. Default::default()
                        });
                    }
                }

                saved.retain(|k| {
                    if !keep.contains(k) {
                        extools.unset_breakpoint(&k.0, k.1, k.2);
                        false
                    } else {
                        true
                    }
                });

                SetFunctionBreakpointsResponse { breakpoints }
            }


            DebugClient::Auxtools(auxtools) => {
                let mut breakpoints = vec![];

                for sbp in inputs {
                    // parse function reference
                    let mut proc = &sbp.name[..];
                    let mut override_id = 0;
                    if let Some(idx) = sbp.name.find('#') {
                        proc = &sbp.name[..idx];
                        override_id = sbp.name[idx+1..].parse()?;
                    }

                    let offset = 0;
                    let tup = (proc.to_owned(), override_id, offset);

                    saved.insert(tup.clone());
                    keep.insert(tup.clone());

                    let result = auxtools.set_breakpoint(auxtools_types::InstructionRef {
                        proc: auxtools_types::ProcRef {
                            path: tup.0,
                            override_id: override_id as u32
                        },
                        offset: offset as u32,
                    }, sbp.condition)?;

                    breakpoints.push(match result {
                        auxtools_types::BreakpointSetResult::Success { line } => {
                            Breakpoint {
                                verified: true,
                                line: line.map(|x| x as i64),
                                .. Default::default()
                            }
                        },

                        auxtools_types::BreakpointSetResult::Failed => {
                            Breakpoint {
                                verified: false,
                                .. Default::default()
                            }
                        }
                    });
                }

                saved.retain(|k| {
                    if !keep.contains(k) {
                        let _ = auxtools.unset_breakpoint(&auxtools_types::InstructionRef {
                            proc: auxtools_types::ProcRef {
                                path: k.0.clone(),
                                override_id: k.1 as u32
                            },
                            offset: k.2 as u32,
                        });
                        false
                    } else {
                        true
                    }
                });

                SetFunctionBreakpointsResponse { breakpoints }
            }
        }
    }

    on StackTrace(&mut self, params) {
        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;
                let thread = extools.get_thread(params.threadId)?;

                let len = thread.call_stack.len();
                let mut frames = Vec::with_capacity(len);
                for (i, ex_frame) in thread.call_stack.into_iter().enumerate() {
                    let mut dap_frame = StackFrame {
                        name: ex_frame.proc.clone(),
                        id: (i * extools.get_all_threads().len()) as i64 + params.threadId,
                        instructionPointerReference: Some(format!("{}@{}#{}", ex_frame.proc, ex_frame.override_id, ex_frame.offset)),
                        .. Default::default()
                    };

                    if i == 0 {
                        // Column must be nonzero for VSC to show the exception widget,
                        // but we don't usually have meaningful column information.
                        dap_frame.column = 1;
                    }

                    if let Some(proc) = self.db.get_proc(&ex_frame.proc, ex_frame.override_id) {
                        if proc.location.is_builtins() {
                            // `stddef.dm` proc.
                            if let Some(stddef_dm_info) = self.stddef_dm_info.as_ref() {
                                if let Some(proc) = get_proc(&stddef_dm_info.objtree, &ex_frame.proc, ex_frame.override_id) {
                                    dap_frame.source = Some(Source {
                                        name: Some("stddef.dm".to_owned()),
                                        sourceReference: Some(STDDEF_SOURCE_REFERENCE),
                                        .. Default::default()
                                    });
                                    dap_frame.line = i64::from(proc.location.line);
                                    //dap_frame.column = i64::from(proc.location.column);
                                }
                            }
                        } else {
                            // Normal proc.
                            let path = self.db.files.get_path(proc.location.file);

                            dap_frame.source = Some(Source {
                                name: Some(path.file_name()
                                    .unwrap_or_default()
                                    .to_string_lossy()
                                    .into_owned()),
                                path: Some(self.db.root_dir.join(path).to_string_lossy().into_owned()),
                                .. Default::default()
                            });
                            dap_frame.line = i64::from(proc.location.line);
                            //dap_frame.column = i64::from(proc.location.column);
                        }
                    }

                    if let Some(line) = extools.offset_to_line(&ex_frame.proc, ex_frame.override_id, ex_frame.offset) {
                        dap_frame.line = line;
                    }

                    frames.push(dap_frame);
                }

                StackTraceResponse {
                    totalFrames: Some(len as i64),
                    stackFrames: frames,
                }
            }

            DebugClient::Auxtools(auxtools) => {
                let (aux_frames, aux_frames_total) = auxtools.get_stack_frames(
                    params.threadId as u32,
                    params.startFrame.map(|x| x as u32),
                    params.levels.map(|x| x as u32))?;

                let mut frames = Vec::with_capacity(aux_frames.len());
                for (i, aux_frame) in aux_frames.iter().enumerate() {
                    let aux_proc = &aux_frame.instruction.proc;
                    let mut dap_frame = StackFrame {
                        name: aux_proc.path.to_owned(),
                        id: aux_frame.id as i64,
                        instructionPointerReference: Some(format!("{}@{}#{}", aux_proc.path, aux_proc.override_id, aux_frame.instruction.offset)),
                        .. Default::default()
                    };

                    if i == 0 {
                        // Column must be nonzero for VSC to show the exception widget,
                        // but we don't usually have meaningful column information.
                        dap_frame.column = 1;
                    }

                    if let Some(proc) = self.db.get_proc(&aux_proc.path, aux_proc.override_id as usize) {
                        if proc.location.is_builtins() {
                            // `stddef.dm` proc.
                            if let Some(stddef_dm_info) = self.stddef_dm_info.as_ref() {
                                if let Some(proc) = get_proc(&stddef_dm_info.objtree, &aux_proc.path, aux_proc.override_id as usize) {
                                    dap_frame.source = Some(Source {
                                        name: Some("stddef.dm".to_owned()),
                                        sourceReference: Some(STDDEF_SOURCE_REFERENCE),
                                        .. Default::default()
                                    });
                                    dap_frame.line = i64::from(proc.location.line);
                                    //dap_frame.column = i64::from(proc.location.column);
                                }
                            }
                        } else {
                            // Normal proc.
                            let path = self.db.files.get_path(proc.location.file);

                            dap_frame.source = Some(Source {
                                name: Some(path.file_name()
                                    .unwrap_or_default()
                                    .to_string_lossy()
                                    .into_owned()),
                                path: Some(self.db.root_dir.join(path).to_string_lossy().into_owned()),
                                .. Default::default()
                            });
                            dap_frame.line = i64::from(proc.location.line);
                            //dap_frame.column = i64::from(proc.location.column);
                        }
                    }

                    if let Some(line) = aux_frame.line {
                        dap_frame.line = line as i64;
                    }

                    frames.push(dap_frame);
                }

                StackTraceResponse {
                    totalFrames: Some(aux_frames_total as i64),
                    stackFrames: frames,
                }
            }
        }
    }

    on Scopes(&mut self, ScopesArguments { frameId }) {
        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;
                let frame_id = frameId as usize;

                let threads = extools.get_all_threads();
                let thread_id = (frame_id % threads.len()) as i64;
                let frame_no = frame_id / threads.len();

                guard!(let Some(frame) = threads[&thread_id].call_stack.get(frame_no) else {
                    return Err(Box::new(GenericError2(format!("Stack frame out of range: {} (thread {}, depth {})", frameId, thread_id, frame_no))));
                });

                ScopesResponse {
                    scopes: vec![
                        Scope {
                            name: "Locals".to_owned(),
                            presentationHint: Some("locals".to_owned()),
                            variablesReference: frameId * 2 + 2,
                            indexedVariables: Some(frame.locals.len() as i64),
                            .. Default::default()
                        },
                        Scope {
                            name: "Arguments".to_owned(),
                            presentationHint: Some("arguments".to_owned()),
                            variablesReference: frameId * 2 + 1,
                            namedVariables: Some(2 + frame.args.len() as i64),
                            .. Default::default()
                        },
                        Scope {
                            name: "Globals".to_owned(),
                            variablesReference: 0x0e_000001,
                            .. Default::default()
                        },
                    ]
                }
            }

            DebugClient::Auxtools(auxtools) => {
                let AuxtoolsScopes { arguments, locals, globals } = auxtools.get_scopes(frameId as u32)?;
                let mut scopes = Vec::with_capacity(locals.is_some() as usize + arguments.is_some() as usize + globals.is_some() as usize);

                if let Some(locals) = locals {
                    scopes.push(Scope {
                        name: "Locals".to_owned(),
                        variablesReference: locals.0 as i64,
                        .. Default::default()
                    });
                }

                if let Some(arguments) = arguments {
                    scopes.push(Scope {
                        name: "Arguments".to_owned(),
                        variablesReference: arguments.0 as i64,
                        .. Default::default()
                    });
                }

                if let Some(globals) = globals {
                    scopes.push(Scope {
                        name: "Globals".to_owned(),
                        variablesReference: globals.0 as i64,
                        .. Default::default()
                    });
                }

                ScopesResponse {
                    scopes
                }
            }
        }
    }

    on Variables(&mut self, params) {
        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;

                if params.variablesReference >= 0x01_000000 {
                    let (var, ref_) = extools_types::ValueText::from_variables_reference(params.variablesReference);
                    let mut variables = Vec::new();

                    if var.is_list {
                        // List reference
                        match extools.get_list_contents(ref_)? {
                            extools_types::ListContents::Linear(entries) => {
                                for (i, entry) in entries.iter().enumerate() {
                                    variables.push(Variable {
                                        name: format!("[{}]", 1 + i),
                                        value: entry.to_string(),
                                        variablesReference: entry.to_variables_reference(),
                                        .. Default::default()
                                    });
                                }
                            }
                            extools_types::ListContents::Associative(entries) => {
                                for (i, (key, val)) in entries.iter().enumerate() {
                                    variables.push(Variable {
                                        name: format!("keys[{}]", 1 + i),
                                        value: key.to_string(),
                                        variablesReference: key.to_variables_reference(),
                                        .. Default::default()
                                    });
                                    variables.push(Variable {
                                        name: format!("vals[{}]", 1 + i),
                                        value: val.to_string(),
                                        variablesReference: val.to_variables_reference(),
                                        .. Default::default()
                                    });
                                }
                            }
                        }
                    } else if var.has_vars {
                        // Datum reference
                        let hashmap = extools.get_all_fields(ref_)?;
                        let mut entries: Vec<_> = hashmap.iter().collect();
                        entries.sort_unstable_by_key(|tup| tup.0);
                        for (name, vt) in entries {
                            variables.push(Variable {
                                name: name.to_owned(),
                                value: vt.to_string(),
                                variablesReference: vt.to_variables_reference(),
                                .. Default::default()
                            })
                        }
                    }

                    return Ok(VariablesResponse { variables });
                }

                // Stack frame, arguments or locals
                let frame_id = (params.variablesReference - 1) / 2;
                let mod2 = params.variablesReference % 2;

                let (thread, frame_no) = extools.get_thread_by_frame_id(frame_id)?;
                guard!(let Some(frame) = thread.call_stack.get(frame_no) else {
                    return Err(Box::new(GenericError("Stack frame out of range")));
                });

                if mod2 == 1 {
                    // arguments
                    let mut variables = Vec::with_capacity(2 + frame.args.len());
                    let mut seen = std::collections::HashMap::with_hasher(RandomState::default());

                    seen.insert("src", 0);
                    variables.push(Variable {
                        name: "src".to_owned(),
                        value: frame.src.to_string(),
                        variablesReference: frame.src.to_variables_reference(),
                        .. Default::default()
                    });
                    seen.insert("usr", 0);
                    variables.push(Variable {
                        name: "usr".to_owned(),
                        value: frame.usr.to_string(),
                        variablesReference: frame.usr.to_variables_reference(),
                        .. Default::default()
                    });

                    variables.extend(frame.args.iter().enumerate().map(|(i, vt)| Variable {
                        name: match frame.arg_names.get(i) {
                            Some(param) => {
                                match seen.entry(param).and_modify(|e| *e += 1).or_default() {
                                    0 => param.clone(),
                                    n => format!("{} #{}", param, n),
                                }
                            }
                            None => format!("args[{}]", i + 1),
                        },
                        value: vt.to_string(),
                        variablesReference: vt.to_variables_reference(),
                        .. Default::default()
                    }));
                    VariablesResponse { variables }
                } else if mod2 == 0 {
                    // locals
                    let mut variables = Vec::with_capacity(1 + frame.locals.len());

                    variables.push(Variable {
                        name: ".".to_owned(),
                        value: frame.dot.to_string(),
                        variablesReference: frame.dot.to_variables_reference(),
                        .. Default::default()
                    });

                    // If VSC receives two Variables with the same name, it only
                    // displays the first one. Avert this by adding suffixes.
                    let mut seen = std::collections::HashMap::with_hasher(RandomState::default());
                    variables.extend(frame.locals.iter().enumerate().map(|(i, vt)| Variable {
                        name: match frame.local_names.get(i) {
                            Some(local) => {
                                match seen.entry(local).and_modify(|e| *e += 1).or_default() {
                                    0 => local.clone(),
                                    n => format!("{} #{}", local, n),
                                }
                            }
                            None => i.to_string(),
                        },
                        value: vt.to_string(),
                        variablesReference: vt.to_variables_reference(),
                        .. Default::default()
                    }));
                    VariablesResponse { variables }
                } else {
                    return Err(Box::new(GenericError("Bad variables reference")));
                }
            }

            DebugClient::Auxtools(auxtools) => {
                let aux_variables = auxtools.get_variables(auxtools_types::VariablesRef(params.variablesReference as i32))?;
                let mut variables = vec![];

                // If VSC receives two Variables with the same name, it only
                // displays the first one. Avert this by adding suffixes.
                let mut seen = std::collections::HashMap::with_hasher(RandomState::default());

                for aux_var in aux_variables {
                    let name = match seen.entry(aux_var.name.clone()).and_modify(|e| *e += 1).or_default() {
                        0 => aux_var.name,
                        n => format!("{} #{}", aux_var.name, n),
                    };

                    variables.push(Variable {
                        name,
                        value: aux_var.value,
                        variablesReference: aux_var.variables.map(|x| x.0 as i64).unwrap_or(0),
                        .. Default::default()
                    });
                }

                VariablesResponse {
                    variables
                }
            }
        }
    }

    on Continue(&mut self, _params) {
        self.notify_continue();

        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;
                extools.continue_execution();
            }

            DebugClient::Auxtools(auxtools) => {
                auxtools.continue_execution()?;
            }
        }

        ContinueResponse {
            allThreadsContinued: Some(true),
        }
    }

    on StepIn(&mut self, params) {
        self.notify_continue();

        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;
                extools.step_in(params.threadId);
            }

            DebugClient::Auxtools(auxtools) => {
                auxtools.step_into(params.threadId as u32)?;
            }
        }
    }

    on Next(&mut self, params) {
        self.notify_continue();

        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;
                extools.step_over(params.threadId);
            }

            DebugClient::Auxtools(auxtools) => {
                auxtools.next(params.threadId as u32)?;
            }
        }
    }

    on StepOut(&mut self, params) {
        self.notify_continue();

        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;
                extools.step_out(params.threadId);
            }

            DebugClient::Auxtools(auxtools) => {
                auxtools.step_out(params.threadId as u32)?;
            }
        }
    }

    on Pause(&mut self, _params) {
        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;
                extools.pause();
            }

            DebugClient::Auxtools(auxtools) => {
                auxtools.pause()?;
            }
        }
    }

    on SetExceptionBreakpoints(&mut self, params) {
        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;
                extools.set_break_on_runtime(params.filters.iter().any(|x| x == EXCEPTION_FILTER_RUNTIMES));
            }

            DebugClient::Auxtools(auxtools) => {
                auxtools.set_catch_runtimes(params.filters.iter().any(|x| x == EXCEPTION_FILTER_RUNTIMES))?;
            }
        }
    }

    on ExceptionInfo(&mut self, _params) {
        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;
                // VSC shows exceptionId, description, stackTrace in that order.
                let message = extools.last_error_message();
                ExceptionInfoResponse {
                    exceptionId: message.unwrap_or_default().to_owned(),
                    description: None,
                    breakMode: ExceptionBreakMode::Always,
                    details: None,
                }
            }

            DebugClient::Auxtools(auxtools) => {
                ExceptionInfoResponse {
                    exceptionId: auxtools.get_last_error_message(),
                    description: None,
                    breakMode: ExceptionBreakMode::Always,
                    details: None,
                }
            }
        }
    }

    on Evaluate(&mut self, params) {
        self.evaluate(params)?
    }

    on Source(&mut self, params) {
        let mut source_reference = params.sourceReference;
        if let Some(source) = params.source {
            if let Some(reference) = source.sourceReference {
                source_reference = reference;
            }
        }

        if source_reference != STDDEF_SOURCE_REFERENCE {
            return Err(Box::new(GenericError("Unknown source reference")));
        }

        if let Some(info) = self.stddef_dm_info.as_ref() {
            SourceResponse::from(info.text.clone())
        } else {
            return Err(Box::new(GenericError("stddef.dm not available")));
        }
    }

    on Disassemble(&mut self, params) {
        match &mut self.client {
            DebugClient::Extools(extools) => {
                guard!(let Some(captures) = MEMORY_REFERENCE_REGEX.captures(&params.memoryReference) else {
                    return Err(Box::new(GenericError("Invalid memory reference")));
                });
                let proc = &captures[1];
                let override_id: usize = captures[2].parse()?;
                //let offset: i64 = captures[3].parse()?;

                let extools = extools.get()?;
                let mut result = Vec::new();
                for instr in extools.bytecode(proc, override_id) {
                    result.push(DisassembledInstruction {
                        address: format!("{}#{}@{}", proc, override_id, instr.offset),
                        instructionBytes: Some(instr.bytes.clone()),
                        instruction: format!("{}  {}", instr.mnemonic, instr.comment),
                        .. Default::default()
                    });
                }

                DisassembleResponse {
                    instructions: result
                }
            }

            DebugClient::Auxtools(_) => {
                return Err(Box::new(GenericError("auxtools can't disassemble yet")));
            }
        }
    }
}

lazy_static! {
    // `/proc#override@offset`
    static ref MEMORY_REFERENCE_REGEX: regex::Regex = regex::Regex::new(r"^([^#]+)#(\d+)@(\d+)$").unwrap();
}

#[derive(Default, Debug)]
struct ClientCaps {
    lines_start_at_1: bool,
    columns_start_at_1: bool,
    variable_type: bool,
    variable_paging: bool,
    run_in_terminal: bool,
    memory_references: bool,
}

impl ClientCaps {
    fn parse(params: &InitializeRequestArguments) -> ClientCaps {
        ClientCaps {
            lines_start_at_1: params.linesStartAt1.unwrap_or(true),
            columns_start_at_1: params.columnsStartAt1.unwrap_or(true),

            variable_type: params.supportsVariableType.unwrap_or(false),
            variable_paging: params.supportsVariablePaging.unwrap_or(false),
            run_in_terminal: params.supportsRunInTerminalRequest.unwrap_or(false),
            memory_references: params.supportsMemoryReferences.unwrap_or(false),
        }
    }
}

type OutStream = Box<dyn std::io::Write + Send>;

pub struct SequenceNumber {
    seq: atomic::AtomicI64,
    stream: Mutex<OutStream>,
}

impl SequenceNumber {
    fn new(stream: OutStream) -> SequenceNumber {
        SequenceNumber {
            seq: Default::default(),
            stream: Mutex::new(stream),
        }
    }

    fn next(&self) -> i64 {
        self.seq.fetch_add(1, atomic::Ordering::Relaxed)
    }

    fn issue_event<E: Event>(&self, event: E) {
        let body = serde_json::to_value(event).expect("event body encode error");
        let message = EventMessage {
            protocol_message: ProtocolMessage {
                seq: self.next(),
                type_: EventMessage::TYPE.to_owned(),
            },
            event: E::EVENT.to_owned(),
            body: Some(body),
        };
        self.send_raw(&serde_json::to_string(&message).expect("event encode error"))
    }

    fn println<S: Into<String>>(&self, output: S) {
        let mut output = output.into();
        output.push('\n');
        self.issue_event(OutputEvent {
            output,
            ..Default::default()
        })
    }

    fn eprintln<S: Into<String>>(&self, output: S) {
        let mut output = output.into();
        output.push('\n');
        self.issue_event(OutputEvent {
            output,
            category: Some("console".to_owned()),
            ..Default::default()
        })
    }

    fn send_raw(&self, json: &str) {
        let mut stream = self.stream.lock().expect("DAP output stream poisoned");
        jrpc_io::write_to(&mut *stream, json);
    }
}

#[derive(Debug)]
pub struct GenericError(&'static str);

impl Error for GenericError {
    fn description(&self) -> &str {
        self.0
    }
}

impl std::fmt::Display for GenericError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.write_str(self.0)
    }
}

#[derive(Debug)]
pub struct GenericError2(String);

impl Error for GenericError2 {
    fn description(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for GenericError2 {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.write_str(&self.0)
    }
}

// ----------------------------------------------------------------------------
// Implementation-specific DAP extensions.

enum LaunchVsc {}

impl Request for LaunchVsc {
    type Params = LaunchRequestArgumentsVsc;
    type Result = <Launch as Request>::Result;
    const COMMAND: &'static str = Launch::COMMAND;
}

#[derive(Deserialize)]
pub struct LaunchRequestArgumentsVsc {
    #[serde(flatten)]
    base: LaunchRequestArguments,

    // provided by vscode
    dmb: String,
    // other keys: __sessionId, name, preLaunchTask, request, type
}

enum AttachVsc {}

impl Request for AttachVsc {
    type Params = AttachRequestArgumentsVsc;
    type Result = <Attach as Request>::Result;
    const COMMAND: &'static str = Attach::COMMAND;
}

#[derive(Deserialize)]
pub struct AttachRequestArgumentsVsc {
    #[serde(flatten)]
    base: AttachRequestArguments,
    port: Option<u16>,
}

// ----------------------------------------------------------------------------
// Tables

// These procs are currently considered "builtins" by the parser, but unlike
// other builtins, they are written in softcode in the the internal `stddef.dm`
// file and therefore *do* take an `override_id` slot.
// TODO: Reserve a FileId for `stddef.dm` instead of using a table here.
const STDDEF_PROCS: &[&str] = &[
    "/sound/New",
    "/sound/RscFile",
    "/icon/New",
    "/icon/Icon",
    "/icon/RscFile",
    "/icon/IconStates",
    "/icon/Turn",
    "/icon/Flip",
    "/icon/Shift",
    "/icon/SetIntensity",
    "/icon/Blend",
    "/icon/SwapColor",
    "/icon/DrawBox",
    "/icon/Insert",
    "/icon/MapColors",
    "/icon/Scale",
    "/icon/Crop",
    "/icon/GetPixel",
    "/icon/Width",
    "/icon/Height",
    "/matrix/New",
    "/matrix/Multiply",
    "/matrix/Add",
    "/matrix/Subtract",
    "/matrix/Invert",
    "/matrix/Turn",
    "/matrix/Scale",
    "/matrix/Translate",
    "/matrix/Interpolate",
    "/database/New",
    "/database/Open",
    "/database/Close",
    "/database/Error",
    "/database/ErrorMsg",
    "/database/query/New",
    "/database/query/Open",
    "/database/query/Clear",
    "/database/query/Add",
    "/database/query/Execute",
    "/database/query/NextRow",
    "/database/query/RowsAffected",
    "/database/query/Reset",
    "/database/query/Columns",
    "/database/query/GetColumn",
    "/database/query/GetRowData",
    "/database/query/Close",
    "/exception/New",
    "/regex/New",
    "/regex/Find",
    "/regex/Replace",
];

const STDDEF_SOURCE_REFERENCE: i64 = 1;

struct StddefDmInfo {
    text: String,
    objtree: ObjectTree,
}

impl StddefDmInfo {
    fn new(text: String) -> StddefDmInfo {
        let context = dm::Context::default();
        let pp = dm::preprocessor::Preprocessor::from_buffer(&context, "stddef.dm".into(), &text);
        let parser =
            dm::parser::Parser::new(&context, dm::indents::IndentProcessor::new(&context, pp));
        let objtree = parser.parse_object_tree_without_builtins();
        StddefDmInfo { text, objtree }
    }
}
