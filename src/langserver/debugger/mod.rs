//! Debug adapter protocol implementation for DreamSeeker.
//!
//! * https://microsoft.github.io/debug-adapter-protocol/
#![allow(dead_code)]

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
    ($($rest:tt)*) => { output!($($rest)*) }
}

#[cfg(not(debug_assertions))]
macro_rules! debug_output {
    ($($rest:tt)*) => { {} }
}

mod dap_types;
mod launched;
mod extools_types;
mod extools;
mod local_names;

use std::error::Error;
use std::sync::{atomic, Arc, Mutex};
use std::collections::{HashMap, HashSet};

use dm::FileId;
use dm::objtree::ObjectTree;

use crate::jrpc_io;
use self::dap_types::*;
use self::launched::Launched;
use self::extools::Extools;

pub fn start_server(dreamseeker_exe: String, db: DebugDatabaseBuilder) -> std::io::Result<u16> {
    use std::net::*;

    let listener = TcpListener::bind((Ipv4Addr::LOCALHOST, 0))?;
    let port = listener.local_addr()?.port();
    eprintln!("listening for debugger connection on port {}", port);

    std::thread::Builder::new()
        .name(format!("DAP listener on port {}", port))
        .spawn(move || {
            let (stream, _) = listener.accept().unwrap();
            drop(listener);

            let mut input = std::io::BufReader::new(stream.try_clone().unwrap());
            let mut debugger = Debugger::new(dreamseeker_exe, db, Box::new(stream));
            jrpc_io::run_with_read(&mut input, |message| debugger.handle_input(message));
        })?;

    Ok(port)
}

pub fn debugger_main<I: Iterator<Item=String>>(mut args: I) {
    eprintln!("acting as debug adapter");
    let mut dreamseeker_exe = None;

    while let Some(arg) = args.next() {
        if arg == "--dreamseeker-exe" {
            dreamseeker_exe = Some(args.next().expect("must specify a value for --dreamseeker-exe"));
        } else {
            panic!("unknown argument {:?}", arg);
        }
    }

    let dreamseeker_exe = dreamseeker_exe.expect("must provide argument `--dreamseeker-exe path/to/dreamseeker.exe`");
    eprintln!("dreamseeker: {}", dreamseeker_exe);

    // This isn't the preferred way to run the DAP server so it's okay for it
    // to be kind of sloppy.
    let environment = dm::detect_environment_default().expect("detect .dme error").expect("did not detect a .dme");
    let ctx = dm::Context::default();
    let mut pp = dm::preprocessor::Preprocessor::new(&ctx, environment).unwrap();
    let objtree = {
        let mut parser = dm::parser::Parser::new(&ctx, dm::indents::IndentProcessor::new(&ctx, &mut pp));
        parser.enable_procs();
        Arc::new(parser.parse_object_tree())
    };

    let db = DebugDatabaseBuilder {
        root_dir: Default::default(),
        files: ctx,
        objtree,
    };
    let mut debugger = Debugger::new(dreamseeker_exe, db, Box::new(std::io::stdout()));
    jrpc_io::run_forever(|message| debugger.handle_input(message));
}

pub struct DebugDatabaseBuilder {
    pub root_dir: std::path::PathBuf,
    pub files: dm::Context,
    pub objtree: Arc<ObjectTree>,
}

impl DebugDatabaseBuilder {
    fn build(self) -> DebugDatabase {
        let DebugDatabaseBuilder { root_dir, files, objtree } = self;
        let mut line_numbers: HashMap<dm::FileId, Vec<(i64, String, String, usize)>> = HashMap::new();

        objtree.root().recurse(&mut |ty| {
            for (name, proc) in ty.procs.iter() {
                for (override_id, pv) in proc.value.iter()
                    .skip_while(|pv| pv.location.is_builtins() && !STDDEF_PROCS.contains(&format!("{}/{}", ty.path, name).as_str()))
                    .enumerate()
                {
                    line_numbers.entry(pv.location.file)
                        .or_default()
                        .push((
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
            local_names: Default::default(),
        }
    }
}

pub struct DebugDatabase {
    root_dir: std::path::PathBuf,
    files: dm::Context,
    objtree: Arc<ObjectTree>,
    line_numbers: HashMap<dm::FileId, Vec<(i64, String, String, usize)>>,
    local_names: HashMap<(String, usize), Option<Vec<String>>>,
}

fn get_proc<'o>(objtree: &'o ObjectTree, proc_ref: &str, override_id: usize) -> Option<&'o dm::objtree::ProcValue> {
    let mut bits: Vec<&str> = proc_ref.split('/').collect();
    let procname = bits.pop().unwrap();
    match bits.last() {
        Some(&"proc") | Some(&"verb") => { bits.pop(); }
        _ => {}
    }
    let typename = bits.join("/");

    if let Some(ty) = objtree.find(&typename) {
        if let Some(ty_proc) = ty.get().procs.get(procname) {
            // Don't consider (most) builtins against the override_id count.
            return ty_proc.value.iter()
                .skip_while(|pv| pv.location.is_builtins() && !STDDEF_PROCS.contains(&proc_ref))
                .skip(override_id)
                .next();
        }
    }
    None
}

impl DebugDatabase {
    fn get_proc(&self, proc_ref: &str, override_id: usize) -> Option<&dm::objtree::ProcValue> {
        get_proc(&self.objtree, proc_ref, override_id)
    }

    fn get_local_names(&mut self, proc_ref: &str, override_id: usize) -> Option<&[String]> {
        let objtree = &self.objtree;
        self.local_names.entry((proc_ref.to_owned(), override_id)).or_insert_with(|| {
            let proc = get_proc(objtree, proc_ref, override_id)?;
            if let dm::objtree::Code::Present(ref code) = proc.code {
                Some(local_names::extract(code))
            } else {
                None
            }
        }).as_ref().map(|x| &x[..])
    }

    fn file_id(&self, file_path: &str) -> Option<FileId> {
        let path = std::path::Path::new(file_path);
        self.files.get_file(path.strip_prefix(&self.root_dir).unwrap_or(path))
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

struct Debugger {
    dreamseeker_exe: String,
    db: DebugDatabase,
    launched: Option<Launched>,
    extools: Option<Extools>,

    seq: Arc<SequenceNumber>,
    client_caps: ClientCaps,

    saved_breakpoints: HashMap<FileId, HashSet<(String, usize, i64)>>,
}

impl Debugger {
    fn new(dreamseeker_exe: String, db: DebugDatabaseBuilder, stream: OutStream) -> Self {
        Debugger {
            dreamseeker_exe,
            db: db.build(),
            launched: None,
            extools: None,

            seq: Arc::new(SequenceNumber::new(stream)),
            client_caps: Default::default(),

            saved_breakpoints: Default::default(),
        }
    }

    fn handle_input(&mut self, message: &str) {
        // TODO: error handling
        self.handle_input_inner(message).expect("error in handle_input");
    }

    fn handle_input_inner(&mut self, message: &str) -> Result<(), Box<dyn Error>> {
        let protocol_message = serde_json::from_str::<ProtocolMessage>(message)?;
        match protocol_message.type_.as_str() {
            RequestMessage::TYPE => {
                let request = serde_json::from_str::<RequestMessage>(message)?;
                let request_seq = request.protocol_message.seq;
                let command = request.command.clone();

                let handled = match Self::handle_request_table(&request.command) {
                    Some(handler) => handler(self, request.arguments.unwrap_or(serde_json::Value::Null)),
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
                            output!(in self.seq, "[main] Error responding to {:?}: {}", command, err);
                            debug_output!(in self.seq, " - {}", message);
                            None
                        },
                    },
                    command,
                };
                self.seq.send_raw(&serde_json::to_string(&response).expect("response encode error"))
            }
            other => return Err(format!("unknown `type` field {:?}", other).into())
        }
        Ok(())
    }

    #[inline]
    fn issue_event<E: Event>(&mut self, event: E) {
        self.seq.issue_event(event);
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
                debug_output!(in self.seq, "[main] client capabilities: {}", &debug[start + 2..end - 1]);
            } else {
                debug_output!(in self.seq, "[main] client capabilities: {}", debug);
            }
        }

        // ... clientID, clientName, adapterID, locale, pathFormat

        // Tell the client our caps
        Some(Capabilities {
            supportTerminateDebuggee: Some(true),
            supportsExceptionInfoRequest: Some(true),
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
        // Extools is not currently enabled on release langserver builds.
        let debug = cfg!(debug_assertions) && !params.base.noDebug.unwrap_or(false);
        self.launched = Some(Launched::new(self.seq.clone(), &self.dreamseeker_exe, &params.dmb, debug)?);
        if debug {
            self.extools = Extools::connect(self.seq.clone(), extools::DEFAULT_PORT)?;
        }
    }

    on AttachVsc(&mut self, params) {
        self.extools = Extools::connect(self.seq.clone(), params.port.unwrap_or(extools::DEFAULT_PORT))?;
    }

    on Disconnect(&mut self, params) {
        let default_terminate = self.launched.is_some();
        let terminate = params.terminateDebuggee.unwrap_or(default_terminate);

        drop(self.extools.take());

        if let Some(launched) = self.launched.take() {
            if terminate {
                launched.kill()?;
            } else {
                launched.detach();
            }
        }
    }

    on Threads(&mut self, ()) {
        ThreadsResponse {
            threads: vec![Thread { id: 0, name: "Main".to_owned() }]
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
        let mut breakpoints = Vec::new();

        guard!(let Some(extools) = self.extools.as_mut() else {
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

        let saved = self.saved_breakpoints.entry(file_id).or_default();
        let mut keep = HashSet::new();

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
                        .. Default::default()
                    });
                } else {
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
            if !keep.contains(&k) {
                extools.unset_breakpoint(&k.0, k.1, k.2);
                false
            } else {
                true
            }
        });

        SetBreakpointsResponse { breakpoints }
    }

    on StackTrace(&mut self, params) {
        guard!(let Some(extools) = self.extools.as_mut() else {
            return Err(Box::new(GenericError("No extools connection")));
        });
        guard!(let Some(thread) = extools.get_thread(params.threadId) else {
            return Err(Box::new(GenericError("Bad thread ID")));
        });

        let len = thread.call_stack.len();
        let mut frames = Vec::with_capacity(len);
        for (i, ex_frame) in thread.call_stack.into_iter().enumerate() {
            let mut dap_frame = StackFrame {
                name: ex_frame.name.clone(),
                id: i as i64,
                .. Default::default()
            };

            if let Some(proc) = self.db.get_proc(&ex_frame.name, ex_frame.override_id) {
                let path = self.db.files.file_path(proc.location.file);

                dap_frame.source = Some(Source {
                    name: Some(path.file_name()
                        .unwrap_or_default()
                        .to_string_lossy()
                        .into_owned()),
                    path: Some(self.db.root_dir.join(path).to_string_lossy().into_owned()),
                    .. Default::default()
                });
                dap_frame.line = i64::from(proc.location.line);
                dap_frame.column = i64::from(proc.location.column);
            }

            if let Some(line) = extools.offset_to_line(&ex_frame.name, ex_frame.override_id, ex_frame.instruction_pointer) {
                dap_frame.line = line;
                // Column must be nonzero for VSC to show the exception widget.
                dap_frame.column = 1;
            }

            frames.push(dap_frame);
        }

        StackTraceResponse {
            totalFrames: Some(len as i64),
            stackFrames: frames,
        }
    }

    on Scopes(&mut self, ScopesArguments { frameId }) {
        guard!(let Some(extools) = self.extools.as_ref() else {
            return Err(Box::new(GenericError("No extools connection")));
        });
        guard!(let Some(thread) = extools.get_thread(0) else {
            return Err(Box::new(GenericError("Bad thread ID")));
        });
        guard!(let Some(frame) = thread.call_stack.get(frameId as usize) else {
            return Err(Box::new(GenericError("Stack frame out of range")));
        });

        ScopesResponse {
            scopes: vec![
                Scope {
                    name: "Arguments".to_owned(),
                    presentationHint: Some("arguments".to_owned()),
                    variablesReference: frameId * 2 + 1,
                    namedVariables: Some(2 + frame.args.len() as i64),
                    .. Default::default()
                },
                Scope {
                    name: "Locals".to_owned(),
                    presentationHint: Some("locals".to_owned()),
                    variablesReference: frameId * 2 + 2,
                    indexedVariables: Some(frame.locals.len() as i64),
                    .. Default::default()
                }
            ]
        }
    }

    on Variables(&mut self, params) {
        guard!(let Some(extools) = self.extools.as_ref() else {
            return Err(Box::new(GenericError("No extools connection")));
        });
        guard!(let Some(thread) = extools.get_thread(0) else {
            return Err(Box::new(GenericError("Bad thread ID")));
        });

        if params.variablesReference >= 0x1000000 {
            // Datum reference
            let typepath = extools.get_reference_type(params.variablesReference)?;
            guard!(let Some(ty) = self.db.objtree.find(&typepath) else {
                return Err(Box::new(GenericError("Unable to find type according to typepath")));
            });

            let mut variables = Vec::new();

            let mut next = Some(ty);
            while let Some(current) = next {
                for (name, ty_var) in current.vars.iter() {
                    if ty_var.declaration.is_some() {
                        match extools.get_reference_field(params.variablesReference, name) {
                            Ok(vt) => variables.push(Variable {
                                name: name.to_owned(),
                                value: vt.to_string(),
                                variablesReference: vt.datum_address(),
                                .. Default::default()
                            }),
                            Err(e) => variables.push(Variable {
                                name: name.to_owned(),
                                value: e.to_string(),
                                .. Default::default()
                            })
                        }
                    }
                }
                next = crate::ignore_root(current.parent_type());
            }

            return Ok(VariablesResponse { variables });
        }

        // Stack frame, arguments or locals
        let frame_idx = (params.variablesReference - 1) / 2;
        let mod2 = params.variablesReference % 2;

        guard!(let Some(frame) = thread.call_stack.get(frame_idx as usize) else {
            return Err(Box::new(GenericError("Stack frame out of range")));
        });

        let (parameters, locals);
        let objtree = self.db.objtree.clone();
        if let Some(proc) = get_proc(&objtree, &frame.name, frame.override_id) {
            parameters = &proc.parameters[..];
            locals = self.db.get_local_names(&frame.name, frame.override_id).unwrap_or(&[]);
        } else {
            parameters = &[];
            locals = &[];
        }

        if mod2 == 1 {
            // arguments
            let mut variables = Vec::with_capacity(2 + frame.args.len());

            variables.push(Variable {
                name: "src".to_string(),
                value: frame.src.to_string(),
                variablesReference: frame.src.datum_address(),
                .. Default::default()
            });
            variables.push(Variable {
                name: "usr".to_string(),
                value: frame.usr.to_string(),
                variablesReference: frame.usr.datum_address(),
                .. Default::default()
            });

            variables.extend(frame.args.iter().enumerate().map(|(i, vt)| Variable {
                name: match parameters.get(i) {
                    Some(param) => param.name.clone(),
                    None => i.to_string(),
                },
                value: vt.to_string(),
                variablesReference: vt.datum_address(),
                .. Default::default()
            }));
            VariablesResponse { variables }
        } else if mod2 == 0 {
            // locals
            let variables = frame.locals.iter().enumerate().map(|(i, vt)| Variable {
                name: match locals.get(i) {
                    Some(local) => local.clone(),
                    None => i.to_string(),
                },
                value: vt.to_string(),
                variablesReference: vt.datum_address(),
                .. Default::default()
            }).collect();
            VariablesResponse { variables }
        } else {
            return Err(Box::new(GenericError("Bad variables reference")));
        }
    }

    on Continue(&mut self, _params) {
        guard!(let Some(extools) = self.extools.as_ref() else {
            return Err(Box::new(GenericError("No extools connection")));
        });

        extools.continue_execution();
        ContinueResponse {
            allThreadsContinued: Some(true),
        }
    }

    on StepIn(&mut self, _params) {
        guard!(let Some(extools) = self.extools.as_ref() else {
            return Err(Box::new(GenericError("No extools connection")));
        });

        extools.step_in();
    }

    on Next(&mut self, _params) {
        guard!(let Some(extools) = self.extools.as_ref() else {
            return Err(Box::new(GenericError("No extools connection")));
        });

        extools.step_over();
    }

    on Pause(&mut self, _params) {
        guard!(let Some(extools) = self.extools.as_ref() else {
            return Err(Box::new(GenericError("No extools connection")));
        });

        extools.step_in();
    }

    on SetExceptionBreakpoints(&mut self, params) {
        guard!(let Some(extools) = self.extools.as_ref() else {
            return Err(Box::new(GenericError("No extools connection")));
        });

        extools.set_break_on_runtime(params.filters.iter().any(|x| x == EXCEPTION_FILTER_RUNTIMES));
    }

    on ExceptionInfo(&mut self, _params) {
        guard!(let Some(extools) = self.extools.as_mut() else {
            return Err(Box::new(GenericError("No extools connection")));
        });

        // VSC shows exceptionId, description, stackTrace in that order.
        let message = extools.last_error_message();
        ExceptionInfoResponse {
            exceptionId: message.unwrap_or_default().to_owned(),
            description: None,
            breakMode: ExceptionBreakMode::Always,
            details: None,
        }
    }
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
            .. Default::default()
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
    fn description(&self) -> &str { self.0 }
}

impl std::fmt::Display for GenericError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.write_str(self.0)
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
    "/regex/Replace"
];
