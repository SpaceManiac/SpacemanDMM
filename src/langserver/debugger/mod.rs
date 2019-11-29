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

use std::error::Error;
use std::sync::{atomic, Arc};

use io;
use self::dap_types::*;
use self::launched::Launched;
use self::extools::Extools;

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

    let mut debugger = Debugger::new(dreamseeker_exe);
    io::run_forever(|message| debugger.handle_input(message));
}

struct Debugger {
    dreamseeker_exe: String,
    launched: Option<Launched>,
    extools: Option<Extools>,
    // TODO: separate field from `child` for attached debugger.

    seq: Arc<SequenceNumber>,
    client_caps: ClientCaps,
}

impl Debugger {
    fn new(dreamseeker_exe: String) -> Self {
        Debugger {
            dreamseeker_exe,
            launched: None,
            extools: None,

            seq: Default::default(),
            client_caps: Default::default(),
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

                let handled = self.handle_request(request);
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
                io::write(serde_json::to_string(&response).expect("response encode error"))
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
            .. Default::default()
        })
    }

    on LaunchVsc(&mut self, params) {
        self.launched = Some(Launched::new(self.seq.clone(), &self.dreamseeker_exe, &params.dmb)?);

        if !params.base.noDebug.unwrap_or(false) {
            self.extools = Some(Extools::connect(self.seq.clone())?);
            self.seq.issue_event(InitializedEvent);
        }
    }

    on Disconnect(&mut self, params) {
        // TODO: `false` if `attach` was used instead of `launch`.
        let default_terminate = true;
        let terminate = params.terminateDebuggee.unwrap_or(default_terminate);

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

    on StackTrace(&mut self, params) {
        if let Some(extools) = self.extools.as_ref() {
            if let Some(thread) = extools.get_thread(params.threadId) {
                return Ok(StackTraceResponse {
                    totalFrames: Some(thread.call_stack.len() as i64),
                    stackFrames: thread.call_stack.into_iter().map(|name| StackFrame {
                        name,
                        // TODO: id, source, line
                        .. Default::default()
                    }).collect(),
                });
            }
        }
        return Err(Box::new(std::io::Error::from(std::io::ErrorKind::InvalidData)));
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

#[derive(Default)]
pub struct SequenceNumber(atomic::AtomicI64);

impl SequenceNumber {
    fn next(&self) -> i64 {
        self.0.fetch_add(1, atomic::Ordering::Relaxed)
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
        io::write(serde_json::to_string(&message).expect("event encode error"))
    }

    fn println<S: Into<String>>(&self, output: S) {
        let mut output = output.into();
        output.push('\n');
        self.issue_event(OutputEvent {
            output,
            .. Default::default()
        })
    }
}

// ----------------------------------------------------------------------------
// Implementation-specific DAP extensions.

enum LaunchVsc {}

impl Request for LaunchVsc {
    type Params = LaunchRequestArgumentsVsc;
    type Result = ();
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
