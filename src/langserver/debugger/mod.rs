//! Debug adapter protocol implementation for DreamSeeker.
//!
//! * https://microsoft.github.io/debug-adapter-protocol/

mod dap_types;

use std::error::Error;
use io;
use self::dap_types::*;

pub fn debugger_main<I: Iterator<Item=String>>(mut args: I) {
    eprintln!("entering debugger mode...");
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

#[derive(Default)]
struct ClientCaps {
    lines_start_at_1: bool,
    columns_start_at_1: bool,
    variable_type: bool,
    variable_paging: bool,
    run_in_terminal: bool,
    memory_references: bool,
}

struct Debugger {
    dreamseeker_exe: String,
    seq: i64,

    client_caps: ClientCaps,
}

impl Debugger {
    fn new(dreamseeker_exe: String) -> Self {
        Debugger {
            dreamseeker_exe,
            seq: 0,

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
            "request" => {
                let request = serde_json::from_str::<RequestMessage>(message)?;
                let request_seq = request.protocol_message.seq;
                let command = request.command.clone();

                let handled = self.handle_request(request);
                let response = Response {
                    protocol_message: ProtocolMessage {
                        seq: self.next_seq(),
                        type_: "response".to_owned(),
                    },
                    request_seq,
                    command,
                    success: handled.is_ok(),
                    message: handled.as_ref().err().map(|err| err.description().to_owned()),
                    body: match handled {
                        Ok(result) => Some(result),
                        Err(_) => None,
                    }
                };
                io::write(serde_json::to_string(&response).expect("encode error"))
            }
            other => return Err(format!("unknown `type` field {:?}", other).into())
        }
        Ok(())
    }

    fn next_seq(&mut self) -> i64 {
        self.seq = self.seq.wrapping_add(1);
        self.seq
    }
}

handle_request! {
    on Initialize(&mut self, params) {
        // Initialize client caps from request
        self.client_caps.lines_start_at_1 = params.linesStartAt1.unwrap_or(true);
        self.client_caps.columns_start_at_1 = params.columnsStartAt1.unwrap_or(true);

        self.client_caps.variable_type = params.supportsVariableType.unwrap_or(false);
        self.client_caps.variable_paging = params.supportsVariablePaging.unwrap_or(false);
        self.client_caps.run_in_terminal = params.supportsRunInTerminalRequest.unwrap_or(false);
        self.client_caps.memory_references = params.supportsMemoryReferences.unwrap_or(false);
        // ... clientID, clientName, adapterID, locale, pathFormat

        // Tell the client our caps
        None
    }
}
