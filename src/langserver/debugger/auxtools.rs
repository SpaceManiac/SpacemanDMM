use super::auxtools_types::*;
use std::{sync::mpsc};
use std::thread;
use std::{
    io::{Read, Write},
    net::Ipv4Addr,
    net::SocketAddr,
    net::TcpStream,
    sync::{Arc, RwLock},
    thread::JoinHandle,
};

use super::dap_types;
use super::SequenceNumber;

// We need to be able to encode/decode VariablesRef into an i64 for DAP to use
// but valid values are between 1 and 2^32-1.
// We make a few assumptions here:
// 1) BYOND doesn't use 0x7F or 0x7E as a tag for any data-types
// 2) The data portion of any BYOND value only needs 24-bits to be stored
// 3) Frame IDs only need 24-bits to be stored
//
// If this doesn't work out we'll just keep a map on the server of identifiers -> refs
impl VariablesRef {
	pub fn encode(&self) -> i64 {
		match self {
            VariablesRef::Arguments { frame } => {
                (0x7F << 24) + *frame as i64
            }

            VariablesRef::Locals { frame } => {
                (0x7E << 24) + *frame as i64
            }

            VariablesRef::Internal { tag, data } => {
                let tag = *tag as i64;
                let data = *data as i64;
                (tag << 24) + data
            }
        }
    }
    
    pub fn decode(value: i64) -> Self {
        let tag = (((value as u32) & 0xFF000000) >> 24) as u8;
        let data = (value as u32) & 0x00FFFFFF;

        match tag {
            0x7F => {
                VariablesRef::Arguments {
                    frame: data,
                }
            }

            0x7E => {
                VariablesRef::Locals {
                    frame: data,
                }
            }

            tag => {
                VariablesRef::Internal {
                    tag,
                    data: data as u32,
                }
            }
        }
    }
}

pub struct Auxtools {
    requests: mpsc::Sender<Request>,
    responses: mpsc::Receiver<Response>,
    _thread: JoinHandle<()>,
    last_error: Arc<RwLock<String>>,
}

pub struct AuxtoolsThread {
    seq: Arc<SequenceNumber>,
    requests: mpsc::Receiver<Request>,
    responses: mpsc::Sender<Response>,
    stream: TcpStream,
    last_error: Arc<RwLock<String>>,
}

impl Auxtools {
    pub fn new(seq: Arc<SequenceNumber>, port: Option<u16>) -> std::io::Result<Self> {
        let addr: SocketAddr = (Ipv4Addr::LOCALHOST, port.unwrap_or(DEFAULT_PORT)).into();
        let (requests_sender, requests_receiver) = mpsc::channel();
        let (responses_sender, responses_receiver) = mpsc::channel();

        let stream = TcpStream::connect_timeout(&addr, std::time::Duration::from_secs(5))?;
        stream.set_nonblocking(true)?;

        // Look at this little trouble-maker right here
        seq.issue_event(dap_types::InitializedEvent);

        let last_error = Arc::new(RwLock::new("".to_owned()));

        let thread = AuxtoolsThread {
            seq,
            requests: requests_receiver,
            responses: responses_sender,
            stream,
            last_error: last_error.clone(),
        };

        Ok(Auxtools {
            requests: requests_sender,
            responses: responses_receiver,
            _thread: thread.start_thread(),
            last_error,
        })
    }

    fn read_response(&mut self) -> Result<Response, mpsc::RecvTimeoutError> {
        self.responses
            .recv_timeout(std::time::Duration::from_secs(5))
    }

    pub fn disconnect(&mut self) {
        self.requests.send(Request::Disconnect).unwrap();
    }

    pub fn get_line_number(&mut self, path: &str, override_id: u32, offset: u32) -> Option<u32> {
        self.requests
            .send(Request::LineNumber {
                proc: ProcRef {
                    path: path.to_owned(),
                    override_id,
                },
                offset,
            })
            .unwrap();

        match self.read_response() {
            Ok(response) => {
                match response {
                    Response::LineNumber { line } => line,

                    // TODO: disconnect
                    _ => panic!("received wrong response"),
                }
            }

            // TODO: disconnect
            _ => panic!("timed out"),
        }
    }

    pub fn get_offset(&mut self, path: &str, override_id: u32, line: u32) -> Option<u32> {
        self.requests
            .send(Request::Offset {
                proc: ProcRef {
                    path: path.to_owned(),
                    override_id,
                },
                line,
            })
            .unwrap();

        match self.read_response() {
            Ok(response) => {
                match response {
                    Response::Offset { offset } => offset,

                    // TODO: disconnect
                    _ => panic!("received wrong response"),
                }
            }

            // TODO: disconnect
            _ => panic!("timed out"),
        }
    }

    pub fn set_breakpoint(&mut self, instruction: &InstructionRef) -> BreakpointSetResult {
        self.requests
            .send(Request::BreakpointSet {
                instruction: instruction.clone(),
            })
            .unwrap();

        // TODO: disconnect on fail
        match self.read_response() {
            Ok(response) => {
                match response {
                    Response::BreakpointSet { result } => {
                        return result;
                    }

                    // TODO: disconnect
                    _ => panic!("received wrong response"),
                }
            }

            // TODO: disconnect
            _ => panic!("timed out"),
        }
    }

    pub fn unset_breakpoint(&mut self, instruction: &InstructionRef) {
        self.requests
            .send(Request::BreakpointUnset {
                instruction: instruction.clone(),
            })
            .unwrap();

        // TODO: disconnect
        match self.read_response() {
            Ok(response) => {
                match response {
                    Response::BreakpointUnset { success: _ } => {}

                    // TODO: disconnect
                    _ => panic!("received wrong response"),
                }
            }

            // TODO: disconnect
            _ => panic!("timed out"),
        }
    }

    pub fn continue_execution(&mut self) {
        // TODO: disconnect
        self.requests
            .send(Request::Continue {
                kind: ContinueKind::Continue,
            })
            .unwrap();
    }

    pub fn next(&mut self, stack_id: u32) {
        // TODO: disconnect
        self.requests
            .send(Request::Continue {
                kind: ContinueKind::StepOver { stack_id },
            })
            .unwrap();
    }

    pub fn step_into(&mut self, stack_id: u32) {
        // TODO: disconnect
        self.requests
            .send(Request::Continue {
                kind: ContinueKind::StepInto { stack_id },
            })
            .unwrap();
    }

    pub fn step_out(&mut self, stack_id: u32) {
        // TODO: disconnect
        self.requests
            .send(Request::Continue {
                kind: ContinueKind::StepOut { stack_id },
            })
            .unwrap();
    }

    pub fn pause(&mut self) {
        // TODO: disconnect
        self.requests.send(Request::Pause).unwrap();
    }

    pub fn get_stacks(&mut self) -> Vec<Stack> {
        self.requests.send(Request::Stacks).unwrap();

        match self.read_response() {
            Ok(response) => {
                match response {
                    Response::Stacks { stacks } => return stacks,

                    // TODO: disconnect
                    _ => panic!("received wrong response"),
                }
            }

            // TODO: disconnect
            _ => panic!("timed out"),
        }
    }

    pub fn get_stack_frames(
        &mut self,
        stack_id: u32,
        start_frame: Option<u32>,
        count: Option<u32>,
    ) -> (Vec<StackFrame>, u32) {
        self.requests.send(Request::StackFrames {
            stack_id,
            start_frame,
            count,
        }).unwrap();

        match self.read_response() {
            Ok(response) => {
                match response {
                    Response::StackFrames {
                        frames,
                        total_count,
                    } => return (frames, total_count),

                    // TODO: disconnect
                    _ => panic!("received wrong response"),
                }
            }

            // TODO: disconnect
            _ => panic!("timed out"),
        }
    }

    // TODO: return all the scopes
    pub fn get_scopes(&mut self, frame_id: u32) -> (Option<VariablesRef>, Option<VariablesRef>, Option<VariablesRef>) {
        self.requests.send(Request::Scopes {
            frame_id
        }).unwrap();

        match self.read_response() {
            Ok(response) => {
                match response {
                    Response::Scopes {
                        arguments,
                        locals,
                        globals,
                    } => {
                        (arguments, locals, globals)
                    }

                    // TODO: disconnect
                    _ => panic!("received wrong response"),
                }
            }

            // TODO: disconnect
            _ => panic!("timed out"),
        }
    }

    pub fn get_variables(&mut self, vars: VariablesRef) -> Vec<Variable> {
        self.requests.send(Request::Variables {
            vars
        }).unwrap();

        match self.read_response() {
            Ok(response) => {
                match response {
                    Response::Variables { vars } => return vars,

                    // TODO: disconnect
                    _ => panic!("received wrong response"),
                }
            }

            // TODO: disconnect
            _ => panic!("timed out"),
        }
    }

    pub fn get_last_error_message(&self) -> String {
        self.last_error.read().unwrap().clone()
    }

    pub fn set_catch_runtimes(&self, should_catch: bool) {
        self.requests.send(Request::SetCatchRuntimes(should_catch)).unwrap();
    }
}

impl AuxtoolsThread {
    fn send(&mut self, request: Request) {
        let mut message = serde_json::to_vec(&request).unwrap();
        message.push(0); // null-terminator
        self.stream.write_all(&message[..]).unwrap();
    }

    // returns true if we should disconnect
    fn handle_message(&mut self, data: &[u8]) -> Result<bool, Box<dyn std::error::Error>> {
        let response = serde_json::from_slice::<Response>(data)?;

        match response {
            Response::Disconnect => return Ok(true),

            Response::BreakpointHit { reason } => {
                let mut description = None;

                let reason = match reason {
                    BreakpointReason::Step => dap_types::StoppedEvent::REASON_STEP,
                    BreakpointReason::Pause => dap_types::StoppedEvent::REASON_PAUSE,
                    BreakpointReason::Breakpoint => dap_types::StoppedEvent::REASON_BREAKPOINT,
                    BreakpointReason::Runtime(error) => {
                        *(self.last_error.write().unwrap()) = error.clone();
                        description = Some(error);
                        dap_types::StoppedEvent::REASON_EXCEPTION
                    }
                };

                self.seq.issue_event(dap_types::StoppedEvent {
                    threadId: Some(0),
                    reason: reason.to_owned(),
                    description,
                    allThreadsStopped: Some(true),
                    ..Default::default()
                });
            }

            x => {
                self.responses.send(x)?;
            }
        }

        Ok(false)
    }

    // TODO: rewrite to not be a non-blocking socket
    pub fn start_thread(mut self) -> JoinHandle<()> {
        thread::spawn(move || {
            let mut buf = [0u8; 4096];
            let mut queued_data = vec![];

            'outer: loop {
                let mut got_data = false;
                match self.stream.read(&mut buf) {
                    Ok(0) => (),
                    Ok(n) => {
                        queued_data.extend_from_slice(&buf[..n]);
                        got_data = true;
                    }

                    // This is a crutch
                    Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {}
                    Err(e) => {
                        println!("{:?}", e);
                        panic!("Handle me!");
                    }
                }

                if got_data {
                    while let Some(pos) = queued_data.iter().position(|x| *x == 0) {
                        let mut message: Vec<u8> = queued_data.drain(0..=pos).collect();
                        message.pop(); // remove null-terminator
                        if self.handle_message(&message).unwrap() {
                            break 'outer;
                        }
                    }
                }

                // Clear any finished messages from the buffer
                if let Some(idx) = queued_data.iter().rposition(|x| *x == 0) {
                    queued_data.drain(..idx);
                }

                // Send any requests to the server
                while let Ok(request) = self.requests.try_recv() {
                    self.send(request);
                }
            }

            self.seq.issue_event(dap_types::TerminatedEvent::default());
        })
    }
}
