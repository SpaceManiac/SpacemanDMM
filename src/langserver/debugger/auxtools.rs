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

pub struct Auxtools {
    responses: mpsc::Receiver<Response>,
    _thread: JoinHandle<()>,
    stream: TcpStream,
    last_error: Arc<RwLock<String>>,
}

pub struct AuxtoolsThread {
    seq: Arc<SequenceNumber>,
    responses: mpsc::Sender<Response>,
    stream: TcpStream,
    last_error: Arc<RwLock<String>>,
}

impl Auxtools {
    pub fn new(seq: Arc<SequenceNumber>, port: Option<u16>) -> std::io::Result<Self> {
        let addr: SocketAddr = (Ipv4Addr::LOCALHOST, port.unwrap_or(DEFAULT_PORT)).into();
        let (responses_sender, responses_receiver) = mpsc::channel();

        let stream = TcpStream::connect_timeout(&addr, std::time::Duration::from_secs(5))?;

        // Look at this little trouble-maker right here
        seq.issue_event(dap_types::InitializedEvent);

        let last_error = Arc::new(RwLock::new("".to_owned()));

        let thread = AuxtoolsThread {
            seq,
            responses: responses_sender,
            stream: stream.try_clone().unwrap(),
            last_error: last_error.clone(),
        };

        Ok(Auxtools {
            responses: responses_receiver,
            _thread: thread.start_thread(),
            stream,
            last_error,
        })
    }

    fn read_response(&mut self) -> Result<Response, mpsc::RecvTimeoutError> {
        self.responses
            .recv_timeout(std::time::Duration::from_secs(5))
    }

    fn send(&mut self, request: Request) -> Result<(), ()> {
        let data = bincode::serialize(&request).unwrap();
        self.stream.write_all(&(data.len() as u32).to_le_bytes()).unwrap();
        self.stream.write_all(&data[..]).unwrap();
        self.stream.flush().unwrap();
        Ok(())
    }

    pub fn disconnect(&mut self) {
        self.send(Request::Disconnect).unwrap();
    }

    pub fn get_line_number(&mut self, path: &str, override_id: u32, offset: u32) -> Option<u32> {
        self.send(Request::LineNumber {
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
        self.send(Request::Offset {
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
        self.send(Request::BreakpointSet {
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
        self.send(Request::BreakpointUnset {
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
        self.send(Request::Continue {
                kind: ContinueKind::Continue,
            })
            .unwrap();
    }

    pub fn next(&mut self, stack_id: u32) {
        // TODO: disconnect
        self.send(Request::Continue {
                kind: ContinueKind::StepOver { stack_id },
            })
            .unwrap();
    }

    pub fn step_into(&mut self, stack_id: u32) {
        // TODO: disconnect
        self.send(Request::Continue {
                kind: ContinueKind::StepInto { stack_id },
            })
            .unwrap();
    }

    pub fn step_out(&mut self, stack_id: u32) {
        // TODO: disconnect
        self.send(Request::Continue {
                kind: ContinueKind::StepOut { stack_id },
            })
            .unwrap();
    }

    pub fn pause(&mut self) {
        // TODO: disconnect
        self.send(Request::Pause).unwrap();
    }

    pub fn get_stacks(&mut self) -> Vec<Stack> {
        self.send(Request::Stacks).unwrap();

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
        self.send(Request::StackFrames {
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
        self.send(Request::Scopes {
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
        self.send(Request::Variables {
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

    pub fn set_catch_runtimes(&mut self, should_catch: bool) {
        self.send(Request::CatchRuntimes { should_catch }).unwrap();
    }
}

impl AuxtoolsThread {
    // returns true if we should disconnect
    fn handle_response(&mut self, data: &[u8]) -> Result<bool, Box<dyn std::error::Error>> {
        let response = bincode::deserialize::<Response>(data)?;

        match response {
            Response::Disconnect => return Ok(true),

            Response::Notification { message } => {
                debug_output!(in self.seq, "[auxtools] {}", message);
            }

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
            let mut buf = vec![];

            // The incoming stream is a u32 followed by a bincode-encoded Request.
            loop {
                let mut len_bytes = [0u8; 4];
                let len = match self.stream.read_exact(&mut len_bytes) {
                    Ok(_) => u32::from_le_bytes(len_bytes),
    
                    Err(e) => {
                        eprintln!("Debug server thread read error: {}", e);
                        break;
                    }
                };

                buf.resize(len as usize, 0);
                match self.stream.read_exact(&mut buf) {
                    Ok(_) => (),
    
                    Err(e) => {
                        eprintln!("Debug server thread read error: {}", e);
                        break;
                    }
                };

                match self.handle_response(&buf[..]) {
                    Ok(requested_disconnect) => {
                        if requested_disconnect {
                            eprintln!("Debug server disconnected");
                            break;
                        }
                    }
    
                    Err(e) => {
                        eprintln!("Debug server thread failed to handle request: {}", e);
                        break;
                    }
                }
            }

            self.seq.issue_event(dap_types::TerminatedEvent::default());
        })
    }
}
