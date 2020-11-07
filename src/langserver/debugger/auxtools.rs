use super::auxtools_types::*;
use std::{collections::HashSet, sync::mpsc};
use std::thread;
use std::{
    collections::HashMap,
    io::{Read, Write},
    net::Ipv4Addr,
    net::SocketAddr,
    net::TcpStream,
    sync::Arc,
    thread::JoinHandle,
};

use super::dap_types;
use super::SequenceNumber;
use dm::FileId;

pub struct Auxtools {
    requests: mpsc::Sender<Request>,
    responses: mpsc::Receiver<Response>,
    breakpoints: HashMap<FileId, HashMap<InstructionRef, BreakpointSetResult>>,
    _thread: JoinHandle<()>,
}

pub struct AuxtoolsThread {
    seq: Arc<SequenceNumber>,
    requests: mpsc::Receiver<Request>,
    responses: mpsc::Sender<Response>,
    stream: TcpStream,
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

        let thread = AuxtoolsThread {
            seq,
            requests: requests_receiver,
            responses: responses_sender,
            stream,
        };

        Ok(Auxtools {
            requests: requests_sender,
            responses: responses_receiver,
            breakpoints: HashMap::new(),
            _thread: thread.start_thread(),
        })
    }

    fn read_response(&mut self) -> Result<Response, mpsc::RecvTimeoutError> {
        self.responses
            .recv_timeout(std::time::Duration::from_secs(5))
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

    pub fn next(&mut self) {
        // TODO: disconnect
        self.requests
            .send(Request::Continue {
                kind: ContinueKind::StepOver,
            })
            .unwrap();
    }

    pub fn step_into(&mut self) {
        // TODO: disconnect
        self.requests
            .send(Request::Continue {
                kind: ContinueKind::StepInto,
            })
            .unwrap();
    }

    pub fn step_out(&mut self) {
        // TODO: disconnect
        self.requests
            .send(Request::Continue {
                kind: ContinueKind::StepOut,
            })
            .unwrap();
    }

    pub fn pause(&mut self) {
        // TODO: disconnect
        self.requests.send(Request::Pause).unwrap();
    }

    pub fn get_stack_frames(
        &mut self,
        thread_id: u32,
        start_frame: Option<u32>,
        count: Option<u32>,
    ) -> (Vec<StackFrame>, u32) {
        self.requests.send(Request::StackFrames {
            thread_id,
            start_frame,
            count,
        });

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

        (vec![], 0)
    }
}

impl AuxtoolsThread {
    fn send(&mut self, request: Request) {
        let mut message = serde_json::to_vec(&request).unwrap();
        message.push(0); // null-terminator
        self.stream.write_all(&message[..]).unwrap();
    }

    fn handle_message(&mut self, data: &[u8]) -> Result<(), Box<dyn std::error::Error>> {
        let response = serde_json::from_slice::<Response>(data)?;

        match response {
            Response::BreakpointHit { reason } => {
                let reason = match reason {
                    BreakpointReason::Step => dap_types::StoppedEvent::REASON_STEP,
                    BreakpointReason::Pause => dap_types::StoppedEvent::REASON_PAUSE,
                    BreakpointReason::Breakpoint => dap_types::StoppedEvent::REASON_BREAKPOINT,
                };

                self.seq.issue_event(dap_types::StoppedEvent {
                    threadId: Some(0),
                    reason: reason.to_owned(),
                    ..Default::default()
                });
            }
            x => {
                self.responses.send(x)?;
            }
        }

        Ok(())
    }

    pub fn start_thread(mut self) -> JoinHandle<()> {
        thread::spawn(move || {
            let mut buf = [0u8; 4096];
            let mut queued_data = vec![];

            loop {
                let mut got_data = false;
                match self.stream.read(&mut buf) {
                    Ok(0) => (),
                    Ok(n) => {
                        queued_data.extend_from_slice(&buf[..n]);
                        got_data = true;
                    }

                    // This is a crutch
                    Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {}
                    Err(_) => panic!("Handle me!"),
                }

                if got_data {
                    for message in queued_data.split(|x| *x == 0) {
                        // split can give us empty slices
                        if message.is_empty() {
                            continue;
                        }

                        self.handle_message(message).unwrap();
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
        })
    }
}
