use super::auxtools_types::*;
use std::{sync::Arc, io::{Error, Read, Write}, net::SocketAddr, net::TcpStream, thread::JoinHandle};
use std::sync::mpsc;
use std::thread;

use super::SequenceNumber;
use super::dap_types;

pub struct Auxtools {
    requests: mpsc::Sender<Request>,
    responses: mpsc::Receiver<Response>,
    _thread: JoinHandle<()>,
}

pub struct AuxtoolsThread {
    seq: Arc<SequenceNumber>,
    requests: mpsc::Receiver<Request>,
    responses: mpsc::Sender<Response>,
    stream: TcpStream,
}

impl Auxtools {
    pub fn new(addr: &SocketAddr, seq: Arc<SequenceNumber>) -> std::io::Result<Self> {
		let (requests_sender, requests_receiver) = mpsc::channel();
		let (responses_sender, responses_receiver) = mpsc::channel();

        let thread = AuxtoolsThread {
            seq,
            requests: requests_receiver,
            responses: responses_sender,
            stream: TcpStream::connect_timeout(&addr, std::time::Duration::from_secs(5))?
        };

        Ok(Auxtools {
            requests: requests_sender,
            responses: responses_receiver,
            _thread: thread.start_thread(),
        })
    }

    fn read_response(&mut self) -> Result<Response, mpsc::RecvTimeoutError> {
        self.responses.recv_timeout(std::time::Duration::from_secs(5))
    }

    pub fn get_line_number(&mut self, path: &str, override_id: u32, offset: u32) -> Option<u32> {
        self.requests.send(Request::LineNumber {
            proc: ProcRef {
                path: path.to_owned(),
                override_id,
            },
            offset,
        }).unwrap();

        match self.read_response() {
            Ok(response) => {
                match response {
                    Response::LineNumber { line } => {
                        line
                    }

                    // TODO: disconnect
                    _ => panic!("received wrong response")
                }
            }

            // TODO: disconnect
            _ => panic!("timed out")
        }
    }

    pub fn get_offset(&mut self, path: &str, override_id: u32, line: u32) -> Option<u32> {
        self.requests.send(Request::Offset {
            proc: ProcRef {
                path: path.to_owned(),
                override_id,
            },
            line,
        }).unwrap();

        match self.read_response() {
            Ok(response) => {
                match response {
                    Response::Offset { offset } => {
                        offset
                    }

                    // TODO: disconnect
                    _ => panic!("received wrong response")
                }
            }

            // TODO: disconnect
            _ => panic!("timed out")
        }
    }

    pub fn set_breakpoint(&mut self, path: &str, override_id: u32, offset: u32) {
        self.requests.send(Request::BreakpointSet {
            proc: ProcRef {
                path: path.to_owned(),
                override_id,
            },
            offset,
        }).unwrap();

        // We ignore the result
        // TODO: disconnect
        self.read_response().unwrap();
    }

    pub fn unset_breakpoint(&mut self, path: &str, override_id: u32, offset: u32) {
        self.requests.send(Request::BreakpointUnset {
            proc: ProcRef {
                path: path.to_owned(),
                override_id,
            },
            offset,
        }).unwrap();

        // We ignore the result
        // TODO: disconnect
        self.read_response().unwrap();
    }

    pub fn continue_execution(&mut self) {
        // TODO: disconnect
        self.requests.send(Request::Continue {
            kind: ContinueKind::Continue
        }).unwrap();
    }

    pub fn step_over(&mut self) {
        // TODO: disconnect
        self.requests.send(Request::Continue {
            kind: ContinueKind::StepOver
        }).unwrap();
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
                    .. Default::default()
                });
            },
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
                match self.stream.read(&mut buf) {
                    Ok(0) => (),
                    Ok(n) => {
                        queued_data.extend_from_slice(&buf[..n]);
                    }
                    Err(_) => panic!("Handle me!"),
                }
    
                for message in queued_data.split(|x| *x == 0) {
                    // split can give us empty slices
                    if message.is_empty() {
                        continue;
                    }
    
                    self.handle_message(message).unwrap();
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