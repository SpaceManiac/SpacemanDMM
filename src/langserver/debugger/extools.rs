//! Client for the Extools debugger protocol.

use std::sync::{Arc, Mutex};
use std::net::{SocketAddr, Ipv4Addr, TcpStream};
use std::collections::HashMap;
use std::io::{Read, Write};
use std::error::Error;

use super::SequenceNumber;
use super::dap_types;
use super::extools_types::*;

// ----------------------------------------------------------------------------
// Data structures

#[derive(Clone, Default, Debug)]
pub struct ThreadInfo {
    pub call_stack: Vec<String>,
    pub args: Vec<ValueText>,
    pub locals: Vec<ValueText>,
}

// ----------------------------------------------------------------------------
// TCP connection management

pub struct Extools {
    seq: Arc<SequenceNumber>,
    threads: Arc<Mutex<HashMap<i64, ThreadInfo>>>,
}

impl Extools {
    pub fn connect(seq: Arc<SequenceNumber>) -> std::io::Result<Extools> {
        let extools = Extools {
            seq,
            threads: Arc::new(Mutex::new(HashMap::new())),
        };
        let seq = extools.seq.clone();
        let threads = extools.threads.clone();

        std::thread::Builder::new()
            .name("extools read thread".to_owned())
            .spawn(move || {
                let addr: SocketAddr = (Ipv4Addr::LOCALHOST, 2448).into();

                debug_output!(in seq, "[extools] Connecting...");
                let stream;
                let mut attempts = 0;
                loop {
                    let _err = match TcpStream::connect_timeout(&addr, std::time::Duration::from_secs(2)) {
                        Ok(s) => {
                            stream = s;
                            break;
                        }
                        Err(err) => err,
                    };
                    std::thread::sleep(std::time::Duration::from_secs(1));
                    attempts += 1;
                    if attempts >= 5 {
                        output!(in seq, "[extools] Connection failed after {} retries, debugging not available.", attempts);
                        debug_output!(in seq, " - {:?}", _err);
                        return;
                    }
                }
                output!(in seq, "[extools] Connected.");

                let sender = ExtoolsSender {
                    seq: seq.clone(),
                    stream: stream.try_clone().expect("try clone bad"),
                };

                ExtoolsThread {
                    seq,
                    sender,
                    threads,
                }.read_loop();
            })?;

        Ok(extools)
    }

    pub fn get_thread(&self, thread_id: i64) -> Option<ThreadInfo> {
        self.threads.lock().unwrap().get(&thread_id).cloned()
    }
}

struct ExtoolsThread {
    seq: Arc<SequenceNumber>,
    sender: ExtoolsSender,
    threads: Arc<Mutex<HashMap<i64, ThreadInfo>>>,
}

impl ExtoolsThread {
    fn read_loop(&mut self) {
        let mut buffer = Vec::new();
        let mut read_buf = [0u8; 4096];
        loop {
            // read into the buffer
            let mut terminator = None;
            match self.sender.stream.read(&mut read_buf[..]) {
                Ok(0) => panic!("extools eof"),
                Ok(n) => {
                    let slice = &read_buf[..n];
                    if let Some(pos) = slice.iter().position(|&x| x == 0) {
                        terminator = Some(buffer.len() + pos);
                    }
                    buffer.extend_from_slice(slice);
                }
                Err(e) => panic!("extools read error: {:?}", e),
            }

            // chop off as many full messages from the buffer as we can
            let mut start = 0;
            while let Some(end) = terminator.take() {
                self.handle_response(&buffer[start..end]).expect("error in extools::handle_response");

                start = end + 1;
                if let Some(pos) = buffer[start..].iter().position(|&x| x == 0) {
                    terminator = Some(start + pos);
                }
            }
            if start > 0 {
                buffer.drain(..start);
            }
        }
    }
}

handle_extools! {
    on Raw(&mut self, Raw(_message)) {
        debug_output!(in self.seq, "[extools] Raw: {}", _message);
    }

    on BreakpointHit(&mut self, hit) {
        debug_output!(in self.seq, "[extools] Hit breakpoint in {}", hit.proc);
        self.seq.issue_event(dap_types::StoppedEvent {
            reason: "breakpoint".to_owned(),
            threadId: Some(0),
            .. Default::default()
        });
        //self.sender.send(BreakpointResume);
    }

    on CallStack(&mut self, stack) {
        let mut map = self.threads.lock().unwrap();
        map.entry(0).or_default().call_stack = stack.0;
    }

    on Locals(&mut self, Locals(values)) {
        let mut map = self.threads.lock().unwrap();
        map.entry(0).or_default().locals = values;
    }

    on Args(&mut self, Args(values)) {
        let mut map = self.threads.lock().unwrap();
        map.entry(0).or_default().args = values;
    }
}

struct ExtoolsSender {
    seq: Arc<SequenceNumber>,
    stream: TcpStream,
}

impl ExtoolsSender {
    pub fn send<M: Request>(&mut self, message: M) {
        let content = serde_json::to_value(message).expect("extools body encode error");
        let mut buffer = serde_json::to_vec(&ProtocolMessage {
            type_: M::TYPE.to_owned(),
            content: Some(content),
        }).expect("extools encode error");
        debug_output!(in self.seq, "[extools] >> {}", String::from_utf8_lossy(&buffer[..]));
        buffer.push(0);
        // TODO: needs more synchronization
        self.stream.write_all(&buffer[..]).expect("extools write error");
    }
}
