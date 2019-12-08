//! Client for the Extools debugger protocol.

use std::time::Duration;
use std::sync::{mpsc, Arc, Mutex};
use std::net::{SocketAddr, Ipv4Addr, TcpStream};
use std::collections::HashMap;
use std::io::{Read, Write};
use std::error::Error;

use super::SequenceNumber;
use super::dap_types;
use super::extools_types::*;

pub const DEFAULT_PORT: u16 = 2448;

const RECV_TIMEOUT: Duration = Duration::from_secs(1);

// ----------------------------------------------------------------------------
// Data structures

#[derive(Clone, Default, Debug)]
pub struct ThreadInfo {
    pub call_stack: Vec<StackFrame>,
}

// ----------------------------------------------------------------------------
// TCP connection management

pub struct Extools {
    seq: Arc<SequenceNumber>,
    sender: ExtoolsSender,
    threads: Arc<Mutex<HashMap<i64, ThreadInfo>>>,
    bytecode: HashMap<(String, usize), Vec<DisassembledInstruction>>,
    get_type_rx: mpsc::Receiver<GetTypeResponse>,
    bytecode_rx: mpsc::Receiver<DisassembledProc>,
    get_field_rx: mpsc::Receiver<FieldResponse>,
}

impl Extools {
    pub fn connect(seq: Arc<SequenceNumber>, port: u16) -> std::io::Result<Option<Extools>> {
        let addr: SocketAddr = (Ipv4Addr::LOCALHOST, port).into();

        debug_output!(in seq, "[extools] Connecting...");
        let stream;
        //let mut attempts = 0;
        loop {
            let _err = match TcpStream::connect_timeout(&addr, std::time::Duration::from_secs(2)) {
                Ok(s) => {
                    stream = s;
                    break;
                }
                Err(err) => err,
            };
            std::thread::sleep(std::time::Duration::from_secs(1));
            // TODO: a more reliable means of detecting if extools is available.
            /*
            attempts += 1;
            if attempts >= 5 {
                output!(in seq, "[extools] Connection failed after {} retries, debugging not available.", attempts);
                debug_output!(in seq, " - {:?}", _err);
                return Ok(None);
            }
            */
        }
        output!(in seq, "[extools] Connected.");
        seq.issue_event(dap_types::InitializedEvent);

        let sender = ExtoolsSender {
            seq: seq.clone(),
            stream: stream.try_clone().expect("try clone bad"),
        };
        //sender.send(ProcListRequest);

        let (get_type_tx, get_type_rx) = mpsc::channel();
        let (bytecode_tx, bytecode_rx) = mpsc::channel();
        let (get_field_tx, get_field_rx) = mpsc::channel();

        let extools = Extools {
            seq,
            sender,
            threads: Arc::new(Mutex::new(HashMap::new())),
            bytecode: HashMap::new(),
            bytecode_rx,
            get_type_rx,
            get_field_rx,
        };
        let seq = extools.seq.clone();
        let threads = extools.threads.clone();
        let sender = extools.sender.clone();

        std::thread::Builder::new()
            .name("extools read thread".to_owned())
            .spawn(move || {
                ExtoolsThread {
                    seq,
                    sender,
                    threads,
                    get_type_tx,
                    bytecode_tx,
                    get_field_tx,
                }.read_loop();
            })?;

        Ok(Some(extools))
    }

    pub fn get_thread(&self, thread_id: i64) -> Option<ThreadInfo> {
        self.threads.lock().unwrap().get(&thread_id).cloned()
    }

    fn bytecode(&mut self, proc_ref: &str, override_id: usize) -> &[DisassembledInstruction] {
        let Extools { bytecode, sender, seq: _seq, bytecode_rx, .. } = self;
        bytecode.entry((proc_ref.to_owned(), override_id)).or_insert_with(|| {
            debug_output!(in _seq, "[extools] Fetching bytecode for {}#{}", proc_ref, override_id);
            sender.send(ProcDisassemblyRequest {
                name: proc_ref.to_owned(),
                override_id,
            });
            bytecode_rx.recv().expect("error receiving bytecode").instructions
        })
    }

    pub fn offset_to_line(&mut self, proc_ref: &str, override_id: usize, offset: i64) -> Option<i64> {
        let bc = self.bytecode(proc_ref, override_id);
        let mut comment = "";
        for instr in bc.iter() {
            if instr.mnemonic == "DBG LINENO" {
                comment = &instr.comment;
            }
            if instr.offset >= offset {
                return parse_lineno(comment);
            }
        }
        None
    }

    pub fn line_to_offset(&mut self, proc_ref: &str, override_id: usize, line: i64) -> Option<i64> {
        let bc = self.bytecode(proc_ref, override_id);
        for instr in bc.iter() {
            if instr.mnemonic == "DBG LINENO" {
                if let Some(parsed) = parse_lineno(&instr.comment) {
                    if parsed == line {
                        return Some(instr.offset);
                    }
                }
            }
        }
        None
    }

    pub fn set_breakpoint(&self, proc: &str, override_id: usize, offset: i64) {
        debug_output!(in self.seq, "[extools] {}#{}@{} set", proc, override_id, offset);
        self.sender.send(BreakpointSet { proc: proc.to_owned(), override_id, offset });
    }

    pub fn unset_breakpoint(&self, proc: &str, override_id: usize, offset: i64) {
        debug_output!(in self.seq, "[extools] {}#{}@{} unset", proc, override_id, offset);
        self.sender.send(BreakpointUnset { proc: proc.to_owned(), override_id, offset });
    }

    pub fn continue_execution(&self) {
        self.sender.send(BreakpointResume);
    }

    pub fn step_in(&self) {
        self.sender.send(BreakpointStepInto);
    }

    pub fn get_reference_type(&self, reference: i64) -> Result<String, Box<dyn Error>> {
        // TODO: error handling
        self.sender.send(GetType {
            datum_type: category_name(reference >> 24)?.to_owned(),
            datum_id: reference & 0xffffff,
        });
        Ok(self.get_type_rx.recv_timeout(RECV_TIMEOUT)?.0)
    }

    pub fn get_reference_field(&self, reference: i64, var: &str) -> Result<ValueText, Box<dyn Error>> {
        self.sender.send(FieldRequest {
            datum_type: category_name(reference >> 24)?.to_owned(),
            datum_id: reference & 0xffffff,
            field_name: var.to_owned(),
        });
        Ok(self.get_field_rx.recv_timeout(RECV_TIMEOUT)?.0)
    }
}

impl Drop for Extools {
    fn drop(&mut self) {
        if let Err(e) = self.sender.stream.shutdown(std::net::Shutdown::Both) {
            output!(in self.seq, "[extools] Shutdown failed: {}", e);
        } else {
            debug_output!(in self.seq, "[extools] Shutdown succeeded");
        }
    }
}

fn parse_lineno(comment: &str) -> Option<i64> {
    let prefix = "Line number: ";
    if comment.starts_with(prefix) {
        comment[prefix.len()..].parse::<i64>().ok()
    } else {
        None
    }
}

struct ExtoolsThread {
    seq: Arc<SequenceNumber>,
    sender: ExtoolsSender,
    threads: Arc<Mutex<HashMap<i64, ThreadInfo>>>,
    get_type_tx: mpsc::Sender<GetTypeResponse>,
    bytecode_tx: mpsc::Sender<DisassembledProc>,
    get_field_tx: mpsc::Sender<FieldResponse>,
}

impl ExtoolsThread {
    fn read_loop(&mut self) {
        let mut buffer = Vec::new();
        let mut read_buf = [0u8; 4096];
        loop {
            // read into the buffer
            let mut terminator = None;
            match self.sender.stream.read(&mut read_buf[..]) {
                Ok(0) => break,
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

        self.seq.issue_event(dap_types::TerminatedEvent::default());
    }

    fn queue<T>(&self, tx: &mpsc::Sender<T>, val: T) {
        // If the other side isn't listening, log that.
        if let Err(_e) = tx.send(val) {
            debug_output!(in self.seq, "[extools] Dropping {:?}", _e);
        }
    }
}

handle_extools! {
    on Raw(&mut self, Raw(_message)) {
        debug_output!(in self.seq, "[extools] Raw: {}", _message);
    }

    on BreakpointSet(&mut self, _bp) {
        debug_output!(in self.seq, "[extools] {}#{}@{} validated", _bp.proc, _bp.override_id, _bp.offset);
    }

    on BreakpointHit(&mut self, _hit) {
        debug_output!(in self.seq, "[extools] {}#{}@{} hit", _hit.proc, _hit.override_id, _hit.offset);
        self.seq.issue_event(dap_types::StoppedEvent {
            reason: "breakpoint".to_owned(),
            threadId: Some(0),
            .. Default::default()
        });
    }

    on CallStack(&mut self, stack) {
        let mut map = self.threads.lock().unwrap();
        map.entry(0).or_default().call_stack = stack.0;
    }

    on DisassembledProc(&mut self, disasm) {
        self.queue(&self.bytecode_tx, disasm);
    }

    on GetTypeResponse(&mut self, response) {
        self.queue(&self.get_type_tx, response);
    }

    on FieldResponse(&mut self, response) {
        self.queue(&self.get_field_tx, response);
    }
}

struct ExtoolsSender {
    seq: Arc<SequenceNumber>,
    stream: TcpStream,
}

impl Clone for ExtoolsSender {
    fn clone(&self) -> ExtoolsSender {
        ExtoolsSender {
            seq: self.seq.clone(),
            stream: self.stream.try_clone().expect("TcpStream::try_clone failed in ExtoolsSender::clone")
        }
    }
}

impl ExtoolsSender {
    pub fn send<M: Request>(&self, message: M) {
        let content = serde_json::to_value(message).expect("extools body encode error");
        let mut buffer = serde_json::to_vec(&ProtocolMessage {
            type_: M::TYPE.to_owned(),
            content: Some(content),
        }).expect("extools encode error");
        buffer.push(0);
        // TODO: needs more synchronization
        (&self.stream).write_all(&buffer[..]).expect("extools write error");
    }
}
