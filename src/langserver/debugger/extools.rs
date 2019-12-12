//! Client for the Extools debugger protocol.

use std::time::Duration;
use std::sync::{mpsc, Arc, Mutex};
use std::net::{SocketAddr, Ipv4Addr, TcpStream, TcpListener};
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

/// Possibly-connected Extools frontend.
pub struct ExtoolsHolder(ExtoolsHolderInner);

enum ExtoolsHolderInner {
    /// Used to avoid a layer of Option.
    None,
    Listening {
        port: u16,
        conn_rx: mpsc::Receiver<Extools>,
    },
    Attaching {
        cancel_tx: mpsc::Sender<()>,
        conn_rx: mpsc::Receiver<Extools>,
    },
    Active(Extools),
}

impl Default for ExtoolsHolder {
    fn default() -> Self {
        ExtoolsHolder(ExtoolsHolderInner::None)
    }
}

impl ExtoolsHolder {
    pub fn listen(seq: Arc<SequenceNumber>) -> std::io::Result<(u16, ExtoolsHolder)> {
        let listener = TcpListener::bind((Ipv4Addr::LOCALHOST, 0))?;
        let port = listener.local_addr()?.port();
        debug_output!(in seq, "[extools] Listening on {}", listener.local_addr()?);

        let (conn_tx, conn_rx) = mpsc::channel();

        std::thread::Builder::new()
            .name(format!("extools listening on port {}", port))
            .spawn(move || {
                let stream = match listener.accept() {
                    Ok((stream, _)) => stream,
                    Err(e) => return output!(in seq, "[extools] Error listening: {}", e),
                };
                drop(listener);
                let (conn, mut thread) = Extools::from_stream(seq, stream);
                if conn_tx.send(conn).is_ok() {
                    thread.read_loop();
                }
            })?;

        Ok((port, ExtoolsHolder(ExtoolsHolderInner::Listening {
            port,
            conn_rx,
        })))
    }

    pub fn attach(seq: Arc<SequenceNumber>, port: u16) -> std::io::Result<ExtoolsHolder> {
        let addr: SocketAddr = (Ipv4Addr::LOCALHOST, port).into();
        output!(in seq, "[extools] Attaching to {}...", addr);

        let (conn_tx, conn_rx) = mpsc::channel();
        let (cancel_tx, cancel_rx) = mpsc::channel();

        std::thread::Builder::new()
            .name(format!("extools attaching on port {}", port))
            .spawn(move || {
                while let Err(mpsc::TryRecvError::Empty) = cancel_rx.try_recv() {
                    if let Ok(stream) = TcpStream::connect_timeout(&addr, std::time::Duration::from_secs(5)) {
                        let (conn, mut thread) = Extools::from_stream(seq, stream);
                        if conn_tx.send(conn).is_ok() {
                            thread.read_loop();
                        }
                        return;
                    }
                }
            })?;

        Ok(ExtoolsHolder(ExtoolsHolderInner::Attaching {
            cancel_tx,
            conn_rx,
        }))
    }

    pub fn as_ref(&mut self) -> Option<&mut Extools> {
        match &mut self.0 {
            ExtoolsHolderInner::Listening { conn_rx, .. } |
            ExtoolsHolderInner::Attaching { conn_rx, .. } => {
                if let Ok(conn) = conn_rx.try_recv() {
                    self.0 = ExtoolsHolderInner::Active(conn);
                }
            }
            _ => {}
        }
        match &mut self.0 {
            ExtoolsHolderInner::Active(conn) => Some(conn),
            _ => None
        }
    }

    pub fn disconnect(&mut self) {
        match std::mem::replace(&mut self.0, ExtoolsHolderInner::None) {
            ExtoolsHolderInner::Attaching { cancel_tx, .. } => { let _ = cancel_tx.send(()); },
            // TODO: ExtoolsHolderInner::Listening
            _ => {}
        }
    }
}

/// Actually connected Extools frontend.
pub struct Extools {
    seq: Arc<SequenceNumber>,
    sender: ExtoolsSender,
    threads: Arc<Mutex<HashMap<i64, ThreadInfo>>>,
    bytecode: HashMap<(String, usize), Vec<DisassembledInstruction>>,
    get_type_rx: mpsc::Receiver<GetTypeResponse>,
    bytecode_rx: mpsc::Receiver<DisassembledProc>,
    get_field_rx: mpsc::Receiver<FieldResponse>,
    runtime_rx: mpsc::Receiver<Runtime>,
    last_runtime: Option<Runtime>,
}

impl Extools {
    fn from_stream(seq: Arc<SequenceNumber>, stream: TcpStream) -> (Extools, ExtoolsThread) {
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
        let (runtime_tx, runtime_rx) = mpsc::channel();

        let extools = Extools {
            seq,
            sender,
            threads: Arc::new(Mutex::new(HashMap::new())),
            bytecode: HashMap::new(),
            bytecode_rx,
            get_type_rx,
            get_field_rx,
            runtime_rx,
            last_runtime: None,
        };
        let seq = extools.seq.clone();
        let threads = extools.threads.clone();
        let sender = extools.sender.clone();
        let thread = ExtoolsThread {
            seq,
            sender,
            threads,
            get_type_tx,
            bytecode_tx,
            get_field_tx,
            runtime_tx,
        };
        (extools, thread)
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

    pub fn set_break_on_runtime(&self, enable: bool) {
        self.sender.send(BreakOnRuntime(enable));
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

    pub fn step_over(&self) {
        self.sender.send(BreakpointStepOver);
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

    pub fn last_error_message(&mut self) -> Option<&str> {
        while let Ok(runtime) = self.runtime_rx.try_recv() {
            self.last_runtime = Some(runtime);
        }
        self.last_runtime.as_ref().map(|r| r.message.as_str())
    }

    pub fn configuration_done(&mut self) {
        debug_output!(in self.seq, "[extools] Configuration done");
        self.sender.send(ConfigurationDone);
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
    runtime_tx: mpsc::Sender<Runtime>,
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
                if let Err(e) = self.handle_response(&buffer[start..end]) {
                    output!(in self.seq, "[extools] Error handling message: {}", e);
                    debug_output!(in self.seq, " - {}", String::from_utf8_lossy(&buffer[start..end]));
                }

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

    fn handle_response(&mut self, buffer: &[u8]) -> Result<(), Box<dyn Error>> {
        let message = serde_json::from_slice::<ProtocolMessage>(buffer)?;
        if let Some(handler) = Self::handle_response_table(&message.type_) {
            handler(self, message.content.unwrap_or(serde_json::Value::Null))
        } else {
            debug_output!(in self.seq, "[extools] NYI: {}", String::from_utf8_lossy(buffer));
            Ok(())
        }
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

    on Runtime(&mut self, runtime) {
        debug_output!(in self.seq, "[extools] {}#{}@{} runtimed", runtime.proc, runtime.override_id, runtime.offset);
        self.seq.issue_event(dap_types::StoppedEvent {
            reason: "exception".to_owned(),
            text: Some(runtime.message.clone()),
            threadId: Some(0),
            .. Default::default()
        });
        self.queue(&self.runtime_tx, runtime);
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

    on BreakOnRuntime(&mut self, _) {
        // Either it worked or it didn't, nothing we can do about it now.
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
