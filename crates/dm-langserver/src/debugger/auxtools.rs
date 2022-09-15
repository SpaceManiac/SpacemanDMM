use super::auxtools_types::*;
use std::thread;
use std::{
    io::{Read, Write},
    net::Ipv4Addr,
    net::SocketAddr,
    net::TcpStream,
    sync::{Arc, RwLock},
    thread::JoinHandle,
};
use std::{net::TcpListener, sync::mpsc};

use super::SequenceNumber;

enum StreamState {
    // The client is waiting for a Stream to be sent from the thread
    Waiting(mpsc::Receiver<TcpStream>),

    Connected(TcpStream),

    // The server has finished being used
    Disconnected,
}

pub struct Auxtools {
    seq: Arc<SequenceNumber>,
    responses: mpsc::Receiver<Response>,
    _thread: JoinHandle<()>,
    stream: StreamState,
    last_error: Arc<RwLock<String>>,
}

pub struct AuxtoolsThread {
    seq: Arc<SequenceNumber>,
    responses: mpsc::Sender<Response>,
    last_error: Arc<RwLock<String>>,
}

pub struct AuxtoolsScopes {
    pub arguments: Option<VariablesRef>,
    pub locals: Option<VariablesRef>,
    pub globals: Option<VariablesRef>,
}

impl Auxtools {
    pub fn connect(seq: Arc<SequenceNumber>, port: Option<u16>) -> std::io::Result<Self> {
        let addr: SocketAddr = (Ipv4Addr::LOCALHOST, port.unwrap_or(DEFAULT_PORT)).into();
        let (responses_sender, responses_receiver) = mpsc::channel();
        let last_error = Arc::new(RwLock::new("".to_owned()));
        let stream = TcpStream::connect_timeout(&addr, std::time::Duration::from_secs(5))?;

        let thread = {
            let seq = seq.clone();
            let last_error = last_error.clone();
            let stream = stream.try_clone().unwrap();
            thread::spawn(move || {
                AuxtoolsThread {
                    seq,
                    responses: responses_sender,
                    last_error,
                }
                .run(stream);
            })
        };

        Ok(Auxtools {
            seq,
            responses: responses_receiver,
            _thread: thread,
            stream: StreamState::Connected(stream),
            last_error,
        })
    }

    pub fn listen(seq: Arc<SequenceNumber>) -> std::io::Result<(u16, Self)> {
        let listener = TcpListener::bind((Ipv4Addr::LOCALHOST, 0))?;
        let port = listener.local_addr()?.port();

        let (connection_sender, connection_receiver) = mpsc::channel();
        let (responses_sender, responses_receiver) = mpsc::channel();
        let last_error = Arc::new(RwLock::new("".to_owned()));

        let thread = {
            let seq = seq.clone();
            let last_error = last_error.clone();
            AuxtoolsThread {
                seq,
                responses: responses_sender,
                last_error,
            }
            .spawn_listener(listener, connection_sender)
        };

        Ok((
            port,
            Auxtools {
                seq,
                responses: responses_receiver,
                _thread: thread,
                stream: StreamState::Waiting(connection_receiver),
                last_error,
            },
        ))
    }

    fn read_response_or_disconnect(&mut self) -> Result<Response, Box<dyn std::error::Error>> {
        match self
            .responses
            .recv_timeout(std::time::Duration::from_secs(5))
        {
            Ok(response) => Ok(response),
            Err(_) => {
                self.disconnect();
                Err(Box::new(super::GenericError(
                    "timed out waiting for response",
                )))
            }
        }
    }

    fn send(&mut self, request: Request) -> Result<(), Box<dyn std::error::Error>> {
        if let StreamState::Waiting(recv) = &self.stream {
            if let Ok(stream) = recv.try_recv() {
                self.stream = StreamState::Connected(stream);
            }
        }

        match &mut self.stream {
            StreamState::Connected(stream) => {
                let data = bincode::serialize(&request)?;
                stream.write_all(&(data.len() as u32).to_le_bytes())?;
                stream.write_all(&data[..])?;
                stream.flush()?;
                Ok(())
            }

            _ => {
                // Success if not connected (kinda dumb)
                Ok(())
            }
        }
    }

    fn send_or_disconnect(&mut self, request: Request) -> Result<(), Box<dyn std::error::Error>> {
        if let Err(e) = self.send(request) {
            self.disconnect();
            return Err(e);
        }

        Ok(())
    }

    pub fn disconnect(&mut self) {
        debug_output!(in self.seq, "[auxtools] disconnecting");
        let _ = self.send(Request::Disconnect);

        if let StreamState::Connected(stream) = &self.stream {
            let _ = stream.shutdown(std::net::Shutdown::Both);
        }

        self.stream = StreamState::Disconnected;
    }

    pub fn configured(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        debug_output!(in self.seq, "[auxtools] configured");
        self.send_or_disconnect(Request::Configured)?;

        match self.read_response_or_disconnect()? {
            Response::Ack { .. } => Ok(()),
            response => Err(Box::new(UnexpectedResponse::new("Ack", response))),
        }
    }

    pub fn get_stddef(&mut self) -> Result<Option<String>, Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::StdDef)?;

        match self.read_response_or_disconnect()? {
            Response::StdDef(contents) => Ok(contents),
            response => Err(Box::new(UnexpectedResponse::new("StdDef", response))),
        }
    }

    pub fn eval(
        &mut self,
        frame_id: Option<u32>,
        command: &str,
        context: Option<String>,
    ) -> Result<EvalResponse, Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::Eval {
            frame_id,
            command: command.to_owned(),
            context,
        })?;

        match self.read_response_or_disconnect()? {
            Response::Eval(res) => Ok(res),
            response => Err(Box::new(UnexpectedResponse::new("Eval", response))),
        }
    }

    pub fn get_current_proc(
        &mut self,
        frame_id: u32,
    ) -> Result<Option<(String, u32)>, Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::CurrentInstruction { frame_id })?;

        match self.read_response_or_disconnect()? {
            Response::CurrentInstruction(ins) => Ok(ins.map(|x| (x.proc.path, x.proc.override_id))),
            response => Err(Box::new(UnexpectedResponse::new(
                "CurrentInstruction",
                response,
            ))),
        }
    }

    pub fn get_line_number(
        &mut self,
        path: &str,
        override_id: u32,
        offset: u32,
    ) -> Result<Option<u32>, Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::LineNumber {
            proc: ProcRef {
                path: path.to_owned(),
                override_id,
            },
            offset,
        })?;

        match self.read_response_or_disconnect()? {
            Response::LineNumber { line } => Ok(line),
            response => Err(Box::new(UnexpectedResponse::new("LineNumber", response))),
        }
    }

    pub fn get_offset(
        &mut self,
        path: &str,
        override_id: u32,
        line: u32,
    ) -> Result<Option<u32>, Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::Offset {
            proc: ProcRef {
                path: path.to_owned(),
                override_id,
            },
            line,
        })?;

        match self.read_response_or_disconnect()? {
            Response::Offset { offset } => Ok(offset),
            response => Err(Box::new(UnexpectedResponse::new("Offset", response))),
        }
    }

    pub fn set_breakpoint(
        &mut self,
        instruction: InstructionRef,
        condition: Option<String>,
    ) -> Result<BreakpointSetResult, Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::BreakpointSet {
            instruction,
            condition,
        })?;

        match self.read_response_or_disconnect()? {
            Response::BreakpointSet { result } => Ok(result),
            response => Err(Box::new(UnexpectedResponse::new("BreakpointSet", response))),
        }
    }

    pub fn unset_breakpoint(
        &mut self,
        instruction: &InstructionRef,
    ) -> Result<(), Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::BreakpointUnset {
            instruction: instruction.clone(),
        })?;

        match self.read_response_or_disconnect()? {
            Response::BreakpointUnset { .. } => Ok(()),
            response => Err(Box::new(UnexpectedResponse::new(
                "BreakpointUnset",
                response,
            ))),
        }
    }

    pub fn continue_execution(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::Continue {
            kind: ContinueKind::Continue,
        })?;

        match self.read_response_or_disconnect()? {
            Response::Ack { .. } => Ok(()),
            response => Err(Box::new(UnexpectedResponse::new("Ack", response))),
        }
    }

    pub fn next(&mut self, stack_id: u32) -> Result<(), Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::Continue {
            kind: ContinueKind::StepOver { stack_id },
        })?;

        match self.read_response_or_disconnect()? {
            Response::Ack { .. } => Ok(()),
            response => Err(Box::new(UnexpectedResponse::new("Ack", response))),
        }
    }

    pub fn step_into(&mut self, stack_id: u32) -> Result<(), Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::Continue {
            kind: ContinueKind::StepInto { stack_id },
        })?;

        match self.read_response_or_disconnect()? {
            Response::Ack { .. } => Ok(()),
            response => Err(Box::new(UnexpectedResponse::new("Ack", response))),
        }
    }

    pub fn step_out(&mut self, stack_id: u32) -> Result<(), Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::Continue {
            kind: ContinueKind::StepOut { stack_id },
        })?;

        match self.read_response_or_disconnect()? {
            Response::Ack { .. } => Ok(()),
            response => Err(Box::new(UnexpectedResponse::new("Ack", response))),
        }
    }

    pub fn pause(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::Pause)?;

        match self.read_response_or_disconnect()? {
            Response::Ack { .. } => Ok(()),
            response => Err(Box::new(UnexpectedResponse::new("Ack", response))),
        }
    }

    pub fn get_stacks(&mut self) -> Result<Vec<Stack>, Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::Stacks)?;

        match self.read_response_or_disconnect()? {
            Response::Stacks { stacks } => Ok(stacks),
            response => Err(Box::new(UnexpectedResponse::new("Stacks", response))),
        }
    }

    pub fn get_stack_frames(
        &mut self,
        stack_id: u32,
        start_frame: Option<u32>,
        count: Option<u32>,
    ) -> Result<(Vec<StackFrame>, u32), Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::StackFrames {
            stack_id,
            start_frame,
            count,
        })?;

        match self.read_response_or_disconnect()? {
            Response::StackFrames {
                frames,
                total_count,
            } => Ok((frames, total_count)),
            response => Err(Box::new(UnexpectedResponse::new("StackFrames", response))),
        }
    }

    // TODO: return all the scopes
    pub fn get_scopes(
        &mut self,
        frame_id: u32,
    ) -> Result<AuxtoolsScopes, Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::Scopes { frame_id })?;

        match self.read_response_or_disconnect()? {
            Response::Scopes {
                arguments,
                locals,
                globals,
            } => Ok(AuxtoolsScopes {
                arguments,
                locals,
                globals,
            }),
            response => Err(Box::new(UnexpectedResponse::new("Scopes", response))),
        }
    }

    pub fn get_variables(
        &mut self,
        vars: VariablesRef,
    ) -> Result<Vec<Variable>, Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::Variables { vars })?;

        match self.read_response_or_disconnect()? {
            Response::Variables { vars } => Ok(vars),
            response => Err(Box::new(UnexpectedResponse::new("Variables", response))),
        }
    }

    pub fn get_last_error_message(&self) -> String {
        self.last_error.read().unwrap().clone()
    }

    pub fn set_catch_runtimes(
        &mut self,
        should_catch: bool,
    ) -> Result<(), Box<dyn std::error::Error>> {
        self.send_or_disconnect(Request::CatchRuntimes { should_catch })
    }
}

impl AuxtoolsThread {
    fn spawn_listener(
        self,
        listener: TcpListener,
        connection_sender: mpsc::Sender<TcpStream>,
    ) -> JoinHandle<()> {
        thread::spawn(move || match listener.accept() {
            Ok((stream, _)) => {
                match connection_sender.send(stream.try_clone().unwrap()) {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("Debug client thread failed to pass cloned TcpStream: {}", e);
                        return;
                    }
                }

                self.run(stream);
            }

            Err(e) => {
                eprintln!("Debug client failed to accept connection: {}", e);
            }
        })
    }

    // returns true if we should disconnect
    fn handle_response(&mut self, data: &[u8]) -> Result<bool, Box<dyn std::error::Error>> {
        let response = bincode::deserialize::<Response>(data)?;

        match response {
            Response::Disconnect => return Ok(true),

            Response::Notification { message: _message } => {
                debug_output!(in self.seq, "[auxtools] {}", _message);
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

    pub fn run(mut self, mut stream: TcpStream) {
        let mut buf = vec![];

        // Look at this little trouble-maker right here
        self.seq.issue_event(dap_types::InitializedEvent);

        // The incoming stream is a u32 followed by a bincode-encoded Request.
        loop {
            let mut len_bytes = [0u8; 4];
            let len = match stream.read_exact(&mut len_bytes) {
                Ok(_) => u32::from_le_bytes(len_bytes),

                Err(e) => {
                    eprintln!("Debug server thread read error: {}", e);
                    break;
                }
            };

            buf.resize(len as usize, 0);
            match stream.read_exact(&mut buf) {
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
    }
}

#[derive(Debug)]
pub struct UnexpectedResponse(String);

impl UnexpectedResponse {
    fn new(expected: &'static str, received: Response) -> Self {
        Self(format!(
            "received unexpected response: expected {}, got {:?}",
            expected, received
        ))
    }
}

impl std::error::Error for UnexpectedResponse {
    fn description(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for UnexpectedResponse {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.write_str(&self.0)
    }
}
