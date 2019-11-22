//! Client for the Extools debugger protocol.

use std::sync::Arc;
use std::net::{SocketAddr, Ipv4Addr, TcpStream};
use std::io::{Read, Write};
use std::error::Error;

use super::SequenceNumber;
use super::extools_types::*;

pub struct Extools {
    seq: Arc<SequenceNumber>,
}

impl Extools {
    pub fn connect(seq: Arc<SequenceNumber>) -> std::io::Result<Extools> {
        let seq2 = seq.clone();

        std::thread::Builder::new()
            .name("extools read thread".to_owned())
            .spawn(move || ExtoolsThread { seq }.main())?;

        Ok(Extools {
            seq: seq2,
        })
    }
}

struct ExtoolsThread {
    seq: Arc<SequenceNumber>,
}

impl ExtoolsThread {
    fn main(&mut self) {
        let seq = self.seq.clone();
        let addr: SocketAddr = (Ipv4Addr::LOCALHOST, 2448).into();

        debug_output!(in seq, "[extools] Connecting...");
        let mut stream;
        let mut attempts = 0;
        loop {
            let err = match TcpStream::connect_timeout(&addr, std::time::Duration::from_secs(2)) {
                Ok(s) => {
                    stream = s;
                    break;
                }
                Err(err) => err,
            };
            std::thread::sleep(std::time::Duration::from_secs(2));
            attempts += 1;
            if attempts > 3 {
                output!(in seq, "[extools] Connection failed: {}", err);
                debug_output!(in seq, " - {:?}", err);
                return;
            }
        }
        output!(in seq, "[extools] Connected.");

        let mut sender = ExtoolsSender {
            seq: seq.clone(),
            stream: stream.try_clone().expect("try clone bad"),
        };
        sender.send(Raw("This is a test.".to_owned()));
        sender.send(ProcListRequest);
        sender.send(ProcDisassemblyRequest("/proc/debugme".to_owned()));
        sender.send(BreakpointSet {
            proc: "/proc/debugme".to_owned(),
            offset: 24,
        });

        let mut buffer = Vec::new();
        let mut read_buf = [0u8; 4096];
        loop {
            // read into the buffer
            let mut terminator = None;
            match stream.read(&mut read_buf[..]) {
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
                let message = &buffer[start..end];
                debug_output!(in seq, "[extools] << {}", String::from_utf8_lossy(message));
                self.handle_recv(message);

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

    fn handle_recv(&mut self, message: &[u8]) {
        // TODO: error handling
        serde_json::from_slice::<ProtocolMessage>(message)
            .map_err(|e| Box::new(e) as Box<dyn Error>)
            .and_then(|m| self.handle_response(m))
            .expect("error in extools::handle_recv");
    }
}

handle_extools! {
    on Raw(&mut self, Raw(message)) {
        debug_output!(in self.seq, "[extools] raw: {}", message);
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
