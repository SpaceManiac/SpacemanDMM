//! Debug adapter protocol implementation for DreamSeeker.
//!
//! * https://microsoft.github.io/debug-adapter-protocol/

mod types;

use std::error::Error;
use io;
use self::types::*;

pub fn debugger_main<I: Iterator<Item=String>>(mut args: I) {
    let mut dreamseeker_exe = None;

    while let Some(arg) = args.next() {
        if arg == "--dreamseeker-exe" {
            dreamseeker_exe = Some(args.next().expect("must specify a value for --dreamseeker-exe"));
        } else {
            panic!("unknown argument {:?}", arg);
        }
    }

    let mut debugger = Debugger {
        dreamseeker_exe: dreamseeker_exe.expect("must provide argument `--dreamseeker-exe path/to/dreamseeker.exe`"),
    };
    io::run_forever(|message| debugger.handle_input(message));
}

struct Debugger {
    dreamseeker_exe: String,
}

impl Debugger {
    fn handle_input(&mut self, message: &str) {
        // TODO: error handling
        self.dispatch_input(message).expect("error in dispatch_input");
    }

    fn dispatch_input(&mut self, message: &str) -> Result<(), Box<dyn Error>> {
        let protocol_message = serde_json::from_str::<ProtocolMessage>(message)?;
        match protocol_message.type_.as_str() {
            "request" => {
                let request = serde_json::from_str::<Request>(message)?;
                eprintln!("{:?}", request);
            }
            other => return Err(format!("unknown `type` field {:?}", other).into())
        }
        Ok(())
    }
}
