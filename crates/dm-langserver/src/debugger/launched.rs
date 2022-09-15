//! Child process lifecycle management.
#![allow(unsafe_code)]

use super::SequenceNumber;
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex};

// active --kill--> killed: emit Terminated, send SIGKILL
// active --detach--> detached: emit Terminated
// active --exit--> exited: emit Terminated + Exited
// killed --exit--> exited: emit Exited
// detached --exit--> exited: emit Exited
// exited --kill--> exited: no-op
// exited --detach--> exited: no-op

#[derive(Copy, Clone, Debug)]
enum State {
    Active,
    Killed,
    Detached,
    Exited,
}

pub struct Launched {
    handle: raw::Handle,
    seq: Arc<SequenceNumber>,
    mutex: Arc<Mutex<State>>,
}

pub enum EngineParams {
    Extools {
        port: u16,
        dll: Option<std::path::PathBuf>,
    },
    Auxtools {
        port: u16,
        dll: Option<std::path::PathBuf>,
    },
}

impl Launched {
    pub fn new(
        seq: Arc<SequenceNumber>,
        dreamseeker_exe: &str,
        dmb: &str,
        params: Option<EngineParams>,
    ) -> std::io::Result<Launched> {
        let mut command = Command::new(dreamseeker_exe);

        #[cfg(unix)]
        {
            if let Some(parent) = std::path::Path::new(dreamseeker_exe).parent() {
                command.env("LD_LIBRARY_PATH", parent);
            }
        }

        command
            .arg(dmb)
            .arg("-trusted")
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        match params {
            Some(EngineParams::Extools { port, dll }) => {
                command.env("EXTOOLS_MODE", "LAUNCHED");
                command.env("EXTOOLS_PORT", port.to_string());
                if let Some(dll) = dll {
                    command.env("EXTOOLS_DLL", dll);
                }
            }

            Some(EngineParams::Auxtools { port, dll }) => {
                command.env("AUXTOOLS_DEBUG_MODE", "LAUNCHED");
                command.env("AUXTOOLS_DEBUG_PORT", port.to_string());
                if let Some(dll) = dll {
                    command.env("AUXTOOLS_DEBUG_DLL", dll);
                }
            }

            None => (),
        }

        let mut child = command.spawn()?;
        output!(in seq, "[launched] Started: {:?}", command);
        let mutex = Arc::new(Mutex::new(State::Active));
        let handle = raw::from(&child);

        let seq2 = seq.clone();
        let mutex2 = mutex.clone();

        pipe_output(seq.clone(), "stdout", child.stdout.take())?;
        pipe_output(seq.clone(), "stderr", child.stderr.take())?;

        std::thread::Builder::new()
            .name("launched debuggee manager thread".to_owned())
            .spawn(move || {
                let wait = child.wait();
                // lock as soon as possible to minimize risk of shenanigans
                let mut state = mutex2.lock().expect("launched mutex poisoned");
                let code = match wait {
                    Ok(status) => {
                        let code = status.code();
                        match code {
                            Some(code) => output!(in seq2, "[launched] Child exited with code {:#x}.", code),
                            None => output!(in seq2, "[launched] Child exited due to signal."),
                        }
                        code.unwrap_or(-1)
                    }
                    Err(err) => {
                        output!(in seq2, "[launched] Lost track of child process, this may be a SpacemanDMM bug.\n - {}", err);
                        debug_output!(in seq2, " - state was {:?}\n - more error details: {:?}", *state, err);
                        -1
                    }
                };
                if let State::Active = *state {
                    seq2.issue_event(dap_types::TerminatedEvent::default());
                }
                *state = State::Exited;
                seq2.issue_event(dap_types::ExitedEvent {
                    exitCode: code as i64,
                });
            })?;

        Ok(Launched { handle, seq, mutex })
    }

    pub fn kill(self) -> std::io::Result<()> {
        let mut state = self.mutex.lock().expect("launched mutex poisoned");
        match *state {
            State::Active => {
                output!(in self.seq, "[launched] Killing child process...");
                *state = State::Killed;
                match unsafe { raw::kill(self.handle) } {
                    true => Ok(()),
                    false => Err(std::io::Error::last_os_error()),
                }
            }
            State::Exited => Ok(()),
            _other => {
                debug_output!(in self.seq, "[launched] kill no-op in state {:?}", _other);
                Ok(())
            }
        }
    }

    pub fn detach(self) {
        let mut state = self.mutex.lock().expect("launched mutex poisoned");
        match *state {
            State::Active => {
                output!(in self.seq, "[launched] Detaching from child process...");
                *state = State::Detached;
            }
            _other => {
                debug_output!(in self.seq, "[launched] detach no-op in state {:?}", _other);
            }
        }
    }
}

fn pipe_output<R: std::io::Read + Send + 'static>(
    seq: Arc<SequenceNumber>,
    keyword: &'static str,
    stream: Option<R>,
) -> std::io::Result<()> {
    guard!(let Some(stream2) = stream else { return Ok(()); });
    std::thread::Builder::new()
        .name(format!("launched debuggee {} relay", keyword))
        .spawn(move || {
            use std::io::BufRead;

            let mut line = vec![];
            let mut buf_stream = std::io::BufReader::new(stream2);
            loop {
                line.clear();
                match buf_stream.read_until(b'\n', &mut line) {
                    Ok(0) => break,
                    Ok(_) => {
                        seq.issue_event(dap_types::OutputEvent {
                            output: String::from_utf8_lossy(&line).into_owned(),
                            category: Some(keyword.to_owned()),
                            ..Default::default()
                        });
                    }
                    Err(e) => {
                        seq.issue_event(dap_types::OutputEvent {
                            output: format!("[launched {}] {}", keyword, e),
                            category: Some("console".to_owned()),
                            ..Default::default()
                        });
                        break;
                    }
                }
            }
        })?;
    Ok(())
}

#[cfg(unix)]
mod raw {
    pub type Handle = i32;

    pub fn from(child: &std::process::Child) -> Handle {
        child.id() as i32
    }

    pub unsafe fn kill(handle: Handle) -> bool {
        libc::kill(handle, libc::SIGKILL) != -1
    }
}

#[cfg(windows)]
mod raw {
    use std::os::windows::io::AsRawHandle;

    pub type Handle = std::os::windows::io::RawHandle;

    pub fn from(child: &std::process::Child) -> Handle {
        child.as_raw_handle()
    }

    pub unsafe fn kill(handle: Handle) -> bool {
        extern "system" {
            fn TerminateProcess(
                hProcess: std::os::windows::raw::HANDLE,
                uExitCode: std::os::raw::c_uint,
            ) -> std::os::raw::c_int;
        }
        TerminateProcess(handle, 1) != 0
    }
}
