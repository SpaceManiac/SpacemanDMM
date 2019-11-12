//! Child process lifecycle management.
#![allow(unsafe_code)]

use std::sync::{Arc, Mutex};
use super::SequenceNumber;
use super::dap_types::{ExitedEvent, TerminatedEvent};

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

// TODO: This code currently emits the Terminated event in order to cover for
// no actual debugging taking place. When debugging is implemented, that event
// should be moved to be

pub struct Launched {
    handle: raw::Handle,
    seq: Arc<SequenceNumber>,
    mutex: Arc<Mutex<State>>,
}

impl Launched {
    pub fn new(seq: Arc<SequenceNumber>, mut child: std::process::Child) -> std::io::Result<Launched> {
        let mutex = Arc::new(Mutex::new(State::Active));
        let handle = raw::from(&child);

        let seq2 = seq.clone();
        let mutex2 = mutex.clone();

        std::thread::Builder::new()
            .name("launched debuggee manager thread".to_owned())
            .spawn(move || {
                eprintln!("[launched] child started");
                let wait = child.wait();
                // lock as soon as possible to minimize risk of shenanigans
                let mut state = mutex2.lock().expect("launched mutex poisoned");
                let code = match wait {
                    Ok(status) => {
                        let code = status.code();
                        eprintln!("[launched] child exited in state {:?} with code {:?}", *state, code);
                        code.unwrap_or(-1)
                    }
                    Err(err) => {
                        eprintln!("[launched] wait() errored in state {:?}: {:?}", *state, err);
                        -1
                    }
                };
                if let State::Active = *state {
                    seq2.issue_event(TerminatedEvent::default());
                }
                *state = State::Exited;
                seq2.issue_event(ExitedEvent {
                    exitCode: code as i64,
                });
            })?;

        Ok(Launched {
            handle,
            seq,
            mutex,
        })
    }

    pub fn kill(self) -> std::io::Result<()> {
        let mut state = self.mutex.lock().expect("launched mutex poisoned");
        match *state {
            State::Active => {
                eprintln!("[launched] killing child process");
                self.seq.issue_event(TerminatedEvent::default());
                *state = State::Killed;
                match unsafe { raw::kill(self.handle) } {
                    true => Ok(()),
                    false => Err(std::io::Error::last_os_error()),
                }
            }
            other => {
                eprintln!("[launched] kill no-op in state {:?}", other);
                Ok(())
            }
        }
    }

    pub fn detach(self) {
        let mut state = self.mutex.lock().expect("launched mutex poisoned");
        match *state {
            State::Active => {
                eprintln!("[launched] detaching from child process");
                self.seq.issue_event(TerminatedEvent::default());
                *state = State::Detached;
            }
            other => {
                eprintln!("[launched] detach no-op in state {:?}", other);
            }
        }
    }
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
