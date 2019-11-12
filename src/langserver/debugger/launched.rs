//! Child process lifecycle management.
#![allow(unsafe_code)]

use std::sync::{Arc, Mutex};
use super::SequenceNumber;
use super::dap_types::{ExitedEvent, TerminatedEvent};

pub struct Launched {
    handle: raw::Handle,
    seq: Arc<SequenceNumber>,
    mutex: Arc<Mutex<bool>>,  // TODO: AtomicBool instead?
}

impl Launched {
    pub fn new(seq: Arc<SequenceNumber>, mut child: std::process::Child) -> std::io::Result<Launched> {
        let handle = raw::from(&child);
        let seq2 = seq.clone();
        let mutex = Arc::new(Mutex::new(false));
        let mutex2 = mutex.clone();

        std::thread::Builder::new()
            .name("launched debuggee manager thread".to_owned())
            .spawn(move || {
                eprintln!("[launched] child started");
                let code = match child.wait() {
                    Ok(status) => {
                        let code = status.code();
                        eprintln!("[launched] child exited with code {:?}", code);
                        code.unwrap_or(-1)
                    }
                    Err(e) => {
                        eprintln!("[launched] wait() errored: {:?}", e);
                        -1
                    }
                };
                *mutex2.lock().unwrap() = true;
                seq2.issue_event(TerminatedEvent::default());
                seq2.issue_event(ExitedEvent {
                    exitCode: code as i64,
                });
                eprintln!("launched - exited issued");
            })?;
        Ok(Launched {
            handle,
            seq,
            mutex,
        })
    }

    pub fn kill(self) -> std::io::Result<()> {
        eprintln!("launched - kill called");
        if *self.mutex.lock().unwrap() {
            // don't kill if the wait() has completed
            eprintln!("launched - kill short circuiting");
            return Ok(());
        }
        eprintln!("[launched] killing child process");
        match unsafe { raw::kill(self.handle) } {
            true => Ok(()),
            false => Err(std::io::Error::last_os_error()),
        }
    }

    pub fn detach(self) {
        eprintln!("[launched] detaching softly");
        self.seq.issue_event(TerminatedEvent::default());
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
