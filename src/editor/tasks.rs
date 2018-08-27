//! Helper for running background tasks.

use std::sync::mpsc::{channel, Receiver, TryRecvError};
use std::thread;

pub type Err = Box<::std::error::Error + Send + Sync>;

pub struct Task<R> {
    name: String,
    rx: Receiver<Result<R, Err>>,
}

impl<R: Send + 'static> Task<R> {
    pub fn spawn<S: Into<String>, F: FnOnce() -> Result<R, Err> + Send + 'static>(name: S, f: F) -> Self {
        let (tx, rx) = channel();
        thread::spawn(move || {
            // TODO: catch unwind
            let _ = tx.send(f());
        });
        Task {
            name: name.into(),
            rx,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn poll<F: FnMut(Result<R, Err>)>(&self, mut f: F) -> bool {
        match self.rx.try_recv() {
            Ok(v) => { f(v); true },
            Err(TryRecvError::Empty) => true,
            Err(TryRecvError::Disconnected) => false,
        }
    }
}
