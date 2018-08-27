//! Helper for running background tasks.

use std::sync::mpsc::{channel, Receiver};
use std::thread;

pub struct Task<R> {
    name: String,
    rx: Receiver<R>,
}

impl<R: Send + 'static> Task<R> {
    pub fn spawn<S: Into<String>, F: FnOnce() -> R + Send + 'static>(name: S, f: F) -> Self {
        let (tx, rx) = channel();
        thread::spawn(move || {
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

    pub fn poll(&self) -> Option<R> {
        self.rx.try_recv().ok()
    }
}
