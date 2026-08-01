//! Helper for running background tasks.

use std::sync::mpsc::{channel, Receiver, TryRecvError};
use std::thread;

pub struct Background<T> {
    value: Option<T>,
    rx: Option<Receiver<T>>,
}

impl<T> Default for Background<T> {
    fn default() -> Self {
        Background {
            value: None,
            rx: None,
        }
    }
}

#[allow(dead_code)]
impl<T: Send + 'static> Background<T> {
    pub fn new<F: FnOnce() -> T + Send + 'static>(f: F) -> Self {
        let mut this = Self::default();
        this.spawn(f);
        this
    }

    pub fn spawn<F: FnOnce() -> T + Send + 'static>(&mut self, f: F) {
        self.rx = Some(spawn(f));
    }

    pub fn poll(&mut self) -> &mut Self {
        if let Some(rx) = self.rx.take() {
            match rx.try_recv() {
                Ok(v) => {
                    self.value = Some(v);
                },
                Err(TryRecvError::Empty) => self.rx = Some(rx),
                Err(TryRecvError::Disconnected) => {},
            }
        }
        self
    }

    pub fn is_busy(&self) -> bool {
        self.rx.is_some()
    }

    pub fn value(&self) -> Option<&T> {
        self.value.as_ref()
    }
}

fn spawn<R: Send + 'static, F: FnOnce() -> R + Send + 'static>(f: F) -> Receiver<R> {
    let (tx, rx) = channel();
    thread::spawn(move || {
        let _ = tx.send(f());
    });
    rx
}
