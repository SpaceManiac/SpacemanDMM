//! Extensions to the language server protocol.

use langserver::notification::*;

pub enum WindowStatus {}
impl Notification for WindowStatus {
    const METHOD: &'static str = "$window/status";
    type Params = WindowStatusParams;
}
#[derive(Debug, Default, Serialize)]
pub struct WindowStatusParams {
    pub environment: Option<String>,
    pub tasks: Vec<String>,
}
