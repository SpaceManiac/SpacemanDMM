//! Extensions to the language server protocol.

use langserver::SymbolKind;
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

pub enum ObjectTree {}
impl Notification for ObjectTree {
    const METHOD: &'static str = "experimental/dreammaker/objectTree";
    type Params = ObjectTreeParams;
}
#[derive(Debug, Default, Serialize)]
pub struct ObjectTreeParams {
    pub roots: Vec<ObjectTreeEntry>,
}
#[derive(Debug, Serialize)]
pub struct ObjectTreeEntry {
    pub name: String,
    pub kind: SymbolKind,
    pub location: Option<langserver::Location>,
    pub children: Vec<ObjectTreeEntry>,
}
