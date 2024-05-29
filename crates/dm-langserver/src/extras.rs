//! Extensions to the language server protocol.

use lsp_types::notification::*;
use lsp_types::request::*;
use lsp_types::SymbolKind;

pub enum WindowStatus {}
impl Notification for WindowStatus {
    const METHOD: &'static str = "$window/status";
    type Params = WindowStatusParams;
}
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct WindowStatusParams {
    pub environment: Option<String>,
    pub tasks: Vec<String>,
}

pub enum ObjectTree {}
impl Notification for ObjectTree {
    const METHOD: &'static str = "experimental/dreammaker/objectTree";
    type Params = ObjectTreeParams;
}
#[derive(Debug, Serialize, Deserialize)]
pub struct ObjectTreeParams {
    pub root: ObjectTreeType,
}
#[derive(Debug, Serialize, Deserialize)]
pub struct ObjectTreeType {
    pub name: String,
    pub kind: SymbolKind,
    pub location: Option<lsp_types::Location>,
    pub vars: Vec<ObjectTreeVar>,
    pub procs: Vec<ObjectTreeProc>,
    pub children: Vec<ObjectTreeType>,
}
#[derive(Debug, Serialize, Deserialize)]
pub struct ObjectTreeVar {
    pub name: String,
    pub kind: SymbolKind,
    pub location: Option<lsp_types::Location>,
    pub is_declaration: bool,
}
#[derive(Debug, Serialize, Deserialize)]
pub struct ObjectTreeProc {
    pub name: String,
    pub kind: SymbolKind,
    pub location: Option<lsp_types::Location>,
    pub is_verb: Option<bool>,
}

pub enum Reparse {}
impl Notification for Reparse {
    const METHOD: &'static str = "experimental/dreammaker/reparse";
    type Params = lsp_types::InitializedParams;
}

pub enum StartDebugger {}
impl Request for StartDebugger {
    const METHOD: &'static str = "experimental/dreammaker/startDebugger";
    type Params = StartDebuggerParams;
    type Result = StartDebuggerResult;
}
#[derive(Debug, Serialize, Deserialize)]
pub struct StartDebuggerParams {
    pub dreamseeker_exe: String,
}
#[derive(Debug, Serialize, Deserialize)]
pub struct StartDebuggerResult {
    pub port: u16,
}
