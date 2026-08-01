//! Extensions to the language server protocol.

use foldhash::HashMap;

use lsp_types::notification::*;
use lsp_types::request::*;
use lsp_types::SetTraceParams;
use lsp_types::SymbolKind;

// ----------------------------------------------------------------------------
// SetTrace variant that VSC sends
pub enum SetTraceVsc {}
impl Notification for SetTraceVsc {
    const METHOD: &'static str = "$/setTraceNotification";
    type Params = SetTraceParams;
}

// ----------------------------------------------------------------------------
// WindowStatus
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

// ----------------------------------------------------------------------------
// ObjectTree
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
    pub n_vars: usize,
    pub n_procs: usize,
    pub n_children: usize,
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

// ----------------------------------------------------------------------------
// ObjectTree2
pub enum ObjectTree2 {}
impl Notification for ObjectTree2 {
    const METHOD: &'static str = "experimental/dreammaker/objectTree2";
    type Params = ObjectTree2Params;
}
#[derive(Debug, Serialize, Deserialize)]
pub struct ObjectTree2Params {}

pub enum QueryObjectTree {}
impl Request for QueryObjectTree {
    const METHOD: &'static str = "experimental/dreammaker/objectTree2";
    type Params = QueryObjectTreeParams;
    type Result = QueryObjectTreeResult;
}
#[derive(Debug, Serialize, Deserialize)]
pub struct QueryObjectTreeParams {
    pub path: String,
}
pub type QueryObjectTreeResult = ObjectTreeType;

// ----------------------------------------------------------------------------
// Reparse
pub enum Reparse {}
impl Notification for Reparse {
    const METHOD: &'static str = "experimental/dreammaker/reparse";
    type Params = lsp_types::InitializedParams;
}

// ----------------------------------------------------------------------------
// StartDebugger
pub enum StartDebugger {}
impl Request for StartDebugger {
    const METHOD: &'static str = "experimental/dreammaker/startDebugger";
    type Params = StartDebuggerParams;
    type Result = StartDebuggerResult;
}
#[derive(Debug, Serialize, Deserialize)]
pub struct StartDebuggerParams {
    pub dreamseeker_exe: String,
    pub env: Option<HashMap<String, String>>,
}
#[derive(Debug, Serialize, Deserialize)]
pub struct StartDebuggerResult {
    pub port: u16,
}
