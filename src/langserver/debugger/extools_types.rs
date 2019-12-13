//! Serde types for the Extools debugger protocol.
//!
//! * https://github.com/MCHSL/extools/blob/master/byond-extools/src/debug_server/protocol.h

use serde_json::Value as Json;

// > All communication happens over a TCP socket using a JSON-based protocol.
// > A null byte signifies the end of a message.

pub trait Request: serde::Serialize {
    const TYPE: &'static str;
}

pub trait Response: for<'de> serde::Deserialize<'de> {
    const TYPE: &'static str;
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ProtocolMessage {
    #[serde(rename = "type")]
    pub type_: String,
    pub content: Option<Json>,
}

// ----------------------------------------------------------------------------
// Requests and responses

// #define MESSAGE_RAW "raw message" //Content is a string, used for debugging purposes (how meta)
#[derive(Serialize, Deserialize, Debug)]
pub struct Raw(pub String);

impl Request for Raw {
    const TYPE: &'static str = "raw message";
}

impl Response for Raw {
    const TYPE: &'static str = "raw message";
}

// #define MESSAGE_PROC_LIST "proc list" // Content is a vector of proc paths.
#[derive(Serialize, Deserialize, Debug)]
pub struct ProcListRequest;

impl Request for ProcListRequest {
    const TYPE: &'static str = "proc list";
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ProcListResponse(pub Vec<ProcListResponseEntry>);

#[derive(Serialize, Deserialize, Debug)]
pub struct ProcListResponseEntry {
    pub name: String,
    pub override_id: usize,
}

impl Response for ProcListResponse {
    const TYPE: &'static str = "proc list";
}

// #define MESSAGE_PROC_DISASSEMBLY "proc disassembly" //Request content is the proc name, response content is DisassembledProc
#[derive(Serialize, Deserialize, Debug)]
pub struct ProcDisassemblyRequest {
    pub name: String,
    pub override_id: usize,
}

impl Request for ProcDisassemblyRequest {
    const TYPE: &'static str = "proc disassembly";
}

#[derive(Serialize, Deserialize, Debug)]
pub struct DisassembledProc {
    pub name: String,
    pub override_id: usize,
    pub instructions: Vec<DisassembledInstruction>,
}

impl Response for DisassembledProc {
    const TYPE: &'static str = "proc disassembly";
}

#[derive(Serialize, Deserialize, Debug)]
pub struct DisassembledInstruction {
    pub offset: i64,
    pub bytes: String,
    pub mnemonic: String,
    pub comment: String,
    pub possible_jumps: Vec<u16>,
}

// #define MESSAGE_BREAKPOINT_SET "breakpoint set" //Content is BreakpointSet
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointSet {
    pub proc: String,
    pub override_id: usize,
    pub offset: i64,
}

impl Request for BreakpointSet {
    const TYPE: &'static str = "breakpoint set";
}

impl Response for BreakpointSet {
    const TYPE: &'static str = "breakpoint set";
}

// #define MESSAGE_BREAKPOINT_UNSET "breakpoint unset" //Content is BreakpointUnset
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointUnset {
    pub proc: String,
    pub override_id: usize,
    pub offset: i64,
}

impl Request for BreakpointUnset {
    const TYPE: &'static str = "breakpoint unset";
}

impl Response for BreakpointUnset {
    const TYPE: &'static str = "breakpoint unset";
}

// #define MESSAGE_BREAKPOINT_STEP_INTO "breakpoint step into" //Content is empty
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointStepInto;

impl Request for BreakpointStepInto {
    const TYPE: &'static str = "breakpoint step into";
}

// #define MESSAGE_BREAKPOINT_STEP_OVER "breakpoint step over" //Content is empty
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointStepOver;

impl Request for BreakpointStepOver {
    const TYPE: &'static str = "breakpoint step over";
}

// #define MESSAGE_BREAKPOINT_RESUME "breakpoint resume" //Content is empty
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointResume;

impl Request for BreakpointResume {
    const TYPE: &'static str = "breakpoint resume";
}

// #define MESSAGE_GET_FIELD "get field" //Request content is FieldRequest, response content is ValueText
#[derive(Serialize, Deserialize, Debug)]
pub struct FieldRequest {
    pub datum_type: String,
    pub datum_id: i64,
    pub field_name: String,
}

impl Request for FieldRequest {
    const TYPE: &'static str = "get field";
}

#[derive(Deserialize, Debug)]
pub struct FieldResponse(pub ValueText);

impl Response for FieldResponse {
    const TYPE: &'static str = "get field";
}

// #define MESSAGE_GET_GLOBAL "get global" //Request content is a string with the global name, response is a ValueText
#[derive(Serialize, Deserialize, Debug)]
pub struct GetGlobal(pub String);

impl Request for GetGlobal {
    const TYPE: &'static str = "get global";
}

#[derive(Deserialize, Debug)]
pub struct GetGlobalResponse(ValueText);

impl Response for GetGlobalResponse {
    const TYPE: &'static str = "get global";
}

// #define MESSAGE_GET_TYPE "get type" //Request content is Datum, response content is a string
#[derive(Serialize, Deserialize, Debug)]
pub struct GetType {
    pub datum_type: String,
    pub datum_id: i64,
}

impl Request for GetType {
    const TYPE: &'static str = "get type";
}

#[derive(Serialize, Deserialize, Debug)]
pub struct GetTypeResponse(pub String);

impl Response for GetTypeResponse {
    const TYPE: &'static str = "get type";
}

// #define MESSAGE_TOGGLE_BREAK_ON_RUNTIME "break on runtimes" //Response content is true or false
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakOnRuntime(pub bool);

impl Request for BreakOnRuntime {
    const TYPE: &'static str = "break on runtimes";
}

impl Response for BreakOnRuntime {
    const TYPE: &'static str = "break on runtimes";
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ConfigurationDone;

impl Request for ConfigurationDone {
    const TYPE: &'static str = "configuration done";
}

// ----------------------------------------------------------------------------
// Spontaneous events

// #define MESSAGE_RUNTIME "runtime" //Content is a Runtime
#[derive(Serialize, Deserialize, Debug)]
pub struct Runtime {
    pub proc: String,
    pub override_id: usize,
    pub offset: i64,
    pub message: String,
}

impl Response for Runtime {
    const TYPE: &'static str = "runtime";
}

// #define MESSAGE_BREAKPOINT_HIT "breakpoint hit" //Content is BreakpointHit
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointHit {
    pub proc: String,
    pub override_id: usize,
    pub offset: i64,
}

impl Response for BreakpointHit {
    const TYPE: &'static str = "breakpoint hit";
}

// #define MESSAGE_CALL_STACK "call stack" //Content is a vector of proc paths
#[derive(Deserialize, Debug)]
pub struct CallStack(pub Vec<StackFrame>);

#[derive(Deserialize, Debug, Clone)]
pub struct StackFrame {
    pub name: String,
    pub override_id: usize,
    pub usr: ValueText,
    pub src: ValueText,
    pub locals: Vec<ValueText>,
    pub args: Vec<ValueText>,
    pub instruction_pointer: i64,
}

impl Response for CallStack {
    const TYPE: &'static str = "call stack";
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Literal {
    #[serde(rename = "ref")]
    Ref(i64),
    #[serde(rename = "number")]
    Number(f32),
    #[serde(rename = "string")]
    String(String),
    #[serde(rename = "typepath")]
    Typepath(String),
    #[serde(rename = "resource")]
    Resource(String),
}

impl Literal {
    pub const NULL: Literal = Literal::Ref(0);
    pub const WORLD: Literal = Literal::Ref(0x0e_00_00_00);
}

#[derive(Deserialize, Debug, Clone)]
pub struct ValueText {
    pub literal: Literal,
    #[serde(default)]
    pub has_vars: bool,
    #[serde(default)]
    pub is_list: bool,
}

impl ValueText {
    pub fn datum_address(&self) -> i64 {
        match self.literal {
            Literal::Ref(r) if self.has_vars => r,
            _ => 0,
        }
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Literal::Ref(0) => fmt.write_str("null"),
            Literal::Ref(0x0e_00_00_00) => fmt.write_str("world"),
            Literal::Ref(v) => write!(fmt, "[0x{:08x}]", v),
            Literal::Number(n) => write!(fmt, "{}", n),
            Literal::String(s) => write!(fmt, "{:?}", s),
            Literal::Typepath(t) => write!(fmt, "{}", t),
            Literal::Resource(f) => write!(fmt, "'{}'", f),
        }
    }
}

impl std::fmt::Display for ValueText {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.literal.fmt(fmt)
    }
}
