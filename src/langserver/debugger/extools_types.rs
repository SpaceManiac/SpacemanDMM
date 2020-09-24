//! Serde types for the Extools debugger protocol.
//!
//! * https://github.com/MCHSL/extools/blob/master/byond-extools/src/debug_server/protocol.h
///
/// > All communication happens over a TCP socket using a JSON-based protocol.
/// > A null byte signifies the end of a message.

use std::collections::HashMap;
use serde_json::Value as Json;

// ----------------------------------------------------------------------------
// Extools data structures

#[derive(Serialize, Deserialize, Debug)]
pub struct ProcId {
    pub proc: String,
    pub override_id: usize,
}

impl std::fmt::Display for ProcId {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}#{}", self.proc, self.override_id)
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ProcOffset {
    pub proc: String,
    pub override_id: usize,
    // end ProcId
    pub offset: i64,
}

impl std::fmt::Display for ProcOffset {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}#{}@{}", self.proc, self.override_id, self.offset)
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct DisassembledProc {
    pub proc: String,
    pub override_id: usize,
    // end ProcId
    pub instructions: Vec<DisassembledInstruction>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct DisassembledInstruction {
    pub offset: i64,
    pub bytes: String,
    pub mnemonic: String,
    pub comment: String,
    pub possible_jumps: Vec<u16>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct StackFrame {
    pub proc: String,
    pub override_id: usize,
    pub offset: i64,
    // end ProcOffset
    pub usr: ValueText,
    pub src: ValueText,
    pub dot: ValueText,
    pub locals: Vec<ValueText>,
    pub args: Vec<ValueText>,
    pub local_names: Vec<String>,
    pub arg_names: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Runtime {
    pub proc: String,
    pub override_id: usize,
    pub offset: i64,
    // end ProcOffset
    pub message: String,
}

// ----------------------------------------------------------------------------
// BYOND value types

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ref(pub i64);

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Literal {
    #[serde(rename = "ref")]
    Ref(Ref),
    #[serde(rename = "number")]
    Number(f32),
    #[serde(rename = "string")]
    String(String),
    #[serde(rename = "typepath")]
    Typepath(String),
    #[serde(rename = "resource")]
    Resource(String),
    #[serde(rename = "proc")]
    Proc(String),
}

#[derive(Deserialize, Debug, Clone)]
pub struct ValueText {
    pub literal: Literal,
    #[serde(default)]
    pub has_vars: bool,
    #[serde(default)]
    pub is_list: bool,
}

#[derive(Deserialize, Debug)]
pub enum ListContents {
    #[serde(rename = "linear")]
    Linear(Vec<ValueText>),
    #[serde(rename = "associative")]
    Associative(Vec<(ValueText, ValueText)>),
}

impl Ref {
    pub const NULL: Ref = Ref(0);
    pub const WORLD: Ref = Ref(0x0e_00_00_00);
}

impl Literal {
    pub const NULL: Literal = Literal::Ref(Ref::NULL);
    pub const WORLD: Literal = Literal::Ref(Ref::WORLD);
}

impl ValueText {
    pub fn from_variables_reference(raw: i64) -> (ValueText, Ref) {
        let ref_ = Ref(raw);
        let is_list = raw >> 24 == 0x0F;

        (ValueText {
            literal: Literal::Ref(ref_),
            has_vars: !is_list,
            is_list,
        }, ref_)
    }

    pub fn to_variables_reference(&self) -> i64 {
        match self.literal {
            Literal::Ref(r) if self.has_vars || self.is_list => r.0,
            _ => 0,
        }
    }
}

impl std::fmt::Display for Ref {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Ref::NULL => fmt.write_str("null"),
            Ref::WORLD => fmt.write_str("world"),
            Ref(v) => write!(fmt, "[0x{:08x}]", v),
        }
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Literal::Ref(v) => write!(fmt, "{}", v),
            Literal::Number(n) => write!(fmt, "{}", n),
            Literal::String(s) => write!(fmt, "{:?}", s),
            Literal::Typepath(t) => write!(fmt, "{}", t),
            Literal::Resource(f) => write!(fmt, "'{}'", f),
            Literal::Proc(p) => {
                match p.rfind('/') {
                    Some(idx) => write!(fmt, "{}/proc/{}", &p[..idx], &p[idx + 1..]),
                    None => write!(fmt, "{}", p),
                }
            }
        }
    }
}

impl std::fmt::Display for ValueText {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.literal.fmt(fmt)
    }
}

// ----------------------------------------------------------------------------
// Requests and responses

#[derive(Serialize, Deserialize, Debug)]
pub struct ProtocolMessage {
    #[serde(rename = "type")]
    pub type_: String,
    pub content: Option<Json>,
}

pub trait Request: serde::Serialize {
    const TYPE: &'static str;
}

pub trait Response: for<'de> serde::Deserialize<'de> {
    const TYPE: &'static str;
}

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
pub struct ProcListResponse(pub Vec<ProcId>);

impl Response for ProcListResponse {
    const TYPE: &'static str = "proc list";
}

// #define MESSAGE_PROC_DISASSEMBLY "proc disassembly" //Request content is the proc name, response content is DisassembledProc
#[derive(Serialize, Deserialize, Debug)]
pub struct ProcDisassemblyRequest(pub ProcId);

impl Request for ProcDisassemblyRequest {
    const TYPE: &'static str = "proc disassembly";
}

impl Response for DisassembledProc {
    const TYPE: &'static str = "proc disassembly";
}

// #define MESSAGE_BREAKPOINT_SET "breakpoint set" //Content is BreakpointSet
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointSet(pub ProcOffset);

impl Request for BreakpointSet {
    const TYPE: &'static str = "breakpoint set";
}

impl Response for BreakpointSet {
    const TYPE: &'static str = "breakpoint set";
}

// #define MESSAGE_BREAKPOINT_UNSET "breakpoint unset" //Content is BreakpointUnset
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointUnset(pub ProcOffset);

impl Request for BreakpointUnset {
    const TYPE: &'static str = "breakpoint unset";
}

impl Response for BreakpointUnset {
    const TYPE: &'static str = "breakpoint unset";
}

// #define MESSAGE_BREAKPOINT_STEP_INTO "breakpoint step into" //Content is empty
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BreakpointStepInto;

impl Request for BreakpointStepInto {
    const TYPE: &'static str = "breakpoint step into";
}

// #define MESSAGE_BREAKPOINT_STEP_OVER "breakpoint step over" //Content is empty
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BreakpointStepOver;

impl Request for BreakpointStepOver {
    const TYPE: &'static str = "breakpoint step over";
}

// #define MESSAGE_BREAKPOINT_STEP_OVER "breakpoint step over" //Content is empty
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BreakpointStepOut;

impl Request for BreakpointStepOut {
    const TYPE: &'static str = "breakpoint step out";
}

// #define MESSAGE_BREAKPOINT_RESUME "breakpoint resume" //Content is empty
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointResume;

impl Request for BreakpointResume {
    const TYPE: &'static str = "breakpoint resume";
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Pause;

impl Request for Pause {
    const TYPE: &'static str = "breakpoint pause";
}

// #define MESSAGE_GET_FIELD "get field" //Request content is FieldRequest, response content is ValueText
#[derive(Serialize, Deserialize, Debug)]
pub struct FieldRequest {
    #[serde(rename = "ref")]
    pub ref_: Ref,
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

#[derive(Serialize, Deserialize, Debug)]
pub struct GetAllFields(pub Ref);

impl Request for GetAllFields {
    const TYPE: &'static str = "get all fields";
}

#[derive(Deserialize, Debug)]
pub struct GetAllFieldsResponse(pub HashMap<String, ValueText>);

impl Response for GetAllFieldsResponse {
    const TYPE: &'static str = "get all fields";
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

#[derive(Serialize, Deserialize, Debug)]
pub struct GetListContents(pub Ref);

impl Request for GetListContents {
    const TYPE: &'static str = "get list contents";
}

impl Response for ListContents {
    const TYPE: &'static str = "get list contents";
}

#[derive(Serialize, Deserialize, Debug)]
pub struct GetSource(pub String);

impl Request for GetSource {
    const TYPE: &'static str = "get source";
}

impl Response for GetSource {
    const TYPE: &'static str = "get source";
}

// #define MESSAGE_GET_TYPE "get type" //Request content is Datum, response content is a string
#[derive(Serialize, Deserialize, Debug)]
pub struct GetType(pub Ref);

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

// #define MESSAGE_BREAKPOINT_HIT "breakpoint hit" //Content is BreakpointHit
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointHit {
    pub proc: String,
    pub override_id: usize,
    pub offset: i64,
    // end ProcOffset
    pub reason: BreakpointHitReason,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum BreakpointHitReason {
    #[serde(rename = "breakpoint opcode")]
    BreakpointOpcode,
    #[serde(rename = "step")]
    Step,
    #[serde(rename = "pause")]
    Pause,
    #[serde(other)]
    Unknown,
}

impl Response for BreakpointHit {
    const TYPE: &'static str = "breakpoint hit";
}

// #define MESSAGE_CALL_STACK "call stack" //Content is a vector of proc paths
#[derive(Deserialize, Debug)]
pub struct CallStack {
    pub current: Vec<StackFrame>,
    pub suspended: Vec<Vec<StackFrame>>,
}

impl Response for CallStack {
    const TYPE: &'static str = "call stack";
}

impl Response for Runtime {
    const TYPE: &'static str = "runtime";
}
