//! Serde types for the Extools debugger protocol.
//!
//! * https://github.com/MCHSL/extools/blob/master/byond-extools/src/debug_server/protocol.h

use serde_json::Value;

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
    pub content: Option<Value>,
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
pub struct ProcListResponse(pub Vec<String>);

impl Response for ProcListResponse {
    const TYPE: &'static str = "proc list";
}

// #define MESSAGE_PROC_DISASSEMBLY "proc disassembly" //Request content is the proc name, response content is DisassembledProc
#[derive(Serialize, Deserialize, Debug)]
pub struct ProcDisassemblyRequest(pub String);

impl Request for ProcDisassemblyRequest {
    const TYPE: &'static str = "proc disassembly";
}

#[derive(Serialize, Deserialize, Debug)]
pub struct DisassembledProc {
    pub name: String,
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
    pub offset: i64,
}

impl Request for BreakpointSet {
    const TYPE: &'static str = "breakpoint set";
}

impl Response for BreakpointSet {
    const TYPE: &'static str = "breakpoint set";
}

// #define MESSAGE_BREAKPOINT_STEP "breakpoint step" //Content is empty
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointStep;

impl Request for BreakpointStep {
    const TYPE: &'static str = "breakpoint step";
}

// #define MESSAGE_BREAKPOINT_RESUME "breakpoint resume" //Content is empty
#[derive(Serialize, Deserialize, Debug)]
pub struct BreakpointResume;

impl Request for BreakpointResume {
    const TYPE: &'static str = "breakpoint resume";
}

// ----------------------------------------------------------------------------
// Spontaneous events

/*
#define MESSAGE_VALUES_LOCALS "locals" //Content is a vector of ValueTexts

#define MESSAGE_BREAKPOINT_HIT "breakpoint hit" //Content is BreakpointHit
#define MESSAGE_BREAKPOINT_UNSET "breakpoint unset" //Content is BreakpointUnset

#define MESSAGE_VALUES_ARGS "args" //^
#define MESSAGE_VALUES_STACK "stack" //^
#define MESSAGE_CALL_STACK "call stack" //Content is a vector of proc paths
*/
