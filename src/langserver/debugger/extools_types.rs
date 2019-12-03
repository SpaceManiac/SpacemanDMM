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

#[derive(Serialize, Deserialize, Debug)]
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

#[derive(Serialize, Deserialize, Debug)]
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

// ----------------------------------------------------------------------------
// Spontaneous events

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
#[derive(Serialize, Deserialize, Debug)]
pub struct CallStack(pub Vec<StackFrame>);

#[derive(Serialize, Deserialize, Debug, Clone)]
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

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ValueText {
    #[serde(rename = "type")]
    pub type_: String,
    pub value: String,
}

impl ValueText {
    fn has_vars(&self) -> bool {
        match self.type_.as_str() {
            "TURF" |
            "OBJ" |
            "MOB" |
            "AREA" |
            "CLIENT" |
            "IMAGE" |
            "WORLD" |
            "DATUM" => true,
            _ => false,
        }
    }

    pub fn datum_address(&self) -> i64 {
        if self.has_vars() {
            match (category_number(&self.type_), self.value.parse::<i64>()) {
                (Some(cat), Ok(id)) => (cat << 24) | id,
                _ => 0
            }
        } else {
            0
        }
    }
}

impl std::fmt::Display for ValueText {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let category = match self.type_.as_str() {
            "NULL" => return fmt.write_str("null"),
            "STRING" => return write!(fmt, "{:?}", self.value),
            "WORLD" => return fmt.write_str("world"),
            "NUMBER" => return write!(fmt, "{}", self.value),
            other => match category_number(other) {
                Some(n) => n,
                None => { write!(fmt, "{} ", self.type_)?; 0 }
            }
        };
        match self.value.parse::<i64>() {
            Ok(v) => write!(fmt, "[0x{:x}{:06x}]", category, v),
            Err(_) => write!(fmt, "[{}]", self.value),
        }
    }
}

fn category_number(type_: &str) -> Option<i64> {
    Some(match type_ {
        "NULL" => 0x00,
        "TURF" => 0x01,
        "OBJ" => 0x02,
        "MOB" => 0x03,
        "AREA" => 0x04,
        "CLIENT" => 0x05,
        "STRING" => 0x06,
        "MOBTYPE" => 0x08,
        "OBJTYPE" => 0x09,
        "IMAGE" => 0x0D,
        "WORLD" => 0x0E,
        "LIST" => 0x0F,
        "DATUM" => 0x21,
        "SAVEFILE" => 0x23,
        "FILE" => 0x27,
        "PATH_LIST" => 0x28,
        "NUMBER" => 0x2A,
        "CLIENTTYPE" => 0x3B,
        "VARS_LIST" => 0x52,
        _ => return None,
    })
}

pub fn category_name(id: i64) -> Option<&'static str> {
    Some(match id {
        0x00 => "NULL",
        0x01 => "TURF",
        0x02 => "OBJ",
        0x03 => "MOB",
        0x04 => "AREA",
        0x05 => "CLIENT",
        0x0D => "IMAGE",
        0x0E => "WORLD",
        0x21 => "DATUM",
        // TODO: others
        _ => return None,
    })
}

// #define MESSAGE_VALUES_LOCALS "locals" //Content is a vector of ValueTexts
#[derive(Serialize, Deserialize, Debug)]
pub struct Locals(pub Vec<ValueText>);

impl Response for Locals {
    const TYPE: &'static str = "locals";
}

// #define MESSAGE_VALUES_ARGS "args" //^
#[derive(Serialize, Deserialize, Debug)]
pub struct Args(pub Vec<ValueText>);

impl Response for Args {
    const TYPE: &'static str = "args";
}

// #define MESSAGE_VALUES_STACK "stack" //^
#[derive(Serialize, Deserialize, Debug)]
pub struct Stack(pub Vec<ValueText>);

impl Response for Stack {
    const TYPE: &'static str = "stack";
}
