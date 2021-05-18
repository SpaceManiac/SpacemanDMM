// TODO: These will be shared properly

use serde::{Deserialize, Serialize};

#[allow(dead_code)]
pub const DEFAULT_PORT: u16 = 2448;

// Message from client -> server
#[derive(Serialize, Deserialize, Debug)]
pub enum Request {
    Disconnect,
    Configured,
    StdDef,
    Eval {
        frame_id: Option<u32>,
        command: String,
        context: Option<String>,
    },
    CurrentInstruction {
        frame_id: u32,
    },
    BreakpointSet {
        instruction: InstructionRef,
        condition: Option<String>,
    },
    BreakpointUnset {
        instruction: InstructionRef,
    },
    CatchRuntimes {
        should_catch: bool,
    },
    LineNumber {
        proc: ProcRef,
        offset: u32,
    },
    Offset {
        proc: ProcRef,
        line: u32,
    },
    Stacks,
    StackFrames {
        stack_id: u32,
        start_frame: Option<u32>,
        count: Option<u32>,
    },
    Scopes {
        frame_id: u32,
    },
    Variables {
        vars: VariablesRef,
    },
    Continue {
        kind: ContinueKind,
    },
    Pause,
}

// Message from server -> client
#[derive(Serialize, Deserialize, Debug)]
pub enum Response {
    Ack,
    StdDef(Option<String>),
    Eval(EvalResponse),
    CurrentInstruction(Option<InstructionRef>),
    BreakpointSet {
        result: BreakpointSetResult,
    },
    BreakpointUnset {
        success: bool,
    },
    LineNumber {
        line: Option<u32>,
    },
    Offset {
        offset: Option<u32>,
    },
    Stacks {
        stacks: Vec<Stack>,
    },
    StackFrames {
        frames: Vec<StackFrame>,
        total_count: u32,
    },
    Scopes {
        arguments: Option<VariablesRef>,
        locals: Option<VariablesRef>,
        globals: Option<VariablesRef>,
    },
    Variables {
        vars: Vec<Variable>,
    },

    // These responses can occur at any moment, even between a request and its response
    // I guess they aren't really responses...
    Disconnect,
    Notification {
        message: String,
    },
    BreakpointHit {
        reason: BreakpointReason,
    },
}

#[derive(Serialize, Deserialize, Debug, Hash, PartialEq, Eq, Clone)]
pub struct ProcRef {
    pub path: String,
    pub override_id: u32,
}

#[derive(Serialize, Deserialize, Debug, Hash, PartialEq, Eq, Clone)]
pub struct InstructionRef {
    pub proc: ProcRef,
    pub offset: u32,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum BreakpointReason {
    Breakpoint,
    Step,
    Pause,
    Runtime(String),
}

#[derive(Serialize, Deserialize, Debug)]
pub enum ContinueKind {
    Continue,
    StepOver { stack_id: u32 },
    StepInto { stack_id: u32 },
    StepOut { stack_id: u32 },
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Stack {
    pub id: u32,
    pub name: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct StackFrame {
    pub id: u32,
    pub instruction: InstructionRef,
    pub line: Option<u32>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum BreakpointSetResult {
    Success { line: Option<u32> },
    Failed,
}

#[derive(Clone, Hash, Eq, PartialEq, Serialize, Deserialize, Debug)]
pub struct VariablesRef(pub i32);

#[derive(Serialize, Deserialize, Debug)]
pub struct Variable {
    pub name: String,
    pub value: String,
    pub variables: Option<VariablesRef>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct EvalResponse {
    pub value: String,
    pub variables: Option<VariablesRef>,
}
