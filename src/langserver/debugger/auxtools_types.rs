// TODO: These will be shared properly

use serde::{Deserialize, Serialize};

#[allow(dead_code)]
pub const DEFAULT_PORT: u16 = 2448;

// Message from client -> server
#[derive(Serialize, Deserialize, Debug)]
pub enum Request {
	BreakpointSet {
		instruction: InstructionRef,
	},
	BreakpointUnset {
		instruction: InstructionRef,
	},
	LineNumber {
		proc: ProcRef,
		offset: u32,
	},
	Offset {
		proc: ProcRef,
		line: u32,
	},
	StackFrames {
		thread_id: u32,
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
	BreakpointHit {
		reason: BreakpointReason,
	},
}

#[derive(Serialize, Deserialize, Debug, Hash, PartialEq, Eq, Clone)]
pub struct ProcRef {
	pub path: String,
	// TODO: this is 0 in some places
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
}

#[derive(Serialize, Deserialize, Debug)]
pub enum ContinueKind {
	Continue,
	StepOver,
	StepInto,
	StepOut,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct StackFrame {
	pub instruction: InstructionRef,
	pub line: Option<u32>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum BreakpointSetResult {
	Success { line: Option<u32> },
	Failed,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum VariablesRef {
	Arguments { frame: u32 },
	Locals { frame: u32 },
	Internal { tag: u8, data: u32 },
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Variable {
	pub name: String,
	pub kind: String,
	pub value: String,
	pub variables: Option<VariablesRef>,
}
