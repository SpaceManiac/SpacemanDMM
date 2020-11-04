// TODO: These will be shared properly

use serde::{Deserialize, Serialize};

// Message from client -> server
#[derive(Serialize, Deserialize, Debug)]
pub enum Request {
	BreakpointSet { proc: ProcRef, offset: u32, },
	BreakpointUnset { proc: ProcRef, offset: u32, },
	LineNumber { proc: ProcRef, offset: u32, },
	Offset { proc: ProcRef, line: u32, },
	Continue { kind: ContinueKind },
}

// Message from server -> client
#[derive(Serialize, Deserialize, Debug)]
pub enum Response {
	BreakpointSet { success: bool },
	BreakpointUnset { success: bool },
	LineNumber { line: Option<u32> },
	Offset { offset: Option<u32> },

	// Notifications (no `Request` counter-part)
	// The server should send back a `Continue` request after getting this
	// These should only be sent between Server/ServerThread on the notifications channel
	BreakpointHit { reason: BreakpointReason },
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ProcRef {
	pub path: String,
	pub override_id: u32,
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
	// StepInto,
	// StepOut,
}
