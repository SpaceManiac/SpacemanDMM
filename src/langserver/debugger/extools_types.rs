//! Serde types for the Extools debugger protocol.
//!
//! * https://github.com/MCHSL/extools/blob/master/byond-extools/src/debug_server/protocol.h

use serde_json::Value;

// > All communication happens over a TCP socket using a JSON-based protocol.
// > A null byte signifies the end of a message.

pub trait Message: serde::Serialize {
    const TYPE: &'static str;
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ProtocolMessage {
    #[serde(rename = "type")]
    pub type_: String,
    pub content: Option<Value>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Raw(pub String);

impl Message for Raw {
    const TYPE: &'static str = "raw message";
}
