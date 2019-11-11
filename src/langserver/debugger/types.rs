//! Serde types for the Debug Adapter Protocol.
//!
//! * https://microsoft.github.io/debug-adapter-protocol/specification

use std::collections::HashMap;
use serde_json::Value;

// ----------------------------------------------------------------------------
// Base Protocol

/// Base class of requests, responses, and events.
#[derive(Serialize, Deserialize, Debug)]
pub struct ProtocolMessage {
    /// Sequence number (also known as message ID). For protocol messages of type 'request' this ID can be used to cancel the request.
    pub seq: i64,
    #[serde(rename = "type")]
    /// Message type.
    /// Values: 'request', 'response', 'event', etc.
    pub type_: String,
}

/// A client or debug adapter initiated request.
#[derive(Serialize, Deserialize, Debug)]
pub struct Request {
    #[serde(flatten)]
    pub protocol_message: ProtocolMessage,
    /// The command to execute.
    pub command: String,
    /// Object containing arguments for the command.
    pub arguments: Option<Value>,
}

/// A debug adapter initiated event.
#[derive(Serialize, Deserialize, Debug)]
pub struct Event {
    #[serde(flatten)]
    pub protocol_message: ProtocolMessage,
    /// Type of event.
    pub event: String,
    /// Event-specific information.
    pub body: Option<Value>,
}

/// Response for a request.
#[derive(Serialize, Deserialize, Debug)]
pub struct Response {
    #[serde(flatten)]
    pub protocol_message: ProtocolMessage,

    /**
     * Sequence number of the corresponding request.
     */
    pub request_seq: i64,

    /**
     * Outcome of the request.
     * If true, the request was successful and the 'body' attribute may contain the result of the request.
     * If the value is false, the attribute 'message' contains the error in short form and the 'body' may contain additional information (see 'ErrorResponse.body.error').
     */
    pub success: bool,

    /**
     * The command requested.
     */
    pub command: String,

    /**
     * Contains the raw error in short form if 'success' is false.
     * This raw error might be interpreted by the frontend and is not shown in the UI.
     * Some predefined values exist.
     * Values:
     * 'cancelled': request was cancelled.
     * etc.
     */
    pub message: Option<String>,

    /**
     * Contains request result if success is true and optional error details if success is false.
     */
    pub body: Option<Value>,
}

/// On error (whenever ‘success’ is false), the body can provide more details.
#[derive(Serialize, Deserialize, Debug)]
pub struct ErrorResponseBody {
    /// An optional, structured error message.
    pub error: Option<Message>,
}

// ----------------------------------------------------------------------------
// Types

#[derive(Serialize, Deserialize, Debug)]
pub struct Message {
    /**
     * Unique identifier for the message.
     */
    pub id: i64,

    /**
     * A format string for the message. Embedded variables have the form '{name}'.
     * If variable name starts with an underscore character, the variable does not contain user data (PII) and can be safely used for telemetry purposes.
     */
    pub format: String,

    /**
     * An object used as a dictionary for looking up the variables in the format string.
     */
    pub variables: Option<HashMap<String, String>>,

    /**
     * If true send to telemetry.
     */
    #[serde(rename = "sendTelemetry")]
    pub send_telemetry: Option<bool>,

    /**
     * If true show user.
     */
    #[serde(rename = "showUser")]
    pub show_user: Option<bool>,

    /**
     * An optional url where additional information about this message can be found.
     */
    pub url: Option<String>,

    /**
     * An optional label that is presented to the user as the UI for opening the url.
     */
    #[serde(rename = "urlLabel")]
    pub url_label: Option<String>,
}
