//! Serde types for the Debug Adapter Protocol.
//!
//! * https://microsoft.github.io/debug-adapter-protocol/specification
#![allow(non_snake_case)]

use std::collections::HashMap;
use serde_json::Value;

pub trait Request {
    type Params;
    type Result;
    const COMMAND: &'static str;
}

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
pub struct RequestMessage {
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
// Request

pub enum Initialize {}

impl Request for Initialize {
    type Params = InitializeRequestArguments;
    type Result = InitializeResponseBody;
    const COMMAND: &'static str = "initialize";
}

/// Arguments for ‘initialize’ request.
#[derive(Serialize, Deserialize, Debug)]
pub struct InitializeRequestArguments {
    /**
     * The ID of the (frontend) client using this adapter.
     */
    clientID: Option<String>,

    /**
     * The human readable name of the (frontend) client using this adapter.
     */
    clientName: Option<String>,

    /**
     * The ID of the debug adapter.
     */
    adapterID: Option<String>,

    /**
     * The ISO-639 locale of the (frontend) client using this adapter, e.g. en-US or de-CH.
     */
    locale: Option<String>,

    /**
     * If true all line numbers are 1-based (default).
     */
    linesStartAt1: Option<bool>,

    /**
     * If true all column numbers are 1-based (default).
     */
    columnsStartAt1: Option<bool>,

    /**
     * Determines in what format paths are specified. The default is 'path', which is the native format.
     * Values: 'path', 'uri', etc.
     */
    pathFormat: Option<String>,

    /**
     * Client supports the optional type attribute for variables.
     */
    supportsVariableType: Option<bool>,

    /**
     * Client supports the paging of variables.
     */
    supportsVariablePaging: Option<bool>,

    /**
     * Client supports the runInTerminal request.
     */
    supportsRunInTerminalRequest: Option<bool>,

    /**
     * Client supports memory references.
     */
    supportsMemoryReferences: Option<bool>,
}

/// Response to ‘initialize’ request.
pub type InitializeResponseBody = Option<Capabilities>;

// ----------------------------------------------------------------------------
// Types

/// Information about the capabilities of a debug adapter.
#[derive(Serialize, Deserialize, Debug)]
pub struct Capabilities {
    /**
     * The debug adapter supports the 'configurationDone' request.
     */
    supportsConfigurationDoneRequest: Option<bool>,

    /**
     * The debug adapter supports function breakpoints.
     */
    supportsFunctionBreakpoints: Option<bool>,

    /**
     * The debug adapter supports conditional breakpoints.
     */
    supportsConditionalBreakpoints: Option<bool>,

    /**
     * The debug adapter supports breakpoints that break execution after a specified number of hits.
     */
    supportsHitConditionalBreakpoints: Option<bool>,

    /**
     * The debug adapter supports a (side effect free) evaluate request for data hovers.
     */
    supportsEvaluateForHovers: Option<bool>,

    /**
     * Available filters or options for the setExceptionBreakpoints request.
     */
    //exceptionBreakpointFilters?: ExceptionBreakpointsFilter[];

    /**
     * The debug adapter supports stepping back via the 'stepBack' and 'reverseContinue' requests.
     */
    supportsStepBack: Option<bool>,

    /**
     * The debug adapter supports setting a variable to a value.
     */
    supportsSetVariable: Option<bool>,

    /**
     * The debug adapter supports restarting a frame.
     */
    supportsRestartFrame: Option<bool>,

    /**
     * The debug adapter supports the 'gotoTargets' request.
     */
    supportsGotoTargetsRequest: Option<bool>,

    /**
     * The debug adapter supports the 'stepInTargets' request.
     */
    supportsStepInTargetsRequest: Option<bool>,

    /**
     * The debug adapter supports the 'completions' request.
     */
    supportsCompletionsRequest: Option<bool>,

    /**
     * The set of characters that should trigger completion in a REPL. If not specified, the UI should assume the '.' character.
     */
    completionTriggerCharacters: Option<Vec<String>>,

    /**
     * The debug adapter supports the 'modules' request.
     */
    supportsModulesRequest: Option<bool>,

    /**
     * The set of additional module information exposed by the debug adapter.
     */
    //additionalModuleColumns?: ColumnDescriptor[];

    /**
     * Checksum algorithms supported by the debug adapter.
     */
    //supportedChecksumAlgorithms?: ChecksumAlgorithm[];

    /**
     * The debug adapter supports the 'restart' request. In this case a client should not implement 'restart' by terminating and relaunching the adapter but by calling the RestartRequest.
     */
    supportsRestartRequest: Option<bool>,

    /**
     * The debug adapter supports 'exceptionOptions' on the setExceptionBreakpoints request.
     */
    supportsExceptionOptions: Option<bool>,

    /**
     * The debug adapter supports a 'format' attribute on the stackTraceRequest, variablesRequest, and evaluateRequest.
     */
    supportsValueFormattingOptions: Option<bool>,

    /**
     * The debug adapter supports the 'exceptionInfo' request.
     */
    supportsExceptionInfoRequest: Option<bool>,

    /**
     * The debug adapter supports the 'terminateDebuggee' attribute on the 'disconnect' request.
     */
    supportTerminateDebuggee: Option<bool>,

    /**
     * The debug adapter supports the delayed loading of parts of the stack, which requires that both the 'startFrame' and 'levels' arguments and the 'totalFrames' result of the 'StackTrace' request are supported.
     */
    supportsDelayedStackTraceLoading: Option<bool>,

    /**
     * The debug adapter supports the 'loadedSources' request.
     */
    supportsLoadedSourcesRequest: Option<bool>,

    /**
     * The debug adapter supports logpoints by interpreting the 'logMessage' attribute of the SourceBreakpoint.
     */
    supportsLogPoints: Option<bool>,

    /**
     * The debug adapter supports the 'terminateThreads' request.
     */
    supportsTerminateThreadsRequest: Option<bool>,

    /**
     * The debug adapter supports the 'setExpression' request.
     */
    supportsSetExpression: Option<bool>,

    /**
     * The debug adapter supports the 'terminate' request.
     */
    supportsTerminateRequest: Option<bool>,

    /**
     * The debug adapter supports data breakpoints.
     */
    supportsDataBreakpoints: Option<bool>,

    /**
     * The debug adapter supports the 'readMemory' request.
     */
    supportsReadMemoryRequest: Option<bool>,

    /**
     * The debug adapter supports the 'disassemble' request.
     */
    supportsDisassembleRequest: Option<bool>,

    /**
     * The debug adapter supports the 'cancel' request.
     */
    supportsCancelRequest: Option<bool>,

    /**
     * The debug adapter supports the 'breakpointLocations' request.
     */
    supportsBreakpointLocationsRequest: Option<bool>,
}

/// A structured message object. Used to return errors from requests.
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
