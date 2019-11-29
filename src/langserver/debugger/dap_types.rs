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

pub trait Event: serde::Serialize {
    const EVENT: &'static str;
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

impl RequestMessage {
    pub const TYPE: &'static str = "request";
}

/// A debug adapter initiated event.
#[derive(Serialize, Deserialize, Debug)]
pub struct EventMessage {
    #[serde(flatten)]
    pub protocol_message: ProtocolMessage,
    /// Type of event.
    pub event: String,
    /// Event-specific information.
    pub body: Option<Value>,
}

impl EventMessage {
    pub const TYPE: &'static str = "event";
}

/// Response for a request.
#[derive(Serialize, Deserialize, Debug)]
pub struct ResponseMessage {
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

impl ResponseMessage {
    pub const TYPE: &'static str = "response";
}

/// On error (whenever ‘success’ is false), the body can provide more details.
#[derive(Serialize, Deserialize, Debug)]
pub struct ErrorResponseBody {
    /// An optional, structured error message.
    pub error: Option<Message>,
}

// ----------------------------------------------------------------------------
// Events

/// The event indicates that the debuggee has exited and returns its exit code.
#[derive(Serialize, Deserialize, Debug)]
pub struct ExitedEvent {
    /**
     * The exit code returned from the debuggee.
     */
    pub exitCode: i64,
}

impl Event for ExitedEvent {
    const EVENT: &'static str = "exited";
}

/// The event indicates that debugging of the debuggee has terminated. This
/// does not mean that the debuggee itself has exited.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct TerminatedEvent {
    /**
     * A debug adapter may set 'restart' to true (or to an arbitrary object) to request that the front end restarts the session.
     * The value is not interpreted by the client and passed unmodified as an attribute '__restart' to the 'launch' and 'attach' requests.
     */
    pub restart: Option<Value>,
}

impl Event for TerminatedEvent {
    const EVENT: &'static str = "terminated";
}

/// The event indicates that the target has produced some output.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct OutputEvent {
    /**
     * The output category. If not specified, 'console' is assumed.
     * Values: 'console', 'stdout', 'stderr', 'telemetry', etc.
     */
    pub category: Option<String>,

    /**
     * The output to report.
     */
    pub output: String,

    /**
     * If an attribute 'variablesReference' exists and its value is > 0, the output contains objects which can be retrieved by passing 'variablesReference' to the 'variables' request. The value should be less than or equal to 2147483647 (2^31 - 1).
     */
    pub variablesReference: Option<i64>,

    /**
     * An optional source location where the output was produced.
     */
    pub source: Option<Value>,  // TODO: Value -> Source

    /**
     * An optional source location line where the output was produced.
     */
    pub line: Option<i64>,

    /**
     * An optional source location column where the output was produced.
     */
    pub column: Option<i64>,

    /**
     * Optional data to report. For the 'telemetry' category the data will be sent to telemetry, for the other categories the data is shown in JSON format.
     */
    pub data: Option<Value>,
}

impl Event for OutputEvent {
    const EVENT: &'static str = "output";
}

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct InitializedEvent;

impl Event for InitializedEvent {
    const EVENT: &'static str = "initialized";
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
    pub clientID: Option<String>,

    /**
     * The human readable name of the (frontend) client using this adapter.
     */
    pub clientName: Option<String>,

    /**
     * The ID of the debug adapter.
     */
    pub adapterID: Option<String>,

    /**
     * The ISO-639 locale of the (frontend) client using this adapter, e.g. en-US or de-CH.
     */
    pub locale: Option<String>,

    /**
     * If true all line numbers are 1-based (default).
     */
    pub linesStartAt1: Option<bool>,

    /**
     * If true all column numbers are 1-based (default).
     */
    pub columnsStartAt1: Option<bool>,

    /**
     * Determines in what format paths are specified. The default is 'path', which is the native format.
     * Values: 'path', 'uri', etc.
     */
    pub pathFormat: Option<String>,

    /**
     * Client supports the optional type attribute for variables.
     */
    pub supportsVariableType: Option<bool>,

    /**
     * Client supports the paging of variables.
     */
    pub supportsVariablePaging: Option<bool>,

    /**
     * Client supports the runInTerminal request.
     */
    pub supportsRunInTerminalRequest: Option<bool>,

    /**
     * Client supports memory references.
     */
    pub supportsMemoryReferences: Option<bool>,
}

/// Response to ‘initialize’ request.
pub type InitializeResponseBody = Option<Capabilities>;

pub enum Launch {}

impl Request for Launch {
    type Params = LaunchRequestArguments;
    type Result = ();
    const COMMAND: &'static str = "launch";
}

/// Arguments for ‘launch’ request. Additional attributes are implementation specific.
#[derive(Serialize, Deserialize, Debug)]
pub struct LaunchRequestArguments {
    /**
     * If noDebug is true the launch request should launch the program without enabling debugging.
     */
    pub noDebug: Option<bool>,

    /**
     * Optional data from the previous, restarted session.
     * The data is sent as the 'restart' attribute of the 'terminated' event.
     * The client should leave the data intact.
     */
    pub __restart: Option<Value>,
}

pub enum Disconnect {}

impl Request for Disconnect {
    type Params = DisconnectArguments;
    type Result = ();
    const COMMAND: &'static str = "disconnect";
}

/// Arguments for ‘disconnect’ request.
#[derive(Serialize, Deserialize, Debug)]
pub struct DisconnectArguments {
    /**
     * A value of true indicates that this 'disconnect' request is part of a restart sequence.
     */
    pub restart: Option<bool>,

    /**
     * Indicates whether the debuggee should be terminated when the debugger is disconnected.
     * If unspecified, the debug adapter is free to do whatever it thinks is best.
     * A client can only rely on this attribute being properly honored if a debug adapter returns true for the 'supportTerminateDebuggee' capability.
     */
    pub terminateDebuggee: Option<bool>,
}

pub enum SetBreakpoints {}

impl Request for SetBreakpoints {
    type Params = SetBreakpointsArguments;
    type Result = SetBreakpointsResponse;
    const COMMAND: &'static str = "setBreakpoints";
}

/// Arguments for ‘setBreakpoints’ request.
#[derive(Deserialize, Debug)]
pub struct SetBreakpointsArguments {
    /**
     * The source location of the breakpoints; either 'source.path' or 'source.reference' must be specified.
     */
    pub source: Source,

    /**
     * The code locations of the breakpoints.
     */
    pub breakpoints: Option<Vec<SourceBreakpoint>>,

    /**
     * Deprecated: The code locations of the breakpoints.
     */
    pub lines: Option<i64>,

    /**
     * A value of true indicates that the underlying source has been modified which results in new breakpoint locations.
     */
    pub sourceModified: Option<bool>,
}

/// Response to ‘setBreakpoints’ request.
///
/// Returned is information about each breakpoint created by this request.
/// This includes the actual code location and whether the breakpoint could be verified.
/// The breakpoints returned are in the same order as the elements of the ‘breakpoints’
/// (or the deprecated ‘lines’) array in the arguments.
#[derive(Serialize, Debug)]
pub struct SetBreakpointsResponse {
    /**
     * Information about the breakpoints. The array elements are in the same order as the elements of the 'breakpoints' (or the deprecated 'lines') array in the arguments.
     */
    pub breakpoints: Vec<Breakpoint>,
}

// ----------------------------------------------------------------------------
// Types

/// Information about a Breakpoint created in setBreakpoints or setFunctionBreakpoints.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct Breakpoint {
    /**
     * An optional identifier for the breakpoint. It is needed if breakpoint events are used to update or remove breakpoints.
     */
    pub id: Option<i64>,

    /**
     * If true breakpoint could be set (but not necessarily at the desired location).
     */
    pub verified: bool,

    /**
     * An optional message about the state of the breakpoint. This is shown to the user and can be used to explain why a breakpoint could not be verified.
     */
    pub message: Option<String>,

    /**
     * The source where the breakpoint is located.
     */
    pub source: Option<Source>,

    /**
     * The start line of the actual range covered by the breakpoint.
     */
    pub line: Option<i64>,

    /**
     * An optional start column of the actual range covered by the breakpoint.
     */
    pub column: Option<i64>,

    /**
     * An optional end line of the actual range covered by the breakpoint.
     */
    pub endLine: Option<i64>,

    /**
     * An optional end column of the actual range covered by the breakpoint. If no end line is given, then the end column is assumed to be in the start line.
     */
    pub endColumn: Option<i64>,
}

/// Information about the capabilities of a debug adapter.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct Capabilities {
    /**
     * The debug adapter supports the 'configurationDone' request.
     */
    pub supportsConfigurationDoneRequest: Option<bool>,

    /**
     * The debug adapter supports function breakpoints.
     */
    pub supportsFunctionBreakpoints: Option<bool>,

    /**
     * The debug adapter supports conditional breakpoints.
     */
    pub supportsConditionalBreakpoints: Option<bool>,

    /**
     * The debug adapter supports breakpoints that break execution after a specified number of hits.
     */
    pub supportsHitConditionalBreakpoints: Option<bool>,

    /**
     * The debug adapter supports a (side effect free) evaluate request for data hovers.
     */
    pub supportsEvaluateForHovers: Option<bool>,

    /**
     * Available filters or options for the setExceptionBreakpoints request.
     */
    //exceptionBreakpointFilters?: ExceptionBreakpointsFilter[];

    /**
     * The debug adapter supports stepping back via the 'stepBack' and 'reverseContinue' requests.
     */
    pub supportsStepBack: Option<bool>,

    /**
     * The debug adapter supports setting a variable to a value.
     */
    pub supportsSetVariable: Option<bool>,

    /**
     * The debug adapter supports restarting a frame.
     */
    pub supportsRestartFrame: Option<bool>,

    /**
     * The debug adapter supports the 'gotoTargets' request.
     */
    pub supportsGotoTargetsRequest: Option<bool>,

    /**
     * The debug adapter supports the 'stepInTargets' request.
     */
    pub supportsStepInTargetsRequest: Option<bool>,

    /**
     * The debug adapter supports the 'completions' request.
     */
    pub supportsCompletionsRequest: Option<bool>,

    /**
     * The set of characters that should trigger completion in a REPL. If not specified, the UI should assume the '.' character.
     */
    pub completionTriggerCharacters: Option<Vec<String>>,

    /**
     * The debug adapter supports the 'modules' request.
     */
    pub supportsModulesRequest: Option<bool>,

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
    pub supportsRestartRequest: Option<bool>,

    /**
     * The debug adapter supports 'exceptionOptions' on the setExceptionBreakpoints request.
     */
    pub supportsExceptionOptions: Option<bool>,

    /**
     * The debug adapter supports a 'format' attribute on the stackTraceRequest, variablesRequest, and evaluateRequest.
     */
    pub supportsValueFormattingOptions: Option<bool>,

    /**
     * The debug adapter supports the 'exceptionInfo' request.
     */
    pub supportsExceptionInfoRequest: Option<bool>,

    /**
     * The debug adapter supports the 'terminateDebuggee' attribute on the 'disconnect' request.
     */
    pub supportTerminateDebuggee: Option<bool>,

    /**
     * The debug adapter supports the delayed loading of parts of the stack, which requires that both the 'startFrame' and 'levels' arguments and the 'totalFrames' result of the 'StackTrace' request are supported.
     */
    pub supportsDelayedStackTraceLoading: Option<bool>,

    /**
     * The debug adapter supports the 'loadedSources' request.
     */
    pub supportsLoadedSourcesRequest: Option<bool>,

    /**
     * The debug adapter supports logpoints by interpreting the 'logMessage' attribute of the SourceBreakpoint.
     */
    pub supportsLogPoints: Option<bool>,

    /**
     * The debug adapter supports the 'terminateThreads' request.
     */
    pub supportsTerminateThreadsRequest: Option<bool>,

    /**
     * The debug adapter supports the 'setExpression' request.
     */
    pub supportsSetExpression: Option<bool>,

    /**
     * The debug adapter supports the 'terminate' request.
     */
    pub supportsTerminateRequest: Option<bool>,

    /**
     * The debug adapter supports data breakpoints.
     */
    pub supportsDataBreakpoints: Option<bool>,

    /**
     * The debug adapter supports the 'readMemory' request.
     */
    pub supportsReadMemoryRequest: Option<bool>,

    /**
     * The debug adapter supports the 'disassemble' request.
     */
    pub supportsDisassembleRequest: Option<bool>,

    /**
     * The debug adapter supports the 'cancel' request.
     */
    pub supportsCancelRequest: Option<bool>,

    /**
     * The debug adapter supports the 'breakpointLocations' request.
     */
    pub supportsBreakpointLocationsRequest: Option<bool>,
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

/// A Source is a descriptor for source code.
///
/// It is returned from the debug adapter as part of a StackFrame and it is used by clients when specifying breakpoints.
#[derive(Serialize, Deserialize, Debug)]
pub struct Source {
    /**
     * The short name of the source. Every source returned from the debug adapter has a name. When sending a source to the debug adapter this name is optional.
     */
    pub name: Option<String>,

    /**
     * The path of the source to be shown in the UI. It is only used to locate and load the content of the source if no sourceReference is specified (or its value is 0).
     */
    pub path: Option<String>,

    /**
     * If sourceReference > 0 the contents of the source must be retrieved through the SourceRequest (even if a path is specified). A sourceReference is only valid for a session, so it must not be used to persist a source. The value should be less than or equal to 2147483647 (2^31 - 1).
     */
    pub sourceReference: Option<i64>,

    /**
     * An optional hint for how to present the source in the UI. A value of 'deemphasize' can be used to indicate that the source is not available or that it is skipped on stepping.
     */
    pub presentationHint: Option<PresentationHint>,

    /**
     * The (optional) origin of this source: possible values 'internal module', 'inlined content from source map', etc.
     */
    pub origin: Option<String>,

    /**
     * An optional list of sources that are related to this source. These may be the source that generated this source.
     */
    pub sources: Option<Vec<Source>>,

    /**
     * Optional data that a debug adapter might want to loop through the client. The client should leave the data intact and persist it across sessions. The client should not interpret the data.
     */
    pub adapterData: Option<Value>,

    /*/**
     * The checksums associated with this file.
     */
    checksums?: Checksum[]; */
}

#[derive(Serialize, Deserialize, Debug)]
pub enum PresentationHint {
    #[serde(rename="normal")]
    Normal,
    #[serde(rename="emphasize")]
    Emphasize,
    #[serde(rename="deemphasize")]
    Deemphasize,
}

/// Properties of a breakpoint or logpoint passed to the setBreakpoints request.
#[derive(Serialize, Deserialize, Debug)]
pub struct SourceBreakpoint {
    /**
     * The source line of the breakpoint or logpoint.
     */
    pub line: i64,

    /**
     * An optional source column of the breakpoint.
     */
    pub column: Option<i64>,

    /**
     * An optional expression for conditional breakpoints.
     */
    pub condition: Option<String>,

    /**
     * An optional expression that controls how many hits of the breakpoint are ignored. The backend is expected to interpret the expression as needed.
     */
    pub hitCondition: Option<String>,

    /**
     * If this attribute exists and is non-empty, the backend must not 'break' (stop) but log the message instead. Expressions within {} are interpolated.
     */
    pub logMessage: Option<String>,
}
