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

/// This event indicates that the debug adapter is ready to accept configuration requests (e.g. SetBreakpointsRequest, SetExceptionBreakpointsRequest).
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct InitializedEvent;

impl Event for InitializedEvent {
    const EVENT: &'static str = "initialized";
}

/// The event indicates that the execution of the debuggee has stopped due to some condition.
///
/// This can be caused by a break point previously set, a stepping action has completed, by executing a debugger statement etc.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct StoppedEvent {
    /**
     * The reason for the event.
     * For backward compatibility this string is shown in the UI if the 'description' attribute is missing (but it must not be translated).
     * Values: 'step', 'breakpoint', 'exception', 'pause', 'entry', 'goto', 'function breakpoint', 'data breakpoint', etc.
     */
    pub reason: String,

    /**
     * The full reason for the event, e.g. 'Paused on exception'. This string is shown in the UI as is and must be translated.
     */
    pub description: Option<String>,

    /**
     * The thread which was stopped.
     */
    pub threadId: Option<i64>,

    /**
     * A value of true hints to the frontend that this event should not change the focus.
     */
    pub preserveFocusHint: Option<bool>,

    /**
     * Additional information. E.g. if reason is 'exception', text contains the exception name. This string is shown in the UI.
     */
    pub text: Option<String>,

    /**
     * If 'allThreadsStopped' is true, a debug adapter can announce that all threads have stopped.
     * - The client should use this information to enable that all threads can be expanded to access their stacktraces.
     * - If the attribute is missing or false, only the thread with the given threadId can be expanded.
     */
    pub allThreadsStopped: Option<bool>,
}

impl Event for StoppedEvent {
    const EVENT: &'static str = "stopped";
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

/// The request returns the variable scopes for a given stackframe ID.
pub enum Scopes {}

impl Request for Scopes {
    type Params = ScopesArguments;
    type Result = ScopesResponse;
    const COMMAND: &'static str = "scopes";
}

/// Arguments for ‘scopes’ request.
#[derive(Serialize, Deserialize, Debug)]
pub struct ScopesArguments {
    /**
     * Retrieve the scopes for this stackframe.
     */
    pub frameId: i64,
}

/// Response to ‘scopes’ request.
#[derive(Serialize, Deserialize, Debug)]
pub struct ScopesResponse {
    /**
     * The scopes of the stackframe. If the array has length zero, there are no scopes available.
     */
    pub scopes: Vec<Scope>,
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
    pub lines: Option<Vec<i64>>,

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

/// The request returns a stacktrace from the current execution state.
pub enum StackTrace {}

impl Request for StackTrace {
    type Params = StackTraceArguments;
    type Result = StackTraceResponse;
    const COMMAND: &'static str = "stackTrace";
}

#[derive(Deserialize, Debug)]
pub struct StackTraceArguments {
    /**
     * Retrieve the stacktrace for this thread.
     */
    pub threadId: i64,

    /**
     * The index of the first frame to return; if omitted frames start at 0.
     */
    pub startFrame: Option<i64>,

    /**
     * The maximum number of frames to return. If levels is not specified or 0, all frames are returned.
     */
    pub levels: Option<i64>,

    /**
     * Specifies details on how to format the stack frames.
     */
    pub format: Option<StackFrameFormat>,
}

#[derive(Serialize, Debug)]
pub struct StackTraceResponse {
    /**
     * The frames of the stackframe. If the array has length zero, there are no stackframes available.
     * This means that there is no location information available.
     */
    pub stackFrames: Vec<StackFrame>,

    /**
     * The total number of frames available.
     */
    pub totalFrames: Option<i64>,
}

/// The request retrieves a list of all threads.
pub enum Threads {}

impl Request for Threads {
    type Params = ();
    type Result = ThreadsResponse;
    const COMMAND: &'static str = "threads";
}

/// Response to ‘threads’ request.
#[derive(Serialize, Debug)]
pub struct ThreadsResponse {
    /**
     * All threads.
     */
    pub threads: Vec<Thread>,
}

/// Retrieves all child variables for the given variable reference.
///
/// An optional filter can be used to limit the fetched children to either named or indexed children.
pub enum Variables {}

impl Request for Variables {
    type Params = VariablesArguments;
    type Result = VariablesResponse;
    const COMMAND: &'static str = "variables";
}

/// Arguments for ‘variables’ request.
#[derive(Serialize, Deserialize, Debug)]
pub struct VariablesArguments {
    /**
     * The Variable reference.
     */
    pub variablesReference: i64,

    /**
     * Optional filter to limit the child variables to either named or indexed. If omitted, both types are fetched.
     */
    pub filter: Option<VariablesFilter>,

    /**
     * The index of the first variable to return; if omitted children start at 0.
     */
    pub start: Option<i64>,

    /**
     * The number of variables to return. If count is missing or 0, all variables are returned.
     */
    pub count: Option<i64>,

    /**
     * Specifies details on how to format the Variable values.
     */
    pub format: Option<ValueFormat>,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum VariablesFilter {
    #[serde(rename="indexed")]
    Indexed,
    #[serde(rename="named")]
    Named,
}

/// Response to ‘variables’ request.
#[derive(Serialize, Deserialize, Debug)]
pub struct VariablesResponse {
    /**
     * All (or a range) of variables for the given variable reference.
     */
    pub variables: Vec<Variable>,
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

/// A Scope is a named container for variables. Optionally a scope can map to a source or a range within a source.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct Scope {
    /**
     * Name of the scope such as 'Arguments', 'Locals', or 'Registers'. This string is shown in the UI as is and can be translated.
     */
    pub name: String,

    /**
     * An optional hint for how to present this scope in the UI. If this attribute is missing, the scope is shown with a generic UI.
     * Values:
     * 'arguments': Scope contains method arguments.
     * 'locals': Scope contains local variables.
     * 'registers': Scope contains registers. Only a single 'registers' scope should be returned from a 'scopes' request.
     * etc.
     */
    pub presentationHint: Option<String>,

    /**
     * The variables of this scope can be retrieved by passing the value of variablesReference to the VariablesRequest.
     */
    pub variablesReference: i64,

    /**
     * The number of named variables in this scope.
     * The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
     */
    pub namedVariables: Option<i64>,

    /**
     * The number of indexed variables in this scope.
     * The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
     */
    pub indexedVariables: Option<i64>,

    /**
     * If true, the number of variables in this scope is large or expensive to retrieve.
     */
    pub expensive: bool,

    /**
     * Optional source for this scope.
     */
    pub source: Option<Source>,

    /**
     * Optional start line of the range covered by this scope.
     */
    pub line: Option<i64>,

    /**
     * Optional start column of the range covered by this scope.
     */
    pub column: Option<i64>,

    /**
     * Optional end line of the range covered by this scope.
     */
    pub endLine: Option<i64>,

    /**
     * Optional end column of the range covered by this scope.
     */
    pub endColumn: Option<i64>,
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
    pub presentationHint: Option<SourcePresentationHint>,

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
pub enum SourcePresentationHint {
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

/// A Stackframe contains the source location.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct StackFrame {
    /**
     * An identifier for the stack frame. It must be unique across all threads. This id can be used to retrieve the scopes of the frame with the 'scopesRequest' or to restart the execution of a stackframe.
     */
    pub id: i64,

    /**
     * The name of the stack frame, typically a method name.
     */
    pub name: String,

    /**
     * The optional source of the frame.
     */
    pub source: Option<Source>,

    /**
     * The line within the file of the frame. If source is null or doesn't exist, line is 0 and must be ignored.
     */
    pub line: i64,

    /**
     * The column within the line. If source is null or doesn't exist, column is 0 and must be ignored.
     */
    pub column: i64,

    /**
     * An optional end line of the range covered by the stack frame.
     */
    pub endLine: Option<i64>,

    /**
     * An optional end column of the range covered by the stack frame.
     */
    pub endColumn: Option<i64>,

    /**
     * Optional memory reference for the current instruction pointer in this frame.
     */
    pub instructionPointerReference: Option<String>,

    /*/**
     * The module associated with this frame, if any.
     */
    moduleId?: number | string;*/

    /**
     * An optional hint for how to present this frame in the UI. A value of 'label' can be used to indicate that the frame is an artificial frame that is used as a visual label or separator. A value of 'subtle' can be used to change the appearance of a frame in a 'subtle' way.
     */
    pub presentationHint: Option<StackFramePresentationHint>,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum StackFramePresentationHint {
    #[serde(rename="normal")]
    Normal,
    #[serde(rename="label")]
    Label,
    #[serde(rename="subtle")]
    Subtle,
}

// Provides formatting information for a stack frame.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct StackFrameFormat {
    /**
     * Display the value in hex.
     */
    pub hex: Option<bool>,

    /**
     * Displays parameters for the stack frame.
     */
    pub parameters: Option<bool>,

    /**
     * Displays the types of parameters for the stack frame.
     */
    pub parameterTypes: Option<bool>,

    /**
     * Displays the names of parameters for the stack frame.
     */
    pub parameterNames: Option<bool>,

    /**
     * Displays the values of parameters for the stack frame.
     */
    pub parameterValues: Option<bool>,

    /**
     * Displays the line number of the stack frame.
     */
    pub line: Option<bool>,

    /**
     * Displays the module of the stack frame.
     */
    pub module: Option<bool>,

    /**
     * Includes all stack frames, including those the debug adapter might otherwise hide.
     */
    pub includeAll: Option<bool>,
}

/// A Thread
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct Thread {
    /**
     * Unique identifier for the thread.
     */
    pub id: i64,

    /**
     * A name of the thread.
     */
    pub name: String,
}

/// Provides formatting information for a value.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct ValueFormat {
    /**
     * Display the value in hex.
     */
    pub hex: Option<bool>,
}

/// A Variable is a name/value pair.
///
/// Optionally a variable can have a ‘type’ that is shown if space permits or
/// when hovering over the variable’s name.
///
/// An optional ‘kind’ is used to render additional properties of the variable,
/// e.g. different icons can be used to indicate that a variable is public or
/// private.
///
/// If the value is structured (has children), a handle is provided to retrieve
/// the children with the VariablesRequest.
///
/// If the number of named or indexed children is large, the numbers should be
/// returned via the optional ‘namedVariables’ and ‘indexedVariables’
/// attributes.
///
/// The client can use this optional information to present the children in a
/// paged UI and fetch them in chunks.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct Variable {
    /**
     * The variable's name.
     */
    pub name: String,

    /**
     * The variable's value. This can be a multi-line text, e.g. for a function the body of a function.
     */
    pub value: String,

    /**
     * The type of the variable's value. Typically shown in the UI when hovering over the value.
     */
    #[serde(rename="type")]
    pub type_: Option<String>,

    /**
     * Properties of a variable that can be used to determine how to render the variable in the UI.
     */
    pub presentationHint: Option<VariablePresentationHint>,

    /**
     * Optional evaluatable name of this variable which can be passed to the 'EvaluateRequest' to fetch the variable's value.
     */
    pub evaluateName: Option<String>,

    /**
     * If variablesReference is > 0, the variable is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
     */
    pub variablesReference: i64,

    /**
     * The number of named child variables.
     * The client can use this optional information to present the children in a paged UI and fetch them in chunks.
     */
    pub namedVariables: Option<i64>,

    /**
     * The number of indexed child variables.
     * The client can use this optional information to present the children in a paged UI and fetch them in chunks.
     */
    pub indexedVariables: Option<i64>,

    /**
     * Optional memory reference for the variable if the variable represents executable code, such as a function pointer.
     */
    pub memoryReference: Option<String>,
}

/// Optional properties of a variable that can be used to determine how to
/// render the variable in the UI.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct VariablePresentationHint {
    /**
     * The kind of variable. Before introducing additional values, try to use the listed values.
     * Values:
     * 'property': Indicates that the object is a property.
     * 'method': Indicates that the object is a method.
     * 'class': Indicates that the object is a class.
     * 'data': Indicates that the object is data.
     * 'event': Indicates that the object is an event.
     * 'baseClass': Indicates that the object is a base class.
     * 'innerClass': Indicates that the object is an inner class.
     * 'interface': Indicates that the object is an interface.
     * 'mostDerivedClass': Indicates that the object is the most derived class.
     * 'virtual': Indicates that the object is virtual, that means it is a synthetic object introduced by the adapter for rendering purposes, e.g. an index range for large arrays.
     * 'dataBreakpoint': Indicates that a data breakpoint is registered for the object.
     * etc.
     */
    pub kind: Option<String>,

    /**
     * Set of attributes represented as an array of strings. Before introducing additional values, try to use the listed values.
     * Values:
     * 'static': Indicates that the object is static.
     * 'constant': Indicates that the object is a constant.
     * 'readOnly': Indicates that the object is read only.
     * 'rawString': Indicates that the object is a raw string.
     * 'hasObjectId': Indicates that the object can have an Object ID created for it.
     * 'canHaveObjectId': Indicates that the object has an Object ID associated with it.
     * 'hasSideEffects': Indicates that the evaluation had side effects.
     * etc.
     */
    pub attributes: Option<Vec<String>>,

    /**
     * Visibility of variable. Before introducing additional values, try to use the listed values.
     * Values: 'public', 'private', 'protected', 'internal', 'final', etc.
     */
    pub visibility: Option<String>,
}
