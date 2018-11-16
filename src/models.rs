use serde_json::Value;
use std::{
    collections::BTreeMap,
    fmt
};

pub const ERROR: usize = 1;

// Request
#[derive(Clone, Debug, Deserialize)]
pub struct Request {
    pub id: Option<usize>,
    pub method: String,
    pub params: Value
}

// Request params
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DidOpen {
    pub text_document: TextDocument
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DidChange {
    pub content_changes: Vec<Change>,
    pub text_document: TextDocument
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Definition {
    pub position: Position,
    pub text_document: TextDocument
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Formatting {
    pub text_document: TextDocument
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RenameParams {
    pub text_document: TextDocument,
    pub position: Position,
    pub new_name: String
}
#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ExtendSelectionParams {
    pub text_document: TextDocument,
    pub selections: Vec<Range>,
}

// General objects
#[derive(Clone, Debug, Deserialize)]
pub struct TextDocument {
    pub text: Option<String>,
    pub uri: String
}
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct Position {
    pub line: usize,
    pub character: usize
}
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct Range {
    pub start: Position,
    pub end: Position
}
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Location {
    pub uri: String,
    pub range: Range
}
#[derive(Clone, Debug, Deserialize)]
pub struct Change {
    pub text: String
}
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TextEdit {
    pub range: Range,
    pub new_text: String
}

// Response
#[derive(Clone, Debug, Serialize)]
pub struct Response<T> {
    pub id: Option<usize>,
    pub result: Option<T>,
    pub error: Option<ResponseError>
}
#[derive(Clone, Debug, Serialize)]
pub struct ResponseError {
    pub code: i32,
    pub message: String
}
impl<T> Response<T> {
    pub fn success(id: Option<usize>, result: T) -> Self {
        Response {
            id,
            result: Some(result),
            error: None
        }
    }
}
impl Response<()> {
    pub fn empty(id: Option<usize>) -> Self {
        Response {
            id,
            result: None,
            error: None
        }
    }

    // To be honest, I don't really care about letting the client know what
    // went wrong. A proper production client shouldn't send invalid JSON for
    // example, so most errors are out of the question. And if a human is using
    // this to debug their client (which, they probably won't), they can just
    // use the nice string error message.
    const UNKNOWN_ERROR_CODE: i32 = -32001;

    pub fn error<E: fmt::Display>(id: Option<usize>, error: E) -> Self {
        Response {
            id,
            result: None,
            error: Some(ResponseError {
                code: Self::UNKNOWN_ERROR_CODE,
                message: error.to_string()
            })
        }
    }
}
#[derive(Clone, Debug, Serialize)]
pub struct Notification<T> {
    pub method: String,
    pub params: T
}

// Response types
#[derive(Clone, Debug, Serialize)]
pub struct InitializeResult {
    pub capabilities: ServerCapabilities
}
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ServerCapabilities {
    pub text_document_sync: TextDocumentSyncOptions,
    pub completion_provider: CompletionOptions,
    pub definition_provider: bool,
    pub document_formatting_provider: bool
}
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TextDocumentSyncOptions {
    pub open_close: bool,
    pub change: u32,
}
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionOptions {
    pub resolve_provider: bool
}
#[derive(Clone, Debug, Serialize)]
pub struct DiagnosticParams {
    pub uri: String,
    pub diagnostics: Vec<Diagnostic>
}
#[derive(Clone, Debug, Serialize)]
pub struct Diagnostic {
    pub range: Range,
    pub severity: usize,
    pub message: String
}
#[derive(Clone, Debug, Serialize)]
pub struct CompletionItem {
    pub label: String,
    pub edit: TextEdit
}
#[derive(Clone, Debug, Serialize)]
pub struct WorkspaceEdit {
    pub changes: BTreeMap<String, Vec<TextEdit>>
}
