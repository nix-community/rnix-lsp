use serde_json::Value;
use std::{
    collections::BTreeMap,
    fmt
};

pub(crate) const ERROR: usize = 1;

// Request
#[derive(Clone, Debug, Deserialize)]
pub(crate) struct Request {
    pub(crate) id: Option<usize>,
    pub(crate) method: String,
    pub(crate) params: Value
}

// Request params
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct DidOpen {
    pub(crate) text_document: TextDocument
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct DidChange {
    pub(crate) content_changes: Vec<Change>,
    pub(crate) text_document: TextDocument
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Definition {
    pub(crate) position: Position,
    pub(crate) text_document: TextDocument
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Formatting {
    pub(crate) text_document: TextDocument
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct RenameParams {
    pub(crate) text_document: TextDocument,
    pub(crate) position: Position,
    pub(crate) new_name: String
}

// General objects
#[derive(Clone, Debug, Deserialize)]
pub(crate) struct TextDocument {
    pub(crate) text: Option<String>,
    pub(crate) uri: String
}
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub(crate) struct Position {
    pub(crate) line: usize,
    pub(crate) character: usize
}
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub(crate) struct Range {
    pub(crate) start: Position,
    pub(crate) end: Position
}
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct Location {
    pub(crate) uri: String,
    pub(crate) range: Range
}
#[derive(Clone, Debug, Deserialize)]
pub(crate) struct Change {
    pub(crate) text: String
}
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct TextEdit {
    pub(crate) range: Range,
    pub(crate) new_text: String
}

// Response
#[derive(Clone, Debug, Serialize)]
pub(crate) struct Response<T> {
    pub(crate) id: Option<usize>,
    pub(crate) result: Option<T>,
    pub(crate) error: Option<ResponseError>
}
#[derive(Clone, Debug, Serialize)]
pub(crate) struct ResponseError {
    pub(crate) code: i32,
    pub(crate) message: String
}
impl<T> Response<T> {
    pub(crate) fn success(id: Option<usize>, result: T) -> Self {
        Response {
            id,
            result: Some(result),
            error: None
        }
    }
}
impl Response<()> {
    pub(crate) fn empty(id: Option<usize>) -> Self {
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

    pub(crate) fn error<E: fmt::Display>(id: Option<usize>, error: E) -> Self {
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
pub(crate) struct Notification<T> {
    pub(crate) method: String,
    pub(crate) params: T
}

// Response types
#[derive(Clone, Debug, Serialize)]
pub(crate) struct InitializeResult {
    pub(crate) capabilities: ServerCapabilities
}
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ServerCapabilities {
    pub(crate) completion_provider: CompletionOptions,
    pub(crate) definition_provider: bool,
    pub(crate) document_formatting_provider: bool
}
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct CompletionOptions {
    pub(crate) resolve_provider: bool
}
#[derive(Clone, Debug, Serialize)]
pub(crate) struct DiagnosticParams {
    pub(crate) uri: String,
    pub(crate) diagnostics: Vec<Diagnostic>
}
#[derive(Clone, Debug, Serialize)]
pub(crate) struct Diagnostic {
    pub(crate) range: Range,
    pub(crate) severity: usize,
    pub(crate) message: String
}
#[derive(Clone, Debug, Serialize)]
pub(crate) struct CompletionItem {
    pub(crate) label: String,
    pub(crate) edit: TextEdit
}
#[derive(Clone, Debug, Serialize)]
pub(crate) struct WorkspaceEdit {
    pub(crate) changes: BTreeMap<String, Vec<TextEdit>>
}
