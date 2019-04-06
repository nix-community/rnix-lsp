use lsp_types::{Position, Range, TextDocumentItem};
use serde_json::Value;
use std::{
    collections::BTreeMap,
    fmt
};

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
pub struct DidChange {
    pub content_changes: Vec<Change>,
    pub text_document: TextDocumentItem
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Definition {
    pub position: Position,
    pub text_document: TextDocumentItem
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Formatting {
    pub text_document: TextDocumentItem
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RenameParams {
    pub text_document: TextDocumentItem,
    pub position: Position,
    pub new_name: String
}
#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ExtendSelectionParams {
    pub text_document: TextDocumentItem,
    pub selections: Vec<Range>,
}

// General objects
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
    pub jsonrpc: &'static str,
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
            jsonrpc: "2.0",
            id,
            result: Some(result),
            error: None
        }
    }
}
impl Response<()> {
    pub fn empty(id: Option<usize>) -> Self {
        Response {
            jsonrpc: "2.0",
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
            jsonrpc: "2.0",
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
    pub jsonrpc: &'static str,
    pub method: String,
    pub params: T
}

// Response types
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
