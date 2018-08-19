use serde_json::Value;
use std::fmt;

crate const ERROR: usize = 1;

// Request
#[derive(Clone, Debug, Deserialize)]
crate struct Request {
    crate id: Option<usize>,
    crate method: String,
    crate params: Value
}

// Request params
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
crate struct DidOpen {
    crate text_document: TextDocument
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
crate struct DidChange {
    crate content_changes: Vec<Change>,
    crate text_document: TextDocument
}
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
crate struct Definition {
    crate position: Position,
    crate text_document: TextDocument
}

// General objects
#[derive(Clone, Debug, Deserialize)]
crate struct TextDocument {
    crate text: Option<String>,
    crate uri: String
}
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
crate struct Position {
    crate line: usize,
    crate character: usize
}
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
crate struct Range {
    crate start: Position,
    crate end: Position
}
#[derive(Clone, Debug, Deserialize, Serialize)]
crate struct Location {
    crate uri: String,
    crate range: Range
}
#[derive(Clone, Debug, Deserialize)]
crate struct Change {
    crate text: String
}
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
crate struct TextEdit {
    crate range: Range,
    crate new_text: String
}

// Response
#[derive(Clone, Debug, Serialize)]
crate struct Response<T> {
    crate id: Option<usize>,
    crate result: Option<T>,
    crate error: Option<ResponseError>
}
#[derive(Clone, Debug, Serialize)]
crate struct ResponseError {
    crate code: i32,
    crate message: String
}
impl<T> Response<T> {
    crate fn success(id: Option<usize>, result: T) -> Self {
        Response {
            id,
            result: Some(result),
            error: None
        }
    }
}
impl Response<()> {
    crate fn empty(id: Option<usize>) -> Self {
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

    crate fn error<E: fmt::Display>(id: Option<usize>, error: E) -> Self {
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
crate struct Notification<T> {
    crate method: String,
    crate params: T
}

// Response types
#[derive(Clone, Debug, Serialize)]
crate struct InitializeResult {
    crate capabilities: ServerCapabilities
}
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
crate struct ServerCapabilities {
    crate definition_provider: bool,
    crate completion_provider: CompletionOptions
}
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
crate struct CompletionOptions {
    crate resolve_provider: bool
}
#[derive(Clone, Debug, Serialize)]
crate struct DiagnosticParams {
    crate uri: String,
    crate diagnostics: Vec<Diagnostic>
}
#[derive(Clone, Debug, Serialize)]
crate struct Diagnostic {
    crate range: Range,
    crate severity: usize,
    crate message: String
}
#[derive(Clone, Debug, Serialize)]
crate struct CompletionItem {
    crate label: String,
    crate edit: TextEdit
}
