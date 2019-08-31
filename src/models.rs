use lsp_types::{Range, TextDocumentItem};

// Custom params
#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ExtendSelectionParams {
    pub text_document: TextDocumentItem,
    pub selections: Vec<Range>,
}
