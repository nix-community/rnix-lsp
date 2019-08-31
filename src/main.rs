#[macro_use]
extern crate serde;

mod lookup;
mod models;
mod utils;

use models::ExtendSelectionParams;

use log::{error, trace, warn};
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    *,
    request::{*, Request as _}
};
use rnix::{parser::*, types::*, SyntaxNode};
use std::{
    collections::HashMap,
    panic,
    rc::Rc,
};

pub type Error = Box<dyn std::error::Error>;

pub const DUMMY_ERROR: &str = "A fatal error has occured and nix-lsp has shut down.";

fn main() -> Result<(), &'static str> {
    env_logger::init();
    panic::set_hook(Box::new(move |panic| {
        error!("----- Panic -----");
        error!("{}", panic);
    }));

    let (connection, io_threads) = Connection::stdio();
    let capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::Full),
                ..Default::default()
            }
        )),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(true),
            ..Default::default()
        }),
        definition_provider: Some(true),
        document_formatting_provider: Some(true),
        rename_provider: Some(RenameProviderCapability::Simple(true)),
        ..Default::default()
    }).unwrap();

    if let Err(err) = connection.initialize(capabilities) {
        error!("{:?}", err);
        return Err(DUMMY_ERROR);
    }

    App {
        files: HashMap::new(),
        conn: connection,
    }.main();

    if let Err(err) = io_threads.join() {
        error!("Unable to shut down I/O threads: {}", err);
        return Err(DUMMY_ERROR);
    }

    Ok(())
}

struct App {
    files: HashMap<Url, (AST, String)>,
    conn: Connection,
}
impl App {
    fn reply(&mut self, response: Response) {
        trace!("Sending response: {:#?}", response);
        self.conn.sender.send(Message::Response(response)).unwrap();
    }
    fn notify(&mut self, notification: Notification) {
        trace!("Sending notification: {:#?}", notification);
        self.conn.sender.send(Message::Notification(notification)).unwrap();
    }
    fn err<E>(&mut self, id: RequestId, err: E)
        where E: std::fmt::Display
    {
        warn!("{}", err);
        self.reply(Response::new_err(id, ErrorCode::UnknownErrorCode as i32, err.to_string()));
    }
    fn main(&mut self) {
        while let Ok(msg) = self.conn.receiver.recv() {
            trace!("Message: {:#?}", msg);
            match msg {
                Message::Request(req) => {
                    let id = req.id.clone();
                    match self.conn.handle_shutdown(&req) {
                        Ok(true) => break,
                        Ok(false) => if let Err(err) = self.handle_request(req) {
                            self.err(id, err);
                        },
                        Err(err) => self.err(id, err),
                    }
                },
                Message::Notification(notification) => {
                    let _ = self.handle_notification(notification);
                },
                Message::Response(_) => (),
            }
        }
    }
    fn handle_request(&mut self, req: Request) -> Result<(), Error> {
        match &*req.method {
            GotoDefinition::METHOD => {
                let params: TextDocumentPositionParams = serde_json::from_value(req.params)?;
                if let Some(pos) = self.lookup_definition(params) {
                    self.reply(Response::new_ok(req.id, pos));
                } else {
                    self.reply(Response::new_ok(req.id, ()));
                }
            },
            Completion::METHOD => {
                let params: TextDocumentPositionParams = serde_json::from_value(req.params)?;
                let completions = self.completions(params).unwrap_or_default();
                self.reply(Response::new_ok(req.id, completions));
            },
            Rename::METHOD => {
                let params: RenameParams = serde_json::from_value(req.params)?;
                let changes = self.rename(params);
                self.reply(Response::new_ok(req.id, WorkspaceEdit {
                    changes,
                    ..Default::default()
                }));
            },
            Formatting::METHOD => {
                let params: DocumentFormattingParams = serde_json::from_value(req.params)?;

                let changes = if let Some((ast, code)) = self.files.get(&params.text_document.uri) {
                    let fmt = nixpkgs_fmt::reformat_node(&ast.node());
                    fmt.text_diff().iter()
                        .filter(|range| !range.delete.is_empty() || !range.insert.is_empty())
                        .map(|edit| TextEdit {
                            range: utils::range(&code, edit.delete),
                            new_text: edit.insert.to_string()
                        })
                        .collect()
                } else {
                    Vec::new()
                };
                self.reply(Response::new_ok(req.id, changes));
            },
            // LSP does not have extend-selection built-in, so we namespace it
            // under nix-lsp, as a custom protocol extension.
            //
            // Extend selection takes a document and a number of ranges in the
            // doc. It returns a vector of "extended" ranges, where extended
            // means "encompassing syntax node".
            "nix-lsp/extendSelection" => {
                let params: ExtendSelectionParams = serde_json::from_value(req.params)?;
                let mut selections = Vec::new();
                if let Some((ast, code)) = self.files.get(&params.text_document.uri) {
                    for sel in params.selections {
                        let mut extended = sel;
                        if let Some(range) = utils::lookup_range(code, sel) {
                            let extended_range = utils::extend(&ast.node(), range);
                            extended = utils::range(code, extended_range);
                        }
                        selections.push(extended);
                    }
                }
                self.reply(Response::new_ok(req.id, selections));
            },
            _ => (),
        }
        Ok(())
    }
    fn handle_notification(&mut self, req: Notification) -> Result<(), Error> {
        match &*req.method {
            "textDocument/didOpen" => {
                let params: DidOpenTextDocumentParams = serde_json::from_value(req.params)?;
                let text = params.text_document.text;
                let parsed = rnix::parse(&text);
                self.send_diagnostics(params.text_document.uri.clone(), &text, &parsed)?;
                self.files.insert(params.text_document.uri, (parsed, text));
            },
            "textDocument/didChange" => {
                let params: DidChangeTextDocumentParams = serde_json::from_value(req.params)?;
                if let Some(change) = params.content_changes.into_iter().last() {
                    let parsed = rnix::parse(&change.text);
                    self.send_diagnostics(params.text_document.uri.clone(), &change.text, &parsed)?;
                    self.files.insert(params.text_document.uri, (parsed, change.text));
                }
            },
            _ => (),
        }
        Ok(())
    }
    fn lookup_definition(&mut self, params: TextDocumentPositionParams) -> Option<Location> {
        let (ast, code) = self.files.get(&params.text_document.uri)?;
        let offset = utils::lookup_pos(code, params.position)?;
        let node = ast.node().to_owned();
        let (name, scope) = self.scope_for_ident(params.text_document.uri, node, offset)?;

        let var = scope.get(name.as_str())?;
        let (_ast, code) = self.files.get(&var.file)?;
        Some(Location {
            uri: (*var.file).clone(),
            range: utils::range(code, var.key.text_range())
        })
    }
    fn completions(&mut self, params: TextDocumentPositionParams) -> Option<Vec<CompletionItem>> {
        let (ast, code) = self.files.get(&params.text_document.uri)?;
        let offset = utils::lookup_pos(code, params.position)?;

        let node = ast.node().to_owned();
        let (name, scope) = self.scope_for_ident(params.text_document.uri.clone(), node, offset)?;

        // Re-open, because scope_for_ident may mutably borrow
        let (_ast, code) = self.files.get(&params.text_document.uri)?;

        let mut completions = Vec::new();
        for var in scope.keys() {
            if var.starts_with(&name.as_str()) {
                completions.push(CompletionItem {
                    label: var.clone(),
                    text_edit: Some(TextEdit {
                        range: utils::range(code, name.node().text_range()),
                        new_text: var.clone()
                    }),
                    ..Default::default()
                });
            }
        }
        Some(completions)
    }
    fn rename(&mut self, params: RenameParams) -> Option<HashMap<Url, Vec<TextEdit>>> {
        let uri = params.text_document_position.text_document.uri;
        let (ast, code) = self.files.get(&uri)?;
        let offset = utils::lookup_pos(code, params.text_document_position.position)?;
        let info = utils::ident_at(ast.node(), offset)?;
        if !info.path.is_empty() {
            // Renaming within a set not supported
            return None;
        }
        let old = info.ident;
        let scope = utils::scope_for(&Rc::new(uri.clone()), old.node().clone())?;

        struct Rename<'a> {
            edits: Vec<TextEdit>,
            code: &'a str,
            old: &'a str,
            new_name: String,
        }
        fn rename_in_node(rename: &mut Rename, node: SyntaxNode) -> Option<()> {
            if let Some(ident) = Ident::cast(node.clone()) {
                if ident.as_str() == rename.old {
                    rename.edits.push(TextEdit {
                        range: utils::range(rename.code, node.text_range()),
                        new_text: rename.new_name.clone()
                    });
                }
            } else if let Some(index) = IndexSet::cast(node.clone()) {
                rename_in_node(rename, index.set()?);
            } else if let Some(attr) = Attribute::cast(node.clone()) {
                let mut path = attr.path();
                if let Some(ident) = path.next() {
                    rename_in_node(rename, ident);
                }
            } else {
                for child in node.children() {
                    rename_in_node(rename, child);
                }
            }
            Some(())
        }

        let mut rename = Rename {
            edits: Vec::new(),
            code,
            old: old.as_str(),
            new_name: params.new_name
        };
        let definition = scope.get(old.as_str())?;
        rename_in_node(&mut rename, definition.set.clone());

        let mut changes = HashMap::new();
        changes.insert(uri, rename.edits);
        Some(changes)
    }
    fn send_diagnostics(&mut self, uri: Url, code: &str, ast: &AST) -> Result<(), Error> {
        let errors = ast.errors();
        let mut diagnostics = Vec::with_capacity(errors.len());
        for err in errors {
            if let ParseError::Unexpected(node) = err {
                diagnostics.push(Diagnostic {
                    range: utils::range(code, node),
                    severity: Some(DiagnosticSeverity::Error),
                    message: err.to_string(),
                    ..Default::default()
                });
            }
        }
        self.notify(Notification::new(
            "textDocument/publishDiagnostics".into(),
            PublishDiagnosticsParams {
                uri,
                diagnostics
            }
        ));
        Ok(())
    }
}
