mod lookup;
mod utils;

use log::{error, trace, warn};
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    *,
    notification::{*, Notification as _},
    request::{*, Request as RequestTrait},
};
use rnix::{parser::*, types::*, SyntaxNode, TextUnit};
use std::{
    collections::HashMap,
    panic,
    process,
    rc::Rc,
};

type Error = Box<dyn std::error::Error>;

fn main() {
    if let Err(err) = real_main() {
        error!("Error: {} ({:?})", err, err);
        error!("A fatal error has occured and rnix-lsp will shut down.");
        drop(err);
        process::exit(libc::EXIT_FAILURE);
    }
}
fn real_main() -> Result<(), Error> {
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
            ..Default::default()
        }),
        definition_provider: Some(true),
        document_formatting_provider: Some(true),
        rename_provider: Some(RenameProviderCapability::Simple(true)),
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
        ..Default::default()
    }).unwrap();

    connection.initialize(capabilities)?;

    App {
        files: HashMap::new(),
        conn: connection,
    }.main();

    io_threads.join()?;

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
                        Err(err) => {
                            // This only fails if a shutdown was
                            // requested in the first place, so it
                            // should definitely break out of the
                            // loop.
                            self.err(id, err);
                            break;
                        },
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
        fn cast<Kind>(req: &mut Option<Request>) -> Option<(RequestId, Kind::Params)>
        where
            Kind: RequestTrait,
            Kind::Params: serde::de::DeserializeOwned,
        {
            match req.take().unwrap().extract::<Kind::Params>(Kind::METHOD) {
                Ok(value) => Some(value),
                Err(owned) => {
                    *req = Some(owned);
                    None
                },
            }
        }
        let mut req = Some(req);
        if let Some((id, params)) = cast::<GotoDefinition>(&mut req) {
            if let Some(pos) = self.lookup_definition(params) {
                self.reply(Response::new_ok(id, pos));
            } else {
                self.reply(Response::new_ok(id, ()));
            }
        } else if let Some((id, params)) = cast::<Completion>(&mut req) {
            let completions = self.completions(params.text_document_position).unwrap_or_default();
            self.reply(Response::new_ok(id, completions));
        } else if let Some((id, params)) = cast::<Rename>(&mut req) {
            let changes = self.rename(params);
            self.reply(Response::new_ok(id, WorkspaceEdit {
                changes,
                ..Default::default()
            }));
        } else if let Some((id, params)) = cast::<Formatting>(&mut req) {
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
            self.reply(Response::new_ok(id, changes));
        } else if let Some((id, params)) = cast::<SelectionRangeRequest>(&mut req) {
            let mut selections = Vec::new();
            if let Some((ast, code)) = self.files.get(&params.text_document.uri) {
                for pos in params.positions {
                    selections.push(self.selection_ranges(&ast.node(), code, pos));
                }
            }
            self.reply(Response::new_ok(id, selections));
        }
        Ok(())
    }
    fn handle_notification(&mut self, req: Notification) -> Result<(), Error> {
        match &*req.method {
            DidOpenTextDocument::METHOD => {
                let params: DidOpenTextDocumentParams = serde_json::from_value(req.params)?;
                let text = params.text_document.text;
                let parsed = rnix::parse(&text);
                self.send_diagnostics(params.text_document.uri.clone(), &text, &parsed)?;
                self.files.insert(params.text_document.uri, (parsed, text));
            },
            DidChangeTextDocument::METHOD => {
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
            } else if let Some(index) = Select::cast(node.clone()) {
                rename_in_node(rename, index.set()?);
            } else if let Some(attr) = Key::cast(node.clone()) {
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
    fn selection_ranges(&self, root: &SyntaxNode, code: &str, pos: Position) -> Option<SelectionRange> {
        let pos = utils::lookup_pos(code, pos)?;
        let node = root.token_at_offset(TextUnit::from_usize(pos)).left_biased()?;

        let mut root = None;
        let mut cursor = &mut root;

        let mut last = None;
        for parent in node.ancestors() {
            // De-duplicate
            if last.as_ref() == Some(&parent) {
                continue;
            }

            let range = parent.text_range();
            *cursor = Some(Box::new(SelectionRange {
                range: utils::range(code, range),
                parent: None,
            }));
            cursor = &mut cursor.as_mut().unwrap().parent;

            last = Some(parent);
        }

        root.map(|b| *b)
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
                diagnostics,
                version: None,
            }
        ));
        Ok(())
    }
}
