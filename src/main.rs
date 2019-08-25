#![feature(panic_info_message)]

#[macro_use] extern crate failure;
#[macro_use] extern crate serde_derive;

mod lookup;
mod models;
mod utils;

use self::models::*;

use failure::Error;
use lsp_types::*;
use rnix::{parser::*, types::*, SyntaxNode};
use std::{
    collections::HashMap,
    fmt,
    fs::File,
    io::{self, prelude::*},
    panic,
    rc::Rc
};

fn main() -> Result<(), Error> {
    let mut log = File::create("/tmp/nix-lsp.log")?;
    let log_clone = log.try_clone()?;

    let stdout = io::stdout();
    let mut app = App {
        files: HashMap::new(),
        log: &mut log,
        stdout: stdout.lock()
    };
    panic::set_hook(Box::new(move |panic| {
        writeln!(&log_clone, "----- Panic -----").unwrap();
        writeln!(&log_clone, "{}", panic).unwrap();
    }));
    if let Err(err) = app.main() {
        writeln!(log, "{:?}", err).unwrap();
        return Err(err);
    }

    Ok(())
}

struct App<'a, W: io::Write> {
    files: HashMap<Url, (AST, String)>,
    log: &'a mut File,
    stdout: W
}
impl<'a, W: io::Write> App<'a, W> {
    fn main(&mut self) -> Result<(), Error> {
        let stdin = io::stdin();
        let mut stdin = stdin.lock();

        loop {
            let mut length = None;
            let mut line = String::new();

            loop {
                line.clear();
                stdin.read_line(&mut line)?;

                let line = line.trim();

                let mut parts = line.split(':');
                match (parts.next(), parts.next()) {
                    (Some("Content-Length"), Some(x)) => length = Some(x.trim().parse()?),
                    _ => ()
                }

                if line.is_empty() {
                    break;
                }
            }

            let length = length.ok_or_else(|| format_err!("missing Content-Length in request"))?;

            let mut body = vec![0; length];
            stdin.read_exact(&mut body)?;

            writeln!(self.log, "Raw: {:?}", std::str::from_utf8(&body).unwrap_or_default())?;
            let req: Result<Request, _> = serde_json::from_slice(&body);
            writeln!(self.log, "{:#?}", req)?;

            let req = match req {
                Ok(req) => req,
                Err(err) => {
                    writeln!(self.log, "{:?}", err)?;
                    self.send(&Response::error(None, err))?;
                    continue;
                }
            };

            let id = req.id;
            if let Err(err) = self.handle_request(req) {
                writeln!(self.log, "{:?}", err)?;
                self.send(&Response::error(id, err))?;
            }
        }
    }
    fn send<T: serde::Serialize + fmt::Debug>(&mut self, msg: &T) -> Result<(), Error> {
        writeln!(self.log, "Sending: {:#?}", msg)?;
        let bytes = serde_json::to_vec(msg)?;
        write!(self.stdout, "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n")?;
        write!(self.stdout, "Content-Length: {}\r\n", bytes.len())?;
        write!(self.stdout, "\r\n")?;

        self.stdout.write_all(&bytes)?;
        self.stdout.flush()?;
        Ok(())
    }
    fn handle_request(&mut self, req: Request) -> Result<(), Error> {
        match &*req.method {
            "initialize" => self.send(&Response::success(req.id, Some(
                InitializeResult {
                    capabilities: ServerCapabilities {
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
                    }
                }
            )))?,
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
            "textDocument/definition" => {
                let params: TextDocumentPositionParams = serde_json::from_value(req.params)?;
                if let Some(pos) = self.lookup_definition(params) {
                    self.send(&Response::success(req.id, pos))?;
                } else {
                    self.send(&Response::empty(req.id))?;
                }
            },
            "textDocument/completion" => {
                let params: TextDocumentPositionParams = serde_json::from_value(req.params)?;
                let completions = self.completions(params).unwrap_or_default();
                self.send(&Response::success(req.id, completions))?;
            },
            "textDocument/rename" => {
                let params: RenameParams = serde_json::from_value(req.params)?;
                let changes = self.rename(params);
                self.send(&Response::success(req.id, WorkspaceEdit {
                    changes,
                    ..Default::default()
                }))?;
            }
            "textDocument/formatting" => {
                let params: DocumentFormattingParams = serde_json::from_value(req.params)?;

                let changes = if let Some((ast, code)) = self.files.get(&params.text_document.uri) {
                    let fmt = nixpkgs_fmt::reformat_node(&ast.node());
                    fmt.diff().iter()
                        .filter(|range| !range.delete.is_empty() || !range.insert.is_empty())
                        .map(|edit| TextEdit {
                            range: utils::range(&code, edit.delete),
                            new_text: edit.insert.to_string()
                        })
                        .collect()
                } else {
                    Vec::new()
                };
                self.send(&Response::success(req.id, changes))?;
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
                self.send(&Response::success(req.id, selections))?;
            }
            _ => ()
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
        self.send(&Notification {
            jsonrpc: "2.0",
            method: "textDocument/publishDiagnostics".into(),
            params: PublishDiagnosticsParams {
                uri,
                diagnostics
            }
        })
    }
}
