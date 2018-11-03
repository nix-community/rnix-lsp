#![feature(panic_info_message)]

#[macro_use] extern crate failure;
#[macro_use] extern crate serde_derive;

mod format;
mod models;
mod utils;

use self::models::*;

use failure::Error;
use rnix::{parser::{AST, ParseError}, types::*};
use rowan::WalkEvent;
use std::{
    collections::{BTreeMap, HashMap},
    fmt,
    fs::File,
    io::{self, prelude::*},
    panic
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
    files: HashMap<String, (AST, String)>,
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
                stdin.read_line(&mut line)?;

                let mut parts = line.split(':');
                match (parts.next().map(str::trim), parts.next().map(str::trim)) {
                    (Some("Content-Length"), Some(x)) => length = Some(x.parse()?),
                    _ => ()
                }

                if line.is_empty() || line.ends_with("\r\n\r\n") {
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
                    writeln!(self.log, "{:?}", err);
                    self.send(&Response::error(None, err))?;
                    continue;
                }
            };

            let id = req.id;
            if let Err(err) = self.handle_request(req) {
                writeln!(self.log, "{:?}", err);
                self.send(&Response::error(id, err))?;
            }
        }
    }
    fn send<T: serde::Serialize + fmt::Debug>(&mut self, msg: &T) -> Result<(), Error> {
        writeln!(self.log, "Sending: {:?}", msg)?;
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
                        completion_provider: CompletionOptions {
                            resolve_provider: true
                        },
                        definition_provider: true,
                        document_formatting_provider: true
                    }
                }
            )))?,
            "textDocument/didOpen" => {
                let params: DidOpen = serde_json::from_value(req.params)?;
                let text = params.text_document.text.ok_or_else(|| format_err!("missing text in request"))?;
                let parsed = rnix::parse(&text);
                self.send_diagnostics(params.text_document.uri.clone(), &text, &parsed)?;
                self.files.insert(params.text_document.uri, (parsed, text));
            },
            "textDocument/didChange" => {
                let params: DidChange = serde_json::from_value(req.params)?;
                if let Some(change) = params.content_changes.into_iter().last() {
                    let parsed = rnix::parse(&change.text);
                    self.send_diagnostics(params.text_document.uri.clone(), &change.text, &parsed)?;
                    self.files.insert(params.text_document.uri, (parsed, change.text));
                }
            },
            "textDocument/definition" => {
                let params: Definition = serde_json::from_value(req.params)?;
                if let Some((ast, code)) = self.files.get_mut(&params.text_document.uri) {
                    let offset = utils::lookup_pos(code, params.position)?;
                    let (name, _) = utils::ident_at(code, offset);
                    let (scopes, _) = utils::scope_for(ast.node().borrowed(), offset, name);

                    //writeln!(self.log, "LOOKUP DEFINITION {:?} {:?} {:?}", offset, name, scopes)?;

                    let response = if let Some(node) = scopes.var(name) {
                        Some(Location {
                            uri: params.text_document.uri,
                            range: utils::range(code, node.range())
                        })
                    } else {
                        None
                    };
                    self.send(&Response::success(req.id, response))?;
                } else {
                    self.send(&Response::empty(req.id))?;
                }
            },
            "textDocument/completion" => {
                let params: Definition = serde_json::from_value(req.params)?;
                let mut completions = Vec::new();

                if let Some((ast, code)) = self.files.get_mut(&params.text_document.uri) {
                    let offset = utils::lookup_pos(code, params.position)?;

                    let (name, range) = utils::ident_at(code, offset);
                    let range = utils::range(code, range);
                    let (scopes, _) = utils::scope_for(ast.node().borrowed(), offset, name);

                    for scope in scopes.0.into_iter().rev() {
                        for var in scope.keys() {
                            if var.starts_with(&name) {
                                completions.push(CompletionItem {
                                    label: var.clone(),
                                    edit: TextEdit {
                                        range,
                                        new_text: var.clone()
                                    }
                                });
                            }
                        }
                    }
                }

                self.send(&Response::success(req.id, completions))?;
            },
            "textDocument/rename" => {
                let params: RenameParams = serde_json::from_value(req.params)?;

                let mut edits = Vec::new();

                if let Some((ast, code)) = self.files.get_mut(&params.text_document.uri) {
                    let offset = utils::lookup_pos(code, params.position)?;
                    let (old_name, _) = utils::ident_at(code, offset);
                    let (_, node) = utils::scope_for(ast.node().borrowed(), offset, old_name);

                    if let Some(node) = node {
                        for event in node.borrowed().preorder() {
                            if let WalkEvent::Enter(node) = event {
                                if let Some(ident) = Ident::cast(node) {
                                    if ident.as_str() == old_name {
                                        edits.push(TextEdit {
                                            range: utils::range(code, node.range()),
                                            new_text: params.new_name.clone()
                                        });
                                    }
                                }
                            }
                        }
                    }
                }

                let mut changes = BTreeMap::new();
                changes.insert(params.text_document.uri, edits);

                self.send(&Response::success(req.id, WorkspaceEdit { changes }))?;
            }
            "textDocument/formatting" => {
                let params: Formatting = serde_json::from_value(req.params)?;

                let mut edits = None;
                if let Some((ast, code)) = self.files.get_mut(&params.text_document.uri) {
                    edits = Some(format::format(code, ast.node().borrowed()));
                }
                self.send(&Response::success(req.id, edits.unwrap_or_default()))?;
            },
            _ => ()
        }
        Ok(())
    }
    fn send_diagnostics(&mut self, uri: String, code: &str, ast: &AST) -> Result<(), Error> {
        let errors = ast.errors();
        let mut diagnostics = Vec::with_capacity(errors.len());
        for err in errors {
            if let ParseError::Unexpected(ref node) = err {
                diagnostics.push(Diagnostic {
                    range: utils::range(code, node.range()),
                    severity: ERROR,
                    message: err.to_string()
                });
            }
        }
        self.send(&Notification {
            method: "textDocument/publishDiagnostics".into(),
            params: DiagnosticParams {
                uri,
                diagnostics
            }
        })
    }
}
