#![feature(panic_info_message)]

#[macro_use] extern crate failure;
#[macro_use] extern crate serde_derive;

mod format;
mod lookup;
mod models;
mod utils;

use self::models::*;

use failure::Error;
use rnix::{parser::*, types::*};
use std::{
    collections::{BTreeMap, HashMap},
    fmt,
    fs::File,
    io::{self, prelude::*},
    panic,
    rc::Rc
};
use url::Url;

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
                        text_document_sync: TextDocumentSyncOptions {
                            open_close: true,
                            change: 1, // 1 = send full text on modification
                        },
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
                writeln!(self.log, "{:?}", params.text_document.uri)?;
                if let Some(pos) = self.lookup_definition(params) {
                    self.send(&Response::success(req.id, pos))?;
                } else {
                    self.send(&Response::empty(req.id))?;
                }
            },
            "textDocument/completion" => {
                let params: Definition = serde_json::from_value(req.params)?;
                let completions = self.completions(params).unwrap_or_default();
                self.send(&Response::success(req.id, completions))?;
            },
            "textDocument/rename" => {
                let params: RenameParams = serde_json::from_value(req.params)?;
                let changes = self.rename(params).unwrap_or_default();
                self.send(&Response::success(req.id, WorkspaceEdit { changes }))?;
            }
            "textDocument/formatting" => {
                let params: Formatting = serde_json::from_value(req.params)?;

                let mut edits = None;
                if let Some((ast, code)) = self.files.get(&params.text_document.uri) {
                    edits = Some(format::format(code, ast.node().borrowed()));
                }
                self.send(&Response::success(req.id, edits.unwrap_or_default()))?;
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
                            let extended_range = utils::extend(ast.node().borrowed(), range);
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
    fn lookup_definition(&mut self, params: Definition) -> Option<Location> {
        let uri = Url::parse(&params.text_document.uri).ok()?;
        let (ast, code) = self.files.get(&params.text_document.uri)?;
        let offset = utils::lookup_pos(code, params.position)?;
        let (name, scope) = self.scope_for_ident(uri, ast.node().owned(), offset)?;

        let var = scope.get(name.as_str())?;
        let uri = var.file.to_string();
        let (_ast, code) = self.files.get(&uri)?;
        Some(Location {
            uri,
            range: utils::range(code, var.key.range())
        })
    }
    fn completions(&mut self, params: Definition) -> Option<Vec<CompletionItem>> {
        let uri = Url::parse(&params.text_document.uri).ok()?;
        let (ast, code) = self.files.get(&params.text_document.uri)?;
        let offset = utils::lookup_pos(code, params.position)?;

        let (name, scope) = self.scope_for_ident(uri, ast.node().owned(), offset)?;

        // Re-open, because scope_for_ident may mutably borrow
        let (_ast, code) = self.files.get(&params.text_document.uri)?;

        let mut completions = Vec::new();
        for var in scope.keys() {
            if var.starts_with(&name.as_str()) {
                completions.push(CompletionItem {
                    label: var.clone(),
                    edit: TextEdit {
                        range: utils::range(code, name.node().range()),
                        new_text: var.clone()
                    }
                });
            }
        }
        Some(completions)
    }
    fn rename(&mut self, params: RenameParams) -> Option<BTreeMap<String, Vec<TextEdit>>> {
        let uri = Url::parse(&params.text_document.uri).ok()?;
        let (ast, code) = self.files.get(&params.text_document.uri)?;
        let offset = utils::lookup_pos(code, params.position)?;
        let info = utils::ident_at(ast.node().borrowed(), offset)?;
        if !info.path.is_empty() {
            // Renaming within a set not supported
            return None;
        }
        let old = info.ident;
        let scope = utils::scope_for(&Rc::new(uri), *old.node());

        struct Rename<'a> {
            edits: Vec<TextEdit>,
            code: &'a str,
            old: &'a str,
            new_name: String,
        }
        fn rename_in_node(rename: &mut Rename, node: Node<rowan::RefRoot<Types>>) {
            if let Some(ident) = Ident::cast(node) {
                if ident.as_str() == rename.old {
                    rename.edits.push(TextEdit {
                        range: utils::range(rename.code, node.range()),
                        new_text: rename.new_name.clone()
                    });
                }
            } else if let Some(index) = IndexSet::cast(node) {
                rename_in_node(rename, index.set());
            } else if let Some(attr) = Attribute::cast(node) {
                let mut path = attr.path();
                if let Some(ident) = path.next() {
                    rename_in_node(rename, ident);
                }
            } else {
                for child in node.children() {
                    rename_in_node(rename, child);
                }
            }
        }

        let mut rename = Rename {
            edits: Vec::new(),
            code,
            old: old.as_str(),
            new_name: params.new_name
        };
        let definition = scope.get(old.as_str())?;
        rename_in_node(&mut rename, definition.set.borrowed());

        let mut changes = BTreeMap::new();
        changes.insert(params.text_document.uri, rename.edits);
        Some(changes)
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
