#[macro_use] extern crate failure;
#[macro_use] extern crate serde_derive;

mod models;
mod utils;

use self::models::*;

use failure::Error;
use rnix::parser::AST;
use std::{
    collections::HashMap,
    fs::File,
    io::{self, prelude::*}
};

fn main() -> Result<(), Error> {
    let mut log = File::create("/tmp/nix-lsp.log")?;

    let mut app = App {
        files: HashMap::new(),
        log: &mut log
    };
    if let Err(err) = app.main() {
        writeln!(log, "{:?}", err);
        return Err(err);
    }

    Ok(())
}

struct App<'a> {
    files: HashMap<String, (Option<AST<'static>>, String)>,
    log: &'a mut File
}
impl<'a> App<'a> {
    fn main(&mut self) -> Result<(), Error> {
        let stdin = io::stdin();
        let mut stdin = stdin.lock();
        let stdout = io::stdout();
        let mut stdout = stdout.lock();

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
                    let error = serde_json::to_vec(&Response::error(None, err))?;
                    self.respond(&mut stdout, &error)?;
                    continue;
                }
            };

            let id = req.id;
            match self.handle_request(req) {
                Ok(None) => (),
                Ok(Some(response)) => self.respond(&mut stdout, &response)?,
                Err(err) => {
                    writeln!(self.log, "{:?}", err);
                    let error = serde_json::to_vec(&Response::error(id, err))?;
                    self.respond(&mut stdout, &error)?;
                }
            }
        }
    }
    fn respond<W: io::Write>(&mut self, stdout: &mut W, bytes: &[u8]) -> Result<(), Error> {
        write!(stdout, "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n")?;
        write!(stdout, "Content-Length: {}\r\n", bytes.len())?;
        write!(stdout, "\r\n")?;

        stdout.write_all(&bytes)?;
        stdout.flush()?;
        Ok(())
    }
    fn handle_request(&mut self, req: Request) -> Result<Option<Vec<u8>>, Error> {
        Ok(match &*req.method {
            "initialize" => Some(serde_json::to_vec(&Response::success(req.id, Some(
                InitializeResult {
                    capabilities: ServerCapabilities {
                        definition_provider: true
                    }
                }
            )))?),
            "textDocument/didOpen" => {
                let params: DidOpen = serde_json::from_value(req.params)?;
                let text = params.text_document.text.ok_or_else(|| format_err!("missing text in request"))?;
                self.files.insert(params.text_document.uri, (rnix::parse(&text).ok(), text));
                None
            },
            "textDocument/didChange" => {
                let params: DidChange = serde_json::from_value(req.params)?;
                if let Some(change) = params.content_changes.into_iter().last() {
                    //writeln!(self.log, "PARSED: {:?}", rnix::parse(&change.text).map(|_| ()))?;
                    self.files.insert(params.text_document.uri, (rnix::parse(&change.text).ok(), change.text));
                }
                None
            },
            "textDocument/definition" => {
                let params: Definition = serde_json::from_value(req.params)?;
                if let Some((Some(ast), code)) = self.files.get(&params.text_document.uri) {
                    let offset = utils::lookup_pos(code, params.position)?;

                    let mut scope = Vec::new();
                    let def = utils::lookup_def(&ast.arena, &ast.root, &mut scope, offset as u32);
                    //writeln!(self.log, "LOOKUP DEFINITION {:?} {:?} {:?}", offset, scope, def)?;

                    Some(serde_json::to_vec(&Response::success(req.id, if let Ok(def) = def {
                        Some(Location {
                            uri: params.text_document.uri,
                            range: Range {
                                start: utils::offset_to_pos(code, def.start as usize),
                                end: utils::offset_to_pos(code, def.end.expect("no span end") as usize)
                            }
                        })
                    } else {
                        None
                    }))?)
                } else {
                    Some(serde_json::to_vec(&Response::empty(req.id))?)
                }
            },
            _ => None
        })
    }
}
