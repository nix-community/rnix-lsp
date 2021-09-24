#![warn(
    // Harden built-in lints
    missing_copy_implementations,
    missing_debug_implementations,

    // Harden clippy lints
    clippy::cargo_common_metadata,
    clippy::clone_on_ref_ptr,
    clippy::dbg_macro,
    clippy::decimal_literal_representation,
    clippy::float_cmp_const,
    clippy::get_unwrap,
    clippy::integer_arithmetic,
    clippy::integer_division,
    clippy::pedantic,
)]
#![allow(
    // filter().map() can sometimes be more readable
    clippy::filter_map,
    // Most integer arithmetics are within an allocated region, so we know it's safe
    clippy::integer_arithmetic,
)]

mod error;
mod eval;
mod lookup;
mod parse;
mod scope;
mod tests;
mod utils;
mod value;

use error::{EvalError, ERR_PARSING};

use dirs::home_dir;
use eval::Expr;
use gc::Gc;
use log::{error, trace, warn};
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{Notification as _, *},
    request::{Request as RequestTrait, *},
    OneOf,
    *,
};
use rnix::{
    parser::*,
    types::*,
    value::{Anchor as RAnchor, Value as RValue},
    SyntaxNode, TextRange, TextSize,
};
use scope::Scope;
use std::{
    collections::{HashMap, VecDeque},
    panic,
    path::{Path, PathBuf},
    process,
    rc::Rc,
    str::FromStr,
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
                change: Some(TextDocumentSyncKind::Incremental),
                ..TextDocumentSyncOptions::default()
            },
        )),
        completion_provider: Some(CompletionOptions {
            ..CompletionOptions::default()
        }),
        definition_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        document_link_provider: Some(DocumentLinkOptions {
            resolve_provider: Some(false),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        rename_provider: Some(OneOf::Left(true)),
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
        ..ServerCapabilities::default()
    })
    .unwrap();

    connection.initialize(capabilities)?;

    App {
        files: HashMap::new(),
        conn: connection,
    }
    .main();

    io_threads.join()?;

    Ok(())
}

struct App {
    files: HashMap<Url, (AST, String, Result<Expr, EvalError>)>,
    conn: Connection,
}
impl App {
    fn reply(&mut self, response: Response) {
        trace!("Sending response: {:#?}", response);
        self.conn.sender.send(Message::Response(response)).unwrap();
    }
    fn notify(&mut self, notification: Notification) {
        trace!("Sending notification: {:#?}", notification);
        self.conn
            .sender
            .send(Message::Notification(notification))
            .unwrap();
    }
    fn err<E>(&mut self, id: RequestId, err: E)
    where
        E: std::fmt::Display,
    {
        warn!("{}", err);
        self.reply(Response::new_err(
            id,
            ErrorCode::UnknownErrorCode as i32,
            err.to_string(),
        ));
    }
    fn main(&mut self) {
        while let Ok(msg) = self.conn.receiver.recv() {
            trace!("Message: {:#?}", msg);
            match msg {
                Message::Request(req) => {
                    let id = req.id.clone();
                    match self.conn.handle_shutdown(&req) {
                        Ok(true) => break,
                        Ok(false) => self.handle_request(req),
                        Err(err) => {
                            // This only fails if a shutdown was
                            // requested in the first place, so it
                            // should definitely break out of the
                            // loop.
                            self.err(id, err);
                            break;
                        }
                    }
                }
                Message::Notification(notification) => {
                    let _ = self.handle_notification(notification);
                }
                Message::Response(_) => (),
            }
        }
    }
    fn handle_request(&mut self, req: Request) {
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
                }
            }
        }
        let mut req = Some(req);
        if let Some((id, params)) = cast::<GotoDefinition>(&mut req) {
            if let Some(pos) = self.lookup_definition(params.text_document_position_params) {
                self.reply(Response::new_ok(id, pos));
            } else {
                self.reply(Response::new_ok(id, ()));
            }
        } else if let Some((id, params)) = cast::<Completion>(&mut req) {
            let completions = self
                .completions(&params.text_document_position)
                .unwrap_or_default();
            self.reply(Response::new_ok(id, completions));
        } else if let Some((id, params)) = cast::<Rename>(&mut req) {
            let changes = self.rename(params);
            self.reply(Response::new_ok(
                id,
                WorkspaceEdit {
                    changes,
                    ..WorkspaceEdit::default()
                },
            ));
        } else if let Some((id, params)) = cast::<DocumentLinkRequest>(&mut req) {
            let document_links = self.document_links(&params).unwrap_or_default();
            self.reply(Response::new_ok(id, document_links));
        } else if let Some((id, params)) = cast::<Formatting>(&mut req) {
            let changes = if let Some((ast, code, _)) = self.files.get(&params.text_document.uri) {
                let fmt = nixpkgs_fmt::reformat_node(&ast.node());
                vec![TextEdit {
                    range: utils::range(&code, TextRange::up_to(ast.node().text().len())),
                    new_text: fmt.text().to_string(),
                }]
            } else {
                Vec::new()
            };
            self.reply(Response::new_ok(id, changes));
        } else if let Some((id, params)) = cast::<HoverRequest>(&mut req) {
            if let Some((range, markdown)) = self.hover(params) {
                self.reply(Response::new_ok(
                    id,
                    Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: markdown,
                        }),
                        range,
                    },
                ));
            } else {
                self.reply(Response::new_ok(id, ()));
            }
        } else if let Some((id, params)) = cast::<SelectionRangeRequest>(&mut req) {
            let mut selections = Vec::new();
            if let Some((ast, code, _)) = self.files.get(&params.text_document.uri) {
                for pos in params.positions {
                    selections.push(utils::selection_ranges(&ast.node(), code, pos));
                }
            }
            self.reply(Response::new_ok(id, selections));
        } else {
            let req = req.expect("internal error: req should have been wrapped in Some");

            self.reply(Response::new_err(
                req.id,
                ErrorCode::MethodNotFound as i32,
                format!("Unhandled method {}", req.method),
            ))
        }
    }

    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didChange
    fn handle_notification(&mut self, req: Notification) -> Result<(), Error> {
        match &*req.method {
            DidOpenTextDocument::METHOD => {
                let params: DidOpenTextDocumentParams = serde_json::from_value(req.params)?;
                let text = params.text_document.text;
                let parsed = rnix::parse(&text);
                self.send_diagnostics(params.text_document.uri.clone(), &text, &parsed)?;
                if let Ok(path) = PathBuf::from_str(params.text_document.uri.path()) {
                    let gc_root = Gc::new(Scope::Root(path));
                    let parsed_root = parsed.root().inner().ok_or(ERR_PARSING);
                    let evaluated = parsed_root.and_then(|x| Expr::parse(x, gc_root));
                    self.files
                        .insert(params.text_document.uri, (parsed, text, evaluated));
                }
            }
            DidChangeTextDocument::METHOD => {
                // Per the language server spec (https://git.io/JcrvY), we should apply changes
                // in order, the same as we would if we received them in separate notifications.
                // That means that, given TextDocumentContentChangeEvents A and B and original
                // document S, change A refers to S -> S' and B refers to S' -> S''. So we don't
                // need to remember original document indicies when applying multiple changes.
                let params: DidChangeTextDocumentParams = serde_json::from_value(req.params)?;
                let uri = params.text_document.uri;
                let mut content = self
                    .files
                    .get(&uri)
                    .map(|f| f.1.clone())
                    .unwrap_or("".to_string());
                for change in params.content_changes.into_iter() {
                    let range = match change.range {
                        Some(x) => x,
                        None => {
                            content = change.text;
                            continue;
                        }
                    };

                    let content_utf16 = content.encode_utf16().collect::<Vec<_>>();
                    let ascii_newline = 10;
                    let mut newline_iter = content_utf16
                        .iter()
                        .enumerate()
                        .filter(|&(_, x)| *x == ascii_newline);

                    let start_idx = if range.start.line == 0 {
                        0
                    } else {
                        newline_iter.nth(range.start.line as usize - 1).unwrap().0 + 1
                    } + range.start.character as usize;

                    let num_changed_lines = range.end.line - range.start.line;
                    let end_idx = if num_changed_lines == 0 {
                        start_idx + (range.end.character - range.start.character) as usize
                    } else {
                        // Note that .nth() is relative, not absolute
                        newline_iter.nth(num_changed_lines as usize - 1).unwrap().0 + 1
                            + range.end.character as usize
                    };

                    // Language server ranges are based on UTF-16 (https://git.io/JcrUi)
                    let mut new_content = String::from_utf16_lossy(&content_utf16[..start_idx]);
                    new_content.push_str(&change.text);
                    let suffix = String::from_utf16_lossy(&content_utf16[end_idx..]);
                    new_content.push_str(&suffix);

                    content = new_content;
                }
                let parsed = rnix::parse(&content);
                self.send_diagnostics(uri.clone(), &content, &parsed)?;
                if let Ok(path) = PathBuf::from_str(uri.path()) {
                    let gc_root = Gc::new(Scope::Root(path));
                    let parsed_root = parsed.root().inner().ok_or(ERR_PARSING);
                    let evaluated = parsed_root.and_then(|x| Expr::parse(x, gc_root));
                    self.files
                        .insert(uri, (parsed, content.to_owned().to_string(), evaluated));
                }
            }
            _ => (),
        }
        Ok(())
    }
    fn lookup_definition(&mut self, params: TextDocumentPositionParams) -> Option<Location> {
        let (current_ast, current_content, _) = self.files.get(&params.text_document.uri)?;
        let offset = utils::lookup_pos(current_content, params.position)?;
        let node = current_ast.node();
        let (name, scope, _) = self.scope_for_ident(params.text_document.uri, &node, offset)?;

        let var_e = scope.get(name.as_str())?;
        if let Some(var) = &var_e.var {
            let (_definition_ast, definition_content, _) = self.files.get(&var.file)?;
            Some(Location {
                uri: (*var.file).clone(),
                range: utils::range(definition_content, var.key.text_range()),
            })
        } else {
            None
        }
    }
    fn hover(&self, params: HoverParams) -> Option<(Option<Range>, String)> {
        let pos_params = params.text_document_position_params;
        let (_, content, expr) = self.files.get(&pos_params.text_document.uri)?;
        let offset = utils::lookup_pos(content, pos_params.position)?;
        let child_expr = climb_expr(expr.as_ref().ok()?, offset).clone();
        let range = utils::range(content, child_expr.range?);
        let msg = match child_expr.eval() {
            Ok(value) => value.format_markdown(),
            Err(EvalError::Value(ref err)) => format!("{}", err),
            Err(EvalError::Internal(_)) => return None,
        };
        Some((Some(range), msg))
    }
    #[allow(clippy::shadow_unrelated)] // false positive
    fn completions(&mut self, params: &TextDocumentPositionParams) -> Option<Vec<CompletionItem>> {
        let (ast, content, _) = self.files.get(&params.text_document.uri)?;
        let offset = utils::lookup_pos(content, params.position)?;

        let node = ast.node();
        let (node, scope, name) =
            self.scope_for_ident(params.text_document.uri.clone(), &node, offset)?;

        // Re-open, because scope_for_ident may mutably borrow
        let (_, content, _) = self.files.get(&params.text_document.uri)?;

        let mut completions = Vec::new();
        for (var, data) in scope {
            if var.starts_with(&name.as_str()) {
                let det = data.render_detail();
                completions.push(CompletionItem {
                    label: var.clone(),
                    documentation: data
                        .documentation
                        .map(|x| lsp_types::Documentation::String(x)),
                    deprecated: Some(data.deprecated),
                    text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                        range: utils::range(content, node.node().text_range()),
                        new_text: var.clone(),
                    })),
                    detail: Some(det),
                    ..CompletionItem::default()
                });
            }
        }
        Some(completions)
    }
    fn rename(&mut self, params: RenameParams) -> Option<HashMap<Url, Vec<TextEdit>>> {
        struct Rename<'a> {
            edits: Vec<TextEdit>,
            code: &'a str,
            old: &'a str,
            new_name: String,
        }
        fn rename_in_node(rename: &mut Rename, node: &SyntaxNode) -> Option<()> {
            if let Some(ident) = Ident::cast(node.clone()) {
                if ident.as_str() == rename.old {
                    rename.edits.push(TextEdit {
                        range: utils::range(rename.code, node.text_range()),
                        new_text: rename.new_name.clone(),
                    });
                }
            } else if let Some(index) = Select::cast(node.clone()) {
                rename_in_node(rename, &index.set()?);
            } else if let Some(attr) = Key::cast(node.clone()) {
                let mut path = attr.path();
                if let Some(ident) = path.next() {
                    rename_in_node(rename, &ident);
                }
            } else {
                for child in node.children() {
                    rename_in_node(rename, &child);
                }
            }
            Some(())
        }

        let uri = params.text_document_position.text_document.uri;
        let (ast, code, _) = self.files.get(&uri)?;
        let offset = utils::lookup_pos(code, params.text_document_position.position)?;
        let info = utils::ident_at(&ast.node(), offset)?;
        if !info.path.is_empty() {
            // Renaming within a set not supported
            return None;
        }
        let old = info.ident;
        let scope = utils::scope_for(&Rc::new(uri.clone()), old.node().clone())?;

        let mut rename = Rename {
            edits: Vec::new(),
            code,
            old: old.as_str(),
            new_name: params.new_name,
        };
        let definition = scope.get(old.as_str())?;
        rename_in_node(&mut rename, &definition.set);

        let mut changes = HashMap::new();
        changes.insert(uri, rename.edits);
        Some(changes)
    }
    fn document_links(&mut self, params: &DocumentLinkParams) -> Option<Vec<DocumentLink>> {
        let (current_ast, current_content, _) = self.files.get(&params.text_document.uri)?;
        let parent_dir = Path::new(params.text_document.uri.path()).parent();
        let home_dir = home_dir();
        let home_dir = home_dir.as_ref();

        let mut links = VecDeque::new();
        for node in current_ast.node().descendants() {
            let value = Value::cast(node.clone()).and_then(|v| v.to_value().ok());
            if let Some(RValue::Path(anchor, path)) = value {
                let file_url = match anchor {
                    RAnchor::Absolute => Some(PathBuf::from(&path)),
                    RAnchor::Relative => parent_dir.map(|p| p.join(path)),
                    RAnchor::Home => home_dir.map(|home| home.join(path)),
                    RAnchor::Store => None,
                }
                .map(|path| {
                    if path.is_dir() {
                        path.join("default.nix")
                    } else {
                        path
                    }
                })
                .filter(|path| path.is_file())
                .and_then(|s| Url::parse(&format!("file://{}", s.to_string_lossy())).ok());

                if let Some(file_url) = file_url {
                    links.push_back((node.text_range(), file_url))
                }
            }
        }

        let mut lsp_links = vec![];

        let mut cur_line_start = 0;
        let mut next_link_pos = usize::from(links.front()?.0.start());
        'pos_search: for (line_num, (cur_line_end, _)) in
            current_content.match_indices('\n').enumerate()
        {
            while next_link_pos >= cur_line_start && next_link_pos < cur_line_end {
                // We already checked if the list is empty
                let (range, url) = links.pop_front().unwrap();

                // Nix doesn't have multi-line links
                let start_pos = Position {
                    line: line_num as u32,
                    character: (next_link_pos - cur_line_start) as u32,
                };
                let end_pos = Position {
                    line: line_num as u32,
                    character: (usize::from(range.end()) - cur_line_start) as u32,
                };
                let lsp_range = Range {
                    start: start_pos,
                    end: end_pos,
                };

                lsp_links.push(DocumentLink {
                    target: Some(url),
                    data: None,
                    range: lsp_range,
                        tooltip: None,
                });

                if let Some((range, _)) = links.front() {
                    next_link_pos = usize::from(range.start());
                } else {
                    break 'pos_search;
                }
            }
            cur_line_start = cur_line_end + 1;
        }

        Some(lsp_links)
    }
    fn send_diagnostics(&mut self, uri: Url, code: &str, ast: &AST) -> Result<(), Error> {
        let errors = ast.errors();
        let mut diagnostics = Vec::with_capacity(errors.len());
        for err in errors {
            let node_range = match err {
                ParseError::Unexpected(range)
                | ParseError::UnexpectedDoubleBind(range)
                | ParseError::UnexpectedExtra(range)
                | ParseError::UnexpectedWanted(_, range, _) => Some(range),
                ParseError::UnexpectedEOF | ParseError::UnexpectedEOFWanted(_) => {
                    Some(TextRange::at(TextSize::of(code), TextSize::from(0)))
                }
                _ => None,
            };
            if let Some(node_range) = node_range {
                diagnostics.push(Diagnostic {
                    range: utils::range(code, node_range),
                    severity: Some(DiagnosticSeverity::Error),
                    message: err.to_string(),
                    ..Diagnostic::default()
                });
            }
        }
        self.notify(Notification::new(
            "textDocument/publishDiagnostics".into(),
            PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: None,
            },
        ));
        Ok(())
    }
}

fn climb_expr(here: &Expr, offset: usize) -> &Expr {
    for child in here.children().clone() {
        let range = match child.range {
            Some(x) => x,
            None => continue,
        };
        let start: usize = range.start().into();
        let end: usize = range.end().into();
        if start <= offset && offset < end {
            return climb_expr(child, offset);
        }
    }
    here
}
