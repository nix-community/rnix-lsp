use super::{models::*, utils};

use rnix::{parser::*, tokenizer::Token, types::*};
use rowan::{SmolStr, WalkEvent};

fn find_whitespace<'a>(mut node: &Node) -> Option<&Node> {
    loop {
        let new = node.prev_sibling();
        if let Some(new) = new {
            node = new;
            break;
        } else {
            node = node.parent()?;
        }
    }

    match node.kind() {
        NodeType::Token(Token::Whitespace) => Some(node),
        _ => None
    }
}
fn do_indent(code: &str, edits: &mut Vec<TextEdit>, node: &Node, indent: usize) {
    match find_whitespace(node) {
        Some(ws) => {
            let newlines = ws.leaf_text().map(SmolStr::as_str).unwrap_or_default()
                .chars().filter(|&c| c == '\n').count();
            if newlines > 0 {
                edits.push(TextEdit {
                    range: utils::range(code, ws.range()),
                    new_text: "\n".repeat(newlines) + &" ".repeat(indent)
                });
            }
        }
        None => {
            let off = utils::offset_to_pos(code, node.range().start().to_usize());
            edits.push(TextEdit {
                range: lsp_types::Range {
                    start: off,
                    end: off
                },
                new_text: String::from(" ")
            });
        }
    }
}
pub fn format(code: &str, node: &Node) -> Vec<TextEdit> {
    let mut indent = 0;
    let mut edits = Vec::new();

    for event in node.preorder() {
        match event {
            WalkEvent::Enter(node) => {
                if let Some(entry) = SetEntry::cast(node) {
                    indent += 2;

                    let attr = entry.key();
                    let mut path = attr.path();
                    if let Some(ident) = path.next() {
                        do_indent(code, &mut edits, ident, indent);
                    }
                }
            },
            WalkEvent::Leave(node) => {
                if SetEntry::cast(node).is_some() {
                    indent -= 2;
                }
            }
        }
    }

    edits
}
