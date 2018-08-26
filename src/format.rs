use rnix::{parser::*, tokenizer::{Meta, Trivia}};
use std::mem;

fn indent_meta(meta: &mut Meta, indent: u32) {
    let mut filtered = mem::replace(&mut meta.leading, Vec::new())
        .into_iter()
        .filter(|trivia| !trivia.is_spaces())
        .peekable();

    let mut is_multiline = false;

    while let Some(trivia) = filtered.next() {
        is_multiline = is_multiline || trivia.is_newlines();
        let should_indent = trivia.is_newlines()
            && filtered.peek().map(|trivia| !trivia.is_newlines()).unwrap_or(false);

        meta.leading.push(trivia);

        if should_indent {
            meta.leading.push(Trivia::Spaces(indent));
        }
    }
    if is_multiline {
        meta.leading.push(Trivia::Spaces(indent));
    }
}
crate fn format(arena: &mut Arena<'static>, id: NodeId, mut indent: u32) {
    let node = &mut arena[id];

    match &mut node.data {
        Data::Ident(meta, _) => indent_meta(meta, indent),
        Data::Interpol { meta, .. } => indent_meta(meta, indent),
        Data::Token(meta, _) => indent_meta(meta, indent),
        Data::Value(meta, _) => indent_meta(meta, indent),
        Data::Error(_)
        | Data::InterpolLiteral { .. }
        | Data::None => ()
    }

    let mut cursor = node.node.child;

    if node.kind == ASTKind::SetEntry || node.kind == ASTKind::Inherit || node.kind == ASTKind::ListItem {
        indent += 2;
    }

    while let Some(child) = cursor {
        cursor = arena[child].node.sibling;

        format(arena, child, indent);
    }
}
