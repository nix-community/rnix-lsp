use rnix::{parser::{*, children::ChildMut}, tokenizer::Trivia};
use std::{fmt, mem};

crate fn format(arena: &mut Arena<'static, ASTNode>, id: NodeId, indent: u32) -> fmt::Result {
    let mut i = 0;
    while let Some(child) = arena[id].1.child_mut(i) {
        i += 1;
        match child {
            ChildMut::Meta(meta) => {
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
            },
            ChildMut::Node(id) => {
                format(arena, id, indent+2)?;
            }
        }
    }
    Ok(())
}
