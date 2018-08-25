use super::models::*;

use failure::Error;
use rnix::{
    parser::{*, types::*},
    tokenizer::Span
};
use std::{
    collections::HashMap,
    borrow::Borrow
};

crate fn lookup_pos(code: &str, mut pos: Position) -> Result<usize, Error> {
    let mut lines = code.split('\n');

    let mut offset = 0;
    for _ in 0..pos.line {
        let line = lines.next().ok_or_else(|| format_err!("invalid position"))?;
        offset += line.len() + 1;
    }

    lines.next()
        .and_then(|line| {
            for c in line.chars() {
                pos.character = match pos.character.checked_sub(1) {
                    Some(i) => i,
                    None => break
                };
                offset += c.len_utf8();
            }
            Some(offset)
        })
        .ok_or_else(|| format_err!("invalid position"))
}
crate fn offset_to_pos(code: &str, offset: usize) -> Position {
    let start_of_line = code[..offset].rfind('\n').map(|n| n+1).unwrap_or(0);
    Position {
        line: code[..start_of_line].chars().filter(|&c| c == '\n').count(),
        character: code[start_of_line..offset].chars().map(|c| c.len_utf16()).sum()
    }
}
crate fn span_to_range(code: &str, span: Span) -> Range {
    Range {
        start: offset_to_pos(code, span.start as usize),
        end: offset_to_pos(code, span.end.map(|i| i as usize).unwrap_or(code.len() - 1)),
    }
}
crate fn ident_at(code: &str, offset: usize) -> (&str, Span) {
    fn is_ident(c: &char) -> bool {
        match *c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' => true,
            _ => false
        }
    }

    let start: usize = offset -
        code[..offset].chars().rev()
            .take_while(is_ident)
            .map(char::len_utf8)
            .sum::<usize>();
    let end = offset + code[offset..].chars()
            .take_while(is_ident)
            .map(char::len_utf8)
            .sum::<usize>();
    (&code[start..end], Span {
        start: start as u32,
        end: Some(end as u32)
    })
}

#[derive(Default)]
crate struct Scopes(crate Vec<HashMap<String, (NodeId, Span)>>);

impl Scopes {
    pub fn find_var(&mut self, name: &str) -> Option<(NodeId, Span)> {
        self.0.iter().rev()
            .filter_map(|scope| scope.get(&*name).cloned())
            .next()
    }
}

crate struct LookupDone<'a, A: 'a> {
    crate arena: &'a mut A,
    //crate id: NodeId,
    crate scopes: &'a mut Scopes,
    //crate span: Span
}

fn set_scope<'a, I>(arena: &Arena, id: NodeId, scopes: &mut Scopes, values: I) -> bool
    where I: Iterator<Item = SetEntry<'a>>
{
    let mut map = HashMap::new();
    for entry in values {
        let attr = entry.key(arena);

        if let Some(first) = attr.node().children(arena).next() {
            if let Data::Ident(meta, name) = &arena[first].data {
                map.insert(name.clone(), (id, meta.span));
            }
        }
    }
    if !map.is_empty() {
        scopes.0.push(map);
        true
    } else {
        false
    }
}
crate fn lookup_var<A, F, T>(
    arena_borrow: &mut A,
    id: NodeId,
    scopes: &mut Scopes,
    offset: u32,
    callback: &mut F
) -> Option<T>
    where A: Borrow<Arena<'static>>,
          F: FnMut(LookupDone<A>) -> T
{
    let arena = (*arena_borrow).borrow();
    let node = &arena[id];
    let span = node.span;
    let pushed_scope = if let Some(set) = Set::cast(node) {
        if set.recursive(arena) {
            set_scope(arena, id, scopes, set.entries(arena))
        } else {
            false
        }
    } else if let Some(let_) = Let::cast(node) {
        set_scope(arena, id, scopes, let_.entries(arena))
    } else if let Some(let_in) = LetIn::cast(node) {
        set_scope(arena, id, scopes, let_in.entries(arena))
    } else {
        false
    };

    let mut cursor = node.node.child;

    while let Some(child) = cursor {
        cursor = (*arena_borrow).borrow()[child].node.sibling;

        if let ret @ Some(_) = lookup_var(arena_borrow, child, scopes, offset, callback) {
            return ret;
        }
    }

    if offset >= span.start && span.end.is_some() && offset <= span.end.unwrap() {
        return Some((*callback)(LookupDone {
            arena: arena_borrow,
            //id,
            scopes,
            //span,
        }));
    }

    if pushed_scope {
        scopes.0.pop().unwrap();
    }
    None
}
crate fn rename(arena: &mut Arena, id: NodeId, old: &str, new: &str) {
    if let Data::Ident(ref _meta, ref mut name) = arena[id].data {
        if name == old {
            *name = new.to_string();
        }
    }

    let mut cursor = arena[id].node.child;

    while let Some(child) = cursor {
        cursor = arena[child].node.sibling;
        rename(arena, child, old, new);
    }
}
