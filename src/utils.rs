use super::models::*;

use failure::Error;
use rnix::{parser::*, tokenizer::Span};
use std::collections::HashMap;

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

crate type Scope = HashMap<String, Span>;

fn set_scope(arena: &Arena<'static, ASTNode>, scopes: &mut Vec<Scope>, values: &[SetEntry]) -> bool {
    let mut map = HashMap::new();
    for entry in values {
        if let SetEntry::Assign(Attribute(attr), _assign, _value, _semi) = entry {
            if let Some(attr) = attr.first() {
                let (key, _) = attr;
                if let ASTType::Var(meta, name) = &arena[*key].1 {
                    map.insert(name.clone(), meta.span);
                }
            }
        }
    }
    if !map.is_empty() {
        scopes.push(map);
        true
    } else {
        false
    }
}
crate fn lookup_def(arena: &Arena<'static, ASTNode>, node: &ASTNode, scopes: &mut Vec<Scope>, offset: u32)
    -> Result<Span, bool>
{
    let mut pushed_scope = false;

    match &node.1 {
        ASTType::Var(meta, name) => {
            if meta.span.start >= offset {
                return scopes.iter().rev()
                    .filter_map(|scope| scope.get(&*name).cloned())
                    .next()
                    .ok_or(true);
            }
        },
        ASTType::Set { recursive: Some(_), values: Brackets(_open, values, _close) } => {
            pushed_scope = set_scope(arena, scopes, values);
        },
        ASTType::Let(_let, Brackets(_open, values, _close)) => {
            pushed_scope = set_scope(arena, scopes, values);
        },
        ASTType::LetIn(_let, values, _in, _body) => {
            pushed_scope = set_scope(arena, scopes, values);
        },
        _ => ()
    }

    for id in node.1.children() {
        let ret = lookup_def(arena, &arena[id], scopes, offset);
        // Returns Ok(_) if it has a result or Err(true) if found
        if ret.is_ok() || ret.unwrap_err() {
            return ret;
        }
    }

    if pushed_scope {
        scopes.pop().unwrap();
    }
    Err(false)
}
