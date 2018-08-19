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

crate type Scope = Vec<HashMap<String, Span>>;

crate fn lookup_def(log: &mut std::fs::File, arena: &Arena<'static, ASTNode>, node: &ASTNode, scope: &mut Scope, offset: u32)
    -> Result<Span, bool>
{
    match &node.1 {
        ASTType::Var(meta, name) => {
            use std::io::prelude::*;
            writeln!(log, "Var {} at offset {}", name, meta.span.start);
            if meta.span.start >= offset {
                return scope.iter().rev()
                    .filter_map(|scope| scope.get(&*name).cloned())
                    .next()
                    .ok_or(true);
            }
        },
        ASTType::Set { recursive: _, values: Brackets(_open, values, _close) } => {
            let mut map = HashMap::new();
            for entry in values {
                if let SetEntry::Assign(Attribute(attr), _assign, _value, _semi) = entry {
                    if attr.len() == 1 {
                        let (key, _) = attr[0];
                        if let ASTType::Var(meta, name) = &arena[key].1 {
                            map.insert(name.clone(), meta.span);
                        }
                    }
                }
            }
            scope.push(map);
        },
        _ => ()
    }

    for id in node.1.children() {
        let ret = lookup_def(log, arena, &arena[id], scope, offset);
        // Returns Ok(_) if it has a result or Err(true) if found
        if ret.is_ok() || ret.unwrap_err() {
            return ret;
        }
    }
    Err(false)
}
