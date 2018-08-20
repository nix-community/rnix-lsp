use super::models::*;

use failure::Error;
use rnix::{parser::{*, children::Child}, tokenizer::Span};
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
crate fn span_to_range(code: &str, span: Span) -> Range {
    Range {
        start: offset_to_pos(code, span.start as usize),
        end: offset_to_pos(code, span.end.expect("no span end") as usize),
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
crate fn lookup_var<F, T>(
    arena: &Arena<'static, ASTNode>,
    id: NodeId,
    scopes: &mut Vec<Scope>,
    offset: u32,
    callback: &mut F
) -> Option<T>
    where F: FnMut(&[Scope], Span) -> T
{
    let node = &arena[id];
    let mut pushed_scope = false;

    match &node.1 {
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

    for child in node.1.children() {
        if let Child::Node(id) = child {
            if let ret @ Some(_) = lookup_var(arena, id, scopes, offset, callback) {
                return ret;
            }
        }
    }

    let span = node.0;
    if offset >= span.start && offset <= span.end.expect("no span end") {
        return Some((*callback)(scopes, span));
    }

    if pushed_scope {
        scopes.pop().unwrap();
    }
    None
}
