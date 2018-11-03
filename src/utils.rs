use super::models::*;

use failure::Error;
use rnix::{parser::*, types::*};
use rowan::{TextRange, TextUnit, WalkEvent};
use std::{
    collections::HashMap,
    borrow::Borrow
};

pub fn lookup_pos(code: &str, pos: Position) -> Result<usize, Error> {
    let mut lines = code.split('\n');

    let mut offset = 0;
    for _ in 0..pos.line {
        let line = lines.next().ok_or_else(|| format_err!("invalid position"))?;
        offset += line.len() + 1;
    }

    lines.next()
        .and_then(|line| {
            Some(
                offset +
                    line.chars()
                        .take(pos.character)
                        .map(char::len_utf8)
                        .sum::<usize>()
            )
        })
        .ok_or_else(|| format_err!("invalid position"))
}
pub fn offset_to_pos(code: &str, offset: usize) -> Position {
    let start_of_line = code[..offset].rfind('\n').map(|n| n+1).unwrap_or(0);
    Position {
        line: code[..start_of_line].chars().filter(|&c| c == '\n').count(),
        character: code[start_of_line..offset].chars().map(|c| c.len_utf16()).sum()
    }
}
pub fn range(code: &str, range: TextRange) -> Range {
    Range {
        start: offset_to_pos(code, range.start().to_usize()),
        end: offset_to_pos(code, range.end().to_usize()),
    }
}
pub fn ident_at(code: &str, offset: usize) -> (&str, TextRange) {
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
    (&code[start..end], TextRange::from_to(TextUnit::from_usize(start), TextUnit::from_usize(end)))
}

#[derive(Debug, Default)]
pub struct Scopes<'a>(pub Vec<HashMap<String, Node<rowan::RefRoot<'a, Types>>>>);
impl<'a> Scopes<'a> {
    pub fn var(&self, name: &str) -> Option<&Node<rowan::RefRoot<'a, Types>>> {
        self.0.iter().rev()
            .filter_map(|scope| scope.get(name))
            .next()
    }
    pub fn populate<T: EntryHolder<rowan::RefRoot<'a, Types>>>(&mut self, set: &T) {
        let mut map = HashMap::new();
        for entry in set.entries() {
            let attr = entry.key();
            let mut path = attr.path();
            if let Some(ident) = path.next().and_then(Ident::cast) {
                map.insert(ident.as_str().into(), *ident.node());
            }
        }
        self.0.push(map);
    }
}

pub fn scope_for<'a>(node: Node<rowan::RefRoot<'a, Types>>, offset: usize, name: &str)
    -> (Scopes<'a>, Option<Node<rowan::RefRoot<'a, Types>>>)
{
    let mut scopes = Scopes::default();

    for event in node.borrow().preorder() {
        match event {
            WalkEvent::Enter(node) => {
                if let Some(let_in) = LetIn::cast(node) {
                    scopes.populate(&let_in);
                } else if let Some(let_) = Let::cast(node) {
                    scopes.populate(&let_);
                } else if let Some(set) = Set::cast(node) {
                    if set.recursive() {
                        scopes.populate(&set);
                    }
                } else if let Some(lambda) = Lambda::cast(node) {
                    let mut map = HashMap::new();
                    if let Some(ident) = Ident::cast(lambda.arg()) {
                        map.insert(ident.as_str().into(), *ident.node());
                    } else if let Some(pattern) = Pattern::cast(lambda.arg()) {
                        for entry in pattern.entries() {
                            let ident = entry.name();
                            map.insert(ident.as_str().into(), *ident.node());
                        }
                    }
                    scopes.0.push(map);
                }
            },
            WalkEvent::Leave(node) => {
                let range = node.range();
                if LetIn::cast(node).is_some()
                        || Let::cast(node).is_some()
                        || Set::cast(node).map(|set| set.recursive()).unwrap_or(false)
                        || Lambda::cast(node).is_some() {
                    if scopes.0.last().unwrap().contains_key(name)
                            && offset >= range.start().to_usize()
                            && offset < range.end().to_usize() {
                        // We found the offset, return all scopes at this point
                        return (scopes, Some(node));
                    }
                    scopes.0.pop().unwrap();
                }
            }
        }
    }

    (scopes, None)
}
