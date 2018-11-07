use super::models::*;

use rnix::{parser::*, types::*};
use rowan::{LeafAtOffset, TextRange, TextUnit};
use std::collections::HashMap;

pub fn lookup_pos(code: &str, pos: Position) -> Option<usize> {
    let mut lines = code.split('\n');

    let mut offset = 0;
    for _ in 0..pos.line {
        let line = lines.next()?;
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
pub struct CursorInfo<'a> {
    pub path: Vec<String>,
    pub ident: Ident<rowan::RefRoot<'a, Types>>
}
pub fn ident_at(root: Node<rowan::RefRoot<Types>>, offset: usize) -> Option<CursorInfo> {
    let ident = match root.leaf_at_offset(TextUnit::from_usize(offset)) {
        LeafAtOffset::None => None,
        LeafAtOffset::Single(node) => Ident::cast(node),
        LeafAtOffset::Between(left, right) => Ident::cast(left).or_else(|| Ident::cast(right))
    }?;
    let parent = ident.node().parent();
    if let Some(attr) = parent.and_then(Attribute::cast) {
        let mut path = Vec::new();
        for item in attr.path() {
            if item == *ident.node() {
                return Some(CursorInfo {
                    path,
                    ident
                });
            }

            path.push(Ident::cast(item)?.as_str().into());
        }
        panic!("identifier at cursor is somehow not a child of its parent");
    } else if let Some(mut index) = parent.and_then(IndexSet::cast) {
        let mut path = Vec::new();
        while let Some(new) = IndexSet::cast(index.set()) {
            path.push(Ident::cast(new.index())?.as_str().into());
            index = new;
        }
        if index.set() != *ident.node() {
            // Only push if not the cursor ident, so that
            // a . b
            //  ^
            // is not [a] and a, but rather [] and a
            path.push(Ident::cast(index.set())?.as_str().into());
        }
        path.reverse();
        Some(CursorInfo {
            path,
            ident
        })
    } else {
        Some(CursorInfo {
            path: Vec::new(),
            ident
        })
    }
}

#[derive(Debug)]
pub struct Var {
    pub set: Node<rowan::OwnedRoot<Types>>,
    pub key: Node<rowan::OwnedRoot<Types>>,
    pub value: Option<Node<rowan::OwnedRoot<Types>>>
}
pub fn scope_for_ident(root: Node<rowan::RefRoot<Types>>, offset: usize)
    -> Option<(Ident<rowan::RefRoot<Types>>, HashMap<String, Var>)>
{
    let info = ident_at(root, offset)?;
    let ident = info.ident;
    let mut entries = scope_for(*ident.node());
    for var in info.path {
        let node = entries.get(&var)?.value.as_ref()?.borrowed();
        entries = scope_from_node(node);
    }
    Some((ident, entries))
}
fn populate<'a, T: EntryHolder<rowan::RefRoot<'a, Types>>>(
    scope: &mut HashMap<String, Var>,
    set: &T
) {
    for entry in set.entries() {
        let attr = entry.key();
        let mut path = attr.path();
        if let Some(ident) = path.next().and_then(Ident::cast) {
            if !scope.contains_key(ident.as_str()) {
                scope.insert(ident.as_str().into(), Var {
                    set: set.node().owned(),
                    key: ident.node().owned(),
                    value: Some(entry.value().owned())
                });
            }
        }
    }
}
pub fn scope_for(node: Node<rowan::RefRoot<Types>>) -> HashMap<String, Var> {
    let mut scope = HashMap::new();

    let mut current = Some(node);
    while let Some(node) = current {
        if let Some(let_in) = LetIn::cast(node) {
            populate(&mut scope, &let_in);
        } else if let Some(let_) = Let::cast(node) {
            populate(&mut scope, &let_);
        } else if let Some(set) = Set::cast(node) {
            if set.recursive() {
                populate(&mut scope, &set);
            }
        } else if let Some(lambda) = Lambda::cast(node) {
            if let Some(ident) = Ident::cast(lambda.arg()) {
                if !scope.contains_key(ident.as_str()) {
                    scope.insert(ident.as_str().into(), Var {
                        set: lambda.node().owned(),
                        key: ident.node().owned(),
                        value: None
                    });
                }
            } else if let Some(pattern) = Pattern::cast(lambda.arg()) {
                for entry in pattern.entries() {
                    let ident = entry.name();
                    if !scope.contains_key(ident.as_str()) {
                        scope.insert(ident.as_str().into(), Var {
                            set: lambda.node().owned(),
                            key: ident.node().owned(),
                            value: None
                        });
                    }
                }
            }
        }
        current = node.parent();
    }

    scope
}
pub fn scope_from_node(mut node: Node<rowan::RefRoot<Types>>) -> HashMap<String, Var> {
    let mut scope = HashMap::new();
    if let Some(entry) = SetEntry::cast(node) {
        node = entry.value();
    }
    if let Some(set) = Set::cast(node) {
        populate(&mut scope, &set);
    }
    scope
}
