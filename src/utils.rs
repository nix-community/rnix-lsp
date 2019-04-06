use lsp_types::Url;
use rnix::{parser::*, types::*};
use rowan::{LeafAtOffset, TextRange, TextUnit, TreeArc};
use std::{
    collections::HashMap,
    path::PathBuf,
    rc::Rc
};

pub fn uri_path(uri: &Url) -> Option<PathBuf> {
    if uri.scheme() != "file" || uri.has_host() {
        return None;
    }
    Some(PathBuf::from(uri.path()))
}
pub fn lookup_pos(code: &str, pos: lsp_types::Position) -> Option<usize> {
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
                        .take(pos.character as usize)
                        .map(char::len_utf8)
                        .sum::<usize>()
            )
        })
}
pub fn lookup_range(code: &str, range: lsp_types::Range) -> Option<TextRange> {
    let start = lookup_pos(code, range.start)?;
    let end = lookup_pos(code, range.end)?;
    let res = TextRange::from_to(
        (start as u32).into(),
        (end as u32).into(),
    );
    Some(res)
}
pub fn offset_to_pos(code: &str, offset: usize) -> lsp_types::Position {
    let start_of_line = code[..offset].rfind('\n').map(|n| n+1).unwrap_or(0);
    lsp_types::Position {
        line: code[..start_of_line].chars().filter(|&c| c == '\n').count() as u64,
        character: code[start_of_line..offset].chars().map(|c| c.len_utf16() as u64).sum()
    }
}
pub fn range(code: &str, range: TextRange) -> lsp_types::Range {
    lsp_types::Range {
        start: offset_to_pos(code, range.start().to_usize()),
        end: offset_to_pos(code, range.end().to_usize()),
    }
}
pub struct CursorInfo<'a> {
    pub path: Vec<String>,
    pub ident: &'a Ident
}
pub fn ident_at(root: &Node, offset: usize) -> Option<CursorInfo> {
    let ident = match root.leaf_at_offset(TextUnit::from_usize(offset)) {
        LeafAtOffset::None => None,
        LeafAtOffset::Single(node) => Ident::cast(node),
        LeafAtOffset::Between(left, right) => Ident::cast(left).or_else(|| Ident::cast(right))
    }?;
    let parent = ident.node().parent();
    if let Some(attr) = parent.and_then(Attribute::cast) {
        let mut path = Vec::new();
        for item in attr.path() {
            if item == ident.node() {
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
        if index.set() != ident.node() {
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
    pub file: Rc<Url>,
    pub set: TreeArc<Types, Node>,
    pub key: TreeArc<Types, Node>,
    pub value: Option<TreeArc<Types, Node>>
}
pub fn populate<'a, T: EntryHolder>(
    file: &Rc<Url>,
    scope: &mut HashMap<String, Var>,
    set: &T
) {
    for entry in set.entries() {
        let attr = entry.key();
        let mut path = attr.path();
        if let Some(ident) = path.next().and_then(Ident::cast) {
            if !scope.contains_key(ident.as_str()) {
                scope.insert(ident.as_str().into(), Var {
                    file: Rc::clone(file),
                    set: set.node().to_owned(),
                    key: ident.node().to_owned(),
                    value: Some(entry.value().to_owned())
                });
            }
        }
    }
}
pub fn scope_for(file: &Rc<Url>, node: &Node) -> HashMap<String, Var> {
    let mut scope = HashMap::new();

    let mut current = Some(node);
    while let Some(node) = current {
        if let Some(let_in) = LetIn::cast(node) {
            populate(&file, &mut scope, let_in);
        } else if let Some(let_) = Let::cast(node) {
            populate(&file, &mut scope, let_);
        } else if let Some(set) = Set::cast(node) {
            if set.recursive() {
                populate(&file, &mut scope, set);
            }
        } else if let Some(lambda) = Lambda::cast(node) {
            if let Some(ident) = Ident::cast(lambda.arg()) {
                if !scope.contains_key(ident.as_str()) {
                    scope.insert(ident.as_str().into(), Var {
                        file: Rc::clone(&file),
                        set: lambda.node().to_owned(),
                        key: ident.node().to_owned(),
                        value: None
                    });
                }
            } else if let Some(pattern) = Pattern::cast(lambda.arg()) {
                for entry in pattern.entries() {
                    let ident = entry.name();
                    if !scope.contains_key(ident.as_str()) {
                        scope.insert(ident.as_str().into(), Var {
                            file: Rc::clone(&file),
                            set: lambda.node().to_owned(),
                            key: ident.node().to_owned(),
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

pub fn extend(root: &Node, range: TextRange) -> TextRange {
    let node = root.covering_node(range);

    match node.ancestors().skip_while(|n| n.range() == range).next() {
        None => range,
        Some(parent) => parent.range(),
    }
}
