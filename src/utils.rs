use lsp_types::*;
use rnix::{
    types::*,
    SyntaxNode,
    TextRange,
    TextUnit,
    TokenAtOffset,
};
use std::{
    collections::HashMap,
    convert::TryFrom,
    path::PathBuf,
    rc::Rc,
};

pub fn uri_path(uri: &Url) -> Option<PathBuf> {
    if uri.scheme() != "file" || uri.has_host() {
        return None;
    }
    Some(PathBuf::from(uri.path()))
}
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
                        .take(pos.character as usize)
                        .map(char::len_utf8)
                        .sum::<usize>()
            )
        })
}
pub fn lookup_range(code: &str, range: Range) -> Option<TextRange> {
    let start = lookup_pos(code, range.start)?;
    let end = lookup_pos(code, range.end)?;
    let res = TextRange::from_to(
        (start as u32).into(),
        (end as u32).into(),
    );
    Some(res)
}
pub fn offset_to_pos(code: &str, offset: usize) -> Position {
    let start_of_line = code[..offset].rfind('\n').map(|n| n+1).unwrap_or(0);
    Position {
        line: code[..start_of_line].chars().filter(|&c| c == '\n').count() as u64,
        character: code[start_of_line..offset].chars().map(|c| c.len_utf16() as u64).sum()
    }
}
pub fn range(code: &str, range: TextRange) -> Range {
    Range {
        start: offset_to_pos(code, range.start().to_usize()),
        end: offset_to_pos(code, range.end().to_usize()),
    }
}
pub struct CursorInfo {
    pub path: Vec<String>,
    pub ident: Ident,
}
pub fn ident_at(root: SyntaxNode, offset: usize) -> Option<CursorInfo> {
    let ident = match root.token_at_offset(TextUnit::from_usize(offset)) {
        TokenAtOffset::None => None,
        TokenAtOffset::Single(node) => Ident::cast(node.parent()),
        TokenAtOffset::Between(left, right) => Ident::cast(left.parent()).or_else(|| Ident::cast(right.parent()))
    }?;
    let parent = ident.node().parent();
    if let Some(attr) = parent.clone().and_then(Attribute::cast) {
        let mut path = Vec::new();
        for item in attr.path() {
            if item == *ident.node() {
                return Some(CursorInfo {
                    path,
                    ident,
                });
            }

            path.push(Ident::cast(item)?.as_str().into());
        }
        panic!("identifier at cursor is somehow not a child of its parent");
    } else if let Some(mut index) = parent.clone().and_then(IndexSet::cast) {
        let mut path = Vec::new();
        while let Some(new) = IndexSet::cast(index.set()?) {
            path.push(Ident::cast(new.index()?)?.as_str().into());
            index = new;
        }
        if index.set()? != *ident.node() {
            // Only push if not the cursor ident, so that
            // a . b
            //  ^
            // is not [a] and a, but rather [] and a
            path.push(Ident::cast(index.set()?)?.as_str().into());
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
    pub set: SyntaxNode,
    pub key: SyntaxNode,
    pub value: Option<SyntaxNode>
}
pub fn populate<'a, T: EntryHolder>(
    file: &Rc<Url>,
    scope: &mut HashMap<String, Var>,
    set: &T
) -> Option<()> {
    for entry in set.entries() {
        let attr = entry.key()?;
        let mut path = attr.path();
        if let Some(ident) = path.next().and_then(Ident::cast) {
            if !scope.contains_key(ident.as_str()) {
                scope.insert(ident.as_str().into(), Var {
                    file: Rc::clone(file),
                    set: set.node().to_owned(),
                    key: ident.node().to_owned(),
                    value: Some(entry.value()?.to_owned())
                });
            }
        }
    }
    Some(())
}
pub fn scope_for(file: &Rc<Url>, node: SyntaxNode) -> Option<HashMap<String, Var>> {
    let mut scope = HashMap::new();

    let mut current = Some(node);
    while let Some(node) = current {
        match ParsedType::try_from(node.clone()) {
            Ok(ParsedType::LetIn(let_in)) => { populate(&file, &mut scope, &let_in); },
            Ok(ParsedType::Let(let_)) => { populate(&file, &mut scope, &let_); },
            Ok(ParsedType::Set(set)) => if set.recursive() {
                populate(&file, &mut scope, &set);
            },
            Ok(ParsedType::Lambda(lambda)) => match ParsedType::try_from(lambda.arg()?) {
                Ok(ParsedType::Ident(ident)) => if !scope.contains_key(ident.as_str()) {
                    scope.insert(ident.as_str().into(), Var {
                        file: Rc::clone(&file),
                        set: lambda.node().clone(),
                        key: ident.node().clone(),
                        value: None
                    });
                },
                Ok(ParsedType::Pattern(pattern)) => {
                    for entry in pattern.entries() {
                        let ident = entry.name()?;
                        if !scope.contains_key(ident.as_str()) {
                            scope.insert(ident.as_str().into(), Var {
                                file: Rc::clone(&file),
                                set: lambda.node().to_owned(),
                                key: ident.node().to_owned(),
                                value: None
                            });
                        }
                    }
                },
                _ => ()
            },
            _ => ()
        }
        current = node.parent();
    }

    Some(scope)
}

pub fn extend(root: &SyntaxNode, range: TextRange) -> TextRange {
    let node = root.covering_element(range);

    match node.ancestors().skip_while(|n| n.text_range() == range).next() {
        None => range,
        Some(parent) => parent.text_range(),
    }
}
