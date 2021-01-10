use lsp_types::*;
use rnix::{types::*, SyntaxNode, TextRange, TextUnit, TokenAtOffset};
use std::{collections::HashMap, convert::TryFrom, path::PathBuf, rc::Rc};
use std::fmt::{Display, Formatter, Result, Debug};

#[derive(Copy, Clone)]
pub enum Datatype {
    Lambda, Variable, Attribute
}

impl Display for Datatype {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Self::Lambda => "Lambda",
            Self::Variable => "Variable",
            Self::Attribute => "Attribute",
        })
    }
}

impl Debug for Datatype {
    fn fmt(&self, f: &mut Formatter) -> Result {
        Display::fmt(self, f)
    }
}

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

    lines.next().and_then(|line| {
        Some(
            offset
                + line
                    .chars()
                    .take(usize::try_from(pos.character).ok()?)
                    .map(char::len_utf8)
                    .sum::<usize>(),
        )
    })
}
pub fn offset_to_pos(code: &str, offset: usize) -> Position {
    let start_of_line = code[..offset].rfind('\n').map_or(0, |n| n + 1);
    Position {
        line: code[..start_of_line].chars().filter(|&c| c == '\n').count() as u64,
        character: code[start_of_line..offset]
            .chars()
            .map(|c| c.len_utf16() as u64)
            .sum(),
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
    pub name: String,
}

impl CursorInfo {
    pub fn new(path: Vec<String>, ident: Ident, name: Option<String>) -> CursorInfo {
        let myname = match name {
            Some(n) => n,
            None => String::from((Ident::cast(ident.node().clone()).unwrap()).as_str()),
        };

        CursorInfo {
            path,
            ident,
            name: myname,
        }
    }
}

pub fn ident_at(root: &SyntaxNode, offset: usize) -> Option<CursorInfo> {
    let mut add = false;
    let ident = match root.token_at_offset(TextUnit::from_usize(offset)) {
        TokenAtOffset::None => None,
        TokenAtOffset::Single(node) => Ident::cast(node.parent()),
        TokenAtOffset::Between(left, right) => {
            let result = Ident::cast(left.parent()).or_else(|| Ident::cast(right.parent()));
            match result {
                Some(_) => result,
                None => {
                    if let Some(sel) = Select::cast(left.parent()) {
                        add = true;
                        if let Some(s) = sel.set().and_then(Select::cast) {
                            Ident::cast(s.index()?)
                        } else {
                            Ident::cast(sel.set()?)
                        }
                    } else {
                        None
                    }
                },
            }
        }
    }?;
    let parent = ident.node().parent();
    if let Some(node) = parent.clone().and_then(Inherit::cast) {
        if let Some(node) = node.from() {
            if let Some(tok) = node.inner() {
                if let Some(_) = Ident::cast(tok.clone()) {
                    return Some(CursorInfo::new(
                        vec![tok.text().to_string()],
                        ident.clone(),
                        None,
                    ))
                } else if let Some(mut attr) = Select::cast(tok.clone()) {
                    let mut result = Vec::new();
                    result.push(attr.index()?.to_string().into());
                    while let Some(new) = Select::cast(attr.set()?) {
                        result.push(Ident::cast(new.index()?)?.as_str().into());
                        attr = new;
                    }
                    result.push(Ident::cast(attr.set()?)?.as_str().into());
                    result.reverse();
                    return Some(CursorInfo::new(
                        result,
                        ident.clone(),
                        None,
                    ))
                }
            }
        }
        Some(CursorInfo::new(
            Vec::new(),
            ident,
            None
        ))
    }
    else if let Some(attr) = parent.clone().and_then(Key::cast) {
        let mut path = Vec::new();
        for item in attr.path() {
            if item == *ident.node() {
                return Some(CursorInfo::new(path, ident, None));
            }

            path.push(Ident::cast(item)?.as_str().into());
        }
        panic!("identifier at cursor is somehow not a child of its parent");
    } else if let Some(mut index) = parent.and_then(Select::cast) {
        let mut path = Vec::new();
        while let Some(new) = Select::cast(index.set()?) {
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
        if add {
            path.push(String::from(ident.as_str()));
        }
        Some(CursorInfo::new(path, ident, match add {
            true => Some(String::from("")),
            false => None,
        }))
    } else {
        Some(CursorInfo::new(
            Vec::new(),
            ident,
            None
        ))
    }
}

#[derive(Debug)]
pub struct Var {
    pub file: Rc<Url>,
    pub set: SyntaxNode,
    pub key: SyntaxNode,
    pub value: Option<SyntaxNode>,
    pub datatype: Datatype,
}
pub fn populate<T: EntryHolder>(
    file: &Rc<Url>,
    scope: &mut HashMap<String, Var>,
    set: &T,
    datatype: Datatype,
) -> Option<()> {
    for entry in set.entries() {
        let attr = entry.key()?;
        let mut path = attr.path();
        if let Some(ident) = path.next().and_then(Ident::cast) {
            if !scope.contains_key(ident.as_str()) {
                scope.insert(
                    ident.as_str().into(),
                    Var {
                        file: Rc::clone(file),
                        set: set.node().to_owned(),
                        key: ident.node().to_owned(),
                        value: Some(entry.value()?.to_owned()),
                        datatype: datatype,
                    },
                );
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
            Ok(ParsedType::LetIn(let_in)) => {
                populate(&file, &mut scope, &let_in, Datatype::Variable);
            }
            Ok(ParsedType::LegacyLet(let_)) => {
                populate(&file, &mut scope, &let_, Datatype::Variable);
            }
            Ok(ParsedType::AttrSet(set)) => {
                if set.recursive() {
                    populate(&file, &mut scope, &set, Datatype::Attribute);
                }
            }
            Ok(ParsedType::Lambda(lambda)) => match ParsedType::try_from(lambda.arg()?) {
                Ok(ParsedType::Ident(ident)) => {
                    if !scope.contains_key(ident.as_str()) {
                        scope.insert(
                            ident.as_str().into(),
                            Var {
                                file: Rc::clone(&file),
                                set: lambda.node().clone(),
                                key: ident.node().clone(),
                                value: None,
                                datatype: Datatype::Lambda,
                            },
                        );
                    }
                }
                Ok(ParsedType::Pattern(pattern)) => {
                    for entry in pattern.entries() {
                        let ident = entry.name()?;
                        if !scope.contains_key(ident.as_str()) {
                            scope.insert(
                                ident.as_str().into(),
                                Var {
                                    file: Rc::clone(&file),
                                    set: lambda.node().to_owned(),
                                    key: ident.node().to_owned(),
                                    value: None,
                                    datatype: Datatype::Lambda,
                                },
                            );
                        }
                    }
                }
                _ => (),
            },
            _ => (),
        }
        current = node.parent();
    }

    Some(scope)
}
pub fn selection_ranges(root: &SyntaxNode, content: &str, pos: Position) -> Option<SelectionRange> {
    let pos = lookup_pos(content, pos)?;
    let node = root
        .token_at_offset(TextUnit::from_usize(pos))
        .left_biased()?;

    let mut root = None;
    let mut cursor = &mut root;

    let mut last = None;
    for parent in node.ancestors() {
        // De-duplicate
        if last.as_ref() == Some(&parent) {
            continue;
        }

        let text_range = parent.text_range();
        *cursor = Some(Box::new(SelectionRange {
            range: range(content, text_range),
            parent: None,
        }));
        cursor = &mut cursor.as_mut().unwrap().parent;

        last = Some(parent);
    }

    root.map(|b| *b)
}
