use crate::value::*;
use crate::EvalError;
use crate::{
    eval::{merge_values, to_string, Tree, TreeSource},
    scope::Scope,
};
use gc::{Finalize, Gc, GcCell, Trace};
use rnix::{
    types::{
        Apply, AttrSet, BinOp, EntryHolder, Ident, IfElse, Lambda, LetIn, List, OrDefault, Paren,
        Select, Str, TokenWrapper, TypedNode, UnaryOp, Value, With, Wrapper,
    },
    StrPart, SyntaxNode,
};
use rnix::{
    types::{Assert, Dynamic},
    value::Anchor,
};
use std::{borrow::Borrow, collections::HashMap};

#[derive(Debug, Clone, Trace, Finalize)]
pub enum BinOpKind {
    Concat,
    Update,
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    Less,
    LessOrEq,
    More,
    MoreOrEq,
    NotEqual,
}

impl Tree {
    pub fn parse(node: SyntaxNode, scope: Gc<Scope>) -> Result<Gc<Self>, EvalError> {
        use rnix::SyntaxKind::*;
        let range = Some(node.text_range());
        let recurse = |node| Tree::parse(node, scope.clone());
        let source = match node.kind() {
            NODE_ERROR => return Err(EvalError::Parsing),
            NODE_ASSERT => {
                let node = Assert::cast(node).ok_or(EvalError::Parsing)?;
                let node_condition = node.condition().ok_or(EvalError::Parsing)?;
                let node_body = node.body().ok_or(EvalError::Parsing)?;
                TreeSource::Assert {
                    condition: recurse(node_condition),
                    body: recurse(node_body),
                }
            }
            NODE_IF_ELSE => {
                let node = IfElse::cast(node).ok_or(EvalError::Parsing)?;
                let node_condition = node.condition().ok_or(EvalError::Parsing)?;
                let node_true_body = node.body().ok_or(EvalError::Parsing)?;
                let node_false_body = node.else_body().ok_or(EvalError::Parsing)?;
                TreeSource::IfElse {
                    condition: recurse(node_condition),
                    true_body: recurse(node_true_body),
                    false_body: recurse(node_false_body),
                }
            }
            NODE_PAREN => {
                let paren = Paren::cast(node).ok_or(EvalError::Parsing)?;
                let inner = paren.inner().ok_or(EvalError::Parsing)?;
                TreeSource::Paren {
                    inner: recurse(inner),
                }
            }
            NODE_DYNAMIC => {
                let paren = Dynamic::cast(node).ok_or(EvalError::Parsing)?;
                let inner = paren.inner().ok_or(EvalError::Parsing)?;
                TreeSource::Dynamic {
                    inner: recurse(inner),
                }
            }
            NODE_LET_IN => {
                let node = LetIn::cast(node).ok_or(EvalError::Parsing)?;

                let new_scope = Gc::new(Scope::Normal {
                    parent: scope.clone(),
                    contents: GcCell::new(HashMap::new()),
                });

                let mut map = HashMap::new();
                let mut definitions = vec![];
                let mut inherits = vec![];

                for entry in node.entries() {
                    let inner =
                        Self::parse(entry.value().ok_or(EvalError::Parsing)?, new_scope.clone())?;

                    let path: Vec<SyntaxNode> =
                        entry.key().ok_or(EvalError::Parsing)?.path().collect();

                    let value: Gc<Tree> = if path.len() == 1 {
                        inner
                    } else {
                        let mut out = inner;
                        for part in path.clone().into_iter().skip(1).rev() {
                            let mut map = HashMap::new();
                            map.insert(to_string(&part, scope.clone())?, out);
                            out = Gc::new(Tree {
                                value: GcCell::new(None),
                                hash: GcCell::new(None),
                                source: TreeSource::Literal {
                                    value: NixValue::Map(map),
                                },
                                range,
                                scope: new_scope.clone(),
                            });
                        }
                        out
                    };

                    let attr = Gc::new(Tree {
                        value: GcCell::new(None),
                        hash: GcCell::new(None),
                        source: TreeSource::LetAttr {
                            path: path
                                .clone()
                                .into_iter()
                                .map(|x| Self::parse(x, scope.clone()))
                                .collect(),
                            value: Ok(value),
                        },
                        range: Some(entry.node().text_range()),
                        scope: new_scope.clone(),
                    });
                    definitions.push(Ok(attr.clone()));

                    let root_key = to_string(&path[0], scope.clone())?;
                    let insertion = match map.get(&root_key) as Option<&Gc<Tree>> {
                        Some(existing) => merge_values(existing.clone(), attr)?,
                        None => attr,
                    };
                    map.insert(root_key, insertion);
                }

                for inherit in node.inherits() {
                    if let Some(from_node) = inherit.from() {
                        let from = Self::parse(
                            from_node.inner().ok_or(EvalError::Parsing)?,
                            new_scope.clone(),
                        )?;
                        inherits.push(Ok(from.clone())); // allow handling inside the parentheses
                        for ident in inherit.idents() {
                            let name = ident.as_str();
                            let index = Gc::new(Tree {
                                value: GcCell::new(None),
                                hash: GcCell::new(None),
                                source: TreeSource::Ident {
                                    name: name.to_string(),
                                },
                                range: Some(from_node.node().text_range()),
                                scope: new_scope.clone(),
                            });
                            let attr = Gc::new(Tree {
                                value: GcCell::new(None),
                                hash: GcCell::new(None),
                                source: TreeSource::Select {
                                    from: Ok(from.clone()),
                                    index: Ok(index),
                                },
                                range: Some(ident.node().text_range()),
                                scope: new_scope.clone(),
                            });
                            inherits.push(Ok(attr.clone()));
                            map.insert(name.to_string(), attr);
                        }
                    } else {
                        for ident in inherit.idents() {
                            let name = ident.as_str();
                            let attr = Gc::new(Tree {
                                value: GcCell::new(None),
                                hash: GcCell::new(None),
                                source: TreeSource::Ident {
                                    name: name.to_string(),
                                },
                                range: Some(ident.node().text_range()),
                                scope: new_scope.clone(),
                            });
                            inherits.push(Ok(attr.clone()));
                            map.insert(name.to_string(), attr);
                        }
                    }
                }

                if let Scope::Normal { contents, .. } = new_scope.borrow() {
                    *contents.borrow_mut() = map;
                }

                return Ok(Gc::new(Tree {
                    value: GcCell::new(None),
                    hash: GcCell::new(None),
                    source: TreeSource::LetIn {
                        inherits,
                        definitions,
                        body: Tree::parse(
                            node.body().ok_or(EvalError::Parsing)?,
                            new_scope.clone(),
                        ),
                    },
                    range,
                    scope: new_scope,
                }));
            }
            NODE_BIN_OP => {
                let node_binop = BinOp::cast(node.clone()).ok_or(EvalError::Parsing)?;
                let left = Self::parse(node_binop.lhs().ok_or(EvalError::Parsing)?, scope.clone());
                let right = Self::parse(node_binop.rhs().ok_or(EvalError::Parsing)?, scope.clone());
                use rnix::types::BinOpKind::*;
                match node_binop.operator() {
                    IsSet => TreeSource::Legacy { syntax: node },
                    And => TreeSource::BoolAnd { left, right },
                    Or => TreeSource::BoolOr { left, right },
                    Implication => TreeSource::Implication { left, right },
                    _ => {
                        let op = match node_binop.operator() {
                            And | Or | IsSet | Implication => unreachable!(),
                            Concat => BinOpKind::Concat,
                            Update => BinOpKind::Update,
                            Add => BinOpKind::Add,
                            Sub => BinOpKind::Sub,
                            Mul => BinOpKind::Mul,
                            Div => BinOpKind::Div,
                            Equal => BinOpKind::Equal,
                            NotEqual => BinOpKind::NotEqual,
                            Less => BinOpKind::Less,
                            LessOrEq => BinOpKind::LessOrEq,
                            More => BinOpKind::More,
                            MoreOrEq => BinOpKind::MoreOrEq,
                        };
                        TreeSource::BinOp { op, left, right }
                    }
                }
            }
            NODE_IDENT => {
                let name = Ident::cast(node)
                    .ok_or(EvalError::Parsing)?
                    .as_str()
                    .to_string();
                TreeSource::Ident { name }
            }
            NODE_WITH => {
                let node = With::cast(node).ok_or(EvalError::Parsing)?;
                let namespace =
                    Self::parse(node.namespace().ok_or(EvalError::Parsing)?, scope.clone());
                let new_scope = Gc::new(Scope::With {
                    parent: scope.clone(),
                    contents: namespace.clone()?,
                });
                TreeSource::With {
                    value: namespace,
                    body: Self::parse(node.body().ok_or(EvalError::Parsing)?, new_scope),
                }
            }
            NODE_LIST => {
                let node = List::cast(node).ok_or(EvalError::Parsing)?;
                let mut out = vec![];
                for item in node.items() {
                    out.push(Self::parse(item, scope.clone()));
                }
                TreeSource::List { items: out }
            }
            NODE_APPLY => {
                let node_apply = Apply::cast(node).ok_or(EvalError::Parsing)?;
                let node_lambda = node_apply.lambda().ok_or(EvalError::Parsing)?;
                let node_arg = node_apply.value().ok_or(EvalError::Parsing)?;
                TreeSource::Apply {
                    lambda: recurse(node_lambda),
                    arg: recurse(node_arg),
                }
            }
            NODE_ATTR_SET => {
                let node = AttrSet::cast(node).ok_or(EvalError::Parsing)?;
                let is_recursive = node.recursive();

                // create a new scope if we're a recursive attr set
                let new_scope = if is_recursive {
                    let new = Scope::Normal {
                        parent: scope.clone(),
                        contents: GcCell::new(HashMap::new()),
                    };
                    Gc::new(new)
                } else {
                    scope.clone()
                };

                let mut map = HashMap::new();
                let mut definitions = vec![];
                let mut inherits = vec![];

                for entry in node.entries() {
                    let inner =
                        Self::parse(entry.value().ok_or(EvalError::Parsing)?, new_scope.clone())?;

                    let mut path = vec![];
                    for tmp in entry.key().ok_or(EvalError::Parsing)?.path() {
                        path.push(Self::parse(tmp, scope.clone()));
                    }

                    let value = if path.len() == 1 {
                        inner
                    } else {
                        let mut out = inner;
                        for part in path.clone().into_iter().skip(1).rev() {
                            let mut map = HashMap::new();
                            map.insert(part?.get_ident()?, out);
                            out = Gc::new(Tree {
                                value: GcCell::new(None),
                                hash: GcCell::new(None),
                                source: TreeSource::Literal {
                                    value: NixValue::Map(map),
                                },
                                range: Some(entry.node().text_range()),
                                scope: new_scope.clone(),
                            });
                        }
                        out
                    };

                    let attr = Gc::new(Tree {
                        value: GcCell::new(None),
                        hash: GcCell::new(None),
                        source: TreeSource::MapAttr {
                            path: path.clone(),
                            value: Ok(value),
                        },
                        range: Some(entry.node().text_range()),
                        scope: new_scope.clone(),
                    });
                    definitions.push(Ok(attr.clone()));

                    let root_key = match path[0].as_ref()?.get_ident() {
                        Ok(x) => x,
                        Err(_) => continue,
                    };
                    let insertion = match map.get(&root_key) as Option<&Gc<Tree>> {
                        Some(existing) => merge_values(existing.clone(), attr)?,
                        None => attr,
                    };
                    map.insert(root_key, insertion);
                }

                for inherit in node.inherits() {
                    if let Some(from) = inherit.from() {
                        let from = Self::parse(
                            from.inner().ok_or(EvalError::Parsing)?,
                            new_scope.clone(),
                        )?;
                        inherits.push(Ok(from.clone())); // allow handling inside the parenthesis
                        for ident in inherit.idents() {
                            let name = ident.as_str();
                            let index = Gc::new(Tree {
                                value: GcCell::new(None),
                                hash: GcCell::new(None),
                                source: TreeSource::Ident {
                                    name: name.to_string(),
                                },
                                range: None,
                                scope: scope.clone(),
                            });
                            let attr = Gc::new(Tree {
                                value: GcCell::new(None),
                                hash: GcCell::new(None),
                                source: TreeSource::Select {
                                    from: Ok(from.clone()),
                                    index: Ok(index),
                                },
                                range: Some(ident.node().text_range()),
                                scope: scope.clone(),
                            });
                            inherits.push(Ok(attr.clone()));
                            map.insert(name.to_string(), attr);
                        }
                    } else {
                        for ident in inherit.idents() {
                            let name = ident.as_str();
                            let attr = Gc::new(Tree {
                                value: GcCell::new(None),
                                hash: GcCell::new(None),
                                source: TreeSource::Ident {
                                    name: name.to_string(),
                                },
                                range: Some(ident.node().text_range()),
                                scope: scope.clone(),
                            });
                            inherits.push(Ok(attr.clone()));
                            map.insert(name.to_string(), attr);
                        }
                    }
                }

                if is_recursive {
                    // update the scope to include our hashmap
                    if let Scope::Normal { contents, .. } = new_scope.borrow() {
                        *contents.borrow_mut() = map.clone();
                    }
                }

                return Ok(Gc::new(Tree {
                    value: GcCell::new(Some(Gc::new(NixValue::Map(map)))),
                    hash: GcCell::new(None),
                    source: TreeSource::Map {
                        inherits,
                        definitions,
                    },
                    range,
                    scope: new_scope,
                }));
            }
            NODE_STRING_INTERPOL => {
                return Self::parse(node.first_child().ok_or(EvalError::Parsing)?, scope)
            }
            NODE_STRING => {
                let root = Str::cast(node).ok_or(EvalError::Parsing)?;
                let parts = root.parts();
                if parts.len() == 1 {
                    if let StrPart::Literal(ref x) = parts[0] {
                        return Ok(Gc::new(Tree {
                            value: GcCell::new(None),
                            hash: GcCell::new(None),
                            source: TreeSource::Literal {
                                value: NixValue::Str(x.clone()),
                            },
                            range,
                            scope: scope.clone(),
                        }));
                    }
                }

                let mut out = vec![];
                for part in root.parts() {
                    let x = match part {
                        StrPart::Literal(ref x) => Gc::new(Tree {
                            value: GcCell::new(None),
                            hash: GcCell::new(None),
                            source: TreeSource::Literal {
                                value: NixValue::Str(x.clone()),
                            },
                            range: None,
                            scope: Gc::new(Scope::None),
                        }),
                        StrPart::Ast(inner_node) => recurse(inner_node)?,
                    };
                    out.push(Ok(x));
                }

                TreeSource::StringInterpol { parts: out }
            }
            NODE_SELECT => {
                let node = Select::cast(node).ok_or(EvalError::Parsing)?;
                TreeSource::Select {
                    from: recurse(node.set().ok_or(EvalError::Parsing)?),
                    index: recurse(node.index().ok_or(EvalError::Parsing)?),
                }
            }
            NODE_UNARY_OP => {
                let node = UnaryOp::cast(node).ok_or(EvalError::Parsing)?;
                use rnix::types::UnaryOpKind;
                match node.operator() {
                    UnaryOpKind::Invert => TreeSource::UnaryInvert {
                        value: recurse(node.value().ok_or(EvalError::Parsing)?),
                    },
                    UnaryOpKind::Negate => TreeSource::UnaryNegate {
                        value: recurse(node.value().ok_or(EvalError::Parsing)?),
                    },
                }
            }
            NODE_LITERAL => {
                let root = Value::cast(node.clone())
                    .ok_or(EvalError::Parsing)?
                    .to_value()
                    .map_err(|_| EvalError::Parsing)?;
                use rnix::value::Value::*;
                TreeSource::Literal {
                    value: match root {
                        Float(x) => NixValue::Float(x),
                        Integer(x) => NixValue::Integer(x),
                        String(x) => NixValue::Str(x),
                        Path(x, y) => match x {
                            Anchor::Relative => NixValue::Path(
                                NixPathAnchor::Relative(
                                    scope
                                        .root_path()
                                        .ok_or(EvalError::Unexpected("missing path".to_string()))?
                                        .parent()
                                        .ok_or(EvalError::Parsing)?
                                        .to_path_buf(),
                                ),
                                y,
                            ),
                            Anchor::Absolute => NixValue::Path(NixPathAnchor::Absolute, y),
                            _ => {
                                return Err(EvalError::Unimplemented(format!(
                                    "{:?} -- {:?}\n\n{}",
                                    x,
                                    y,
                                    node.text()
                                )))
                            }
                        },
                    },
                }
            }
            NODE_LAMBDA => {
                let node_lambda = Lambda::cast(node).ok_or(EvalError::Parsing)?;
                TreeSource::Lambda {
                    literal: Gc::new(NixValue::Lambda(NixLambda::Node {
                        lambda: node_lambda,
                        scope: scope.clone(),
                    })),
                    params: vec![],
                    body: Err(EvalError::Parsing),
                }
            }
            _ => TreeSource::Legacy { syntax: node },
        };
        Ok(Gc::new(Self {
            hash: GcCell::new(None),
            value: GcCell::new(None),
            source,
            range,
            scope,
        }))
    }
}

pub fn eval_node(node: &SyntaxNode, scope: Gc<Scope>) -> Result<Gc<NixValue>, EvalError> {
    use rnix::SyntaxKind::*;
    Ok(match node.kind() {
        NODE_LAMBDA => {
            let out = NixValue::Lambda(NixLambda::Node {
                lambda: Lambda::cast(node.clone()).ok_or(EvalError::Parsing)?,
                scope,
            });
            Gc::new(out)
        }
        NODE_BIN_OP => {
            let root = BinOp::cast(node.clone()).ok_or(EvalError::Parsing)?;
            use rnix::types::BinOpKind;
            match root.operator() {
                BinOpKind::IsSet => {
                    let tmp =
                        Tree::parse_legacy(&root.lhs().ok_or(EvalError::Parsing)?, scope.clone())?;
                    match tmp.eval()?.borrow() {
                        NixValue::Map(map) => {
                            let rhs = root.rhs().ok_or(EvalError::Parsing)?;
                            match rhs.kind() {
                                NODE_IDENT | NODE_DYNAMIC => {
                                    let key = to_string(&rhs, scope)?;
                                    Gc::new(NixValue::Bool(map.contains_key(&key)))
                                }
                                NODE_SELECT => {
                                    let mut path = vec![]; // NB: inner ... outer
                                    let mut base = rhs;
                                    while base.kind() == NODE_SELECT {
                                        let tmp =
                                            Select::cast(base.clone()).ok_or(EvalError::Parsing)?;
                                        let part = to_string(
                                            &tmp.index().ok_or(EvalError::Parsing)?,
                                            scope.clone(),
                                        )?;
                                        path.push(part);
                                        base = tmp.set().ok_or(EvalError::Parsing)?;
                                    }
                                    let part = to_string(&base, scope)?;
                                    path.push(part);

                                    let mut cursor = tmp.eval()?;
                                    let mut valid = true;
                                    'foobar: for part in path.into_iter().rev() {
                                        let map = match cursor.borrow() {
                                            NixValue::Map(x) => x,
                                            _ => {
                                                valid = false;
                                                break 'foobar;
                                            }
                                        };
                                        match map.get(&part) {
                                            Some(x) => cursor = x.eval()?,
                                            None => {
                                                valid = false;
                                                break 'foobar;
                                            }
                                        }
                                    }

                                    Gc::new(NixValue::Bool(valid))
                                }
                                x => {
                                    return Err(EvalError::Unimplemented(format!(
                                        "cannot handle {:?}",
                                        x
                                    )))
                                }
                            }
                        }
                        _ => Gc::new(NixValue::Bool(false)),
                    }
                }
                _ => unreachable!(),
            }
        }
        NODE_OR_DEFAULT => {
            let nodex = OrDefault::cast(node.clone()).ok_or(EvalError::Parsing)?;
            let select = nodex.index().ok_or(EvalError::Parsing)?;
            let tmp = Tree::parse_legacy(&select.set().ok_or(EvalError::Parsing)?, scope.clone())?;
            match tmp.eval()?.borrow() {
                NixValue::Map(x) => {
                    let key = to_string(&select.index().ok_or(EvalError::Parsing)?, scope.clone())?;
                    match x.get(&key) {
                        Some(x) => x.eval()?,
                        None => {
                            Tree::parse_legacy(&nodex.default().ok_or(EvalError::Parsing)?, scope)?
                                .eval()?
                        }
                    }
                }
                _ => Tree::parse_legacy(&nodex.default().ok_or(EvalError::Parsing)?, scope)?
                    .eval()?,
            }
        }
        x => {
            return Err(EvalError::Unimplemented(format!(
                "Unexpected syntax node kind: {:?}\n\n{}",
                x,
                node.text()
            )))
        }
    })
}
