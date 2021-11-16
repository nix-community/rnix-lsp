use std::borrow::Borrow;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::FromIterator;

use crate::error::{EvalError, InternalError, ValueError, ERR_PARSING};
use crate::eval::merge_set_literal;
use crate::value::*;
use crate::{
    eval::{Expr, ExprSource},
    scope::Scope,
};
use gc::{Finalize, Gc, GcCell, Trace};
use rnix::types::{EntryHolder, TokenWrapper, TypedNode};
use rnix::TextRange;
use rnix::{
    types::{ParsedType, Wrapper},
    SyntaxNode,
};

/// Unlike `and`, `or`, and `->`, this subset of binops
/// does not need special handling for lazy evaluation.
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
    Greater,
    GreaterOrEq,
    NotEqual,
}

impl Expr {
    /// Convert a rnix-parser tree into a syntax tree that can be lazily evaluated.
    ///
    /// Note that the lsp searches inward from the root of the file, so if a
    /// rnix::SyntaxNode isn't recognized, we don't get tooling for its children.
    pub fn parse(node: SyntaxNode, scope: Gc<Scope>) -> Result<Self, EvalError> {
        let range = Some(node.text_range());
        // We don't want to use ? because that would make a parent fail
        // to parse due to a child error. So, we have these helper functions
        // to handle error wrapping etc.
        let recurse_box = |node_res: Result<SyntaxNode, _>| {
            node_res
                .and_then(|node| Expr::parse(node, scope.clone()))
                .map(|x| Box::new(x))
        };
        let recurse_gc = |node_res: Result<SyntaxNode, _>| {
            node_res
                .and_then(|node| Expr::parse(node, scope.clone()))
                .map(|x| Gc::new(x))
        };
        let source = match ParsedType::try_from(node.clone()).map_err(|_| ERR_PARSING)? {
            ParsedType::Select(select) => ExprSource::Select {
                from: recurse_gc(select.set().ok_or(ERR_PARSING)),
                index: recurse_box(select.index().ok_or(ERR_PARSING)),
            },
            ParsedType::AttrSet(set) => {
                let is_recursive = set.recursive();

                // Create a new scope if we're a recursive attr set. We'll later
                // populate this scope with the non-dynamic keys of the set.
                let new_scope = if is_recursive {
                    let new = Scope::Let {
                        parent: scope.clone(),
                        contents: GcCell::new(HashMap::new()),
                    };
                    Gc::new(new)
                } else {
                    scope.clone()
                };

                // Used for the NixValue of this attribute set.
                let mut value_map = HashMap::new();
                // Used for the ExprSource. See ExprSource::AttrSet for
                // details on why we create both a hashmap and a vector.
                let mut definitions = vec![];

                for entry in set.entries() {
                    // Where x, y, z are KeyValuePairs:
                    //
                    //   services.bluetooth.enable = true;
                    //                      +------------+ x
                    //            +----------------------+ y
                    //   +-------------------------------+ z
                    //
                    // Hovering over `x` should show `true`.
                    // Hovering over `y` should show `{ enable }`.
                    // Hovering over `z` should show `{ bluetooth }`.
                    //
                    // This matches what we would see for verbose syntax:
                    //
                    //   services = { bluetooth = { enable = true; }; };
                    //
                    // So, we rewrite paths into the verbose syntax.

                    let mut path = entry
                        .key()
                        .ok_or(ERR_PARSING)?
                        .path()
                        .map(|node| Self::parse(node, scope.clone()).map(Gc::new))
                        .collect::<Vec<_>>();

                    // NOTE: This pops from the end, so we want to remove
                    //       the inmost element before reversing
                    let inmost_key = path.pop().unwrap()?;

                    // After this, our path lists path elements from right to left
                    path.reverse();

                    let inmost_value_syntax = entry.value().ok_or(ERR_PARSING)?;
                    let entry_end = inmost_value_syntax.text_range().end();
                    let inmost_value = Self::parse(inmost_value_syntax, new_scope.clone())?;

                    let here_start = inmost_key.range.ok_or(ERR_PARSING)?.start();

                    let mut cursor_range = TextRange::new(here_start, entry_end);
                    let mut cursor_key_name = inmost_key.as_ident()?;
                    let mut cursor_value = Gc::new(Expr {
                        value: GcCell::new(None),
                        source: ExprSource::KeyValuePair {
                            key: Ok(inmost_key),
                            value: Ok(Gc::new(inmost_value)),
                        },
                        range: Some(cursor_range),
                        scope: new_scope.clone(),
                    });

                    for element in path {
                        let here_start = element.as_ref()?.range.ok_or(ERR_PARSING)?.start();

                        // Create an invisible attr set
                        let tmp_map = NixValue::Map(HashMap::from_iter(vec![(
                            cursor_key_name,
                            cursor_value.clone(),
                        )]));
                        let tmp_attr_set = Gc::new(Expr {
                            value: GcCell::new(Some(Gc::new(tmp_map))),
                            source: ExprSource::AttrSet {
                                definitions: vec![Ok(cursor_value)],
                            },
                            range: Some(cursor_range),
                            scope: new_scope.clone(),
                        });

                        cursor_range = TextRange::new(here_start, entry_end);
                        cursor_key_name = element.as_ref()?.as_ident()?;
                        cursor_value = Gc::new(Expr {
                            value: GcCell::new(None),
                            source: ExprSource::KeyValuePair {
                                key: element,
                                value: Ok(tmp_attr_set),
                            },
                            range: Some(cursor_range.clone()),
                            scope: new_scope.clone(),
                        });
                    }

                    definitions.push(Ok(cursor_value.clone()));

                    // Merge values if needed. For example:
                    // { a.b = 1; a.c = 2; } => { a = { b = 1; c = 2; }; }
                    let merged_value = match value_map.get(&cursor_key_name) as Option<&Gc<Expr>> {
                        Some(existing) => merge_set_literal(
                            cursor_key_name.clone(),
                            existing.clone(),
                            cursor_value.clone(),
                        )?,
                        None => cursor_value,
                    };
                    value_map.insert(cursor_key_name, merged_value);
                }

                use std::collections::hash_map::Entry;

                // Note that we don't query the scope yet, since that would
                // cause expressions like `with pkgs; { inherit htop; }` to
                // evaluate the `with` statement earlier than needed. Instead
                // we create ExprSource::Ident and ExprSource::Select expressions
                // then put those in the attribute set.
                for inherit in set.inherits() {
                    // Handle syntax like `inherit (some_expression) foo` by
                    // rewriting it to `foo = some_expression.foo`, allowing
                    // `some_expression` to be lazily evaluated.
                    if let Some(from) = inherit.from() {
                        let from = Gc::new(Self::parse(
                            from.inner().ok_or(ERR_PARSING)?,
                            new_scope.clone(),
                        )?);

                        // For our example described above, add `some_expression`,
                        // `foo`, and `bar` to the ExprSource so they're all visible
                        // to interactive tooling.
                        definitions.push(Ok(from.clone()));

                        for ident in inherit.idents() {
                            let name = ident.as_str();
                            let index = Box::new(Expr {
                                value: GcCell::new(None),
                                source: ExprSource::Ident {
                                    name: name.to_string(),
                                },
                                range: None,
                                scope: scope.clone(),
                            });
                            let attr = Gc::new(Expr {
                                value: GcCell::new(None),
                                source: ExprSource::Select {
                                    from: Ok(from.clone()),
                                    index: Ok(index),
                                },
                                range: Some(ident.node().text_range()),
                                scope: scope.clone(),
                            });
                            definitions.push(Ok(attr.clone()));
                            let name = name.to_string();
                            match value_map.entry(name.clone()) {
                                Entry::Occupied(_) => {
                                    return Err(EvalError::Value(ValueError::AttrAlreadyDefined(name)))
                                }
                                Entry::Vacant(entry) => entry.insert(attr),
                            };
                        }
                    } else {
                        // Handle `inherit` from scope
                        for ident in inherit.idents() {
                            let name = ident.as_str();
                            let attr = Gc::new(Expr {
                                value: GcCell::new(None),
                                source: ExprSource::Ident {
                                    name: name.to_string(),
                                },
                                range: Some(ident.node().text_range()),
                                scope: scope.clone(),
                            });
                            definitions.push(Ok(attr.clone()));
                            let name = name.to_string();
                            match value_map.entry(name.clone()) {
                                Entry::Occupied(_) => {
                                    return Err(EvalError::Value(ValueError::AttrAlreadyDefined(name)))
                                }
                                Entry::Vacant(entry) => entry.insert(attr),
                            };
                        }
                    }
                }

                if is_recursive {
                    // update the scope to include our hashmap
                    if let Scope::Let { contents, .. } = new_scope.borrow() {
                        *contents.borrow_mut() = value_map.clone();
                    }
                }

                return Ok(Expr {
                    value: GcCell::new(Some(Gc::new(NixValue::Map(value_map)))),
                    source: ExprSource::AttrSet { definitions },
                    range,
                    scope: new_scope,
                });
            }
            ParsedType::Paren(paren) => ExprSource::Paren {
                inner: recurse_box(paren.inner().ok_or(ERR_PARSING)),
            },
            ParsedType::BinOp(binop) => {
                use rnix::types::BinOpKind::*;
                let left = recurse_box(binop.lhs().ok_or(ERR_PARSING));
                let right = recurse_box(binop.rhs().ok_or(ERR_PARSING));
                macro_rules! binop_source {
                    ( $op:expr ) => {
                        ExprSource::BinOp {
                            op: $op,
                            left,
                            right,
                        }
                    };
                }
                match binop.operator() {
                    And => ExprSource::BoolAnd { left, right },
                    Or => ExprSource::BoolOr { left, right },
                    Implication => ExprSource::Implication { left, right },
                    IsSet => {
                        return Err(EvalError::Internal(InternalError::Unimplemented(
                            "IsSet".to_string(),
                        )))
                    }
                    Concat => binop_source!(BinOpKind::Concat),
                    Update => binop_source!(BinOpKind::Update),
                    Add => binop_source!(BinOpKind::Add),
                    Sub => binop_source!(BinOpKind::Sub),
                    Mul => binop_source!(BinOpKind::Mul),
                    Div => binop_source!(BinOpKind::Div),
                    Equal => binop_source!(BinOpKind::Equal),
                    NotEqual => binop_source!(BinOpKind::NotEqual),
                    Less => binop_source!(BinOpKind::Less),
                    LessOrEq => binop_source!(BinOpKind::LessOrEq),
                    More => binop_source!(BinOpKind::Greater),
                    MoreOrEq => binop_source!(BinOpKind::GreaterOrEq),
                }
            }
            ParsedType::UnaryOp(unary) => {
                use rnix::types::UnaryOpKind;
                match unary.operator() {
                    UnaryOpKind::Invert => ExprSource::UnaryInvert {
                        value: recurse_box(unary.value().ok_or(ERR_PARSING)),
                    },
                    UnaryOpKind::Negate => ExprSource::UnaryNegate {
                        value: recurse_box(unary.value().ok_or(ERR_PARSING)),
                    },
                }
            }
            ParsedType::Ident(ident) => ExprSource::Ident {
                name: ident.as_str().to_string(),
            },
            ParsedType::Dynamic(dynamic) => ExprSource::Dynamic {
                inner: recurse_box(dynamic.inner().ok_or(ERR_PARSING)),
            },
            ParsedType::Value(literal) => {
                use rnix::value::Value::*;
                // Booleans `true` and `false` are global variables, not literals
                ExprSource::Literal {
                    value: match literal.to_value().map_err(|_| ERR_PARSING)? {
                        Float(x) => NixValue::Float(x),
                        Integer(x) => NixValue::Integer(x),
                        String(_) => {
                            return Err(EvalError::Internal(InternalError::Unimplemented(
                                "string literal".to_string(),
                            )))
                        }
                        Path(_, _) => {
                            return Err(EvalError::Internal(InternalError::Unimplemented(
                                "path literal".to_string(),
                            )))
                        }
                    },
                }
            }
            node => {
                return Err(EvalError::Internal(InternalError::Unimplemented(format!(
                    "rnix-parser node {:?}",
                    node
                ))))
            }
        };
        Ok(Self {
            value: GcCell::new(None),
            source,
            range,
            scope,
        })
    }
}
