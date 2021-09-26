use std::borrow::Borrow;
use std::collections::HashMap;
use std::convert::TryFrom;

use crate::error::{EvalError, InternalError, ERR_PARSING};
use crate::eval::merge_values;
use crate::value::*;
use crate::{
    eval::{Expr, ExprSource},
    scope::Scope,
};
use gc::{Finalize, Gc, GcCell, Trace};
use rnix::types::{EntryHolder, TokenWrapper, TypedNode};
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
        let recurse_box = |node| Expr::parse(node, scope.clone()).map(|x| Box::new(x));
        let recurse_gc = |node| Expr::parse(node, scope.clone()).map(|x| Gc::new(x));
        let source = match ParsedType::try_from(node.clone()).map_err(|_| ERR_PARSING)? {
            ParsedType::Select(select) => ExprSource::Select {
                from: recurse_gc(select.set().ok_or(ERR_PARSING)?),
                index: recurse_box(select.index().ok_or(ERR_PARSING)?),
            },
            ParsedType::AttrSet(set) => {
                let is_recursive = set.recursive();

                // Create a new scope if we're a recursive attr set. We'll later
                // populate this scope with the non-dynamic keys of the set.
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

                // Construct key-value pairs. We store the original path syntax so the user
                // can hover over it to inspect dynamic values in code like:
                // `{ foo."${toString (1+1)}".bar = 2; }`.
                for entry in set.entries() {
                    let inner = Self::parse(entry.value().ok_or(ERR_PARSING)?, new_scope.clone())?;

                    let path = entry
                        .key()
                        .ok_or(ERR_PARSING)?
                        .path()
                        .map(|node| Self::parse(node, scope.clone()).map(Box::new))
                        .collect::<Vec<_>>();

                    let root_key = path[0].as_ref()?.as_ident()?;

                    // Although we need to preserve the original path syntax for user friendliness,
                    // we'll still convert the value to nested maps, for the sake of simplicity.
                    let value = if path.len() == 1 {
                        inner
                    } else {
                        let mut out = inner;
                        for part in path.iter().skip(1).rev() {
                            let mut map = HashMap::new();
                            map.insert(part.as_ref()?.as_ident()?, Gc::new(out));
                            out = Expr {
                                value: GcCell::new(None),
                                source: ExprSource::Literal {
                                    value: NixValue::Map(map),
                                },
                                range: Some(entry.node().text_range()),
                                scope: new_scope.clone(),
                            };
                        }
                        out
                    };

                    let attr = Gc::new(Expr {
                        value: GcCell::new(None),
                        source: ExprSource::MapAttr {
                            path: path,
                            value: Ok(Box::new(value)),
                        },
                        range: Some(entry.node().text_range()),
                        scope: new_scope.clone(),
                    });
                    definitions.push(Ok(attr.clone()));

                    // Merge values if needed. For example:
                    // { a.b = 1; a.c = 2; } => { a = { b = 1; c = 2; }; }
                    let insertion = match map.get(&root_key) as Option<&Gc<Expr>> {
                        Some(existing) => merge_values(existing.clone(), &attr)?,
                        None => attr,
                    };
                    map.insert(root_key, insertion);
                }
                // Note that we don't query the scope yet, since that would
                // cause expressions like `with pkgs; { inherit htop; }` to
                // evaluate the `with` statement earlier than needed. Instead
                // we create ExprSource::Ident expressions then put those in
                // the attribute set.
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
                        inherits.push(Ok(from.clone()));

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
                            inherits.push(Ok(attr.clone()));
                            map.insert(name.to_string(), attr);
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

                return Ok(Expr {
                    value: GcCell::new(Some(Gc::new(NixValue::Map(map)))),
                    source: ExprSource::Map {
                        inherits,
                        definitions,
                    },
                    range,
                    scope: new_scope,
                });
            }
            ParsedType::Paren(paren) => {
                let inner = paren.inner().ok_or(ERR_PARSING)?;
                ExprSource::Paren {
                    inner: recurse_box(inner),
                }
            }
            ParsedType::BinOp(binop) => {
                use rnix::types::BinOpKind::*;
                let left = recurse_box(binop.lhs().ok_or(ERR_PARSING)?);
                let right = recurse_box(binop.rhs().ok_or(ERR_PARSING)?);
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
                        value: recurse_box(unary.value().ok_or(ERR_PARSING)?),
                    },
                    UnaryOpKind::Negate => ExprSource::UnaryNegate {
                        value: recurse_box(unary.value().ok_or(ERR_PARSING)?),
                    },
                }
            }
            ParsedType::Ident(ident) => ExprSource::Ident {
                name: ident.as_str().to_string(),
            },
            ParsedType::Dynamic(dynamic) => ExprSource::Dynamic {
                inner: recurse_box(dynamic.inner().ok_or(ERR_PARSING)?),
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
