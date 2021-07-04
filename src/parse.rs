use std::convert::TryFrom;

use crate::value::*;
use crate::{
    eval::{Tree, TreeSource},
    scope::Scope,
    EvalError,
};
use gc::{Finalize, Gc, GcCell, Trace};
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

impl Tree {
    /// Convert a rnix-parser tree into a syntax tree that can be lazily evaluated.
    ///
    /// Note that the lsp searches inward from the root of the file, so if a
    /// rnix::SyntaxNode isn't recognized, we don't get tooling for its children.
    pub fn parse(node: SyntaxNode, scope: Gc<Scope>) -> Result<Self, EvalError> {
        let range = Some(node.text_range());
        let recurse = |node| Tree::parse(node, scope.clone()).map(|x| Box::new(x));
        let source = match ParsedType::try_from(node.clone()).map_err(|_| EvalError::Parsing)? {
            ParsedType::Paren(paren) => {
                let inner = paren.inner().ok_or(EvalError::Parsing)?;
                TreeSource::Paren {
                    inner: recurse(inner),
                }
            }
            ParsedType::BinOp(binop) => {
                use rnix::types::BinOpKind::*;
                let left = recurse(binop.lhs().ok_or(EvalError::Parsing)?);
                let right = recurse(binop.rhs().ok_or(EvalError::Parsing)?);
                macro_rules! binop_source {
                    ( $op:expr ) => {
                        TreeSource::BinOp {
                            op: $op,
                            left,
                            right,
                        }
                    };
                }
                let op = match binop.operator() {
                    And => TreeSource::BoolAnd { left, right },
                    Or => TreeSource::BoolOr { left, right },
                    Implication => TreeSource::Implication { left, right },
                    IsSet => return Err(EvalError::Unimplemented("IsSet".to_string())),
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
                };
                TreeSource::BinOp { op, left, right }
            }
            ParsedType::UnaryOp(unary) => {
                use rnix::types::UnaryOpKind;
                match unary.operator() {
                    UnaryOpKind::Invert => TreeSource::UnaryInvert {
                        value: recurse(unary.value().ok_or(EvalError::Parsing)?),
                    },
                    UnaryOpKind::Negate => TreeSource::UnaryNegate {
                        value: recurse(unary.value().ok_or(EvalError::Parsing)?),
                    },
                }
            }
            ParsedType::Value(literal) => {
                use rnix::value::Value::*;
                // Booleans `true` and `false` are global variables, not literals
                TreeSource::Literal {
                    value: match literal.to_value().map_err(|_| EvalError::Parsing)? {
                        Float(x) => NixValue::Float(x),
                        Integer(x) => NixValue::Integer(x),
                        String(_) => {
                            return Err(EvalError::Unimplemented("string literal".to_string()))
                        }
                        Path(_, _) => {
                            return Err(EvalError::Unimplemented("path literal".to_string()))
                        }
                    },
                }
            }
            node => {
                return Err(EvalError::Unimplemented(format!(
                    "rnix-parser node {:?}",
                    node
                )))
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
