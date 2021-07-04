use crate::parse::BinOpKind;
use crate::scope::*;
use crate::value::*;
use crate::EvalError;
use gc::{Finalize, Gc, GcCell, Trace};
use rnix::TextRange;
use std::borrow::Borrow;

type TreeResult = Result<Box<Tree>, EvalError>;

/// Used to lazily calculate the value of a Tree. This should be
/// tolerant of parsing and evaluation errors from child Trees.
#[derive(Debug, Clone, Trace, Finalize)]
pub enum TreeSource {
    Literal {
        value: NixValue,
    },
    Paren {
        inner: TreeResult,
    },
    BinOp {
        op: BinOpKind,
        left: TreeResult,
        right: TreeResult,
    },
    BoolAnd {
        left: TreeResult,
        right: TreeResult,
    },
    BoolOr {
        left: TreeResult,
        right: TreeResult,
    },
    Implication {
        left: TreeResult,
        right: TreeResult,
    },
    UnaryInvert {
        value: TreeResult,
    },
    UnaryNegate {
        value: TreeResult,
    },
}

/// Syntax node that has context and can be lazily evaluated.
#[derive(Clone, Trace, Finalize)]
pub struct Tree {
    #[unsafe_ignore_trace]
    pub range: Option<TextRange>,
    pub value: GcCell<Option<Gc<NixValue>>>,
    pub source: TreeSource,
    pub scope: Gc<Scope>,
}

impl std::fmt::Debug for Tree {
    // The scope can be recursive, so we don't want to print it by default
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tree")
            .field("value", &self.value)
            .field("source", &self.source)
            .field("range", &self.range)
            .finish()
    }
}

impl Tree {
    /// Lazily evaluate a Tree, caching its value
    pub fn eval(&self) -> Result<Gc<NixValue>, EvalError> {
        let mut value_borrow = self.value.borrow_mut();
        if let Some(ref value) = *value_borrow {
            Ok(value.clone())
        } else {
            // We can later build a stack trace by wrapping errors here
            let value = self.eval_uncached()?;
            *value_borrow = Some(value.clone());
            Ok(value)
        }
    }

    fn eval_uncached(&self) -> Result<Gc<NixValue>, EvalError> {
        match &self.source {
            TreeSource::Paren { inner } => inner.as_ref()?.eval(),
            TreeSource::Literal { value } => Ok(Gc::new(value.clone())),
            TreeSource::BoolAnd { left, right } => {
                if left.as_ref()?.eval()?.as_bool()? {
                    right.as_ref()?.eval()
                } else {
                    Ok(Gc::new(NixValue::Bool(false)))
                }
            }
            TreeSource::BoolOr { left, right } => {
                if !left.as_ref()?.eval()?.as_bool()? {
                    right.as_ref()?.eval()
                } else {
                    Ok(Gc::new(NixValue::Bool(true)))
                }
            }
            TreeSource::Implication { left, right } => {
                if !left.as_ref()?.eval()?.as_bool()? {
                    Ok(Gc::new(NixValue::Bool(true)))
                } else {
                    right.as_ref()?.eval()
                }
            }

            #[allow(clippy::enum_glob_use)]
            #[allow(clippy::float_cmp)] // We want to match the Nix reference implementation
            TreeSource::BinOp { op, left, right } => {
                use BinOpKind::*;
                use NixValue::*;

                // Workaround for "temporary value dropped while borrowed"
                // https://doc.rust-lang.org/error-index.html#E0716
                let left_tmp = left.as_ref()?.eval()?;
                let left_val = left_tmp.borrow();
                let right_tmp = right.as_ref()?.eval()?;
                let right_val = right_tmp.borrow();

                // Specially handle integer division by zero
                if let (Div, Integer(_), Integer(0)) = (op, left_val, right_val) {
                    return Err(EvalError::Unexpected("division by zero".to_string()));
                }

                macro_rules! match_binops {
                    ( arithmetic [ $( $arith_kind:pat => $arith_oper:tt, )+ ],
                      comparisons [ $( $comp_kind:pat => $comp_oper:tt, )+ ],
                      $( $pattern:pat => $expr:expr ),*  ) => {
                        match (op, left_val, right_val) {
                            $(
                                ($arith_kind, Integer(x), Integer(y)) => Integer(x $arith_oper y),
                                ($arith_kind, Float(x), Float(y)) => Float(x $arith_oper y),
                                ($arith_kind, Integer(x), Float(y)) => Float((*x as f64) $arith_oper y),
                                ($arith_kind, Float(x), Integer(y)) => Float(x $arith_oper (*y as f64)),
                            )*
                            $(
                                ($comp_kind, Integer(x), Integer(y)) => Bool(x $comp_oper y),
                                ($comp_kind, Float(x), Float(y)) => Bool(x $comp_oper y),
                                ($comp_kind, Integer(x), Float(y)) => Bool((*x as f64) $comp_oper *y),
                                ($comp_kind, Float(x), Integer(y)) => Bool(*x $comp_oper (*y as f64)),
                            )*
                            $(
                                $pattern => $expr,
                            )*
                        }
                    };
                }

                let out = match_binops! {
                    arithmetic [
                        Add => +, Sub => -, Mul => *, Div => /,
                    ],
                    comparisons [
                        Equal => ==, NotEqual => !=,
                        Greater => >, GreaterOrEq => >=,
                        Less => <, LessOrEq => <=,
                    ],
                    _ => {
                        return Err(EvalError::Unexpected(format!(
                            "{:?} {:?} {:?} unsupported",
                            left, op, right
                        )))
                    }
                };

                Ok(Gc::new(out))
            }
            TreeSource::UnaryInvert { value } => {
                Ok(Gc::new(NixValue::Bool(!value.as_ref()?.eval()?.as_bool()?)))
            }
            TreeSource::UnaryNegate { value } => {
                Ok(Gc::new(match value.as_ref()?.eval()?.borrow() {
                    NixValue::Integer(x) => NixValue::Integer(-x),
                    NixValue::Float(x) => NixValue::Float(-x),
                    _ => {
                        return Err(EvalError::TypeError(
                            "cannot negate a non-number".to_string(),
                        ))
                    }
                }))
            }
        }
    }

    /// Used for recursing to find the Tree at a cursor position
    pub fn children(&self) -> Vec<&Box<Tree>> {
        match &self.source {
            TreeSource::Paren { inner } => vec![inner],
            TreeSource::Literal { value: _ } => vec![],
            TreeSource::BinOp { op: _, left, right } => vec![left, right],
            TreeSource::BoolAnd { left, right } => vec![left, right],
            TreeSource::BoolOr { left, right } => vec![left, right],
            TreeSource::Implication { left, right } => vec![left, right],
            TreeSource::UnaryInvert { value } => vec![value],
            TreeSource::UnaryNegate { value } => vec![value],
        }
        .into_iter()
        .map(|x| x.as_ref())
        .filter_map(Result::ok)
        .collect()
    }
}
