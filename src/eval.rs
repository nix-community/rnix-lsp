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
        use std::ops::Deref;
        let value_borrow = self.value.borrow();
        if let Some(ref value) = value_borrow.deref() {
            Ok(value.clone())
        } else {
            drop(value_borrow);
            // We can later build a stack trace by wrapping errors here
            let value = self.eval_uncached()?;
            *self.value.borrow_mut() = Some(value.clone());
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
            TreeSource::BinOp { op, left, right } => {
                use BinOpKind::*;
                use NixValue::*;
                let tmp1 = left.as_ref()?.eval()?;
                let tmp2 = right.as_ref()?.eval()?;
                let left = tmp1.borrow();
                let right = tmp2.borrow();
                let out = match (op, left, right) {
                    (Add, Integer(x), Integer(y)) => Integer(x + y),
                    (Add, Float(x), Float(y)) => Float(x + y),
                    (Add, Integer(x), Float(y)) => Float(*x as f64 + y),
                    (Add, Float(x), Integer(y)) => Float(x + *y as f64),

                    (Sub, Integer(x), Integer(y)) => Integer(x - y),
                    (Sub, Float(x), Float(y)) => Float(x - y),
                    (Sub, Integer(x), Float(y)) => Float(*x as f64 - y),
                    (Sub, Float(x), Integer(y)) => Float(x - *y as f64),

                    (Mul, Integer(x), Integer(y)) => Integer(x * y),
                    (Mul, Float(x), Float(y)) => Float(x * y),
                    (Mul, Integer(x), Float(y)) => Float(*x as f64 * y),
                    (Mul, Float(x), Integer(y)) => Float(x * *y as f64),

                    (Div, Integer(x), Integer(y)) => Integer(
                        x.checked_div(*y)
                            .ok_or_else(|| EvalError::Unexpected("division by zero".to_string()))?,
                    ),
                    (Div, Float(x), Float(y)) => Float(x / y),
                    (Div, Integer(x), Float(y)) => Float(*x as f64 / y),
                    (Div, Float(x), Integer(y)) => Float(x / *y as f64),

                    // It seems like the Nix reference implementation compares floats exactly
                    // https://github.com/NixOS/nix/blob/4a5aa1dbf6/src/libutil/comparator.hh#L26
                    (Equal, Integer(x), Integer(y)) => Bool(x == y),
                    (Equal, Float(x), Float(y)) => Bool(x == y),
                    (Equal, Integer(x), Float(y)) => Bool(*x as f64 == *y),
                    (Equal, Float(x), Integer(y)) => Bool(*x == *y as f64),
                    (Equal, Bool(x), Bool(y)) => Bool(x == y),

                    (NotEqual, Integer(x), Integer(y)) => Bool(x != y),
                    (NotEqual, Float(x), Float(y)) => Bool(x != y),
                    (NotEqual, Integer(x), Float(y)) => Bool(*x as f64 != *y),
                    (NotEqual, Float(x), Integer(y)) => Bool(*x != *y as f64),
                    (NotEqual, Bool(x), Bool(y)) => Bool(x != y),

                    (Less, Integer(x), Integer(y)) => Bool(x < y),
                    (Less, Float(x), Float(y)) => Bool(x < y),
                    (Less, Integer(x), Float(y)) => Bool((*x as f64) < *y),
                    (Less, Float(x), Integer(y)) => Bool(*x < *y as f64),

                    (LessOrEq, Integer(x), Integer(y)) => Bool(x <= y),
                    (LessOrEq, Float(x), Float(y)) => Bool(x <= y),
                    (LessOrEq, Integer(x), Float(y)) => Bool(*x as f64 <= *y),
                    (LessOrEq, Float(x), Integer(y)) => Bool(*x <= *y as f64),

                    (Greater, Integer(x), Integer(y)) => Bool(x > y),
                    (Greater, Float(x), Float(y)) => Bool(x > y),
                    (Greater, Integer(x), Float(y)) => Bool(*x as f64 > *y),
                    (Greater, Float(x), Integer(y)) => Bool(*x > (*y as f64)),

                    (GreaterOrEq, Integer(x), Integer(y)) => Bool(x >= y),
                    (GreaterOrEq, Float(x), Float(y)) => Bool(x >= y),
                    (GreaterOrEq, Integer(x), Float(y)) => Bool(*x as f64 >= *y),
                    (GreaterOrEq, Float(x), Integer(y)) => Bool(*x >= (*y as f64)),

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
