use crate::error::{InternalError, ValueError};
use crate::parse::BinOpKind;
use crate::scope::*;
use crate::value::*;
use crate::EvalError;
use gc::{Finalize, Gc, GcCell, Trace};
use rnix::TextRange;
use std::borrow::Borrow;

type ExprResult = Result<Box<Expr>, EvalError>;

/// Used to lazily calculate the value of a Expr. This should be
/// tolerant of parsing and evaluation errors from child Exprs.
#[derive(Debug, Clone, Trace, Finalize)]
pub enum ExprSource {
    Literal {
        value: NixValue,
    },
    Paren {
        inner: ExprResult,
    },
    BinOp {
        op: BinOpKind,
        left: ExprResult,
        right: ExprResult,
    },
    BoolAnd {
        left: ExprResult,
        right: ExprResult,
    },
    BoolOr {
        left: ExprResult,
        right: ExprResult,
    },
    Implication {
        left: ExprResult,
        right: ExprResult,
    },
    UnaryInvert {
        value: ExprResult,
    },
    UnaryNegate {
        value: ExprResult,
    },
}

/// Syntax node that has context and can be lazily evaluated.
#[derive(Clone, Trace, Finalize)]
pub struct Expr {
    #[unsafe_ignore_trace]
    pub range: Option<TextRange>,
    pub value: GcCell<Option<Gc<NixValue>>>,
    pub source: ExprSource,
    pub scope: Gc<Scope>,
}

impl std::fmt::Debug for Expr {
    // The scope can be recursive, so we don't want to print it by default
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Expr")
            .field("value", &self.value)
            .field("source", &self.source)
            .field("range", &self.range)
            .finish()
    }
}

impl Expr {
    /// Lazily evaluate a Expr, caching its value
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
            ExprSource::Paren { inner } => inner.as_ref()?.eval(),
            ExprSource::Literal { value } => Ok(Gc::new(value.clone())),
            ExprSource::BoolAnd { left, right } => {
                if left.as_ref()?.eval()?.as_bool()? {
                    right.as_ref()?.eval()
                } else {
                    Ok(Gc::new(NixValue::Bool(false)))
                }
            }
            ExprSource::BoolOr { left, right } => {
                if !left.as_ref()?.eval()?.as_bool()? {
                    right.as_ref()?.eval()
                } else {
                    Ok(Gc::new(NixValue::Bool(true)))
                }
            }
            ExprSource::Implication { left, right } => {
                if !left.as_ref()?.eval()?.as_bool()? {
                    Ok(Gc::new(NixValue::Bool(true)))
                } else {
                    right.as_ref()?.eval()
                }
            }

            #[allow(clippy::enum_glob_use)]
            #[allow(clippy::float_cmp)]
            // We want to match the Nix reference implementation
            ExprSource::BinOp { op, left, right } => {
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
                    return Err(EvalError::Value(ValueError::DivisionByZero));
                }

                macro_rules! match_binops {
                    ( arithmetic [ $( $arith_kind:pat => $arith_oper:tt, )+ ],
                      comparisons [ $( $comp_kind:pat => $comp_oper:tt, )+ ],
                      $( $pattern:pat => $expr:expr ),* ) => {
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
                        // We assume that it's our fault if an operation is unsupported.
                        // Over time, we can rewrite common cases into type errors.
                        return Err(EvalError::Internal(InternalError::Unimplemented(format!(
                            "{:?} {:?} {:?} unsupported",
                            left, op, right
                        ))))
                    }
                };

                Ok(Gc::new(out))
            }
            ExprSource::UnaryInvert { value } => {
                Ok(Gc::new(NixValue::Bool(!value.as_ref()?.eval()?.as_bool()?)))
            }
            ExprSource::UnaryNegate { value } => {
                Ok(Gc::new(match value.as_ref()?.eval()?.borrow() {
                    NixValue::Integer(x) => NixValue::Integer(-x),
                    NixValue::Float(x) => NixValue::Float(-x),
                    _ => {
                        return Err(EvalError::Value(ValueError::TypeError(
                            "cannot negate a non-number".to_string(),
                        )))
                    }
                }))
            }
        }
    }

    /// Used for recursing to find the Expr at a cursor position
    pub fn children(&self) -> Vec<&Box<Expr>> {
        match &self.source {
            ExprSource::Paren { inner } => vec![inner],
            ExprSource::Literal { value: _ } => vec![],
            ExprSource::BinOp { op: _, left, right } => vec![left, right],
            ExprSource::BoolAnd { left, right } => vec![left, right],
            ExprSource::BoolOr { left, right } => vec![left, right],
            ExprSource::Implication { left, right } => vec![left, right],
            ExprSource::UnaryInvert { value } => vec![value],
            ExprSource::UnaryNegate { value } => vec![value],
        }
        .into_iter()
        .map(|x| x.as_ref())
        .filter_map(Result::ok)
        .collect()
    }
}
