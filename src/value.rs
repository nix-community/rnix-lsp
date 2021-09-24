use crate::{EvalError, error::ValueError};
use gc::{Finalize, Trace};
use std::fmt::Debug;

#[derive(Clone, Trace, Finalize)]
pub enum NixValue {
    Bool(bool),
    Float(f64),
    Integer(i64),
}

impl NixValue {
    pub fn type_name(&self) -> String {
        match self {
            NixValue::Bool(_) => "bool",
            NixValue::Float(_) => "float",
            NixValue::Integer(_) => "integer",
        }
        .to_string()
    }

    pub fn as_bool(&self) -> Result<bool, EvalError> {
        match self {
            NixValue::Bool(x) => Ok(*x),
            _ => Err(EvalError::Value(ValueError::TypeError(format!(
                "expected bool, got {}",
                self.type_name()
            )))),
        }
    }

    #[allow(dead_code)] // this function is used by tests
    pub fn as_int(&self) -> Result<i64, EvalError> {
        match self {
            NixValue::Integer(x) => Ok(*x),
            _ => Err(EvalError::Value(ValueError::TypeError(format!(
                "expected int, got {}",
                self.type_name()
            )))),
        }
    }

    #[allow(dead_code)] // this function is used by tests
    pub fn as_float(&self) -> Result<f64, EvalError> {
        match self {
            NixValue::Float(x) => Ok(*x),
            _ => Err(EvalError::Value(ValueError::TypeError(format!(
                "expected float, got {}",
                self.type_name()
            )))),
        }
    }

    pub fn format_markdown(&self) -> String {
        use NixValue::*;
        match self {
            Bool(x) => format!("```nix\n{}\n```", x),
            Float(x) => format!("```nix\n{}\n```", x),
            Integer(x) => format!("```nix\n{}\n```", x),
        }
    }
}

impl Debug for NixValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NixValue::Bool(x) => write!(f, "{}", x),
            // TODO: format nicely like `nix rep`
            NixValue::Float(x) => write!(f, "{}", x),
            NixValue::Integer(x) => write!(f, "{}", x),
        }
    }
}

