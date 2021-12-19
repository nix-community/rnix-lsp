use crate::{error::ValueError, eval::Expr, EvalError};
use gc::{Finalize, Gc, Trace};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

#[derive(Clone, Trace, Finalize)]
pub enum NixValue {
    Map(HashMap<String, Gc<Expr>>),
    #[allow(dead_code)] // TODO: implement parsing for strings
    Str(String),
    Bool(bool),
    Float(f64),
    Integer(i64),
    Null,
}

impl NixValue {
    pub fn type_name(&self) -> String {
        match self {
            NixValue::Map(_) => "attribute set",
            NixValue::Str(_) => "string",
            NixValue::Bool(_) => "bool",
            NixValue::Float(_) => "float",
            NixValue::Integer(_) => "integer",
            NixValue::Null => "null",
        }
        .to_string()
    }

    pub fn as_map(&self) -> Result<HashMap<String, Gc<Expr>>, EvalError> {
        match self {
            NixValue::Map(x) => Ok(x.clone()),
            _ => Err(EvalError::Value(ValueError::TypeError(format!(
                "expected map, got {}",
                self.type_name()
            )))),
        }
    }

    pub fn as_str(&self) -> Result<String, EvalError> {
        match self {
            NixValue::Str(x) => Ok(x.clone()),
            _ => Err(EvalError::Value(ValueError::TypeError(format!(
                "expected string, got {}",
                self.type_name()
            )))),
        }
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
            Str(_) | Bool(_) | Float(_) | Integer(_) | Null => format!("```nix\n{}\n```", self),
            Map(_) => format!("```text\n{}\n```", self),
        }
    }
}

/// Maximum number of keys to show when displaying attribute sets in tooltips.
const MAX_DISPLAY_KEYS: usize = 5;

impl Display for NixValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NixValue::Str(x) => write!(f, "{:?}", x),
            NixValue::Bool(x) => write!(f, "{}", x),
            // TODO: format nicely like `nix repl`
            NixValue::Float(x) => write!(f, "{}", x),
            NixValue::Integer(x) => write!(f, "{}", x),
            NixValue::Null => write!(f, "null"),
            NixValue::Map(map) => {
                let mut keys = map
                    .keys()
                    .take(MAX_DISPLAY_KEYS)
                    .cloned()
                    .collect::<Vec<_>>();
                // Sorting a random subset of attributes would
                // confuse users who think that we're showing
                // the first MAX_DISPLAY_KEYS values *after*
                // sorting, instead of showing a sorted random
                // subset.
                if map.keys().len() <= MAX_DISPLAY_KEYS {
                    keys.sort();
                }
                let mut body = keys.join(", ");
                if map.keys().len() > MAX_DISPLAY_KEYS {
                    body.push_str(", ...");
                }
                write!(f, "{{ {} }}", body)
            }
        }
    }
}

impl Debug for NixValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}
