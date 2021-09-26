use gc::{Finalize, Trace};

pub const ERR_PARSING: EvalError = EvalError::Internal(InternalError::Parsing);


#[derive(Debug, Clone, Trace, Finalize)]
pub enum EvalError {
    Internal(InternalError),
    /// Used for Nix errors such as division by zero
    Value(ValueError),
}

impl From<&EvalError> for EvalError {
    fn from(x: &EvalError) -> Self {
        x.clone()
    }
}

#[derive(Debug, Clone, Trace, Finalize)]
pub enum InternalError {
    /// Used when the error might be our fault
    Unimplemented(String),
    /// Used instead of panics like `unreachable!`
    Unexpected(String),
    Parsing,
}

#[derive(Debug, Clone, Trace, Finalize)]
pub enum ValueError {
    DivisionByZero,
    TypeError(String),
}

impl std::error::Error for EvalError {}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            EvalError::Internal(x) => x.fmt(f),
            EvalError::Value(x) => x.fmt(f),
        }
    }
}

impl std::fmt::Display for InternalError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            InternalError::Parsing => write!(f, "parsing"),
            InternalError::Unimplemented(msg) => write!(f, "unimplemented: {}", msg),
            InternalError::Unexpected(msg) => write!(f, "unexpected: {}", msg),
        }
    }
}

impl std::fmt::Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ValueError::DivisionByZero => write!(f, "division by zero"),
            ValueError::TypeError(msg) => write!(f, "{}", msg),
        }
    }
}
