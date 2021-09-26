use gc::{Finalize, Trace};

pub const ERR_PARSING: EvalError = EvalError::Internal(InternalError::Parsing);


#[derive(Debug, Clone, Trace, Finalize)]
pub enum EvalError {
    Internal(InternalError),
    Value(ValueError),
}

impl From<&EvalError> for EvalError {
    fn from(x: &EvalError) -> Self {
        x.clone()
    }
}

/// Used when an error might be the fault of the rnix-lsp implementation.
/// Also used for parsing, since the LSP uses rnix-parser directly for
/// parsing; there's no need for the evaluator to return its own copies
/// of rnix-lsp errors.
///
/// When reporting errors, we strongly want to avoid false positives;
/// we never want an error message shown on correct code.
#[derive(Debug, Clone, Trace, Finalize)]
pub enum InternalError {
    /// Used when the error might be our fault
    Unimplemented(String),
    /// Used instead of panics like `unreachable!`
    Unexpected(String),
    Parsing,
}

#[derive(Debug, Clone, Trace, Finalize)]
/// Used when we're confident that the user/code is at fault, such as
/// division by zero. We use the parser directly for error reporting,
/// so the evaluator returns its copies of parsing errors as internal,
/// silent errors (see InternalError above).
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
