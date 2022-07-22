use std::error::Error;

use gc::{Finalize, Trace};
use rnix::TextRange;

pub const ERR_PARSING: EvalError = EvalError::Internal(InternalError::Parsing);

/// Used for rnix-lsp main functions
#[derive(Debug, Clone, Trace, Finalize)]
pub enum AppError {
    /// Unspecified internal error
    Internal(String),
}

impl std::error::Error for AppError {}

impl std::fmt::Display for AppError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AppError::Internal(x) => write!(f, "internal error: {}", x),
        }
    }
}

/// Could not evaluate an AST node
#[derive(Debug, Clone, Trace, Finalize)]
pub enum EvalError {
    /// failed to evaluate because of a limitation of our evaluator
    Internal(InternalError),
    /// the code of the user definitely does not evaluate
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

#[derive(Debug, Clone, Trace, Finalize, PartialEq, Eq)]
/// Used when we're confident that the user/code is at fault, such as
/// division by zero. We use the parser directly for error reporting,
/// so the evaluator returns its copies of parsing errors as internal,
/// silent errors (see InternalError above).
pub enum ValueError {
    /// Division by zero
    DivisionByZero,
    /// Type error
    TypeError(String),
    /// An attribute name is present twice in the same attrset
    AttrAlreadyDefined(String),
    /// An identifier is definitely not defined. Argument is the identifer.
    ///
    /// Note that this should not be used when one cannot know like
    /// `with import <nixpkgs> {}; some_attr`
    UnboundIdentifier(String),
}

#[derive(Debug, Clone, Trace, Finalize, PartialEq, Eq)]
/// An error augmented with a location
pub struct Located<T: Error + Clone + Trace + Finalize> {
    /// Where the error is located
    #[unsafe_ignore_trace]
    pub range: TextRange,
    /// The nature of the issue
    pub kind: T
}

impl<T: Error + Clone + Trace + Finalize> std::error::Error for Located<T> {}

impl<T: Error + Clone + Trace + Finalize> std::fmt::Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} at {:?}", &self.kind, &self.range)
    }
}

impl std::error::Error for ValueError {}

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
            ValueError::AttrAlreadyDefined(name) => write!(f, "attribute `{}` defined more than once", name),
            ValueError::TypeError(msg) => write!(f, "{}", msg),
            ValueError::UnboundIdentifier(name) => write!(f, "identifier {} is unbound", name),
        }
    }
}
