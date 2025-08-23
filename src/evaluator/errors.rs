//! Error types for the Monkey language evaluator.
//!
//! This module defines structured error types for evaluation-time errors,
//! providing better error categorization and integration with Rust's error handling ecosystem.

use crate::lexer::Position;
use std::fmt;

/// Errors that can occur during evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum EvaluatorError {
    /// Type error - operation not supported for given types
    TypeError {
        message: String,
        position: Option<Position>,
    },
    /// Runtime error - general execution error
    RuntimeError {
        message: String,
        position: Option<Position>,
    },
    /// Identifier not found in environment
    IdentifierNotFound {
        name: String,
        position: Option<Position>,
    },
    /// Division by zero error
    DivisionByZero {
        position: Option<Position>,
    },
    /// Invalid function call (wrong arity, not callable, etc.)
    InvalidFunctionCall {
        message: String,
        position: Option<Position>,
    },
    /// Invalid array or hash index
    InvalidIndex {
        message: String,
        position: Option<Position>,
    },
    /// Parser error wrapped in evaluator error
    ParseError(crate::parser::ParserError),
    /// Lexer error wrapped in evaluator error
    LexerError(crate::lexer::LexerError),
}

/// Result type for evaluator operations.
pub type Result<T> = std::result::Result<T, EvaluatorError>;

impl EvaluatorError {
    /// Create a new TypeError
    pub fn type_error(message: impl Into<String>) -> Self {
        Self::TypeError {
            message: message.into(),
            position: None,
        }
    }

    /// Create a new TypeError with position
    pub fn type_error_at(message: impl Into<String>, position: Position) -> Self {
        Self::TypeError {
            message: message.into(),
            position: Some(position),
        }
    }

    /// Create a new RuntimeError
    pub fn runtime_error(message: impl Into<String>) -> Self {
        Self::RuntimeError {
            message: message.into(),
            position: None,
        }
    }

    /// Create a new RuntimeError with position
    pub fn runtime_error_at(message: impl Into<String>, position: Position) -> Self {
        Self::RuntimeError {
            message: message.into(),
            position: Some(position),
        }
    }

    /// Create a new IdentifierNotFound error
    pub fn identifier_not_found(name: impl Into<String>) -> Self {
        Self::IdentifierNotFound {
            name: name.into(),
            position: None,
        }
    }

    /// Create a new IdentifierNotFound error with position
    pub fn identifier_not_found_at(name: impl Into<String>, position: Position) -> Self {
        Self::IdentifierNotFound {
            name: name.into(),
            position: Some(position),
        }
    }

    /// Create a new DivisionByZero error
    pub fn division_by_zero() -> Self {
        Self::DivisionByZero { position: None }
    }

    /// Create a new DivisionByZero error with position
    pub fn division_by_zero_at(position: Position) -> Self {
        Self::DivisionByZero {
            position: Some(position),
        }
    }

    /// Create a new InvalidFunctionCall error
    pub fn invalid_function_call(message: impl Into<String>) -> Self {
        Self::InvalidFunctionCall {
            message: message.into(),
            position: None,
        }
    }

    /// Create a new InvalidIndex error
    pub fn invalid_index(message: impl Into<String>) -> Self {
        Self::InvalidIndex {
            message: message.into(),
            position: None,
        }
    }
}

impl fmt::Display for EvaluatorError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvaluatorError::TypeError { message, position } => {
                if let Some(pos) = position {
                    write!(f, "Type error at line {}, column {}: {}", pos.line, pos.column, message)
                } else {
                    write!(f, "Type error: {}", message)
                }
            }
            EvaluatorError::RuntimeError { message, position } => {
                if let Some(pos) = position {
                    write!(f, "Runtime error at line {}, column {}: {}", pos.line, pos.column, message)
                } else {
                    write!(f, "Runtime error: {}", message)
                }
            }
            EvaluatorError::IdentifierNotFound { name, position } => {
                if let Some(pos) = position {
                    write!(f, "Identifier '{}' not found at line {}, column {}", name, pos.line, pos.column)
                } else {
                    write!(f, "Identifier '{}' not found", name)
                }
            }
            EvaluatorError::DivisionByZero { position } => {
                if let Some(pos) = position {
                    write!(f, "Division by zero at line {}, column {}", pos.line, pos.column)
                } else {
                    write!(f, "Division by zero")
                }
            }
            EvaluatorError::InvalidFunctionCall { message, position } => {
                if let Some(pos) = position {
                    write!(f, "Invalid function call at line {}, column {}: {}", pos.line, pos.column, message)
                } else {
                    write!(f, "Invalid function call: {}", message)
                }
            }
            EvaluatorError::InvalidIndex { message, position } => {
                if let Some(pos) = position {
                    write!(f, "Invalid index at line {}, column {}: {}", pos.line, pos.column, message)
                } else {
                    write!(f, "Invalid index: {}", message)
                }
            }
            EvaluatorError::ParseError(err) => write!(f, "Parse error: {}", err),
            EvaluatorError::LexerError(err) => write!(f, "Lexer error: {}", err),
        }
    }
}

impl std::error::Error for EvaluatorError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            EvaluatorError::ParseError(err) => Some(err),
            EvaluatorError::LexerError(err) => Some(err),
            _ => None,
        }
    }
}

// From implementations for automatic error conversion
impl From<crate::parser::ParserError> for EvaluatorError {
    fn from(err: crate::parser::ParserError) -> Self {
        EvaluatorError::ParseError(err)
    }
}

impl From<crate::lexer::LexerError> for EvaluatorError {
    fn from(err: crate::lexer::LexerError) -> Self {
        EvaluatorError::LexerError(err)
    }
}

// For backward compatibility during migration
impl From<String> for EvaluatorError {
    fn from(message: String) -> Self {
        EvaluatorError::RuntimeError {
            message,
            position: None,
        }
    }
}

impl From<&str> for EvaluatorError {
    fn from(message: &str) -> Self {
        EvaluatorError::RuntimeError {
            message: message.to_string(),
            position: None,
        }
    }
}

// From implementation for converting EvaluatorError to String (for REPL compatibility)
impl From<EvaluatorError> for String {
    fn from(err: EvaluatorError) -> Self {
        err.to_string()
    }
}