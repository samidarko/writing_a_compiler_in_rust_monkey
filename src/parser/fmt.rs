use crate::parser::Error;
use std::fmt;
use std::fmt::Debug;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::LexerError(value) => write!(f, "LexerError({})", value),
            Error::UnexpectedToken(value) => write!(f, "LexerError({:?})", value),
            Error::UnexpectedInfix(value) => write!(f, "LexerError({})", value),
            Error::UnexpectedPrefix(value) => write!(f, "LexerError({})", value),
        }
    }
}
