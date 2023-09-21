use crate::parser::{ParserError, UnexpectedToken};
use std::fmt;

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::LexerError(value) => write!(f, "ParserError::LexerError({})", value),
            ParserError::UnexpectedToken(value) => {
                write!(f, "ParserError::UnexpectedToken({:?})", value)
            }
            ParserError::UnexpectedInfix(value) => {
                write!(f, "ParserError::UnexpectedInfix({})", value)
            }
            ParserError::UnexpectedPrefix(value) => {
                write!(f, "ParserError::UnexpectedPrefix({})", value)
            }
        }
    }
}

impl fmt::Display for UnexpectedToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "UnexpectedToken {{ want: {}, got: {} }}",
            self.want, self.got
        )
    }
}
