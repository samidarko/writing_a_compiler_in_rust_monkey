use crate::parser::{ParserError, UnexpectedToken};
use std::error::Error;
use std::fmt;

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::LexerError(value) => {
                writeln!(f, "Lexer error:")?;
                write!(f, "{}", value)
            }
            ParserError::UnexpectedToken(unexpected_token) => {
                writeln!(
                    f,
                    "Parse error at line {}, column {}:",
                    unexpected_token.position.line, unexpected_token.position.column
                )?;
                write!(
                    f,
                    "  Expected {} but got {}",
                    unexpected_token.want, unexpected_token.got
                )
            }
            ParserError::UnexpectedInfix(token, pos) => {
                writeln!(
                    f,
                    "Parse error at line {}, column {}:",
                    pos.line, pos.column
                )?;
                write!(f, "  Unexpected infix operator '{}'", token)
            }
            ParserError::UnexpectedPrefix(token, pos) => {
                writeln!(
                    f,
                    "Parse error at line {}, column {}:",
                    pos.line, pos.column
                )?;
                write!(f, "  Unexpected prefix operator '{}'", token)
            }
        }
    }
}

impl Error for ParserError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ParserError::LexerError(err) => Some(err),
            _ => None,
        }
    }
}

impl From<crate::lexer::LexerError> for ParserError {
    fn from(err: crate::lexer::LexerError) -> Self {
        ParserError::LexerError(err)
    }
}

impl fmt::Display for UnexpectedToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Expected {} but got {} at line {}, column {}",
            self.want, self.got, self.position.line, self.position.column
        )
    }
}
