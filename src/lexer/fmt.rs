use std::error::Error;
use std::fmt::{Display, Formatter, Result};

use super::LexerError;

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            LexerError::IllegalInteger(err, pos) => {
                writeln!(
                    f,
                    "illegal integer number at line {}, column {}: {}",
                    pos.line, pos.column, err
                )?;
                write!(f, "  Error occurred while parsing integer")
            }
            LexerError::IllegalCharacter(ch, pos) => {
                writeln!(
                    f,
                    "illegal character '{}' at line {}, column {}",
                    ch, pos.line, pos.column
                )?;
                write!(f, "  Unexpected character in source code")
            }
        }
    }
}

impl Error for LexerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            LexerError::IllegalInteger(err, _) => Some(err),
            LexerError::IllegalCharacter(_, _) => None,
        }
    }
}
