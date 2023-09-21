use std::fmt::{Display, Formatter, Result};

use super::LexerError;

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            LexerError::IllegalInteger(err) => write!(f, "illegal integer number: {}", err),
        }
    }
}
