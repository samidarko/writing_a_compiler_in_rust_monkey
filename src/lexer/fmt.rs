use std::fmt::{Display, Formatter, Result};

use super::Error;

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Error::IllegalInteger(err) => write!(f, "illegal integer number: {}", err),
        }
    }
}
