use crate::object::Object;
use std::fmt;

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(value) => value.fmt(f),
            Object::Boolean(value) => value.fmt(f),
            Object::Return(return_value) => return_value.value.fmt(f),
            Object::Null => write!(f, "null"),
            Object::Function(function) => {
                writeln!(f, "fn({}) {{", function.parameters.join(", "))?;
                writeln!(f, "{}", function.body)?;
                write!(f, "}}")
            }
        }
    }
}
