use crate::object::Object;
use std::fmt;

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Error(value) => value.fmt(f),
            Object::Int(value) => value.fmt(f),
            Object::Boolean(value) => value.fmt(f),
            Object::String(value) => value.fmt(f),
            Object::Builtin(_builtin_function) => write!(f, "builtin function call with ()"),
            Object::Return(return_value) => return_value.value.fmt(f),
            Object::Null => write!(f, "null"),
            Object::Function(function) => {
                writeln!(f, "fn({}) {{", function.parameters.join(", "))?;
                writeln!(f, "{}", function.body)?;
                write!(f, "}}")
            }
            Object::Array(array_literal) => {
                write!(f, "[")?;
                let elements = array_literal
                    .elements
                    .iter()
                    .map(|s| format!("{}", s))
                    .collect::<Vec<String>>();
                write!(f, "{}", elements.join(", "))?;
                write!(f, "]")
            }
            Object::Hash(hash_literal) => {
                write!(f, "{{")?;
                let elements = hash_literal
                    .pairs
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<String>>();
                write!(f, "{}", elements.join(", "))?;
                write!(f, "}}")
            }
            Object::Exit(code) => write!(f, "exit({})", code),
        }
    }
}
