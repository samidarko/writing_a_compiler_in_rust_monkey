pub mod environment;
mod fmt;

use crate::ast;
use std::mem;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Int(isize),
    Boolean(bool),
    Null,
    Return(ReturnValue),
    Function(Function),
}

impl Object {
    pub fn is_same_variant(&self, other: &Object) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

// TODO check Object value is not Return
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub(crate) parameters: Vec<String>,
    pub(crate) body: ast::BlockStatement,
    pub(crate) env: environment::Env, // shared, not a by-value clone
}

#[cfg(test)]
mod tests {
    use crate::object::Object;

    #[test]
    fn is_same_variant() {
        let tests = vec![
            (Object::Boolean(true), Object::Boolean(true), true),
            (Object::Boolean(false), Object::Boolean(true), true),
            (Object::Int(1), Object::Int(5), true),
            (Object::Null, Object::Null, true),
            (Object::Boolean(true), Object::Int(5), false),
            (Object::Int(1), Object::Null, false),
            (Object::Null, Object::Boolean(false), false),
        ];

        for test in tests {
            let result = test.0.is_same_variant(&test.1);
            assert_eq!(result, test.2);
        }
    }
}
