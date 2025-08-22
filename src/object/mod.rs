pub mod environment;
mod fmt;

use crate::ast;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::mem;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Object {
    Int(isize),
    Boolean(bool),
    String(String),
    Null,
    Return(ReturnValue),
    Function(Function),
    Builtin(BuiltinFunction),
    Array(ArrayLiteral),
    Error(String),
    Hash(HashLiteral),
    Exit(isize),
}

impl Object {
    pub fn is_same_variant(&self, other: &Object) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

// TODO check Object value is not Return
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub(crate) parameters: Vec<String>,
    pub(crate) body: ast::BlockStatement,
    pub(crate) env: environment::Env, // shared, not a by-value clone
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        // Note: we exclude env from comparison
        self.parameters == other.parameters && self.body == other.body
    }
}

impl Eq for Function {}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Function {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Note: we exclude env from comparison
        match self.parameters.cmp(&other.parameters) {
            std::cmp::Ordering::Equal => self.body.cmp(&other.body),
            other => other,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BuiltinFunction {
    pub f: fn(&[Object]) -> Object,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArrayLiteral {
    pub elements: Vec<Object>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HashLiteral {
    pub(crate) pairs: BTreeMap<Object, Object>,
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
