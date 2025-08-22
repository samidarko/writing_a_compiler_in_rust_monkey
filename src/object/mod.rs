//! Runtime object system for the Monkey programming language.
//!
//! This module defines the runtime values (Objects) that result from evaluating
//! Monkey expressions. It also provides the environment system for variable scoping.
//!
//! # Object Types
//!
//! The [`Object`] enum represents all possible runtime values:
//! - Primitives: integers, booleans, strings, null
//! - Functions: user-defined functions with closures
//! - Built-ins: native functions implemented in Rust
//! - Collections: arrays and hash maps
//! - Control flow: return values and error objects
//!
//! # Environment System
//!
//! The [`environment`] module provides lexical scoping through environments
//! that maintain variable bindings and support nested scopes for closures.
//!
//! # Examples
//!
//! ```
//! use monkey_interpreter_rs::object::{Object, environment::Environment};
//!
//! // Create some objects
//! let number = Object::Int(42);
//! let text = Object::String("hello".to_string());
//! let bool_val = Object::Boolean(true);
//!
//! // Use an environment
//! let env = Environment::new();
//! env.borrow_mut().set("x", &number);
//! 
//! assert_eq!(env.borrow().get("x"), Some(number));
//! ```

pub mod environment;
mod fmt;

use crate::ast;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::mem;

/// Represents all possible runtime values in the Monkey language.
///
/// Objects are the result of evaluating expressions and can be stored in variables,
/// passed to functions, and returned from functions.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Object {
    /// Integer number
    Int(isize),
    /// Boolean value (true/false)
    Boolean(bool),
    /// String value
    String(String),
    /// Null value
    Null,
    /// Return value wrapper (used for control flow)
    Return(ReturnValue),
    /// User-defined function with closure
    Function(Function),
    /// Built-in function implemented in Rust
    Builtin(BuiltinFunction),
    /// Array of objects
    Array(ArrayLiteral),
    /// Error object containing error message
    Error(String),
    /// Hash map with object keys and values
    Hash(HashLiteral),
    /// Exit command with status code
    Exit(isize),
}

impl Object {
    /// Checks if two objects are of the same variant (type).
    ///
    /// This method compares the discriminant of enum variants, ignoring
    /// the actual values. Useful for type checking without value comparison.
    ///
    /// # Examples
    ///
    /// ```
    /// use monkey_interpreter_rs::object::Object;
    ///
    /// let a = Object::Int(5);
    /// let b = Object::Int(10);
    /// let c = Object::String("hello".to_string());
    ///
    /// assert!(a.is_same_variant(&b)); // Both are Int variants
    /// assert!(!a.is_same_variant(&c)); // Different variants
    /// ```
    pub fn is_same_variant(&self, other: &Object) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

impl ReturnValue {
    pub fn new(value: Object) -> Self {
        // Prevent double-wrapping of return values
        match value {
            Object::Return(inner_return) => inner_return,
            _ => ReturnValue {
                value: Box::new(value),
            },
        }
    }
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
