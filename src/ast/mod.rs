//! Abstract Syntax Tree (AST) for the Monkey programming language.
//!
//! This module defines the AST node types that represent parsed Monkey programs.
//! The AST serves as the intermediate representation between parsing and evaluation.
//!
//! # Node Types
//!
//! - [`Node`] - Top-level node enum containing programs, statements, and expressions
//! - [`Statement`] - Executable statements (let, return, expression statements)  
//! - [`Expression`] - Value-producing expressions (literals, operators, function calls)
//!
//! # Examples
//!
//! ```
//! use monkey_interpreter_rs::ast::{Node, Program, Statement, Expression, LetStatement};
//!
//! // Represents: let x = 5;
//! let program = Program {
//!     statements: vec![
//!         Statement::Let(LetStatement {
//!             name: "x".to_string(),
//!             value: Expression::Int(5),
//!         })
//!     ]
//! };
//! ```

use crate::token::Token;
use std::collections::BTreeMap;

mod fmt;

/// Top-level AST node that can represent any part of a Monkey program.
#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    /// A statement node
    Statement(Statement),
    /// An expression node
    Expression(Expression),
    /// A complete program
    Program(Program),
}

/// A complete Monkey program consisting of a sequence of statements.
#[derive(Clone, Debug, PartialEq, Default)]
pub struct Program {
    /// The statements that make up this program
    pub statements: Vec<Statement>,
}

/// Statements are executable units that don't produce values.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Statement {
    /// An expression used as a statement
    Expression(Expression),
    /// Variable binding statement (`let x = value;`)
    Let(LetStatement),
    /// Return statement (`return value;`)
    Return(ReturnStatement),
    /// Block of statements (`{ stmt1; stmt2; }`)
    Block(BlockStatement),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ReturnStatement {
    pub value: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

/// Expressions are units that produce values when evaluated.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Expression {
    /// Variable or function name
    Identifier(String),
    /// String literal
    String(String),
    /// Boolean literal (true/false)
    Boolean(bool),
    /// Null literal
    Null,
    /// Integer literal
    Int(isize),
    /// Prefix expression (e.g., `-x`, `!condition`)
    Prefix(PrefixExpression),
    /// Infix expression (e.g., `a + b`, `x == y`)
    Infix(InfixExpression),
    /// Conditional expression (`if (condition) { ... } else { ... }`)
    If(IfExpression),
    /// Function literal (`fn(params) { body }`)
    Function(FunctionLiteral),
    /// Function call (`func(args)`)
    Call(CallExpression),
    /// Array literal (`[1, 2, 3]`)
    Array(ArrayLiteral),
    /// Index expression (`arr[0]`, `hash["key"]`)
    Index(IndexExpression),
    /// Hash map literal (`{"key": "value"}`)
    Hash(HashLiteral),
    /// Assignment expression (`x = value`)
    Assignment(AssignmentExpression),
    /// While loop expression (`while (condition) { body }`)
    While(WhileExpression),
    /// For loop expression (`for (variable in collection) { body }`)
    For(ForExpression),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PrefixExpression {
    pub(crate) operator: Token,
    pub(crate) right: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IndexExpression {
    pub(crate) left: Box<Expression>,
    pub(crate) index: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InfixExpression {
    pub(crate) left: Box<Expression>,
    pub(crate) operator: Token,
    pub(crate) right: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IfExpression {
    pub(crate) condition: Box<Expression>,
    pub(crate) consequence: BlockStatement,
    pub(crate) alternative: Option<BlockStatement>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FunctionLiteral {
    pub(crate) parameters: Vec<String>, // Parameter names (identifiers)
    pub(crate) body: BlockStatement,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ArrayLiteral {
    pub(crate) elements: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HashLiteral {
    pub(crate) pairs: BTreeMap<Expression, Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CallExpression {
    pub(crate) function: Box<Expression>, // Identifier or FunctionLiteral
    pub(crate) arguments: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AssignmentExpression {
    pub(crate) name: String,             // Variable name being assigned to
    pub(crate) value: Box<Expression>,   // Value being assigned
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct WhileExpression {
    pub(crate) condition: Box<Expression>,  // Loop condition
    pub(crate) body: BlockStatement,        // Loop body
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ForExpression {
    pub(crate) variable: String,            // Loop variable name
    pub(crate) collection: Box<Expression>, // Collection to iterate over
    pub(crate) body: BlockStatement,        // Loop body
}

// #[derive(Clone, Debug, PartialEq)]
// pub struct Identifier {
//     pub name: String,
//     // Could include source location information
// }

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, LetStatement, Program, Statement};

    #[test]
    fn string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                name: "x".to_string(),
                value: Expression::Identifier("y".to_string()),
            })],
        };
        assert_eq!(format!("{}", program), "let x = y;");
    }
}
