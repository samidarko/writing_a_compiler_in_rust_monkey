use crate::token::Token;
use std::collections::BTreeMap;

mod fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Program),
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

// Statements
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Statement {
    Expression(Expression),
    Let(LetStatement),
    Return(ReturnStatement),
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

// Expressions
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Expression {
    Identifier(String),
    String(String),
    Boolean(bool), // Simple literal - no need for separate struct
    Null,
    Int(isize), // isize allows negative numbers; prefix minus is handled by PrefixExpression
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    Array(ArrayLiteral),
    Index(IndexExpression),
    Hash(HashLiteral),
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
