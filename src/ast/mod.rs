use crate::token::Token;

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
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Let(LetStatement),
    Return(ReturnStatement),
    Block(BlockStatement),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStatement {
    pub value: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

// Expressions
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    Boolean(bool), // TODO create a BooleanExpression struct? Or maybe reuse Token::True & Token::False?
    Int(isize), // TODO create a IntExpression struct? should we change to usize because of Minus prefix?
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExpression {
    pub(crate) operator: Token,
    pub(crate) right: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InfixExpression {
    pub(crate) left: Box<Expression>,
    pub(crate) operator: Token,
    pub(crate) right: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpression {
    pub(crate) condition: Box<Expression>,
    pub(crate) consequence: BlockStatement,
    pub(crate) alternative: Option<BlockStatement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionLiteral {
    pub(crate) parameters: Vec<String>, // Vec<Identifier> ?
    pub(crate) body: BlockStatement,
}

#[derive(Clone, Debug, PartialEq)]
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
