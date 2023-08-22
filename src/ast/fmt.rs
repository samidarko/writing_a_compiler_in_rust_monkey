use std::fmt::{Display, Formatter, Result};

use super::*;

impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Node::Statement(statement) => statement.fmt(f),
            Node::Expression(expression) => expression.fmt(f),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for stmt in &self.statements {
            stmt.fmt(f)?;
        }

        Ok(())
    }
}

// Statements
impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Statement::Expression(expression) => expression.fmt(f),
            Statement::Let(let_statement) => let_statement.fmt(f),
            Statement::Return(return_statement) => return_statement.fmt(f),
            Statement::Block(block_statement) => block_statement.fmt(f),
        }
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "return {};", self.value)
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for stmt in &self.statements {
            stmt.fmt(f)?;
        }

        Ok(())
    }
}

// Expressions

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(value) => value.fmt(f),
            Expression::Boolean(value) => value.fmt(f),
            Expression::Int(value) => value.fmt(f),
            Expression::Prefix(prefix_expression) => prefix_expression.fmt(f),
            Expression::Infix(infix_expression) => infix_expression.fmt(f),
            Expression::If(if_expression) => if_expression.fmt(f),
            Expression::Function(function_literal) => function_literal.fmt(f),
            Expression::Call(call_expression) => call_expression.fmt(f),
        }
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "if {} {{ {} }}", self.condition, self.consequence)?;
        if let Some(block_statement) = &self.alternative {
            write!(f, " else {{ {} }}", block_statement)?;
        }
        Ok(())
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fn({}) {{ {} }}", self.parameters.join(", "), self.body)
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let arguments = self
            .arguments
            .iter()
            .map(|s| format!("{}", s))
            .collect::<Vec<String>>();
        write!(f, "{}({})", self.function, arguments.join(", "))
    }
}
