use std::fmt::{Display, Formatter, Result};

use super::*;

impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Node::Statement(statement) => statement.fmt(f),
            Node::Expression(expression) => expression.fmt(f),
            Node::Program(program) => program.fmt(f),
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
            Expression::String(value) => value.fmt(f),
            Expression::Boolean(value) => value.fmt(f),
            Expression::Int(value) => value.fmt(f),
            Expression::Prefix(prefix_expression) => prefix_expression.fmt(f),
            Expression::Infix(infix_expression) => infix_expression.fmt(f),
            Expression::If(if_expression) => if_expression.fmt(f),
            Expression::Function(function_literal) => function_literal.fmt(f),
            Expression::Call(call_expression) => call_expression.fmt(f),
            Expression::Array(array_literal) => array_literal.fmt(f),
            Expression::Index(index_expression) => index_expression.fmt(f),
            Expression::Hash(hash_literal) => hash_literal.fmt(f),
            Expression::Assignment(assignment_expression) => assignment_expression.fmt(f),
            Expression::While(while_expression) => while_expression.fmt(f),
            Expression::For(for_expression) => for_expression.fmt(f),
            Expression::Null => write!(f, "null"),
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

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let joined = self
            .elements
            .iter()
            .map(ToString::to_string) // Expression -> String via Display
            .collect::<Vec<_>>() // Vec<String>
            .join(", "); // now join works
        write!(f, "[{}]", joined)
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
impl Display for IndexExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "(")?;
        write!(f, "{}", self.left)?;
        write!(f, "[")?;
        write!(f, "{}", self.index)?;
        write!(f, "])")
    }
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{{")?;
        let pairs = self
            .pairs
            .iter()
            .map(|(key, value)| format!("{}:{}", key, value))
            .collect::<Vec<String>>();
        write!(f, "{}", pairs.join(", "))?;
        write!(f, "}}")
    }
}

impl Display for AssignmentExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

impl Display for WhileExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "while {} {{ {} }}", self.condition, self.body)
    }
}

impl Display for ForExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "for ({} in {}) {{ {} }}", self.variable, self.collection, self.body)
    }
}
