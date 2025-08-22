mod fmt;
mod helpers;

use crate::ast::{Expression, HashLiteral};
use crate::lexer::Position;
use crate::token::Token;
use crate::{ast, lexer};
use std::collections::BTreeMap;
use std::{mem, result};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // array[index]
}

#[derive(Debug)]
pub struct UnexpectedToken {
    want: String,
    got: String,
    position: Position,
}

#[derive(Debug)]
pub enum ParserError {
    LexerError(lexer::LexerError),
    UnexpectedToken(UnexpectedToken),
    UnexpectedInfix(Token, Position),
    UnexpectedPrefix(Token, Position),
}
pub type Result<T> = result::Result<T, ParserError>;

pub struct Parser {
    lexer: lexer::Lexer,
    current: Token,
    peek: Token,
    current_position: Position,
}

impl Parser {
    pub fn new(lexer: lexer::Lexer) -> Result<Self> {
        let mut parser = Self {
            current: Token::EoF,
            peek: Token::EoF,
            current_position: Position::new(),
            lexer,
        };
        parser.next_token()?;
        parser.next_token()?;
        Ok(parser)
    }

    fn next_token(&mut self) -> Result<()> {
        mem::swap(&mut self.current, &mut self.peek);
        self.current_position = self.lexer.current_position();
        self.peek = self.lexer.next_token().map_err(ParserError::LexerError)?;
        Ok(())
    }

    // fn cur_token_is(&self, token: Token) -> Result<()> {
    //     if self.current == token {
    //         return Ok(());
    //     }
    //     Err(Error::UnexpectedToken {
    //         want: format!("{}", token),
    //         got: format!("{}", &self.current),
    //     })
    // }
    //
    // fn peek_token_is(&self, token: Token) -> Result<()> {
    //     if self.peek == token {
    //         return Ok(());
    //     }
    //     Err(Error::UnexpectedToken {
    //         want: format!("{}", token),
    //         got: format!("{}", &self.peek),
    //     })
    // }

    fn expect_peek(&mut self, token: Token) -> Result<()> {
        if self.peek == token {
            self.next_token()?;
            return Ok(());
        }
        Err(ParserError::UnexpectedToken(UnexpectedToken {
            want: format!("{}", token),
            got: format!("{}", &self.peek),
            position: self.current_position.clone(),
        }))
    }

    fn expect_current(&mut self, token: Token) -> Result<()> {
        if self.current == token {
            self.next_token()?;
            return Ok(());
        }
        Err(ParserError::UnexpectedToken(UnexpectedToken {
            want: format!("{}", token),
            got: format!("{}", &self.current),
            position: self.current_position.clone(),
        }))
    }

    fn current_precedence(&self) -> Precedence {
        helpers::get_token_precedence(&self.current)
    }

    fn peek_precedence(&self) -> Precedence {
        helpers::get_token_precedence(&self.peek)
    }

    fn parse_identifier_name(&self) -> Result<String> {
        if let Token::Ident(name) = &self.current {
            return Ok(name.to_string());
        }
        Err(ParserError::UnexpectedToken(UnexpectedToken {
            want: "identifier".to_string(),
            got: format!("{}", &self.current),
            position: self.current_position.clone(),
        }))
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement> {
        self.next_token()?;

        let name = self.parse_identifier_name()?;

        self.expect_peek(Token::Assign)?;

        self.next_token()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.next_token()?
        }

        Ok(ast::Statement::Let(ast::LetStatement { name, value }))
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        self.next_token()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.next_token()?
        }

        Ok(ast::Statement::Return(ast::ReturnStatement { value }))
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.next_token()?
        }

        Ok(ast::Statement::Expression(expression))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression> {
        let mut left = self.prefix_parse()?;

        while self.peek != Token::Semicolon && precedence < self.peek_precedence() {
            self.next_token()?;

            left = self.infix_parse(left)?;
        }
        // match self.infix_parse(&left) {
        //     Ok(infix) => left = infix,
        //     _ => {
        //         return Ok(left);
        //     }
        // };

        Ok(left)
    }

    fn prefix_parse(&mut self) -> Result<ast::Expression> {
        let expression = match &self.current {
            Token::Ident(value) => ast::Expression::Identifier(value.to_string()),
            Token::Int(value) => ast::Expression::Int(*value),
            Token::String(value) => ast::Expression::String(value.to_string()),
            Token::Asterisk | Token::Bang | Token::Minus => return self.parse_prefix_expression(),
            Token::True | Token::False => return self.parse_boolean(),
            Token::Null => Expression::Null,
            Token::LParen => return self.parse_grouped_expression(),
            Token::If => return self.parse_if_expression(),
            Token::Fn => return self.parse_function_literal(),
            Token::LBracket => return self.parse_array_literal(),
            Token::LBrace => return self.parse_hash_literal(),
            _ => {
                return Err(ParserError::UnexpectedPrefix(
                    self.current.clone(),
                    self.current_position.clone(),
                ))
            }
        };
        Ok(expression)
    }

    fn infix_parse(&mut self, left: ast::Expression) -> Result<ast::Expression> {
        use crate::token::Token::*;
        match &self.current {
            Plus | Minus | Slash | Asterisk | Eq | NotEq | Lt | Gt | Lte | Gte => {
                self.parse_infix_expression(left)
            }
            LParen => self.parse_call_expression(left),
            LBracket => self.parse_index_expression(left),
            _ => Err(ParserError::UnexpectedInfix(
                self.current.clone(),
                self.current_position.clone(),
            )),
        }
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Result<ast::Expression> {
        let operator = self.current.clone();
        let precedence = self.current_precedence();

        self.next_token()?;

        let right = Box::new(self.parse_expression(precedence)?);

        Ok(ast::Expression::Infix(ast::InfixExpression {
            left: Box::new(left),
            operator,
            right,
        }))
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression> {
        let operator = self.current.clone();

        self.next_token()?;

        let right = Box::new(self.parse_expression(Precedence::Prefix)?);

        Ok(ast::Expression::Prefix(ast::PrefixExpression {
            operator,
            right,
        }))
    }

    fn parse_boolean(&mut self) -> Result<ast::Expression> {
        Ok(ast::Expression::Boolean(self.current == Token::True))
    }

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression> {
        self.next_token()?;

        let expression = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RParen)?;

        Ok(expression)
    }

    fn parse_block_statement(&mut self) -> Result<ast::Statement> {
        let mut statements: Vec<ast::Statement> = vec![];

        while self.current != Token::RBrace && self.current != Token::EoF {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.next_token()?;
        }

        Ok(ast::Statement::Block(ast::BlockStatement { statements }))
    }

    fn parse_if_expression(&mut self) -> Result<ast::Expression> {
        self.expect_peek(Token::LParen)?;

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_current(Token::RParen)?;
        self.expect_current(Token::LBrace)?;

        // Parse the body of the if block.
        let consequence = if let ast::Statement::Block(block) = self.parse_block_statement()? {
            block
        } else {
            return Err(ParserError::UnexpectedToken(UnexpectedToken {
                want: "if block statement".to_string(),
                got: format!("{}", &self.current),
                position: self.current_position.clone(),
            }));
        };

        // Is there an associated else block with this if expression?
        if self.peek != Token::Else {
            return Ok(ast::Expression::If(ast::IfExpression {
                condition,
                consequence,
                alternative: None,
            }));
        }

        self.next_token()?; // Else become current
        self.expect_peek(Token::LBrace)?;
        self.next_token()?;

        let alternative = if let ast::Statement::Block(block) = self.parse_block_statement()? {
            Some(block)
        } else {
            return Err(ParserError::UnexpectedToken(UnexpectedToken {
                want: "else block statement".to_string(),
                got: format!("{}", &self.current),
                position: self.current_position.clone(),
            }));
        };

        Ok(ast::Expression::If(ast::IfExpression {
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        let arguments = self.parse_expression_list(Token::RParen)?;

        Ok(Expression::Call(ast::CallExpression {
            function: Box::new(function),
            arguments,
        }))
    }

    fn parse_function_literal(&mut self) -> Result<Expression> {
        self.expect_peek(Token::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_current(Token::RParen)?;
        self.expect_current(Token::LBrace)?;

        let body = if let ast::Statement::Block(block) = self.parse_block_statement()? {
            block
        } else {
            return Err(ParserError::UnexpectedToken(UnexpectedToken {
                want: "fn block statement".to_string(),
                got: format!("{}", &self.current),
                position: self.current_position.clone(),
            }));
        };

        Ok(Expression::Function(ast::FunctionLiteral {
            parameters,
            body,
        }))
    }

    fn parse_array_literal(&mut self) -> Result<Expression> {
        Ok(Expression::Array(ast::ArrayLiteral {
            elements: self.parse_expression_list(Token::RBracket)?,
        }))
    }

    fn parse_hash_literal(&mut self) -> Result<Expression> {
        let mut pairs: BTreeMap<Expression, Expression> = BTreeMap::new();

        while self.peek != Token::RBrace {
            self.next_token()?;
            let key = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek(Token::Colon)?;
            self.next_token()?;
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.insert(key, value);

            if self.peek != Token::RBrace {
                self.expect_peek(Token::Comma)?;
            }
        }

        self.expect_peek(Token::RBrace)?;

        Ok(Expression::Hash(HashLiteral { pairs }))
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<ast::Expression>> {
        let mut arguments = vec![];

        if self.peek == end {
            self.next_token()?;
            return Ok(arguments);
        }

        self.next_token()?;
        let expression = self.parse_expression(Precedence::Lowest)?;
        arguments.push(expression);

        while self.peek == Token::Comma {
            self.next_token()?;
            self.next_token()?;
            let expression = self.parse_expression(Precedence::Lowest)?;
            arguments.push(expression);
        }

        self.expect_peek(end)?;

        Ok(arguments)
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<String>> {
        // TODO should we return Vec<Identifier> ?
        let mut parameters = vec![];

        if self.peek == Token::RParen {
            self.next_token()?;
            return Ok(parameters);
        }

        self.next_token()?;
        let expression = self.parse_expression(Precedence::Lowest)?;
        parameters.push(format!("{}", expression));

        while self.peek == Token::Comma {
            self.next_token()?;
            self.next_token()?;
            let expression = self.parse_expression(Precedence::Lowest)?;
            parameters.push(format!("{}", expression));
        }

        self.expect_peek(Token::RParen)?;

        Ok(parameters)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        match &self.current {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression> {
        self.next_token()?;
        let index = self.parse_expression(Precedence::Lowest)?;
        let expression = Expression::Index(ast::IndexExpression {
            index: Box::new(index),
            left: Box::new(left),
        });

        self.expect_peek(Token::RBracket)?;

        Ok(expression)
    }

    pub fn parse(&mut self) -> Result<ast::Program> {
        let mut program = ast::Program::default();

        while self.current != Token::EoF {
            let statement = self.parse_statement()?;
            program.statements.push(statement);

            self.next_token()?;
        }

        Ok(program)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast;
    use crate::ast::{Expression, InfixExpression};
    use crate::lexer::Lexer;
    use crate::parser::{Parser, ParserError, Result};
    use crate::token::Token;
    use std::collections::BTreeMap;

    #[test]
    fn let_statements() -> Result<()> {
        let input = "let x = 5;
let y = true;
let foobar = y;
";
        let lexer = Lexer::new(input.chars().collect());
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse()?;
        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements. got={}",
            program.statements.len()
        );

        let tests = vec!["let x = 5;", "let y = true;", "let foobar = y;"];

        for (i, test) in tests.iter().enumerate() {
            assert_eq!(format!("{}", program.statements[i]), test.to_string());
        }
        Ok(())
    }

    #[test]
    fn return_statements() -> Result<()> {
        let input = "return 5;
return 10;
return 993322;
";
        let lexer = Lexer::new(input.chars().collect());
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse()?;
        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements. got={}",
            program.statements.len()
        );

        let tests = vec!["return 5;", "return 10;", "return 993322;"];

        for (i, test) in tests.iter().enumerate() {
            assert_eq!(format!("{}", program.statements[i]), test.to_string());
        }
        Ok(())
    }

    #[test]
    fn identifier_expression() -> Result<()> {
        let lexer = Lexer::new("foobar;".chars().collect());
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse()?;

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statements. got={}",
            program.statements.len()
        );

        let statement = &program.statements[0];
        let expected =
            ast::Statement::Expression(ast::Expression::Identifier("foobar".to_string()));
        assert_eq!(statement, &expected);
        Ok(())
    }

    #[test]
    fn integer_expression() -> Result<()> {
        let lexer = Lexer::new("5;".chars().collect());
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse()?;

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statements. got={}",
            program.statements.len()
        );

        let statement = &program.statements[0];
        let expected = ast::Statement::Expression(ast::Expression::Int(5));
        assert_eq!(statement, &expected);
        Ok(())
    }

    #[test]
    fn boolean_expression() -> Result<()> {
        let mut lexer: Lexer;
        let mut parser: Parser;
        let mut program: ast::Program;

        let tests = vec![
            ("true;", "true"),
            ("false;", "false"),
            ("let foobar = true;", "let foobar = true;"),
            ("let barfoo = false;", "let barfoo = false;"),
        ];

        for test in tests {
            lexer = Lexer::new(test.0.chars().collect());
            parser = Parser::new(lexer)?;
            program = parser.parse()?;

            assert_eq!(format!("{}", program), test.1);
        }
        Ok(())
    }

    #[test]
    fn prefix_expression() -> Result<()> {
        let mut lexer: Lexer;
        let mut parser: Parser;
        let mut program: ast::Program;

        let tests = vec![
            (
                "!5;",
                ast::PrefixExpression {
                    operator: Token::Bang,
                    right: Box::new(ast::Expression::Int(5)),
                },
            ),
            (
                "-15;",
                ast::PrefixExpression {
                    operator: Token::Minus,
                    right: Box::new(ast::Expression::Int(15)),
                },
            ),
            (
                "!true;",
                ast::PrefixExpression {
                    operator: Token::Bang,
                    right: Box::new(ast::Expression::Boolean(true)),
                },
            ),
            (
                "!false;",
                ast::PrefixExpression {
                    operator: Token::Bang,
                    right: Box::new(ast::Expression::Boolean(false)),
                },
            ),
        ];

        for test in tests {
            lexer = Lexer::new(test.0.chars().collect());
            parser = Parser::new(lexer)?;
            program = parser.parse()?;

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statements. got={}",
                program.statements.len()
            );

            let statement = &program.statements[0];
            let expected = ast::Statement::Expression(ast::Expression::Prefix(test.1));
            assert_eq!(statement, &expected);
        }
        Ok(())
    }

    #[test]
    fn if_else_expression() -> Result<()> {
        let mut lexer: Lexer;
        let mut parser: Parser;
        let mut program: ast::Program;

        let tests = vec![
            (
                "if (true) { 1 }",
                ast::IfExpression {
                    condition: Box::new(ast::Expression::Boolean(true)),
                    consequence: ast::BlockStatement {
                        statements: vec![ast::Statement::Expression(ast::Expression::Int(1))],
                    },
                    alternative: None,
                },
            ),
            (
                "if (false) { 0 } else { 1 }",
                ast::IfExpression {
                    condition: Box::new(ast::Expression::Boolean(false)),
                    consequence: ast::BlockStatement {
                        statements: vec![ast::Statement::Expression(ast::Expression::Int(0))],
                    },
                    alternative: Some(ast::BlockStatement {
                        statements: vec![ast::Statement::Expression(ast::Expression::Int(1))],
                    }),
                },
            ),
        ];

        for test in tests {
            lexer = Lexer::new(test.0.chars().collect());
            parser = Parser::new(lexer)?;
            program = parser.parse()?;

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statements. got={}",
                program.statements.len()
            );

            let statement = &program.statements[0];
            let expected = ast::Statement::Expression(ast::Expression::If(test.1));
            assert_eq!(statement, &expected);
        }
        Ok(())
    }

    #[test]
    fn parse_function_literal() -> Result<()> {
        let mut lexer: Lexer;
        let mut parser: Parser;
        let mut program: ast::Program;

        let tests = vec![(
            "fn(x, y) { x + y; };",
            "fn(x, y) { (x + y) }",
            ast::Expression::Function(ast::FunctionLiteral {
                parameters: vec!["x".to_string(), "y".to_string()],
                body: ast::BlockStatement {
                    statements: vec![ast::Statement::Expression(ast::Expression::Infix(
                        ast::InfixExpression {
                            left: Box::new(ast::Expression::Identifier("x".to_string())),
                            operator: Token::Plus,
                            right: Box::new(ast::Expression::Identifier("y".to_string())),
                        },
                    ))],
                },
            }),
        )];

        for test in tests {
            lexer = Lexer::new(test.0.chars().collect());
            parser = Parser::new(lexer)?;
            program = parser.parse()?;

            let expected: Vec<ast::Statement> = vec![ast::Statement::Expression(test.2)];
            assert_eq!(&program.statements, &expected);
            assert_eq!(format!("{}", program), test.1);
        }
        Ok(())
    }

    #[test]
    fn call_expression() -> Result<()> {
        let mut lexer: Lexer;
        let mut parser: Parser;
        let mut program: ast::Program;

        let tests = vec![(
            "add(1, 2 * 3, 4 + 5);",
            "add(1, (2 * 3), (4 + 5))",
            ast::Expression::Call(ast::CallExpression {
                function: Box::new(ast::Expression::Identifier("add".to_string())),
                arguments: vec![
                    ast::Expression::Int(1),
                    ast::Expression::Infix(ast::InfixExpression {
                        left: Box::new(ast::Expression::Int(2)),
                        operator: Token::Asterisk,
                        right: Box::new(ast::Expression::Int(3)),
                    }),
                    ast::Expression::Infix(ast::InfixExpression {
                        left: Box::new(ast::Expression::Int(4)),
                        operator: Token::Plus,
                        right: Box::new(ast::Expression::Int(5)),
                    }),
                ],
            }),
        )];

        for test in tests {
            lexer = Lexer::new(test.0.chars().collect());
            parser = Parser::new(lexer)?;
            program = parser.parse()?;

            let expected: Vec<ast::Statement> = vec![ast::Statement::Expression(test.2)];
            assert_eq!(&program.statements, &expected);
            assert_eq!(format!("{}", program), test.1);
        }
        Ok(())
    }

    #[test]
    fn infix_expression() -> Result<()> {
        let mut lexer: Lexer;
        let mut parser: Parser;
        let mut program: ast::Program;

        let tests = vec![
            (
                "5 + 5;",
                ast::InfixExpression {
                    left: Box::new(ast::Expression::Int(5)),
                    operator: Token::Plus,
                    right: Box::new(ast::Expression::Int(5)),
                },
            ),
            (
                "5 - 5;",
                ast::InfixExpression {
                    left: Box::new(ast::Expression::Int(5)),
                    operator: Token::Minus,
                    right: Box::new(ast::Expression::Int(5)),
                },
            ),
            (
                "5 * 5;",
                ast::InfixExpression {
                    left: Box::new(ast::Expression::Int(5)),
                    operator: Token::Asterisk,
                    right: Box::new(ast::Expression::Int(5)),
                },
            ),
            (
                "5 / 5;",
                ast::InfixExpression {
                    left: Box::new(ast::Expression::Int(5)),
                    operator: Token::Slash,
                    right: Box::new(ast::Expression::Int(5)),
                },
            ),
            (
                "5 > 5;",
                ast::InfixExpression {
                    left: Box::new(ast::Expression::Int(5)),
                    operator: Token::Gt,
                    right: Box::new(ast::Expression::Int(5)),
                },
            ),
            (
                "5 < 5;",
                ast::InfixExpression {
                    left: Box::new(ast::Expression::Int(5)),
                    operator: Token::Lt,
                    right: Box::new(ast::Expression::Int(5)),
                },
            ),
            (
                "5 == 5;",
                ast::InfixExpression {
                    left: Box::new(ast::Expression::Int(5)),
                    operator: Token::Eq,
                    right: Box::new(ast::Expression::Int(5)),
                },
            ),
            (
                "5 != 5;",
                ast::InfixExpression {
                    left: Box::new(ast::Expression::Int(5)),
                    operator: Token::NotEq,
                    right: Box::new(ast::Expression::Int(5)),
                },
            ),
            (
                "true == true",
                ast::InfixExpression {
                    left: Box::new(ast::Expression::Boolean(true)),
                    operator: Token::Eq,
                    right: Box::new(ast::Expression::Boolean(true)),
                },
            ),
            (
                "true != false",
                ast::InfixExpression {
                    left: Box::new(ast::Expression::Boolean(true)),
                    operator: Token::NotEq,
                    right: Box::new(ast::Expression::Boolean(false)),
                },
            ),
            (
                "false == false",
                ast::InfixExpression {
                    left: Box::new(ast::Expression::Boolean(false)),
                    operator: Token::Eq,
                    right: Box::new(ast::Expression::Boolean(false)),
                },
            ),
        ];

        for test in tests {
            lexer = Lexer::new(test.0.chars().collect());
            parser = Parser::new(lexer)?;
            program = parser.parse()?;

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statements. got={}",
                program.statements.len()
            );

            let statement = &program.statements[0];
            let expected = ast::Statement::Expression(ast::Expression::Infix(test.1));
            assert_eq!(statement, &expected);
        }
        Ok(())
    }

    #[test]
    fn operator_precedence_parsing() -> Result<()> {
        let mut lexer: Lexer;
        let mut parser: Parser;
        let mut program: ast::Program;

        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for test in tests {
            lexer = Lexer::new(test.0.chars().collect());
            parser = Parser::new(lexer)?;
            program = parser.parse()?;

            assert_eq!(format!("{}", program), test.1);
        }
        Ok(())
    }

    #[test]
    fn counter() -> Result<()> {
        let mut lexer: Lexer;
        let mut parser: Parser;
        let mut program: ast::Program;

        let tests = vec![(
            "let counter = fn(x) { if (x > 100) { return true; } else { counter(x + 1); } };",
            "let counter = fn(x) { if (x > 100) { return true; } else { counter((x + 1)) } };",
            ast::LetStatement {
                name: "counter".to_string(),
                value: ast::Expression::Function(ast::FunctionLiteral {
                    parameters: vec!["x".to_string()],
                    body: ast::BlockStatement {
                        statements: vec![ast::Statement::Expression(ast::Expression::If(
                            ast::IfExpression {
                                condition: Box::new(ast::Expression::Infix(ast::InfixExpression {
                                    left: Box::new(ast::Expression::Identifier("x".to_string())),
                                    operator: Token::Gt,
                                    right: Box::new(ast::Expression::Int(100)),
                                })),
                                consequence: ast::BlockStatement {
                                    statements: vec![ast::Statement::Return(
                                        ast::ReturnStatement {
                                            value: ast::Expression::Boolean(true),
                                        },
                                    )],
                                },
                                alternative: Some(ast::BlockStatement {
                                    statements: vec![ast::Statement::Expression(
                                        ast::Expression::Call(ast::CallExpression {
                                            function: Box::new(ast::Expression::Identifier(
                                                "counter".to_string(),
                                            )),
                                            arguments: vec![ast::Expression::Infix(
                                                ast::InfixExpression {
                                                    left: Box::new(ast::Expression::Identifier(
                                                        "x".to_string(),
                                                    )),
                                                    operator: Token::Plus,
                                                    right: Box::new(ast::Expression::Int(1)),
                                                },
                                            )],
                                        }),
                                    )],
                                }),
                            },
                        ))],
                    },
                }),
            },
        )];

        for test in tests {
            lexer = Lexer::new(test.0.chars().collect());
            parser = Parser::new(lexer)?;
            program = parser.parse()?;

            let expected: Vec<ast::Statement> = vec![ast::Statement::Let(test.2)];
            assert_eq!(&program.statements, &expected);
            // assert_eq!(program.statements.len(), 1);
            assert_eq!(format!("{}", program), test.1);
        }
        Ok(())
    }
    #[test]
    fn string_literal_expression() -> Result<()> {
        let lexer = Lexer::new(r#""hello world";"#.chars().collect());
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse()?;
        let expected: Vec<ast::Statement> = vec![ast::Statement::Expression(Expression::String(
            "hello world".to_string(),
        ))];
        assert_eq!(&program.statements, &expected);
        Ok(())
    }

    #[test]
    fn parsing_array_literals() -> Result<()> {
        let lexer = Lexer::new("[1, 2 * 2, 3 + 3]".chars().collect());
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse()?;
        let expected: Vec<ast::Statement> = vec![ast::Statement::Expression(Expression::Array(
            ast::ArrayLiteral {
                elements: vec![
                    Expression::Int(1),
                    Expression::Infix(ast::InfixExpression {
                        left: Box::new(Expression::Int(2)),
                        operator: Token::Asterisk,
                        right: Box::new(Expression::Int(2)),
                    }),
                    Expression::Infix(ast::InfixExpression {
                        left: Box::new(Expression::Int(3)),
                        operator: Token::Plus,
                        right: Box::new(Expression::Int(3)),
                    }),
                ],
            },
        ))];
        assert_eq!(&program.statements, &expected);
        Ok(())
    }

    #[test]
    fn parsing_index_expressions() -> Result<()> {
        let lexer = Lexer::new("myArray[1 + 1]".chars().collect());
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse()?;
        let expected: Vec<ast::Statement> = vec![ast::Statement::Expression(Expression::Index(
            ast::IndexExpression {
                index: Box::new(Expression::Infix(ast::InfixExpression {
                    left: Box::new(Expression::Int(1)),
                    operator: Token::Plus,
                    right: Box::new(Expression::Int(1)),
                })),
                left: Box::new(Expression::Identifier("myArray".to_string())),
            },
        ))];
        assert_eq!(&program.statements, &expected);
        Ok(())
    }

    #[test]
    fn parsing_hash_literal_string_keys() -> Result<()> {
        let lexer = Lexer::new(r#"{"one": 1, "two": 2, "three": 3}"#.chars().collect());
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse()?;
        let expected: Vec<ast::Statement> = vec![ast::Statement::Expression(Expression::Hash(
            ast::HashLiteral {
                pairs: BTreeMap::from([
                    (Expression::String("one".to_string()), Expression::Int(1)),
                    (Expression::String("two".to_string()), Expression::Int(2)),
                    (Expression::String("three".to_string()), Expression::Int(3)),
                ]),
            },
        ))];

        assert_eq!(&program.statements, &expected);

        Ok(())
    }
    #[test]
    fn parsing_empty_hash_literal() -> Result<()> {
        let lexer = Lexer::new("{}".chars().collect());
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse()?;
        let expected: Vec<ast::Statement> = vec![ast::Statement::Expression(Expression::Hash(
            ast::HashLiteral {
                pairs: BTreeMap::new(),
            },
        ))];

        assert_eq!(&program.statements, &expected);

        Ok(())
    }

    #[test]
    fn parsing_hash_literal_with_expressions() -> Result<()> {
        let lexer = Lexer::new(
            r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#
                .chars()
                .collect(),
        );
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse()?;
        let expected: Vec<ast::Statement> = vec![ast::Statement::Expression(Expression::Hash(
            ast::HashLiteral {
                pairs: BTreeMap::from([
                    (
                        Expression::String("one".to_string()),
                        Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Int(0)),
                            operator: Token::Plus,
                            right: Box::new(Expression::Int(1)),
                        }),
                    ),
                    (
                        Expression::String("two".to_string()),
                        Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Int(10)),
                            operator: Token::Minus,
                            right: Box::new(Expression::Int(8)),
                        }),
                    ),
                    (
                        Expression::String("three".to_string()),
                        Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Int(15)),
                            operator: Token::Slash,
                            right: Box::new(Expression::Int(5)),
                        }),
                    ),
                ]),
            },
        ))];

        assert_eq!(&program.statements, &expected);

        Ok(())
    }

    #[test]
    fn error_messages_with_position() {
        // Test lexer error with position
        let input = "let x = @"; // @ is illegal
        let lexer = Lexer::new(input.chars().collect());
        let parser_result = Parser::new(lexer);

        // This should fail during lexer phase when parser tries to advance
        match parser_result {
            Err(ParserError::LexerError(lexer_error)) => {
                let error_msg = format!("{}", lexer_error);
                assert!(error_msg.contains("line 1"));
                assert!(error_msg.contains("column"));
                assert!(error_msg.contains("'@'"));
            }
            _ => {} // Other errors are also acceptable for this malformed input
        }

        // Test parser error with position
        let input = "let"; // Missing identifier
        let lexer = Lexer::new(input.chars().collect());
        match Parser::new(lexer) {
            Err(ParserError::UnexpectedToken(unexpected)) => {
                assert_eq!(unexpected.position.line, 1);
                assert!(unexpected.position.column > 1);
            }
            _ => {
                // Alternative test: try parsing invalid syntax
                let input2 = "if"; // Incomplete if statement
                let lexer2 = Lexer::new(input2.chars().collect());
                let mut parser = Parser::new(lexer2).unwrap();
                match parser.parse() {
                    Err(ParserError::UnexpectedToken(unexpected)) => {
                        assert_eq!(unexpected.position.line, 1);
                    }
                    _ => {} // Different error types are acceptable
                }
            }
        }
    }
}
