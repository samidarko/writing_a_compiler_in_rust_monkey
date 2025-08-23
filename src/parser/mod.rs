//! Recursive descent parser for the Monkey programming language.
//!
//! This module implements a parser that converts a stream of tokens from the lexer
//! into an Abstract Syntax Tree (AST). The parser uses:
//!
//! - **Recursive Descent**: Top-down parsing approach for statements
//! - **Pratt Parsing**: Operator precedence parsing for expressions
//! - **Two-token Lookahead**: Current token and peek token for decision making
//! - **Comprehensive Error Reporting**: Position-aware error messages
//!
//! # Supported Language Features
//!
//! - Variable declarations (`let` statements)
//! - Return statements  
//! - Expression statements
//! - All expression types (literals, operators, function calls, etc.)
//! - Proper operator precedence and associativity
//! - Block statements with proper scoping
//!
//! # Examples
//!
//! ```
//! use monkey_interpreter_rs::{parser::Parser, lexer::Lexer};
//!
//! let input = "let x = 5 + 3; return x * 2;";
//! let lexer = Lexer::new(input.chars().collect());
//! let mut parser = Parser::new(lexer).unwrap();
//! let program = parser.parse().unwrap();
//!
//! assert_eq!(program.statements.len(), 2);
//! ```

mod fmt;
pub mod precedence;

use crate::ast::{Expression, HashLiteral};
use crate::lexer::Position;
use crate::token::Token;
use crate::{ast, lexer};
use std::collections::BTreeMap;
use std::{mem, result};

// Re-export for convenience
pub use precedence::{get_token_precedence, Precedence};


/// Details about an unexpected token encountered during parsing.
#[derive(Debug, Clone, PartialEq)]
pub struct UnexpectedToken {
    want: String,
    got: String,
    position: Position,
}

/// Errors that can occur during parsing.
#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    /// Error from the underlying lexer
    LexerError(lexer::LexerError),
    /// Expected one token but got another
    UnexpectedToken(UnexpectedToken),
    /// Unexpected infix operator
    UnexpectedInfix(Token, Position),
    /// Unexpected prefix operator
    UnexpectedPrefix(Token, Position),
}

/// Result type for parser operations.
pub type Result<T> = result::Result<T, ParserError>;

/// Recursive descent parser with Pratt parsing for expressions.
///
/// The parser maintains two-token lookahead (current and peek tokens) to make
/// parsing decisions. It uses recursive descent for statements and Pratt parsing
/// for expressions to handle operator precedence correctly.
///
/// # Examples
///
/// ```
/// use monkey_interpreter_rs::{parser::Parser, lexer::Lexer};
///
/// let input = "let add = fn(a, b) { a + b; };";
/// let lexer = Lexer::new(input.chars().collect());
/// let mut parser = Parser::new(lexer).unwrap();
/// let program = parser.parse().unwrap();
///
/// println!("Parsed {} statements", program.statements.len());
/// ```
pub struct Parser {
    lexer: lexer::Lexer,
    current: Token,
    peek: Token,
    current_position: Position,
}

impl Parser {
    /// Creates a new parser from a lexer.
    ///
    /// The constructor reads the first two tokens to initialize the current
    /// and peek token state required for two-token lookahead.
    ///
    /// # Arguments
    ///
    /// * `lexer` - The lexer to read tokens from
    ///
    /// # Returns
    ///
    /// A new parser instance, or an error if the initial tokens can't be read.
    ///
    /// # Examples
    ///
    /// ```
    /// use monkey_interpreter_rs::{parser::Parser, lexer::Lexer};
    ///
    /// let lexer = Lexer::new("let x = 5;".chars().collect());
    /// let parser = Parser::new(lexer).unwrap();
    /// ```
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
        get_token_precedence(&self.current)
    }

    fn peek_precedence(&self) -> Precedence {
        get_token_precedence(&self.peek)
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
            Token::While => return self.parse_while_expression(),
            Token::For => return self.parse_for_expression(),
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
            Assign => self.parse_assignment_expression(left),
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
    
    fn parse_while_expression(&mut self) -> Result<ast::Expression> {
        self.expect_peek(Token::LParen)?;

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_current(Token::RParen)?;
        self.expect_current(Token::LBrace)?;

        // Parse the body of the while block
        let body = if let ast::Statement::Block(block) = self.parse_block_statement()? {
            block
        } else {
            return Err(ParserError::UnexpectedToken(UnexpectedToken {
                want: "while block statement".to_string(),
                got: format!("{}", &self.current),
                position: self.current_position.clone(),
            }));
        };

        Ok(ast::Expression::While(ast::WhileExpression {
            condition,
            body,
        }))
    }

    fn parse_for_expression(&mut self) -> Result<ast::Expression> {
        // Follow the exact same pattern as while_expression
        self.expect_peek(Token::LParen)?;
        self.next_token()?; // Advance to identifier
        
        // Parse variable name
        let variable = if let Token::Ident(name) = &self.current {
            name.clone()
        } else {
            return Err(ParserError::UnexpectedToken(UnexpectedToken {
                want: "identifier".to_string(),
                got: format!("{}", &self.current),
                position: self.current_position.clone(),
            }));
        };

        self.next_token()?; // pass Ident
        self.expect_current(Token::In)?;

        // Parse collection expression
        let collection = Box::new(self.parse_expression(Precedence::Lowest)?);
        
        // Follow exact same pattern as while parsing  
        self.expect_peek(Token::RParen)?;
        self.expect_current(Token::RParen)?;
        self.expect_current(Token::LBrace)?;

        // Parse the body of the for block
        let body = if let ast::Statement::Block(block) = self.parse_block_statement()? {
            block
        } else {
            return Err(ParserError::UnexpectedToken(UnexpectedToken {
                want: "for block statement".to_string(),
                got: format!("{}", &self.current),
                position: self.current_position.clone(),
            }));
        };

        Ok(ast::Expression::For(ast::ForExpression {
            variable,
            collection,
            body,
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
        let mut parameters = vec![];

        if self.peek == Token::RParen {
            self.next_token()?;
            return Ok(parameters);
        }

        self.next_token()?;
        // Function parameters must be identifiers
        match &self.current {
            Token::Ident(name) => parameters.push(name.clone()),
            _ => {
                return Err(ParserError::UnexpectedToken(UnexpectedToken {
                    want: "identifier".to_string(),
                    got: format!("{}", self.current),
                    position: self.current_position.clone(),
                }))
            }
        }

        while self.peek == Token::Comma {
            self.next_token()?;
            self.next_token()?;
            match &self.current {
                Token::Ident(name) => parameters.push(name.clone()),
                _ => {
                    return Err(ParserError::UnexpectedToken(UnexpectedToken {
                        want: "identifier".to_string(),
                        got: format!("{}", self.current),
                        position: self.current_position.clone(),
                    }))
                }
            }
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
    
    fn parse_assignment_expression(&mut self, left: Expression) -> Result<Expression> {
        // The left side must be an identifier
        if let Expression::Identifier(name) = left {
            self.next_token()?; // consume '='
            // For right associativity, use Assignment precedence - 1, but since Assignment is already low, use Lowest
            let value = Box::new(self.parse_expression(Precedence::Assignment)?);
            Ok(Expression::Assignment(ast::AssignmentExpression { name, value }))
        } else {
            Err(ParserError::UnexpectedInfix(
                Token::Assign,
                self.current_position.clone(),
            ))
        }
    }

    /// Parses the entire input and returns a Program AST.
    ///
    /// This is the main entry point for parsing. It continues parsing statements
    /// until it reaches the end of file token, building up a complete program.
    ///
    /// # Returns
    ///
    /// A complete Program AST containing all parsed statements, or a parser error
    /// if invalid syntax is encountered.
    ///
    /// # Examples
    ///
    /// ```
    /// use monkey_interpreter_rs::{parser::Parser, lexer::Lexer};
    ///
    /// let input = r#"
    ///     let x = 5;
    ///     let y = 10;
    ///     let add = fn(a, b) { return a + b; };
    ///     add(x, y);
    /// "#;
    /// let lexer = Lexer::new(input.chars().collect());
    /// let mut parser = Parser::new(lexer).unwrap();
    /// let program = parser.parse().unwrap();
    ///
    /// assert_eq!(program.statements.len(), 4);
    /// ```
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
mod tests;
