use crate::ast;
use crate::lexer::Lexer;
use crate::parser::{Parser, Result};
use crate::token::Token;

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