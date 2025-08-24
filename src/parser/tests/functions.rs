use crate::ast;
use crate::lexer::Lexer;
use crate::parser::{Parser, Result};
use crate::token::Token;
use smallvec::smallvec;

#[test]
fn parse_function_literal() -> Result<()> {
    let mut lexer: Lexer;
    let mut parser: Parser;
    let mut program: ast::Program;

    let tests = vec![(
        "fn(x, y) { x + y; };",
        "fn(x, y) { (x + y) }",
        ast::Expression::Function(ast::FunctionLiteral {
            parameters: smallvec!["x".into(), "y".into()],
            body: Box::new(ast::BlockStatement {
                statements: smallvec![Box::new(ast::Statement::Expression(
                    ast::Expression::Infix(ast::InfixExpression {
                        left: Box::new(ast::Expression::Identifier("x".into())),
                        operator: Token::Plus,
                        right: Box::new(ast::Expression::Identifier("y".into())),
                    },)
                ))],
            }),
        }),
    )];

    for test in tests {
        lexer = Lexer::new(test.0.chars().collect());
        parser = Parser::new(lexer)?;
        program = parser.parse()?;

        let expected: Vec<ast::Statement> = vec![ast::Statement::Expression(test.2)];
        assert_eq!(&program.statements.to_vec(), &expected);
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
        ast::Expression::Call(Box::new(ast::CallExpression {
            function: Box::new(ast::Expression::Identifier("add".into())),
            arguments: smallvec![
                Box::new(ast::Expression::Int(1)),
                Box::new(ast::Expression::Infix(ast::InfixExpression {
                    left: Box::new(ast::Expression::Int(2)),
                    operator: Token::Asterisk,
                    right: Box::new(ast::Expression::Int(3)),
                })),
                Box::new(ast::Expression::Infix(ast::InfixExpression {
                    left: Box::new(ast::Expression::Int(4)),
                    operator: Token::Plus,
                    right: Box::new(ast::Expression::Int(5)),
                })),
            ],
        })),
    )];

    for test in tests {
        lexer = Lexer::new(test.0.chars().collect());
        parser = Parser::new(lexer)?;
        program = parser.parse()?;

        let expected: Vec<ast::Statement> = vec![ast::Statement::Expression(test.2)];
        assert_eq!(&program.statements.to_vec(), &expected);
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
            name: "counter".into(),
            value: ast::Expression::Function(ast::FunctionLiteral {
                parameters: smallvec!["x".into()],
                body: Box::new(ast::BlockStatement {
                    statements: smallvec![Box::new(ast::Statement::Expression(
                        ast::Expression::If(ast::IfExpression {
                            condition: Box::new(ast::Expression::Infix(ast::InfixExpression {
                                left: Box::new(ast::Expression::Identifier("x".into())),
                                operator: Token::Gt,
                                right: Box::new(ast::Expression::Int(100)),
                            })),
                            consequence: Box::new(ast::BlockStatement {
                                statements: smallvec![Box::new(ast::Statement::Return(
                                    ast::ReturnStatement {
                                        value: ast::Expression::Boolean(true),
                                    },
                                ))],
                            }),
                            alternative: Some(Box::new(ast::BlockStatement {
                                statements: smallvec![Box::new(ast::Statement::Expression(
                                    ast::Expression::Call(Box::new(ast::CallExpression {
                                        function: Box::new(ast::Expression::Identifier(
                                            "counter".into(),
                                        )),
                                        arguments: smallvec![Box::new(ast::Expression::Infix(
                                            ast::InfixExpression {
                                                left: Box::new(ast::Expression::Identifier(
                                                    "x".into(),
                                                )),
                                                operator: Token::Plus,
                                                right: Box::new(ast::Expression::Int(1)),
                                            },
                                        ))],
                                    })),
                                ))],
                            })),
                        },)
                    ))],
                }),
            }),
        },
    )];

    for test in tests {
        lexer = Lexer::new(test.0.chars().collect());
        parser = Parser::new(lexer)?;
        program = parser.parse()?;

        let expected: Vec<ast::Statement> = vec![ast::Statement::Let(test.2)];
        assert_eq!(&program.statements.to_vec(), &expected);
        // assert_eq!(program.statements.len(), 1);
        assert_eq!(format!("{}", program), test.1);
    }
    Ok(())
}
