use crate::ast;
use crate::ast::Expression;
use crate::lexer::Lexer;
use crate::parser::{Parser, Result};
use crate::token::Token;

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