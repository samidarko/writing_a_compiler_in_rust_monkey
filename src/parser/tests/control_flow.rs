use crate::ast;
use crate::lexer::Lexer;
use crate::parser::{Parser, Result};
use smallvec::smallvec;

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
                consequence: Box::new(ast::BlockStatement {
                    statements: smallvec![Box::new(ast::Statement::Expression(
                        ast::Expression::Int(1)
                    ))],
                }),
                alternative: None,
            },
        ),
        (
            "if (false) { 0 } else { 1 }",
            ast::IfExpression {
                condition: Box::new(ast::Expression::Boolean(false)),
                consequence: Box::new(ast::BlockStatement {
                    statements: smallvec![Box::new(ast::Statement::Expression(
                        ast::Expression::Int(0)
                    ))],
                }),
                alternative: Some(Box::new(ast::BlockStatement {
                    statements: smallvec![Box::new(ast::Statement::Expression(
                        ast::Expression::Int(1)
                    ))],
                })),
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
fn for_loop_expression() -> Result<()> {
    let mut lexer: Lexer;
    let mut parser: Parser;
    let mut program: ast::Program;

    let tests = vec![(
        "for (x in arr) { x }",
        ast::ForExpression {
            variable: "x".into(),
            collection: Box::new(ast::Expression::Identifier("arr".into())),
            body: Box::new(ast::BlockStatement {
                statements: smallvec![Box::new(ast::Statement::Expression(
                    ast::Expression::Identifier("x".into())
                ))],
            }),
        },
    )];

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
        let expected = ast::Statement::Expression(ast::Expression::For(test.1));
        assert_eq!(statement, &expected);
    }
    Ok(())
}
