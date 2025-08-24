use crate::ast;
use crate::ast::{Expression, InfixExpression};
use crate::lexer::Lexer;
use crate::parser::{Parser, Result};
use crate::token::Token;
use smallvec::smallvec;
use std::collections::BTreeMap;

#[test]
fn parsing_array_literals() -> Result<()> {
    let lexer = Lexer::new("[1, 2 * 2, 3 + 3]".chars().collect());
    let mut parser = Parser::new(lexer)?;
    let program = parser.parse()?;
    let expected: Vec<ast::Statement> = vec![ast::Statement::Expression(Expression::Array(
        Box::new(ast::ArrayLiteral {
            elements: smallvec![
                Box::new(Expression::Int(1)),
                Box::new(Expression::Infix(ast::InfixExpression {
                    left: Box::new(Expression::Int(2)),
                    operator: Token::Asterisk,
                    right: Box::new(Expression::Int(2)),
                })),
                Box::new(Expression::Infix(ast::InfixExpression {
                    left: Box::new(Expression::Int(3)),
                    operator: Token::Plus,
                    right: Box::new(Expression::Int(3)),
                })),
            ],
        }),
    ))];
    assert_eq!(&program.statements.to_vec(), &expected);
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
            left: Box::new(Expression::Identifier("myArray".into())),
        },
    ))];
    assert_eq!(&program.statements.to_vec(), &expected);
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
                (Expression::String("one".into()), Expression::Int(1)),
                (Expression::String("two".into()), Expression::Int(2)),
                (Expression::String("three".into()), Expression::Int(3)),
            ]),
        },
    ))];

    assert_eq!(&program.statements.to_vec(), &expected);

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

    assert_eq!(&program.statements.to_vec(), &expected);

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
                    Expression::String("one".into()),
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Int(0)),
                        operator: Token::Plus,
                        right: Box::new(Expression::Int(1)),
                    }),
                ),
                (
                    Expression::String("two".into()),
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Int(10)),
                        operator: Token::Minus,
                        right: Box::new(Expression::Int(8)),
                    }),
                ),
                (
                    Expression::String("three".into()),
                    Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Int(15)),
                        operator: Token::Slash,
                        right: Box::new(Expression::Int(5)),
                    }),
                ),
            ]),
        },
    ))];

    assert_eq!(&program.statements.to_vec(), &expected);

    Ok(())
}
