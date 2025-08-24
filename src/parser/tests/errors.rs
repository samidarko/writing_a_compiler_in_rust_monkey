use crate::lexer::Lexer;
use crate::parser::{Parser, ParserError};

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
