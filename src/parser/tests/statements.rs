use crate::lexer::Lexer;
use crate::parser::{Parser, Result};

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