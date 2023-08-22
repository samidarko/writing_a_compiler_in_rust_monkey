use crate::lexer::Lexer;

use crate::parser::Parser;
use crate::token::Token::EoF;
use std::io::{self, Write};

pub fn repl() -> Result<(), String> {
    let mut input = String::new();

    let mut stdout = io::stdout().lock();

    loop {
        write!(stdout, ">> ").map_err(|e| e.to_string())?;
        stdout.flush().map_err(|e| e.to_string())?;

        match io::stdin().read_line(&mut input) {
            Ok(_n) => {
                let lexer = Lexer::new(input.chars().collect());
                let mut parser = Parser::new(lexer).map_err(|e| e.to_string())?;
                let program = parser.parse().map_err(|e| e.to_string())?;
                write!(stdout, "{}\n", program).map_err(|e| e.to_string())?;
                input.clear();
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
