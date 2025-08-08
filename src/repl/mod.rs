use crate::lexer::Lexer;

use crate::ast::Node;
use crate::evaluator::eval;
use crate::object::environment::Environment;
use crate::parser::Parser;
use std::io::{self, Write};
use std::rc::Rc;

pub fn repl() -> Result<(), String> {
    let mut input = String::new();

    let mut stdout = io::stdout().lock();
    let environment = Environment::new();

    loop {
        write!(stdout, ">> ").map_err(|e| e.to_string())?;
        stdout.flush().map_err(|e| e.to_string())?;

        match io::stdin().read_line(&mut input) {
            Ok(_n) => {
                let lexer = Lexer::new(input.chars().collect());
                let mut parser = Parser::new(lexer).map_err(|e| e.to_string())?;
                let program = parser.parse().map_err(|e| e.to_string())?;
                let evaluated = eval(Node::Program(program), Rc::clone(&environment))?;
                writeln!(stdout, "{}", evaluated).map_err(|e| e.to_string())?;
                input.clear();
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
