use clap::{Arg, Command};
use monkey_interpreter_rs::{evaluator, lexer::Lexer, object::environment::Environment, parser::Parser, repl};
use std::fs;

fn main() -> Result<(), String> {
    let matches = Command::new("monkey")
        .version(env!("CARGO_PKG_VERSION"))
        .author("samidarko")
        .about("A complete interpreter for the Monkey programming language")
        .arg(
            Arg::new("file")
                .help("Monkey source file to execute")
                .value_name("FILE")
                .index(1),
        )
        .arg(
            Arg::new("basic-repl")
                .long("basic-repl")
                .action(clap::ArgAction::SetTrue)
                .help("Use basic REPL instead of enhanced REPL"),
        )
        .get_matches();

    if let Some(file_path) = matches.get_one::<String>("file") {
        // Execute file mode
        execute_file(file_path)
    } else {
        // REPL mode
        let exit_code = if matches.get_flag("basic-repl") {
            // Use basic REPL
            println!("Hello! This is the Monkey programming language!");
            println!("Feel free to type in commands");
            repl::repl()?
        } else {
            // Use enhanced REPL
            repl::enhanced_repl()?
        };
        
        if exit_code != 0 {
            std::process::exit(exit_code);
        }
        Ok(())
    }
}

fn execute_file(file_path: &str) -> Result<(), String> {
    // Read the file
    let source = fs::read_to_string(file_path)
        .map_err(|e| format!("Error reading file '{}': {}", file_path, e))?;

    // Parse the source code
    let lexer = Lexer::new(source.chars().collect());
    let mut parser = Parser::new(lexer).map_err(|e| format!("Parser error: {}", e))?;
    let program = parser.parse().map_err(|e| format!("Parse error: {}", e))?;

    // Evaluate the program
    let env = Environment::new();
    match evaluator::eval_program(program, env) {
        Ok(result) => {
            // Only print non-null results
            if !matches!(result, monkey_interpreter_rs::object::Object::Null) {
                println!("{}", result);
            }
            Ok(())
        }
        Err(error_msg) => Err(format!("Runtime error: {}", error_msg)),
    }
}
