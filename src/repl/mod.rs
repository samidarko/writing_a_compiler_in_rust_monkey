//! Read-Eval-Print Loop (REPL) for the Monkey programming language.
//!
//! This module provides an interactive shell that allows users to enter Monkey
//! expressions and statements and see their results immediately. The REPL maintains
//! state between evaluations, so variables and functions defined in one line can
//! be used in subsequent lines.
//!
//! # Features
//!
//! - Interactive evaluation of Monkey code
//! - Persistent environment between evaluations  
//! - Comprehensive error reporting
//! - Support for exit commands with custom exit codes
//! - Enhanced REPL with history, tab completion, and better UX
//!
//! # Examples
//!
//! ```no_run
//! use monkey_interpreter_rs::repl::repl;
//!
//! // Start the REPL
//! let exit_code = repl().expect("REPL failed");
//! println!("REPL exited with code: {}", exit_code);
//! ```

pub mod enhanced;

// Re-export the enhanced REPL as the default
pub use enhanced::enhanced_repl;

use crate::lexer::Lexer;

use crate::ast::Node;
use crate::evaluator::eval;
use crate::object::environment::Environment;
use crate::parser::Parser;
use std::io::{self, Write};
use std::rc::Rc;

/// Starts the Monkey REPL (Read-Eval-Print Loop).
///
/// This function runs an interactive shell that:
/// 1. Reads user input from stdin
/// 2. Parses the input as Monkey code  
/// 3. Evaluates the parsed expressions/statements
/// 4. Prints the results to stdout
/// 5. Loops back to step 1
///
/// The REPL maintains a persistent environment, so variables and functions
/// defined in previous inputs remain available.
///
/// # Returns
///
/// Returns `Ok(exit_code)` when the user exits (via `exit()` builtin function),
/// where `exit_code` is the code passed to `exit()` (default 0).
/// Returns `Err(message)` if a fatal error occurs.
///
/// # Examples
///
/// ```no_run
/// use monkey_interpreter_rs::repl::repl;
///
/// match repl() {
///     Ok(0) => println!("Clean exit"),
///     Ok(code) => println!("Exited with code {}", code),
///     Err(msg) => eprintln!("Error: {}", msg),
/// }
/// ```
pub fn repl() -> Result<i32, String> {
    let mut input = String::new();

    let mut stdout = io::stdout().lock();
    let environment = Environment::new();

    loop {
        write!(stdout, ">> ").map_err(|e| e.to_string())?;
        stdout.flush().map_err(|e| e.to_string())?;

        match io::stdin().read_line(&mut input) {
            Ok(_n) => {
                let trimmed = input.trim();

                let lexer = Lexer::new(trimmed.chars().collect());
                let mut parser = Parser::new(lexer).map_err(|e| e.to_string())?;
                let program = parser.parse().map_err(|e| e.to_string())?;
                let evaluated = eval(Node::Program(program), Rc::clone(&environment))?;

                // Check if the evaluated result is an exit command
                if let crate::object::Object::Exit(code) = &evaluated {
                    writeln!(stdout, "Goodbye! (exit code: {})", code)
                        .map_err(|e| e.to_string())?;
                    return Ok(*code as i32);
                }

                writeln!(stdout, "{}", evaluated).map_err(|e| e.to_string())?;
                input.clear();
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
