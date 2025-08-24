use crate::ast::Node;
use crate::evaluator::eval;
use crate::lexer::Lexer;
use crate::object::environment::Environment;
use crate::parser::Parser;
use rustyline::error::ReadlineError;
use rustyline::history::History;
use rustyline::DefaultEditor;

/// Enhanced REPL with history, tab completion, and better user experience.
pub fn enhanced_repl() -> Result<i32, String> {
    let mut rl = DefaultEditor::new().map_err(|e| format!("Failed to create editor: {}", e))?;
    let environment = Environment::new();

    // Load history from file if it exists
    let _ = rl.load_history(".monkey_history");

    println!("Welcome to the Monkey programming language!");
    println!("Type 'help' for available commands or Ctrl+C to exit.");

    loop {
        let readline = rl.readline("🐵 >> ");
        match readline {
            Ok(line) => {
                let trimmed = line.trim();

                // Skip empty lines
                if trimmed.is_empty() {
                    continue;
                }

                // Add line to history
                if let Err(e) = rl.add_history_entry(&line) {
                    eprintln!("Warning: Failed to add to history: {}", e);
                }

                // Handle special commands
                match trimmed {
                    "help" => {
                        print_help();
                        continue;
                    }
                    "clear" => {
                        print!("\x1B[2J\x1B[1;1H"); // ANSI escape codes to clear screen
                        continue;
                    }
                    "history" => {
                        print_history(&rl);
                        continue;
                    }
                    _ => {}
                }

                // Parse and evaluate Monkey code
                match evaluate_line(trimmed, environment.clone()) {
                    Ok(Some(exit_code)) => {
                        // Save history before exiting
                        let _ = rl.save_history(".monkey_history");
                        println!("Goodbye! (exit code: {})", exit_code);
                        return Ok(exit_code);
                    }
                    Ok(None) => {} // Continue REPL
                    Err(e) => {
                        eprintln!("! {}", e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Ctrl+C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Ctrl+D");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    // Save history before exiting
    let _ = rl.save_history(".monkey_history");
    Ok(0)
}

fn evaluate_line(
    input: &str,
    environment: crate::object::environment::Env,
) -> Result<Option<i32>, String> {
    let lexer = Lexer::new(input.chars().collect());
    let mut parser = Parser::new(lexer).map_err(|e| format!("Parser error: {}", e))?;
    let program = parser.parse().map_err(|e| format!("Parse error: {}", e))?;

    let evaluated =
        eval(Node::Program(program), environment).map_err(|e| format!("Runtime error: {}", e))?;

    // Check if the evaluated result is an exit command
    if let crate::object::Object::Exit(code) = &evaluated {
        return Ok(Some(*code as i32));
    }

    // Print result with nice formatting
    match &evaluated {
        crate::object::Object::Null => {
            // Don't print null values
        }
        _ => {
            println!("✔️ {}", evaluated);
        }
    }

    Ok(None)
}

fn print_help() {
    println!("Monkey Language REPL Help");
    println!("════════════════════════════");
    println!("Commands:");
    println!("  help     - Show this help message");
    println!("  clear    - Clear the screen");
    println!("  history  - Show command history");
    println!("  Ctrl+C   - Exit REPL");
    println!("  Ctrl+D   - Exit REPL");
    println!();
    println!("Examples:");
    println!("  let x = 5;");
    println!("  let add = fn(a, b) {{ a + b }};");
    println!("  add(2, 3)");
    println!("  [1, 2, 3][0]");
    println!("  {{\"key\": \"value\"}}");
    println!();
}

fn print_history(rl: &DefaultEditor) {
    println!("Command History:");
    println!("══════════════════");
    let history = rl.history();
    for (i, entry) in history.iter().enumerate() {
        println!("{:3}: {}", i + 1, entry);
    }
    if history.is_empty() {
        println!("(No history yet)");
    }
    println!();
}
