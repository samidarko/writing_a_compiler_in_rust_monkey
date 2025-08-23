//! # Monkey Interpreter
//!
//! A complete interpreter for the Monkey programming language implemented in Rust.
//! This crate provides a full interpreter with lexer, parser, AST, and evaluator components.
//!
//! ## Features
//!
//! - **Lexical Analysis**: Tokenizes source code with comprehensive error reporting
//! - **Parsing**: Recursive descent parser with Pratt parsing for expressions
//! - **AST**: Clean abstract syntax tree representation  
//! - **Evaluation**: Tree-walking interpreter with environment scoping
//! - **Built-ins**: Standard library functions (len, first, last, etc.)
//! - **REPL**: Interactive Read-Eval-Print Loop
//!
//! ## Quick Start
//!
//! ```rust
//! use monkey_interpreter_rs::{
//!     lexer::Lexer, 
//!     parser::Parser, 
//!     evaluator::{eval, EvaluatorError}, 
//!     ast::Node, 
//!     object::environment::Environment
//! };
//!
//! // Parse and evaluate a simple Monkey program
//! let input = "let x = 5; x + 10";
//! let lexer = Lexer::new(input.chars().collect());
//! let mut parser = Parser::new(lexer)?;
//! let program = parser.parse()?;
//! let environment = Environment::new();
//! let result = eval(Node::Program(program), environment)?;
//!
//! println!("Result: {}", result);
//! # Ok::<(), EvaluatorError>(())
//! ```
//!
//! ## Error Handling
//!
//! The interpreter provides structured error types that implement `std::error::Error`:
//!
//! ```rust
//! use monkey_interpreter_rs::{lexer::Lexer, parser::Parser, evaluator::eval, ast::Node, object::environment::Environment};
//!
//! let input = "x + 5"; // Undefined variable 
//! let lexer = Lexer::new(input.chars().collect());
//! let mut parser = Parser::new(lexer)?;
//! let program = parser.parse()?;
//! let environment = Environment::new();
//! 
//! match eval(Node::Program(program), environment) {
//!     Ok(result) => println!("Result: {}", result),
//!     Err(err) => match err {
//!         monkey_interpreter_rs::evaluator::EvaluatorError::IdentifierNotFound { name, .. } => {
//!             println!("Undefined variable: {}", name);
//!         },
//!         other => println!("Error: {}", other),
//!     }
//! }
//! # Ok::<(), monkey_interpreter_rs::evaluator::EvaluatorError>(())
//! ```
//!
//! ## Architecture
//!
//! The interpreter follows a traditional compiler pipeline:
//!
//! 1. **[Lexer](lexer)** - Converts source text into tokens
//! 2. **[Parser](parser)** - Builds an abstract syntax tree from tokens  
//! 3. **[Evaluator](evaluator)** - Walks the AST and produces values
//!
//! ## Example Monkey Code
//!
//! ```monkey
//! // Variables and functions
//! let fibonacci = fn(x) {
//!   if (x < 2) { x } else { fibonacci(x-1) + fibonacci(x-2) }
//! };
//!
//! // Arrays and built-ins
//! let numbers = [1, 2, 3, 4, 5];
//! let doubled = map(numbers, fn(x) { x * 2 });
//!
//! // Hash maps
//! let person = {"name": "Alice", "age": 30};
//! ```

pub mod ast;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod repl;
pub mod token;
