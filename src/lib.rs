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
//! use monkey_interpreter_rs::{lexer::Lexer, parser::Parser, evaluator::eval, ast::Node, object::environment::Environment};
//! use std::rc::Rc;
//!
//! // Parse and evaluate a simple Monkey program
//! let input = "let x = 5; x + 10";
//! let lexer = Lexer::new(input.chars().collect());
//! let mut parser = Parser::new(lexer).map_err(|e| e.to_string())?;
//! let program = parser.parse().map_err(|e| e.to_string())?;
//! let environment = Environment::new();
//! let result = eval(Node::Program(program), environment)?;
//! 
//! println!("Result: {}", result);
//! # Ok::<(), String>(())
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