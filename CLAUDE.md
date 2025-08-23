# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Rust implementation of an interpreter for the Monkey programming language, following the "Writing An Interpreter In Go" book but adapted to Rust. The project implements a complete interpreter with lexer, parser, AST, and evaluator.

## Common Development Commands

```bash
# Build the project
cargo build

# Run the REPL interpreter
cargo run

# Run all tests
cargo test

# Run tests for a specific module
cargo test lexer
cargo test parser
cargo test evaluator

# Check code without building
cargo check

# Build with optimizations
cargo build --release

# Run a specific test by name
cargo test next_token

# Show test output
cargo test -- --nocapture
```

## Architecture Overview

The interpreter follows a traditional compiler architecture:

1. **Lexer** (`src/lexer/mod.rs`): Tokenizes input text into tokens
   - Main struct: `Lexer`
   - Key method: `next_token() -> Result<Token>`
   - Handles identifiers, integers, strings, operators, and keywords

2. **Token** (`src/token/mod.rs`): Defines all token types
   - Enum `Token` with variants for all language constructs
   - Includes operators, keywords, literals, and delimiters

3. **AST** (`src/ast/mod.rs`): Abstract Syntax Tree representation
   - `Node` enum with Statement, Expression, and Program variants
   - Recursive expression types: Prefix, Infix, If, Function, Call, Array, Index
   - Statement types: Let, Return, Block, Expression

4. **Parser** (`src/parser/mod.rs`): Recursive descent parser with Pratt parsing
   - Main struct: `Parser`
   - Key method: `parse() -> Result<ast::Program>`
   - Implements operator precedence and expression parsing
   - Uses two-token lookahead (current and peek)

5. **Evaluator** (`src/evaluator/mod.rs`): Tree-walking interpreter
   - Main function: `eval(node: Node, environment: Env) -> Result<Object>`
   - Evaluates AST nodes and returns object values

6. **Object** (`src/object/mod.rs`): Runtime value representation
   - `Object` enum for all runtime values
   - Includes: Int, Boolean, String, Function, Array, Builtin, Return, Error
   - Environment system for variable scoping

7. **REPL** (`src/repl/mod.rs`): Read-Eval-Print Loop
   - Interactive interpreter interface
   - Maintains environment state between evaluations

## Key Implementation Details

- **Error Handling**: Uses `Result` types throughout for proper error propagation
- **Memory Management**: Uses `Rc<RefCell<>>` for shared environment references
- **Precedence Parsing**: Implements Pratt parser for expression precedence
- **Comprehensive Testing**: Each module has extensive unit tests covering edge cases

## Monkey Language Features

The interpreter supports:
- Variables with `let` statements and mutable assignment (`variable = new_value`)
- Data types: integers, booleans, strings, arrays, hash maps
- Control flow: `if`/`else` conditionals and `while` loops
- Arithmetic and comparison operators
- Functions with closures and first-class support
- Built-in functions: `len`, `first`, `last`, `rest`, `push`, `puts`, `exit`, `int`, `string`
- Type casting with `int()` and `string()` builtin functions
- Array indexing and hash map access
- Enhanced REPL for interactive programming
- Comments (single-line `//` and multi-line `/* */`)
- String escape sequences

## Development Workflow

When working on this codebase:
- Always update the README before git commit
- Run tests after making changes: `cargo test`
- Use meaningful commit messages following conventional commits format. Use only type and description, no need for a scope. Example: `feat: add new feature`.
- Include comprehensive test coverage for new features