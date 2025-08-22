# Monkey Interpreter in Rust

A complete interpreter for the Monkey programming language, implemented in Rust. This project is a Rust adaptation of the interpreter described in "Writing An Interpreter In Go" by Thorsten Ball.

## Project Background

I recently wrapped up *Writing an Interpreter in Go*, a book that demystified lexer, parser, ASTs, and evaluator logic. To deepen my understanding and sharpen my skills in Rust, I reimplemented the interpreter in Rust—and I couldn't be more satisfied with the outcome.

After over a decade in software engineering—split between systems infrastructure and full-stack development—I realized that, despite my experience across multiple paradigms and languages, I never truly understood the mechanics behind interpreters. This project filled that gap, layer by layer.

Here's what I learned:

- The interpreter is built on clear, sequential stages—lexing raw text, parsing into syntax trees, and evaluating with the right semantics.
- Working through the Monkey interpreter in Rust taught me how these pieces fit together in practice—and how Rust's features impact implementation choices.
- It wasn't just about the code—it was about learning how languages work under the hood.

This project reminded me how rewarding it is to rediscover fundamentals—even after years in tech. It's been one of my most fulfilling learning experiences in a while.

## Features

The Monkey interpreter supports:

- **Variables** with `let` statements
- **Data types**: integers, booleans, strings, arrays, and hash maps
- **Operators**: arithmetic (`+`, `-`, `*`, `/`), comparison (`<`, `>`, `<=`, `>=`, `==`, `!=`), logical (`&&`, `||`), and unary (`!`, `-`)
- **Functions** with closures and first-class support
- **Conditionals** (`if`/`else` expressions)
- **Built-in functions**: `len`, `first`, `last`, `rest`, `push`, `puts`, `exit`
- **Array indexing** and hash map access
- **Comments**: single-line (`//`) and multi-line (`/* */`)
- **String escape sequences**: `\n`, `\t`, `\"`, `\\`, `\r`
- **Enhanced error reporting** with line/column position tracking
- **Interactive REPL** for live programming

## Architecture

The interpreter follows a traditional compiler architecture:

1. **Lexer** (`src/lexer/mod.rs`): Tokenizes input text into tokens
2. **Parser** (`src/parser/mod.rs`): Recursive descent parser with Pratt parsing for expressions
3. **AST** (`src/ast/mod.rs`): Abstract Syntax Tree representation
4. **Evaluator** (`src/evaluator/mod.rs`): Tree-walking interpreter that evaluates AST nodes
5. **Object System** (`src/object/mod.rs`): Runtime value representation with environment scoping
6. **REPL** (`src/repl/mod.rs`): Interactive Read-Eval-Print Loop

## Usage

### Installation

```bash
# Install from crates.io (when published)
cargo install monkey-interpreter-rs

# Or build from source
git clone https://github.com/samidarko/monkey-interpreter-rs
cd monkey-interpreter-rs
cargo build --release
```

### Building and Running

```bash
# Build the project
cargo build

# Run the interactive REPL
cargo run
# Or after building: ./target/release/monkey

# Run tests
cargo test

# Run tests with output
cargo test -- --nocapture
```

### Example Monkey Code

```monkey
// Variables and functions
let fibonacci = fn(x) {
  if (x == 0) {
    0
  } else {
    if (x == 1) {
      1
    } else {
      fibonacci(x - 1) + fibonacci(x - 2);
    }
  }
};

fibonacci(10);

// Arrays and higher-order functions  
let map = fn(arr, f) {
  let iter = fn(arr, accumulated) {
    if (len(arr) == 0) {
      accumulated
    } else {
      iter(rest(arr), push(accumulated, f(first(arr))));
    }
  };
  iter(arr, []);
};

let a = [1, 2, 3, 4];
let double = fn(x) { x * 2; };
map(a, double); // [2, 4, 6, 8]

/* Multi-line comments 
   are also supported */
let person = {"name": "Alice", "age": 30};
person["name"]; // "Alice"

// Enhanced comparison and logical operators
let adult = person["age"] >= 18 && person["name"] != "";
if (adult) {
    puts("Person is an adult named: " + person["name"]);
}

// String with escape sequences
puts("Hello\nWorld!\tTab\tSeparated\"Quote\"");

// Exit the REPL
exit(); // or exit(42) for custom exit code
```

## Built-in Functions

- `len(array|string)` - Returns length of arrays or strings
- `first(array)` - Returns first element of an array
- `last(array)` - Returns last element of an array
- `rest(array)` - Returns array with all elements except the first
- `push(array, element)` - Returns new array with element appended
- `puts(args...)` - Prints arguments to stdout
- `exit([code])` - Exits the REPL with optional exit code

## Development

### Running Specific Tests

```bash
# Test specific modules
cargo test lexer
cargo test parser
cargo test evaluator

# Run a specific test by name
cargo test fibonacci
```

### Code Quality

```bash
# Check code without building
cargo check

# Run clippy for linting
cargo clippy

# Format code
cargo fmt
```

## Implementation Highlights

### Recent Enhancements

- **Enhanced Error Reporting**: Comprehensive error messages with line/column position tracking and contextual information
- **Extended Operator Support**: Added comparison operators (`<=`, `>=`) and logical operators (`&&`, `||`)
- **Comment Support**: Full single-line (`//`) and multi-line (`/* */`) comment parsing
- **String Escape Sequences**: Support for common escape sequences (`\n`, `\t`, `\"`, `\\`, `\r`)
- **Modular Architecture**: Split large files into logical modules for better maintainability
- **Comprehensive Testing**: 58+ tests covering all features including edge cases and error conditions

### Rust-Specific Design Choices

- **Error Handling**: Uses `Result` types throughout for proper error propagation
- **Memory Management**: Uses `Rc<RefCell<>>` for shared environment references in closures
- **Pattern Matching**: Leverages Rust's powerful pattern matching for AST evaluation
- **Type Safety**: Rust's type system catches many interpreter bugs at compile time
- **Position Tracking**: Detailed source position tracking for enhanced debugging experience

### Architecture Benefits

- **Separation of Concerns**: Each phase (lexing, parsing, evaluation) is cleanly separated into modules
- **Extensibility**: Easy to add new language features by extending the AST and evaluator
- **Testing**: Comprehensive test suite covering all language features and error conditions
- **Error Quality**: Professional-quality error messages that help users identify and fix issues quickly
- **Performance**: Tree-walking interpreter optimized for clarity over speed

## Learning Outcomes

This project provided deep insights into:

- How programming languages are structured and implemented
- The relationship between syntax, semantics, and evaluation
- Rust's ownership model in the context of tree structures and shared state
- The beauty of recursive descent parsing and tree-walking evaluation
- How closures and environments work under the hood

## License

This project is for educational purposes, inspired by "Writing An Interpreter In Go" by Thorsten Ball.