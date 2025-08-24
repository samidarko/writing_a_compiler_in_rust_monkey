//! Tree-walking evaluator for the Monkey programming language.
//!
//! This module implements a tree-walking interpreter that evaluates Monkey AST nodes
//! and produces runtime values (Objects). The evaluator handles:
//!
//! - Variable binding and lookup
//! - Function definitions and calls with closures
//! - Built-in functions and operators
//! - Control flow (if/else statements)
//! - Data structures (arrays, hash maps)
//! - Error propagation and handling
//!
//! # Architecture
//!
//! The evaluator is split into focused submodules:
//! - `statements` - Handles executable statements (let, return, blocks)
//! - `expressions` - Handles value-producing expressions
//! - `operators` - Handles prefix and infix operators
//! - `builtins` - Provides built-in functions (len, first, etc.)
//!
//! # Examples
//!
//! ```
//! use monkey_interpreter_rs::{evaluator::eval, ast::Node, parser::Parser, lexer::Lexer, object::environment::Environment};
//! use std::rc::Rc;
//!
//! let input = "let x = 5; x + 10";
//! let lexer = Lexer::new(input.chars().collect());
//! let mut parser = Parser::new(lexer).unwrap();
//! let program = parser.parse().unwrap();
//! let env = Environment::new();
//!
//! let result = eval(Node::Program(program), env).unwrap();
//! assert_eq!(format!("{}", result), "15");
//! ```

mod builtins;
mod errors;
mod expressions;
mod operators;
mod statements;

use crate::ast::Node;
use crate::object::environment::Env;
use std::rc::Rc;

// Re-export public functions and types
pub use errors::{EvaluatorError, Result};
pub use expressions::eval_expression;
pub use operators::{eval_infix_expression, eval_prefix_expression};
pub use statements::{eval_program, eval_statement};

/// Evaluates an AST node and returns the resulting object.
///
/// This is the main entry point for the evaluator. It dispatches to the
/// appropriate evaluation function based on the node type.
///
/// # Arguments
///
/// * `node` - The AST node to evaluate
/// * `environment` - The evaluation environment containing variable bindings
///
/// # Returns
///
/// The evaluated result as an `Object`, or an error message if evaluation fails.
///
/// # Examples
///
/// ```
/// use monkey_interpreter_rs::{evaluator::eval, ast::Node, parser::Parser, lexer::Lexer, object::environment::Environment};
/// use std::rc::Rc;
///
/// let input = "5 + 3";
/// let lexer = Lexer::new(input.chars().collect());
/// let mut parser = Parser::new(lexer).unwrap();
/// let program = parser.parse().unwrap();
/// let env = Environment::new();
///
/// let result = eval(Node::Program(program), env).unwrap();
/// println!("Result: {}", result); // Output: Result: 8
/// ```
pub fn eval(node: Node, environment: Env) -> Result<Object> {
    match node {
        Node::Statement(statement) => eval_statement(statement, Rc::clone(&environment)),
        Node::Expression(expression) => eval_expression(expression, Rc::clone(&environment)),
        Node::Program(program) => eval_program(program, Rc::clone(&environment)),
    }
}

// Re-export Object for convenience
pub use crate::object::Object;

#[cfg(test)]
mod tests {
    use crate::ast;
    use crate::evaluator::{eval, EvaluatorError, Result};
    use crate::lexer::Lexer;
    use crate::object;
    use crate::object::environment::Environment;
    use crate::object::{ArrayLiteral, Object};
    use crate::parser::Parser;
    use crate::token::Token;
    use smallvec::smallvec;
    use std::collections::BTreeMap;

    fn test_eval(input: &str) -> Result<Object> {
        let lexer = Lexer::new(input.chars().collect());
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse()?;
        let environment = Environment::new();
        let object = eval(ast::Node::Program(program), environment)?;
        Ok(object)
    }

    #[test]
    fn eval_integer_expression() -> Result<()> {
        let tests = vec![
            ("5", Object::Int(5)),
            ("10", Object::Int(10)),
            ("-5", Object::Int(-5)),
            ("-10", Object::Int(-10)),
            ("5 + 5 + 5 + 5 - 10", Object::Int(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Int(32)),
            ("-50 + 100 + -50", Object::Int(0)),
            ("5 * 2 + 10", Object::Int(20)),
            ("5 + 2 * 10", Object::Int(25)),
            ("20 + 2 * -10", Object::Int(0)),
            ("50 / 2 * 2 + 10", Object::Int(60)),
            ("2 * (5 + 10)", Object::Int(30)),
            ("3 * 3 * 3 + 10", Object::Int(37)),
            ("3 * (3 * 3) + 10", Object::Int(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Int(50)),
        ];

        for test in tests {
            let object = test_eval(test.0)?;
            assert_eq!(object, test.1);
        }

        Ok(())
    }

    #[test]
    fn eval_boolean_expression() -> Result<()> {
        let tests = vec![
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
            ("1 < 2", Object::Boolean(true)),
            ("1 > 2", Object::Boolean(false)),
            ("1 < 1", Object::Boolean(false)),
            ("1 > 1", Object::Boolean(false)),
            ("1 <= 2", Object::Boolean(true)),
            ("1 >= 2", Object::Boolean(false)),
            ("1 <= 1", Object::Boolean(true)),
            ("1 >= 1", Object::Boolean(true)),
            ("2 <= 1", Object::Boolean(false)),
            ("1 >= 2", Object::Boolean(false)),
            ("1 == 1", Object::Boolean(true)),
            ("1 != 1", Object::Boolean(false)),
            ("1 == 2", Object::Boolean(false)),
            ("1 != 2", Object::Boolean(true)),
            ("true == true", Object::Boolean(true)),
            ("false == false", Object::Boolean(true)),
            ("true == false", Object::Boolean(false)),
            ("true != false", Object::Boolean(true)),
            ("false != true", Object::Boolean(true)),
            ("(1 < 2) == true", Object::Boolean(true)),
            ("(1 < 2) == false", Object::Boolean(false)),
            ("(1 > 2) == true", Object::Boolean(false)),
            ("(1 > 2) == false", Object::Boolean(true)),
        ];

        for test in tests {
            let object = test_eval(test.0)?;
            assert_eq!(object, test.1);
        }

        Ok(())
    }

    #[test]
    fn bang_operator() -> Result<()> {
        let tests = vec![
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!5", Object::Boolean(false)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
            ("!!5", Object::Boolean(true)),
        ];

        for test in tests {
            let object = test_eval(test.0)?;
            assert_eq!(object, test.1);
        }

        Ok(())
    }

    #[test]
    fn if_else_expression() -> Result<()> {
        let tests = vec![
            ("if (true) { 10 }", Object::Int(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Int(10)),
            ("if (1 < 2) { 10 }", Object::Int(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Int(10)),
        ];

        for test in tests {
            let object = test_eval(test.0)?;
            assert_eq!(object, test.1);
        }

        Ok(())
    }

    #[test]
    fn return_statement() -> Result<()> {
        let tests = vec![
            ("return 10;", Object::Int(10)),
            ("return 10; 9;", Object::Int(10)),
            ("return 2 * 5; 9;", Object::Int(10)),
            ("9; return 2 * 5; 9;", Object::Int(10)),
            ("if (10 > 1) { return 10; }", Object::Int(10)),
            (
                "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
                Object::Int(10),
            ),
            (
                "let f = fn(x) { return x; x + 10; }; f(10);",
                Object::Int(10),
            ),
            (
                "let f = fn(x) {
  let result = x + 10;
  return result;
  return 10;
};
f(10);",
                Object::Int(20),
            ),
        ];

        for test in tests {
            let object = test_eval(test.0)?;
            assert_eq!(object, test.1);
        }

        Ok(())
    }

    #[test]
    fn error_handling() -> Result<()> {
        let tests = vec![
            (
                "5 + true;",
                Err(EvaluatorError::type_error("type mismatch: 5 + true")),
            ),
            (
                "5 + true; 5;",
                Err(EvaluatorError::type_error("type mismatch: 5 + true")),
            ),
            (
                "-true;",
                Err(EvaluatorError::type_error("unknown operator: -true")),
            ),
            (
                "true + false;",
                Err(EvaluatorError::type_error("unknown operator: true + false")),
            ),
            (
                "5; true + false; 5",
                Err(EvaluatorError::type_error("unknown operator: true + false")),
            ),
            (
                "true + false + true + false;",
                Err(EvaluatorError::type_error("unknown operator: true + false")),
            ),
            (
                "5; true + false; 5",
                Err(EvaluatorError::type_error("unknown operator: true + false")),
            ),
            (
                "if (10 > 1) { true + false; }",
                Err(EvaluatorError::type_error("unknown operator: true + false")),
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                Err(EvaluatorError::type_error("unknown operator: true + false")),
            ),
            (
                "foobar;",
                Err(EvaluatorError::identifier_not_found("foobar")),
            ),
            (
                r#""Hello" - "World""#,
                Err(EvaluatorError::type_error(
                    "unknown operator: 'Hello' - 'World'",
                )),
            ),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                Ok(Object::Error(
                    "unusable as hash key: fn(x) {\nx\n}".to_string(),
                )),
            ),
        ];

        for test in tests {
            let result = test_eval(test.0);
            assert_eq!(result, test.1);
        }

        Ok(())
    }

    #[test]
    fn let_statement() -> Result<()> {
        let tests = vec![
            ("let a = 5; a;", Object::Int(5)),
            ("let a = 5 * 5; a;", Object::Int(25)),
            ("let a = 5; let b = a; b;", Object::Int(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Int(15),
            ),
        ];

        for test in tests {
            let object = test_eval(test.0)?;
            assert_eq!(object, test.1);
        }

        Ok(())
    }

    #[test]
    fn function_object() -> Result<()> {
        let tests = vec![(
            "fn(x) { x + 2; };",
            Object::Function(object::Function {
                parameters: smallvec!["x".into()],
                body: ast::BlockStatement {
                    statements: smallvec![Box::new(ast::Statement::Expression(
                        ast::Expression::Infix(ast::InfixExpression {
                            left: Box::new(ast::Expression::Identifier("x".into())),
                            operator: Token::Plus,
                            right: Box::new(ast::Expression::Int(2)),
                        },)
                    ))],
                },
                env: Environment::new(),
            }),
        )];

        for test in tests {
            let object = test_eval(test.0)?;
            assert_eq!(object, test.1);
        }

        Ok(())
    }

    #[test]
    fn function_application() -> Result<()> {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", Object::Int(5)),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Int(5),
            ),
            ("let double = fn(x) { x * 2; }; double(5);", Object::Int(10)),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", Object::Int(10)),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Int(20),
            ),
            ("fn(x) { x; }(5)", Object::Int(5)),
        ];

        for test in tests {
            let object = test_eval(test.0)?;
            assert_eq!(object, test.1);
        }

        Ok(())
    }

    #[test]
    fn enclosing_environments() -> Result<()> {
        let input = "let first = 10;
let second = 10;
let third = 10;

let ourFunction = fn(first) {
  let second = 20;

  first + second + third;
};

ourFunction(20) + first + second;
";

        let object = test_eval(input)?;
        assert_eq!(object, Object::Int(70));

        Ok(())
    }

    #[test]
    fn closures() -> Result<()> {
        let input = "let newAdder = fn(x) { fn(y) { x + y }; };
let addTwo = newAdder(2);
addTwo(2);
";

        let object = test_eval(input)?;
        assert_eq!(object, Object::Int(4));

        Ok(())
    }

    #[test]
    fn counter() -> Result<()> {
        let input = "let counter = fn(x) { if (x > 10) { return true; } else { counter(x + 1); } }; counter(0);";

        let object = test_eval(input)?;
        assert_eq!(object, Object::Boolean(true));

        Ok(())
    }

    #[test]
    fn string_literal() -> Result<()> {
        let input = r#""Hello" + " " + "World!""#;

        let object = test_eval(input)?;
        assert_eq!(object, Object::String("Hello World!".into()));

        Ok(())
    }

    #[test]
    fn array_literals() -> Result<()> {
        let input = "[1, 2 * 2, 3 + 3]";

        let object = test_eval(input)?;
        assert_eq!(
            object,
            Object::Array(Box::new(ArrayLiteral {
                elements: smallvec![
                    Box::new(Object::Int(1)),
                    Box::new(Object::Int(4)),
                    Box::new(Object::Int(6))
                ]
            }))
        );

        Ok(())
    }

    #[test]
    fn builtin_functions() -> Result<()> {
        enum Expected {
            Obj(Object),
            Err(&'static str),
        }
        use Expected::*;

        let tests: Vec<(&str, Expected)> = vec![
            (r#"len("")"#, Obj(Object::Int(0))),
            (r#"len("four")"#, Obj(Object::Int(4))),
            (r#"len("hello world")"#, Obj(Object::Int(11))),
            (r#"len(1)"#, Err("argument to `len` not supported, got 1")),
            (
                r#"len("one", "two")"#,
                Err("wrong number of arguments. got=2, want=1"),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?; // same helper as in your other test

            match expected {
                Obj(obj) => {
                    assert_eq!(evaluated, obj);
                }
                Err(msg) => match evaluated {
                    Object::Error(ref e) => assert_eq!(e, msg, "wrong error message"),
                    other => {
                        panic!("object is not Error. got={:?}", other)
                    }
                },
            }
        }

        Ok(())
    }

    #[test]
    fn array_index_expressions() -> Result<()> {
        let tests: Vec<(&str, Object)> = vec![
            ("[1, 2, 3][0]", Object::Int(1)),
            ("[1, 2, 3][1]", Object::Int(2)),
            ("[1, 2, 3][2]", Object::Int(3)),
            ("let i = 0; [1][i];", Object::Int(1)),
            ("[1, 2, 3][1 + 1];", Object::Int(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Int(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Int(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Object::Int(2),
            ),
            (
                "[1, 2, 3][3]",
                Object::Error("index out of range: 3".to_string()),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?; // same helper as in your other test
            assert_eq!(evaluated, expected);
        }

        Ok(())
    }

    #[test]
    fn map_statement() -> Result<()> {
        let input = "let map = fn(arr, f) {
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
map(a, double);
";

        let object = test_eval(input)?;
        assert_eq!(
            object,
            Object::Array(Box::new(ArrayLiteral {
                elements: smallvec![
                    Box::new(Object::Int(2)),
                    Box::new(Object::Int(4)),
                    Box::new(Object::Int(6)),
                    Box::new(Object::Int(8))
                ]
            }))
        );

        Ok(())
    }

    #[test]
    fn reduce_statement() -> Result<()> {
        let input = "let reduce = fn(arr, initial, f) {
  let iter = fn(arr, result) {
    if (len(arr) == 0) {
      result
    } else {
      iter(rest(arr), f(result, first(arr)));
    }
  };

  iter(arr, initial);
};
let sum = fn(arr) {
  reduce(arr, 0, fn(initial, el) { initial + el });
};
sum([1, 2, 3, 4, 5]);
";

        let object = test_eval(input)?;
        assert_eq!(object, Object::Int(15));

        Ok(())
    }

    #[test]
    fn hash_literals() -> Result<()> {
        let input = r#"let two = "two";
{
    "one": 10 - 9,
    two: 1 + 1,
    "thr" + "ee": 6 / 2,
    4: 4,
    true: 5,
    false: 6
}
"#;

        let object = test_eval(input)?;
        let expected: BTreeMap<Object, Object> = BTreeMap::from([
            (Object::String("one".into()), Object::Int(1)),
            (Object::String("two".into()), Object::Int(2)),
            (Object::String("three".into()), Object::Int(3)),
            (Object::Int(4), Object::Int(4)),
            (Object::Boolean(true), Object::Int(5)),
            (Object::Boolean(false), Object::Int(6)),
        ]);
        if let Object::Hash(hash_literal) = object {
            assert_eq!(hash_literal.pairs, expected);
        } else {
            panic!("not an Hash")
        }

        Ok(())
    }

    #[test]
    fn hash_index_expressions() -> Result<()> {
        let tests: Vec<(&str, Object)> = vec![
            (r#"{"foo": 5}["foo"]"#, Object::Int(5)),
            (r#"{"foo": 5}["bar"]"#, Object::Null),
            (r#"let key = "foo"; {"foo": 5}[key]"#, Object::Int(5)),
            (r#"{}["foo"]"#, Object::Null),
            (r#"{5: 5}[5]"#, Object::Int(5)),
            (r#"{true: 5}[true]"#, Object::Int(5)),
            (r#"{false: 5}[false]"#, Object::Int(5)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?; // same helper as in your other test
            assert_eq!(evaluated, expected);
        }

        Ok(())
    }

    #[test]
    fn string_escape_sequences() -> Result<()> {
        let tests: Vec<(&str, Object)> = vec![
            (r#""hello\nworld""#, Object::String("hello\nworld".into())),
            (r#""tab\there""#, Object::String("tab\there".into())),
            (r#""quote\"here""#, Object::String("quote\"here".into())),
            (
                r#""backslash\\here""#,
                Object::String("backslash\\here".into()),
            ),
            (
                r#""multiple\n\t\"\\\r""#,
                Object::String("multiple\n\t\"\\\r".into()),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            assert_eq!(evaluated, expected);
        }

        Ok(())
    }

    #[test]
    fn variable_assignment() -> Result<()> {
        let tests: Vec<(&str, Object)> = vec![
            // Basic assignment
            ("let x = 5; x = 10; x;", Object::Int(10)),
            ("let x = 5; x = x + 1; x;", Object::Int(6)),
            // Multiple assignments
            ("let x = 5; let y = 10; x = y; x;", Object::Int(10)),
            // Assignment with expressions
            ("let x = 5; x = 10 * 2; x;", Object::Int(20)),
            ("let x = 5; let y = 3; x = x * y + 1; x;", Object::Int(16)),
            // Assignment with function calls
            (
                "let add = fn(a, b) { a + b; }; let x = 5; x = add(x, 10); x;",
                Object::Int(15),
            ),
            // String assignment
            (
                "let x = \"hello\"; x = \"world\"; x;",
                Object::String("world".into()),
            ),
            (
                "let x = \"hello\"; x = x + \" world\"; x;",
                Object::String("hello world".into()),
            ),
            // Boolean assignment
            ("let x = true; x = false; x;", Object::Boolean(false)),
            ("let x = true; x = !x; x;", Object::Boolean(false)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            assert_eq!(evaluated, expected, "Input: {}", input);
        }

        Ok(())
    }

    #[test]
    fn variable_assignment_returns_value() -> Result<()> {
        // Test that assignment returns the assigned value
        let tests: Vec<(&str, Object)> = vec![
            ("let x = 5; x = 10", Object::Int(10)), // Assignment returns value
            ("let x = true; x = false", Object::Boolean(false)),
            ("let x = \"old\"; x = \"new\"", Object::String("new".into())),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            assert_eq!(evaluated, expected, "Input: {}", input);
        }

        Ok(())
    }

    #[test]
    fn variable_assignment_errors() -> Result<()> {
        let error_tests = vec![
            // Assigning to undeclared variable
            ("x = 5;", "Identifier 'x' not found"),
            ("let y = 10; x = y;", "Identifier 'x' not found"),
        ];

        for (input, expected_error) in error_tests {
            let result = test_eval(input);
            match result {
                Err(error) => assert!(
                    error.to_string().contains(expected_error),
                    "Expected error '{}' but got '{}' for input '{}'",
                    expected_error,
                    error,
                    input
                ),
                Ok(obj) => panic!(
                    "Expected error '{}' but got successful result '{}' for input '{}'",
                    expected_error, obj, input
                ),
            }
        }

        Ok(())
    }

    #[test]
    fn while_loops() -> Result<()> {
        let tests: Vec<(&str, Object)> = vec![
            // Basic while loop
            ("let i = 0; while (i < 3) { i = i + 1; } i;", Object::Int(3)),

            // While loop with different conditions  
            ("let x = 10; while (x > 5) { x = x - 1; } x;", Object::Int(5)),

            // While loop that doesn't execute
            ("let j = 5; while (j < 0) { j = j + 1; } j;", Object::Int(5)),

            // While loop with accumulator
            ("let sum = 0; let i = 1; while (i <= 3) { sum = sum + i; i = i + 1; } sum;", Object::Int(6)),

            // While loop with boolean condition
            ("let flag = true; let count = 0; while (flag) { count = count + 1; if (count >= 2) { flag = false; } } count;", Object::Int(2)),

            // While loop that returns early
            ("let test = fn() { let i = 0; while (i < 10) { if (i == 3) { return i; } i = i + 1; } return -1; }; test();", Object::Int(3)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            assert_eq!(evaluated, expected, "Input: {}", input);
        }

        Ok(())
    }

    #[test]
    fn while_loop_edge_cases() -> Result<()> {
        let tests: Vec<(&str, Object)> = vec![
            // Empty body while loop
            ("let x = 0; while (false) { } x;", Object::Int(0)),

            // Complex condition
            ("let a = 1; while (a < 5) { a = a + 1; } a;", Object::Int(5)),

            // Nested variable access
            ("let outer = 10; while (outer > 8) { let inner = 5; outer = outer - inner + 3; } outer;", Object::Int(8)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            assert_eq!(evaluated, expected, "Input: {}", input);
        }

        Ok(())
    }

    #[test]
    fn while_loop_with_functions() -> Result<()> {
        let input = r#"
            let fibonacci = fn(n) {
                let a = 0;
                let b = 1;
                let i = 0;
                while (i < n) {
                    let temp = a + b;
                    a = b;
                    b = temp;
                    i = i + 1;
                }
                return a;
            };
            fibonacci(5);
        "#;

        let evaluated = test_eval(input)?;
        assert_eq!(evaluated, Object::Int(5)); // 5th Fibonacci number

        Ok(())
    }

    #[test]
    fn for_loop_array_iteration() -> Result<()> {
        let tests: Vec<(&str, Object)> = vec![
            // Simple for loop over array elements
            ("for (x in [1, 2, 3]) { x }", Object::Int(3)), // Returns last iteration value
            ("let arr = [1, 2, 3]; for (x in arr) { x }", Object::Int(3)), // Returns last iteration value
            // For loop with accumulator
            (
                r#"
                let sum = 0;
                for (x in [1, 2, 3, 4, 5]) {
                    sum = sum + x;
                }
                sum;
            "#,
                Object::Int(15),
            ),
            // For loop with complex expressions
            (
                r#"
                let result = [];
                for (x in [1, 2, 3]) {
                    result = push(result, x * 2);
                }
                result;
            "#,
                Object::Array(Box::new(ArrayLiteral {
                    elements: smallvec![
                        Box::new(Object::Int(2)),
                        Box::new(Object::Int(4)),
                        Box::new(Object::Int(6))
                    ],
                })),
            ),
            // Empty array
            ("for (x in []) { x }", Object::Null),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            assert_eq!(evaluated, expected);
        }

        Ok(())
    }

    #[test]
    fn for_loop_hash_iteration() -> Result<()> {
        let tests: Vec<(&str, usize)> = vec![
            // Simple for loop over hash keys - count iterations
            (
                r#"
                let count = 0;
                for (key in {"a": 1, "b": 2, "c": 3}) {
                    count = count + 1;
                }
                count;
            "#,
                3,
            ),
            // Empty hash
            (
                r#"
                let count = 0;
                for (key in {}) {
                    count = count + 1;
                }
                count;
            "#,
                0,
            ),
        ];

        for (input, expected_count) in tests {
            let evaluated = test_eval(input)?;
            assert_eq!(evaluated, Object::Int(expected_count as isize));
        }

        Ok(())
    }

    #[test]
    fn for_loop_variable_scoping() -> Result<()> {
        // Test that loop variable doesn't leak to outer scope
        let input = r#"
            let x = 10;
            for (x in [1, 2, 3]) {
                x + 1;
            }
            x; // Should still be 10
        "#;

        let evaluated = test_eval(input)?;
        assert_eq!(evaluated, Object::Int(10));

        Ok(())
    }

    #[test]
    fn for_loop_with_return() -> Result<()> {
        // Test early return from for loop inside function
        let input = r#"
            let find_first = fn(arr, target) {
                for (x in arr) {
                    if (x == target) {
                        return x;
                    }
                }
                return -1;
            };
            find_first([1, 2, 3, 4, 5], 3);
        "#;

        let evaluated = test_eval(input)?;
        assert_eq!(evaluated, Object::Int(3));

        Ok(())
    }

    #[test]
    fn for_loop_error_cases() -> Result<()> {
        let error_cases = vec![
            // Invalid collection type
            "for (x in 42) { x }",
            "for (x in true) { x }",
            "for (x in \"hello\") { x }",
        ];

        for input in error_cases {
            let result = test_eval(input);
            assert!(result.is_err(), "Expected error for input: {}", input);
        }

        Ok(())
    }
}
