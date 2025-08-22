mod builtins;

use crate::ast::{BlockStatement, Expression, Node, Program, Statement};
use crate::object::environment::Env;
use crate::object::environment::Environment;
use crate::object::{ArrayLiteral, HashLiteral, Object, ReturnValue};
use crate::token::Token;
use crate::{ast, object};
use std::collections::BTreeMap;
use std::rc::Rc;
use std::result;

pub type Result<T> = result::Result<T, String>;

pub fn eval(node: Node, environment: Env) -> Result<Object> {
    match node {
        Node::Statement(statement) => eval_statement(statement, Rc::clone(&environment)),
        Node::Expression(expression) => eval_expression(expression, Rc::clone(&environment)),
        Node::Program(program) => eval_program(program, Rc::clone(&environment)),
    }
}

fn eval_program(program: Program, environment: Env) -> Result<Object> {
    let mut result: Object = Object::Null;
    for statement in program.statements {
        let object = eval(Node::Statement(statement), Rc::clone(&environment))?;

        if let Object::Return(return_value) = object {
            return Ok(*return_value.value);
        }

        result = object;
    }

    Ok(result)
}

fn eval_block_statement(block_statement: BlockStatement, environment: Env) -> Result<Object> {
    let mut result: Object = Object::Null;
    for statement in block_statement.statements {
        let object = eval(Node::Statement(statement), Rc::clone(&environment))?;

        if let Object::Return(_) = object {
            return Ok(object);
        }

        result = object;
    }

    Ok(result)
}

fn eval_statement(statement: Statement, environment: Env) -> Result<Object> {
    match statement {
        Statement::Expression(expression) => eval(Node::Expression(expression), environment),
        Statement::Let(ast::LetStatement { name, value }) => {
            let obj = eval(Node::Expression(value), Rc::clone(&environment))?;
            environment.borrow_mut().set(&name, &obj);
            Ok(obj)
        }
        Statement::Return(return_statement) => {
            let object = eval(Node::Expression(return_statement.value), environment)?;
            Ok(Object::Return(ReturnValue {
                value: Box::new(object),
            }))
        }
        Statement::Block(block_statement) => eval_block_statement(block_statement, environment),
    }
}

fn eval_expression(expression: ast::Expression, environment: Env) -> Result<Object> {
    let object = match expression {
        Expression::Identifier(name) => {
            let identifier = environment.borrow().get(&name);
            if let Some(value) = identifier {
                return Ok(value);
            }

            if let Object::Builtin(builtin) = builtins::get_builtin(&name) {
                return Ok(Object::Builtin(builtin));
            }

            return Err(format!("identifier not found: {}", name));
        }
        Expression::Boolean(value) => Object::Boolean(value),
        Expression::Null => Object::Null,
        Expression::String(value) => Object::String(value),
        Expression::Int(value) => Object::Int(value),
        Expression::Infix(infix_expression) => {
            let right = eval(
                Node::Expression(*infix_expression.right),
                Rc::clone(&environment),
            )?;
            let left = eval(Node::Expression(*infix_expression.left), environment)?;
            return eval_infix_expression(infix_expression.operator, left, right);
        }
        Expression::Prefix(pre_expression) => {
            let right = eval(Node::Expression(*pre_expression.right), environment)?;
            return eval_prefix_expression(pre_expression.operator, right);
        }
        Expression::If(if_expression) => return eval_if_expression(if_expression, environment),
        Expression::Function(function_literal) => Object::Function(object::Function {
            parameters: function_literal.parameters,
            body: function_literal.body,
            env: Rc::clone(&environment), // capture shared env
        }),
        Expression::Call(call_expression) => {
            let func_obj = eval(
                Node::Expression(*call_expression.function),
                Rc::clone(&environment),
            )?;
            let args = eval_expressions(call_expression.arguments, Rc::clone(&environment))?;
            return apply_function(func_obj, &args);
        }
        Expression::Array(array_literal) => {
            let elements = eval_expressions(array_literal.elements, Rc::clone(&environment))?;
            if elements.len() == 1 {
                if let Object::Error(_) = elements[0] {
                    return Ok(elements[0].clone());
                }
            }
            Object::Array(ArrayLiteral { elements })
        }
        Expression::Index(index_expression) => {
            let left = eval_expression(*(index_expression.left), Rc::clone(&environment))?;
            if let Object::Error(_) = left {
                return Ok(left);
            }
            let index = eval_expression(*(index_expression.index), Rc::clone(&environment))?;
            if let Object::Error(_) = index {
                return Ok(index);
            }
            return Ok(eval_index_expression(left, index));
        }
        Expression::Hash(hash_literal) => {
            return eval_hash_literal(Expression::Hash(hash_literal), environment)
        }
    };
    Ok(object)
}

fn eval_expressions(expressions: Vec<Expression>, environment: Env) -> Result<Vec<Object>> {
    let mut objects = vec![];

    for expression in expressions {
        let object = eval(Node::Expression(expression), Rc::clone(&environment))?;
        objects.push(object);
    }

    Ok(objects)
}

#[allow(clippy::mutable_key_type)]
fn eval_hash_literal(expression: Expression, environment: Env) -> Result<Object> {
    let mut pairs = BTreeMap::new();

    if let Expression::Hash(hash_literal) = expression {
        for (key_expression, value_expression) in hash_literal.pairs {
            let key = eval(Node::Expression(key_expression), Rc::clone(&environment))?;
            if let Object::Error(error) = key {
                return Ok(Object::Error(error));
            }
            let value = eval(Node::Expression(value_expression), Rc::clone(&environment))?;
            if let Object::Error(error) = value {
                return Ok(Object::Error(error));
            }
            pairs.insert(key, value);
        }
    } else {
        return Ok(Object::Error(format!(
            "invalid hash literal: {}",
            expression
        )));
    }

    Ok(Object::Hash(HashLiteral { pairs }))
}

fn apply_function(function: Object, arguments: &[Object]) -> Result<Object> {
    match function {
        Object::Function(f) => {
            let env = extended_function_environment(&f, arguments);
            let evaluated = eval(Node::Statement(Statement::Block(f.body)), env)?;
            Ok(unwrap_return_value(evaluated))
        }
        Object::Builtin(f) => Ok((f.f)(arguments)),
        _ => Err(format!("not a function: {}", function)),
    }
}

fn unwrap_return_value(object: Object) -> Object {
    match object {
        Object::Return(return_value) => *return_value.value,
        _ => object,
    }
}

fn extended_function_environment(function: &object::Function, arguments: &[Object]) -> Env {
    let environment = Environment::new_enclosed_environment(Rc::clone(&function.env));

    for (i, name) in function.parameters.iter().enumerate() {
        environment.borrow_mut().set(name, &arguments[i]);
    }

    environment
}

fn eval_if_expression(if_expression: ast::IfExpression, environment: Env) -> Result<Object> {
    let condition = eval(
        Node::Expression(*if_expression.condition),
        Rc::clone(&environment),
    )?;

    if is_truthy(condition) {
        eval(
            Node::Statement(Statement::Block(if_expression.consequence)),
            environment,
        )
    } else if let Some(alternative) = if_expression.alternative {
        eval(Node::Statement(Statement::Block(alternative)), environment)
    } else {
        Ok(Object::Null)
    }
}

fn is_truthy(object: Object) -> bool {
    !matches!(object, Object::Null | Object::Boolean(false))
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_operator_expression(right: Object) -> Result<Object> {
    match right {
        Object::Int(value) => Ok(Object::Int(-value)),
        _ => Err(format!("unknown operator: -{}", right)),
    }
}

fn eval_prefix_expression(operator: Token, right: Object) -> Result<Object> {
    match operator {
        Token::Bang => Ok(eval_bang_operator_expression(right)),
        Token::Minus => eval_minus_operator_expression(right),
        _ => Err(format!("unknown operator: {}{}", operator, right)),
    }
}
fn eval_infix_expression(operator: Token, left: Object, right: Object) -> Result<Object> {
    let object = match (&operator, &left, &right) {
        (_, Object::Int(_), Object::Int(_)) => {
            return eval_integer_infix_expression(operator, left, right)
        }
        (Token::EQ, _, _) => Object::Boolean(left == right),
        (Token::NotEq, _, _) => Object::Boolean(left != right),
        (_, Object::String(_), Object::String(_)) => {
            eval_string_infix_expression(operator, left, right)?
        }
        (_, left, right) if !left.is_same_variant(right) => {
            return Err(format!("type mismatch: {} {} {}", left, operator, right))
        }
        _ => return Err(format!("unknown operator: {} {} {}", left, operator, right)),
    };
    Ok(object)
}

fn eval_integer_infix_expression(operator: Token, left: Object, right: Object) -> Result<Object> {
    let object = match (&operator, &left, &right) {
        (Token::Plus, Object::Int(left), Object::Int(right)) => Object::Int(left + right),
        (Token::Minus, Object::Int(left), Object::Int(right)) => Object::Int(left - right),
        (Token::Asterisk, Object::Int(left), Object::Int(right)) => Object::Int(left * right),
        (Token::Slash, Object::Int(left), Object::Int(right)) => Object::Int(left / right),
        (Token::LT, Object::Int(left), Object::Int(right)) => Object::Boolean(left < right),
        (Token::GT, Object::Int(left), Object::Int(right)) => Object::Boolean(left > right),
        (Token::EQ, Object::Int(left), Object::Int(right)) => Object::Boolean(left == right),
        (Token::NotEq, Object::Int(left), Object::Int(right)) => Object::Boolean(left != right),
        _ => {
            return Err(format!(
                "unknown operator: {} {} {}",
                &left, &operator, &right
            ))
        }
    };
    Ok(object)
}

fn eval_string_infix_expression(operator: Token, left: Object, right: Object) -> Result<Object> {
    if operator != Token::Plus {
        return Err(format!(
            "unknown operator: {} {} {}",
            &left, &operator, &right
        ));
    }

    let left_value = left.to_string();
    let right_value = right.to_string();
    let object = Object::String(left_value + &right_value);
    Ok(object)
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    match (left, index) {
        (Object::Array(array_literal), Object::Int(index)) => {
            let max = (array_literal.elements.len() - 1) as isize;

            if index < 0 || index > max {
                return Object::Error(format!("index out of range: {}", index));
            }
            array_literal.elements[index as usize].clone()
        }
        (Object::Hash(hash_literal), key) => match key {
            Object::Int(_) | Object::Boolean(_) | Object::String(_) => {
                if let Some(object) = hash_literal.pairs.get(&key) {
                    return object.clone();
                }
                Object::Null
            }
            _ => Object::Error(format!("unusable as hash key: {}", key)),
        },
        (left, _) => Object::Error(format!("index operator isn't supported: {}", left)),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast;
    use crate::evaluator::{eval, Result};
    use crate::lexer::Lexer;
    use crate::object;
    use crate::object::environment::Environment;
    use crate::object::{ArrayLiteral, Object};
    use crate::parser::Parser;
    use crate::token::Token;
    use std::collections::BTreeMap;

    fn test_eval(input: &str) -> Result<Object> {
        let lexer = Lexer::new(input.chars().collect());
        let mut parser = Parser::new(lexer).expect("a new parser to be created");
        let program = parser.parse().expect("the parse function to be successful");
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
            ("5 + true;", Err("type mismatch: 5 + true".to_string())),
            ("5 + true; 5;", Err("type mismatch: 5 + true".to_string())),
            ("-true;", Err("unknown operator: -true".to_string())),
            (
                "true + false;",
                Err("unknown operator: true + false".to_string()),
            ),
            (
                "5; true + false; 5",
                Err("unknown operator: true + false".to_string()),
            ),
            (
                "true + false + true + false;",
                Err("unknown operator: true + false".to_string()),
            ),
            (
                "5; true + false; 5",
                Err("unknown operator: true + false".to_string()),
            ),
            (
                "if (10 > 1) { true + false; }",
                Err("unknown operator: true + false".to_string()),
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                Err("unknown operator: true + false".to_string()),
            ),
            ("foobar;", Err("identifier not found: foobar".to_string())),
            (
                r#""Hello" - "World""#,
                Err("unknown operator: Hello - World".to_string()),
            ),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                Ok(Object::Error(
                    "unusable as hash key: fn(x) {\nx\n}".to_string(),
                )),
            ),
        ];

        for test in tests {
            let object = test_eval(test.0);
            assert_eq!(object, test.1);
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
                parameters: vec!["x".to_string()],
                body: ast::BlockStatement {
                    statements: vec![ast::Statement::Expression(ast::Expression::Infix(
                        ast::InfixExpression {
                            left: Box::new(ast::Expression::Identifier("x".to_string())),
                            operator: Token::Plus,
                            right: Box::new(ast::Expression::Int(2)),
                        },
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
        assert_eq!(object, Object::String("Hello World!".to_string()));

        Ok(())
    }

    #[test]
    fn array_literals() -> Result<()> {
        let input = "[1, 2 * 2, 3 + 3]";

        let object = test_eval(input)?;
        assert_eq!(
            object,
            Object::Array(object::ArrayLiteral {
                elements: vec![Object::Int(1), Object::Int(4), Object::Int(6)]
            })
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
            Object::Array(ArrayLiteral {
                elements: vec![
                    Object::Int(2),
                    Object::Int(4),
                    Object::Int(6),
                    Object::Int(8)
                ]
            })
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
            (Object::String("one".to_string()), Object::Int(1)),
            (Object::String("two".to_string()), Object::Int(2)),
            (Object::String("three".to_string()), Object::Int(3)),
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
}
