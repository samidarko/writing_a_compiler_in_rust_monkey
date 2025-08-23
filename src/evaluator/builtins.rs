use crate::object::{ArrayLiteral, BuiltinFunction, Object};

fn builtin_len(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    match &args[0] {
        Object::Array(array_literal) => Object::Int(array_literal.elements.len() as isize), // byte length like Go's len()
        Object::String(value) => Object::Int(value.len() as isize), // byte length like Go's len()
        other => Object::Error(format!("argument to `len` not supported, got {other}",)),
    }
}

fn builtin_first(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    match &args[0] {
        Object::Array(array_literal) if !array_literal.elements.is_empty() => {
            array_literal.elements[0].clone()
        }
        Object::Array(_array_literal) => {
            Object::Error("an empty array has no first element".to_string())
        }
        other => Object::Error(format!("argument to `first` must be ARRAY, got {other}",)),
    }
}

fn builtin_last(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    match &args[0] {
        Object::Array(array_literal) if !array_literal.elements.is_empty() => {
            array_literal.elements[array_literal.elements.len() - 1].clone()
        }
        Object::Array(_array_literal) => {
            Object::Error("an empty array has no last element".to_string())
        }
        other => Object::Error(format!("argument to `last` must be ARRAY, got {other}",)),
    }
}

fn builtin_rest(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    match &args[0] {
        Object::Array(array_literal) => {
            if !array_literal.elements.is_empty() {
                return Object::Array(ArrayLiteral {
                    elements: array_literal.elements[1..].to_vec(),
                });
            }
            Object::Array(ArrayLiteral { elements: vec![] })
        }
        other => Object::Error(format!("argument to `last` must be ARRAY, got {other}",)),
    }
}

fn builtin_push(args: &[Object]) -> Object {
    if args.len() != 2 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        ));
    }
    let element = args[1].clone();
    let array = args[0].clone();
    match array {
        Object::Array(mut array_literal) => {
            array_literal.elements.push(element);
            Object::Array(ArrayLiteral {
                elements: array_literal.elements,
            })
        }
        other => Object::Error(format!("argument to `last` must be ARRAY, got {other}",)),
    }
}

fn builtin_puts(args: &[Object]) -> Object {
    for arg in args {
        println!("{}", arg)
    }
    Object::Null
}

fn builtin_exit(args: &[Object]) -> Object {
    let exit_code = if args.is_empty() {
        0
    } else if args.len() == 1 {
        match &args[0] {
            Object::Int(code) => *code,
            other => {
                return Object::Error(format!(
                    "argument to `exit` must be an integer, got {}",
                    other
                ))
            }
        }
    } else {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=0 or 1",
            args.len()
        ));
    };

    Object::Exit(exit_code)
}

fn builtin_int(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    match &args[0] {
        Object::String(value) => {
            match value.parse::<isize>() {
                Ok(num) => Object::Int(num),
                Err(_) => Object::Error(format!(
                    "cannot convert string '{}' to integer",
                    value
                )),
            }
        }
        other => Object::Error(format!(
            "argument to `int` must be a string, got {}",
            other
        )),
    }
}

fn builtin_string(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    match &args[0] {
        Object::Int(value) => Object::String(value.to_string()),
        Object::Array(_) | Object::Hash(_) => {
            Object::String(format!("{}", args[0]))
        },
        Object::String(value) => Object::String(value.to_string()),
        Object::Boolean(value) => Object::String(value.to_string()),
        Object::Null => Object::String("null".to_string()),
        other => Object::Error(format!(
            "argument to `string` must be an integer, array, hash, string, boolean, or null, got {}",
            other
        )),
    }
}

pub fn get_builtin(name: &str) -> Object {
    match name {
        "len" => Object::Builtin(BuiltinFunction { f: builtin_len }),
        "first" => Object::Builtin(BuiltinFunction { f: builtin_first }),
        "last" => Object::Builtin(BuiltinFunction { f: builtin_last }),
        "rest" => Object::Builtin(BuiltinFunction { f: builtin_rest }),
        "push" => Object::Builtin(BuiltinFunction { f: builtin_push }),
        "puts" => Object::Builtin(BuiltinFunction { f: builtin_puts }),
        "exit" => Object::Builtin(BuiltinFunction { f: builtin_exit }),
        "int" => Object::Builtin(BuiltinFunction { f: builtin_int }),
        "string" => Object::Builtin(BuiltinFunction { f: builtin_string }),
        _builtin => Object::Error("unknown builtin".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::HashLiteral;
    use std::collections::BTreeMap;

    #[test]
    fn test_builtin_int() {
        let tests = vec![
            // Valid conversions
            (vec![Object::String("123".to_string())], Object::Int(123)),
            (vec![Object::String("0".to_string())], Object::Int(0)),
            (vec![Object::String("-456".to_string())], Object::Int(-456)),
            (vec![Object::String("007".to_string())], Object::Int(7)),
            
            // Error cases - invalid string format
            (vec![Object::String("abc".to_string())], Object::Error("cannot convert string 'abc' to integer".to_string())),
            (vec![Object::String("".to_string())], Object::Error("cannot convert string '' to integer".to_string())),
            (vec![Object::String("12.34".to_string())], Object::Error("cannot convert string '12.34' to integer".to_string())),
            
            // Error cases - wrong argument type
            (vec![Object::Int(123)], Object::Error("argument to `int` must be a string, got 123".to_string())),
            (vec![Object::Boolean(true)], Object::Error("argument to `int` must be a string, got true".to_string())),
            (vec![Object::Null], Object::Error("argument to `int` must be a string, got null".to_string())),
        ];

        for (args, expected) in tests {
            let result = builtin_int(&args);
            assert_eq!(result, expected);
        }

        // Test wrong number of arguments
        assert_eq!(
            builtin_int(&[]),
            Object::Error("wrong number of arguments. got=0, want=1".to_string())
        );
        assert_eq!(
            builtin_int(&[Object::String("123".to_string()), Object::String("456".to_string())]),
            Object::Error("wrong number of arguments. got=2, want=1".to_string())
        );
    }

    #[test]
    fn test_builtin_string() {
        let tests = vec![
            // Valid conversions
            (vec![Object::Int(123)], Object::String("123".to_string())),
            (vec![Object::Int(0)], Object::String("0".to_string())),
            (vec![Object::Int(-456)], Object::String("-456".to_string())),
            (vec![Object::Array(ArrayLiteral{
                elements: vec![Object::Int(1), Object::Int(2)]
            })], Object::String("[1, 2]".to_string())),
            (vec![Object::Hash(HashLiteral{
                pairs: BTreeMap::new()
            })], Object::String("{}".to_string())),
            (vec![Object::String("-456".to_string())], Object::String("-456".to_string())),
            (vec![Object::Boolean(true)], Object::String("true".to_string())),
            (vec![Object::Boolean(false)], Object::String("false".to_string())),
            (vec![Object::Null], Object::String("null".to_string())),
        ];

        for (args, expected) in tests {
            let result = builtin_string(&args);
            assert_eq!(result, expected);
        }

        // Test wrong number of arguments
        assert_eq!(
            builtin_string(&[]),
            Object::Error("wrong number of arguments. got=0, want=1".to_string())
        );
        assert_eq!(
            builtin_string(&[Object::Int(123), Object::Int(456)]),
            Object::Error("wrong number of arguments. got=2, want=1".to_string())
        );
    }

    #[test]
    fn test_get_builtin_casting_functions() {
        // Test that the casting functions are properly registered
        let int_builtin = get_builtin("int");
        let string_builtin = get_builtin("string");
        
        assert!(matches!(int_builtin, Object::Builtin(_)));
        assert!(matches!(string_builtin, Object::Builtin(_)));
        
        // Test that we can actually call them through the builtin interface
        if let Object::Builtin(int_fn) = int_builtin {
            let result = (int_fn.f)(&[Object::String("42".to_string())]);
            assert_eq!(result, Object::Int(42));
        }
        
        if let Object::Builtin(string_fn) = string_builtin {
            let result = (string_fn.f)(&[Object::Int(42)]);
            assert_eq!(result, Object::String("42".to_string()));
        }
    }
}
