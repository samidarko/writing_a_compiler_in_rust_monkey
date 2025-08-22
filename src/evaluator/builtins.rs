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

pub fn get_builtin(name: &str) -> Object {
    match name {
        "len" => Object::Builtin(BuiltinFunction { f: builtin_len }),
        "first" => Object::Builtin(BuiltinFunction { f: builtin_first }),
        "last" => Object::Builtin(BuiltinFunction { f: builtin_last }),
        "rest" => Object::Builtin(BuiltinFunction { f: builtin_rest }),
        "push" => Object::Builtin(BuiltinFunction { f: builtin_push }),
        "puts" => Object::Builtin(BuiltinFunction { f: builtin_puts }),
        "exit" => Object::Builtin(BuiltinFunction { f: builtin_exit }),
        _builtin => Object::Error("unknown builtin".to_string()),
    }
}
