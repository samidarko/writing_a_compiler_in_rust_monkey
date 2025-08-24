use crate::ast::{Expression, Node, Statement};
use crate::object::environment::{Env, Environment};
use crate::object::{ArrayLiteral, HashLiteral, Object};
use crate::{ast, object};
use smallvec::SmallVec;
use std::collections::BTreeMap;
use std::rc::Rc;

use super::{
    builtins, eval, eval_infix_expression, eval_prefix_expression, EvaluatorError, Result,
};

pub fn eval_expression(expression: ast::Expression, environment: Env) -> Result<Object> {
    let object = match expression {
        Expression::Identifier(name) => {
            let identifier = environment.borrow().get(&name);
            if let Some(value) = identifier {
                return Ok(value);
            }

            if let Object::Builtin(builtin) = builtins::get_builtin(&name) {
                return Ok(Object::Builtin(builtin));
            }

            return Err(EvaluatorError::identifier_not_found(name));
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
            body: *function_literal.body,
            env: Rc::clone(&environment), // capture shared env
        }),
        Expression::Call(call_expression) => {
            let func_obj = eval(
                Node::Expression(*call_expression.function),
                Rc::clone(&environment),
            )?;
            let args = eval_expressions(
                call_expression
                    .arguments
                    .iter()
                    .map(|e| (**e).clone())
                    .collect::<Vec<_>>()
                    .into(),
                Rc::clone(&environment),
            )?;
            return apply_function(func_obj, &args);
        }
        Expression::Array(array_literal) => {
            let elements = eval_expressions(
                array_literal
                    .elements
                    .iter()
                    .map(|e| (**e).clone())
                    .collect::<Vec<_>>()
                    .into(),
                Rc::clone(&environment),
            )?;
            if elements.len() == 1 {
                if let Object::Error(_) = elements[0] {
                    return Ok(elements[0].clone());
                }
            }
            Object::Array(Box::new(ArrayLiteral {
                elements: elements.into_iter().map(Box::new).collect(),
            }))
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
        Expression::Assignment(assignment_expression) => {
            // Check if variable exists before allowing assignment
            if environment
                .borrow()
                .get(&assignment_expression.name)
                .is_none()
            {
                return Err(EvaluatorError::identifier_not_found(
                    &*assignment_expression.name,
                ));
            }

            let value = eval(
                Node::Expression(*assignment_expression.value),
                Rc::clone(&environment),
            )?;
            environment
                .borrow_mut()
                .set(&assignment_expression.name, &value);
            return Ok(value.clone()); // Assignment returns the assigned value
        }
        Expression::While(while_expression) => {
            return eval_while_expression(while_expression, environment);
        }
        Expression::For(for_expression) => {
            return eval_for_expression(for_expression, environment);
        }
    };
    Ok(object)
}

pub fn eval_expressions(
    expressions: SmallVec<[Expression; 4]>,
    environment: Env,
) -> Result<Vec<Object>> {
    let mut objects = vec![];

    for expression in expressions {
        let object = eval(Node::Expression(expression), Rc::clone(&environment))?;
        objects.push(object);
    }

    Ok(objects)
}

#[allow(clippy::mutable_key_type)]
pub fn eval_hash_literal(expression: Expression, environment: Env) -> Result<Object> {
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

pub fn apply_function(function: Object, arguments: &[Object]) -> Result<Object> {
    match function {
        Object::Function(f) => {
            let env = extended_function_environment(&f, arguments);
            let evaluated = eval(Node::Statement(Statement::Block(f.body)), env)?;
            Ok(unwrap_return_value(evaluated))
        }
        Object::Builtin(f) => Ok((f.f)(arguments)),
        _ => Err(EvaluatorError::invalid_function_call(format!(
            "not a function: {}",
            function
        ))),
    }
}

pub fn unwrap_return_value(object: Object) -> Object {
    match object {
        Object::Return(return_value) => *return_value.value,
        _ => object,
    }
}

pub fn extended_function_environment(function: &object::Function, arguments: &[Object]) -> Env {
    let environment = Environment::new_enclosed_environment(Rc::clone(&function.env));

    for (i, name) in function.parameters.iter().enumerate() {
        environment.borrow_mut().set(name, &arguments[i]);
    }

    environment
}

pub fn eval_if_expression(if_expression: ast::IfExpression, environment: Env) -> Result<Object> {
    let condition = eval(
        Node::Expression(*if_expression.condition),
        Rc::clone(&environment),
    )?;

    if is_truthy(condition) {
        eval(
            Node::Statement(Statement::Block(*if_expression.consequence)),
            environment,
        )
    } else if let Some(alternative) = if_expression.alternative {
        eval(Node::Statement(Statement::Block(*alternative)), environment)
    } else {
        Ok(Object::Null)
    }
}

pub fn is_truthy(object: Object) -> bool {
    !matches!(object, Object::Null | Object::Boolean(false))
}

pub fn eval_index_expression(left: Object, index: Object) -> Object {
    match (left, index) {
        (Object::Array(array_literal), Object::Int(index)) => {
            let max = (array_literal.elements.len() - 1) as isize;

            if index < 0 || index > max {
                return Object::Error(format!("index out of range: {}", index));
            }
            *array_literal.elements[index as usize].clone()
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

pub fn eval_while_expression(
    while_expression: ast::WhileExpression,
    environment: Env,
) -> Result<Object> {
    let mut result = Object::Null;

    loop {
        let condition = eval(
            Node::Expression(*while_expression.condition.clone()),
            Rc::clone(&environment),
        )?;

        if !is_truthy(condition) {
            break;
        }

        result = eval(
            Node::Statement(Statement::Block(*while_expression.body.clone())),
            Rc::clone(&environment),
        )?;

        // Handle return values - if we encounter a return, break out of the loop
        if matches!(result, Object::Return(_)) {
            break;
        }
    }

    Ok(result)
}

pub fn eval_for_expression(for_expression: ast::ForExpression, environment: Env) -> Result<Object> {
    let mut result = Object::Null;

    // Evaluate the collection expression
    let collection = eval(
        Node::Expression(*for_expression.collection.clone()),
        Rc::clone(&environment),
    )?;

    match collection {
        Object::Array(array) => {
            // Save the original value of the loop variable (if it exists)
            let original_value = environment.borrow().get(&for_expression.variable);

            for element in array.elements.iter() {
                // Set the loop variable to the current array element in the current environment
                environment
                    .borrow_mut()
                    .set(&for_expression.variable, element);

                result = eval(
                    Node::Statement(Statement::Block(*for_expression.body.clone())),
                    Rc::clone(&environment),
                )?;

                // Handle return values - if we encounter a return, break out of the loop
                if matches!(result, Object::Return(_)) {
                    break;
                }
            }

            // Restore the original value of the loop variable (or remove it if it didn't exist)
            match original_value {
                Some(value) => environment
                    .borrow_mut()
                    .set(&for_expression.variable, &value),
                None => {
                    // Remove the loop variable from the environment
                    environment
                        .borrow_mut()
                        .store
                        .remove(for_expression.variable.as_ref());
                }
            }
        }
        Object::Hash(hash_map) => {
            // Save the original value of the loop variable (if it exists)
            let original_value = environment.borrow().get(&for_expression.variable);

            // For hash maps, iterate over keys only
            for key in hash_map.pairs.keys() {
                // Set the loop variable to the current hash key in the current environment
                environment.borrow_mut().set(&for_expression.variable, key);

                result = eval(
                    Node::Statement(Statement::Block(*for_expression.body.clone())),
                    Rc::clone(&environment),
                )?;

                // Handle return values - if we encounter a return, break out of the loop
                if matches!(result, Object::Return(_)) {
                    break;
                }
            }

            // Restore the original value of the loop variable (or remove it if it didn't exist)
            match original_value {
                Some(value) => environment
                    .borrow_mut()
                    .set(&for_expression.variable, &value),
                None => {
                    // Remove the loop variable from the environment
                    environment
                        .borrow_mut()
                        .store
                        .remove(for_expression.variable.as_ref());
                }
            }
        }
        _ => {
            return Err(EvaluatorError::type_error(format!(
                "for loop collection must be an array or hash map, got {}",
                collection
            )));
        }
    }

    Ok(result)
}
