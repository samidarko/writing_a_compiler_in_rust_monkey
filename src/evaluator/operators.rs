use crate::object::Object;
use crate::token::Token;

use super::{EvaluatorError, Result};

pub fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

pub fn eval_minus_operator_expression(right: Object) -> Result<Object> {
    match right {
        Object::Int(value) => Ok(Object::Int(-value)),
        _ => Err(EvaluatorError::type_error(format!("unknown operator: -{}", right))),
    }
}

pub fn eval_prefix_expression(operator: Token, right: Object) -> Result<Object> {
    match operator {
        Token::Bang => Ok(eval_bang_operator_expression(right)),
        Token::Minus => eval_minus_operator_expression(right),
        _ => Err(EvaluatorError::type_error(format!("unknown operator: {}{}", operator, right))),
    }
}

pub fn eval_infix_expression(operator: Token, left: Object, right: Object) -> Result<Object> {
    let object = match (&operator, &left, &right) {
        (_, Object::Int(_), Object::Int(_)) => {
            return eval_integer_infix_expression(operator, left, right)
        }
        (Token::Eq, _, _) => Object::Boolean(left == right),
        (Token::NotEq, _, _) => Object::Boolean(left != right),
        (_, Object::String(_), Object::String(_)) => {
            eval_string_infix_expression(operator, left, right)?
        }
        (_, left, right) if !left.is_same_variant(right) => {
            return Err(EvaluatorError::type_error(format!("type mismatch: {} {} {}", left, operator, right)))
        }
        _ => return Err(EvaluatorError::type_error(format!("unknown operator: {} {} {}", left, operator, right))),
    };
    Ok(object)
}

pub fn eval_integer_infix_expression(
    operator: Token,
    left: Object,
    right: Object,
) -> Result<Object> {
    let object = match (&operator, &left, &right) {
        (Token::Plus, Object::Int(left), Object::Int(right)) => Object::Int(left + right),
        (Token::Minus, Object::Int(left), Object::Int(right)) => Object::Int(left - right),
        (Token::Asterisk, Object::Int(left), Object::Int(right)) => Object::Int(left * right),
        (Token::Slash, Object::Int(left), Object::Int(right)) => Object::Int(left / right),
        (Token::Lt, Object::Int(left), Object::Int(right)) => Object::Boolean(left < right),
        (Token::Gt, Object::Int(left), Object::Int(right)) => Object::Boolean(left > right),
        (Token::Lte, Object::Int(left), Object::Int(right)) => Object::Boolean(left <= right),
        (Token::Gte, Object::Int(left), Object::Int(right)) => Object::Boolean(left >= right),
        (Token::Eq, Object::Int(left), Object::Int(right)) => Object::Boolean(left == right),
        (Token::NotEq, Object::Int(left), Object::Int(right)) => Object::Boolean(left != right),
        _ => {
            return Err(EvaluatorError::type_error(format!(
                "unknown operator: {} {} {}",
                &left, &operator, &right
            )))
        }
    };
    Ok(object)
}

pub fn eval_string_infix_expression(
    operator: Token,
    left: Object,
    right: Object,
) -> Result<Object> {
    if operator != Token::Plus {
        return Err(EvaluatorError::type_error(format!(
            "unknown operator: {} {} {}",
            &left, &operator, &right
        )));
    }

    // Extract the actual string values, not their Display representation
    let left_value = match left {
        Object::String(s) => s,
        _ => return Err(EvaluatorError::type_error("left operand is not a string")),
    };
    let right_value = match right {
        Object::String(s) => s,
        _ => return Err(EvaluatorError::type_error("right operand is not a string")),
    };
    
    let object = Object::String(left_value + &right_value);
    Ok(object)
}
