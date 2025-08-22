use crate::ast;
use crate::ast::{BlockStatement, Program, Statement};
use crate::object::environment::Env;
use crate::object::{Object, ReturnValue};
use std::rc::Rc;

use super::{eval_expression, Result};

pub fn eval_program(program: Program, environment: Env) -> Result<Object> {
    let mut result: Object = Object::Null;
    for statement in program.statements {
        let object = eval_statement(statement, Rc::clone(&environment))?;

        if let Object::Return(return_value) = object {
            return Ok(*return_value.value);
        }

        result = object;
    }

    Ok(result)
}

pub fn eval_block_statement(block_statement: BlockStatement, environment: Env) -> Result<Object> {
    let mut result: Object = Object::Null;
    for statement in block_statement.statements {
        let object = eval_statement(statement, Rc::clone(&environment))?;

        if let Object::Return(_) = object {
            return Ok(object);
        }

        result = object;
    }

    Ok(result)
}

pub fn eval_statement(statement: Statement, environment: Env) -> Result<Object> {
    match statement {
        Statement::Expression(expression) => eval_expression(expression, environment),
        Statement::Let(ast::LetStatement { name, value }) => {
            let obj = eval_expression(value, Rc::clone(&environment))?;
            environment.borrow_mut().set(&name, &obj);
            Ok(obj)
        }
        Statement::Return(return_statement) => {
            let object = eval_expression(return_statement.value, environment)?;
            Ok(Object::Return(ReturnValue {
                value: Box::new(object),
            }))
        }
        Statement::Block(block_statement) => eval_block_statement(block_statement, environment),
    }
}
