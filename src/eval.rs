use crate::lexer::Token;
use crate::parser::ExpressionNode;
use crate::parser::StatementNode;
use std::any::type_name_of_val;
use std::fmt::format;
use std::mem;
use std::ops::Add;

#[derive(Debug, Eq, PartialEq)]
pub enum Object {
    Nil,
    Integer(i32),
    Boolean(bool),
}

#[derive(Debug)]
pub struct EvalError {
    msg: String,
}

impl EvalError {
    fn unexpected_type(object: Object) -> EvalError {
        Self {
            msg: format!("unexpected type {object:?}"),
        }
    }

    fn unexpected_operator(token: Token) -> EvalError {
        Self {
            msg: format!("unexpected operator {token:?}"),
        }
    }

    fn mixed_operands(a: Object, b: Object) -> EvalError {
        Self {
            msg: format!("unexpected mixed operands {a:?} and {b:?}"),
        }
    }
}

pub fn eval(stmt: StatementNode) -> Result<Object, EvalError> {
    match stmt {
        StatementNode::Expression(expr) => eval_expression(expr),
        _ => todo!(),
    }
}

fn eval_expression(expr: ExpressionNode) -> Result<Object, EvalError> {
    match expr {
        ExpressionNode::Integer(val) => Ok(Object::Integer(val)),
        ExpressionNode::Boolean(val) => Ok(Object::Boolean(val)),
        ExpressionNode::Prefix(operator, left) => {
            let left = eval_expression(*left)?;
            match left {
                Object::Integer(value) => match operator {
                    Token::Minus => Ok(Object::Integer(-1 * value)),
                    other => Err(EvalError::unexpected_operator(other)),
                },
                Object::Boolean(value) => match operator {
                    Token::Bang => Ok(Object::Boolean(!value)),
                    other => Err(EvalError::unexpected_operator(other)),
                },
                other => Err(EvalError::unexpected_type(other)),
            }
        }
        ExpressionNode::Infix(operator, left, right) => {
            let left = eval_expression(*left)?;
            let right = eval_expression(*right)?;
            match operator {
                Token::Plus => {
                    if let Object::Integer(a) = left {
                        if let Object::Integer(b) = right {
                            Ok(Object::Integer(a + b))
                        } else {
                            Err(EvalError::mixed_operands(left, right))
                        }
                    } else {
                        Err(EvalError::unexpected_type(left))
                    }
                },
                _ => todo!()
            }
        }
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_eval() {
        let assertions = vec![
            (
                StatementNode::Expression(ExpressionNode::Integer(5)),
                Object::Integer(5),
            ),
            (
                StatementNode::Expression(ExpressionNode::Integer(10)),
                Object::Integer(10),
            ),
        ];
        for assert in assertions {
            let actual = eval(assert.0).unwrap();
            assert_eq!(assert.1, actual);
        }
    }

    #[test]
    fn test_prefix_expression_eval() {
        let assertions = vec![
            (
                StatementNode::Expression(ExpressionNode::Prefix(
                    Token::Minus,
                    Box::new(ExpressionNode::Integer(10)),
                )),
                Object::Integer(-10),
            ),
            (
                StatementNode::Expression(ExpressionNode::Prefix(
                    Token::Bang,
                    Box::new(ExpressionNode::Boolean(true)),
                )),
                Object::Boolean(false),
            ),
        ];
        for assert in assertions {
            let actual = eval(assert.0).unwrap();
            assert_eq!(assert.1, actual);
        }
    }

    #[test]
    fn test_infix_expression_eval() {
        let assertions = vec![(
            StatementNode::Expression(ExpressionNode::Infix(
                Token::Plus,
                Box::new(ExpressionNode::Integer(10)),
                Box::new(ExpressionNode::Prefix(Token::Minus, Box::new(ExpressionNode::Integer(20)))),
            )),
            Object::Integer(-10),
        )];
        for assert in assertions {
            let actual = eval(assert.0).unwrap();
            assert_eq!(assert.1, actual);
        }
    }
}
