use crate::lexer::Token;
use crate::parser::ExpressionNode;
use crate::parser::StatementNode;
use std::ops::{Add, Div, Mul, Not, Sub};

#[derive(Debug, Eq, PartialEq)]
pub enum Object {
    Nil,
    Integer(i32),
    Boolean(bool),
}

impl Object {
    fn less_than(self, rhs: Self) -> Result<Self, EvalError> {
        match self {
            Object::Integer(a) => match rhs {
                Object::Integer(b) => Ok(Object::Boolean(a < b)),
                other => Err(EvalError::unexpected_type(other)),
            },
            other => Err(EvalError::unexpected_type(other)),
        }
    }
}

impl Not for Object {
    type Output = Result<Self, EvalError>;

    fn not(self) -> Self::Output {
        match self {
            Object::Boolean(a) => Ok(Object::Boolean(!a)),
            other => Err(EvalError::unexpected_type(other)),
        }
    }
}

impl Add for Object {
    type Output = Result<Self, EvalError>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Object::Integer(a) => match rhs {
                Object::Integer(b) => Ok(Object::Integer(a + b)),
                other => Err(EvalError::unexpected_type(other)),
            },
            other => Err(EvalError::unexpected_type(other)),
        }
    }
}

impl Sub for Object {
    type Output = Result<Self, EvalError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Object::Integer(a) => match rhs {
                Object::Integer(b) => Ok(Object::Integer(a - b)),
                other => Err(EvalError::unexpected_type(other)),
            },
            other => Err(EvalError::unexpected_type(other)),
        }
    }
}

impl Mul for Object {
    type Output = Result<Self, EvalError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Object::Integer(a) => match rhs {
                Object::Integer(b) => Ok(Object::Integer(a * b)),
                other => Err(EvalError::unexpected_type(other)),
            },
            other => Err(EvalError::unexpected_type(other)),
        }
    }
}

impl Div for Object {
    type Output = Result<Self, EvalError>;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Object::Integer(a) => match rhs {
                Object::Integer(b) => Ok(Object::Integer(a / b)),
                other => Err(EvalError::unexpected_type(other)),
            },
            other => Err(EvalError::unexpected_type(other)),
        }
    }
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
            match operator {
                Token::Minus => Object::Integer(-1) * left,
                Token::Bang => !left,
                other => Err(EvalError::unexpected_operator(other)),
            }
        }
        ExpressionNode::Infix(operator, left, right) => {
            let left = eval_expression(*left)?;
            let right = eval_expression(*right)?;
            match operator {
                Token::Plus => left + right,
                Token::Minus => left - right,
                Token::Asterisk => left * right,
                Token::Slash => left / right,
                Token::LessThan => left.less_than(right),
                Token::GreaterThan => right.less_than(left),
                _ => todo!(),
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
        let assertions = vec![
            (
                StatementNode::Expression(ExpressionNode::Infix(
                    Token::Plus,
                    Box::new(ExpressionNode::Integer(10)),
                    Box::new(ExpressionNode::Prefix(
                        Token::Minus,
                        Box::new(ExpressionNode::Integer(20)),
                    )),
                )),
                Object::Integer(-10),
            ),
            (
                StatementNode::Expression(ExpressionNode::Infix(
                    Token::Minus,
                    Box::new(ExpressionNode::Integer(15)),
                    Box::new(ExpressionNode::Prefix(
                        Token::Minus,
                        Box::new(ExpressionNode::Integer(10)),
                    )),
                )),
                Object::Integer(25),
            ),
            (
                StatementNode::Expression(ExpressionNode::Infix(
                    Token::Asterisk,
                    Box::new(ExpressionNode::Integer(10)),
                    Box::new(ExpressionNode::Prefix(
                        Token::Minus,
                        Box::new(ExpressionNode::Integer(10)),
                    )),
                )),
                Object::Integer(-100),
            ),
            (
                StatementNode::Expression(ExpressionNode::Infix(
                    Token::Slash,
                    Box::new(ExpressionNode::Integer(10)),
                    Box::new(ExpressionNode::Integer(2)),
                )),
                Object::Integer(5),
            ),
            (
                StatementNode::Expression(ExpressionNode::Infix(
                    Token::LessThan,
                    Box::new(ExpressionNode::Integer(10)),
                    Box::new(ExpressionNode::Integer(2)),
                )),
                Object::Boolean(false),
            ),
            (
                StatementNode::Expression(ExpressionNode::Infix(
                    Token::GreaterThan,
                    Box::new(ExpressionNode::Integer(10)),
                    Box::new(ExpressionNode::Integer(2)),
                )),
                Object::Boolean(true),
            ),
        ];
        for assert in assertions {
            let actual = eval(assert.0).unwrap();
            assert_eq!(assert.1, actual);
        }
    }
}
