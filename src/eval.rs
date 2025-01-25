use crate::lexer::Token;
use crate::parser::ExpressionNode;
use crate::parser::StatementNode;

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
                StatementNode::Expression(ExpressionNode::Prefix(Token::Minus, Box::new(ExpressionNode::Integer(10)))),
                Object::Integer(-10),
            ),
            (
                StatementNode::Expression(ExpressionNode::Prefix(Token::Bang, Box::new(ExpressionNode::Boolean(true)))),
                Object::Boolean(false),
            ),
        ];
        for assert in assertions {
            let actual = eval(assert.0).unwrap();
            assert_eq!(assert.1, actual);
        }
    }
}
