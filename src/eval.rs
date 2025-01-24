use crate::lexer::Token;
use crate::parser::StatementNode;
use crate::parser::ExpressionNode;

#[derive(Debug, Eq, PartialEq)]
pub enum Object {
    Nil,
    Integer(i32),
    Boolean(bool),
}

#[derive(Debug)]
pub struct EvalError {

}

pub fn eval(stmt: StatementNode) -> Result<Object, EvalError> {
    match stmt {
        StatementNode::Expression(expr) => eval_expression(expr),
        _ => todo!()
    }
}

fn eval_expression(expr: ExpressionNode) -> Result<Object, EvalError> {
    match expr {
        ExpressionNode::Integer(val) => Ok(Object::Integer(val)),
        _ => todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_eval() {
        let assertions = vec![
            (StatementNode::Expression(ExpressionNode::Integer(5)), Object::Integer(5)),
            (StatementNode::Expression(ExpressionNode::Integer(10)), Object::Integer(10))
        ];
        for assert in assertions {
            let actual = eval(assert.0).unwrap();
            assert_eq!(assert.1, actual);
        }
    }
}