use crate::builtin;
use crate::lexer::Token;
use crate::parser::ExpressionNode;
use crate::parser::StatementNode;
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::ops::{Add, Div, Mul, Not, Sub};
use std::rc::Rc;

pub trait Environment {
    fn declared(&self, name: &String) -> bool;
    fn get(&self, name: &String) -> Object;
    fn set(&mut self, name: String, value: Object);
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BaseEnvironment {
    mem: HashMap<String, Object>,
}

impl BaseEnvironment {
    pub fn new() -> Self {
        let mut mem = HashMap::new();
        builtin::all().into_iter().for_each(|(name, func)| {
            mem.insert(name, Object::Builtin(func));
        });
        Self { mem }
    }
}

impl Environment for BaseEnvironment {
    fn declared(&self, name: &String) -> bool {
        self.mem.get(name).is_some()
    }

    fn get(&self, name: &String) -> Object {
        match self.mem.get(name) {
            None => Object::Nil,
            Some(value) => value.clone(),
        }
    }

    fn set(&mut self, name: String, value: Object) {
        self.mem.insert(name, value);
    }
}

pub struct ChildEnvironment {
    parent: Rc<RefCell<dyn Environment>>,
    mem: HashMap<String, Object>,
}

impl ChildEnvironment {
    fn new(parent: Rc<RefCell<dyn Environment>>) -> Self {
        Self {
            parent,
            mem: HashMap::new(),
        }
    }
}

impl Environment for ChildEnvironment {
    fn declared(&self, name: &String) -> bool {
        self.mem.get(name).is_some() || self.parent.borrow().declared(name)
    }

    fn get(&self, name: &String) -> Object {
        match self.mem.get(name) {
            None => self.parent.borrow().get(name),
            Some(value) => value.clone(),
        }
    }

    fn set(&mut self, name: String, value: Object) {
        self.mem.insert(name, value);
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Object {
    Nil,
    Integer(i32),
    String(String),
    Boolean(bool),
    Function(Vec<Token>, StatementNode),
    Builtin(fn(Vec<Object>) -> Object),
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
            Object::String(a) => match rhs {
                Object::String(b) => Ok(Object::String(a + b.as_str())),
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
    fn new(msg: &str) -> Self {
        Self {
            msg: msg.to_string(),
        }
    }

    fn unexpected_type(object: Object) -> EvalError {
        Self {
            msg: format!("unexpected type {object:?}"),
        }
    }

    fn unexpected_token(token: Token) -> EvalError {
        Self {
            msg: format!("unexpected token {token:?}"),
        }
    }

    fn mixed_operands(a: Object, b: Object) -> EvalError {
        Self {
            msg: format!("unexpected mixed operands {a:?} and {b:?}"),
        }
    }
}

pub struct Evaluator {
    env: Rc<RefCell<dyn Environment>>,
    queue: VecDeque<StatementNode>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(BaseEnvironment::new())),
            queue: VecDeque::new(),
        }
    }

    pub fn eval(&mut self, stmt: StatementNode) -> Result<(), EvalError> {
        self.queue.clear();
        self.queue.push_back(stmt);
        while let Some(stmt) = self.queue.pop_front() {
            self.eval_stmt(stmt)?;
        }
        Ok(())
    }

    fn eval_stmt(&mut self, stmt: StatementNode) -> Result<Object, EvalError> {
        match stmt {
            StatementNode::Let(name, expr) => {
                let value = match expr {
                    None => Ok(Object::Nil),
                    Some(expr) => self.eval_expr(expr),
                }?;
                self.env.borrow_mut().set(name, value);
                Ok(Object::Nil)
            }
            StatementNode::Assign(name, expr) => {
                if !self.env.borrow().declared(&name) {
                    return Err(EvalError::new(
                        format!("undeclared variable '{}'", name).as_str(),
                    ));
                }
                let value = match expr {
                    None => Ok(Object::Nil),
                    Some(expr) => self.eval_expr(expr),
                }?;
                self.env.borrow_mut().set(name, value);
                Ok(Object::Nil)
            }
            StatementNode::If(condition, consequence, alternative) => {
                match self.eval_expr(condition)? {
                    Object::Boolean(val) => {
                        if val {
                            self.eval_stmt(*consequence)
                        } else if let Some(alt) = alternative {
                            self.eval_stmt(*alt)
                        } else {
                            Ok(Object::Nil)
                        }
                    }
                    other => Err(EvalError::unexpected_type(other)),
                }
            }
            StatementNode::While(condition, body) => {
                if let Object::Boolean(true) = self.eval_expr(condition.clone())? {
                    self.eval_stmt(*body.clone())?;
                    self.queue.push_back(StatementNode::While(condition, body));
                }
                Ok(Object::Nil)
            }
            StatementNode::Block(stmts) => {
                let mut out = Object::Nil;
                for s in stmts {
                    out = self.eval_stmt(s)?;
                }
                Ok(out)
            }
            StatementNode::Return(expr) => match expr {
                None => Ok(Object::Nil),
                Some(expr) => self.eval_expr(expr),
            },
            StatementNode::Expression(expr) => self.eval_expr(expr),
        }
    }

    fn eval_expr(&self, expr: ExpressionNode) -> Result<Object, EvalError> {
        match expr {
            ExpressionNode::Identifier(val) => Ok(self.env.borrow().get(&val)),
            ExpressionNode::Integer(val) => Ok(Object::Integer(val)),
            ExpressionNode::String(val) => Ok(Object::String(val)),
            ExpressionNode::Boolean(val) => Ok(Object::Boolean(val)),
            ExpressionNode::Prefix(operator, left) => {
                let left = self.eval_expr(*left)?;
                match operator {
                    Token::Minus => Object::Integer(-1) * left,
                    Token::Bang => !left,
                    other => Err(EvalError::unexpected_token(other)),
                }
            }
            ExpressionNode::Infix(operator, left, right) => {
                let left = self.eval_expr(*left)?;
                let right = self.eval_expr(*right)?;
                match operator {
                    Token::Plus => left + right,
                    Token::Minus => left - right,
                    Token::Asterisk => left * right,
                    Token::Slash => left / right,
                    Token::LessThan => left.less_than(right),
                    Token::GreaterThan => right.less_than(left),
                    Token::Equals => Ok(Object::Boolean(left == right)),
                    Token::NotEquals => Ok(Object::Boolean(left != right)),
                    _ => Err(EvalError::unexpected_token(operator)),
                }
            }
            ExpressionNode::Function(params, body) => {
                Ok(Object::Function(params, body.as_ref().clone()))
            }
            ExpressionNode::Call(function, mut args) => {
                let function = self.eval_expr(*function)?;
                if let Object::Function(params, body) = function {
                    let mut scope = ChildEnvironment::new(self.env.clone());
                    for param in params.iter() {
                        if let Token::Identifier(name) = param {
                            scope.set(name.clone(), self.eval_expr(args.pop().unwrap())?)
                        } else {
                            return Err(EvalError::unexpected_token(param.clone()));
                        }
                    }
                    Ok(Self {
                        env: Rc::new(RefCell::new(scope)),
                        queue: VecDeque::new(),
                    }
                    .eval_stmt(body)?)
                } else if let Object::Builtin(builtin) = function {
                    let args = args
                        .into_iter()
                        .map(|arg| self.eval_expr(arg).unwrap())
                        .collect();
                    Ok(builtin(args))
                } else {
                    Err(EvalError::new("invalid function expression"))
                }
            }
        }
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
        let mut evaluator = Evaluator::new();
        for assert in assertions {
            let actual = evaluator.eval_stmt(assert.0).unwrap();
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
        let mut evaluator = Evaluator::new();
        for assert in assertions {
            let actual = evaluator.eval_stmt(assert.0).unwrap();
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
            (
                StatementNode::Expression(ExpressionNode::Infix(
                    Token::Equals,
                    Box::new(ExpressionNode::Integer(10)),
                    Box::new(ExpressionNode::Integer(2)),
                )),
                Object::Boolean(false),
            ),
            (
                StatementNode::Expression(ExpressionNode::Infix(
                    Token::Equals,
                    Box::new(ExpressionNode::Integer(10)),
                    Box::new(ExpressionNode::Boolean(true)),
                )),
                Object::Boolean(false),
            ),
            (
                StatementNode::Expression(ExpressionNode::Infix(
                    Token::Equals,
                    Box::new(ExpressionNode::Boolean(true)),
                    Box::new(ExpressionNode::Boolean(true)),
                )),
                Object::Boolean(true),
            ),
            (
                StatementNode::Expression(ExpressionNode::Infix(
                    Token::Equals,
                    Box::new(ExpressionNode::Boolean(false)),
                    Box::new(ExpressionNode::Boolean(false)),
                )),
                Object::Boolean(true),
            ),
            (
                StatementNode::Expression(ExpressionNode::Infix(
                    Token::Equals,
                    Box::new(ExpressionNode::Integer(10)),
                    Box::new(ExpressionNode::Integer(10)),
                )),
                Object::Boolean(true),
            ),
        ];
        let mut evaluator = Evaluator::new();
        for assert in assertions {
            let actual = evaluator.eval_stmt(assert.0).unwrap();
            assert_eq!(assert.1, actual);
        }
    }

    #[test]
    fn test_block_statement_eval() {
        let assertions = vec![
            (vec![], Object::Nil),
            (
                vec![StatementNode::Expression(ExpressionNode::Infix(
                    Token::Plus,
                    Box::new(ExpressionNode::Integer(10)),
                    Box::new(ExpressionNode::Integer(20)),
                ))],
                Object::Integer(30),
            ),
            (
                vec![StatementNode::Expression(ExpressionNode::Integer(-110))],
                Object::Integer(-110),
            ),
            (
                vec![StatementNode::Expression(ExpressionNode::Infix(
                    Token::Equals,
                    Box::new(ExpressionNode::Boolean(true)),
                    Box::new(ExpressionNode::Boolean(false)),
                ))],
                Object::Boolean(false),
            ),
        ];
        let mut evaluator = Evaluator::new();
        for assert in assertions {
            let out = evaluator.eval_stmt(StatementNode::Block(assert.0)).unwrap();
            assert_eq!(assert.1, out);
        }
    }

    #[test]
    fn test_return_statement_eval() {
        let assertions = vec![
            (None, Object::Nil),
            (Some(ExpressionNode::Integer(5)), Object::Integer(5)),
            (Some(ExpressionNode::Boolean(true)), Object::Boolean(true)),
            (
                Some(ExpressionNode::Infix(
                    Token::Asterisk,
                    Box::new(ExpressionNode::Integer(5)),
                    Box::new(ExpressionNode::Integer(10)),
                )),
                Object::Integer(50),
            ),
        ];
        let mut evaluator = Evaluator::new();
        for assert in assertions {
            let out = evaluator
                .eval_stmt(StatementNode::Return(assert.0))
                .unwrap();
            assert_eq!(assert.1, out);
        }
    }
}
