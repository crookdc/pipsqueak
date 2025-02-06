use crate::builtin;
use crate::lexer::{Lexer, Token};
use crate::parser::StatementNode;
use crate::parser::{ExpressionNode, Parser};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::fs;
use std::ops::{Add, Div, Mul, Not, Sub};
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    mem: HashMap<String, Object>,
}

impl Environment {
    fn base() -> Self {
        let mut mem = HashMap::new();
        builtin::all().into_iter().for_each(|(name, func)| {
            mem.insert(name, Object::Builtin(func));
        });
        Self { parent: None, mem }
    }

    fn child(parent: Rc<RefCell<Environment>>) -> Self {
        Self {
            parent: Some(parent),
            mem: HashMap::new(),
        }
    }

    fn declared(&self, name: &String) -> bool {
        if let Some(parent) = &self.parent {
            parent.borrow().declared(name)
        } else {
            self.mem.get(name).is_some()
        }
    }

    fn get(&self, name: &String) -> Object {
        match self.mem.get(name) {
            None => {
                if let Some(parent) = &self.parent {
                    parent.borrow().get(name)
                } else {
                    Object::Nil
                }
            }
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
    List(Vec<Object>),
    Function(Vec<Token>, StatementNode),
    Builtin(fn(Vec<Object>) -> Object),
    Return(Box<Object>),
    Module(Evaluator),
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

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Object::Nil => "nil".to_string(),
            Object::Integer(val) => val.to_string(),
            Object::String(val) => val.to_string(),
            Object::Boolean(val) => val.to_string(),
            _ => String::new(),
        };
        write!(f, "{}", str)
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
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Evaluator {
    working_directory: Box<PathBuf>,
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new(working_directory: Box<PathBuf>) -> Self {
        Self {
            working_directory,
            env: Rc::new(RefCell::new(Environment::base())),
        }
    }

    pub fn eval_stmt(&mut self, stmt: StatementNode) -> Result<Object, EvalError> {
        match stmt {
            StatementNode::Let(name, expr) => {
                self.env.borrow_mut().set(name.clone(), Object::Nil);
                self.eval_assign(name, expr)?;
                Ok(Object::Nil)
            }
            StatementNode::Assign(name, expr) => {
                self.eval_assign(name, expr)?;
                Ok(Object::Nil)
            }
            StatementNode::If(condition, consequence, alternative) => {
                match self.eval_expr(condition)? {
                    Object::Boolean(true) => self.eval_stmt(*consequence),
                    Object::Boolean(false) => {
                        if let Some(alt) = alternative {
                            self.eval_stmt(*alt)
                        } else {
                            Ok(Object::Nil)
                        }
                    }
                    other => Err(EvalError::unexpected_type(other)),
                }
            }
            StatementNode::While(condition, body) => {
                let mut out = Object::Nil;
                while let Object::Boolean(true) = self.eval_expr(condition.clone())? {
                    out = self.eval_stmt(*body.clone())?;
                    if let Object::Return(_) = &out {
                        break;
                    }
                }
                Ok(out)
            }
            StatementNode::Block(stmts) => {
                let mut out = Object::Nil;
                for s in stmts {
                    out = self.eval_stmt(s)?;
                    if let Object::Return(_) = &out {
                        break;
                    }
                }
                Ok(out)
            }
            StatementNode::Return(expr) => match expr {
                None => Ok(Object::Return(Box::new(Object::Nil))),
                Some(expr) => Ok(Object::Return(Box::new(self.eval_expr(expr)?))),
            },
            StatementNode::Expression(expr) => self.eval_expr(expr),
        }
    }

    fn eval_assign(&mut self, name: String, expr: Option<ExpressionNode>) -> Result<(), EvalError> {
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
        Ok(())
    }

    fn eval_expr(&self, expr: ExpressionNode) -> Result<Object, EvalError> {
        match expr {
            ExpressionNode::Identifier(val) => Ok(self.env.borrow().get(&val)),
            ExpressionNode::Integer(val) => Ok(Object::Integer(val)),
            ExpressionNode::String(val) => Ok(Object::String(val)),
            ExpressionNode::Boolean(val) => Ok(Object::Boolean(val)),
            ExpressionNode::List(vals) => Ok(Object::List(
                vals.into_iter()
                    .map(|e| self.eval_expr(e).unwrap())
                    .collect(),
            )),
            ExpressionNode::Prefix(operator, left) => {
                let left = self.eval_expr(*left)?;
                match operator {
                    Token::Minus => Object::Integer(-1) * left,
                    Token::Bang => !left,
                    other => Err(EvalError::unexpected_token(other)),
                }
            }
            ExpressionNode::Infix(operator, left, right) => {
                if let Token::FullStop = operator {
                    let left = self.eval_expr(*left)?;
                    return self.eval_mod_resolution(left, *right);
                }
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
            ExpressionNode::Function(params, body) => Ok(Object::Function(params, *body)),
            ExpressionNode::Index(list, index) => match self.eval_expr(*list)? {
                Object::List(elements) => match self.eval_expr(*index)? {
                    Object::Integer(index) => Ok(elements[index as usize].clone()),
                    other => Err(EvalError::unexpected_type(other)),
                },
                other => Err(EvalError::unexpected_type(other)),
            },
            ExpressionNode::Call(function, args) => self.eval_call(function, args),
            ExpressionNode::Import(path) => {
                let mut path = self.working_directory.join(path);
                let src = fs::read_to_string(path.clone()).unwrap();
                let mut program = Parser::new(Lexer::new(src.chars().collect()))
                    .parse()
                    .unwrap();
                // Popping the path ensures that the working directory of the child evaluator is
                // just that, a directory. This should always be valid since imports can only occur
                // on files.
                path.pop();
                let mut module = Self::new(Box::new(path));
                while let Some(stmt) = program.pop() {
                    module.eval_stmt(stmt)?;
                }
                Ok(Object::Module(module))
            }
        }
    }

    fn eval_mod_resolution(&self, id: Object, expr: ExpressionNode) -> Result<Object, EvalError> {
        match id {
            Object::Module(eval) => eval.eval_expr(expr),
            other => Err(EvalError::unexpected_type(other)),
        }
    }

    fn eval_call(
        &self,
        function: Box<ExpressionNode>,
        args: Vec<ExpressionNode>,
    ) -> Result<Object, EvalError> {
        let function = self.eval_expr(*function)?;
        if let Object::Function(params, body) = function {
            self.eval_user_func(params, self.eval_args(args), body)
        } else if let Object::Builtin(builtin) = function {
            Ok(builtin(self.eval_args(args)))
        } else {
            Err(EvalError::new("invalid function expression"))
        }
    }

    fn eval_args(&self, args: Vec<ExpressionNode>) -> Vec<Object> {
        args.into_iter()
            .map(|arg| self.eval_expr(arg).unwrap())
            .collect()
    }

    fn eval_user_func(
        &self,
        params: Vec<Token>,
        mut args: Vec<Object>,
        body: StatementNode,
    ) -> Result<Object, EvalError> {
        let mut scope = Environment::child(self.env.clone());
        for param in params.iter() {
            if let Token::Identifier(name) = param {
                scope.set(name.clone(), args.pop().unwrap());
            } else {
                return Err(EvalError::unexpected_token(param.clone()));
            }
        }
        let mut child = Self {
            working_directory: self.working_directory.clone(),
            env: Rc::new(RefCell::new(scope)),
        };
        match child.eval_stmt(body)? {
            Object::Return(value) => Ok(*value),
            other => Ok(other),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

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
        let mut evaluator = Evaluator::new(Box::new(PathBuf::from(Path::new("."))));
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
        let mut evaluator = Evaluator::new(Box::new(PathBuf::from(Path::new("."))));
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
        let mut evaluator = Evaluator::new(Box::new(PathBuf::from(Path::new("."))));
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
        let mut evaluator = Evaluator::new(Box::new(PathBuf::from(Path::new("."))));
        for assert in assertions {
            let out = evaluator.eval_stmt(StatementNode::Block(assert.0)).unwrap();
            assert_eq!(assert.1, out);
        }
    }
}
