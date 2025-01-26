use crate::lexer::{Lexer, Token};
use crate::parser::ExpressionNode::Call;
use std::cmp::PartialOrd;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct ParseError {
    token: Option<Token>,
    msg: String,
}

impl ParseError {
    fn eof() -> Self {
        Self {
            msg: "unexpected EOF".to_string(),
            token: None,
        }
    }

    fn unexpected_token(expected: Token, got: Token) -> ParseError {
        Self {
            msg: format!("unexpected token {got:?}"),
            token: Some(expected),
        }
    }

    fn unrecognized_token(found: Token) -> ParseError {
        Self {
            msg: format!("unrecognized token for current context {found:?}"),
            token: Some(found),
        }
    }

    fn malformed(value: &str) -> ParseError {
        Self {
            msg: format!("malformed input for type {value}"),
            token: None,
        }
    }
}

#[derive(Debug, PartialOrd, PartialEq)]
enum Precedence {
    Lowest = 0,
    Comparison = 1,
    Sum = 2,
    Product = 3,
    Prefix = 4,
}

impl Precedence {
    fn from_operator(op: &Token) -> Option<Precedence> {
        match op {
            Token::RightParenthesis => Some(Precedence::Lowest),
            Token::Plus => Some(Precedence::Sum),
            Token::Minus => Some(Precedence::Sum),
            Token::Asterisk => Some(Precedence::Product),
            Token::Slash => Some(Precedence::Product),
            Token::Equals => Some(Precedence::Comparison),
            Token::NotEquals => Some(Precedence::Comparison),
            Token::LessThan => Some(Precedence::Comparison),
            Token::GreaterThan => Some(Precedence::Comparison),
            _ => None,
        }
    }
}

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<ProgramNode, ParseError> {
        let mut program = ProgramNode {
            statements: VecDeque::new(),
        };
        while let Some(token) = self.lexer.next() {
            program.push(self.parse_statement(token)?);
        }
        Ok(program)
    }

    fn parse_statement(&mut self, token: Token) -> Result<StatementNode, ParseError> {
        match token {
            Token::Let => {
                let identifier = match self.lexer.next() {
                    Some(Token::Identifier(name)) => Ok(name),
                    Some(token) => Err(ParseError::unrecognized_token(token)),
                    None => Err(ParseError::eof()),
                }?;
                self.expect_next_token(Token::Assign)?;
                Ok(StatementNode::Let(
                    identifier,
                    self.parse_remaining_expression()?,
                ))
            }
            Token::Return => Ok(StatementNode::Return(self.parse_remaining_expression()?)),
            Token::LeftCurlyBrace => {
                let mut block = vec![];
                while let Some(peeked) = self.lexer.peek() {
                    if peeked == Token::RightCurlyBrace {
                        break;
                    }
                    self.lexer.next().unwrap();
                    block.push(self.parse_statement(peeked)?);
                }
                self.expect_next_token(Token::RightCurlyBrace)?;
                Ok(StatementNode::Block(block))
            }
            Token::If => {
                self.expect_next_token(Token::LeftParenthesis)?;
                let condition = match self.lexer.next() {
                    None => Err(ParseError::eof()),
                    Some(next) => self.parse_expression(next),
                }?;
                self.expect_next_token(Token::RightParenthesis)?;
                let consequence = match self.lexer.next() {
                    None => Err(ParseError::eof()),
                    Some(next) => self.parse_statement(next),
                }?;

                match self.lexer.peek() {
                    Some(Token::Else) => {
                        self.expect_next_token(Token::Else)?;
                        if let Some(next) = self.lexer.next() {
                            let alternative = self.parse_statement(next)?;
                            Ok(StatementNode::If(
                                condition,
                                Box::new(consequence),
                                Some(Box::new(alternative)),
                            ))
                        } else {
                            Err(ParseError::eof())
                        }
                    }
                    _ => Ok(StatementNode::If(condition, Box::new(consequence), None)),
                }
            }
            other => Ok(StatementNode::Expression(self.parse_expression(other)?)),
        }
    }

    fn parse_remaining_expression(&mut self) -> Result<Option<ExpressionNode>, ParseError> {
        match self.lexer.next() {
            Some(Token::Semicolon) => Ok(None),
            Some(token) => self.parse_expression(token).map(|expr| Some(expr)),
            None => Err(ParseError::eof()),
        }
    }

    fn parse_expression(&mut self, token: Token) -> Result<ExpressionNode, ParseError> {
        let expr = self.parse_expression_recursive(token, Precedence::Lowest)?;
        if let Some(Token::Semicolon) = self.lexer.peek() {
            self.lexer.next().unwrap();
        }
        Ok(expr)
    }

    fn parse_expression_recursive(
        &mut self,
        token: Token,
        precedence: Precedence,
    ) -> Result<ExpressionNode, ParseError> {
        let mut expr = match token {
            Token::Identifier(name) => Ok(ExpressionNode::Identifier(name)),
            Token::IntegerLiteral(value) => {
                let parsed = value
                    .parse::<i32>()
                    .map_err(|err| ParseError::malformed(value.as_str()))?;
                Ok(ExpressionNode::Integer(parsed))
            }
            Token::True => Ok(ExpressionNode::Boolean(true)),
            Token::False => Ok(ExpressionNode::Boolean(false)),
            Token::Bang => self.parse_prefix_expression(Token::Bang),
            Token::Minus => self.parse_prefix_expression(Token::Minus),
            Token::LeftParenthesis => {
                let expr = match self.lexer.next() {
                    None => Err(ParseError::eof()),
                    Some(next) => self.parse_expression_recursive(next, Precedence::Lowest),
                }?;
                self.expect_next_token(Token::RightParenthesis)?;
                Ok(expr)
            }
            Token::Function => {
                let params = self.parse_parameter_list()?;
                let body = match self.lexer.next() {
                    Some(next) => self.parse_statement(next),
                    _ => Err(ParseError::eof()),
                }?;
                Ok(ExpressionNode::Function(params, Box::new(body)))
            }
            _ => Err(ParseError::unrecognized_token(token)),
        }?;
        while let Some(peeked) = self.lexer.peek() {
            match peeked {
                Token::Semicolon => {
                    break;
                }
                Token::LeftParenthesis => {
                    expr = Call(Box::new(expr), self.parse_argument_list()?);
                }
                _ => match Precedence::from_operator(&peeked) {
                    None => break,
                    Some(other) => {
                        if precedence < other {
                            self.lexer.next();
                            expr = self.parse_infix_expression(peeked, expr)?;
                        } else {
                            break;
                        }
                    }
                },
            }
        }
        Ok(expr)
    }

    fn parse_prefix_expression(&mut self, operator: Token) -> Result<ExpressionNode, ParseError> {
        match self.lexer.next() {
            None => Err(ParseError::eof()),
            Some(expr) => Ok(ExpressionNode::Prefix(
                operator,
                Box::new(self.parse_expression_recursive(expr, Precedence::Prefix)?),
            )),
        }
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<Token>, ParseError> {
        self.expect_next_token(Token::LeftParenthesis)?;
        if let Some(Token::RightParenthesis) = self.lexer.peek() {
            self.lexer.next();
            return Ok(vec![]);
        }

        let mut params = vec![];
        loop {
            match self.lexer.next() {
                Some(Token::Identifier(name)) => params.push(Token::Identifier(name)),
                Some(other) => {
                    return Err(ParseError::unrecognized_token(other));
                }
                None => {
                    return Err(ParseError::eof());
                }
            }
            if let Some(Token::RightParenthesis) = self.lexer.peek() {
                break;
            } else {
                self.expect_next_token(Token::Comma)?;
            }
        }
        self.expect_next_token(Token::RightParenthesis)?;
        Ok(params)
    }

    fn parse_argument_list(&mut self) -> Result<Vec<ExpressionNode>, ParseError> {
        self.expect_next_token(Token::LeftParenthesis)?;
        if let Some(Token::RightParenthesis) = self.lexer.peek() {
            self.lexer.next();
            return Ok(vec![]);
        }

        let mut args = vec![];
        loop {
            match self.lexer.next() {
                Some(token) => {
                    args.push(self.parse_expression_recursive(token, Precedence::Lowest)?)
                }
                None => {
                    return Err(ParseError::eof());
                }
            }
            if let Some(Token::RightParenthesis) = self.lexer.peek() {
                break;
            } else {
                self.expect_next_token(Token::Comma)?;
            }
        }
        self.expect_next_token(Token::RightParenthesis)?;
        Ok(args)
    }

    fn parse_infix_expression(
        &mut self,
        operator: Token,
        left: ExpressionNode,
    ) -> Result<ExpressionNode, ParseError> {
        let precedence = Precedence::from_operator(&operator).unwrap();
        if let Some(next) = self.lexer.next() {
            Ok(ExpressionNode::Infix(
                operator,
                Box::new(left),
                Box::new(self.parse_expression_recursive(next, precedence)?),
            ))
        } else {
            Err(ParseError::eof())
        }
    }

    fn expect_next_token(&mut self, kind: Token) -> Result<Token, ParseError> {
        if let Some(token) = self.lexer.next() {
            if token == kind {
                Ok(token)
            } else {
                Err(ParseError::unexpected_token(kind, token))
            }
        } else {
            Err(ParseError::eof())
        }
    }

    fn seek_token(&mut self, kind: Token) -> Result<(), ParseError> {
        while let Some(token) = self.lexer.next() {
            if token == Token::Eof {
                return Err(ParseError::eof());
            } else if token == kind {
                return Ok(());
            }
        }
        Err(ParseError::eof())
    }
}

pub trait Node {
    fn literal(&self) -> String;
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum StatementNode {
    Let(String, Option<ExpressionNode>),
    Return(Option<ExpressionNode>),
    Expression(ExpressionNode),
    Block(Vec<StatementNode>),
    If(
        ExpressionNode,
        Box<StatementNode>,
        Option<Box<StatementNode>>,
    ),
}

impl Node for StatementNode {
    fn literal(&self) -> String {
        match self {
            StatementNode::Let(name, value) => {
                let mut out = format!("let {}", name);
                if let Some(expr) = value {
                    out += format!(" = {}", expr.literal()).as_str()
                }
                out += ";";
                out
            }
            StatementNode::Return(expr) => {
                let mut out = "return".to_string();
                if let Some(expr) = expr {
                    out += expr.literal().as_str();
                }
                out += ";";
                out
            }
            StatementNode::Expression(expr) => expr.literal(),
            StatementNode::Block(statements) => {
                statements.iter().map(|stmt| stmt.literal()).collect()
            }
            StatementNode::If(condition, consequence, alternative) => {
                format!("if {condition:?} then {consequence:?} else {alternative:?}")
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExpressionNode {
    Identifier(String),
    Integer(i32),
    Boolean(bool),
    Prefix(Token, Box<ExpressionNode>),
    Infix(Token, Box<ExpressionNode>, Box<ExpressionNode>),
    Function(Vec<Token>, Box<StatementNode>),
    Call(Box<ExpressionNode>, Vec<ExpressionNode>),
}

impl Node for ExpressionNode {
    fn literal(&self) -> String {
        match self {
            ExpressionNode::Identifier(name) => name.clone(),
            ExpressionNode::Integer(value) => value.to_string(),
            ExpressionNode::Boolean(value) => value.to_string(),
            ExpressionNode::Prefix(operator, left) => format!("{operator:?}{}", left.literal()),
            ExpressionNode::Infix(operator, left, right) => {
                format!("{}{operator:?}{}", left.literal(), right.literal())
            }
            ExpressionNode::Function(params, body) => {
                let params = params
                    .iter()
                    .map(|param| match param {
                        Token::Identifier(name) => name.clone(),
                        _ => panic!("invalid parameter token value {param:?}"),
                    })
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("fn({}) {}", params, body.literal())
            }
            ExpressionNode::Call(function, args) => {
                let args = args
                    .iter()
                    .map(|a| a.literal())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{}({})", function.literal(), args)
            }
        }
    }
}

pub struct ProgramNode {
    statements: VecDeque<StatementNode>,
}

impl ProgramNode {
    pub fn pop(&mut self) -> Option<StatementNode> {
        self.statements.pop_front()
    }

    pub fn push(&mut self, node: StatementNode) {
        self.statements.push_back(node)
    }

    pub fn len(&self) -> usize {
        self.statements.len()
    }
}

impl Node for ProgramNode {
    fn literal(&self) -> String {
        let root = self.statements.get(0);
        match root {
            Some(node) => node.literal(),
            _ => "".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_let() {
        let source = r#"
        let a = 5;
        let b = 10;
        "#;
        let mut parser = Parser::new(Lexer::new(source.chars().collect()));
        let mut program = parser.parse().unwrap();
        assert_eq!(2, program.len());
        for identifier in vec!["a", "b"] {
            let stmt = program.pop().unwrap();
            match stmt {
                StatementNode::Let(name, ..) => assert_eq!(identifier, name),
                _ => panic!("unexpected statement {stmt:?}"),
            }
        }
    }

    #[test]
    fn test_parse_return() {
        let source = r#"
        return 5;
        return 11012;
        "#;
        let mut parser = Parser::new(Lexer::new(source.chars().collect()));
        let program = parser.parse().unwrap();
        assert_eq!(2, program.len());
        for stmt in program.statements {
            match stmt {
                StatementNode::Return(..) => {}
                other => panic!("unexpected statement {other:?}"),
            }
        }
    }

    #[test]
    fn test_parse_identifier_expression() {
        let source = "foobar; foo; bar;";
        let mut parser = Parser::new(Lexer::new(source.chars().collect()));
        let mut program = parser.parse().unwrap();
        assert_eq!(3, program.len());
        for identifier in vec!["foobar", "foo", "bar"] {
            let stmt = program.pop();
            match stmt {
                Some(StatementNode::Expression(ExpressionNode::Identifier(name))) => {
                    assert_eq!(identifier, name);
                }
                Some(other) => panic!("unexpected statement {other:?}"),
                None => panic!("unexpected end of statements"),
            }
        }
    }

    #[test]
    fn test_parse_integer_expression() {
        let source = "5; 110;";
        let mut parser = Parser::new(Lexer::new(source.chars().collect()));
        let mut program = parser.parse().unwrap();
        assert_eq!(2, program.len());
        for literal in vec![5, 110] {
            match program.pop() {
                None => panic!("unexpected end on statements"),
                Some(StatementNode::Expression(ExpressionNode::Integer(value))) => {
                    assert_eq!(literal, value);
                }
                Some(other) => panic!("unexpected statement {other:?}"),
            }
        }
    }

    #[test]
    fn test_parse_boolean_expression() {
        let source = "true; false;";
        let mut parser = Parser::new(Lexer::new(source.chars().collect()));
        let mut program = parser.parse().unwrap();
        assert_eq!(2, program.len());
        for literal in vec![true, false] {
            match program.pop().unwrap() {
                StatementNode::Expression(ExpressionNode::Boolean(value)) => {
                    assert_eq!(literal, value);
                }
                other => panic!("unexpected statement {other:?}"),
            }
        }
    }

    #[test]
    fn test_parse_prefix_expression() {
        let assertions = vec![
            ("-5;", Token::Minus, Box::new(ExpressionNode::Integer(5))),
            (
                "!foo;",
                Token::Bang,
                Box::new(ExpressionNode::Identifier("foo".to_string())),
            ),
        ];
        for assertion in assertions {
            let mut parser = Parser::new(Lexer::new(assertion.0.chars().collect()));
            let mut program = parser.parse().unwrap();
            assert_eq!(1, program.len());
            match program.pop().unwrap() {
                StatementNode::Expression(ExpressionNode::Prefix(operator, expr)) => {
                    assert_eq!(assertion.1, operator);
                    assert_eq!(assertion.2, expr);
                }
                other => panic!("unexpected statement {other:?}"),
            }
        }
    }

    #[test]
    fn test_parse_infix_expression() {
        let tests = vec![
            (
                "5 + 5;",
                Box::new(ExpressionNode::Integer(5)),
                Token::Plus,
                Box::new(ExpressionNode::Integer(5)),
            ),
            (
                "-5 + 10;",
                Box::new(ExpressionNode::Prefix(
                    Token::Minus,
                    Box::new(ExpressionNode::Integer(5)),
                )),
                Token::Plus,
                Box::new(ExpressionNode::Integer(10)),
            ),
            (
                "-5 > 10 - 6;",
                Box::new(ExpressionNode::Prefix(
                    Token::Minus,
                    Box::new(ExpressionNode::Integer(5)),
                )),
                Token::GreaterThan,
                Box::new(ExpressionNode::Infix(
                    Token::Minus,
                    Box::new(ExpressionNode::Integer(10)),
                    Box::new(ExpressionNode::Integer(6)),
                )),
            ),
            (
                "true == false;",
                Box::new(ExpressionNode::Boolean(true)),
                Token::Equals,
                Box::new(ExpressionNode::Boolean(false)),
            ),
            (
                "false != false;",
                Box::new(ExpressionNode::Boolean(false)),
                Token::NotEquals,
                Box::new(ExpressionNode::Boolean(false)),
            ),
            (
                "(1 + 2) * 5;",
                Box::new(ExpressionNode::Infix(
                    Token::Plus,
                    Box::new(ExpressionNode::Integer(1)),
                    Box::new(ExpressionNode::Integer(2)),
                )),
                Token::Asterisk,
                Box::new(ExpressionNode::Integer(5)),
            ),
        ];
        for test in tests {
            let mut parser = Parser::new(Lexer::new(test.0.chars().collect()));
            let mut program = parser.parse().unwrap();
            assert_eq!(1, program.len());
            match program.pop().unwrap() {
                StatementNode::Expression(ExpressionNode::Infix(operator, left, right)) => {
                    assert_eq!(test.1, left);
                    assert_eq!(test.2, operator);
                    assert_eq!(test.3, right);
                }
                other => panic!("unexpected statement {other:?}"),
            }
        }
    }

    #[test]
    fn test_parse_if_statement() {
        let source = r#"
        if (a < b) {
            let c = 10;
            11 + 11;
        }
        "#;
        let mut parser = Parser::new(Lexer::new(source.chars().collect()));
        let mut program = parser.parse().unwrap();
        assert_eq!(1, program.len());
        match program.pop().unwrap() {
            StatementNode::If(condition, consequence, None) => {
                assert_eq!(
                    ExpressionNode::Infix(
                        Token::LessThan,
                        Box::new(ExpressionNode::Identifier("a".to_string())),
                        Box::new(ExpressionNode::Identifier("b".to_string())),
                    ),
                    condition
                );
                assert_eq!(
                    Box::new(StatementNode::Block(vec![
                        StatementNode::Let("c".to_string(), Some(ExpressionNode::Integer(10)),),
                        StatementNode::Expression(ExpressionNode::Infix(
                            Token::Plus,
                            Box::new(ExpressionNode::Integer(11)),
                            Box::new(ExpressionNode::Integer(11))
                        )),
                    ])),
                    consequence
                );
            }
            other => panic!("unexpected statement {other:?}"),
        }
    }

    fn test_parse_if_else_statement() {
        let source = r#"
        if (a < b) {
            11 + 11;
        } else {
            12 + 12;
        }
        "#;
        let mut parser = Parser::new(Lexer::new(source.chars().collect()));
        let mut program = parser.parse().unwrap();
        assert_eq!(1, program.len());
        match program.pop().unwrap() {
            StatementNode::If(condition, consequence, alternative) => {
                assert_eq!(
                    ExpressionNode::Infix(
                        Token::LessThan,
                        Box::new(ExpressionNode::Identifier("a".to_string())),
                        Box::new(ExpressionNode::Identifier("b".to_string())),
                    ),
                    condition
                );
                assert_eq!(
                    Box::new(StatementNode::Block(vec![StatementNode::Expression(
                        ExpressionNode::Infix(
                            Token::Plus,
                            Box::new(ExpressionNode::Integer(11)),
                            Box::new(ExpressionNode::Integer(11))
                        )
                    ),])),
                    consequence
                );
                assert_eq!(
                    Box::new(StatementNode::Block(vec![StatementNode::Expression(
                        ExpressionNode::Infix(
                            Token::Plus,
                            Box::new(ExpressionNode::Integer(12)),
                            Box::new(ExpressionNode::Integer(12))
                        )
                    ),])),
                    alternative.unwrap()
                );
            }
            other => panic!("unexpected statement {other:?}"),
        }
    }

    #[test]
    fn test_parse_function_expression() {
        let source = r#"
        let sum = fn(a, b) {
            return;
        }
        "#;
        let mut parser = Parser::new(Lexer::new(source.chars().collect()));
        let mut program = parser.parse().unwrap();
        assert_eq!(1, program.len());
        match program.pop().unwrap() {
            StatementNode::Let(name, Some(ExpressionNode::Function(params, body))) => {
                assert_eq!("sum".to_string(), name);
                for (i, param) in vec!["a", "b"].into_iter().enumerate() {
                    match params.get(i).unwrap() {
                        Token::Identifier(actual) => assert_eq!(param, actual),
                        other => panic!("unexpected parameter {other:?}"),
                    }
                }
                assert_eq!(
                    Box::new(StatementNode::Block(vec![StatementNode::Return(None)])),
                    body
                );
            }
            other => panic!("unexpected statement {other:?}"),
        }

        let source = r#"
        let todo = fn() {
            return;
        }
        "#;
        let mut parser = Parser::new(Lexer::new(source.chars().collect()));
        let mut program = parser.parse().unwrap();
        assert_eq!(1, program.len());
        match program.pop().unwrap() {
            StatementNode::Let(name, Some(ExpressionNode::Function(params, body))) => {
                assert_eq!("todo".to_string(), name);
                assert_eq!(0, params.len());
                assert_eq!(
                    Box::new(StatementNode::Block(vec![StatementNode::Return(None)])),
                    body
                );
            }
            other => panic!("unexpected statement {other:?}"),
        }
    }

    #[test]
    fn test_parse_call_expression() {
        let source = r#"
        add(a, b + 5);
        "#;
        let mut parser = Parser::new(Lexer::new(source.chars().collect()));
        let mut program = parser.parse().unwrap();
        assert_eq!(1, program.len());
        match program.pop().unwrap() {
            StatementNode::Expression(ExpressionNode::Call(function, arguments)) => {
                assert_eq!(2, arguments.len());
                match arguments.get(0).unwrap() {
                    ExpressionNode::Identifier(name) => assert_eq!("a", name),
                    other => panic!("unexpected parameter {other:?}"),
                }
                match arguments.get(1).unwrap() {
                    ExpressionNode::Infix(Token::Plus, left, right) => {
                        assert_eq!(Box::new(ExpressionNode::Identifier("b".to_string())), *left);
                        assert_eq!(Box::new(ExpressionNode::Integer(5)), *right);
                    }
                    other => panic!("unexpected parameter {other:?}"),
                }
            }
            other => panic!("unexpected statement {other:?}"),
        }

        let source = r#"
        fn(n, m) { return a + b; } (a, b + 5);
        "#;
        let mut parser = Parser::new(Lexer::new(source.chars().collect()));
        let mut program = parser.parse().unwrap();
        assert_eq!(1, program.len());
        match program.pop().unwrap() {
            StatementNode::Expression(ExpressionNode::Call(function, args)) => {
                assert_eq!(
                    Box::new(ExpressionNode::Function(
                        vec![
                            Token::Identifier("n".to_string()),
                            Token::Identifier("m".to_string())
                        ],
                        Box::new(StatementNode::Block(vec![StatementNode::Return(Some(
                            ExpressionNode::Infix(
                                Token::Plus,
                                Box::new(ExpressionNode::Identifier("a".to_string())),
                                Box::new(ExpressionNode::Identifier("b".to_string()))
                            )
                        ))]))
                    )),
                    function
                );
                assert_eq!(
                    vec![
                        ExpressionNode::Identifier("a".to_string()),
                        ExpressionNode::Infix(
                            Token::Plus,
                            Box::new(ExpressionNode::Identifier("b".to_string())),
                            Box::new(ExpressionNode::Integer(5))
                        )
                    ],
                    args
                );
            }
            other => panic!("unexpected statement {other:?}"),
        }
    }
}
