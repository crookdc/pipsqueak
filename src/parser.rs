use crate::lexer::{Lexer, Token};
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
            match token {
                Token::Let => program.push(self.parse_let(token)?),
                Token::Return => program.push(self.parse_return(token)?),
                other => program.push(StatementNode::Expression(self.parse_expression(other)?)),
            }
        }
        Ok(program)
    }

    fn parse_let(&mut self, parent: Token) -> Result<StatementNode, ParseError> {
        debug_assert_eq!(Token::Let, parent);
        let identifier;
        match self.lexer.next() {
            Some(Token::Identifier(name)) => identifier = name,
            Some(token) => {
                return Err(ParseError {
                    msg: format!("expected identifier but got {token:?}"),
                    token: Some(token),
                });
            }
            _ => {
                return Err(ParseError {
                    msg: "unexpected end of statement".to_string(),
                    token: Some(parent),
                });
            }
        }
        self.expect_next_token(Token::Assign)?;
        self.seek_token(Token::Semicolon)?;
        Ok(StatementNode::Let(identifier, None))
    }

    fn parse_return(&mut self, parent: Token) -> Result<StatementNode, ParseError> {
        debug_assert_eq!(Token::Return, parent);
        let expr = if let Some(token) = self.lexer.next() {
            match token {
                Token::Semicolon => Ok(None),
                other => Ok(Some(self.parse_expression(other)?)),
            }
        } else {
            Err(ParseError::eof())
        }?;
        Ok(StatementNode::Return(expr))
    }

    fn parse_expression(&mut self, token: Token) -> Result<ExpressionNode, ParseError> {
        match token {
            Token::Identifier(name) => {
                self.seek_token(Token::Semicolon)?;
                Ok(ExpressionNode::Identifier(name))
            }
            Token::IntegerLiteral(value) => {
                self.seek_token(Token::Semicolon)?;
                let parsed = value
                    .parse::<i32>()
                    .map_err(|err| ParseError::malformed(value.as_str()))?;
                Ok(ExpressionNode::Integer(parsed))
            }
            Token::Bang => self.parse_prefix_expression("!"),
            Token::Minus => self.parse_prefix_expression("-"),
            _ => Err(ParseError::unrecognized_token(token)),
        }
    }

    fn parse_prefix_expression(&mut self, operator: &str) -> Result<ExpressionNode, ParseError> {
        match self.lexer.next() {
            None => Err(ParseError::eof()),
            Some(expr) => Ok(ExpressionNode::Prefix(
                operator.to_string(),
                Box::new(self.parse_expression(expr)?),
            )),
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

trait Node {
    fn literal(&self) -> String;
}

#[derive(Debug)]
enum StatementNode {
    Let(String, Option<ExpressionNode>),
    Return(Option<ExpressionNode>),
    Expression(ExpressionNode),
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
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum ExpressionNode {
    Identifier(String),
    Integer(i32),
    Prefix(String, Box<ExpressionNode>),
}

impl Node for ExpressionNode {
    fn literal(&self) -> String {
        match self {
            ExpressionNode::Identifier(name) => name.clone(),
            ExpressionNode::Integer(value) => value.to_string(),
            ExpressionNode::Prefix(operator, expr) => format!("{operator}{}", expr.literal()),
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
    use std::ops::Deref;

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
        let mut program = parser.parse().unwrap();
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
    fn test_parse_prefix_expression() {
        let assertions = vec![
            ("-5;", "-", Box::new(ExpressionNode::Integer(5))),
            (
                "!foo;",
                "!",
                Box::new(ExpressionNode::Identifier("foo".to_string())),
            ),
        ];
        for assertion in assertions {
            let mut parser = Parser::new(Lexer::new(assertion.0.chars().collect()));
            let mut program = parser.parse().unwrap();
            assert_eq!(1, program.len());
            match program.pop() {
                Some(StatementNode::Expression(ExpressionNode::Prefix(operator, expr))) => {
                    assert_eq!(assertion.1, operator);
                    assert_eq!(assertion.2, expr);
                }
                Some(other) => panic!("unexpected statement {other:?}"),
                None => panic!("unexpected end of statements"),
            }
        }
    }
}
