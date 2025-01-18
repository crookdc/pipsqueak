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
                _ => todo!(),
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
        let expr = if let Ok(e) = self.parse_expression() {
            Some(e)
        } else {
            None
        };
        self.seek_token(Token::Semicolon)?;
        Ok(StatementNode::Return(expr))
    }

    fn parse_expression(&mut self) -> Result<ExpressionNode, ParseError> {
        Err(ParseError {
            msg: "not implemented".to_string(),
            token: None,
        })
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
        }
    }
}

#[derive(Debug)]
enum ExpressionNode {
    Identifier(String),
}

impl Node for ExpressionNode {
    fn literal(&self) -> String {
        match self {
            ExpressionNode::Identifier(name) => name.clone(),
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
        let mut program = parser.parse().unwrap();
        assert_eq!(2, program.len());
        for stmt in program.statements {
            match stmt {
                StatementNode::Return(..) => {}
                _ => panic!("unexpected statement {stmt:?}"),
            }
        }
    }
}
