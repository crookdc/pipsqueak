use crate::lexer::Lexer;

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Option<ProgramNode> {
        todo!()
    }
}

trait Node {
    fn literal(&self) -> String;
}

enum StatementNode {
    Let(String, Option<ExpressionNode>),
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
        }
    }
}

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
    statements: Vec<StatementNode>,
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

}
