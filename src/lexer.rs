#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Illegal(String),
    Eof,
    Identifier(String),
    IntegerLiteral(String),
    StringLiteral(String),
    TrueLiteral,
    FalseLiteral,
    If,
    While,
    LessThan,
    GreaterThan,
    Equals,
    NotEquals,
    Else,
    Return,
    Assign,
    Bang,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Comma,
    Semicolon,
    LeftParenthesis,
    RightParenthesis,
    LeftCurlyBrace,
    RightCurlyBrace,
    Function,
    Let,
}

impl Token {
    fn keyword(word: &String) -> Option<Token> {
        match word.as_str() {
            "let" => Some(Token::Let),
            "fn" => Some(Token::Function),
            "if" => Some(Token::If),
            "while" => Some(Token::While),
            "else" => Some(Token::Else),
            "return" => Some(Token::Return),
            "true" => Some(Token::TrueLiteral),
            "false" => Some(Token::FalseLiteral),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer {
    index: usize,
    source: Vec<char>,
}

impl Lexer {
    pub fn new(source: Vec<char>) -> Self {
        Self { index: 0, source }
    }

    fn peek_char(&self) -> Option<char> {
        self.source.get(self.index + 1).cloned()
    }

    fn next_word(&mut self) -> String {
        let mut word = String::new();
        while self.index < self.source.len() && Self::valid_identifier(self.source[self.index]) {
            word.push(self.source[self.index]);
            self.index += 1;
        }
        self.index -= 1;
        word
    }

    fn next_int(&mut self) -> String {
        let mut number = String::new();
        while self.index < self.source.len() && self.source[self.index].is_numeric() {
            number.push(self.source[self.index]);
            self.index += 1
        }
        self.index -= 1;
        number
    }

    fn seek_non_whitespace(&mut self) {
        while self.index < self.source.len() && self.source[self.index].is_whitespace() {
            self.index += 1;
        }
    }

    fn valid_identifier(character: char) -> bool {
        character.is_alphabetic() || character == '_'
    }

    pub fn peek(&mut self) -> Option<Token> {
        let previous = self.index;
        let item = self.next();
        self.index = previous;
        item
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.seek_non_whitespace();
        if self.index >= self.source.len() {
            return None;
        }
        let character = self.source[self.index];
        let token = match character {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            '=' => match self.peek_char() {
                Some('=') => {
                    self.index += 1;
                    Token::Equals
                }
                _ => Token::Assign,
            },
            '!' => match self.peek_char() {
                Some('=') => {
                    self.index += 1;
                    Token::NotEquals
                }
                _ => Token::Bang,
            },
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '(' => Token::LeftParenthesis,
            ')' => Token::RightParenthesis,
            '{' => Token::LeftCurlyBrace,
            '}' => Token::RightCurlyBrace,
            '\0' => Token::Eof,
            '"' => {
                self.index += 1;
                let start = self.index;
                while self.source[self.index] != '"' && self.source[self.index] != '\0' {
                    self.index += 1;
                }
                Token::StringLiteral(self.source[start..self.index].into_iter().collect())
            }
            _ => {
                if Self::valid_identifier(character) {
                    let word = self.next_word();
                    if let Some(keyword) = Token::keyword(&word) {
                        keyword
                    } else {
                        Token::Identifier(word)
                    }
                } else if character.is_numeric() {
                    Token::IntegerLiteral(self.next_int())
                } else {
                    Token::Illegal(character.to_string())
                }
            }
        };
        self.index += 1;
        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_next() {
        let input = r#"
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y
            };
            let result = add(five, ten);

            if (five * 2 < 15) {
                return true;
            } else {
                return false;
            }

            10 == 10
            10 != 10
        "#;
        let expected = vec![
            Token::Let,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::IntegerLiteral("5".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::IntegerLiteral("10".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LeftParenthesis,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::RightParenthesis,
            Token::LeftCurlyBrace,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::RightCurlyBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier("result".to_string()),
            Token::Assign,
            Token::Identifier("add".to_string()),
            Token::LeftParenthesis,
            Token::Identifier("five".to_string()),
            Token::Comma,
            Token::Identifier("ten".to_string()),
            Token::RightParenthesis,
            Token::Semicolon,
            Token::If,
            Token::LeftParenthesis,
            Token::Identifier("five".to_string()),
            Token::Asterisk,
            Token::IntegerLiteral("2".to_string()),
            Token::LessThan,
            Token::IntegerLiteral("15".to_string()),
            Token::RightParenthesis,
            Token::LeftCurlyBrace,
            Token::Return,
            Token::TrueLiteral,
            Token::Semicolon,
            Token::RightCurlyBrace,
            Token::Else,
            Token::LeftCurlyBrace,
            Token::Return,
            Token::FalseLiteral,
            Token::Semicolon,
            Token::RightCurlyBrace,
            Token::IntegerLiteral("10".to_string()),
            Token::Equals,
            Token::IntegerLiteral("10".to_string()),
            Token::IntegerLiteral("10".to_string()),
            Token::NotEquals,
            Token::IntegerLiteral("10".to_string()),
            Token::Eof,
        ];
        for (i, token) in Lexer::new(input.chars().collect()).enumerate() {
            assert_eq!(expected[i], token, "mismatch on token {i}");
        }
    }
}
