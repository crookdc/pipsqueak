#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Illegal(String),
    Eof,

    Identifier(String),
    IntegerLiteral(String),
    True,
    False,
    If,
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
            "else" => Some(Token::Else),
            "return" => Some(Token::Return),
            "true" => Some(Token::True),
            "false" => Some(Token::False),
            _ => None,
        }
    }
}

pub struct Lexer(usize, Vec<char>);

impl Lexer {
    pub fn new(source: Vec<char>) -> Self {
        Self(0, source)
    }

    fn peek(&self) -> Option<char> {
        self.1.get(self.0 + 1).cloned()
    }

    fn next_word(&mut self) -> String {
        let mut word = String::new();
        while self.0 < self.1.len() && Self::valid_identifier(self.1[self.0]) {
            word.push(self.1[self.0]);
            self.0 += 1;
        }
        self.0 -= 1;
        word
    }

    fn next_int(&mut self) -> String {
        let mut number = String::new();
        while self.0 < self.1.len() && self.1[self.0].is_numeric() {
            number.push(self.1[self.0]);
            self.0 += 1
        }
        self.0 -= 1;
        number
    }

    fn seek_non_whitespace(&mut self) {
        while self.0 < self.1.len() && self.1[self.0].is_whitespace() {
            self.0 += 1;
        }
    }

    fn valid_identifier(character: char) -> bool {
        character.is_alphabetic() || character == '_'
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.seek_non_whitespace();
        if self.0 >= self.1.len() {
            return None;
        }
        let character = self.1[self.0];
        let token = match character {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            '=' => match self.peek() {
                Some('=') => {
                    self.0 += 1;
                    Token::Equals
                }
                _ => Token::Assign,
            },
            '!' => match self.peek() {
                Some('=') => {
                    self.0 += 1;
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
        self.0 += 1;
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
            Token::True,
            Token::Semicolon,
            Token::RightCurlyBrace,
            Token::Else,
            Token::LeftCurlyBrace,
            Token::Return,
            Token::False,
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
