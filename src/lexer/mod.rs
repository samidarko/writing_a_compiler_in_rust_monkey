mod fmt;

use crate::token::Token;
use std::{num, result};

#[derive(Debug, PartialEq, Eq)]
pub enum LexerError {
    IllegalInteger(num::ParseIntError),
}
pub type Result<T> = result::Result<T, LexerError>;

pub struct Lexer {
    input: Vec<char>,
    position: usize,      // current position in input (points to current char)
    read_position: usize, // current reading position in input (after current char)
    ch: char,             // current char under examination
}

impl Lexer {
    pub fn new(input: Vec<char>) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    // TODO maybe not all methods need to be public

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let tok = match self.ch {
            '=' if self.peek_char() == '=' => {
                self.read_char();
                Token::EQ
            }
            '=' => Token::Assign,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' if self.peek_char() == '=' => {
                self.read_char();
                Token::NotEq
            }
            '!' => Token::Bang,
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            '<' => Token::LT,
            '>' => Token::GT,
            ';' => Token::Semicolon,
            ',' => Token::Comma,
            ':' => Token::Colon,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '"' => Token::String(self.read_string()),
            '\u{0000}' => Token::EoF,
            _ if is_letter(self.ch) => {
                let literal = self.read_identifier();
                let tok = match literal.as_ref() {
                    "fn" => Token::Fn,
                    "let" => Token::Let,
                    "true" => Token::True,
                    "false" => Token::False,
                    "null" => Token::Null,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "return" => Token::Return,
                    i => Token::Ident(i.to_string()),
                };
                return Ok(tok);
            }
            _ if self.ch.is_ascii_digit() => {
                return self.read_number();
            }
            _ => Token::Illegal(self.ch),
        };

        self.read_char();
        Ok(tok)
    }

    pub fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    pub fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        String::from_iter(&self.input[position..self.position])
    }

    pub fn read_number(&mut self) -> Result<Token> {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        match String::from_iter(&self.input[position..self.position]).parse::<isize>() {
            Ok(value) => Ok(Token::Int(value)),
            Err(error) => Err(LexerError::IllegalInteger(error)),
        }
    }
    pub fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }
        String::from_iter(&self.input[position..self.position])
    }
}

pub fn is_letter(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Result};
    use crate::token::Token;
    use crate::token::Token::*;

    #[test]
    fn next_token() -> Result<()> {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;

"foobar";
"foo bar";
[1, 2];
{"foo": "bar"}
"#;
        let expectations: Vec<Token> = Vec::from([
            Let,
            Ident("five".to_string()),
            Assign,
            Int(5),
            Semicolon,
            Let,
            Ident("ten".to_string()),
            Assign,
            Int(10),
            Semicolon,
            Let,
            Ident("add".to_string()),
            Assign,
            Fn,
            LParen,
            Ident("x".to_string()),
            Comma,
            Ident("y".to_string()),
            RParen,
            LBrace,
            Ident("x".to_string()),
            Plus,
            Ident("y".to_string()),
            Semicolon,
            RBrace,
            Semicolon,
            Let,
            Ident("result".to_string()),
            Assign,
            Ident("add".to_string()),
            LParen,
            Ident("five".to_string()),
            Comma,
            Ident("ten".to_string()),
            RParen,
            Semicolon,
            Bang,
            Minus,
            Slash,
            Asterisk,
            Int(5),
            Semicolon,
            Int(5),
            LT,
            Int(10),
            GT,
            Int(5),
            Semicolon,
            If,
            LParen,
            Int(5),
            LT,
            Int(10),
            RParen,
            LBrace,
            Return,
            True,
            Semicolon,
            RBrace,
            Else,
            LBrace,
            Return,
            False,
            Semicolon,
            RBrace,
            Int(10),
            EQ,
            Int(10),
            Semicolon,
            Int(10),
            NotEq,
            Int(9),
            Semicolon,
            String("foobar".to_string()),
            Semicolon,
            String("foo bar".to_string()),
            Semicolon,
            LBracket,
            Int(1),
            Comma,
            Int(2),
            RBracket,
            Semicolon,
            LBrace,
            String("foo".to_string()),
            Colon,
            String("bar".to_string()),
            RBrace,
            EoF,
        ]);
        let mut lexer = Lexer::new(input.chars().collect());
        let mut tok;

        for expectation in expectations {
            tok = lexer.next_token()?;
            assert_eq!(tok, expectation);
        }

        Ok(())
    }

    #[test]
    fn counter() -> Result<()> {
        let input =
            "let counter = fn(x) { if (x > 100) { return true; } else { counter(x + 1); } };";
        let expectations: Vec<Token> = Vec::from([
            Let,
            Ident("counter".to_string()),
            Assign,
            Fn,
            LParen,
            Ident("x".to_string()),
            RParen,
            LBrace,
            If,
            LParen,
            Ident("x".to_string()),
            GT,
            Int(100),
            RParen,
            LBrace,
            Return,
            True,
            Semicolon,
            RBrace,
            Else,
            LBrace,
            Ident("counter".to_string()),
            LParen,
            Ident("x".to_string()),
            Plus,
            Int(1),
            RParen,
            Semicolon,
            RBrace,
            RBrace,
            Semicolon,
            EoF,
        ]);
        let mut lexer = Lexer::new(input.chars().collect());
        let mut tok;

        for expectation in expectations {
            tok = lexer.next_token()?;
            assert_eq!(tok, expectation);
        }

        Ok(())
    }
}
