mod fmt;

use crate::token::Token;
use crate::token::Token::*;
use std::{num, result};

#[derive(Debug, PartialEq)]
pub enum Error {
    IllegalInteger(num::ParseIntError),
}
pub type Result<T> = result::Result<T, Error>;

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
            ch: 0 as char,
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
                EQ
            }
            '=' => Assign,
            '+' => Plus,
            '-' => Minus,
            '!' if self.peek_char() == '=' => {
                self.read_char();
                NotEq
            }
            '!' => Bang,
            '/' => Slash,
            '*' => Asterisk,
            '<' => LT,
            '>' => GT,
            ';' => Semicolon,
            ',' => Comma,
            '{' => LBrace,
            '}' => RBrace,
            '(' => LParen,
            ')' => RParen,
            '\u{0000}' => EoF,
            _ if is_letter(self.ch) => {
                let literal = self.read_identifier();
                let tok = match literal.as_ref() {
                    "fn" => Fn,
                    "let" => Let,
                    "true" => True,
                    "false" => False,
                    "if" => If,
                    "else" => Else,
                    "return" => Return,
                    i => Ident(i.to_string()),
                };
                return Ok(tok);
            }
            _ if self.ch.is_ascii_digit() => {
                return self.read_number();
            }
            _ => Illegal(self.ch),
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
            self.ch = 0 as char;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            0 as char
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
            Ok(value) => Ok(Int(value)),
            Err(error) => Err(Error::IllegalInteger(error)),
        }
    }
}

pub fn is_letter(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;
    use crate::token::Token::*;

    #[test]
    fn next_token() {
        let input = "let five = 5;
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
";
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
            EoF,
        ]);
        let mut lexer = Lexer::new(input.chars().collect());
        let mut tok;

        for expectation in expectations {
            tok = lexer.next_token().expect("test");    // TODO test to return Result?
            assert_eq!(tok, expectation);
        }
    }
}
