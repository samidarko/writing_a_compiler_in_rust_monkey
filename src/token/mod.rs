mod fmt;
mod from;

#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(char),
    EoF,

    // Identifiers + literals
    Ident(String), // add, foobar, x, y, ...
    Int(isize),    // 1343456

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,
    EQ,
    NotEq,

    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Fn,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}
