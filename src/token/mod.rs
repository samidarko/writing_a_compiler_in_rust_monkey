mod fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Token {
    EoF,

    // Identifiers + literals
    Ident(String), // add, foobar, x, y, ...
    Int(isize),    // 1343456
    String(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    NotEq,
    And,
    Or,

    // Delimiters
    Comma,
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Fn,
    Let,
    True,
    False,
    Null,
    If,
    Else,
    Return,
}
