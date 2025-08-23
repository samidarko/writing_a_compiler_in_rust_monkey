use std::fmt;

use super::Token;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::EoF => "EoF",
                Token::Ident(value) => value,
                Token::Int(value) => return value.fmt(f),
                Token::String(value) => value,
                Token::Assign => "=",
                Token::Plus => "+",
                Token::Minus => "-",
                Token::Bang => "!",
                Token::Asterisk => "*",
                Token::Slash => "/",
                Token::Lt => "<",
                Token::Gt => ">",
                Token::Lte => "<=",
                Token::Gte => ">=",
                Token::Eq => "==",
                Token::NotEq => "!=",
                Token::And => "&&",
                Token::Or => "||",
                Token::Comma => ",",
                Token::Colon => ":",
                Token::Semicolon => ";",
                Token::LParen => "(",
                Token::RParen => ")",
                Token::LBrace => "{",
                Token::RBrace => "}",
                Token::Fn => "fn",
                Token::Let => "let",
                Token::True => "true",
                Token::False => "false",
                Token::Null => "null",
                Token::If => "if",
                Token::Else => "else",
                Token::Return => "return",
                Token::While => "while",
                Token::For => "for",
                Token::In => "in",
                Token::LBracket => "[",
                Token::RBracket => "]",
            }
        )
    }
}
