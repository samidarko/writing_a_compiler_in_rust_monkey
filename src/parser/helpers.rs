use super::Precedence;
use crate::token::Token;

pub fn get_token_precedence(token: &Token) -> Precedence {
    match token {
        Token::EQ | Token::NotEq => Precedence::Equals,
        Token::LT | Token::GT => Precedence::LessGreater,
        Token::Minus | Token::Plus => Precedence::Sum,
        Token::Slash | Token::Asterisk => Precedence::Product,
        Token::LParen => Precedence::Call,
        Token::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}
