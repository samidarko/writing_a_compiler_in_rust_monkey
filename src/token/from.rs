// use std::str::FromStr;
//
// use super::Token;
//
// impl FromStr for Token {
//     type Err = ();
//
//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         match s {
//             "ILLEGAL" => Ok(Token::Illegal),
//             "EOF" => Ok(Token::EoF),
//             // "IDENT" => Ok(Token::Ident),
//             // "INT" => Ok(Token::Int),
//             "=" => Ok(Token::Assign),
//             "+" => Ok(Token::Plus),
//             "-" => Ok(Token::Minus),
//             "!" => Ok(Token::Bang),
//             "*" => Ok(Token::Asterisk),
//             "/" => Ok(Token::Slash),
//             "<" => Ok(Token::LT),
//             ">" => Ok(Token::GT),
//             "==" => Ok(Token::EQ),
//             "!=" => Ok(Token::NotEq),
//             "," => Ok(Token::Comma),
//             ";" => Ok(Token::Semicolon),
//             "(" => Ok(Token::LParen),
//             ")" => Ok(Token::RParen),
//             "{" => Ok(Token::LBrace),
//             "}" => Ok(Token::RBrace),
//             "fn" => Ok(Token::Fn),
//             "let" => Ok(Token::Let),
//             "true" => Ok(Token::True),
//             "false" => Ok(Token::False),
//             "if" => Ok(Token::If),
//             "else" => Ok(Token::Else),
//             "return" => Ok(Token::Return),
//             _ => Err(()),
//         }
//     }
// }
