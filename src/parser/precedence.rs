//! Operator precedence handling for the Monkey parser.
//!
//! This module defines operator precedence levels and provides utilities
//! for converting tokens to their corresponding precedence levels.

use crate::token::Token;

/// Operator precedence levels for expression parsing.
///
/// Used by the Pratt parser to determine the order of operations.
/// Higher precedence operators bind more tightly.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    /// Lowest precedence
    Lowest,
    /// Assignment operators (`=`) - right-associative
    Assignment,
    /// Equality operators (`==`, `!=`)  
    Equals,
    /// Comparison operators (`<`, `>`, `<=`, `>=`)
    LessGreater,
    /// Addition/subtraction (`+`, `-`)
    Sum,
    /// Multiplication/division (`*`, `/`)
    Product,
    /// Prefix operators (`-x`, `!x`)
    Prefix,
    /// Function calls (`func()`)
    Call,
    /// Array/hash indexing (`arr[0]`)
    Index,
}

/// Returns the precedence level for a given token.
///
/// This function maps tokens to their precedence levels for use in
/// Pratt parsing. Tokens that are not operators return `Precedence::Lowest`.
///
/// # Arguments
///
/// * `token` - The token to get precedence for
///
/// # Returns
///
/// The precedence level for the token
///
/// # Examples
///
/// ```
/// use monkey_interpreter_rs::parser::precedence::{get_token_precedence, Precedence};
/// use monkey_interpreter_rs::token::Token;
///
/// assert_eq!(get_token_precedence(&Token::Plus), Precedence::Sum);
/// assert_eq!(get_token_precedence(&Token::Asterisk), Precedence::Product);
/// assert_eq!(get_token_precedence(&Token::LParen), Precedence::Call);
/// ```
pub fn get_token_precedence(token: &Token) -> Precedence {
    match token {
        Token::Assign => Precedence::Assignment,
        Token::Eq | Token::NotEq => Precedence::Equals,
        Token::Lt | Token::Gt | Token::Lte | Token::Gte => Precedence::LessGreater,
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Slash | Token::Asterisk => Precedence::Product,
        Token::LParen => Precedence::Call,
        Token::LBracket => Precedence::Index,
        Token::And | Token::Or => Precedence::Equals, // Same as equality for now
        _ => Precedence::Lowest,
    }
}
