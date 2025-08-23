//! Token types for the Monkey programming language.
//!
//! This module defines all the different types of tokens that can be produced
//! by the lexer. Tokens represent the basic building blocks of Monkey programs.

mod fmt;

/// Represents a token in the Monkey programming language.
///
/// Tokens are the output of the lexer and input to the parser. Each token
/// represents a meaningful unit in the source code, such as keywords,
/// operators, literals, or identifiers.
///
/// # Examples
///
/// ```
/// use monkey_interpreter_rs::token::Token;
///
/// let identifier = Token::Ident("variable".to_string());
/// let number = Token::Int(42);
/// let operator = Token::Plus;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Token {
    /// End of file marker
    EoF,

    // Identifiers + literals
    /// Variable or function identifier
    Ident(String),
    /// Integer literal  
    Int(isize),
    /// String literal
    String(String),

    // Operators
    /// Assignment operator `=`
    Assign,
    /// Addition operator `+`
    Plus,
    /// Subtraction operator `-`
    Minus,
    /// Logical NOT operator `!`
    Bang,
    /// Multiplication operator `*`
    Asterisk,
    /// Division operator `/`
    Slash,
    /// Less than operator `<`
    Lt,
    /// Greater than operator `>`
    Gt,
    /// Less than or equal operator `<=`
    Lte,
    /// Greater than or equal operator `>=`
    Gte,
    /// Equality operator `==`
    Eq,
    /// Inequality operator `!=`
    NotEq,
    /// Logical AND operator `&&`
    And,
    /// Logical OR operator `||`
    Or,

    // Delimiters
    /// Comma separator `,`
    Comma,
    /// Colon separator `:`
    Colon,
    /// Semicolon separator `;`
    Semicolon,
    /// Left parenthesis `(`
    LParen,
    /// Right parenthesis `)`
    RParen,
    /// Left brace `{`
    LBrace,
    /// Right brace `}`
    RBrace,
    /// Left bracket `[`
    LBracket,
    /// Right bracket `]`
    RBracket,

    // Keywords
    /// Function keyword `fn`
    Fn,
    /// Let keyword `let`
    Let,
    /// Boolean true literal
    True,
    /// Boolean false literal
    False,
    /// Null literal
    Null,
    /// If keyword `if`
    If,
    /// Else keyword `else`
    Else,
    /// Return keyword `return`
    Return,
    /// While keyword `while`
    While,
    /// For keyword `for`
    For,
    /// In keyword `in`
    In,
}
