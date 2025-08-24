//! Lexical analysis for the Monkey programming language.
//!
//! This module provides the [`Lexer`] struct which converts raw source code text
//! into a stream of [`Token`]s. The lexer handles:
//!
//! - Keywords, identifiers, and literals
//! - Operators and delimiters  
//! - String escape sequences
//! - Single-line (`//`) and multi-line (`/* */`) comments
//! - Comprehensive error reporting with position tracking
//!
//! # Examples
//!
//! ```
//! use monkey_interpreter_rs::lexer::Lexer;
//! use monkey_interpreter_rs::token::Token;
//!
//! let input = "let x = 42;";
//! let mut lexer = Lexer::new(input.chars().collect());
//!
//! assert_eq!(lexer.next_token().unwrap(), Token::Let);
//! assert_eq!(lexer.next_token().unwrap(), Token::Ident("x".into()));
//! assert_eq!(lexer.next_token().unwrap(), Token::Assign);
//! assert_eq!(lexer.next_token().unwrap(), Token::Int(42));
//! assert_eq!(lexer.next_token().unwrap(), Token::Semicolon);
//! ```

mod fmt;

use crate::token::{intern::create_optimized_string, Token};
use std::{num, result};

/// Represents a position in the source code.
///
/// Used for error reporting to provide line and column information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub column: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}

impl Position {
    /// Creates a new position at line 1, column 1.
    pub fn new() -> Self {
        Self::default()
    }
}

/// Errors that can occur during lexical analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    /// Failed to parse an integer literal
    IllegalInteger(num::ParseIntError, Position),
    /// Encountered an unexpected character
    IllegalCharacter(char, Position),
}

/// Result type for lexer operations.
pub type Result<T> = result::Result<T, LexerError>;

/// The lexer converts raw source code into tokens.
///
/// The lexer maintains internal state including current position, line/column tracking,
/// and handles comments transparently by skipping them during tokenization.
///
/// # Examples
///
/// ```
/// use monkey_interpreter_rs::lexer::Lexer;
/// use monkey_interpreter_rs::token::Token;
///
/// let mut lexer = Lexer::new("5 + 10".chars().collect());
///
/// assert_eq!(lexer.next_token().unwrap(), Token::Int(5));
/// assert_eq!(lexer.next_token().unwrap(), Token::Plus);  
/// assert_eq!(lexer.next_token().unwrap(), Token::Int(10));
/// assert_eq!(lexer.next_token().unwrap(), Token::EoF);
/// ```
pub struct Lexer {
    input: Vec<char>,
    position: usize,         // current position in input (points to current char)
    read_position: usize,    // current reading position in input (after current char)
    ch: char,                // current char under examination
    line: usize,             // current line number (1-indexed)
    column: usize,           // current column number (1-indexed)
    line_starts: Vec<usize>, // indices where each line starts
}

impl Lexer {
    /// Creates a new lexer from a vector of characters.
    ///
    /// # Arguments
    ///
    /// * `input` - The source code as a vector of characters
    ///
    /// # Examples
    ///
    /// ```
    /// use monkey_interpreter_rs::lexer::Lexer;
    ///
    /// let lexer = Lexer::new("let x = 5;".chars().collect());
    /// ```
    pub fn new(input: Vec<char>) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
            line: 1,
            column: 0,            // Will be incremented to 1 by first read_char call
            line_starts: vec![0], // First line starts at index 0
        };
        lexer.read_char();
        lexer
    }

    /// Returns the current position in the source code.
    pub fn current_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    /// Retrieves the content of a specific line.
    ///
    /// Used for enhanced error reporting to show the problematic line.
    ///
    /// # Arguments
    ///
    /// * `line_number` - The line number to retrieve (1-indexed)
    ///
    /// # Returns
    ///
    /// The content of the specified line, or an empty string if the line doesn't exist.
    #[allow(dead_code)]
    pub fn get_line_content(&self, line_number: usize) -> String {
        if line_number == 0 || line_number > self.line_starts.len() {
            return String::new();
        }

        let line_index = line_number - 1; // Convert to 0-based index
        let start = self.line_starts[line_index];

        let end = if line_index + 1 < self.line_starts.len() {
            // Not the last line - end before the next line's start
            self.line_starts[line_index + 1].saturating_sub(1) // Subtract 1 to exclude the newline
        } else {
            // Last line - end at input length
            self.input.len()
        };

        String::from_iter(&self.input[start..end])
    }

    /// Advances the lexer and returns the next token.
    ///
    /// This is the main method of the lexer. It processes the current character(s)
    /// and returns the corresponding token. Comments are automatically skipped.
    ///
    /// # Returns
    ///
    /// The next token in the input stream, or an error if invalid syntax is encountered.
    ///
    /// # Examples
    ///
    /// ```
    /// use monkey_interpreter_rs::lexer::Lexer;
    /// use monkey_interpreter_rs::token::Token;
    ///
    /// let mut lexer = Lexer::new("42".chars().collect());
    /// assert_eq!(lexer.next_token().unwrap(), Token::Int(42));
    /// assert_eq!(lexer.next_token().unwrap(), Token::EoF);
    /// ```
    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let tok = match self.ch {
            '=' if self.peek_char() == '=' => {
                self.read_char();
                Token::Eq
            }
            '=' => Token::Assign,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' if self.peek_char() == '=' => {
                self.read_char();
                Token::NotEq
            }
            '!' => Token::Bang,
            '/' if self.peek_char() == '/' => {
                self.skip_single_line_comment();
                return self.next_token(); // Recursively get the next token
            }
            '/' if self.peek_char() == '*' => {
                self.skip_multi_line_comment()?;
                return self.next_token(); // Recursively get the next token
            }
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            '<' if self.peek_char() == '=' => {
                self.read_char();
                Token::Lte
            }
            '<' => Token::Lt,
            '>' if self.peek_char() == '=' => {
                self.read_char();
                Token::Gte
            }
            '>' => Token::Gt,
            '&' if self.peek_char() == '&' => {
                self.read_char();
                Token::And
            }
            '|' if self.peek_char() == '|' => {
                self.read_char();
                Token::Or
            }
            ';' => Token::Semicolon,
            ',' => Token::Comma,
            ':' => Token::Colon,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '"' => Token::String(self.read_string().into()),
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
                    "while" => Token::While,
                    "for" => Token::For,
                    "in" => Token::In,
                    i => Token::Ident(create_optimized_string(i.into())),
                };
                return Ok(tok);
            }
            _ if self.ch.is_ascii_digit() => {
                return self.read_number();
            }
            _ => {
                return Err(LexerError::IllegalCharacter(
                    self.ch,
                    self.current_position(),
                ));
            }
        };

        self.read_char();
        Ok(tok)
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }

        // Update position tracking
        if self.position < self.input.len() && self.input[self.position] == '\n' {
            self.line += 1;
            self.column = 1;
            // Record the start of the new line
            if self.read_position < self.input.len() {
                self.line_starts.push(self.read_position);
            }
        } else {
            self.column += 1;
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        String::from_iter(&self.input[position..self.position])
    }

    fn read_number(&mut self) -> Result<Token> {
        let position = self.position;
        let start_pos = self.current_position();
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        match String::from_iter(&self.input[position..self.position]).parse::<isize>() {
            Ok(value) => Ok(Token::Int(value)),
            Err(error) => Err(LexerError::IllegalInteger(error, start_pos)),
        }
    }
    fn read_string(&mut self) -> String {
        let mut result = String::new();

        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }

            if self.ch == '\\' {
                // Handle escape sequences
                self.read_char();
                match self.ch {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    'x' => {
                        // Hexadecimal escape sequence: \xHH
                        if let Some(ch) = self.read_hex_escape() {
                            result.push(ch);
                        } else {
                            // Invalid hex escape, treat literally
                            result.push('\\');
                            result.push('x');
                        }
                    }
                    'u' => {
                        // Unicode escape sequence: \uHHHH
                        if let Some(ch) = self.read_unicode_escape() {
                            result.push(ch);
                        } else {
                            // Invalid unicode escape, treat literally
                            result.push('\\');
                            result.push('u');
                        }
                    }
                    '\0' => {
                        // Unterminated escape sequence at EOF
                        result.push('\\');
                        break;
                    }
                    _ => {
                        // Unknown escape sequence, treat literally
                        result.push('\\');
                        result.push(self.ch);
                    }
                }
            } else {
                result.push(self.ch);
            }
        }

        result
    }

    fn read_hex_escape(&mut self) -> Option<char> {
        // Look ahead to check if we have two valid hex digits
        let saved_position = self.position;
        let saved_read_position = self.read_position;
        let saved_ch = self.ch;
        let saved_line = self.line;
        let saved_column = self.column;
        
        let mut hex_digits = String::new();
        
        for _ in 0..2 {
            self.read_char();
            if self.ch.is_ascii_hexdigit() {
                hex_digits.push(self.ch);
            } else {
                // Invalid hex digit, restore state
                self.position = saved_position;
                self.read_position = saved_read_position;
                self.ch = saved_ch;
                self.line = saved_line;
                self.column = saved_column;
                return None;
            }
        }
        
        // Parse the hex value
        if let Ok(value) = u8::from_str_radix(&hex_digits, 16) {
            Some(value as char)
        } else {
            // Restore state on parse error
            self.position = saved_position;
            self.read_position = saved_read_position;
            self.ch = saved_ch;
            self.line = saved_line;
            self.column = saved_column;
            None
        }
    }

    fn read_unicode_escape(&mut self) -> Option<char> {
        // Look ahead to check if we have four valid hex digits
        let saved_position = self.position;
        let saved_read_position = self.read_position;
        let saved_ch = self.ch;
        let saved_line = self.line;
        let saved_column = self.column;
        
        let mut hex_digits = String::new();
        
        for _ in 0..4 {
            self.read_char();
            if self.ch.is_ascii_hexdigit() {
                hex_digits.push(self.ch);
            } else {
                // Invalid hex digit, restore state
                self.position = saved_position;
                self.read_position = saved_read_position;
                self.ch = saved_ch;
                self.line = saved_line;
                self.column = saved_column;
                return None;
            }
        }
        
        // Parse the hex value and convert to char
        if let Ok(value) = u32::from_str_radix(&hex_digits, 16) {
            if let Some(ch) = char::from_u32(value) {
                Some(ch)
            } else {
                // Invalid unicode code point, restore state
                self.position = saved_position;
                self.read_position = saved_read_position;
                self.ch = saved_ch;
                self.line = saved_line;
                self.column = saved_column;
                None
            }
        } else {
            // Restore state on parse error
            self.position = saved_position;
            self.read_position = saved_read_position;
            self.ch = saved_ch;
            self.line = saved_line;
            self.column = saved_column;
            None
        }
    }

    fn skip_single_line_comment(&mut self) {
        // Skip the '//' characters
        self.read_char(); // Skip first '/'
        self.read_char(); // Skip second '/'

        // Skip until end of line or EOF
        while self.ch != '\n' && self.ch != '\0' {
            self.read_char();
        }
    }

    fn skip_multi_line_comment(&mut self) -> Result<()> {
        let start_pos = self.current_position();

        // Skip the '/*' characters
        self.read_char(); // Skip '/'
        self.read_char(); // Skip '*'

        // Look for closing */
        while self.ch != '\0' {
            if self.ch == '*' && self.peek_char() == '/' {
                self.read_char(); // Skip '*'
                self.read_char(); // Skip '/'
                return Ok(());
            }
            self.read_char();
        }

        // If we reach here, the comment is unterminated
        Err(LexerError::IllegalCharacter('*', start_pos))
    }
}

fn is_letter(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, LexerError, Result};
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
!-/ *5;
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
{"foo": "bar"};
10 >= 9;
10 <= 9;
true && false;
true || false;
"#;
        let expectations: Vec<Token> = Vec::from([
            Let,
            Ident("five".into()),
            Assign,
            Int(5),
            Semicolon,
            Let,
            Ident("ten".into()),
            Assign,
            Int(10),
            Semicolon,
            Let,
            Ident("add".into()),
            Assign,
            Fn,
            LParen,
            Ident("x".into()),
            Comma,
            Ident("y".into()),
            RParen,
            LBrace,
            Ident("x".into()),
            Plus,
            Ident("y".into()),
            Semicolon,
            RBrace,
            Semicolon,
            Let,
            Ident("result".into()),
            Assign,
            Ident("add".into()),
            LParen,
            Ident("five".into()),
            Comma,
            Ident("ten".into()),
            RParen,
            Semicolon,
            Bang,
            Minus,
            Slash,
            Asterisk,
            Int(5),
            Semicolon,
            Int(5),
            Lt,
            Int(10),
            Gt,
            Int(5),
            Semicolon,
            If,
            LParen,
            Int(5),
            Lt,
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
            Eq,
            Int(10),
            Semicolon,
            Int(10),
            NotEq,
            Int(9),
            Semicolon,
            String("foobar".into()),
            Semicolon,
            String("foo bar".into()),
            Semicolon,
            LBracket,
            Int(1),
            Comma,
            Int(2),
            RBracket,
            Semicolon,
            LBrace,
            String("foo".into()),
            Colon,
            String("bar".into()),
            RBrace,
            Semicolon,
            Int(10),
            Gte,
            Int(9),
            Semicolon,
            Int(10),
            Lte,
            Int(9),
            Semicolon,
            True,
            And,
            False,
            Semicolon,
            True,
            Or,
            False,
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

    #[test]
    fn counter() -> Result<()> {
        let input =
            "let counter = fn(x) { if (x > 100) { return true; } else { counter(x + 1); } };";
        let expectations: Vec<Token> = Vec::from([
            Let,
            Ident("counter".into()),
            Assign,
            Fn,
            LParen,
            Ident("x".into()),
            RParen,
            LBrace,
            If,
            LParen,
            Ident("x".into()),
            Gt,
            Int(100),
            RParen,
            LBrace,
            Return,
            True,
            Semicolon,
            RBrace,
            Else,
            LBrace,
            Ident("counter".into()),
            LParen,
            Ident("x".into()),
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

    #[test]
    fn string_escape_sequences() -> Result<()> {
        let input = r#""hello\nworld\t\"test\"\\";"#;
        let expectations: Vec<Token> =
            Vec::from([String("hello\nworld\t\"test\"\\".into()), Semicolon, EoF]);
        let mut lexer = Lexer::new(input.chars().collect());
        let mut tok;

        for expectation in expectations {
            tok = lexer.next_token()?;
            assert_eq!(tok, expectation);
        }

        Ok(())
    }

    #[test]
    fn string_unknown_escape_sequences() -> Result<()> {
        let input = r#""hello\z\k";"#;
        let expectations: Vec<Token> = Vec::from([
            Token::String("hello\\z\\k".into()),
            Token::Semicolon,
            Token::EoF,
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
    fn string_hex_escape_sequences() -> Result<()> {
        let input = r#""Hello \x41\x42\x43 World";"#;
        let expectations: Vec<Token> = Vec::from([
            Token::String("Hello ABC World".into()),
            Token::Semicolon,
            Token::EoF,
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
    fn string_unicode_escape_sequences() -> Result<()> {
        let input = r#""Hello \u0041\u0042\u0043 \u2764\uFE0F";"#;
        let expectations: Vec<Token> = Vec::from([
            Token::String("Hello ABC ❤️".into()),
            Token::Semicolon,
            Token::EoF,
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
    fn string_mixed_escape_sequences() -> Result<()> {
        let input = r#""Line1\nTab\tHex:\x48Uni:\u0065\x6C\u006Co\r\n";"#;
        let expectations: Vec<Token> = Vec::from([
            Token::String("Line1\nTab\tHex:HUni:ello\r\n".into()),
            Token::Semicolon,
            Token::EoF,
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
    fn string_invalid_hex_escape_sequences() -> Result<()> {
        let input = r#""Invalid \xGG \xZ1 \x";"#;
        let expectations: Vec<Token> = Vec::from([
            Token::String("Invalid \\xGG \\xZ1 \\x".into()),
            Token::Semicolon,
            Token::EoF,
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
    fn string_invalid_unicode_escape_sequences() -> Result<()> {
        let input = r#""Invalid \uGGGG \uZ123 \u12";"#;
        let expectations: Vec<Token> = Vec::from([
            Token::String("Invalid \\uGGGG \\uZ123 \\u12".into()),
            Token::Semicolon,
            Token::EoF,
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
    fn string_hex_escape_edge_cases() -> Result<()> {
        // Test boundary values for hex escapes
        let input = r#""\x00\x20\x7F\xFF";"#;
        let expected_string = format!("{}{}{}{}", '\0', ' ', '\x7F', '\u{FF}');
        let expectations: Vec<Token> = Vec::from([
            Token::String(expected_string.into()),
            Token::Semicolon,
            Token::EoF,
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
    fn string_unicode_escape_edge_cases() -> Result<()> {
        // Test various Unicode ranges
        let input = r#""\u0000\u0020\u007F\u00A0\u2603\uFFFD";"#;
        let expectations: Vec<Token> = Vec::from([
            Token::String("\u{0000}\u{0020}\u{007F}\u{00A0}\u{2603}\u{FFFD}".into()),
            Token::Semicolon,
            Token::EoF,
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
    fn position_tracking() -> Result<()> {
        let input = "let x = 5\nlet y = 10";
        let mut lexer = Lexer::new(input.chars().collect());

        // First token: "let" at line 1, column 1
        let token = lexer.next_token()?;
        assert_eq!(token, Token::Let);

        // Second token: "x" at line 1, column 5
        let token = lexer.next_token()?;
        assert_eq!(token, Token::Ident("x".into()));

        // Skip to second line - "let"
        lexer.next_token()?; // "="
        lexer.next_token()?; // "5"
        let token = lexer.next_token()?; // "let" on line 2
        assert_eq!(token, Token::Let);

        // Position should be line 2
        let pos = lexer.current_position();
        assert_eq!(pos.line, 2);
        assert!(pos.column > 1);

        Ok(())
    }

    #[test]
    fn illegal_character_position() {
        let input = "let x = @"; // @ is illegal
        let mut lexer = Lexer::new(input.chars().collect());

        // Skip valid tokens
        lexer.next_token().unwrap(); // let
        lexer.next_token().unwrap(); // x
        lexer.next_token().unwrap(); // =

        // This should error with position info
        let result = lexer.next_token();
        match result {
            Err(LexerError::IllegalCharacter(ch, pos)) => {
                assert_eq!(ch, '@');
                assert_eq!(pos.line, 1);
                assert_eq!(pos.column, 9); // @ is at column 9
            }
            _ => panic!("Expected IllegalCharacter error"),
        }
    }

    #[test]
    fn enhanced_error_messages() {
        let input = "let x = @";
        let mut lexer = Lexer::new(input.chars().collect());

        // Skip valid tokens
        lexer.next_token().unwrap(); // let
        lexer.next_token().unwrap(); // x
        lexer.next_token().unwrap(); // =

        // This should produce an enhanced error message
        let result = lexer.next_token();
        match result {
            Err(error) => {
                let error_msg = format!("{}", error);
                assert!(error_msg.contains("line 1"));
                assert!(error_msg.contains("column 9"));
                assert!(error_msg.contains("'@'"));
                assert!(error_msg.contains("Unexpected character"));
            }
            _ => panic!("Expected error"),
        }
    }

    #[test]
    fn line_content_retrieval() -> Result<()> {
        let input = "let x = 5\nlet y = 10\nlet z = 15";
        let mut lexer = Lexer::new(input.chars().collect());

        // We need to advance through the lexer to build up line_starts
        while lexer.next_token()? != Token::EoF {
            // Continue tokenizing to track all line starts
        }

        assert_eq!(lexer.get_line_content(1), "let x = 5");
        assert_eq!(lexer.get_line_content(2), "let y = 10");
        assert_eq!(lexer.get_line_content(3), "let z = 15");
        assert_eq!(lexer.get_line_content(4), ""); // Non-existent line

        Ok(())
    }

    #[test]
    fn single_line_comments() -> Result<()> {
        let input = r#"let five = 5; // This is a comment
let ten = 10; // Another comment
// Full line comment
let result = five + ten;"#;

        let expectations: Vec<Token> = Vec::from([
            Let,
            Ident("five".into()),
            Assign,
            Int(5),
            Semicolon,
            Let,
            Ident("ten".into()),
            Assign,
            Int(10),
            Semicolon,
            Let,
            Ident("result".into()),
            Assign,
            Ident("five".into()),
            Plus,
            Ident("ten".into()),
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

    #[test]
    fn multi_line_comments() -> Result<()> {
        let input = r#"let five = 5;
/* This is a 
   multi-line comment */
let ten = 10;
/* Another
   comment */ let result = five + ten;"#;

        let expectations: Vec<Token> = Vec::from([
            Let,
            Ident("five".into()),
            Assign,
            Int(5),
            Semicolon,
            Let,
            Ident("ten".into()),
            Assign,
            Int(10),
            Semicolon,
            Let,
            Ident("result".into()),
            Assign,
            Ident("five".into()),
            Plus,
            Ident("ten".into()),
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

    #[test]
    fn unterminated_multi_line_comment() {
        let input = "let x = 5; /* This comment is not closed";
        let mut lexer = Lexer::new(input.chars().collect());

        // Skip valid tokens
        lexer.next_token().unwrap(); // let
        lexer.next_token().unwrap(); // x
        lexer.next_token().unwrap(); // =
        lexer.next_token().unwrap(); // 5
        lexer.next_token().unwrap(); // ;

        // This should error due to unterminated comment
        let result = lexer.next_token();
        assert!(result.is_err());
        match result {
            Err(LexerError::IllegalCharacter('*', _)) => {
                // Expected error for unterminated comment
            }
            _ => panic!("Expected IllegalCharacter error for unterminated comment"),
        }
    }

    #[test]
    fn comments_with_special_characters() -> Result<()> {
        let input = r#"// Comment with symbols: !@#$%^&*()
/* Comment with 
   more symbols: {}[]<>=+- */
let x = 1;"#;

        let expectations: Vec<Token> =
            Vec::from([Let, Ident("x".into()), Assign, Int(1), Semicolon, EoF]);

        let mut lexer = Lexer::new(input.chars().collect());
        let mut tok;

        for expectation in expectations {
            tok = lexer.next_token()?;
            assert_eq!(tok, expectation);
        }

        Ok(())
    }
}
