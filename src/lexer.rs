#![allow(dead_code)]
use std::iter::Peekable;
use std::ops::DerefMut;
use std::str::Chars;

#[derive(Debug, Clone)]
pub enum Token {
    SemiColon,
    Let,
    Rec,
    And,
    In,
    If,
    Else,
    Then,
    Bool(bool),
    Unit,
    EOF,
    LowercaseIdent(String),
    CapitalizedIdent(String),
    LParen,
    RParen,
    Integer(i32),
    Op(String, i32), // operator, precedence
}

/// Defines an error encountered by the `Lexer`.
pub struct LexError {
    pub error: &'static str,
    pub index: usize,
}

impl LexError {
    pub fn new(msg: &'static str) -> LexError {
        LexError {
            error: msg,
            index: 0,
        }
    }

    pub fn with_index(msg: &'static str, index: usize) -> LexError {
        LexError { error: msg, index }
    }
}

/// Defines the result of a lexing operation; namely a
/// `Token` on success, or a `LexError` on failure.
pub type LexResult = Result<Token, LexError>;

/// Defines a lexer which transforms an input `String` into
/// a `Token` stream.
pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    nesting_level: usize,
    pos: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer`, given its source `input`.
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            chars: Box::new(input.chars().peekable()),
            nesting_level: 0,
            pos: 0,
        }
    }

    /// Lexes and returns the next `Token` from the source code.
    pub fn lex(&mut self) -> LexResult {
        let chars = self.chars.deref_mut();
        let src = self.input;

        let mut pos = self.pos;

        // Skip whitespaces
        loop {
            match chars.peek() {
                Some(ch) => {
                    if !ch.is_whitespace() {
                        break;
                    }
                }
                None => {
                    self.pos = pos;
                    return Ok(Token::EOF);
                }
            }
            chars.next();
            pos += 1;
        }

        let start = pos;
        let mut next = chars.next();

        if next.is_none() {
            return Ok(Token::EOF);
        }

        pos += 1;

        // ingore comment: (* xxx  *)
        match next.unwrap() {
            '(' => {
                if chars.peek() == Some(&'*') {
                    chars.next();
                    next = chars.next();
                    pos += 2;
                    self.nesting_level += 1;
                    while self.nesting_level > 0 {
                        match next {
                            Some(ch) => {
                                if ch == '*' {
                                    if chars.peek() == Some(&')') {
                                        chars.next();
                                        next = chars.next();
                                        pos += 2;
                                        self.nesting_level -= 1;
                                    } else {
                                        // eat *
                                        next = chars.next();
                                        pos += 1;
                                    }
                                } else if ch == '(' {
                                    if chars.peek() == Some(&'*') {
                                        chars.next();
                                        next = chars.next();
                                        pos += 2;
                                        self.nesting_level += 1;
                                    } else {
                                        // eat (
                                        next = chars.next();
                                        pos += 1;
                                    }
                                } else {
                                    // eat any char
                                    next = chars.next();
                                    pos += 1;
                                }
                            }
                            None => {
                                return Err(LexError::with_index("Expect *)", pos));
                            }
                        }
                    }
                    self.pos = pos;
                    return self.lex();
                }
            }
            _ => {}
        }

        // Actually get the next token.
        let result = match next.unwrap() {
            // unit or (
            '(' => {
                if chars.peek().unwrap() == &')' {
                    chars.next();
                    pos += 1;

                    Ok(Token::Unit)
                } else {
                    Ok(Token::LParen)
                }
            }
            ')' => Ok(Token::RParen),

            // ; or ;;
            ';' => {
                if chars.peek().unwrap() == &';' {
                    chars.next();
                    pos += 1;

                    Ok(Token::EOF)
                } else {
                    Ok(Token::SemiColon)
                }
            }

            '=' => Ok(Token::Op("=".to_string(), 10)),
            '+' => Ok(Token::Op("+".to_string(), 20)),
            '-' => Ok(Token::Op("-".to_string(), 20)),
            '*' => Ok(Token::Op("*".to_string(), 40)),
            '/' => Ok(Token::Op("/".to_string(), 40)),
            '|' => {
                if chars.peek().unwrap() == &'|' {
                    chars.next();
                    pos += 1;

                    Ok(Token::Op("||".to_string(), 10))
                } else {
                    Ok(Token::Op("|".to_string(), 10))
                }
            }
            '&' => {
                if chars.peek().unwrap() == &'&' {
                    chars.next();
                    pos += 1;

                    Ok(Token::Op("&&".to_string(), 10))
                } else {
                    Ok(Token::Op("&".to_string(), 10))
                }
            }
            '<' => {
                if chars.peek().unwrap() == &'=' {
                    chars.next();
                    pos += 1;

                    Ok(Token::Op("<=".to_string(), 10))
                } else if chars.peek().unwrap() == &'>' {
                    chars.next();
                    pos += 1;
                    Ok(Token::Op("<>".to_string(), 10))
                } else {
                    Ok(Token::Op("<".to_string(), 10))
                }
            }
            '>' => {
                if chars.peek().unwrap() == &'=' {
                    chars.next();
                    pos += 1;

                    Ok(Token::Op(">=".to_string(), 10))
                } else {
                    Ok(Token::Op(">".to_string(), 10))
                }
            }

            'a'..='z' | '_' => {
                // lowercase ident
                while chars.peek().unwrap().is_alphanumeric()
                    || chars.peek().unwrap() == &'_'
                    || chars.peek().unwrap() == &'\''
                {
                    chars.next();
                    pos += 1;
                }

                match &src[start..pos] {
                    // key words
                    "if" => Ok(Token::If),
                    "then" => Ok(Token::Then),
                    "else" => Ok(Token::Else),
                    "true" => Ok(Token::Bool(true)),
                    "false" => Ok(Token::Bool(false)),
                    "let" => Ok(Token::Let),
                    "rec" => Ok(Token::Rec),
                    "and" => Ok(Token::And),
                    "in" => Ok(Token::In),
                    "mod" => Ok(Token::Op("mod".to_string(), 30)),

                    ident => Ok(Token::LowercaseIdent(ident.to_string())),
                }
            }

            'A'..='Z' => {
                // capitalized ident
                while chars.peek().unwrap().is_alphanumeric()
                    || chars.peek().unwrap() == &'_'
                    || chars.peek().unwrap() == &'\''
                {
                    chars.next();
                    pos += 1;
                }

                Ok(Token::CapitalizedIdent(src[start..pos].to_string()))
            }

            '0'..='9' => {
                // integer
                while chars.peek().unwrap().is_digit(10) || chars.peek().unwrap() == &'_' {
                    chars.next();
                    pos += 1;
                }

                Ok(Token::Integer(
                    src[start..pos].replace("_", "").parse().unwrap(),
                ))
            }

            _ => Err(LexError::with_index("Unexpected character in lex", pos)),
        };

        // Update stored position, and return
        self.pos = pos;

        result
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    /// Lexes the next `Token` and returns it.
    /// On EOF or failure, `None` will be returned.
    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Ok(Token::EOF) | Err(_) => None,
            Ok(token) => Some(token),
        }
    }
}
