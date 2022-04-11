use std::io;
use std::io::prelude::*;
use std::iter::Peekable;
use std::ops::DerefMut;
use std::str::Chars;

#[derive(Debug, Clone)]
pub enum Token {
    SemiColon,
    Let,
    Rec,
    If,
    Else,
    Then,
    True,
    False,
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
    pos: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer`, given its source `input`.
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            chars: Box::new(input.chars().peekable()),
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
            // Note: the following lines are in their own scope to
            // limit how long 'chars' is borrowed, and in order to allow
            // it to be borrowed again in the loop by 'chars.next()'.
            {
                let ch = chars.peek();

                if ch.is_none() {
                    self.pos = pos;

                    return Ok(Token::EOF);
                }

                if !ch.unwrap().is_whitespace() {
                    break;
                }
            }

            chars.next();
            pos += 1;
        }

        let start = pos;
        let next = chars.next();

        if next.is_none() {
            return Ok(Token::EOF);
        }

        pos += 1;

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

            '=' => Ok(Token::Op("=".to_string(), 1)),
            '+' => Ok(Token::Op("+".to_string(), 2)),
            '-' => Ok(Token::Op("-".to_string(), 2)),
            '*' => Ok(Token::Op("*".to_string(), 3)),
            '/' => Ok(Token::Op("/".to_string(), 3)),
            '<' => {
                if chars.peek().unwrap() == &'=' {
                    chars.next();
                    pos += 1;

                    Ok(Token::Op("<=".to_string(), 4))
                } else if chars.peek().unwrap() == &'>' {
                    chars.next();
                    pos += 1;
                    Ok(Token::Op("<>".to_string(), 4))
                } else {
                    Ok(Token::Op("<".to_string(), 4))
                }
            }
            '>' => {
                if chars.peek().unwrap() == &'=' {
                    chars.next();
                    pos += 1;

                    Ok(Token::Op(">=".to_string(), 4))
                } else {
                    Ok(Token::Op(">".to_string(), 4))
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
                    "true" => Ok(Token::True),
                    "false" => Ok(Token::False),
                    "let" => Ok(Token::Let),
                    "rec" => Ok(Token::Rec),

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

            _ => Err(LexError::with_index("Unexpected character", pos)),
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

macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

fn run_toplevel() {
    loop {
        println!();
        print_flush!("# ");

        // Read input from stdin
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Could not read from standard input.");

        if input.starts_with("#quit") {
            break;
        } else if input.chars().all(char::is_whitespace) {
            continue;
        }

        println!(
            "Tokens: \n{:?}\n",
            Lexer::new(input.as_str()).collect::<Vec<Token>>()
        );
    }
}

fn main() {
    run_toplevel();
}
