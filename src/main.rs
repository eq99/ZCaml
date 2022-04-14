use lazy_static::lazy_static;
use std::fmt::{self, Display, Formatter};
use std::io;
use std::io::prelude::*;
use std::iter::Peekable;
use std::ops::DerefMut;
use std::str::Chars;
use std::sync::Mutex;

lazy_static! {
    static ref INDENT: Mutex<String> = Mutex::new(String::from(""));
}

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

            '=' => Ok(Token::Op("=".to_string(), 10)),
            '+' => Ok(Token::Op("+".to_string(), 20)),
            '-' => Ok(Token::Op("-".to_string(), 20)),
            '*' => Ok(Token::Op("*".to_string(), 40)),
            '/' => Ok(Token::Op("/".to_string(), 40)),
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

// ======================================================================================
// PARSER ===============================================================================
// ======================================================================================
/// Defines a primitive expression.

#[derive(Debug)]
pub enum ExprAST {
    Integer {
        value: i32,
    },

    Bool {
        value: bool,
    },

    LowerCaseIdent {
        name: String,
    },

    Unit,

    Binary {
        left: Box<ExprAST>,
        op: String,
        right: Box<ExprAST>,
    },

    Call {
        fn_name: String,
        args: Vec<ExprAST>,
    },
}

impl ExprAST {
    pub fn print_tree(&self, level: usize) {
        let indent = "   ".repeat(level);

        match self {
            ExprAST::Integer { value } => println!("{}{}", indent, value),
            ExprAST::Bool { value } => println!("{}{}", indent, value),
            ExprAST::LowerCaseIdent { name } => println!("{}{}", indent, name),
            ExprAST::Unit => println!("{}Unit", indent),
            ExprAST::Binary { left, op, right } => {
                println!("{}{}", indent, op);
                left.print_tree(level + 1);
                right.print_tree(level + 1);
            }
            ExprAST::Call { fn_name, args } => {
                println!("{}Call: {}", indent, fn_name);
                for arg in args {
                    arg.print_tree(level + 1);
                }
            }
        }
    }
}

/// Represents the `Expr` parser.
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

// I'm ignoring the 'must_use' lint in order to call 'self.advance' without checking
// the result when an EOF is acceptable.
#[allow(unused_must_use)]
impl Parser {
    /// Creates a new parser, given an input `str` and a `HashMap` binding
    /// an operator and its precedence in binary expressions.
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer::new(input.as_str());
        let tokens = lexer.by_ref().collect();

        Parser { tokens, pos: 0 }
    }

    /// Parses the content of the parser.
    pub fn parse(&mut self) -> Result<ExprAST, &'static str> {
        let result = self.parse_expr();
        match result {
            Ok(result) => {
                if !self.at_end() {
                    match self.current()? {
                        Token::EOF => Ok(result),
                        _ => Err("Unexpected token after parsed expression."),
                    }
                } else {
                    Ok(result)
                }
            }

            _ => Err("Failed to parse expression."),
        }
    }

    /// Returns the current `Token`, without performing safety checks beforehand.
    fn curr(&self) -> Token {
        self.tokens[self.pos].clone()
    }

    /// Returns the current `Token`, or an error that
    /// indicates that the end of the file has been unexpectedly reached if it is the case.
    fn current(&self) -> Result<Token, &'static str> {
        if self.pos >= self.tokens.len() {
            Err("Unexpected end of file.")
        } else {
            Ok(self.tokens[self.pos].clone())
        }
    }

    /// Advances the position, and returns an empty `Result` whose error
    /// indicates that the end of the file has been unexpectedly reached.
    /// This allows to use the `self.advance()?;` syntax.
    fn advance(&mut self) -> Result<(), &'static str> {
        let npos = self.pos + 1;

        self.pos = npos;

        if npos < self.tokens.len() {
            Ok(())
        } else {
            Err("Unexpected end of file.")
        }
    }

    /// Returns a value indicating whether or not the `Parser`
    /// has reached the end of the input.
    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Returns the precedence of the current `Token`, or 0 if it is not recognized as a binary operator.
    fn get_tok_precedence(&self) -> i32 {
        if let Ok(Token::Op(_, preced)) = self.current() {
            preced
        } else {
            -1
        }
    }

    /// Parses any expression.
    fn parse_expr(&mut self) -> Result<ExprAST, &'static str> {
        let left = self.parse_primary()?;
        self.parse_op_rhs(0, left)
    }

    /// Parses a primary expression (an identifier, a number or a parenthesized expression).
    fn parse_primary(&mut self) -> Result<ExprAST, &'static str> {
        match self.curr() {
            Token::LowercaseIdent(_) => self.parse_ident_expr(),
            Token::Integer(_) => self.parse_integer_expr(),
            Token::LParen => self.parse_paren_expr(),
            _ => Err("Unknown expression."),
        }
    }

    /// Parses a binary expression, given its left-hand expression.
    fn parse_op_rhs(&mut self, left_prec: i32, mut left: ExprAST) -> Result<ExprAST, &'static str> {
        loop {
            let curr_prec = self.get_tok_precedence();

            if curr_prec < left_prec || self.at_end() {
                return Ok(left);
            }

            let op_name = match self.curr() {
                Token::Op(op, _) => op,
                _ => return Err("Invalid operator."),
            };

            // eat op
            self.advance()?;

            let mut right = self.parse_primary()?;

            let next_prec = self.get_tok_precedence();

            if curr_prec < next_prec {
                right = self.parse_op_rhs(curr_prec + 1, right)?;
            }

            left = ExprAST::Binary {
                op: op_name,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
    }

    /// Parses a integer .
    fn parse_integer_expr(&mut self) -> Result<ExprAST, &'static str> {
        // Simply convert Token::Number to Expr::Number
        match self.curr() {
            Token::Integer(integer) => {
                // eat integer
                self.advance();
                Ok(ExprAST::Integer { value: integer })
            }
            _ => Err("Expected integer."),
        }
    }

    /// Parses an expression enclosed in parenthesis.
    fn parse_paren_expr(&mut self) -> Result<ExprAST, &'static str> {
        match self.current()? {
            Token::LParen => (),
            _ => return Err("Expected '(' character at start of parenthesized expression."),
        }

        // eat '('
        self.advance()?;

        let expr = self.parse_expr()?;

        match self.current()? {
            Token::RParen => (),
            _ => return Err("Expected ')' character at end of parenthesized expression."),
        }

        // eat ')'
        self.advance();

        Ok(expr)
    }

    /// Parses an expression that starts with an identifier (either a variable or a function call).
    fn parse_ident_expr(&mut self) -> Result<ExprAST, &'static str> {
        let lowercase_ident = match self.curr() {
            Token::LowercaseIdent(id) => id,
            _ => return Err("bad identifier."),
        };

        // tail ident
        if self.advance().is_err() {
            return Ok(ExprAST::LowerCaseIdent {
                name: lowercase_ident.clone(),
            });
        }

        // expr ::= expr { argument }+
        match self.curr() {
            Token::LParen => {
                // eat '('
                self.advance()?;

                // parse arguments
                if let Token::RParen = self.curr() {
                    return Ok(ExprAST::Call {
                        fn_name: lowercase_ident.clone(),
                        args: vec![],
                    });
                }

                let mut args = vec![];

                loop {
                    args.push(self.parse_expr()?);

                    if let Token::RParen = self.current()? {
                        break;
                    }
                    self.advance()?;
                }

                // eat ')'
                self.advance();

                Ok(ExprAST::Call {
                    fn_name: lowercase_ident.clone(),
                    args,
                })
            }

            // '(' ident ')'
            _ => Ok(ExprAST::LowerCaseIdent {
                name: lowercase_ident,
            }),
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

        Parser::new(input).parse().unwrap().print_tree(0);
    }
}

fn main() {
    run_toplevel();
}
