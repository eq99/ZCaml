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

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

#[derive(Debug)]
pub struct DefAST {
    pub name: String,
    pub is_rec: bool,
    pub params: Vec<(String, String)>, // (name, type)
    pub body: Box<ExprAST>,
}

impl DefAST {
    /// print the definition as tree
    pub fn print_tree(&self, level: usize) {
        let indent_str = "  ".repeat(level);
        let head = if self.is_rec {
            format!("{}Define Rec:{}", indent_str, self.name)
        } else {
            format!("{}Define:{}", indent_str, self.name)
        };
        println!("{}", head);
        println!("{}Params:", indent_str);
        for param in &self.params {
            println!("  {}{}:{}", indent_str, param.0, param.1);
        }
        println!("{}Body:", indent_str);
        self.body.print_tree(level + 1);
    }
}

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

    Unary {
        op: String,
        expr: Box<ExprAST>,
    },

    Binary {
        left: Box<ExprAST>,
        op: String,
        right: Box<ExprAST>,
    },

    LetBinding {
        defs: Vec<DefAST>,
        expr: Box<ExprAST>,
    },

    Call {
        fn_name: String,
        args: Vec<ExprAST>,
    },
}

impl ExprAST {
    // display AST as tree
    pub fn print_tree(&self, level: usize) {
        // tree level
        let indent = "   ".repeat(level);

        match self {
            ExprAST::Integer { value } => println!("{}{}", indent, value),
            ExprAST::Bool { value } => println!("{}{}", indent, value),
            ExprAST::LowerCaseIdent { name } => println!("{}{}", indent, name),
            ExprAST::Unit => println!("{}Unit", indent),
            ExprAST::Binary { left, op, right } => {
                println!("{}Binary:({})", indent, op);
                left.print_tree(level + 1);
                right.print_tree(level + 1);
            }
            ExprAST::Unary { op, expr } => {
                print!("{}Unary:({})", indent, op);
                expr.print_tree(0);
            }
            ExprAST::LetBinding { defs, expr } => {
                println!("{}LetBindingExpr:", indent);
                for def in defs {
                    def.print_tree(level + 1);
                }
                println!("{}In:", indent);
                expr.print_tree(level + 1);
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

pub enum ModuleAST {
    Defs(Vec<DefAST>),
    Expr(ExprAST),
}

impl ModuleAST {
    pub fn print_tree(&self) {
        match self {
            ModuleAST::Defs(defs) => {
                for def in defs {
                    def.print_tree(0);
                }
            }
            ModuleAST::Expr(expr) => {
                expr.print_tree(0);
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
// #[allow(unused_must_use)]
impl Parser {
    /// Creates a new parser, given an input `str` and a `HashMap` binding
    /// an operator and its precedence in binary expressions.
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer::new(input.as_str());
        let tokens = lexer.by_ref().collect();

        Parser { tokens, pos: 0 }
    }

    /// Parses the content of the parser.
    pub fn parse(&mut self) -> Result<ModuleAST, &'static str> {
        let result = match self.current()? {
            Token::Let => ModuleAST::Defs(self.parse_defs()?),
            _ => ModuleAST::Expr(self.parse_expr()?),
        };

        if !self.at_end() {
            match self.current()? {
                Token::EOF => Ok(result),
                _ => {
                    println!("Unexpected token: {:?}", self.current());
                    Err("Unexpected token after parsed expression.")
                }
            }
        } else {
            Ok(result)
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
            Err("Current: unexpected end of file.")
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
            Err("Advance: unexpected end of file.")
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

    /// expr ::= unary binoprhs
    fn parse_expr(&mut self) -> Result<ExprAST, &'static str> {
        let left = self.parse_unary()?;
        self.parse_op_rhs(0, left)
    }

    /// Parses a primary expression (an identifier, a number or a parenthesized expression).
    /// primary ::= identexpr
    ///         ::= numberexpr
    ///         ::= parenexpr
    ///         ::= 'let' [ 'rec' ] let-binding { 'and' let-binding } 'in' expr   
    fn parse_primary(&mut self) -> Result<ExprAST, &'static str> {
        match self.curr() {
            Token::LowercaseIdent(_) => self.parse_ident_expr(),
            Token::Integer(_) => self.parse_integer_expr(),
            Token::LParen => self.parse_paren_expr(),
            Token::Let => self.parse_let_expr(),
            _ => Err("Unknown primary expression."),
        }
    }

    // unary ::= primary
    //       ::= ('-' | '+') primary
    fn parse_unary(&mut self) -> Result<ExprAST, &'static str> {
        let op_name = match self.current()? {
            Token::Op(op_name, _) => {
                // eat operator
                self.advance().expect("parse unary: expected operator");

                // '-2', '+3'
                match op_name.as_str() {
                    "+" => String::from("+"),
                    "-" => String::from("-"),
                    _ => return Err("bad unary operator."),
                }
            }

            // just a primary expression
            _ => return self.parse_primary(),
        };

        Ok(ExprAST::Unary {
            op: op_name,
            expr: Box::new(self.parse_primary()?),
        })
    }

    /// binoprhs ::= (op unary)*
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

            // eat operator
            self.advance().expect("parse op rhs: eat operator");

            let mut right = self.parse_unary()?;

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
                let _ = self.advance();
                Ok(ExprAST::Integer { value: integer })
            }
            _ => Err("Expected integer."),
        }
    }

    /// parenexpr ::= '(' expr ')'
    fn parse_paren_expr(&mut self) -> Result<ExprAST, &'static str> {
        match self.current()? {
            Token::LParen => {
                // eat '('
                self.advance().expect("parse paren expr: eat (");
            }
            _ => return Err("Expected '(' character at start of parenthesized expression."),
        }

        let expr = self.parse_expr()?;

        match self.current()? {
            Token::RParen => {
                // eat ')'
                let _ = self.advance();
            }
            _ => return Err("Expected ')' character at end of parenthesized expression."),
        }

        Ok(expr)
    }

    /// Parses an expression that starts with an identifier (either a variable or a function call).
    fn parse_ident_expr(&mut self) -> Result<ExprAST, &'static str> {
        let ident_name = match self.curr() {
            Token::LowercaseIdent(id) => id,
            _ => return Err("bad identifier."),
        };

        // last ident
        if self.advance().is_err() {
            return Ok(ExprAST::LowerCaseIdent { name: ident_name });
        }

        // expr ::= ident_name args+
        // args can be: parenexpr, ident, integer, bool
        let mut args = vec![];
        loop {
            match self.curr() {
                Token::LParen => {
                    args.push(self.parse_paren_expr()?);

                    if self.at_end() {
                        break;
                    }
                }
                Token::LowercaseIdent(name) => {
                    args.push(ExprAST::LowerCaseIdent { name });
                    // eat ident
                    if self.advance().is_err() {
                        break;
                    }
                }
                Token::Integer(value) => {
                    args.push(ExprAST::Integer { value });
                    // eat integer
                    if self.advance().is_err() {
                        break;
                    }
                }
                Token::Bool(value) => {
                    args.push(ExprAST::Bool { value });
                    // eat bool
                    if self.advance().is_err() {
                        break;
                    }
                }
                _ => {
                    break;
                }
            }
        }

        if args.len() == 0 {
            Ok(ExprAST::LowerCaseIdent { name: ident_name })
        } else {
            Ok(ExprAST::Call {
                fn_name: ident_name,
                args,
            })
        }
    }

    /// Parses a let expression.
    fn parse_let_expr(&mut self) -> Result<ExprAST, &'static str> {
        let defs = self.parse_defs()?;
        match self.current()? {
            Token::In => {
                // eat 'in'
                self.advance()?;
                let expr = self.parse_expr()?;
                Ok(ExprAST::LetBinding {
                    defs,
                    expr: Box::new(expr),
                })
            }
            _ => return Err("Expected 'in' after let-binding."),
        }
    }

    /// definition	::=	'let' ['rec'] let-binding { 'and' let-binding }
    fn parse_defs(&mut self) -> Result<Vec<DefAST>, &'static str> {
        match self.curr() {
            Token::Let => (),
            _ => return Err("Expected 'let' keyword."),
        }

        // eat 'let'
        self.advance().expect("Expect define body");

        // match 'rec'
        let is_rec = match self.curr() {
            Token::Rec => {
                // eat 'rec'
                self.advance().expect("Expect define body");
                true
            }
            _ => false,
        };

        let mut defs = vec![];
        loop {
            let def = self.parse_binding(is_rec).expect("parse define");
            defs.push(def);

            if self.at_end() {
                break;
            }

            match self.curr() {
                Token::And => {
                    // eat 'and'
                    self.advance()?;
                    defs.push(self.parse_binding(is_rec)?);
                }
                _ => break,
            }
        }
        Ok(defs)
    }

    /// parse let bindings
    /// let-binding ::= ident {ident} '=' expr
    pub fn parse_binding(&mut self, is_rec: bool) -> Result<DefAST, &'static str> {
        match self.curr() {
            Token::LowercaseIdent(name) => {
                // eat ident
                self.advance().expect("eat binding name");

                // parse params
                let mut params = vec![];
                loop {
                    match self.curr() {
                        Token::LowercaseIdent(name) => {
                            // eat ident
                            self.advance().expect("eat param name");
                            params.push((name, "Untyped".to_string()));
                        }
                        Token::Op(name, _) => {
                            if name.as_str() == "=" {
                                // eat "="
                                self.advance().expect("eat '='");
                                break;
                            }
                        }
                        _ => return Err("params name must be lowercase ident."),
                    }
                }

                // parse body
                let body = self.parse_expr().expect("parse binding body");

                Ok(DefAST {
                    is_rec,
                    name,
                    params,
                    body: Box::new(body),
                })
            }
            _ => Err("Expected identifier."),
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
        print_flush!("ZCaml# ");

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

        println!("Tree:");
        match Parser::new(input).parse() {
            Ok(ast) => ast.print_tree(),
            Err(err) => println!("{}", err),
        }
    }
}

fn main() {
    run_toplevel();
}
