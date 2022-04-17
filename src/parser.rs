#![allow(dead_code)]
use crate::lexer::{Lexer, Token};

pub enum ModuleAST {
    Defs(Vec<DefAST>),
    Expr(ExprAST),
    Blank,
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
            ModuleAST::Blank => {}
        }
    }
}

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

    Branch {
        condexpr: Box<ExprAST>,
        thenexpr: Box<ExprAST>,
        elseexpr: Box<ExprAST>,
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
            ExprAST::Branch {
                condexpr,
                thenexpr,
                elseexpr,
            } => {
                println!("{}Cond:", indent);
                condexpr.print_tree(level + 1);
                println!("{}Then:", indent);
                thenexpr.print_tree(level + 1);
                println!("{}Else:", indent);
                elseexpr.print_tree(level + 1);
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
// #[allow(unused_must_use)]
impl Parser {
    /// Creates a new parser, given an input `str` and a `HashMap` binding
    /// an operator and its precedence in binary expressions.
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer::new(input.as_str());
        let tokens = lexer.by_ref().collect();

        Parser { tokens, pos: 0 }
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

    /// module ::= definition | expr
    pub fn parse(&mut self) -> Result<ModuleAST, &'static str> {
        // blank or only comment
        if self.at_end() {
            return Ok(ModuleAST::Blank);
        }

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

    /// expr ::= unary binoprhs
    pub fn parse_expr(&mut self) -> Result<ExprAST, &'static str> {
        let left = self.parse_unary()?;
        self.parse_op_rhs(0, left)
    }

    /// unary ::= primary
    ///       ::= ('-' | '+') primary
    pub fn parse_unary(&mut self) -> Result<ExprAST, &'static str> {
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

    /// Parses a primary expression (an identifier, a number or a parenthesized expression).
    /// primary ::= identexpr
    ///         ::= integerexpr
    ///         ::= boolexpr
    ///         ::= parenexpr
    ///         ::= 'let' [ 'rec' ] let-binding { 'and' let-binding } 'in' expr   
    pub fn parse_primary(&mut self) -> Result<ExprAST, &'static str> {
        match self.curr() {
            Token::LowercaseIdent(_) => self.parse_ident_expr(),
            Token::Integer(_) => self.parse_integer_expr(),
            Token::Bool(_) => self.parse_bool_expr(),
            Token::LParen => self.parse_paren_expr(),
            Token::Let => self.parse_let_expr(),
            Token::If => self.parse_branch_expr(),
            _ => Err("Unknown primary expression."),
        }
    }

    /// binoprhs ::= (op unary)*
    pub fn parse_op_rhs(
        &mut self,
        left_prec: i32,
        mut left: ExprAST,
    ) -> Result<ExprAST, &'static str> {
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

    /// Parses a integer
    pub fn parse_integer_expr(&mut self) -> Result<ExprAST, &'static str> {
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

    /// parses a bool
    pub fn parse_bool_expr(&mut self) -> Result<ExprAST, &'static str> {
        // Simply convert Token::Bool to Expr::Bool
        match self.curr() {
            Token::Bool(val) => {
                // eat bool
                let _ = self.advance();
                Ok(ExprAST::Bool { value: val })
            }
            _ => Err("Expected bool."),
        }
    }

    /// parenexpr ::= '(' expr ')'
    pub fn parse_paren_expr(&mut self) -> Result<ExprAST, &'static str> {
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

    /// Parses a single identifier or function call
    /// idnetexpr ::= ident_name args*
    /// args ::= parenexpr
    ///      ::= lowercaseident
    ///      ::= integerexpr
    ///      ::= boolexpr
    pub fn parse_ident_expr(&mut self) -> Result<ExprAST, &'static str> {
        let ident_name = match self.curr() {
            Token::LowercaseIdent(id) => id,
            _ => return Err("bad identifier."),
        };

        // last ident
        if self.advance().is_err() {
            return Ok(ExprAST::LowerCaseIdent { name: ident_name });
        }

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

    /// letexpr ::= defs 'in' expr
    pub fn parse_let_expr(&mut self) -> Result<ExprAST, &'static str> {
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

    /// branchexpr ::= 'if' expr 'then' expr 'else' expr
    pub fn parse_branch_expr(&mut self) -> Result<ExprAST, &'static str> {
        match self.curr() {
            Token::If => {
                // eat 'if'
                self.advance().expect("parse branch expr: eat if");
            }
            _ => return Err("Expected 'if' after if-branch."),
        }

        let condexpr = self.parse_expr()?;

        match self.current().expect("Must have then expr in branch expr") {
            Token::Then => {
                // eat 'then'
                self.advance().expect("parse branch expr: eat then");
            }
            _ => return Err("Expected 'then' after if-branch."),
        }

        let thenexpr = self.parse_expr()?;

        match self.current().expect("Must have else expr in expr") {
            Token::Else => {
                // eat 'else'
                self.advance().expect("parse branch expr: eat else");
            }
            _ => return Err("Expected 'else' after if-branch."),
        }

        let elseexpr = self.parse_expr()?;

        Ok(ExprAST::Branch {
            condexpr: Box::new(condexpr),
            thenexpr: Box::new(thenexpr),
            elseexpr: Box::new(elseexpr),
        })
    }

    /// defs ::= 'let' ['rec'] let-binding { 'and' let-binding }
    pub fn parse_defs(&mut self) -> Result<Vec<DefAST>, &'static str> {
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
                Token::And => {}
                _ => break,
            }
            // eat 'and'
            self.advance().expect("parse defs: eat 'and'");
        }
        Ok(defs)
    }

    /// parse let bindings
    /// let binding defines a variable or a function
    /// let-binding ::= ident ident* '=' expr
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

#[cfg(test)]
mod tests {
    use crate::parser::*;

    #[test]
    fn test_comment() {
        let input =
            r#"(**Requires: [n >= 0].*) 1+1 (*[odd n] is whether [n] is odd.(*ooxx**)**) +1;;"#;
        let mut parser = Parser::new(input.to_string());
        parser.parse().unwrap();
    }

    #[test]
    fn test_comment2() {
        let input = r#"(** [even n] is whether [n] is even.
    Requires: [n >= 0]. *)
let rec even n =
  n = 0 || odd (n - 1)

(** [odd n] is whether [n] is odd.
    Requires: [n >= 0]. *)
and odd n =
  n <> 0 && even (n - 1);;"#;
        let mut parser = Parser::new(input.to_string());
        parser.parse().unwrap();
    }

    #[test]
    fn test_defines() {
        let input = r#"let a =1 and b = 2;;"#;
        let mut parser = Parser::new(input.to_string());
        parser.parse().unwrap();
    }

    #[test]
    fn test_ident() {
        let input = r#"let a_s''' = 3 + 2_00;;"#;
        let mut parser = Parser::new(input.to_string());
        parser.parse().unwrap();
    }

    #[test]
    fn test_ident2() {
        let input = r#"(1+ add x (1+2));;"#;
        let mut parser = Parser::new(input.to_string());
        parser.parse().unwrap();
    }

    #[test]
    fn test_paren() {
        let input = r#"(true && not (true && true));;"#;
        let mut parser = Parser::new(input.to_string());
        parser.parse().unwrap();
    }

    #[test]
    fn test_prime() {
        let input = r#"let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1)) in
    n <> 1 && is_not_divisor 2;;"#;

        let mut parser = Parser::new(input.to_string());
        parser.parse().unwrap();
    }

    #[test]
    fn test_branch() {
        let input = r#"let rec gcd a b = if b = 0 then a else gcd b (a mod b);;"#;
        let mut parser = Parser::new(input.to_string());
        parser.parse().unwrap();
    }
}
