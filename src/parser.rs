#![allow(dead_code)]
use crate::lexer::{Lexer, Token};
use std::collections::HashMap;

/// symbol table
#[derive(Debug)]
pub struct SymbolTable {
    // (name, type)
    // vec!["bool", "bool"]: bool -> bool
    tables: Vec<HashMap<String, Vec<String>>>,
}

impl SymbolTable {
    /// create a new symbol table
    pub fn new() -> Self {
        let mut root_talbe = HashMap::new();
        root_talbe.insert(
            "not".to_string(),
            vec!["bool".to_string(), "bool".to_string()],
        );
        root_talbe.insert(
            "abs".to_string(),
            vec!["int".to_string(), "int".to_string()],
        );
        SymbolTable {
            tables: vec![root_talbe],
        }
    }

    /// find a symbol's type
    pub fn lookup(&self, name: &str, scope: usize) -> Vec<String> {
        for i in (0..scope + 1).rev() {
            if let Some(ty) = self.tables[i].get(name) {
                return ty.to_vec();
            }
        }
        vec![]
    }

    pub fn insert(&mut self, name: String, symbol_type: Vec<String>) {
        let current_table = self.tables.last_mut().expect("Get current table failed.");
        current_table.insert(name, symbol_type);
    }

    pub fn push(&mut self, new_table: HashMap<String, Vec<String>>) {
        self.tables.push(new_table);
    }

    pub fn instance_type(
        &mut self,
        name: &str,
        generic_type: &str,
        instant_type: &str,
        scope: usize,
    ) {
        for i in (0..scope + 1).rev() {
            if let Some(_) = self.tables[i].get(name) {
                for (_, val) in self.tables[i].iter_mut() {
                    for ty in val {
                        *ty = ty.replace(generic_type, instant_type);
                    }
                }
            }
        }
    }

    pub fn print(&self) {
        for i in (0..self.tables.len()).rev() {
            println!("table[{}]:\n{:?}", i, self.tables[i]);
        }
    }
}

/// Top AST
/// may be an expression or blank input
pub enum ModuleAST {
    Expr(ExprAST),
    Blank,
}

impl ModuleAST {
    pub fn print_tree(&self) {
        match self {
            ModuleAST::Expr(expr) => {
                expr.print_tree(0);
            }
            ModuleAST::Blank => {}
        }
    }

    pub fn check_type(&self, symbols: &mut SymbolTable) -> Vec<String> {
        match self {
            ModuleAST::Expr(expr) => expr.check_type(symbols, 0, vec![]),
            ModuleAST::Blank => {
                vec![]
            }
        }
    }

    pub fn print_type(&self, symbols: &SymbolTable, expr_type: Vec<String>) {
        match self {
            ModuleAST::Expr(expr) => match expr {
                ExprAST::Defs(defs) => {
                    for def in defs {
                        let def_type = symbols.lookup(&def.name, symbols.tables.len() - 1);
                        let val = if def_type.len() > 1 { "<fun>" } else { "?" };
                        println!("val {} : {} = {}", def.name, def_type.join(" -> "), val);
                    }
                }
                _ => {
                    if expr_type.len() > 0 {
                        let val = if expr_type.len() > 1 { "<fun>" } else { "?" };
                        println!("- : {} = {}", expr_type.join(" -> "), val);
                    }
                }
            },
            ModuleAST::Blank => {}
        }
    }
}

/// function or variable declaration
#[derive(Debug)]
pub struct DefAST {
    pub name: String,
    pub is_rec: bool,
    pub params: Vec<String>,
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
            println!("  {}{}", indent_str, param);
        }
        println!("{}Body:", indent_str);
        self.body.print_tree(level + 1);
    }

    /// check the returned type of the definition
    /// return the type of the definition
    pub fn check_type(&self, symbols: &mut SymbolTable, scope: usize) -> Vec<String> {
        let def_type = symbols.lookup(&self.name, scope);
        if def_type.len() == 0 {
            panic!("[Type error] Undefined symbol: {}", self.name);
        }

        let body_type = self.body.check_type(symbols, scope, vec![]);

        // check the returned type of the definition
        let last_def_type = def_type.last().expect("get returned type from def failed");
        if last_def_type != &body_type[0] {
            if last_def_type.starts_with("'") {
                symbols.instance_type(&self.name, last_def_type, &body_type[0], scope);
            } else {
                panic!(
                    "[Type error] expe def return {:?}, got {:?}",
                    body_type[0], last_def_type
                );
            }
        }

        symbols.lookup(&self.name, scope)
    }
}

/// expressions
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

    Defs(Vec<DefAST>),

    LetBinding {
        defs: Vec<DefAST>,
        expr: Box<ExprAST>,
    },

    Branch {
        condexpr: Box<ExprAST>,
        thenexpr: Box<ExprAST>,
        elseexpr: Box<ExprAST>,
    },

    // if args.len()==0: variable eval
    // else: function call
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
            ExprAST::Defs(defs) => {
                for def in defs {
                    def.print_tree(level);
                }
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

    /// check type
    /// return the type of the expression
    /// supported types:
    ///   - int
    ///   - bool
    ///   - int->int->bool
    pub fn check_type(
        &self,
        symbols: &mut SymbolTable,
        scope: usize,
        type_hint: Vec<String>,
    ) -> Vec<String> {
        match self {
            ExprAST::Integer { .. } => {
                let int_type = vec!["int".to_string()];
                if type_hint.len() == 0 || type_hint == int_type {
                    return int_type;
                } else {
                    panic!("[Type error] expected int, got {:?}", type_hint);
                }
            }
            ExprAST::Bool { .. } => {
                let bool_type = vec!["bool".to_string()];
                if type_hint.len() == 0 || type_hint == bool_type {
                    return bool_type;
                } else {
                    panic!("[Type error] expected bool, got {:?}", type_hint);
                }
            }
            ExprAST::Unit => {
                let unit_type = vec!["unit".to_string()];
                if type_hint.len() == 0 || type_hint == unit_type {
                    return unit_type;
                } else {
                    panic!("[Type error] expected unit, got {:?}", type_hint);
                }
            }
            ExprAST::LowerCaseIdent { name } => {
                let id_type = symbols.lookup(&name, scope);
                if id_type.len() == 0 {
                    panic!("[Type error] undefined identifier: {}", name);
                }

                if type_hint.len() == 0 {
                    return id_type;
                }

                if id_type.len() != type_hint.len() {
                    panic!("[Type error] expected {:?}, got {:?}", type_hint, id_type);
                } else {
                    for (i, t) in id_type.iter().enumerate() {
                        if t != &type_hint[i] {
                            if t.starts_with("'") {
                                symbols.instance_type(name, t, &type_hint[i], scope);
                            } else {
                                panic!("[Type error] expected {:?}, got {:?}", type_hint, id_type);
                            }
                        }
                    }
                }

                type_hint
            }
            ExprAST::Binary { left, op, right } => {
                let int_ops = vec!["+", "-", "*", "/", "mod"];
                let int_bool_ops = vec!["=", "<>", "<", ">", "<=", ">="];
                let bool_ops = vec!["&&", "||"];
                let binary_type = if int_ops.contains(&op.as_str()) {
                    let int_type = vec!["int".to_string()];
                    left.check_type(symbols, scope, int_type.clone());
                    right.check_type(symbols, scope, int_type.clone());
                    int_type
                } else if bool_ops.contains(&op.as_str()) {
                    let bool_type = vec!["bool".to_string()];
                    left.check_type(symbols, scope, bool_type.clone());
                    right.check_type(symbols, scope, bool_type.clone());
                    bool_type
                } else if int_bool_ops.contains(&op.as_str()) {
                    let int_type = vec!["int".to_string()];
                    left.check_type(symbols, scope, int_type.clone());
                    right.check_type(symbols, scope, int_type.clone());
                    vec!["bool".to_string()]
                } else {
                    panic!("[Type error] Unknown op type:  {} ", op);
                };

                if binary_type == type_hint || type_hint.len() == 0 {
                    binary_type
                } else {
                    panic!(
                        "[Type error] expected {:?}, got {:?}",
                        type_hint, binary_type
                    );
                }
            }
            ExprAST::Unary { op: _, expr } => {
                let int_type = vec!["int".to_string()];
                if type_hint.len() != 0 && type_hint != int_type {
                    panic!("[Type error] expected int, got {:?}", type_hint);
                }
                expr.check_type(symbols, scope, int_type.clone());
                int_type
            }
            ExprAST::Defs(defs) => {
                for def in defs {
                    def.check_type(symbols, scope + 1);
                }
                defs[defs.len() - 1].check_type(symbols, scope + 1)
            }
            ExprAST::LetBinding { defs, expr } => {
                for def in defs {
                    def.check_type(symbols, scope + 1);
                }
                let expr_type = expr.check_type(symbols, scope + 1, type_hint.clone());
                if type_hint.len() == 0 || type_hint == expr_type {
                    expr_type
                } else {
                    panic!("[Type error] expected {:?}, got {:?}", type_hint, expr_type);
                }
            }
            ExprAST::Branch {
                condexpr,
                thenexpr,
                elseexpr,
            } => {
                if type_hint.len() == 0 {
                    condexpr.check_type(symbols, scope, vec!["bool".to_string()]);
                    let then_type = thenexpr.check_type(symbols, scope, vec![]);
                    let else_type = elseexpr.check_type(symbols, scope, vec![]);
                    if then_type != else_type {
                        if then_type[0].starts_with("'") && !else_type[0].starts_with("'") {
                            else_type
                        } else if then_type[0].starts_with("'") && else_type[0].starts_with("'") {
                            then_type
                        } else if !then_type[0].starts_with("'") && else_type[0].starts_with("'") {
                            then_type
                        } else {
                            panic!("[Type error] expected {:?}, got {:?}", then_type, else_type);
                        }
                    } else {
                        then_type
                    }
                } else {
                    condexpr.check_type(symbols, scope, vec!["bool".to_string()]);
                    thenexpr.check_type(symbols, scope, type_hint.clone());
                    elseexpr.check_type(symbols, scope, type_hint)
                }
            }
            ExprAST::Call { fn_name, args } => {
                let fn_type = symbols.lookup(&fn_name, scope);
                if fn_type.len() == 0 {
                    panic!("[Type error] undefined identifier: {}", fn_name);
                }

                let num_args = args.len();
                if num_args >= fn_type.len() {
                    panic!("[Type error] too many arguments for function: {}", fn_name);
                }

                let mut i = 0;
                while i < num_args {
                    let arg_type = args[i].check_type(symbols, scope, vec![]);
                    if arg_type.len() == 0 {
                        panic!("[Type error] Unknown type for arg[{}].", i);
                    }
                    // arg should not be a function
                    if arg_type[0] != fn_type[i] {
                        if !fn_type[i].starts_with("'") && !arg_type[0].starts_with("'") {
                            panic!("[Type error] expected {:?}, got {:?}", fn_type[i], arg_type);
                        }
                    }
                    i += 1;
                }

                let mut call_type = vec![];
                while i < fn_type.len() {
                    call_type.push(fn_type[i].clone());
                    i += 1;
                }

                if type_hint.len() == 0 {
                    call_type
                } else {
                    if type_hint.len() != call_type.len() {
                        panic!("[Type error] expected {:?}, got {:?}", type_hint, call_type);
                    } else {
                        for i in 0..type_hint.len() {
                            if type_hint[i] != call_type[i] {
                                if !type_hint[i].starts_with("'") && !call_type[i].starts_with("'")
                                {
                                    panic!(
                                        "[Type error] expected {:?}, got {:?}",
                                        type_hint, call_type
                                    );
                                }
                            }
                        }
                        call_type
                    }
                }
            }
        }
    }
}

/// Represents the `Expr` parser.
pub struct Parser<'a> {
    tokens: Vec<Token>,
    symbols: &'a mut SymbolTable,
    pos: usize,
}

// I'm ignoring the 'must_use' lint in order to call 'self.advance' without checking
// the result when an EOF is acceptable.
// #[allow(unused_must_use)]
impl<'a> Parser<'a> {
    /// Creates a new parser, given an input `str` and a `HashMap` binding
    /// an operator and its precedence in binary expressions.
    pub fn new(input: String, symbols: &'a mut SymbolTable) -> Self {
        let mut lexer = Lexer::new(input.as_str());
        let tokens = lexer.by_ref().collect();

        Parser {
            tokens,
            symbols,
            pos: 0,
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

    /// module ::= expr | blank
    pub fn parse(&mut self) -> Result<ModuleAST, &'static str> {
        // blank or only comment
        if self.at_end() {
            return Ok(ModuleAST::Blank);
        }

        let result = match self.curr() {
            _ => ModuleAST::Expr(self.parse_expr().expect("parse module expr")),
        };

        if !self.at_end() {
            match self.curr() {
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
        let left = self
            .parse_unary()
            .expect("parse expr: parse expr unary error");
        self.parse_op_rhs(0, left)
    }

    /// unary ::= primary
    ///       ::= ('-' | '+') primary
    pub fn parse_unary(&mut self) -> Result<ExprAST, &'static str> {
        let op_name = match self.current().expect("Unexpected end of file.") {
            Token::Op(op_name, _) => {
                // eat operator
                self.advance().expect("parse unary: expected operator");

                // '-2', '+3'
                match op_name.as_str() {
                    "+" => String::from("+"),
                    "-" => String::from("-"),
                    _ => return Err("unknown unary operator."),
                }
            }

            // just a primary expression
            _ => return self.parse_primary(),
        };

        Ok(ExprAST::Unary {
            op: op_name,
            expr: Box::new(
                self.parse_primary()
                    .expect("parse unary: parse primary error"),
            ),
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

            let mut right = self.parse_unary().expect("parse op rhs: parse unary error");

            let next_prec = self.get_tok_precedence();

            if curr_prec < next_prec {
                right = self
                    .parse_op_rhs(curr_prec + 1, right)
                    .expect("parse op rhs: parse op rhs error");
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

    /// letexpr ::= defs ['in' expr]
    pub fn parse_let_expr(&mut self) -> Result<ExprAST, &'static str> {
        let defs = self.parse_defs().expect("parse let expr: parse defs");

        if self.at_end() {
            return Ok(ExprAST::Defs(defs));
        }

        match self.curr() {
            Token::In => {
                // eat 'in'
                self.advance().expect("parse let expr: eat 'in'");
                let expr = self.parse_expr().expect("parse let expr: parse expr");
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

        // create a new scope
        let new_table = HashMap::new();
        self.symbols.push(new_table);

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
                            params.push(name);
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

                // insert into symbol table
                // name or params have generic type like: 'a,
                // we will instance it when parse binding body.
                let mut generic_type_name = 97u8; // start with 'a'
                let mut func_type: Vec<String> = vec![];

                for param in params.iter() {
                    let param_type = format!("{}{}", "'", generic_type_name as char);
                    func_type.push(param_type.clone());
                    self.symbols.insert(param.clone(), vec![param_type]);
                    generic_type_name += 1;
                }
                func_type.push(format!("{}{}", "'", generic_type_name as char));
                self.symbols.insert(name.clone(), func_type);

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
        let mut symbols = SymbolTable::new();
        let input =
            r#"(**Requires: [n >= 0].*) 1+1 (*[odd n] is whether [n] is odd.(*ooxx**)**) +1;;"#;
        let mut parser = Parser::new(input.to_string(), &mut symbols);
        parser.parse().unwrap();
    }

    #[test]
    fn test_comment2() {
        let mut symbols = SymbolTable::new();
        let input = r#"(** [even n] is whether [n] is even.
    Requires: [n >= 0]. *)
let rec even n =
  n = 0 || odd (n - 1)

(** [odd n] is whether [n] is odd.
    Requires: [n >= 0]. *)
and odd n =
  n <> 0 && even (n - 1);;"#;
        let mut parser = Parser::new(input.to_string(), &mut symbols);
        parser.parse().unwrap();
    }

    #[test]
    fn test_defines() {
        let mut symbols = SymbolTable::new();
        let input = r#"let a =1 and b = 2;;"#;
        let mut parser = Parser::new(input.to_string(), &mut symbols);
        parser.parse().unwrap();
    }

    #[test]
    fn test_ident() {
        let mut symbols = SymbolTable::new();
        let input = r#"let a_s''' = 3 + 2_00;;"#;
        let mut parser = Parser::new(input.to_string(), &mut symbols);
        parser.parse().unwrap();
    }

    #[test]
    fn test_ident2() {
        let mut symbols = SymbolTable::new();
        let input = r#"(1+ add x (1+2));;"#;
        let mut parser = Parser::new(input.to_string(), &mut symbols);
        parser.parse().unwrap();
    }

    #[test]
    fn test_paren() {
        let mut symbols = SymbolTable::new();
        let input = r#"(true && not (true && true));;"#;
        let mut parser = Parser::new(input.to_string(), &mut symbols);
        parser.parse().unwrap();
    }

    #[test]
    fn test_prime() {
        let mut symbols = SymbolTable::new();
        let input = r#"let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1)) in
    n <> 1 && is_not_divisor 2;;"#;

        let mut parser = Parser::new(input.to_string(), &mut symbols);
        parser.parse().unwrap();
    }

    #[test]
    fn test_branch() {
        let mut symbols = SymbolTable::new();
        let input = r#"let rec gcd a b = if b = 0 then a else gcd b (a mod b);;"#;
        let mut parser = Parser::new(input.to_string(), &mut symbols);
        parser.parse().unwrap();
    }

    #[test]
    fn test_letin() {
        let mut symbols = SymbolTable::new();
        let input = r#"let x = 1 in x+1;;"#;
        let mut parser = Parser::new(input.to_string(), &mut symbols);
        parser.parse().unwrap();
    }
}
