mod lexer;
mod parser;

// use crate::lexer::*;
use crate::parser::*;
use std::io;
use std::io::prelude::*;

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

        let mut symbols = SymbolTable::new();
        match Parser::new(input, &mut symbols).parse() {
            Ok(ast) => {
                let expr_type = ast.check_type(&mut symbols);
                ast.print_type(&symbols, expr_type);
                println!("\nSymbol table:");
                symbols.print();
            }
            Err(err) => println!("{}", err),
        }
    }
}

fn main() {
    run_toplevel();
}
