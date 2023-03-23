mod ast;
mod lexer_rules;
mod semantic_analysis;
mod token;

use std::fs::read_to_string;
use std::path::Path;

use ast::MainProgram;
use compiler::ast::parse;
use compiler::lexer::Lexer;
use semantic_analysis::{OpenQASMProgram, SemanticError};
use token::TokenMatch;

use crate::token::Token;

#[derive(Debug)]
pub enum OpenQASMError {
    FileError,
    TokenError,
    SyntaxError,
    SemanticError(SemanticError),
}

pub fn parse_openqasm(file_path: &Path) -> Result<OpenQASMProgram, OpenQASMError> {
    let mut lexer = Lexer::new();
    lexer_rules::add_open_qasm_rules(&mut lexer);

    let file_str = read_to_string(file_path).map_err(|_| OpenQASMError::FileError)?;
    let file_chars = file_str.bytes().map(|b| b as char).collect();

    let tokens = lexer
        .parse(file_chars)
        .map_err(|_| OpenQASMError::TokenError)?;

    let t_vec: Vec<TokenMatch> = tokens
        .iter()
        .map(|(a, s)| (*a, s.into_iter().collect::<String>()))
        .collect();

    let ast = parse::<MainProgram, Token>(&t_vec).map_err(|_| OpenQASMError::SyntaxError)?;

    OpenQASMProgram::from_ast(&ast).map_err(|e| OpenQASMError::SemanticError(e))
}

fn main() {
    let b = parse_openqasm(Path::new("example.qasm")).unwrap();
    println!("QRegs: ");
    for (n, s) in b.qregs.iter() {
        println!("  {}: {}", n, s);
    }
    println!("CRegs: ");
    for (n, s) in b.cregs.iter() {
        println!("  {}: {}", n, s);
    }
    println!("GATES: ");
    for (n, g) in b.gates.iter() {
        println!("  {}: {}, {}", n, g.num_arguments, g.num_targets);
    }
    println!("Operations: ");
    for op in b.operations.iter() {
        println!("  {:?}", op);
    }
}
