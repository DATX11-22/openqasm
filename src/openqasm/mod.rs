mod ast;
mod lexer_rules;
pub mod semantic_analysis;
mod token;

use std::fs::read_to_string;
use std::path::Path;

use crate::parser::ast::parse;
use crate::parser::lexer::Lexer;
use ast::MainProgram;
use semantic_analysis::{OpenQASMProgram, SemanticError};
use token::TokenMatch;

use crate::openqasm::token::Token;

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

    let tokens = read_file_tokens(&lexer, file_path)?;

    let t_vec: Vec<TokenMatch> = tokens
        .iter()
        .map(|(a, s)| (*a, s.into_iter().collect::<String>()))
        .collect();

    let ast = parse::<MainProgram, Token>(&t_vec).map_err(|_| OpenQASMError::SyntaxError)?;

    OpenQASMProgram::from_ast(&ast).map_err(|e| OpenQASMError::SemanticError(e))
}

fn read_file_tokens(
    lexer: &Lexer<Token>,
    file_path: &Path,
) -> Result<Vec<(Token, Vec<char>)>, OpenQASMError> {
    let file_str = read_to_string(file_path).map_err(|_| OpenQASMError::FileError)?;
    let file_chars = file_str.bytes().map(|b| b as char).collect();

    let mut tokens = lexer
        .parse(file_chars)
        .map_err(|_| OpenQASMError::TokenError)?;

    for i in 0..tokens.len() {
        if i + 2 < tokens.len()
            && tokens[i + 0].0 == Token::Include
            && tokens[i + 1].0 == Token::Str
            && tokens[i + 2].0 == Token::Semicolon
        {
            let file_dir = file_path.parent().ok_or(OpenQASMError::FileError)?;

            let other_file_path_str = tokens[i + 1].1.clone().into_iter().collect::<String>();
            let other_file_path_str = &other_file_path_str[1..other_file_path_str.len() - 1];
            let other_file_path = file_dir.join(Path::new(other_file_path_str));

            let other_tokens = read_file_tokens(lexer, &other_file_path)?;

            tokens.splice(i..i + 3, other_tokens);
        }
    }

    Ok(tokens)
}
