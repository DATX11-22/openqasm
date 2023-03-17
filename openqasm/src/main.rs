mod ast;
mod lexer_rules;
mod token;

use ast::MainProgram;
use compiler::ast::ast_debug::ASTDebug;
use compiler::ast::ast_node::ASTNode;
use compiler::lexer::Lexer;
use token::TokenMatch;

fn main() {
    let file_str = "
        OPENQASM 2.0;

        qreg r1[10];
        qreg r2[10];
    ";

    let mut lexer = Lexer::new();
    lexer_rules::add_open_qasm_rules(&mut lexer);

    let file_chars = file_str.bytes().map(|b| b as char).collect();
    let tokens = lexer.parse(file_chars);

    let t_vec: Vec<TokenMatch> = tokens
        .iter()
        .map(|(a, s)| (*a, s.into_iter().collect::<String>()))
        .collect();

    let mut iter = t_vec.iter();
    let a = MainProgram::parse(&mut iter);
    if let Some(a) = a {
        a.print();
    }
}
