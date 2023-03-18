mod ast;
mod lexer_rules;
mod token;

use ast::MainProgram;
use compiler::ast::ast_debug::ASTDebug;
use compiler::ast::ast_node::{ASTNode, TokenIter};
use compiler::lexer::Lexer;
use token::TokenMatch;

fn main() {
    let file_str = "
        OPENQASM 2.0;

        // Gate definitions
        gate u3(theta,phi,lambda) q { U(theta,phi,lambda) q; }
        gate u1(lambda) q { U(0,0,lambda) q; }
        gate cx c,t { CX c,t; }
        gate id a { U(0,0,0) a; }

        // Registers
        qreg q[3]; // Comment after code
        creg c0[1];

        // Operations
        u3(0.3,0.2,0.1) q[0];
        cx q[1],q[2];

        // Measure
        measure q[0] -> c0[0];
    ";

    let mut lexer = Lexer::new();
    lexer_rules::add_open_qasm_rules(&mut lexer);

    let file_chars = file_str.bytes().map(|b| b as char).collect();
    let tokens = lexer.parse(file_chars);

    let t_vec: Vec<TokenMatch> = tokens
        .iter()
        .map(|(a, s)| (*a, s.into_iter().collect::<String>()))
        .collect();

    let mut iter = TokenIter::create(&t_vec);
    let a = MainProgram::parse(&mut iter);
    if iter.next().is_none() && a.is_some() {
        a.unwrap().print();
    }
    else {
        println!("INCORRECT!!");
    }
}
