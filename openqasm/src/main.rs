mod ast;
mod lexer_rules;
mod token;
mod semantic_analysis;

use ast::MainProgram;
use compiler::ast::ast_debug::ASTDebug;
use compiler::ast::ast_node::{ASTNode, TokenIter};
use compiler::lexer::Lexer;
use semantic_analysis::OpenQASMProgram;
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
        let a = a.unwrap();
        a.print();

        let b = OpenQASMProgram::from_ast(&a).unwrap();
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
    else {
        println!("INCORRECT!!");
    }
}
