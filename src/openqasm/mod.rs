mod ast;
mod lexer_rules;
pub mod semantic_analysis;
mod token;

use std::path::Path;
use std::{collections::HashMap, fs::read_to_string};

use crate::parser::ast::parse;
use crate::parser::lexer::Lexer;
use ast::MainProgram;
use semantic_analysis::{OpenQASMProgram, SemanticError};
use token::TokenMatch;

use crate::openqasm::token::Token;

use self::semantic_analysis::{Cbit, Condition, Gate, GateOperation, Operation, Qubit};

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

pub enum BasicOp {
    U(f32, f32, f32, Qubit),
    CX(Qubit, Qubit),
    Measure(Qubit, Cbit),
    ResetQ(Qubit),
    ResetC(Cbit),
}

impl OpenQASMProgram {
    pub fn get_basic_operations(&self) -> Vec<(Option<Condition>, BasicOp)> {
        let mut res = Vec::new();

        for (condition, op) in self.operations.iter() {
            for op in op.get_basic_operations(&self.gates) {
                res.push((condition.clone(), op));
            }
        }

        res
    }
}

impl Operation {
    pub fn get_basic_operations(&self, gates: &HashMap<String, Gate>) -> Vec<BasicOp> {
        match self {
            Operation::U(p1, p2, p3, a) => vec![BasicOp::U(*p1, *p2, *p3, a.clone())],
            Operation::CX(a1, a2) => vec![BasicOp::CX(a1.clone(), a2.clone())],
            Operation::Custom(name, params, args) => gates
                .get(name)
                .map(|gate| {
                    gate.operations
                        .iter()
                        .flat_map(|gate_op| {
                            gate_op
                                .get_operation(params, args)
                                .get_basic_operations(gates)
                        })
                        .collect()
                })
                .unwrap_or(vec![]),
            Operation::Measure(a1, a2) => vec![BasicOp::Measure(a1.clone(), a2.clone())],
            Operation::ResetQ(a) => vec![BasicOp::ResetQ(a.clone())],
            Operation::ResetC(a) => vec![BasicOp::ResetC(a.clone())],
        }
    }
}

impl GateOperation {
    pub fn get_operation(&self, params: &Vec<f32>, args: &Vec<Qubit>) -> Operation {
        match self {
            GateOperation::U(p1, p2, p3, a) => {
                Operation::U(p1(params), p2(params), p3(params), args[*a].clone())
            }
            GateOperation::CX(a1, a2) => Operation::CX(args[*a1].clone(), args[*a2].clone()),
            GateOperation::Custom(name, gate_params, gate_args) => Operation::Custom(
                name.clone(),
                gate_params.iter().map(|gp| gp(params)).collect(),
                gate_args.iter().map(|ga| args[*ga].clone()).collect(),
            ),
        }
    }
}
