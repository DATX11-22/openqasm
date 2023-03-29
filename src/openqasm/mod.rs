//! All the openqasm specific code

mod ast;
mod lexer_rules;
pub mod semantic_analysis;
mod token;

use self::semantic_analysis::{Cbit, Condition, Gate, GateOperation, Operation, Qubit};
use crate::openqasm::token::Token;
use crate::parser::ast::parse;
use crate::parser::lexer::Lexer;
use ast::MainProgram;
use semantic_analysis::{OpenQASMProgram, SemanticError};
use std::path::Path;
use std::{collections::HashMap, fs::read_to_string};
use token::TokenMatch;

/// The different errors that can occur when parsing an openqasm program
#[derive(Debug)]
pub enum OpenQASMError {
    /// The file(or some included file) could not be opened.
    FileError,

    /// The program contains some invalid token.
    TokenError,

    /// The program contains a syntactic error.
    SyntaxError,

    /// The program contains a semantic error. It contains a [SemanticError]
    /// which gives more precise information about what kind of semantic error occured.
    SemanticError(SemanticError),
}

/// The most importan function exposed by the library.
/// This function reads a file containing openqasm 2.0 code and parses it.
/// If the file include statments they are replaced by the contents of the included file
/// before being parsed.
///
/// Returns an [OpenQASMProgram] if parsing was successful. This datatype contains information
/// about the different quantum and classical registers, custom gates and operations performed.
///
/// If parsing was unsuccuessful an [OpenQASMError] is returned.
///
/// # Example
/// ```
/// use openqasm_parser::openqasm;
/// use std::path::Path;
///
/// let program = openqasm::parse_openqasm(Path::new("openqasmfile.qasm"));
/// ```
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

/// Reads a file and converts it into tokens. If the file contains include statements
/// the function recursively calls itself on the included file and splices it into the
/// tokenstream at the position of the include statement. Used by parse_openqasm.
///
/// Returns either a vector of Tokens, FileError or TokenError.
fn read_file_tokens(
    lexer: &Lexer<Token>,
    file_path: &Path,
) -> Result<Vec<(Token, Vec<char>)>, OpenQASMError> {
    let file_str = read_to_string(file_path).map_err(|_| OpenQASMError::FileError)?;
    let file_chars = file_str.bytes().map(|b| b as char).collect();

    // Parse the file into a list of tokens
    let mut tokens = lexer
        .parse(file_chars)
        .map_err(|_| OpenQASMError::TokenError)?;

    // Check for include statements and splice in the tokens from the other file
    for i in 0..tokens.len() {
        if i + 2 < tokens.len()
            && tokens[i + 0].0 == Token::Include
            && tokens[i + 1].0 == Token::Str
            && tokens[i + 2].0 == Token::Semicolon
        {
            let file_dir = file_path.parent().ok_or(OpenQASMError::FileError)?;

            // Convert the included files path to be relative to the parent file
            let other_file_path_str = tokens[i + 1].1.clone().into_iter().collect::<String>();
            let other_file_path_str = &other_file_path_str[1..other_file_path_str.len() - 1];
            let other_file_path = file_dir.join(Path::new(other_file_path_str));

            let other_tokens = read_file_tokens(lexer, &other_file_path)?;

            tokens.splice(i..i + 3, other_tokens);
        }
    }

    Ok(tokens)
}

/// An enum containing only the "basic" operations which can be performed by
/// an openqasm program. This is useful because unlike the [Operation] enum, this
/// doesn't contain custom gates which are more difficult to handle.
pub enum BasicOp {
    /// The general unary gate supported by openqasm
    U(f32, f32, f32, Qubit),

    /// The binary gate supported by openqasm, also called cnot
    CX(Qubit, Qubit),

    /// Operation for measuring a qubit
    Measure(Qubit, Cbit),

    /// Reset a qubit to its zero state
    ResetQ(Qubit),

    /// Reset a cbit to its zero state
    ResetC(Cbit),
}

impl OpenQASMProgram {
    /// Retrieves a list of [BasicOp]s from an [OpenQASMProgram] so that the operations can
    /// be for example perfomed by a simulator.
    ///
    /// Each [BasicOp] can optionally be paired with a [Condition], if the operation was applied
    /// in an 'if' statement.
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

    /// Retrieves a vector of [BasicOp]s from an [Operation]. These are similar datastructures
    /// except [BasicOp] doesn't contain a [Custom](Operation::Custom) variant. If the [Operation] is a custom gate
    /// then it is converted to a list of several basic gates. Otherwise the single element list
    /// containing the [BasicOp] is returned.
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

    /// Converts a [GateOperation] into an [Operation].
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
