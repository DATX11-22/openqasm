//! The code which performs semantic analyzis on an AST according to
//! the openqasm 2.0 specification.

mod misc;

use self::misc::{arg_id, arg_to_id, is_unique};
use crate::openqasm::ast::{
    ast_to_vec::UOpOrBarrier,
    AnyList, Argument, Decl, Exp, Exp1, Exp2, Exp3, Exp4, GateDecl, GopList, Identifier,
    MainProgram, QOp, Statement, UOp,
    UnaryOp::{Cos, Exp as Exponent, Ln, Sin, Sqrt, Tan},
};
use crate::parser::ast::ast_node::{ToRefVec, ToVec};
use std::{cmp::Eq, collections::HashMap, hash::Hash};

/// The type returned after performing semantic analysis on an AST.
///
/// It contains the quantum and classical registers and the gates defined
/// in the program. I also contains a list of the [Operation]s to perform in
/// order. The [Operation]s are optionally paired with a [Condition] for running.
/// the operation.
pub struct OpenQASMProgram {
    /// The custom gates defined in the openqasm program. Maps the gate names
    /// to [Gate]s
    pub gates: HashMap<String, Gate>,

    /// The quantum registers defined in the openqasm program. Maps the register names
    /// to the size of the register.
    pub qregs: HashMap<String, usize>,

    /// The classical registers defined in the openqasm program. Maps the register names
    /// to the size of the register.
    pub cregs: HashMap<String, usize>,

    /// The [Operation]s to execute when running the openqasm program. The operations are
    /// optionally paired with a [Condition] which controls whether to run the operation or not.
    pub operations: Vec<(Option<Condition>, Operation)>,
}

/// Describes a custom gate defined in openqasm
pub struct Gate {
    /// How many qubits the gate operates on
    pub num_targets: usize,

    /// How many parameters the gate requires
    pub num_arguments: usize,

    /// The list of [GateOperation]s to apply when running the gate in order of execution.
    pub operations: Vec<GateOperation>,
}

/// When defining a gate they can take parameter variables. These variables can then
/// be used in the gate definition to create an expression which can be evaluated into
/// a number. However when defining the gate the parameter variables do not have a specified
/// value so [GateOperation]s can not simply store [f32] as parameters, they need to contain
/// functions which along with some values for the parameter variables produce a [f32].
pub type Expression = dyn Fn(&Vec<f32>) -> f32;

/// Operations which can be used inside of a gate declaration in openqasm. This is a subset of [Operation].
/// Unlike [Operation], however, they contain [Expression]s instead of [f32] (for reasons described in the [Expression]
/// documentation) and [usize] instead of [Qubit]. The [usize] arguments specify which qubit to use from the
/// gates argument list, starting from the first one at index zero.
pub enum GateOperation {
    U(Box<Expression>, Box<Expression>, Box<Expression>, usize),
    CX(usize, usize),
    Custom(String, Vec<Box<Expression>>, Vec<usize>),
}

/// A qubit defined in an openqasm program. Contains the name of the register containing
/// the qubit, as well as the index of the qubit in the register.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Qubit(pub String, pub usize);

/// A classical bit defined in an openqasm program. Contains the name of the register containing
/// the bit, as well as the index of the bit in the register.
#[derive(Clone, Debug)]
pub struct Cbit(pub String, pub usize);

/// A condition that determines whether to perform an [Operation] or not. The first parameter
/// is the name to a classical register. The second parameter is a value to compare the register
/// to when interpreting the register as a binary number with index 0 as the least significan bit.
/// When the register is equal to the second parameter the operation should be applied.
#[derive(Debug, Clone)]
pub struct Condition(pub String, pub u32);

/// An operation to run when running an openqasm program.
#[derive(Debug)]
pub enum Operation {
    /// The general unary gate supported by openqasm. Takes 3 parameters an a qubit target.
    U(f32, f32, f32, Qubit),

    /// The CX/CNot gate. The first argument is the control bit, and the second is the target.
    CX(Qubit, Qubit),

    /// A custom gate defined in the openqasm program. Contains the name of the gate, the parameters
    /// used when calling the gate, and the qubit targets. When returned from [OpenQASMProgram::from_ast]
    /// it is guaranteed that there is a gate matching the name given in the first field. This gate
    /// is also guaranteed to take as many parameters and arguments as is stored in this datatype.
    Custom(String, Vec<f32>, Vec<Qubit>),

    /// Operation for measuring a qubit onto a classical bit.
    Measure(Qubit, Cbit),

    /// Operation for reseting a qubit to its zero state |0>.
    ResetQ(Qubit),

    /// Operation for reseting a classical bit to its zero state 0.
    ResetC(Cbit),
}

/// The different errors that can occur when running semantic analysis on an AST.
#[derive(Debug)]
pub enum SemanticError {
    /// Only openqasm 2.0 is supported, so the first line has to be: OPENQASM 2.0;
    UnsupportedVersion,

    /// Two registers have been defined with the same name.
    DuplicateRegisterDeclaration,

    /// An operation was applied with the wrong number of parameters.
    WrongNumberOfParameters,

    /// An operation was applied with the wrong number of qubits.
    WrongNumberOfTargets,

    /// An identifier was used which had not been previously defined.
    UnknownIdentifier,

    /// Inside of a gate declaration you are only allowed to use the parameters and
    /// arguments defined in the gate declaration. The arguments are not allowed to be
    /// indexed inside the gate definition, otherwise this error is returned.
    IndexedRegisterInGateDecl,

    /// An operation was applied to some qubits which contained the same qubit several times.
    SameTargetUsedTwice,

    /// An operation can be called on entire registers or on single qubits. When more than one
    /// of the arguments specify an entire register then all these registers have to have the same
    /// size. Otherwise this error is returned.
    InvalidTargetDimensions,

    /// A register was indexed outside of its range [0..size)
    IndexOutOfBounds,

    /// An opaque gate is one which doesn't have a definition. This is currently not supported.
    OpaqueIsNotSupported,
}

impl OpenQASMProgram {
    /// The main function for running semantic analysis on an AST.
    pub fn from_ast(ast_node: &MainProgram) -> Result<Self, SemanticError> {
        let mut openqasm_program = OpenQASMProgram {
            gates: HashMap::new(),
            qregs: HashMap::new(),
            cregs: HashMap::new(),
            operations: Vec::new(),
        };

        // Only openqasm version 2.0 is supported
        if ast_node.0 .0 != "2.0" {
            return Err(SemanticError::UnsupportedVersion);
        }

        for statement in ast_node.1.to_ref_vec() {
            match statement {
                Statement::Decl(decl) => openqasm_program.create_reg(decl)?,
                Statement::GateDecl(gatedecl, goplist) => {
                    openqasm_program.create_gate(gatedecl, Some(goplist))?
                }
                Statement::GateDeclEmpty(gatedecl) => {
                    openqasm_program.create_gate(gatedecl, None)?
                }
                Statement::Opaque => return Err(SemanticError::OpaqueIsNotSupported),
                Statement::QOp(qop) => {
                    openqasm_program.create_qop(qop, None)?;
                }
                Statement::If(id, int, qop) => {
                    if !openqasm_program.cregs.contains_key(&id.0) {
                        return Err(SemanticError::UnknownIdentifier);
                    }
                    let condition = Some(Condition(id.0.clone(), int.0));
                    openqasm_program.create_qop(qop, condition)?;
                }
                Statement::Barrier(_) => {}
            };
        }

        Ok(openqasm_program)
    }

    /// Validates a register declaration and adds the new register to self.qregs or
    /// self.cregs if the declaration was valid.
    fn create_reg(&mut self, decl: &Decl) -> Result<(), SemanticError> {
        let res = match decl {
            Decl::QReg(id, size) => self.qregs.insert(id.0.clone(), size.0 as usize),
            Decl::CReg(id, size) => self.cregs.insert(id.0.clone(), size.0 as usize),
        };
        if res.is_some() {
            return Err(SemanticError::DuplicateRegisterDeclaration);
        }
        Ok(())
    }

    /// Validates a gate declaration and adds the new gate along with its associated operations
    /// to self.gates.
    fn create_gate(
        &mut self,
        gatedecl: &GateDecl,
        goplist: Option<&GopList>,
    ) -> Result<(), SemanticError> {
        // Get the name params and targets
        let (name, params, targets) = match gatedecl {
            GateDecl::NoArgList(name, targets) => (name, vec![], targets.to_ref_vec()),
            GateDecl::EmptyArgList(name, targets) => (name, vec![], targets.to_ref_vec()),
            GateDecl::WithArgList(name, params, targets) => {
                (name, params.to_ref_vec(), targets.to_ref_vec())
            }
        };

        // Get the gate operations
        let mut gate_ops = Vec::new();
        let goplist = goplist.map_or(vec![], |gl| gl.to_vec());
        for gop in goplist {
            match gop {
                UOpOrBarrier::UOp(UOp::U(exps, target)) => {
                    let exps = exps.to_ref_vec();
                    if exps.len() != 3 {
                        return Err(SemanticError::WrongNumberOfParameters);
                    }
                    let exp1 = create_exp(exps[0], &params)?;
                    let exp2 = create_exp(exps[1], &params)?;
                    let exp3 = create_exp(exps[2], &params)?;

                    let target = create_gate_targets(&vec![arg_to_id(&target)?], &targets)?;

                    gate_ops.push(GateOperation::U(exp1, exp2, exp3, target[0]));
                }
                UOpOrBarrier::UOp(UOp::CX(target1, target2)) => {
                    let op_targets = create_gate_targets(
                        &vec![arg_to_id(&target1)?, arg_to_id(&target2)?],
                        &targets,
                    )?;

                    gate_ops.push(GateOperation::CX(op_targets[0], op_targets[1]));
                }
                UOpOrBarrier::UOp(UOp::NoArgList(op_name, op_targets)) => {
                    gate_ops.push(create_custom_gate_op(
                        &params,
                        &op_name,
                        vec![],
                        &op_targets,
                        &targets,
                        &self.gates,
                    )?);
                }
                UOpOrBarrier::UOp(UOp::EmptyArgList(op_name, op_targets)) => {
                    gate_ops.push(create_custom_gate_op(
                        &params,
                        &op_name,
                        vec![],
                        &op_targets,
                        &targets,
                        &self.gates,
                    )?);
                }
                UOpOrBarrier::UOp(UOp::WithArgList(op_name, op_args, op_targets)) => {
                    gate_ops.push(create_custom_gate_op(
                        &params,
                        &op_name,
                        op_args.to_ref_vec(),
                        &op_targets,
                        &targets,
                        &self.gates,
                    )?);
                }
                UOpOrBarrier::Barrier(_) => {} // Barriers are ignored (for now atleast)
            }
        }

        self.gates.insert(
            name.0.clone(),
            Gate {
                num_targets: targets.len(),
                num_arguments: params.len(),
                operations: gate_ops,
            },
        );
        Ok(())
    }

    /// Validates a quantum operation AST node and an optional condition and adds a new [Operation] to
    /// self.operations if they are valid.
    fn create_qop(&mut self, qop: &QOp, condition: Option<Condition>) -> Result<(), SemanticError> {
        match qop {
            QOp::UOp(uop) => self.create_uop(uop, condition)?,
            QOp::Measure(qubit, cbit) => {
                let qbits = create_uop_targets(&vec![qubit.clone()], &self.qregs)?;
                let cbits = create_uop_targets(&vec![cbit.clone()], &self.cregs)?;
                if qbits.len() != cbits.len() {
                    return Err(SemanticError::InvalidTargetDimensions);
                }
                for (qubit, cbit) in qbits.iter().zip(cbits.iter()) {
                    let cbit = Cbit(cbit[0].0.clone(), cbit[0].1);
                    self.operations.push((
                        condition.clone(),
                        Operation::Measure(qubit[0].clone(), cbit),
                    ));
                }
            }
            QOp::Reset(bit) => {
                let qubits = create_uop_targets(&vec![bit.clone()], &self.qregs);
                if let Ok(qubits) = qubits {
                    for qubits in qubits {
                        self.operations
                            .push((condition.clone(), Operation::ResetQ(qubits[0].clone())));
                    }
                } else {
                    let cbits = create_uop_targets(&vec![bit.clone()], &self.cregs)?;
                    for cbits in cbits {
                        let cbit = Cbit(cbits[0].0.clone(), cbits[0].1);
                        self.operations
                            .push((condition.clone(), Operation::ResetC(cbit)));
                    }
                }
            }
        }
        Ok(())
    }

    /// Used by [create_qop](OpenQASMProgram::create_qop) to handle gate applications.
    fn create_uop(&mut self, uop: &UOp, condition: Option<Condition>) -> Result<(), SemanticError> {
        match uop {
            UOp::U(exps, target) => {
                let exps: Result<Vec<_>, _> =
                    exps.to_ref_vec().iter().map(|e| exp_to_float(e)).collect();
                let exps = exps?;
                if exps.len() != 3 {
                    return Err(SemanticError::WrongNumberOfParameters);
                }
                let targets = create_uop_targets(&vec![target.clone()], &self.qregs)?;

                for targets in targets.iter() {
                    self.operations.push((
                        condition.clone(),
                        Operation::U(exps[0], exps[1], exps[2], targets[0].clone()),
                    ));
                }
            }
            UOp::CX(target1, target2) => {
                let targets =
                    create_uop_targets(&vec![target1.clone(), target2.clone()], &self.qregs)?;

                for targets in targets.iter() {
                    self.operations.push((
                        condition.clone(),
                        Operation::CX(targets[0].clone(), targets[1].clone()),
                    ));
                }
            }
            UOp::NoArgList(name, targets) => {
                let targets = create_uop_targets(&targets.to_vec(), &self.qregs)?;
                for targets in targets {
                    self.operations.push((
                        condition.clone(),
                        create_cusom_uop(name, vec![], targets, &self.gates)?,
                    ));
                }
            }
            UOp::EmptyArgList(name, targets) => {
                let targets = create_uop_targets(&targets.to_vec(), &self.qregs)?;
                for targets in targets {
                    self.operations.push((
                        condition.clone(),
                        create_cusom_uop(name, vec![], targets, &self.gates)?,
                    ));
                }
            }
            UOp::WithArgList(name, params, targets) => {
                let targets = create_uop_targets(&targets.to_vec(), &self.qregs)?;
                for targets in targets {
                    self.operations.push((
                        condition.clone(),
                        create_cusom_uop(name, params.to_ref_vec(), targets, &self.gates)?,
                    ));
                }
            }
        }

        Ok(())
    }
}

/// Used by [OpenQASMProgram::create_gate] to validate and create a custom gate.
fn create_custom_gate_op(
    params: &Vec<&Identifier>, // The parameters defined in the gate definition.
    op_name: &Identifier,      // The name of the gate operation.
    op_params: Vec<&Exp>,      // The expressions passed in as parameters to the gate operation.
    op_targets: &AnyList,      // The arguments applied to the gate operation.
    target_names: &Vec<&Identifier>, // The arguments defined in the gate definition.
    gates: &HashMap<String, Gate>, // All the previously defined gates.
) -> Result<GateOperation, SemanticError> {
    let op_name = &op_name.0;
    let op_targets = match op_targets {
        AnyList::IdList(op_targets) => op_targets.to_ref_vec(),
        AnyList::MixedList(_) => Err(SemanticError::IndexedRegisterInGateDecl)?,
    };

    if let Some(gate) = gates.get(op_name) {
        if gate.num_targets != op_targets.len() {
            return Err(SemanticError::WrongNumberOfTargets);
        }

        if gate.num_arguments != op_params.len() {
            return Err(SemanticError::WrongNumberOfParameters);
        }
    } else {
        return Err(SemanticError::UnknownIdentifier);
    }

    let op_params: Result<Vec<_>, _> = op_params.iter().map(|a| create_exp(a, &params)).collect();
    let op_params = op_params?;
    let op_targets = create_gate_targets(&op_targets, target_names)?;

    Ok(GateOperation::Custom(
        op_name.clone(),
        op_params,
        op_targets,
    ))
}

/// Creates an expression of priority 1.
fn create_exp1(exp: &Exp1, exps: &Vec<&Identifier>) -> Result<Box<Expression>, SemanticError> {
    match exp {
        Exp1::Number(num) => {
            let f = num.0.parse::<f32>().unwrap();
            Ok(Box::new(move |_| f))
        }
        Exp1::Integer(int) => {
            let i = int.0 as f32;
            Ok(Box::new(move |_| i))
        }
        Exp1::Identifier(id) => {
            let search = exps.iter().position(|&arg| arg.0 == id.0);
            if let Some(i) = search {
                Ok(Box::new(move |args| args[i]))
            } else {
                Err(SemanticError::UnknownIdentifier)
            }
        }
        Exp1::Pi => Ok(Box::new(|_| std::f32::consts::PI)),
        Exp1::Paren(exp) => create_exp(exp, exps),
        Exp1::UnaryOp(uop, exp) => {
            let exp = create_exp(exp, exps)?;
            match uop {
                Sin => Ok(Box::new(move |args| f32::sin(exp(args)))),
                Cos => Ok(Box::new(move |args| f32::cos(exp(args)))),
                Tan => Ok(Box::new(move |args| f32::tan(exp(args)))),
                Exponent => Ok(Box::new(move |args| f32::exp(exp(args)))),
                Ln => Ok(Box::new(move |args| f32::ln(exp(args)))),
                Sqrt => Ok(Box::new(move |args| f32::sqrt(exp(args)))),
            }
        }
        Exp1::Neg(exp) => {
            let exp = create_exp(exp, exps)?;
            Ok(Box::new(move |args| -exp(args)))
        }
    }
}

/// Creates an expression of priority 2.
fn create_exp2(exp: &Exp2, exps: &Vec<&Identifier>) -> Result<Box<Expression>, SemanticError> {
    match exp {
        Exp2::Pow(lhs, rhs) => {
            let lhs = create_exp1(lhs, exps)?;
            let rhs = create_exp2(rhs, exps)?;
            Ok(Box::new(move |args| lhs(args).powf(rhs(args))))
        }
        Exp2::Exp1(exp1) => create_exp1(exp1, exps),
    }
}

/// Creates an expression of priority 3.
fn create_exp3(exp: &Exp3, exps: &Vec<&Identifier>) -> Result<Box<Expression>, SemanticError> {
    match exp {
        Exp3::Mul(lhs, rhs) => {
            let lhs = create_exp2(lhs, exps)?;
            let rhs = create_exp3(rhs, exps)?;
            Ok(Box::new(move |args| lhs(args) * rhs(args)))
        }
        Exp3::Div(lhs, rhs) => {
            let lhs = create_exp2(lhs, exps)?;
            let rhs = create_exp3(rhs, exps)?;
            Ok(Box::new(move |args| lhs(args) / rhs(args)))
        }
        Exp3::Exp2(exp2) => create_exp2(exp2, exps),
    }
}

/// Creates an expression of priority 4.
fn create_exp4(exp: &Exp4, exps: &Vec<&Identifier>) -> Result<Box<Expression>, SemanticError> {
    match exp {
        Exp4::Add(lhs, rhs) => {
            let lhs = create_exp3(lhs, exps)?;
            let rhs = create_exp4(rhs, exps)?;
            Ok(Box::new(move |args| lhs(args) + rhs(args)))
        }
        Exp4::Sub(lhs, rhs) => {
            let lhs = create_exp3(lhs, exps)?;
            let rhs = create_exp4(rhs, exps)?;
            Ok(Box::new(move |args| lhs(args) - rhs(args)))
        }
        Exp4::Exp3(exp3) => create_exp3(exp3, exps),
    }
}

/// Creates an arithmetic expression which can contain some variables.
/// These expressions depend on the values of the variables, which is not known at time of
/// construction. Therefore, an [Expression] is returned which takes some values for
/// the variables and returns the value of the expression.
fn create_exp(exp: &Exp, exps: &Vec<&Identifier>) -> Result<Box<Expression>, SemanticError> {
    create_exp4(exp.0.as_ref(), exps)
}

/// Validates a list of targets which will be applied to a gate operation. These should be some
/// subset of the targets defined in the gate declaration.
fn create_gate_targets(
    targets: &Vec<&Identifier>,
    gate_targets: &Vec<&Identifier>,
) -> Result<Vec<usize>, SemanticError> {
    let res: Result<Vec<_>, _> = targets
        .iter()
        .map(|target| {
            gate_targets
                .iter()
                .position(|t| t.0 == target.0)
                .ok_or(SemanticError::UnknownIdentifier)
        })
        .collect();
    let res = res?;
    if is_unique(&res) {
        Ok(res)
    } else {
        Err(SemanticError::SameTargetUsedTwice)
    }
}

/// Converts an expression which doesn't contain variables into an [f32]. This is
/// used in operations outside of gate definitions, because here there are no variables
/// to use.
fn exp_to_float(exp: &Exp) -> Result<f32, SemanticError> {
    let exp_fn = create_exp(exp, &vec![])?;
    Ok(exp_fn(&vec![]))
}

/// Converts a list of targets which will be applied to an operations and converts it
/// into [Qubit]s which reference some previously defined register.
fn create_uop_targets(
    targets: &Vec<Argument>,
    regs: &HashMap<String, usize>,
) -> Result<Vec<Vec<Qubit>>, SemanticError> {
    // Check that all the target registers have the same size
    let mut arg_len = None;
    for target in targets.iter() {
        let target_len = regs
            .get(arg_id(target))
            .ok_or(SemanticError::UnknownIdentifier)?;

        match target {
            Argument::Id(_) => {
                if let Some(arg_len) = arg_len {
                    if arg_len != target_len {
                        return Err(SemanticError::InvalidTargetDimensions);
                    }
                } else {
                    arg_len = Some(target_len);
                }
            }
            Argument::Indexed(_, index) => {
                if index.0 as usize >= *target_len {
                    return Err(SemanticError::IndexOutOfBounds);
                }
            }
        }
    }
    let arg_len = *arg_len.unwrap_or(&1);

    let mut res = vec![];
    for i in 0..arg_len {
        let mut row = Vec::new();

        for target in targets.iter() {
            let qubit = match target {
                Argument::Id(id) => Qubit(id.0.clone(), i),
                Argument::Indexed(id, index) => Qubit(id.0.clone(), index.0 as usize),
            };

            row.push(qubit);
        }

        if !is_unique(&row) {
            return Err(SemanticError::SameTargetUsedTwice);
        }

        res.push(row);
    }

    Ok(res)
}

/// Creates an [Operation] which applies a custom gate.
fn create_cusom_uop(
    name: &Identifier,             // The name of the gate
    params: Vec<&Exp>, // The parameters. Should not contain any variables and can therefore be evaluated into f32s
    targets: Vec<Qubit>, // The qubits to apply the gate to
    gates: &HashMap<String, Gate>, // The previously defined gates
) -> Result<Operation, SemanticError> {
    let params: Result<Vec<_>, _> = params.to_vec().iter().map(|e| exp_to_float(e)).collect();
    let params = params?;

    let gate = gates.get(&name.0).ok_or(SemanticError::UnknownIdentifier)?;
    if params.len() != gate.num_arguments {
        return Err(SemanticError::WrongNumberOfParameters);
    }
    if targets.len() != gate.num_targets {
        return Err(SemanticError::WrongNumberOfTargets);
    }

    Ok(Operation::Custom(name.0.clone(), params, targets))
}
