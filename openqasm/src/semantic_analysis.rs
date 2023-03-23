mod ast_to_vec;
mod misc;

use compiler::ast::ast_node::{ToRefVec, ToVec};
use std::{cmp::Eq, collections::HashMap, hash::Hash};

use crate::ast::{
    AnyList, Argument, Decl, Exp, Exp1, Exp2, Exp3, Exp4, GateDecl, GopList, Identifier,
    MainProgram, QOp, Statement, UOp,
    UnaryOp::{Cos, Exp as Exponent, Ln, Sin, Sqrt, Tan},
};

use self::misc::{arg_id, arg_to_id, is_unique};

pub struct OpenQASMProgram {
    pub gates: HashMap<String, Gate>,
    pub qregs: HashMap<String, usize>,
    pub cregs: HashMap<String, usize>,
    pub operations: Vec<Operation>,
}

pub struct Gate {
    pub num_targets: usize,
    pub num_arguments: usize,
    pub operations: Vec<GateOperation>,
}

pub type Expression = dyn Fn(&Vec<f32>) -> f32;

pub enum GateOperation {
    U(Box<Expression>, Box<Expression>, Box<Expression>, usize),
    CX(usize, usize),
    Custom(String, Vec<Box<Expression>>, Vec<usize>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Qubit(String, usize);
#[derive(Clone, Debug)]
pub struct Cbit(String, usize);

#[derive(Debug)]
pub enum Operation {
    U(f32, f32, f32, Qubit),
    CX(Qubit, Qubit),
    Custom(String, Vec<f32>, Vec<Qubit>),
    Measure(Qubit, Cbit),
    ResetQ(Qubit),
    ResetC(Cbit),
}

#[derive(Debug)]
pub enum SemanticError {
    UnsupportedVersion,
    DuplicateRegisterDeclaration,
    WrongNumberOfArguments,
    WrongNumberOfTargets,
    UnknownIdentifier,
    IndexedRegisterInGateDecl,
    SameTargetUsedTwice,
    InvalidTargetDimensions,
    IndexOutOfBounds,
}

impl OpenQASMProgram {
    pub fn from_ast(ast_node: &MainProgram) -> Result<Self, SemanticError> {
        let mut openqasm_program = OpenQASMProgram {
            gates: HashMap::new(),
            qregs: HashMap::new(),
            cregs: HashMap::new(),
            operations: Vec::new(),
        };

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
                Statement::QOp(qop) => {
                    openqasm_program.create_qop(qop)?;
                }
            };
        }

        Ok(openqasm_program)
    }

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

    fn create_gate(
        &mut self,
        gatedecl: &GateDecl,
        goplist: Option<&GopList>,
    ) -> Result<(), SemanticError> {
        let (name, args, targets) = match gatedecl {
            GateDecl::NoArgList(name, targets) => (name, vec![], targets.to_ref_vec()),
            GateDecl::EmptyArgList(name, targets) => (name, vec![], targets.to_ref_vec()),
            GateDecl::WithArgList(name, args, targets) => {
                (name, args.to_ref_vec(), targets.to_ref_vec())
            }
        };

        let mut gate_ops = Vec::new();
        let goplist = goplist.map_or(vec![], |gl| gl.to_ref_vec());
        for gop in goplist {
            match gop {
                UOp::U(exps, target) => {
                    let exps = exps.to_ref_vec();
                    if exps.len() != 3 {
                        return Err(SemanticError::WrongNumberOfArguments);
                    }
                    let exp1 = create_exp(exps[0], &args)?;
                    let exp2 = create_exp(exps[1], &args)?;
                    let exp3 = create_exp(exps[2], &args)?;

                    let target = create_gate_targets(&vec![arg_to_id(target)?], &targets)?;

                    gate_ops.push(GateOperation::U(exp1, exp2, exp3, target[0]));
                }
                UOp::CX(target1, target2) => {
                    let op_targets = create_gate_targets(
                        &vec![arg_to_id(target1)?, arg_to_id(target2)?],
                        &targets,
                    )?;

                    gate_ops.push(GateOperation::CX(op_targets[0], op_targets[1]));
                }
                UOp::NoArgList(op_name, op_targets) => {
                    gate_ops.push(create_custom_gate_op(
                        &args,
                        op_name,
                        vec![],
                        op_targets,
                        &targets,
                        &self.gates,
                    )?);
                }
                UOp::EmptyArgList(op_name, op_targets) => {
                    gate_ops.push(create_custom_gate_op(
                        &args,
                        op_name,
                        vec![],
                        op_targets,
                        &targets,
                        &self.gates,
                    )?);
                }
                UOp::WithArgList(op_name, op_args, op_targets) => {
                    gate_ops.push(create_custom_gate_op(
                        &args,
                        op_name,
                        op_args.to_ref_vec(),
                        op_targets,
                        &targets,
                        &self.gates,
                    )?);
                }
            }
        }

        self.gates.insert(
            name.0.clone(),
            Gate {
                num_targets: targets.len(),
                num_arguments: args.len(),
                operations: gate_ops,
            },
        );
        Ok(())
    }

    fn create_qop(&mut self, qop: &QOp) -> Result<(), SemanticError> {
        match qop {
            QOp::UOp(uop) => self.create_uop(uop)?,
            QOp::Measure(qubit, cbit) => {
                let qbits = create_uop_targets(&vec![qubit.clone()], &self.qregs)?;
                let cbits = create_uop_targets(&vec![cbit.clone()], &self.cregs)?;
                if qbits.len() != cbits.len() {
                    return Err(SemanticError::InvalidTargetDimensions);
                }
                for (qubit, cbit) in qbits.iter().zip(cbits.iter()) {
                    let cbit = Cbit(cbit[0].0.clone(), cbit[0].1);
                    self.operations
                        .push(Operation::Measure(qubit[0].clone(), cbit));
                }
            }
            QOp::Reset(bit) => {
                let qubits = create_uop_targets(&vec![bit.clone()], &self.qregs);
                if let Ok(qubits) = qubits {
                    for qubits in qubits {
                        self.operations.push(Operation::ResetQ(qubits[0].clone()));
                    }
                } else {
                    let cbits = create_uop_targets(&vec![bit.clone()], &self.cregs)?;
                    for cbits in cbits {
                        let cbit = Cbit(cbits[0].0.clone(), cbits[0].1);
                        self.operations.push(Operation::ResetC(cbit));
                    }
                }
            }
        }
        Ok(())
    }

    fn create_uop(&mut self, uop: &UOp) -> Result<(), SemanticError> {
        match uop {
            UOp::U(exps, target) => {
                let exps: Result<Vec<_>, _> =
                    exps.to_ref_vec().iter().map(|e| exp_to_float(e)).collect();
                let exps = exps?;
                if exps.len() != 3 {
                    return Err(SemanticError::WrongNumberOfArguments);
                }
                let targets = create_uop_targets(&vec![target.clone()], &self.qregs)?;

                for targets in targets.iter() {
                    self.operations.push(Operation::U(
                        exps[0],
                        exps[1],
                        exps[2],
                        targets[0].clone(),
                    ));
                }
            }
            UOp::CX(target1, target2) => {
                let targets =
                    create_uop_targets(&vec![target1.clone(), target2.clone()], &self.qregs)?;

                for targets in targets.iter() {
                    self.operations
                        .push(Operation::CX(targets[0].clone(), targets[1].clone()));
                }
            }
            UOp::NoArgList(name, targets) => {
                let targets = create_uop_targets(&targets.to_vec(), &self.qregs)?;
                for targets in targets {
                    self.operations
                        .push(create_cusom_uop(name, vec![], targets, &self.gates)?);
                }
            }
            UOp::EmptyArgList(name, targets) => {
                let targets = create_uop_targets(&targets.to_vec(), &self.qregs)?;
                for targets in targets {
                    self.operations
                        .push(create_cusom_uop(name, vec![], targets, &self.gates)?);
                }
            }
            UOp::WithArgList(name, params, targets) => {
                let targets = create_uop_targets(&targets.to_vec(), &self.qregs)?;
                for targets in targets {
                    self.operations.push(create_cusom_uop(
                        name,
                        params.to_ref_vec(),
                        targets,
                        &self.gates,
                    )?);
                }
            }
        }

        Ok(())
    }
}

fn create_custom_gate_op(
    args: &Vec<&Identifier>,
    op_name: &Identifier,
    op_args: Vec<&Exp>,
    op_targets: &AnyList,
    target_names: &Vec<&Identifier>,
    gates: &HashMap<String, Gate>,
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

        if gate.num_arguments != op_args.len() {
            return Err(SemanticError::WrongNumberOfArguments);
        }
    } else {
        return Err(SemanticError::UnknownIdentifier);
    }

    let op_args: Result<Vec<_>, _> = op_args.iter().map(|a| create_exp(a, &args)).collect();
    let op_args = op_args?;
    let op_targets = create_gate_targets(&op_targets, target_names)?;

    Ok(GateOperation::Custom(op_name.clone(), op_args, op_targets))
}

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

fn create_exp(exp: &Exp, exps: &Vec<&Identifier>) -> Result<Box<Expression>, SemanticError> {
    create_exp4(exp.0.as_ref(), exps)
}

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

fn exp_to_float(exp: &Exp) -> Result<f32, SemanticError> {
    let exp_fn = create_exp(exp, &vec![])?;
    Ok(exp_fn(&vec![]))
}

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

fn create_cusom_uop(
    name: &Identifier,
    params: Vec<&Exp>,
    targets: Vec<Qubit>,
    gates: &HashMap<String, Gate>,
) -> Result<Operation, SemanticError> {
    let params: Result<Vec<_>, _> = params.to_vec().iter().map(|e| exp_to_float(e)).collect();
    let params = params?;

    let gate = gates.get(&name.0).ok_or(SemanticError::UnknownIdentifier)?;
    if params.len() != gate.num_arguments {
        return Err(SemanticError::WrongNumberOfArguments);
    }
    if targets.len() != gate.num_targets {
        return Err(SemanticError::WrongNumberOfTargets);
    }

    Ok(Operation::Custom(name.0.clone(), params, targets))
}
