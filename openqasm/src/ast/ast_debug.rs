use compiler::ast::ast_debug::ASTDebug;
use crate::ast::{MainProgram, Program, Statement, Decl, Identifier, Number, Integer};

impl ASTDebug for MainProgram {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        vec![&self.0, &self.1]
    }

    fn name(&self) -> String {
        "Main Program".to_string()
    }
}

impl ASTDebug for Program {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            Program::Multiple(statement, program) => vec![statement, program.as_ref()],
            Program::Single(statement) => vec![statement],
        }
    }

    fn name(&self) -> String {
        "Program".to_string()
    }
}

impl ASTDebug for Statement {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            Statement::Decl(decl) => vec![decl],
            Statement::GateDecl(_, _) => todo!(),
            Statement::GateDeclEmpty(_) => todo!(),
            Statement::QOp(_) => todo!(),
        }
    }

    fn name(&self) -> String {
        match self {
            Statement::Decl(_) => "Decl".to_string(),
            Statement::GateDecl(_, _) => todo!(),
            Statement::GateDeclEmpty(_) => todo!(),
            Statement::QOp(_) => todo!(),
        }
    }
}

impl ASTDebug for Decl {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            Decl::QReg(id, size) => vec![id, size],
            Decl::CReg(id, size) => vec![id, size],
        }
    }

    fn name(&self) -> String {
        match self {
            Decl::QReg(_, _) => "QReg".to_string(),
            Decl::CReg(_, _) => "CReg".to_string(),
        }
    }
}

impl ASTDebug for Identifier {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        vec![]
    }

    fn name(&self) -> String {
        format!("Identifier: {}", self.0)
    }
}

impl ASTDebug for Number {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        vec![]
    }

    fn name(&self) -> String {
        format!("Number: {}", self.0)
    }
}

impl ASTDebug for Integer {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        vec![]
    }

    fn name(&self) -> String {
        format!("Integer: {}", self.0)
    }
}

