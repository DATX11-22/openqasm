use super::{
    AnyList, Argument, Decl, Exp, ExpList, GateDecl, GopList, IdList, Identifier, Integer,
    MainProgram, MixedList, Number, Program, QOp, Statement, UOp, UnaryOp,
};
use compiler::ast::ast_debug::ASTDebug;

impl ASTDebug for MainProgram {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        vec![&self.0, &self.1]
    }

    fn name(&self) -> Option<String> {
        Some("Main Program".to_string())
    }
}

impl ASTDebug for Program {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            Program::Multiple(statement, program) => vec![statement, program.as_ref()],
            Program::Single(statement) => vec![statement],
        }
    }
}

impl ASTDebug for Statement {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            Statement::Decl(decl) => vec![decl],
            Statement::GateDecl(gatedecl, goplist) => vec![gatedecl, goplist],
            Statement::GateDeclEmpty(gatedecl) => vec![gatedecl],
            Statement::QOp(qop) => vec![qop],
        }
    }

    fn name(&self) -> Option<String> {
        Some("Statement".to_string())
    }
}

impl ASTDebug for Decl {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            Decl::QReg(id, size) => vec![id, size],
            Decl::CReg(id, size) => vec![id, size],
        }
    }

    fn name(&self) -> Option<String> {
        match self {
            Decl::QReg(_, _) => Some("QReg".to_string()),
            Decl::CReg(_, _) => Some("CReg".to_string()),
        }
    }
}

impl ASTDebug for GateDecl {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            GateDecl::NoArgList(name, targets) => vec![name, targets],
            GateDecl::EmptyArgList(name, targets) => vec![name, targets],
            GateDecl::WithArgList(name, args, targets) => vec![name, args, targets],
        }
    }

    fn name(&self) -> Option<String> {
        Some("GateDecl".to_string())
    }
}

impl ASTDebug for GopList {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            GopList::UOp(uop) => vec![uop],
            GopList::GopList(uop, goplist) => vec![uop, goplist.as_ref()],
        }
    }

    fn name(&self) -> Option<String> {
        Some("Goplist".to_string())
    }
}

impl ASTDebug for QOp {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            QOp::UOp(uop) => vec![uop],
            QOp::Measure(qarg, carg) => vec![qarg, carg],
            QOp::Reset(qarg) => vec![qarg],
        }
    }

    fn name(&self) -> Option<String> {
        match self {
            QOp::UOp(_) => None,
            QOp::Measure(_, _) => Some("measure".to_string()),
            QOp::Reset(_) => Some("reset".to_string()),
        }
    }
}

impl ASTDebug for UOp {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            UOp::U(explist, arg) => vec![explist, arg],
            UOp::CX(arg1, arg2) => vec![arg1, arg2],
            UOp::NoArgList(name, targets) => vec![name, targets],
            UOp::EmptyArgList(name, targets) => vec![name, targets],
            UOp::WithArgList(name, explist, targets) => vec![name, explist, targets],
        }
    }

    fn name(&self) -> Option<String> {
        match self {
            UOp::U(_, _) => Some("U".to_string()),
            UOp::CX(_, _) => Some("CX".to_string()),
            _ => Some("UOp".to_string()),
        }
    }
}

impl ASTDebug for AnyList {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            AnyList::IdList(idlist) => vec![idlist],
            AnyList::MixedList(mixedlist) => vec![mixedlist],
        }
    }
}

impl ASTDebug for IdList {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            IdList::IdList(id, idlist) => vec![id, idlist.as_ref()],
            IdList::Id(id) => vec![id],
        }
    }

    fn name(&self) -> Option<String> {
        Some("Idlist".to_string())
    }
}

impl ASTDebug for MixedList {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            MixedList::Indexed(id, index) => vec![id, index],
            MixedList::IdMixedList(id, mixedlist) => vec![id, mixedlist.as_ref()],
            MixedList::IndexedMixedList(id, index, mixedlist) => {
                vec![id, index, mixedlist.as_ref()]
            }
            MixedList::IndexedIdList(id, index, idlist) => vec![id, index, idlist],
        }
    }

    fn name(&self) -> Option<String> {
        Some("Mixedlist".to_string())
    }
}

impl ASTDebug for Argument {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            Argument::Id(id) => vec![id],
            Argument::Indexed(id, index) => vec![id, index],
        }
    }

    fn name(&self) -> Option<String> {
        Some("Argument".to_string())
    }
}

impl ASTDebug for ExpList {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            ExpList::ExpList(exp, explist) => vec![exp, explist.as_ref()],
            ExpList::Exp(exp) => vec![exp],
        }
    }

    fn name(&self) -> Option<String> {
        Some("Explist".to_string())
    }
}

impl ASTDebug for Exp {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        match self {
            Exp::Number(num) => vec![num],
            Exp::Integer(int) => vec![int],
            Exp::Identifier(id) => vec![id],
        }
    }
}

impl ASTDebug for UnaryOp {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        vec![]
    }
}

impl ASTDebug for Identifier {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        vec![]
    }

    fn name(&self) -> Option<String> {
        Some(format!("Identifier: {}", self.0))
    }
}

impl ASTDebug for Number {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        vec![]
    }

    fn name(&self) -> Option<String> {
        Some(format!("Number: {}", self.0))
    }
}

impl ASTDebug for Integer {
    fn chidren(&self) -> Vec<&dyn ASTDebug> {
        vec![]
    }

    fn name(&self) -> Option<String> {
        Some(format!("Integer: {}", self.0))
    }
}
