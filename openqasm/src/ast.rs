pub mod ast_debug;

use crate::token::Token;
use compiler::ast::ast_node::{ASTNode, ASTNodeSimple, TokenIter};

impl Token {
    fn parse(tokens: &mut TokenIter<Token>, token: Token) -> Option<Token> {
        if let Some(t) = tokens.next() {
            if t.0 == token {
                return Some(t.0);
            }
        }
        None
    }
}

pub struct MainProgram(pub Number, pub Program);

impl ASTNodeSimple<Token> for MainProgram {
    fn parse_impl(tokens: &mut TokenIter<Token>) -> Option<Self> {
        Token::parse(tokens, Token::OPENQASM)?;
        let num = Number::parse(tokens)?;
        Token::parse(tokens, Token::Semicolon)?;
        let program = Program::parse(tokens)?;

        Some(MainProgram(num, program))
    }
}

pub enum Program {
    Multiple(Statement, Box<Program>),
    Single(Statement),
}

impl ASTNode<Token> for Program {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let statement = Statement::parse(tokens)?;
                let program = Box::new(Program::parse(tokens)?);
                Some(Program::Multiple(statement, program))
            },
            |tokens| {
                let statement = Statement::parse(tokens)?;
                Some(Program::Single(statement))
            },
        ]
    }
}

pub enum Statement {
    Decl(Decl),
    GateDecl(GateDecl, GopList),
    GateDeclEmpty(GateDecl),
    // Opaque
    QOp(QOp),
    // If,
    // Barrier
}

impl ASTNode<Token> for Statement {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let decl = Decl::parse(tokens)?;
                Some(Statement::Decl(decl))
            },
            |tokens| {
                let gatedecl = GateDecl::parse(tokens)?;
                let goplist = GopList::parse(tokens)?;
                Token::parse(tokens, Token::CloseCurly);
                Some(Statement::GateDecl(gatedecl, goplist))
            },
            |tokens| {
                let gatedecl = GateDecl::parse(tokens)?;
                Token::parse(tokens, Token::CloseCurly);
                Some(Statement::GateDeclEmpty(gatedecl))
            },
            |tokens| {
                let qop = QOp::parse(tokens)?;
                Some(Statement::QOp(qop))
            },
        ]
    }
}

pub enum Decl {
    QReg(Identifier, Integer),
    CReg(Identifier, Integer),
}

impl ASTNode<Token> for Decl {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                Token::parse(tokens, Token::QReg)?;
                let id = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenSquare)?;
                let size = Integer::parse(tokens)?;
                Token::parse(tokens, Token::CloseSquare)?;
                Token::parse(tokens, Token::Semicolon)?;

                Some(Decl::QReg(id, size))
            },
            |tokens| {
                Token::parse(tokens, Token::CReg)?;
                let id = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenSquare)?;
                let size = Integer::parse(tokens)?;
                Token::parse(tokens, Token::CloseSquare)?;
                Token::parse(tokens, Token::Semicolon)?;

                Some(Decl::CReg(id, size))
            },
        ]
    }
}

pub enum GateDecl {
    NoArgList(Identifier, IdList),
    EmptyArgList(Identifier, IdList),
    WithArgList(Identifier, IdList, IdList),
}

impl ASTNode<Token> for GateDecl {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                Token::parse(tokens, Token::Gate)?;
                let name = Identifier::parse(tokens)?;
                let targetlist = IdList::parse(tokens)?;
                Token::parse(tokens, Token::OpenCurly)?;
                Some(GateDecl::NoArgList(name, targetlist))
            },
            |tokens| {
                Token::parse(tokens, Token::Gate)?;
                let name = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenParen)?;
                Token::parse(tokens, Token::CloseParen)?;
                let targetlist = IdList::parse(tokens)?;
                Token::parse(tokens, Token::OpenCurly)?;
                Some(GateDecl::EmptyArgList(name, targetlist))
            },
            |tokens| {
                Token::parse(tokens, Token::Gate)?;
                let name = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenParen)?;
                let arglist = IdList::parse(tokens)?;
                Token::parse(tokens, Token::CloseParen)?;
                let targetlist = IdList::parse(tokens)?;
                Token::parse(tokens, Token::OpenCurly)?;
                Some(GateDecl::WithArgList(name, arglist, targetlist))
            },
        ]
    }
}

pub enum GopList {
    UOp(UOp),
    // Barrier
    GopList(UOp, Box<GopList>),
    // GopList barrier
}

impl ASTNode<Token> for GopList {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let uop = UOp::parse(tokens)?;
                let goplist = Box::new(GopList::parse(tokens)?);
                Some(GopList::GopList(uop, goplist))
            },
            |tokens| {
                let uop = UOp::parse(tokens)?;
                Some(GopList::UOp(uop))
            },
        ]
    }
}

pub enum QOp {
    UOp(UOp),
    Measure(Argument, Argument),
    Reset(Argument),
}

impl ASTNode<Token> for QOp {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let uop = UOp::parse(tokens)?;
                Some(QOp::UOp(uop))
            },
            |tokens| {
                Token::parse(tokens, Token::Measure)?;
                let source = Argument::parse(tokens)?;
                Token::parse(tokens, Token::Arrow)?;
                let dest = Argument::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(QOp::Measure(source, dest))
            },
            |tokens| {
                Token::parse(tokens, Token::Reset)?;
                let target = Argument::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(QOp::Reset(target))
            },
        ]
    }
}

pub enum UOp {
    U(ExpList, Argument),
    CX(Argument, Argument),
    NoArgList(Identifier, AnyList),
    EmptyArgList(Identifier, AnyList),
    WithArgList(Identifier, ExpList, AnyList),
}

impl ASTNode<Token> for UOp {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                Token::parse(tokens, Token::U)?;
                Token::parse(tokens, Token::OpenParen)?;
                let arglist = ExpList::parse(tokens)?;
                Token::parse(tokens, Token::CloseParen)?;
                let target = Argument::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(UOp::U(arglist, target))
            },
            |tokens| {
                Token::parse(tokens, Token::CX)?;
                let target1 = Argument::parse(tokens)?;
                Token::parse(tokens, Token::Comma)?;
                let target2 = Argument::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(UOp::CX(target1, target2))
            },
            |tokens| {
                let name = Identifier::parse(tokens)?;
                let targets = AnyList::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(UOp::NoArgList(name, targets))
            },
            |tokens| {
                let name = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenParen)?;
                Token::parse(tokens, Token::CloseParen)?;
                let targets = AnyList::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(UOp::EmptyArgList(name, targets))
            },
            |tokens| {
                let name = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenParen)?;
                let arguments = ExpList::parse(tokens)?;
                Token::parse(tokens, Token::CloseParen)?;
                let targets = AnyList::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(UOp::WithArgList(name, arguments, targets))
            },
        ]
    }
}

pub enum AnyList {
    IdList(IdList),
    MixedList(MixedList),
}

impl ASTNode<Token> for AnyList {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let idlist = IdList::parse(tokens)?;
                Some(AnyList::IdList(idlist))
            },
            |tokens| {
                let mixedlist = MixedList::parse(tokens)?;
                Some(AnyList::MixedList(mixedlist))
            },
        ]
    }
}

pub enum IdList {
    IdList(Identifier, Box<IdList>),
    Id(Identifier),
}

impl ASTNode<Token> for IdList {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let id = Identifier::parse(tokens)?;
                Some(IdList::Id(id))
            },
            |tokens| {
                let id = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::Comma)?;
                let idlist = Box::new(IdList::parse(tokens)?);
                Some(IdList::IdList(id, idlist))
            },
        ]
    }
}

pub enum MixedList {
    Indexed(Identifier, Integer),
    IdMixedList(Identifier, Box<MixedList>),
    IndexedMixedList(Identifier, Integer, Box<MixedList>),
    IndexedIdList(Identifier, Integer, IdList),
}

impl ASTNode<Token> for MixedList {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let id = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenSquare)?;
                let index = Integer::parse(tokens)?;
                Token::parse(tokens, Token::CloseSquare)?;
                Some(MixedList::Indexed(id, index))
            },
            |tokens| {
                let id = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::Comma)?;
                let mixedlist = Box::new(MixedList::parse(tokens)?);
                Some(MixedList::IdMixedList(id, mixedlist))
            },
            |tokens| {
                let id = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenSquare)?;
                let index = Integer::parse(tokens)?;
                Token::parse(tokens, Token::CloseSquare)?;
                Token::parse(tokens, Token::Comma)?;
                let mixedlist = Box::new(MixedList::parse(tokens)?);
                Some(MixedList::IndexedMixedList(id, index, mixedlist))
            },
            |tokens| {
                let id = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenSquare)?;
                let index = Integer::parse(tokens)?;
                Token::parse(tokens, Token::CloseSquare)?;
                Token::parse(tokens, Token::Comma)?;
                let idlist = IdList::parse(tokens)?;
                Some(MixedList::IndexedIdList(id, index, idlist))
            },
        ]
    }
}

#[derive(Clone)]
pub enum Argument {
    Id(Identifier),
    Indexed(Identifier, Integer),
}

impl ASTNode<Token> for Argument {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let id = Identifier::parse(tokens)?;
                Some(Argument::Id(id))
            },
            |tokens| {
                let id = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenSquare)?;
                let index = Integer::parse(tokens)?;
                Token::parse(tokens, Token::CloseSquare)?;
                Some(Argument::Indexed(id, index))
            },
        ]
    }
}

pub enum ExpList {
    ExpList(Exp, Box<ExpList>),
    Exp(Exp),
}

impl ASTNode<Token> for ExpList {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let exp = Exp::parse(tokens)?;
                Some(ExpList::Exp(exp))
            },
            |tokens| {
                let exp = Exp::parse(tokens)?;
                Token::parse(tokens, Token::Comma)?;
                let explist = Box::new(ExpList::parse(tokens)?);
                Some(ExpList::ExpList(exp, explist))
            },
        ]
    }
}

pub enum Exp {
    Number(Number),
    Integer(Integer),
    // pi,
    Identifier(Identifier),
    //+, -, *, /, -, ^, (), unaryop
}

impl ASTNode<Token> for Exp {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let num = Number::parse(tokens)?;
                Some(Exp::Number(num))
            },
            |tokens| {
                let int = Integer::parse(tokens)?;
                Some(Exp::Integer(int))
            },
            |tokens| {
                let id = Identifier::parse(tokens)?;
                Some(Exp::Identifier(id))
            },
        ]
    }
}

pub enum UnaryOp {}

impl ASTNode<Token> for UnaryOp {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        todo!()
    }
}

#[derive(Clone)]
pub struct Identifier(pub String);

impl ASTNodeSimple<Token> for Identifier {
    fn parse_impl(tokens: &mut TokenIter<Token>) -> Option<Self> {
        if let Some((Token::Identifier, s)) = tokens.next() {
            return Some(Identifier(s.clone()));
        }
        None
    }
}

pub struct Number(pub String);

impl ASTNodeSimple<Token> for Number {
    fn parse_impl(tokens: &mut TokenIter<Token>) -> Option<Self> {
        if let Some((Token::Number, s)) = tokens.next() {
            return Some(Number(s.clone()));
        }
        None
    }
}

#[derive(Clone)]
pub struct Integer(pub u32);

impl ASTNodeSimple<Token> for Integer {
    fn parse_impl(tokens: &mut TokenIter<Token>) -> Option<Self> {
        if let Some((Token::Int, s)) = tokens.next() {
            return Some(Integer(s.parse().ok()?));
        }
        None
    }
}
