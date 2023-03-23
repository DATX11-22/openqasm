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
    Opaque,
    QOp(QOp),
    If(Identifier, Integer, QOp),
    Barrier(AnyList),
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
                Token::parse(tokens, Token::CloseCurly)?;
                Some(Statement::GateDecl(gatedecl, goplist))
            },
            |tokens| {
                let gatedecl = GateDecl::parse(tokens)?;
                Token::parse(tokens, Token::CloseCurly)?;
                Some(Statement::GateDeclEmpty(gatedecl))
            },
            |tokens| {
                Token::parse(tokens, Token::Opaque)?;
                Identifier::parse(tokens)?;
                IdList::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(Statement::Opaque)
            },
            |tokens| {
                Token::parse(tokens, Token::Opaque)?;
                Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenParen)?;
                Token::parse(tokens, Token::CloseParen)?;
                IdList::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(Statement::Opaque)
            },
            |tokens| {
                Token::parse(tokens, Token::Opaque)?;
                Identifier::parse(tokens)?;
                Token::parse(tokens, Token::OpenParen)?;
                IdList::parse(tokens)?;
                Token::parse(tokens, Token::CloseParen)?;
                IdList::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(Statement::Opaque)
            },
            |tokens| {
                let qop = QOp::parse(tokens)?;
                Some(Statement::QOp(qop))
            },
            |tokens| {
                Token::parse(tokens, Token::If)?;
                Token::parse(tokens, Token::OpenParen)?;
                let creg = Identifier::parse(tokens)?;
                Token::parse(tokens, Token::Equal)?;
                let int = Integer::parse(tokens)?;
                Token::parse(tokens, Token::CloseParen)?;
                let qop = QOp::parse(tokens)?;
                Some(Statement::If(creg, int, qop))
            },
            |tokens| {
                Token::parse(tokens, Token::Barrier)?;
                let anylist = AnyList::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(Statement::Barrier(anylist))
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
    Barrier(IdList),
    GopListUOp(UOp, Box<GopList>),
    GopListBarrier(IdList, Box<GopList>),
}

impl ASTNode<Token> for GopList {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let uop = UOp::parse(tokens)?;
                Some(GopList::UOp(uop))
            },
            |tokens| {
                Token::parse(tokens, Token::Barrier)?;
                let idlist = IdList::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                Some(GopList::Barrier(idlist))
            },
            |tokens| {
                let uop = UOp::parse(tokens)?;
                let goplist = Box::new(GopList::parse(tokens)?);
                Some(GopList::GopListUOp(uop, goplist))
            },
            |tokens| {
                Token::parse(tokens, Token::Barrier)?;
                let idlist = IdList::parse(tokens)?;
                Token::parse(tokens, Token::Semicolon)?;
                let goplist = Box::new(GopList::parse(tokens)?);
                Some(GopList::GopListBarrier(idlist, goplist))
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

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
pub enum Exp1 {
    Number(Number),
    Integer(Integer),
    Pi,
    Identifier(Identifier),
    Paren(Exp),
    UnaryOp(UnaryOp, Exp),
    Neg(Exp),
}

#[derive(Clone)]
pub enum Exp2 {
    Pow(Exp1, Box<Exp2>),
    Exp1(Exp1),
}

#[derive(Clone)]
pub enum Exp3 {
    Mul(Exp2, Box<Exp3>),
    Div(Exp2, Box<Exp3>),
    Exp2(Exp2),
}

#[derive(Clone)]
pub enum Exp4 {
    Add(Exp3, Box<Exp4>),
    Sub(Exp3, Box<Exp4>),
    Exp3(Exp3),
}

#[derive(Clone)]
pub struct Exp(pub Box<Exp4>);

impl ASTNode<Token> for Exp1 {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let num = Number::parse(tokens)?;
                Some(Exp1::Number(num))
            },
            |tokens| {
                let int = Integer::parse(tokens)?;
                Some(Exp1::Integer(int))
            },
            |tokens| {
                Token::parse(tokens, Token::Pi)?;
                Some(Exp1::Pi)
            },
            |tokens| {
                let id = Identifier::parse(tokens)?;
                Some(Exp1::Identifier(id))
            },
            |tokens| {
                Token::parse(tokens, Token::OpenParen)?;
                let exp = Exp::parse(tokens)?;
                Token::parse(tokens, Token::CloseParen)?;
                Some(Exp1::Paren(exp))
            },
            |tokens| {
                let uop = UnaryOp::parse(tokens)?;
                Token::parse(tokens, Token::OpenParen)?;
                let exp = Exp::parse(tokens)?;
                Token::parse(tokens, Token::CloseParen)?;
                Some(Exp1::UnaryOp(uop, exp))
            },
            |tokens| {
                Token::parse(tokens, Token::Minus)?;
                let exp = Exp::parse(tokens)?;
                Some(Exp1::Neg(exp))
            },
        ]
    }
}

impl ASTNode<Token> for Exp2 {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let lhs = Exp1::parse(tokens)?;
                Token::parse(tokens, Token::Pow)?;
                let rhs = Box::new(Exp2::parse(tokens)?);
                Some(Exp2::Pow(lhs, rhs))
            },
            |tokens| {
                let exp1 = Exp1::parse(tokens)?;
                Some(Exp2::Exp1(exp1))
            },
        ]
    }
}

impl ASTNode<Token> for Exp3 {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let lhs = Exp2::parse(tokens)?;
                Token::parse(tokens, Token::Mul)?;
                let rhs = Box::new(Exp3::parse(tokens)?);
                Some(Exp3::Mul(lhs, rhs))
            },
            |tokens| {
                let lhs = Exp2::parse(tokens)?;
                Token::parse(tokens, Token::Div)?;
                let rhs = Box::new(Exp3::parse(tokens)?);
                Some(Exp3::Div(lhs, rhs))
            },
            |tokens| {
                let exp2 = Exp2::parse(tokens)?;
                Some(Exp3::Exp2(exp2))
            },
        ]
    }
}

impl ASTNode<Token> for Exp4 {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let lhs = Exp3::parse(tokens)?;
                Token::parse(tokens, Token::Plus)?;
                let rhs = Box::new(Exp4::parse(tokens)?);
                Some(Exp4::Add(lhs, rhs))
            },
            |tokens| {
                let lhs = Exp3::parse(tokens)?;
                Token::parse(tokens, Token::Minus)?;
                let rhs = Box::new(Exp4::parse(tokens)?);
                Some(Exp4::Sub(lhs, rhs))
            },
            |tokens| {
                let exp3 = Exp3::parse(tokens)?;
                Some(Exp4::Exp3(exp3))
            },
        ]
    }
}

impl ASTNodeSimple<Token> for Exp {
    fn parse_impl(tokens: &mut TokenIter<Token>) -> Option<Self> {
        let exp = Box::new(Exp4::parse(tokens)?);
        Some(Exp(exp))
    }
}
// |tokens| {
//     Token::parse(tokens, Token::Minus)?;
//     let rhs = Box::new(Exp::parse(tokens)?);
//     Some(Exp::Neg(rhs))
// },

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Sin,
    Cos,
    Tan,
    Exp,
    Ln,
    Sqrt,
}

impl ASTNode<Token> for UnaryOp {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                Token::parse(tokens, Token::Sin)?;
                Some(UnaryOp::Sin)
            },
            |tokens| {
                Token::parse(tokens, Token::Cos)?;
                Some(UnaryOp::Cos)
            },
            |tokens| {
                Token::parse(tokens, Token::Tan)?;
                Some(UnaryOp::Tan)
            },
            |tokens| {
                Token::parse(tokens, Token::Exp)?;
                Some(UnaryOp::Exp)
            },
            |tokens| {
                Token::parse(tokens, Token::Ln)?;
                Some(UnaryOp::Ln)
            },
            |tokens| {
                Token::parse(tokens, Token::Sqrt)?;
                Some(UnaryOp::Sqrt)
            },
        ]
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

#[derive(Clone)]
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
