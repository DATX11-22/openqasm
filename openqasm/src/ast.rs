pub mod ast_debug;

use std::slice::Iter;
use compiler::ast::ast_node::{ASTNodeSimple, ASTNode};
use crate::token::{Token, TokenMatch};

impl Token {
    fn parse(tokens: &mut Iter<TokenMatch>, token: Token) -> Option<Token> {
        if let Some(t) = tokens.next() {
            if t.0 == token {
                return Some(t.0);
            }
        }
        None
    }
}

pub struct MainProgram(Number, Program);

impl ASTNodeSimple<TokenMatch> for MainProgram {
    fn parse_impl(tokens: &mut Iter<TokenMatch>) -> Option<Self> {
        Token::parse(tokens, Token::OPENQASM)?;
        let num = Number::parse(tokens)?;
        Token::parse(tokens, Token::Semicolon)?;
        let program = Program::parse(tokens)?;

        Some(MainProgram(num, program))
    }
}

pub enum Program {
    Multiple(Statement, Box<Program>),
    Sigle(Statement),
}

impl ASTNode<TokenMatch> for Program {
    fn parse_impls() -> Vec<fn(&mut Iter<TokenMatch>) -> Option<Self>> {
        vec![
            |tokens| {
                let statement = Statement::parse(tokens)?;
                let program = Box::new(Program::parse(tokens)?);
                Some(Program::Multiple(statement, program))
            },
            |tokens| {
                let statement = Statement::parse(tokens)?;
                Some(Program::Sigle(statement))
            },
        ]
    }
}

pub enum Statement {
    Decl(Decl),
    GateDecl,
    GateDeclEmpty,
    // Opaque
    QOp,
    If,
    // Barrier
}

impl ASTNode<TokenMatch> for Statement {
    fn parse_impls() -> Vec<fn(&mut Iter<TokenMatch>) -> Option<Self>> {
        vec![|tokens| {
            let decl = Decl::parse(tokens)?;
            Some(Statement::Decl(decl))
        }]
    }
}

pub enum Decl {
    QReg(Identifier, Integer),
    CReg(Identifier, Integer),
}

impl ASTNode<TokenMatch> for Decl {
    fn parse_impls() -> Vec<fn(&mut Iter<TokenMatch>) -> Option<Self>> {
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

pub struct Identifier(String);

impl ASTNodeSimple<TokenMatch> for Identifier {
    fn parse_impl(tokens: &mut Iter<TokenMatch>) -> Option<Self> {
        if let Some((Token::Identifier, s)) = tokens.next() {
            return Some(Identifier(s.clone()));
        }
        None
    }
}

pub struct Number(String);

impl ASTNodeSimple<TokenMatch> for Number {
    fn parse_impl(tokens: &mut Iter<TokenMatch>) -> Option<Self> {
        if let Some((Token::Number, s)) = tokens.next() {
            return Some(Number(s.clone()));
        }
        None
    }
}

pub struct Integer(u32);

impl ASTNodeSimple<TokenMatch> for Integer {
    fn parse_impl(tokens: &mut Iter<TokenMatch>) -> Option<Self> {
        if let Some((Token::Int, s)) = tokens.next() {
            return Some(Integer(s.parse().ok()?));
        }
        None
    }
}


