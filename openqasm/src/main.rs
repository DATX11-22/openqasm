use compiler::ast::ast_debug::ASTDebug;
use compiler::ast::ast_node::{ASTNode, ASTNodeSimple};
use compiler::lexer::rule::{Rule, RuleCondition, RuleState, RuleStateTransition};
use compiler::lexer::Lexer;
use std::slice::Iter;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Token {
    OPENQASM,
    Include,
    QReg,
    CReg,
    Gate,
    U,
    CX,
    Measure,
    Reset,
    If,
    Barrier,
    Semicolon,
    Comma,
    Dot,
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
    Arrow,
    Equal,
    Int,
    Number,
    Str,
    Identifier,
}

type TokenMatch = (Token, String);

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

struct MainProgram(Number, Program);

impl ASTNodeSimple<TokenMatch> for MainProgram {
    fn parse_impl(tokens: &mut Iter<TokenMatch>) -> Option<Self> {
        Token::parse(tokens, Token::OPENQASM)?;
        let num = Number::parse(tokens)?;
        Token::parse(tokens, Token::Semicolon)?;
        let program = Program::parse(tokens)?;

        Some(MainProgram(num, program))
    }
}

enum Program {
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

enum Statement {
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

enum Decl {
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

struct Identifier(String);

impl ASTNodeSimple<TokenMatch> for Identifier {
    fn parse_impl(tokens: &mut Iter<TokenMatch>) -> Option<Self> {
        if let Some((Token::Identifier, s)) = tokens.next() {
            return Some(Identifier(s.clone()));
        }
        None
    }
}

struct Number(String);

impl ASTNodeSimple<TokenMatch> for Number {
    fn parse_impl(tokens: &mut Iter<TokenMatch>) -> Option<Self> {
        if let Some((Token::Number, s)) = tokens.next() {
            return Some(Number(s.clone()));
        }
        None
    }
}

struct Integer(u32);

impl ASTNodeSimple<TokenMatch> for Integer {
    fn parse_impl(tokens: &mut Iter<TokenMatch>) -> Option<Self> {
        if let Some((Token::Int, s)) = tokens.next() {
            return Some(Integer(s.parse().ok()?));
        }
        None
    }
}

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
            Program::Sigle(statement) => vec![statement],
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
            Statement::GateDecl => todo!(),
            Statement::GateDeclEmpty => todo!(),
            Statement::QOp => todo!(),
            Statement::If => todo!(),
        }
    }

    fn name(&self) -> String {
        match self {
            Statement::Decl(_) => "Decl".to_string(),
            Statement::GateDecl => todo!(),
            Statement::GateDeclEmpty => todo!(),
            Statement::QOp => todo!(),
            Statement::If => todo!(),
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

fn main() {
    let file_str = "
        OPENQASM 2.0;

        qreg r1[10];
        qreg r2[10];
    ";

    let mut lexer = Lexer::new();

    // Keyword rules
    lexer.add_rule(Rule::equal_str(Some(Token::OPENQASM), "OPENQASM"));
    lexer.add_rule(Rule::equal_str(Some(Token::Include), "include"));
    lexer.add_rule(Rule::equal_str(Some(Token::QReg), "qreg"));
    lexer.add_rule(Rule::equal_str(Some(Token::CReg), "creg"));
    lexer.add_rule(Rule::equal_str(Some(Token::Gate), "gate"));
    lexer.add_rule(Rule::equal_str(Some(Token::U), "U"));
    lexer.add_rule(Rule::equal_str(Some(Token::CX), "CX"));
    lexer.add_rule(Rule::equal_str(Some(Token::Measure), "measure"));
    lexer.add_rule(Rule::equal_str(Some(Token::Reset), "reset"));
    lexer.add_rule(Rule::equal_str(Some(Token::If), "if"));
    lexer.add_rule(Rule::equal_str(Some(Token::Barrier), "barrier"));

    // Symbol rules
    lexer.add_rule(Rule::equal_str(Some(Token::Semicolon), ";"));
    lexer.add_rule(Rule::equal_str(Some(Token::Comma), ","));
    lexer.add_rule(Rule::equal_str(Some(Token::Dot), "."));
    lexer.add_rule(Rule::equal_str(Some(Token::OpenParen), "("));
    lexer.add_rule(Rule::equal_str(Some(Token::CloseParen), ")"));
    lexer.add_rule(Rule::equal_str(Some(Token::OpenSquare), "["));
    lexer.add_rule(Rule::equal_str(Some(Token::CloseSquare), "]"));
    lexer.add_rule(Rule::equal_str(Some(Token::OpenCurly), "{"));
    lexer.add_rule(Rule::equal_str(Some(Token::CloseCurly), "}"));
    lexer.add_rule(Rule::equal_str(Some(Token::Arrow), "->"));
    lexer.add_rule(Rule::equal_str(Some(Token::Equal), "=="));

    // Int rule
    lexer.add_rule(Rule::any_str(Some(Token::Int), "0123456789"));

    // Number rule
    let mut number_rule = Rule::new(Some(Token::Number));
    number_rule.add_state_simple_str(RuleCondition::OneOf("0123456789"), Some(1), false);
    number_rule.add_state(RuleState {
        transitions: vec![
            RuleStateTransition::new_str(RuleCondition::OneOf("0123456789"), Some(1)),
            RuleStateTransition::new_str(RuleCondition::OneOf("."), Some(2)),
        ],
        done: false,
    });
    number_rule.add_state_simple_str(RuleCondition::OneOf("0123456789"), Some(3), false);
    number_rule.add_state_simple_str(RuleCondition::OneOf("0123456789"), Some(3), true);
    lexer.add_rule(number_rule);

    // String rule
    let mut string_rule = Rule::new(Some(Token::Str));
    string_rule.add_state_simple_str(RuleCondition::OneOf("\""), Some(1), false);
    string_rule.add_state(RuleState {
        transitions: vec![
            RuleStateTransition::new_str(RuleCondition::NotOneOf("\""), Some(1)),
            RuleStateTransition::new_str(RuleCondition::OneOf("\""), Some(2)),
        ],
        done: false,
    });
    string_rule.add_state_simple_str(RuleCondition::OneOf(""), None, true);
    lexer.add_rule(string_rule);

    // Identifier rule
    let mut identifier_rule = Rule::new(Some(Token::Identifier));
    identifier_rule.add_state_simple_str(
        RuleCondition::OneOf("abcdefghijklmnopqrstuvwxyz"),
        Some(1),
        false,
    );
    identifier_rule.add_state_simple_str(
        RuleCondition::OneOf("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"),
        Some(1),
        true,
    );
    lexer.add_rule(identifier_rule);

    // Comment rule
    let mut comment_rule = Rule::new(None);
    comment_rule.add_state_simple_str(RuleCondition::OneOf("/"), Some(1), false);
    comment_rule.add_state_simple_str(RuleCondition::OneOf("/"), Some(2), false);
    comment_rule.add_state(RuleState {
        transitions: vec![
            RuleStateTransition::new_str(RuleCondition::OneOf("\n"), None),
            RuleStateTransition::new_str(RuleCondition::NotOneOf("\n"), Some(2)),
        ],
        done: true,
    });
    lexer.add_rule(comment_rule);

    // Whitespace rule
    lexer.add_rule(Rule::any_str(None, " \n\t"));

    let file_chars = file_str.bytes().map(|b| b as char).collect();
    let tokens = lexer.parse(file_chars);
    for token in tokens.iter() {
        println!("Token: {:?}", token);
    }

    let t_vec: Vec<TokenMatch> = tokens
        .iter()
        .map(|(a, s)| (*a, s.into_iter().collect::<String>()))
        .collect();

    let mut iter = t_vec.iter();
    let a = MainProgram::parse(&mut iter);
    if let Some(a) = a {
        a.print();
    }
    // println!("{:?}", a.is_some() && iter.next().is_none());
}
