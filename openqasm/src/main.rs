use std::slice::Iter;

use compiler::lexer::rule::{Rule, RuleCondition, RuleState, RuleStateTransition};
use compiler::lexer::Lexer;

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

trait ASTNodeSimple<Token> {
    fn parse_impl(tokens: &mut Iter<Token>) -> Option<Self>
    where
        Self: Sized;
}

impl<Token, T: ASTNodeSimple<Token>> ASTNode<Token> for T {
    fn parse_impls() -> Vec<fn(&mut Iter<Token>) -> Option<Self>>
    where
        Self: Sized,
    {
        vec![|tokens| Self::parse_impl(tokens)]
    }
}

trait ASTNode<Token> {
    fn parse(tokens: &mut Iter<Token>) -> Option<Self>
    where
        Self: Sized,
    {
        for parse_impl in Self::parse_impls() {
            let mut tokens_cpy = tokens.clone();

            let res = parse_impl(&mut tokens_cpy);

            if res.is_some() {
                *tokens = tokens_cpy;
                return res;
            }
        }

        None
    }

    fn parse_impls() -> Vec<fn(&mut Iter<Token>) -> Option<Self>>
    where
        Self: Sized;
}

impl Token {
    fn parse(tokens: &mut Iter<Self>, token: Self) -> Option<Self> {
        if tokens.next() == Some(&token) {
            return Some(token);
        }
        None
    }
}

struct MainProgram(Number, Program);

impl ASTNodeSimple<Token> for MainProgram {
    fn parse_impl(tokens: &mut Iter<Token>) -> Option<Self> {
        Token::parse(tokens, Token::OPENQASM)?;
        let num = Number::parse(tokens)?;
        Token::parse(tokens, Token::Semicolon)?;
        let program = Program::parse(tokens)?;

        Some(MainProgram(num, program))
    }
}

enum Program {
    Sigle(Statement),
    Multiple(Statement, Box<Program>),
}

impl ASTNode<Token> for Program {
    fn parse_impls() -> Vec<fn(&mut Iter<Token>) -> Option<Self>> {
        vec![
            |tokens| {
                let statement = Statement::parse(tokens)?;
                Some(Program::Sigle(statement))
            },
            |tokens| {
                let statement = Statement::parse(tokens)?;
                Some(Program::Sigle(statement))
            },
        ]
    }
}

enum Statement {
    Test,
}

impl ASTNodeSimple<Token> for Statement {
    fn parse_impl(tokens: &mut Iter<Token>) -> Option<Self> {
        Token::parse(tokens, Token::U)?;
        Some(Statement::Test)
    }
}

struct Number;

impl ASTNodeSimple<Token> for Number {
    fn parse_impl(tokens: &mut Iter<Token>) -> Option<Self> {
        Token::parse(tokens, Token::Number)?;
        Some(Number)
    }
}

fn main() {
    let file_str = "
        OPENQASM 2.0; U
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
    lexer.add_rule(Rule::equal_str(Some(Token::OpenSquare), ")"));
    lexer.add_rule(Rule::equal_str(Some(Token::CloseSquare), ")"));
    lexer.add_rule(Rule::equal_str(Some(Token::OpenCurly), ")"));
    lexer.add_rule(Rule::equal_str(Some(Token::CloseCurly), ")"));
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

    // // String rule
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
    
    let t_vec: Vec<Token> = tokens.iter().map(|(a, _)| *a).collect();

    let a = MainProgram::parse(&mut t_vec.iter());
    println!("{:?}", a.is_some());
}
