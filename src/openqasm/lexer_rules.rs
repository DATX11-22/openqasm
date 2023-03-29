//! Defines the rules for tokenizing an openqasm 2.0 file.

use crate::openqasm::token::Token;
use crate::parser::lexer::{
    rule::{Rule, RuleCondition, RuleState, RuleStateTransition},
    Lexer,
};

/// Adds the rules required to tokenize an openqasm 2.0 file to a lexer.
pub fn add_open_qasm_rules(lexer: &mut Lexer<Token>) {
    // Keyword rules, simple string matches
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

    // Symbol rules, simple string matches
    lexer.add_rule(Rule::equal_str(Some(Token::Semicolon), ";"));
    lexer.add_rule(Rule::equal_str(Some(Token::Comma), ","));
    lexer.add_rule(Rule::equal_str(Some(Token::Dot), "."));
    lexer.add_rule(Rule::equal_str(Some(Token::OpenParen), "("));
    lexer.add_rule(Rule::equal_str(Some(Token::CloseParen), ")"));
    lexer.add_rule(Rule::equal_str(Some(Token::OpenSquare), "["));
    lexer.add_rule(Rule::equal_str(Some(Token::CloseSquare), "]"));
    lexer.add_rule(Rule::equal_str(Some(Token::OpenCurly), "{"));
    lexer.add_rule(Rule::equal_str(Some(Token::CloseCurly), "}"));
    lexer.add_rule(Rule::equal_str(Some(Token::Plus), "+"));
    lexer.add_rule(Rule::equal_str(Some(Token::Minus), "-"));
    lexer.add_rule(Rule::equal_str(Some(Token::Mul), "*"));
    lexer.add_rule(Rule::equal_str(Some(Token::Div), "/"));
    lexer.add_rule(Rule::equal_str(Some(Token::Pow), "^"));
    lexer.add_rule(Rule::equal_str(Some(Token::Sin), "sin"));
    lexer.add_rule(Rule::equal_str(Some(Token::Cos), "cos"));
    lexer.add_rule(Rule::equal_str(Some(Token::Tan), "tan"));
    lexer.add_rule(Rule::equal_str(Some(Token::Exp), "exp"));
    lexer.add_rule(Rule::equal_str(Some(Token::Ln), "ln"));
    lexer.add_rule(Rule::equal_str(Some(Token::Sqrt), "sqrt"));
    lexer.add_rule(Rule::equal_str(Some(Token::Pi), "pi"));
    lexer.add_rule(Rule::equal_str(Some(Token::Arrow), "->"));
    lexer.add_rule(Rule::equal_str(Some(Token::Equal), "=="));

    // Int rule. A sequence of at least 1 number
    lexer.add_rule(Rule::any_str(Some(Token::Int), "0123456789"));

    // Number rule. Parses Integer.Integer
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

    // String rule. Parses "Some sequence of characters"
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

    // Identifier rule. Has to start with a lowercase alphabetic character. Can then
    // contain any number of alphabetic characters, numbers or _
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

    // Comment rule. Parses // and then any number of characters until a newline is found.
    // The characters matched are then discarded.
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
}
