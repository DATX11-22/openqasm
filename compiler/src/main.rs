mod rule;
mod symbol_analyzer;
use rule::{Rule, RuleCondition, RuleState, RuleStateTransition, ClassT};
use symbol_analyzer::SymbolAnalyzer;

#[derive(Clone, Copy, Debug)]
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

impl RuleStateTransition<char> {
    pub fn new_str(condition: RuleCondition<&str>, next: Option<usize>) -> Self {
        let convert = |t: &str| t.as_bytes().iter().map(|b| *b as char).collect();

        let condition_vec = match condition {
            RuleCondition::OneOf(t) => RuleCondition::OneOf(convert(t)),
            RuleCondition::NotOneOf(t) => RuleCondition::NotOneOf(convert(t)),
        };

        RuleStateTransition::new(condition_vec, next)
    }
}

impl<Class: ClassT> Rule<char, Class> {
    pub fn equal_str(class: Option<Class>, match_str: &str) -> Rule<char, Class> {
        Rule::equal(class, match_str.bytes().map(|b| b as char).collect())
    }

    pub fn any_str(class: Option<Class>, char_set: &str) -> Rule<char, Class> {
        Rule::any(class, char_set.bytes().map(|b| b as char).collect())
    }

    pub fn add_state_simple_str(
        &mut self,
        condition: RuleCondition<&str>,
        next: Option<usize>,
        done: bool,
    ) {
        let convert = |t: &str| t.as_bytes().iter().map(|b| *b as char).collect();

        let condition_vec = match condition {
            RuleCondition::OneOf(t) => RuleCondition::OneOf(convert(t)),
            RuleCondition::NotOneOf(t) => RuleCondition::NotOneOf(convert(t)),
        };

        self.add_state_simple(condition_vec, next, done);
    }
}


fn main() {
    let file_str = "
        hello hello there02334
        OPENQASM
        123.12123215 123there
        \"Hello there 12398u1289dfsd.f.sd\"12->3
    ";

    let mut lexer = SymbolAnalyzer::<char, Token>::new();

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
}
