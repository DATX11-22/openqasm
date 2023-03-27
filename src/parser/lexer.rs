pub mod rule;
mod symbol_analyzer;

use rule::{Rule, RuleCondition, RuleStateTransition, TokenT};
use symbol_analyzer::SymbolAnalyzer;

pub type Lexer<Token> = SymbolAnalyzer<char, Token>;

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

impl<Token: TokenT> Rule<char, Token> {
    pub fn equal_str(token: Option<Token>, match_str: &str) -> Rule<char, Token> {
        Rule::equal(token, match_str.bytes().map(|b| b as char).collect())
    }

    pub fn any_str(token: Option<Token>, char_set: &str) -> Rule<char, Token> {
        Rule::any(token, char_set.bytes().map(|b| b as char).collect())
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

