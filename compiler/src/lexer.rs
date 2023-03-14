use crate::rule::{Rule, RuleCondition, RuleStateTransition, ClassT};
use crate::symbol_analyzer::SymbolAnalyzer;

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

