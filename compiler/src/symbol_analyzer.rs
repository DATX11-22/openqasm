use crate::rule::{Rule, SymbolT, ClassT};

struct RuleMatch<'a, Symbol, Class> {
    rule: &'a Rule<Symbol, Class>,
    state_id: usize,
}

impl<'a, Symbol: SymbolT, Class: ClassT> RuleMatch<'a, Symbol, Class> {
    pub fn new(rule: &'a Rule<Symbol, Class>) -> Self {
        Self { rule, state_id: 0 }
    }

    pub fn class(&self) -> Option<Class> {
        self.rule.class()
    }

    pub fn done(&self) -> bool {
        self.rule.done(self.state_id)
    }

    pub fn next(&self, c: Symbol) -> Option<RuleMatch<'a, Symbol, Class>> {
        if let Some(next_state) = self.rule.next_state(self.state_id, c) {
            return Some(RuleMatch {
                rule: self.rule,
                state_id: next_state,
            });
        }

        None
    }
}

pub struct SymbolAnalyzer<Symbol, Class> {
    rules: Vec<Rule<Symbol, Class>>,
}

impl<Symbol: SymbolT, Class: ClassT> SymbolAnalyzer<Symbol, Class> {
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    pub fn add_rule(&mut self, rule: Rule<Symbol, Class>) {
        // TODO: Validate rule
        self.rules.push(rule);
    }

    pub fn parse(&self, symbols: Vec<Symbol>) -> Vec<Class> {
        let mut classifications = Vec::new();

        let mut i = 0;
        loop {
            let mut done_match = None;
            let mut classification_matches: Vec<_> =
                self.rules.iter().map(|r| RuleMatch::new(r)).collect();

            loop {
                // Store the first complete match
                for classification_match in classification_matches.iter() {
                    if classification_match.done() {
                        done_match = Some((classification_match.class(), i));
                    }
                }

                if i >= symbols.len() || classification_matches.is_empty() {
                    break;
                }

                // Filter out matches which no longer match after reading the character at index i
                classification_matches = classification_matches
                    .iter()
                    .filter_map(|cm| cm.next(symbols[i]))
                    .collect();

                i += 1;
            }

            if let Some((classification, end_index)) = done_match {
                if let Some(classification) = classification {
                    classifications.push(classification);
                }
                i = end_index;
            } else {
                println!("ERROR!!!!!!!!!!!!!!!!");
                break;
            }

            if i >= symbols.len() {
                break;
            }
        }

        classifications
    }
}

