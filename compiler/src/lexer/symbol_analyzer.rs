use super::rule::{Rule, SymbolT, TokenT};

struct RuleMatch<'a, Symbol, Token> {
    rule: &'a Rule<Symbol, Token>,
    state_id: usize,
}

impl<'a, Symbol: SymbolT, Token: TokenT> RuleMatch<'a, Symbol, Token> {
    pub fn new(rule: &'a Rule<Symbol, Token>) -> Self {
        Self { rule, state_id: 0 }
    }

    pub fn token(&self) -> Option<Token> {
        self.rule.token()
    }

    pub fn done(&self) -> bool {
        self.rule.done(self.state_id)
    }

    pub fn next(&self, c: Symbol) -> Option<RuleMatch<'a, Symbol, Token>> {
        if let Some(next_state) = self.rule.next_state(self.state_id, c) {
            return Some(RuleMatch {
                rule: self.rule,
                state_id: next_state,
            });
        }

        None
    }
}

pub struct SymbolAnalyzer<Symbol, TokenType> {
    rules: Vec<Rule<Symbol, TokenType>>,
}

impl<Symbol: SymbolT, TokenType: TokenT> SymbolAnalyzer<Symbol, TokenType> {
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    pub fn add_rule(&mut self, rule: Rule<Symbol, TokenType>) {
        // TODO: Validate rule
        self.rules.push(rule);
    }

    pub fn parse(&self, symbols: Vec<Symbol>) -> Vec<(TokenType, Vec<Symbol>)> {
        let mut tokens = Vec::new();

        let mut i = 0;
        loop {
            let mut done_match = None;
            let mut token_matches: Vec<_> =
                self.rules.iter().map(|r| RuleMatch::new(r)).collect();

            let match_start = i;
            loop {
                // Store the first complete match
                for token_match in token_matches.iter() {
                    if token_match.done() {
                        done_match = Some((token_match.token(), i));
                    }
                }

                if i >= symbols.len() || token_matches.is_empty() {
                    break;
                }

                // Filter out matches which no longer match after reading the character at index i
                token_matches = token_matches
                    .iter()
                    .filter_map(|cm| cm.next(symbols[i]))
                    .collect();

                i += 1;
            }

            if let Some((token, match_end)) = done_match {
                if let Some(token) = token {
                    tokens.push((token, symbols[match_start..match_end].to_vec()));
                }
                i = match_end;
            } else {
                println!("ERROR!!!!!!!!!!!!!!!!");
                break;
            }

            if i >= symbols.len() {
                break;
            }
        }

        tokens
    }
}

