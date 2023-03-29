//! Code for a generic tokenizer which can tokenize a sequence of symbols to
//! a sequence of tokens.

use super::rule::{Rule, SymbolT, TokenT};

/// The error returned when an invalid token is found.
#[derive(Debug)]
pub enum TokenizerError {
    InvalidToken(usize),
}

/// Represents a match following a certin [Rule].
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

    /// Given the next symbol in the sequence returns a new updated match if
    /// the symbol followed some valid transition in the previous match.
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

/// A tokenizer which can take a sequence of symbols and converts them into a
/// sequence of tokens using some rules.
pub struct SymbolAnalyzer<Symbol, TokenType> {
    rules: Vec<Rule<Symbol, TokenType>>,
}

impl<Symbol: SymbolT, TokenType: TokenT> SymbolAnalyzer<Symbol, TokenType> {
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    /// Add a new rule which will be used to tokenize.
    pub fn add_rule(&mut self, rule: Rule<Symbol, TokenType>) {
        // TODO: Validate rule
        self.rules.push(rule);
    }

    /// The function used for tokenizing a sequence of symbols. For each valid token found
    /// the sequence of symbols matched is also returned.
    pub fn parse(&self, symbols: Vec<Symbol>) -> Result<Vec<(TokenType, Vec<Symbol>)>, TokenizerError> {
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
                        break;
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
                return Err(TokenizerError::InvalidToken(i));
            }

            if i >= symbols.len() {
                break;
            }
        }

        Ok(tokens)
    }
}

