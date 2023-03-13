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

enum RuleCondition<T> {
    OneOf(T),
    NotOneOf(T),
}

impl RuleCondition<Vec<char>> {
    pub fn check_condition(&self, c: char) -> bool {
        match self {
            RuleCondition::OneOf(vec) => vec.contains(&c),
            RuleCondition::NotOneOf(vec) => !vec.contains(&c),
        }
    }
}

struct RuleStateTransition {
    condition: RuleCondition<Vec<char>>,
    next: Option<usize>,
}

impl RuleStateTransition {
    pub fn new(condition: RuleCondition<&str>, next: Option<usize>) -> Self {
        let convert = |t: &str| t.as_bytes().iter().map(|b| *b as char).collect();

        let condition_vec = match condition {
            RuleCondition::OneOf(t) => RuleCondition::OneOf(convert(t)),
            RuleCondition::NotOneOf(t) => RuleCondition::NotOneOf(convert(t)),
        };

        Self {
            condition: condition_vec,
            next,
        }
    }
}

struct RuleState {
    transitions: Vec<RuleStateTransition>,
    done: bool,
}

struct TokenRule {
    token: Option<Token>,
    states: Vec<RuleState>,
}

impl TokenRule {
    pub fn new(token: Option<Token>) -> TokenRule {
        Self {
            token,
            states: Vec::new(),
        }
    }

    pub fn equal(token: Option<Token>, match_str: &str) -> TokenRule {
        let mut states: Vec<_> = match_str
            .chars()
            .enumerate()
            .map(|(i, c)| RuleState {
                transitions: vec![RuleStateTransition::new(
                    RuleCondition::OneOf(&c.to_string()),
                    Some(i + 1),
                )],
                done: false,
            })
            .collect();

        states.push(RuleState {
            transitions: vec![RuleStateTransition::new(RuleCondition::OneOf(""), None)],
            done: true,
        });

        Self { token, states }
    }

    pub fn any(token: Option<Token>, char_set: &str) -> TokenRule {
        let states = vec![
            RuleState {
                transitions: vec![RuleStateTransition::new(
                    RuleCondition::OneOf(char_set),
                    Some(1),
                )],
                done: false,
            },
            RuleState {
                transitions: vec![RuleStateTransition::new(
                    RuleCondition::OneOf(char_set),
                    Some(1),
                )],
                done: true,
            },
        ];

        Self { token, states }
    }

    pub fn add_state(&mut self, rule_state: RuleState) {
        self.states.push(rule_state);
    }

    pub fn add_state_simple(
        &mut self,
        condition: RuleCondition<&str>,
        next: Option<usize>,
        done: bool,
    ) {
        self.states.push(RuleState {
            transitions: vec![RuleStateTransition::new(condition, next)],
            done,
        });
    }

    pub fn next_state(&self, state_id: usize, c: char) -> Option<usize> {
        for transition in self.states[state_id].transitions.iter() {
            if !transition.condition.check_condition(c) {
                continue;
            }

            if let Some(next) = transition.next {
                return Some(next);
            }
        }

        None
    }

    pub fn done(&self, state_id: usize) -> bool {
        self.states[state_id].done
    }
}

struct TokenMatch<'a> {
    rule: &'a TokenRule,
    state_id: usize,
}

impl<'a> TokenMatch<'a> {
    pub fn new(rule: &'a TokenRule) -> Self {
        Self { rule, state_id: 0 }
    }

    pub fn token(&self) -> Option<Token> {
        self.rule.token
    }

    pub fn done(&self) -> bool {
        self.rule.done(self.state_id)
    }

    pub fn next(&self, c: char) -> Option<TokenMatch<'a>> {
        if let Some(next_state) = self.rule.next_state(self.state_id, c) {
            return Some(TokenMatch {
                rule: self.rule,
                state_id: next_state,
            });
        }

        None
    }
}

struct Lexer {
    rules: Vec<TokenRule>,
}

impl Lexer {
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    pub fn add_rule(&mut self, rule: TokenRule) {
        // TODO: Validate rule
        self.rules.push(rule);
    }

    pub fn parse(&self, file_str: &str) -> Vec<Token> {
        let mut tokens = Vec::new();

        let str_bytes = file_str.as_bytes();
        let mut i = 0;
        loop {
            let mut done_match = None;
            let mut token_matches: Vec<_> = self.rules.iter().map(|r| TokenMatch::new(r)).collect();

            loop {
                // Store the first complete match
                for token_match in token_matches.iter() {
                    if token_match.done() {
                        done_match = Some((token_match.token(), i));
                    }
                }

                if i >= str_bytes.len() || token_matches.is_empty() {
                    break;
                }

                // Filter out matches which no longer match after reading the character at index i
                let c = str_bytes[i] as char;
                token_matches = token_matches.iter().filter_map(|tm| tm.next(c)).collect();

                i += 1;
            }

            if let Some((token, end_index)) = done_match {
                if let Some(token) = token {
                    tokens.push(token);
                }
                i = end_index;
            } else {
                println!("ERROR!!!!!!!!!!!!!!!!");
                break;
            }

            if i >= str_bytes.len() {
                break;
            }
        }

        tokens
    }
}

fn main() {
    let file_str = "
        hello hello there02334
        OPENQASM
        123.12123215 123there
        \"Hello there 12398u1289dfsd.f.sd\"12->3
    ";

    let mut lexer = Lexer::new();

    // Keyword rules
    lexer.add_rule(TokenRule::equal(Some(Token::OPENQASM), "OPENQASM"));
    lexer.add_rule(TokenRule::equal(Some(Token::Include), "include"));
    lexer.add_rule(TokenRule::equal(Some(Token::QReg), "qreg"));
    lexer.add_rule(TokenRule::equal(Some(Token::CReg), "creg"));
    lexer.add_rule(TokenRule::equal(Some(Token::Gate), "gate"));
    lexer.add_rule(TokenRule::equal(Some(Token::U), "U"));
    lexer.add_rule(TokenRule::equal(Some(Token::CX), "CX"));
    lexer.add_rule(TokenRule::equal(Some(Token::Measure), "measure"));
    lexer.add_rule(TokenRule::equal(Some(Token::Reset), "reset"));
    lexer.add_rule(TokenRule::equal(Some(Token::If), "if"));
    lexer.add_rule(TokenRule::equal(Some(Token::Barrier), "barrier"));

    // Symbol rules
    lexer.add_rule(TokenRule::equal(Some(Token::Semicolon), ";"));
    lexer.add_rule(TokenRule::equal(Some(Token::Comma), ","));
    lexer.add_rule(TokenRule::equal(Some(Token::Dot), "."));
    lexer.add_rule(TokenRule::equal(Some(Token::OpenParen), "("));
    lexer.add_rule(TokenRule::equal(Some(Token::CloseParen), ")"));
    lexer.add_rule(TokenRule::equal(Some(Token::OpenSquare), ")"));
    lexer.add_rule(TokenRule::equal(Some(Token::CloseSquare), ")"));
    lexer.add_rule(TokenRule::equal(Some(Token::OpenCurly), ")"));
    lexer.add_rule(TokenRule::equal(Some(Token::CloseCurly), ")"));
    lexer.add_rule(TokenRule::equal(Some(Token::Arrow), "->"));
    lexer.add_rule(TokenRule::equal(Some(Token::Equal), "=="));

    // Int rule
    lexer.add_rule(TokenRule::any(Some(Token::Int), "0123456789"));

    // Number rule
    let mut number_rule = TokenRule::new(Some(Token::Number));
    number_rule.add_state_simple(RuleCondition::OneOf("0123456789"), Some(1), false);
    number_rule.add_state(RuleState {
        transitions: vec![
            RuleStateTransition::new(RuleCondition::OneOf("0123456789"), Some(1)),
            RuleStateTransition::new(RuleCondition::OneOf("."), Some(2)),
        ],
        done: false,
    });
    number_rule.add_state_simple(RuleCondition::OneOf("0123456789"), Some(3), false);
    number_rule.add_state_simple(RuleCondition::OneOf("0123456789"), Some(3), true);
    lexer.add_rule(number_rule);

    // // String rule
    let mut string_rule = TokenRule::new(Some(Token::Str));
    string_rule.add_state_simple(RuleCondition::OneOf("\""), Some(1), false);
    string_rule.add_state(RuleState {
        transitions: vec![
            RuleStateTransition::new(RuleCondition::NotOneOf("\""), Some(1)),
            RuleStateTransition::new(RuleCondition::OneOf("\""), Some(2)),
        ],
        done: false,
    });
    string_rule.add_state_simple(RuleCondition::OneOf(""), None, true);
    lexer.add_rule(string_rule);

    // Identifier rule
    let mut identifier_rule = TokenRule::new(Some(Token::Identifier));
    identifier_rule.add_state_simple(
        RuleCondition::OneOf("abcdefghijklmnopqrstuvwxyz"),
        Some(1),
        false,
    );
    identifier_rule.add_state_simple(
        RuleCondition::OneOf("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"),
        Some(1),
        true,
    );
    lexer.add_rule(identifier_rule);

    // Comment rule
    let mut comment_rule = TokenRule::new(None);
    comment_rule.add_state_simple(RuleCondition::OneOf("/"), Some(1), false);
    comment_rule.add_state_simple(RuleCondition::OneOf("/"), Some(2), false);
    comment_rule.add_state(RuleState {
        transitions: vec![
            RuleStateTransition::new(RuleCondition::OneOf("\n"), None),
            RuleStateTransition::new(RuleCondition::NotOneOf("\n"), Some(2)),
        ],
        done: true,
    });
    lexer.add_rule(comment_rule);

    // Whitespace rule
    lexer.add_rule(TokenRule::any(None, " \n\t"));

    let tokens = lexer.parse(file_str);
    for token in tokens.iter() {
        println!("Token: {:?}", token);
    }
}
