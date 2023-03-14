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

trait SymbolT: Copy + PartialEq  {}
impl<Symbol: Copy + PartialEq> SymbolT for Symbol {}
trait ClassT: Copy {}
impl<Class: Copy> ClassT for Class {}

enum RuleCondition<T> {
    OneOf(T),
    NotOneOf(T),
}

impl<Symbol: SymbolT> RuleCondition<Vec<Symbol>> {
    pub fn check_condition(&self, s: Symbol) -> bool {
        match self {
            RuleCondition::OneOf(vec) => vec.contains(&s),
            RuleCondition::NotOneOf(vec) => !vec.contains(&s),
        }
    }
}

struct RuleStateTransition<Symbol> {
    condition: RuleCondition<Vec<Symbol>>,
    next: Option<usize>,
}

impl<Symbol: SymbolT> RuleStateTransition<Symbol> {
    pub fn new(condition: RuleCondition<Vec<Symbol>>, next: Option<usize>) -> Self {
        Self { condition, next }
    }
}

impl RuleStateTransition<char> {
    pub fn new_str(condition: RuleCondition<&str>, next: Option<usize>) -> Self {
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

struct RuleState<Symbol> {
    transitions: Vec<RuleStateTransition<Symbol>>,
    done: bool,
}

struct Rule<Symbol, Class> {
    class: Option<Class>,
    states: Vec<RuleState<Symbol>>,
}

impl<Symbol: SymbolT, Class: ClassT> Rule<Symbol, Class> {
    pub fn new(class: Option<Class>) -> Rule<Symbol, Class> {
        Self {
            class,
            states: Vec::new(),
        }
    }

    pub fn equal(class: Option<Class>, match_vec: Vec<Symbol>) -> Rule<Symbol, Class> {
        let mut states: Vec<_> = match_vec
            .iter()
            .enumerate()
            .map(|(i, c)| RuleState {
                transitions: vec![RuleStateTransition::new(
                    RuleCondition::OneOf(vec![*c]),
                    Some(i + 1),
                )],
                done: false,
            })
            .collect();

        states.push(RuleState {
            transitions: vec![RuleStateTransition::new(RuleCondition::OneOf(vec![]), None)],
            done: true,
        });

        Self { class, states }
    }

    pub fn any(class: Option<Class>, symbol_set: Vec<Symbol>) -> Rule<Symbol, Class> {
        let states = vec![
            RuleState {
                transitions: vec![RuleStateTransition::new(
                    RuleCondition::OneOf(symbol_set.clone()),
                    Some(1),
                )],
                done: false,
            },
            RuleState {
                transitions: vec![RuleStateTransition::new(
                    RuleCondition::OneOf(symbol_set),
                    Some(1),
                )],
                done: true,
            },
        ];

        Self { class, states }
    }

    pub fn add_state(&mut self, rule_state: RuleState<Symbol>) {
        self.states.push(rule_state);
    }

    pub fn add_state_simple(
        &mut self,
        condition: RuleCondition<Vec<Symbol>>,
        next: Option<usize>,
        done: bool,
    ) {
        self.states.push(RuleState {
            transitions: vec![RuleStateTransition::new(condition, next)],
            done,
        });
    }

    pub fn next_state(&self, state_id: usize, c: Symbol) -> Option<usize> {
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

struct RuleMatch<'a, Symbol, Class> {
    rule: &'a Rule<Symbol, Class>,
    state_id: usize,
}

impl<'a, Symbol: SymbolT, Class: ClassT> RuleMatch<'a, Symbol, Class> {
    pub fn new(rule: &'a Rule<Symbol, Class>) -> Self {
        Self { rule, state_id: 0 }
    }

    pub fn class(&self) -> Option<Class> {
        self.rule.class
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

struct SymbolAnalyzer<Symbol, Class> {
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
