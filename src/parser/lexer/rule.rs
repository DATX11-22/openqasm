//! The code for defining rules which specify how to tokenize a sequence
//! of symbols into a sequence of tokens.

pub trait SymbolT: Copy + PartialEq {}
impl<Symbol: Copy + PartialEq> SymbolT for Symbol {}
pub trait TokenT: Copy {}
impl<Token: Copy> TokenT for Token {}

/// The condition for following a rule
pub enum RuleCondition<T> {
    OneOf(T),    // The symbol is one of a set of symbols
    NotOneOf(T), // The symbol is not one of a set of symbols
}

impl<Symbol: SymbolT> RuleCondition<Vec<Symbol>> {
    /// Checks that a condition holds
    pub fn check_condition(&self, s: Symbol) -> bool {
        match self {
            RuleCondition::OneOf(vec) => vec.contains(&s),
            RuleCondition::NotOneOf(vec) => !vec.contains(&s),
        }
    }
}

/// A transition from one [RuleState] to another in a [Rule]
pub struct RuleStateTransition<Symbol> {
    condition: RuleCondition<Vec<Symbol>>,
    next: Option<usize>,
}

impl<Symbol: SymbolT> RuleStateTransition<Symbol> {
    pub fn new(condition: RuleCondition<Vec<Symbol>>, next: Option<usize>) -> Self {
        Self { condition, next }
    }
}

/// A state in a [Rule]
pub struct RuleState<Symbol> {
    pub transitions: Vec<RuleStateTransition<Symbol>>,
    pub done: bool,
}

/// A rule for deciding whether or not a sequence of symbols match a certain token.
///
/// Works as a state machine by containing different [RuleState]s and [RuleStateTransition]s between
/// them.
///
/// If the token is [None] this doesn't match a token, but will instead be discarded if matched.
pub struct Rule<Symbol, Token> {
    token: Option<Token>,
    states: Vec<RuleState<Symbol>>,
}

impl<Symbol: SymbolT, Token: TokenT> Rule<Symbol, Token> {
    /// Construct a [Rule] which doesn't contain any [RuleState]s.
    pub fn new(token: Option<Token>) -> Rule<Symbol, Token> {
        Self {
            token,
            states: Vec::new(),
        }
    }

    /// Construct a rule which matches exactly a sequence of symbols.
    pub fn equal(token: Option<Token>, match_vec: Vec<Symbol>) -> Rule<Symbol, Token> {
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

        Self { token, states }
    }

    /// Constructs a rule which matches a sequence of at least one symbol which matches any
    /// of the symbols in the specified set of symbols.
    pub fn any(token: Option<Token>, symbol_set: Vec<Symbol>) -> Rule<Symbol, Token> {
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

        Self { token, states }
    }

    /// Adds a new state.
    pub fn add_state(&mut self, rule_state: RuleState<Symbol>) {
        self.states.push(rule_state);
    }

    /// Adds a new state which only contains one transition.
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

    /// Matches a symbol and determines which state to transition to next.
    ///
    /// If no transition matches it instead returns [None] which indicates that
    /// the rule no longer matches the sequence of symbols.
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

    /// Checks if the rule is a complete match.
    pub fn done(&self, state_id: usize) -> bool {
        self.states[state_id].done
    }

    /// Gets the token which the [Rule] matches to.
    pub fn token(&self) -> Option<Token> {
        self.token
    }
}
