pub trait SymbolT: Copy + PartialEq  {}
impl<Symbol: Copy + PartialEq> SymbolT for Symbol {}
pub trait TokenT: Copy {}
impl<Token: Copy> TokenT for Token {}

pub enum RuleCondition<T> {
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

pub struct RuleStateTransition<Symbol> {
    condition: RuleCondition<Vec<Symbol>>,
    next: Option<usize>,
}

impl<Symbol: SymbolT> RuleStateTransition<Symbol> {
    pub fn new(condition: RuleCondition<Vec<Symbol>>, next: Option<usize>) -> Self {
        Self { condition, next }
    }
}

pub struct RuleState<Symbol> {
    pub transitions: Vec<RuleStateTransition<Symbol>>,
    pub done: bool,
}

pub struct Rule<Symbol, Token> {
    token: Option<Token>,
    states: Vec<RuleState<Symbol>>,
}

impl<Symbol: SymbolT, Token: TokenT> Rule<Symbol, Token> {
    pub fn new(token: Option<Token>) -> Rule<Symbol, Token> {
        Self {
            token,
            states: Vec::new(),
        }
    }

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

    pub fn token(&self) -> Option<Token> {
        self.token
    }
}

