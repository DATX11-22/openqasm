use std::{collections::HashSet, hash::Hash, cmp::Eq};
use super::SemanticError;
use crate::openqasm::ast::{Argument, Identifier};

pub fn arg_id(arg: &Argument) -> &String {
    match arg {
        Argument::Id(id) => &id.0,
        Argument::Indexed(id, _) => &id.0,
    }
}

pub fn arg_to_id(arg: &Argument) -> Result<&Identifier, SemanticError> {
    match arg {
        Argument::Id(id) => Ok(id),
        Argument::Indexed(_, _) => Err(SemanticError::IndexedRegisterInGateDecl),
    }
}

pub fn is_unique<T: Hash + Eq>(vec: &Vec<T>) -> bool {
    let mut found = HashSet::new();
    vec.iter().all(|e| found.insert(e))
}
