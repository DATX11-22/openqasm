//! Miscelanious functions used in semantic analysis

use super::SemanticError;
use crate::openqasm::ast::{Argument, Identifier};
use std::{cmp::Eq, collections::HashSet, hash::Hash};

/// Takes an argument and returns the id of the register it references.
pub fn arg_id(arg: &Argument) -> &String {
    match arg {
        Argument::Id(id) => &id.0,
        Argument::Indexed(id, _) => &id.0,
    }
}

/// Takes an argument and if the argument regerences an entire register it returns
/// the name of that register. Otherwise it returns an error.
pub fn arg_to_id(arg: &Argument) -> Result<&Identifier, SemanticError> {
    match arg {
        Argument::Id(id) => Ok(id),
        Argument::Indexed(_, _) => Err(SemanticError::IndexedRegisterInGateDecl),
    }
}

/// Checks that a vector only contains unique elements.
pub fn is_unique<T: Hash + Eq>(vec: &Vec<T>) -> bool {
    let mut found = HashSet::new();
    vec.iter().all(|e| found.insert(e))
}
