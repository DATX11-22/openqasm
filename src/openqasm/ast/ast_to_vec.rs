//! Contains implementations of [ToVec](crate::parser::ast::ast_node::ToVec) and [ToRefVec](crate::parser::ast::ast_node::ToRefVec) for some of
//! the AST nodes which describe a list like structure. This makes it easier to handle the nodes when
//! performing semantic analysis.

use crate::openqasm::ast::{
    AnyList, Argument, Exp, ExpList, GopList, IdList, Identifier, MixedList, Program, Statement,
    UOp,
};
use crate::parser::ast::ast_node::{ToRefVec, ToVec};

impl ToRefVec<Statement> for Program {
    fn next(&self) -> (Option<&Self>, &Statement) {
        match self {
            Program::Multiple(item, next) => (Some(next), item),
            Program::Single(item) => (None, item),
        }
    }
}

impl ToRefVec<Identifier> for IdList {
    fn next(&self) -> (Option<&Self>, &Identifier) {
        match self {
            IdList::IdList(id, next) => (Some(next), id),
            IdList::Id(id) => (None, id),
        }
    }
}

impl ToRefVec<Exp> for ExpList {
    fn next(&self) -> (Option<&Self>, &Exp) {
        match self {
            ExpList::ExpList(exp, next) => (Some(next), exp),
            ExpList::Exp(exp) => (None, exp),
        }
    }
}

pub enum UOpOrBarrier {
    UOp(UOp),
    Barrier(IdList),
}

/// Some AST nodes can not implement [ToRefVec] since they need to create a list of
/// some other datatype than what they contain. This requires cloning and is therefore
/// not optimal for performance. ToRefVec should therefore be prefered when possible.
impl ToVec<UOpOrBarrier> for GopList {
    fn next(&self) -> (Option<&dyn ToVec<UOpOrBarrier>>, UOpOrBarrier) {
        match self {
            GopList::UOp(uop) => (None, UOpOrBarrier::UOp(uop.clone())),
            GopList::Barrier(idlist) => (None, UOpOrBarrier::Barrier(idlist.clone())),
            GopList::GopListUOp(uop, goplist) => {
                (Some(goplist.as_ref()), UOpOrBarrier::UOp(uop.clone()))
            }
            GopList::GopListBarrier(idlist, goplist) => (
                Some(goplist.as_ref()),
                UOpOrBarrier::Barrier(idlist.clone()),
            ),
        }
    }
}

impl ToVec<Argument> for AnyList {
    fn next(&self) -> (Option<&dyn ToVec<Argument>>, Argument) {
        match self {
            AnyList::IdList(idlist) => ToVec::next(idlist),
            AnyList::MixedList(mixedlist) => ToVec::next(mixedlist),
        }
    }
}

impl ToVec<Argument> for MixedList {
    fn next(&self) -> (Option<&dyn ToVec<Argument>>, Argument) {
        match self {
            MixedList::Indexed(id, index) => (None, Argument::Indexed(id.clone(), index.clone())),
            MixedList::IdMixedList(id, mixedlist) => {
                (Some(mixedlist.as_ref()), Argument::Id(id.clone()))
            }
            MixedList::IndexedMixedList(id, index, mixedlist) => (
                Some(mixedlist.as_ref()),
                Argument::Indexed(id.clone(), index.clone()),
            ),
            MixedList::IndexedIdList(id, index, idlist) => {
                (Some(idlist), Argument::Indexed(id.clone(), index.clone()))
            }
        }
    }
}

impl ToVec<Argument> for IdList {
    fn next(&self) -> (Option<&dyn ToVec<Argument>>, Argument) {
        match self {
            IdList::IdList(id, idlist) => (Some(idlist.as_ref()), Argument::Id(id.clone())),
            IdList::Id(id) => (None, Argument::Id(id.clone())),
        }
    }
}
