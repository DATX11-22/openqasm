use compiler::ast::ast_node::{ToRefVec, ToVec};

use crate::ast::{
    AnyList, Argument, Exp, ExpList, GopList, IdList, Identifier, MixedList, Program, Statement,
    UOp,
};

impl ToRefVec<Statement> for Program {
    fn next(&self) -> (Option<&Self>, &Statement) {
        match self {
            Program::Multiple(item, next) => (Some(next), item),
            Program::Single(item) => (None, item),
        }
    }
}

impl ToRefVec<UOp> for GopList {
    fn next(&self) -> (Option<&Self>, &UOp) {
        match self {
            GopList::UOp(uop) => (None, uop),
            GopList::GopList(uop, next) => (Some(next), uop),
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
