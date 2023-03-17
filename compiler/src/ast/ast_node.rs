use std::slice::Iter;

pub trait ASTNodeSimple<TokenMatch> {
    fn parse_impl(tokens: &mut Iter<TokenMatch>) -> Option<Self>
    where
        Self: Sized;
}

impl<TokenMatch, T: ASTNodeSimple<TokenMatch>> ASTNode<TokenMatch> for T {
    fn parse_impls() -> Vec<fn(&mut Iter<TokenMatch>) -> Option<Self>>
    where
        Self: Sized,
    {
        vec![|tokens| Self::parse_impl(tokens)]
    }
}

pub trait ASTNode<TokenMatch> {
    fn parse(tokens: &mut Iter<TokenMatch>) -> Option<Self>
    where
        Self: Sized,
    {
        for parse_impl in Self::parse_impls() {
            let mut tokens_cpy = tokens.clone();

            let res = parse_impl(&mut tokens_cpy);

            if res.is_some() {
                *tokens = tokens_cpy;
                return res;
            }
        }

        None
    }

    fn parse_impls() -> Vec<fn(&mut Iter<TokenMatch>) -> Option<Self>>
    where
        Self: Sized;
}


