#[derive(Copy, Clone)]
pub struct TokenIter<'a, Token: Copy> {
    tokens: &'a Vec<(Token, String)>,
    i: usize,
}

impl<'a, Token: Copy> TokenIter<'a, Token> {
    pub fn create(tokens: &'a Vec<(Token, String)>) -> Self {
        TokenIter {
            tokens,
            i: 0,
        }
    }

    pub fn distance(from: &Self, to: &Self) -> Option<i32> {
        if from.tokens.as_ptr() == to.tokens.as_ptr() {
            return Some(to.i as i32 - from.i as i32);
        }
        None
    }
}

impl<'a, Token: Copy> Iterator for TokenIter<'a, Token> {
    type Item = &'a(Token, String);

    fn next(&mut self) -> Option<Self::Item> {
        let next_element = self.tokens.get(self.i);
        self.i += 1;
        next_element
    }
}

pub trait ASTNodeSimple<Token: Copy> {
    fn parse_impl(tokens: &mut TokenIter<Token>) -> Option<Self>
    where
        Self: Sized;
}

impl<Token: Copy, T: ASTNodeSimple<Token>> ASTNode<Token> for T {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>>
    where
        Self: Sized,
    {
        vec![|tokens| Self::parse_impl(tokens)]
    }
}

pub trait ASTNode<Token: Copy> {
    fn parse(tokens: &mut TokenIter<Token>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut res = (None, *tokens);
        for parse_impl in Self::parse_impls() {
            let mut tokens_cpy = *tokens;

            let current_res = parse_impl(&mut tokens_cpy);

            if current_res.is_some() {
                let len = TokenIter::distance(&res.1, &tokens_cpy);
                if let Some(len) = len {
                    if len > 0 {
                        res = (current_res, tokens_cpy);
                    }
                }
            }
        }

        *tokens = res.1;
        res.0
    }

    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>>
    where
        Self: Sized;
}

pub trait ToRefVec<T> {
    fn to_ref_vec(&self) -> Vec<&T> {
        let mut vec = Vec::new();
        let mut node = Some(self);
        while let Some(current) = node {
            let (next, item) = current.next();
            node = next;
            vec.push(item);
        }

        vec
    }

    fn next(&self) -> (Option<&Self>, &T);
}

pub trait ToVec<T> {
    fn to_vec(&self) -> Vec<T>
    where Self: Sized {
        let mut vec = Vec::new();
        let mut node = Some(self as &dyn ToVec<T>);
        while let Some(current) = node {
            let (next, item) = current.next();
            node = next;
            vec.push(item);
        }

        vec
    }

    fn next(&self) -> (Option<&dyn ToVec<T>>, T);
}
