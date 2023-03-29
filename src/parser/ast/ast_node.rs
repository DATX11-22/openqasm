//! Contains code related to creating ASTs for the language.
//! When creating an AST node it should implement either [ASTNode] or [ASTNodeSimple].
//! This requires defining a list of parse rules for each AST node using [parse_impls](ASTNode::parse_impls).

/// The iterator type used for iterating over a list of tokens when parsing an
/// AST. This type was used over the default Vec iterator because this makes it simpler
/// to, for example, compare the iterators.
#[derive(Copy, Clone)]
pub struct TokenIter<'a, Token: Copy> {
    tokens: &'a Vec<(Token, String)>,
    i: usize,
}

impl<'a, Token: Copy> TokenIter<'a, Token> {
    pub fn create(tokens: &'a Vec<(Token, String)>) -> Self {
        TokenIter { tokens, i: 0 }
    }

    /// Calculates the 'distance' between two iterators. This comparison
    /// is only meaningful if the two iterators iterate over the same list.
    /// If the first iterator is further along the list than the second the result is negative.
    ///
    /// This condition is therefore checked first.
    pub fn distance(from: &Self, to: &Self) -> Option<i32> {
        if from.tokens.as_ptr() == to.tokens.as_ptr() {
            return Some(to.i as i32 - from.i as i32);
        }
        None
    }
}

impl<'a, Token: Copy> Iterator for TokenIter<'a, Token> {
    type Item = &'a (Token, String);

    fn next(&mut self) -> Option<Self::Item> {
        let next_element = self.tokens.get(self.i);
        self.i += 1;
        next_element
    }
}

/// A tool used to implement [ASTNode] for types which only have one parse implementation
/// in a slightly cleaner way.
pub trait ASTNodeSimple<Token: Copy> {
    fn parse_impl(tokens: &mut TokenIter<Token>) -> Option<Self>
    where
        Self: Sized;
}

/// All types which implement [ASTNodeSimple] also implement [ASTNode]
/// (As long as the token type used is copyable).
impl<Token: Copy, T: ASTNodeSimple<Token>> ASTNode<Token> for T {
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>>
    where
        Self: Sized,
    {
        vec![|tokens| Self::parse_impl(tokens)]
    }
}

/// The main trait used for specifying an AST node. In order to implement
/// this trait it is required to implement [parse_impls](ASTNode::parse_impls) which
/// specify the parse rules for creating the AST node.
///
/// The parse rules take a [TokenIter] and returns the AST node if parsing
/// was sucessfull.
/// The parse rules can use the [parse](ASTNode::parse) function for some other
/// AST node or itself in order to parse recursive grammars.
/// All the different parse rules are tested when parsing an AST, and the one which
/// parses the longest sequence of tokens is selected. If multiple parse rules
pub trait ASTNode<Token: Copy> {

    /// The function used to parse an AST node.
    ///
    /// The supplied token iterator will be advanced to after the last token parsed
    /// if parsing was successful. Otherwise the token iterator will not be changed.
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

    /// This needs to be implemented when implementing [ASTNode] for some type.
    fn parse_impls() -> Vec<fn(&mut TokenIter<Token>) -> Option<Self>>
    where
        Self: Sized;
}

/// Trait for converting an type into a vector of references to some contained type.
/// This is useful for AST nodes which represent a list like structure.
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

/// Alternative to [ToRefVec] where the retuned vector contains owned types instead of
/// references. This might be worse for performance depending on the type, but might be
/// necessary under certain circumstances.
pub trait ToVec<T> {
    fn to_vec(&self) -> Vec<T>
    where
        Self: Sized,
    {
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
