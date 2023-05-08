//! Code for converting a list of tokens into an AST.

use self::ast_node::{ASTNode, TokenIter};

pub mod ast_debug;
pub mod ast_node;

/// The error type returned when parsing an AST.
#[derive(Debug)]
pub enum ParseError {
    SyntaxError,
}

/// The function used to parse an AST from a list of tokens.
pub fn parse<RootToken: ASTNode<Token>, Token: Copy>(
    tokens: &Vec<(Token, String)>,
) -> Result<RootToken, ParseError> {
    let mut iter = TokenIter::create(tokens);

    // Parse the AST starting from the root token, return error if unsuccessful.
    // iter is updated to the last position where parsing was unsuccessful.
    let res = RootToken::parse(&mut iter).ok_or(ParseError::SyntaxError)?;

    // Make sure that the entire sequence of token has been parsed correctly
    if iter.next().is_some() {
        return Err(ParseError::SyntaxError);
    }

    Ok(res)
}
