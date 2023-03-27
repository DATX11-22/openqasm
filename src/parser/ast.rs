use self::ast_node::{ASTNode, TokenIter};

pub mod ast_debug;
pub mod ast_node;

#[derive(Debug)]
pub enum ParseError {
    SyntaxError,
}

pub fn parse<RootToken: ASTNode<Token>, Token: Copy>(
    tokens: &Vec<(Token, String)>,
) -> Result<RootToken, ParseError> {
    let mut iter = TokenIter::create(&tokens);

    // Parse the AST starting from the root token, return error if unsuccessful.
    // iter is updated to the last position where parsing was unsuccessful.
    let res = RootToken::parse(&mut iter).ok_or(ParseError::SyntaxError)?;

    // Make sure that the entire sequence of token has been parsed correctly
    if iter.next().is_some() {
        return Err(ParseError::SyntaxError);
    }

    Ok(res)
}
