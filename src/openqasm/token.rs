//! Defines all the possible tokens in openqasm 2.0.

/// An enum containing all the possible tokens in openqasm 2.0
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    OPENQASM,
    Include,
    QReg,
    CReg,
    Gate,
    Opaque,
    U,
    CX,
    Measure,
    Reset,
    If,
    Barrier,
    Semicolon,
    Comma,
    Dot,
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
    Plus,
    Minus,
    Mul,
    Div,
    Pow,
    Sin,
    Cos,
    Tan,
    Exp,
    Ln,
    Sqrt,
    Pi,
    Arrow,
    Equal,
    Int,
    Number,
    Str,
    Identifier,
}

/// Tokenizing an openqasm file returns a list of [TokenMatch]es. It is
/// a tuple where the first element is the type of token found. The second
/// is the String that was matched in the openqasm file. For keywords like
/// [OPENQASM](Token::OPENQASM) or [If](Token::If) the String is unecessary as it will always be the constant
/// string matching the keyword. For [Str](Token::Str), [Identifier](Token::Identifier), [Number](Token::Number) and [Int](Token::Int) however
/// there are different strings that can match to that tokentype.
pub type TokenMatch = (Token, String);
