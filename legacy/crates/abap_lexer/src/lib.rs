mod lexer;
mod token;

pub use lexer::{LexError, Lexer, TokenizeResult, tokenize};
pub use token::{
    LexedSource, TextRange, Token, TokenKind, TriviaKind, TriviaPiece, TriviaSpan,
    have_space_between, range_between,
};
