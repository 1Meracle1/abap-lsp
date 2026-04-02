//! Token kinds and spans. Byte offsets use the standard library half-open [`TextRange`]
//! (`std::ops::Range<usize>`), matching Rust string slicing (`source[r.start..r.end]`).

use std::ops::Range;

/// Byte offsets into the source string: `start` inclusive, `end` exclusive.
pub type TextRange = Range<usize>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Ident,
    Number,
    Comment,
    /// `'...'`
    String,
    /// Delimiter `|` of an ABAP character string template (`|literal { expr }|`).
    StringTemplate,
    /// Literal text run inside a string template (between `|`…`{`, between `}`…`{`, and before closing `|`).
    StringTemplateLit,
    Period,
    Comma,
    Colon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Arrow,
    FatArrow,
    Tilde,
    Hash,
    At,
    Eq,
    Minus,
    Plus,
    Star,
    Slash,
    Lt,
    Gt,
    Le,
    Ge,
    Ne,
    QuestionEq,
    Pipe,
    Ampersand,
    Other,
    Eof,
}

impl TokenKind {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Ident => "Ident",
            Self::Number => "Number",
            Self::Comment => "Comment",
            Self::String => "String",
            Self::StringTemplate => "StringTemplate",
            Self::StringTemplateLit => "StringTemplateLit",
            Self::Period => "Period",
            Self::Comma => "Comma",
            Self::Colon => "Colon",
            Self::LParen => "LParen",
            Self::RParen => "RParen",
            Self::LBrace => "LBrace",
            Self::RBrace => "RBrace",
            Self::LBracket => "LBracket",
            Self::RBracket => "RBracket",
            Self::Arrow => "Arrow",
            Self::FatArrow => "FatArrow",
            Self::Tilde => "Tilde",
            Self::Hash => "Hash",
            Self::At => "At",
            Self::Eq => "Eq",
            Self::Minus => "Minus",
            Self::Plus => "Plus",
            Self::Star => "Star",
            Self::Slash => "Slash",
            Self::Lt => "Lt",
            Self::Gt => "Gt",
            Self::Le => "Le",
            Self::Ge => "Ge",
            Self::Ne => "Ne",
            Self::QuestionEq => "QuestionEq",
            Self::Pipe => "Pipe",
            Self::Ampersand => "Ampersand",
            Self::Other => "Other",
            Self::Eof => "Eof",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: TextRange,
}

impl Token {
    pub fn lexeme<'a>(&self, source: &'a str) -> &'a str {
        source.get(self.range.clone()).unwrap_or("")
    }
}

#[inline]
pub fn have_space_between(lhs: &Token, rhs: &Token) -> bool {
    lhs.range.end < rhs.range.start
}

#[inline]
pub fn range_between(lhs: &Token, rhs: &Token) -> TextRange {
    let start = lhs.range.end.min(rhs.range.start);
    let end = lhs.range.end.max(rhs.range.start);
    start..end
}
