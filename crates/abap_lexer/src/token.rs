//! Token kinds, trivia, and spans. Byte offsets use the standard library half-open
//! [`TextRange`] (`std::ops::Range<usize>`), matching Rust string slicing
//! (`source[r.start..r.end]`).

use std::ops::Range;

/// Byte offsets into the source string: `start` inclusive, `end` exclusive.
pub type TextRange = Range<usize>;

const TOKEN_FLAG_HAS_NEWLINE_BEFORE: u8 = 1 << 0;
const TOKEN_FLAG_HAS_TRAILING_INLINE_COMMENT: u8 = 1 << 1;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TriviaKind {
    Whitespace,
    Newline,
    Comment,
    Pragma,
}

impl TriviaKind {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Whitespace => "Whitespace",
            Self::Newline => "Newline",
            Self::Comment => "Comment",
            Self::Pragma => "Pragma",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TriviaPiece {
    pub kind: TriviaKind,
    pub range: TextRange,
}

impl TriviaPiece {
    pub fn lexeme<'a>(&self, source: &'a str) -> &'a str {
        source.get(self.range.clone()).unwrap_or("")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TriviaSpan {
    start: u32,
    end: u32,
}

impl TriviaSpan {
    pub const fn empty() -> Self {
        Self { start: 0, end: 0 }
    }

    pub const fn from_usize(start: usize, end: usize) -> Self {
        Self {
            start: start as u32,
            end: end as u32,
        }
    }

    pub const fn as_range(self) -> Range<usize> {
        self.start as usize..self.end as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: TextRange,
    index: u32,
    leading_trivia: TriviaSpan,
    trailing_trivia: TriviaSpan,
    flags: u8,
}

impl Token {
    pub fn new(kind: TokenKind, range: TextRange) -> Self {
        Self {
            kind,
            range,
            index: u32::MAX,
            leading_trivia: TriviaSpan::empty(),
            trailing_trivia: TriviaSpan::empty(),
            flags: 0,
        }
    }

    pub fn lexeme<'a>(&self, source: &'a str) -> &'a str {
        source.get(self.range.clone()).unwrap_or("")
    }

    pub fn index(&self) -> usize {
        self.index as usize
    }

    pub fn leading_trivia_span(&self) -> Range<usize> {
        self.leading_trivia.as_range()
    }

    pub fn trailing_trivia_span(&self) -> Range<usize> {
        self.trailing_trivia.as_range()
    }

    pub fn has_newline_before(&self) -> bool {
        self.flags & TOKEN_FLAG_HAS_NEWLINE_BEFORE != 0
    }

    pub fn has_trailing_inline_comment(&self) -> bool {
        self.flags & TOKEN_FLAG_HAS_TRAILING_INLINE_COMMENT != 0
    }

    pub(crate) fn set_index(&mut self, index: usize) {
        self.index = index as u32;
    }

    pub(crate) fn set_leading_trivia(&mut self, span: TriviaSpan, has_newline_before: bool) {
        self.leading_trivia = span;
        if has_newline_before {
            self.flags |= TOKEN_FLAG_HAS_NEWLINE_BEFORE;
        } else {
            self.flags &= !TOKEN_FLAG_HAS_NEWLINE_BEFORE;
        }
    }

    pub(crate) fn set_trailing_trivia(&mut self, span: TriviaSpan, has_inline_comment: bool) {
        self.trailing_trivia = span;
        if has_inline_comment {
            self.flags |= TOKEN_FLAG_HAS_TRAILING_INLINE_COMMENT;
        } else {
            self.flags &= !TOKEN_FLAG_HAS_TRAILING_INLINE_COMMENT;
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexedSource {
    pub tokens: std::sync::Arc<[Token]>,
    pub trivia: std::sync::Arc<[TriviaPiece]>,
}

impl LexedSource {
    pub fn token(&self, index: usize) -> Option<&Token> {
        self.tokens.get(index)
    }

    pub fn trivia_piece(&self, index: usize) -> Option<&TriviaPiece> {
        self.trivia.get(index)
    }

    pub fn leading_trivia<'a>(&'a self, token: &'a Token) -> &'a [TriviaPiece] {
        &self.trivia[token.leading_trivia_span()]
    }

    pub fn trailing_trivia<'a>(&'a self, token: &'a Token) -> &'a [TriviaPiece] {
        &self.trivia[token.trailing_trivia_span()]
    }

    pub fn leading_comments<'a>(&'a self, token: &'a Token) -> impl Iterator<Item = &'a TriviaPiece> + 'a {
        self.leading_trivia(token)
            .iter()
            .filter(|piece| matches!(piece.kind, TriviaKind::Comment | TriviaKind::Pragma))
    }

    pub fn trailing_comments<'a>(
        &'a self,
        token: &'a Token,
    ) -> impl Iterator<Item = &'a TriviaPiece> + 'a {
        self.trailing_trivia(token)
            .iter()
            .filter(|piece| matches!(piece.kind, TriviaKind::Comment | TriviaKind::Pragma))
    }

    pub fn trailing_inline_comment<'a>(&'a self, token: &'a Token) -> Option<&'a TriviaPiece> {
        self.trailing_comments(token).next()
    }

    pub fn has_newline_before(&self, token: &Token) -> bool {
        token.has_newline_before()
    }

    pub fn has_space_between(&self, lhs: &Token, rhs: &Token) -> bool {
        have_space_between(lhs, rhs)
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
