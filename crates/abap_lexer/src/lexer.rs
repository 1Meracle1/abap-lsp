use std::collections::VecDeque;

use unicode_general_category::{GeneralCategory, get_general_category};

use crate::token::{TextRange, Token, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub range: TextRange,
    pub message: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenizeResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<LexError>,
}

pub fn tokenize(source: &str) -> TokenizeResult {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.scan();
        let done = token.kind == TokenKind::Eof;
        tokens.push(token);
        if done {
            break;
        }
    }
    TokenizeResult {
        tokens,
        errors: lexer.errors,
    }
}

pub struct Lexer<'a> {
    src: &'a str,
    bytes: &'a [u8],
    /// Byte index of the start of `ch`.
    pos: usize,
    /// Byte index immediately after `ch`.
    read_pos: usize,
    ch: Option<char>,
    line_start: usize,
    /// Queued tokens produced ahead of the cursor (e.g. inside `|...|` string templates).
    pending: VecDeque<Token>,
    pub errors: Vec<LexError>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut lexer = Self {
            src,
            bytes: src.as_bytes(),
            pos: 0,
            read_pos: 0,
            ch: Some(' '),
            line_start: 0,
            pending: VecDeque::new(),
            errors: Vec::new(),
        };
        lexer.advance();
        if lexer.ch == Some('\u{FEFF}') {
            lexer.advance();
        }
        lexer
    }

    pub fn scan(&mut self) -> Token {
        if let Some(token) = self.pending.pop_front() {
            return token;
        }

        self.skip_whitespace();
        let start = self.pos;

        let Some(ch) = self.ch else {
            return Token {
                kind: TokenKind::Eof,
                range: start..self.pos,
            };
        };

        if ch == '|' {
            return self.lex_string_template(start);
        }

        self.scan_next_token()
    }

    /// Rest of [`Self::scan`] after whitespace, `|...|`, and `EOF` are ruled out.
    fn scan_next_token(&mut self) -> Token {
        let start = self.pos;

        let Some(ch) = self.ch else {
            return Token {
                kind: TokenKind::Eof,
                range: start..self.pos,
            };
        };

        if ch == '/' {
            if self.is_namespace_start() {
                self.scan_identifier();
                return Token {
                    kind: TokenKind::Ident,
                    range: start..self.pos,
                };
            }
            self.advance();
            return Token {
                kind: TokenKind::Slash,
                range: start..self.pos,
            };
        }

        if ch == '<' && self.is_field_symbol_identifier_start() {
            self.scan_field_symbol_identifier();
            return Token {
                kind: TokenKind::Ident,
                range: start..self.pos,
            };
        }

        if is_letter(ch) {
            self.scan_identifier();
            return Token {
                kind: TokenKind::Ident,
                range: start..self.pos,
            };
        }

        if ('0'..='9').contains(&ch) {
            self.scan_number();
            return Token {
                kind: TokenKind::Number,
                range: start..self.pos,
            };
        }

        // Full-line `*` comments: first non-whitespace character on the line is `*`.
        // `"` starts a comment that runs to end of line (ABAP; same as SAP editor after `"`).
        if (ch == '*' && self.line_leading_trivia_is_whitespace_only()) || ch == '"' {
            self.scan_comment();
            return Token {
                kind: TokenKind::Comment,
                range: start..self.pos,
            };
        }

        if ch == '#' && self.peek_byte(0) == Some(b'#') {
            self.scan_pragma();
            return Token {
                kind: TokenKind::Comment,
                range: start..self.pos,
            };
        }

        // Punctuation / strings: consume the current code point first (Odin `advance` then `switch ch`).
        self.advance();
        let kind = match ch {
            '\'' => {
                self.scan_string();
                TokenKind::String
            }
            '`' => {
                self.scan_string_backtick();
                TokenKind::String
            }
            '.' => TokenKind::Period,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '+' => TokenKind::Plus,
            '*' => TokenKind::Star,
            '=' => {
                if self.ch == Some('>') {
                    self.advance();
                    TokenKind::FatArrow
                } else {
                    TokenKind::Eq
                }
            }
            '-' => {
                if self.ch == Some('>') {
                    self.advance();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '~' => TokenKind::Tilde,
            '#' => TokenKind::Hash,
            '@' => TokenKind::At,
            // `|` starts `|...|` string templates — handled in [`Self::scan`].
            '&' => TokenKind::Ampersand,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '<' => {
                if self.ch == Some('=') {
                    self.advance();
                    TokenKind::Le
                } else if self.ch == Some('>') {
                    self.advance();
                    TokenKind::Ne
                } else {
                    TokenKind::Lt
                }
            }
            '>' => {
                if self.ch == Some('=') {
                    self.advance();
                    TokenKind::Ge
                } else {
                    TokenKind::Gt
                }
            }
            '?' => {
                if self.ch == Some('=') {
                    self.advance();
                    TokenKind::QuestionEq
                } else {
                    TokenKind::Other
                }
            }
            '!' => TokenKind::Other,
            _ => TokenKind::Other,
        };

        Token {
            kind,
            range: start..self.pos,
        }
    }

    /// ABAP character string template (`|…|`): literal text with `\|`, `\{`, `\}`, `\\`, `\n`/`\r`/`\t`,
    /// and embedded `{ … }` expressions. See SAP ABAP Keyword Documentation (string templates / literals).
    fn lex_string_template(&mut self, template_start: usize) -> Token {
        debug_assert_eq!(self.ch, Some('|'));
        let mut out = Vec::new();
        let p0 = self.pos;
        let p1 = p0 + '|'.len_utf8();
        out.push(Token {
            kind: TokenKind::StringTemplate,
            range: p0..p1,
        });
        self.advance();

        loop {
            let lit_start = self.pos;
            self.consume_template_literal_fragment();
            if self.pos > lit_start {
                out.push(Token {
                    kind: TokenKind::StringTemplateLit,
                    range: lit_start..self.pos,
                });
            }

            match self.ch {
                Some('|') => {
                    let open = self.pos;
                    let close = open + '|'.len_utf8();
                    self.advance();
                    out.push(Token {
                        kind: TokenKind::StringTemplate,
                        range: open..close,
                    });
                    return self.defer_tokens(out);
                }
                Some('{') => {
                    let b0 = self.pos;
                    let b1 = b0 + '{'.len_utf8();
                    self.advance();
                    out.push(Token {
                        kind: TokenKind::LBrace,
                        range: b0..b1,
                    });
                    self.scan_embedded_expression(&mut out);
                }
                None => {
                    self.error(
                        template_start,
                        self.read_pos,
                        "string template was not terminated",
                    );
                    return self.defer_tokens(out);
                }
                Some(_) => {
                    let b = self.pos;
                    self.error(
                        b,
                        self.read_pos,
                        "unexpected character in string template literal",
                    );
                    self.advance();
                }
            }
        }
    }

    fn defer_tokens(&mut self, tokens: Vec<Token>) -> Token {
        let mut iter = tokens.into_iter();
        let first = iter.next().unwrap_or_else(|| Token {
            kind: TokenKind::Eof,
            range: self.pos..self.pos,
        });
        self.pending.extend(iter);
        first
    }

    fn scan_embedded_expression(&mut self, out: &mut Vec<Token>) {
        let mut depth = 1usize;
        while depth > 0 {
            let token = self.scan();
            if token.kind == TokenKind::Eof {
                out.push(token);
                return;
            }
            let mut batch = vec![token];
            while let Some(extra) = self.pending.pop_front() {
                batch.push(extra);
            }
            let mut done = false;
            for t in batch {
                match t.kind {
                    TokenKind::LBrace => depth += 1,
                    TokenKind::RBrace => {
                        depth = depth.saturating_sub(1);
                    }
                    _ => {}
                }
                out.push(t);
                if depth == 0 {
                    done = true;
                    break;
                }
            }
            if done {
                break;
            }
        }
    }

    /// Literal characters inside `|…|` until an unescaped `{`, closing `|`, or newline.
    fn consume_template_literal_fragment(&mut self) {
        loop {
            let Some(c) = self.ch else {
                return;
            };
            match c {
                '|' | '{' => return,
                '\n' => {
                    let b = self.pos;
                    self.error(b, self.read_pos, "unescaped newline in string template");
                    self.advance();
                    return;
                }
                '\\' => {
                    self.advance();
                    match self.ch {
                        None => {
                            self.error(
                                self.pos,
                                self.read_pos,
                                "string template escape incomplete",
                            );
                        }
                        Some('|' | '{' | '}' | '\\' | 'n' | 'r' | 't') => {
                            self.advance();
                        }
                        Some(_) => {
                            let b = self.pos;
                            self.error(b, self.read_pos, "invalid escape in string template");
                            self.advance();
                        }
                    }
                }
                _ => self.advance(),
            }
        }
    }

    fn scan_string(&mut self) {
        let start = self.pos.saturating_sub(1);
        loop {
            let Some(c) = self.ch else {
                self.error(start, self.read_pos, "string literal was not terminated");
                break;
            };
            if c == '\n' {
                self.error(start, self.read_pos, "string literal was not terminated");
                break;
            }
            self.advance();
            if c == '\'' {
                break;
            }
        }
    }

    fn scan_string_backtick(&mut self) {
        let start = self.pos.saturating_sub(1);
        loop {
            let Some(c) = self.ch else {
                self.error(start, self.read_pos, "string template was not terminated");
                break;
            };
            if c == '\n' {
                self.error(start, self.read_pos, "string template was not terminated");
                break;
            }
            if c == '`' {
                self.advance();
                if self.ch == Some('`') {
                    self.advance();
                    continue;
                }
                break;
            }
            self.advance();
        }
    }

    fn scan_comment(&mut self) {
        while let Some(c) = self.ch {
            if c == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn scan_pragma(&mut self) {
        self.advance(); // first '#'
        self.advance(); // second '#'
        while let Some(c) = self.ch {
            if is_pragma_char(c) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn scan_number(&mut self) {
        while let Some(c) = self.ch {
            if is_digit(c) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn scan_identifier(&mut self) {
        while let Some(c) = self.ch {
            if is_letter(c) || is_digit(c) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn scan_field_symbol_identifier(&mut self) {
        self.advance(); // '<'
        while let Some(c) = self.ch {
            if is_letter(c) || is_digit(c) {
                self.advance();
            } else {
                break;
            }
        }
        if self.ch == Some('>') {
            self.advance();
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.ch {
            match c {
                ' ' | '\t' | '\r' | '\n' => self.advance(),
                _ => break,
            }
        }
    }

    /// True when every byte from the start of the current line through `self.pos` is ASCII whitespace.
    /// After [`Self::skip_whitespace`], `self.pos` is the first non-whitespace on the line, so this is
    /// true iff there is only leading whitespace before the current character in the line.
    fn line_leading_trivia_is_whitespace_only(&self) -> bool {
        match self.src.get(self.line_start..self.pos) {
            None => true,
            Some(s) => s.bytes().all(|b| matches!(b, b' ' | b'\t' | b'\r')),
        }
    }

    fn advance(&mut self) {
        if self.read_pos >= self.bytes.len() {
            if self.ch == Some('\n') {
                self.line_start = self.bytes.len();
            }
            self.pos = self.bytes.len();
            self.ch = None;
            return;
        }

        if self.ch == Some('\n') {
            self.line_start = self.read_pos;
        }

        self.pos = self.read_pos;
        let Some(window) = self.src.get(self.pos..) else {
            self.error(self.pos, self.read_pos, "illegal UTF-8 encoding");
            self.read_pos = self.bytes.len();
            self.ch = None;
            return;
        };
        let Some(c) = window.chars().next() else {
            self.read_pos = self.bytes.len();
            self.ch = None;
            return;
        };

        let w = c.len_utf8();
        if c == '\0' {
            self.error(self.pos, self.pos + w, "illegal character NUL");
        }
        if c == '\u{FEFF}' && self.pos > 0 {
            self.error(self.pos, self.pos + w, "illegal byte order mark");
        }

        self.read_pos = self.pos + w;
        self.ch = Some(c);
    }

    fn peek_byte(&self, offset: usize) -> Option<u8> {
        self.bytes.get(self.read_pos + offset).copied()
    }

    fn is_namespace_start(&self) -> bool {
        if self.ch != Some('/') {
            return false;
        }
        let Some(b) = self.peek_byte(0) else {
            return false;
        };
        let next = b as char;
        next.is_ascii_alphabetic()
    }

    fn is_field_symbol_identifier_start(&self) -> bool {
        if self.ch != Some('<') {
            return false;
        }
        let after = self.src.get(self.read_pos..).unwrap_or("");
        let mut it = after.chars();
        let Some(first) = it.next() else {
            return false;
        };
        if !is_letter(first) {
            return false;
        }
        loop {
            match it.next() {
                Some(c) if is_letter(c) || is_digit(c) => continue,
                Some('>') => return true,
                _ => return false,
            }
        }
    }

    fn error(&mut self, start: usize, end: usize, message: &'static str) {
        self.errors.push(LexError {
            range: start..end,
            message,
        });
    }
}

#[inline]
fn is_unicode_letter(c: char) -> bool {
    matches!(
        get_general_category(c),
        GeneralCategory::UppercaseLetter
            | GeneralCategory::LowercaseLetter
            | GeneralCategory::TitlecaseLetter
            | GeneralCategory::ModifierLetter
            | GeneralCategory::OtherLetter
    )
}

#[inline]
fn is_letter(c: char) -> bool {
    matches!(c, '_' | '/') || c.is_ascii_alphabetic() || (c >= '\u{80}' && is_unicode_letter(c))
}

#[inline]
fn is_digit(c: char) -> bool {
    c.is_ascii_digit() || get_general_category(c) == GeneralCategory::DecimalNumber
}

#[inline]
fn is_pragma_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind;

    #[test]
    fn skips_whitespace_and_emits_eof() {
        let r = tokenize("  a  ");
        assert_eq!(
            r.tokens.iter().map(|t| t.kind).collect::<Vec<_>>(),
            vec![TokenKind::Ident, TokenKind::Eof]
        );
    }

    #[test]
    fn namespace_slash_is_ident_minus_slash_is_slash() {
        let r = tokenize("/FOO/bar");
        assert_eq!(r.tokens[0].kind, TokenKind::Ident);
        assert_eq!(r.tokens[0].lexeme("/FOO/bar"), "/FOO/bar");

        // `/` is an ident character; `a/1` is a single ident. Use spacing to lex division.
        let r2 = tokenize("a / 1");
        assert_eq!(r2.tokens[0].kind, TokenKind::Ident);
        assert_eq!(r2.tokens[1].kind, TokenKind::Slash);
        assert_eq!(r2.tokens[2].kind, TokenKind::Number);
    }

    #[test]
    fn field_symbol_is_single_ident() {
        let r = tokenize("<fs>");
        assert_eq!(r.tokens[0].kind, TokenKind::Ident);
        assert_eq!(r.tokens[0].lexeme("<fs>"), "<fs>");
    }

    #[test]
    fn line_comment_star_and_string_pragma() {
        let r = tokenize("* line\nDATA");
        assert_eq!(r.tokens[0].kind, TokenKind::Comment);
        assert_eq!(r.tokens[1].kind, TokenKind::Ident);

        let ind = tokenize("  * indented star comment\nDATA");
        assert_eq!(ind.tokens[0].kind, TokenKind::Comment);
        assert_eq!(ind.tokens[1].kind, TokenKind::Ident);

        let p = tokenize("##ENH_OK DATA");
        assert_eq!(p.tokens[0].kind, TokenKind::Comment);
        assert_eq!(p.tokens[0].lexeme("##ENH_OK DATA"), "##ENH_OK");
        assert_eq!(p.tokens[1].kind, TokenKind::Ident);
    }

    #[test]
    fn two_char_operators() {
        let r = tokenize("=>-><= <><>");
        assert_eq!(
            r.tokens.iter().map(|t| t.kind).collect::<Vec<_>>(),
            vec![
                TokenKind::FatArrow,
                TokenKind::Arrow,
                TokenKind::Le,
                TokenKind::Ne,
                TokenKind::Ne,
                TokenKind::Eof,
            ]
        );
        assert_eq!(r.errors.len(), 0);
    }

    #[test]
    fn nested_unterminated_string_records_error() {
        let r = tokenize("'open");
        assert_eq!(r.tokens[0].kind, TokenKind::String);
        assert!(!r.errors.is_empty());
    }

    #[test]
    fn string_template_segments_and_embedded_expr() {
        let src = "|Hello, { lv_name }!|";
        let r = tokenize(src);
        assert_eq!(
            r.tokens
                .iter()
                .map(|t| t.kind)
                .filter(|k| *k != TokenKind::Eof)
                .collect::<Vec<_>>(),
            vec![
                TokenKind::StringTemplate,
                TokenKind::StringTemplateLit,
                TokenKind::LBrace,
                TokenKind::Ident,
                TokenKind::RBrace,
                TokenKind::StringTemplateLit,
                TokenKind::StringTemplate,
            ]
        );
        assert_eq!(r.errors.len(), 0);
        assert_eq!(
            r.tokens
                .iter()
                .find(|t| t.kind == TokenKind::StringTemplateLit)
                .map(|t| t.lexeme(src)),
            Some("Hello, ")
        );
    }

    #[test]
    fn string_template_empty() {
        let r = tokenize("||");
        assert_eq!(
            r.tokens.iter().map(|t| t.kind).collect::<Vec<_>>(),
            vec![
                TokenKind::StringTemplate,
                TokenKind::StringTemplate,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn string_template_escapes_pipe_in_literal() {
        let src = "|a \\| b|";
        let r = tokenize(src);
        assert_eq!(r.errors.len(), 0);
        assert!(
            r.tokens
                .iter()
                .any(|t| t.kind == TokenKind::StringTemplateLit && t.lexeme(src).contains('|'))
        );
    }

    #[test]
    fn string_template_nested_pipe_template_in_embed() {
        let src = "|outer { |inner| }|";
        let r = tokenize(src);
        assert_eq!(r.errors.len(), 0);
        let kinds: Vec<_> = r.tokens.iter().map(|t| t.kind).collect();
        assert!(
            kinds
                .iter()
                .filter(|k| **k == TokenKind::StringTemplate)
                .count()
                >= 4
        );
        assert!(kinds.contains(&TokenKind::StringTemplateLit));
    }

    /// Formatting options inside `{ }` lex as normal tokens (identifiers, `=`, numbers).
    #[test]
    fn string_template_format_specifiers_tokenize() {
        let cases = [
            (
                "{ lv_amount }",
                &[TokenKind::LBrace, TokenKind::Ident, TokenKind::RBrace][..],
            ),
            (
                "{ lv_number WIDTH = 8 }",
                &[
                    TokenKind::LBrace,
                    TokenKind::Ident,
                    TokenKind::Ident,
                    TokenKind::Eq,
                    TokenKind::Number,
                    TokenKind::RBrace,
                ][..],
            ),
            (
                "{ lv_text ALIGN = left WIDTH = 15 }",
                &[
                    TokenKind::LBrace,
                    TokenKind::Ident,
                    TokenKind::Ident,
                    TokenKind::Eq,
                    TokenKind::Ident,
                    TokenKind::Ident,
                    TokenKind::Eq,
                    TokenKind::Number,
                    TokenKind::RBrace,
                ][..],
            ),
            (
                "{ lv_amount DECIMALS = 2 }",
                &[
                    TokenKind::LBrace,
                    TokenKind::Ident,
                    TokenKind::Ident,
                    TokenKind::Eq,
                    TokenKind::Number,
                    TokenKind::RBrace,
                ][..],
            ),
            (
                "{ lv_matnr ALPHA = IN }",
                &[
                    TokenKind::LBrace,
                    TokenKind::Ident,
                    TokenKind::Ident,
                    TokenKind::Eq,
                    TokenKind::Ident,
                    TokenKind::RBrace,
                ][..],
            ),
            (
                "{ lv_ts TIMESTAMP = SPACE }",
                &[
                    TokenKind::LBrace,
                    TokenKind::Ident,
                    TokenKind::Ident,
                    TokenKind::Eq,
                    TokenKind::Ident,
                    TokenKind::RBrace,
                ][..],
            ),
            (
                "{ lv_date DATE = USER }",
                &[
                    TokenKind::LBrace,
                    TokenKind::Ident,
                    TokenKind::Ident,
                    TokenKind::Eq,
                    TokenKind::Ident,
                    TokenKind::RBrace,
                ][..],
            ),
            (
                "{ sy-uzeit TIME = ISO }",
                &[
                    TokenKind::LBrace,
                    TokenKind::Ident,
                    TokenKind::Minus,
                    TokenKind::Ident,
                    TokenKind::Ident,
                    TokenKind::Eq,
                    TokenKind::Ident,
                    TokenKind::RBrace,
                ][..],
            ),
        ];
        for (embed, expected) in cases {
            let src = format!("|x {embed} y|");
            let r = tokenize(&src);
            assert_eq!(r.errors.len(), 0, "{src}");
            let slice: Vec<TokenKind> = r
                .tokens
                .iter()
                .map(|t| t.kind)
                .filter(|k| {
                    *k != TokenKind::StringTemplate
                        && *k != TokenKind::StringTemplateLit
                        && *k != TokenKind::Eof
                })
                .collect();
            assert_eq!(&slice[..], expected, "{src}");
        }
    }

    #[test]
    fn string_template_full_example_concatenation() {
        let src = "|Amount: { lv_amount DECIMALS = 2 WIDTH = 12 } EUR\\n| && \
                 |Material: { lv_matnr ALPHA = IN }\\n| && \
                 |Date: { lv_date DATE = USER }\\n| && \
                 |Time: { sy-uzeit TIME = ISO }|";
        let r = tokenize(src);
        assert_eq!(r.errors.len(), 0);
        assert!(
            r.tokens
                .iter()
                .filter(|t| t.kind == TokenKind::StringTemplate)
                .count()
                >= 8
        );
        assert!(
            r.tokens.iter().any(|t| {
                t.kind == TokenKind::StringTemplateLit && t.lexeme(src).contains("EUR")
            })
        );
    }

    #[test]
    fn string_template_multiline_sales_letter_pattern() {
        let src = "|Dear { lv_name },\\n\\n| && \
                 |Thank you for your order of { lv_qty } pieces of | && \
                 |material { lv_matnr ALPHA = IN }.\\n\\n| && \
                 |Best regards,\\nYour Sales Team|";
        let r = tokenize(src);
        assert_eq!(r.errors.len(), 0);
        let templates = r
            .tokens
            .iter()
            .filter(|t| t.kind == TokenKind::StringTemplate)
            .count();
        assert!(
            templates >= 8,
            "expected opening+closing pipes for four templates"
        );
    }

    #[test]
    fn string_template_escapes_braces_and_backslash_in_literal() {
        // SAP: `\|`, `\{`, `\}` in literal text — use a raw string so `\` stays single in source.
        let src = r"|use \{ \} and \| in literals.|";
        let r = tokenize(src);
        assert_eq!(r.errors.len(), 0);
        let lit = r
            .tokens
            .iter()
            .find(|t| t.kind == TokenKind::StringTemplateLit)
            .expect("literal");
        let text = lit.lexeme(src);
        assert!(text.contains('{'), "{text:?}");
        assert!(text.contains('}'), "{text:?}");
        assert!(text.contains('|'), "{text:?}");
    }

    #[test]
    fn string_template_escape_control_chars_in_literal() {
        let src = "|line\\nnext\\ttab|";
        let r = tokenize(src);
        assert_eq!(r.errors.len(), 0);
        assert!(
            r.tokens
                .iter()
                .any(|t| t.kind == TokenKind::StringTemplateLit && t.lexeme(src).contains('n'))
        );
    }
}
