//! Scan for the end-of-statement `.` without stealing a later statement's period (ABAP is
//! line-oriented; a missing `.` must not bind to the next physical line's terminator).

use abap_lexer::{Token, TokenKind, have_space_between};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Delimiter {
    Paren,
    Bracket,
    Brace,
}

impl Delimiter {
    fn open_text(self) -> &'static str {
        match self {
            Delimiter::Paren => "(",
            Delimiter::Bracket => "[",
            Delimiter::Brace => "{",
        }
    }

    fn close_text(self) -> &'static str {
        match self {
            Delimiter::Paren => ")",
            Delimiter::Bracket => "]",
            Delimiter::Brace => "}",
        }
    }
}

#[inline]
pub(crate) fn token_begins_line(tok: &Token) -> bool {
    tok.range.start == 0 || tok.has_newline_before()
}

#[inline]
pub(crate) fn line_start_assignment(tokens: &[Token], idx: usize) -> bool {
    tokens.get(idx).is_some_and(|token| {
        token.kind == TokenKind::Ident
            && token_begins_line(token)
            && matches!(
                tokens.get(idx + 1).map(|next| next.kind),
                Some(TokenKind::Eq | TokenKind::QuestionEq)
            )
    })
}

#[inline]
pub(crate) fn compact_dynamic_selector_lparen(prev: &Token, lparen: &Token) -> bool {
    matches!(prev.kind, TokenKind::Arrow | TokenKind::FatArrow) && !have_space_between(prev, lparen)
}

#[inline]
pub(crate) fn has_non_comment_tokens(tokens: &[Token], start: usize, end_exclusive: usize) -> bool {
    tokens
        .get(start..end_exclusive)
        .unwrap_or(&[])
        .iter()
        .any(|token| token.kind != TokenKind::Comment)
}

pub(crate) fn delimiter_error(
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<crate::ParseError> {
    let mut stack: Vec<(Delimiter, &Token)> = Vec::new();
    for token in tokens.get(start..end_exclusive).unwrap_or(&[]) {
        let closing = match token.kind {
            TokenKind::LParen => {
                stack.push((Delimiter::Paren, token));
                continue;
            }
            TokenKind::LBracket => {
                stack.push((Delimiter::Bracket, token));
                continue;
            }
            TokenKind::LBrace => {
                stack.push((Delimiter::Brace, token));
                continue;
            }
            TokenKind::RParen => Some(Delimiter::Paren),
            TokenKind::RBracket => Some(Delimiter::Bracket),
            TokenKind::RBrace => Some(Delimiter::Brace),
            TokenKind::Comment => continue,
            _ => None,
        };
        let Some(closing) = closing else {
            continue;
        };
        if !matches!(stack.last(), Some((open, _)) if *open == closing) {
            return Some(crate::ParseError {
                message: format!("syntax error: unmatched closing '{}'", closing.close_text()),
                range: token.range.clone(),
            });
        }
        stack.pop();
    }

    stack.first().map(|(open, token)| crate::ParseError {
        message: format!("syntax error: unclosed '{}' in statement", open.open_text()),
        range: token.range.clone(),
    })
}

#[inline]
fn is_inline_data_start(tokens: &[Token], idx: usize) -> bool {
    tokens.get(idx + 1).map(|tok| tok.kind) == Some(TokenKind::LParen)
}

#[inline]
fn is_inline_field_symbol_start(tokens: &[Token], idx: usize) -> bool {
    tokens.get(idx + 1).map(|tok| tok.kind) == Some(TokenKind::Minus)
        && tokens
            .get(idx + 2)
            .is_some_and(|tok| tok.kind == TokenKind::Ident)
        && tokens.get(idx + 3).map(|tok| tok.kind) == Some(TokenKind::LParen)
}

#[inline]
pub(crate) fn is_inline_decl_continuation(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(tok) = tokens.get(idx) else {
        return false;
    };
    tok.kind == TokenKind::Ident
        && ((tok.lexeme(source).eq_ignore_ascii_case("DATA") && is_inline_data_start(tokens, idx))
            || (tok.lexeme(source).eq_ignore_ascii_case("FIELD")
                && is_inline_field_symbol_start(tokens, idx)))
}

#[rustfmt::skip]
const DEFINITE_STMT_LEAD_KEYWORDS: &[&str] = &["DATA", "STATICS", "FIELD", "REPORT", "INCLUDE", "LOAD", "START", "END", "TOP", "START-OF-SELECTION", "END-OF-SELECTION", "INITIALIZATION", "TOP-OF-PAGE", "END-OF-PAGE", "IF", "ELSEIF", "ELSE", "ENDIF", "ASSERT", "CHECK", "CASE", "WHEN", "ENDCASE", "WRITE", "CONCATENATE", "CONDENSE", "CLASS", "ENDCLASS", "INTERFACE", "ENDINTERFACE", "METHOD", "ENDMETHOD", "READ", "INSERT", "DELETE", "SYNTAX", "AUTHORITY", "MOVE", "COMPUTE", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "TRANSLATE", "SHIFT", "SEARCH", "OVERLAY", "PACK", "UNPACK", "SKIP", "ULINE", "RESERVE", "BACK", "FORMAT", "POSITION", "HIDE", "SUPPRESS", "SORT", "SELECT", "ENDSELECT", "EXEC", "ENDEXEC", "OPEN", "FETCH", "CLOSE", "FORM", "ENDFORM", "PERFORM", "LOOP", "ENDLOOP", "WHILE", "ENDWHILE", "DO", "ENDDO", "DEFINE", "CONTINUE", "EXIT", "RETURN", "STOP", "TRY", "CATCH", "CLEANUP", "ENDTRY", "ENDCATCH", "TYPES", "CONSTANTS", "FIELD-SYMBOLS", "PARAMETER", "PARAMETERS", "SELECTION", "TABLES", "FUNCTION", "ENDFUNCTION", "MODULE", "ENDMODULE", "ENHANCEMENT", "ENDENHANCEMENT"];
const NAMED_ARG_CLAUSE_KEYWORDS: &[&str] = &[
    "EXPORTING",
    "IMPORTING",
    "CHANGING",
    "RECEIVING",
    "EXCEPTIONS",
];
const CONDITION_CONTINUATION_KEYWORDS: &[&str] = &["AND", "OR", "NOT", "WHERE", "HAVING", "ON"];
#[rustfmt::skip]
const CONDITION_COMPARISON_KEYWORDS: &[&str] = &["EQ", "NE", "LT", "LE", "GT", "GE", "CP", "NP", "CO", "CN", "CA", "NA", "CS", "NS", "IS", "IN", "BETWEEN", "LIKE"];

#[inline]
pub(crate) fn keyword_any(text: &str, keywords: &[&str]) -> bool {
    keywords.iter().any(|kw| text.eq_ignore_ascii_case(kw))
}

/// Keywords that almost always start a new compilation-unit statement at the beginning of a line.
#[inline]
pub(crate) fn is_definite_stmt_lead_keyword(source: &str, tok: &Token) -> bool {
    if tok.kind != TokenKind::Ident {
        return false;
    }
    let text = tok.lexeme(source);
    keyword_any(text, DEFINITE_STMT_LEAD_KEYWORDS)
}

#[inline]
pub(crate) fn is_named_arg_clause_keyword(source: &str, tok: &Token) -> bool {
    if tok.kind != TokenKind::Ident {
        return false;
    }
    let text = tok.lexeme(source);
    keyword_any(text, NAMED_ARG_CLAUSE_KEYWORDS)
}

#[inline]
pub(crate) fn is_condition_continuation_keyword(source: &str, tok: &Token) -> bool {
    if tok.kind != TokenKind::Ident {
        return false;
    }
    let text = tok.lexeme(source);
    keyword_any(text, CONDITION_CONTINUATION_KEYWORDS)
}

#[inline]
fn token_is_keyword(source: &str, tok: &Token, keyword: &str) -> bool {
    tok.kind == TokenKind::Ident && tok.lexeme(source).eq_ignore_ascii_case(keyword)
}

pub(crate) fn previous_non_comment_token(tokens: &[Token], before: usize) -> Option<usize> {
    let mut idx = before.checked_sub(1)?;
    loop {
        if tokens
            .get(idx)
            .is_some_and(|token| token.kind != TokenKind::Comment)
        {
            return Some(idx);
        }
        idx = idx.checked_sub(1)?;
    }
}

pub(crate) fn skip_comment_tokens_until(
    tokens: &[Token],
    mut idx: usize,
    end_exclusive: usize,
) -> usize {
    while idx < end_exclusive
        && tokens
            .get(idx)
            .is_some_and(|token| token.kind == TokenKind::Comment)
    {
        idx += 1;
    }
    idx
}

fn skip_comment_tokens(tokens: &[Token], idx: usize) -> usize {
    skip_comment_tokens_until(tokens, idx, tokens.len())
}

#[inline]
fn statement_lead_matches(source: &str, tokens: &[Token], start: usize, keyword: &str) -> bool {
    tokens
        .get(start)
        .is_some_and(|tok| token_is_keyword(source, tok, keyword))
        || previous_non_comment_token(tokens, start)
            .and_then(|idx| tokens.get(idx))
            .is_some_and(|tok| token_is_keyword(source, tok, keyword))
}

pub(crate) fn find_top_level_keyword_index(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    keyword: &str,
) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            idx += 1;
            continue;
        }
        if paren == 0 && bracket == 0 && brace == 0 && token_is_keyword(source, token, keyword) {
            return Some(idx);
        }
        match token.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            _ => {}
        }
        idx += 1;
    }
    None
}

#[inline]
pub(crate) fn is_perform_if_found_addition(
    source: &str,
    tokens: &[Token],
    start: usize,
    idx: usize,
) -> bool {
    tokens
        .get(start)
        .is_some_and(|tok| token_is_keyword(source, tok, "perform"))
        && tokens
            .get(idx)
            .is_some_and(|tok| token_is_keyword(source, tok, "if"))
        && tokens
            .get(idx + 1)
            .is_some_and(|tok| token_is_keyword(source, tok, "found"))
}

#[inline]
pub(crate) fn is_signature_addition(
    source: &str,
    tokens: &[Token],
    start: usize,
    idx: usize,
) -> bool {
    (statement_lead_matches(source, tokens, start, "perform")
        || statement_lead_matches(source, tokens, start, "form"))
        && tokens.get(idx).is_some_and(|tok| {
            token_is_keyword(source, tok, "tables")
                || token_is_keyword(source, tok, "using")
                || token_is_keyword(source, tok, "changing")
        })
}

pub(crate) fn statement_starts_chained_methods_decl(
    source: &str,
    tokens: &[Token],
    start: usize,
) -> bool {
    if tokens
        .get(start)
        .is_some_and(|tok| token_is_keyword(source, tok, "methods"))
    {
        let next = skip_comment_tokens(tokens, start + 1);
        return tokens
            .get(next)
            .is_some_and(|tok| tok.kind == TokenKind::Colon);
    }

    if !tokens
        .get(start)
        .is_some_and(|tok| token_is_keyword(source, tok, "class"))
        || tokens.get(start + 1).map(|tok| tok.kind) != Some(TokenKind::Minus)
        || !tokens
            .get(start + 2)
            .is_some_and(|tok| token_is_keyword(source, tok, "methods"))
    {
        return false;
    }
    let next = skip_comment_tokens(tokens, start + 3);
    tokens
        .get(next)
        .is_some_and(|tok| tok.kind == TokenKind::Colon)
}

pub(crate) fn is_chained_methods_entry_after_separator(tokens: &[Token], idx: usize) -> bool {
    previous_non_comment_token(tokens, idx)
        .and_then(|prev| tokens.get(prev))
        .is_some_and(|tok| matches!(tok.kind, TokenKind::Colon | TokenKind::Comma))
}

fn statement_starts_authority_check(source: &str, tokens: &[Token], start: usize) -> bool {
    let Some(check_idx) = previous_non_comment_token(tokens, start) else {
        return false;
    };
    let Some(minus_idx) = previous_non_comment_token(tokens, check_idx) else {
        return false;
    };
    let Some(authority_idx) = previous_non_comment_token(tokens, minus_idx) else {
        return false;
    };

    tokens
        .get(authority_idx)
        .is_some_and(|tok| token_is_keyword(source, tok, "authority"))
        && tokens
            .get(minus_idx)
            .is_some_and(|tok| tok.kind == TokenKind::Minus)
        && tokens
            .get(check_idx)
            .is_some_and(|tok| token_is_keyword(source, tok, "check"))
}

pub(crate) fn is_authority_check_field_continuation(
    source: &str,
    tokens: &[Token],
    start: usize,
    idx: usize,
) -> bool {
    tokens
        .get(idx)
        .is_some_and(|tok| token_is_keyword(source, tok, "field"))
        && statement_starts_authority_check(source, tokens, start)
}

#[inline]
fn is_condition_comparison_keyword(source: &str, tok: &Token) -> bool {
    if tok.kind != TokenKind::Ident {
        return false;
    }
    let text = tok.lexeme(source);
    keyword_any(text, CONDITION_COMPARISON_KEYWORDS)
}

#[inline]
fn is_condition_comparison_operator(source: &str, tok: &Token) -> bool {
    matches!(
        tok.kind,
        TokenKind::Eq
            | TokenKind::QuestionEq
            | TokenKind::Lt
            | TokenKind::Le
            | TokenKind::Gt
            | TokenKind::Ge
            | TokenKind::Ne
    ) || is_condition_comparison_keyword(source, tok)
}

pub(crate) fn line_start_condition_operand_continues(
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> bool {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut i = idx + 1;
    while let Some(tok) = tokens.get(i) {
        if tok.has_newline_before() || tok.kind == TokenKind::Eof {
            return false;
        }
        if paren == 0 && bracket == 0 && brace == 0 {
            if matches!(
                tok.kind,
                TokenKind::Period | TokenKind::Comma | TokenKind::Colon
            ) {
                return false;
            }
            if is_condition_comparison_operator(source, tok) {
                return true;
            }
        }
        match tok.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen if paren > 0 => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket if bracket > 0 => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace if brace > 0 => brace -= 1,
            _ => {}
        }
        i += 1;
    }
    false
}

pub(crate) fn line_start_table_key_component_continues(tokens: &[Token], idx: usize) -> bool {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut i = idx + 1;
    while let Some(tok) = tokens.get(i) {
        if tok.has_newline_before() || tok.kind == TokenKind::Eof {
            return false;
        }
        if paren == 0 && bracket == 0 && brace == 0 {
            if matches!(
                tok.kind,
                TokenKind::Period | TokenKind::Comma | TokenKind::Colon
            ) {
                return false;
            }
            if matches!(tok.kind, TokenKind::Eq | TokenKind::QuestionEq) {
                return true;
            }
        }
        match tok.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen if paren > 0 => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket if bracket > 0 => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace if brace > 0 => brace -= 1,
            _ => {}
        }
        i += 1;
    }
    false
}

#[inline]
pub(crate) fn line_start_named_arg_continues(tokens: &[Token], idx: usize) -> bool {
    matches!(
        tokens.get(idx + 1).map(|token| token.kind),
        Some(TokenKind::Eq | TokenKind::QuestionEq)
    )
}

pub(crate) fn starts_with_table_key_clause(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(with_tok) = tokens.get(idx) else {
        return false;
    };
    if !token_is_keyword(source, with_tok, "with") {
        return false;
    }
    let mut j = skip_comment_tokens(tokens, idx + 1);
    if tokens
        .get(j)
        .is_some_and(|tok| token_is_keyword(source, tok, "table"))
    {
        j = skip_comment_tokens(tokens, j + 1);
    }
    tokens
        .get(j)
        .is_some_and(|tok| token_is_keyword(source, tok, "key"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StmtPeriodScan {
    Found(usize),
    /// No `.` before EOF or before a token that begins another statement.
    Unterminated {
        end_exclusive: usize,
    },
}

pub(crate) fn unterminated_err_end(
    tokens: &[Token],
    end_exclusive: usize,
    fallback_start: usize,
) -> usize {
    if end_exclusive == 0 {
        return fallback_start;
    }
    match tokens.get(end_exclusive) {
        Some(token) if token.kind == TokenKind::Eof => tokens
            .get(end_exclusive.saturating_sub(1))
            .map(|t| t.range.end)
            .unwrap_or(fallback_start),
        Some(token) => token.range.start,
        None => tokens.last().map(|t| t.range.end).unwrap_or(fallback_start),
    }
}
