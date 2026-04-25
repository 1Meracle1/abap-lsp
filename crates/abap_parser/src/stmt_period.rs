//! Scan for the end-of-statement `.` without stealing a later statement's period (ABAP is
//! line-oriented; a missing `.` must not bind to the next physical line's terminator).

use abap_lexer::{Token, TokenKind};

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
pub(crate) fn token_begins_line(source: &str, tok: &Token) -> bool {
    let _ = source;
    tok.range.start == 0 || tok.has_newline_before()
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
    tokens
        .get(idx)
        .is_some_and(|tok| tok.kind == TokenKind::Ident)
        && tokens.get(idx + 1).map(|tok| tok.kind) == Some(TokenKind::LParen)
}

#[inline]
fn is_inline_field_symbol_start(tokens: &[Token], idx: usize) -> bool {
    tokens
        .get(idx)
        .is_some_and(|tok| tok.kind == TokenKind::Ident)
        && tokens.get(idx + 1).map(|tok| tok.kind) == Some(TokenKind::Minus)
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

/// Keywords that almost always start a new compilation-unit statement at the beginning of a line.
#[inline]
pub(crate) fn is_definite_stmt_lead_keyword(source: &str, tok: &Token) -> bool {
    if tok.kind != TokenKind::Ident {
        return false;
    }
    let s = tok.lexeme(source);
    s.eq_ignore_ascii_case("DATA")
        || s.eq_ignore_ascii_case("STATICS")
        || s.eq_ignore_ascii_case("FIELD")
        || s.eq_ignore_ascii_case("REPORT")
        || s.eq_ignore_ascii_case("INCLUDE")
        || s.eq_ignore_ascii_case("START")
        || s.eq_ignore_ascii_case("END")
        || s.eq_ignore_ascii_case("TOP")
        || s.eq_ignore_ascii_case("START-OF-SELECTION")
        || s.eq_ignore_ascii_case("END-OF-SELECTION")
        || s.eq_ignore_ascii_case("INITIALIZATION")
        || s.eq_ignore_ascii_case("TOP-OF-PAGE")
        || s.eq_ignore_ascii_case("END-OF-PAGE")
        || s.eq_ignore_ascii_case("IF")
        || s.eq_ignore_ascii_case("ELSEIF")
        || s.eq_ignore_ascii_case("ELSE")
        || s.eq_ignore_ascii_case("ENDIF")
        || s.eq_ignore_ascii_case("ASSERT")
        || s.eq_ignore_ascii_case("CHECK")
        || s.eq_ignore_ascii_case("CASE")
        || s.eq_ignore_ascii_case("WHEN")
        || s.eq_ignore_ascii_case("ENDCASE")
        || s.eq_ignore_ascii_case("WRITE")
        || s.eq_ignore_ascii_case("CONCATENATE")
        || s.eq_ignore_ascii_case("CONDENSE")
        || s.eq_ignore_ascii_case("CLASS")
        || s.eq_ignore_ascii_case("ENDCLASS")
        || s.eq_ignore_ascii_case("INTERFACE")
        || s.eq_ignore_ascii_case("ENDINTERFACE")
        || s.eq_ignore_ascii_case("METHOD")
        || s.eq_ignore_ascii_case("ENDMETHOD")
        || s.eq_ignore_ascii_case("READ")
        || s.eq_ignore_ascii_case("AUTHORITY")
        || s.eq_ignore_ascii_case("MOVE")
        || s.eq_ignore_ascii_case("SORT")
        || s.eq_ignore_ascii_case("SELECT")
        || s.eq_ignore_ascii_case("ENDSELECT")
        || s.eq_ignore_ascii_case("OPEN")
        || s.eq_ignore_ascii_case("FETCH")
        || s.eq_ignore_ascii_case("CLOSE")
        || s.eq_ignore_ascii_case("FORM")
        || s.eq_ignore_ascii_case("ENDFORM")
        || s.eq_ignore_ascii_case("PERFORM")
        || s.eq_ignore_ascii_case("LOOP")
        || s.eq_ignore_ascii_case("ENDLOOP")
        || s.eq_ignore_ascii_case("WHILE")
        || s.eq_ignore_ascii_case("ENDWHILE")
        || s.eq_ignore_ascii_case("DO")
        || s.eq_ignore_ascii_case("ENDDO")
        || s.eq_ignore_ascii_case("CONTINUE")
        || s.eq_ignore_ascii_case("EXIT")
        || s.eq_ignore_ascii_case("RETURN")
        || s.eq_ignore_ascii_case("STOP")
        || s.eq_ignore_ascii_case("CASE")
        || s.eq_ignore_ascii_case("ENDCASE")
        || s.eq_ignore_ascii_case("TRY")
        || s.eq_ignore_ascii_case("CATCH")
        || s.eq_ignore_ascii_case("CLEANUP")
        || s.eq_ignore_ascii_case("ENDTRY")
        || s.eq_ignore_ascii_case("TYPES")
        || s.eq_ignore_ascii_case("CONSTANTS")
        || s.eq_ignore_ascii_case("FIELD-SYMBOLS")
        || s.eq_ignore_ascii_case("PARAMETER")
        || s.eq_ignore_ascii_case("PARAMETERS")
        || s.eq_ignore_ascii_case("SELECTION")
        || s.eq_ignore_ascii_case("TABLES")
        || s.eq_ignore_ascii_case("FUNCTION")
        || s.eq_ignore_ascii_case("ENDFUNCTION")
        || s.eq_ignore_ascii_case("MODULE")
        || s.eq_ignore_ascii_case("ENDMODULE")
}

#[inline]
pub(crate) fn is_named_arg_clause_keyword(source: &str, tok: &Token) -> bool {
    tok.kind == TokenKind::Ident
        && (tok.lexeme(source).eq_ignore_ascii_case("EXPORTING")
            || tok.lexeme(source).eq_ignore_ascii_case("IMPORTING")
            || tok.lexeme(source).eq_ignore_ascii_case("CHANGING")
            || tok.lexeme(source).eq_ignore_ascii_case("RECEIVING")
            || tok.lexeme(source).eq_ignore_ascii_case("EXCEPTIONS"))
}

#[inline]
pub(crate) fn is_condition_continuation_keyword(source: &str, tok: &Token) -> bool {
    tok.kind == TokenKind::Ident
        && (tok.lexeme(source).eq_ignore_ascii_case("AND")
            || tok.lexeme(source).eq_ignore_ascii_case("OR")
            || tok.lexeme(source).eq_ignore_ascii_case("NOT")
            || tok.lexeme(source).eq_ignore_ascii_case("WHERE")
            || tok.lexeme(source).eq_ignore_ascii_case("HAVING")
            || tok.lexeme(source).eq_ignore_ascii_case("ON"))
}

#[inline]
fn token_matches_keyword(source: &str, tok: &Token, keyword: &str) -> bool {
    tok.kind == TokenKind::Ident && tok.lexeme(source).eq_ignore_ascii_case(keyword)
}

#[inline]
fn is_perform_if_found_addition(source: &str, tokens: &[Token], start: usize, idx: usize) -> bool {
    tokens
        .get(start)
        .is_some_and(|tok| token_matches_keyword(source, tok, "perform"))
        && tokens
            .get(idx)
            .is_some_and(|tok| token_matches_keyword(source, tok, "if"))
        && tokens
            .get(idx + 1)
            .is_some_and(|tok| token_matches_keyword(source, tok, "found"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StmtPeriodScan {
    Found(usize),
    /// No `.` before EOF or before a token that begins another statement.
    Unterminated {
        end_exclusive: usize,
    },
}

/// From `start` (inclusive), find the first top-level `.` that terminates this statement, or decide
/// the statement ended early because another statement begins.
///
/// `start` is the first token that belongs to the syntactic unit (e.g. LHS of an assignment, or the
/// keyword of a `REPORT`/`DATA` fragment). Boundary detection only applies to positions **after**
/// `start` so the opening keyword itself is never treated as a boundary.
pub(crate) fn scan_until_statement_period(
    tokens: &[Token],
    source: &str,
    start: usize,
) -> StmtPeriodScan {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut allow_line_start_named_args = false;
    let mut allow_line_start_condition_comparison = false;
    let mut i = start;
    while i < tokens.len() {
        let t = &tokens[i];
        if t.kind == TokenKind::Eof {
            return StmtPeriodScan::Unterminated { end_exclusive: i };
        }
        if paren == 0 && bracket == 0 && brace == 0 {
            if t.kind == TokenKind::Period {
                return StmtPeriodScan::Found(i);
            }
            if is_named_arg_clause_keyword(source, t) {
                allow_line_start_named_args = true;
            }
            if is_condition_continuation_keyword(source, t) {
                allow_line_start_condition_comparison = true;
            }
            if i > start {
                if t.kind == TokenKind::Ident
                    && token_begins_line(source, t)
                    && is_definite_stmt_lead_keyword(source, t)
                    && !is_perform_if_found_addition(source, tokens, start, i)
                    && !is_inline_decl_continuation(source, tokens, i)
                {
                    return StmtPeriodScan::Unterminated { end_exclusive: i };
                }
                if t.kind == TokenKind::Ident && token_begins_line(source, t) {
                    let next_kind = tokens.get(i + 1).map(|x| x.kind);
                    if !allow_line_start_named_args
                        && !allow_line_start_condition_comparison
                        && matches!(next_kind, Some(TokenKind::Eq | TokenKind::QuestionEq))
                    {
                        return StmtPeriodScan::Unterminated { end_exclusive: i };
                    }
                }
            }
        }
        match t.kind {
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
    StmtPeriodScan::Unterminated {
        end_exclusive: tokens.len(),
    }
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
