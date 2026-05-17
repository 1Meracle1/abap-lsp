//! Statement-level assignment: `lhs = rhs .` or `lhs ?= rhs .`.

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_lexer::{TextRange, Token, TokenKind};

use crate::expr::parse_arithmetic_expr;
use crate::parser::{PResult, ParseFailure, Parser as CursorParser};
use crate::stmt_period::{delimiter_error, is_definite_stmt_lead_keyword, token_begins_line};

#[inline]
fn is_data_keyword(source: &str, t: &Token) -> bool {
    t.kind == TokenKind::Ident && t.lexeme(source).eq_ignore_ascii_case("data")
}

#[inline]
fn is_non_assignment_stmt_keyword(source: &str, t: &Token) -> bool {
    t.kind == TokenKind::Ident
        && (t.lexeme(source).eq_ignore_ascii_case("assert")
            || t.lexeme(source).eq_ignore_ascii_case("check")
            || t.lexeme(source).eq_ignore_ascii_case("compute")
            || t.lexeme(source).eq_ignore_ascii_case("perform"))
}

/// After a failed `DATA` typed declaration, `DATA lv = 1.` is still tokenized as `DATA` then `lv = 1 .`;
/// suppress assignment on the second chunk so invalid inline/assignment-style `DATA` is not half-parsed.
fn assign_preceded_by_data_keyword(tokens: &[Token], idx: usize, source: &str) -> bool {
    let mut j = idx;
    while j > 0 {
        j -= 1;
        match tokens[j].kind {
            TokenKind::Comment => continue,
            TokenKind::Ident if is_data_keyword(source, &tokens[j]) => return true,
            _ => break,
        }
    }
    false
}

fn find_stmt_level_assign_op(tokens: &[Token], start: usize) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut i = start;
    while i < tokens.len() {
        let t = &tokens[i];
        if t.kind == TokenKind::Eof {
            return None;
        }
        match t.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen if paren > 0 => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket if bracket > 0 => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace if brace > 0 => brace -= 1,
            TokenKind::Period if paren == 0 && bracket == 0 && brace == 0 => return None,
            TokenKind::Eq | TokenKind::QuestionEq if paren == 0 && bracket == 0 && brace == 0 => {
                return Some(i);
            }
            _ => {}
        }
        i += 1;
    }
    None
}

fn child_range(
    cursor: &CursorParser<'_, '_>,
    children: &[NodeId],
    fallback: TextRange,
) -> TextRange {
    let Some(first) = children.first() else {
        return fallback;
    };
    let last = *children.last().unwrap_or(first);
    cursor.span(*first).start..cursor.span(last).end
}

fn current_delimiter_error(
    cursor: &CursorParser<'_, '_>,
    start: usize,
) -> Option<crate::ParseError> {
    let current = cursor.index();
    delimiter_error(cursor.tokens(), start, current.saturating_add(1))
}

fn cursor_failure(cursor: &CursorParser<'_, '_>, message: impl Into<String>) -> ParseFailure {
    ParseFailure {
        message: message.into(),
        range: cursor
            .current()
            .or_else(|| cursor.previous())
            .map_or(0..0, |token| token.range.clone()),
    }
}

fn nested_eq_before_rhs_boundary(cursor: &CursorParser<'_, '_>, start: usize) -> bool {
    let mut depth = 0usize;
    let mut i = start;
    while let Some(token) = cursor.tokens().get(i) {
        if token.kind == TokenKind::Eof {
            break;
        }
        if depth == 0 {
            if token.kind == TokenKind::Period {
                break;
            }
            if i > start && token.kind == TokenKind::Ident && token_begins_line(token) {
                let next_kind = cursor.tokens().get(i + 1).map(|next| next.kind);
                if is_definite_stmt_lead_keyword(cursor.source(), token)
                    || matches!(next_kind, Some(TokenKind::Eq | TokenKind::QuestionEq))
                {
                    break;
                }
            }
        }
        match token.kind {
            TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => depth += 1,
            TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                depth = depth.saturating_sub(1);
            }
            TokenKind::Eq | TokenKind::QuestionEq if depth > 0 => return true,
            _ => {}
        }
        i += 1;
    }
    false
}

fn expect_assignment_operator_result(cursor: &mut CursorParser<'_, '_>) -> PResult<NodeId> {
    cursor.skip_trivia();
    match cursor.current().map(|token| token.kind) {
        Some(TokenKind::Eq) => cursor.expect_token_result(TokenKind::Eq),
        Some(TokenKind::QuestionEq) => cursor.expect_token_result(TokenKind::QuestionEq),
        _ => Err(cursor_failure(
            cursor,
            "syntax error: expected assignment operator",
        )),
    }
}

fn rhs_logical_continues(cursor: &mut CursorParser<'_, '_>) -> bool {
    cursor.skip_trivia();
    let Some(token) = cursor.current() else {
        return false;
    };
    match token.kind {
        TokenKind::Eq
        | TokenKind::Lt
        | TokenKind::Gt
        | TokenKind::Le
        | TokenKind::Ge
        | TokenKind::Ne => true,
        TokenKind::Ident => {
            let text = token.lexeme(cursor.source());
            text.eq_ignore_ascii_case("AND")
                || text.eq_ignore_ascii_case("OR")
                || text.eq_ignore_ascii_case("IS")
                || text.eq_ignore_ascii_case("BETWEEN")
                || text.eq_ignore_ascii_case("IN")
                || text.eq_ignore_ascii_case("EQ")
                || text.eq_ignore_ascii_case("NE")
                || text.eq_ignore_ascii_case("LT")
                || text.eq_ignore_ascii_case("LE")
                || text.eq_ignore_ascii_case("GT")
                || text.eq_ignore_ascii_case("GE")
                || text.eq_ignore_ascii_case("CO")
                || text.eq_ignore_ascii_case("CN")
                || text.eq_ignore_ascii_case("CA")
                || text.eq_ignore_ascii_case("NA")
                || text.eq_ignore_ascii_case("CS")
                || text.eq_ignore_ascii_case("NS")
                || text.eq_ignore_ascii_case("CP")
                || text.eq_ignore_ascii_case("NP")
        }
        _ => false,
    }
}

fn expect_assignment_rhs_result(
    cursor: &mut CursorParser<'_, '_>,
    rhs_start: usize,
) -> PResult<NodeId> {
    let rhs = cursor
        .expect_arithmetic_expr_result("assignment value after '='")
        .map_err(|mut failure| {
            failure.message = if nested_eq_before_rhs_boundary(cursor, rhs_start) {
                "syntax error: assignment value must not contain '=' inside nested parentheses, brackets, or braces"
                    .to_string()
            } else {
                "syntax error: expected assignment value after '='".to_string()
            };
            failure
        })?;
    if !rhs_logical_continues(cursor) {
        return Ok(rhs);
    }
    cursor.set_position(rhs_start, rhs_start.checked_sub(1));
    cursor
        .expect_logical_expr_result("assignment value after '='")
        .map_err(|mut failure| {
            failure.message = "syntax error: expected assignment value after '='".to_string();
            failure
        })
}

pub(crate) fn parse_assign_stmt_result_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<PResult<NodeId>> {
    cursor.skip_trivia();
    let start = cursor.index();
    let first = cursor.current()?;
    if is_data_keyword(cursor.source(), first)
        || is_non_assignment_stmt_keyword(cursor.source(), first)
    {
        return None;
    }
    if assign_preceded_by_data_keyword(cursor.tokens(), start, cursor.source()) {
        return None;
    }

    let Some(eq_i) = find_stmt_level_assign_op(cursor.tokens(), start) else {
        return None;
    };
    if eq_i == start {
        return None;
    }

    let lhs = {
        let (b, source, tokens, _) = cursor.parts_mut();
        let prev_before_lhs = start.checked_sub(1).and_then(|index| tokens.get(index));
        parse_arithmetic_expr(b, source, &tokens[start..eq_i], prev_before_lhs)
    };
    cursor.set_position(eq_i, eq_i.checked_sub(1));

    let eq = match expect_assignment_operator_result(cursor) {
        Ok(eq) => eq,
        Err(failure) => return Some(Err(failure)),
    };
    let rhs_start = cursor.index();
    let mut children = vec![lhs, eq];

    let rhs = match expect_assignment_rhs_result(cursor, rhs_start) {
        Ok(rhs) => rhs,
        Err(failure) => return Some(Err(failure)),
    };
    children.push(rhs);

    if cursor.current().is_some_and(|token| {
        matches!(
            token.kind,
            TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace
        )
    }) && let Some(error) = current_delimiter_error(cursor, start)
    {
        return Some(Err(ParseFailure {
            message: error.message,
            range: error.range,
        }));
    }

    let period = match cursor.expect_token_after_result(TokenKind::Period, "assignment statement") {
        Ok(period) => period,
        Err(mut failure) => {
            failure.message = "syntax error: expected '.' to end assignment statement".to_string();
            return Some(Err(failure));
        }
    };
    children.push(period);
    let range = child_range(cursor, &children, cursor.span(lhs));
    Some(Ok(cursor.builder().branch(
        SyntaxKind::AssignStmt,
        range,
        &children,
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use abap_ast::File;
    use abap_lexer::tokenize;

    use crate::syntax::build_file_tree;

    fn build_ok(src: &str) -> File {
        let tok = tokenize(src);
        let mut err = Vec::new();
        let file = build_file_tree(src, &tok.tokens, src.len(), &mut err);
        assert!(err.is_empty(), "{:?}", err);
        file
    }

    #[test]
    fn simple_assign_eq() {
        let src = "lv_count = 1.";
        let file = build_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::AssignStmt), 1);
    }

    #[test]
    fn assign_question_eq() {
        let src = "lr ?= ref.";
        let file = build_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::AssignStmt), 1);
    }

    #[test]
    fn assign_rhs_precedence() {
        let src = "lv = a + b * c.";
        let file = build_ok(src);
        let a = file
            .find_first_kind(file.root(), SyntaxKind::AssignStmt)
            .expect("assign");
        let ch: Vec<_> = file.children(a).collect();
        let rhs_tmpl = ch[2];
        assert_eq!(file.kind(rhs_tmpl), SyntaxKind::TemplateExpr);
        let inner = file
            .find_first_kind(rhs_tmpl, SyntaxKind::BinaryExpr)
            .expect("binary");
        let op = file.children(inner).nth(1).unwrap();
        assert_eq!(file.range(op), src.find('+').map(|s| s..s + 1).unwrap());
    }

    #[test]
    fn assign_rejects_eq_inside_nested_group_in_rhs() {
        let parsed = crate::parse("lv = ( a = 1 ).");
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("nested parentheses")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::Error),
            0
        );
        assert!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InvalidStmt)
                >= 1
        );
    }

    #[test]
    fn assign_allows_table_expression_key_eq_in_rhs() {
        let parsed = crate::parse(
            "ls_range = lt_ranges[ serno_from = lv_min_init serno_to = lv_max_init ].",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            1
        );
    }

    #[test]
    fn error_on_assign_without_period_at_eof() {
        let parsed = crate::parse("lv = 1");
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("expected '.'")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::Error),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InvalidStmt),
            1
        );
    }

    #[test]
    fn error_on_assign_without_period_before_next_statement() {
        let src = "lv_s = 'unterminated'\nDATA lv_after TYPE i.";
        let parsed = crate::parse(src);
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("expected '.'")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::Error),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InvalidStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
            1
        );
    }

    #[test]
    fn assign_stmt_period_may_follow_multiline_rhs() {
        let src = "lv = \n 1.";
        let file = build_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::AssignStmt), 1);
    }

    #[test]
    fn assign_allows_multiline_constructor_named_arguments_in_rhs() {
        let src = "lo_assign = NEW zcl_assign_stmt(\n  iv_name = 'x'\n  io_expr = lo_expr1\n).";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            1
        );
    }

    #[test]
    fn lone_identifier_before_dot_can_be_macro_call() {
        let parsed = crate::parse("do_something.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MacroCallStmt),
            1
        );
    }

    #[test]
    fn data_keyword_not_assign() {
        let src = "DATA lv = 1.";
        let file = build_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::AssignStmt), 0);
    }
}
