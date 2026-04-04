//! Statement-level assignment: `lhs = rhs .` or `lhs ?= rhs .`.

use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind, have_space_between};

use crate::expr::parse_arithmetic_expr;
use crate::stmt_period::{
    is_definite_stmt_lead_keyword, token_begins_line, unterminated_err_end,
};

fn token_leaf(b: &mut SyntaxTreeBuilder, token: &Token) -> NodeId {
    b.leaf(SyntaxKind::Token, token.range.clone())
}

#[inline]
fn is_data_keyword(source: &str, t: &Token) -> bool {
    t.kind == TokenKind::Ident && t.lexeme(source).eq_ignore_ascii_case("data")
}

#[inline]
fn is_non_assignment_stmt_keyword(source: &str, t: &Token) -> bool {
    t.kind == TokenKind::Ident
        && (t.lexeme(source).eq_ignore_ascii_case("assert")
            || t.lexeme(source).eq_ignore_ascii_case("check")
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
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
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

#[inline]
fn is_named_arg_clause_keyword(source: &str, tok: &Token) -> bool {
    tok.kind == TokenKind::Ident
        && (tok.lexeme(source).eq_ignore_ascii_case("EXPORTING")
            || tok.lexeme(source).eq_ignore_ascii_case("IMPORTING")
            || tok.lexeme(source).eq_ignore_ascii_case("CHANGING")
            || tok.lexeme(source).eq_ignore_ascii_case("RECEIVING")
            || tok.lexeme(source).eq_ignore_ascii_case("EXCEPTIONS"))
}

#[inline]
fn is_condition_continuation_keyword(source: &str, tok: &Token) -> bool {
    tok.kind == TokenKind::Ident
        && (tok.lexeme(source).eq_ignore_ascii_case("AND")
            || tok.lexeme(source).eq_ignore_ascii_case("OR")
            || tok.lexeme(source).eq_ignore_ascii_case("WHERE")
            || tok.lexeme(source).eq_ignore_ascii_case("HAVING")
            || tok.lexeme(source).eq_ignore_ascii_case("ON"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AssignRhsScan {
    Found {
        period_i: usize,
        nested_eq: bool,
    },
    Unterminated {
        end_exclusive: usize,
        nested_eq: bool,
    },
}

fn scan_assign_rhs(tokens: &[Token], source: &str, start: usize) -> AssignRhsScan {
    let mut group_paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut allow_line_start_named_args = false;
    let mut allow_line_start_condition_comparison = false;
    let mut nested_eq = false;
    let mut non_call_paren_depth = 0i32;
    let mut paren_stack: Vec<bool> = Vec::new();
    let mut prev_token: Option<&Token> = None;
    let mut i = start;
    while i < tokens.len() {
        let t = &tokens[i];
        if t.kind == TokenKind::Eof {
            return AssignRhsScan::Unterminated {
                end_exclusive: i,
                nested_eq,
            };
        }
        if group_paren == 0 && bracket == 0 && brace == 0 {
            if t.kind == TokenKind::Period {
                return AssignRhsScan::Found {
                    period_i: i,
                    nested_eq,
                };
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
                {
                    return AssignRhsScan::Unterminated {
                        end_exclusive: i,
                        nested_eq,
                    };
                }
                if t.kind == TokenKind::Ident && token_begins_line(source, t) {
                    let next_kind = tokens.get(i + 1).map(|x| x.kind);
                    if !allow_line_start_named_args
                        && !allow_line_start_condition_comparison
                        && matches!(next_kind, Some(TokenKind::Eq | TokenKind::QuestionEq))
                    {
                        return AssignRhsScan::Unterminated {
                            end_exclusive: i,
                            nested_eq,
                        };
                    }
                }
            }
        }
        match t.kind {
            TokenKind::LParen => {
                let is_call_paren = prev_token.is_some_and(|prev| !have_space_between(prev, t));
                paren_stack.push(is_call_paren);
                group_paren += 1;
                if !is_call_paren {
                    non_call_paren_depth += 1;
                }
            }
            TokenKind::RParen => {
                group_paren -= 1;
                if !paren_stack.pop().unwrap_or(false) {
                    non_call_paren_depth -= 1;
                }
            }
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            TokenKind::Eq | TokenKind::QuestionEq if non_call_paren_depth > 0 => {
                if !paren_stack.iter().rev().any(|is_call| *is_call) {
                    nested_eq = true;
                }
            }
            _ => {}
        }
        prev_token = Some(t);
        i += 1;
    }
    AssignRhsScan::Unterminated {
        end_exclusive: tokens.len(),
        nested_eq,
    }
}

/// If `tokens[idx]..` is `lhs (=|?=) rhs .`, returns [`AssignStmt`] and the index after `.`.
///
/// Skips when the first token is the `DATA` keyword so invalid forms like `DATA lv = 1.` are not classified
/// as assignments (they stay ordinary tokens until wider `DATA` support exists).
pub fn try_parse_assign_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    if is_data_keyword(source, first) || is_non_assignment_stmt_keyword(source, first) {
        return None;
    }
    if assign_preceded_by_data_keyword(tokens, idx, source) {
        return None;
    }

    let eq_i = find_stmt_level_assign_op(tokens, idx)?;
    if eq_i == idx {
        return None;
    }

    let lhs_tokens = &tokens[idx..eq_i];
    if lhs_tokens.is_empty() {
        return None;
    }

    let eq_tok = &tokens[eq_i];

    match scan_assign_rhs(tokens, source, eq_i + 1) {
        AssignRhsScan::Found { period_i, nested_eq } => {
            let period_tok = &tokens[period_i];
            let rhs_tokens = &tokens[eq_i + 1..period_i];

            if nested_eq {
                errors.push(crate::ParseError {
                    message: "syntax error: assignment value must not contain '=' inside nested parentheses, brackets, or braces"
                        .to_string(),
                    range: first.range.start..period_tok.range.end,
                });
                let mut kids = Vec::with_capacity(period_i - idx + 1);
                for t in &tokens[idx..=period_i] {
                    kids.push(token_leaf(b, t));
                }
                let node = b.branch(
                    SyntaxKind::Error,
                    first.range.start..period_tok.range.end,
                    &kids,
                );
                return Some((node, period_i + 1));
            }

            let prev_before_lhs = idx.checked_sub(1).and_then(|j| tokens.get(j));
            let lhs = parse_arithmetic_expr(b, source, lhs_tokens, prev_before_lhs);
            let rhs = parse_arithmetic_expr(b, source, rhs_tokens, Some(eq_tok));
            let eq_leaf = token_leaf(b, eq_tok);
            let period_leaf = token_leaf(b, period_tok);

            let end = period_tok.range.end;
            let node = b.branch(
                SyntaxKind::AssignStmt,
                first.range.start..end,
                &[lhs, eq_leaf, rhs, period_leaf],
            );
            Some((node, period_i + 1))
        }
        AssignRhsScan::Unterminated {
            end_exclusive,
            nested_eq,
        } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, first.range.end);

            if nested_eq {
                errors.push(crate::ParseError {
                    message: "syntax error: assignment value must not contain '=' inside nested parentheses, brackets, or braces"
                        .to_string(),
                    range: first.range.start..err_end,
                });
            } else {
                errors.push(crate::ParseError {
                    message: "syntax error: expected '.' to end assignment statement".to_string(),
                    range: first.range.start..err_end,
                });
            }

            let mut kids = Vec::with_capacity(end_exclusive - idx);
            for t in &tokens[idx..end_exclusive] {
                kids.push(token_leaf(b, t));
            }
            let node = b.branch(SyntaxKind::Error, first.range.start..err_end, &kids);
            let next = if tokens.get(end_exclusive).map(|t| t.kind) == Some(TokenKind::Eof) {
                tokens.len()
            } else {
                end_exclusive
            };
            Some((node, next))
        }
    }
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
            1
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
    fn lone_identifier_before_dot_is_syntax_error() {
        let parsed = crate::parse("do_something.");
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("lone identifier")),
            "{:?}",
            parsed.errors
        );
    }

    #[test]
    fn data_keyword_not_assign() {
        let src = "DATA lv = 1.";
        let file = build_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::AssignStmt), 0);
    }
}
