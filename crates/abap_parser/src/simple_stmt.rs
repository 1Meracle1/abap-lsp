//! Simple statements that are not structured further yet: a run of tokens up to a top-level `.`.

use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind};

use crate::stmt_period::{StmtPeriodScan, scan_until_statement_period, unterminated_err_end};

fn token_leaf(b: &mut SyntaxTreeBuilder, token: &Token) -> NodeId {
    b.leaf(SyntaxKind::Token, token.range.clone())
}

fn token_matches_keyword(source: &str, token: &Token, keyword: &str) -> bool {
    token.kind == TokenKind::Ident && token.lexeme(source).eq_ignore_ascii_case(keyword)
}

fn method_statement_name_idx(source: &str, significant: &[&Token]) -> Option<usize> {
    let first = *significant.first()?;
    if token_matches_keyword(source, first, "methods") {
        return Some(1);
    }
    let second = *significant.get(1)?;
    let third = *significant.get(2)?;
    if token_matches_keyword(source, first, "class")
        && second.kind == TokenKind::Minus
        && token_matches_keyword(source, third, "methods")
    {
        return Some(3);
    }
    None
}

fn method_header_modifier_len(source: &str, significant: &[&Token], idx: usize) -> Option<usize> {
    let token = *significant.get(idx)?;
    if token_matches_keyword(source, token, "abstract")
        || token_matches_keyword(source, token, "final")
        || token_matches_keyword(source, token, "redefinition")
    {
        return Some(1);
    }
    if token_matches_keyword(source, token, "for")
        && significant
            .get(idx + 1)
            .is_some_and(|next| token_matches_keyword(source, next, "testing"))
    {
        return Some(2);
    }
    None
}

fn method_signature_section(source: &str, token: &Token) -> bool {
    token_matches_keyword(source, token, "importing")
        || token_matches_keyword(source, token, "exporting")
        || token_matches_keyword(source, token, "changing")
        || token_matches_keyword(source, token, "receiving")
        || token_matches_keyword(source, token, "returning")
        || token_matches_keyword(source, token, "raising")
        || token_matches_keyword(source, token, "exceptions")
}

fn validate_method_modifier_order(
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
    errors: &mut Vec<crate::ParseError>,
) {
    let significant: Vec<_> = tokens[idx..=period_i]
        .iter()
        .filter(|token| token.kind != TokenKind::Comment)
        .collect();
    let Some(name_idx) = method_statement_name_idx(source, &significant) else {
        return;
    };
    if significant.get(name_idx).map(|token| token.kind) != Some(TokenKind::Ident) {
        return;
    }

    let mut saw_parameter_clause = false;
    let mut i = name_idx + 1;
    while i < significant.len() {
        let token = significant[i];
        if token.kind == TokenKind::Period {
            break;
        }
        if method_signature_section(source, token) {
            saw_parameter_clause = true;
            i += 1;
            continue;
        }
        if let Some(modifier_len) = method_header_modifier_len(source, &significant, i) {
            if saw_parameter_clause {
                let end = significant[i + modifier_len - 1].range.end;
                let modifier = if modifier_len == 2 {
                    format!(
                        "{} {}",
                        significant[i].lexeme(source).to_ascii_uppercase(),
                        significant[i + 1].lexeme(source).to_ascii_uppercase()
                    )
                } else {
                    significant[i].lexeme(source).to_ascii_uppercase()
                };
                errors.push(crate::ParseError {
                    message: format!(
                        "syntax error: method modifier {modifier} must appear before parameter declarations"
                    ),
                    range: token.range.start..end,
                });
                return;
            }
            i += modifier_len;
            continue;
        }
        i += 1;
    }
}

fn validate_simple_stmt(
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
    errors: &mut Vec<crate::ParseError>,
) {
    validate_method_modifier_order(source, tokens, idx, period_i, errors);
}

/// `keyword token* .` for any leading identifier after dedicated parsers (`DATA`, `IF`, …) have
/// declined.
pub fn try_parse_simple_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    if first.kind != TokenKind::Ident {
        return None;
    }

    match scan_until_statement_period(tokens, source, idx) {
        StmtPeriodScan::Found(period_i) => {
            let period_tok = &tokens[period_i];
            validate_simple_stmt(source, tokens, idx, period_i, errors);
            let mut kids = Vec::with_capacity(period_i - idx + 1);
            for t in &tokens[idx..=period_i] {
                kids.push(token_leaf(b, t));
            }
            let node = b.branch(
                SyntaxKind::SimpleStmt,
                first.range.start..period_tok.range.end,
                &kids,
            );
            Some((node, period_i + 1))
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, first.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' to end statement".to_string(),
                range: first.range.start..err_end,
            });
            let mut kids = Vec::with_capacity(end_exclusive.saturating_sub(idx));
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
    use abap_ast::SyntaxKind;

    #[test]
    fn reports_method_modifier_after_parameter_declarations() {
        let src = "\
CLASS zcl_ast_node DEFINITION ABSTRACT.\n\
  PUBLIC SECTION.\n\
    METHODS to_string\n\
      RETURNING VALUE(rv_text) TYPE string\n\
      ABSTRACT.\n\
ENDCLASS.";
        let parsed = crate::parse(src);
        assert!(
            parsed.errors.iter().any(|err| {
                err.message.contains("method modifier ABSTRACT must appear before parameter declarations")
            }),
            "{:?}",
            parsed.errors
        );
    }

    #[test]
    fn accepts_method_modifier_before_parameter_declarations() {
        let src = "\
CLASS zcl_ast_node DEFINITION ABSTRACT.\n\
  PUBLIC SECTION.\n\
    METHODS to_string ABSTRACT\n\
      RETURNING VALUE(rv_text) TYPE string.\n\
ENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ClassDecl),
            1
        );
    }
}
