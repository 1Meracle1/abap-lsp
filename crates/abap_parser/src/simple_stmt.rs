//! Simple statements that are not structured further yet: a run of tokens up to a top-level `.`.

use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind, have_space_between};

use crate::expr::{parse_arithmetic_expr, parse_logical_expr};
use crate::stmt_period::{StmtPeriodScan, scan_until_statement_period, unterminated_err_end};
use crate::type_ref::build_type_ref_node;

fn token_leaf(b: &mut SyntaxTreeBuilder, token: &Token) -> NodeId {
    b.leaf(SyntaxKind::Token, token.range.clone())
}

fn token_matches_keyword(source: &str, token: &Token, keyword: &str) -> bool {
    token.kind == TokenKind::Ident && token.lexeme(source).eq_ignore_ascii_case(keyword)
}

type SimpleStmtClassifier = fn(&str, &[&Token]) -> Option<SyntaxKind>;

#[derive(Clone, Copy)]
struct GuardedSimpleStmtClassifier {
    lead_keywords: &'static [&'static str],
    classify: SimpleStmtClassifier,
}

impl GuardedSimpleStmtClassifier {
    const fn new(lead_keywords: &'static [&'static str], classify: SimpleStmtClassifier) -> Self {
        Self {
            lead_keywords,
            classify,
        }
    }

    fn matches(self, lead_keyword: &str) -> bool {
        self.lead_keywords.is_empty()
            || self
                .lead_keywords
                .iter()
                .any(|keyword| lead_keyword.eq_ignore_ascii_case(keyword))
    }
}

const STRUCTURAL_SIMPLE_STMT_CLASSIFIERS: &[GuardedSimpleStmtClassifier] = &[
    GuardedSimpleStmtClassifier::new(
        &["public", "protected", "private"],
        classify_class_section_stmt,
    ),
    GuardedSimpleStmtClassifier::new(&["methods", "class"], classify_methods_stmt),
    GuardedSimpleStmtClassifier::new(&[], classify_direct_call_stmt),
];

const KEYWORD_SIMPLE_STMT_KINDS: &[(&str, SyntaxKind)] = &[
    ("assert", SyntaxKind::AssertStmt),
    ("check", SyntaxKind::CheckStmt),
    ("perform", SyntaxKind::PerformStmt),
];

fn significant_stmt_tokens(tokens: &[Token], idx: usize, period_i: usize) -> Vec<&Token> {
    tokens[idx..=period_i]
        .iter()
        .filter(|token| token.kind != TokenKind::Comment)
        .collect()
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

fn method_signature_starts_parameter(
    source: &str,
    tokens: &[Token],
    idx: usize,
    end: usize,
) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if token.kind != TokenKind::Ident {
        return false;
    }
    let mut j = idx;
    if token_matches_keyword(source, token, "value")
        || token_matches_keyword(source, token, "reference")
    {
        if tokens.get(j + 1).map(|t| t.kind) != Some(TokenKind::LParen)
            || tokens.get(j + 2).map(|t| t.kind) != Some(TokenKind::Ident)
            || tokens.get(j + 3).map(|t| t.kind) != Some(TokenKind::RParen)
        {
            return false;
        }
        j += 4;
    } else {
        j += 1;
    }
    while j < end && tokens[j].kind == TokenKind::Comment {
        j += 1;
    }
    tokens.get(j).is_some_and(|t| {
        token_matches_keyword(source, t, "type") || token_matches_keyword(source, t, "like")
    })
}

fn skip_method_signature_type_expression(
    source: &str,
    tokens: &[Token],
    mut idx: usize,
    end: usize,
) -> usize {
    let mut depth = 0i32;
    while idx < end {
        let token = &tokens[idx];
        match token.kind {
            TokenKind::Comment => idx += 1,
            TokenKind::LParen => {
                depth += 1;
                idx += 1;
            }
            TokenKind::RParen => {
                depth -= 1;
                idx += 1;
            }
            TokenKind::Period if depth == 0 => return idx,
            _ if depth == 0
                && (method_signature_section(source, token)
                    || token_matches_keyword(source, token, "raising")
                    || token_matches_keyword(source, token, "exceptions")
                    || token_matches_keyword(source, token, "abstract")
                    || token_matches_keyword(source, token, "final")
                    || token_matches_keyword(source, token, "redefinition")
                    || (token_matches_keyword(source, token, "for")
                        && tokens.get(idx + 1).is_some_and(|next| {
                            token_matches_keyword(source, next, "testing")
                        }))
                    || token_matches_keyword(source, token, "optional")
                    || token_matches_keyword(source, token, "default")
                    || token_matches_keyword(source, token, "preferred")
                    || method_signature_starts_parameter(source, tokens, idx, end)) =>
            {
                return idx;
            }
            _ => idx += 1,
        }
    }
    idx
}

fn methods_stmt_type_ref_ranges(
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<(usize, usize)> {
    let significant: Vec<_> = tokens[idx..=period_i]
        .iter()
        .enumerate()
        .filter(|(_, token)| token.kind != TokenKind::Comment)
        .map(|(offset, token)| (idx + offset, token))
        .collect();
    let significant_tokens: Vec<_> = significant.iter().map(|(_, token)| *token).collect();
    let Some(name_idx) = method_statement_name_idx(source, &significant_tokens) else {
        return Vec::new();
    };
    if significant_tokens.get(name_idx).map(|token| token.kind) != Some(TokenKind::Ident) {
        return Vec::new();
    }
    let mut ranges = Vec::new();
    let mut i = name_idx + 1;
    let mut saw_parameter_section = false;
    let mut in_raising = false;
    while i < significant_tokens.len() {
        let token = significant_tokens[i];
        if token.kind == TokenKind::Period {
            break;
        }
        if let Some(modifier_len) = method_header_modifier_len(source, &significant_tokens, i) {
            if saw_parameter_section {
                break;
            }
            i += modifier_len;
            continue;
        }
        if token_matches_keyword(source, token, "raising") {
            saw_parameter_section = true;
            in_raising = true;
            i += 1;
            continue;
        }
        if token_matches_keyword(source, token, "exceptions") {
            break;
        }
        if method_signature_section(source, token) {
            saw_parameter_section = true;
            in_raising = false;
            i += 1;
            continue;
        }
        if in_raising {
            while matches!(
                significant_tokens.get(i).map(|token| token.kind),
                Some(TokenKind::Colon | TokenKind::Comma)
            ) {
                i += 1;
            }
            let Some(exception_token) = significant_tokens.get(i) else {
                break;
            };
            if token_matches_keyword(source, exception_token, "resumable") {
                if significant_tokens.get(i + 1).map(|token| token.kind) != Some(TokenKind::LParen)
                {
                    i += 1;
                    continue;
                }
                let raw_expr_start = significant.get(i + 2).map(|(idx, _)| *idx);
                let mut depth = 1i32;
                let mut j = i + 2;
                while j < significant_tokens.len() {
                    match significant_tokens[j].kind {
                        TokenKind::LParen => depth += 1,
                        TokenKind::RParen => {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                    j += 1;
                }
                if let Some(expr_start) = raw_expr_start {
                    let expr_end = significant.get(j).map(|(idx, _)| *idx).unwrap_or(period_i);
                    if expr_start < expr_end {
                        ranges.push((expr_start, expr_end));
                    }
                }
                i = j.saturating_add(1);
                continue;
            }
            if exception_token.kind == TokenKind::Ident {
                let raw_expr_start = significant[i].0;
                let mut j = i + 1;
                while j + 1 < significant_tokens.len() {
                    let op = significant_tokens[j];
                    let next = significant_tokens[j + 1];
                    if matches!(
                        op.kind,
                        TokenKind::Minus | TokenKind::Arrow | TokenKind::Tilde | TokenKind::FatArrow
                    ) && next.kind == TokenKind::Ident
                    {
                        j += 2;
                    } else {
                        break;
                    }
                }
                let expr_end = significant
                    .get(j)
                    .map(|(idx, _)| *idx)
                    .unwrap_or(period_i + 1);
                if raw_expr_start < expr_end {
                    ranges.push((raw_expr_start, expr_end));
                }
                i = j;
                continue;
            }
            i += 1;
            continue;
        }
        let mut j = i;
        while matches!(
            significant_tokens.get(j).map(|token| token.kind),
            Some(TokenKind::Colon | TokenKind::Comma)
        ) {
            j += 1;
        }
        let Some(name_token) = significant_tokens.get(j) else {
            break;
        };
        let mut after_name = if token_matches_keyword(source, name_token, "value")
            || token_matches_keyword(source, name_token, "reference")
        {
            j + 4
        } else if name_token.kind == TokenKind::Ident {
            j + 1
        } else {
            i += 1;
            continue;
        };
        while matches!(
            significant_tokens.get(after_name).map(|token| token.kind),
            Some(TokenKind::Colon | TokenKind::Comma)
        ) {
            after_name += 1;
        }
        let Some(type_token) = significant_tokens.get(after_name) else {
            break;
        };
        if !token_matches_keyword(source, type_token, "type")
            && !token_matches_keyword(source, type_token, "like")
        {
            i += 1;
            continue;
        }
        let raw_type_idx = significant[after_name].0;
        let mut expr_start = raw_type_idx + 1;
        while expr_start <= period_i && tokens[expr_start].kind == TokenKind::Comment {
            expr_start += 1;
        }
        let expr_end =
            skip_method_signature_type_expression(source, tokens, expr_start, period_i + 1);
        if expr_start < expr_end {
            ranges.push((expr_start, expr_end));
        }
        i = after_name + 1;
    }
    ranges
}

fn build_methods_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let ranges = methods_stmt_type_ref_ranges(source, tokens, idx, period_i);
    if ranges.is_empty() {
        return tokens[idx..=period_i]
            .iter()
            .map(|t| token_leaf(b, t))
            .collect();
    }
    let mut children = Vec::with_capacity(period_i - idx + 1);
    let mut i = idx;
    let mut range_idx = 0usize;
    while i <= period_i {
        if let Some((start, end)) = ranges.get(range_idx).copied()
            && i == start
        {
            children.push(build_type_ref_node(b, source, &tokens[start..end]));
            i = end;
            range_idx += 1;
            continue;
        }
        children.push(token_leaf(b, &tokens[i]));
        i += 1;
    }
    children
}

fn class_section_statement(source: &str, significant: &[&Token]) -> bool {
    let Some(first) = significant.first() else {
        return false;
    };
    let Some(second) = significant.get(1) else {
        return false;
    };
    (token_matches_keyword(source, first, "public")
        || token_matches_keyword(source, first, "protected")
        || token_matches_keyword(source, first, "private"))
        && token_matches_keyword(source, second, "section")
}

fn direct_call_statement(significant: &[&Token]) -> bool {
    let Some(last) = significant.last() else {
        return false;
    };
    if last.kind != TokenKind::Period {
        return false;
    }

    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut first_top_level_lparen = None;
    for (idx, token) in significant.iter().enumerate() {
        match token.kind {
            TokenKind::LParen if paren == 0 && bracket == 0 && brace == 0 => {
                first_top_level_lparen = Some(idx);
                break;
            }
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            _ => {}
        }
    }

    let Some(lparen_idx) = first_top_level_lparen else {
        return false;
    };
    let target = &significant[..lparen_idx];
    if target.is_empty() {
        return false;
    }

    if target.len() == 1 {
        return target[0].kind == TokenKind::Ident;
    }

    target.iter().any(|token| {
        matches!(
            token.kind,
            TokenKind::Arrow | TokenKind::FatArrow | TokenKind::Tilde
        )
    })
}

fn direct_call_paren_pair(significant: &[&Token]) -> Option<(usize, usize)> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut first_top_level_lparen = None;
    for (idx, token) in significant.iter().enumerate() {
        match token.kind {
            TokenKind::LParen if paren == 0 && bracket == 0 && brace == 0 => {
                first_top_level_lparen = Some(idx);
                paren += 1;
            }
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => {
                paren -= 1;
                if paren == 0 && bracket == 0 && brace == 0 {
                    return first_top_level_lparen.map(|lparen_idx| (lparen_idx, idx));
                }
            }
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            _ => {}
        }
    }
    None
}

fn direct_call_padding_is_valid(significant: &[&Token]) -> bool {
    let Some((lparen_idx, rparen_idx)) = direct_call_paren_pair(significant) else {
        return false;
    };
    let lparen = significant[lparen_idx];
    let rparen = significant[rparen_idx];
    let inner = &significant[lparen_idx + 1..rparen_idx];
    match (inner.first(), inner.last()) {
        (Some(first), Some(last)) => {
            have_space_between(lparen, first) && have_space_between(last, rparen)
        }
        _ => have_space_between(lparen, rparen),
    }
}

fn classify_class_section_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    class_section_statement(source, significant).then_some(SyntaxKind::ClassSectionStmt)
}

fn classify_methods_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    method_statement_name_idx(source, significant).map(|_| SyntaxKind::MethodsStmt)
}

fn classify_direct_call_stmt(_source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    direct_call_statement(significant).then(|| {
        if direct_call_padding_is_valid(significant) {
            SyntaxKind::CallStmt
        } else {
            SyntaxKind::Error
        }
    })
}

fn classify_commit_or_rollback_work_stmt(
    source: &str,
    significant: &[&Token],
) -> Option<SyntaxKind> {
    if significant.len() != 3 || significant[1].kind != TokenKind::Ident || significant[2].kind != TokenKind::Period {
        return None;
    }

    if !token_matches_keyword(source, significant[1], "work") {
        return None;
    }

    if token_matches_keyword(source, significant[0], "commit") {
        Some(SyntaxKind::CommitWorkStmt)
    } else if token_matches_keyword(source, significant[0], "rollback") {
        Some(SyntaxKind::RollbackWorkStmt)
    } else {
        None
    }
}

fn simple_stmt_kind(source: &str, significant: &[&Token]) -> SyntaxKind {
    let Some(first) = significant.first() else {
        return SyntaxKind::UnparsedStmt;
    };

    let lead_keyword = first.lexeme(source);
    for classifier in STRUCTURAL_SIMPLE_STMT_CLASSIFIERS {
        if classifier.matches(lead_keyword)
            && let Some(kind) = (classifier.classify)(source, significant)
        {
            return kind;
        }
    }

    for (keyword, kind) in KEYWORD_SIMPLE_STMT_KINDS {
        if lead_keyword.eq_ignore_ascii_case(keyword) {
            return *kind;
        }
    }

    if let Some(kind) = classify_commit_or_rollback_work_stmt(source, significant) {
        return kind;
    }

    SyntaxKind::UnparsedStmt
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

fn validate_unparsed_stmt(
    source: &str,
    significant: &[&Token],
    tokens: &[Token],
    idx: usize,
    period_i: usize,
    errors: &mut Vec<crate::ParseError>,
) {
    validate_method_modifier_order(source, tokens, idx, period_i, errors);
    let is_method_stmt = method_statement_name_idx(source, significant).is_some();
    if !is_method_stmt && direct_call_statement(significant) && !direct_call_padding_is_valid(significant) {
        errors.push(crate::ParseError {
            message: "syntax error: method call arguments must have whitespace or a line break immediately inside parentheses"
                .to_string(),
            range: significant
                .first()
                .map(|token| token.range.start)
                .unwrap_or(tokens[idx].range.start)
                ..significant
                    .last()
                    .map(|token| token.range.end)
                    .unwrap_or(tokens[period_i].range.end),
        });
    }
}

fn build_assert_or_check_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = vec![token_leaf(b, &tokens[idx])];
    if idx + 1 < period_i {
        children.push(parse_logical_expr(
            b,
            source,
            &tokens[idx + 1..period_i],
            None,
        ));
    }
    children.push(token_leaf(b, &tokens[period_i]));
    children
}

fn build_direct_call_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::new();
    if idx < period_i {
        children.push(parse_arithmetic_expr(
            b,
            source,
            &tokens[idx..period_i],
            None,
        ));
    }
    children.push(token_leaf(b, &tokens[period_i]));
    children
}

/// Fallback parser for valid statement-shaped token runs when no dedicated parser claims them yet.
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
            let significant = significant_stmt_tokens(tokens, idx, period_i);
            validate_unparsed_stmt(source, &significant, tokens, idx, period_i, errors);
            let kind = simple_stmt_kind(source, &significant);
            let kids = match kind {
                SyntaxKind::MethodsStmt => {
                    build_methods_stmt_children(b, source, tokens, idx, period_i)
                }
                SyntaxKind::AssertStmt | SyntaxKind::CheckStmt => {
                    build_assert_or_check_stmt_children(b, source, tokens, idx, period_i)
                }
                SyntaxKind::CallStmt => {
                    build_direct_call_stmt_children(b, source, tokens, idx, period_i)
                }
                _ => tokens[idx..=period_i]
                    .iter()
                    .map(|t| token_leaf(b, t))
                    .collect(),
            };
            let node = b.branch(kind, first.range.start..period_tok.range.end, &kids);
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
                err.message
                    .contains("method modifier ABSTRACT must appear before parameter declarations")
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

    #[test]
    fn classifies_methods_statement_specifically() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    METHODS run IMPORTING iv_x TYPE i.\nENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MethodsStmt),
            1
        );
    }

    #[test]
    fn methods_stmt_builds_structured_type_refs() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    METHODS run IMPORTING iv_x TYPE REF TO zif_demo=>ty_row.\nENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let methods = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::MethodsStmt)
            .expect("methods stmt");
        assert_eq!(
            parsed.file.count_kind(methods, SyntaxKind::TypeRefSimple),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(methods, SyntaxKind::TypeRefSelectorChain),
            1
        );
    }

    #[test]
    fn class_methods_with_returning_value_and_namespaced_type_parse() {
        let src = "\
CLASS /STTP/CL_MESSAGES DEFINITION\n\
  PUBLIC\n\
  INHERITING FROM /CDBASIS/CL_MESSAGES\n\
  CREATE PUBLIC .\n\
\n\
PUBLIC SECTION.\n\
\n\
  CLASS-METHODS CREATE_NEW_HANDLER_ATT\n\
    IMPORTING\n\
      !IV_OBJECT TYPE BALOBJ_D OPTIONAL\n\
      !IV_SUBOBJECT TYPE BALSUBOBJ OPTIONAL\n\
      !IV_EXTNUMBER TYPE BALNREXT OPTIONAL\n\
      !IV_REPID TYPE SYREPID OPTIONAL\n\
      !IV_TITLE TYPE BALTITLE OPTIONAL\n\
      !IV_LOGLEVEL TYPE /STTP/CL_MESSAGES=>TE_LOGLEVEL DEFAULT 1\n\
      !IV_TYPELEVEL TYPE /STTP/CL_MESSAGES=>TE_TYPELEVEL DEFAULT 'I'\n\
    RETURNING\n\
      VALUE(RO_MESSAGES) TYPE REF TO /STTP/CL_MESSAGES .\n\
ENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MethodsStmt),
            1
        );
    }

    #[test]
    fn methods_stmt_builds_type_refs_for_raising_and_resumable_exceptions() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    METHODS run RAISING resumable(/sttp/cx_demo) cx_other.\nENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let methods = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::MethodsStmt)
            .expect("methods stmt");
        assert_eq!(
            parsed.file.count_kind(methods, SyntaxKind::TypeRefSimple),
            2
        );
    }

    #[test]
    fn classifies_class_section_statement_specifically() {
        let parsed = crate::parse("CLASS lcl DEFINITION. PUBLIC SECTION. ENDCLASS.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ClassSectionStmt),
            1
        );
    }

    #[test]
    fn classifies_assert_check_and_perform_statements_specifically() {
        let parsed = crate::parse("ASSERT lo_ref IS BOUND. CHECK lv_ok = abap_true. PERFORM run.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AssertStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CheckStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::PerformStmt), 1);
    }

    #[test]
    fn classifies_commit_and_rollback_work_statements_specifically() {
        let parsed = crate::parse("COMMIT WORK. ROLLBACK WORK.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CommitWorkStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::RollbackWorkStmt), 1);
    }

    #[test]
    fn rejects_instance_method_calls_without_inner_padding() {
        for src in [
            "lo_prog->add_statement(lo_assign ).",
            "lo_prog->add_statement( lo_print).",
        ] {
            let parsed = crate::parse(src);
            assert!(
                parsed
                    .errors
                    .iter()
                    .any(|err| err.message.contains("method call arguments")),
                "{src}: {:?}",
                parsed.errors
            );
            assert_eq!(
                parsed
                    .file
                    .count_kind(parsed.file.root(), SyntaxKind::CallStmt),
                0
            );
            assert!(
                parsed
                    .file
                    .count_kind(parsed.file.root(), SyntaxKind::Error)
                    >= 1
            );
        }
    }

    #[test]
    fn accepts_instance_method_calls_with_inner_padding() {
        let parsed = crate::parse("lo_prog->add_statement( lo_print ).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CallStmt),
            1
        );
    }

    #[test]
    fn assert_check_and_direct_call_build_expression_children() {
        let parsed = crate::parse(
            "ASSERT lo_ref IS BOUND. CHECK lv_ok = abap_true. lo_prog->add_statement( lo_item ).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert!(parsed.file.count_kind(root, SyntaxKind::IsPredicate) >= 1);
        assert!(parsed.file.count_kind(root, SyntaxKind::BinaryExpr) >= 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CallExpr), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectorExpr), 1);
    }
}
