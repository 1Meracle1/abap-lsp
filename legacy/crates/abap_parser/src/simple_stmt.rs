//! Simple statements that are not structured further yet: a run of tokens up to a top-level `.`.

use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind, have_space_between};

use crate::block_helpers::{inline_name_spacing_is_valid, parse_inline_name};
use crate::expr::{parse_arithmetic_expr, parse_logical_expr};
use crate::parser::{PResult, Parser as CursorParser};
use crate::stmt_period::{
    compact_dynamic_selector_lparen, delimiter_error, find_top_level_keyword_index,
    has_non_comment_tokens, is_definite_stmt_lead_keyword, line_start_assignment,
    previous_non_comment_token, token_begins_line,
};
use crate::syntax::token_leaf;
use crate::type_ref::build_type_ref_node;

fn token_matches_keyword(source: &str, token: &Token, keyword: &str) -> bool {
    token.kind == TokenKind::Ident && token.lexeme(source).eq_ignore_ascii_case(keyword)
}

fn token_matches_occurrence_keyword(source: &str, token: &Token) -> bool {
    token_matches_keyword(source, token, "occurrence")
        || token_matches_keyword(source, token, "occurrences")
        || token_matches_keyword(source, token, "occurence")
        || token_matches_keyword(source, token, "occurences")
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
    GuardedSimpleStmtClassifier::new(&["class"], classify_class_deferred_stmt),
    GuardedSimpleStmtClassifier::new(&["interface"], classify_interface_deferred_stmt),
    GuardedSimpleStmtClassifier::new(&["set"], classify_set_gui_stmt),
    GuardedSimpleStmtClassifier::new(&["read"], classify_read_runtime_stmt),
    GuardedSimpleStmtClassifier::new(&["open", "close", "delete"], classify_dataset_stmt),
    GuardedSimpleStmtClassifier::new(&["generate"], classify_generate_stmt),
    GuardedSimpleStmtClassifier::new(&["get"], classify_get_runtime_stmt),
    GuardedSimpleStmtClassifier::new(&["type"], classify_type_pools_stmt),
    GuardedSimpleStmtClassifier::new(&["function"], classify_function_pool_stmt),
    GuardedSimpleStmtClassifier::new(&["methods", "class"], classify_methods_stmt),
    GuardedSimpleStmtClassifier::new(&["events", "class"], classify_events_stmt),
    GuardedSimpleStmtClassifier::new(&["interfaces", "interface"], classify_interfaces_stmt),
    GuardedSimpleStmtClassifier::new(&["field"], classify_field_stmt),
    GuardedSimpleStmtClassifier::new(&["suppress"], classify_suppress_dialog_stmt),
    GuardedSimpleStmtClassifier::new(&["log"], classify_log_point_stmt),
    GuardedSimpleStmtClassifier::new(&["new"], classify_new_list_control_stmt),
    GuardedSimpleStmtClassifier::new(&[], classify_direct_call_stmt),
];

const KEYWORD_SIMPLE_STMT_KINDS: &[(&str, SyntaxKind)] = &[
    ("add", SyntaxKind::AddStmt),
    ("aliases", SyntaxKind::AliasesStmt),
    ("assert", SyntaxKind::AssertStmt),
    ("back", SyntaxKind::BackStmt),
    ("check", SyntaxKind::CheckStmt),
    ("clear", SyntaxKind::ClearStmt),
    ("compute", SyntaxKind::ComputeStmt),
    ("continue", SyntaxKind::ContinueStmt),
    ("convert", SyntaxKind::ConvertStmt),
    ("describe", SyntaxKind::DescribeStmt),
    ("divide", SyntaxKind::DivideStmt),
    ("exit", SyntaxKind::ExitStmt),
    ("format", SyntaxKind::FormatStmt),
    ("hide", SyntaxKind::HideStmt),
    ("multiply", SyntaxKind::MultiplyStmt),
    ("overlay", SyntaxKind::OverlayStmt),
    ("pack", SyntaxKind::PackStmt),
    ("perform", SyntaxKind::PerformStmt),
    ("position", SyntaxKind::PositionStmt),
    ("return", SyntaxKind::ReturnStmt),
    ("search", SyntaxKind::SearchStmt),
    ("shift", SyntaxKind::ShiftStmt),
    ("skip", SyntaxKind::SkipStmt),
    ("submit", SyntaxKind::SubmitStmt),
    ("reserve", SyntaxKind::ReserveStmt),
    ("stop", SyntaxKind::StopStmt),
    ("subtract", SyntaxKind::SubtractStmt),
    ("translate", SyntaxKind::TranslateStmt),
    ("transfer", SyntaxKind::TransferStmt),
    ("uline", SyntaxKind::UlineStmt),
    ("unpack", SyntaxKind::UnpackStmt),
    ("replace", SyntaxKind::ReplaceStmt),
    ("resume", SyntaxKind::ResumeStmt),
    ("retry", SyntaxKind::RetryStmt),
    ("wait", SyntaxKind::WaitStmt),
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

fn event_statement_name_idx(source: &str, significant: &[&Token]) -> Option<usize> {
    let first = *significant.first()?;
    if token_matches_keyword(source, first, "events") {
        return Some(1);
    }
    let second = *significant.get(1)?;
    let third = *significant.get(2)?;
    if token_matches_keyword(source, first, "class")
        && second.kind == TokenKind::Minus
        && token_matches_keyword(source, third, "events")
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
            TokenKind::Comma if depth == 0 => return idx,
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
    let Some(mut name_idx) = method_statement_name_idx(source, &significant_tokens) else {
        return Vec::new();
    };
    while matches!(
        significant_tokens.get(name_idx).map(|token| token.kind),
        Some(TokenKind::Colon | TokenKind::Comma)
    ) {
        name_idx += 1;
    }
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
        if token.kind == TokenKind::Comma {
            saw_parameter_section = false;
            in_raising = false;
            i += 1;
            continue;
        }
        if let Some(modifier_len) = method_header_modifier_len(source, &significant_tokens, i) {
            if saw_parameter_section {
                break;
            }
            i += modifier_len;
            continue;
        }
        if token_matches_keyword(source, token, "for")
            && significant_tokens
                .get(i + 1)
                .is_some_and(|next| token_matches_keyword(source, next, "event"))
        {
            let mut of_idx = i + 2;
            while of_idx < significant_tokens.len()
                && !token_matches_keyword(source, significant_tokens[of_idx], "of")
                && significant_tokens[of_idx].kind != TokenKind::Period
            {
                of_idx += 1;
            }
            if token_matches_keyword(source, significant_tokens[of_idx], "of")
                && let Some((raw_type_idx, _)) = significant.get(of_idx + 1)
            {
                let mut expr_start = *raw_type_idx;
                while expr_start <= period_i && tokens[expr_start].kind == TokenKind::Comment {
                    expr_start += 1;
                }
                let expr_end =
                    skip_method_signature_type_expression(source, tokens, expr_start, period_i + 1);
                if expr_start < expr_end {
                    ranges.push((expr_start, expr_end));
                }
                i = of_idx + 2;
                continue;
            }
        }
        if token_matches_keyword(source, token, "raising") {
            saw_parameter_section = true;
            in_raising = true;
            i += 1;
            continue;
        }
        if token_matches_keyword(source, token, "exceptions") {
            saw_parameter_section = true;
            in_raising = false;
            i += 1;
            while i < significant_tokens.len()
                && significant_tokens[i].kind != TokenKind::Comma
                && significant_tokens[i].kind != TokenKind::Period
            {
                i += 1;
            }
            continue;
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
                        TokenKind::Minus
                            | TokenKind::Arrow
                            | TokenKind::Tilde
                            | TokenKind::FatArrow
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

fn events_stmt_type_ref_ranges(
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
    let Some(mut i) = event_statement_name_idx(source, &significant_tokens) else {
        return Vec::new();
    };
    while matches!(
        significant_tokens.get(i).map(|token| token.kind),
        Some(TokenKind::Colon | TokenKind::Comma)
    ) {
        i += 1;
    }
    if significant_tokens.get(i).map(|token| token.kind) != Some(TokenKind::Ident) {
        return Vec::new();
    }
    let mut ranges = Vec::new();
    i += 1;
    while i < significant_tokens.len() {
        let token = significant_tokens[i];
        if token.kind == TokenKind::Period {
            break;
        }
        if token_matches_keyword(source, token, "exporting") {
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

fn interfaces_stmt_type_ref_ranges(
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
    if significant.len() < 3 {
        return Vec::new();
    }
    let first = significant[0].1;
    let Some(last) = significant.last().map(|(_, token)| *token) else {
        return Vec::new();
    };
    if last.kind != TokenKind::Period {
        return Vec::new();
    }
    if token_matches_keyword(source, first, "interfaces") {
        let Some(start) = significant.get(1).map(|(idx, _)| *idx) else {
            return Vec::new();
        };
        let Some(end) = significant.last().map(|(idx, _)| *idx) else {
            return Vec::new();
        };
        return (start < end).then_some((start, end)).into_iter().collect();
    }
    if token_matches_keyword(source, first, "interface") {
        let mut ranges = Vec::new();
        let mut i = 1usize;
        loop {
            while matches!(
                significant.get(i).map(|(_, token)| token.kind),
                Some(TokenKind::Colon | TokenKind::Comma)
            ) {
                i += 1;
            }
            if significant
                .get(i)
                .is_some_and(|(_, token)| token.kind == TokenKind::Period)
            {
                break;
            }
            let Some((start, name_token)) = significant.get(i).copied() else {
                return Vec::new();
            };
            let Some((end, load_token)) = significant.get(i + 1).copied() else {
                return Vec::new();
            };
            if name_token.kind != TokenKind::Ident
                || !token_matches_keyword(source, load_token, "load")
            {
                return Vec::new();
            }
            if start < end {
                ranges.push((start, end));
            }
            i += 2;
        }
        return ranges;
    }
    Vec::new()
}

fn build_stmt_children_with_type_refs(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
    ranges: Vec<(usize, usize)>,
) -> Vec<NodeId> {
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

fn build_class_section_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    let mut wrapped_visibility = false;
    for token in &tokens[idx..=period_i] {
        if !wrapped_visibility
            && token.kind == TokenKind::Ident
            && (token_matches_keyword(source, token, "public")
                || token_matches_keyword(source, token, "protected")
                || token_matches_keyword(source, token, "private"))
        {
            let leaf = token_leaf(b, token);
            children.push(b.branch(
                SyntaxKind::ClassSectionVisibility,
                token.range.clone(),
                &[leaf],
            ));
            wrapped_visibility = true;
            continue;
        }
        children.push(token_leaf(b, token));
    }
    children
}

fn build_deferred_type_stmt_children(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    let mut expect_name = false;
    for i in idx..=period_i {
        if i == idx {
            children.push(token_leaf(b, &tokens[i]));
            expect_name = true;
            continue;
        }
        if matches!(tokens[i].kind, TokenKind::Colon | TokenKind::Comma) {
            children.push(token_leaf(b, &tokens[i]));
            expect_name = true;
            continue;
        }
        if expect_name && let Some((name, next_i)) = parse_inline_name(b, tokens, i) {
            children.push(name);
            expect_name = false;
            if next_i == i + 1 {
                continue;
            }
        }
        if tokens[i].kind != TokenKind::Comment {
            expect_name = false;
        }
        children.push(token_leaf(b, &tokens[i]));
    }
    children
}

fn build_data_inline_decl_local(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
) -> Option<NodeId> {
    let data_tok = tokens.get(start)?;
    if !(token_matches_keyword(source, data_tok, "data")
        || token_matches_keyword(source, data_tok, "final"))
    {
        return None;
    }
    let lparen = tokens.get(start + 1)?;
    if lparen.kind != TokenKind::LParen {
        return None;
    }
    let (name, next_idx) = parse_inline_name(b, tokens, start + 2)?;
    let rparen = tokens.get(next_idx)?;
    if rparen.kind != TokenKind::RParen || next_idx + 1 != end {
        return None;
    }
    if !inline_name_spacing_is_valid(tokens, start + 1, start + 2, next_idx) {
        let children: Vec<_> = tokens[start..end]
            .iter()
            .map(|token| token_leaf(b, token))
            .collect();
        return Some(b.branch(
            SyntaxKind::Error,
            data_tok.range.start..rparen.range.end,
            &children,
        ));
    }

    let data_leaf = token_leaf(b, data_tok);
    let lparen_leaf = token_leaf(b, lparen);
    let rparen_leaf = token_leaf(b, rparen);
    Some(b.branch(
        SyntaxKind::DataInlineDecl,
        data_tok.range.start..rparen.range.end,
        &[data_leaf, lparen_leaf, name, rparen_leaf],
    ))
}

fn next_non_comment(tokens: &[Token], mut idx: usize, end: usize) -> usize {
    while idx < end && tokens[idx].kind == TokenKind::Comment {
        idx += 1;
    }
    idx
}

fn trim_trailing_comments(tokens: &[Token], start: usize, mut end: usize) -> usize {
    while end > start && tokens[end - 1].kind == TokenKind::Comment {
        end -= 1;
    }
    end
}

fn find_top_level_keyword_sequence_start(
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
    keywords: &[&str],
) -> Option<usize> {
    if keywords.is_empty() {
        return None;
    }

    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            idx += 1;
            continue;
        }

        if paren == 0
            && bracket == 0
            && brace == 0
            && token_matches_keyword(source, token, keywords[0])
        {
            let mut probe = idx;
            let mut matched = true;
            for keyword in keywords.iter().skip(1) {
                probe = next_non_comment(tokens, probe + 1, end);
                if probe >= end || !token_matches_keyword(source, &tokens[probe], keyword) {
                    matched = false;
                    break;
                }
            }
            if matched {
                return Some(idx);
            }
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

fn scan_expr_end(
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
    stop_keywords: &[&str],
    stop_token_kinds: &[TokenKind],
) -> usize {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            idx += 1;
            continue;
        }
        if paren == 0
            && bracket == 0
            && brace == 0
            && (stop_token_kinds.contains(&token.kind)
                || stop_keywords
                    .iter()
                    .any(|keyword| token_matches_keyword(source, token, keyword)))
        {
            break;
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
    idx
}

fn push_token_range(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    tokens: &[Token],
    start: usize,
    end: usize,
) {
    children.extend(tokens[start..end].iter().map(|token| token_leaf(b, token)));
}

fn build_wrapped_expr_child(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
    kind: SyntaxKind,
) -> NodeId {
    let expr = parse_arithmetic_expr(b, source, &tokens[start..end], None);
    b.branch(
        kind,
        tokens[start].range.start..tokens[end - 1].range.end,
        &[expr],
    )
}

fn build_wrapped_data_inline_decl_child(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
    kind: SyntaxKind,
) -> Option<NodeId> {
    let inner = build_data_inline_decl_local(b, source, tokens, start, end)?;
    Some(b.branch(
        kind,
        tokens[start].range.start..tokens[end - 1].range.end,
        &[inner],
    ))
}

fn build_wrapped_expr_or_data_inline_decl_child(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
    kind: SyntaxKind,
) -> Option<NodeId> {
    let value_start = next_non_comment(tokens, start, end);
    let value_end = trim_trailing_comments(tokens, value_start, end);
    if value_start >= value_end {
        return None;
    }
    if token_matches_keyword(source, &tokens[value_start], "data")
        || token_matches_keyword(source, &tokens[value_start], "final")
    {
        return build_wrapped_data_inline_decl_child(
            b,
            source,
            tokens,
            value_start,
            value_end,
            kind,
        );
    }
    Some(build_wrapped_expr_child(
        b,
        source,
        tokens,
        value_start,
        value_end,
        kind,
    ))
}

fn build_alias_entry_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
) -> Option<NodeId> {
    let alias_idx = next_non_comment(tokens, start, end);
    let alias_tok = tokens.get(alias_idx)?;
    if alias_tok.kind != TokenKind::Ident {
        return None;
    }
    let for_idx = next_non_comment(tokens, alias_idx + 1, end);
    if for_idx >= end || !token_matches_keyword(source, &tokens[for_idx], "for") {
        return None;
    }
    let interface_start = next_non_comment(tokens, for_idx + 1, end);
    if interface_start >= end {
        return None;
    }
    let tilde_idx = (interface_start..end).find(|&idx| {
        tokens[idx].kind == TokenKind::Tilde
            && tokens[interface_start..idx]
                .iter()
                .any(|token| token.kind != TokenKind::Comment)
    })?;
    let member_idx = next_non_comment(tokens, tilde_idx + 1, end);
    let member_tok = tokens.get(member_idx)?;
    if member_tok.kind != TokenKind::Ident || member_idx + 1 != end {
        return None;
    }

    let mut children = Vec::new();
    push_token_range(b, &mut children, tokens, start, alias_idx);
    let alias_leaf = token_leaf(b, alias_tok);
    children.push(b.branch(
        SyntaxKind::AliasName,
        alias_tok.range.clone(),
        &[alias_leaf],
    ));
    push_token_range(b, &mut children, tokens, alias_idx + 1, interface_start);
    children.push(build_type_ref_node(
        b,
        source,
        &tokens[interface_start..tilde_idx],
    ));
    push_token_range(b, &mut children, tokens, tilde_idx, member_idx);
    let member_leaf = token_leaf(b, member_tok);
    children.push(b.branch(
        SyntaxKind::AliasMember,
        member_tok.range.clone(),
        &[member_leaf],
    ));

    Some(b.branch(
        SyntaxKind::AliasEntry,
        tokens[start].range.start..tokens[end - 1].range.end,
        &children,
    ))
}

fn build_aliases_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    let mut cursor = idx;
    while cursor <= period_i {
        let token = &tokens[cursor];
        if matches!(
            token.kind,
            TokenKind::Comment | TokenKind::Colon | TokenKind::Comma
        ) {
            children.push(token_leaf(b, token));
            cursor += 1;
            continue;
        }
        if token.kind == TokenKind::Period {
            children.push(token_leaf(b, token));
            break;
        }
        if cursor == idx {
            children.push(token_leaf(b, token));
            cursor += 1;
            continue;
        }
        let entry_end = scan_expr_end(source, tokens, cursor, period_i, &[], &[TokenKind::Comma]);
        if let Some(entry) = build_alias_entry_node(b, source, tokens, cursor, entry_end) {
            children.push(entry);
            cursor = entry_end;
            continue;
        }
        children.push(token_leaf(b, token));
        cursor += 1;
    }
    children
}

fn build_clear_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    let mut cursor = idx;
    while cursor <= period_i {
        let token = &tokens[cursor];
        if matches!(
            token.kind,
            TokenKind::Comment | TokenKind::Colon | TokenKind::Comma
        ) {
            children.push(token_leaf(b, token));
            cursor += 1;
            continue;
        }
        if token.kind == TokenKind::Period {
            children.push(token_leaf(b, token));
            break;
        }
        if cursor == idx {
            children.push(token_leaf(b, token));
            cursor += 1;
            continue;
        }
        let end = scan_expr_end(
            source,
            tokens,
            cursor,
            period_i,
            &["with", "in"],
            &[TokenKind::Comma],
        );
        if cursor < end {
            children.push(build_wrapped_expr_child(
                b,
                source,
                tokens,
                cursor,
                end,
                SyntaxKind::ClearOperand,
            ));
            cursor = end;
            continue;
        }
        children.push(token_leaf(b, token));
        cursor += 1;
    }
    children
}

fn build_convert_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let next_idx = next_non_comment(tokens, idx + 1, period_i);
    if next_idx < period_i && token_matches_keyword(source, &tokens[next_idx], "time") {
        let stamp_idx = next_non_comment(tokens, next_idx + 1, period_i);
        if stamp_idx < period_i && token_matches_keyword(source, &tokens[stamp_idx], "stamp") {
            return build_convert_time_stamp_stmt_children(b, source, tokens, idx, period_i);
        }
    }
    build_convert_date_into_time_stamp_stmt_children(b, source, tokens, idx, period_i)
}

fn build_convert_date_into_time_stamp_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    children.push(token_leaf(b, &tokens[idx]));
    let mut cursor = idx + 1;
    let mut trailing_start = cursor;
    if cursor < period_i && token_matches_keyword(source, &tokens[cursor], "date") {
        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
    }
    let date_start = next_non_comment(tokens, cursor, period_i);
    let date_end = scan_expr_end(source, tokens, date_start, period_i, &["time", "into"], &[]);
    if date_start < date_end {
        push_token_range(b, &mut children, tokens, cursor, date_start);
        children.push(build_wrapped_expr_child(
            b,
            source,
            tokens,
            date_start,
            date_end,
            SyntaxKind::ConvertOperand,
        ));
    }
    cursor = date_end;

    if cursor < period_i
        && token_matches_keyword(source, &tokens[cursor], "time")
        && !tokens
            .get(next_non_comment(tokens, cursor + 1, period_i))
            .is_some_and(|token| token_matches_keyword(source, token, "zone"))
    {
        children.push(token_leaf(b, &tokens[cursor]));
        let time_start = next_non_comment(tokens, cursor + 1, period_i);
        let time_end = scan_expr_end(source, tokens, time_start, period_i, &["into"], &[]);
        if time_start < time_end {
            push_token_range(b, &mut children, tokens, cursor + 1, time_start);
            children.push(build_wrapped_expr_child(
                b,
                source,
                tokens,
                time_start,
                time_end,
                SyntaxKind::ConvertOperand,
            ));
        }
        cursor = time_end;
    }

    if cursor < period_i && token_matches_keyword(source, &tokens[cursor], "into") {
        children.push(token_leaf(b, &tokens[cursor]));
        cursor = next_non_comment(tokens, cursor + 1, period_i);
        if cursor < period_i && token_matches_keyword(source, &tokens[cursor], "time") {
            children.push(token_leaf(b, &tokens[cursor]));
            cursor = next_non_comment(tokens, cursor + 1, period_i);
        }
        if cursor < period_i && token_matches_keyword(source, &tokens[cursor], "stamp") {
            children.push(token_leaf(b, &tokens[cursor]));
            cursor = next_non_comment(tokens, cursor + 1, period_i);
        }
        let target_start = cursor;
        let target_end = scan_expr_end(source, tokens, target_start, period_i, &["time"], &[]);
        if target_start < target_end {
            children.push(build_wrapped_expr_child(
                b,
                source,
                tokens,
                target_start,
                target_end,
                SyntaxKind::ConvertTargetOperand,
            ));
        }
        cursor = target_end;
        trailing_start = cursor;
    }

    if cursor < period_i
        && token_matches_keyword(source, &tokens[cursor], "time")
        && tokens
            .get(next_non_comment(tokens, cursor + 1, period_i))
            .is_some_and(|token| token_matches_keyword(source, token, "zone"))
    {
        children.push(token_leaf(b, &tokens[cursor]));
        let zone_keyword_idx = next_non_comment(tokens, cursor + 1, period_i);
        push_token_range(
            b,
            &mut children,
            tokens,
            cursor + 1,
            zone_keyword_idx.saturating_add(1),
        );
        let zone_start = next_non_comment(
            tokens,
            next_non_comment(tokens, cursor + 1, period_i) + 1,
            period_i,
        );
        if zone_start < period_i {
            push_token_range(b, &mut children, tokens, zone_keyword_idx + 1, zone_start);
            children.push(build_wrapped_expr_child(
                b,
                source,
                tokens,
                zone_start,
                period_i,
                SyntaxKind::ConvertTimeZoneOperand,
            ));
        }
        trailing_start = period_i;
    }

    push_token_range(b, &mut children, tokens, trailing_start, period_i + 1);
    children
}

fn build_convert_time_stamp_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    children.push(token_leaf(b, &tokens[idx]));

    let mut cursor = idx + 1;
    let time_idx = next_non_comment(tokens, cursor, period_i);
    push_token_range(b, &mut children, tokens, cursor, time_idx);
    if time_idx >= period_i || !token_matches_keyword(source, &tokens[time_idx], "time") {
        push_token_range(b, &mut children, tokens, time_idx, period_i + 1);
        return children;
    }
    children.push(token_leaf(b, &tokens[time_idx]));

    cursor = time_idx + 1;
    let stamp_idx = next_non_comment(tokens, cursor, period_i);
    push_token_range(b, &mut children, tokens, cursor, stamp_idx);
    if stamp_idx >= period_i || !token_matches_keyword(source, &tokens[stamp_idx], "stamp") {
        push_token_range(b, &mut children, tokens, stamp_idx, period_i + 1);
        return children;
    }
    children.push(token_leaf(b, &tokens[stamp_idx]));

    cursor = stamp_idx + 1;
    let source_start = next_non_comment(tokens, cursor, period_i);
    let Some(time_zone_idx) = find_top_level_keyword_sequence_start(
        source,
        tokens,
        source_start,
        period_i,
        &["time", "zone"],
    ) else {
        push_token_range(b, &mut children, tokens, cursor, period_i + 1);
        return children;
    };
    push_token_range(b, &mut children, tokens, cursor, source_start);
    if source_start < time_zone_idx {
        children.push(build_wrapped_expr_child(
            b,
            source,
            tokens,
            source_start,
            time_zone_idx,
            SyntaxKind::ConvertOperand,
        ));
    }

    children.push(token_leaf(b, &tokens[time_zone_idx]));
    let zone_idx = next_non_comment(tokens, time_zone_idx + 1, period_i);
    push_token_range(b, &mut children, tokens, time_zone_idx + 1, zone_idx);
    if zone_idx >= period_i || !token_matches_keyword(source, &tokens[zone_idx], "zone") {
        push_token_range(b, &mut children, tokens, zone_idx, period_i + 1);
        return children;
    }
    children.push(token_leaf(b, &tokens[zone_idx]));

    cursor = zone_idx + 1;
    let zone_start = next_non_comment(tokens, cursor, period_i);
    let Some(into_idx) = find_top_level_keyword_index(source, tokens, zone_start, period_i, "into")
    else {
        push_token_range(b, &mut children, tokens, cursor, period_i + 1);
        return children;
    };
    push_token_range(b, &mut children, tokens, cursor, zone_start);
    if zone_start < into_idx {
        children.push(build_wrapped_expr_child(
            b,
            source,
            tokens,
            zone_start,
            into_idx,
            SyntaxKind::ConvertTimeZoneOperand,
        ));
    }

    children.push(token_leaf(b, &tokens[into_idx]));
    cursor = into_idx + 1;

    let daylight_idx = find_top_level_keyword_sequence_start(
        source,
        tokens,
        cursor,
        period_i,
        &["daylight", "saving", "time"],
    );

    let date_idx = next_non_comment(tokens, cursor, period_i);
    if date_idx < period_i && token_matches_keyword(source, &tokens[date_idx], "date") {
        push_token_range(b, &mut children, tokens, cursor, date_idx);
        children.push(token_leaf(b, &tokens[date_idx]));
        let target_start = next_non_comment(tokens, date_idx + 1, period_i);
        let time_target_idx =
            find_top_level_keyword_index(source, tokens, target_start, period_i, "time").filter(
                |time_idx| {
                    daylight_idx
                        .map(|daylight_idx| *time_idx < daylight_idx)
                        .unwrap_or(true)
                },
            );
        let target_end = time_target_idx.or(daylight_idx).unwrap_or(period_i);
        push_token_range(b, &mut children, tokens, date_idx + 1, target_start);
        if let Some(target) = build_wrapped_expr_or_data_inline_decl_child(
            b,
            source,
            tokens,
            target_start,
            target_end,
            SyntaxKind::ConvertDateTarget,
        ) {
            children.push(target);
        }
        push_token_range(
            b,
            &mut children,
            tokens,
            trim_trailing_comments(tokens, target_start, target_end),
            target_end,
        );
        cursor = target_end;
    }

    let time_idx = next_non_comment(tokens, cursor, period_i);
    if time_idx < period_i
        && token_matches_keyword(source, &tokens[time_idx], "time")
        && !tokens
            .get(next_non_comment(tokens, time_idx + 1, period_i))
            .is_some_and(|token| token_matches_keyword(source, token, "zone"))
    {
        push_token_range(b, &mut children, tokens, cursor, time_idx);
        children.push(token_leaf(b, &tokens[time_idx]));
        let target_start = next_non_comment(tokens, time_idx + 1, period_i);
        let target_end = daylight_idx.unwrap_or(period_i);
        push_token_range(b, &mut children, tokens, time_idx + 1, target_start);
        if let Some(target) = build_wrapped_expr_or_data_inline_decl_child(
            b,
            source,
            tokens,
            target_start,
            target_end,
            SyntaxKind::ConvertTimeTarget,
        ) {
            children.push(target);
        }
        push_token_range(
            b,
            &mut children,
            tokens,
            trim_trailing_comments(tokens, target_start, target_end),
            target_end,
        );
        cursor = target_end;
    }

    if let Some(daylight_idx) = daylight_idx {
        push_token_range(b, &mut children, tokens, cursor, daylight_idx);
        children.push(token_leaf(b, &tokens[daylight_idx]));
        let saving_idx = next_non_comment(tokens, daylight_idx + 1, period_i);
        push_token_range(b, &mut children, tokens, daylight_idx + 1, saving_idx);
        if saving_idx >= period_i || !token_matches_keyword(source, &tokens[saving_idx], "saving") {
            push_token_range(b, &mut children, tokens, saving_idx, period_i + 1);
            return children;
        }
        children.push(token_leaf(b, &tokens[saving_idx]));

        let dst_time_idx = next_non_comment(tokens, saving_idx + 1, period_i);
        push_token_range(b, &mut children, tokens, saving_idx + 1, dst_time_idx);
        if dst_time_idx >= period_i || !token_matches_keyword(source, &tokens[dst_time_idx], "time")
        {
            push_token_range(b, &mut children, tokens, dst_time_idx, period_i + 1);
            return children;
        }
        children.push(token_leaf(b, &tokens[dst_time_idx]));

        let target_start = next_non_comment(tokens, dst_time_idx + 1, period_i);
        push_token_range(b, &mut children, tokens, dst_time_idx + 1, target_start);
        if let Some(target) = build_wrapped_expr_or_data_inline_decl_child(
            b,
            source,
            tokens,
            target_start,
            period_i,
            SyntaxKind::ConvertDaylightSavingTarget,
        ) {
            children.push(target);
        }
        push_token_range(
            b,
            &mut children,
            tokens,
            trim_trailing_comments(tokens, target_start, period_i),
            period_i,
        );
        cursor = period_i;
    }

    push_token_range(b, &mut children, tokens, cursor, period_i + 1);
    children
}

fn build_describe_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = vec![token_leaf(b, &tokens[idx])];
    let table_idx = next_non_comment(tokens, idx + 1, period_i);
    if table_idx >= period_i || !token_matches_keyword(source, &tokens[table_idx], "table") {
        return tokens[idx..=period_i]
            .iter()
            .map(|token| token_leaf(b, token))
            .collect();
    }
    push_token_range(b, &mut children, tokens, idx + 1, table_idx);
    children.push(token_leaf(b, &tokens[table_idx]));

    let mut cursor = table_idx + 1;
    while cursor <= period_i {
        let token = &tokens[cursor];
        if matches!(
            token.kind,
            TokenKind::Comment | TokenKind::Colon | TokenKind::Comma
        ) {
            children.push(token_leaf(b, token));
            cursor += 1;
            continue;
        }
        if token.kind == TokenKind::Period {
            children.push(token_leaf(b, token));
            break;
        }

        let entry_end = scan_expr_end(source, tokens, cursor, period_i, &[], &[TokenKind::Comma]);
        let Some(lines_idx) =
            find_top_level_keyword_index(source, tokens, cursor, entry_end, "lines")
        else {
            children.push(token_leaf(b, token));
            cursor += 1;
            continue;
        };

        let source_start = next_non_comment(tokens, cursor, lines_idx);
        if cursor < source_start {
            push_token_range(b, &mut children, tokens, cursor, source_start);
        }
        if source_start < lines_idx {
            children.push(build_wrapped_expr_child(
                b,
                source,
                tokens,
                source_start,
                lines_idx,
                SyntaxKind::DescribeTableOperand,
            ));
        }

        children.push(token_leaf(b, &tokens[lines_idx]));

        let target_start = next_non_comment(tokens, lines_idx + 1, entry_end);
        if lines_idx + 1 < target_start {
            push_token_range(b, &mut children, tokens, lines_idx + 1, target_start);
        }
        if let Some(target) = build_wrapped_expr_or_data_inline_decl_child(
            b,
            source,
            tokens,
            target_start,
            entry_end,
            SyntaxKind::DescribeLinesTarget,
        ) {
            children.push(target);
            push_token_range(
                b,
                &mut children,
                tokens,
                trim_trailing_comments(tokens, target_start, entry_end),
                entry_end,
            );
        } else {
            push_token_range(b, &mut children, tokens, target_start, entry_end);
        }
        cursor = entry_end;
    }
    children
}

fn build_replace_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    children.push(token_leaf(b, &tokens[idx]));
    let mut cursor = idx + 1;
    if cursor < period_i
        && (token_matches_keyword(source, &tokens[cursor], "first")
            || token_matches_keyword(source, &tokens[cursor], "all"))
    {
        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
        if cursor < period_i && token_matches_occurrence_keyword(source, &tokens[cursor]) {
            children.push(token_leaf(b, &tokens[cursor]));
            cursor += 1;
        }
    }
    if cursor < period_i && token_matches_keyword(source, &tokens[cursor], "of") {
        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
    }
    if cursor < period_i && token_matches_keyword(source, &tokens[cursor], "regex") {
        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
    }

    let source_start = next_non_comment(tokens, cursor, period_i);
    let source_end = scan_expr_end(source, tokens, source_start, period_i, &["in", "with"], &[]);
    if source_start < source_end {
        push_token_range(b, &mut children, tokens, cursor, source_start);
        children.push(build_wrapped_expr_child(
            b,
            source,
            tokens,
            source_start,
            source_end,
            SyntaxKind::ReplacePatternOperand,
        ));
    }
    cursor = source_end;

    while cursor < period_i {
        cursor = next_non_comment(tokens, cursor, period_i);
        if cursor >= period_i {
            break;
        }
        if token_matches_keyword(source, &tokens[cursor], "in") {
            let mode_token = next_non_comment(tokens, cursor + 1, period_i);
            if tokens.get(mode_token).is_some_and(|token| {
                token_matches_keyword(source, token, "character")
                    || token_matches_keyword(source, token, "byte")
            }) && tokens
                .get(next_non_comment(tokens, mode_token + 1, period_i))
                .is_some_and(|token| token_matches_keyword(source, token, "mode"))
            {
                cursor = next_non_comment(tokens, mode_token + 2, period_i);
                continue;
            }
            children.push(token_leaf(b, &tokens[cursor]));
            let mut target_start = next_non_comment(tokens, cursor + 1, period_i);
            push_token_range(b, &mut children, tokens, cursor + 1, target_start);
            if target_start < period_i
                && token_matches_keyword(source, &tokens[target_start], "table")
            {
                children.push(token_leaf(b, &tokens[target_start]));
                let after_table = next_non_comment(tokens, target_start + 1, period_i);
                push_token_range(b, &mut children, tokens, target_start + 1, after_table);
                target_start = after_table;
            }
            let target_end =
                scan_expr_end(source, tokens, target_start, period_i, &["with", "in"], &[]);
            if target_start < target_end {
                children.push(build_wrapped_expr_child(
                    b,
                    source,
                    tokens,
                    target_start,
                    target_end,
                    SyntaxKind::ReplaceTargetOperand,
                ));
            }
            cursor = target_end;
            continue;
        }
        if token_matches_keyword(source, &tokens[cursor], "with") {
            children.push(token_leaf(b, &tokens[cursor]));
            let replacement_start = next_non_comment(tokens, cursor + 1, period_i);
            let replacement_end =
                scan_expr_end(source, tokens, replacement_start, period_i, &["in"], &[]);
            if replacement_start < replacement_end {
                push_token_range(b, &mut children, tokens, cursor + 1, replacement_start);
                children.push(build_wrapped_expr_child(
                    b,
                    source,
                    tokens,
                    replacement_start,
                    replacement_end,
                    SyntaxKind::ReplaceWithOperand,
                ));
            }
            cursor = replacement_end;
            continue;
        }
        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
    }

    children.push(token_leaf(b, &tokens[period_i]));
    children
}

fn build_wait_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = vec![token_leaf(b, &tokens[idx])];
    let mut cursor = idx + 1;
    if cursor < period_i && token_matches_keyword(source, &tokens[cursor], "up") {
        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
    }
    if cursor < period_i && token_matches_keyword(source, &tokens[cursor], "to") {
        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
    }
    let expr_start = next_non_comment(tokens, cursor, period_i);
    let expr_end = find_top_level_keyword_index(source, tokens, expr_start, period_i, "seconds")
        .unwrap_or(period_i);
    if expr_start < expr_end {
        push_token_range(b, &mut children, tokens, cursor, expr_start);
        children.push(build_wrapped_expr_child(
            b,
            source,
            tokens,
            expr_start,
            expr_end,
            SyntaxKind::WaitOperand,
        ));
    } else {
        push_token_range(b, &mut children, tokens, cursor, expr_end);
    }
    push_token_range(b, &mut children, tokens, expr_end, period_i + 1);
    children
}

fn find_top_level_token_kind_index(
    tokens: &[Token],
    start: usize,
    end: usize,
    kind: TokenKind,
) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            idx += 1;
            continue;
        }
        if paren == 0 && bracket == 0 && brace == 0 && token.kind == kind {
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

fn find_top_level_any_keyword_index(
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
    keywords: &[&str],
) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            idx += 1;
            continue;
        }
        if paren == 0
            && bracket == 0
            && brace == 0
            && keywords
                .iter()
                .any(|keyword| token_matches_keyword(source, token, keyword))
        {
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

fn push_arithmetic_operand_child(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
    kind: SyntaxKind,
) -> Option<NodeId> {
    let value_start = next_non_comment(tokens, start, end);
    let value_end = trim_trailing_comments(tokens, value_start, end);
    push_token_range(b, children, tokens, start, value_start);
    if value_start >= value_end {
        push_token_range(b, children, tokens, value_start, end);
        return None;
    }

    let operand = if kind == SyntaxKind::ArithmeticTargetOperand {
        build_wrapped_expr_or_data_inline_decl_child(
            b,
            source,
            tokens,
            value_start,
            value_end,
            kind,
        )
        .unwrap_or_else(|| {
            build_wrapped_expr_child(b, source, tokens, value_start, value_end, kind)
        })
    } else {
        build_wrapped_expr_child(b, source, tokens, value_start, value_end, kind)
    };
    children.push(operand);
    push_token_range(b, children, tokens, value_end, end);
    Some(operand)
}

fn build_compute_arithmetic_entry_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
) -> Vec<NodeId> {
    let mut children = Vec::new();
    let exact_idx = next_non_comment(tokens, entry_start, entry_end);
    let mut cursor = entry_start;
    if exact_idx < entry_end && token_matches_keyword(source, &tokens[exact_idx], "exact") {
        push_token_range(b, &mut children, tokens, cursor, exact_idx + 1);
        cursor = exact_idx + 1;
    }

    let Some(eq_idx) = find_top_level_token_kind_index(tokens, cursor, entry_end, TokenKind::Eq)
    else {
        push_token_range(b, &mut children, tokens, cursor, entry_end);
        return children;
    };

    push_arithmetic_operand_child(
        b,
        &mut children,
        source,
        tokens,
        cursor,
        eq_idx,
        SyntaxKind::ArithmeticTargetOperand,
    );
    children.push(token_leaf(b, &tokens[eq_idx]));
    push_arithmetic_operand_child(
        b,
        &mut children,
        source,
        tokens,
        eq_idx + 1,
        entry_end,
        SyntaxKind::ArithmeticSourceOperand,
    );
    children
}

fn arithmetic_operator_keywords(kind: SyntaxKind) -> &'static [&'static str] {
    match kind {
        SyntaxKind::AddStmt => &["to"],
        SyntaxKind::SubtractStmt => &["from"],
        SyntaxKind::MultiplyStmt => &["by"],
        SyntaxKind::DivideStmt => &["by", "into"],
        _ => &[],
    }
}

fn build_classic_arithmetic_entry_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
    kind: SyntaxKind,
) -> Vec<NodeId> {
    let mut children = Vec::new();
    let Some(operator_idx) = find_top_level_any_keyword_index(
        source,
        tokens,
        entry_start,
        entry_end,
        arithmetic_operator_keywords(kind),
    ) else {
        push_token_range(b, &mut children, tokens, entry_start, entry_end);
        return children;
    };
    let giving_idx =
        find_top_level_keyword_index(source, tokens, operator_idx + 1, entry_end, "giving");
    let after_operator_end = giving_idx.unwrap_or(entry_end);
    let operator_is_by = token_matches_keyword(source, &tokens[operator_idx], "by");

    let first_operand_kind =
        if kind == SyntaxKind::DivideStmt && operator_is_by && giving_idx.is_none() {
            SyntaxKind::ArithmeticTargetOperand
        } else {
            SyntaxKind::ArithmeticSourceOperand
        };
    push_arithmetic_operand_child(
        b,
        &mut children,
        source,
        tokens,
        entry_start,
        operator_idx,
        first_operand_kind,
    );

    children.push(token_leaf(b, &tokens[operator_idx]));

    let second_operand_kind =
        if giving_idx.is_some() || (kind == SyntaxKind::DivideStmt && operator_is_by) {
            SyntaxKind::ArithmeticSourceOperand
        } else {
            SyntaxKind::ArithmeticTargetOperand
        };
    push_arithmetic_operand_child(
        b,
        &mut children,
        source,
        tokens,
        operator_idx + 1,
        after_operator_end,
        second_operand_kind,
    );

    if let Some(giving_idx) = giving_idx {
        children.push(token_leaf(b, &tokens[giving_idx]));
        push_arithmetic_operand_child(
            b,
            &mut children,
            source,
            tokens,
            giving_idx + 1,
            entry_end,
            SyntaxKind::ArithmeticTargetOperand,
        );
    }

    children
}

fn build_arithmetic_entry(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
    kind: SyntaxKind,
) -> Option<NodeId> {
    if entry_start >= entry_end {
        return None;
    }
    let children = if kind == SyntaxKind::ComputeStmt {
        build_compute_arithmetic_entry_children(b, source, tokens, entry_start, entry_end)
    } else {
        build_classic_arithmetic_entry_children(b, source, tokens, entry_start, entry_end, kind)
    };
    Some(b.branch(
        SyntaxKind::ArithmeticEntry,
        tokens[entry_start].range.start..tokens[entry_end - 1].range.end,
        &children,
    ))
}

fn build_arithmetic_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
    kind: SyntaxKind,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    children.push(token_leaf(b, &tokens[idx]));
    let mut cursor = idx + 1;

    let colon_idx = next_non_comment(tokens, cursor, period_i);
    if colon_idx < period_i && tokens[colon_idx].kind == TokenKind::Colon {
        push_token_range(b, &mut children, tokens, cursor, colon_idx + 1);
        cursor = colon_idx + 1;
    }

    while cursor < period_i {
        let entry_start = next_non_comment(tokens, cursor, period_i);
        push_token_range(b, &mut children, tokens, cursor, entry_start);
        if entry_start >= period_i {
            break;
        }
        let entry_end =
            find_top_level_token_kind_index(tokens, entry_start, period_i, TokenKind::Comma)
                .unwrap_or(period_i);
        if let Some(entry) = build_arithmetic_entry(b, source, tokens, entry_start, entry_end, kind)
        {
            children.push(entry);
        }
        cursor = entry_end;
        if cursor < period_i && tokens[cursor].kind == TokenKind::Comma {
            children.push(token_leaf(b, &tokens[cursor]));
            cursor += 1;
        }
    }

    push_token_range(b, &mut children, tokens, cursor, period_i + 1);
    children
}

fn push_text_operand_child(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
    kind: SyntaxKind,
) -> Option<NodeId> {
    let value_start = next_non_comment(tokens, start, end);
    let value_end = trim_trailing_comments(tokens, value_start, end);
    push_token_range(b, children, tokens, start, value_start);
    if value_start >= value_end {
        push_token_range(b, children, tokens, value_start, end);
        return None;
    }

    let operand = if kind == SyntaxKind::TextTargetOperand {
        build_wrapped_expr_or_data_inline_decl_child(
            b,
            source,
            tokens,
            value_start,
            value_end,
            kind,
        )
        .unwrap_or_else(|| {
            build_wrapped_expr_child(b, source, tokens, value_start, value_end, kind)
        })
    } else {
        build_wrapped_expr_child(b, source, tokens, value_start, value_end, kind)
    };
    children.push(operand);
    push_token_range(b, children, tokens, value_end, end);
    Some(operand)
}

fn build_pack_like_text_entry_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
) -> Vec<NodeId> {
    let mut children = Vec::new();
    let Some(to_idx) = find_top_level_keyword_index(source, tokens, entry_start, entry_end, "to")
    else {
        push_token_range(b, &mut children, tokens, entry_start, entry_end);
        return children;
    };

    push_text_operand_child(
        b,
        &mut children,
        source,
        tokens,
        entry_start,
        to_idx,
        SyntaxKind::TextSourceOperand,
    );
    children.push(token_leaf(b, &tokens[to_idx]));
    push_text_operand_child(
        b,
        &mut children,
        source,
        tokens,
        to_idx + 1,
        entry_end,
        SyntaxKind::TextTargetOperand,
    );
    children
}

fn build_overlay_text_entry_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
) -> Vec<NodeId> {
    let mut children = Vec::new();
    let Some(with_idx) =
        find_top_level_keyword_index(source, tokens, entry_start, entry_end, "with")
    else {
        push_token_range(b, &mut children, tokens, entry_start, entry_end);
        return children;
    };

    push_text_operand_child(
        b,
        &mut children,
        source,
        tokens,
        entry_start,
        with_idx,
        SyntaxKind::TextTargetOperand,
    );
    children.push(token_leaf(b, &tokens[with_idx]));

    let only_idx = find_top_level_keyword_index(source, tokens, with_idx + 1, entry_end, "only");
    let mask_end = only_idx.unwrap_or(entry_end);
    push_text_operand_child(
        b,
        &mut children,
        source,
        tokens,
        with_idx + 1,
        mask_end,
        SyntaxKind::TextSourceOperand,
    );
    if let Some(only_idx) = only_idx {
        children.push(token_leaf(b, &tokens[only_idx]));
        push_text_operand_child(
            b,
            &mut children,
            source,
            tokens,
            only_idx + 1,
            entry_end,
            SyntaxKind::TextSourceOperand,
        );
    }
    children
}

fn build_search_text_entry_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
) -> Vec<NodeId> {
    let mut children = Vec::new();
    let Some(for_idx) = find_top_level_keyword_index(source, tokens, entry_start, entry_end, "for")
    else {
        push_token_range(b, &mut children, tokens, entry_start, entry_end);
        return children;
    };

    push_text_operand_child(
        b,
        &mut children,
        source,
        tokens,
        entry_start,
        for_idx,
        SyntaxKind::TextSourceOperand,
    );
    children.push(token_leaf(b, &tokens[for_idx]));

    let pattern_end = find_top_level_any_keyword_index(
        source,
        tokens,
        for_idx + 1,
        entry_end,
        &["starting", "ending", "and", "abbreviated"],
    )
    .unwrap_or(entry_end);
    push_text_operand_child(
        b,
        &mut children,
        source,
        tokens,
        for_idx + 1,
        pattern_end,
        SyntaxKind::TextSourceOperand,
    );

    let mut cursor = pattern_end;
    while cursor < entry_end {
        if token_matches_keyword(source, &tokens[cursor], "starting")
            || token_matches_keyword(source, &tokens[cursor], "ending")
        {
            children.push(token_leaf(b, &tokens[cursor]));
            let at_idx = next_non_comment(tokens, cursor + 1, entry_end);
            push_token_range(b, &mut children, tokens, cursor + 1, at_idx);
            let operand_start =
                if at_idx < entry_end && token_matches_keyword(source, &tokens[at_idx], "at") {
                    children.push(token_leaf(b, &tokens[at_idx]));
                    at_idx + 1
                } else {
                    at_idx
                };
            let operand_end = find_top_level_any_keyword_index(
                source,
                tokens,
                operand_start,
                entry_end,
                &["starting", "ending", "and", "abbreviated"],
            )
            .unwrap_or(entry_end);
            push_text_operand_child(
                b,
                &mut children,
                source,
                tokens,
                operand_start,
                operand_end,
                SyntaxKind::TextSourceOperand,
            );
            cursor = operand_end;
            continue;
        }
        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
    }
    children
}

fn push_translate_code_page_or_number_format_clause(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    cursor: usize,
    entry_end: usize,
) -> Option<usize> {
    let lead_idx = next_non_comment(tokens, cursor + 1, entry_end);
    if lead_idx >= entry_end {
        return None;
    }

    let second_keyword = if token_matches_keyword(source, &tokens[lead_idx], "code") {
        "page"
    } else if token_matches_keyword(source, &tokens[lead_idx], "number") {
        "format"
    } else {
        return None;
    };

    push_token_range(b, children, tokens, cursor + 1, lead_idx);
    children.push(token_leaf(b, &tokens[lead_idx]));
    let second_idx = next_non_comment(tokens, lead_idx + 1, entry_end);
    push_token_range(b, children, tokens, lead_idx + 1, second_idx);
    if second_idx >= entry_end
        || !token_matches_keyword(source, &tokens[second_idx], second_keyword)
    {
        return Some(second_idx);
    }
    children.push(token_leaf(b, &tokens[second_idx]));
    let operand_start = second_idx + 1;
    let operand_end = find_top_level_any_keyword_index(
        source,
        tokens,
        operand_start,
        entry_end,
        &["to", "from", "using"],
    )
    .unwrap_or(entry_end);
    push_text_operand_child(
        b,
        children,
        source,
        tokens,
        operand_start,
        operand_end,
        SyntaxKind::TextSourceOperand,
    );
    Some(operand_end)
}

fn build_translate_text_entry_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
) -> Vec<NodeId> {
    let mut children = Vec::new();
    let target_end = find_top_level_any_keyword_index(
        source,
        tokens,
        entry_start,
        entry_end,
        &["to", "using", "from"],
    )
    .unwrap_or(entry_end);
    push_text_operand_child(
        b,
        &mut children,
        source,
        tokens,
        entry_start,
        target_end,
        SyntaxKind::TextTargetOperand,
    );

    let mut cursor = target_end;
    while cursor < entry_end {
        if token_matches_keyword(source, &tokens[cursor], "using") {
            children.push(token_leaf(b, &tokens[cursor]));
            let operand_end = find_top_level_any_keyword_index(
                source,
                tokens,
                cursor + 1,
                entry_end,
                &["to", "from"],
            )
            .unwrap_or(entry_end);
            push_text_operand_child(
                b,
                &mut children,
                source,
                tokens,
                cursor + 1,
                operand_end,
                SyntaxKind::TextSourceOperand,
            );
            cursor = operand_end;
            continue;
        }
        if token_matches_keyword(source, &tokens[cursor], "to")
            || token_matches_keyword(source, &tokens[cursor], "from")
        {
            children.push(token_leaf(b, &tokens[cursor]));
            if let Some(next_cursor) = push_translate_code_page_or_number_format_clause(
                b,
                &mut children,
                source,
                tokens,
                cursor,
                entry_end,
            ) {
                cursor = next_cursor;
                continue;
            }
            cursor += 1;
            continue;
        }
        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
    }
    children
}

fn build_shift_text_entry_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
) -> Vec<NodeId> {
    let mut children = Vec::new();
    let target_end = find_top_level_any_keyword_index(
        source,
        tokens,
        entry_start,
        entry_end,
        &["by", "up", "left", "right", "circular", "deleting", "in"],
    )
    .unwrap_or(entry_end);
    push_text_operand_child(
        b,
        &mut children,
        source,
        tokens,
        entry_start,
        target_end,
        SyntaxKind::TextTargetOperand,
    );

    let mut cursor = target_end;
    while cursor < entry_end {
        if token_matches_keyword(source, &tokens[cursor], "by") {
            children.push(token_leaf(b, &tokens[cursor]));
            let operand_end = find_top_level_any_keyword_index(
                source,
                tokens,
                cursor + 1,
                entry_end,
                &[
                    "places", "left", "right", "circular", "deleting", "in", "up",
                ],
            )
            .unwrap_or(entry_end);
            push_text_operand_child(
                b,
                &mut children,
                source,
                tokens,
                cursor + 1,
                operand_end,
                SyntaxKind::TextSourceOperand,
            );
            cursor = operand_end;
            continue;
        }
        if token_matches_keyword(source, &tokens[cursor], "up") {
            children.push(token_leaf(b, &tokens[cursor]));
            let to_idx = next_non_comment(tokens, cursor + 1, entry_end);
            push_token_range(b, &mut children, tokens, cursor + 1, to_idx);
            if to_idx < entry_end && token_matches_keyword(source, &tokens[to_idx], "to") {
                children.push(token_leaf(b, &tokens[to_idx]));
                let operand_end = find_top_level_any_keyword_index(
                    source,
                    tokens,
                    to_idx + 1,
                    entry_end,
                    &[
                        "by", "places", "left", "right", "circular", "deleting", "in",
                    ],
                )
                .unwrap_or(entry_end);
                push_text_operand_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    to_idx + 1,
                    operand_end,
                    SyntaxKind::TextSourceOperand,
                );
                cursor = operand_end;
                continue;
            }
            cursor = to_idx;
            continue;
        }
        if token_matches_keyword(source, &tokens[cursor], "leading")
            || token_matches_keyword(source, &tokens[cursor], "trailing")
        {
            children.push(token_leaf(b, &tokens[cursor]));
            let operand_end = find_top_level_any_keyword_index(
                source,
                tokens,
                cursor + 1,
                entry_end,
                &[
                    "by", "places", "left", "right", "circular", "deleting", "in", "up",
                ],
            )
            .unwrap_or(entry_end);
            push_text_operand_child(
                b,
                &mut children,
                source,
                tokens,
                cursor + 1,
                operand_end,
                SyntaxKind::TextSourceOperand,
            );
            cursor = operand_end;
            continue;
        }
        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
    }
    children
}

fn build_classic_text_entry_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
    kind: SyntaxKind,
) -> Vec<NodeId> {
    match kind {
        SyntaxKind::TranslateStmt => {
            build_translate_text_entry_children(b, source, tokens, entry_start, entry_end)
        }
        SyntaxKind::ShiftStmt => {
            build_shift_text_entry_children(b, source, tokens, entry_start, entry_end)
        }
        SyntaxKind::SearchStmt => {
            build_search_text_entry_children(b, source, tokens, entry_start, entry_end)
        }
        SyntaxKind::OverlayStmt => {
            build_overlay_text_entry_children(b, source, tokens, entry_start, entry_end)
        }
        SyntaxKind::PackStmt | SyntaxKind::UnpackStmt => {
            build_pack_like_text_entry_children(b, source, tokens, entry_start, entry_end)
        }
        _ => {
            let mut children = Vec::new();
            push_token_range(b, &mut children, tokens, entry_start, entry_end);
            children
        }
    }
}

fn build_classic_text_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
    kind: SyntaxKind,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    children.push(token_leaf(b, &tokens[idx]));
    let mut cursor = idx + 1;

    let colon_idx = next_non_comment(tokens, cursor, period_i);
    if colon_idx < period_i && tokens[colon_idx].kind == TokenKind::Colon {
        push_token_range(b, &mut children, tokens, cursor, colon_idx + 1);
        cursor = colon_idx + 1;
    }

    while cursor < period_i {
        let entry_start = next_non_comment(tokens, cursor, period_i);
        push_token_range(b, &mut children, tokens, cursor, entry_start);
        if entry_start >= period_i {
            break;
        }
        let entry_end =
            find_top_level_token_kind_index(tokens, entry_start, period_i, TokenKind::Comma)
                .unwrap_or(period_i);
        children.extend(build_classic_text_entry_children(
            b,
            source,
            tokens,
            entry_start,
            entry_end,
            kind,
        ));
        cursor = entry_end;
        if cursor < period_i && tokens[cursor].kind == TokenKind::Comma {
            children.push(token_leaf(b, &tokens[cursor]));
            cursor += 1;
        }
    }

    push_token_range(b, &mut children, tokens, cursor, period_i + 1);
    children
}

fn token_matches_sequence_part(source: &str, token: &Token, part: &str) -> bool {
    match part {
        "-" => token.kind == TokenKind::Minus,
        "/" => token.kind == TokenKind::Slash,
        _ => token_matches_keyword(source, token, part),
    }
}

fn match_list_control_token_sequence(
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
    parts: &[&str],
) -> Option<usize> {
    let mut cursor = start;
    for part in parts {
        let part_idx = next_non_comment(tokens, cursor, end);
        if part_idx >= end || !token_matches_sequence_part(source, &tokens[part_idx], part) {
            return None;
        }
        cursor = part_idx + 1;
    }
    Some(cursor)
}

fn list_control_new_page_operand_clause_end(
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
) -> Option<usize> {
    let clauses: &[&[&str]] = &[
        &["line", "-", "size"],
        &["line", "-", "count"],
        &["destination"],
        &["copies"],
        &["list", "name"],
        &["with", "-", "title"],
        &["to", "page"],
    ];
    clauses
        .iter()
        .find_map(|parts| match_list_control_token_sequence(source, tokens, start, end, parts))
}

fn list_control_new_page_static_clause_end(
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
) -> Option<usize> {
    let clauses: &[&[&str]] = &[
        &["no", "-", "title"],
        &["no", "-", "heading"],
        &["with", "-", "heading"],
        &["print", "on"],
        &["print", "off"],
        &["print"],
    ];
    clauses
        .iter()
        .find_map(|parts| match_list_control_token_sequence(source, tokens, start, end, parts))
}

fn list_control_new_page_clause_start(
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
) -> bool {
    list_control_new_page_operand_clause_end(source, tokens, start, end).is_some()
        || list_control_new_page_static_clause_end(source, tokens, start, end).is_some()
}

fn scan_list_control_operand_end(
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
) -> usize {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            idx += 1;
            continue;
        }
        if paren == 0
            && bracket == 0
            && brace == 0
            && (token.kind == TokenKind::Comma
                || list_control_new_page_clause_start(source, tokens, idx, end))
        {
            break;
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
    idx
}

fn push_list_control_operand_child(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
) -> Option<NodeId> {
    let value_start = next_non_comment(tokens, start, end);
    let value_end = trim_trailing_comments(tokens, value_start, end);
    push_token_range(b, children, tokens, start, value_start);
    if value_start >= value_end {
        push_token_range(b, children, tokens, value_start, end);
        return None;
    }

    let operand = build_wrapped_expr_child(
        b,
        source,
        tokens,
        value_start,
        value_end,
        SyntaxKind::ListControlOperand,
    );
    children.push(operand);
    push_token_range(b, children, tokens, value_end, end);
    Some(operand)
}

fn build_skip_list_control_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = vec![token_leaf(b, &tokens[idx])];
    let body_start = idx + 1;

    if let Some(line_end) =
        match_list_control_token_sequence(source, tokens, body_start, period_i, &["to", "line"])
    {
        push_token_range(b, &mut children, tokens, body_start, line_end);
        push_list_control_operand_child(b, &mut children, source, tokens, line_end, period_i);
        children.push(token_leaf(b, &tokens[period_i]));
        return children;
    }

    let line_keyword_idx =
        find_top_level_any_keyword_index(source, tokens, body_start, period_i, &["line", "lines"]);
    let operand_end = line_keyword_idx.unwrap_or(period_i);
    push_list_control_operand_child(b, &mut children, source, tokens, body_start, operand_end);
    push_token_range(b, &mut children, tokens, operand_end, period_i + 1);
    children
}

fn build_reserve_list_control_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = vec![token_leaf(b, &tokens[idx])];
    let body_start = idx + 1;
    let line_keyword_idx =
        find_top_level_any_keyword_index(source, tokens, body_start, period_i, &["line", "lines"]);
    let operand_end = line_keyword_idx.unwrap_or(period_i);
    push_list_control_operand_child(b, &mut children, source, tokens, body_start, operand_end);
    push_token_range(b, &mut children, tokens, operand_end, period_i + 1);
    children
}

fn build_new_page_list_control_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    let lead_end =
        match_list_control_token_sequence(source, tokens, idx, period_i, &["new", "-", "page"])
            .unwrap_or(idx + 1);
    push_token_range(b, &mut children, tokens, idx, lead_end);

    let mut cursor = lead_end;
    while cursor < period_i {
        if tokens[cursor].kind == TokenKind::Comment {
            children.push(token_leaf(b, &tokens[cursor]));
            cursor += 1;
            continue;
        }

        if let Some(clause_end) =
            list_control_new_page_operand_clause_end(source, tokens, cursor, period_i)
        {
            push_token_range(b, &mut children, tokens, cursor, clause_end);
            let operand_end = scan_list_control_operand_end(source, tokens, clause_end, period_i);
            push_list_control_operand_child(
                b,
                &mut children,
                source,
                tokens,
                clause_end,
                operand_end,
            );
            cursor = operand_end;
            continue;
        }

        if let Some(clause_end) =
            list_control_new_page_static_clause_end(source, tokens, cursor, period_i)
        {
            push_token_range(b, &mut children, tokens, cursor, clause_end);
            cursor = clause_end;
            continue;
        }

        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
    }

    children.push(token_leaf(b, &tokens[period_i]));
    children
}

fn find_matching_rparen_token(tokens: &[Token], lparen_idx: usize, end: usize) -> Option<usize> {
    let mut depth = 0i32;
    for idx in lparen_idx..end {
        match tokens[idx].kind {
            TokenKind::LParen => depth += 1,
            TokenKind::RParen => {
                depth -= 1;
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
    }
    None
}

fn consume_uline_operand(tokens: &[Token], start: usize, end: usize) -> usize {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            idx += 1;
            continue;
        }
        if paren == 0
            && bracket == 0
            && brace == 0
            && matches!(
                token.kind,
                TokenKind::LParen | TokenKind::Slash | TokenKind::Comma
            )
        {
            break;
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
    idx
}

fn build_uline_list_control_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = vec![token_leaf(b, &tokens[idx])];
    let mut cursor = idx + 1;
    while cursor < period_i {
        let token = &tokens[cursor];
        if token.kind == TokenKind::Comment
            || token.kind == TokenKind::Slash
            || token.kind == TokenKind::Comma
            || token_matches_keyword(source, token, "at")
        {
            children.push(token_leaf(b, token));
            cursor += 1;
            continue;
        }

        if token.kind == TokenKind::LParen
            && let Some(close_idx) = find_matching_rparen_token(tokens, cursor, period_i)
        {
            children.push(token_leaf(b, token));
            push_list_control_operand_child(
                b,
                &mut children,
                source,
                tokens,
                cursor + 1,
                close_idx,
            );
            children.push(token_leaf(b, &tokens[close_idx]));
            cursor = close_idx + 1;
            continue;
        }

        let operand_end = consume_uline_operand(tokens, cursor, period_i);
        if operand_end > cursor {
            push_list_control_operand_child(b, &mut children, source, tokens, cursor, operand_end);
            cursor = operand_end;
        } else {
            children.push(token_leaf(b, token));
            cursor += 1;
        }
    }
    children.push(token_leaf(b, &tokens[period_i]));
    children
}

fn build_list_control_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
    kind: SyntaxKind,
) -> Vec<NodeId> {
    match kind {
        SyntaxKind::SkipStmt => {
            build_skip_list_control_stmt_children(b, source, tokens, idx, period_i)
        }
        SyntaxKind::ReserveStmt => {
            build_reserve_list_control_stmt_children(b, source, tokens, idx, period_i)
        }
        SyntaxKind::UlineStmt => {
            build_uline_list_control_stmt_children(b, source, tokens, idx, period_i)
        }
        SyntaxKind::NewPageStmt => {
            build_new_page_list_control_stmt_children(b, source, tokens, idx, period_i)
        }
        _ => tokens[idx..=period_i]
            .iter()
            .map(|token| token_leaf(b, token))
            .collect(),
    }
}

fn push_maybe_inline_target_child(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
) {
    let target_start = next_non_comment(tokens, start, end);
    let target_end = trim_trailing_comments(tokens, target_start, end);
    push_token_range(b, children, tokens, start, target_start);
    if target_start >= target_end {
        push_token_range(b, children, tokens, target_start, end);
        return;
    }

    if let Some(inline_decl) =
        build_data_inline_decl_local(b, source, tokens, target_start, target_end)
    {
        children.push(inline_decl);
        push_token_range(b, children, tokens, target_end, end);
    } else {
        push_token_range(b, children, tokens, target_start, end);
    }
}

fn build_get_time_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let Some(field_idx) = find_top_level_keyword_index(source, tokens, idx + 2, period_i, "field")
    else {
        return tokens[idx..=period_i]
            .iter()
            .map(|token| token_leaf(b, token))
            .collect();
    };

    let mut children = Vec::with_capacity(period_i - idx + 1);
    push_token_range(b, &mut children, tokens, idx, field_idx + 1);
    push_maybe_inline_target_child(b, &mut children, source, tokens, field_idx + 1, period_i);
    children.push(token_leaf(b, &tokens[period_i]));
    children
}

fn build_get_parameter_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let Some(field_idx) = find_top_level_keyword_index(source, tokens, idx + 2, period_i, "field")
    else {
        return tokens[idx..=period_i]
            .iter()
            .map(|token| token_leaf(b, token))
            .collect();
    };

    let mut children = Vec::with_capacity(period_i - idx + 1);
    push_token_range(b, &mut children, tokens, idx, field_idx + 1);
    push_maybe_inline_target_child(b, &mut children, source, tokens, field_idx + 1, period_i);
    children.push(token_leaf(b, &tokens[period_i]));
    children
}

fn runtime_env_target_end(source: &str, tokens: &[Token], start: usize, end: usize) -> usize {
    let start = next_non_comment(tokens, start, end);
    if start >= end {
        return end;
    }
    if (token_matches_keyword(source, &tokens[start], "data")
        || token_matches_keyword(source, &tokens[start], "final"))
        && tokens.get(start + 1).map(|token| token.kind) == Some(TokenKind::LParen)
        && let Some(close_idx) = find_matching_rparen_token(tokens, start + 1, end)
    {
        return close_idx + 1;
    }

    let mut idx = start + 1;
    while idx + 1 < end
        && matches!(
            tokens[idx].kind,
            TokenKind::Minus | TokenKind::Arrow | TokenKind::FatArrow | TokenKind::Tilde
        )
    {
        idx += 2;
    }
    idx
}

fn build_get_locale_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let clauses = ["language", "country", "modifier"];
    let mut children = Vec::with_capacity(period_i - idx + 1);
    let mut cursor = idx;
    while cursor < period_i {
        if clauses
            .iter()
            .any(|keyword| token_matches_keyword(source, &tokens[cursor], keyword))
        {
            children.push(token_leaf(b, &tokens[cursor]));
            let target_start = cursor + 1;
            let target_end = runtime_env_target_end(source, tokens, target_start, period_i);
            push_maybe_inline_target_child(
                b,
                &mut children,
                source,
                tokens,
                target_start,
                target_end,
            );
            cursor = target_end;
        } else {
            children.push(token_leaf(b, &tokens[cursor]));
            cursor += 1;
        }
    }
    children.push(token_leaf(b, &tokens[period_i]));
    children
}

fn build_get_pf_status_stmt_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    let status_start = idx + 4;
    push_token_range(b, &mut children, tokens, idx, status_start);
    let status_end = runtime_env_target_end(source, tokens, status_start, period_i);
    push_maybe_inline_target_child(b, &mut children, source, tokens, status_start, status_end);

    let mut cursor = status_end;
    while cursor < period_i {
        if token_matches_keyword(source, &tokens[cursor], "program")
            || token_matches_keyword(source, &tokens[cursor], "excluding")
        {
            children.push(token_leaf(b, &tokens[cursor]));
            let target_start = cursor + 1;
            let target_end = runtime_env_target_end(source, tokens, target_start, period_i);
            push_maybe_inline_target_child(
                b,
                &mut children,
                source,
                tokens,
                target_start,
                target_end,
            );
            cursor = target_end;
        } else {
            children.push(token_leaf(b, &tokens[cursor]));
            cursor += 1;
        }
    }
    children.push(token_leaf(b, &tokens[period_i]));
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

fn direct_call_statement(source: &str, significant: &[&Token]) -> bool {
    let Some(last) = significant.last() else {
        return false;
    };
    if last.kind != TokenKind::Period {
        return false;
    }
    if significant.first().is_some_and(|token| {
        token_matches_keyword(source, token, "submit")
            || token_matches_keyword(source, token, "perform")
    }) {
        return false;
    }

    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut first_top_level_lparen = None;
    for (idx, token) in significant.iter().enumerate() {
        match token.kind {
            TokenKind::LParen if paren == 0 && bracket == 0 && brace == 0 => {
                if significant_dynamic_selector_lparen(significant, idx) {
                    paren += 1;
                    continue;
                }
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

fn significant_dynamic_selector_lparen(significant: &[&Token], lparen_idx: usize) -> bool {
    if significant.get(lparen_idx).map(|token| token.kind) != Some(TokenKind::LParen) {
        return false;
    }
    let Some(prev) = lparen_idx
        .checked_sub(1)
        .and_then(|idx| significant.get(idx).copied())
    else {
        return false;
    };
    compact_dynamic_selector_lparen(prev, significant[lparen_idx])
}

fn direct_call_paren_pair(significant: &[&Token]) -> Option<(usize, usize)> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut first_top_level_lparen = None;
    for (idx, token) in significant.iter().enumerate() {
        match token.kind {
            TokenKind::LParen if paren == 0 && bracket == 0 && brace == 0 => {
                if significant_dynamic_selector_lparen(significant, idx) {
                    paren += 1;
                    continue;
                }
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
        (Some(first), Some(_)) => have_space_between(lparen, first),
        _ => have_space_between(lparen, rparen),
    }
}

fn classify_class_section_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    class_section_statement(source, significant).then_some(SyntaxKind::ClassSectionStmt)
}

fn classify_class_deferred_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let last = *significant.last()?;
    if last.kind != TokenKind::Period || !token_matches_keyword(source, significant[0], "class") {
        return None;
    }

    let mut idx = 1usize;
    let mut stmt_kind = None;
    let mut saw_entry = false;
    loop {
        while matches!(
            significant.get(idx).map(|token| token.kind),
            Some(TokenKind::Colon | TokenKind::Comma)
        ) {
            idx += 1;
        }
        if significant
            .get(idx)
            .is_some_and(|token| token.kind == TokenKind::Period)
        {
            break;
        }
        if significant.get(idx).map(|token| token.kind) != Some(TokenKind::Ident)
            || !significant
                .get(idx + 1)
                .is_some_and(|token| token_matches_keyword(source, token, "definition"))
        {
            return None;
        }
        idx += 2;

        let entry_kind = if significant
            .get(idx)
            .is_some_and(|token| token_matches_keyword(source, token, "deferred"))
        {
            idx += 1;
            if significant
                .get(idx)
                .is_some_and(|token| token_matches_keyword(source, token, "public"))
            {
                idx += 1;
            }
            SyntaxKind::ClassDeferredStmt
        } else if significant
            .get(idx)
            .is_some_and(|token| token_matches_keyword(source, token, "load"))
        {
            idx += 1;
            SyntaxKind::ClassLoadStmt
        } else {
            return None;
        };

        if let Some(existing) = stmt_kind {
            if existing != entry_kind {
                return None;
            }
        } else {
            stmt_kind = Some(entry_kind);
        }
        saw_entry = true;
    }

    saw_entry.then_some(stmt_kind?)
}

fn classify_interface_deferred_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let last = *significant.last()?;
    if last.kind != TokenKind::Period || !token_matches_keyword(source, significant[0], "interface")
    {
        return None;
    }

    let mut idx = 1usize;
    let mut saw_entry = false;
    loop {
        while matches!(
            significant.get(idx).map(|token| token.kind),
            Some(TokenKind::Colon | TokenKind::Comma)
        ) {
            idx += 1;
        }
        if significant
            .get(idx)
            .is_some_and(|token| token.kind == TokenKind::Period)
        {
            break;
        }
        if significant.get(idx).map(|token| token.kind) != Some(TokenKind::Ident)
            || !significant
                .get(idx + 1)
                .is_some_and(|token| token_matches_keyword(source, token, "deferred"))
        {
            return None;
        }
        idx += 2;
        saw_entry = true;
    }

    saw_entry.then_some(SyntaxKind::InterfaceDeferredStmt)
}

fn classify_methods_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    method_statement_name_idx(source, significant).map(|_| SyntaxKind::MethodsStmt)
}

fn classify_events_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    event_statement_name_idx(source, significant).map(|_| SyntaxKind::EventsStmt)
}

fn classify_interfaces_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let first = *significant.first()?;
    let last = *significant.last()?;
    if last.kind != TokenKind::Period {
        return None;
    }
    if token_matches_keyword(source, first, "interfaces")
        && significant
            .get(1)
            .is_some_and(|token| token.kind == TokenKind::Ident)
    {
        return Some(SyntaxKind::InterfacesStmt);
    }
    if token_matches_keyword(source, first, "interface") {
        let mut idx = 1usize;
        let mut saw_entry = false;
        loop {
            while matches!(
                significant.get(idx).map(|token| token.kind),
                Some(TokenKind::Colon | TokenKind::Comma)
            ) {
                idx += 1;
            }
            if significant
                .get(idx)
                .is_some_and(|token| token.kind == TokenKind::Period)
            {
                break;
            }
            if significant.get(idx).map(|token| token.kind) != Some(TokenKind::Ident)
                || !significant
                    .get(idx + 1)
                    .is_some_and(|token| token_matches_keyword(source, token, "load"))
            {
                return None;
            }
            idx += 2;
            saw_entry = true;
        }
        if saw_entry {
            return Some(SyntaxKind::InterfacesStmt);
        }
    }
    None
}

fn classify_type_pools_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    if significant.len() < 5 {
        return None;
    }
    let first = significant[0];
    let second = significant[1];
    let third = significant[2];
    let last = *significant.last()?;
    if !token_matches_keyword(source, first, "type")
        || second.kind != TokenKind::Minus
        || !token_matches_keyword(source, third, "pools")
        || last.kind != TokenKind::Period
    {
        return None;
    }

    let mut i = 3usize;
    let has_colon = significant
        .get(i)
        .is_some_and(|token| token.kind == TokenKind::Colon);
    if has_colon {
        i += 1;
    }
    let mut saw_name = false;
    loop {
        let token = significant.get(i)?;
        if token.kind == TokenKind::Period {
            return saw_name.then_some(SyntaxKind::TypePoolsStmt);
        }
        if token.kind != TokenKind::Ident {
            return None;
        }
        saw_name = true;
        i += 1;
        match significant.get(i).map(|token| token.kind) {
            Some(TokenKind::Comma) if has_colon => i += 1,
            Some(TokenKind::Period) => {}
            _ => return None,
        }
    }
}

fn classify_function_pool_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let last = *significant.last()?;
    if significant.len() < 5
        || last.kind != TokenKind::Period
        || !token_matches_keyword(source, significant[0], "function")
        || significant[1].kind != TokenKind::Minus
        || !token_matches_keyword(source, significant[2], "pool")
        || significant[3].kind != TokenKind::Ident
    {
        return None;
    }
    Some(SyntaxKind::FunctionPoolStmt)
}

fn classify_read_runtime_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let last = *significant.last()?;
    if last.kind != TokenKind::Period
        || significant.len() < 3
        || !token_matches_keyword(source, significant[0], "read")
    {
        return None;
    }

    if token_matches_keyword(source, significant[1], "dataset") {
        Some(SyntaxKind::ReadDatasetStmt)
    } else if token_matches_keyword(source, significant[1], "textpool") {
        Some(SyntaxKind::ReadTextpoolStmt)
    } else if token_matches_keyword(source, significant[1], "line") {
        Some(SyntaxKind::ReadLineStmt)
    } else {
        None
    }
}

fn classify_dataset_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let last = *significant.last()?;
    if last.kind != TokenKind::Period
        || significant.len() < 3
        || !token_matches_keyword(source, significant[1], "dataset")
    {
        return None;
    }

    if token_matches_keyword(source, significant[0], "open") {
        Some(SyntaxKind::OpenDatasetStmt)
    } else if token_matches_keyword(source, significant[0], "close") {
        Some(SyntaxKind::CloseDatasetStmt)
    } else if token_matches_keyword(source, significant[0], "delete") {
        Some(SyntaxKind::DeleteDatasetStmt)
    } else {
        None
    }
}

fn classify_generate_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let last = *significant.last()?;
    if last.kind != TokenKind::Period
        || significant.len() < 3
        || !token_matches_keyword(source, significant[0], "generate")
    {
        return None;
    }

    if token_matches_keyword(source, significant[1], "dynpro") {
        return Some(SyntaxKind::GenerateDynproStmt);
    }
    if significant.len() >= 4
        && token_matches_keyword(source, significant[1], "subroutine")
        && token_matches_keyword(source, significant[2], "pool")
    {
        return Some(SyntaxKind::GenerateSubroutinePoolStmt);
    }
    None
}

fn classify_get_runtime_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let last = *significant.last()?;
    if last.kind != TokenKind::Period
        || significant.len() < 3
        || !token_matches_keyword(source, significant[0], "get")
    {
        return None;
    }

    if significant.len() >= 5
        && token_matches_keyword(source, significant[1], "pf")
        && significant[2].kind == TokenKind::Minus
        && token_matches_keyword(source, significant[3], "status")
    {
        Some(SyntaxKind::GetPfStatusStmt)
    } else if token_matches_keyword(source, significant[1], "badi") {
        Some(SyntaxKind::GetBadiStmt)
    } else if token_matches_keyword(source, significant[1], "cursor") {
        Some(SyntaxKind::GetCursorStmt)
    } else if token_matches_keyword(source, significant[1], "locale") {
        Some(SyntaxKind::GetLocaleStmt)
    } else if token_matches_keyword(source, significant[1], "parameter") {
        Some(SyntaxKind::GetParameterStmt)
    } else if significant.len() >= 5
        && token_matches_keyword(source, significant[1], "run")
        && token_matches_keyword(source, significant[2], "time")
    {
        Some(SyntaxKind::GetRunTimeStmt)
    } else if token_matches_keyword(source, significant[1], "time")
        && !significant
            .get(2)
            .is_some_and(|token| token_matches_keyword(source, token, "stamp"))
    {
        Some(SyntaxKind::GetTimeStmt)
    } else {
        None
    }
}

fn classify_set_gui_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let last = *significant.last()?;
    if last.kind != TokenKind::Period {
        return None;
    }

    if significant.len() >= 6
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "run")
        && token_matches_keyword(source, significant[2], "time")
        && (token_matches_keyword(source, significant[3], "clock")
            || token_matches_keyword(source, significant[3], "analyzer"))
    {
        Some(SyntaxKind::SetRunTimeStmt)
    } else if significant.len() >= 5
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "update")
        && token_matches_keyword(source, significant[2], "task")
        && token_matches_keyword(source, significant[3], "local")
    {
        Some(SyntaxKind::SetUpdateTaskStmt)
    } else if significant.len() >= 5
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "user")
        && significant[2].kind == TokenKind::Minus
        && token_matches_keyword(source, significant[3], "command")
    {
        Some(SyntaxKind::SetUserCommandStmt)
    } else if significant.len() >= 4
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "locale")
        && token_matches_keyword(source, significant[2], "language")
    {
        Some(SyntaxKind::SetLocaleStmt)
    } else if significant.len() >= 3
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "country")
    {
        Some(SyntaxKind::SetCountryStmt)
    } else if significant.len() >= 3
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "language")
    {
        Some(SyntaxKind::SetLanguageStmt)
    } else if significant.len() >= 6
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "pf")
        && significant[2].kind == TokenKind::Minus
        && token_matches_keyword(source, significant[3], "status")
    {
        Some(SyntaxKind::SetPfStatusStmt)
    } else if significant.len() >= 4
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "titlebar")
    {
        Some(SyntaxKind::SetTitlebarStmt)
    } else if significant.len() >= 3
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "screen")
    {
        Some(SyntaxKind::SetScreenStmt)
    } else if significant.len() >= 3
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "cursor")
    {
        Some(SyntaxKind::SetCursorStmt)
    } else if significant.len() >= 3
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "handler")
    {
        Some(SyntaxKind::SetHandlerStmt)
    } else if significant.len() >= 3
        && token_matches_keyword(source, significant[0], "set")
        && token_matches_keyword(source, significant[1], "parameter")
    {
        Some(SyntaxKind::SetParameterStmt)
    } else {
        None
    }
}

fn classify_suppress_dialog_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let first = *significant.first()?;
    let second = *significant.get(1)?;
    let last = *significant.last()?;
    (token_matches_keyword(source, first, "suppress")
        && token_matches_keyword(source, second, "dialog")
        && last.kind == TokenKind::Period)
        .then_some(SyntaxKind::SuppressDialogStmt)
}

fn significant_starts_hyphenated_keyword(
    source: &str,
    significant: &[&Token],
    parts: &[&str],
) -> bool {
    let mut i = 0usize;
    for (part_idx, part) in parts.iter().enumerate() {
        let Some(token) = significant.get(i) else {
            return false;
        };
        if !token_matches_keyword(source, token, part) {
            return false;
        }
        i += 1;
        if part_idx + 1 < parts.len() {
            if significant.get(i).map(|token| token.kind) != Some(TokenKind::Minus) {
                return false;
            }
            i += 1;
        }
    }
    true
}

fn classify_field_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let first = *significant.first()?;
    let last = *significant.last()?;
    if last.kind != TokenKind::Period || !token_matches_keyword(source, first, "field") {
        return None;
    }

    if significant_starts_hyphenated_keyword(source, significant, &["field", "groups"]) {
        Some(SyntaxKind::FieldGroupsStmt)
    } else {
        Some(SyntaxKind::FieldStmt)
    }
}

fn classify_log_point_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let last = *significant.last()?;
    (last.kind == TokenKind::Period
        && significant_starts_hyphenated_keyword(source, significant, &["log", "point"]))
    .then_some(SyntaxKind::LogPointStmt)
}

fn classify_new_list_control_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let last = *significant.last()?;
    if last.kind != TokenKind::Period {
        return None;
    }

    if significant_starts_hyphenated_keyword(source, significant, &["new", "line"]) {
        Some(SyntaxKind::NewLineStmt)
    } else if significant_starts_hyphenated_keyword(source, significant, &["new", "page"]) {
        Some(SyntaxKind::NewPageStmt)
    } else {
        None
    }
}

fn classify_direct_call_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    direct_call_statement(source, significant).then(|| {
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
    if significant.len() != 3
        || significant[1].kind != TokenKind::Ident
        || significant[2].kind != TokenKind::Period
    {
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

fn classify_macro_call_stmt(source: &str, significant: &[&Token]) -> Option<SyntaxKind> {
    let first = *significant.first()?;
    let last = *significant.last()?;
    if first.kind != TokenKind::Ident
        || last.kind != TokenKind::Period
        || is_definite_stmt_lead_keyword(source, first)
        || direct_call_statement(source, significant)
    {
        return None;
    }
    Some(SyntaxKind::MacroCallStmt)
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

    if let Some(kind) = classify_macro_call_stmt(source, significant) {
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

fn validate_methods_stmt_chaining(
    source: &str,
    significant: &[&Token],
    errors: &mut Vec<crate::ParseError>,
) {
    let Some(name_idx) = method_statement_name_idx(source, significant) else {
        return;
    };
    if significant.get(name_idx).map(|token| token.kind) == Some(TokenKind::Colon) {
        return;
    }
    if significant.get(name_idx).map(|token| token.kind) != Some(TokenKind::Ident) {
        return;
    }

    let mut i = name_idx + 1;
    while i + 1 < significant.len()
        && significant[i].kind == TokenKind::Tilde
        && significant[i + 1].kind == TokenKind::Ident
    {
        i += 2;
    }
    while i < significant.len() {
        let token = significant[i];
        if token.kind == TokenKind::Period {
            return;
        }
        if token.kind == TokenKind::Comma {
            break;
        }
        if let Some(modifier_len) = method_header_modifier_len(source, significant, i) {
            i += modifier_len;
            continue;
        }
        if token_matches_keyword(source, token, "default")
            && significant.get(i + 1).is_some_and(|next| {
                token_matches_keyword(source, next, "ignore")
                    || token_matches_keyword(source, next, "fail")
            })
        {
            i += 2;
            continue;
        }
        if method_signature_section(source, token)
            || token_matches_keyword(source, token, "for")
            || token_matches_keyword(source, token, "amdp")
        {
            break;
        }
        errors.push(crate::ParseError {
            message: "syntax error: expected '.' after method declaration; use METHODS: for chained declarations"
                .to_string(),
            range: token.range.clone(),
        });
        return;
    }

    let mut depth = 0i32;
    for token in significant.iter().skip(i) {
        match token.kind {
            TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => depth += 1,
            TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => depth -= 1,
            TokenKind::Comma if depth == 0 => {
                errors.push(crate::ParseError {
                    message: "syntax error: expected '.' after method declaration; use METHODS: for chained declarations"
                            .to_string(),
                    range: token.range.clone(),
                });
                return;
            }
            TokenKind::Period if depth == 0 => return,
            _ => {}
        }
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
    if is_method_stmt {
        validate_methods_stmt_chaining(source, significant, errors);
    }
    if !is_method_stmt
        && direct_call_statement(source, significant)
        && !direct_call_padding_is_valid(significant)
    {
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

fn validate_stop_stmt(significant: &[&Token], errors: &mut Vec<crate::ParseError>) {
    if significant.len() <= 2 {
        return;
    }
    let start = significant
        .get(1)
        .map(|token| token.range.start)
        .unwrap_or(significant[0].range.end);
    let end = significant
        .last()
        .map(|token| token.range.end)
        .unwrap_or(significant[0].range.end);
    errors.push(crate::ParseError {
        message: "syntax error: STOP does not allow additions".to_string(),
        range: start..end,
    });
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

fn build_simple_stmt_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
    errors: &mut Vec<crate::ParseError>,
) -> NodeId {
    let first = &tokens[idx];
    let period_tok = &tokens[period_i];
    let significant = significant_stmt_tokens(tokens, idx, period_i);
    let kind = simple_stmt_kind(source, &significant);
    if kind != SyntaxKind::MacroCallStmt
        && let Some(delim_error) = delimiter_error(tokens, idx, period_i)
    {
        errors.push(delim_error);
        let kids = tokens[idx..=period_i]
            .iter()
            .map(|t| token_leaf(b, t))
            .collect::<Vec<_>>();
        return b.branch(
            SyntaxKind::Error,
            first.range.start..period_tok.range.end,
            &kids,
        );
    }
    if kind != SyntaxKind::MacroCallStmt {
        validate_unparsed_stmt(source, &significant, tokens, idx, period_i, errors);
    }
    if kind == SyntaxKind::StopStmt {
        validate_stop_stmt(&significant, errors);
    }
    if matches!(kind, SyntaxKind::AssertStmt | SyntaxKind::CheckStmt)
        && !has_non_comment_tokens(tokens, idx + 1, period_i)
    {
        let keyword = first.lexeme(source).to_ascii_uppercase();
        errors.push(crate::ParseError {
            message: format!("syntax error: expected condition after {keyword}"),
            range: first.range.start..period_tok.range.end,
        });
    }
    let kids = match kind {
        SyntaxKind::ClassDeferredStmt => {
            build_deferred_type_stmt_children(b, tokens, idx, period_i)
        }
        SyntaxKind::InterfaceDeferredStmt => {
            build_deferred_type_stmt_children(b, tokens, idx, period_i)
        }
        SyntaxKind::ClassLoadStmt => build_deferred_type_stmt_children(b, tokens, idx, period_i),
        SyntaxKind::ClassSectionStmt => {
            build_class_section_stmt_children(b, source, tokens, idx, period_i)
        }
        SyntaxKind::AliasesStmt => build_aliases_stmt_children(b, source, tokens, idx, period_i),
        SyntaxKind::MethodsStmt => build_stmt_children_with_type_refs(
            b,
            source,
            tokens,
            idx,
            period_i,
            methods_stmt_type_ref_ranges(source, tokens, idx, period_i),
        ),
        SyntaxKind::EventsStmt => build_stmt_children_with_type_refs(
            b,
            source,
            tokens,
            idx,
            period_i,
            events_stmt_type_ref_ranges(source, tokens, idx, period_i),
        ),
        SyntaxKind::InterfacesStmt => build_stmt_children_with_type_refs(
            b,
            source,
            tokens,
            idx,
            period_i,
            interfaces_stmt_type_ref_ranges(source, tokens, idx, period_i),
        ),
        SyntaxKind::ClearStmt => build_clear_stmt_children(b, source, tokens, idx, period_i),
        SyntaxKind::ConvertStmt => build_convert_stmt_children(b, source, tokens, idx, period_i),
        SyntaxKind::DescribeStmt => build_describe_stmt_children(b, source, tokens, idx, period_i),
        SyntaxKind::AssertStmt | SyntaxKind::CheckStmt => {
            build_assert_or_check_stmt_children(b, source, tokens, idx, period_i)
        }
        SyntaxKind::CallStmt => build_direct_call_stmt_children(b, source, tokens, idx, period_i),
        SyntaxKind::ReplaceStmt => build_replace_stmt_children(b, source, tokens, idx, period_i),
        SyntaxKind::WaitStmt => build_wait_stmt_children(b, source, tokens, idx, period_i),
        SyntaxKind::GetTimeStmt | SyntaxKind::GetRunTimeStmt => {
            build_get_time_stmt_children(b, source, tokens, idx, period_i)
        }
        SyntaxKind::GetParameterStmt => {
            build_get_parameter_stmt_children(b, source, tokens, idx, period_i)
        }
        SyntaxKind::GetLocaleStmt => {
            build_get_locale_stmt_children(b, source, tokens, idx, period_i)
        }
        SyntaxKind::GetPfStatusStmt => {
            build_get_pf_status_stmt_children(b, source, tokens, idx, period_i)
        }
        SyntaxKind::AddStmt
        | SyntaxKind::SubtractStmt
        | SyntaxKind::ComputeStmt
        | SyntaxKind::MultiplyStmt
        | SyntaxKind::DivideStmt => {
            build_arithmetic_stmt_children(b, source, tokens, idx, period_i, kind)
        }
        SyntaxKind::TranslateStmt
        | SyntaxKind::ShiftStmt
        | SyntaxKind::SearchStmt
        | SyntaxKind::OverlayStmt
        | SyntaxKind::PackStmt
        | SyntaxKind::UnpackStmt => {
            build_classic_text_stmt_children(b, source, tokens, idx, period_i, kind)
        }
        SyntaxKind::SkipStmt
        | SyntaxKind::UlineStmt
        | SyntaxKind::NewLineStmt
        | SyntaxKind::NewPageStmt
        | SyntaxKind::ReserveStmt
        | SyntaxKind::BackStmt => {
            build_list_control_stmt_children(b, source, tokens, idx, period_i, kind)
        }
        _ => tokens[idx..=period_i]
            .iter()
            .map(|t| token_leaf(b, t))
            .collect(),
    };
    b.branch(kind, first.range.start..period_tok.range.end, &kids)
}

fn perform_line_continuation(source: &str, tokens: &[Token], start: usize, idx: usize) -> bool {
    let Some(first) = tokens.get(start) else {
        return false;
    };
    if !token_matches_keyword(source, first, "perform") {
        return false;
    }
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token_matches_keyword(source, token, "tables")
        || (token_matches_keyword(source, token, "if")
            && tokens
                .get(idx + 1)
                .is_some_and(|next| token_matches_keyword(source, next, "found")))
}

fn chained_decl_entry_continuation(
    source: &str,
    tokens: &[Token],
    start: usize,
    idx: usize,
) -> bool {
    let Some(prev) = previous_non_comment_token(tokens, idx) else {
        return false;
    };
    if !matches!(tokens[prev].kind, TokenKind::Colon | TokenKind::Comma) {
        return false;
    }
    let significant = tokens[start..idx]
        .iter()
        .filter(|token| token.kind != TokenKind::Comment)
        .collect::<Vec<_>>();
    if !significant
        .iter()
        .any(|token| token.kind == TokenKind::Colon)
    {
        return false;
    }
    method_statement_name_idx(source, &significant).is_some()
        || significant
            .first()
            .is_some_and(|first| token_matches_keyword(source, first, "aliases"))
}

fn simple_stmt_line_continuation(source: &str, tokens: &[Token], start: usize, idx: usize) -> bool {
    perform_line_continuation(source, tokens, start, idx)
        || chained_decl_entry_continuation(source, tokens, start, idx)
}

fn simple_stmt_boundary_at(source: &str, tokens: &[Token], start: usize, idx: usize) -> bool {
    idx > start
        && tokens.get(idx).is_some_and(|token| {
            token_begins_line(token)
                && !simple_stmt_line_continuation(source, tokens, start, idx)
                && (is_definite_stmt_lead_keyword(source, token)
                    || line_start_assignment(tokens, idx))
        })
}

fn simple_stmt_period_index(source: &str, tokens: &[Token], start: usize) -> usize {
    let mut paren = 0usize;
    let mut bracket = 0usize;
    let mut brace = 0usize;
    let mut i = start + 1;
    while let Some(token) = tokens.get(i) {
        if token.kind == TokenKind::Eof {
            return i;
        }
        let top = paren == 0 && bracket == 0 && brace == 0;
        if top {
            if token.kind == TokenKind::Period || simple_stmt_boundary_at(source, tokens, start, i)
            {
                return i;
            }
        }
        match token.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren = paren.saturating_sub(1),
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket = bracket.saturating_sub(1),
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace = brace.saturating_sub(1),
            _ => {}
        }
        i += 1;
    }
    tokens.len()
}

fn direct_call_statement_without_period(source: &str, significant: &[&Token]) -> bool {
    let Some(first) = significant.first() else {
        return false;
    };
    if token_matches_keyword(source, first, "submit")
        || token_matches_keyword(source, first, "perform")
    {
        return false;
    }

    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut first_top_level_lparen = None;
    for (idx, token) in significant.iter().enumerate() {
        match token.kind {
            TokenKind::LParen if paren == 0 && bracket == 0 && brace == 0 => {
                if significant_dynamic_selector_lparen(significant, idx) {
                    paren += 1;
                    continue;
                }
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

fn try_parse_direct_call_stmt_result_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
    start: usize,
    saved_previous: Option<usize>,
) -> Option<PResult<NodeId>> {
    let Some(expr) = cursor.parse_arithmetic_expr() else {
        cursor.set_position(start, saved_previous);
        return None;
    };
    cursor.skip_trivia();

    let body_end = cursor.index();
    let significant = cursor.tokens()[start..body_end]
        .iter()
        .filter(|token| token.kind != TokenKind::Comment)
        .collect::<Vec<_>>();
    if !direct_call_statement_without_period(cursor.source(), &significant) {
        cursor.set_position(start, saved_previous);
        return None;
    }

    let period = match cursor.expect_token_after_result(TokenKind::Period, "method call") {
        Ok(period) => period,
        Err(failure) => return Some(Err(failure)),
    };
    let children = vec![expr, period];
    let kind = if direct_call_padding_is_valid(&significant) {
        SyntaxKind::CallStmt
    } else {
        let range = cursor.children_range(&children, cursor.span(expr));
        cursor.push_error(
            "syntax error: method call arguments must have whitespace or a line break immediately inside parentheses"
                .to_string(),
            range,
        );
        SyntaxKind::Error
    };
    let range = cursor.children_range(&children, cursor.span(expr));
    Some(Ok(cursor.builder().branch(kind, range, &children)))
}

pub(crate) fn try_parse_simple_stmt_result_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<PResult<NodeId>> {
    cursor.skip_trivia();
    let start = cursor.index();
    let saved_previous = cursor.previous_index();
    if cursor.current().map(|token| token.kind) != Some(TokenKind::Ident) {
        return None;
    }

    if let Some(result) =
        try_parse_direct_call_stmt_result_from_cursor(cursor, start, saved_previous)
    {
        return Some(result);
    }

    let period_i = simple_stmt_period_index(cursor.source(), cursor.tokens(), start);
    cursor.set_position(period_i, period_i.checked_sub(1));
    if let Err(mut failure) = cursor.expect_token_after_result(TokenKind::Period, "statement") {
        failure.message = "syntax error: expected '.' to end statement".to_string();
        return Some(Err(failure));
    }

    let node = {
        let (b, source, tokens, errors) = cursor.parts_mut();
        build_simple_stmt_node(b, source, tokens, start, period_i, errors)
    };
    Some(Ok(node))
}

#[cfg(test)]
mod tests {
    use abap_ast::SyntaxKind;
    use abap_ast::ast::{
        AstNode, ClassSectionStmt, ClassSectionVisibilityKind, EventsStmt, MethodsStmt,
        SyntaxNodeRef,
    };

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
    fn chained_methods_allow_keyword_names_after_colon() {
        let src = "\
CLASS lcl DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS:\n\
      read,\n\
      delete.\n\
ENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let methods = MethodsStmt::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::MethodsStmt)
                .expect("methods stmt"),
        ))
        .expect("methods stmt");
        let names = methods
            .entries(src)
            .iter()
            .filter_map(|entry| entry.name_token(src))
            .filter_map(|name| name.text(src))
            .map(str::to_ascii_lowercase)
            .collect::<Vec<_>>();
        assert_eq!(names, vec!["read".to_string(), "delete".to_string()]);
    }

    #[test]
    fn reports_plain_methods_statement_with_multiple_method_entries() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    METHODS run IMPORTING iv_x TYPE i, reset.\nENDCLASS.",
        );
        assert!(
            parsed.errors.iter().any(|err| err
                .message
                .contains("expected '.' after method declaration")),
            "{:?}",
            parsed.errors
        );
    }

    #[test]
    fn reports_plain_methods_statement_with_second_name_before_period() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    METHODS run reset.\nENDCLASS.",
        );
        assert!(
            parsed.errors.iter().any(|err| err
                .message
                .contains("expected '.' after method declaration")),
            "{:?}",
            parsed.errors
        );
    }

    #[test]
    fn reports_plain_class_methods_statement_with_multiple_method_entries() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    CLASS-METHODS run, reset.\nENDCLASS.",
        );
        assert!(
            parsed.errors.iter().any(|err| err
                .message
                .contains("expected '.' after method declaration")),
            "{:?}",
            parsed.errors
        );
    }

    #[test]
    fn classifies_submit_statement_specifically() {
        let parsed = crate::parse("SUBMIT rsnast00 AND RETURN.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::SubmitStmt),
            1
        );
    }

    #[test]
    fn classifies_stop_statement_specifically() {
        let parsed = crate::parse("STOP.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::StopStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn classifies_simple_flow_statements_specifically() {
        let parsed = crate::parse("EXIT. CONTINUE. RETURN.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ExitStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ContinueStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ReturnStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn rejects_stop_statement_additions() {
        let parsed = crate::parse("STOP lv_flag.");
        assert!(
            parsed
                .errors
                .iter()
                .any(|err| err.message.contains("STOP does not allow additions")),
            "{:?}",
            parsed.errors
        );
    }

    #[test]
    fn class_section_exposes_visibility_node() {
        let src = "CLASS lcl DEFINITION. PUBLIC SECTION. ENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let section = ClassSectionStmt::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::ClassSectionStmt)
                .expect("section"),
        ))
        .expect("section");
        assert_eq!(
            section
                .visibility()
                .and_then(|visibility| visibility.kind(src)),
            Some(ClassSectionVisibilityKind::Public)
        );
    }

    #[test]
    fn classifies_interfaces_statement_specifically() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    INTERFACES if_demo.\nENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::InterfacesStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn classifies_events_statement_specifically() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    EVENTS changed EXPORTING VALUE(value) TYPE string.\nENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::EventsStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn events_stmt_exposes_signature_parameters() {
        let src = "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    CLASS-EVENTS changed EXPORTING VALUE(value) TYPE string OPTIONAL.\nENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let events = EventsStmt::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::EventsStmt)
                .expect("events stmt"),
        ))
        .expect("events stmt");
        let entries = events.entries(src);
        assert_eq!(entries.len(), 1);
        let params = entries[0].signature(src).parameters().to_vec();
        assert_eq!(params.len(), 1);
        assert_eq!(
            params[0]
                .name_token()
                .text(src)
                .map(|text| text.to_ascii_lowercase())
                .as_deref(),
            Some("value")
        );
        assert_eq!(params[0].type_display_text(src), Some("string"));
        assert!(params[0].is_optional());
    }

    #[test]
    fn methods_event_handler_builds_source_type_ref() {
        let src = "CLASS lcl_handler DEFINITION.\n  PUBLIC SECTION.\n    METHODS on_changed FOR EVENT changed OF lcl_sender IMPORTING value sender.\nENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let methods = MethodsStmt::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::MethodsStmt)
                .expect("methods stmt"),
        ))
        .expect("methods stmt");
        let entry = methods
            .entries(src)
            .into_iter()
            .next()
            .expect("method entry");
        let handler = entry.event_handler(src).expect("event handler");
        assert_eq!(
            handler.source_type_ref().display_text(src),
            Some("lcl_sender")
        );
        assert_eq!(
            handler
                .importing_names()
                .iter()
                .filter_map(|name| name.text(src))
                .map(|name| name.to_ascii_lowercase())
                .collect::<Vec<_>>(),
            vec!["value".to_string(), "sender".to_string()]
        );
    }

    #[test]
    fn interfaces_stmt_builds_type_ref_children() {
        let parsed = crate::parse("INTERFACE if_outer.\n  INTERFACES zif_demo.\nENDINTERFACE.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let interfaces = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::InterfacesStmt)
            .expect("interfaces stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(interfaces, SyntaxKind::TypeRefSimple),
            1
        );
    }

    #[test]
    fn classifies_class_deferred_statement_specifically() {
        let parsed = crate::parse("CLASS lcl_demo DEFINITION DEFERRED PUBLIC.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::ClassDeferredStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ClassDecl), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataDeclName), 1);
    }

    #[test]
    fn classifies_chained_class_load_statement_specifically() {
        let parsed = crate::parse(
            "CLASS: cl_ex_me_cin_mm06efko DEFINITION LOAD,\n  cl_other DEFINITION LOAD.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ClassLoadStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ClassDecl), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataDeclName), 2);
    }

    #[test]
    fn classifies_interface_deferred_statement_specifically() {
        let parsed = crate::parse("INTERFACE if_da_stat_wl DEFERRED.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::InterfaceDeferredStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::InterfaceDecl), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataDeclName), 1);
    }

    #[test]
    fn classifies_chained_interface_deferred_statement_specifically() {
        let parsed = crate::parse("INTERFACE: lif_environment DEFERRED,\n  lif_other DEFERRED.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::InterfaceDeferredStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::InterfaceDecl), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataDeclName), 2);
    }

    #[test]
    fn interface_load_stmt_is_classified_as_interfaces_stmt() {
        let parsed = crate::parse("INTERFACE if_outer.\n  INTERFACE zif_demo LOAD.\nENDINTERFACE.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::InterfacesStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn interface_load_stmt_builds_type_ref_children_without_load_keyword() {
        let parsed = crate::parse("INTERFACE if_outer.\n  INTERFACE zif_demo LOAD.\nENDINTERFACE.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let interfaces = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::InterfacesStmt)
            .expect("interfaces stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(interfaces, SyntaxKind::TypeRefSimple),
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
    fn chained_methods_stmt_builds_parameter_type_refs_after_colon() {
        let parsed = crate::parse(
            "CLASS lcl_demo DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS : check_wp_availability EXPORTING ev_ok TYPE char1,\n\
      process_reload,\n\
      send_email IMPORTING iv_content  TYPE xstring\n\
                 EXPORTING ev_response TYPE string.\n\
ENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let methods = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::MethodsStmt)
            .expect("methods stmt");
        assert_eq!(
            parsed.file.count_kind(methods, SyntaxKind::TypeRefSimple),
            3
        );
    }

    #[test]
    fn chained_methods_legacy_entries_expose_signatures_after_exceptions() {
        let src = "\
CLASS cl_any_table_mm DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS:\n\
      free,\n\
      get_table\n\
        EXPORTING\n\
          ex_fieldcatalog TYPE lvc_t_fcat\n\
          ex_table TYPE REF TO data,\n\
      set_table\n\
        IMPORTING\n\
          im_fieldcatalog TYPE lvc_t_fcat OPTIONAL\n\
          im_table TYPE any TABLE,\n\
      insert\n\
        IMPORTING\n\
          im_wa TYPE any\n\
          im_component TYPE c OPTIONAL\n\
          im_key TYPE any OPTIONAL\n\
        EXCEPTIONS\n\
          duplicate_record,\n\
      read\n\
        IMPORTING\n\
          im_component TYPE c\n\
          im_key TYPE any\n\
        EXPORTING\n\
          ex_wa TYPE any\n\
        EXCEPTIONS\n\
          not_found,\n\
      refresh,\n\
      delete\n\
        IMPORTING\n\
          im_component TYPE c\n\
          im_key TYPE any\n\
        EXCEPTIONS\n\
          not_found.\n\
ENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let methods = MethodsStmt::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::MethodsStmt)
                .expect("methods stmt"),
        ))
        .expect("methods stmt");
        let entries = methods.entries(src);
        let names = entries
            .iter()
            .filter_map(|entry| entry.name_token(src))
            .filter_map(|name| name.text(src))
            .map(str::to_ascii_lowercase)
            .collect::<Vec<_>>();
        assert_eq!(
            names,
            vec![
                "free".to_string(),
                "get_table".to_string(),
                "set_table".to_string(),
                "insert".to_string(),
                "read".to_string(),
                "refresh".to_string(),
                "delete".to_string()
            ]
        );
        assert_eq!(entries[1].signature(src).parameters().len(), 2);
        assert_eq!(entries[2].signature(src).parameters().len(), 2);
        assert_eq!(entries[3].signature(src).parameters().len(), 3);
        assert_eq!(entries[4].signature(src).parameters().len(), 3);
        assert_eq!(entries[6].signature(src).parameters().len(), 2);
        assert!(
            entries[3]
                .signature_text(src)
                .contains("EXCEPTIONS duplicate_record")
        );
    }

    #[test]
    fn chained_methods_parse_modifiers_per_entry() {
        let src = "\
CLASS lcl DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS:\n\
      to_text ABSTRACT RETURNING VALUE(rv_text) TYPE string,\n\
      normalize FINAL IMPORTING iv_text TYPE string,\n\
      if_demo~run REDEFINITION.\n\
ENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let methods = MethodsStmt::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::MethodsStmt)
                .expect("methods stmt"),
        ))
        .expect("methods stmt");
        let entries = methods.entries(src);
        assert_eq!(entries.len(), 3);
        assert_eq!(entries[0].signature(src).parameters().len(), 1);
        assert_eq!(entries[1].signature(src).parameters().len(), 1);
        assert_eq!(
            entries[2].name_token(src).and_then(|token| token.text(src)),
            Some("run")
        );
        assert_eq!(
            entries[2]
                .qualifier_token(src)
                .and_then(|token| token.text(src)),
            Some("if_demo")
        );
        assert!(entries[2].signature(src).is_redefinition());
        assert!(entries[0].signature_text(src).contains("ABSTRACT"));
        assert!(entries[1].signature_text(src).contains("FINAL"));
        assert!(entries[2].signature_text(src).contains("REDEFINITION"));
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
    fn classifies_chained_perform_statement_specifically() {
        let parsed = crate::parse("PERFORM append_fldcat1 USING: a b c d, e f g h.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::PerformStmt), 1);
    }

    #[test]
    fn classifies_grouped_perform_statement_specifically() {
        let parsed =
            crate::parse("PERFORM: f_set_status USING lv_status, f_set_mode USING lv_mode.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::PerformStmt), 1);
    }

    #[test]
    fn parses_dynamic_perform_in_program_without_direct_call_spacing_error() {
        let parsed = crate::parse(
            "PERFORM (lc_formname) IN PROGRAM (lc_progname) USING lt_files_index_del[] lv_log_handle.",
        );
        let root = parsed.file.root();
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::PerformStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CallStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_perform_in_program_if_found_with_structured_dynamic_operands() {
        let parsed = crate::parse(
            "PERFORM (lw_parameter-callback-userexitf)\n  IN PROGRAM (lw_parameter-callback-userexitp)\n  IF FOUND USING lv_object lw_parameter-t_par.",
        );
        let root = parsed.file.root();
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::PerformStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CallStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_multiline_perform_signature_additions() {
        let parsed = crate::parse(
            "PERFORM get_item_services IN PROGRAM saplmepo\n\
                    TABLES   ex_acc_tab\n\
                    USING    po_item_number\n\
                             im_limit\n\
                    CHANGING ex_comsrv\n\
                             ch_return.",
        );
        let root = parsed.file.root();
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::PerformStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn classifies_commit_and_rollback_work_statements_specifically() {
        let parsed = crate::parse("COMMIT WORK. ROLLBACK WORK.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CommitWorkStmt), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::RollbackWorkStmt),
            1
        );
    }

    #[test]
    fn classifies_type_pools_statement_specifically() {
        let parsed = crate::parse("TYPE-POOLS abap.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TypePoolsStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn classifies_structured_simple_statement_families_specifically() {
        let parsed = crate::parse(
            "\
INTERFACE if_demo.
  METHODS meth.
ENDINTERFACE.
INTERFACE if_other.
  INTERFACES if_demo.
  ALIASES alias_meth FOR if_demo~meth.
ENDINTERFACE.
DATA lv_text TYPE string.
DATA lv_date TYPE d.
DATA lv_time TYPE t.
DATA lv_stamp TYPE timestamp.
DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
ALIASES alias_meth FOR if_demo~meth.
CLEAR: lv_text, lv_stamp.
CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_stamp.
DESCRIBE TABLE lt_text LINES DATA(lv_lines).
REPLACE 'a' IN lv_text WITH 'b'.
WAIT UP TO lv_stamp SECONDS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AliasesStmt), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ClearStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ConvertStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DescribeStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ReplaceStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WaitStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn classifies_classic_arithmetic_statements_specifically() {
        let parsed = crate::parse(
            "\
DATA lv_a TYPE i.
DATA lv_b TYPE i.
DATA lv_c TYPE i.
ADD lv_a TO lv_b.
SUBTRACT lv_a FROM lv_b.
COMPUTE lv_c = lv_a + lv_b.
MULTIPLY lv_a BY lv_b.
DIVIDE lv_a BY lv_b GIVING lv_c.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AddStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SubtractStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ComputeStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::MultiplyStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DivideStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ArithmeticEntry), 5);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn classifies_classic_text_statements_with_operands() {
        let parsed = crate::parse(
            "\
DATA lv_text TYPE string.
DATA lv_mask TYPE string.
DATA lv_only TYPE string.
DATA lv_source TYPE string.
DATA lv_target TYPE string.
DATA lv_count TYPE i.
DATA lv_start TYPE i.
DATA lv_end TYPE i.
DATA lv_pattern TYPE string.
TRANSLATE lv_text TO UPPER CASE.
TRANSLATE lv_text USING lv_mask.
SHIFT lv_text RIGHT BY lv_count PLACES.
SEARCH lv_text FOR lv_pattern STARTING AT lv_start ENDING AT lv_end.
CONDENSE lv_text NO-GAPS.
OVERLAY lv_text WITH lv_mask ONLY lv_only.
PACK lv_source TO lv_target.
UNPACK lv_source TO lv_target.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TranslateStmt), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ShiftStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SearchStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CondenseStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::OverlayStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::PackStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnpackStmt), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::TextTargetOperand),
            7
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::TextSourceOperand),
            10
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn classifies_classic_list_control_statements_with_operands() {
        let parsed = crate::parse(
            "\
DATA lv_skip TYPE i.
DATA lv_line TYPE i.
DATA lv_col TYPE i.
DATA lv_len TYPE i.
DATA lv_size TYPE i.
DATA lv_count TYPE i.
DATA lv_title TYPE string.
DATA lv_reserve TYPE i.
SKIP.
SKIP lv_skip.
SKIP TO LINE lv_line.
ULINE.
ULINE /(45).
ULINE AT lv_col(lv_len).
NEW-LINE.
NEW-LINE NO-SCROLLING.
NEW-PAGE NO-TITLE LINE-SIZE 79.
NEW-PAGE LINE-SIZE lv_size LINE-COUNT lv_count WITH-TITLE lv_title.
RESERVE lv_reserve LINES.
BACK.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SkipStmt), 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UlineStmt), 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::NewLineStmt), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::NewPageStmt), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ReserveStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::BackStmt), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::ListControlOperand),
            10
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn structured_simple_statements_build_high_signal_children() {
        let parsed = crate::parse(
            "\
INTERFACE if_demo.
  METHODS meth.
ENDINTERFACE.
ALIASES alias_meth FOR if_demo~meth.
DATA lv_text TYPE string.
DATA lv_date TYPE d.
DATA lv_time TYPE t.
DATA lv_stamp TYPE timestamp.
DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
CLEAR: lv_text, lv_stamp.
CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_stamp.
DESCRIBE TABLE lt_text LINES DATA(lv_lines).
REPLACE FIRST OCCURRENCE OF 'a' IN lv_text WITH 'b'.
WAIT UP TO lv_stamp SECONDS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();

        let aliases = parsed
            .file
            .find_first_kind(root, SyntaxKind::AliasesStmt)
            .expect("aliases stmt");
        assert_eq!(parsed.file.count_kind(aliases, SyntaxKind::AliasEntry), 1);
        assert_eq!(parsed.file.count_kind(aliases, SyntaxKind::AliasName), 1);
        assert_eq!(parsed.file.count_kind(aliases, SyntaxKind::AliasMember), 1);
        assert_eq!(
            parsed.file.count_kind(aliases, SyntaxKind::TypeRefSimple),
            1
        );

        let clear = parsed
            .file
            .find_first_kind(root, SyntaxKind::ClearStmt)
            .expect("clear stmt");
        assert_eq!(parsed.file.count_kind(clear, SyntaxKind::ClearOperand), 2);
        assert_eq!(parsed.file.count_kind(clear, SyntaxKind::ExprIdent), 2);

        let convert = parsed
            .file
            .find_first_kind(root, SyntaxKind::ConvertStmt)
            .expect("convert stmt");
        assert_eq!(
            parsed.file.count_kind(convert, SyntaxKind::ConvertOperand),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(convert, SyntaxKind::ConvertTargetOperand),
            1
        );
        assert_eq!(parsed.file.count_kind(convert, SyntaxKind::ExprIdent), 3);

        let describe = parsed
            .file
            .find_first_kind(root, SyntaxKind::DescribeStmt)
            .expect("describe stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(describe, SyntaxKind::DescribeTableOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(describe, SyntaxKind::DescribeLinesTarget),
            1
        );
        assert_eq!(parsed.file.count_kind(describe, SyntaxKind::ExprIdent), 1);
        assert_eq!(
            parsed.file.count_kind(describe, SyntaxKind::DataInlineDecl),
            1
        );

        let replace = parsed
            .file
            .find_first_kind(root, SyntaxKind::ReplaceStmt)
            .expect("replace stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(replace, SyntaxKind::ReplacePatternOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(replace, SyntaxKind::ReplaceTargetOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(replace, SyntaxKind::ReplaceWithOperand),
            1
        );
        assert_eq!(parsed.file.count_kind(replace, SyntaxKind::ExprIdent), 1);

        let wait = parsed
            .file
            .find_first_kind(root, SyntaxKind::WaitStmt)
            .expect("wait stmt");
        assert_eq!(parsed.file.count_kind(wait, SyntaxKind::WaitOperand), 1);
        assert_eq!(parsed.file.count_kind(wait, SyntaxKind::ExprIdent), 1);
    }

    #[test]
    fn replace_all_occurences_addition_builds_operands() {
        let parsed = crate::parse("REPLACE ALL OCCURENCES OF '*' IN p_plant-low WITH '%'.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let replace = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::ReplaceStmt)
            .expect("replace stmt");
        let pattern = parsed
            .file
            .find_first_kind(replace, SyntaxKind::ReplacePatternOperand)
            .expect("replace pattern");

        assert_eq!(
            parsed
                .file
                .count_kind(replace, SyntaxKind::ReplacePatternOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(replace, SyntaxKind::ReplaceTargetOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(replace, SyntaxKind::ReplaceWithOperand),
            1
        );
        assert_eq!(parsed.file.count_kind(pattern, SyntaxKind::ExprIdent), 0);
        assert_eq!(parsed.file.count_kind(replace, SyntaxKind::SelectorExpr), 1);
    }

    #[test]
    fn replace_in_table_keeps_table_keyword_out_of_target_operand() {
        let src = "REPLACE ALL OCCURRENCES OF '&V1&' IN TABLE rt_itf WITH <lv_msgv>.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let replace = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::ReplaceStmt)
            .expect("replace stmt");
        let target = parsed
            .file
            .find_first_kind(replace, SyntaxKind::ReplaceTargetOperand)
            .expect("replace target");
        let target_ident = parsed
            .file
            .find_first_kind(target, SyntaxKind::ExprIdent)
            .expect("target ident");

        assert_eq!(
            parsed
                .file
                .count_kind(replace, SyntaxKind::ReplaceTargetOperand),
            1
        );
        assert_eq!(&src[parsed.file.range(target)], "rt_itf");
        assert_eq!(&src[parsed.file.range(target_ident)], "rt_itf");
    }

    #[test]
    fn parses_convert_time_stamp_with_existing_targets() {
        let parsed = crate::parse(
            "\
DATA lv_stamp TYPE timestamp.
DATA lv_tzone TYPE tznzone.
DATA lv_date TYPE d.
DATA lv_time TYPE t.
DATA lv_dst TYPE c LENGTH 1.
CONVERT TIME STAMP lv_stamp TIME ZONE lv_tzone INTO DATE lv_date TIME lv_time DAYLIGHT SAVING TIME lv_dst.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::ConvertStmt)
            .expect("convert stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ConvertOperand), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::ConvertTimeZoneOperand),
            1
        );
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::ConvertDateTarget),
            1
        );
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::ConvertTimeTarget),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::ConvertDaylightSavingTarget),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 5);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_convert_time_stamp_with_inline_targets() {
        let parsed = crate::parse(
            "\
DATA lv_stamp TYPE timestamp.
CONVERT TIME STAMP lv_stamp TIME ZONE 'UTC' INTO DATE FINAL(lv_date) TIME DATA(lv_time) DAYLIGHT SAVING TIME FINAL(lv_dst).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::ConvertStmt)
            .expect("convert stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ConvertOperand), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::ConvertTimeZoneOperand),
            1
        );
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::ConvertDateTarget),
            1
        );
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::ConvertTimeTarget),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::ConvertDaylightSavingTarget),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::DataInlineDecl), 3);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn classifies_set_gui_statements_specifically() {
        let src = "\
SET PF-STATUS lv_status OF PROGRAM lv_prog EXCLUDING lt_excl.\n\
SET TITLEBAR lv_title OF PROGRAM lv_prog WITH lv_text1 lv_text2.";
        let parsed = crate::parse(src);
        let root = parsed.file.root();

        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SetPfStatusStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SetTitlebarStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn classifies_runtime_environment_statements_specifically() {
        let src = "\
GET RUN TIME FIELD DATA(runtime).\n\
SET RUN TIME CLOCK RESOLUTION HIGH.\n\
SET RUN TIME ANALYZER ON.\n\
SET RUN TIME ANALYZER OFF.\n\
GET LOCALE LANGUAGE DATA(lang) COUNTRY DATA(country) MODIFIER DATA(modifier).\n\
SET LOCALE LANGUAGE lang COUNTRY country MODIFIER modifier.\n\
SET COUNTRY country.\n\
SET LANGUAGE lang.\n\
SET USER-COMMAND ucomm.\n\
SET UPDATE TASK LOCAL.\n\
GET PF-STATUS DATA(status) PROGRAM prog EXCLUDING excl.";
        let parsed = crate::parse(src);
        let root = parsed.file.root();

        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::GetRunTimeStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SetRunTimeStmt), 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::GetLocaleStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SetLocaleStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SetCountryStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SetLanguageStmt), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SetUserCommandStmt),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SetUpdateTaskStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::GetPfStatusStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataInlineDecl), 5);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn chained_aliases_allow_keyword_alias_names() {
        let parsed = crate::parse(
            "\
ALIASES:
  set_null FOR zif_demo~set_null,
  delete FOR zif_demo~delete.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let aliases = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::AliasesStmt)
            .expect("aliases stmt");
        assert_eq!(parsed.file.count_kind(aliases, SyntaxKind::AliasEntry), 2);
    }

    #[test]
    fn describe_table_lines_without_period_is_reported_as_error() {
        let parsed = crate::parse(
            "DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.\nDESCRIBE TABLE lt_text LINES DATA(lv_lines)",
        );
        assert!(
            parsed
                .errors
                .iter()
                .any(|err| err.message.contains("expected '.'")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DescribeStmt),
            0
        );
    }

    #[test]
    fn chained_describe_table_builds_structured_operands_and_targets() {
        let parsed = crate::parse(
            "\
DATA lt_aux_bup_adr TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lt_aux_dm_evt_sdr TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lv_n_customers TYPE i.
DATA lv_n_total TYPE i.

DESCRIBE TABLE: lt_aux_bup_adr    LINES lv_n_customers,
                lt_aux_dm_evt_sdr LINES lv_n_total.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let describe = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::DescribeStmt)
            .expect("describe stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(describe, SyntaxKind::DescribeTableOperand),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(describe, SyntaxKind::DescribeLinesTarget),
            2
        );
        assert_eq!(parsed.file.count_kind(describe, SyntaxKind::ExprIdent), 4);
    }

    #[test]
    fn rejects_instance_method_calls_without_opening_padding() {
        let src = "lo_prog->add_statement(lo_assign ).";
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

    #[test]
    fn accepts_instance_method_calls_without_closing_padding() {
        let parsed = crate::parse("lo_prog->add_statement( lo_print).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CallStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::Error),
            0
        );
    }

    #[test]
    fn accepts_direct_call_with_compact_closing_paren_before_pragma() {
        let parsed =
            crate::parse("mo_writer->open_element( name = lv_name prefix = 'cbvmda') ##NO_TEXT.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallStmt)
            .expect("call stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
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
    fn parses_unqualified_method_call_with_inline_importing_targets() {
        let parsed = crate::parse(
            "check_sequencing_rs(\n  EXPORTING\n    iv_rule_type = lv_rule_type\n  IMPORTING\n    ev_sequencing_error = DATA(lv_seq_err)\n    ev_sequencing_error_msg = DATA(lv_seq_err_msg)\n    et_objids = DATA(lt_seq_objids)\n).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallStmt)
            .expect("call stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgSection), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 4);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::DataInlineDecl), 3);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_unqualified_method_call_with_inline_importing_targets_and_trailing_comments() {
        let parsed = crate::parse(
            "check_sequencing_rs(\n  EXPORTING\n    iv_rule_type            = lv_rule_type                 \" Type of Rule\n  IMPORTING\n    ev_sequencing_error     = DATA(lv_seq_err)             \" Sequence error\n    ev_sequencing_error_msg = DATA(lv_seq_err_msg)         \" Sequence error message\n    et_objids               = DATA(lt_seq_objids)          \" Object Identifiers\n).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallStmt)
            .expect("call stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgSection), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 4);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::DataInlineDecl), 3);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
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
