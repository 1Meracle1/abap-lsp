//! Declaration parsing for `DATA`-family statements plus adjacent typed declaration forms.

use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind, have_space_between};

use crate::block_helpers::{
    inline_name_spacing_is_valid, is_keyword, match_hyphenated_keyword, parse_inline_name,
};
use crate::parser::{PResult, ParseFailure, Parser as CursorParser};
use crate::stmt_period::{is_definite_stmt_lead_keyword, token_begins_line};
use crate::syntax::token_leaf;
use crate::type_ref::{parse_type_ref_from_cursor, parse_type_ref_tokens};

#[inline]
fn is_parameters_keyword(source: &str, token: &Token) -> bool {
    is_keyword(source, token, "parameters") || is_keyword(source, token, "parameter")
}

fn is_structured_decl_continuation_keyword(source: &str, token: &Token) -> bool {
    is_keyword(source, token, "types")
        || is_keyword(source, token, "data")
        || is_parameters_keyword(source, token)
        || is_keyword(source, token, "statics")
        || is_keyword(source, token, "constants")
        || is_keyword(source, token, "field-symbols")
}

const PARAMETERS_LENGTH_STOP_KEYWORDS: &[&str] = &[
    "TYPE",
    "LIKE",
    "AS",
    "DEFAULT",
    "HELP",
    "LOWER",
    "MATCHCODE",
    "MEMORY",
    "MODIF",
    "NO",
    "OBLIGATORY",
    "RADIOBUTTON",
    "USER",
    "VALUE",
    "VISIBLE",
];

const PARAMETERS_TYPE_STOP_KEYWORDS: &[&str] = &[
    "AS",
    "DEFAULT",
    "HELP",
    "LOWER",
    "MATCHCODE",
    "MEMORY",
    "MODIF",
    "NO",
    "OBLIGATORY",
    "RADIOBUTTON",
    "USER",
    "VALUE",
    "VISIBLE",
];

const SELECT_OPTIONS_FOR_STOP_KEYWORDS: &[&str] = &[
    "DEFAULT",
    "HELP",
    "LOWER",
    "MATCHCODE",
    "MEMORY",
    "MODIF",
    "NO",
    "OBLIGATORY",
    "VALUE",
    "VISIBLE",
];

const DATA_TYPE_REF_STOP_KEYWORDS: &[&str] = &["OCCURS", "VALUE", "ASSOCIATION"];

enum DeclParseResult {
    Parsed(NodeId),
    Malformed(ParseFailure),
    Unsupported,
}

enum ClauseResult {
    Parsed(NodeId),
    Malformed(String),
    Unsupported,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StructuredDeclFlavor {
    Struct,
    Enum,
    Mesh,
}

fn decl_failure(cursor: &CursorParser<'_, '_>, message: impl Into<String>) -> ParseFailure {
    ParseFailure {
        message: message.into(),
        range: cursor.current_range(),
    }
}

fn declaration_boundary_end(source: &str, tokens: &[Token], start: usize) -> usize {
    let mut paren = 0usize;
    let mut bracket = 0usize;
    let mut brace = 0usize;
    let mut i = start;
    while let Some(tok) = tokens.get(i) {
        if tok.kind == TokenKind::Eof {
            return i;
        }
        let top = paren == 0 && bracket == 0 && brace == 0;
        if top {
            if tok.kind == TokenKind::Period {
                return i + 1;
            }
            if i > start && tok.kind == TokenKind::Ident && token_begins_line(tok) {
                let next_kind = tokens.get(i + 1).map(|next| next.kind);
                if is_definite_stmt_lead_keyword(source, tok)
                    || matches!(next_kind, Some(TokenKind::Eq | TokenKind::QuestionEq))
                {
                    return i;
                }
            }
        }
        match tok.kind {
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

fn try_parse_structured_data_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let data_tok = tokens.get(idx)?;
    let mut i = idx + 1;
    let has_colon = match tokens.get(i).map(|t| t.kind) {
        Some(TokenKind::Colon) => {
            i += 1;
            true
        }
        _ => false,
    };

    let mut clause_nodes = Vec::new();
    loop {
        while tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comment) {
            i += 1;
        }
        let (clause, next_i) = {
            parse_common_part_clause(b, source, tokens, i).or_else(|| {
                parse_begin_of_decl_clause(
                    b,
                    source,
                    tokens,
                    i,
                    SyntaxKind::DataTypedClause,
                    true,
                    has_colon,
                )
            })
        }
        .or_else(|| {
            parse_decl_clause(
                b,
                source,
                tokens,
                i,
                SyntaxKind::DataTypedClause,
                true,
                true,
                true,
            )
        })?;
        clause_nodes.push(clause);
        i = next_i;

        while tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comment) {
            i += 1;
        }
        let next = tokens.get(i)?;
        match next.kind {
            TokenKind::Comma => {
                if !has_colon {
                    return None;
                }
                i += 1;
            }
            TokenKind::Period => {
                let end = next.range.end;
                i += 1;
                let mut child_ids: Vec<NodeId> = Vec::with_capacity(2 + clause_nodes.len());
                child_ids.push(token_leaf(b, data_tok));
                child_ids.extend(clause_nodes);
                child_ids.push(token_leaf(b, next));
                let node = b.branch(SyntaxKind::DataDecl, data_tok.range.start..end, &child_ids);
                return Some((node, i));
            }
            _ => return None,
        }
    }
}

fn parse_data_decl_name(
    b: &mut SyntaxTreeBuilder,
    _source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    if first.kind != TokenKind::Ident {
        return None;
    }
    let mut children = vec![token_leaf(b, first)];
    let mut i = idx + 1;
    while let Some(op) = tokens.get(i) {
        if op.kind != TokenKind::Minus {
            break;
        }
        let prev = tokens.get(i.wrapping_sub(1))?;
        if have_space_between(prev, op) {
            break;
        }
        let field = tokens.get(i + 1)?;
        if field.kind != TokenKind::Ident {
            return None;
        }
        children.push(token_leaf(b, op));
        children.push(token_leaf(b, field));
        i += 2;
    }
    let start = first.range.start;
    let end = b.span(*children.last().unwrap()).end;
    Some((b.branch(SyntaxKind::DataDeclName, start..end, &children), i))
}

fn parse_numeric_prefixed_decl_name(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    let second = tokens.get(idx + 1)?;
    if first.kind != TokenKind::Number || second.kind != TokenKind::Ident {
        return None;
    }
    if have_space_between(first, second) {
        return None;
    }
    let children = vec![token_leaf(b, first), token_leaf(b, second)];
    Some((
        b.branch(
            SyntaxKind::DataDeclName,
            first.range.start..second.range.end,
            &children,
        ),
        idx + 2,
    ))
}

fn parse_structured_field_decl_name(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    parse_data_decl_name(b, source, tokens, idx)
        .or_else(|| parse_numeric_prefixed_decl_name(b, tokens, idx))
}

fn collect_raw_decl_tail(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    mut idx: usize,
    children: &mut Vec<NodeId>,
) -> usize {
    let mut saw_association = false;
    while let Some(tok) = tokens.get(idx) {
        if matches!(
            tok.kind,
            TokenKind::Comma | TokenKind::Period | TokenKind::Eof
        ) {
            break;
        }
        if tok.kind == TokenKind::Ident && is_keyword(source, tok, "association") {
            saw_association = true;
        }
        if token_begins_line(tok) && !saw_association {
            let next_kind = tokens.get(idx + 1).map(|next| next.kind);
            if is_definite_stmt_lead_keyword(source, tok)
                || matches!(next_kind, Some(TokenKind::Eq | TokenKind::QuestionEq))
            {
                break;
            }
        }
        children.push(token_leaf(b, tok));
        idx += 1;
    }
    idx
}

fn parse_data_decl_name_from_cursor(cursor: &mut CursorParser<'_, '_>) -> Option<NodeId> {
    cursor.skip_trivia();
    let first = cursor.current()?;
    if first.kind != TokenKind::Ident {
        return None;
    }
    let start = first.range.start;
    let mut children = vec![cursor.bump()?];
    while cursor
        .current()
        .is_some_and(|token| token.kind == TokenKind::Minus)
    {
        let op = cursor.current()?;
        let prev = cursor.previous()?;
        if have_space_between(prev, op) {
            break;
        }
        if cursor
            .tokens()
            .get(cursor.index() + 1)
            .is_none_or(|token| token.kind != TokenKind::Ident)
        {
            return None;
        }
        children.push(cursor.bump()?);
        children.push(cursor.expect_token_after(TokenKind::Ident, "'-'"));
    }
    let end = cursor.span(*children.last().unwrap()).end;
    Some(
        cursor
            .builder()
            .branch(SyntaxKind::DataDeclName, start..end, &children),
    )
}

fn parse_numeric_prefixed_decl_name_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<NodeId> {
    cursor.skip_trivia();
    let first = cursor.current()?;
    let second = cursor.tokens().get(cursor.index() + 1)?;
    if first.kind != TokenKind::Number || second.kind != TokenKind::Ident {
        return None;
    }
    if have_space_between(first, second) {
        return None;
    }
    let start = first.range.start;
    let first = cursor.bump()?;
    let second = cursor.bump()?;
    let end = cursor.span(second).end;
    Some(
        cursor
            .builder()
            .branch(SyntaxKind::DataDeclName, start..end, &[first, second]),
    )
}

fn parse_structured_field_decl_name_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<NodeId> {
    parse_data_decl_name_from_cursor(cursor)
        .or_else(|| parse_numeric_prefixed_decl_name_from_cursor(cursor))
}

fn parse_tables_decl_name_from_cursor(cursor: &mut CursorParser<'_, '_>) -> Option<NodeId> {
    cursor.skip_trivia();
    if cursor
        .current()
        .is_none_or(|token| token.kind != TokenKind::Star)
    {
        return parse_data_decl_name_from_cursor(cursor);
    }
    let start = cursor.current()?.range.start;
    let star = cursor.bump()?;
    let name = cursor.expect_token_after(TokenKind::Ident, "'*'");
    let end = cursor.span(name).end;
    Some(
        cursor
            .builder()
            .branch(SyntaxKind::DataDeclName, start..end, &[star, name]),
    )
}

fn collect_raw_decl_tail_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
    children: &mut Vec<NodeId>,
) {
    let mut saw_association = false;
    loop {
        cursor.skip_trivia();
        let Some(tok) = cursor.current() else {
            break;
        };
        if matches!(
            tok.kind,
            TokenKind::Comma | TokenKind::Period | TokenKind::Eof
        ) {
            break;
        }
        if tok.kind == TokenKind::Ident && is_keyword(cursor.source(), tok, "association") {
            saw_association = true;
        }
        if token_begins_line(tok) && !saw_association {
            let next_kind = cursor
                .tokens()
                .get(cursor.index() + 1)
                .map(|next| next.kind);
            if is_definite_stmt_lead_keyword(cursor.source(), tok)
                || matches!(next_kind, Some(TokenKind::Eq | TokenKind::QuestionEq))
            {
                break;
            }
        }
        if let Some(child) = cursor.bump() {
            children.push(child);
        } else {
            break;
        }
    }
}

fn is_invalid_decl_name_keyword(source: &str, token: &Token) -> bool {
    is_keyword(source, token, "type")
        || is_keyword(source, token, "like")
        || is_keyword(source, token, "value")
        || is_keyword(source, token, "for")
}

fn parse_optional_length_spec(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    stop_keywords: &[&str],
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    if first.kind != TokenKind::Ident
        || (!first.lexeme(source).eq_ignore_ascii_case("length")
            && !first.lexeme(source).eq_ignore_ascii_case("decimals"))
    {
        return None;
    }
    let start = idx;
    let mut i = idx;
    while i < tokens.len() {
        let tok = &tokens[i];
        if i > start
            && tok.kind == TokenKind::Ident
            && stop_keywords
                .iter()
                .any(|kw| tok.lexeme(source).eq_ignore_ascii_case(kw))
        {
            break;
        }
        if i > start
            && matches!(
                tok.kind,
                TokenKind::Comma | TokenKind::Period | TokenKind::Eof
            )
        {
            break;
        }
        if i > start
            && tok.kind == TokenKind::Ident
            && (tok.lexeme(source).eq_ignore_ascii_case("length")
                || tok.lexeme(source).eq_ignore_ascii_case("decimals"))
        {
            break;
        }
        i += 1;
    }
    let mut children = Vec::with_capacity(i - start);
    for t in &tokens[start..i] {
        children.push(token_leaf(b, t));
    }
    let end = b.span(*children.last().unwrap()).end;
    Some((
        b.branch(SyntaxKind::LengthSpec, first.range.start..end, &children),
        i,
    ))
}

fn parse_value_clause_keywords(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    keywords: &[&str],
) -> Option<(NodeId, usize)> {
    parse_value_clause_keywords_until(b, source, tokens, idx, keywords, &[])
}

fn parse_value_clause_keywords_until(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    keywords: &[&str],
    stop_keywords: &[&str],
) -> Option<(NodeId, usize)> {
    let value_tok = tokens.get(idx)?;
    if !keywords
        .iter()
        .any(|keyword| is_keyword(source, value_tok, keyword))
    {
        return None;
    }
    let value_kw = token_leaf(b, value_tok);
    let (expr, next) = parse_type_ref_tokens(b, source, tokens, idx + 1, stop_keywords)?;
    let range = value_tok.range.start..b.span(expr).end;
    Some((
        b.branch(SyntaxKind::ValueClause, range, &[value_kw, expr]),
        next,
    ))
}

fn parse_value_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    parse_value_clause_keywords(b, source, tokens, idx, &["value"])
}

fn parse_optional_paren_length(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let lparen = tokens.get(idx)?;
    if lparen.kind != TokenKind::LParen {
        return None;
    }
    let mut next = idx + 1;
    let mut depth = 1i32;
    while next < tokens.len() {
        match tokens[next].kind {
            TokenKind::LParen => depth += 1,
            TokenKind::RParen => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            TokenKind::Eof => return None,
            _ => {}
        }
        next += 1;
    }
    let rparen = tokens.get(next)?;
    if rparen.kind != TokenKind::RParen {
        return None;
    }
    let mut expr_children = Vec::with_capacity(next.saturating_sub(idx + 1));
    for t in &tokens[idx + 1..next] {
        expr_children.push(token_leaf(b, t));
    }
    let expr = if expr_children.is_empty() {
        b.branch(SyntaxKind::Error, lparen.range.end..rparen.range.start, &[])
    } else {
        b.branch(
            SyntaxKind::LengthSpec,
            tokens[idx + 1].range.start..tokens[next - 1].range.end,
            &expr_children,
        )
    };
    let l = token_leaf(b, lparen);
    let r = token_leaf(b, rparen);
    Some((
        b.branch(
            SyntaxKind::LengthSpec,
            lparen.range.start..rparen.range.end,
            &[l, expr, r],
        ),
        next + 1,
    ))
}

fn parse_optional_paren_length_from_cursor(cursor: &mut CursorParser<'_, '_>) -> Option<NodeId> {
    let start = cursor.index();
    let parsed = {
        let (b, _, tokens, _) = cursor.parts_mut();
        parse_optional_paren_length(b, tokens, start)
    };
    let (node, next) = parsed?;
    cursor.set_position(next, next.checked_sub(1));
    Some(node)
}

fn parse_optional_length_spec_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
    stop_keywords: &[&str],
) -> Option<NodeId> {
    cursor.skip_trivia();
    let first = cursor.current()?;
    if first.kind != TokenKind::Ident
        || (!first.lexeme(cursor.source()).eq_ignore_ascii_case("length")
            && !first
                .lexeme(cursor.source())
                .eq_ignore_ascii_case("decimals"))
    {
        return None;
    }
    let start = first.range.start;
    let mut children = Vec::new();
    while let Some(tok) = cursor.current() {
        if !children.is_empty()
            && tok.kind == TokenKind::Ident
            && stop_keywords
                .iter()
                .any(|kw| tok.lexeme(cursor.source()).eq_ignore_ascii_case(kw))
        {
            break;
        }
        if !children.is_empty()
            && matches!(
                tok.kind,
                TokenKind::Comma | TokenKind::Period | TokenKind::Eof
            )
        {
            break;
        }
        if !children.is_empty()
            && tok.kind == TokenKind::Ident
            && (tok.lexeme(cursor.source()).eq_ignore_ascii_case("length")
                || tok.lexeme(cursor.source()).eq_ignore_ascii_case("decimals"))
        {
            break;
        }
        children.push(cursor.bump()?);
    }
    let end = cursor.span(*children.last().unwrap()).end;
    Some(
        cursor
            .builder()
            .branch(SyntaxKind::LengthSpec, start..end, &children),
    )
}

fn parse_value_clause_keywords_until_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
    keywords: &[&str],
    stop_keywords: &[&str],
) -> Option<NodeId> {
    cursor.skip_trivia();
    let value_tok = cursor.current()?;
    if !keywords
        .iter()
        .any(|keyword| is_keyword(cursor.source(), value_tok, keyword))
    {
        return None;
    }
    let start = value_tok.range.start;
    let value_kw = cursor.bump()?;
    let expr = parse_type_ref_from_cursor(cursor, stop_keywords)?;
    let range = start..cursor.span(expr).end;
    Some(
        cursor
            .builder()
            .branch(SyntaxKind::ValueClause, range, &[value_kw, expr]),
    )
}

fn parse_value_clause_from_cursor(cursor: &mut CursorParser<'_, '_>) -> Option<NodeId> {
    parse_value_clause_keywords_until_from_cursor(cursor, &["value"], &[])
}

fn parse_structured_decl_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
    node_kind: SyntaxKind,
    allow_like: bool,
    allow_value: bool,
) -> Option<NodeId> {
    let start = cursor.index();
    let parsed = {
        let (b, source, tokens, _) = cursor.parts_mut();
        parse_begin_of_decl_clause(b, source, tokens, start, node_kind, allow_like, allow_value)
    };
    let (node, next) = parsed?;
    cursor.set_position(next, next.checked_sub(1));
    Some(node)
}

fn parse_common_part_clause_from_cursor(cursor: &mut CursorParser<'_, '_>) -> Option<NodeId> {
    let start = cursor.index();
    let parsed = {
        let (b, source, tokens, _) = cursor.parts_mut();
        parse_common_part_clause(b, source, tokens, start)
    };
    let (node, next) = parsed?;
    cursor.set_position(next, next.checked_sub(1));
    Some(node)
}

fn parse_decl_clause_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
    node_kind: SyntaxKind,
    allow_like: bool,
    allow_value: bool,
    allow_untyped: bool,
    type_context: &str,
) -> ClauseResult {
    cursor.skip_trivia();
    let Some(first) = cursor.current() else {
        return ClauseResult::Unsupported;
    };
    if node_kind != SyntaxKind::StructuredFieldClause
        && is_invalid_decl_name_keyword(cursor.source(), first)
    {
        return ClauseResult::Unsupported;
    }
    let name = if node_kind == SyntaxKind::StructuredFieldClause {
        parse_structured_field_decl_name_from_cursor(cursor)
    } else {
        parse_data_decl_name_from_cursor(cursor)
    };
    let Some(name) = name else {
        return ClauseResult::Unsupported;
    };
    let mut children = vec![name];

    if let Some(legacy_len) = parse_optional_paren_length_from_cursor(cursor) {
        children.push(legacy_len);
    }

    while let Some(length) =
        parse_optional_length_spec_from_cursor(cursor, &["TYPE", "LIKE", "VALUE"])
    {
        children.push(length);
    }

    let mut has_type_or_like = false;
    if let Some(type_kw) = cursor.current()
        && (is_keyword(cursor.source(), type_kw, "type")
            || (allow_like && is_keyword(cursor.source(), type_kw, "like")))
    {
        let keyword = type_kw.lexeme(cursor.source()).to_ascii_uppercase();
        has_type_or_like = true;
        children.push(cursor.bump().expect("type keyword exists"));

        let Some(typed) = parse_type_ref_from_cursor(cursor, DATA_TYPE_REF_STOP_KEYWORDS) else {
            return ClauseResult::Malformed(format!(
                "syntax error: expected type name after {keyword} in {type_context}"
            ));
        };
        children.push(typed);

        while let Some(length) =
            parse_optional_length_spec_from_cursor(cursor, DATA_TYPE_REF_STOP_KEYWORDS)
        {
            children.push(length);
        }
    }

    if allow_value && let Some(value) = parse_value_clause_from_cursor(cursor) {
        children.push(value);
    }
    if !has_type_or_like && !allow_untyped {
        return ClauseResult::Unsupported;
    }
    if cursor
        .current()
        .is_some_and(|tok| tok.kind == TokenKind::Eq)
    {
        return ClauseResult::Unsupported;
    }
    collect_raw_decl_tail_from_cursor(cursor, &mut children);

    let range = cursor.children_range(&children, cursor.current_range());
    ClauseResult::Parsed(cursor.builder().branch(node_kind, range, &children))
}

fn parse_parameters_clause_from_cursor(cursor: &mut CursorParser<'_, '_>) -> ClauseResult {
    let Some(name) = parse_data_decl_name_from_cursor(cursor) else {
        return ClauseResult::Unsupported;
    };
    let mut children = vec![name];

    if let Some(legacy_len) = parse_optional_paren_length_from_cursor(cursor) {
        children.push(legacy_len);
    }

    while let Some(length) =
        parse_optional_length_spec_from_cursor(cursor, PARAMETERS_LENGTH_STOP_KEYWORDS)
    {
        children.push(length);
    }

    if let Some(type_kw) = cursor.current()
        && (is_keyword(cursor.source(), type_kw, "type")
            || is_keyword(cursor.source(), type_kw, "like"))
    {
        let keyword = type_kw.lexeme(cursor.source()).to_ascii_uppercase();
        children.push(cursor.bump().expect("parameter type keyword exists"));

        let Some(typed) = parse_type_ref_from_cursor(cursor, PARAMETERS_TYPE_STOP_KEYWORDS) else {
            return ClauseResult::Malformed(format!(
                "syntax error: expected type name after {keyword} in PARAMETERS declaration"
            ));
        };
        children.push(typed);

        while let Some(length) =
            parse_optional_length_spec_from_cursor(cursor, PARAMETERS_TYPE_STOP_KEYWORDS)
        {
            children.push(length);
        }
    }

    loop {
        cursor.skip_trivia();
        let Some(tok) = cursor.current() else {
            break;
        };
        match tok.kind {
            TokenKind::Comma | TokenKind::Period | TokenKind::Eof => break,
            _ => {
                if let Some(value) = parse_value_clause_keywords_until_from_cursor(
                    cursor,
                    &["default"],
                    PARAMETERS_TYPE_STOP_KEYWORDS,
                ) {
                    children.push(value);
                } else {
                    children.push(cursor.bump().expect("parameter tail token exists"));
                }
            }
        }
    }

    let range = cursor.children_range(&children, cursor.current_range());
    ClauseResult::Parsed(
        cursor
            .builder()
            .branch(SyntaxKind::DataTypedClause, range, &children),
    )
}

fn parse_select_options_clause_from_cursor(cursor: &mut CursorParser<'_, '_>) -> ClauseResult {
    let Some(name) = parse_data_decl_name_from_cursor(cursor) else {
        return ClauseResult::Unsupported;
    };
    let mut children = vec![name];

    cursor.skip_trivia();
    if !cursor.at_keyword("FOR") {
        return ClauseResult::Unsupported;
    }
    children.push(cursor.expect_keyword("FOR"));

    let Some(typed) = parse_type_ref_from_cursor(cursor, SELECT_OPTIONS_FOR_STOP_KEYWORDS) else {
        return ClauseResult::Malformed(
            "syntax error: expected type name after FOR in SELECT-OPTIONS declaration".to_string(),
        );
    };
    children.push(typed);

    loop {
        cursor.skip_trivia();
        let Some(tok) = cursor.current() else {
            break;
        };
        if matches!(
            tok.kind,
            TokenKind::Comma | TokenKind::Period | TokenKind::Eof
        ) {
            break;
        }
        children.push(cursor.bump().expect("select-options tail token exists"));
    }

    let range = cursor.children_range(&children, cursor.current_range());
    ClauseResult::Parsed(
        cursor
            .builder()
            .branch(SyntaxKind::DataTypedClause, range, &children),
    )
}

fn parse_tables_clause_from_cursor(cursor: &mut CursorParser<'_, '_>) -> ClauseResult {
    let Some(name) = parse_tables_decl_name_from_cursor(cursor) else {
        return ClauseResult::Unsupported;
    };
    let mut children = vec![name];
    collect_raw_decl_tail_from_cursor(cursor, &mut children);
    let range = cursor.children_range(&children, cursor.current_range());
    ClauseResult::Parsed(
        cursor
            .builder()
            .branch(SyntaxKind::DataTypedClause, range, &children),
    )
}

fn parse_controls_clause_from_cursor(cursor: &mut CursorParser<'_, '_>) -> ClauseResult {
    cursor.skip_trivia();
    if cursor
        .current()
        .is_some_and(|first| is_invalid_decl_name_keyword(cursor.source(), first))
    {
        return ClauseResult::Unsupported;
    }
    let Some(name) = parse_data_decl_name_from_cursor(cursor) else {
        return ClauseResult::Unsupported;
    };
    let mut children = vec![name];

    cursor.skip_trivia();
    if cursor.at_keyword("TYPE") {
        children.push(cursor.expect_keyword("TYPE"));
    }

    collect_raw_decl_tail_from_cursor(cursor, &mut children);
    let range = cursor.children_range(&children, cursor.current_range());
    ClauseResult::Parsed(
        cursor
            .builder()
            .branch(SyntaxKind::DataTypedClause, range, &children),
    )
}

fn parse_clause_list_decl_from_cursor<F>(
    cursor: &mut CursorParser<'_, '_>,
    start: usize,
    saved_previous: Option<usize>,
    keyword_children: Vec<NodeId>,
    kind: SyntaxKind,
    stmt_name: &str,
    mut parse_clause: F,
) -> DeclParseResult
where
    F: FnMut(&mut CursorParser<'_, '_>) -> ClauseResult,
{
    let mut children = keyword_children;
    let has_colon = cursor.allow_token(TokenKind::Colon).is_some();
    let mut after_comma = false;

    loop {
        cursor.skip_trivia();
        let Some(tok) = cursor.current() else {
            return DeclParseResult::Malformed(decl_failure(
                cursor,
                format!("syntax error: expected declaration name in {stmt_name}"),
            ));
        };
        if matches!(
            tok.kind,
            TokenKind::Comma | TokenKind::Period | TokenKind::Eof
        ) || (tok.kind == TokenKind::Ident && is_invalid_decl_name_keyword(cursor.source(), tok))
        {
            let message = if after_comma {
                format!("syntax error: expected declaration after ',' in {stmt_name}")
            } else {
                format!("syntax error: expected declaration name in {stmt_name}")
            };
            return DeclParseResult::Malformed(decl_failure(cursor, message));
        }

        match parse_clause(cursor) {
            ClauseResult::Parsed(clause) => children.push(clause),
            ClauseResult::Malformed(message) => {
                return DeclParseResult::Malformed(decl_failure(cursor, message));
            }
            ClauseResult::Unsupported => {
                cursor.set_position(start, saved_previous);
                return DeclParseResult::Unsupported;
            }
        }
        cursor.skip_trivia();
        if cursor
            .current()
            .is_some_and(|token| token.kind == TokenKind::Comma)
        {
            if has_colon {
                let _ = cursor.expect_token_after(TokenKind::Comma, stmt_name);
                after_comma = true;
                continue;
            }
            cursor.set_position(start, saved_previous);
            return DeclParseResult::Unsupported;
        }

        let period = match cursor.expect_token_result(TokenKind::Period) {
            Ok(period) => period,
            Err(failure) => return DeclParseResult::Malformed(failure),
        };
        children.push(period);
        return DeclParseResult::Parsed(cursor.branch_from_children(
            kind,
            &children,
            cursor.tokens()[start].range.clone(),
        ));
    }
}

fn consume_hyphenated_keyword_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
    parts: &[&str],
) -> Option<Vec<NodeId>> {
    if match_hyphenated_keyword(cursor.source(), cursor.tokens(), cursor.index(), parts).is_none() {
        return None;
    }
    let mut children = Vec::with_capacity(parts.len() * 2 - 1);
    for (part_idx, part) in parts.iter().enumerate() {
        if part_idx > 0 {
            children.push(cursor.expect_token_after(TokenKind::Minus, parts[part_idx - 1]));
        }
        children.push(cursor.expect_keyword(part));
    }
    Some(children)
}

fn cursor_is_parameters_keyword(cursor: &CursorParser<'_, '_>) -> bool {
    cursor
        .current()
        .is_some_and(|token| is_parameters_keyword(cursor.source(), token))
}

fn try_parse_keyword_decl_from_cursor<F>(
    cursor: &mut CursorParser<'_, '_>,
    keyword: &str,
    kind: SyntaxKind,
    stmt_name: &str,
    parse_clause: F,
) -> DeclParseResult
where
    F: FnMut(&mut CursorParser<'_, '_>) -> ClauseResult,
{
    cursor.skip_trivia();
    let start = cursor.index();
    let saved_previous = cursor.previous_index();
    if !cursor.at_keyword(keyword) {
        return DeclParseResult::Unsupported;
    }
    let keyword_children = vec![cursor.expect_keyword(keyword)];
    parse_clause_list_decl_from_cursor(
        cursor,
        start,
        saved_previous,
        keyword_children,
        kind,
        stmt_name,
        parse_clause,
    )
}

fn try_parse_hyphenated_decl_from_cursor<F>(
    cursor: &mut CursorParser<'_, '_>,
    parts: &[&str],
    kind: SyntaxKind,
    stmt_name: &str,
    parse_clause: F,
) -> DeclParseResult
where
    F: FnMut(&mut CursorParser<'_, '_>) -> ClauseResult,
{
    cursor.skip_trivia();
    let start = cursor.index();
    let saved_previous = cursor.previous_index();
    let Some(keyword_children) = consume_hyphenated_keyword_from_cursor(cursor, parts) else {
        return DeclParseResult::Unsupported;
    };
    parse_clause_list_decl_from_cursor(
        cursor,
        start,
        saved_previous,
        keyword_children,
        kind,
        stmt_name,
        parse_clause,
    )
}

fn try_parse_structured_data_decl_from_cursor(cursor: &mut CursorParser<'_, '_>) -> Option<NodeId> {
    let start = cursor.index();
    let parsed = {
        let (b, source, tokens, _) = cursor.parts_mut();
        try_parse_structured_data_decl(b, source, tokens, start)
    };
    let (node, next) = parsed?;
    cursor.set_position(next, next.checked_sub(1));
    Some(node)
}

fn try_parse_types_structured_block_decl_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<NodeId> {
    let start = cursor.index();
    let parsed = {
        let (b, source, tokens, _) = cursor.parts_mut();
        try_parse_types_structured_block_decl(b, source, tokens, start)
    };
    let (node, next) = parsed?;
    cursor.set_position(next, next.checked_sub(1));
    Some(node)
}

fn finish_decl_result(result: DeclParseResult) -> Option<PResult<NodeId>> {
    match result {
        DeclParseResult::Parsed(node) => Some(Ok(node)),
        DeclParseResult::Malformed(failure) => Some(Err(failure)),
        DeclParseResult::Unsupported => None,
    }
}

fn try_parse_data_inline_decl_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<PResult<NodeId>> {
    cursor.skip_trivia();
    let start = cursor.index();
    let saved_previous = cursor.previous_index();
    if !cursor.at_keyword("DATA")
        || cursor
            .tokens()
            .get(start + 1)
            .is_none_or(|token| token.kind != TokenKind::LParen)
    {
        return None;
    }

    let fallback = cursor.tokens()[start].range.clone();
    let mut children = Vec::new();
    children.push(match cursor.expect_keyword_result("DATA") {
        Ok(keyword) => keyword,
        Err(failure) => return Some(Err(failure)),
    });
    let lparen_idx = cursor.index();
    children.push(
        match cursor.expect_token_after_result(TokenKind::LParen, "DATA") {
            Ok(lparen) => lparen,
            Err(failure) => return Some(Err(failure)),
        },
    );
    let name_idx = cursor.index();
    let name = {
        let (b, _, tokens, _) = cursor.parts_mut();
        parse_inline_name(b, tokens, name_idx)
    };
    let Some((name, next)) = name else {
        cursor.set_position(start, saved_previous);
        return None;
    };
    cursor.set_position(next, next.checked_sub(1));
    children.push(name);
    let rparen_idx = cursor.index();
    children.push(
        match cursor.expect_token_after_result(TokenKind::RParen, "inline DATA name") {
            Ok(rparen) => rparen,
            Err(failure) => return Some(Err(failure)),
        },
    );

    if !inline_name_spacing_is_valid(cursor.tokens(), lparen_idx, name_idx, rparen_idx) {
        return Some(Err(decl_failure(
            cursor,
            "syntax error: inline DATA declaration must not contain whitespace inside parentheses"
                .to_string(),
        )));
    }

    if !cursor
        .current()
        .is_some_and(|token| token.kind == TokenKind::Eq)
    {
        cursor.set_position(start, saved_previous);
        return None;
    }
    children.push(
        match cursor.expect_token_after_result(TokenKind::Eq, "inline DATA declaration") {
            Ok(eq) => eq,
            Err(failure) => return Some(Err(failure)),
        },
    );

    let rhs = match cursor.expect_arithmetic_expr_result("inline DATA declaration") {
        Ok(rhs) => rhs,
        Err(mut failure) => {
            failure.message =
                "syntax error: expected expression after '=' in inline DATA declaration"
                    .to_string();
            return Some(Err(failure));
        }
    };
    children.push(rhs);

    let period =
        match cursor.expect_token_after_result(TokenKind::Period, "inline DATA declaration") {
            Ok(period) => period,
            Err(failure) => return Some(Err(failure)),
        };
    children.push(period);
    Some(Ok(cursor.branch_from_children(
        SyntaxKind::DataInlineDecl,
        &children,
        fallback,
    )))
}

fn try_parse_data_decl_from_cursor(cursor: &mut CursorParser<'_, '_>) -> Option<PResult<NodeId>> {
    cursor.skip_trivia();
    let start = cursor.index();
    let saved_previous = cursor.previous_index();
    if !cursor.at_keyword("DATA") {
        return None;
    }
    if cursor
        .tokens()
        .get(start + 1)
        .is_some_and(|token| token.kind == TokenKind::LParen)
    {
        return try_parse_data_inline_decl_from_cursor(cursor);
    }
    if let Some(node) = try_parse_structured_data_decl_from_cursor(cursor) {
        return Some(Ok(node));
    }
    cursor.set_position(start, saved_previous);

    let keyword_children = vec![cursor.expect_keyword("DATA")];
    finish_decl_result(parse_clause_list_decl_from_cursor(
        cursor,
        start,
        saved_previous,
        keyword_children,
        SyntaxKind::DataDecl,
        "DATA statement",
        |cursor| {
            parse_common_part_clause_from_cursor(cursor)
                .map(ClauseResult::Parsed)
                .or_else(|| {
                    parse_structured_decl_from_cursor(
                        cursor,
                        SyntaxKind::DataTypedClause,
                        true,
                        true,
                    )
                    .map(ClauseResult::Parsed)
                })
                .unwrap_or_else(|| {
                    parse_decl_clause_from_cursor(
                        cursor,
                        SyntaxKind::DataTypedClause,
                        true,
                        true,
                        true,
                        "DATA declaration",
                    )
                })
        },
    ))
}

fn try_parse_parameters_decl_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<PResult<NodeId>> {
    cursor.skip_trivia();
    let start = cursor.index();
    let saved_previous = cursor.previous_index();
    if !cursor_is_parameters_keyword(cursor) {
        return None;
    }
    let keyword_children = vec![cursor.bump().expect("PARAMETERS keyword exists")];
    finish_decl_result(parse_clause_list_decl_from_cursor(
        cursor,
        start,
        saved_previous,
        keyword_children,
        SyntaxKind::ParametersDecl,
        "PARAMETERS statement",
        parse_parameters_clause_from_cursor,
    ))
}

fn try_parse_tables_decl_from_cursor(cursor: &mut CursorParser<'_, '_>) -> Option<PResult<NodeId>> {
    finish_decl_result(try_parse_keyword_decl_from_cursor(
        cursor,
        "TABLES",
        SyntaxKind::TablesDecl,
        "TABLES statement",
        parse_tables_clause_from_cursor,
    ))
}

fn try_parse_select_options_decl_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<PResult<NodeId>> {
    finish_decl_result(try_parse_hyphenated_decl_from_cursor(
        cursor,
        &["SELECT", "OPTIONS"],
        SyntaxKind::SelectOptionsDecl,
        "SELECT-OPTIONS statement",
        parse_select_options_clause_from_cursor,
    ))
}

fn try_parse_ranges_decl_from_cursor(cursor: &mut CursorParser<'_, '_>) -> Option<PResult<NodeId>> {
    finish_decl_result(try_parse_keyword_decl_from_cursor(
        cursor,
        "RANGES",
        SyntaxKind::RangesDecl,
        "RANGES statement",
        parse_select_options_clause_from_cursor,
    ))
}

fn try_parse_controls_decl_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<PResult<NodeId>> {
    finish_decl_result(try_parse_keyword_decl_from_cursor(
        cursor,
        "CONTROLS",
        SyntaxKind::ControlsDecl,
        "CONTROLS statement",
        parse_controls_clause_from_cursor,
    ))
}

fn try_parse_class_data_decl_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<PResult<NodeId>> {
    finish_decl_result(try_parse_hyphenated_decl_from_cursor(
        cursor,
        &["CLASS", "DATA"],
        SyntaxKind::DataDecl,
        "CLASS-DATA statement",
        |cursor| {
            parse_decl_clause_from_cursor(
                cursor,
                SyntaxKind::DataTypedClause,
                true,
                true,
                false,
                "CLASS-DATA declaration",
            )
        },
    ))
}

fn try_parse_statics_decl_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<PResult<NodeId>> {
    finish_decl_result(try_parse_keyword_decl_from_cursor(
        cursor,
        "STATICS",
        SyntaxKind::StaticsDecl,
        "STATICS statement",
        |cursor| {
            parse_structured_decl_from_cursor(cursor, SyntaxKind::DataTypedClause, true, true)
                .map(ClauseResult::Parsed)
                .unwrap_or_else(|| {
                    parse_decl_clause_from_cursor(
                        cursor,
                        SyntaxKind::DataTypedClause,
                        true,
                        true,
                        true,
                        "STATICS declaration",
                    )
                })
        },
    ))
}

fn try_parse_types_decl_from_cursor(cursor: &mut CursorParser<'_, '_>) -> Option<PResult<NodeId>> {
    cursor.skip_trivia();
    let start = cursor.index();
    let saved_previous = cursor.previous_index();
    if !cursor.at_keyword("TYPES") {
        return None;
    }
    if let Some(node) = try_parse_types_structured_block_decl_from_cursor(cursor) {
        return Some(Ok(node));
    }
    cursor.set_position(start, saved_previous);
    finish_decl_result(try_parse_keyword_decl_from_cursor(
        cursor,
        "TYPES",
        SyntaxKind::TypesDecl,
        "TYPES statement",
        |cursor| {
            parse_structured_decl_from_cursor(cursor, SyntaxKind::TypesTypedClause, true, false)
                .map(ClauseResult::Parsed)
                .unwrap_or_else(|| {
                    parse_decl_clause_from_cursor(
                        cursor,
                        SyntaxKind::TypesTypedClause,
                        true,
                        false,
                        false,
                        "TYPES declaration",
                    )
                })
        },
    ))
}

fn try_parse_constants_chained_from_cursor(cursor: &mut CursorParser<'_, '_>) -> DeclParseResult {
    try_parse_keyword_decl_from_cursor(
        cursor,
        "CONSTANTS",
        SyntaxKind::ConstantsDecl,
        "CONSTANTS statement",
        |cursor| {
            parse_structured_decl_from_cursor(cursor, SyntaxKind::ConstantClause, true, true)
                .map(ClauseResult::Parsed)
                .unwrap_or_else(|| {
                    parse_decl_clause_from_cursor(
                        cursor,
                        SyntaxKind::ConstantClause,
                        true,
                        true,
                        true,
                        "CONSTANTS declaration",
                    )
                })
        },
    )
}

fn try_parse_constants_decl_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<PResult<NodeId>> {
    cursor.skip_trivia();
    let start = cursor.index();
    let saved_previous = cursor.previous_index();
    if !cursor.at_keyword("CONSTANTS") {
        return None;
    }

    match try_parse_constants_chained_from_cursor(cursor) {
        DeclParseResult::Parsed(node) => {
            let end = cursor.index();
            match validate_structured_decl_nesting(cursor.source(), cursor.tokens(), start, end) {
                Ok(_) => Some(Ok(node)),
                Err(message) => {
                    let parsed = {
                        let (b, _, tokens, errors) = cursor.parts_mut();
                        parse_malformed_constants_decl(b, tokens, start, errors, message, end)
                    };
                    let (node, next) = parsed?;
                    cursor.set_position(next, next.checked_sub(1));
                    Some(Ok(node))
                }
            }
        }
        DeclParseResult::Malformed(failure) => Some(Err(failure)),
        DeclParseResult::Unsupported => {
            cursor.set_position(start, saved_previous);
            let end = declaration_boundary_end(cursor.source(), cursor.tokens(), start);
            match validate_structured_decl_nesting(cursor.source(), cursor.tokens(), start, end) {
                Ok(false) => None,
                Ok(true) => None,
                Err(message) => {
                    let parsed = {
                        let (b, _, tokens, errors) = cursor.parts_mut();
                        parse_malformed_constants_decl(b, tokens, start, errors, message, end)
                    };
                    let (node, next) = parsed?;
                    cursor.set_position(next, next.checked_sub(1));
                    Some(Ok(node))
                }
            }
        }
    }
}

fn try_parse_field_symbols_decl_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<PResult<NodeId>> {
    finish_decl_result(try_parse_hyphenated_decl_from_cursor(
        cursor,
        &["FIELD", "SYMBOLS"],
        SyntaxKind::FieldSymbolsDecl,
        "FIELD-SYMBOLS statement",
        |cursor| {
            parse_decl_clause_from_cursor(
                cursor,
                SyntaxKind::FieldSymbolClause,
                true,
                false,
                true,
                "FIELD-SYMBOLS declaration",
            )
        },
    ))
}

pub(crate) fn parse_decl_result_from_cursor(
    cursor: &mut CursorParser<'_, '_>,
) -> Option<PResult<NodeId>> {
    try_parse_data_decl_from_cursor(cursor)
        .or_else(|| try_parse_tables_decl_from_cursor(cursor))
        .or_else(|| try_parse_ranges_decl_from_cursor(cursor))
        .or_else(|| try_parse_controls_decl_from_cursor(cursor))
        .or_else(|| try_parse_parameters_decl_from_cursor(cursor))
        .or_else(|| try_parse_select_options_decl_from_cursor(cursor))
        .or_else(|| try_parse_class_data_decl_from_cursor(cursor))
        .or_else(|| try_parse_statics_decl_from_cursor(cursor))
        .or_else(|| try_parse_types_decl_from_cursor(cursor))
        .or_else(|| try_parse_constants_decl_from_cursor(cursor))
        .or_else(|| try_parse_field_symbols_decl_from_cursor(cursor))
}

fn parse_decl_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    node_kind: SyntaxKind,
    allow_like: bool,
    allow_value: bool,
    allow_untyped: bool,
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    if node_kind != SyntaxKind::StructuredFieldClause && is_invalid_decl_name_keyword(source, first)
    {
        return None;
    }
    let (name, mut i) = if node_kind == SyntaxKind::StructuredFieldClause {
        parse_structured_field_decl_name(b, source, tokens, idx)?
    } else {
        parse_data_decl_name(b, source, tokens, idx)?
    };
    let mut children = vec![name];

    if let Some((legacy_len, j)) = parse_optional_paren_length(b, tokens, i) {
        children.push(legacy_len);
        i = j;
    }

    while let Some((length, j)) =
        parse_optional_length_spec(b, source, tokens, i, &["TYPE", "LIKE", "VALUE"])
    {
        children.push(length);
        i = j;
    }

    let mut has_type_or_like = false;
    if let Some(type_kw) = tokens.get(i)
        && (is_keyword(source, type_kw, "type")
            || (allow_like && is_keyword(source, type_kw, "like")))
    {
        has_type_or_like = true;
        children.push(token_leaf(b, type_kw));
        i += 1;

        let (typed, j) = parse_type_ref_tokens(b, source, tokens, i, DATA_TYPE_REF_STOP_KEYWORDS)?;
        children.push(typed);
        i = j;

        while let Some((length, j)) =
            parse_optional_length_spec(b, source, tokens, i, DATA_TYPE_REF_STOP_KEYWORDS)
        {
            children.push(length);
            i = j;
        }
    }

    if allow_value && let Some((value, j)) = parse_value_clause(b, source, tokens, i) {
        children.push(value);
        i = j;
    }
    if !has_type_or_like && !allow_untyped {
        return None;
    }
    if tokens.get(i).is_some_and(|tok| tok.kind == TokenKind::Eq) {
        return None;
    }
    i = collect_raw_decl_tail(b, source, tokens, i, &mut children);

    let range = b.span(*children.first().unwrap()).start..b.span(*children.last().unwrap()).end;
    Some((b.branch(node_kind, range, &children), i))
}

fn parse_begin_of_decl_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    node_kind: SyntaxKind,
    allow_like: bool,
    allow_value: bool,
) -> Option<(NodeId, usize)> {
    parse_structured_decl(b, source, tokens, idx, node_kind, allow_like, allow_value)
}

fn parse_common_part_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let boundary_tok = tokens.get(idx)?;
    if !is_keyword(source, boundary_tok, "begin") && !is_keyword(source, boundary_tok, "end") {
        return None;
    }
    if !tokens
        .get(idx + 1)
        .is_some_and(|tok| is_keyword(source, tok, "of"))
        || !tokens
            .get(idx + 2)
            .is_some_and(|tok| is_keyword(source, tok, "common"))
        || !tokens
            .get(idx + 3)
            .is_some_and(|tok| is_keyword(source, tok, "part"))
    {
        return None;
    }

    let mut children = Vec::new();
    let mut i = idx;
    while let Some(tok) = tokens.get(i) {
        if matches!(
            tok.kind,
            TokenKind::Comma | TokenKind::Period | TokenKind::Eof
        ) {
            break;
        }
        children.push(token_leaf(b, tok));
        i += 1;
    }
    let range = b.span(*children.first().unwrap()).start..b.span(*children.last().unwrap()).end;
    Some((b.branch(SyntaxKind::DataTypedClause, range, &children), i))
}

fn parse_structured_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    node_kind: SyntaxKind,
    allow_like: bool,
    allow_value: bool,
) -> Option<(NodeId, usize)> {
    let begin_tok = tokens.get(idx)?;
    let (mut children, mut i, flavor) = parse_structured_decl_header(b, source, tokens, idx)?;
    while let Some(tok) = tokens.get(i) {
        if matches!(
            tok.kind,
            TokenKind::Comma | TokenKind::Period | TokenKind::Eof
        ) {
            break;
        }
        children.push(token_leaf(b, tok));
        i += 1;
    }
    if tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comma) {
        children.push(token_leaf(b, tokens.get(i)?));
        i += 1;
    } else if tokens.get(i).map(|t| t.kind) == Some(TokenKind::Period) {
        i = consume_structured_decl_period_separator(b, source, tokens, i, &mut children)?;
    }

    while i < tokens.len() {
        while tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comment) {
            children.push(token_leaf(b, tokens.get(i)?));
            i += 1;
        }
        let tok = tokens.get(i)?;
        if tok.kind == TokenKind::Eof {
            return None;
        }
        if let Some((end_children, next_i)) =
            parse_structured_decl_end(b, source, tokens, i, flavor)
        {
            children.extend(end_children);
            let node = b.branch(
                node_kind,
                begin_tok.range.start..b.span(*children.last().unwrap()).end,
                &children,
            );
            return Some((node, next_i));
        }

        let component_allow_value = allow_value || flavor == StructuredDeclFlavor::Enum;
        let (component, next_i) = parse_structured_decl(
            b,
            source,
            tokens,
            i,
            SyntaxKind::StructuredDecl,
            allow_like,
            component_allow_value,
        )
        .or_else(|| parse_structured_include_clause(b, source, tokens, i))
        .or_else(|| {
            parse_structured_field_clause(b, source, tokens, i, allow_like, component_allow_value)
        })?;
        children.push(component);
        i = next_i;

        while tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comment) {
            children.push(token_leaf(b, tokens.get(i)?));
            i += 1;
        }
        if tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comma) {
            children.push(token_leaf(b, tokens.get(i)?));
            i += 1;
            continue;
        }
        if tokens.get(i).map(|t| t.kind) == Some(TokenKind::Period) {
            i = consume_structured_decl_period_separator(b, source, tokens, i, &mut children)?;
        }
    }
    None
}

fn parse_structured_decl_header(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(Vec<NodeId>, usize, StructuredDeclFlavor)> {
    let begin_tok = tokens.get(idx)?;
    if !is_keyword(source, begin_tok, "begin")
        || !tokens
            .get(idx + 1)
            .is_some_and(|tok| is_keyword(source, tok, "of"))
    {
        return None;
    }

    let mut children = vec![
        token_leaf(b, begin_tok),
        token_leaf(b, tokens.get(idx + 1)?),
    ];
    let mut i = idx + 2;
    let flavor = if tokens
        .get(i + 1)
        .is_some_and(|tok| tok.kind == TokenKind::Ident)
        && tokens
            .get(i)
            .is_some_and(|tok| is_keyword(source, tok, "enum"))
    {
        children.push(token_leaf(b, tokens.get(i)?));
        i += 1;
        StructuredDeclFlavor::Enum
    } else if tokens
        .get(i + 1)
        .is_some_and(|tok| tok.kind == TokenKind::Ident)
        && tokens
            .get(i)
            .is_some_and(|tok| is_keyword(source, tok, "mesh"))
    {
        children.push(token_leaf(b, tokens.get(i)?));
        i += 1;
        StructuredDeclFlavor::Mesh
    } else {
        StructuredDeclFlavor::Struct
    };

    if tokens.get(i)?.kind != TokenKind::Ident {
        return None;
    }
    children.push(token_leaf(b, tokens.get(i)?));
    i += 1;

    while let Some(tok) = tokens.get(i) {
        if matches!(
            tok.kind,
            TokenKind::Comma | TokenKind::Period | TokenKind::Eof
        ) {
            break;
        }
        if flavor == StructuredDeclFlavor::Enum
            && is_keyword(source, tok, "base")
            && tokens
                .get(i + 1)
                .is_some_and(|next| is_keyword(source, next, "type"))
        {
            children.push(token_leaf(b, tok));
            children.push(token_leaf(b, tokens.get(i + 1)?));
            let (type_ref, next_i) =
                parse_type_ref_tokens(b, source, tokens, i + 2, &["STRUCTURE"])?;
            children.push(type_ref);
            i = next_i;
            continue;
        }
        children.push(token_leaf(b, tok));
        i += 1;
    }

    Some((children, i, flavor))
}

fn parse_structured_decl_end(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    flavor: StructuredDeclFlavor,
) -> Option<(Vec<NodeId>, usize)> {
    let end_tok = tokens.get(idx)?;
    if !is_keyword(source, end_tok, "end")
        || !tokens
            .get(idx + 1)
            .is_some_and(|tok| is_keyword(source, tok, "of"))
    {
        return None;
    }

    let mut children = vec![token_leaf(b, end_tok), token_leaf(b, tokens.get(idx + 1)?)];
    let mut i = idx + 2;
    match flavor {
        StructuredDeclFlavor::Enum => {
            if !tokens
                .get(i)
                .is_some_and(|tok| is_keyword(source, tok, "enum"))
            {
                return None;
            }
            children.push(token_leaf(b, tokens.get(i)?));
            i += 1;
        }
        StructuredDeclFlavor::Mesh => {
            if !tokens
                .get(i)
                .is_some_and(|tok| is_keyword(source, tok, "mesh"))
            {
                return None;
            }
            children.push(token_leaf(b, tokens.get(i)?));
            i += 1;
        }
        StructuredDeclFlavor::Struct => {}
    }

    if tokens.get(i)?.kind != TokenKind::Ident {
        return None;
    }
    children.push(token_leaf(b, tokens.get(i)?));
    i += 1;

    while let Some(tok) = tokens.get(i) {
        if matches!(
            tok.kind,
            TokenKind::Comma | TokenKind::Period | TokenKind::Eof
        ) {
            break;
        }
        children.push(token_leaf(b, tok));
        i += 1;
    }

    Some((children, i))
}

fn consume_structured_decl_period_separator(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    children: &mut Vec<NodeId>,
) -> Option<usize> {
    if tokens.get(idx).map(|t| t.kind) != Some(TokenKind::Period) {
        return None;
    }

    children.push(token_leaf(b, tokens.get(idx)?));
    let mut i = idx + 1;
    while tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comment) {
        children.push(token_leaf(b, tokens.get(i)?));
        i += 1;
    }
    if tokens
        .get(i)
        .is_some_and(|token| is_structured_decl_continuation_keyword(source, token))
    {
        children.push(token_leaf(b, tokens.get(i)?));
        i += 1;
        if tokens.get(i).map(|t| t.kind) == Some(TokenKind::Colon) {
            children.push(token_leaf(b, tokens.get(i)?));
            i += 1;
        }
    }
    Some(i)
}

fn parse_structured_include_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let include_tok = tokens.get(idx)?;
    if !is_keyword(source, include_tok, "include") {
        return None;
    }
    let kind_tok = tokens.get(idx + 1)?;
    if !is_keyword(source, kind_tok, "type") && !is_keyword(source, kind_tok, "structure") {
        return None;
    }

    let mut children = vec![token_leaf(b, include_tok), token_leaf(b, kind_tok)];
    let (type_ref, mut i) = parse_type_ref_tokens(b, source, tokens, idx + 2, &["AS", "RENAMING"])?;
    children.push(type_ref);

    while let Some(tok) = tokens.get(i) {
        if matches!(
            tok.kind,
            TokenKind::Comma | TokenKind::Period | TokenKind::Eof
        ) {
            break;
        }
        children.push(token_leaf(b, tok));
        i += 1;
    }

    let range = b.span(*children.first().unwrap()).start..b.span(*children.last().unwrap()).end;
    Some((
        b.branch(SyntaxKind::StructuredIncludeClause, range, &children),
        i,
    ))
}

fn parse_structured_field_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    allow_like: bool,
    allow_value: bool,
) -> Option<(NodeId, usize)> {
    parse_decl_clause(
        b,
        source,
        tokens,
        idx,
        SyntaxKind::StructuredFieldClause,
        allow_like,
        allow_value,
        false,
    )
    .or_else(|| parse_untyped_structured_field_clause(b, source, tokens, idx, allow_value))
}

fn parse_untyped_structured_field_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    allow_value: bool,
) -> Option<(NodeId, usize)> {
    let (name, mut i) = parse_structured_field_decl_name(b, source, tokens, idx)?;
    let mut children = vec![name];

    if let Some((legacy_len, j)) = parse_optional_paren_length(b, tokens, i) {
        children.push(legacy_len);
        i = j;
    }

    while let Some((length, j)) =
        parse_optional_length_spec(b, source, tokens, i, &["TYPE", "LIKE", "VALUE"])
    {
        children.push(length);
        i = j;
    }

    if tokens
        .get(i)
        .is_some_and(|tok| is_keyword(source, tok, "type") || is_keyword(source, tok, "like"))
    {
        return None;
    }

    if allow_value && let Some((value, j)) = parse_value_clause(b, source, tokens, i) {
        children.push(value);
        i = j;
    }

    let range = b.span(*children.first().unwrap()).start..b.span(*children.last().unwrap()).end;
    Some((
        b.branch(SyntaxKind::StructuredFieldClause, range, &children),
        i,
    ))
}

fn try_parse_types_structured_block_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let types_tok = tokens.get(idx)?;
    if !is_keyword(source, types_tok, "types") {
        return None;
    }

    let mut i = idx + 1;
    if tokens.get(i).map(|t| t.kind) == Some(TokenKind::Colon) {
        i += 1;
    }

    let (structured, next_i) = parse_types_structured_block(b, source, tokens, i)?;
    let types_kw = token_leaf(b, types_tok);
    let node = b.branch(
        SyntaxKind::TypesDecl,
        types_tok.range.start..b.span(structured).end,
        &[types_kw, structured],
    );
    Some((node, next_i))
}

fn parse_types_structured_block(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let begin_tok = tokens.get(idx)?;
    let (mut children, header_i, flavor) = parse_structured_decl_header(b, source, tokens, idx)?;
    if tokens.get(header_i).map(|t| t.kind) != Some(TokenKind::Period) {
        return None;
    }

    children.push(token_leaf(b, tokens.get(header_i)?));
    let mut i = header_i + 1;

    loop {
        while tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comment) {
            children.push(token_leaf(b, tokens.get(i)?));
            i += 1;
        }

        let tok = tokens.get(i)?;
        if tok.kind == TokenKind::Eof {
            return None;
        }
        if let Some((end_children, next_i)) =
            parse_structured_decl_end(b, source, tokens, i, flavor)
            && tokens.get(next_i).map(|t| t.kind) == Some(TokenKind::Period)
        {
            children.extend(end_children);
            children.push(token_leaf(b, tokens.get(next_i)?));
            let node = b.branch(
                SyntaxKind::StructuredDecl,
                begin_tok.range.start..tokens.get(next_i)?.range.end,
                &children,
            );
            return Some((node, next_i + 1));
        }

        if is_keyword(source, tok, "include") {
            let (include, next_i) = parse_structured_include_clause(b, source, tokens, i)?;
            if tokens.get(next_i).map(|t| t.kind) != Some(TokenKind::Period) {
                return None;
            }
            children.push(include);
            children.push(token_leaf(b, tokens.get(next_i)?));
            i = next_i + 1;
            continue;
        }

        if is_keyword(source, tok, "types") {
            i += 1;
            if tokens.get(i).map(|t| t.kind) == Some(TokenKind::Colon) {
                i += 1;
            }
            let next_i =
                parse_structured_types_component_run(b, source, tokens, i, flavor, &mut children)?;
            i = next_i;
            continue;
        }

        return None;
    }
}

fn parse_structured_types_component_run(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    mut idx: usize,
    flavor: StructuredDeclFlavor,
    out: &mut Vec<NodeId>,
) -> Option<usize> {
    loop {
        while tokens.get(idx).map(|t| t.kind) == Some(TokenKind::Comment) {
            out.push(token_leaf(b, tokens.get(idx)?));
            idx += 1;
        }

        let tok = tokens.get(idx)?;
        if tok.kind == TokenKind::Eof {
            return None;
        }
        if is_keyword(source, tok, "end")
            && tokens
                .get(idx + 1)
                .is_some_and(|next| is_keyword(source, next, "of"))
        {
            return Some(idx);
        }

        let allow_value = flavor == StructuredDeclFlavor::Enum;
        let (component, next_i) = parse_structured_decl(
            b,
            source,
            tokens,
            idx,
            SyntaxKind::StructuredDecl,
            true,
            allow_value,
        )
        .or_else(|| parse_structured_include_clause(b, source, tokens, idx))
        .or_else(|| parse_structured_field_clause(b, source, tokens, idx, true, allow_value))?;
        out.push(component);
        idx = next_i;

        while tokens.get(idx).map(|t| t.kind) == Some(TokenKind::Comment) {
            out.push(token_leaf(b, tokens.get(idx)?));
            idx += 1;
        }

        if tokens.get(idx).map(|t| t.kind) == Some(TokenKind::Comma) {
            idx += 1;
            continue;
        }

        if tokens
            .get(idx)
            .is_some_and(|tok| is_keyword(source, tok, "end"))
            && tokens
                .get(idx + 1)
                .is_some_and(|next| is_keyword(source, next, "of"))
        {
            return Some(idx);
        }

        return None;
    }
}

fn structured_decl_marker_name(
    source: &str,
    tokens: &[Token],
    idx: usize,
    keyword: &str,
) -> Option<usize> {
    if is_keyword(source, tokens.get(idx)?, keyword)
        && tokens
            .get(idx + 1)
            .is_some_and(|tok| is_keyword(source, tok, "of"))
        && tokens.get(idx + 2)?.kind == TokenKind::Ident
    {
        Some(idx + 2)
    } else {
        None
    }
}

fn skip_structured_decl_continuation_prefix(
    source: &str,
    tokens: &[Token],
    mut idx: usize,
) -> Option<usize> {
    while tokens.get(idx).map(|t| t.kind) == Some(TokenKind::Comment) {
        idx += 1;
    }
    if !tokens
        .get(idx)
        .is_some_and(|tok| is_structured_decl_continuation_keyword(source, tok))
    {
        return None;
    }
    idx += 1;
    if tokens.get(idx).map(|t| t.kind) == Some(TokenKind::Colon) {
        idx += 1;
    }
    Some(idx)
}

fn previous_non_comment_kind(tokens: &[Token], mut idx: usize) -> Option<TokenKind> {
    loop {
        idx = idx.checked_sub(1)?;
        let kind = tokens.get(idx)?.kind;
        if kind != TokenKind::Comment {
            return Some(kind);
        }
    }
}

fn validate_structured_decl_marker_separator(
    tokens: &[Token],
    idx: usize,
    saw_marker: bool,
    marker: &str,
    name: &str,
) -> Result<(), String> {
    if !saw_marker
        || matches!(
            previous_non_comment_kind(tokens, idx),
            Some(TokenKind::Colon | TokenKind::Comma | TokenKind::Period)
        )
    {
        return Ok(());
    }
    Err(format!(
        "syntax error: expected ',' before {marker} OF {name}"
    ))
}

fn validate_structured_decl_nesting(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Result<bool, String> {
    let mut stack: Vec<usize> = Vec::new();
    let mut saw_marker = false;
    let mut i = start;

    while i < end_exclusive {
        let Some(tok) = tokens.get(i) else {
            break;
        };
        if tok.kind == TokenKind::Comment {
            i += 1;
            continue;
        }

        if let Some(name_i) = structured_decl_marker_name(source, tokens, i, "begin") {
            let name = tokens[name_i].lexeme(source);
            validate_structured_decl_marker_separator(tokens, i, saw_marker, "BEGIN", name)?;
            saw_marker = true;
            stack.push(name_i);
            i = name_i + 1;
            continue;
        }

        if let Some(name_i) = structured_decl_marker_name(source, tokens, i, "end") {
            let close_name = tokens[name_i].lexeme(source);
            validate_structured_decl_marker_separator(tokens, i, saw_marker, "END", close_name)?;
            saw_marker = true;
            let Some(&open_i) = stack.last() else {
                return Err(format!("syntax error: unexpected END OF {close_name}"));
            };
            let open_name = tokens[open_i].lexeme(source);
            if !open_name.eq_ignore_ascii_case(close_name) {
                return Err(format!(
                    "syntax error: expected END OF {open_name} before END OF {close_name}"
                ));
            }
            stack.pop();
            i = name_i + 1;
            continue;
        }

        if tok.kind == TokenKind::Period
            && let Some(&open_i) = stack.last()
        {
            if let Some(next_i) = skip_structured_decl_continuation_prefix(source, tokens, i + 1) {
                i = next_i;
                continue;
            }
            let open_name = tokens[open_i].lexeme(source);
            return Err(format!(
                "syntax error: expected END OF {open_name} before '.'"
            ));
        }

        i += 1;
    }

    if let Some(&open_i) = stack.last() {
        let open_name = tokens[open_i].lexeme(source);
        return Err(format!("syntax error: expected END OF {open_name}"));
    }
    Ok(saw_marker)
}

fn token_end_before_eof(tokens: &[Token], end_exclusive: usize, fallback: usize) -> usize {
    tokens
        .get(end_exclusive.saturating_sub(1))
        .filter(|tok| tok.kind != TokenKind::Eof)
        .map_or(fallback, |tok| tok.range.end)
}

fn parse_malformed_constants_decl(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
    message: String,
    end_exclusive: usize,
) -> Option<(NodeId, usize)> {
    let constants_tok = tokens.get(idx)?;
    let err_end = token_end_before_eof(tokens, end_exclusive, constants_tok.range.end);

    errors.push(crate::ParseError {
        message,
        range: constants_tok.range.start..err_end,
    });
    let mut children = Vec::with_capacity(end_exclusive.saturating_sub(idx));
    for tok in &tokens[idx..end_exclusive] {
        children.push(token_leaf(b, tok));
    }
    let node = b.branch(
        SyntaxKind::Error,
        constants_tok.range.start..err_end,
        &children,
    );
    let next = if tokens.get(end_exclusive).map(|t| t.kind) == Some(TokenKind::Eof) {
        tokens.len()
    } else {
        end_exclusive
    };
    Some((node, next))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::build_file_tree;
    use abap_ast::File;
    use abap_lexer::tokenize;

    fn tree_ok(src: &str) -> File {
        let tok = tokenize(src);
        let mut err = Vec::new();
        let file = build_file_tree(src, &tok.tokens, src.len(), &mut err);
        assert!(err.is_empty(), "{:?}", err);
        file
    }

    #[test]
    fn single_data_typed_decl() {
        let src = "DATA lv_count TYPE i.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 1);
    }

    #[test]
    fn chained_data_colon() {
        let src = "DATA: lv_a TYPE i, lv_b TYPE string.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 2);
    }

    #[test]
    fn chained_data_accepts_table_type_and_inline_comment() {
        let src =
            "DATA: lt_split TYPE STANDARD TABLE OF string, \" comment\n      ls_split TYPE string.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 2);
    }

    #[test]
    fn chained_data_accepts_value_clause() {
        let src = "DATA: lv_a TYPE i, lv_b TYPE int2 VALUE 1.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 2);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ValueClause), 1);
    }

    #[test]
    fn parameters_chain_parses_typed_clauses_and_defaults() {
        let src = "PARAMETERS:\n  p_text TYPE string LOWER CASE OBLIGATORY,\n  p_pub TYPE localfile LOWER CASE OBLIGATORY,\n  p_app TYPE ssfappl DEFAULT 'DFAULT',\n  p_sym TYPE ssfencr DEFAULT 'AES128-CBC'.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ParametersDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 4);
        assert!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple) >= 4);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ValueClause), 2);
    }

    #[test]
    fn parameter_chain_parses_typed_clauses_and_defaults() {
        let src = "PARAMETER:\n  p_text TYPE string LOWER CASE OBLIGATORY,\n  p_pub TYPE localfile LOWER CASE OBLIGATORY,\n  p_app TYPE ssfappl DEFAULT 'DFAULT',\n  p_sym TYPE ssfencr DEFAULT 'AES128-CBC'.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ParametersDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 4);
        assert!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple) >= 4);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ValueClause), 2);
    }

    #[test]
    fn parameters_clause_accepts_implicit_char_length_form() {
        let file = tree_ok("PARAMETERS p_pass(30) LOWER CASE.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ParametersDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 1);
        assert!(file.count_kind(file.root(), SyntaxKind::LengthSpec) >= 1);
    }

    #[test]
    fn parameters_clause_accepts_checkbox_form_without_explicit_type() {
        let file = tree_ok("PARAMETERS: c_rom AS CHECKBOX.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ParametersDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 0);
    }

    #[test]
    fn parameters_default_stops_before_user_command_addition() {
        let src = "\
PARAMETERS: p_backgr RADIOBUTTON GROUP g01 DEFAULT 'X' USER-COMMAND upd,
            p_manual RADIOBUTTON GROUP g01.";
        let file = tree_ok(src);
        let value = file
            .find_first_kind(file.root(), SyntaxKind::ValueClause)
            .expect("default value");
        assert_eq!(&src[file.range(value)], "DEFAULT 'X'");
    }

    #[test]
    fn select_options_clause_parses_for_operand_structurally() {
        let file = tree_ok("SELECT-OPTIONS: s_rogln FOR lv_rogln.");
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::SelectOptionsDecl),
            1
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 1);
    }

    #[test]
    fn select_options_clause_accepts_documented_additions() {
        let src = "\
SELECT-OPTIONS:
  s_gln FOR gv_gln OBLIGATORY VISIBLE LENGTH 20 DEFAULT 'A' TO 'Z' OPTION BT SIGN I LOWER CASE MATCHCODE OBJECT /sttp/h_loc_gln MEMORY ID gln NO DATABASE SELECTION HELP-REQUEST FOR LOW VALUE-REQUEST FOR HIGH,
  s_dyn FOR (lv_type) NO-DISPLAY NO-EXTENSION NO INTERVALS MODIF ID grp.";
        let file = tree_ok(src);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::SelectOptionsDecl),
            1
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 2);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 2);
    }

    #[test]
    fn tables_decl_parses_chained_work_areas() {
        let file = tree_ok("TABLES: tbtco, v_op.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TablesDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 2);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDeclName), 2);
    }

    #[test]
    fn tables_decl_parses_single_work_area() {
        let file = tree_ok("TABLES tbtco.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TablesDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 1);
    }

    #[test]
    fn chained_data_accepts_like_clauses() {
        let src = "DATA: ls_line LIKE LINE OF itab, lt_copy LIKE itab.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 2);
        assert!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple) >= 2);
    }

    #[test]
    fn class_data_accepts_like_clause() {
        let src = "CLASS-DATA gt_copy LIKE gt_source.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 1);
    }

    #[test]
    fn data_begin_end_of_clause() {
        let file = tree_ok("DATA: BEGIN OF ls_dat, yyyy(4), mm(2), dd(2), END OF ls_dat.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
    }

    #[test]
    fn grouped_data_begin_end_accepts_like_components_and_following_clauses() {
        let src = "\
DATA: BEGIN OF gs_user_creation,\n\
        username  LIKE bapibname-bapibname,\n\
        firstname TYPE bapiaddr3-firstname,\n\
        password  LIKE bapipwd,\n\
      END OF gs_user_creation,\n\
\n\
      gt_user_creation LIKE TABLE OF gs_user_creation,\n\
      gv_file_name     TYPE string.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 3);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            3
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::Error), 0);
    }

    #[test]
    fn block_structured_data_decl_parses_include_and_data_components() {
        let src = "\
DATA BEGIN OF wa_zatt_trans_cust.\n\
INCLUDE TYPE  zatt_trans_cust.\n\
DATA: status_info     TYPE string,\n\
      transport_info  TYPE string,\n\
      recall_info     TYPE string,\n\
      zz_req_del_date TYPE datum,\n\
      zz_plan_gi_date TYPE datum,\n\
      check           TYPE char1,\n\
      END OF wa_zatt_trans_cust.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::StructuredIncludeClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::StructuredFieldClause),
            6
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IncludeStmt),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::Error),
            0
        );
    }

    #[test]
    fn data_common_part_delimiters_parse_as_data_decls() {
        let src = "\
DATA: BEGIN OF COMMON PART fm06lcbe.\n\
DATA: END OF COMMON PART.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 2);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::UnparsedStmt), 0);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::Error), 0);
    }

    #[test]
    fn data_begin_of_occurs_include_structure_form_parses() {
        let src = "\
DATA: BEGIN OF bet OCCURS 50.\n\
        INCLUDE STRUCTURE ekbe.\n\
DATA: END OF bet.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredIncludeClause),
            1
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::Error), 0);
    }

    #[test]
    fn data_occurs_header_line_is_decl_tail_not_type_ref() {
        use abap_ast::ast::{AstNode, DeclClause, SyntaxNodeRef};

        let src = "DATA: int_eket LIKE beket OCCURS 0 WITH HEADER LINE.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let clause = DeclClause::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::DataTypedClause)
                .expect("data typed clause"),
        ))
        .expect("decl clause");
        let (type_ref, _) = clause.type_ref_with_namespace(src).expect("type ref");
        assert_eq!(type_ref.display_text(src), Some("beket"));
    }

    #[test]
    fn data_name_minus_chain() {
        let src = "DATA screen0100-serial TYPE c.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDeclName), 1);
    }

    #[test]
    fn type_ref_selector_chain() {
        let src = "DATA r TYPE ty_ref=>elem.";
        let file = tree_ok(src);
        let type_refs = file.count_kind(file.root(), SyntaxKind::TypeRefSimple);
        assert!(type_refs >= 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::TypeRefSelectorChain),
            1
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefName), 2);
    }

    #[test]
    fn data_ref_to_class_type_ref() {
        let src = "DATA lo_instance TYPE REF TO some_class.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 2);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefName), 1);
    }

    #[test]
    fn table_wrapper_type_ref_nests_inner_type() {
        let src = "TYPES ty_stmt_tab TYPE STANDARD TABLE OF REF TO zcl_stmt WITH DEFAULT KEY.";
        let file = tree_ok(src);
        let type_ref = file
            .find_first_kind(file.root(), SyntaxKind::TypeRefSimple)
            .expect("type ref");
        assert_eq!(file.count_kind(type_ref, SyntaxKind::TypeRefName), 1);
        assert!(file.count_kind(type_ref, SyntaxKind::TypeRefSimple) >= 2);
    }

    #[test]
    fn data_inline_decl() {
        let file = tree_ok("DATA(lv_value) = 1.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataInlineDecl), 1);
    }

    #[test]
    fn data_inline_decl_rejects_whitespace_inside_parentheses() {
        for src in [
            "DATA( lv_var) = 1.",
            "DATA( lv_var ) = 1.",
            "DATA(lv_var ) = 1.",
            "DATA(\nlv_var) = 1.",
        ] {
            let parsed = crate::parse(src);
            assert!(
                parsed
                    .errors
                    .iter()
                    .any(|err| err.message.contains("inline DATA declaration")),
                "{src}: {:?}",
                parsed.errors
            );
            assert_eq!(
                parsed
                    .file
                    .count_kind(parsed.file.root(), SyntaxKind::DataInlineDecl),
                0
            );
            assert!(
                parsed
                    .file
                    .count_kind(parsed.file.root(), SyntaxKind::InvalidStmt)
                    >= 1
            );
        }
    }

    #[test]
    fn statics_decl_with_value() {
        let file = tree_ok("STATICS sv_last TYPE tznzone VALUE 'UTC'.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::StaticsDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ValueClause), 1);
    }

    #[test]
    fn types_chain_decl() {
        let file = tree_ok("TYPES: ty_int TYPE i, ty_name TYPE string.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::TypesTypedClause),
            2
        );
    }

    #[test]
    fn constants_decl_with_length_and_value() {
        let file = tree_ok("CONSTANTS lcv_max(14) TYPE p DECIMALS 7 VALUE '0.9999999'.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ConstantsDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ConstantClause), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::LengthSpec), 2);
    }

    #[test]
    fn field_symbols_like_line_of() {
        let file = tree_ok("FIELD-SYMBOLS <line> LIKE LINE OF itab.");
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::FieldSymbolsDecl),
            1
        );
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::FieldSymbolClause),
            1
        );
    }

    #[test]
    fn field_symbols_generic_standard_table() {
        let file = tree_ok("FIELD-SYMBOLS: <lt_records> TYPE STANDARD TABLE.");
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::FieldSymbolsDecl),
            1
        );
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::FieldSymbolClause),
            1
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::Error), 0);
    }

    #[test]
    fn types_begin_end_of_clause() {
        let file = tree_ok("TYPES: BEGIN OF ty_pair, a TYPE i, b TYPE string, END OF ty_pair.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
    }

    #[test]
    fn types_decl_accepts_like_clause() {
        let file = tree_ok("TYPES ty_repid LIKE sy-repid.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::TypesTypedClause),
            1
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 1);
    }

    #[test]
    fn structured_types_clause_parses_component_fields() {
        let file = tree_ok(
            "TYPES: BEGIN OF ts_cust_info, type TYPE char1, root TYPE string, END OF ts_cust_info.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            2
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 2);
    }

    #[test]
    fn structured_types_clause_parses_include_components() {
        let file = tree_ok(
            "TYPES: BEGIN OF ty_outer, INCLUDE TYPE ty_inner AS inner, field TYPE i, END OF ty_outer.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredIncludeClause),
            1
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 2);
    }

    #[test]
    fn structured_types_clause_accepts_like_components() {
        let file = tree_ok(
            "TYPES: BEGIN OF ty_evt, ucomm LIKE sy-ucomm, fieldname LIKE dd03p-fieldname, END OF ty_evt.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            2
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 2);
    }

    #[test]
    fn structured_types_clause_accepts_untyped_components() {
        let file =
            tree_ok("TYPES: BEGIN OF ty_sel, stext(40), sign0(1), length TYPE p, END OF ty_sel.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            3
        );
    }

    #[test]
    fn block_structured_types_clause_parses_include_components() {
        let file = tree_ok(
            "TYPES: BEGIN OF ty_outer. INCLUDE TYPE ty_inner AS inner. TYPES: field TYPE i, other TYPE string, END OF ty_outer.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::StructuredDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredIncludeClause),
            1
        );
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            2
        );
    }

    #[test]
    fn block_structured_types_clause_accepts_like_and_untyped_components() {
        let src = "\
TYPES: BEGIN OF slis_seldis1_alv.\n\
TYPES: field LIKE dfies-fieldname,\n\
       table LIKE dfies-tabname,\n\
       stext(40),\n\
       valuf(80),\n\
       length TYPE p,\n\
END OF slis_seldis1_alv.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::StructuredFieldClause),
            5
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::TypesTypedClause),
            0
        );
    }

    #[test]
    fn block_structured_types_clause_keeps_namespaced_include_type_as_structured_include() {
        let src = "\
TYPES:\n\
  BEGIN OF ts_notif_attr_split.\n\
  INCLUDE TYPE /sttp/s_ru_notif_attr_split AS attr_split.\n\
  TYPES: split_by_size_end TYPE abap_bool,\n\
         split_by_size_seq TYPE i,\n\
  END OF ts_notif_attr_split.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::StructuredIncludeClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IncludeStmt),
            0
        );
    }

    #[test]
    fn structured_include_clause_exposes_alias_and_suffix() {
        use abap_ast::ast::{
            AstNode, StructuredIncludeClause, StructuredIncludeKind, SyntaxNodeRef,
        };

        let src = "TYPES: BEGIN OF ty_outer, INCLUDE TYPE ty_inner AS inner RENAMING WITH SUFFIX _x, END OF ty_outer.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let clause = StructuredIncludeClause::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::StructuredIncludeClause)
                .expect("structured include"),
        ))
        .expect("structured include");
        assert_eq!(clause.kind(src), Some(StructuredIncludeKind::Type));
        assert_eq!(clause.alias_name(src).as_deref(), Some("inner"));
        assert_eq!(clause.suffix(src).as_deref(), Some("_x"));
        assert!(clause.type_ref().is_some());
    }

    #[test]
    fn block_structured_types_clause_accepts_following_types_prefix_without_space() {
        let src = "\
CLASS /STTP/CL_REP_RU DEFINITION.\n\
  PUBLIC SECTION.\n\
    TYPES:\n\
      BEGIN OF ts_notif_attr_split.\n\
      INCLUDE TYPE /sttp/s_ru_notif_attr_split AS attr_split.\n\
      TYPES:split_by_size_end TYPE abap_bool,\n\
            split_by_size_seq TYPE i,\n\
            END OF ts_notif_attr_split .\n\
ENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::StructuredIncludeClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IncludeStmt),
            0
        );
    }

    #[test]
    fn structured_types_clause_accepts_hybrid_comma_and_period_include_form() {
        let src = "\
METHOD run.\n\
  TYPES:\n\
    BEGIN OF ts_revt_obj_rel,\n\
      objid TYPE /sttp/e_objid.\n\
  INCLUDE TYPE /sttp/rep_evt AS rep_evt.\n\
  TYPES: END OF ts_revt_obj_rel,\n\
         tt_revt_obj_rel TYPE STANDARD TABLE OF ts_revt_obj_rel WITH DEFAULT KEY.\n\
ENDMETHOD.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::StructuredIncludeClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IncludeStmt),
            0
        );
    }

    #[test]
    fn nested_structured_types_clause_creates_nested_nodes() {
        let file = tree_ok(
            "TYPES: BEGIN OF ty_outer, BEGIN OF inner, a TYPE i, END OF inner, END OF ty_outer.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::StructuredDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            1
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 1);
    }

    #[test]
    fn types_enum_chained_accepts_base_type_and_value_initial() {
        let file = tree_ok(
            "TYPES: BEGIN OF ENUM ty_flag BASE TYPE abap_bool, false VALUE IS INITIAL, true VALUE abap_true, END OF ENUM ty_flag.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::TypesTypedClause),
            1
        );
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            2
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ValueClause), 2);
    }

    #[test]
    fn types_enum_block_form_accepts_types_prefixes() {
        let src = "\
TYPES BEGIN OF ENUM ty_status.\n\
TYPES open.\n\
TYPES closed.\n\
TYPES END OF ENUM ty_status.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            2
        );
    }

    #[test]
    fn types_enum_accepts_structure_addition_on_begin_and_end() {
        let file = tree_ok(
            "TYPES: BEGIN OF ENUM ty_status STRUCTURE status, open VALUE IS INITIAL, closed VALUE 1, END OF ENUM ty_status STRUCTURE status.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            2
        );
    }

    #[test]
    fn types_mesh_chained_accepts_association_using_key() {
        let file = tree_ok(
            "TYPES: BEGIN OF MESH ty_graph, nodes TYPE tt_node ASSOCIATION to_edges TO edges ON id = source USING KEY by_source, edges TYPE tt_edge, END OF MESH ty_graph.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            2
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 2);
    }

    #[test]
    fn constants_begin_end_of_clause() {
        let file = tree_ok(
            "CONSTANTS: BEGIN OF gc_pair, a TYPE i VALUE 1, b TYPE i VALUE 2, END OF gc_pair.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ConstantsDecl), 1);
    }

    fn constants_class_src(body: &str) -> String {
        format!("CLASS z_demo DEFINITION.\n  PUBLIC SECTION.\n    CONSTANTS:\n{body}\nENDCLASS.")
    }

    fn nested_constants_body(dispatch_suffix: &str, tail: &str) -> String {
        format!(
            "      BEGIN OF gcs_aif_ifname,\n\
        BEGIN OF europe,\n\
          aggregation_epa_32    TYPE string  VALUE 'ZEU_EPA_32' ##no_text,\n\
          dispatch_edp_33       TYPE string  VALUE 'ZEU_EDP_33' ##no_text{dispatch_suffix}\n\
{tail}"
        )
    }

    fn assert_malformed_constants_class(body: &str, message_fragment: &str) {
        let src = constants_class_src(body);
        let parsed = crate::parse(&src);
        assert!(
            parsed
                .errors
                .iter()
                .any(|err| err.message.contains(message_fragment)),
            "expected parser error containing {message_fragment:?}, got {:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ClassDecl),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ConstantsDecl),
            0,
            "malformed structured CONSTANTS should not produce a ConstantsDecl"
        );
        assert!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::Error)
                >= 1
        );
    }

    #[test]
    fn nested_constants_begin_end_of_clause() {
        let body =
            nested_constants_body(",", "        END OF europe,\n      END OF gcs_aif_ifname.");
        let src = constants_class_src(&body);
        let parsed = crate::parse(&src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ConstantsDecl),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::StructuredFieldClause),
            2
        );
    }

    #[test]
    fn malformed_nested_constants_report_structural_errors() {
        for (dispatch_suffix, tail, message) in [
            (
                ",",
                "        END OF europe.",
                "expected END OF gcs_aif_ifname before '.'",
            ),
            (".", "", "expected END OF europe before '.'"),
            (
                ",",
                "      END OF gcs_aif_ifname.",
                "expected END OF europe before END OF gcs_aif_ifname",
            ),
            (
                ",",
                "        END OF asia,\n      END OF gcs_aif_ifname.",
                "expected END OF europe before END OF asia",
            ),
            (
                ",",
                "        END OF europe,\n      END OF gcs_aif_name.",
                "expected END OF gcs_aif_ifname before END OF gcs_aif_name",
            ),
            (
                ",",
                "        END OF europe,\n      END OF gcs_aif_ifname,\n      END OF extra.",
                "unexpected END OF extra",
            ),
            (
                "",
                "        END OF europe,\n      END OF gcs_aif_ifname.",
                "expected ',' before END OF europe",
            ),
            (
                ",",
                "        END OF europe\n      END OF gcs_aif_ifname.",
                "expected ',' before END OF gcs_aif_ifname",
            ),
        ] {
            let body = nested_constants_body(dispatch_suffix, tail);
            assert_malformed_constants_class(&body, message);
        }
    }

    #[test]
    fn constants_grouped_multiline_matches_class_style() {
        let src = "CONSTANTS:\n\
      BEGIN OF gc_s_gln_partition_table,\n\
        p0_cp_number_of_bits   TYPE i VALUE 40,\n\
        p0_cp_number_of_digits TYPE i VALUE  12,\n\
      END OF gc_s_gln_partition_table .";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ConstantsDecl), 1);
        assert!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause) >= 2,
            "expected structured constant components"
        );
    }

    #[test]
    fn constants_begin_of_accepts_value_only_and_numeric_component_names() {
        let src = "\
CONSTANTS: BEGIN OF gc_bapi_proc_mode,\n\
             aip VALUE 'A',\n\
             46c VALUE 'B',\n\
           END OF gc_bapi_proc_mode.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ConstantsDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            2
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ValueClause), 2);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::Error), 0);
    }

    #[test]
    fn data_inline_not_matched() {
        let src = "DATA(ref) = 1.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 0);
    }

    #[test]
    fn comma_chain_requires_colon() {
        let src = "DATA a TYPE i, b TYPE i.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 0);
    }

    /// Unsupported `DATA` forms must not get a `DataDecl` wrapper or spurious diagnostics.
    fn assert_not_classified_as_data_decl(src: &str) {
        let parsed = crate::parse(src);
        assert!(
            parsed.errors.is_empty(),
            "expected no errors for {src:?}, got {:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
            0,
            "expected no DataDecl node for {src:?}"
        );
    }

    fn assert_malformed_data_decl(src: &str, message_fragment: &str) {
        let parsed = crate::parse(src);
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains(message_fragment)),
            "expected parser error containing {message_fragment:?} for {src:?}, got {:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
            0,
            "expected malformed DATA to avoid DataDecl node for {src:?}"
        );
        assert!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InvalidStmt)
                >= 1,
            "expected malformed DATA to produce an InvalidStmt node for {src:?}"
        );
    }

    #[test]
    fn negative_missing_variable_name() {
        assert_malformed_data_decl("DATA TYPE i.", "expected declaration name");
    }

    #[test]
    fn negative_missing_type_ref() {
        assert_malformed_data_decl("DATA lv TYPE .", "expected type name");
    }

    #[test]
    fn negative_missing_trailing_period() {
        let parsed = crate::parse("DATA lv TYPE string");
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
            0
        );
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("expected '.'")),
            "{:?}",
            parsed.errors
        );
    }

    #[test]
    fn single_data_like_decl() {
        let file = tree_ok("DATA lv LIKE lv_other.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 1);
    }

    #[test]
    fn data_decl_with_value() {
        let file = tree_ok("DATA lv TYPE i VALUE 0.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ValueClause), 1);
    }

    #[test]
    fn class_data_decl_with_value_and_comment() {
        let file = tree_ok(r##"CLASS-DATA sv_loglevel TYPE i VALUE 0. "#EC NOTEXT" ."##);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ValueClause), 1);
    }

    #[test]
    fn negative_assignment_style_not_typed_decl() {
        assert_not_classified_as_data_decl("DATA lv = 1.");
    }

    #[test]
    fn negative_empty_chain_after_colon() {
        assert_malformed_data_decl("DATA: .", "expected declaration name");
    }

    #[test]
    fn negative_double_comma() {
        assert_malformed_data_decl(
            "DATA: lv_a TYPE i,, lv_b TYPE i.",
            "expected declaration after ','",
        );
    }

    #[test]
    fn malformed_data_recovers_before_next_statement() {
        let parsed = crate::parse("DATA lv TYPE string\nlv_after = 1.");
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
            1
        );
    }
}
