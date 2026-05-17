use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind};

use crate::block_helpers::{
    inline_name_spacing_is_valid, is_keyword, parse_inline_name, skip_trivia,
};
use crate::parser::{PResult, ParseFailure, Parser};
use crate::stmt_period::{is_definite_stmt_lead_keyword, line_start_assignment, token_begins_line};
use crate::syntax::token_leaf;
use crate::type_ref::build_type_ref_node;

fn scan_catch_type_ref_end(tokens: &[Token], idx: usize) -> usize {
    let Some(first) = tokens.get(idx) else {
        return idx;
    };
    if first.kind != TokenKind::Ident {
        return idx;
    }

    let mut i = idx + 1;
    while i + 1 < tokens.len() {
        let op = &tokens[i];
        let next = &tokens[i + 1];
        if !matches!(
            op.kind,
            TokenKind::Minus | TokenKind::Arrow | TokenKind::FatArrow | TokenKind::Tilde
        ) || next.kind != TokenKind::Ident
        {
            break;
        }
        i += 2;
    }
    i
}

fn catch_system_exceptions_body_start(source: &str, tokens: &[Token], idx: usize) -> Option<usize> {
    let catch_tok = tokens.get(idx)?;
    if !is_keyword(source, catch_tok, "catch") {
        return None;
    }

    let system_idx = skip_trivia(tokens, idx + 1);
    let system_tok = tokens.get(system_idx)?;
    if !is_keyword(source, system_tok, "system") {
        return None;
    }
    if tokens.get(system_idx + 1).map(|token| token.kind) != Some(TokenKind::Minus) {
        return None;
    }
    let exceptions_idx = system_idx + 2;
    let exceptions_tok = tokens.get(exceptions_idx)?;
    if !is_keyword(source, exceptions_tok, "exceptions") {
        return None;
    }
    Some(exceptions_idx + 1)
}

fn try_parse_loop_inline_data_target(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let data_tok = tokens.get(idx)?;
    if !is_keyword(source, data_tok, "data") {
        return None;
    }
    let lparen = tokens.get(idx + 1)?;
    if lparen.kind != TokenKind::LParen {
        return None;
    }
    let (name, next_idx) = parse_inline_name(b, tokens, idx + 2)?;
    let rparen = tokens.get(next_idx)?;
    if rparen.kind != TokenKind::RParen {
        return None;
    }
    if !inline_name_spacing_is_valid(tokens, idx + 1, idx + 2, next_idx) {
        let mut children = Vec::with_capacity(next_idx - idx + 1);
        for token in &tokens[idx..=next_idx] {
            children.push(token_leaf(b, token));
        }
        return Some((
            b.branch(
                SyntaxKind::Error,
                data_tok.range.start..rparen.range.end,
                &children,
            ),
            next_idx + 1,
        ));
    }
    let data_leaf = token_leaf(b, data_tok);
    let lparen_leaf = token_leaf(b, lparen);
    let rparen_leaf = token_leaf(b, rparen);
    Some((
        b.branch(
            SyntaxKind::DataInlineDecl,
            data_tok.range.start..rparen.range.end,
            &[data_leaf, lparen_leaf, name, rparen_leaf],
        ),
        next_idx + 1,
    ))
}

fn try_parse_loop_inline_field_symbol_target(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let field_tok = tokens.get(idx)?;
    if !is_keyword(source, field_tok, "field")
        || tokens.get(idx + 1).map(|tok| tok.kind) != Some(TokenKind::Minus)
        || !tokens
            .get(idx + 2)
            .is_some_and(|tok| is_keyword(source, tok, "symbol"))
    {
        return None;
    }

    let lparen = tokens.get(idx + 3)?;
    if lparen.kind != TokenKind::LParen {
        return None;
    }
    let (name, next_idx) = parse_inline_name(b, tokens, idx + 4)?;
    let rparen = tokens.get(next_idx)?;
    if rparen.kind != TokenKind::RParen {
        return None;
    }
    if !inline_name_spacing_is_valid(tokens, idx + 3, idx + 4, next_idx) {
        let mut children = Vec::with_capacity(next_idx - idx + 1);
        for token in &tokens[idx..=next_idx] {
            children.push(token_leaf(b, token));
        }
        return Some((
            b.branch(
                SyntaxKind::Error,
                field_tok.range.start..rparen.range.end,
                &children,
            ),
            next_idx + 1,
        ));
    }
    let field_leaf = token_leaf(b, field_tok);
    let minus_leaf = token_leaf(b, &tokens[idx + 1]);
    let symbol_leaf = token_leaf(b, &tokens[idx + 2]);
    let lparen_leaf = token_leaf(b, lparen);
    let rparen_leaf = token_leaf(b, rparen);

    Some((
        b.branch(
            SyntaxKind::FieldSymbolInlineDecl,
            field_tok.range.start..rparen.range.end,
            &[
                field_leaf,
                minus_leaf,
                symbol_leaf,
                lparen_leaf,
                name,
                rparen_leaf,
            ],
        ),
        next_idx + 1,
    ))
}

fn at_stmt_body_start(source: &str, tokens: &[Token], idx: usize) -> Option<usize> {
    let at_tok = tokens.get(idx)?;
    if !is_keyword(source, at_tok, "at") {
        return None;
    }

    let first = skip_trivia(tokens, idx + 1);
    let first_tok = tokens.get(first)?;
    if is_keyword(source, first_tok, "first") || is_keyword(source, first_tok, "last") {
        return Some(first + 1);
    }
    if is_keyword(source, first_tok, "new") {
        return Some(skip_trivia(tokens, first + 1));
    }
    if is_keyword(source, first_tok, "end") {
        let of_idx = skip_trivia(tokens, first + 1);
        let of_tok = tokens.get(of_idx)?;
        if is_keyword(source, of_tok, "of") {
            return Some(skip_trivia(tokens, of_idx + 1));
        }
    }
    None
}

pub(crate) fn at_stmt_starts(cursor: &Parser<'_, '_>) -> bool {
    at_stmt_body_start(cursor.source(), cursor.tokens(), cursor.index()).is_some()
}

pub(crate) fn catch_system_exceptions_stmt_starts(cursor: &Parser<'_, '_>) -> bool {
    catch_system_exceptions_body_start(cursor.source(), cursor.tokens(), cursor.index()).is_some()
}

const WHILE_BOUNDARY_KEYWORDS: &[&str] = &["ENDWHILE"];
const DO_BOUNDARY_KEYWORDS: &[&str] = &["ENDDO"];
const LOOP_BOUNDARY_KEYWORDS: &[&str] = &["ENDLOOP"];
const AT_BOUNDARY_KEYWORDS: &[&str] = &["ENDAT"];
const TRY_BODY_BOUNDARY_KEYWORDS: &[&str] = &["CATCH", "CLEANUP", "ENDTRY"];
const CLEANUP_BOUNDARY_KEYWORDS: &[&str] = &["ENDTRY"];
const CATCH_SYSTEM_BOUNDARY_KEYWORDS: &[&str] = &["ENDCATCH"];
const CASE_TYPE_REF_STOP_KEYWORDS: &[&str] = &["INTO"];
const LOOP_CLAUSE_BOUNDARY_KEYWORDS: &[&str] = &[
    "INTO",
    "ASSIGNING",
    "REFERENCE",
    "WHERE",
    "USING",
    "TRANSPORTING",
    "GROUP",
    "FROM",
    "TO",
    "STEP",
];
const CASE_BOUNDARY_KEYWORDS: &[&str] = &["WHEN", "ENDCASE"];

fn token_leaf_at(cursor: &mut Parser<'_, '_>, index: usize) -> Option<NodeId> {
    let (range, token_index, kind) = {
        let token = cursor.tokens().get(index)?;
        (token.range.clone(), token.index(), token.kind)
    };
    Some(
        cursor
            .builder()
            .token_leaf(SyntaxKind::Token, range, token_index, kind),
    )
}

fn failure(cursor: &Parser<'_, '_>, message: impl Into<String>) -> ParseFailure {
    ParseFailure {
        message: message.into(),
        range: cursor.current_range(),
    }
}

fn with_failure_message(mut failure: ParseFailure, message: impl Into<String>) -> ParseFailure {
    failure.message = message.into();
    failure
}

fn positioned_failure(
    cursor: &Parser<'_, '_>,
    index: usize,
    message: impl Into<String>,
) -> ParseFailure {
    ParseFailure {
        message: message.into(),
        range: cursor
            .tokens()
            .get(index)
            .map_or_else(|| cursor.current_range(), |token| token.range.clone()),
    }
}

fn expr_boundary(cursor: &Parser<'_, '_>, stop_keywords: &[&str]) -> bool {
    let Some(token) = cursor.current() else {
        return true;
    };
    match token.kind {
        TokenKind::Period
        | TokenKind::Comma
        | TokenKind::Colon
        | TokenKind::RParen
        | TokenKind::RBracket
        | TokenKind::RBrace
        | TokenKind::Eof => true,
        TokenKind::Ident => {
            stop_keywords.iter().any(|keyword| {
                if keyword.eq_ignore_ascii_case("GROUP") {
                    cursor_at_group_by_start(cursor)
                } else {
                    cursor.at_keyword(keyword)
                }
            }) || token_begins_line(token)
        }
        _ => false,
    }
}

fn expect_expr_result(
    cursor: &mut Parser<'_, '_>,
    logical: bool,
    after: &str,
    message: impl Into<String>,
    stop_keywords: &[&str],
) -> PResult<NodeId> {
    let message = message.into();
    cursor.skip_trivia();
    if expr_boundary(cursor, stop_keywords) {
        return Err(failure(cursor, message));
    }
    let result = if logical {
        cursor.expect_logical_expr_result(after)
    } else {
        cursor.expect_arithmetic_expr_result(after)
    };
    result.map_err(|failure| with_failure_message(failure, message))
}

fn expect_control_condition_result(cursor: &mut Parser<'_, '_>, keyword: &str) -> PResult<NodeId> {
    expect_expr_result(
        cursor,
        true,
        keyword,
        format!("syntax error: expected condition after {keyword}"),
        &[],
    )
}

fn expect_loop_expr_result(
    cursor: &mut Parser<'_, '_>,
    logical: bool,
    after: &str,
) -> PResult<NodeId> {
    expect_expr_result(
        cursor,
        logical,
        after,
        format!("syntax error: expected expression after {after}"),
        LOOP_CLAUSE_BOUNDARY_KEYWORDS,
    )
}

fn expect_loop_target_result(cursor: &mut Parser<'_, '_>, after: &str) -> PResult<NodeId> {
    expect_expr_result(
        cursor,
        false,
        after,
        format!("syntax error: expected target after {after}"),
        LOOP_CLAUSE_BOUNDARY_KEYWORDS,
    )
}

fn push_raw_arithmetic_expr_result(
    cursor: &mut Parser<'_, '_>,
    children: &mut Vec<NodeId>,
    after: &str,
    message: impl Into<String>,
    stop_keywords: &[&str],
) -> PResult<()> {
    let start = cursor.index();
    let previous = cursor.previous_index();
    expect_expr_result(cursor, false, after, message, stop_keywords)?;
    let end = cursor.index();
    cursor.set_position(start, previous);
    while cursor.index() < end {
        children.push(cursor.bump().expect("expression token exists"));
    }
    Ok(())
}

fn cursor_keyword_at(cursor: &Parser<'_, '_>, index: usize, keyword: &str) -> bool {
    cursor
        .tokens()
        .get(index)
        .is_some_and(|token| is_keyword(cursor.source(), token, keyword))
}

fn cursor_at_group_by_start(cursor: &Parser<'_, '_>) -> bool {
    cursor.at_keyword("GROUP")
        && cursor_keyword_at(
            cursor,
            skip_trivia(cursor.tokens(), cursor.index() + 1),
            "BY",
        )
}

fn cursor_at_loop_at_group_start(cursor: &Parser<'_, '_>) -> bool {
    cursor.at_keyword("GROUP")
        && !cursor_keyword_at(
            cursor,
            skip_trivia(cursor.tokens(), cursor.index() + 1),
            "BY",
        )
}

fn cursor_at_reference_into_start(cursor: &Parser<'_, '_>) -> bool {
    cursor.at_keyword("REFERENCE")
        && cursor_keyword_at(
            cursor,
            skip_trivia(cursor.tokens(), cursor.index() + 1),
            "INTO",
        )
}

fn cursor_at_loop_clause_start(cursor: &Parser<'_, '_>) -> bool {
    LOOP_CLAUSE_BOUNDARY_KEYWORDS.iter().any(|keyword| {
        if keyword.eq_ignore_ascii_case("GROUP") {
            cursor_at_group_by_start(cursor)
        } else {
            cursor.at_keyword(keyword)
        }
    })
}

fn cursor_at_loop_tail_keyword(cursor: &Parser<'_, '_>) -> bool {
    cursor.at_keyword("ASCENDING")
        || cursor.at_keyword("DESCENDING")
        || cursor.at_keyword("WITHOUT")
}

fn loop_header_should_stop(cursor: &Parser<'_, '_>) -> bool {
    let Some(token) = cursor.current() else {
        return true;
    };
    token.kind == TokenKind::Period
        || token.kind == TokenKind::Eof
        || cursor.at_keyword("ENDLOOP")
        || (token_begins_line(token)
            && !cursor_at_loop_clause_start(cursor)
            && !cursor_at_loop_tail_keyword(cursor))
}

fn try_parse_loop_inline_data_target_from_cursor(cursor: &mut Parser<'_, '_>) -> Option<NodeId> {
    let start = cursor.index();
    let parsed = {
        let (b, source, tokens, _) = cursor.parts_mut();
        try_parse_loop_inline_data_target(b, source, tokens, start)
    };
    let (node, next) = parsed?;
    cursor.set_position(next, next.checked_sub(1));
    Some(node)
}

fn try_parse_loop_inline_field_symbol_target_from_cursor(
    cursor: &mut Parser<'_, '_>,
) -> Option<NodeId> {
    let start = cursor.index();
    let parsed = {
        let (b, source, tokens, _) = cursor.parts_mut();
        try_parse_loop_inline_field_symbol_target(b, source, tokens, start)
    };
    let (node, next) = parsed?;
    cursor.set_position(next, next.checked_sub(1));
    Some(node)
}

fn try_parse_catch_inline_data_target_from_cursor(cursor: &mut Parser<'_, '_>) -> Option<NodeId> {
    try_parse_loop_inline_data_target_from_cursor(cursor)
}

fn try_parse_catch_type_ref(cursor: &mut Parser<'_, '_>) -> Option<NodeId> {
    let start = cursor.index();
    let end = scan_catch_type_ref_end(cursor.tokens(), start);
    if end <= start {
        return None;
    }
    let node = {
        let (b, source, tokens, _) = cursor.parts_mut();
        build_type_ref_node(b, source, &tokens[start..end])
    };
    cursor.set_position(end, end.checked_sub(1));
    Some(node)
}

fn parse_loop_expr_clause(
    cursor: &mut Parser<'_, '_>,
    kind: SyntaxKind,
    keyword_indices: &[usize],
    logical: bool,
    after: &str,
) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = keyword_indices
        .iter()
        .filter_map(|&index| token_leaf_at(cursor, index))
        .collect::<Vec<_>>();
    children.push(expect_loop_expr_result(cursor, logical, after)?);
    Ok(cursor.branch_from_children(kind, &children, fallback))
}

fn parse_loop_target_clause(
    cursor: &mut Parser<'_, '_>,
    kind: SyntaxKind,
    keyword_indices: &[usize],
    after: &str,
) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = keyword_indices
        .iter()
        .filter_map(|&index| token_leaf_at(cursor, index))
        .collect::<Vec<_>>();
    let inline = if kind == SyntaxKind::LoopIntoClause {
        try_parse_loop_inline_data_target_from_cursor(cursor)
    } else if kind == SyntaxKind::LoopAssigningClause {
        try_parse_loop_inline_field_symbol_target_from_cursor(cursor)
    } else {
        None
    };
    children.push(match inline {
        Some(inline) => inline,
        None => expect_loop_target_result(cursor, after)?,
    });
    Ok(cursor.branch_from_children(kind, &children, fallback))
}

fn parse_loop_group_by_clause_from_cursor(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();
    children.push(cursor.expect_keyword_result("GROUP")?);
    children.push(cursor.expect_keyword_after_result("BY", "GROUP")?);
    if cursor
        .current()
        .is_some_and(|token| token.kind == TokenKind::LParen)
    {
        push_loop_group_by_structured_key(cursor, &mut children);
    } else {
        children.push(expect_loop_expr_result(cursor, false, "GROUP BY")?);
    }
    Ok(cursor.branch_from_children(SyntaxKind::LoopGroupByClause, &children, fallback))
}

fn push_loop_group_by_structured_key(cursor: &mut Parser<'_, '_>, children: &mut Vec<NodeId>) {
    let mut depth = 0usize;
    loop {
        let Some(token) = cursor.current() else {
            break;
        };
        if token.kind == TokenKind::Eof || token.kind == TokenKind::Period {
            break;
        }
        if depth == 0 && cursor_at_loop_clause_start(cursor) {
            break;
        }

        let closes_outer = token.kind == TokenKind::RParen && depth == 1;
        match token.kind {
            TokenKind::LParen => depth += 1,
            TokenKind::RParen => depth = depth.saturating_sub(1),
            _ => {}
        }
        if let Some(node) = cursor.bump() {
            children.push(node);
        } else {
            break;
        }
        if closes_outer {
            break;
        }
    }
}

fn parse_loop_at_group_clause(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();
    children.push(cursor.expect_keyword_result("GROUP")?);
    children.push(expect_loop_expr_result(cursor, false, "GROUP")?);
    Ok(cursor.branch_from_children(SyntaxKind::LoopAtGroupClause, &children, fallback))
}

fn parse_loop_source_clause(cursor: &mut Parser<'_, '_>, at_index: usize) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();
    if let Some(at_leaf) = token_leaf_at(cursor, at_index) {
        children.push(at_leaf);
    }
    children.push(expect_expr_result(
        cursor,
        false,
        "LOOP AT",
        "syntax error: expected loop source after LOOP AT".to_string(),
        LOOP_CLAUSE_BOUNDARY_KEYWORDS,
    )?);
    Ok(cursor.branch_from_children(SyntaxKind::LoopSourceClause, &children, fallback))
}

fn parse_loop_header_result(
    cursor: &mut Parser<'_, '_>,
    children: &mut Vec<NodeId>,
    at_index: usize,
) -> PResult<()> {
    if cursor_at_loop_at_group_start(cursor) {
        children.push(parse_loop_at_group_clause(cursor)?);
    } else {
        children.push(parse_loop_source_clause(cursor, at_index)?);
    }

    while !loop_header_should_stop(cursor) {
        cursor.skip_trivia();
        if cursor_at_group_by_start(cursor) {
            children.push(parse_loop_group_by_clause_from_cursor(cursor)?);
            continue;
        }
        if cursor.at_keyword("INTO") {
            let into_idx = cursor.index();
            cursor.expect_keyword_result("INTO")?;
            children.push(parse_loop_target_clause(
                cursor,
                SyntaxKind::LoopIntoClause,
                &[into_idx],
                "INTO",
            )?);
            continue;
        }
        if cursor.at_keyword("ASSIGNING") {
            let assigning_idx = cursor.index();
            cursor.expect_keyword_result("ASSIGNING")?;
            children.push(parse_loop_target_clause(
                cursor,
                SyntaxKind::LoopAssigningClause,
                &[assigning_idx],
                "ASSIGNING",
            )?);
            continue;
        }
        if cursor_at_reference_into_start(cursor) {
            let reference_idx = cursor.index();
            cursor.expect_keyword_result("REFERENCE")?;
            let into_idx = cursor.index();
            cursor.expect_keyword_result("INTO")?;
            children.push(parse_loop_target_clause(
                cursor,
                SyntaxKind::LoopReferenceIntoClause,
                &[reference_idx, into_idx],
                "REFERENCE INTO",
            )?);
            continue;
        }
        let clause = if cursor.at_keyword("WHERE") {
            Some((SyntaxKind::LoopWhereClause, true, "WHERE"))
        } else if cursor.at_keyword("FROM") {
            Some((SyntaxKind::LoopFromClause, false, "FROM"))
        } else if cursor.at_keyword("TO") {
            Some((SyntaxKind::LoopToClause, false, "TO"))
        } else if cursor.at_keyword("STEP") {
            Some((SyntaxKind::LoopStepClause, false, "STEP"))
        } else {
            None
        };
        if let Some((kind, logical, after)) = clause {
            let keyword_idx = cursor.index();
            cursor.expect_keyword_result(after)?;
            children.push(parse_loop_expr_clause(
                cursor,
                kind,
                &[keyword_idx],
                logical,
                after,
            )?);
            continue;
        }
        if let Some(node) = cursor.bump() {
            children.push(node);
        } else {
            break;
        }
    }

    children.push(cursor.expect_token_after_result(TokenKind::Period, "LOOP header")?);
    Ok(())
}

pub(crate) fn parse_while_stmt_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    cursor.skip_trivia();
    let fallback = cursor.current_range();
    let mut children = Vec::new();
    children.push(cursor.expect_keyword_result("WHILE")?);
    children.push(expect_control_condition_result(cursor, "WHILE")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "WHILE condition")?);
    children.extend(cursor.parse_stmt_list_until(WHILE_BOUNDARY_KEYWORDS));
    children.push(cursor.expect_keyword_result("ENDWHILE")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "ENDWHILE")?);
    Ok(cursor.branch_from_children(SyntaxKind::WhileStmt, &children, fallback))
}

/// `DO .` or `DO <arith> TIMES .`; parses the repetition count as an expression.
pub(crate) fn parse_do_stmt_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    cursor.skip_trivia();
    let fallback = cursor.current_range();
    let mut children = Vec::new();
    children.push(cursor.expect_keyword_result("DO")?);
    cursor.skip_trivia();
    if !cursor
        .current()
        .is_some_and(|token| token.kind == TokenKind::Period)
    {
        if cursor.at_keyword("TIMES") {
            return Err(failure(
                cursor,
                "syntax error: expected repetition count before TIMES",
            ));
        } else {
            children.push(expect_expr_result(
                cursor,
                false,
                "DO",
                "syntax error: expected repetition count before TIMES".to_string(),
                DO_BOUNDARY_KEYWORDS,
            )?);
        }
        children.push(cursor.expect_keyword_after_result("TIMES", "DO repetition count")?);
    }
    children.push(cursor.expect_token_after_result(TokenKind::Period, "DO header")?);
    children.extend(cursor.parse_stmt_list_until(DO_BOUNDARY_KEYWORDS));
    children.push(cursor.expect_keyword_result("ENDDO")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "ENDDO")?);
    Ok(cursor.branch_from_children(SyntaxKind::DoStmt, &children, fallback))
}

pub(crate) fn parse_at_stmt_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();
    children.push(cursor.expect_keyword_result("AT")?);
    cursor.skip_trivia();
    if cursor.at_keyword("FIRST") || cursor.at_keyword("LAST") {
        let keyword = if cursor.at_keyword("FIRST") {
            "FIRST"
        } else {
            "LAST"
        };
        children.push(cursor.expect_keyword_result(keyword)?);
    } else if cursor.at_keyword("NEW") {
        children.push(cursor.expect_keyword_result("NEW")?);
        push_raw_arithmetic_expr_result(
            cursor,
            &mut children,
            "AT NEW",
            "syntax error: expected group key after AT NEW",
            AT_BOUNDARY_KEYWORDS,
        )?;
    } else {
        children.push(cursor.expect_keyword_result("END")?);
        children.push(cursor.expect_keyword_after_result("OF", "AT END")?);
        push_raw_arithmetic_expr_result(
            cursor,
            &mut children,
            "AT END OF",
            "syntax error: expected group key after AT END OF",
            AT_BOUNDARY_KEYWORDS,
        )?;
    }
    children.push(cursor.expect_token_after_result(TokenKind::Period, "AT header")?);
    children.extend(cursor.parse_stmt_list_until(AT_BOUNDARY_KEYWORDS));
    children.push(cursor.expect_keyword_result("ENDAT")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "ENDAT")?);
    Ok(cursor.branch_from_children(SyntaxKind::AtStmt, &children, fallback))
}

pub(crate) fn parse_loop_stmt_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    cursor.skip_trivia();
    let at_index = skip_trivia(cursor.tokens(), cursor.index() + 1);
    let fallback = cursor.current_range();
    let mut children = Vec::new();
    children.push(cursor.expect_keyword_result("LOOP")?);
    children.push(cursor.expect_keyword_after_result("AT", "LOOP")?);
    parse_loop_header_result(cursor, &mut children, at_index)?;
    children.extend(cursor.parse_stmt_list_until(LOOP_BOUNDARY_KEYWORDS));
    children.push(cursor.expect_keyword_result("ENDLOOP")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "ENDLOOP")?);
    Ok(cursor.branch_from_children(SyntaxKind::LoopStmt, &children, fallback))
}

fn parse_when_clause_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();

    children.push(cursor.expect_keyword_result("WHEN")?);
    cursor.skip_trivia();
    if cursor.at_keyword("OTHERS") {
        children.push(cursor.expect_keyword_result("OTHERS")?);
    } else {
        children.push(parse_when_operand_result(cursor)?);
        loop {
            cursor.skip_trivia();
            if !cursor.at_keyword("OR") {
                break;
            }
            children.push(cursor.expect_keyword_result("OR")?);
            children.push(parse_when_operand_result(cursor)?);
        }
    }
    children.push(cursor.expect_token_result(TokenKind::Period)?);
    children.extend(cursor.parse_stmt_list_until(CASE_BOUNDARY_KEYWORDS));
    Ok(cursor.branch_from_children(SyntaxKind::WhenClause, &children, fallback))
}

fn parse_case_type_when_clause_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();

    children.push(cursor.expect_keyword_result("WHEN")?);
    cursor.skip_trivia();
    if cursor.at_keyword("OTHERS") {
        children.push(cursor.expect_keyword_result("OTHERS")?);
    } else {
        children.push(cursor.expect_keyword_after_result("TYPE", "WHEN")?);
        children.push(
            crate::type_ref::parse_type_ref_from_cursor(cursor, CASE_TYPE_REF_STOP_KEYWORDS)
                .ok_or_else(|| failure(cursor, "syntax error: expected type after WHEN TYPE"))?,
        );
        cursor.skip_trivia();
        if cursor.at_keyword("INTO") {
            children.push(cursor.expect_keyword_result("INTO")?);
            if let Some(inline) = try_parse_loop_inline_data_target_from_cursor(cursor) {
                children.push(inline);
            } else {
                children.push(expect_expr_result(
                    cursor,
                    false,
                    "INTO",
                    "syntax error: expected target after INTO",
                    CASE_BOUNDARY_KEYWORDS,
                )?);
            }
        }
    }
    children.push(cursor.expect_token_result(TokenKind::Period)?);
    children.extend(cursor.parse_stmt_list_until(CASE_BOUNDARY_KEYWORDS));
    Ok(cursor.branch_from_children(SyntaxKind::WhenClause, &children, fallback))
}

fn parse_case_when_clauses(
    cursor: &mut Parser<'_, '_>,
    children: &mut Vec<NodeId>,
    parse_when: fn(&mut Parser<'_, '_>) -> PResult<NodeId>,
) {
    loop {
        cursor.skip_trivia();
        if !cursor.at_keyword("WHEN") {
            break;
        }
        let mark = cursor.mark_stmt();
        match parse_when(cursor) {
            Ok(when_clause) => children.push(when_clause),
            Err(failure) => {
                cursor.push_failure(failure);
                if !cursor.consumed_significant_since(mark) {
                    cursor.skip_trivia();
                    if cursor
                        .current()
                        .is_some_and(|token| token.kind != TokenKind::Eof)
                    {
                        cursor.bump();
                    }
                }
                children.push(cursor.invalid_stmt_from_mark(mark));
                children.extend(cursor.parse_stmt_list_until(CASE_BOUNDARY_KEYWORDS));
            }
        }
    }
}

fn case_type_of_starts(cursor: &Parser<'_, '_>) -> bool {
    let type_index = skip_trivia(cursor.tokens(), cursor.index() + 1);
    let of_index = skip_trivia(cursor.tokens(), type_index + 1);
    cursor
        .tokens()
        .get(type_index)
        .is_some_and(|token| is_keyword(cursor.source(), token, "TYPE"))
        && cursor
            .tokens()
            .get(of_index)
            .is_some_and(|token| is_keyword(cursor.source(), token, "OF"))
}

fn parse_case_type_stmt_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();

    children.push(cursor.expect_keyword_result("CASE")?);
    children.push(cursor.expect_keyword_after_result("TYPE", "CASE")?);
    children.push(cursor.expect_keyword_after_result("OF", "CASE TYPE")?);
    children.push(cursor.expect_arithmetic_expr_result("CASE TYPE OF")?);
    children.push(cursor.expect_token_result(TokenKind::Period)?);
    parse_case_when_clauses(cursor, &mut children, parse_case_type_when_clause_result);
    children.push(cursor.expect_keyword_result("ENDCASE")?);
    children.push(cursor.expect_token_result(TokenKind::Period)?);
    Ok(cursor.branch_from_children(SyntaxKind::CaseStmt, &children, fallback))
}

pub(crate) fn parse_case_stmt_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    cursor.skip_trivia();
    if case_type_of_starts(cursor) {
        return parse_case_type_stmt_result(cursor);
    }
    let fallback = cursor.current_range();
    let mut children = Vec::new();

    children.push(cursor.expect_keyword_result("CASE")?);
    children.push(cursor.expect_arithmetic_expr_result("CASE")?);
    children.push(cursor.expect_token_result(TokenKind::Period)?);
    parse_case_when_clauses(cursor, &mut children, parse_when_clause_result);

    children.push(cursor.expect_keyword_result("ENDCASE")?);
    children.push(cursor.expect_token_result(TokenKind::Period)?);
    Ok(cursor.branch_from_children(SyntaxKind::CaseStmt, &children, fallback))
}

fn parse_when_operand_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    cursor.skip_trivia();
    let start = cursor.index();
    let end = scan_when_operand_end(cursor, start);
    if end == start {
        return Err(failure(
            cursor,
            "syntax error: expected expression after WHEN",
        ));
    }
    if let Some(invalid) = invalid_when_operand_token(cursor, start, end) {
        let stop = cursor.tokens().get(end).map(|token| token.kind);
        let next = if stop == Some(TokenKind::Period) {
            end + 1
        } else {
            end
        };
        cursor.set_position(next, next.checked_sub(1));
        let first = first_significant_token(cursor.tokens(), start, end).unwrap_or(invalid);
        let message = if invalid == first {
            "syntax error: expected expression after WHEN"
        } else {
            "syntax error: invalid operand after WHEN"
        };
        return Err(positioned_failure(cursor, invalid, message));
    }
    let node = {
        let previous = cursor.previous_index();
        let (builder, source, tokens, _) = cursor.parts_mut();
        let prev = previous.and_then(|index| tokens.get(index));
        crate::expr::parse_arithmetic_expr(builder, source, &tokens[start..end], prev)
    };
    cursor.set_position(end, end.checked_sub(1));
    Ok(node)
}

fn scan_when_operand_end(cursor: &Parser<'_, '_>, start: usize) -> usize {
    let mut idx = start;
    let mut paren = 0usize;
    let mut bracket = 0usize;
    let mut brace = 0usize;

    while let Some(token) = cursor.tokens().get(idx) {
        if token.kind == TokenKind::Eof {
            break;
        }
        let top = paren == 0 && bracket == 0 && brace == 0;
        if top {
            if token.kind == TokenKind::Period
                || is_keyword(cursor.source(), token, "OR")
                || is_keyword(cursor.source(), token, "WHEN")
                || is_keyword(cursor.source(), token, "ENDCASE")
            {
                break;
            }
            if idx > start
                && token.kind == TokenKind::Ident
                && (is_definite_stmt_lead_keyword(cursor.source(), token)
                    || token_begins_line(token))
            {
                break;
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
        idx += 1;
    }
    idx
}

fn first_significant_token(tokens: &[Token], start: usize, end: usize) -> Option<usize> {
    (start..end).find(|index| tokens[*index].kind != TokenKind::Comment)
}

fn invalid_when_operand_token(cursor: &Parser<'_, '_>, start: usize, end: usize) -> Option<usize> {
    let mut paren = 0usize;
    let mut bracket = 0usize;
    let mut brace = 0usize;
    let mut seen_operand_token = false;

    for idx in start..end {
        let token = &cursor.tokens()[idx];
        if token.kind == TokenKind::Comment {
            continue;
        }
        let top = paren == 0 && bracket == 0 && brace == 0;
        if top {
            match token.kind {
                TokenKind::Eq
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::Le
                | TokenKind::Ge
                | TokenKind::Ne
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Ampersand
                | TokenKind::LBracket => return Some(idx),
                TokenKind::Plus => {
                    if seen_operand_token {
                        return Some(idx);
                    }
                }
                TokenKind::Minus => {
                    if seen_operand_token {
                        return Some(idx);
                    }
                }
                TokenKind::Ident
                    if is_keyword(cursor.source(), token, "AND")
                        || is_keyword(cursor.source(), token, "BETWEEN")
                        || is_keyword(cursor.source(), token, "DIV")
                        || is_keyword(cursor.source(), token, "MOD")
                        || is_keyword(cursor.source(), token, "TO") =>
                {
                    return Some(idx);
                }
                _ => {}
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
        seen_operand_token = true;
    }
    None
}

fn parse_catch_into_target_result(
    cursor: &mut Parser<'_, '_>,
    children: &mut Vec<NodeId>,
) -> PResult<()> {
    children.push(cursor.expect_keyword_result("INTO")?);
    if let Some(inline) = try_parse_catch_inline_data_target_from_cursor(cursor) {
        children.push(inline);
    } else {
        children.push(expect_expr_result(
            cursor,
            false,
            "INTO",
            "syntax error: expected target after INTO",
            TRY_BODY_BOUNDARY_KEYWORDS,
        )?);
    }
    Ok(())
}

fn catch_header_should_stop_after_exception(cursor: &Parser<'_, '_>) -> bool {
    let Some(token) = cursor.current() else {
        return true;
    };
    token.kind == TokenKind::Eof
        || cursor.at_keyword("CATCH")
        || cursor.at_keyword("CLEANUP")
        || cursor.at_keyword("ENDTRY")
        || (token.kind == TokenKind::Ident
            && token_begins_line(token)
            && (is_definite_stmt_lead_keyword(cursor.source(), token)
                || line_start_assignment(cursor.tokens(), cursor.index())))
}

fn parse_catch_header_result(
    cursor: &mut Parser<'_, '_>,
    children: &mut Vec<NodeId>,
) -> PResult<()> {
    children.push(cursor.expect_keyword_result("CATCH")?);
    let mut saw_exception = false;

    loop {
        cursor.skip_trivia();
        if cursor
            .current()
            .is_none_or(|token| matches!(token.kind, TokenKind::Period | TokenKind::Eof))
            || cursor.at_keyword("CATCH")
            || cursor.at_keyword("CLEANUP")
            || cursor.at_keyword("ENDTRY")
        {
            break;
        }
        if saw_exception && catch_header_should_stop_after_exception(cursor) {
            break;
        }
        if cursor.at_keyword("INTO") {
            if !saw_exception {
                return Err(failure(
                    cursor,
                    "syntax error: expected exception class after CATCH",
                ));
            }
            parse_catch_into_target_result(cursor, children)?;
            break;
        }
        if cursor.at_keyword("BEFORE") || cursor.at_keyword("UNWIND") {
            children.push(cursor.bump().expect("current token exists"));
            continue;
        }
        if let Some(type_ref) = try_parse_catch_type_ref(cursor) {
            saw_exception = true;
            children.push(type_ref);
            continue;
        }
        if !saw_exception {
            return Err(failure(
                cursor,
                "syntax error: expected exception class after CATCH",
            ));
        }
        children.push(cursor.bump().expect("current token exists"));
    }

    if !saw_exception {
        return Err(failure(
            cursor,
            "syntax error: expected exception class after CATCH",
        ));
    }
    children.push(cursor.expect_token_after_result(TokenKind::Period, "CATCH clause")?);
    Ok(())
}

fn parse_catch_clause_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();
    parse_catch_header_result(cursor, &mut children)?;
    children.extend(cursor.parse_stmt_list_until(TRY_BODY_BOUNDARY_KEYWORDS));
    Ok(cursor.branch_from_children(SyntaxKind::CatchClause, &children, fallback))
}

fn parse_cleanup_clause_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();
    children.push(cursor.expect_keyword_result("CLEANUP")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "CLEANUP")?);
    children.extend(cursor.parse_stmt_list_until(CLEANUP_BOUNDARY_KEYWORDS));
    Ok(cursor.branch_from_children(SyntaxKind::CleanupClause, &children, fallback))
}

pub(crate) fn parse_try_stmt_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    cursor.skip_trivia();
    let fallback = cursor.current_range();
    let mut children = Vec::new();
    children.push(cursor.expect_keyword_result("TRY")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "TRY")?);
    children.extend(cursor.parse_stmt_list_until(TRY_BODY_BOUNDARY_KEYWORDS));

    while {
        cursor.skip_trivia();
        cursor.at_keyword("CATCH")
    } {
        children.push(parse_catch_clause_result(cursor)?);
    }
    cursor.skip_trivia();
    if cursor.at_keyword("CLEANUP") {
        children.push(parse_cleanup_clause_result(cursor)?);
    }
    children.push(cursor.expect_keyword_result("ENDTRY")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "ENDTRY")?);
    Ok(cursor.branch_from_children(SyntaxKind::TryStmt, &children, fallback))
}

fn consume_raw_header_until_period(
    cursor: &mut Parser<'_, '_>,
    stop_keywords: &[&str],
    children: &mut Vec<NodeId>,
) {
    loop {
        cursor.skip_trivia();
        let Some(token) = cursor.current() else {
            break;
        };
        if token.kind == TokenKind::Period
            || token.kind == TokenKind::Eof
            || stop_keywords
                .iter()
                .any(|keyword| cursor.at_keyword(keyword))
        {
            break;
        }
        children.push(cursor.bump().expect("current token exists"));
    }
}

pub(crate) fn parse_catch_system_exceptions_stmt_result(
    cursor: &mut Parser<'_, '_>,
) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();
    children.push(cursor.expect_keyword_result("CATCH")?);
    children.push(cursor.expect_keyword_after_result("SYSTEM", "CATCH")?);
    children.push(cursor.expect_token_after_result(TokenKind::Minus, "CATCH SYSTEM")?);
    children.push(cursor.expect_keyword_after_result("EXCEPTIONS", "CATCH SYSTEM-")?);
    consume_raw_header_until_period(cursor, CATCH_SYSTEM_BOUNDARY_KEYWORDS, &mut children);
    children.push(
        cursor.expect_token_after_result(TokenKind::Period, "CATCH SYSTEM-EXCEPTIONS header")?,
    );
    children.extend(cursor.parse_stmt_list_until(CATCH_SYSTEM_BOUNDARY_KEYWORDS));
    children.push(cursor.expect_keyword_result("ENDCATCH")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "ENDCATCH")?);
    Ok(cursor.branch_from_children(SyntaxKind::CatchSystemExceptionsStmt, &children, fallback))
}

#[cfg(test)]
mod tests {
    use abap_ast::SyntaxKind;

    #[test]
    fn parses_while_loop() {
        let parsed = crate::parse("WHILE lv > 0. lv = lv - 1. ENDWHILE.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::WhileStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            1
        );
    }

    #[test]
    fn parses_case_when() {
        let parsed = crate::parse("CASE lv. WHEN 1. lv = 1. WHEN OTHERS. lv = 2. ENDCASE.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CaseStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::WhenClause),
            2
        );
    }

    #[test]
    fn parses_case_when_or_operands() {
        let parsed = crate::parse("CASE lv. WHEN 1 OR 2. lv = 1. ENDCASE.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::WhenClause),
            1
        );
    }

    #[test]
    fn parses_case_when_validated_operand_forms() {
        let parsed = crate::parse(
            "CASE lv.
               WHEN abs( lv_num ).
               WHEN strlen( lv_text ) OR lines( lt_int ).
               WHEN CONV i( lv_text ).
               WHEN lcl_helper=>get_code( ).
               WHEN OTHERS.
             ENDCASE.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CaseStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WhenClause), 5);
        assert!(parsed.file.count_kind(root, SyntaxKind::CallExpr) >= 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ConstructorExpr), 1);
    }

    #[test]
    fn parses_case_type_of_when_type() {
        let parsed = crate::parse(
            "CASE TYPE OF lo_ref.
               WHEN TYPE lcl_child INTO DATA(lo_child).
                 lv = 1.
               WHEN TYPE zif_any.
                 lv = 2.
               WHEN OTHERS.
                 lv = 3.
             ENDCASE.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CaseStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WhenClause), 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TypeRefSimple), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataInlineDecl), 1);
    }

    #[test]
    fn parses_try_catch_cleanup() {
        let parsed = crate::parse("TRY. a = 1. CATCH cx_root. b = 2. CLEANUP. c = 3. ENDTRY.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::TryStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CatchClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CleanupClause),
            1
        );
    }

    #[test]
    fn parses_catch_type_and_into_target_with_pragma() {
        let parsed = crate::parse(
            "TRY. WRITE 'x'. CATCH cx_sxml_parse_error INTO lo_parse_error ##no_handler. WRITE 'y'. ENDTRY.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TryStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CatchClause), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TypeRefSimple), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ExprIdent), 1);
    }

    #[test]
    fn parses_catch_into_inline_data_target_with_pragma() {
        let parsed = crate::parse(
            "TRY. WRITE 'x'. CATCH cx_root INTO DATA(lo_root) ##catch_all. WRITE lo_root->get_text( ). ENDTRY.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TryStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CatchClause), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TypeRefSimple), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataInlineDecl), 1);
    }

    #[test]
    fn parses_resumable_exception_flow() {
        let parsed = crate::parse(
            "TRY. RAISE RESUMABLE EXCEPTION TYPE cx_demo. CATCH BEFORE UNWIND cx_demo. RESUME. CATCH cx_root. RETRY. ENDTRY.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TryStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CatchClause), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::RaiseStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ResumeStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::RetryStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TypeRefSimple), 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_catch_system_exceptions_block() {
        let parsed =
            crate::parse("CATCH SYSTEM-EXCEPTIONS move_cast_error = 1.\n  lv = 1.\nENDCATCH.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::CatchSystemExceptionsStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AssignStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_catch_system_exceptions_multiline_mappings() {
        let parsed = crate::parse(
            "CATCH SYSTEM-EXCEPTIONS\n  dataset_too_many_files = 6\n  open_dataset_no_authority = 7\n  open_pipe_no_authority = 8\n  dataset_no_pipe = 9.\n  IF lv = 1.\n    lv = 2.\n  ENDIF.\nENDCATCH.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::CatchSystemExceptionsStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::IfStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AssignStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_loop_header_with_multiline_where_condition() {
        let parsed = crate::parse(
            "LOOP AT it_reg_attr TRANSPORTING NO FIELDS\n  WHERE reg_valid_from IS INITIAL OR\n        reg_valid_from = ''.\nENDLOOP.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::LoopStmt),
            1
        );
    }

    #[test]
    fn parses_loop_group_by_and_loop_at_group_clauses() {
        let parsed = crate::parse(
            "LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row>) WHERE status = space GROUP BY <row>-archivekey.\n  LOOP AT GROUP <row> ASSIGNING FIELD-SYMBOL(<member>).\n  ENDLOOP.\nENDLOOP.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::LoopStmt), 2);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::LoopGroupByClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::LoopAtGroupClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::LoopAssigningClause),
            2
        );
    }

    #[test]
    fn parses_structured_loop_group_by_clause() {
        let parsed = crate::parse(
            "LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row>)\n  GROUP BY ( key = <row>-archivekey size = GROUP SIZE )\n  INTO DATA(ls_group).\nENDLOOP.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        let group_by = parsed
            .file
            .find_first_kind(root, SyntaxKind::LoopGroupByClause)
            .expect("GROUP BY clause");
        assert_eq!(parsed.file.count_kind(group_by, SyntaxKind::Error), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::LoopIntoClause), 1);
    }

    #[test]
    fn parses_do_unconditional_and_times() {
        let parsed = crate::parse("DO. EXIT. ENDDO.\nDO 7 TIMES. CONTINUE. ENDDO.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DoStmt), 2);
    }

    #[test]
    fn parses_at_group_processing_blocks_inside_loop() {
        let parsed = crate::parse(
            "LOOP AT itab INTO wa.\n  AT NEW a.\n    x = 1.\n  ENDAT.\n  AT END OF a.\n    y = 2.\n  ENDAT.\n  AT LAST.\n    z = 3.\n  ENDAT.\nENDLOOP.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::LoopStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AtStmt), 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::EndAtStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn parses_do_times_count_as_expression_for_semantics() {
        let parsed = crate::parse("DO lv_max_len TIMES. ENDDO.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert!(
            parsed.file.count_kind(root, SyntaxKind::ExprIdent) >= 1,
            "expected repetition count identifier in structured DO header"
        );
    }

    #[test]
    fn parses_nested_do_enddo() {
        let parsed = crate::parse("DO lv_max TIMES.\nDO 7 TIMES.\na = 1.\nENDDO.\nb = 2.\nENDDO.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DoStmt), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AssignStmt), 2);
    }
}
