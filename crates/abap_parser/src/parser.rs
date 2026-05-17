#![allow(dead_code)]

use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{TextRange, Token, TokenKind};

use crate::ParseError;
use crate::block_helpers::ensure_forward_progress;
use crate::stmt_period::{is_definite_stmt_lead_keyword, line_start_assignment, token_begins_line};

pub(crate) type PResult<T> = Result<T, ParseFailure>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ParseFailure {
    pub(crate) message: String,
    pub(crate) range: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct StmtMark {
    index: usize,
}

pub(crate) struct Parser<'a, 'b> {
    source: &'a str,
    tokens: &'a [Token],
    index: usize,
    previous_index: Option<usize>,
    builder: &'b mut SyntaxTreeBuilder,
    errors: &'b mut Vec<ParseError>,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub(crate) fn new(
        builder: &'b mut SyntaxTreeBuilder,
        source: &'a str,
        tokens: &'a [Token],
        index: usize,
        errors: &'b mut Vec<ParseError>,
    ) -> Self {
        Self {
            source,
            tokens,
            index,
            previous_index: None,
            builder,
            errors,
        }
    }

    pub(crate) fn index(&self) -> usize {
        self.index
    }

    pub(crate) fn current(&self) -> Option<&'a Token> {
        self.tokens.get(self.index)
    }

    pub(crate) fn previous(&self) -> Option<&'a Token> {
        self.previous_index.and_then(|index| self.tokens.get(index))
    }

    pub(crate) fn previous_index(&self) -> Option<usize> {
        self.previous_index
    }

    pub(crate) fn error_count(&self) -> usize {
        self.errors.len()
    }

    pub(crate) fn source(&self) -> &'a str {
        self.source
    }

    pub(crate) fn tokens(&self) -> &'a [Token] {
        self.tokens
    }

    pub(crate) fn builder(&mut self) -> &mut SyntaxTreeBuilder {
        self.builder
    }

    pub(crate) fn span(&self, node: NodeId) -> TextRange {
        self.builder.span(node)
    }

    pub(crate) fn children_range(&self, children: &[NodeId], fallback: TextRange) -> TextRange {
        let Some(first) = children.first() else {
            return fallback;
        };
        let last = *children.last().unwrap_or(first);
        self.span(*first).start..self.span(last).end
    }

    pub(crate) fn branch_from_children(
        &mut self,
        kind: SyntaxKind,
        children: &[NodeId],
        fallback: TextRange,
    ) -> NodeId {
        let range = self.children_range(children, fallback);
        self.builder.branch(kind, range, children)
    }

    pub(crate) fn parts_mut(
        &mut self,
    ) -> (
        &mut SyntaxTreeBuilder,
        &'a str,
        &'a [Token],
        &mut Vec<ParseError>,
    ) {
        (self.builder, self.source, self.tokens, self.errors)
    }

    pub(crate) fn set_position(&mut self, index: usize, previous_index: Option<usize>) {
        self.index = index;
        self.previous_index = previous_index;
    }

    pub(crate) fn push_error(&mut self, message: String, range: TextRange) {
        self.errors.push(ParseError { message, range });
    }

    pub(crate) fn push_failure(&mut self, failure: ParseFailure) {
        self.push_error(failure.message, failure.range);
    }

    pub(crate) fn skip_trivia(&mut self) {
        while self
            .current()
            .is_some_and(|token| token.kind == TokenKind::Comment)
        {
            self.advance();
        }
    }

    pub(crate) fn bump(&mut self) -> Option<NodeId> {
        let index = self.index;
        let token = self.tokens.get(index)?;
        let node = self.builder.token_leaf(
            SyntaxKind::Token,
            token.range.clone(),
            token.index(),
            token.kind,
        );
        self.advance();
        Some(node)
    }

    pub(crate) fn allow_token(&mut self, kind: TokenKind) -> Option<NodeId> {
        self.skip_trivia();
        (self.current().is_some_and(|token| token.kind == kind))
            .then(|| self.bump())
            .flatten()
    }

    pub(crate) fn allow_keyword(&mut self, keyword: &str) -> Option<NodeId> {
        self.skip_trivia();
        self.at_keyword(keyword).then(|| self.bump()).flatten()
    }

    pub(crate) fn expect_token(&mut self, kind: TokenKind) -> NodeId {
        self.expect_token_message(kind, format!("syntax error: expected {}", token_text(kind)))
    }

    pub(crate) fn expect_token_after(&mut self, kind: TokenKind, after: &str) -> NodeId {
        self.expect_token_message(
            kind,
            format!("syntax error: expected {} after {after}", token_text(kind)),
        )
    }

    pub(crate) fn expect_token_result(&mut self, kind: TokenKind) -> PResult<NodeId> {
        self.expect_token_result_message(
            kind,
            format!("syntax error: expected {}", token_text(kind)),
        )
    }

    pub(crate) fn expect_token_after_result(
        &mut self,
        kind: TokenKind,
        after: &str,
    ) -> PResult<NodeId> {
        self.expect_token_result_message(
            kind,
            format!("syntax error: expected {} after {after}", token_text(kind)),
        )
    }

    pub(crate) fn expect_keyword(&mut self, keyword: &str) -> NodeId {
        self.expect_keyword_message(keyword, format!("syntax error: expected {keyword}"))
    }

    pub(crate) fn expect_keyword_after(&mut self, keyword: &str, after: &str) -> NodeId {
        self.expect_keyword_message(
            keyword,
            format!("syntax error: expected {keyword} after {after}"),
        )
    }

    pub(crate) fn expect_keyword_result(&mut self, keyword: &str) -> PResult<NodeId> {
        self.expect_keyword_result_message(keyword, format!("syntax error: expected {keyword}"))
    }

    pub(crate) fn expect_keyword_after_result(
        &mut self,
        keyword: &str,
        after: &str,
    ) -> PResult<NodeId> {
        self.expect_keyword_result_message(
            keyword,
            format!("syntax error: expected {keyword} after {after}"),
        )
    }

    pub(crate) fn mark_stmt(&mut self) -> StmtMark {
        self.skip_trivia();
        StmtMark { index: self.index }
    }

    pub(crate) fn consumed_significant_since(&self, mark: StmtMark) -> bool {
        self.tokens[mark.index..self.index.min(self.tokens.len())]
            .iter()
            .any(|token| !matches!(token.kind, TokenKind::Comment | TokenKind::Eof))
    }

    pub(crate) fn invalid_stmt_from_mark(&mut self, mark: StmtMark) -> NodeId {
        let end = self.index.min(self.tokens.len());
        let mut children = Vec::with_capacity(end.saturating_sub(mark.index));
        for index in mark.index..end {
            let token = &self.tokens[index];
            if token.kind == TokenKind::Eof {
                break;
            }
            children.push(self.builder.token_leaf(
                SyntaxKind::Token,
                token.range.clone(),
                token.index(),
                token.kind,
            ));
        }
        let range = consumed_range(self.tokens, mark.index, end).unwrap_or_else(|| {
            self.tokens.get(mark.index).map_or_else(
                || self.current_range(),
                |token| token.range.start..token.range.start,
            )
        });
        self.builder
            .branch(SyntaxKind::InvalidStmt, range, &children)
    }

    pub(crate) fn bump_until_stmt_boundary(&mut self, stop_keywords: &[&str]) -> Vec<NodeId> {
        self.advance_to_stmt_boundary(stop_keywords, false)
    }

    pub(crate) fn at_stmt_boundary(&self, stop_keywords: &[&str]) -> bool {
        let Some(token) = self.current() else {
            return true;
        };
        let at_top_level_boundary = token.kind == TokenKind::Period || token.kind == TokenKind::Eof;
        at_top_level_boundary
            || self.at_any_keyword(stop_keywords)
            || (token_begins_line(token)
                && (is_definite_stmt_lead_keyword(self.source, token)
                    || line_start_assignment(self.tokens, self.index)))
    }

    fn advance_to_stmt_boundary(
        &mut self,
        stop_keywords: &[&str],
        consume_period: bool,
    ) -> Vec<NodeId> {
        let mut skipped = Vec::new();
        let mut paren_depth = 0usize;
        let mut bracket_depth = 0usize;
        let mut brace_depth = 0usize;

        loop {
            self.skip_trivia();
            let Some(token) = self.current() else {
                break;
            };
            if token.kind == TokenKind::Eof {
                break;
            }
            let at_top = paren_depth == 0 && bracket_depth == 0 && brace_depth == 0;
            if at_top
                && (self.at_any_keyword(stop_keywords)
                    || (token_begins_line(token)
                        && (is_definite_stmt_lead_keyword(self.source, token)
                            || line_start_assignment(self.tokens, self.index))))
            {
                break;
            }

            match token.kind {
                TokenKind::Period if at_top => {
                    if consume_period {
                        if let Some(period) = self.bump() {
                            skipped.push(period);
                        }
                    }
                    break;
                }
                TokenKind::LParen => paren_depth += 1,
                TokenKind::RParen => paren_depth = paren_depth.saturating_sub(1),
                TokenKind::LBracket => bracket_depth += 1,
                TokenKind::RBracket => bracket_depth = bracket_depth.saturating_sub(1),
                TokenKind::LBrace => brace_depth += 1,
                TokenKind::RBrace => brace_depth = brace_depth.saturating_sub(1),
                _ => {}
            }
            if let Some(node) = self.bump() {
                skipped.push(node);
            } else {
                break;
            }
        }

        skipped
    }

    pub(crate) fn expect_arithmetic_expr(&mut self, after: &str) -> NodeId {
        crate::expr::expect_arithmetic_expr_from_cursor(self, after)
    }

    pub(crate) fn expect_logical_expr(&mut self, after: &str) -> NodeId {
        crate::expr::expect_logical_expr_from_cursor(self, after)
    }

    pub(crate) fn expect_arithmetic_expr_result(&mut self, after: &str) -> PResult<NodeId> {
        crate::expr::expect_arithmetic_expr_result_from_cursor(self, after)
    }

    pub(crate) fn expect_logical_expr_result(&mut self, after: &str) -> PResult<NodeId> {
        crate::expr::expect_logical_expr_result_from_cursor(self, after)
    }

    pub(crate) fn expect_condition_result(&mut self, keyword: &str) -> PResult<NodeId> {
        self.expect_logical_expr_result(keyword)
            .map_err(|failure| ParseFailure {
                message: format!("syntax error: expected condition after {keyword}"),
                ..failure
            })
    }

    pub(crate) fn parse_arithmetic_expr(&mut self) -> Option<NodeId> {
        crate::expr::parse_arithmetic_expr_from_cursor(self)
    }

    pub(crate) fn parse_logical_expr(&mut self) -> Option<NodeId> {
        crate::expr::parse_logical_expr_from_cursor(self)
    }

    pub(crate) fn parse_file_level_item(&mut self) -> NodeId {
        self.parse_stmt()
    }

    pub(crate) fn parse_stmt(&mut self) -> NodeId {
        if self
            .current()
            .is_some_and(|token| matches!(token.kind, TokenKind::Comment | TokenKind::Eof))
        {
            return self.bump().expect("current token exists");
        }

        let mark = self.mark_stmt();
        match self.parse_stmt_result() {
            Ok(node) => node,
            Err(failure) => {
                self.push_failure(failure);
                if !self.consumed_significant_since(mark) {
                    self.skip_trivia();
                    if self
                        .current()
                        .is_some_and(|token| token.kind != TokenKind::Eof)
                    {
                        self.bump();
                    }
                }
                self.invalid_stmt_from_mark(mark)
            }
        }
    }

    fn parse_stmt_result(&mut self) -> PResult<NodeId> {
        let idx = self.index;
        let Some(token) = self.current() else {
            return Err(self.unexpected_token_failure());
        };
        let kind = token.kind;

        match kind {
            TokenKind::Eof | TokenKind::Comment => {
                return Ok(self.bump().expect("current token exists"));
            }
            TokenKind::StringTemplate => {
                let (node, next) = crate::syntax::parse_char_string_template(
                    self.source,
                    self.tokens,
                    idx,
                    self.builder,
                );
                self.set_after_parse(next);
                return Ok(node);
            }
            TokenKind::Ampersand => {
                return Ok(self.bump().expect("current token exists"));
            }
            TokenKind::Ident => {
                if let Some(result) = crate::data_decl::parse_decl_result_from_cursor(self) {
                    return result;
                }
                if self.at_keyword("CASE") {
                    return crate::control_stmt::parse_case_stmt_result(self);
                }
                if self.at_keyword("IF") {
                    return crate::if_stmt::parse_if_stmt_result(self);
                }
                if self.at_keyword("WHILE") {
                    return crate::control_stmt::parse_while_stmt_result(self);
                }
                if self.at_keyword("DO") {
                    return crate::control_stmt::parse_do_stmt_result(self);
                }
                if self.at_keyword("LOOP") {
                    return crate::control_stmt::parse_loop_stmt_result(self);
                }
                if self.at_keyword("CATCH")
                    && crate::control_stmt::catch_system_exceptions_stmt_starts(self)
                {
                    return crate::control_stmt::parse_catch_system_exceptions_stmt_result(self);
                }
                if self.at_keyword("TRY") {
                    return crate::control_stmt::parse_try_stmt_result(self);
                }
                if self.at_keyword("AT") && crate::control_stmt::at_stmt_starts(self) {
                    return crate::control_stmt::parse_at_stmt_result(self);
                }
                if let Some(result) = self.try_parse_ident_lead_stmt() {
                    return result;
                }
            }
            _ => {}
        }

        if let Some(result) = crate::assign_stmt::parse_assign_stmt_result_from_cursor(self) {
            return result;
        }
        if let Some(node) = crate::try_parse_stray_block_boundary_error(self) {
            return Ok(node);
        }
        if let Some(result) = crate::simple_stmt::try_parse_simple_stmt_result_from_cursor(self) {
            return result;
        }

        Err(self.unexpected_token_failure())
    }

    fn try_parse_ident_lead_stmt(&mut self) -> Option<PResult<NodeId>> {
        if self.at_keyword("DEFINE") {
            return crate::surface_stmt::try_parse_macro_def_result_from_cursor(self);
        }
        if self.at_keyword("REPORT") || self.at_keyword("PROGRAM") {
            return crate::surface_stmt::try_parse_report_stmt_result_from_cursor(self);
        }
        if self.at_keyword("INCLUDE") {
            return crate::surface_stmt::try_parse_include_stmt_result_from_cursor(self);
        }
        if self.at_keyword("SELECTION") {
            return crate::surface_stmt::try_parse_selection_screen_stmt_result_from_cursor(self);
        }
        if self.at_keyword("TEST") {
            return crate::surface_stmt::try_parse_test_block_stmt_result_from_cursor(self);
        }
        if self.at_keyword("AT")
            || self.at_keyword("INITIALIZATION")
            || self.at_keyword("LOAD")
            || self.at_keyword("START")
            || self.at_keyword("END")
            || self.at_keyword("TOP")
        {
            if let Some(result) =
                crate::surface_stmt::try_parse_event_block_result_from_cursor(self)
            {
                return Some(result);
            }
        }
        if self.at_keyword("FORM") {
            return crate::surface_stmt::try_parse_form_decl_result_from_cursor(self);
        }
        if self.at_keyword("FUNCTION") {
            return crate::surface_stmt::try_parse_function_decl_result_from_cursor(self);
        }
        if self.at_keyword("MODULE") {
            return crate::surface_stmt::try_parse_module_decl_result_from_cursor(self);
        }
        if self.at_keyword("ENHANCEMENT") {
            if let Some(result) =
                crate::surface_stmt::try_parse_enhancement_point_stmt_result_from_cursor(self)
            {
                return Some(result);
            }
            if let Some(result) =
                crate::surface_stmt::try_parse_enhancement_section_stmt_result_from_cursor(self)
            {
                return Some(result);
            }
            if let Some(result) =
                crate::surface_stmt::try_parse_enhancement_stmt_result_from_cursor(self)
            {
                return Some(result);
            }
        }
        if self.at_keyword("CLASS") {
            return crate::surface_stmt::try_parse_class_decl_result_from_cursor(self);
        }
        if self.at_keyword("INTERFACE") {
            return crate::surface_stmt::try_parse_interface_decl_result_from_cursor(self);
        }
        if self.at_keyword("METHOD") {
            return crate::surface_stmt::try_parse_method_decl_result_from_cursor(self);
        }
        if self.at_keyword("EXEC") {
            return crate::surface_stmt::try_parse_exec_sql_stmt_result_from_cursor(self);
        }
        if self.at_keyword("SELECT") {
            return crate::surface_stmt::try_parse_select_stmt_result_from_cursor(self);
        }
        if self.at_keyword("WITH") {
            return crate::surface_stmt::try_parse_with_select_stmt_result_from_cursor(self);
        }
        if self.at_keyword("OPEN") {
            if let Some(result) =
                crate::surface_stmt::try_parse_open_cursor_stmt_result_from_cursor(self)
            {
                return Some(result);
            }
        }
        if self.at_keyword("FETCH") {
            return crate::surface_stmt::try_parse_fetch_cursor_stmt_result_from_cursor(self);
        }
        if self.at_keyword("CLOSE") {
            if let Some(result) =
                crate::surface_stmt::try_parse_close_cursor_stmt_result_from_cursor(self)
            {
                return Some(result);
            }
        }
        if self.at_keyword("OPEN")
            || self.at_keyword("CLOSE")
            || self.at_keyword("DELETE")
            || self.at_keyword("READ")
            || self.at_keyword("TRANSFER")
            || self.at_keyword("GET")
            || self.at_keyword("SET")
            || self.at_keyword("TRUNCATE")
        {
            if let Some(result) =
                crate::surface_stmt::try_parse_dataset_stmt_result_from_cursor(self)
            {
                return Some(result);
            }
        }
        if self.at_keyword("READ") {
            if let Some(result) =
                crate::surface_stmt::try_parse_read_report_stmt_result_from_cursor(self)
            {
                return Some(result);
            }
            return crate::surface_stmt::try_parse_read_table_stmt_result_from_cursor(self);
        }
        if self.at_keyword("AUTHORITY") {
            return crate::surface_stmt::try_parse_authority_check_stmt_result_from_cursor(self);
        }
        if self.at_keyword("APPEND") {
            return crate::surface_stmt::try_parse_append_stmt_result_from_cursor(self);
        }
        if self.at_keyword("INSERT") {
            if let Some(result) =
                crate::surface_stmt::try_parse_insert_report_stmt_result_from_cursor(self)
            {
                return Some(result);
            }
            return crate::surface_stmt::try_parse_insert_table_stmt_result_from_cursor(self);
        }
        if self.at_keyword("MOVE") {
            return crate::surface_stmt::try_parse_move_stmt_result_from_cursor(self);
        }
        if self.at_keyword("SORT") {
            return crate::surface_stmt::try_parse_sort_stmt_result_from_cursor(self);
        }
        if self.at_keyword("MODIFY") {
            return crate::surface_stmt::try_parse_modify_stmt_result_from_cursor(self);
        }
        if self.at_keyword("DELETE") {
            if let Some(result) =
                crate::surface_stmt::try_parse_delete_report_stmt_result_from_cursor(self)
            {
                return Some(result);
            }
            return crate::surface_stmt::try_parse_delete_stmt_result_from_cursor(self);
        }
        if self.at_keyword("SYNTAX") {
            return crate::surface_stmt::try_parse_syntax_check_stmt_result_from_cursor(self);
        }
        if self.at_keyword("UPDATE") {
            return crate::surface_stmt::try_parse_update_stmt_result_from_cursor(self);
        }
        if self.at_keyword("REFRESH") {
            return crate::surface_stmt::try_parse_refresh_stmt_result_from_cursor(self);
        }
        if self.at_keyword("COLLECT") {
            return crate::surface_stmt::try_parse_collect_stmt_result_from_cursor(self);
        }
        if self.at_keyword("FREE") {
            return crate::surface_stmt::try_parse_free_stmt_result_from_cursor(self);
        }
        if self.at_keyword("UNASSIGN") {
            return crate::surface_stmt::try_parse_unassign_stmt_result_from_cursor(self);
        }
        if self.at_keyword("IMPORT") {
            return crate::surface_stmt::try_parse_import_memory_stmt_result_from_cursor(self);
        }
        if self.at_keyword("EXPORT") {
            return crate::surface_stmt::try_parse_export_memory_stmt_result_from_cursor(self);
        }
        if self.at_keyword("WRITE") {
            return crate::surface_stmt::try_parse_write_stmt_result_from_cursor(self);
        }
        if self.at_keyword("SPLIT") {
            return crate::surface_stmt::try_parse_split_stmt_result_from_cursor(self);
        }
        if self.at_keyword("CONCATENATE") {
            return crate::surface_stmt::try_parse_concatenate_stmt_result_from_cursor(self);
        }
        if self.at_keyword("CONDENSE") {
            return crate::surface_stmt::try_parse_condense_stmt_result_from_cursor(self);
        }
        if self.at_keyword("RAISE") {
            return crate::surface_stmt::try_parse_raise_stmt_result_from_cursor(self);
        }
        if self.at_keyword("MESSAGE") {
            return crate::surface_stmt::try_parse_message_stmt_result_from_cursor(self);
        }
        if self.at_keyword("SUBMIT") {
            return crate::surface_stmt::try_parse_submit_stmt_result_from_cursor(self);
        }
        if self.at_keyword("LEAVE") {
            return crate::surface_stmt::try_parse_leave_stmt_result_from_cursor(self);
        }
        if self.at_keyword("ENDAT") {
            return crate::surface_stmt::try_parse_endat_stmt_result_from_cursor(self);
        }
        if self.at_keyword("FIND") {
            return crate::surface_stmt::try_parse_find_stmt_result_from_cursor(self);
        }
        if self.at_keyword("GET") {
            if let Some(result) =
                crate::surface_stmt::try_parse_get_reference_stmt_result_from_cursor(self)
            {
                return Some(result);
            }
            if let Some(result) =
                crate::surface_stmt::try_parse_get_bit_stmt_result_from_cursor(self)
            {
                return Some(result);
            }
            return crate::surface_stmt::try_parse_get_time_stamp_stmt_result_from_cursor(self);
        }
        if self.at_keyword("SET") {
            return crate::surface_stmt::try_parse_set_bit_stmt_result_from_cursor(self);
        }
        if self.at_keyword("ASSIGN") {
            return crate::surface_stmt::try_parse_assign_keyword_stmt_result_from_cursor(self);
        }
        if self.at_keyword("CALL") || self.at_keyword("CREATE") {
            return crate::surface_stmt::try_parse_call_like_stmt_result_from_cursor(self);
        }
        None
    }

    pub(crate) fn parse_stmt_list_until(&mut self, stop_keywords: &[&str]) -> Vec<NodeId> {
        self.parse_stmt_list_until_with(stop_keywords, |_| false)
    }

    pub(crate) fn parse_stmt_list_until_with(
        &mut self,
        stop_keywords: &[&str],
        mut stop: impl FnMut(&Self) -> bool,
    ) -> Vec<NodeId> {
        let mut nodes = Vec::new();
        loop {
            if self.at_stop_or_eof_after_trivia(stop_keywords) || stop(self) {
                break;
            }

            let start = self.index;
            let node = self.parse_stmt();
            nodes.push(node);

            let next = ensure_forward_progress(self.tokens, start, self.index);
            if next != self.index {
                self.set_position(next, next.checked_sub(1));
            }
        }
        nodes
    }

    fn expect_token_message(&mut self, kind: TokenKind, message: String) -> NodeId {
        self.skip_trivia();
        if self.current().is_some_and(|token| token.kind == kind) {
            return self.bump().expect("current token exists");
        }
        self.error_node(message)
    }

    fn expect_keyword_message(&mut self, keyword: &str, message: String) -> NodeId {
        self.skip_trivia();
        if self.at_keyword(keyword) {
            return self.bump().expect("current token exists");
        }
        self.error_node(message)
    }

    fn expect_token_result_message(&mut self, kind: TokenKind, message: String) -> PResult<NodeId> {
        self.skip_trivia();
        if self.current().is_some_and(|token| token.kind == kind) {
            return Ok(self.bump().expect("current token exists"));
        }
        Err(self.failure(message))
    }

    fn expect_keyword_result_message(&mut self, keyword: &str, message: String) -> PResult<NodeId> {
        self.skip_trivia();
        if self.at_keyword(keyword) {
            return Ok(self.bump().expect("current token exists"));
        }
        Err(self.failure(message))
    }

    pub(crate) fn at_keyword(&self, keyword: &str) -> bool {
        self.current().is_some_and(|token| {
            token.kind == TokenKind::Ident
                && token.lexeme(self.source).eq_ignore_ascii_case(keyword)
        })
    }

    fn at_any_keyword(&self, keywords: &[&str]) -> bool {
        keywords.iter().any(|keyword| self.at_keyword(keyword))
    }

    fn at_stop_or_eof_after_trivia(&self, stop_keywords: &[&str]) -> bool {
        let mut index = self.index;
        while self
            .tokens
            .get(index)
            .is_some_and(|token| token.kind == TokenKind::Comment)
        {
            index += 1;
        }
        let Some(token) = self.tokens.get(index) else {
            return true;
        };
        if token.kind == TokenKind::Eof {
            return true;
        }
        token.kind == TokenKind::Ident
            && stop_keywords
                .iter()
                .any(|keyword| token.lexeme(self.source).eq_ignore_ascii_case(keyword))
    }

    fn error_node(&mut self, message: String) -> NodeId {
        let range = self.current_range();
        self.errors.push(ParseError {
            message,
            range: range.clone(),
        });
        if self
            .current()
            .is_some_and(|token| token.kind != TokenKind::Eof)
        {
            let child = self.bump().expect("current token exists");
            self.builder.branch(SyntaxKind::Error, range, &[child])
        } else {
            self.builder.branch(SyntaxKind::Error, range, &[])
        }
    }

    fn failure(&self, message: String) -> ParseFailure {
        ParseFailure {
            message,
            range: self.failure_range(),
        }
    }

    fn unexpected_token_failure(&self) -> ParseFailure {
        ParseFailure {
            message: "syntax error: unexpected token".to_string(),
            range: self.current_range(),
        }
    }

    fn failure_range(&self) -> TextRange {
        let current = self.current_range();
        let Some(previous) = self.previous_significant_range() else {
            return current;
        };
        if previous.end <= current.start {
            let gap = &self.source[previous.end..current.start];
            if gap.chars().all(char::is_whitespace) {
                return previous.end..current.start;
            }
        }
        previous.end.saturating_sub(1)..previous.end
    }

    fn previous_significant_range(&self) -> Option<TextRange> {
        let end = self.index.min(self.tokens.len());
        self.tokens[..end]
            .iter()
            .rfind(|token| !matches!(token.kind, TokenKind::Comment | TokenKind::Eof))
            .map(|token| token.range.clone())
    }

    pub(crate) fn current_range(&self) -> TextRange {
        self.current()
            .or_else(|| self.previous())
            .map_or(0..0, |token| token.range.clone())
    }

    fn advance(&mut self) {
        self.previous_index = Some(self.index);
        self.index += 1;
    }

    fn set_after_parse(&mut self, next: usize) {
        self.index = next;
        self.previous_index = next
            .checked_sub(1)
            .filter(|index| *index < self.tokens.len());
    }
}

fn token_text(kind: TokenKind) -> &'static str {
    match kind {
        TokenKind::Period => "'.'",
        TokenKind::Comma => "','",
        TokenKind::Colon => "':'",
        TokenKind::LParen => "'('",
        TokenKind::RParen => "')'",
        TokenKind::LBrace => "'{'",
        TokenKind::RBrace => "'}'",
        TokenKind::LBracket => "'['",
        TokenKind::RBracket => "']'",
        TokenKind::Eq => "'='",
        TokenKind::Ident => "identifier",
        _ => kind.as_str(),
    }
}

fn consumed_range(tokens: &[Token], start: usize, end: usize) -> Option<TextRange> {
    let mut iter = tokens
        .get(start..end)?
        .iter()
        .filter(|token| token.kind != TokenKind::Eof);
    let first = iter.next()?;
    let end = iter.last().map_or(first.range.end, |token| token.range.end);
    Some(first.range.start..end)
}

#[cfg(test)]
mod tests {
    use abap_ast::arena::SyntaxTreeBuilder;
    use abap_lexer::tokenize;

    use super::*;

    fn parser_for<'a, 'b>(
        builder: &'b mut SyntaxTreeBuilder,
        source: &'a str,
        tokens: &'a [Token],
        errors: &'b mut Vec<ParseError>,
    ) -> Parser<'a, 'b> {
        Parser::new(builder, source, tokens, 0, errors)
    }

    #[test]
    fn skips_comment_tokens_before_keyword() {
        let source = "* comment\nDATA lv.";
        let lexed = tokenize(source);
        let mut builder = SyntaxTreeBuilder::default();
        let mut errors = Vec::new();
        let mut parser = parser_for(&mut builder, source, &lexed.tokens, &mut errors);

        parser.skip_trivia();
        assert!(parser.at_keyword("data"));
        assert!(parser.allow_keyword("DATA").is_some());
        assert!(errors.is_empty());
    }

    #[test]
    fn expect_token_result_mismatch_does_not_advance() {
        let source = "WRITE lv.";
        let lexed = tokenize(source);
        let mut builder = SyntaxTreeBuilder::default();
        let mut errors = Vec::new();
        let mut parser = parser_for(&mut builder, source, &lexed.tokens, &mut errors);

        let failure = parser.expect_token_result(TokenKind::Period).unwrap_err();

        assert_eq!(parser.index(), 0);
        assert!(parser.at_keyword("WRITE"));
        assert_eq!(failure.message, "syntax error: expected '.'");
        assert_eq!(failure.range, 0..5);
        assert!(errors.is_empty());
    }

    #[test]
    fn expect_token_result_match_advances() {
        let source = ". WRITE lv.";
        let lexed = tokenize(source);
        let mut builder = SyntaxTreeBuilder::default();
        let mut errors = Vec::new();
        let mut parser = parser_for(&mut builder, source, &lexed.tokens, &mut errors);

        let period = parser.expect_token_result(TokenKind::Period).unwrap();

        assert_eq!(parser.index(), 1);
        assert_eq!(parser.span(period), 0..1);
        assert!(errors.is_empty());
    }

    #[test]
    fn expect_keyword_result_mismatch_does_not_advance() {
        let source = "* comment\nDATA lv.";
        let lexed = tokenize(source);
        let mut builder = SyntaxTreeBuilder::default();
        let mut errors = Vec::new();
        let mut parser = parser_for(&mut builder, source, &lexed.tokens, &mut errors);

        let failure = parser.expect_keyword_result("WRITE").unwrap_err();

        assert_eq!(parser.index(), 0);
        assert!(parser.at_keyword("DATA"));
        assert_eq!(failure.message, "syntax error: expected WRITE");
        assert_eq!(failure.range, 10..14);
        assert!(errors.is_empty());
    }

    #[test]
    fn expect_keyword_result_match_advances() {
        let source = "* comment\nWRITE lv.";
        let lexed = tokenize(source);
        let mut builder = SyntaxTreeBuilder::default();
        let mut errors = Vec::new();
        let mut parser = parser_for(&mut builder, source, &lexed.tokens, &mut errors);

        let keyword = parser.expect_keyword_result("WRITE").unwrap();

        assert_eq!(parser.index(), 1);
        assert_eq!(parser.span(keyword), 10..15);
        assert!(errors.is_empty());
    }
}
