use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_ast::ast::{
    AliasesStmt, AstNode, AuthorityCheckStmt, CallMethodStmt, CallStmt, CallStmtKind, ClearStmt,
    ConcatenateStmt, ConvertStmt, CreateDataStmt, CreateObjectStmt, DeleteStmt, DescribeStmt,
    EventsStmt, FindStmt, MessageStmt, MethodsStmt, RaiseStmt, ReadTableStmt, ReplaceStmt,
    SplitStmt, SubmitStmt, WaitStmt, WriteStmt,
};
use abap_lexer::TextRange;

use crate::def_map::{
    AssignmentSiteData, CallSiteData, FieldAccess, FieldAccessSegment, FieldTypeRefData,
    FindSiteData, FindWriteTargetData, NamedArgumentTarget, ReferenceKind, RoutineSiteData,
    RoutineSiteKind, SymbolKind, SystemFieldStatementKind, TypeFactData, ValueFlowEdgeData,
    ValueFlowKind, ValueFlowTargetData,
};
use crate::ids::ScopeId;
use crate::scope::Namespace;

use super::emit::RefSink;
use super::{Collector, SyntaxTokenInfo};

pub(super) struct StmtLowering<'ctx, 'a> {
    collector: &'ctx mut Collector<'a>,
}

impl<'a> Collector<'a> {
    pub(super) fn stmt_lowering(&mut self) -> StmtLowering<'_, 'a> {
        StmtLowering { collector: self }
    }
}

impl<'ctx, 'a> StmtLowering<'ctx, 'a> {
    pub(super) fn collect_move_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let stmt_range = self.collector.file.range(node);
        let mut source_expr = None;
        let mut target_expr = None;
        let mut saw_to = false;

        for child in self.collector.file.children(node) {
            if self.collector.file.kind(child) == SyntaxKind::Token {
                if let Some(token) = self.collector.syntax_token_nodes(child).into_iter().next()
                    && token.text.eq_ignore_ascii_case("to")
                {
                    saw_to = true;
                }
                continue;
            }

            if saw_to {
                if target_expr.is_none() {
                    target_expr = Some(child);
                }
            } else if source_expr.is_none() {
                source_expr = Some(child);
            }

            self.collector.walk_node(child, scope);
        }

        if let Some(target_expr) = target_expr {
            let rhs_nodes = source_expr.into_iter().collect::<Vec<_>>();
            self.emit_assignment_site_from_ranges(scope, stmt_range, target_expr, &rhs_nodes);
        }
    }

    fn collect_leave_operand_tokens(&mut self, tail: &[SyntaxTokenInfo], scope: ScopeId) {
        if tail.is_empty() {
            return;
        }

        if Self::tokens_match_keyword_sequence(tail, &["list", "-", "processing"])
            || Self::tokens_match_keyword_sequence(tail, &["program"])
            || Self::tokens_match_keyword_sequence(tail, &["screen"])
        {
            return;
        }

        if Self::tokens_match_keyword_sequence(tail, &["to", "screen"]) {
            self.collector
                .collect_token_expression_refs_infos(&tail[2..], scope, true);
            return;
        }

        if Self::tokens_match_keyword_sequence(tail, &["to", "transaction"]) {
            self.collector
                .collect_token_expression_refs_infos(&tail[2..], scope, true);
            return;
        }

        if Self::tokens_match_keyword_sequence(tail, &["to", "list", "-", "processing"]) {
            if let Some(screen_idx) = tail.windows(4).position(|window| {
                Self::tokens_match_keyword_sequence(window, &["and", "return", "to", "screen"])
            }) {
                let expr_start = screen_idx + 4;
                if expr_start < tail.len() {
                    self.collector.collect_token_expression_refs_infos(
                        &tail[expr_start..],
                        scope,
                        true,
                    );
                }
            }
            return;
        }

        self.collector
            .collect_token_expression_refs_infos(tail, scope, true);
    }

    fn consume_simple_operand_tokens(&self, tokens: &[SyntaxTokenInfo], start: usize) -> usize {
        let Some(token) = tokens.get(start) else {
            return start;
        };
        if token.text.as_ref() == "." {
            return start;
        }

        let mut end = start + 1;
        if matches!(token.text.as_ref(), "@" | "#") && end < tokens.len() {
            end += 1;
        }

        loop {
            let Some(next) = tokens.get(end) else {
                break;
            };
            match next.text.as_ref() {
                "-" | "->" | "=>" | "~" if end + 1 < tokens.len() => {
                    end += 2;
                }
                "[" => {
                    let Some(group_end) = self
                        .collector
                        .find_matching_group_end_infos(tokens, end, "[", "]")
                    else {
                        break;
                    };
                    end = group_end + 1;
                }
                _ => break,
            }
        }

        end
    }

    fn collect_positional_operand_tokens(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        mut idx: usize,
        count: usize,
        scope: ScopeId,
    ) -> usize {
        for _ in 0..count {
            let end = self.consume_simple_operand_tokens(tokens, idx);
            if end <= idx {
                break;
            }
            self.collector
                .collect_token_expression_refs_infos(&tokens[idx..end], scope, true);
            idx = end;
        }
        idx
    }

    fn collect_call_screen_stmt_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        if tokens.len() < 3 || !Self::tokens_match_keyword_sequence(tokens, &["call", "screen"]) {
            return;
        }

        let mut idx = 2usize;
        let clause_idx = tokens
            .iter()
            .position(|token| {
                token.text.as_ref() == "."
                    || token.text.eq_ignore_ascii_case("starting")
                    || token.text.eq_ignore_ascii_case("ending")
            })
            .unwrap_or(tokens.len());
        if idx < clause_idx {
            self.collector.collect_token_expression_refs_infos(
                &tokens[idx..clause_idx],
                scope,
                true,
            );
            idx = clause_idx;
        }

        while idx < tokens.len() {
            if tokens[idx].text.as_ref() == "." {
                break;
            }
            if tokens[idx].text.eq_ignore_ascii_case("starting")
                || tokens[idx].text.eq_ignore_ascii_case("ending")
            {
                idx += 1;
                if tokens
                    .get(idx)
                    .is_some_and(|token| token.text.eq_ignore_ascii_case("at"))
                {
                    idx += 1;
                }
                idx = self.collect_positional_operand_tokens(tokens, idx, 2, scope);
                continue;
            }
            idx += 1;
        }
    }

    fn collect_call_transaction_stmt_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        if tokens.len() < 3
            || !Self::tokens_match_keyword_sequence(tokens, &["call", "transaction"])
        {
            return;
        }

        let mut idx = self.collect_positional_operand_tokens(tokens, 2, 1, scope);
        while idx < tokens.len() {
            if tokens[idx].text.as_ref() == "." {
                break;
            }
            if Self::tokens_match_keyword_sequence(&tokens[idx..], &["using"]) {
                idx = self.collect_positional_operand_tokens(tokens, idx + 1, 1, scope);
                continue;
            }
            if Self::tokens_match_keyword_sequence(&tokens[idx..], &["mode"])
                || Self::tokens_match_keyword_sequence(&tokens[idx..], &["update"])
            {
                idx = self.collect_positional_operand_tokens(tokens, idx + 1, 1, scope);
                continue;
            }
            if Self::tokens_match_keyword_sequence(&tokens[idx..], &["messages", "into"]) {
                idx = self.collect_positional_operand_tokens(tokens, idx + 2, 1, scope);
                continue;
            }
            if Self::tokens_match_keyword_sequence(&tokens[idx..], &["options", "from"]) {
                idx = self.collect_positional_operand_tokens(tokens, idx + 2, 1, scope);
                continue;
            }
            idx += 1;
        }
    }

    fn collect_modify_screen_stmt_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        if tokens.len() < 2 || !Self::tokens_match_keyword_sequence(tokens, &["modify", "screen"]) {
            return;
        }
        let Some(from_idx) = tokens
            .iter()
            .position(|token| token.text.eq_ignore_ascii_case("from"))
        else {
            return;
        };
        if from_idx + 1 < tokens.len() {
            self.collector.collect_token_expression_refs_infos(
                &tokens[from_idx + 1..],
                scope,
                true,
            );
        }
    }

    fn find_top_level_keyword_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
        keywords: &[&str],
    ) -> Option<usize> {
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut idx = start;
        while idx < tokens.len() {
            let token = &tokens[idx];
            match token.text.as_ref() {
                "(" => paren += 1,
                ")" => paren -= 1,
                "[" => bracket += 1,
                "]" => bracket -= 1,
                "{" => brace += 1,
                "}" => brace -= 1,
                _ => {}
            }
            if paren == 0
                && bracket == 0
                && brace == 0
                && keywords
                    .iter()
                    .any(|keyword| token.text.eq_ignore_ascii_case(keyword))
            {
                return Some(idx);
            }
            idx += 1;
        }
        None
    }

    fn collect_token_expression_refs_range(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
        end: usize,
        scope: ScopeId,
    ) {
        if start < end {
            self.collector
                .collect_token_expression_refs_infos(&tokens[start..end], scope, true);
        }
    }

    fn read_table_clause_starts_infos(&self, tokens: &[SyntaxTokenInfo], idx: usize) -> bool {
        let Some(token) = tokens.get(idx) else {
            return false;
        };
        token.kind == abap_lexer::TokenKind::Ident
            && (token.text.eq_ignore_ascii_case("into")
                || token.text.eq_ignore_ascii_case("assigning")
                || token.text.eq_ignore_ascii_case("with")
                || token.text.eq_ignore_ascii_case("index")
                || token.text.eq_ignore_ascii_case("using")
                || token.text.eq_ignore_ascii_case("transporting")
                || token.text.eq_ignore_ascii_case("comparing")
                || token.text.eq_ignore_ascii_case("binary")
                || (token.text.eq_ignore_ascii_case("reference")
                    && tokens
                        .get(idx + 1)
                        .is_some_and(|next| next.text.eq_ignore_ascii_case("into"))))
    }

    fn scan_read_table_key_value_end_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
    ) -> usize {
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut idx = start;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if paren == 0 && bracket == 0 && brace == 0 {
                if token.text.as_ref() == "." || self.read_table_clause_starts_infos(tokens, idx) {
                    break;
                }
                if token.kind == abap_lexer::TokenKind::Ident
                    && tokens
                        .get(idx + 1)
                        .is_some_and(|next| next.text.as_ref() == "=")
                {
                    break;
                }
            }
            match token.text.as_ref() {
                "(" => paren += 1,
                ")" => paren -= 1,
                "[" => bracket += 1,
                "]" => bracket -= 1,
                "{" => brace += 1,
                "}" => brace -= 1,
                _ => {}
            }
            idx += 1;
        }
        idx
    }

    fn read_table_key_field_segments_from_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
    ) -> Option<(Vec<FieldAccessSegment>, usize)> {
        let token = tokens.get(start)?;
        if token.kind != abap_lexer::TokenKind::Ident {
            return None;
        }
        let mut segments = vec![FieldAccessSegment {
            name: Arc::<str>::from(token.text.to_ascii_lowercase()),
            range: token.range.clone(),
        }];
        let mut idx = start + 1;
        while idx + 1 < tokens.len() {
            let sep = &tokens[idx];
            let next = &tokens[idx + 1];
            if sep.text.as_ref() != "-" || next.kind != abap_lexer::TokenKind::Ident {
                break;
            }
            segments.push(FieldAccessSegment {
                name: Arc::<str>::from(next.text.to_ascii_lowercase()),
                range: next.range.clone(),
            });
            idx += 2;
        }
        Some((segments, idx))
    }

    fn collect_read_table_with_key_field_accesses(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
        source_access: &FieldAccess,
    ) {
        let Some(with_idx) = tokens
            .windows(2)
            .position(|window| Self::tokens_match_keyword_sequence(window, &["with", "key"]))
        else {
            return;
        };

        let mut idx = with_idx + 2;
        while idx < tokens.len() {
            if self.read_table_clause_starts_infos(tokens, idx) || tokens[idx].text.as_ref() == "."
            {
                break;
            }
            let Some((mut key_path, eq_idx)) =
                self.read_table_key_field_segments_from_infos(tokens, idx)
            else {
                idx += 1;
                continue;
            };
            if tokens.get(eq_idx).map(|token| token.text.as_ref()) != Some("=") {
                idx += 1;
                continue;
            }

            let mut field_path = source_access.field_path.clone();
            field_path.append(&mut key_path);
            self.collector.emit_field_access(FieldAccess {
                scope,
                base_namespace: source_access.base_namespace,
                base_name: Arc::clone(&source_access.base_name),
                base_range: source_access.base_range.clone(),
                field_path,
                in_type_position: false,
            });

            idx = self.scan_read_table_key_value_end_infos(tokens, eq_idx + 1);
        }
    }

    fn read_table_binary_search_range_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
    ) -> Option<TextRange> {
        tokens.windows(2).find_map(|window| {
            if Self::tokens_match_keyword_sequence(window, &["binary", "search"]) {
                Some(window[0].range.start..window[1].range.end)
            } else {
                None
            }
        })
    }

    fn read_table_key_fields_from_infos(&self, tokens: &[SyntaxTokenInfo]) -> Vec<Arc<str>> {
        let Some(with_idx) = tokens.windows(2).position(|window| {
            Self::tokens_match_keyword_sequence(window, &["with", "key"])
                || Self::tokens_match_keyword_sequence(window, &["table", "key"])
        }) else {
            return Vec::new();
        };

        let mut fields = Vec::new();
        let mut idx = with_idx + 2;
        while idx < tokens.len() {
            if self.read_table_clause_starts_infos(tokens, idx) || tokens[idx].text.as_ref() == "."
            {
                break;
            }
            let Some((segments, eq_idx)) =
                self.read_table_key_field_segments_from_infos(tokens, idx)
            else {
                idx += 1;
                continue;
            };
            if tokens.get(eq_idx).map(|token| token.text.as_ref()) != Some("=") {
                idx += 1;
                continue;
            }
            fields.push(Self::read_table_key_name_from_segments(&segments));
            idx = self.scan_read_table_key_value_end_infos(tokens, eq_idx + 1);
        }
        fields
    }

    fn read_table_key_name_from_segments(segments: &[FieldAccessSegment]) -> Arc<str> {
        Arc::from(
            segments
                .iter()
                .map(|segment| segment.name.as_ref())
                .collect::<Vec<_>>()
                .join("-"),
        )
    }

    fn significant_infos_from_children(&self, children: &[NodeId]) -> Vec<SyntaxTokenInfo> {
        let mut tokens = Vec::new();
        for &child in children {
            for token in self.collector.syntax_token_nodes(child) {
                if !self.collector.syntax_token_is_comment(&token) {
                    tokens.push(token);
                }
            }
        }
        tokens
    }

    fn collect_read_table_entry(&mut self, entry_children: &[NodeId], scope: ScopeId) {
        let data_inline_targets: Vec<_> = entry_children
            .iter()
            .copied()
            .filter(|&child| self.collector.file.kind(child) == SyntaxKind::DataInlineDecl)
            .collect();
        let field_symbol_targets: Vec<_> = entry_children
            .iter()
            .copied()
            .filter(|&child| self.collector.file.kind(child) == SyntaxKind::FieldSymbolInlineDecl)
            .collect();
        let mut source_expr = None;
        let mut target_kind = None;
        let mut named_into_target = None;
        let mut named_field_symbol_target = None;

        for &child in entry_children {
            match self.collector.file.kind(child) {
                SyntaxKind::Token => {
                    if let Some(token) = self.collector.syntax_token_nodes(child).into_iter().next()
                    {
                        if token.text.eq_ignore_ascii_case("into") {
                            target_kind = Some("into");
                        } else if token.text.eq_ignore_ascii_case("assigning") {
                            target_kind = Some("assigning");
                        }
                    }
                }
                SyntaxKind::DataInlineDecl | SyntaxKind::FieldSymbolInlineDecl => {}
                _ => {
                    if source_expr.is_none() {
                        source_expr = Some(child);
                    } else if target_kind == Some("into")
                        && named_into_target.is_none()
                        && self
                            .collector
                            .value_access_from_node(child, scope)
                            .is_some()
                    {
                        named_into_target = Some(child);
                    } else if target_kind == Some("assigning")
                        && named_field_symbol_target.is_none()
                        && let Some(target) = self.direct_field_symbol_target(child, scope)
                    {
                        named_field_symbol_target = Some(target);
                    }
                    self.collector.walk_node(child, scope);
                }
            }
        }

        let inferred_metadata = source_expr
            .map(|expr| {
                self.collector
                    .control_lowering()
                    .loop_source_line_metadata_from_node(expr, scope)
            })
            .unwrap_or((None, None));

        if target_kind == Some("into") {
            let decl_scope = self.collector.declaration_scope(scope);
            for &node in &data_inline_targets {
                if let Some(name_node) = self
                    .collector
                    .file
                    .children(node)
                    .find(|&child| self.collector.file.kind(child) == SyntaxKind::DataDeclName)
                    && let Some((name, range)) = self.collector.node_name(name_node)
                {
                    self.collector.declare_symbol(
                        decl_scope,
                        name,
                        SymbolKind::Variable,
                        range,
                        inferred_metadata.0,
                        inferred_metadata.1.clone(),
                        None,
                        None,
                    );
                }
            }
        }

        if let Some(source_expr) = source_expr {
            self.collector.add_routine_site(RoutineSiteData {
                scope,
                range: self.collector.file.range(source_expr),
                kind: RoutineSiteKind::ReadTable,
                target_range: named_into_target.map(|target| self.collector.file.range(target)),
            });
            if let Some(source_access) = self.collector.value_access_from_node(source_expr, scope) {
                let significant = self.significant_infos_from_children(entry_children);
                if let Some(binary_search_range) =
                    self.read_table_binary_search_range_infos(&significant)
                {
                    let key_fields = self.read_table_key_fields_from_infos(&significant);
                    let table_name = self.collector.table_order_name_from_access(&source_access);
                    self.collector.record_read_table_binary_search(
                        scope,
                        binary_search_range,
                        table_name,
                        key_fields,
                    );
                }
                self.collect_read_table_with_key_field_accesses(
                    &significant,
                    scope,
                    &source_access,
                );
            }
        }

        if target_kind == Some("assigning") {
            for target in field_symbol_targets {
                let target_name = self
                    .collector
                    .file
                    .children(target)
                    .find(|&child| self.collector.file.kind(child) == SyntaxKind::DataDeclName)
                    .and_then(|child| self.collector.node_name(child));
                self.collector
                    .decl_lowering()
                    .declare_inline_field_symbol_decl(
                        target,
                        scope,
                        inferred_metadata.0,
                        inferred_metadata.1.clone(),
                        None,
                    );
                if let Some((target_name, target_range)) = target_name {
                    self.emit_field_symbol_binding_edge(
                        scope,
                        ValueFlowKind::ConditionalFieldSymbolAssignment,
                        source_expr,
                        inferred_metadata.0,
                        inferred_metadata.1.clone(),
                        target_name,
                        target_range,
                    );
                }
            }
            if let Some((target_name, target_range)) = named_field_symbol_target {
                self.emit_field_symbol_binding_edge(
                    scope,
                    ValueFlowKind::ConditionalFieldSymbolAssignment,
                    source_expr,
                    inferred_metadata.0,
                    inferred_metadata.1.clone(),
                    target_name,
                    target_range,
                );
            }
        }
    }

    fn collect_set_pf_status_stmt_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) -> bool {
        if tokens.len() < 6
            || !Self::tokens_match_keyword_sequence(tokens, &["set", "pf", "-", "status"])
        {
            return false;
        }

        let Some(period_idx) = tokens.iter().position(|token| token.text.as_ref() == ".") else {
            return false;
        };
        let status_start = 4usize;
        let of_program_idx = self
            .find_top_level_keyword_infos(tokens, status_start, &["OF"])
            .filter(|&idx| {
                tokens
                    .get(idx + 1)
                    .is_some_and(|token| token.text.eq_ignore_ascii_case("program"))
            });
        let excluding_idx = self.find_top_level_keyword_infos(tokens, status_start, &["EXCLUDING"]);
        let status_end = of_program_idx
            .into_iter()
            .chain(excluding_idx)
            .chain(std::iter::once(period_idx))
            .min()
            .unwrap_or(period_idx);
        self.collect_token_expression_refs_range(tokens, status_start, status_end, scope);

        if let Some(of_idx) = of_program_idx {
            let program_start = of_idx + 2;
            let program_end = excluding_idx
                .filter(|&idx| idx > of_idx)
                .unwrap_or(period_idx);
            self.collect_token_expression_refs_range(tokens, program_start, program_end, scope);
        }

        if let Some(excluding_idx) = excluding_idx {
            self.collect_token_expression_refs_range(tokens, excluding_idx + 1, period_idx, scope);
        }

        true
    }

    fn collect_set_titlebar_stmt_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) -> bool {
        if tokens.len() < 4 || !Self::tokens_match_keyword_sequence(tokens, &["set", "titlebar"]) {
            return false;
        }

        let Some(period_idx) = tokens.iter().position(|token| token.text.as_ref() == ".") else {
            return false;
        };
        let title_start = 2usize;
        let of_program_idx = self
            .find_top_level_keyword_infos(tokens, title_start, &["OF"])
            .filter(|&idx| {
                tokens
                    .get(idx + 1)
                    .is_some_and(|token| token.text.eq_ignore_ascii_case("program"))
            });
        let with_idx = self.find_top_level_keyword_infos(tokens, title_start, &["WITH"]);
        let title_end = of_program_idx
            .into_iter()
            .chain(with_idx)
            .chain(std::iter::once(period_idx))
            .min()
            .unwrap_or(period_idx);
        self.collect_token_expression_refs_range(tokens, title_start, title_end, scope);

        if let Some(of_idx) = of_program_idx {
            let program_start = of_idx + 2;
            let program_end = with_idx.filter(|&idx| idx > of_idx).unwrap_or(period_idx);
            self.collect_token_expression_refs_range(tokens, program_start, program_end, scope);
        }

        if let Some(with_idx) = with_idx {
            let mut idx = with_idx + 1;
            while idx < period_idx {
                let end = self.consume_simple_operand_tokens(tokens, idx);
                if end <= idx {
                    idx += 1;
                    continue;
                }
                self.collect_token_expression_refs_range(tokens, idx, end, scope);
                idx = end;
            }
        }

        true
    }

    fn record_routine_site(
        &mut self,
        scope: ScopeId,
        range: abap_lexer::TextRange,
        kind: RoutineSiteKind,
    ) {
        self.collector.add_routine_site(RoutineSiteData {
            scope,
            range,
            kind,
            target_range: None,
        });
    }

    fn record_unknown_effect(&mut self, node: NodeId, scope: ScopeId) {
        self.record_routine_site(
            scope,
            self.collector.file.range(node),
            RoutineSiteKind::UnknownEffect,
        );
    }

    fn record_system_field_updates(
        &mut self,
        scope: ScopeId,
        node: NodeId,
        statement: SystemFieldStatementKind,
        field_names: &[&'static str],
    ) {
        let range = self.collector.file.range(node);
        for &field_name in field_names {
            self.collector
                .add_system_field_update(scope, range.clone(), statement, field_name);
        }
    }

    fn tokens_match_keyword_sequence(tokens: &[SyntaxTokenInfo], keywords: &[&str]) -> bool {
        tokens.len() >= keywords.len()
            && tokens
                .iter()
                .zip(keywords.iter())
                .all(|(token, keyword)| token.text.eq_ignore_ascii_case(keyword))
    }

    fn is_field_symbol_name(name: &str) -> bool {
        name.starts_with('<') && name.ends_with('>')
    }

    fn direct_field_symbol_target(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> Option<(Arc<str>, abap_lexer::TextRange)> {
        let access = self.collector.value_access_from_node(node, scope)?;
        if access.base_namespace != Namespace::Value
            || !access.field_path.is_empty()
            || !Self::is_field_symbol_name(access.base_name.as_ref())
        {
            return None;
        }
        Some((
            Arc::clone(&access.base_name),
            self.collector.file.range(node),
        ))
    }

    fn emit_field_symbol_binding_edge(
        &mut self,
        scope: ScopeId,
        kind: ValueFlowKind,
        source_expr: Option<NodeId>,
        structure: Option<crate::ids::StructureId>,
        declared_type: Option<FieldTypeRefData>,
        target_name: Arc<str>,
        target_range: abap_lexer::TextRange,
    ) {
        let source_range = source_expr
            .map(|expr| self.collector.file.range(expr))
            .unwrap_or_else(|| target_range.clone());
        let source_type = TypeFactData {
            structure,
            declared_type: declared_type.clone(),
            type_clause_display: None,
            table_line: None,
        };
        self.collector.emit_value_flow_edge(ValueFlowEdgeData {
            scope,
            kind,
            source_range,
            source_type: source_type.clone(),
            target: ValueFlowTargetData::FieldSymbol {
                range: target_range,
                name: Some(target_name),
            },
            target_type: source_type,
        });
    }

    fn emit_assignment_site_from_ranges(
        &mut self,
        scope: ScopeId,
        range: abap_lexer::TextRange,
        lhs: NodeId,
        rhs_nodes: &[NodeId],
    ) {
        self.emit_assignment_site_with_type_facts(
            scope,
            range,
            lhs,
            rhs_nodes,
            None,
            match rhs_nodes {
                [rhs] => Some(self.type_fact_from_assignment_node(*rhs, scope)),
                _ => None,
            },
        );
    }

    fn emit_assignment_site_with_type_facts(
        &mut self,
        scope: ScopeId,
        range: abap_lexer::TextRange,
        lhs: NodeId,
        rhs_nodes: &[NodeId],
        lhs_fact: Option<TypeFactData>,
        rhs_fact: Option<TypeFactData>,
    ) {
        let lhs_range = self.collector.file.range(lhs);
        let rhs_range = rhs_nodes
            .iter()
            .map(|&node| self.collector.file.range(node))
            .reduce(|acc, next| acc.start.min(next.start)..acc.end.max(next.end))
            .unwrap_or_else(|| range.start..range.start);
        let lhs_fact = lhs_fact.unwrap_or_else(|| self.type_fact_from_assignment_node(lhs, scope));
        let rhs_fact = rhs_fact.unwrap_or_default();

        self.collector.emit_assignment_site(AssignmentSiteData {
            scope,
            range,
            lhs_range,
            rhs_range,
            lhs_target_access: self.collector.value_access_from_node(lhs, scope),
            lhs: lhs_fact,
            rhs: rhs_fact,
            rhs_is_top_level_sum: matches!(rhs_nodes, [rhs] if self.collector.rhs_is_top_level_sum(*rhs)),
        });
    }

    fn type_fact_from_assignment_node(&self, node: NodeId, scope: ScopeId) -> TypeFactData {
        let (mut structure, mut declared_type) =
            if self.collector.file.kind(node) == SyntaxKind::DataInlineDecl {
                self.collector.inline_decl_inferred_type(node, scope)
            } else {
                self.collector
                    .inline_decl_assignment_source_metadata(node, scope)
            };
        let mut type_clause_display = None;
        if let Some(access) = self.collector.value_access_from_node(node, scope)
            && access.base_namespace == Namespace::Value
            && access.field_path.is_empty()
            && let Some(symbol_id) = self.collector.lookup_symbol_in_scope_chain(
                scope,
                Namespace::Value,
                access.base_name.as_ref(),
            )
        {
            let symbol = self.collector.symbol(symbol_id);
            structure = structure.or(symbol.structure);
            declared_type = declared_type.or_else(|| symbol.declared_type.clone());
            type_clause_display = symbol.type_clause_display.clone();
        }
        TypeFactData {
            structure,
            declared_type,
            type_clause_display,
            table_line: None,
        }
    }

    fn type_fact_from_table_line_node(&self, node: NodeId, scope: ScopeId) -> TypeFactData {
        let fact = self.type_fact_from_assignment_node(node, scope);
        let (structure, declared_type) = self.collector.internal_table_line_metadata(
            scope,
            fact.structure,
            fact.declared_type.clone(),
        );
        TypeFactData {
            structure,
            declared_type,
            type_clause_display: None,
            table_line: None,
        }
    }

    fn assign_keyword_binding_kind(
        &self,
        node: NodeId,
        scope: ScopeId,
        source_expr: Option<NodeId>,
    ) -> ValueFlowKind {
        let is_component_assign = self.collector.file.children(node).any(|child| {
            self.collector.file.kind(child) == SyntaxKind::Token
                && self
                    .collector
                    .syntax_token_nodes(child)
                    .into_iter()
                    .next()
                    .is_some_and(|token| token.text.eq_ignore_ascii_case("component"))
        });
        if is_component_assign {
            return ValueFlowKind::ConditionalFieldSymbolAssignment;
        }
        let Some(source_expr) = source_expr else {
            return ValueFlowKind::ConditionalFieldSymbolAssignment;
        };
        let Some(access) = self.collector.value_access_from_node(source_expr, scope) else {
            return ValueFlowKind::ConditionalFieldSymbolAssignment;
        };
        if access.base_namespace == Namespace::Value && access.field_path.is_empty() {
            ValueFlowKind::FieldSymbolAssignment
        } else {
            ValueFlowKind::ConditionalFieldSymbolAssignment
        }
    }

    fn collect_log_point_stmt_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) -> bool {
        if tokens.len() < 4
            || !tokens[0].text.eq_ignore_ascii_case("log")
            || tokens[1].text.as_ref() != "-"
            || !tokens[2].text.eq_ignore_ascii_case("point")
        {
            return false;
        }

        let mut idx = 3usize;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if token.text.as_ref() == "." {
                break;
            }

            if token.text.eq_ignore_ascii_case("id") {
                idx += 1;
                if idx < tokens.len() && tokens[idx].text.as_ref() != "." {
                    idx += 1;
                }
                continue;
            }

            if token.text.eq_ignore_ascii_case("subkey") {
                let start = idx + 1;
                let mut end = start;
                while end < tokens.len()
                    && tokens[end].text.as_ref() != "."
                    && !tokens[end].text.eq_ignore_ascii_case("fields")
                {
                    end += 1;
                }
                if start < end {
                    self.collector.collect_token_expression_refs_infos(
                        &tokens[start..end],
                        scope,
                        true,
                    );
                }
                idx = end;
                continue;
            }

            if token.text.eq_ignore_ascii_case("fields") {
                let start = idx + 1;
                let mut end = start;
                while end < tokens.len() && tokens[end].text.as_ref() != "." {
                    end += 1;
                }
                if start < end {
                    self.collector.collect_token_expression_refs_infos(
                        &tokens[start..end],
                        scope,
                        true,
                    );
                }
                return true;
            }

            idx += 1;
        }

        true
    }

    fn delete_stmt_operands(
        &self,
        node: NodeId,
    ) -> (Option<NodeId>, Option<NodeId>, Option<NodeId>) {
        let mut head_expr = None;
        let mut from_expr = None;
        let mut where_expr = None;
        let mut saw_where = false;
        let mut expect_from_expr = false;

        for child in self.collector.file.children(node) {
            if self.collector.file.kind(child) == SyntaxKind::Token {
                if let Some(text) = self.collector.syntax(child).text(self.collector.source) {
                    if text.eq_ignore_ascii_case("from") {
                        saw_where = false;
                        expect_from_expr = true;
                    } else if text.eq_ignore_ascii_case("where") {
                        saw_where = true;
                        expect_from_expr = false;
                    } else if expect_from_expr && text.eq_ignore_ascii_case("table") {
                        continue;
                    }
                }
                continue;
            }

            if head_expr.is_none() {
                head_expr = Some(child);
                if expect_from_expr {
                    expect_from_expr = false;
                }
                continue;
            }
            if expect_from_expr {
                from_expr = Some(child);
                expect_from_expr = false;
                continue;
            }
            if saw_where && where_expr.is_none() {
                where_expr = Some(child);
            }
        }

        (head_expr, from_expr, where_expr)
    }

    fn delete_stmt_starts_with_from(&self, node: NodeId) -> bool {
        let tokens = self.collector.significant_stmt_token_infos(node);
        tokens
            .first()
            .is_some_and(|token| token.text.eq_ignore_ascii_case("delete"))
            && tokens
                .get(1)
                .is_some_and(|token| token.text.eq_ignore_ascii_case("from"))
    }

    fn modify_stmt_operands(
        &self,
        node: NodeId,
    ) -> (bool, bool, Option<NodeId>, Option<NodeId>, Option<NodeId>) {
        let mut saw_table_keyword = false;
        let mut saw_internal_table_clause = false;
        let mut head_expr = None;
        let mut from_expr = None;
        let mut where_expr = None;
        let mut expect_from_expr = false;
        let mut saw_where = false;

        for child in self.collector.file.children(node) {
            if self.collector.file.kind(child) == SyntaxKind::Token {
                if let Some(text) = self.collector.syntax(child).text(self.collector.source) {
                    if head_expr.is_none() && text.eq_ignore_ascii_case("table") {
                        saw_table_keyword = true;
                    } else if text.eq_ignore_ascii_case("from") {
                        expect_from_expr = true;
                        saw_where = false;
                    } else if text.eq_ignore_ascii_case("index")
                        || text.eq_ignore_ascii_case("transporting")
                    {
                        saw_internal_table_clause = true;
                    } else if text.eq_ignore_ascii_case("where") {
                        saw_internal_table_clause = true;
                        saw_where = true;
                        expect_from_expr = false;
                    } else if expect_from_expr && text.eq_ignore_ascii_case("table") {
                        continue;
                    }
                }
                continue;
            }

            if head_expr.is_none() {
                head_expr = Some(child);
                continue;
            }
            if expect_from_expr {
                from_expr = Some(child);
                expect_from_expr = false;
                continue;
            }
            if saw_where && where_expr.is_none() {
                where_expr = Some(child);
            }
        }

        (
            saw_table_keyword,
            saw_internal_table_clause,
            head_expr,
            from_expr,
            where_expr,
        )
    }

    fn modify_transporting_field_paths(&self, node: NodeId) -> Vec<Vec<FieldAccessSegment>> {
        let tokens = self.collector.significant_stmt_token_infos(node);
        let Some(transporting_idx) = tokens
            .iter()
            .position(|token| token.text.eq_ignore_ascii_case("transporting"))
        else {
            return Vec::new();
        };
        let clause_end = tokens[transporting_idx + 1..]
            .iter()
            .position(|token| {
                token.text.eq_ignore_ascii_case("where") || token.text.as_ref() == "."
            })
            .map(|offset| transporting_idx + 1 + offset)
            .unwrap_or(tokens.len());
        let mut idx = transporting_idx + 1;
        let mut out = Vec::new();

        while idx < clause_end {
            let Some(token) = tokens.get(idx) else {
                break;
            };
            if !self.collector.syntax_token_is_ident_like(token) {
                idx += 1;
                continue;
            }

            if let Some((next_idx, namespace, base_name, base_range, mut field_path, _)) = self
                .collector
                .consume_selector_access_from_infos(&tokens[..clause_end], idx)
                && namespace == Namespace::Value
                && next_idx <= clause_end
            {
                let mut components = Vec::with_capacity(1 + field_path.len());
                components.push(FieldAccessSegment {
                    name: base_name,
                    range: base_range,
                });
                components.append(&mut field_path);
                out.push(components);
                idx = next_idx;
                continue;
            }

            out.push(vec![FieldAccessSegment {
                name: Arc::<str>::from(token.text.to_ascii_lowercase()),
                range: token.range.clone(),
            }]);
            idx += 1;
        }

        out
    }

    fn modify_where_clause_tokens(
        &self,
        node: NodeId,
    ) -> Option<(Vec<SyntaxTokenInfo>, abap_lexer::TextRange)> {
        let tokens = self.collector.significant_stmt_token_infos(node);
        let where_idx = tokens
            .iter()
            .position(|token| token.text.eq_ignore_ascii_case("where"))?;
        let clause_end = tokens[where_idx + 1..]
            .iter()
            .position(|token| token.text.as_ref() == ".")
            .map(|offset| where_idx + 1 + offset)
            .unwrap_or(tokens.len());
        if where_idx + 1 >= clause_end {
            return None;
        }
        Some((
            tokens[where_idx + 1..clause_end].to_vec(),
            tokens[where_idx].range.start..tokens[clause_end - 1].range.end,
        ))
    }

    fn collect_modify_where_clause_refs(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        let mut segment_start = 0usize;
        let mut paren_depth = 0usize;
        let mut bracket_depth = 0usize;
        let mut brace_depth = 0usize;

        let flush_segment = |collector: &mut Collector<'a>, start: usize, end: usize| {
            if start < end {
                collector.collect_token_expression_refs_infos(&tokens[start..end], scope, true);
            }
        };

        for (idx, token) in tokens.iter().enumerate() {
            match token.text.as_ref() {
                "(" => paren_depth += 1,
                ")" => paren_depth = paren_depth.saturating_sub(1),
                "[" => bracket_depth += 1,
                "]" => bracket_depth = bracket_depth.saturating_sub(1),
                "{" => brace_depth += 1,
                "}" => brace_depth = brace_depth.saturating_sub(1),
                _ => {}
            }

            if paren_depth == 0
                && bracket_depth == 0
                && brace_depth == 0
                && matches!(token.text.to_ascii_uppercase().as_str(), "AND" | "OR")
            {
                flush_segment(self.collector, segment_start, idx);
                segment_start = idx + 1;
            }
        }

        flush_segment(self.collector, segment_start, tokens.len());
    }

    fn simple_delete_source_name(&self, node: NodeId) -> Option<Arc<str>> {
        let tokens: Vec<_> = self
            .collector
            .syntax_token_nodes(node)
            .into_iter()
            .filter(|token| !self.collector.syntax_token_is_comment(token))
            .collect();
        if tokens.len() != 1 || !self.collector.syntax_token_is_ident_like(&tokens[0]) {
            return None;
        }
        Some(Arc::<str>::from(tokens[0].text.to_ascii_lowercase()))
    }

    fn delete_operand_expr_node(&self, node: NodeId) -> NodeId {
        if self.collector.file.kind(node) == SyntaxKind::TemplateExpr {
            self.collector.first_non_token_child(node).unwrap_or(node)
        } else {
            node
        }
    }

    fn delete_comparing_field_segments_from_expr(
        &self,
        inner: NodeId,
    ) -> Option<Vec<FieldAccessSegment>> {
        match self.collector.file.kind(inner) {
            SyntaxKind::ExprIdent => {
                let (name, range) = self.collector.node_name(inner)?;
                Some(vec![FieldAccessSegment { name, range }])
            }
            SyntaxKind::SelectorExpr => {
                let (namespace, base_name, base_range, mut path) =
                    self.collector.selector_access_chain(inner)?;
                if namespace != Namespace::Value {
                    return None;
                }
                let mut out = vec![FieldAccessSegment {
                    name: base_name,
                    range: base_range,
                }];
                out.append(&mut path);
                Some(out)
            }
            _ => None,
        }
    }

    fn builtin_type(name: &'static str) -> FieldTypeRefData {
        FieldTypeRefData {
            namespace: Namespace::Type,
            is_ref: false,
            base_name: Arc::<str>::from(name),
            field_path: Vec::new(),
        }
    }

    fn declare_split_inline_data_target(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        is_table_target: bool,
        is_byte_mode: bool,
    ) {
        let decl_scope = self.collector.declaration_scope(scope);
        let base_type = if is_byte_mode { "xstring" } else { "string" };
        let declared_type = FieldTypeRefData {
            namespace: Namespace::Type,
            is_ref: false,
            base_name: Arc::<str>::from(base_type),
            field_path: Vec::new(),
        };
        let type_clause_display =
            is_table_target.then(|| Arc::<str>::from(format!("STANDARD TABLE OF {base_type}")));
        if let Some(name_node) = self
            .collector
            .file
            .children(node)
            .find(|&child| self.collector.file.kind(child) == SyntaxKind::DataDeclName)
            && let Some((name, range)) = self.collector.node_name(name_node)
        {
            self.collector.declare_symbol(
                decl_scope,
                name,
                SymbolKind::Variable,
                range,
                None,
                Some(declared_type),
                type_clause_display,
                None,
            );
        }
    }

    fn declare_concatenate_inline_data_target(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        is_byte_mode: bool,
    ) {
        let decl_scope = self.collector.declaration_scope(scope);
        let declared_type = Self::builtin_type(if is_byte_mode { "xstring" } else { "string" });
        if let Some(name_node) = self
            .collector
            .file
            .children(node)
            .find(|&child| self.collector.file.kind(child) == SyntaxKind::DataDeclName)
            && let Some((name, range)) = self.collector.node_name(name_node)
        {
            self.collector.declare_symbol(
                decl_scope,
                name,
                SymbolKind::Variable,
                range,
                None,
                Some(declared_type),
                None,
                None,
            );
        }
    }

    fn declare_describe_lines_inline_target(&mut self, node: NodeId, scope: ScopeId) -> bool {
        let Some(name_node) = self
            .collector
            .file
            .children(node)
            .find(|&child| self.collector.file.kind(child) == SyntaxKind::DataDeclName)
        else {
            return false;
        };
        let Some((name, range)) = self.collector.node_name(name_node) else {
            return false;
        };
        self.collector.declare_symbol(
            self.collector.declaration_scope(scope),
            name,
            SymbolKind::Variable,
            range,
            None,
            Some(Self::builtin_type("i")),
            None,
            None,
        );
        true
    }

    fn declare_find_inline_target(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        type_name: &'static str,
    ) -> bool {
        let Some(name_node) = self
            .collector
            .file
            .children(node)
            .find(|&child| self.collector.file.kind(child) == SyntaxKind::DataDeclName)
        else {
            return false;
        };
        let Some((name, range)) = self.collector.node_name(name_node) else {
            return false;
        };
        self.collector.declare_symbol(
            self.collector.declaration_scope(scope),
            name,
            SymbolKind::Variable,
            range,
            None,
            Some(Self::builtin_type(type_name)),
            None,
            None,
        );
        true
    }

    fn declare_find_match_inline_target(&mut self, node: NodeId, scope: ScopeId) -> bool {
        self.declare_find_inline_target(node, scope, "i")
    }

    fn declare_find_submatch_inline_target(&mut self, node: NodeId, scope: ScopeId) -> bool {
        self.declare_find_inline_target(node, scope, "string")
    }

    fn find_stmt_is_all_occurrences(&self, stmt_node: NodeId) -> bool {
        self.collector
            .significant_stmt_token_infos(stmt_node)
            .get(1)
            .map(|token| token.text.eq_ignore_ascii_case("all"))
            .unwrap_or(false)
    }

    fn find_results_type_name(&self, stmt_node: NodeId) -> &'static str {
        if self.find_stmt_is_all_occurrences(stmt_node) {
            "match_result_tab"
        } else {
            "match_result"
        }
    }

    fn declare_find_results_inline_target(
        &mut self,
        stmt_node: NodeId,
        node: NodeId,
        scope: ScopeId,
    ) -> bool {
        let Some(name_node) = self
            .collector
            .file
            .children(node)
            .find(|&child| self.collector.file.kind(child) == SyntaxKind::DataDeclName)
        else {
            return false;
        };
        let Some((name, range)) = self.collector.node_name(name_node) else {
            return false;
        };
        let type_name = self.find_results_type_name(stmt_node);
        let declared_type = Self::builtin_type(type_name);
        let structure = self
            .collector
            .lookup_symbol_in_scope_chain(scope, Namespace::Type, type_name)
            .and_then(|symbol_id| self.collector.symbol(symbol_id).structure);
        self.collector.declare_symbol(
            self.collector.declaration_scope(scope),
            name,
            SymbolKind::Variable,
            range,
            structure,
            Some(declared_type),
            None,
            None,
        );
        true
    }

    fn declare_convert_inline_target(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        type_name: &'static str,
    ) -> bool {
        let Some(name_node) = self
            .collector
            .file
            .children(node)
            .find(|&child| self.collector.file.kind(child) == SyntaxKind::DataDeclName)
        else {
            return false;
        };
        let Some((name, range)) = self.collector.node_name(name_node) else {
            return false;
        };
        self.collector.declare_symbol(
            self.collector.declaration_scope(scope),
            name,
            SymbolKind::Variable,
            range,
            None,
            Some(Self::builtin_type(type_name)),
            None,
            None,
        );
        true
    }

    fn collect_convert_output_target(
        &mut self,
        target_id: NodeId,
        scope: ScopeId,
        type_name: &'static str,
    ) {
        if self.collector.file.kind(target_id) == SyntaxKind::DataInlineDecl {
            if !self.declare_convert_inline_target(target_id, scope, type_name) {
                self.collector
                    .decl_lowering()
                    .walk_inline_decl(target_id, scope);
            }
        } else {
            self.collector.walk_node(target_id, scope);
        }
    }

    pub(super) fn collect_delete_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        let Some((stmt_source_expr, stmt_where_expr, stmt_comparing_operands)) =
            DeleteStmt::cast(self.collector.syntax(node)).map(|stmt| {
                (
                    stmt.source().map(|expr| expr.id()),
                    stmt.where_expr(self.collector.source).map(|expr| expr.id()),
                    stmt.comparing_operands(self.collector.source)
                        .into_iter()
                        .map(|expr| expr.id())
                        .collect::<Vec<_>>(),
                )
            })
        else {
            self.collector.walk_children(node, scope);
            return;
        };

        let (source_expr, from_expr, where_expr) = self.delete_stmt_operands(node);
        if let Some(source_expr) = source_expr
            && (from_expr.is_some() || self.delete_stmt_starts_with_from(node))
            && let Some(source_name) = self.simple_delete_source_name(source_expr)
            && self
                .collector
                .lookup_symbol_in_scope_chain(scope, Namespace::Value, source_name.as_ref())
                .is_none()
        {
            self.collector
                .sql_lowering()
                .collect_delete_db_table_stmt(node, scope);
            return;
        }

        self.record_system_field_updates(
            scope,
            node,
            SystemFieldStatementKind::DeleteTable,
            &["subrc"],
        );

        let source_expr = source_expr.or(stmt_source_expr);
        let comparing_itab_base = source_expr.and_then(|expr| {
            self.collector
                .sql_target_name_from_expr(self.delete_operand_expr_node(expr))
        });
        if let Some(source_expr) = source_expr {
            self.record_routine_site(
                scope,
                self.collector.file.range(source_expr),
                RoutineSiteKind::Delete,
            );
        }

        for child in self.collector.file.children(node) {
            if stmt_comparing_operands.contains(&child) {
                continue;
            }
            self.collector.walk_node(child, scope);
        }

        if let Some(itab_base) = comparing_itab_base {
            for child in stmt_comparing_operands {
                let expr = self.delete_operand_expr_node(child);
                if let Some(field_path) = self.delete_comparing_field_segments_from_expr(expr) {
                    self.collector.emit_field_access(FieldAccess {
                        scope,
                        base_namespace: Namespace::Value,
                        base_name: Arc::clone(&itab_base),
                        base_range: self.collector.file.range(expr),
                        field_path,
                        in_type_position: false,
                    });
                    continue;
                }
                self.collector.walk_node(child, scope);
            }
        } else {
            for child in stmt_comparing_operands {
                self.collector.walk_node(child, scope);
            }
        }

        let where_expr = where_expr.or(stmt_where_expr);

        let Some(source_expr) = source_expr else {
            return;
        };
        let Some(where_expr) = where_expr else {
            return;
        };
        let Some(source_access) = self.collector.value_access_from_node(source_expr, scope) else {
            return;
        };
        self.collector
            .loop_where_field_contexts
            .push(crate::def_map::LoopWhereFieldContext {
                scope,
                range: self.collector.file.range(where_expr),
                source_access,
                target_access: None,
            });
    }

    pub(super) fn collect_modify_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        let significant = self.collector.significant_stmt_token_infos(node);
        if Self::tokens_match_keyword_sequence(&significant, &["modify", "screen"]) {
            self.collect_modify_screen_stmt_infos(&significant, scope);
            return;
        }

        let (saw_table_keyword, saw_internal_table_clause, head_expr, from_expr, _where_expr) =
            self.modify_stmt_operands(node);
        if !saw_table_keyword
            && !saw_internal_table_clause
            && from_expr.is_some()
            && let Some(source_expr) = head_expr
            && let Some(source_name) = self.simple_delete_source_name(source_expr)
            && self
                .collector
                .lookup_symbol_in_scope_chain(scope, Namespace::Value, source_name.as_ref())
                .is_none()
        {
            self.collector
                .sql_lowering()
                .collect_modify_db_table_stmt(node, scope);
            return;
        }

        self.record_system_field_updates(
            scope,
            node,
            SystemFieldStatementKind::ModifyTable,
            &["subrc"],
        );

        let modify_target_access = head_expr.and_then(|expr| {
            self.collector
                .value_access_from_node(expr, scope)
                .filter(|access| access.base_namespace == Namespace::Value)
        });
        let modify_from_access = from_expr.and_then(|expr| {
            self.collector
                .value_access_from_node(expr, scope)
                .filter(|access| access.base_namespace == Namespace::Value)
        });

        self.collector.walk_children(node, scope);

        if let Some(base_access) = modify_target_access
            .clone()
            .or_else(|| modify_from_access.clone())
        {
            for component_path in self.modify_transporting_field_paths(node) {
                let mut field_path = base_access.field_path.clone();
                field_path.extend(component_path);
                self.collector.emit_field_access(FieldAccess {
                    scope,
                    base_namespace: base_access.base_namespace,
                    base_name: Arc::clone(&base_access.base_name),
                    base_range: base_access.base_range.clone(),
                    field_path,
                    in_type_position: false,
                });
            }
        }

        if let Some((where_tokens, where_range)) = self.modify_where_clause_tokens(node) {
            self.collect_modify_where_clause_refs(&where_tokens, scope);

            if let Some(source_access) = modify_target_access.or_else(|| modify_from_access.clone())
            {
                self.collector.loop_where_field_contexts.push(
                    crate::def_map::LoopWhereFieldContext {
                        scope,
                        range: where_range,
                        source_access,
                        target_access: modify_from_access,
                    },
                );
            }
        }
    }

    pub(super) fn collect_append_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        self.record_system_field_updates(scope, node, SystemFieldStatementKind::Append, &["tabix"]);

        #[derive(Clone, Copy, PartialEq, Eq)]
        enum AppendClause {
            Source,
            Target,
            Assigning,
            ReferenceInto,
            SortedBy,
        }

        let stmt_range = self.collector.file.range(node);
        let mut clause = AppendClause::Source;
        let mut saw_reference = false;
        let mut saw_lines_keyword = false;
        let mut source_is_lines_of = false;
        let mut source_expr = None;
        let mut target_expr = None;

        for child in self.collector.file.children(node) {
            if self.collector.file.kind(child) == SyntaxKind::Token {
                if let Some(token) = self.collector.syntax_token_nodes(child).into_iter().next() {
                    if token.text.eq_ignore_ascii_case("to") {
                        clause = AppendClause::Target;
                        saw_reference = false;
                        saw_lines_keyword = false;
                    } else if token.text.eq_ignore_ascii_case("assigning") {
                        clause = AppendClause::Assigning;
                        saw_reference = false;
                        saw_lines_keyword = false;
                    } else if token.text.eq_ignore_ascii_case("reference") {
                        saw_reference = true;
                    } else if saw_reference && token.text.eq_ignore_ascii_case("into") {
                        clause = AppendClause::ReferenceInto;
                        saw_reference = false;
                        saw_lines_keyword = false;
                    } else if token.text.eq_ignore_ascii_case("sorted") {
                        clause = AppendClause::SortedBy;
                        saw_reference = false;
                        saw_lines_keyword = false;
                    } else if clause == AppendClause::Source
                        && token.text.eq_ignore_ascii_case("lines")
                    {
                        saw_lines_keyword = true;
                    } else if clause == AppendClause::Source
                        && saw_lines_keyword
                        && token.text.eq_ignore_ascii_case("of")
                    {
                        source_is_lines_of = true;
                        saw_lines_keyword = false;
                    } else if !token.text.eq_ignore_ascii_case("line")
                        && !token.text.eq_ignore_ascii_case("lines")
                        && !token.text.eq_ignore_ascii_case("of")
                    {
                        saw_reference = false;
                        saw_lines_keyword = false;
                    }
                }
                continue;
            }

            match clause {
                AppendClause::Source => {
                    if source_expr.is_none() {
                        source_expr = Some(child);
                    }
                }
                AppendClause::Target => {
                    if target_expr.is_none() {
                        target_expr = Some(child);
                    }
                }
                AppendClause::Assigning | AppendClause::ReferenceInto | AppendClause::SortedBy => {}
            }

            self.collector.walk_node(child, scope);
        }

        if let Some(target_expr) = target_expr {
            let rhs_nodes = source_expr.into_iter().collect::<Vec<_>>();
            let rhs_fact = source_expr
                .filter(|_| source_is_lines_of)
                .map(|expr| self.type_fact_from_table_line_node(expr, scope));
            self.emit_assignment_site_with_type_facts(
                scope,
                stmt_range,
                target_expr,
                &rhs_nodes,
                None,
                rhs_fact,
            );
        }
    }

    pub(super) fn collect_read_table_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_system_field_updates(
            scope,
            node,
            SystemFieldStatementKind::ReadTable,
            &["subrc", "tabix", "tfill", "tleng"],
        );
        if ReadTableStmt::cast(self.collector.syntax(node)).is_some() {
            let children: Vec<_> = self.collector.file.children(node).collect();
            let mut entry_start = 0usize;
            while entry_start < children.len() {
                let mut entry_end = entry_start;
                while entry_end < children.len() {
                    let child = children[entry_end];
                    if self.collector.file.kind(child) == SyntaxKind::Token
                        && self
                            .collector
                            .syntax_token_nodes(child)
                            .into_iter()
                            .next()
                            .is_some_and(|token| token.text.as_ref() == ",")
                    {
                        break;
                    }
                    entry_end += 1;
                }
                self.collect_read_table_entry(&children[entry_start..entry_end], scope);
                entry_start = entry_end.saturating_add(1);
            }
            return;
        }
        self.collector.walk_children(node, scope);
    }

    pub(super) fn collect_authority_check_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        self.record_system_field_updates(
            scope,
            node,
            SystemFieldStatementKind::AuthorityCheck,
            &["subrc"],
        );
        if let Some(stmt) = AuthorityCheckStmt::cast(self.collector.syntax(node)) {
            let mut operand_ids = Vec::new();
            if let Some(object) = stmt.object().and_then(|operand| operand.value()) {
                operand_ids.push(object.id());
            }
            if let Some(user) = stmt.user().and_then(|operand| operand.value()) {
                operand_ids.push(user.id());
            }
            for clause in stmt.id_clauses() {
                if let Some(id) = clause.id().and_then(|operand| operand.value()) {
                    operand_ids.push(id.id());
                }
                if let Some(field) = clause.field().and_then(|operand| operand.value()) {
                    operand_ids.push(field.id());
                }
            }
            for operand_id in operand_ids {
                self.collector.walk_node(operand_id, scope);
            }
            return;
        }
        self.collector.walk_children(node, scope);
    }

    pub(super) fn collect_message_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        self.record_system_field_updates(
            scope,
            node,
            SystemFieldStatementKind::Message,
            &[
                "msgid", "msgno", "msgty", "msgv1", "msgv2", "msgv3", "msgv4",
            ],
        );
        let Some((
            head_clause_id,
            with_clause_id,
            into_clause_id,
            display_clause_id,
            raising_clause_id,
        )) = (match MessageStmt::cast(self.collector.syntax(node)) {
            Some(stmt) => Some((
                stmt.head_clause().map(|clause| clause.syntax().id()),
                stmt.with_clause().map(|clause| clause.syntax().id()),
                stmt.into_clause().map(|clause| clause.syntax().id()),
                stmt.display_like_clause()
                    .map(|clause| clause.syntax().id()),
                stmt.raising_clause().map(|clause| clause.syntax().id()),
            )),
            None => None,
        })
        else {
            return;
        };

        if let Some(head_clause_id) = head_clause_id {
            self.collect_message_head_clause_infos(head_clause_id, scope);
        }

        if let Some(with_clause_id) = with_clause_id {
            for child in self.collector.file.children(with_clause_id) {
                match self.collector.file.kind(child) {
                    SyntaxKind::Token | SyntaxKind::MessageTextPoolId => {}
                    SyntaxKind::MessageOperand => self.collect_message_operand_node(child, scope),
                    _ => self.collector.walk_node(child, scope),
                }
            }
        }

        if let Some(into_clause_id) = into_clause_id {
            let mut target_range = None;
            let mut lhs_target_access = None;
            for child in self.collector.file.children(into_clause_id) {
                match self.collector.file.kind(child) {
                    SyntaxKind::Token => {}
                    SyntaxKind::DataInlineDecl => {
                        let (structure, declared_type) =
                            self.collector.inline_decl_inferred_type(child, scope);
                        self.collector.decl_lowering().declare_inline_variable_decl(
                            child,
                            scope,
                            structure,
                            declared_type,
                        );
                        target_range = self
                            .collector
                            .file
                            .children(child)
                            .find(|&grandchild| {
                                self.collector.file.kind(grandchild) == SyntaxKind::DataDeclName
                            })
                            .map(|grandchild| self.collector.file.range(grandchild))
                            .or_else(|| Some(self.collector.file.range(child)));
                    }
                    _ => {
                        self.collector.walk_node(child, scope);
                        if target_range.is_none() {
                            target_range = Some(self.collector.file.range(child));
                            lhs_target_access = self.collector.value_access_from_node(child, scope);
                        }
                    }
                }
            }
            if target_range.is_none() {
                let sig = self.collector.significant_stmt_token_infos(into_clause_id);
                if sig.len() > 1 {
                    self.collect_message_operand_refs_infos(&sig[1..], scope);
                    let lhs_range = sig[1..]
                        .iter()
                        .map(|token| token.range.clone())
                        .reduce(|acc, next| acc.start.min(next.start)..acc.end.max(next.end));
                    target_range = lhs_range;
                }
            }
            if let Some(lhs_range) = target_range {
                let rhs_range = self.message_into_rhs_range(
                    node,
                    &[
                        head_clause_id,
                        with_clause_id,
                        display_clause_id,
                        raising_clause_id,
                    ],
                );
                self.collector.emit_assignment_site(AssignmentSiteData {
                    scope,
                    range: self.collector.file.range(node),
                    lhs_range,
                    rhs_range,
                    lhs_target_access,
                    lhs: TypeFactData::default(),
                    rhs: TypeFactData::default(),
                    rhs_is_top_level_sum: false,
                });
            }
        }

        if let Some(display_clause_id) = display_clause_id {
            let mut had_non_token = false;
            for child in self.collector.file.children(display_clause_id) {
                if self.collector.file.kind(child) != SyntaxKind::Token {
                    had_non_token = true;
                    self.collector.walk_node(child, scope);
                }
            }
            if !had_non_token {
                let sig = self
                    .collector
                    .significant_stmt_token_infos(display_clause_id);
                if sig.len() > 2 {
                    self.collect_message_operand_refs_infos(&sig[2..], scope);
                }
            }
        }

        if let Some(raising_clause_id) = raising_clause_id {
            for child in self.collector.file.children(raising_clause_id) {
                if self.collector.file.kind(child) == SyntaxKind::Token {
                    continue;
                }
                // MESSAGE ... RAISING raises a non-class-based exception name.
                // The trailing identifier is not a value expression and should not
                // produce unresolved value-symbol diagnostics such as `file_error`.
            }
        }
    }

    pub(super) fn collect_insert_table_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        self.record_system_field_updates(
            scope,
            node,
            SystemFieldStatementKind::InsertTable,
            &["subrc"],
        );

        #[derive(Clone, Copy, PartialEq, Eq)]
        enum InsertClause {
            Source,
            Target,
            Index,
            Assigning,
            ReferenceInto,
        }

        let stmt_range = self.collector.file.range(node);
        let mut clause = InsertClause::Source;
        let mut saw_reference = false;
        let mut saw_lines_keyword = false;
        let mut source_is_lines_of = false;
        let mut source_expr = None;
        let mut target_expr = None;

        for child in self.collector.file.children(node) {
            if self.collector.file.kind(child) == SyntaxKind::Token {
                if let Some(token) = self.collector.syntax_token_nodes(child).into_iter().next() {
                    if token.text.eq_ignore_ascii_case("reference") {
                        saw_reference = true;
                    } else if saw_reference && token.text.eq_ignore_ascii_case("into") {
                        clause = InsertClause::ReferenceInto;
                        saw_reference = false;
                        saw_lines_keyword = false;
                    } else if token.text.eq_ignore_ascii_case("into") {
                        clause = InsertClause::Target;
                        saw_reference = false;
                        saw_lines_keyword = false;
                    } else if token.text.eq_ignore_ascii_case("index") {
                        clause = InsertClause::Index;
                        saw_reference = false;
                        saw_lines_keyword = false;
                    } else if token.text.eq_ignore_ascii_case("assigning") {
                        clause = InsertClause::Assigning;
                        saw_reference = false;
                        saw_lines_keyword = false;
                    } else if clause == InsertClause::Source
                        && token.text.eq_ignore_ascii_case("lines")
                    {
                        saw_lines_keyword = true;
                    } else if clause == InsertClause::Source
                        && saw_lines_keyword
                        && token.text.eq_ignore_ascii_case("of")
                    {
                        source_is_lines_of = true;
                        saw_lines_keyword = false;
                    } else if !token.text.eq_ignore_ascii_case("line")
                        && !token.text.eq_ignore_ascii_case("lines")
                        && !token.text.eq_ignore_ascii_case("of")
                        && !token.text.eq_ignore_ascii_case("table")
                    {
                        saw_reference = false;
                        saw_lines_keyword = false;
                    }
                }
                continue;
            }

            match clause {
                InsertClause::Source => {
                    if source_expr.is_none() {
                        source_expr = Some(child);
                    }
                }
                InsertClause::Target => {
                    if target_expr.is_none() {
                        target_expr = Some(child);
                    }
                }
                InsertClause::Index | InsertClause::Assigning | InsertClause::ReferenceInto => {}
            }

            self.collector.walk_node(child, scope);
        }

        if let Some(target_expr) = target_expr {
            let rhs_nodes = source_expr.into_iter().collect::<Vec<_>>();
            let rhs_fact = source_expr
                .filter(|_| source_is_lines_of)
                .map(|expr| self.type_fact_from_table_line_node(expr, scope));
            self.emit_assignment_site_with_type_facts(
                scope,
                stmt_range,
                target_expr,
                &rhs_nodes,
                None,
                rhs_fact,
            );
        }
    }

    pub(super) fn collect_insert_textpool_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        self.record_system_field_updates(
            scope,
            node,
            SystemFieldStatementKind::InsertTextpool,
            &["subrc"],
        );
        for child in self.collector.file.children(node) {
            if self.collector.file.kind(child) != SyntaxKind::Token {
                self.collector.walk_node(child, scope);
            }
        }
    }

    fn collect_message_head_clause_infos(&mut self, node: NodeId, scope: ScopeId) {
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::MessageIdOperand
                | SyntaxKind::MessageTypeOperand
                | SyntaxKind::MessageNumberOperand
                | SyntaxKind::MessageCodeOperand => self.collect_message_operand_node(child, scope),
                _ => {}
            }
        }
    }

    fn message_into_rhs_range(
        &self,
        stmt: NodeId,
        clause_ids: &[Option<NodeId>],
    ) -> abap_lexer::TextRange {
        clause_ids
            .iter()
            .flatten()
            .map(|&clause| self.collector.file.range(clause))
            .reduce(|acc, next| acc.start.min(next.start)..acc.end.max(next.end))
            .unwrap_or_else(|| {
                let range = self.collector.file.range(stmt);
                range.start..range.start
            })
    }

    fn collect_message_operand_node(&mut self, node: NodeId, scope: ScopeId) {
        let sig = self.collector.significant_stmt_token_infos(node);
        if sig.is_empty() {
            return;
        }
        if self.collector.file.kind(node) == SyntaxKind::MessageCodeOperand
            && self.is_compact_message_short_form(&sig)
        {
            // Short MESSAGE forms like `MESSAGE i043.` are not identifiers.
            return;
        }

        let non_token_children: Vec<_> = self
            .collector
            .file
            .children(node)
            .filter(|&child| self.collector.file.kind(child) != SyntaxKind::Token)
            .collect();
        if non_token_children
            .iter()
            .any(|&child| self.collector.file.kind(child) == SyntaxKind::MessageTextPoolId)
        {
            return;
        }
        if !non_token_children.is_empty() {
            for child in non_token_children {
                self.collector.walk_node(child, scope);
            }
            return;
        }
        match self.collector.file.kind(node) {
            SyntaxKind::MessageIdOperand => {
                if let Some((name, range)) = self.collector.simple_type_ref_base_from_infos(&sig) {
                    self.collector.add_reference(
                        scope,
                        name,
                        Namespace::Type,
                        ReferenceKind::MessageClass,
                        range,
                    );
                } else {
                    self.collect_message_operand_refs_infos(&sig, scope);
                }
            }
            SyntaxKind::MessageCodeOperand => {
                if self.is_compact_message_class_form(&sig) {
                    self.collect_compact_message_class_ref_infos(&sig, scope);
                } else {
                    self.collect_message_operand_refs_infos(&sig, scope);
                }
            }
            _ => self.collect_message_operand_refs_infos(&sig, scope),
        }
    }

    fn collect_message_operand_refs_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        let mut batch_start = 0usize;
        let mut idx = 0usize;
        while idx < tokens.len() {
            let is_text_pool = tokens[idx].text.eq_ignore_ascii_case("text")
                && tokens
                    .get(idx + 1)
                    .is_some_and(|token| token.text.as_ref() == "-")
                && tokens
                    .get(idx + 2)
                    .is_some_and(|token| token.text.chars().all(|ch| ch.is_ascii_digit()));
            if is_text_pool {
                if batch_start < idx {
                    self.collector.collect_token_expression_refs_infos(
                        &tokens[batch_start..idx],
                        scope,
                        true,
                    );
                }
                idx += 3;
                batch_start = idx;
                continue;
            }
            idx += 1;
        }
        if batch_start < tokens.len() {
            self.collector
                .collect_token_expression_refs_infos(&tokens[batch_start..], scope, true);
        }
    }

    fn collect_selection_screen_title_refs(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
        scope: ScopeId,
    ) {
        let period_idx = tokens
            .iter()
            .position(|token| token.text.as_ref() == ".")
            .unwrap_or(tokens.len());
        let title_end = tokens[start..period_idx]
            .iter()
            .position(|token| token.text.as_ref() == ",")
            .map(|offset| start + offset)
            .unwrap_or(period_idx);
        if start >= title_end {
            return;
        }

        let mut batch_start = start;
        let mut idx = start;
        while idx < title_end {
            let is_text_pool = tokens[idx].text.eq_ignore_ascii_case("text")
                && tokens
                    .get(idx + 1)
                    .is_some_and(|token| token.text.as_ref() == "-")
                && tokens
                    .get(idx + 2)
                    .is_some_and(|token| token.text.chars().all(|ch| ch.is_ascii_alphanumeric()));
            if is_text_pool {
                self.collect_token_expression_refs_range(tokens, batch_start, idx, scope);
                idx += 3;
                batch_start = idx;
                continue;
            }
            idx += 1;
        }
        self.collect_token_expression_refs_range(tokens, batch_start, title_end, scope);
    }

    pub(super) fn collect_selection_screen_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let tokens = self.collector.significant_stmt_token_infos(node);
        if tokens.len() < 6
            || !Self::tokens_match_keyword_sequence(&tokens, &["selection", "-", "screen"])
        {
            return;
        }

        let body_start = if tokens
            .get(3)
            .is_some_and(|token| token.text.as_ref() == ":")
        {
            4
        } else {
            3
        };

        if Self::tokens_match_keyword_sequence(&tokens[body_start..], &["begin", "of", "block"]) {
            if let Some(with_idx) =
                self.find_top_level_keyword_infos(&tokens, body_start + 3, &["WITH"])
                && Self::tokens_match_keyword_sequence(
                    &tokens[with_idx..],
                    &["with", "frame", "title"],
                )
            {
                self.collect_selection_screen_title_refs(&tokens, with_idx + 3, scope);
            }
        }
    }

    pub(super) fn collect_generic_simple_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.collector.node_has_structured_children(node) {
            self.collector.walk_children(node, scope);
            return;
        }

        let significant = self.collector.significant_stmt_token_infos(node);
        let Some((head, tail)) = significant.split_first() else {
            return;
        };

        if head.text.eq_ignore_ascii_case("return") {
            self.record_routine_site(
                scope,
                self.collector.file.range(node),
                RoutineSiteKind::Return,
            );
        } else if head.text.eq_ignore_ascii_case("raise") {
            self.record_routine_site(
                scope,
                self.collector.file.range(node),
                RoutineSiteKind::Raise,
            );
        } else if head.text.eq_ignore_ascii_case("leave") {
            self.record_routine_site(
                scope,
                self.collector.file.range(node),
                RoutineSiteKind::Leave,
            );
        } else if head.text.eq_ignore_ascii_case("exit") {
            self.record_routine_site(
                scope,
                self.collector.file.range(node),
                RoutineSiteKind::Exit,
            );
        } else if head.text.eq_ignore_ascii_case("continue") {
            self.record_routine_site(
                scope,
                self.collector.file.range(node),
                RoutineSiteKind::Continue,
            );
        } else if head.text.eq_ignore_ascii_case("stop") {
            self.record_routine_site(
                scope,
                self.collector.file.range(node),
                RoutineSiteKind::Stop,
            );
        }

        if (head.text.eq_ignore_ascii_case("commit") || head.text.eq_ignore_ascii_case("rollback"))
            && matches!(tail.first(), Some(token) if token.text.eq_ignore_ascii_case("work"))
        {
            return;
        }

        if head.text.eq_ignore_ascii_case("set")
            && Self::tokens_match_keyword_sequence(tail, &["update", "task", "local"])
        {
            return;
        }

        if head.text.eq_ignore_ascii_case("set")
            && (self.collect_set_pf_status_stmt_infos(&significant, scope)
                || self.collect_set_titlebar_stmt_infos(&significant, scope))
        {
            self.record_unknown_effect(node, scope);
            return;
        }

        if head.text.eq_ignore_ascii_case("get")
            && matches!(tail.first(), Some(token) if token.text.eq_ignore_ascii_case("time"))
        {
            return;
        }

        if head.text.eq_ignore_ascii_case("get")
            && matches!(tail.first(), Some(token) if token.text.eq_ignore_ascii_case("badi"))
        {
            let target_start = 1usize;
            let target_end = tail
                .iter()
                .position(|token| token.text.as_ref() == ".")
                .unwrap_or(tail.len());
            if target_start < target_end {
                self.collector.collect_token_expression_refs_infos(
                    &tail[target_start..target_end],
                    scope,
                    true,
                );
            }
            return;
        }

        if head.text.eq_ignore_ascii_case("refresh") {
            self.collect_refresh_stmt_infos(tail, scope);
            return;
        }

        if head.text.eq_ignore_ascii_case("log")
            && self.collect_log_point_stmt_infos(&significant, scope)
        {
            return;
        }

        self.record_unknown_effect(node, scope);
        self.collector
            .collect_token_expression_refs_infos(tail, scope, true);
    }

    fn collect_refresh_stmt_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        let mut idx = 0usize;
        while idx < tokens.len() {
            match tokens[idx].text.as_ref() {
                ":" | "," => {
                    idx += 1;
                }
                "." => break,
                _ => {
                    let end = self.consume_simple_operand_tokens(tokens, idx);
                    if end <= idx {
                        idx += 1;
                        continue;
                    }
                    self.collector.collect_token_expression_refs_infos(
                        &tokens[idx..end],
                        scope,
                        true,
                    );
                    self.record_routine_site(
                        scope,
                        tokens[idx].range.start..tokens[end - 1].range.end,
                        RoutineSiteKind::Clear,
                    );
                    idx = end;
                }
            }
        }
    }

    pub(super) fn collect_close_cursor_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        let handle_tokens = self
            .collector
            .file
            .children(node)
            .find(|&child| self.collector.file.kind(child) == SyntaxKind::CursorHandleOperand)
            .map(|handle| self.collector.syntax_token_nodes(handle))
            .unwrap_or_default();
        self.collector
            .collect_token_expression_refs_infos(&handle_tokens, scope, true);
    }

    pub(super) fn collect_fetch_cursor_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        let children: Vec<_> = self
            .collector
            .file
            .children(node)
            .map(|child| (child, self.collector.file.kind(child)))
            .collect();
        for (child, kind) in children {
            match kind {
                SyntaxKind::CursorHandleOperand => {
                    let tokens = self.collector.syntax_token_nodes(child);
                    self.collector
                        .collect_token_expression_refs_infos(&tokens, scope, true);
                }
                SyntaxKind::SelectPackageSizeClause => {
                    let tokens = self.collector.syntax_token_nodes(child);
                    if let Some(size_idx) = tokens
                        .iter()
                        .position(|token| token.text.eq_ignore_ascii_case("size"))
                    {
                        self.collector.collect_token_expression_refs_infos(
                            &tokens[size_idx + 1..],
                            scope,
                            true,
                        );
                    }
                }
                _ => self.collector.walk_node(child, scope),
            }
        }
    }

    pub(super) fn collect_wait_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        if let Some(stmt) = WaitStmt::cast(self.collector.syntax(node))
            && let Some(duration) = stmt.duration().and_then(|operand| operand.value())
        {
            self.collector.walk_node(duration.id(), scope);
        }
    }

    pub(super) fn collect_aliases_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if let Some(entry_ids) = AliasesStmt::cast(self.collector.syntax(node)).map(|stmt| {
            stmt.entries()
                .map(|entry| {
                    (
                        entry.alias_name().map(|node| node.syntax().id()),
                        entry.target_interface().map(|node| node.syntax().id()),
                        entry.target_member().map(|node| node.syntax().id()),
                    )
                })
                .collect::<Vec<_>>()
        }) {
            let Some(owner_symbol) = self.collector.class_lowering().enclosing_type_owner(scope)
            else {
                return;
            };
            let mut recorded = false;
            for (alias_name_id, type_ref_id, target_member_id) in entry_ids {
                let Some(alias_name_id) = alias_name_id else {
                    continue;
                };
                let Some((alias_name, alias_range)) = self.collector.node_name(alias_name_id)
                else {
                    continue;
                };
                let Some(type_ref_id) = type_ref_id else {
                    continue;
                };
                self.collector
                    .decl_lowering()
                    .collect_type_ref(type_ref_id, scope);
                let Some((_, _, interface_name, _, _)) = self
                    .collector
                    .type_ref_access_chain(type_ref_id, Namespace::Type)
                else {
                    continue;
                };
                let Some(target_member_id) = target_member_id else {
                    continue;
                };
                let Some((target_member_name, target_member_range)) =
                    self.collector.node_name(target_member_id)
                else {
                    continue;
                };
                self.collector.emit_field_access(crate::FieldAccess {
                    scope,
                    base_namespace: Namespace::Type,
                    base_name: Arc::clone(&interface_name),
                    base_range: self.collector.file.range(type_ref_id),
                    field_path: vec![crate::FieldAccessSegment {
                        name: Arc::clone(&target_member_name),
                        range: target_member_range.clone(),
                    }],
                    in_type_position: false,
                });
                self.collector.member_aliases.push(crate::MemberAliasData {
                    owner_symbol,
                    alias_name,
                    target_interface_name: interface_name,
                    target_member_name,
                    range: alias_range,
                });
                recorded = true;
            }
            if recorded {
                return;
            }
        }
        let significant = self.collector.significant_stmt_token_infos(node);
        self.collect_aliases_stmt_infos(&significant, scope);
    }

    pub(super) fn collect_clear_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if let Some(stmt) = ClearStmt::cast(self.collector.syntax(node)) {
            let operands: Vec<_> = stmt
                .operands()
                .filter_map(|operand| operand.value())
                .map(|operand| operand.id())
                .collect();
            for operand in operands {
                self.record_routine_site(
                    scope,
                    self.collector.file.range(operand),
                    RoutineSiteKind::Clear,
                );
                self.collector.walk_node(operand, scope);
            }
        }
    }

    pub(super) fn collect_refresh_stmt(&mut self, node: NodeId, scope: ScopeId) {
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::RefreshOperand => {
                    self.record_routine_site(
                        scope,
                        self.collector.file.range(child),
                        RoutineSiteKind::Clear,
                    );
                    self.collector.walk_children(child, scope);
                }
                SyntaxKind::Token => {}
                _ => self.collector.walk_node(child, scope),
            }
        }
    }

    pub(super) fn collect_structured_effect_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        self.collector.walk_children(node, scope);
    }

    pub(super) fn collect_describe_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        self.record_system_field_updates(
            scope,
            node,
            SystemFieldStatementKind::DescribeTable,
            &["tfill", "tleng"],
        );
        if let Some(stmt) = DescribeStmt::cast(self.collector.syntax(node)) {
            let table_operands: Vec<_> = stmt
                .table_operands()
                .filter_map(|operand| operand.value())
                .map(|value| value.id())
                .collect();
            let lines_targets: Vec<_> = stmt
                .lines_targets()
                .filter_map(|target| target.value())
                .map(|value| value.id())
                .collect();
            for table_operand in table_operands {
                self.collector.walk_node(table_operand, scope);
            }
            for lines_target in lines_targets {
                if !self.declare_describe_lines_inline_target(lines_target, scope)
                    || self.collector.file.kind(lines_target) != SyntaxKind::DataInlineDecl
                {
                    self.collector.walk_node(lines_target, scope);
                }
            }
        }
    }

    pub(super) fn collect_convert_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        self.record_system_field_updates(
            scope,
            node,
            SystemFieldStatementKind::Convert,
            &["subrc"],
        );
        let Some((operands, time_zone, target, date_target, time_target, daylight_saving_target)) =
            (match ConvertStmt::cast(self.collector.syntax(node)) {
                Some(stmt) => Some((
                    stmt.operands()
                        .filter_map(|operand| operand.value())
                        .map(|value| value.id())
                        .collect::<Vec<_>>(),
                    stmt.time_zone()
                        .and_then(|target| target.value())
                        .map(|value| value.id()),
                    stmt.target()
                        .and_then(|target| target.value())
                        .map(|value| value.id()),
                    stmt.date_target()
                        .and_then(|target| target.value())
                        .map(|value| value.id()),
                    stmt.time_target()
                        .and_then(|target| target.value())
                        .map(|value| value.id()),
                    stmt.daylight_saving_target()
                        .and_then(|target| target.value())
                        .map(|value| value.id()),
                )),
                None => None,
            })
        else {
            return;
        };

        for operand in operands {
            self.collector.walk_node(operand, scope);
        }
        if let Some(time_zone) = time_zone {
            self.collector.walk_node(time_zone, scope);
        }
        if let Some(target) = target {
            self.collector.walk_node(target, scope);
        }
        if let Some(target) = date_target {
            self.collect_convert_output_target(target, scope, "d");
        }
        if let Some(target) = time_target {
            self.collect_convert_output_target(target, scope, "t");
        }
        if let Some(target) = daylight_saving_target {
            self.collect_convert_output_target(target, scope, "c");
        }
    }

    pub(super) fn collect_replace_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        if let Some(stmt) = ReplaceStmt::cast(self.collector.syntax(node)) {
            let operands: Vec<_> = stmt
                .patterns()
                .filter_map(|operand| operand.value())
                .chain(stmt.targets().filter_map(|operand| operand.value()))
                .chain(stmt.replacements().filter_map(|operand| operand.value()))
                .map(|value| value.id())
                .collect();
            for operand in operands {
                self.collector.walk_node(operand, scope);
            }
        }
    }

    pub(super) fn collect_raise_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let Some((type_ref_id, trailing_children)) =
            (match RaiseStmt::cast(self.collector.syntax(node)) {
                Some(stmt) => stmt.exception_type_ref().map(|type_ref| {
                    (
                        type_ref.syntax().id(),
                        stmt.trailing_children()
                            .into_iter()
                            .map(|child| (child.id(), child.kind()))
                            .collect::<Vec<_>>(),
                    )
                }),
                None => None,
            })
        else {
            self.collect_generic_simple_stmt(node, scope);
            return;
        };

        self.record_routine_site(
            scope,
            self.collector.file.range(node),
            RoutineSiteKind::Raise,
        );
        self.collector
            .decl_lowering()
            .collect_type_ref(type_ref_id, scope);

        let constructor_target = self
            .collector
            .type_ref_access_chain(type_ref_id, Namespace::Type)
            .and_then(|(namespace, _, base_name, _, field_path)| {
                (namespace == Namespace::Type && field_path.is_empty()).then_some(
                    NamedArgumentTarget::Constructor {
                        type_name: base_name,
                    },
                )
            });

        let call_range = self.collector.file.range(node);
        let mut trailing_tokens = Vec::new();
        for (child_id, child_kind) in trailing_children {
            if child_kind == SyntaxKind::CallArgList {
                if let Some(target) = constructor_target.clone() {
                    self.collector.expr_lowering().collect_call_argument_list(
                        child_id,
                        scope,
                        target,
                        call_range.clone(),
                    );
                } else {
                    self.collector
                        .expr_lowering()
                        .collect_structured_argument_values_from_children(child_id, scope);
                }
                continue;
            }
            trailing_tokens.extend(self.collector.syntax_token_nodes(child_id));
        }
        if !trailing_tokens.is_empty() {
            self.collector
                .collect_token_expression_refs_infos(&trailing_tokens, scope, true);
        }
    }

    pub(super) fn collect_raise_event_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_routine_site(
            scope,
            self.collector.file.range(node),
            RoutineSiteKind::Raise,
        );
        let significant = self.collector.significant_stmt_token_infos(node);
        if significant.len() < 3 {
            return;
        }

        let mut qualifier = None;
        let mut event_name = None;
        if significant
            .get(3)
            .is_some_and(|token| token.text.as_ref() == "~")
            && let (Some(qualifier_tok), Some(event_tok)) = (significant.get(2), significant.get(4))
            && self.collector.syntax_token_is_ident_like(qualifier_tok)
            && self.collector.syntax_token_is_ident_like(event_tok)
        {
            let qualifier_name = Arc::<str>::from(qualifier_tok.text.to_ascii_lowercase());
            self.collector.add_reference(
                scope,
                qualifier_name.clone(),
                Namespace::Type,
                ReferenceKind::TypeRef,
                qualifier_tok.range.clone(),
            );
            self.collector.emit_field_access(FieldAccess {
                scope,
                base_namespace: Namespace::Type,
                base_name: qualifier_name.clone(),
                base_range: qualifier_tok.range.clone(),
                field_path: vec![FieldAccessSegment {
                    name: Arc::<str>::from(event_tok.text.to_ascii_lowercase()),
                    range: event_tok.range.clone(),
                }],
                in_type_position: false,
            });
            qualifier = Some(qualifier_name);
            event_name = Some(Arc::<str>::from(event_tok.text.to_ascii_lowercase()));
        } else if let Some(event_tok) = significant.get(2)
            && self.collector.syntax_token_is_ident_like(event_tok)
        {
            let event = Arc::<str>::from(event_tok.text.to_ascii_lowercase());
            self.collector.add_reference(
                scope,
                event.clone(),
                Namespace::Routine,
                ReferenceKind::RoutineCall,
                event_tok.range.clone(),
            );
            event_name = Some(event);
        }

        let Some(event_name) = event_name else {
            self.record_unknown_effect(node, scope);
            self.collector
                .collect_token_expression_refs_infos(&significant[2..], scope, true);
            return;
        };

        let arg_list = self
            .collector
            .file
            .children(node)
            .find(|&child| self.collector.file.kind(child) == SyntaxKind::CallArgList);
        let stmt_range = self.collector.file.range(node);
        if let Some(arg_list) = arg_list {
            self.collector.expr_lowering().collect_call_argument_list(
                arg_list,
                scope,
                NamedArgumentTarget::Event {
                    qualifier,
                    event_name,
                },
                stmt_range,
            );
        }
    }

    pub(super) fn collect_leave_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let significant = self.collector.significant_stmt_token_infos(node);
        let Some((head, tail)) = significant.split_first() else {
            return;
        };

        if !head.text.eq_ignore_ascii_case("leave") {
            self.collect_generic_simple_stmt(node, scope);
            return;
        }

        let kind = if Self::tokens_match_keyword_sequence(tail, &["list", "-", "processing"]) {
            RoutineSiteKind::LeaveListProcessing
        } else {
            RoutineSiteKind::Leave
        };
        self.record_routine_site(scope, self.collector.file.range(node), kind);
        self.record_unknown_effect(node, scope);
        self.collect_leave_operand_tokens(tail, scope);
    }

    fn collect_aliases_stmt_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        let Some(owner_symbol) = self.collector.class_lowering().enclosing_type_owner(scope) else {
            return;
        };

        let mut idx = 1usize;
        while idx < tokens.len() {
            while idx < tokens.len() && matches!(tokens[idx].text.as_ref(), ":" | "," | ".") {
                if tokens[idx].text.as_ref() == "." {
                    return;
                }
                idx += 1;
            }
            let Some(alias_tok) = tokens.get(idx) else {
                return;
            };
            if !self.collector.syntax_token_is_ident_like(alias_tok) {
                idx += 1;
                continue;
            }
            let alias_name = Arc::<str>::from(alias_tok.text.to_ascii_lowercase());
            idx += 1;
            if !tokens
                .get(idx)
                .is_some_and(|token| token.text.eq_ignore_ascii_case("for"))
            {
                continue;
            }
            idx += 1;
            let Some(interface_tok) = tokens.get(idx) else {
                return;
            };
            if !self.collector.syntax_token_is_ident_like(interface_tok) {
                continue;
            }
            let Some(tilde_tok) = tokens.get(idx + 1) else {
                return;
            };
            let Some(member_tok) = tokens.get(idx + 2) else {
                return;
            };
            if tilde_tok.text.as_ref() != "~"
                || !self.collector.syntax_token_is_ident_like(member_tok)
            {
                idx += 1;
                continue;
            }

            let interface_name = Arc::<str>::from(interface_tok.text.to_ascii_lowercase());
            let target_member_name = Arc::<str>::from(member_tok.text.to_ascii_lowercase());
            self.collector.add_reference(
                scope,
                Arc::clone(&interface_name),
                Namespace::Type,
                ReferenceKind::TypeRef,
                interface_tok.range.clone(),
            );
            self.collector.emit_field_access(crate::FieldAccess {
                scope,
                base_namespace: Namespace::Type,
                base_name: Arc::clone(&interface_name),
                base_range: interface_tok.range.clone(),
                field_path: vec![crate::FieldAccessSegment {
                    name: Arc::clone(&target_member_name),
                    range: member_tok.range.clone(),
                }],
                in_type_position: false,
            });
            self.collector.member_aliases.push(crate::MemberAliasData {
                owner_symbol,
                alias_name,
                target_interface_name: interface_name,
                target_member_name,
                range: alias_tok.range.clone(),
            });
            idx += 3;
        }
    }

    fn collect_compact_message_class_ref_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        let Some(lparen_idx) = tokens.iter().position(|token| token.text.as_ref() == "(") else {
            return;
        };
        let Some(rparen_idx) = self
            .collector
            .find_matching_group_end_infos(tokens, lparen_idx, "(", ")")
        else {
            return;
        };
        if rparen_idx <= lparen_idx + 1 {
            return;
        }
        if let Some((name, range)) = self
            .collector
            .simple_type_ref_base_from_infos(&tokens[lparen_idx + 1..rparen_idx])
        {
            self.collector.add_reference(
                scope,
                name,
                Namespace::Type,
                ReferenceKind::MessageClass,
                range,
            );
        }
    }

    fn is_compact_message_class_form(&self, tokens: &[SyntaxTokenInfo]) -> bool {
        let Some(head) = tokens.first() else {
            return false;
        };
        let mut chars = head.text.chars();
        let Some(msgty) = chars.next() else {
            return false;
        };
        if !matches!(
            msgty.to_ascii_lowercase(),
            'a' | 'e' | 'i' | 's' | 'w' | 'x'
        ) {
            return false;
        }
        chars.all(|ch| ch.is_ascii_digit()) && tokens.iter().any(|token| token.text.as_ref() == "(")
    }

    fn is_compact_message_short_form(&self, tokens: &[SyntaxTokenInfo]) -> bool {
        let [head] = tokens else {
            return false;
        };
        let mut chars = head.text.chars();
        let Some(msgty) = chars.next() else {
            return false;
        };
        matches!(
            msgty.to_ascii_lowercase(),
            'a' | 'e' | 'i' | 's' | 'w' | 'x'
        ) && chars.all(|ch| ch.is_ascii_digit())
    }

    pub(super) fn collect_find_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_system_field_updates(
            scope,
            node,
            SystemFieldStatementKind::Find,
            &["subrc", "fdpos"],
        );
        if let Some(stmt) = FindStmt::cast(self.collector.syntax(node)) {
            let mut read_operand_ids = Vec::new();
            let mut read_ranges = Vec::new();
            let mut write_targets = Vec::new();
            let pattern_id = stmt
                .pattern()
                .and_then(|operand| operand.value())
                .map(|value| value.id());
            let target_id = stmt
                .target()
                .and_then(|operand| operand.value())
                .map(|value| value.id());
            let match_target_ids: Vec<_> = stmt
                .match_targets()
                .filter_map(|operand| operand.value())
                .map(|target| target.id())
                .collect();
            let submatch_target_ids: Vec<_> = stmt
                .submatch_targets()
                .filter_map(|operand| operand.value())
                .map(|target| target.id())
                .collect();
            let result_target_ids: Vec<_> = stmt
                .results_targets()
                .filter_map(|target| target.value())
                .map(|value| value.id())
                .collect();

            if let Some(pattern_id) = pattern_id {
                read_ranges.push(self.collector.file.range(pattern_id));
                read_operand_ids.push(pattern_id);
            }
            if let Some(target_id) = target_id {
                read_ranges.push(self.collector.file.range(target_id));
                read_operand_ids.push(target_id);
            }

            for target_id in match_target_ids {
                write_targets.push(FindWriteTargetData {
                    range: self.collector.file.range(target_id),
                    definitely_assigned: true,
                });
                if self.collector.file.kind(target_id) == SyntaxKind::DataInlineDecl {
                    if !self.declare_find_match_inline_target(target_id, scope) {
                        self.collector
                            .decl_lowering()
                            .walk_inline_decl(target_id, scope);
                    }
                } else {
                    self.collector.walk_node(target_id, scope);
                }
            }

            for target_id in submatch_target_ids {
                write_targets.push(FindWriteTargetData {
                    range: self.collector.file.range(target_id),
                    definitely_assigned: true,
                });
                if self.collector.file.kind(target_id) == SyntaxKind::DataInlineDecl {
                    if !self.declare_find_submatch_inline_target(target_id, scope) {
                        self.collector
                            .decl_lowering()
                            .walk_inline_decl(target_id, scope);
                    }
                } else {
                    self.collector.walk_node(target_id, scope);
                }
            }

            let mut inline_result_targets = Vec::new();
            for value_id in result_target_ids {
                write_targets.push(FindWriteTargetData {
                    range: self.collector.file.range(value_id),
                    definitely_assigned: self.find_stmt_is_all_occurrences(node),
                });
                if self.collector.file.kind(value_id) == SyntaxKind::DataInlineDecl {
                    inline_result_targets.push(value_id);
                } else {
                    self.collector.walk_node(value_id, scope);
                }
            }

            for operand_id in read_operand_ids {
                self.collector.walk_node(operand_id, scope);
            }
            for value_id in inline_result_targets {
                if !self.declare_find_results_inline_target(node, value_id, scope) {
                    self.collector
                        .decl_lowering()
                        .walk_inline_decl(value_id, scope);
                }
            }

            self.collector.find_sites.push(FindSiteData {
                scope,
                range: self.collector.file.range(node),
                read_ranges,
                write_targets,
            });
        }
    }

    pub(super) fn collect_get_time_stamp_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        if self.collector.node_has_structured_children(node) {
            self.collector.walk_children(node, scope);
            return;
        }
        let mut significant = Vec::new();
        let mut inline_target = None;
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::Token => {
                    if let Some(token) = self.collector.syntax_token_nodes(child).into_iter().next()
                        && !self.collector.syntax_token_is_comment(&token)
                    {
                        significant.push(token);
                    }
                }
                SyntaxKind::DataInlineDecl => inline_target = Some(child),
                _ => self.collector.walk_node(child, scope),
            }
        }

        if let Some(inline_decl) = inline_target {
            self.collector
                .decl_lowering()
                .walk_inline_decl(inline_decl, scope);
            return;
        }

        if significant.len() > 4 {
            self.collector
                .collect_token_expression_refs_infos(&significant[4..], scope, true);
        }
    }

    pub(super) fn collect_methods_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let (type_refs, handler_infos) = {
            let methods_stmt =
                MethodsStmt::cast(self.collector.syntax(node)).expect("methods stmt");
            let type_refs = methods_stmt
                .type_refs()
                .map(|type_ref| type_ref.syntax().id())
                .collect::<Vec<_>>();
            let handler_infos = methods_stmt
                .entries(self.collector.source)
                .into_iter()
                .filter_map(|entry| {
                    let handler = entry.event_handler(self.collector.source)?;
                    let event_name = Arc::<str>::from(
                        handler
                            .event_name()
                            .text(self.collector.source)?
                            .to_ascii_lowercase(),
                    );
                    let source_type_name = Arc::<str>::from(
                        handler
                            .source_type_ref()
                            .display_text(self.collector.source)?
                            .to_ascii_lowercase(),
                    );
                    Some((
                        handler.event_qualifier().and_then(|token| {
                            Some((
                                Arc::<str>::from(
                                    token.text(self.collector.source)?.to_ascii_lowercase(),
                                ),
                                token.range(),
                            ))
                        }),
                        source_type_name,
                        handler.source_type_ref().syntax().range(),
                        event_name,
                        handler.event_name().range(),
                    ))
                })
                .collect::<Vec<_>>();
            (type_refs, handler_infos)
        };
        for type_ref in type_refs {
            self.collector
                .decl_lowering()
                .collect_type_ref(type_ref, scope);
        }
        for (qualifier, source_type_name, source_type_range, event_name, event_range) in
            handler_infos
        {
            if let Some((qualifier_name, qualifier_range)) = qualifier {
                self.collector.add_reference(
                    scope,
                    qualifier_name.clone(),
                    Namespace::Type,
                    ReferenceKind::TypeRef,
                    qualifier_range.clone(),
                );
                self.collector.emit_field_access(crate::FieldAccess {
                    scope,
                    base_namespace: Namespace::Type,
                    base_name: qualifier_name,
                    base_range: qualifier_range,
                    field_path: vec![crate::FieldAccessSegment {
                        name: event_name,
                        range: event_range,
                    }],
                    in_type_position: false,
                });
            } else {
                self.collector.emit_field_access(crate::FieldAccess {
                    scope,
                    base_namespace: Namespace::Type,
                    base_name: source_type_name,
                    base_range: source_type_range,
                    field_path: vec![crate::FieldAccessSegment {
                        name: event_name,
                        range: event_range,
                    }],
                    in_type_position: false,
                });
            }
        }
    }

    pub(super) fn collect_events_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let events_stmt = EventsStmt::cast(self.collector.syntax(node)).expect("events stmt");
        let type_refs: Vec<_> = events_stmt
            .type_refs()
            .map(|type_ref| type_ref.syntax().id())
            .collect();
        for type_ref in type_refs {
            self.collector
                .decl_lowering()
                .collect_type_ref(type_ref, scope);
        }
    }

    pub(super) fn collect_interfaces_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let Some(owner_symbol) = self.collector.class_lowering().enclosing_type_owner(scope) else {
            self.collector.walk_children(node, scope);
            return;
        };
        let mut recorded = false;
        for child in self.collector.file.children(node) {
            if self.collector.file.kind(child) != SyntaxKind::TypeRefSimple {
                continue;
            }
            self.collector
                .decl_lowering()
                .collect_type_ref(child, scope);
            let Some((_, _, interface_name, range, _)) =
                self.collector.type_ref_access_chain(child, Namespace::Type)
            else {
                continue;
            };
            if !self
                .collector
                .implemented_interfaces
                .iter()
                .any(|implemented| {
                    implemented.owner_symbol == owner_symbol
                        && implemented.interface_name == interface_name
                })
            {
                self.collector
                    .implemented_interfaces
                    .push(crate::ImplementedInterfaceData {
                        owner_symbol,
                        interface_name,
                        range,
                    });
            }
            recorded = true;
        }
        if !recorded {
            self.collector.walk_children(node, scope);
        }
    }

    pub(super) fn collect_assert_or_check_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        if self.collector.node_has_structured_children(node) {
            self.collector.walk_children(node, scope);
            return;
        }
        let significant = self.collector.significant_stmt_token_infos(node);
        let Some((_, tail)) = significant.split_first() else {
            return;
        };
        self.collector
            .collect_token_expression_refs_infos(tail, scope, true);
    }

    pub(super) fn collect_create_object_stmt_node(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        if let Some(stmt) = CreateObjectStmt::cast(self.collector.syntax(node)) {
            let target_id = stmt.target().map(|target| target.id());
            let type_ref_id = stmt.type_ref().map(|type_ref| type_ref.syntax().id());
            let arg_list_id = stmt.arg_list().map(|arg_list| arg_list.syntax().id());
            let mut constructor_target = None;
            if let Some(target_id) = target_id {
                self.collector
                    .expr_lowering()
                    .collect_expr(target_id, scope);
                if let Some(access) = self.collector.value_access_from_node(target_id, scope)
                    && let Some(symbol_id) = self.collector.lookup_symbol_in_scope_chain(
                        scope,
                        access.base_namespace,
                        access.base_name.as_ref(),
                    )
                    && let Some(declared_type) =
                        self.collector.symbol(symbol_id).declared_type.as_ref()
                    && declared_type.is_ref
                    && declared_type.namespace == Namespace::Type
                    && declared_type.field_path.is_empty()
                {
                    constructor_target = Some(NamedArgumentTarget::Constructor {
                        type_name: Arc::clone(&declared_type.base_name),
                    });
                }
            }
            if let Some(type_ref_id) = type_ref_id {
                self.collector
                    .decl_lowering()
                    .collect_type_ref(type_ref_id, scope);
                if let Some((namespace, _, base_name, _, field_path)) = self
                    .collector
                    .type_ref_access_chain(type_ref_id, Namespace::Type)
                    && namespace == Namespace::Type
                    && field_path.is_empty()
                {
                    constructor_target = Some(NamedArgumentTarget::Constructor {
                        type_name: base_name,
                    });
                }
            }
            if let Some(arg_list_id) = arg_list_id {
                let call_range = self.collector.file.range(node);
                if let Some(target) = constructor_target {
                    self.collector.expr_lowering().collect_call_argument_list(
                        arg_list_id,
                        scope,
                        target,
                        call_range,
                    );
                } else {
                    self.collector
                        .expr_lowering()
                        .collect_structured_argument_values_from_children(arg_list_id, scope);
                }
            }
            return;
        }
        let significant = self.collector.significant_stmt_token_infos(node);
        self.collect_create_object_stmt_infos(&significant, scope);
    }

    pub(super) fn collect_create_data_stmt_node(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        if let Some(stmt) = CreateDataStmt::cast(self.collector.syntax(node)) {
            let target_id = stmt.target().map(|target| target.id());
            let clause_kind = stmt.type_clause_kind(self.collector.source);
            let type_ref_id = stmt.type_ref().map(|type_ref| type_ref.syntax().id());
            let type_value_id = stmt
                .type_value(self.collector.source)
                .map(|value| value.id());
            if let Some(target_id) = target_id {
                self.collector
                    .expr_lowering()
                    .collect_expr(target_id, scope);
            }
            match clause_kind {
                Some(abap_ast::ast::TypeClauseKind::Type) => {
                    if let Some(type_ref_id) = type_ref_id {
                        self.collector
                            .decl_lowering()
                            .collect_type_ref(type_ref_id, scope);
                    } else if let Some(value_id) = type_value_id {
                        self.collector.walk_node(value_id, scope);
                    }
                }
                Some(abap_ast::ast::TypeClauseKind::Like) => {
                    if let Some(value_id) = type_value_id {
                        self.collector.walk_node(value_id, scope);
                        if let Some(access) = self.collector.value_access_from_node(value_id, scope)
                        {
                            self.collector.add_reference(
                                scope,
                                access.base_name,
                                access.base_namespace,
                                ReferenceKind::TypeRef,
                                self.collector.file.range(value_id),
                            );
                        }
                    }
                }
                Some(abap_ast::ast::TypeClauseKind::For) => {}
                None => {}
            }
            return;
        }
        let significant = self.collector.significant_stmt_token_infos(node);
        self.collect_create_data_stmt_infos(&significant, scope);
    }

    pub(super) fn collect_call_method_stmt_node(&mut self, node: NodeId, scope: ScopeId) {
        if let Some(stmt) = CallMethodStmt::cast(self.collector.syntax(node)) {
            let target_node_id = stmt
                .target()
                .and_then(|target_node| target_node.callee().map(|callee| callee.id()));
            let arg_list_id = stmt.arg_list().map(|arg_list| arg_list.syntax().id());
            let mut target = None;
            if let Some(mut callee) = target_node_id {
                while self.collector.file.kind(callee) == SyntaxKind::TemplateExpr {
                    let Some(inner) = self.collector.first_non_token_child(callee) else {
                        break;
                    };
                    callee = inner;
                }
                match self.collector.file.kind(callee) {
                    SyntaxKind::ExprIdent => {
                        let Some((method_name, range)) = self.collector.node_name(callee) else {
                            return;
                        };
                        self.collector.add_reference(
                            scope,
                            Arc::clone(&method_name),
                            Namespace::Routine,
                            ReferenceKind::RoutineCall,
                            range,
                        );
                        target = Some(NamedArgumentTarget::ImplicitMethod { method_name });
                    }
                    SyntaxKind::CallExpr => {
                        self.collector
                            .expr_lowering()
                            .collect_call_expr(callee, scope);
                        return;
                    }
                    SyntaxKind::SelectorExpr => {
                        self.collector
                            .expr_lowering()
                            .collect_selector_expr(callee, scope);
                        target = self.collector.named_argument_target_for_callee(callee);
                    }
                    _ => self.collector.expr_lowering().collect_expr(callee, scope),
                }
            }
            if let Some(target) = target {
                let call_range = self.collector.file.range(node);
                if let Some(arg_list_id) = arg_list_id {
                    self.collector.expr_lowering().collect_call_argument_list(
                        arg_list_id,
                        scope,
                        target,
                        call_range,
                    );
                } else {
                    self.collector.emit_call_site(CallSiteData {
                        scope,
                        range: call_range,
                        target,
                        arguments: Vec::new(),
                    });
                }
                return;
            }
            if target_node_id.is_some() {
                if let Some(arg_list_id) = arg_list_id {
                    self.collector
                        .expr_lowering()
                        .collect_structured_argument_values_from_children(arg_list_id, scope);
                }
                return;
            }
        }

        let significant = self.collector.significant_stmt_token_infos(node);
        self.collect_call_method_stmt_infos(&significant, scope);
    }

    pub(super) fn collect_call_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let Some(stmt) = CallStmt::cast(self.collector.syntax(node)) else {
            self.collector.walk_children(node, scope);
            return;
        };
        let stmt_range = self.collector.file.range(node);

        let call_kind = stmt.call_kind(self.collector.source);

        if call_kind == Some(CallStmtKind::Screen) || call_kind == Some(CallStmtKind::Transaction) {
            self.record_unknown_effect(node, scope);
            let significant = self.collector.significant_stmt_token_infos(node);
            if call_kind == Some(CallStmtKind::Screen) {
                self.collect_call_screen_stmt_infos(&significant, scope);
            } else {
                self.collect_call_transaction_stmt_infos(&significant, scope);
            }
            return;
        }

        if call_kind == Some(CallStmtKind::SystemFunction) {
            self.record_unknown_effect(node, scope);
            for child in self.collector.file.children(node) {
                match self.collector.file.kind(child) {
                    SyntaxKind::CallArgList => self
                        .collector
                        .expr_lowering()
                        .collect_structured_argument_values_from_children(child, scope),
                    SyntaxKind::Token => {}
                    _ => self.collector.walk_node(child, scope),
                }
            }
            return;
        }

        let function_name = if call_kind == Some(CallStmtKind::Function) {
            let function_info = stmt.callee_token().and_then(|token| {
                let range = token.range();
                let name = token.text(self.collector.source)?;
                let name = name.trim();
                let unquoted = name
                    .strip_prefix('\'')
                    .and_then(|name| name.strip_suffix('\''))
                    .or_else(|| {
                        name.strip_prefix('`')
                            .and_then(|name| name.strip_suffix('`'))
                    })
                    .unwrap_or(name);
                Some((range, Arc::<str>::from(unquoted.to_ascii_lowercase())))
            });
            function_info.map(|(range, function_name)| {
                self.collector.add_reference(
                    scope,
                    Arc::clone(&function_name),
                    Namespace::Routine,
                    ReferenceKind::RoutineCall,
                    range,
                );
                function_name
            })
        } else {
            None
        };
        let mut emitted_function_call_site = false;

        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::CallArgList => {
                    if let Some(function_name) = function_name.clone() {
                        self.collector.expr_lowering().collect_call_argument_list(
                            child,
                            scope,
                            NamedArgumentTarget::Function { function_name },
                            stmt_range.clone(),
                        );
                        emitted_function_call_site = true;
                    } else {
                        self.collector
                            .expr_lowering()
                            .collect_structured_argument_values_from_children(child, scope);
                    }
                }
                SyntaxKind::CallExpr => {
                    self.collector.walk_node(child, scope);
                }
                SyntaxKind::Token => {}
                _ => self.collector.walk_node(child, scope),
            }
        }

        if let Some(function_name) = function_name
            && !emitted_function_call_site
        {
            self.collector.emit_call_site(CallSiteData {
                scope,
                range: stmt_range,
                target: NamedArgumentTarget::Function { function_name },
                arguments: Vec::new(),
            });
        }
    }

    pub(super) fn collect_submit_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let Some(stmt) = SubmitStmt::cast(self.collector.syntax(node)) else {
            self.collector.walk_children(node, scope);
            return;
        };

        let tokens = self.collector.significant_stmt_token_infos(node);
        if tokens.len() < 3 || !tokens[0].text.eq_ignore_ascii_case("submit") {
            self.collector.walk_children(node, scope);
            return;
        }
        let Some(period_idx) = tokens.iter().position(|token| token.text.as_ref() == ".") else {
            self.record_unknown_effect(node, scope);
            self.collector.walk_children(node, scope);
            return;
        };

        let stmt_range = self.collector.file.range(node);
        let mut idx = 1usize;
        let mut has_and_return = false;
        let mut report_name: Option<Arc<str>> = None;

        if tokens
            .get(idx)
            .is_some_and(|token| token.text.as_ref() == "(")
        {
            if let Some(end_idx) = self
                .collector
                .find_matching_group_end_infos(&tokens, idx, "(", ")")
            {
                self.collect_token_expression_refs_range(&tokens, idx + 1, end_idx, scope);
                idx = end_idx + 1;
            } else {
                idx += 1;
            }
        } else if let Some(target) = stmt.target_token()
            && let Some(name) = target.lower_trimmed_text(self.collector.source)
        {
            self.collector.add_reference(
                scope,
                Arc::clone(&name),
                Namespace::Value,
                ReferenceKind::Identifier,
                target.range(),
            );
            report_name = Some(name);
            idx += 1;
        }

        while idx < period_idx {
            if Self::tokens_match_keyword_sequence(&tokens[idx..], &["and", "return"]) {
                has_and_return = true;
                idx += 2;
                continue;
            }
            if Self::tokens_match_keyword_sequence(
                &tokens[idx..],
                &["using", "selection", "-", "screen"],
            ) {
                idx = self.collect_positional_operand_tokens(&tokens, idx + 4, 1, scope);
                continue;
            }
            if Self::tokens_match_keyword_sequence(
                &tokens[idx..],
                &["using", "selection", "-", "set"],
            ) {
                idx = self.collect_positional_operand_tokens(&tokens, idx + 4, 1, scope);
                continue;
            }
            if Self::tokens_match_keyword_sequence(
                &tokens[idx..],
                &["using", "selection", "-", "sets", "of", "program"],
            ) {
                idx = self.collect_positional_operand_tokens(&tokens, idx + 6, 1, scope);
                continue;
            }
            if Self::tokens_match_keyword_sequence(
                &tokens[idx..],
                &["with", "selection", "-", "table"],
            ) {
                idx = self.collect_positional_operand_tokens(&tokens, idx + 4, 1, scope);
                continue;
            }
            if Self::tokens_match_keyword_sequence(&tokens[idx..], &["with", "free", "selections"])
            {
                idx = self.collect_positional_operand_tokens(&tokens, idx + 3, 1, scope);
                continue;
            }
            if Self::tokens_match_keyword_sequence(&tokens[idx..], &["with"]) {
                let mut clause_idx = self.consume_simple_operand_tokens(&tokens, idx + 1);
                if clause_idx >= period_idx {
                    idx = period_idx;
                    continue;
                }
                if tokens
                    .get(clause_idx)
                    .is_some_and(|token| token.text.eq_ignore_ascii_case("not"))
                {
                    clause_idx += 1;
                }
                if tokens
                    .get(clause_idx)
                    .is_some_and(|token| token.text.eq_ignore_ascii_case("between"))
                {
                    clause_idx =
                        self.collect_positional_operand_tokens(&tokens, clause_idx + 1, 1, scope);
                    if tokens
                        .get(clause_idx)
                        .is_some_and(|token| token.text.eq_ignore_ascii_case("and"))
                    {
                        clause_idx = self.collect_positional_operand_tokens(
                            &tokens,
                            clause_idx + 1,
                            1,
                            scope,
                        );
                    }
                } else {
                    if clause_idx < period_idx {
                        clause_idx += 1;
                    }
                    clause_idx =
                        self.collect_positional_operand_tokens(&tokens, clause_idx, 1, scope);
                }
                if tokens
                    .get(clause_idx)
                    .is_some_and(|token| token.text.eq_ignore_ascii_case("sign"))
                {
                    clause_idx =
                        self.collect_positional_operand_tokens(&tokens, clause_idx + 1, 1, scope);
                }
                idx = clause_idx;
                continue;
            }
            if Self::tokens_match_keyword_sequence(&tokens[idx..], &["line", "-", "size"])
                || Self::tokens_match_keyword_sequence(&tokens[idx..], &["line", "-", "count"])
            {
                idx = self.collect_positional_operand_tokens(&tokens, idx + 3, 1, scope);
                continue;
            }
            if Self::tokens_match_keyword_sequence(
                &tokens[idx..],
                &["to", "sap", "-", "spool", "spool", "parameters"],
            ) {
                idx = self.collect_positional_operand_tokens(&tokens, idx + 6, 1, scope);
                if Self::tokens_match_keyword_sequence(&tokens[idx..], &["archive", "parameters"]) {
                    idx = self.collect_positional_operand_tokens(&tokens, idx + 2, 1, scope);
                }
                continue;
            }
            if Self::tokens_match_keyword_sequence(&tokens[idx..], &["user"]) {
                idx = self.collect_positional_operand_tokens(&tokens, idx + 1, 1, scope);
                continue;
            }
            if Self::tokens_match_keyword_sequence(&tokens[idx..], &["via", "job"]) {
                idx = self.collect_positional_operand_tokens(&tokens, idx + 2, 1, scope);
                if Self::tokens_match_keyword_sequence(&tokens[idx..], &["number"]) {
                    idx = self.collect_positional_operand_tokens(&tokens, idx + 1, 1, scope);
                }
                if Self::tokens_match_keyword_sequence(&tokens[idx..], &["language"]) {
                    idx = self.collect_positional_operand_tokens(&tokens, idx + 1, 1, scope);
                }
                continue;
            }
            idx += 1;
        }

        if let Some(report_name) = report_name {
            self.collector.emit_call_site(CallSiteData {
                scope,
                range: stmt_range.clone(),
                target: NamedArgumentTarget::Report { report_name },
                arguments: Vec::new(),
            });
        }

        if has_and_return {
            self.record_unknown_effect(node, scope);
        } else {
            self.record_routine_site(scope, stmt_range, RoutineSiteKind::Leave);
        }
    }

    fn collect_call_method_stmt_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        if tokens.len() < 3
            || !tokens[0].text.eq_ignore_ascii_case("call")
            || !tokens[1].text.eq_ignore_ascii_case("method")
        {
            return;
        }

        let mut idx = 2usize;
        let target =
            if let Some((next_idx, namespace, base_name, base_range, field_path, bracket_groups)) =
                self.collector
                    .consume_selector_access_from_infos(tokens, idx)
            {
                for (group_start, group_end, is_legacy_table_body) in bracket_groups {
                    if is_legacy_table_body {
                        continue;
                    }
                    self.collector.collect_token_expression_refs_infos(
                        &tokens[group_start + 1..group_end],
                        scope,
                        true,
                    );
                }
                let Some(method_name) = field_path.last().map(|segment| Arc::clone(&segment.name))
                else {
                    return;
                };
                let kind = if namespace == Namespace::Type {
                    ReferenceKind::StaticTarget
                } else {
                    ReferenceKind::Identifier
                };
                self.collector.add_reference(
                    scope,
                    Arc::clone(&base_name),
                    namespace,
                    kind,
                    base_range.clone(),
                );
                self.collector.emit_field_access(crate::FieldAccess {
                    scope,
                    base_namespace: namespace,
                    base_name: Arc::clone(&base_name),
                    base_range: base_range.clone(),
                    field_path,
                    in_type_position: false,
                });
                idx = next_idx;
                NamedArgumentTarget::Method {
                    base_namespace: namespace,
                    base_name,
                    method_name,
                }
            } else {
                let Some(token) = tokens.get(idx) else {
                    return;
                };
                if !self.collector.syntax_token_is_ident_like(token) {
                    return;
                }
                let method_name = Arc::<str>::from(token.text.to_ascii_lowercase());
                self.collector.add_reference(
                    scope,
                    Arc::clone(&method_name),
                    Namespace::Routine,
                    ReferenceKind::RoutineCall,
                    token.range.clone(),
                );
                idx += 1;
                NamedArgumentTarget::ImplicitMethod { method_name }
            };

        self.collector
            .expr_lowering()
            .collect_call_arguments_from_infos(
                &tokens[idx..],
                scope,
                target,
                tokens[0].range.start
                    ..tokens
                        .last()
                        .map(|token| token.range.end)
                        .unwrap_or(tokens[0].range.end),
            );
    }

    pub(super) fn collect_assign_keyword_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_system_field_updates(scope, node, SystemFieldStatementKind::Assign, &["subrc"]);
        let mut source_expr = None;
        let mut inline_targets = Vec::new();
        let mut named_target = None;
        let mut in_casting_clause = false;
        let mut casting_type_ref = None;
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::Token => {
                    if self
                        .collector
                        .syntax_token_nodes(child)
                        .into_iter()
                        .next()
                        .is_some_and(|token| token.text.eq_ignore_ascii_case("casting"))
                    {
                        in_casting_clause = true;
                    }
                }
                SyntaxKind::AssignSourceExpr => {
                    let Some(expr) = self.collector.first_non_token_child(child) else {
                        continue;
                    };
                    source_expr = Some(expr);
                    self.collector.expr_lowering().collect_expr(expr, scope);
                }
                SyntaxKind::FieldSymbolInlineDecl => inline_targets.push(child),
                SyntaxKind::TypeRefSimple if in_casting_clause => {
                    casting_type_ref = Some(child);
                    self.collector.walk_node(child, scope);
                }
                _ => {
                    if named_target.is_none() {
                        named_target = self.direct_field_symbol_target(child, scope);
                    }
                    self.collector.walk_node(child, scope);
                }
            }
        }

        let mut inferred_metadata = source_expr
            .map(|expr| {
                self.collector
                    .control_lowering()
                    .loop_source_line_metadata_from_node(expr, scope)
            })
            .unwrap_or((None, None));
        if let Some(type_ref_node) = casting_type_ref
            && let Some(type_ref) = self
                .collector
                .field_type_ref_from_node(type_ref_node, Namespace::Type)
        {
            inferred_metadata = (
                self.collector.resolve_field_type_ref(scope, &type_ref),
                Some(type_ref),
            );
        }
        let flow_kind = self.assign_keyword_binding_kind(node, scope, source_expr);
        for target in inline_targets {
            let target_name = self
                .collector
                .file
                .children(target)
                .find(|&child| self.collector.file.kind(child) == SyntaxKind::DataDeclName)
                .and_then(|child| self.collector.node_name(child));
            self.collector
                .decl_lowering()
                .declare_inline_field_symbol_decl(
                    target,
                    scope,
                    inferred_metadata.0,
                    inferred_metadata.1.clone(),
                    None,
                );
            if let Some(source_expr) = source_expr
                && let Some((target_name, target_range)) = target_name.clone()
            {
                self.emit_field_symbol_binding_edge(
                    scope,
                    flow_kind,
                    Some(source_expr),
                    inferred_metadata.0,
                    inferred_metadata.1.clone(),
                    target_name,
                    target_range,
                );
            }
        }
        if let Some((target_name, target_range)) = named_target {
            self.emit_field_symbol_binding_edge(
                scope,
                flow_kind,
                source_expr,
                inferred_metadata.0,
                inferred_metadata.1,
                target_name,
                target_range,
            );
        }
    }

    pub(super) fn collect_write_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        if let Some(stmt) = WriteStmt::cast(self.collector.syntax(node)) {
            let operand_ids: Vec<_> = stmt.operands().map(|child| child.id()).collect();
            if !operand_ids.is_empty() {
                for child in operand_ids {
                    self.collector.walk_node(child, scope);
                }
                return;
            }
        }
        let significant = self.collector.significant_stmt_token_infos(node);
        let Some((_, tail)) = significant.split_first() else {
            return;
        };
        self.collector
            .collect_token_expression_refs_infos(tail, scope, true);
    }

    pub(super) fn collect_split_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        if SplitStmt::cast(self.collector.syntax(node)).is_some() {
            let children: Vec<_> = self.collector.file.children(node).collect();
            let mut split_target_byte_modes = Vec::new();
            let mut entry_start = 0usize;
            while entry_start < children.len() {
                let mut entry_end = entry_start;
                while entry_end < children.len() {
                    let child = children[entry_end];
                    if self.collector.file.kind(child) == SyntaxKind::Token
                        && self
                            .collector
                            .syntax_token_nodes(child)
                            .into_iter()
                            .next()
                            .is_some_and(|token| token.text.as_ref() == ",")
                    {
                        break;
                    }
                    entry_end += 1;
                }
                let byte_mode = children[entry_start..entry_end].iter().any(|&child| {
                    self.collector.file.kind(child) == SyntaxKind::Token
                        && self
                            .collector
                            .syntax_token_nodes(child)
                            .into_iter()
                            .next()
                            .is_some_and(|token| token.text.eq_ignore_ascii_case("byte"))
                });
                for &child in &children[entry_start..entry_end] {
                    if self.collector.file.kind(child) == SyntaxKind::SplitTargetOperand {
                        split_target_byte_modes.push((child, byte_mode));
                    }
                }
                entry_start = entry_end.saturating_add(1);
            }
            let mut seen_into = false;
            let mut into_table = false;
            for child in children {
                match self.collector.file.kind(child) {
                    SyntaxKind::Token => {
                        if let Some(token) =
                            self.collector.syntax_token_nodes(child).into_iter().next()
                        {
                            if token.text.eq_ignore_ascii_case("into") {
                                seen_into = true;
                                into_table = false;
                            } else if seen_into && token.text.eq_ignore_ascii_case("table") {
                                into_table = true;
                            } else if token.text.as_ref() == "," {
                                seen_into = false;
                                into_table = false;
                            }
                        }
                    }
                    SyntaxKind::SplitSourceOperand | SyntaxKind::SplitSeparatorOperand => {
                        if let Some(value) = self.collector.first_non_token_child(child) {
                            self.collector.walk_node(value, scope);
                        }
                    }
                    SyntaxKind::SplitTargetOperand => {
                        let Some(value) = self.collector.first_non_token_child(child) else {
                            continue;
                        };
                        if self.collector.file.kind(value) == SyntaxKind::DataInlineDecl {
                            let byte_mode = split_target_byte_modes
                                .iter()
                                .find_map(|(target, byte_mode)| {
                                    (*target == child).then_some(*byte_mode)
                                })
                                .unwrap_or(false);
                            self.declare_split_inline_data_target(
                                value, scope, into_table, byte_mode,
                            );
                        } else {
                            self.collector.walk_node(value, scope);
                        }
                    }
                    _ => self.collector.walk_node(child, scope),
                }
            }
            return;
        }
    }

    pub(super) fn collect_concatenate_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        if ConcatenateStmt::cast(self.collector.syntax(node)).is_some() {
            let stmt_range = self.collector.file.range(node);
            let children: Vec<_> = self.collector.file.children(node).collect();
            let mut entry_start = 0usize;
            while entry_start < children.len() {
                let mut entry_end = entry_start;
                while entry_end < children.len() {
                    let child = children[entry_end];
                    if self.collector.file.kind(child) == SyntaxKind::Token
                        && self
                            .collector
                            .syntax_token_nodes(child)
                            .into_iter()
                            .next()
                            .is_some_and(|token| token.text.as_ref() == ",")
                    {
                        break;
                    }
                    entry_end += 1;
                }

                let byte_mode = children[entry_start..entry_end].iter().any(|&child| {
                    self.collector.file.kind(child) == SyntaxKind::Token
                        && self
                            .collector
                            .syntax_token_nodes(child)
                            .into_iter()
                            .next()
                            .is_some_and(|token| token.text.eq_ignore_ascii_case("byte"))
                });
                let operand_ids: Vec<_> = children[entry_start..entry_end]
                    .iter()
                    .copied()
                    .filter(|&child| {
                        self.collector.file.kind(child) == SyntaxKind::ConcatenateSourceOperand
                    })
                    .filter_map(|operand| self.collector.first_non_token_child(operand))
                    .collect();
                let target_id = children[entry_start..entry_end]
                    .iter()
                    .copied()
                    .find(|&child| {
                        self.collector.file.kind(child) == SyntaxKind::ConcatenateTargetOperand
                    })
                    .and_then(|target| self.collector.first_non_token_child(target));
                let separator_id = children[entry_start..entry_end]
                    .iter()
                    .copied()
                    .find(|&child| {
                        self.collector.file.kind(child) == SyntaxKind::ConcatenateSeparatorOperand
                    })
                    .and_then(|separator| self.collector.first_non_token_child(separator));

                for &operand in &operand_ids {
                    self.collector.walk_node(operand, scope);
                }
                if let Some(target) = target_id {
                    if self.collector.file.kind(target) == SyntaxKind::DataInlineDecl {
                        self.declare_concatenate_inline_data_target(target, scope, byte_mode);
                    } else {
                        self.collector.walk_node(target, scope);
                    }
                }
                if let Some(separator) = separator_id {
                    self.collector.walk_node(separator, scope);
                }
                if let Some(target) = target_id {
                    let mut rhs_nodes = operand_ids;
                    if let Some(separator) = separator_id {
                        rhs_nodes.push(separator);
                    }
                    self.emit_assignment_site_from_ranges(
                        scope,
                        stmt_range.clone(),
                        target,
                        &rhs_nodes,
                    );
                }

                entry_start = entry_end.saturating_add(1);
            }
            return;
        }
    }

    pub(super) fn collect_update_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_unknown_effect(node, scope);
        self.collector
            .sql_lowering()
            .collect_update_db_table_stmt(node, scope);
    }

    pub(super) fn collect_create_object_stmt_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        if tokens.len() < 3
            || !tokens[0].text.eq_ignore_ascii_case("create")
            || !tokens[1].text.eq_ignore_ascii_case("object")
        {
            return;
        }

        let target = &tokens[2];
        if self.collector.syntax_token_is_ident_like(target) {
            let name = Arc::<str>::from(target.text.to_ascii_lowercase());
            self.collector.add_reference(
                scope,
                name,
                Namespace::Value,
                ReferenceKind::Identifier,
                target.range.clone(),
            );
        }

        for idx in 3..tokens.len() {
            let token = &tokens[idx];
            if !token.text.eq_ignore_ascii_case("type") {
                continue;
            }
            if let Some((name, range)) = self
                .collector
                .simple_type_ref_base_from_infos(&tokens[idx + 1..])
            {
                self.collector.add_reference(
                    scope,
                    name,
                    Namespace::Type,
                    ReferenceKind::TypeRef,
                    range,
                );
            }
            break;
        }
    }

    pub(super) fn collect_create_data_stmt_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        if tokens.len() < 4
            || !tokens[0].text.eq_ignore_ascii_case("create")
            || !tokens[1].text.eq_ignore_ascii_case("data")
        {
            return;
        }

        let target = &tokens[2];
        if self.collector.syntax_token_is_ident_like(target) {
            let name = Arc::<str>::from(target.text.to_ascii_lowercase());
            self.collector.add_reference(
                scope,
                name,
                Namespace::Value,
                ReferenceKind::Identifier,
                target.range.clone(),
            );
        }

        for idx in 3..tokens.len() {
            let token = &tokens[idx];
            if !token.text.eq_ignore_ascii_case("type") && !token.text.eq_ignore_ascii_case("like")
            {
                continue;
            }
            let tail = &tokens[idx + 1..];
            if token.text.eq_ignore_ascii_case("type")
                && tail.first().is_some_and(|token| token.text.as_ref() == "(")
            {
                self.collector
                    .collect_token_expression_refs_infos(tail, scope, true);
            } else if let Some((name, range)) = self.collector.simple_type_ref_base_from_infos(tail)
            {
                self.collector.add_reference(
                    scope,
                    name,
                    if token.text.eq_ignore_ascii_case("like") {
                        Namespace::Value
                    } else {
                        Namespace::Type
                    },
                    ReferenceKind::TypeRef,
                    range,
                );
            }
            break;
        }
    }
}
