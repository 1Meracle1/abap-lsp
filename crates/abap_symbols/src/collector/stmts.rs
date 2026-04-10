use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_ast::ast::{AstNode, MethodsStmt};

use crate::def_map::{FieldTypeRefData, NamedArgumentTarget, ReferenceKind, SymbolKind};
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

    fn declare_describe_lines_inline_target_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
        end: usize,
        scope: ScopeId,
    ) -> bool {
        let mut idx = start;
        while idx < end
            && tokens
                .get(idx)
                .is_some_and(|token| self.collector.syntax_token_is_comment(token))
        {
            idx += 1;
        }

        let Some(head) = tokens.get(idx) else {
            return false;
        };
        if !head.text.eq_ignore_ascii_case("data") {
            return false;
        }
        if tokens.get(idx + 1).map(|token| token.text.as_ref()) != Some("(") {
            return false;
        }
        let Some(name_tok) = tokens.get(idx + 2) else {
            return false;
        };
        if !self.collector.syntax_token_is_ident_like(name_tok) {
            return false;
        }
        if tokens.get(idx + 3).map(|token| token.text.as_ref()) != Some(")") {
            return false;
        }

        let mut tail = idx + 4;
        while tail < end && self.collector.syntax_token_is_comment(&tokens[tail]) {
            tail += 1;
        }
        if tail != end {
            return false;
        }

        self.collector.declare_symbol(
            self.collector.declaration_scope(scope),
            Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
            SymbolKind::Variable,
            name_tok.range.clone(),
            None,
            Some(Self::builtin_type("i")),
            None,
            None,
        );
        true
    }

    pub(super) fn collect_delete_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let mut where_expr = None;
        let mut seen_where = false;
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::Token => {
                    if self
                        .collector
                        .syntax(child)
                        .text(self.collector.source)
                        .is_some_and(|text| text.eq_ignore_ascii_case("where"))
                    {
                        seen_where = true;
                    }
                }
                _ => {
                    if seen_where && where_expr.is_none() {
                        where_expr = Some(child);
                    }
                    self.collector.walk_node(child, scope);
                }
            }
        }

        if !seen_where {
            return;
        }
        let Some(source_expr) = self.collector.first_non_token_child(node) else {
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

    fn call_function_name_from_tokens(tokens: &[SyntaxTokenInfo]) -> Option<Arc<str>> {
        let name = tokens.get(2)?.text.as_ref().trim();
        let unquoted = name
            .strip_prefix('\'')
            .and_then(|name| name.strip_suffix('\''))
            .or_else(|| {
                name.strip_prefix('`')
                    .and_then(|name| name.strip_suffix('`'))
            })
            .unwrap_or(name);
        Some(Arc::<str>::from(unquoted.to_ascii_lowercase()))
    }

    pub(super) fn collect_read_table_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.collector.node_has_structured_children(node) {
            let mut source_expr = None;
            let mut target_kind = None;
            let mut data_inline_targets = Vec::new();
            let mut field_symbol_targets = Vec::new();
            for child in self.collector.file.children(node) {
                match self.collector.file.kind(child) {
                    SyntaxKind::Token => {
                        if let Some(token) =
                            self.collector.syntax_token_nodes(child).into_iter().next()
                        {
                            if token.text.eq_ignore_ascii_case("into") {
                                target_kind = Some("into");
                            } else if token.text.eq_ignore_ascii_case("assigning") {
                                target_kind = Some("assigning");
                            }
                        }
                    }
                    SyntaxKind::DataInlineDecl => data_inline_targets.push(child),
                    SyntaxKind::FieldSymbolInlineDecl => field_symbol_targets.push(child),
                    _ => {
                        if source_expr.is_none() {
                            source_expr = Some(child);
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
                for node in data_inline_targets {
                    if let Some(name_node) =
                        self.collector.file.children(node).find(|&child| {
                            self.collector.file.kind(child) == SyntaxKind::DataDeclName
                        })
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

            if target_kind == Some("assigning") {
                for target in field_symbol_targets {
                    self.collector
                        .decl_lowering()
                        .declare_inline_field_symbol_decl(
                            target,
                            scope,
                            inferred_metadata.0,
                            inferred_metadata.1.clone(),
                        );
                }
            }
            return;
        }
        self.collector.walk_children(node, scope);
    }

    pub(super) fn collect_message_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.collector.node_has_structured_children(node) {
            let mut inline_target = None;
            for child in self.collector.file.children(node) {
                match self.collector.file.kind(child) {
                    SyntaxKind::Token => {}
                    SyntaxKind::DataInlineDecl => inline_target = Some(child),
                    _ => self.collector.walk_node(child, scope),
                }
            }
            if let Some(inline_decl) = inline_target {
                self.collector
                    .decl_lowering()
                    .walk_inline_decl(inline_decl, scope);
            }
        }

        let sig = self.collector.significant_stmt_token_infos(node);
        if sig.is_empty() || !sig[0].text.eq_ignore_ascii_case("message") {
            return;
        }
        let period_pos = sig
            .iter()
            .position(|t| t.text.as_ref() == ".")
            .unwrap_or(sig.len());

        let with_ix = self
            .collector
            .find_top_level_keyword_index_infos(&sig, 1, "with")
            .filter(|&ix| ix < period_pos);
        let into_ix = self
            .collector
            .find_top_level_keyword_index_infos(&sig, 1, "into")
            .filter(|&ix| ix < period_pos);
        let display_ix = self
            .collector
            .find_top_level_keyword_index_infos(&sig, 1, "display")
            .filter(|&ix| ix < period_pos);
        let raising_ix = self
            .collector
            .find_top_level_keyword_index_infos(&sig, 1, "raising")
            .filter(|&ix| ix < period_pos);

        let head_end = [with_ix, into_ix, display_ix, raising_ix, Some(period_pos)]
            .into_iter()
            .flatten()
            .min()
            .unwrap();

        if sig
            .get(1)
            .is_some_and(|t| t.text.eq_ignore_ascii_case("id"))
        {
            let mut i = 2usize;
            let end_mid = self.collector.consume_concatenate_operand_infos(
                &sig,
                i,
                &["type", "with", "into", "display", "raising"],
            );
            if end_mid > i {
                if let Some((name, range)) = self
                    .collector
                    .simple_type_ref_base_from_infos(&sig[i..end_mid])
                {
                    self.collector.add_reference(
                        scope,
                        name,
                        Namespace::Type,
                        ReferenceKind::MessageClass,
                        range,
                    );
                } else {
                    self.collect_message_operand_refs_infos(&sig[i..end_mid], scope);
                }
            }
            i = end_mid;
            if i < head_end
                && sig
                    .get(i)
                    .is_some_and(|t| t.text.eq_ignore_ascii_case("type"))
            {
                i += 1;
                let end_ty = self.collector.consume_concatenate_operand_infos(
                    &sig,
                    i,
                    &["number", "with", "into", "display", "raising"],
                );
                if end_ty > i {
                    self.collect_message_operand_refs_infos(&sig[i..end_ty], scope);
                }
                i = end_ty;
            }
            if i < head_end
                && sig
                    .get(i)
                    .is_some_and(|t| t.text.eq_ignore_ascii_case("number"))
            {
                i += 1;
                let end_num = self.collector.consume_concatenate_operand_infos(
                    &sig,
                    i,
                    &["with", "into", "display", "raising"],
                );
                if end_num > i {
                    self.collect_message_operand_refs_infos(&sig[i..end_num], scope);
                }
                i = end_num;
            }
            if i < head_end
                && sig
                    .get(i)
                    .is_some_and(|t| t.text.eq_ignore_ascii_case("display"))
                && sig
                    .get(i + 1)
                    .is_some_and(|t| t.text.eq_ignore_ascii_case("like"))
            {
                i += 2;
                let end_disp = self.collector.consume_concatenate_operand_infos(
                    &sig,
                    i,
                    &["with", "into", "raising"],
                );
                if end_disp > i {
                    self.collect_message_operand_refs_infos(&sig[i..end_disp], scope);
                }
            }
        } else {
            let mut i = 1usize;
            let code_end = self.collector.consume_concatenate_operand_infos(
                &sig,
                i,
                &["type", "with", "into", "display", "raising"],
            );
            if code_end > i {
                if self.is_compact_message_class_form(&sig[i..code_end]) {
                    self.collect_compact_message_class_ref_infos(&sig[i..code_end], scope);
                } else {
                    self.collect_message_operand_refs_infos(&sig[i..code_end], scope);
                }
            }
            i = code_end;
            if i < head_end
                && sig
                    .get(i)
                    .is_some_and(|t| t.text.eq_ignore_ascii_case("type"))
            {
                i += 1;
                let ty_end = self.collector.consume_concatenate_operand_infos(
                    &sig,
                    i,
                    &["with", "into", "display", "raising"],
                );
                if ty_end > i {
                    self.collect_message_operand_refs_infos(&sig[i..ty_end], scope);
                }
                i = ty_end;
            }
            if i < head_end
                && sig
                    .get(i)
                    .is_some_and(|t| t.text.eq_ignore_ascii_case("display"))
                && sig
                    .get(i + 1)
                    .is_some_and(|t| t.text.eq_ignore_ascii_case("like"))
            {
                i += 2;
                let disp_end = self.collector.consume_concatenate_operand_infos(
                    &sig,
                    i,
                    &["with", "into", "raising"],
                );
                if disp_end > i {
                    self.collect_message_operand_refs_infos(&sig[i..disp_end], scope);
                }
            }
        }

        if let Some(with_ix) = with_ix {
            let args_end = [into_ix, display_ix, raising_ix, Some(period_pos)]
                .into_iter()
                .flatten()
                .min()
                .unwrap();
            if args_end > with_ix + 1 {
                self.collect_message_operand_refs_infos(&sig[with_ix + 1..args_end], scope);
            }
        }
        if let Some(into_ix) = into_ix {
            let into_end = [display_ix, raising_ix, Some(period_pos)]
                .into_iter()
                .flatten()
                .min()
                .unwrap();
            self.declare_message_inline_into_target_infos(&sig, into_ix + 1, into_end, scope);
            let value_start = self.message_into_expression_start(&sig, into_ix + 1, into_end);
            if into_end > value_start {
                self.collect_message_operand_refs_infos(&sig[value_start..into_end], scope);
            }
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

    fn declare_message_inline_into_target_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
        end: usize,
        scope: ScopeId,
    ) {
        let mut idx = start;
        while idx < end
            && tokens
                .get(idx)
                .is_some_and(|token| self.collector.syntax_token_is_comment(token))
        {
            idx += 1;
        }
        let Some(head) = tokens.get(idx) else {
            return;
        };
        if !head.text.eq_ignore_ascii_case("data") {
            return;
        }
        if tokens.get(idx + 1).map(|token| token.text.as_ref()) != Some("(") {
            return;
        }
        let Some(name_tok) = tokens.get(idx + 2) else {
            return;
        };
        if !self.collector.syntax_token_is_ident_like(name_tok) {
            return;
        }
        if tokens.get(idx + 3).map(|token| token.text.as_ref()) != Some(")") {
            return;
        }
        self.collector.declare_symbol(
            scope,
            Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
            crate::def_map::SymbolKind::Variable,
            name_tok.range.clone(),
            None,
            None,
            None,
            None,
        );
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

        if (head.text.eq_ignore_ascii_case("commit") || head.text.eq_ignore_ascii_case("rollback"))
            && matches!(tail.first(), Some(token) if token.text.eq_ignore_ascii_case("work"))
        {
            return;
        } else if head.text.eq_ignore_ascii_case("aliases") {
            self.collect_aliases_stmt_infos(&significant, scope);
        } else if head.text.eq_ignore_ascii_case("clear") {
            self.collect_clear_stmt_infos(tail, scope);
        } else if head.text.eq_ignore_ascii_case("convert") {
            self.collect_convert_stmt_infos(&significant, scope);
        } else if head.text.eq_ignore_ascii_case("describe") {
            self.collect_describe_stmt_infos(&significant, scope);
        } else if head.text.eq_ignore_ascii_case("find") {
            self.collect_find_stmt_infos(&significant, scope);
        } else if head.text.eq_ignore_ascii_case("replace") {
            self.collect_replace_stmt_infos(&significant, scope);
        } else if head.text.eq_ignore_ascii_case("wait") {
            self.collect_wait_stmt_infos(&significant, scope);
        } else {
            self.collector
                .collect_token_expression_refs_infos(tail, scope, true);
        }
    }

    fn collect_wait_stmt_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        if tokens.is_empty() || !tokens[0].text.eq_ignore_ascii_case("wait") {
            return;
        }

        let mut expr_start = 1usize;
        if tokens
            .get(expr_start)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("up"))
        {
            expr_start += 1;
        }
        if tokens
            .get(expr_start)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("to"))
        {
            expr_start += 1;
        }

        let expr_end = self
            .collector
            .find_top_level_keyword_index_infos(tokens, expr_start, "seconds")
            .unwrap_or(tokens.len());
        if expr_end > expr_start {
            self.collector.collect_token_expression_refs_infos(
                &tokens[expr_start..expr_end],
                scope,
                true,
            );
        }
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

    fn message_into_expression_start(
        &self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
        end: usize,
    ) -> usize {
        if start + 3 < end
            && tokens[start].text.eq_ignore_ascii_case("data")
            && tokens[start + 1].text.as_ref() == "("
            && self
                .collector
                .syntax_token_is_ident_like(&tokens[start + 2])
            && tokens[start + 3].text.as_ref() == ")"
        {
            return start + 4;
        }
        start
    }

    pub(super) fn collect_clear_stmt_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        let mut idx = 0usize;
        while idx < tokens.len() {
            let end_idx =
                self.collector
                    .consume_concatenate_operand_infos(tokens, idx, &["with", "in"]);
            if end_idx > idx {
                self.collector.collect_token_expression_refs_infos(
                    &tokens[idx..end_idx],
                    scope,
                    true,
                );
                idx = end_idx;
            } else {
                idx += 1;
            }
            while idx < tokens.len()
                && !self
                    .collector
                    .token_starts_concatenate_operand_infos(tokens, idx)
                && tokens[idx].text.as_ref() != "."
            {
                idx += 1;
            }
        }
    }

    pub(super) fn collect_describe_stmt_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        if tokens.len() < 3 || !tokens[0].text.eq_ignore_ascii_case("describe") {
            return;
        }
        if !tokens
            .get(1)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("table"))
        {
            self.collector
                .collect_token_expression_refs_infos(&tokens[1..], scope, true);
            return;
        }

        let lines_idx = self
            .collector
            .find_top_level_keyword_index_infos(tokens, 2, "lines");
        let lines_idx = lines_idx.unwrap_or(tokens.len());
        // `DESCRIBE TABLE itab[] LINES lv_lines` uses the legacy table-body form. Treat
        // `TABLE`/`LINES` as statement keywords and only collect the actual table/target operands.
        if lines_idx > 2 {
            self.collector
                .collect_token_expression_refs_infos(&tokens[2..lines_idx], scope, true);
        }
        if lines_idx < tokens.len() {
            let target_start = lines_idx + 1;
            let target_end = tokens
                .iter()
                .position(|token| token.text.as_ref() == ".")
                .unwrap_or(tokens.len());
            if target_start < target_end
                && self.declare_describe_lines_inline_target_infos(
                    tokens,
                    target_start,
                    target_end,
                    scope,
                )
            {
                return;
            }
            if target_start < tokens.len() {
                self.collector.collect_token_expression_refs_infos(
                    &tokens[target_start..],
                    scope,
                    true,
                );
            }
        }
    }

    pub(super) fn collect_convert_stmt_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        if tokens.is_empty() || !tokens[0].text.eq_ignore_ascii_case("convert") {
            return;
        }

        let mut idx = 1usize;
        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("date"))
        {
            idx += 1;
        }
        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("time"))
        {
            idx += 1;
        }
        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("stamp"))
        {
            idx += 1;
        }

        let date_end =
            self.collector
                .consume_concatenate_operand_infos(tokens, idx, &["time", "into"]);
        if date_end > idx {
            self.collector
                .collect_token_expression_refs_infos(&tokens[idx..date_end], scope, true);
        }
        idx = date_end;

        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("time"))
            && !tokens
                .get(idx + 1)
                .is_some_and(|token| token.text.eq_ignore_ascii_case("zone"))
        {
            let time_start = idx + 1;
            let time_end =
                self.collector
                    .consume_concatenate_operand_infos(tokens, time_start, &["into"]);
            if time_end > time_start {
                self.collector.collect_token_expression_refs_infos(
                    &tokens[time_start..time_end],
                    scope,
                    true,
                );
            }
            idx = time_end;
        }

        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("into"))
        {
            idx += 1;
            if tokens
                .get(idx)
                .is_some_and(|token| token.text.eq_ignore_ascii_case("time"))
            {
                idx += 1;
            }
            if tokens
                .get(idx)
                .is_some_and(|token| token.text.eq_ignore_ascii_case("stamp"))
            {
                idx += 1;
            }
            let target_end =
                self.collector
                    .consume_concatenate_operand_infos(tokens, idx, &["time"]);
            if target_end > idx {
                self.collector.collect_token_expression_refs_infos(
                    &tokens[idx..target_end],
                    scope,
                    true,
                );
            }
            idx = target_end;
        }

        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("time"))
            && tokens
                .get(idx + 1)
                .is_some_and(|token| token.text.eq_ignore_ascii_case("zone"))
        {
            idx += 2;
            let end_idx = self
                .collector
                .consume_concatenate_operand_infos(tokens, idx, &[]);
            if end_idx > idx {
                self.collector.collect_token_expression_refs_infos(
                    &tokens[idx..end_idx],
                    scope,
                    true,
                );
            }
        }
    }

    pub(super) fn collect_replace_stmt_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        if tokens.is_empty() || !tokens[0].text.eq_ignore_ascii_case("replace") {
            return;
        }

        let mut idx = 1usize;
        if tokens.get(idx).is_some_and(|token| {
            token.text.eq_ignore_ascii_case("first") || token.text.eq_ignore_ascii_case("all")
        }) {
            idx += 1;
            if tokens.get(idx).is_some_and(|token| {
                token.text.eq_ignore_ascii_case("occurrence")
                    || token.text.eq_ignore_ascii_case("occurrences")
            }) {
                idx += 1;
            }
        }
        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("of"))
        {
            idx += 1;
        }
        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("regex"))
        {
            idx += 1;
        }

        let source_end =
            self.collector
                .consume_concatenate_operand_infos(tokens, idx, &["in", "with"]);
        if source_end > idx {
            self.collector.collect_token_expression_refs_infos(
                &tokens[idx..source_end],
                scope,
                true,
            );
        }
        idx = source_end;

        while idx < tokens.len() {
            let token = &tokens[idx];
            if token.text.as_ref() == "." {
                break;
            }
            if token.text.eq_ignore_ascii_case("in") {
                if tokens.get(idx + 1).is_some_and(|next| {
                    next.text.eq_ignore_ascii_case("character")
                        || next.text.eq_ignore_ascii_case("byte")
                }) && tokens
                    .get(idx + 2)
                    .is_some_and(|next| next.text.eq_ignore_ascii_case("mode"))
                {
                    idx += 3;
                    continue;
                }

                let end_idx = self.collector.consume_concatenate_operand_infos(
                    tokens,
                    idx + 1,
                    &["with", "in"],
                );
                if end_idx > idx + 1 {
                    self.collector.collect_token_expression_refs_infos(
                        &tokens[idx + 1..end_idx],
                        scope,
                        true,
                    );
                }
                idx = end_idx;
                continue;
            }
            if token.text.eq_ignore_ascii_case("with") {
                let end_idx =
                    self.collector
                        .consume_concatenate_operand_infos(tokens, idx + 1, &["in"]);
                if end_idx > idx + 1 {
                    self.collector.collect_token_expression_refs_infos(
                        &tokens[idx + 1..end_idx],
                        scope,
                        true,
                    );
                }
                idx = end_idx;
                continue;
            }
            idx += 1;
        }
    }

    pub(super) fn collect_find_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.collector.node_has_structured_children(node) {
            self.collector.walk_children(node, scope);
            return;
        }
        let tokens = self.collector.significant_stmt_token_infos(node);
        self.collect_find_stmt_infos(&tokens, scope);
    }

    pub(super) fn collect_find_stmt_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        if tokens.is_empty() || !tokens[0].text.eq_ignore_ascii_case("find") {
            return;
        }

        let mut idx = 1usize;
        if tokens.get(idx).is_some_and(|token| {
            token.text.eq_ignore_ascii_case("first") || token.text.eq_ignore_ascii_case("all")
        }) {
            idx += 1;
            if tokens.get(idx).is_some_and(|token| {
                token.text.eq_ignore_ascii_case("occurrence")
                    || token.text.eq_ignore_ascii_case("occurrences")
            }) {
                idx += 1;
            }
        }
        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("of"))
        {
            idx += 1;
        }
        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("regex"))
        {
            idx += 1;
        }

        let pattern_end = self
            .collector
            .consume_concatenate_operand_infos(tokens, idx, &["in"]);
        if pattern_end > idx {
            self.collector.collect_token_expression_refs_infos(
                &tokens[idx..pattern_end],
                scope,
                true,
            );
        }
        idx = pattern_end;

        while idx < tokens.len() {
            let token = &tokens[idx];
            if token.text.as_ref() == "." {
                break;
            }
            if token.text.eq_ignore_ascii_case("in") {
                if tokens.get(idx + 1).is_some_and(|next| {
                    next.text.eq_ignore_ascii_case("character")
                        || next.text.eq_ignore_ascii_case("byte")
                }) && tokens
                    .get(idx + 2)
                    .is_some_and(|next| next.text.eq_ignore_ascii_case("mode"))
                {
                    idx += 3;
                    continue;
                }

                let end_idx = self.collector.consume_concatenate_operand_infos(
                    tokens,
                    idx + 1,
                    &["match", "submatches", "ignoring", "respecting", "in"],
                );
                if end_idx > idx + 1 {
                    self.collector.collect_token_expression_refs_infos(
                        &tokens[idx + 1..end_idx],
                        scope,
                        true,
                    );
                }
                idx = end_idx;
                continue;
            }
            if token.text.eq_ignore_ascii_case("match") {
                let clause_start = idx + 1;
                let value_start = if tokens.get(clause_start).is_some_and(|next| {
                    next.text.eq_ignore_ascii_case("offset")
                        || next.text.eq_ignore_ascii_case("length")
                }) {
                    clause_start + 1
                } else {
                    clause_start
                };
                let end_idx = self.collector.consume_concatenate_operand_infos(
                    tokens,
                    value_start,
                    &["match", "submatches", "ignoring", "respecting", "in"],
                );
                if end_idx > value_start {
                    self.collector.collect_token_expression_refs_infos(
                        &tokens[value_start..end_idx],
                        scope,
                        true,
                    );
                }
                idx = end_idx;
                continue;
            }
            if token.text.eq_ignore_ascii_case("submatches") {
                let end_idx = self.collector.consume_concatenate_operand_infos(
                    tokens,
                    idx + 1,
                    &["match", "ignoring", "respecting", "in"],
                );
                if end_idx > idx + 1 {
                    self.collector.collect_token_expression_refs_infos(
                        &tokens[idx + 1..end_idx],
                        scope,
                        true,
                    );
                }
                idx = end_idx;
                continue;
            }
            idx += 1;
        }
    }

    pub(super) fn collect_get_time_stamp_stmt(&mut self, node: NodeId, scope: ScopeId) {
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
        let methods_stmt = MethodsStmt::cast(self.collector.syntax(node)).expect("methods stmt");
        let type_refs: Vec<_> = methods_stmt
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
            self.collector
                .implemented_interfaces
                .push(crate::ImplementedInterfaceData {
                    owner_symbol,
                    interface_name,
                    range,
                });
            recorded = true;
        }
        if !recorded {
            self.collector.walk_children(node, scope);
        }
    }

    pub(super) fn collect_assert_or_check_stmt(&mut self, node: NodeId, scope: ScopeId) {
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
        if self.collector.node_has_structured_children(node) {
            self.collector.walk_children(node, scope);
            return;
        }
        let significant = self.collector.significant_stmt_token_infos(node);
        self.collect_create_object_stmt_infos(&significant, scope);
    }

    pub(super) fn collect_create_data_stmt_node(&mut self, node: NodeId, scope: ScopeId) {
        if self.collector.node_has_structured_children(node) {
            self.collector.walk_children(node, scope);
            return;
        }
        let significant = self.collector.significant_stmt_token_infos(node);
        self.collect_create_data_stmt_infos(&significant, scope);
    }

    pub(super) fn collect_call_method_stmt_node(&mut self, node: NodeId, scope: ScopeId) {
        let mut target = None;
        let mut arg_list = None;

        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::CallMethodTarget => {
                    let Some(mut callee) = self.collector.first_non_token_child(child) else {
                        continue;
                    };
                    while self.collector.file.kind(callee) == SyntaxKind::TemplateExpr {
                        let Some(inner) = self.collector.first_non_token_child(callee) else {
                            break;
                        };
                        callee = inner;
                    }
                    match self.collector.file.kind(callee) {
                        SyntaxKind::ExprIdent => {
                            let Some((method_name, _)) = self.collector.node_name(callee) else {
                                continue;
                            };
                            target = Some(NamedArgumentTarget::ImplicitMethod { method_name });
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
                SyntaxKind::CallArgList => arg_list = Some(child),
                _ => {}
            }
        }

        if let (Some(target), Some(arg_list)) = (target, arg_list) {
            self.collector
                .expr_lowering()
                .collect_call_argument_list(arg_list, scope, target);
        }
    }

    pub(super) fn collect_call_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.collector.node_has_structured_children(node) {
            let significant = self.collector.significant_stmt_token_infos(node);
            let function_name = if significant.len() >= 3
                && significant[0].text.eq_ignore_ascii_case("call")
                && significant[1].text.eq_ignore_ascii_case("function")
            {
                Self::call_function_name_from_tokens(&significant)
            } else {
                None
            };

            for child in self.collector.file.children(node) {
                match self.collector.file.kind(child) {
                    SyntaxKind::CallArgList => {
                        if let Some(function_name) = function_name.clone() {
                            self.collector.expr_lowering().collect_call_argument_list(
                                child,
                                scope,
                                NamedArgumentTarget::Function { function_name },
                            );
                        } else {
                            self.collector
                                .expr_lowering()
                                .collect_structured_argument_values_from_children(child, scope);
                        }
                    }
                    SyntaxKind::Token => {}
                    _ => self.collector.walk_node(child, scope),
                }
            }
            return;
        }
        self.collect_generic_simple_stmt(node, scope);
    }

    pub(super) fn collect_assign_keyword_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let mut source_expr = None;
        let mut inline_targets = Vec::new();
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::Token => {}
                SyntaxKind::AssignSourceExpr => {
                    let Some(expr) = self.collector.first_non_token_child(child) else {
                        continue;
                    };
                    source_expr = Some(expr);
                    self.collector.expr_lowering().collect_expr(expr, scope);
                }
                SyntaxKind::FieldSymbolInlineDecl => inline_targets.push(child),
                _ => self.collector.walk_node(child, scope),
            }
        }

        if inline_targets.is_empty() {
            return;
        }

        let inferred_metadata = source_expr
            .map(|expr| {
                self.collector
                    .control_lowering()
                    .loop_source_line_metadata_from_node(expr, scope)
            })
            .unwrap_or((None, None));
        for target in inline_targets {
            self.collector
                .decl_lowering()
                .declare_inline_field_symbol_decl(
                    target,
                    scope,
                    inferred_metadata.0,
                    inferred_metadata.1.clone(),
                );
        }
    }

    pub(super) fn collect_write_stmt(&mut self, node: NodeId, scope: ScopeId) {
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

    pub(super) fn collect_split_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.collector.node_has_structured_children(node) {
            let byte_mode = self.collector.file.children(node).any(|child| {
                self.collector.file.kind(child) == SyntaxKind::Token
                    && self
                        .collector
                        .syntax_token_nodes(child)
                        .into_iter()
                        .next()
                        .is_some_and(|token| token.text.eq_ignore_ascii_case("byte"))
            });
            let mut seen_into = false;
            let mut into_table = false;
            for child in self.collector.file.children(node) {
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
                            }
                        }
                    }
                    SyntaxKind::DataInlineDecl => {
                        self.declare_split_inline_data_target(child, scope, into_table, byte_mode);
                    }
                    _ => self.collector.walk_node(child, scope),
                }
            }
            return;
        }
        let significant = self.collector.significant_stmt_token_infos(node);
        if significant.is_empty() || !significant[0].text.eq_ignore_ascii_case("split") {
            return;
        }

        let Some(at_idx) = self
            .collector
            .find_top_level_keyword_index_infos(&significant, 1, "at")
        else {
            self.collector
                .collect_token_expression_refs_infos(&significant[1..], scope, true);
            return;
        };
        let Some(into_idx) =
            self.collector
                .find_top_level_keyword_index_infos(&significant, at_idx + 1, "into")
        else {
            self.collector.collect_token_expression_refs_infos(
                &significant[1..at_idx],
                scope,
                true,
            );
            self.collector.collect_token_expression_refs_infos(
                &significant[at_idx + 1..],
                scope,
                true,
            );
            return;
        };

        self.collector
            .collect_token_expression_refs_infos(&significant[1..at_idx], scope, true);

        let separator_end =
            self.collector
                .consume_concatenate_operand_infos(&significant, at_idx + 1, &["into"]);
        if separator_end > at_idx + 1 {
            self.collector.collect_token_expression_refs_infos(
                &significant[at_idx + 1..separator_end],
                scope,
                true,
            );
        }

        let mut idx = separator_end.max(into_idx + 1);
        if significant
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("table"))
        {
            idx += 1;
        }
        while idx < significant.len() {
            let token = &significant[idx];
            if token.text.as_ref() == "." {
                break;
            }
            if token.text.eq_ignore_ascii_case("in") {
                idx += 1;
                if significant.get(idx).is_some_and(|next| {
                    next.text.eq_ignore_ascii_case("character")
                        || next.text.eq_ignore_ascii_case("byte")
                }) {
                    idx += 1;
                }
                if significant
                    .get(idx)
                    .is_some_and(|next| next.text.eq_ignore_ascii_case("mode"))
                {
                    idx += 1;
                }
                continue;
            }
            let end_idx =
                self.collector
                    .consume_concatenate_operand_infos(&significant, idx, &["in"]);
            if end_idx > idx {
                self.collector.collect_token_expression_refs_infos(
                    &significant[idx..end_idx],
                    scope,
                    true,
                );
                idx = end_idx;
            } else {
                idx += 1;
            }
        }
    }

    pub(super) fn collect_concatenate_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.collector.node_has_structured_children(node) {
            self.collector.walk_children(node, scope);
            return;
        }
        let significant = self.collector.significant_stmt_token_infos(node);
        if significant.is_empty() || !significant[0].text.eq_ignore_ascii_case("concatenate") {
            return;
        }

        let Some(into_idx) =
            self.collector
                .find_top_level_keyword_index_infos(&significant, 1, "into")
        else {
            self.collector
                .collect_token_expression_refs_infos(&significant[1..], scope, true);
            return;
        };

        let mut idx = 1usize;
        while idx < into_idx {
            let end_idx =
                self.collector
                    .consume_concatenate_operand_infos(&significant, idx, &["into"]);
            if end_idx == idx {
                idx += 1;
                continue;
            }
            self.collector.collect_token_expression_refs_infos(
                &significant[idx..end_idx],
                scope,
                true,
            );
            idx = end_idx;
        }

        idx = into_idx + 1;
        let target_end = self.collector.consume_concatenate_operand_infos(
            &significant,
            idx,
            &["separated", "respecting", "in"],
        );
        if target_end > idx {
            self.collector.collect_token_expression_refs_infos(
                &significant[idx..target_end],
                scope,
                true,
            );
        }
        idx = target_end;

        while idx < significant.len() {
            let token = &significant[idx];
            if token.text.as_ref() == "." {
                break;
            }
            if token.text.eq_ignore_ascii_case("separated")
                && significant
                    .get(idx + 1)
                    .is_some_and(|next| next.text.eq_ignore_ascii_case("by"))
            {
                let sep_start = idx + 2;
                let sep_end = self.collector.consume_concatenate_operand_infos(
                    &significant,
                    sep_start,
                    &["respecting", "in"],
                );
                if sep_end > sep_start {
                    self.collector.collect_token_expression_refs_infos(
                        &significant[sep_start..sep_end],
                        scope,
                        true,
                    );
                }
                idx = sep_end;
                continue;
            }
            if token.text.eq_ignore_ascii_case("respecting") {
                idx += 1;
                if significant
                    .get(idx)
                    .is_some_and(|next| next.text.eq_ignore_ascii_case("blanks"))
                {
                    idx += 1;
                }
                continue;
            }
            if token.text.eq_ignore_ascii_case("in") {
                idx += 1;
                if significant.get(idx).is_some_and(|next| {
                    next.text.eq_ignore_ascii_case("character")
                        || next.text.eq_ignore_ascii_case("byte")
                }) {
                    idx += 1;
                }
                if significant
                    .get(idx)
                    .is_some_and(|next| next.text.eq_ignore_ascii_case("mode"))
                {
                    idx += 1;
                }
                continue;
            }
            idx += 1;
        }
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
