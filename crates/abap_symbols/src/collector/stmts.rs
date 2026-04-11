use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_ast::ast::{
    AliasesStmt, AstNode, CallMethodStmt, CallStmt, ClearStmt, ConcatenateStmt, ConvertStmt,
    CreateDataStmt, CreateObjectStmt, DescribeStmt, FindStmt, MessageStmt, MethodsStmt, RaiseStmt,
    ReadTableStmt, ReplaceStmt, SplitStmt, UpdateStmt, WaitStmt, WriteStmt,
};

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
        if let Some(stmt) = ReadTableStmt::cast(self.collector.syntax(node)) {
            let data_inline_targets: Vec<_> = stmt
                .data_inline_targets()
                .map(|target| target.id())
                .collect();
            let field_symbol_targets: Vec<_> = stmt
                .field_symbol_inline_targets()
                .map(|target| target.id())
                .collect();
            let mut source_expr = None;
            let mut target_kind = None;
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
                    SyntaxKind::DataInlineDecl | SyntaxKind::FieldSymbolInlineDecl => {}
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
            let mut has_data_inline = false;
            for child in self.collector.file.children(into_clause_id) {
                match self.collector.file.kind(child) {
                    SyntaxKind::Token => {}
                    SyntaxKind::DataInlineDecl => {
                        self.collector
                            .decl_lowering()
                            .walk_inline_decl(child, scope);
                        has_data_inline = true;
                    }
                    _ => self.collector.walk_node(child, scope),
                }
            }
            if !has_data_inline {
                let sig = self.collector.significant_stmt_token_infos(into_clause_id);
                if sig.len() > 1 {
                    self.collect_message_operand_refs_infos(&sig[1..], scope);
                }
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
            let mut had_non_token = false;
            for child in self.collector.file.children(raising_clause_id) {
                if self.collector.file.kind(child) != SyntaxKind::Token {
                    had_non_token = true;
                    self.collector.walk_node(child, scope);
                }
            }
            if !had_non_token {
                let sig = self
                    .collector
                    .significant_stmt_token_infos(raising_clause_id);
                if sig.len() > 1 {
                    self.collect_message_operand_refs_infos(&sig[1..], scope);
                }
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

    fn collect_message_operand_node(&mut self, node: NodeId, scope: ScopeId) {
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

        let sig = self.collector.significant_stmt_token_infos(node);
        if sig.is_empty() {
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
        } else {
            self.collector
                .collect_token_expression_refs_infos(tail, scope, true);
        }
    }

    pub(super) fn collect_wait_stmt(&mut self, node: NodeId, scope: ScopeId) {
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
                self.collector.walk_node(operand, scope);
            }
        }
    }

    pub(super) fn collect_describe_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if let Some(stmt) = DescribeStmt::cast(self.collector.syntax(node)) {
            let table_operand = stmt
                .table_operand()
                .and_then(|operand| operand.value())
                .map(|value| value.id());
            let lines_target = stmt
                .lines_target()
                .and_then(|target| target.value())
                .map(|value| value.id());
            if let Some(table_operand) = table_operand {
                self.collector.walk_node(table_operand, scope);
            }
            if let Some(lines_target) = lines_target
                && (!self.declare_describe_lines_inline_target(lines_target, scope)
                    || self.collector.file.kind(lines_target) != SyntaxKind::DataInlineDecl)
            {
                self.collector.walk_node(lines_target, scope);
            }
        }
    }

    pub(super) fn collect_convert_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if let Some(stmt) = ConvertStmt::cast(self.collector.syntax(node)) {
            let mut operands: Vec<_> = stmt
                .operands()
                .filter_map(|operand| operand.value())
                .map(|value| value.id())
                .collect();
            if let Some(target) = stmt.target().and_then(|target| target.value()) {
                operands.push(target.id());
            }
            if let Some(time_zone) = stmt.time_zone().and_then(|target| target.value()) {
                operands.push(time_zone.id());
            }
            for operand in operands {
                self.collector.walk_node(operand, scope);
            }
        }
    }

    pub(super) fn collect_replace_stmt(&mut self, node: NodeId, scope: ScopeId) {
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
        let Some((type_ref_id, trailing_child_ids)) =
            (match RaiseStmt::cast(self.collector.syntax(node)) {
                Some(stmt) => stmt.exception_type_ref().map(|type_ref| {
                    (
                        type_ref.syntax().id(),
                        stmt.trailing_children()
                            .into_iter()
                            .map(|child| child.id())
                            .collect::<Vec<_>>(),
                    )
                }),
                None => None,
            })
        else {
            self.collect_generic_simple_stmt(node, scope);
            return;
        };

        self.collector
            .decl_lowering()
            .collect_type_ref(type_ref_id, scope);

        let trailing_tokens: Vec<_> = trailing_child_ids
            .into_iter()
            .flat_map(|child_id| self.collector.syntax_token_nodes(child_id))
            .collect();
        if !trailing_tokens.is_empty() {
            self.collector
                .collect_token_expression_refs_infos(&trailing_tokens, scope, true);
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

    pub(super) fn collect_find_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if let Some(stmt) = FindStmt::cast(self.collector.syntax(node)) {
            let mut operand_ids = Vec::new();
            if let Some(pattern) = stmt.pattern().and_then(|operand| operand.value()) {
                operand_ids.push(pattern.id());
            }
            if let Some(target) = stmt.target().and_then(|operand| operand.value()) {
                operand_ids.push(target.id());
            }
            operand_ids.extend(
                stmt.match_targets()
                    .filter_map(|operand| operand.value())
                    .map(|operand| operand.id()),
            );
            operand_ids.extend(
                stmt.submatch_targets()
                    .filter_map(|operand| operand.value())
                    .map(|operand| operand.id()),
            );
            for operand_id in operand_ids {
                self.collector.walk_node(operand_id, scope);
            }
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
                if let Some(target) = constructor_target {
                    self.collector.expr_lowering().collect_call_argument_list(
                        arg_list_id,
                        scope,
                        target,
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
            if let (Some(target), Some(arg_list_id)) = (target, arg_list_id) {
                self.collector.expr_lowering().collect_call_argument_list(
                    arg_list_id,
                    scope,
                    target,
                );
                return;
            }
        }

        let significant = self.collector.significant_stmt_token_infos(node);
        self.collect_call_method_stmt_infos(&significant, scope);
    }

    pub(super) fn collect_call_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if CallStmt::cast(self.collector.syntax(node)).is_some() {
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
                    SyntaxKind::CallExpr => {
                        self.collector.walk_node(child, scope);
                    }
                    SyntaxKind::Token => {}
                    _ => self.collector.walk_node(child, scope),
                }
            }
            return;
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
        if SplitStmt::cast(self.collector.syntax(node)).is_some() {
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
        if let Some(stmt) = ConcatenateStmt::cast(self.collector.syntax(node)) {
            let mut operands: Vec<_> = stmt
                .sources()
                .filter_map(|operand| operand.value())
                .map(|value| value.id())
                .collect();
            if let Some(target) = stmt.target().and_then(|operand| operand.value()) {
                operands.push(target.id());
            }
            if let Some(separator) = stmt.separator().and_then(|operand| operand.value()) {
                operands.push(separator.id());
            }
            for operand in operands {
                self.collector.walk_node(operand, scope);
            }
            return;
        }
    }

    pub(super) fn collect_update_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let Some((from_operand, set_values, where_clause)) =
            UpdateStmt::cast(self.collector.syntax(node)).map(|stmt| {
                (
                    stmt.from_operand()
                        .and_then(|operand| operand.value())
                        .map(|value| value.id()),
                    stmt.set_clause()
                        .into_iter()
                        .flat_map(|clause| clause.assignments())
                        .filter_map(|assignment| {
                            assignment.value().and_then(|operand| operand.value())
                        })
                        .map(|value| value.id())
                        .collect::<Vec<_>>(),
                    stmt.where_clause().map(|clause| clause.syntax().id()),
                )
            })
        else {
            self.collect_generic_simple_stmt(node, scope);
            return;
        };

        if let Some(from_operand) = from_operand {
            self.collector.walk_node(from_operand, scope);
        }

        for value in set_values {
            self.collector.walk_node(value, scope);
        }

        if let Some(where_clause) = where_clause {
            let significant = self.collector.significant_stmt_token_infos(where_clause);
            let Some((_, tail)) = significant.split_first() else {
                return;
            };
            self.collector
                .collect_token_expression_refs_infos(tail, scope, true);
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
