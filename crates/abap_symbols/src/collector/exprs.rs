use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_ast::ast::{AstNode, CallArgList, CallExpr, CallNamedArg, CallPositionalArg};

use crate::builtins::builtin_routine_spec;
use crate::def_map::{
    FieldAccess, FieldTypeRefData, NamedArgumentAccess, NamedArgumentSection, NamedArgumentTarget,
    ReferenceKind, SymbolKind,
};
use crate::ids::ScopeId;
use crate::ids::StructureId;
use crate::scope::Namespace;

use super::Collector;
use super::context::ExprContext;

pub(super) struct ExprLowering<'ctx, 'a> {
    ctx: ExprContext<'ctx, 'a>,
}

impl<'a> Collector<'a> {
    pub(super) fn expr_lowering(&mut self) -> ExprLowering<'_, 'a> {
        ExprLowering {
            ctx: ExprContext::new(self),
        }
    }
}

impl<'ctx, 'a> ExprLowering<'ctx, 'a> {
    fn builtin_type(name: &'static str) -> FieldTypeRefData {
        FieldTypeRefData {
            namespace: Namespace::Type,
            is_ref: false,
            base_name: Arc::<str>::from(name),
            field_path: Vec::new(),
        }
    }

    fn kind(&self, node: NodeId) -> SyntaxKind {
        self.ctx.file().kind(node)
    }

    fn source(&self) -> &'a str {
        self.ctx.source()
    }

    fn constructor_keyword(&self, node: NodeId) -> Option<Arc<str>> {
        self.ctx
            .file()
            .children(node)
            .find(|&child| self.kind(child) == SyntaxKind::Token)
            .and_then(|child| self.ctx.syntax(child).text(self.source()))
            .map(|text| Arc::<str>::from(text.to_ascii_lowercase()))
    }

    fn inferred_metadata_from_tokens(
        &self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        let tokens: Vec<_> = tokens
            .iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .cloned()
            .collect();
        let Some(first) = tokens.first() else {
            return (None, None);
        };
        if tokens.len() == 1 {
            let text = first.text.as_ref();
            if text.chars().all(|ch| ch.is_ascii_digit()) {
                return (None, Some(Self::builtin_type("i")));
            }
            if text.starts_with('`') && text.ends_with('`') {
                return (None, Some(Self::builtin_type("string")));
            }
            if self.ctx.syntax_token_is_ident_like(first)
                && let Some(symbol_id) =
                    self.ctx
                        .lookup_symbol_in_scope_chain(scope, Namespace::Value, text)
            {
                return (
                    self.ctx.symbol_structure(symbol_id),
                    self.ctx.symbol_declared_type(symbol_id),
                );
            }
        }
        (None, None)
    }

    pub(super) fn collect_expr(&mut self, node: NodeId, scope: ScopeId) {
        match self.kind(node) {
            SyntaxKind::AssignStmt => self.collect_assign_stmt(node, scope),
            SyntaxKind::ExprIdent => {
                if let Some((name, range)) = self.ctx.node_name(node) {
                    self.ctx.add_reference(
                        scope,
                        name,
                        Namespace::Value,
                        ReferenceKind::Identifier,
                        range,
                    );
                }
            }
            SyntaxKind::SelectorExpr => self.collect_selector_expr(node, scope),
            SyntaxKind::SubstringExpr => self.collect_substring_expr(node, scope),
            SyntaxKind::CallExpr => self.collect_call_expr(node, scope),
            SyntaxKind::LetExpr => self.collect_let_expr(node, scope),
            SyntaxKind::ConstructorExpr => {
                let constructor_keyword = self.constructor_keyword(node);
                let mut arg_list = None;
                for child in self.ctx.file().children(node) {
                    match self.kind(child) {
                        SyntaxKind::TypeRefSimple => {
                            self.ctx.decl_lowering().collect_type_ref(child, scope)
                        }
                        SyntaxKind::CallArgList => arg_list = Some(child),
                        SyntaxKind::Token => {}
                        _ => self.collect_expr(child, scope),
                    }
                }
                if let Some(arg_list) = arg_list {
                    if constructor_keyword.as_deref() == Some("new")
                        && let Some((type_name, _)) = self.ctx.constructor_type_ref(node)
                    {
                        self.collect_call_argument_list(
                            arg_list,
                            scope,
                            NamedArgumentTarget::Constructor { type_name },
                        );
                    } else if constructor_keyword.as_deref() == Some("value") {
                        self.collect_value_constructor_arg_list(arg_list, scope);
                    } else if constructor_keyword.as_deref() == Some("cond") {
                        self.collect_cond_constructor_arg_list(arg_list, scope);
                    } else if constructor_keyword.as_deref() == Some("reduce") {
                        let tokens = self.ctx.syntax_token_nodes(arg_list);
                        if tokens.len() >= 2 {
                            self.collect_reduce_constructor_tokens(
                                &tokens[1..tokens.len() - 1],
                                scope,
                            );
                        }
                    } else {
                        self.collect_structured_argument_values_from_children(arg_list, scope);
                    }
                }
            }
            SyntaxKind::TypeRefSimple => self.ctx.decl_lowering().collect_type_ref(node, scope),
            _ => {
                let token_children = self.ctx.syntax_token_nodes(node);
                if self.kind(node) == SyntaxKind::TemplateExpr
                    && token_children
                        .iter()
                        .any(|token| matches!(token.text.as_ref(), "[" | "]"))
                    && self
                        .ctx
                        .file()
                        .find_first_kind(node, SyntaxKind::ConstructorExpr)
                        .is_none()
                    && self
                        .ctx
                        .file()
                        .find_first_kind(node, SyntaxKind::CallExpr)
                        .is_none()
                {
                    self.ctx
                        .collect_token_expression_refs_infos(&token_children, scope, true);
                    return;
                }
                if !token_children.is_empty()
                    && self
                        .ctx
                        .file()
                        .children(node)
                        .all(|child| self.kind(child) == SyntaxKind::Token)
                {
                    self.ctx
                        .collect_token_expression_refs_infos(&token_children, scope, true);
                    return;
                }
                for child in self.ctx.file().children(node) {
                    match self.kind(child) {
                        SyntaxKind::ExprIdent
                        | SyntaxKind::SelectorExpr
                        | SyntaxKind::SubstringExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::ConstructorExpr
                        | SyntaxKind::LetExpr
                        | SyntaxKind::TemplateExpr
                        | SyntaxKind::TemplateInterpolation
                        | SyntaxKind::TemplateFormatSpec
                        | SyntaxKind::IsPredicate
                        | SyntaxKind::InstanceOfPredicate
                        | SyntaxKind::BetweenExpr
                        | SyntaxKind::AssignStmt
                        | SyntaxKind::TypeRefSimple => self.collect_expr(child, scope),
                        _ => self.ctx.walk_node(child, scope),
                    }
                }
            }
        }
    }

    fn collect_value_constructor_arg_list(&mut self, node: NodeId, scope: ScopeId) {
        let Some(arg_list) = CallArgList::cast(self.ctx.syntax(node)) else {
            return;
        };
        let arg_list_tokens = self.ctx.syntax_token_nodes(node);
        if arg_list_tokens.len() >= 2
            && arg_list_tokens[1..arg_list_tokens.len() - 1]
                .iter()
                .any(|token| {
                    token.text.eq_ignore_ascii_case("FOR") || token.text.eq_ignore_ascii_case("LET")
                })
        {
            self.collect_value_constructor_tokens(
                &arg_list_tokens[1..arg_list_tokens.len() - 1],
                scope,
            );
            return;
        }
        let items: Vec<_> = arg_list
            .items()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in items {
            match kind_syntax {
                SyntaxKind::CallNamedArg => {
                    let value_children: Vec<_> = CallNamedArg::cast(self.ctx.syntax(child))
                        .map(|arg| {
                            arg.value_children()
                                .into_iter()
                                .map(|child| child.id())
                                .collect()
                        })
                        .unwrap_or_default();
                    self.collect_structured_argument_values(&value_children, scope);
                }
                SyntaxKind::CallPositionalArg => {
                    let value_children: Vec<_> = CallPositionalArg::cast(self.ctx.syntax(child))
                        .map(|arg| {
                            arg.value_children()
                                .into_iter()
                                .map(|child| child.id())
                                .collect()
                        })
                        .unwrap_or_default();
                    if value_children
                        .iter()
                        .all(|&node| self.kind(node) == SyntaxKind::Token)
                    {
                        let tokens = value_children
                            .iter()
                            .flat_map(|&node| self.ctx.syntax_token_nodes(node))
                            .collect::<Vec<_>>();
                        self.collect_value_constructor_tokens(&tokens, scope);
                    } else {
                        self.collect_structured_argument_values(&value_children, scope);
                    }
                }
                _ => {}
            }
        }
    }

    fn collect_cond_constructor_arg_list(&mut self, node: NodeId, scope: ScopeId) {
        let Some(arg_list) = CallArgList::cast(self.ctx.syntax(node)) else {
            return;
        };
        let mut clause_scope = scope;
        let items: Vec<_> = arg_list
            .items()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in items {
            if kind_syntax != SyntaxKind::CallPositionalArg {
                continue;
            }
            let value_children: Vec<_> = CallPositionalArg::cast(self.ctx.syntax(child))
                .map(|arg| {
                    arg.value_children()
                        .into_iter()
                        .map(|child| child.id())
                        .collect()
                })
                .unwrap_or_default();
            if value_children.is_empty() {
                continue;
            }
            if value_children
                .iter()
                .all(|&node| self.kind(node) == SyntaxKind::Token)
            {
                let tokens = value_children
                    .iter()
                    .flat_map(|&node| self.ctx.syntax_token_nodes(node))
                    .collect::<Vec<_>>();
                if tokens
                    .first()
                    .is_some_and(|token| token.text.eq_ignore_ascii_case("LET"))
                {
                    clause_scope = self.collect_cond_leading_let_tokens(&tokens, clause_scope);
                    continue;
                }
            }
            self.collect_structured_argument_values(&value_children, clause_scope);
        }
    }

    fn collect_let_expr(&mut self, node: NodeId, scope: ScopeId) {
        let tokens = self.ctx.syntax_token_nodes(node);
        if !tokens.is_empty() {
            self.collect_let_expression(&tokens, 0, scope);
        }
    }

    fn collect_cond_leading_let_tokens(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
    ) -> ScopeId {
        let Some(in_idx) = self.find_top_level_keyword(tokens, 1, &["IN"]) else {
            self.ctx
                .collect_token_expression_refs_infos(tokens, scope, true);
            return scope;
        };
        let Some(last_binding_tok) = tokens.get(in_idx) else {
            return scope;
        };
        let let_scope = self.ctx.push_scope(
            crate::scope::ScopeKind::LoopBlock,
            tokens[0].range.start..last_binding_tok.range.end,
            Some(scope),
            None,
        );

        let mut idx = 1usize;
        while idx < in_idx {
            if self.ctx.syntax_token_is_comment(&tokens[idx]) {
                idx += 1;
                continue;
            }
            if !self.is_named_assignment_start(tokens, idx) {
                self.ctx
                    .collect_token_expression_refs_infos(&tokens[idx..in_idx], let_scope, true);
                break;
            }
            let name_tok = &tokens[idx];
            let value_end = self.constructor_assignment_value_end(&tokens[..in_idx], idx + 2);
            self.ctx.collect_token_expression_refs_infos(
                &tokens[idx + 2..value_end],
                let_scope,
                true,
            );
            let (structure, declared_type) =
                self.inferred_metadata_from_tokens(&tokens[idx + 2..value_end], let_scope);
            let symbol_kind = if self.is_field_symbol_name(name_tok.text.as_ref()) {
                SymbolKind::FieldSymbol
            } else {
                SymbolKind::Variable
            };
            self.ctx.declare_symbol(
                let_scope,
                Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
                symbol_kind,
                name_tok.range.clone(),
                structure,
                declared_type,
                None,
                None,
            );
            idx = value_end;
        }

        let_scope
    }

    fn collect_value_constructor_tokens(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        let mut idx = 0usize;
        let mut segment_start = 0usize;
        let mut in_string_template = false;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if self.ctx.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }

            if token.text.as_ref() == "|" {
                in_string_template = !in_string_template;
                idx += 1;
                continue;
            }

            if in_string_template && token.text.as_ref() == "{" {
                if let Some(end_idx) = self
                    .ctx
                    .find_matching_group_end_infos(tokens, idx, "{", "}")
                {
                    idx = end_idx + 1;
                    continue;
                }
            }

            match token.text.as_ref() {
                text if text.eq_ignore_ascii_case("BASE") => {
                    self.collect_value_token_segment(&tokens[segment_start..idx], scope);
                    let operand_start = idx + 1;
                    let operand_end = self.value_base_operand_end(tokens, operand_start);
                    self.ctx.collect_token_expression_refs_infos(
                        &tokens[operand_start..operand_end],
                        scope,
                        true,
                    );
                    idx = operand_end;
                    segment_start = idx;
                }
                text if text.eq_ignore_ascii_case("LINES")
                    && tokens
                        .get(idx + 1)
                        .is_some_and(|next| next.text.eq_ignore_ascii_case("OF")) =>
                {
                    self.collect_value_token_segment(&tokens[segment_start..idx], scope);
                    idx = self.collect_value_lines_of_clause(tokens, idx, scope);
                    segment_start = idx;
                }
                text if text.eq_ignore_ascii_case("FOR") => {
                    self.collect_value_token_segment(&tokens[segment_start..idx], scope);
                    self.collect_value_for_clause(tokens, idx, scope);
                    return;
                }
                text if text.eq_ignore_ascii_case("LET") => {
                    self.collect_value_token_segment(&tokens[segment_start..idx], scope);
                    self.collect_let_expression(tokens, idx, scope);
                    return;
                }
                text if text.eq_ignore_ascii_case("OPTIONAL") => {
                    self.collect_value_token_segment(&tokens[segment_start..idx], scope);
                    idx += 1;
                    segment_start = idx;
                }
                _ if self.is_named_assignment_start(tokens, idx) => {
                    self.collect_value_token_segment(&tokens[segment_start..idx], scope);
                    let value_end = self.constructor_assignment_value_end(tokens, idx + 2);
                    self.collect_value_constructor_tokens(&tokens[idx + 2..value_end], scope);
                    idx = value_end;
                    segment_start = idx;
                }
                "(" | "[" | "{" => {
                    let (open_text, close_text) = match token.text.as_ref() {
                        "(" => ("(", ")"),
                        "[" => ("[", "]"),
                        "{" => ("{", "}"),
                        _ => unreachable!(),
                    };
                    if let Some(end_idx) = self
                        .ctx
                        .find_matching_group_end_infos(tokens, idx, open_text, close_text)
                    {
                        if token.text.as_ref() == "[" {
                            // Keep table-expression brackets attached to the surrounding segment so
                            // selectors/substrings like `itab[ 1 ]-field+2` resolve as one access.
                        } else if token.text.as_ref() == "("
                            && self.paren_belongs_to_constructor_or_call(tokens, segment_start, idx)
                        {
                            self.collect_value_token_segment(
                                &tokens[segment_start..=end_idx],
                                scope,
                            );
                            segment_start = end_idx + 1;
                        } else {
                            self.collect_value_token_segment(&tokens[segment_start..idx], scope);
                            self.collect_value_constructor_tokens(&tokens[idx + 1..end_idx], scope);
                            segment_start = end_idx + 1;
                        }
                        idx = end_idx + 1;
                    } else {
                        idx += 1;
                    }
                }
                _ => {
                    idx += 1;
                }
            }
        }

        self.collect_value_token_segment(&tokens[segment_start..], scope);
    }

    fn collect_value_lines_of_clause(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        start: usize,
        scope: ScopeId,
    ) -> usize {
        let source_start = start + 2;
        if source_start >= tokens.len() {
            return tokens.len();
        }

        let source_end = self
            .find_value_lines_of_keyword(tokens, source_start)
            .unwrap_or_else(|| self.value_lines_of_clause_end(tokens, source_start));
        self.ctx.collect_token_expression_refs_infos(
            &tokens[source_start..source_end],
            scope,
            true,
        );

        let mut idx = source_end;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if self.ctx.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }

            if token.text.eq_ignore_ascii_case("FROM") || token.text.eq_ignore_ascii_case("TO") {
                let expr_start = idx + 1;
                let expr_end = self
                    .find_value_lines_of_keyword(tokens, expr_start)
                    .unwrap_or_else(|| self.value_lines_of_clause_end(tokens, expr_start));
                self.ctx.collect_token_expression_refs_infos(
                    &tokens[expr_start..expr_end],
                    scope,
                    true,
                );
                idx = expr_end;
                continue;
            }

            if token.text.eq_ignore_ascii_case("USING")
                && tokens
                    .get(idx + 1)
                    .is_some_and(|next| next.text.eq_ignore_ascii_case("KEY"))
            {
                idx += 3;
                continue;
            }

            break;
        }

        idx
    }

    fn paren_belongs_to_constructor_or_call(
        &self,
        tokens: &[super::SyntaxTokenInfo],
        segment_start: usize,
        paren_idx: usize,
    ) -> bool {
        if paren_idx <= segment_start {
            return false;
        }
        if tokens.get(paren_idx - 1).is_some_and(|prev| {
            self.ctx.syntax_token_is_ident_like(prev)
                && !self
                    .ctx
                    .syntax_tokens_have_space_between(prev, &tokens[paren_idx])
        }) {
            return true;
        }

        let mut idx = segment_start;
        while idx < paren_idx {
            let token = &tokens[idx];
            if self.ctx.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }
            if self.ctx.syntax_token_is_ident_like(token)
                && matches!(
                    token.text.to_ascii_uppercase().as_str(),
                    "COND"
                        | "CONV"
                        | "CORRESPONDING"
                        | "EXACT"
                        | "FILTER"
                        | "NEW"
                        | "REDUCE"
                        | "REF"
                        | "SWITCH"
                        | "VALUE"
                        | "CAST"
                )
            {
                return true;
            }
            break;
        }
        false
    }

    pub(super) fn collect_value_constructor_tokens_infos(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        self.collect_value_constructor_tokens(tokens, scope);
    }

    pub(super) fn collect_cond_constructor_tokens_infos(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        if tokens.is_empty() {
            return;
        }
        let first_clause_idx = self
            .find_top_level_keyword(tokens, 0, &["WHEN", "ELSE"])
            .unwrap_or(tokens.len());
        let mut clause_scope = scope;

        if first_clause_idx > 0 {
            if tokens[0].text.eq_ignore_ascii_case("LET") {
                clause_scope =
                    self.collect_cond_leading_let_tokens(&tokens[..first_clause_idx], scope);
            } else {
                self.ctx.collect_token_expression_refs_infos(
                    &tokens[..first_clause_idx],
                    scope,
                    true,
                );
            }
        }

        let mut idx = first_clause_idx;
        while idx < tokens.len() {
            let clause_end = self
                .find_top_level_keyword(tokens, idx + 1, &["WHEN", "ELSE"])
                .unwrap_or(tokens.len());
            let token = &tokens[idx];
            if token.text.eq_ignore_ascii_case("WHEN") {
                let Some(then_idx) = self.find_top_level_keyword(tokens, idx + 1, &["THEN"]) else {
                    self.ctx.collect_token_expression_refs_infos(
                        &tokens[idx + 1..clause_end],
                        clause_scope,
                        true,
                    );
                    break;
                };
                self.ctx.collect_token_expression_refs_infos(
                    &tokens[idx + 1..then_idx],
                    clause_scope,
                    true,
                );
                let result_tokens = &tokens[then_idx + 1..clause_end];
                if result_tokens
                    .first()
                    .is_some_and(|t| t.text.eq_ignore_ascii_case("LET"))
                {
                    self.collect_let_expression(result_tokens, 0, clause_scope);
                } else {
                    self.ctx
                        .collect_token_expression_refs_infos(result_tokens, clause_scope, true);
                }
            } else if token.text.eq_ignore_ascii_case("ELSE") {
                let result_tokens = &tokens[idx + 1..clause_end];
                if result_tokens
                    .first()
                    .is_some_and(|t| t.text.eq_ignore_ascii_case("LET"))
                {
                    self.collect_let_expression(result_tokens, 0, clause_scope);
                } else {
                    self.ctx
                        .collect_token_expression_refs_infos(result_tokens, clause_scope, true);
                }
            } else {
                self.ctx.collect_token_expression_refs_infos(
                    &tokens[idx..clause_end],
                    clause_scope,
                    true,
                );
            }
            idx = clause_end;
        }
    }

    fn collect_value_token_segment(&mut self, tokens: &[super::SyntaxTokenInfo], scope: ScopeId) {
        if tokens.is_empty() {
            return;
        }
        self.ctx
            .collect_token_expression_refs_infos(tokens, scope, true);
    }

    fn value_base_operand_end(&self, tokens: &[super::SyntaxTokenInfo], start: usize) -> usize {
        let mut idx = start;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if self.ctx.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }
            if token.text.eq_ignore_ascii_case("FOR") {
                break;
            }
            if token.text.as_ref() == "("
                && idx > start
                && tokens
                    .get(idx - 1)
                    .is_some_and(|prev| self.ctx.syntax_tokens_have_space_between(prev, token))
            {
                break;
            }
            if self.ctx.syntax_token_is_ident_like(token)
                && tokens.get(idx + 1).map(|next| next.text.as_ref()) == Some("=")
            {
                break;
            }
            idx += 1;
        }
        idx
    }

    fn value_for_source_end(&self, tokens: &[super::SyntaxTokenInfo], start: usize) -> usize {
        let mut idx = start;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if self.ctx.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }
            if token.text.eq_ignore_ascii_case("FOR")
                || token.text.eq_ignore_ascii_case("LET")
                || token.text.eq_ignore_ascii_case("WHERE")
                || token.text.eq_ignore_ascii_case("UNTIL")
                || token.text.eq_ignore_ascii_case("WHILE")
            {
                break;
            }
            if token.text.as_ref() == "("
                && idx > start
                && tokens
                    .get(idx - 1)
                    .is_some_and(|prev| self.ctx.syntax_tokens_have_space_between(prev, token))
            {
                break;
            }
            idx += 1;
        }
        idx
    }

    fn find_top_level_keyword(
        &self,
        tokens: &[super::SyntaxTokenInfo],
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

    fn constructor_assignment_value_end(
        &self,
        tokens: &[super::SyntaxTokenInfo],
        start: usize,
    ) -> usize {
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
            if paren == 0 && bracket == 0 && brace == 0 {
                if token.text.eq_ignore_ascii_case("FOR")
                    || token.text.eq_ignore_ascii_case("LET")
                    || token.text.eq_ignore_ascii_case("IN")
                    || token.text.eq_ignore_ascii_case("BASE")
                    || token.text.eq_ignore_ascii_case("OPTIONAL")
                    || token.text.eq_ignore_ascii_case("INIT")
                    || token.text.eq_ignore_ascii_case("NEXT")
                    || token.text.eq_ignore_ascii_case("WHERE")
                    || token.text.eq_ignore_ascii_case("UNTIL")
                    || token.text.eq_ignore_ascii_case("WHILE")
                    || token.text.eq_ignore_ascii_case("THEN")
                {
                    break;
                }
                if self.is_named_assignment_start(tokens, idx) {
                    break;
                }
            }
            idx += 1;
        }
        idx
    }

    fn find_value_lines_of_keyword(
        &self,
        tokens: &[super::SyntaxTokenInfo],
        start: usize,
    ) -> Option<usize> {
        self.find_top_level_keyword(tokens, start, &["FROM", "TO", "USING"])
    }

    fn value_lines_of_clause_end(&self, tokens: &[super::SyntaxTokenInfo], start: usize) -> usize {
        let mut idx = start;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if self.ctx.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }
            if token.text.eq_ignore_ascii_case("FOR")
                || token.text.eq_ignore_ascii_case("LET")
                || token.text.eq_ignore_ascii_case("BASE")
                || token.text.eq_ignore_ascii_case("OPTIONAL")
                || token.text.eq_ignore_ascii_case("INIT")
                || token.text.eq_ignore_ascii_case("NEXT")
                || token.text.eq_ignore_ascii_case("WHERE")
                || token.text.eq_ignore_ascii_case("UNTIL")
                || token.text.eq_ignore_ascii_case("WHILE")
                || token.text.eq_ignore_ascii_case("THEN")
            {
                break;
            }
            if self.is_named_assignment_start(tokens, idx) {
                break;
            }
            idx += 1;
        }
        idx
    }

    fn is_named_assignment_start(&self, tokens: &[super::SyntaxTokenInfo], idx: usize) -> bool {
        self.ctx.syntax_token_is_ident_like(&tokens[idx])
            && tokens.get(idx + 1).map(|next| next.text.as_ref()) == Some("=")
    }

    fn collect_let_expression(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        start: usize,
        scope: ScopeId,
    ) {
        let Some(in_idx) = self.find_top_level_keyword(tokens, start + 1, &["IN"]) else {
            self.ctx
                .collect_token_expression_refs_infos(&tokens[start..], scope, true);
            return;
        };
        let Some(last) = tokens.last() else {
            return;
        };
        let let_scope = self.ctx.push_scope(
            crate::scope::ScopeKind::LoopBlock,
            tokens[start].range.start..last.range.end,
            Some(scope),
            None,
        );

        let mut idx = start + 1;
        while idx < in_idx {
            if self.ctx.syntax_token_is_comment(&tokens[idx]) {
                idx += 1;
                continue;
            }
            if !self.is_named_assignment_start(tokens, idx) {
                self.ctx
                    .collect_token_expression_refs_infos(&tokens[idx..in_idx], let_scope, true);
                break;
            }
            let name_tok = &tokens[idx];
            let value_end = self.constructor_assignment_value_end(&tokens[..in_idx], idx + 2);
            self.ctx.collect_token_expression_refs_infos(
                &tokens[idx + 2..value_end],
                let_scope,
                true,
            );
            let (structure, declared_type) =
                self.inferred_metadata_from_tokens(&tokens[idx + 2..value_end], let_scope);
            let symbol_kind = if self.is_field_symbol_name(name_tok.text.as_ref()) {
                SymbolKind::FieldSymbol
            } else {
                SymbolKind::Variable
            };
            self.ctx.declare_symbol(
                let_scope,
                Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
                symbol_kind,
                name_tok.range.clone(),
                structure,
                declared_type,
                None,
                None,
            );
            idx = value_end;
        }

        self.collect_value_constructor_tokens(&tokens[in_idx + 1..], let_scope);
    }

    fn collect_value_for_clause(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        start: usize,
        scope: ScopeId,
    ) {
        let Some(name_tok) = tokens.get(start + 1) else {
            return;
        };
        let Some(third_tok) = tokens.get(start + 2) else {
            return;
        };

        if !self.ctx.syntax_token_is_ident_like(name_tok) {
            self.ctx
                .collect_token_expression_refs_infos(&tokens[start + 1..], scope, true);
            return;
        }

        if third_tok.text.as_ref() == "=" {
            self.collect_conditional_for_clause(tokens, start, scope);
            return;
        }

        if !third_tok.text.eq_ignore_ascii_case("IN") {
            self.ctx
                .collect_token_expression_refs_infos(&tokens[start + 1..], scope, true);
            return;
        }

        let source_start = start + 3;
        let source_end = self.value_for_source_end(tokens, source_start);
        let source_access = self.value_access_from_infos(&tokens[source_start..source_end], scope);
        self.ctx.collect_token_expression_refs_infos(
            &tokens[source_start..source_end],
            scope,
            true,
        );

        let Some(last) = tokens.last() else {
            return;
        };
        let child_scope = self.ctx.push_scope(
            crate::scope::ScopeKind::LoopBlock,
            name_tok.range.start..last.range.end,
            Some(scope),
            None,
        );
        self.ctx.declare_symbol(
            child_scope,
            Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
            SymbolKind::Variable,
            name_tok.range.clone(),
            None,
            None,
            None,
            None,
        );
        let mut cursor = source_end;
        if tokens
            .get(cursor)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("WHERE"))
        {
            let condition_end = self.value_for_where_condition_end(tokens, cursor + 1);
            if condition_end > cursor + 1
                && let Some(source_access) = source_access
            {
                self.ctx
                    .push_loop_where_field_context(crate::def_map::LoopWhereFieldContext {
                        scope: child_scope,
                        range: tokens[cursor].range.start..tokens[condition_end - 1].range.end,
                        source_access,
                        target_access: Some(FieldAccess {
                            scope: child_scope,
                            base_namespace: Namespace::Value,
                            base_name: Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
                            field_path: Vec::new(),
                            in_type_position: false,
                        }),
                    });
            }
            self.collect_reduce_where_condition_tokens(
                &tokens[cursor + 1..condition_end],
                child_scope,
            );
            cursor = condition_end;
        }
        self.collect_value_constructor_tokens(&tokens[cursor..], child_scope);
    }

    fn value_for_where_condition_end(
        &self,
        tokens: &[super::SyntaxTokenInfo],
        start: usize,
    ) -> usize {
        let mut idx = start;
        while tokens
            .get(idx)
            .is_some_and(|token| self.ctx.syntax_token_is_comment(token))
        {
            idx += 1;
        }
        if tokens.get(idx).map(|token| token.text.as_ref()) == Some("(")
            && let Some(end_idx) = self
                .ctx
                .find_matching_group_end_infos(tokens, idx, "(", ")")
        {
            return end_idx + 1;
        }
        self.find_top_level_keyword(tokens, start, &["LET", "FOR"])
            .unwrap_or(tokens.len())
    }

    fn collect_conditional_for_clause(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        start: usize,
        scope: ScopeId,
    ) {
        let Some(name_tok) = tokens.get(start + 1) else {
            return;
        };
        let Some(term_idx) =
            self.find_top_level_keyword(tokens, start + 3, &["THEN", "UNTIL", "WHILE"])
        else {
            self.ctx
                .collect_token_expression_refs_infos(&tokens[start + 1..], scope, true);
            return;
        };

        self.ctx
            .collect_token_expression_refs_infos(&tokens[start + 3..term_idx], scope, true);
        let (structure, declared_type) =
            self.inferred_metadata_from_tokens(&tokens[start + 3..term_idx], scope);

        let Some(last) = tokens.last() else {
            return;
        };
        let child_scope = self.ctx.push_scope(
            crate::scope::ScopeKind::LoopBlock,
            name_tok.range.start..last.range.end,
            Some(scope),
            None,
        );
        self.ctx.declare_symbol(
            child_scope,
            Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
            SymbolKind::Variable,
            name_tok.range.clone(),
            structure,
            declared_type,
            None,
            None,
        );

        let mut cursor = term_idx;
        if tokens[cursor].text.eq_ignore_ascii_case("THEN") {
            let Some(next_term_idx) =
                self.find_top_level_keyword(tokens, cursor + 1, &["UNTIL", "WHILE"])
            else {
                self.ctx.collect_token_expression_refs_infos(
                    &tokens[cursor + 1..],
                    child_scope,
                    true,
                );
                return;
            };
            self.ctx.collect_token_expression_refs_infos(
                &tokens[cursor + 1..next_term_idx],
                child_scope,
                true,
            );
            cursor = next_term_idx;
        }

        let condition_end = self
            .find_top_level_keyword(tokens, cursor + 1, &["LET", "FOR", "NEXT"])
            .unwrap_or(tokens.len());
        self.ctx.collect_token_expression_refs_infos(
            &tokens[cursor + 1..condition_end],
            child_scope,
            true,
        );

        if condition_end < tokens.len() && tokens[condition_end].text.eq_ignore_ascii_case("LET") {
            self.collect_let_expression(tokens, condition_end, child_scope);
        } else {
            self.collect_value_constructor_tokens(&tokens[condition_end..], child_scope);
        }
    }

    fn collect_reduce_constructor_tokens(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        if tokens.is_empty() {
            return;
        }
        if tokens[0].text.eq_ignore_ascii_case("LET") {
            self.collect_let_expression(tokens, 0, scope);
            return;
        }
        let Some(init_idx) = self.find_top_level_keyword(tokens, 0, &["INIT"]) else {
            self.ctx
                .collect_token_expression_refs_infos(tokens, scope, true);
            return;
        };
        let Some(for_idx) = self.find_top_level_keyword(tokens, init_idx + 1, &["FOR"]) else {
            self.ctx
                .collect_token_expression_refs_infos(&tokens[init_idx + 1..], scope, true);
            return;
        };
        let Some(last) = tokens.last() else {
            return;
        };
        let reduce_scope = self.ctx.push_scope(
            crate::scope::ScopeKind::LoopBlock,
            tokens[init_idx].range.start..last.range.end,
            Some(scope),
            None,
        );
        self.collect_reduce_init_decls(tokens, init_idx + 1, for_idx, scope, reduce_scope);
        self.collect_reduce_tail(&tokens[for_idx..], reduce_scope);
    }

    fn collect_reduce_init_decls(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        start: usize,
        end: usize,
        value_scope: ScopeId,
        decl_scope: ScopeId,
    ) {
        let mut idx = start;
        while idx < end {
            if self.ctx.syntax_token_is_comment(&tokens[idx]) {
                idx += 1;
                continue;
            }
            if self.is_named_assignment_start(tokens, idx) {
                let name_tok = &tokens[idx];
                let value_end = self.constructor_assignment_value_end(&tokens[..end], idx + 2);
                self.ctx.collect_token_expression_refs_infos(
                    &tokens[idx + 2..value_end],
                    value_scope,
                    true,
                );
                self.ctx.declare_symbol(
                    decl_scope,
                    Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
                    SymbolKind::Variable,
                    name_tok.range.clone(),
                    None,
                    None,
                    None,
                    None,
                );
                idx = value_end;
                continue;
            }
            if self.ctx.syntax_token_is_ident_like(&tokens[idx])
                && tokens
                    .get(idx + 1)
                    .is_some_and(|token| token.text.eq_ignore_ascii_case("TYPE"))
            {
                let name_tok = &tokens[idx];
                self.ctx.declare_symbol(
                    decl_scope,
                    Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
                    SymbolKind::Variable,
                    name_tok.range.clone(),
                    None,
                    None,
                    None,
                    None,
                );
                idx += 2;
                while idx < end
                    && !self.ctx.syntax_token_is_comment(&tokens[idx])
                    && !self.is_named_assignment_start(tokens, idx)
                    && !tokens[idx].text.eq_ignore_ascii_case("FOR")
                {
                    idx += 1;
                }
                continue;
            }
            self.ctx
                .collect_token_expression_refs_infos(&tokens[idx..end], value_scope, true);
            break;
        }
    }

    fn collect_reduce_tail(&mut self, tokens: &[super::SyntaxTokenInfo], scope: ScopeId) {
        if tokens.is_empty() {
            return;
        }
        let Some(next_idx) = self.find_top_level_keyword(tokens, 0, &["NEXT"]) else {
            self.collect_reduce_iteration_chain(tokens, scope);
            return;
        };
        let next_scope = self.collect_reduce_iteration_chain(&tokens[..next_idx], scope);
        self.collect_reduce_next_assignments(&tokens[next_idx + 1..], next_scope);
    }

    fn collect_reduce_next_assignments(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        let mut idx = 0usize;
        while idx < tokens.len() {
            if self.ctx.syntax_token_is_comment(&tokens[idx]) {
                idx += 1;
                continue;
            }
            if !self.is_named_assignment_start(tokens, idx) {
                self.ctx
                    .collect_token_expression_refs_infos(&tokens[idx..], scope, true);
                break;
            }
            let value_end = self.constructor_assignment_value_end(tokens, idx + 2);
            self.ctx
                .collect_token_expression_refs_infos(&tokens[idx + 2..value_end], scope, true);
            idx = value_end;
        }
    }

    fn collect_reduce_iteration_chain(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
    ) -> ScopeId {
        if tokens.is_empty() {
            return scope;
        }
        let mut idx = 0usize;
        while idx < tokens.len() && self.ctx.syntax_token_is_comment(&tokens[idx]) {
            idx += 1;
        }
        let Some(token) = tokens.get(idx) else {
            return scope;
        };
        if token.text.eq_ignore_ascii_case("LET") {
            return self.collect_reduce_let_chain(tokens, idx, scope);
        }
        if token.text.eq_ignore_ascii_case("FOR") {
            return self.collect_reduce_for_chain(tokens, idx, scope);
        }
        self.ctx
            .collect_token_expression_refs_infos(&tokens[idx..], scope, true);
        scope
    }

    fn collect_reduce_let_chain(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        start: usize,
        scope: ScopeId,
    ) -> ScopeId {
        let Some(in_idx) = self.find_top_level_keyword(tokens, start + 1, &["IN"]) else {
            self.ctx
                .collect_token_expression_refs_infos(&tokens[start..], scope, true);
            return scope;
        };
        let Some(last) = tokens.last() else {
            return scope;
        };
        let let_scope = self.ctx.push_scope(
            crate::scope::ScopeKind::LoopBlock,
            tokens[start].range.start..last.range.end,
            Some(scope),
            None,
        );

        let mut idx = start + 1;
        while idx < in_idx {
            if self.ctx.syntax_token_is_comment(&tokens[idx]) {
                idx += 1;
                continue;
            }
            if !self.is_named_assignment_start(tokens, idx) {
                self.ctx
                    .collect_token_expression_refs_infos(&tokens[idx..in_idx], let_scope, true);
                break;
            }
            let name_tok = &tokens[idx];
            let value_end = self.constructor_assignment_value_end(&tokens[..in_idx], idx + 2);
            self.ctx.collect_token_expression_refs_infos(
                &tokens[idx + 2..value_end],
                let_scope,
                true,
            );
            let (structure, declared_type) =
                self.inferred_metadata_from_tokens(&tokens[idx + 2..value_end], let_scope);
            let symbol_kind = if self.is_field_symbol_name(name_tok.text.as_ref()) {
                SymbolKind::FieldSymbol
            } else {
                SymbolKind::Variable
            };
            self.ctx.declare_symbol(
                let_scope,
                Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
                symbol_kind,
                name_tok.range.clone(),
                structure,
                declared_type,
                None,
                None,
            );
            idx = value_end;
        }

        self.collect_reduce_iteration_chain(&tokens[in_idx + 1..], let_scope)
    }

    fn is_field_symbol_name(&self, name: &str) -> bool {
        name.starts_with('<') && name.ends_with('>')
    }

    fn collect_reduce_for_chain(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        start: usize,
        scope: ScopeId,
    ) -> ScopeId {
        let Some(name_tok) = tokens.get(start + 1) else {
            return scope;
        };
        let Some(third_tok) = tokens.get(start + 2) else {
            return scope;
        };
        if !self.ctx.syntax_token_is_ident_like(name_tok) {
            self.ctx
                .collect_token_expression_refs_infos(&tokens[start + 1..], scope, true);
            return scope;
        }

        let Some(last) = tokens.last() else {
            return scope;
        };
        let child_scope = self.ctx.push_scope(
            crate::scope::ScopeKind::LoopBlock,
            name_tok.range.start..last.range.end,
            Some(scope),
            None,
        );
        self.ctx.declare_symbol(
            child_scope,
            Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
            SymbolKind::Variable,
            name_tok.range.clone(),
            None,
            None,
            None,
            None,
        );

        if third_tok.text.eq_ignore_ascii_case("IN") {
            let source_start = start + 3;
            let source_end = self.value_for_source_end(tokens, source_start);
            let source_access =
                self.value_access_from_infos(&tokens[source_start..source_end], scope);
            self.ctx.collect_token_expression_refs_infos(
                &tokens[source_start..source_end],
                scope,
                true,
            );
            let mut cursor = source_end;
            if tokens
                .get(cursor)
                .is_some_and(|token| token.text.eq_ignore_ascii_case("WHERE"))
            {
                let condition_end = self
                    .find_top_level_keyword(tokens, cursor + 1, &["LET", "FOR", "NEXT"])
                    .unwrap_or(tokens.len());
                if condition_end > cursor + 1
                    && let Some(source_access) = source_access
                {
                    self.ctx
                        .push_loop_where_field_context(crate::def_map::LoopWhereFieldContext {
                            scope: child_scope,
                            range: tokens[cursor].range.start..tokens[condition_end - 1].range.end,
                            source_access,
                            target_access: Some(FieldAccess {
                                scope: child_scope,
                                base_namespace: Namespace::Value,
                                base_name: Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
                                field_path: Vec::new(),
                                in_type_position: false,
                            }),
                        });
                }
                self.collect_reduce_where_condition_tokens(
                    &tokens[cursor + 1..condition_end],
                    child_scope,
                );
                cursor = condition_end;
            }
            return self.collect_reduce_iteration_chain(&tokens[cursor..], child_scope);
        }

        if third_tok.text.as_ref() == "=" {
            let Some(term_idx) =
                self.find_top_level_keyword(tokens, start + 3, &["THEN", "UNTIL", "WHILE"])
            else {
                self.ctx
                    .collect_token_expression_refs_infos(&tokens[start + 3..], scope, true);
                return child_scope;
            };
            self.ctx
                .collect_token_expression_refs_infos(&tokens[start + 3..term_idx], scope, true);

            let mut cursor = term_idx;
            if tokens[cursor].text.eq_ignore_ascii_case("THEN") {
                let Some(next_term_idx) =
                    self.find_top_level_keyword(tokens, cursor + 1, &["UNTIL", "WHILE"])
                else {
                    self.ctx.collect_token_expression_refs_infos(
                        &tokens[cursor + 1..],
                        child_scope,
                        true,
                    );
                    return child_scope;
                };
                self.ctx.collect_token_expression_refs_infos(
                    &tokens[cursor + 1..next_term_idx],
                    child_scope,
                    true,
                );
                cursor = next_term_idx;
            }

            let condition_end = self
                .find_top_level_keyword(tokens, cursor + 1, &["LET", "FOR"])
                .unwrap_or(tokens.len());
            self.ctx.collect_token_expression_refs_infos(
                &tokens[cursor + 1..condition_end],
                child_scope,
                true,
            );
            return self.collect_reduce_iteration_chain(&tokens[condition_end..], child_scope);
        }

        self.ctx
            .collect_token_expression_refs_infos(&tokens[start + 1..], scope, true);
        scope
    }

    fn value_access_from_infos(
        &self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
    ) -> Option<FieldAccess> {
        let first_idx = tokens
            .iter()
            .position(|token| !self.ctx.syntax_token_is_comment(token))?;
        if let Some((next_idx, namespace, base_name, _, field_path, _)) = self
            .ctx
            .consume_selector_access_from_infos(tokens, first_idx)
            && next_idx == tokens.len()
            && namespace == Namespace::Value
        {
            return Some(FieldAccess {
                scope,
                base_namespace: namespace,
                base_name,
                field_path,
                in_type_position: false,
            });
        }

        let token = &tokens[first_idx];
        self.ctx
            .syntax_token_is_ident_like(token)
            .then(|| FieldAccess {
                scope,
                base_namespace: Namespace::Value,
                base_name: Arc::<str>::from(token.text.to_ascii_lowercase()),
                field_path: Vec::new(),
                in_type_position: false,
            })
    }

    fn collect_reduce_where_condition_tokens(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        self.ctx
            .collect_token_expression_refs_infos(tokens, scope, true);
        self.collect_comparison_lhs_identifier_refs(tokens, scope);
    }

    fn collect_comparison_lhs_identifier_refs(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        let mut idx = 0usize;
        while idx < tokens.len() {
            let token = &tokens[idx];
            match token.text.as_ref() {
                "(" | "[" | "{" => {
                    let (open, close) = match token.text.as_ref() {
                        "(" => ("(", ")"),
                        "[" => ("[", "]"),
                        "{" => ("{", "}"),
                        _ => unreachable!(),
                    };
                    if let Some(end_idx) = self
                        .ctx
                        .find_matching_group_end_infos(tokens, idx, open, close)
                    {
                        self.collect_comparison_lhs_identifier_refs(
                            &tokens[idx + 1..end_idx],
                            scope,
                        );
                        idx = end_idx + 1;
                        continue;
                    }
                }
                _ => {}
            }

            if self.ctx.syntax_token_is_ident_like(token)
                && tokens.get(idx + 1).map(|next| next.text.as_ref()) == Some("=")
                && !matches!(
                    idx.checked_sub(1)
                        .and_then(|prev| tokens.get(prev))
                        .map(|prev| prev.text.as_ref()),
                    Some("->" | "=>" | "~" | "-")
                )
                && !matches!(
                    token.text.to_ascii_uppercase().as_str(),
                    "AND"
                        | "OR"
                        | "NOT"
                        | "IS"
                        | "IN"
                        | "LET"
                        | "FOR"
                        | "WHERE"
                        | "UNTIL"
                        | "WHILE"
                        | "INIT"
                        | "NEXT"
                        | "WHEN"
                        | "THEN"
                        | "ELSE"
                )
            {
                self.ctx.add_reference(
                    scope,
                    Arc::<str>::from(token.text.to_ascii_lowercase()),
                    Namespace::Value,
                    ReferenceKind::Identifier,
                    token.range.clone(),
                );
            }

            idx += 1;
        }
    }

    fn collect_assign_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let mut non_token_children = self
            .ctx
            .file()
            .children(node)
            .filter(|&child| self.kind(child) != SyntaxKind::Token);
        let Some(lhs) = non_token_children.next() else {
            return;
        };
        let Some(rhs) = non_token_children.next() else {
            self.collect_expr(lhs, scope);
            return;
        };

        let inferred_metadata = self
            .ctx
            .control_lowering()
            .loop_source_line_metadata_from_node(rhs, scope);

        let lhs_inline = if self.kind(lhs) == SyntaxKind::DataInlineDecl {
            Some(lhs)
        } else {
            self.ctx
                .file()
                .find_first_kind(lhs, SyntaxKind::DataInlineDecl)
        };

        if let Some(lhs_inline) = lhs_inline {
            self.declare_inline_assign_target(lhs_inline, scope, &inferred_metadata);
        } else {
            self.collect_expr(lhs, scope);
        }
        self.collect_expr(rhs, scope);
    }

    fn declare_inline_assign_target(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        inferred_metadata: &(Option<StructureId>, Option<FieldTypeRefData>),
    ) {
        let decl_scope = self.ctx.declaration_scope(scope);
        for child in self.ctx.file().children(node) {
            if self.kind(child) == SyntaxKind::DataDeclName
                && let Some((name, range)) = self.ctx.node_name(child)
            {
                self.ctx.declare_symbol(
                    decl_scope,
                    name,
                    SymbolKind::Variable,
                    range,
                    inferred_metadata.0,
                    inferred_metadata.1.clone(),
                    None,
                    None,
                );
                break;
            }
        }
    }

    pub(super) fn collect_named_arguments_from_infos(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
        target: NamedArgumentTarget,
    ) {
        let mut idx = 0usize;
        let mut segment_start = 0usize;
        let mut current_section = None;
        let mut paren_depth = 0i32;
        let mut bracket_depth = 0i32;
        let mut brace_depth = 0i32;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if self.ctx.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }
            let at_top_level = paren_depth == 0 && bracket_depth == 0 && brace_depth == 0;
            if at_top_level
                && self.ctx.syntax_token_is_ident_like(token)
                && let Some(section) = self
                    .ctx
                    .named_argument_section_from_text(token.text.as_ref())
            {
                if segment_start < idx {
                    self.ctx.collect_token_expression_refs_infos(
                        &tokens[segment_start..idx],
                        scope,
                        true,
                    );
                }
                current_section = Some(section);
                idx += 1;
                segment_start = idx;
                continue;
            }
            if at_top_level
                && self.ctx.syntax_token_is_ident_like(token)
                && tokens.get(idx + 1).map(|next| next.text.as_ref()) == Some("=")
            {
                if segment_start < idx {
                    self.ctx.collect_token_expression_refs_infos(
                        &tokens[segment_start..idx],
                        scope,
                        true,
                    );
                }
                let argument_name = Arc::<str>::from(token.text.to_ascii_lowercase());
                let value_start = idx + 2;
                let value_end = self.ctx.call_argument_value_end_infos(tokens, value_start);
                self.ctx.emit_named_argument(NamedArgumentAccess {
                    scope,
                    name: Arc::clone(&argument_name),
                    range: token.range.clone(),
                    section: current_section,
                    target: target.clone(),
                });
                let consumed_inline_target = self.ctx.declare_inline_named_argument_target_infos(
                    scope,
                    &target,
                    current_section,
                    argument_name,
                    &tokens[value_start..value_end],
                );
                if !consumed_inline_target {
                    self.ctx.collect_token_expression_refs_infos(
                        &tokens[value_start..value_end],
                        scope,
                        true,
                    );
                }
                idx = value_end;
                segment_start = idx;
                continue;
            }
            match token.text.as_ref() {
                "(" => paren_depth += 1,
                ")" => paren_depth -= 1,
                "[" => bracket_depth += 1,
                "]" => bracket_depth -= 1,
                "{" => brace_depth += 1,
                "}" => brace_depth -= 1,
                _ => {}
            }
            idx += 1;
        }
        if segment_start < tokens.len() {
            self.ctx
                .collect_token_expression_refs_infos(&tokens[segment_start..], scope, true);
        }
    }

    pub(super) fn call_arg_section_from_node(&self, node: NodeId) -> Option<NamedArgumentSection> {
        abap_ast::ast::CallArgSection::cast(self.ctx.syntax(node))
            .and_then(|section| section.first_token())
            .and_then(|token| token.text(self.source()))
            .and_then(|text| self.ctx.named_argument_section_from_text(text))
    }

    pub(super) fn collect_structured_argument_values(&mut self, nodes: &[NodeId], scope: ScopeId) {
        if nodes.is_empty() {
            return;
        }
        if nodes
            .iter()
            .all(|&node| self.kind(node) == SyntaxKind::Token)
        {
            let tokens = nodes
                .iter()
                .flat_map(|&node| self.ctx.syntax_token_nodes(node))
                .collect::<Vec<_>>();
            self.ctx
                .collect_token_expression_refs_infos(&tokens, scope, true);
            return;
        }

        for &node in nodes {
            match self.kind(node) {
                SyntaxKind::DataInlineDecl => {
                    self.ctx.decl_lowering().walk_inline_decl(node, scope)
                }
                SyntaxKind::FieldSymbolInlineDecl => self
                    .ctx
                    .decl_lowering()
                    .walk_inline_field_symbol_decl(node, scope),
                SyntaxKind::Token => {}
                _ => self.collect_expr(node, scope),
            }
        }
    }

    pub(super) fn collect_structured_argument_values_from_children(
        &mut self,
        node: NodeId,
        scope: ScopeId,
    ) {
        let mut all_tokens = true;
        for child in self.ctx.file().children(node) {
            if self.kind(child) != SyntaxKind::Token {
                all_tokens = false;
                break;
            }
        }
        if all_tokens {
            let mut tokens = Vec::new();
            for child in self.ctx.file().children(node) {
                tokens.extend(self.ctx.syntax_token_nodes(child));
            }
            self.ctx
                .collect_token_expression_refs_infos(&tokens, scope, true);
            return;
        }

        for child in self.ctx.file().children(node) {
            match self.kind(child) {
                SyntaxKind::CallArgSection => {}
                SyntaxKind::CallNamedArg => {
                    let value_children: Vec<_> = CallNamedArg::cast(self.ctx.syntax(child))
                        .map(|arg| {
                            arg.value_children()
                                .into_iter()
                                .map(|child| child.id())
                                .collect()
                        })
                        .unwrap_or_default();
                    self.collect_structured_argument_values(&value_children, scope);
                }
                SyntaxKind::CallPositionalArg => {
                    let value_children: Vec<_> = CallPositionalArg::cast(self.ctx.syntax(child))
                        .map(|arg| {
                            arg.value_children()
                                .into_iter()
                                .map(|child| child.id())
                                .collect()
                        })
                        .unwrap_or_default();
                    self.collect_structured_argument_values(&value_children, scope);
                }
                SyntaxKind::DataInlineDecl => {
                    self.ctx.decl_lowering().walk_inline_decl(child, scope)
                }
                SyntaxKind::FieldSymbolInlineDecl => self
                    .ctx
                    .decl_lowering()
                    .walk_inline_field_symbol_decl(child, scope),
                SyntaxKind::Token => {}
                _ => self.collect_expr(child, scope),
            }
        }
    }

    pub(super) fn collect_structured_named_argument(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        target: &NamedArgumentTarget,
        section: Option<NamedArgumentSection>,
    ) {
        let Some(named_arg) = CallNamedArg::cast(self.ctx.syntax(node)) else {
            return;
        };
        let value_children: Vec<_> = named_arg
            .value_children()
            .into_iter()
            .map(|child| child.id())
            .collect();
        let (argument_name, argument_range) = {
            let Some(name_token) = named_arg.name_token() else {
                return;
            };
            let Some(name_text) = name_token.text(self.source()) else {
                return;
            };
            (
                Arc::<str>::from(name_text.to_ascii_lowercase()),
                name_token.range(),
            )
        };
        self.ctx.emit_named_argument(NamedArgumentAccess {
            scope,
            name: Arc::clone(&argument_name),
            range: argument_range,
            section,
            target: target.clone(),
        });

        let consumed_inline_target = self
            .ctx
            .declare_inline_named_argument_target_from_nodes(
                scope,
                target,
                section,
                Arc::clone(&argument_name),
                &value_children,
            )
            .unwrap_or_else(|| {
                let value_tokens = value_children
                    .iter()
                    .flat_map(|&child| self.ctx.syntax_token_nodes(child))
                    .collect::<Vec<_>>();
                self.ctx.declare_inline_named_argument_target_infos(
                    scope,
                    target,
                    section,
                    argument_name,
                    &value_tokens,
                )
            });
        if !consumed_inline_target {
            self.collect_structured_argument_values(&value_children, scope);
        }
    }

    pub(super) fn collect_call_argument_list(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        target: NamedArgumentTarget,
    ) {
        let Some(arg_list) = CallArgList::cast(self.ctx.syntax(node)) else {
            return;
        };
        let mut current_section = None;
        let items: Vec<_> = arg_list
            .items()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in items {
            match kind_syntax {
                SyntaxKind::CallArgSection => {
                    current_section = self.call_arg_section_from_node(child);
                }
                SyntaxKind::CallNamedArg => {
                    self.collect_structured_named_argument(child, scope, &target, current_section);
                }
                SyntaxKind::CallPositionalArg => {
                    let value_children: Vec<_> = CallPositionalArg::cast(self.ctx.syntax(child))
                        .map(|arg| {
                            arg.value_children()
                                .into_iter()
                                .map(|child| child.id())
                                .collect()
                        })
                        .unwrap_or_default();
                    self.collect_structured_argument_values(&value_children, scope);
                }
                _ => {}
            }
        }
    }

    pub(super) fn collect_selector_expr(&mut self, node: NodeId, scope: ScopeId) {
        if let Some((namespace, base_name, base_range, field_path)) =
            self.ctx.selector_access_chain(node)
        {
            let kind = if namespace == Namespace::Type {
                ReferenceKind::StaticTarget
            } else {
                ReferenceKind::Identifier
            };
            self.ctx
                .add_reference(scope, Arc::clone(&base_name), namespace, kind, base_range);
            if !field_path.is_empty() {
                self.ctx.emit_field_access(FieldAccess {
                    scope,
                    base_namespace: namespace,
                    base_name,
                    field_path,
                    in_type_position: false,
                });
            }
            return;
        }

        let mut children = self.ctx.file().children(node);
        let base = children.next();
        let op = children.next();
        let field = children.next();
        let Some(base) = base else {
            return;
        };
        let namespace = match op.and_then(|op_node| self.ctx.syntax(op_node).text(self.source())) {
            Some("=>") => Namespace::Type,
            _ => Namespace::Value,
        };
        match self.kind(base) {
            SyntaxKind::ExprIdent => {
                if let Some((name, range)) = self.ctx.node_name(base) {
                    let kind = if namespace == Namespace::Type {
                        ReferenceKind::StaticTarget
                    } else {
                        ReferenceKind::Identifier
                    };
                    self.ctx.add_reference(scope, name, namespace, kind, range);
                }
            }
            _ => self.collect_expr(base, scope),
        }
        if let Some(field_node) = field
            && self.kind(field_node) != SyntaxKind::ExprIdent
        {
            self.collect_expr(field_node, scope);
        }
    }

    pub(super) fn collect_substring_expr(&mut self, node: NodeId, scope: ScopeId) {
        let mut children = self.ctx.file().children(node);
        let Some(base) = children.next() else {
            return;
        };

        match self.kind(base) {
            SyntaxKind::ExprIdent => {
                if let Some((name, range)) = self.ctx.node_name(base) {
                    let namespace = if self
                        .ctx
                        .lookup_symbol_in_scope_chain(scope, Namespace::Value, name.as_ref())
                        .is_some()
                        || builtin_routine_spec(name.as_ref()).is_none()
                    {
                        Namespace::Value
                    } else {
                        Namespace::Routine
                    };
                    let kind = if namespace == Namespace::Routine {
                        ReferenceKind::RoutineCall
                    } else {
                        ReferenceKind::Identifier
                    };
                    self.ctx.add_reference(scope, name, namespace, kind, range);
                }
            }
            SyntaxKind::SelectorExpr => self.collect_selector_expr(base, scope),
            _ => self.collect_expr(base, scope),
        }

        for child in children {
            if self.kind(child) != SyntaxKind::Token {
                self.collect_expr(child, scope);
            }
        }
    }

    pub(super) fn collect_call_expr(&mut self, node: NodeId, scope: ScopeId) {
        let Some(call) = CallExpr::cast(self.ctx.syntax(node)) else {
            return;
        };
        let callee = call.callee().map(|callee| (callee.id(), callee.kind()));
        let arg_list = call.arg_list().map(|arg_list| arg_list.syntax().id());
        if let Some((callee_id, callee_kind)) = callee {
            match callee_kind {
                SyntaxKind::ExprIdent => {
                    if let Some((name, range)) = self.ctx.node_name(callee_id) {
                        self.ctx.add_reference(
                            scope,
                            Arc::clone(&name),
                            Namespace::Routine,
                            ReferenceKind::RoutineCall,
                            range,
                        );
                        if let Some(arg_list) = arg_list {
                            let target =
                                if crate::builtins::builtin_routine_spec(name.as_ref()).is_some() {
                                    NamedArgumentTarget::Routine {
                                        routine_name: Arc::clone(&name),
                                    }
                                } else {
                                    NamedArgumentTarget::ImplicitMethod {
                                        method_name: Arc::clone(&name),
                                    }
                                };
                            self.collect_call_argument_list(arg_list, scope, target);
                        }
                    }
                }
                _ => self.collect_expr(callee_id, scope),
            }
            if let Some(target) = self.ctx.named_argument_target_for_callee(callee_id)
                && let Some(arg_list) = arg_list
            {
                self.collect_call_argument_list(arg_list, scope, target);
            }
        }
    }
}
