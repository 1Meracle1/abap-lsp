use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_ast::ast::{AstNode, DataDeclName};
use abap_lexer::TextRange;

use crate::def_map::{
    FieldAccess, FieldAccessSegment, FieldTypeRefData, NamedArgumentSection, NamedArgumentTarget,
    ReferenceKind, SymbolKind,
};
use crate::ids::{ScopeId, SymbolId};
use crate::scope::Namespace;

use super::emit::RefSink;
use super::{Collector, SyntaxTokenInfo};

impl<'a> Collector<'a> {
    fn lower_arc(text: &str) -> Arc<str> {
        Arc::<str>::from(text.to_ascii_lowercase())
    }

    pub(super) fn collect_token_expression_refs_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
        allow_leading_value_ident: bool,
    ) {
        let mut idx = 0usize;
        while idx < tokens.len() {
            let token = &tokens[idx];
            match token.text.as_ref() {
                text if text.trim_start().starts_with('"') => {
                    idx += 1;
                }
                _ if self.syntax_token_is_literal_like(token) => {
                    idx += 1;
                }
                "(" => {
                    if let Some(end_idx) = self.find_matching_group_end_infos(tokens, idx, "(", ")")
                    {
                        self.collect_token_expression_refs_infos(
                            &tokens[idx + 1..end_idx],
                            scope,
                            true,
                        );
                        idx = end_idx + 1;
                    } else {
                        idx += 1;
                    }
                }
                "[" => {
                    if let Some(end_idx) = self.find_matching_group_end_infos(tokens, idx, "[", "]")
                    {
                        self.collect_token_expression_refs_infos(
                            &tokens[idx + 1..end_idx],
                            scope,
                            true,
                        );
                        idx = end_idx + 1;
                    } else {
                        idx += 1;
                    }
                }
                "{" => {
                    if let Some(end_idx) = self.find_matching_group_end_infos(tokens, idx, "{", "}")
                    {
                        self.collect_token_expression_refs_infos(
                            &tokens[idx + 1..end_idx],
                            scope,
                            true,
                        );
                        idx = end_idx + 1;
                    } else {
                        idx += 1;
                    }
                }
                text if self.syntax_token_is_ident_like(token) => {
                    if text.eq_ignore_ascii_case("new") {
                        idx = self.collect_new_expression_infos(tokens, idx, scope);
                        continue;
                    }
                    if let Some((next_idx, namespace, base_name, base_range, field_path)) =
                        self.consume_selector_access_from_infos(tokens, idx)
                    {
                        let method_name =
                            field_path.last().map(|segment| Arc::clone(&segment.name));
                        let kind = if namespace == Namespace::Type {
                            ReferenceKind::StaticTarget
                        } else {
                            ReferenceKind::Identifier
                        };
                        self.add_reference(scope, base_name.clone(), namespace, kind, base_range);
                        if !field_path.is_empty() {
                            self.emit_field_access(FieldAccess {
                                scope,
                                base_namespace: namespace,
                                base_name: Arc::clone(&base_name),
                                field_path,
                                in_type_position: false,
                            });
                        }
                        idx = next_idx;
                        if tokens.get(idx).map(|token| token.text.as_ref()) == Some("(")
                            && let Some(end_idx) =
                                self.find_matching_group_end_infos(tokens, idx, "(", ")")
                        {
                            if let Some(method_name) = method_name {
                                self.expr_lowering().collect_named_arguments_from_infos(
                                    &tokens[idx + 1..end_idx],
                                    scope,
                                    NamedArgumentTarget::Method {
                                        base_namespace: namespace,
                                        base_name: Arc::clone(&base_name),
                                        method_name,
                                    },
                                );
                            } else {
                                self.collect_token_expression_refs_infos(
                                    &tokens[idx + 1..end_idx],
                                    scope,
                                    true,
                                );
                            }
                            idx = end_idx + 1;
                        }
                        continue;
                    }
                    if self.token_is_expression_value_ident_info(
                        tokens,
                        idx,
                        allow_leading_value_ident,
                    ) {
                        self.add_reference(
                            scope,
                            Self::lower_arc(text),
                            Namespace::Value,
                            ReferenceKind::Identifier,
                            token.range.clone(),
                        );
                    }
                    idx += 1;
                }
                _ => {
                    idx += 1;
                }
            }
        }
    }

    fn collect_new_expression_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        idx: usize,
        scope: ScopeId,
    ) -> usize {
        let mut cursor = idx + 1;
        while tokens
            .get(cursor)
            .is_some_and(|token| self.syntax_token_is_comment(token))
        {
            cursor += 1;
        }
        let Some(lparen_idx) = tokens[cursor..]
            .iter()
            .position(|token| token.text.as_ref() == "(")
            .map(|relative| cursor + relative)
        else {
            return idx + 1;
        };
        if let Some((name, range)) =
            self.simple_type_ref_base_from_infos(&tokens[cursor..lparen_idx])
        {
            self.add_reference(scope, name, Namespace::Type, ReferenceKind::TypeRef, range);
        }
        if let Some(rparen_idx) = self.find_matching_group_end_infos(tokens, lparen_idx, "(", ")") {
            if let Some((name, _)) =
                self.simple_type_ref_base_from_infos(&tokens[cursor..lparen_idx])
            {
                self.expr_lowering().collect_named_arguments_from_infos(
                    &tokens[lparen_idx + 1..rparen_idx],
                    scope,
                    NamedArgumentTarget::Constructor { type_name: name },
                );
            } else {
                self.collect_token_expression_refs_infos(
                    &tokens[lparen_idx + 1..rparen_idx],
                    scope,
                    true,
                );
            }
            return rparen_idx + 1;
        }
        lparen_idx + 1
    }

    pub(super) fn named_argument_target_for_callee(
        &self,
        callee: NodeId,
    ) -> Option<NamedArgumentTarget> {
        let (base_namespace, base_name, _, field_path) = self.selector_access_chain(callee)?;
        let method_name = field_path.last()?.name.clone();
        Some(NamedArgumentTarget::Method {
            base_namespace,
            base_name,
            method_name,
        })
    }

    pub(super) fn named_argument_section_from_text(
        &self,
        text: &str,
    ) -> Option<NamedArgumentSection> {
        if text.eq_ignore_ascii_case("exporting") {
            return Some(NamedArgumentSection::Exporting);
        }
        if text.eq_ignore_ascii_case("importing") {
            return Some(NamedArgumentSection::Importing);
        }
        if text.eq_ignore_ascii_case("changing") {
            return Some(NamedArgumentSection::Changing);
        }
        if text.eq_ignore_ascii_case("tables") {
            return Some(NamedArgumentSection::Tables);
        }
        if text.eq_ignore_ascii_case("receiving") {
            return Some(NamedArgumentSection::Receiving);
        }
        if text.eq_ignore_ascii_case("exceptions") {
            return Some(NamedArgumentSection::Exceptions);
        }
        None
    }

    pub(super) fn named_argument_section_allows_inline_target(
        &self,
        section: Option<NamedArgumentSection>,
    ) -> bool {
        matches!(
            section,
            Some(
                NamedArgumentSection::Importing
                    | NamedArgumentSection::Changing
                    | NamedArgumentSection::Receiving
            )
        )
    }

    pub(super) fn call_argument_value_end_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        start_idx: usize,
    ) -> usize {
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut idx = start_idx;
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
                if self.syntax_token_is_ident_like(token)
                    && self
                        .named_argument_section_from_text(token.text.as_ref())
                        .is_some()
                {
                    break;
                }
                if self.syntax_token_is_ident_like(token)
                    && tokens.get(idx + 1).map(|next| next.text.as_ref()) == Some("=")
                {
                    break;
                }
            }
            idx += 1;
        }
        idx
    }

    fn resolve_method_target_class_symbol(
        &self,
        scope: ScopeId,
        base_namespace: Namespace,
        base_name: &Arc<str>,
    ) -> Option<SymbolId> {
        match base_namespace {
            Namespace::Type => self
                .lookup_symbol_in_scope_chain(scope, Namespace::Type, base_name.as_ref())
                .filter(|&symbol_id| self.symbol(symbol_id).kind == SymbolKind::Class),
            Namespace::Value => {
                let symbol_id =
                    self.lookup_symbol_in_scope_chain(scope, Namespace::Value, base_name.as_ref())?;
                let declared_type = self.symbol(symbol_id).declared_type.as_ref()?;
                if !declared_type.is_ref || !declared_type.field_path.is_empty() {
                    return None;
                }
                self.lookup_symbol_in_scope_chain(
                    scope,
                    declared_type.namespace,
                    declared_type.base_name.as_ref(),
                )
                .filter(|&class_symbol_id| self.symbol(class_symbol_id).kind == SymbolKind::Class)
            }
            Namespace::Routine => None,
        }
    }

    fn resolve_named_argument_declared_type(
        &self,
        scope: ScopeId,
        target: &NamedArgumentTarget,
        argument_name: &Arc<str>,
    ) -> Option<FieldTypeRefData> {
        match target {
            NamedArgumentTarget::Constructor { type_name } => {
                let class_symbol =
                    self.lookup_symbol_in_scope_chain(scope, Namespace::Type, type_name.as_ref())?;
                let signature = self.class_method_signature(class_symbol, "constructor", scope)?;
                signature
                    .parameters
                    .iter()
                    .find(|param| param.name == *argument_name)
                    .and_then(|param| param.declared_type.clone())
            }
            NamedArgumentTarget::Function { .. } => None,
            NamedArgumentTarget::Routine { .. } => None,
            NamedArgumentTarget::ImplicitMethod { method_name } => {
                let class_symbol = self.enclosing_class_owner(scope)?;
                let signature =
                    self.class_method_signature(class_symbol, method_name.as_ref(), scope)?;
                signature
                    .parameters
                    .iter()
                    .find(|param| param.name == *argument_name)
                    .and_then(|param| param.declared_type.clone())
            }
            NamedArgumentTarget::Method {
                base_namespace,
                base_name,
                method_name,
            } => {
                let class_symbol =
                    self.resolve_method_target_class_symbol(scope, *base_namespace, base_name)?;
                let signature =
                    self.class_method_signature(class_symbol, method_name.as_ref(), scope)?;
                signature
                    .parameters
                    .iter()
                    .find(|param| param.name == *argument_name)
                    .and_then(|param| param.declared_type.clone())
            }
        }
    }

    pub(super) fn declare_inline_named_argument_target_infos(
        &mut self,
        scope: ScopeId,
        target: &NamedArgumentTarget,
        section: Option<NamedArgumentSection>,
        argument_name: Arc<str>,
        value_tokens: &[SyntaxTokenInfo],
    ) -> bool {
        if !self.named_argument_section_allows_inline_target(section) {
            return false;
        }
        let mut idx = 0usize;
        while value_tokens
            .get(idx)
            .is_some_and(|token| self.syntax_token_is_comment(token))
        {
            idx += 1;
        }
        let Some(token) = value_tokens.get(idx) else {
            return false;
        };
        let declared_type =
            self.resolve_named_argument_declared_type(scope, target, &argument_name);
        let structure = declared_type
            .as_ref()
            .and_then(|type_ref| self.resolve_field_type_ref(scope, type_ref));
        if token.text.eq_ignore_ascii_case("data")
            && value_tokens.get(idx + 1).map(|token| token.text.as_ref()) == Some("(")
            && let Some(name_tok) = value_tokens.get(idx + 2)
            && self.syntax_token_is_ident_like(name_tok)
            && value_tokens.get(idx + 3).map(|token| token.text.as_ref()) == Some(")")
        {
            self.declare_symbol(
                scope,
                Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
                SymbolKind::Variable,
                name_tok.range.clone(),
                structure,
                declared_type,
                None,
            );
            return true;
        }
        if token.text.eq_ignore_ascii_case("field")
            && value_tokens.get(idx + 1).map(|token| token.text.as_ref()) == Some("-")
            && value_tokens
                .get(idx + 2)
                .is_some_and(|token| token.text.eq_ignore_ascii_case("symbol"))
            && value_tokens.get(idx + 3).map(|token| token.text.as_ref()) == Some("(")
            && let Some(name_tok) = value_tokens.get(idx + 4)
            && self.syntax_token_is_ident_like(name_tok)
            && value_tokens.get(idx + 5).map(|token| token.text.as_ref()) == Some(")")
        {
            self.declare_symbol(
                scope,
                Arc::<str>::from(name_tok.text.to_ascii_lowercase()),
                SymbolKind::FieldSymbol,
                name_tok.range.clone(),
                structure,
                declared_type,
                None,
            );
            return true;
        }
        false
    }

    pub(super) fn declare_inline_named_argument_target_from_nodes(
        &mut self,
        scope: ScopeId,
        target: &NamedArgumentTarget,
        section: Option<NamedArgumentSection>,
        argument_name: Arc<str>,
        value_children: &[NodeId],
    ) -> Option<bool> {
        if !self.named_argument_section_allows_inline_target(section) {
            return Some(false);
        }
        let first_value = value_children
            .iter()
            .copied()
            .find(|&node| self.file.kind(node) != SyntaxKind::Token)?;
        let declared_type =
            self.resolve_named_argument_declared_type(scope, target, &argument_name);
        let structure = declared_type
            .as_ref()
            .and_then(|type_ref| self.resolve_field_type_ref(scope, type_ref));
        let Some(name_node) = self
            .syntax(first_value)
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
        else {
            return Some(false);
        };
        let Some(name) = name_node.name(self.source) else {
            return Some(false);
        };
        match self.file.kind(first_value) {
            SyntaxKind::DataInlineDecl => {
                self.declare_symbol(
                    scope,
                    name,
                    SymbolKind::Variable,
                    name_node.range(),
                    structure,
                    declared_type,
                    None,
                );
                Some(true)
            }
            SyntaxKind::FieldSymbolInlineDecl => {
                self.declare_symbol(
                    scope,
                    name,
                    SymbolKind::FieldSymbol,
                    name_node.range(),
                    structure,
                    declared_type,
                    None,
                );
                Some(true)
            }
            _ => Some(false),
        }
    }

    fn token_is_expression_value_ident_info(
        &self,
        tokens: &[SyntaxTokenInfo],
        idx: usize,
        allow_leading_value_ident: bool,
    ) -> bool {
        let token = &tokens[idx];
        if !self.syntax_token_is_ident_like(token)
            || token.text.eq_ignore_ascii_case("new")
            || token.text.eq_ignore_ascii_case("ref")
            || token.text.eq_ignore_ascii_case("to")
        {
            return false;
        }
        if matches!(
            tokens.get(idx + 1).map(|token| token.text.as_ref()),
            Some("=" | "->" | "=>" | "~" | "-")
        ) {
            return false;
        }
        let prev = idx.checked_sub(1).and_then(|prev| tokens.get(prev));
        allow_leading_value_ident && idx == 0
            || matches!(
                prev.map(|token| token.text.as_ref()),
                Some(
                    "=" | ","
                        | "("
                        | "["
                        | "{"
                        | "/"
                        | "+"
                        | "-"
                        | "*"
                        | "&"
                        | "<"
                        | ">"
                        | "<="
                        | ">="
                        | "<>"
                        | "?="
                )
            )
    }

    pub(super) fn find_top_level_keyword_index_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
        keyword: &str,
    ) -> Option<usize> {
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut idx = start;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if paren == 0 && bracket == 0 && brace == 0 && token.text.eq_ignore_ascii_case(keyword)
            {
                return Some(idx);
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
        None
    }

    pub(super) fn consume_concatenate_operand_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
        clause_keywords: &[&str],
    ) -> usize {
        let mut idx = start;
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut consumed_any = false;

        while idx < tokens.len() {
            let token = &tokens[idx];
            if paren == 0 && bracket == 0 && brace == 0 {
                if token.text.as_ref() == "." {
                    break;
                }
                if self.syntax_token_is_ident_like(token)
                    && clause_keywords
                        .iter()
                        .any(|keyword| token.text.eq_ignore_ascii_case(keyword))
                {
                    break;
                }
                if consumed_any && self.token_starts_concatenate_operand_infos(tokens, idx) {
                    break;
                }
            }

            consumed_any = true;
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

    pub(super) fn token_starts_concatenate_operand_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        idx: usize,
    ) -> bool {
        if !self.token_starts_perform_argument_infos(tokens, idx) {
            return false;
        }
        let Some(prev) = idx.checked_sub(1).and_then(|prev_idx| tokens.get(prev_idx)) else {
            return true;
        };
        !(self.syntax_token_is_ident_like(prev)
            && (prev.text.eq_ignore_ascii_case("new")
                || prev.text.eq_ignore_ascii_case("ref")
                || prev.text.eq_ignore_ascii_case("to")))
    }

    pub(super) fn token_starts_perform_argument_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        idx: usize,
    ) -> bool {
        let Some(token) = tokens.get(idx) else {
            return false;
        };
        if !(self.syntax_token_is_ident_like(token)
            || matches!(token.text.as_ref(), "(" | "[" | "{" | "@" | "#")
            || self.syntax_token_is_literal_like(token))
        {
            return false;
        }
        if self.syntax_token_is_ident_like(token)
            && (token.text.eq_ignore_ascii_case("tables")
                || token.text.eq_ignore_ascii_case("using")
                || token.text.eq_ignore_ascii_case("changing"))
        {
            return false;
        }
        let Some(prev) = idx.checked_sub(1).and_then(|prev_idx| tokens.get(prev_idx)) else {
            return true;
        };
        self.syntax_tokens_have_space_between(prev, token)
            && !matches!(
                prev.text.as_ref(),
                "->" | "=>"
                    | "~"
                    | "="
                    | "-"
                    | "+"
                    | "*"
                    | "/"
                    | "<"
                    | ">"
                    | "<="
                    | ">="
                    | "<>"
                    | "?="
                    | "("
                    | "["
                    | "{"
                    | "@"
                    | "#"
                    | "&"
                    | "|"
            )
    }

    fn consume_selector_access_from_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        idx: usize,
    ) -> Option<(
        usize,
        Namespace,
        Arc<str>,
        TextRange,
        Vec<FieldAccessSegment>,
    )> {
        let base = tokens.get(idx)?;
        if !self.syntax_token_is_ident_like(base) {
            return None;
        }
        let mut cursor = idx;
        let mut namespace = None;
        let mut field_path = Vec::with_capacity((tokens.len().saturating_sub(idx + 1)) / 2);
        while cursor + 2 < tokens.len() {
            let op = &tokens[cursor + 1];
            let field = &tokens[cursor + 2];
            if !self.syntax_token_is_ident_like(field)
                && !(op.text.as_ref() == "->" && field.text.as_ref() == "*")
            {
                break;
            }
            let step_namespace = match op.text.as_ref() {
                "=>" => Namespace::Type,
                "->" | "~" => Namespace::Value,
                "-" if !self.syntax_tokens_have_space_between(&tokens[cursor], op)
                    && !self.syntax_tokens_have_space_between(op, field) =>
                {
                    Namespace::Value
                }
                _ => break,
            };
            namespace.get_or_insert(step_namespace);
            field_path.push(FieldAccessSegment {
                name: Self::lower_arc(field.text.as_ref()),
                range: field.range.clone(),
            });
            cursor += 2;
        }
        Some((
            cursor + 1,
            namespace?,
            Self::lower_arc(base.text.as_ref()),
            base.range.clone(),
            field_path,
        ))
    }

    pub(super) fn find_matching_group_end_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        start_idx: usize,
        open_text: &str,
        close_text: &str,
    ) -> Option<usize> {
        let mut depth = 0i32;
        for (idx, token) in tokens.iter().enumerate().skip(start_idx) {
            if token.text.as_ref() == open_text {
                depth += 1;
            } else if token.text.as_ref() == close_text {
                depth -= 1;
                if depth == 0 {
                    return Some(idx);
                }
            }
        }
        None
    }
}
