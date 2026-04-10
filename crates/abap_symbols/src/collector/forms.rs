use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_ast::ast::AstNode;

use crate::def_map::{
    FormParameterData, FormParameterPassingKind, FormParameterSection, PerformArgumentData,
    PerformCallData, PerformParameterSection, ReferenceKind, SymbolKind,
};
use crate::ids::{ScopeId, SymbolId};
use crate::scope::Namespace;

use super::emit::FormSink;
use super::{Collector, SyntaxTokenInfo};

#[derive(Clone, Copy)]
enum FormHeaderParamSection {
    Tables,
    Using,
    Changing,
}

impl FormHeaderParamSection {
    fn as_form_parameter_section(self) -> FormParameterSection {
        match self {
            Self::Tables => FormParameterSection::Tables,
            Self::Using => FormParameterSection::Using,
            Self::Changing => FormParameterSection::Changing,
        }
    }
}

struct FormConsumedParameter {
    next_idx: usize,
    symbol: SymbolId,
    passing: FormParameterPassingKind,
}

pub(super) struct FormsLowering<'ctx, 'a> {
    collector: &'ctx mut Collector<'a>,
}

impl<'a> Collector<'a> {
    pub(super) fn forms_lowering(&mut self) -> FormsLowering<'_, 'a> {
        FormsLowering { collector: self }
    }
}

impl<'ctx, 'a> FormsLowering<'ctx, 'a> {
    pub(super) fn declare_form_parameters_from_header(
        &mut self,
        form_node: NodeId,
        form_scope: ScopeId,
    ) -> Vec<FormParameterData> {
        let tokens = self.form_header_tokens(form_node);
        let type_ref_nodes = self.collector.direct_type_ref_children(form_node);
        let mut type_ref_idx = 0usize;
        if tokens.len() < 2 {
            return Vec::new();
        }
        if !tokens[0].text.eq_ignore_ascii_case("form") {
            return Vec::new();
        }
        let mut i = 1usize;
        while i < tokens.len() && self.collector.syntax_token_is_comment(&tokens[i]) {
            i += 1;
        }
        if !tokens
            .get(i)
            .is_some_and(|t| self.collector.syntax_token_is_ident_like(t))
        {
            return Vec::new();
        }
        i += 1;

        let mut section: Option<FormHeaderParamSection> = None;
        let mut depth = 0i32;
        let mut parameters = Vec::new();

        while i < tokens.len() {
            let t = &tokens[i];
            if self.collector.syntax_token_is_comment(t) {
                i += 1;
                continue;
            }
            match t.text.as_ref() {
                "(" => {
                    depth += 1;
                    i += 1;
                }
                ")" => {
                    depth -= 1;
                    i += 1;
                }
                "." if depth == 0 => break,
                lit if depth == 0 && self.collector.syntax_token_is_ident_like(t) => {
                    if lit.eq_ignore_ascii_case("tables") {
                        section = Some(FormHeaderParamSection::Tables);
                        i += 1;
                        continue;
                    }
                    if lit.eq_ignore_ascii_case("using") {
                        section = Some(FormHeaderParamSection::Using);
                        i += 1;
                        continue;
                    }
                    if lit.eq_ignore_ascii_case("changing") {
                        section = Some(FormHeaderParamSection::Changing);
                        i += 1;
                        continue;
                    }
                    if lit.eq_ignore_ascii_case("raises") {
                        section = None;
                        i += 1;
                        continue;
                    }

                    match section {
                        Some(FormHeaderParamSection::Using)
                        | Some(FormHeaderParamSection::Changing) => {
                            if let Some(consumed) = self.try_consume_form_value_or_reference_param(
                                &tokens,
                                i,
                                form_scope,
                                type_ref_nodes.get(type_ref_idx).copied(),
                            ) {
                                if self
                                    .collector
                                    .symbol(consumed.symbol)
                                    .declared_type
                                    .is_some()
                                {
                                    type_ref_idx += 1;
                                }
                                let current_section = section.expect("parameter section");
                                parameters.push(FormParameterData {
                                    symbol: consumed.symbol,
                                    section: current_section.as_form_parameter_section(),
                                    passing: consumed.passing,
                                });
                                i = consumed.next_idx;
                                continue;
                            }
                            if self.form_header_starts_typed_param(&tokens, i) {
                                let range = t.range.clone();
                                let name = Arc::<str>::from(lit.to_ascii_lowercase());
                                let mut j = i + 1;
                                while j < tokens.len()
                                    && self.collector.syntax_token_is_comment(&tokens[j])
                                {
                                    j += 1;
                                }
                                let declared_type = match tokens.get(j) {
                                    Some(tok) if tok.text.eq_ignore_ascii_case("type") => {
                                        j += 1;
                                        while j < tokens.len()
                                            && self.collector.syntax_token_is_comment(&tokens[j])
                                        {
                                            j += 1;
                                        }
                                        let expr_end =
                                            self.skip_form_header_type_expression(&tokens, j);
                                        let dt = type_ref_nodes
                                            .get(type_ref_idx)
                                            .copied()
                                            .and_then(|node| {
                                                self.collector
                                                    .field_type_ref_from_node(node, Namespace::Type)
                                            });
                                        if dt.is_some() {
                                            type_ref_idx += 1;
                                        }
                                        j = expr_end;
                                        dt
                                    }
                                    Some(tok) if tok.text.eq_ignore_ascii_case("like") => {
                                        j += 1;
                                        while j < tokens.len()
                                            && self.collector.syntax_token_is_comment(&tokens[j])
                                        {
                                            j += 1;
                                        }
                                        let expr_end =
                                            self.skip_form_header_type_expression(&tokens, j);
                                        let dt = type_ref_nodes
                                            .get(type_ref_idx)
                                            .copied()
                                            .and_then(|node| {
                                                self.collector.field_type_ref_from_node(
                                                    node,
                                                    Namespace::Value,
                                                )
                                            });
                                        if dt.is_some() {
                                            type_ref_idx += 1;
                                        }
                                        j = expr_end;
                                        dt
                                    }
                                    _ => None,
                                };
                                let symbol = self.collector.declare_symbol(
                                    form_scope,
                                    name,
                                    SymbolKind::Parameter,
                                    range,
                                    None,
                                    declared_type,
                                    type_ref_nodes
                                        .get(type_ref_idx.saturating_sub(1))
                                        .copied()
                                        .and_then(|node| {
                                            abap_ast::ast::TypeRefSimple::cast(
                                                self.collector.syntax(node),
                                            )
                                        })
                                        .and_then(|type_ref| {
                                            type_ref.display_text(self.collector.source)
                                        })
                                        .map(Arc::from),
                                    None,
                                );
                                parameters.push(FormParameterData {
                                    symbol,
                                    section: section
                                        .expect("parameter section")
                                        .as_form_parameter_section(),
                                    passing: FormParameterPassingKind::Direct,
                                });
                                i = j;
                                continue;
                            }
                            i += 1;
                        }
                        Some(FormHeaderParamSection::Tables) => {
                            if self.collector.syntax_token_is_ident_like(t) {
                                let symbol = self.collector.declare_symbol(
                                    form_scope,
                                    Arc::<str>::from(lit.to_ascii_lowercase()),
                                    SymbolKind::Parameter,
                                    t.range.clone(),
                                    None,
                                    None,
                                    None,
                                    None,
                                );
                                parameters.push(FormParameterData {
                                    symbol,
                                    section: FormParameterSection::Tables,
                                    passing: FormParameterPassingKind::Direct,
                                });
                            }
                            i += 1;
                        }
                        None => {
                            i += 1;
                        }
                    }
                }
                _ => {
                    i += 1;
                }
            }
        }
        parameters
    }

    pub(super) fn collect_perform_stmt_node(&mut self, node: NodeId, scope: ScopeId) {
        let significant = self.collector.significant_stmt_token_infos(node);
        self.collect_perform_stmt_infos(&significant, scope);
    }

    fn form_header_tokens(&self, form_node: NodeId) -> Vec<SyntaxTokenInfo> {
        let mut out = Vec::new();
        for child in self.collector.file.children(form_node) {
            match self.collector.file.kind(child) {
                SyntaxKind::Token => {
                    let tokens = self.collector.syntax_token_nodes(child);
                    if let Some(token) = tokens.first().cloned() {
                        let is_period = token.text.as_ref() == ".";
                        out.push(token);
                        if is_period {
                            break;
                        }
                    }
                }
                SyntaxKind::TypeRefSimple => out.extend(self.collector.syntax_token_nodes(child)),
                _ => break,
            }
        }
        out
    }

    fn form_header_section_keyword(&self, token: &SyntaxTokenInfo) -> bool {
        matches!(
            token.text.to_ascii_uppercase().as_str(),
            "TABLES" | "USING" | "CHANGING" | "RAISES"
        )
    }

    fn form_header_starts_typed_param(&self, tokens: &[SyntaxTokenInfo], idx: usize) -> bool {
        let name = match tokens.get(idx) {
            Some(t) if self.collector.syntax_token_is_ident_like(t) => t,
            _ => return false,
        };
        if name.text.eq_ignore_ascii_case("value") || name.text.eq_ignore_ascii_case("reference") {
            return false;
        }
        let mut j = idx + 1;
        while j < tokens.len() && self.collector.syntax_token_is_comment(&tokens[j]) {
            j += 1;
        }
        tokens.get(j).is_some_and(|tok| {
            tok.text.eq_ignore_ascii_case("type") || tok.text.eq_ignore_ascii_case("like")
        })
    }

    fn try_consume_form_value_or_reference_param(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        i: usize,
        scope: ScopeId,
        type_ref_node: Option<NodeId>,
    ) -> Option<FormConsumedParameter> {
        let kw = tokens.get(i)?;
        let passing = if kw.text.eq_ignore_ascii_case("value") {
            FormParameterPassingKind::Value
        } else if kw.text.eq_ignore_ascii_case("reference") {
            FormParameterPassingKind::Reference
        } else {
            return None;
        };
        let mut j = i + 1;
        while j < tokens.len() && self.collector.syntax_token_is_comment(&tokens[j]) {
            j += 1;
        }
        let (name, range) = if tokens.get(j).map(|t| t.text.as_ref()) == Some("(") {
            j += 1;
            while j < tokens.len() && self.collector.syntax_token_is_comment(&tokens[j]) {
                j += 1;
            }
            let inner = tokens.get(j)?;
            if !self.collector.syntax_token_is_ident_like(inner) {
                return None;
            }
            let name = Arc::<str>::from(inner.text.to_ascii_lowercase());
            let range = inner.range.clone();
            j += 1;
            while j < tokens.len() && self.collector.syntax_token_is_comment(&tokens[j]) {
                j += 1;
            }
            if tokens.get(j).map(|t| t.text.as_ref()) != Some(")") {
                return None;
            }
            j += 1;
            (name, range)
        } else {
            let inner = tokens.get(j)?;
            if !self.collector.syntax_token_is_ident_like(inner) {
                return None;
            }
            let name = Arc::<str>::from(inner.text.to_ascii_lowercase());
            let range = inner.range.clone();
            j += 1;
            (name, range)
        };
        while j < tokens.len() && self.collector.syntax_token_is_comment(&tokens[j]) {
            j += 1;
        }
        let type_tok = tokens.get(j)?;
        let clause_ns = if type_tok.text.eq_ignore_ascii_case("type") {
            Namespace::Type
        } else if type_tok.text.eq_ignore_ascii_case("like") {
            Namespace::Value
        } else {
            return None;
        };
        j += 1;
        while j < tokens.len() && self.collector.syntax_token_is_comment(&tokens[j]) {
            j += 1;
        }
        let expr_start = j;
        let expr_end = self.skip_form_header_type_expression(tokens, expr_start);
        let declared_type =
            type_ref_node.and_then(|node| self.collector.field_type_ref_from_node(node, clause_ns));
        let symbol = self.collector.declare_symbol(
            scope,
            name,
            SymbolKind::Parameter,
            range,
            None,
            declared_type,
            type_ref_node
                .and_then(|node| abap_ast::ast::TypeRefSimple::cast(self.collector.syntax(node)))
                .and_then(|type_ref| type_ref.display_text(self.collector.source))
                .map(Arc::from),
            None,
        );
        Some(FormConsumedParameter {
            next_idx: expr_end,
            symbol,
            passing,
        })
    }

    fn skip_form_header_type_expression(&self, tokens: &[SyntaxTokenInfo], mut i: usize) -> usize {
        let mut depth = 0i32;
        while i < tokens.len() {
            let t = &tokens[i];
            if self.collector.syntax_token_is_comment(t) {
                i += 1;
                continue;
            }
            match t.text.as_ref() {
                "(" => {
                    depth += 1;
                    i += 1;
                }
                ")" => {
                    if depth == 0 {
                        return i;
                    }
                    depth -= 1;
                    i += 1;
                }
                "." if depth == 0 => return i,
                _ if depth == 0 && self.form_header_section_keyword(t) => return i,
                _ if depth == 0 && self.form_header_starts_typed_param(tokens, i) => return i,
                _ => i += 1,
            }
        }
        i
    }

    fn collect_perform_stmt_infos(&mut self, tokens: &[SyntaxTokenInfo], scope: ScopeId) {
        if tokens.len() < 2 || !tokens[0].text.eq_ignore_ascii_case("perform") {
            return;
        }
        let routine = &tokens[1];
        if !self.collector.syntax_token_is_ident_like(routine) {
            return;
        }

        let routine_name = Arc::<str>::from(routine.text.to_ascii_lowercase());
        self.collector.add_reference(
            scope,
            Arc::clone(&routine_name),
            Namespace::Routine,
            ReferenceKind::RoutineCall,
            routine.range.clone(),
        );

        let mut parameters = Vec::new();
        let mut arguments = Vec::new();
        let mut section: Option<PerformParameterSection> = None;
        let mut highest_section_rank = 0u8;
        let mut section_order_invalid = false;
        let mut tables_ordinal = 0usize;
        let mut using_ordinal = 0usize;
        let mut changing_ordinal = 0usize;
        let mut idx = 2usize;

        while idx < tokens.len() {
            let token = &tokens[idx];
            if token.text.as_ref() == "." {
                break;
            }

            if self.collector.syntax_token_is_ident_like(token) {
                let next_section = if token.text.eq_ignore_ascii_case("tables") {
                    Some((PerformParameterSection::Tables, 1))
                } else if token.text.eq_ignore_ascii_case("using") {
                    Some((PerformParameterSection::Using, 2))
                } else if token.text.eq_ignore_ascii_case("changing") {
                    Some((PerformParameterSection::Changing, 3))
                } else {
                    None
                };
                if let Some((next_section, rank)) = next_section {
                    if rank <= highest_section_rank {
                        section_order_invalid = true;
                    } else {
                        highest_section_rank = rank;
                    }
                    section = Some(next_section);
                    idx += 1;
                    continue;
                }
            }

            let Some(current_section) = section else {
                idx += 1;
                continue;
            };
            let next_idx = self.consume_perform_argument_infos(tokens, idx);
            if next_idx == idx {
                idx += 1;
                continue;
            }
            parameters.push(current_section);
            let ordinal_in_section = match current_section {
                PerformParameterSection::Tables => {
                    let current = tables_ordinal;
                    tables_ordinal += 1;
                    current
                }
                PerformParameterSection::Using => {
                    let current = using_ordinal;
                    using_ordinal += 1;
                    current
                }
                PerformParameterSection::Changing => {
                    let current = changing_ordinal;
                    changing_ordinal += 1;
                    current
                }
            };
            if let Some(last_token) = tokens.get(next_idx.saturating_sub(1)) {
                arguments.push(PerformArgumentData {
                    range: tokens[idx].range.start..last_token.range.end,
                    section: current_section,
                    ordinal_in_section,
                });
            }
            idx = next_idx;
        }

        let end = tokens
            .last()
            .map(|token| token.range.end)
            .unwrap_or(routine.range.end);
        self.collector.emit_perform_call(PerformCallData {
            scope,
            range: tokens[0].range.start..end,
            routine_name,
            routine_range: routine.range.clone(),
            parameters,
            arguments,
            section_order_invalid,
        });
    }

    fn consume_perform_argument_infos(&self, tokens: &[SyntaxTokenInfo], start: usize) -> usize {
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
                if self.collector.syntax_token_is_ident_like(token)
                    && (token.text.eq_ignore_ascii_case("tables")
                        || token.text.eq_ignore_ascii_case("using")
                        || token.text.eq_ignore_ascii_case("changing"))
                {
                    break;
                }
                if consumed_any
                    && self
                        .collector
                        .token_starts_perform_argument_infos(tokens, idx)
                {
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
}
