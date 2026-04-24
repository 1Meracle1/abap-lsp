use std::sync::Arc;

use abap_ast::arena::NodeId;
use abap_ast::ast::{
    AstNode, FormDecl, FormParamPassingKind as AstFormParamPassingKind,
    FormParamSectionKind as AstFormParamSectionKind, FunctionDecl,
    FunctionParamSectionKind as AstFunctionParamSectionKind, PerformStmt, TypeClauseKind,
};

use crate::def_map::{
    FormParameterData, FormParameterPassingKind, FormParameterSection, FunctionModuleData,
    FunctionModuleExceptionData, FunctionModuleParameterData, FunctionModuleParameterSection,
    PerformArgumentData, PerformCallData, PerformParameterSection, PerformProgramData,
    ReferenceKind, SymbolKind,
};
use crate::ids::ScopeId;
use crate::scope::Namespace;

use super::emit::FormSink;
use super::{Collector, SyntaxTokenInfo};

pub(super) struct FormsLowering<'ctx, 'a> {
    collector: &'ctx mut Collector<'a>,
}

#[derive(Debug, Clone)]
struct PerformTargetInfo {
    routine_name: Arc<str>,
    routine_range: abap_lexer::TextRange,
    is_dynamic: bool,
    program: Option<PerformProgramData>,
    has_if_found: bool,
    arguments_start_idx: usize,
}

impl<'a> Collector<'a> {
    pub(super) fn forms_lowering(&mut self) -> FormsLowering<'_, 'a> {
        FormsLowering { collector: self }
    }
}

impl<'ctx, 'a> FormsLowering<'ctx, 'a> {
    fn lowered_source_range(&self, range: &abap_lexer::TextRange) -> Option<Arc<str>> {
        self.collector
            .source
            .get(range.clone())
            .map(|text| Arc::<str>::from(text.trim().to_ascii_lowercase()))
    }

    fn token_span(
        tokens: &[SyntaxTokenInfo],
        start: usize,
        end_exclusive: usize,
    ) -> Option<abap_lexer::TextRange> {
        let first = tokens.get(start)?;
        let last = tokens.get(end_exclusive.checked_sub(1)?)?;
        Some(first.range.start..last.range.end)
    }

    fn tokens_match_keyword_sequence(
        tokens: &[SyntaxTokenInfo],
        start: usize,
        keywords: &[&str],
    ) -> bool {
        keywords.iter().enumerate().all(|(offset, keyword)| {
            tokens
                .get(start + offset)
                .is_some_and(|token| token.text.eq_ignore_ascii_case(keyword))
        })
    }

    fn token_is_perform_parameter_section(token: &SyntaxTokenInfo) -> bool {
        token.text.eq_ignore_ascii_case("tables")
            || token.text.eq_ignore_ascii_case("using")
            || token.text.eq_ignore_ascii_case("changing")
    }

    fn consume_static_perform_program_operand(
        &self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
    ) -> Option<(PerformProgramData, usize)> {
        let first = tokens.get(start)?;
        if first.text.as_ref() == "."
            || Self::token_is_perform_parameter_section(first)
            || Self::tokens_match_keyword_sequence(tokens, start, &["if", "found"])
        {
            return None;
        }
        if !(self.collector.syntax_token_is_ident_like(first) || first.text.as_ref() == "/") {
            return None;
        }

        let mut idx = start;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if token.text.as_ref() == "."
                || Self::token_is_perform_parameter_section(token)
                || Self::tokens_match_keyword_sequence(tokens, idx, &["if", "found"])
            {
                break;
            }
            idx += 1;
        }
        if idx == start {
            return None;
        }

        let range = Self::token_span(tokens, start, idx)?;
        let name = self.lowered_source_range(&range)?;
        Some((
            PerformProgramData {
                name,
                range,
                is_dynamic: false,
            },
            idx,
        ))
    }

    fn function_table_parameter_type_display(
        section: FunctionModuleParameterSection,
        type_clause_display: Option<Arc<str>>,
    ) -> Option<Arc<str>> {
        if section != FunctionModuleParameterSection::Tables {
            return type_clause_display;
        }
        let display = type_clause_display?;
        if display.as_ref().to_ascii_uppercase().contains(" TABLE OF ") {
            return Some(display);
        }
        Some(Arc::from(format!("STANDARD TABLE OF {display}")))
    }

    fn first_non_comment_range(tokens: &[SyntaxTokenInfo]) -> Option<abap_lexer::TextRange> {
        tokens
            .iter()
            .find(|token| token.kind != abap_lexer::TokenKind::Comment)
            .map(|token| token.range.clone())
    }

    fn split_chained_perform_calls_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
    ) -> Option<Vec<(Vec<SyntaxTokenInfo>, abap_lexer::TextRange)>> {
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut colon_idx = None;
        let mut period_idx = None;

        for (idx, token) in tokens.iter().enumerate() {
            if paren == 0 && bracket == 0 && brace == 0 {
                if token.text.as_ref() == ":" {
                    colon_idx = Some(idx);
                    continue;
                }
                if token.text.as_ref() == "." {
                    period_idx = Some(idx);
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
        }

        let colon_idx = colon_idx?;
        let period_idx = period_idx?;
        let prefix = &tokens[..colon_idx];
        let period = tokens[period_idx].clone();

        let mut items = Vec::new();
        let mut item_start = colon_idx + 1;
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut idx = item_start;
        while idx <= period_idx {
            let token = &tokens[idx];
            let is_separator = paren == 0
                && bracket == 0
                && brace == 0
                && (token.text.as_ref() == "," || token.text.as_ref() == ".");
            if is_separator {
                let item_tokens = &tokens[item_start..idx];
                if let Some(item_range) = Self::first_non_comment_range(item_tokens).map(|start| {
                    let end = item_tokens
                        .iter()
                        .rev()
                        .find(|token| token.kind != abap_lexer::TokenKind::Comment)
                        .map(|token| token.range.end)
                        .unwrap_or(start.end);
                    start.start..end
                }) {
                    let mut expanded =
                        Vec::with_capacity(prefix.len() + item_tokens.len() + usize::from(true));
                    expanded.extend_from_slice(prefix);
                    expanded.extend_from_slice(item_tokens);
                    expanded.push(period.clone());
                    items.push((expanded, item_range));
                }
                item_start = idx + 1;
                idx += 1;
                continue;
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

        (!items.is_empty()).then_some(items)
    }

    fn function_param_has_adt_untyped_pragma(
        &self,
        param: abap_ast::ast::FunctionParam<'_>,
    ) -> bool {
        let end = param.syntax().range().end;
        self.collector
            .source
            .get(end..)
            .and_then(|tail| tail.lines().next())
            .is_some_and(|line| line.contains("##ADT_PARAMETER_UNTYPED"))
    }

    fn collect_dynamic_perform_operand_refs(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
        end_exclusive: usize,
        scope: ScopeId,
    ) {
        if start < end_exclusive {
            self.collector.collect_token_expression_refs_infos(
                &tokens[start..end_exclusive],
                scope,
                true,
            );
        }
    }

    fn collect_perform_target_info(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) -> Option<PerformTargetInfo> {
        if tokens.len() < 2 || !tokens[0].text.eq_ignore_ascii_case("perform") {
            return None;
        }

        let mut idx = 1usize;
        let (routine_name, routine_range, is_dynamic) = if tokens
            .get(idx)
            .is_some_and(|token| token.text.as_ref() == "(")
        {
            let end_idx = self
                .collector
                .find_matching_group_end_infos(tokens, idx, "(", ")")?;
            self.collect_dynamic_perform_operand_refs(tokens, idx + 1, end_idx, scope);
            let routine_range = Self::token_span(tokens, idx + 1, end_idx)
                .unwrap_or_else(|| tokens[idx].range.clone());
            let routine_name = self
                .lowered_source_range(&routine_range)
                .unwrap_or_else(|| Arc::<str>::from("<dynamic>"));
            idx = end_idx + 1;
            (routine_name, routine_range, true)
        } else {
            let token = tokens.get(idx)?;
            if !self.collector.syntax_token_is_ident_like(token) {
                return None;
            }
            idx += 1;
            (
                Arc::<str>::from(token.text.to_ascii_lowercase()),
                token.range.clone(),
                false,
            )
        };

        let mut program = None;
        let mut has_if_found = false;
        if Self::tokens_match_keyword_sequence(tokens, idx, &["in", "program"]) {
            idx += 2;
            if tokens
                .get(idx)
                .is_some_and(|token| token.text.as_ref() == "(")
            {
                if let Some(end_idx) = self
                    .collector
                    .find_matching_group_end_infos(tokens, idx, "(", ")")
                {
                    self.collect_dynamic_perform_operand_refs(tokens, idx + 1, end_idx, scope);
                    let program_range = Self::token_span(tokens, idx + 1, end_idx)
                        .unwrap_or_else(|| tokens[idx].range.clone());
                    let program_name = self
                        .lowered_source_range(&program_range)
                        .unwrap_or_else(|| Arc::<str>::from("<dynamic>"));
                    program = Some(PerformProgramData {
                        name: program_name,
                        range: program_range,
                        is_dynamic: true,
                    });
                    idx = end_idx + 1;
                } else {
                    idx += 1;
                }
            } else if let Some((target_program, next_idx)) =
                self.consume_static_perform_program_operand(tokens, idx)
            {
                program = Some(target_program);
                idx = next_idx;
            }
        }

        if Self::tokens_match_keyword_sequence(tokens, idx, &["if", "found"]) {
            has_if_found = true;
            idx += 2;
        }

        Some(PerformTargetInfo {
            routine_name,
            routine_range,
            is_dynamic,
            program,
            has_if_found,
            arguments_start_idx: idx,
        })
    }

    pub(super) fn declare_form_parameters_from_header(
        &mut self,
        form_node: NodeId,
        form_scope: ScopeId,
    ) -> Vec<FormParameterData> {
        let Some(form_decl) = FormDecl::cast(self.collector.syntax(form_node)) else {
            return Vec::new();
        };
        if form_decl.name_token().is_none() {
            return Vec::new();
        }
        let mut param_infos = Vec::new();
        for section in form_decl.param_sections() {
            let section_kind = match section.kind(self.collector.source) {
                Some(AstFormParamSectionKind::Tables) => FormParameterSection::Tables,
                Some(AstFormParamSectionKind::Using) => FormParameterSection::Using,
                Some(AstFormParamSectionKind::Changing) => FormParameterSection::Changing,
                None => continue,
            };
            for param in section.params() {
                let Some(name_node) = param.name_token() else {
                    continue;
                };
                let Some(name) = name_node.name(self.collector.source) else {
                    continue;
                };
                let declared_type = match param.type_clause_kind(self.collector.source) {
                    Some(TypeClauseKind::Type) => param.type_ref().and_then(|type_ref| {
                        self.collector
                            .field_type_ref_from_node(type_ref.syntax().id(), Namespace::Type)
                    }),
                    Some(TypeClauseKind::Like) => param.type_ref().and_then(|type_ref| {
                        self.collector
                            .field_type_ref_from_node(type_ref.syntax().id(), Namespace::Value)
                    }),
                    Some(TypeClauseKind::For) => None,
                    None if section_kind == FormParameterSection::Tables => {
                        param.type_ref().and_then(|type_ref| {
                            self.collector
                                .field_type_ref_from_node(type_ref.syntax().id(), Namespace::Type)
                        })
                    }
                    None => None,
                };
                let type_clause_display = param
                    .type_ref()
                    .and_then(|type_ref| type_ref.display_text(self.collector.source))
                    .map(Arc::from);
                let passing = match param.passing_kind(self.collector.source) {
                    AstFormParamPassingKind::Direct => FormParameterPassingKind::Direct,
                    AstFormParamPassingKind::Value => FormParameterPassingKind::Value,
                    AstFormParamPassingKind::Reference => FormParameterPassingKind::Reference,
                };
                param_infos.push((
                    name,
                    name_node.range(),
                    section_kind,
                    passing,
                    declared_type,
                    type_clause_display,
                ));
            }
        }

        let mut parameters = Vec::new();
        for (name, range, section, passing, declared_type, type_clause_display) in param_infos {
            let symbol = self.collector.declare_symbol(
                form_scope,
                name,
                SymbolKind::Parameter,
                range,
                None,
                declared_type,
                type_clause_display,
                None,
            );
            parameters.push(FormParameterData {
                symbol,
                section,
                passing,
            });
        }
        parameters
    }

    pub(super) fn collect_perform_stmt_node(&mut self, node: NodeId, scope: ScopeId) {
        let Some(_stmt) = PerformStmt::cast(self.collector.syntax(node)) else {
            return;
        };
        let tokens = self.collector.significant_stmt_token_infos(node);
        let Some(target) = self.collect_perform_target_info(&tokens, scope) else {
            return;
        };
        self.collect_perform_stmt_infos(&tokens, scope, target, self.collector.file.range(node));
    }

    fn collect_perform_stmt_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
        target: PerformTargetInfo,
        stmt_range: abap_lexer::TextRange,
    ) {
        if tokens.is_empty() || !tokens[0].text.eq_ignore_ascii_case("perform") {
            return;
        }
        if !target.is_dynamic && target.program.is_none() {
            self.collector.add_reference(
                scope,
                Arc::clone(&target.routine_name),
                Namespace::Routine,
                ReferenceKind::RoutineCall,
                target.routine_range.clone(),
            );
        }

        if let Some(calls) = self.split_chained_perform_calls_infos(tokens) {
            for (call_tokens, call_range) in calls {
                self.collect_single_perform_stmt_infos(
                    &call_tokens,
                    scope,
                    target.clone(),
                    call_range,
                );
            }
            return;
        }

        self.collect_single_perform_stmt_infos(tokens, scope, target, stmt_range);
    }

    fn collect_single_perform_stmt_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
        target: PerformTargetInfo,
        stmt_range: abap_lexer::TextRange,
    ) {
        if tokens.is_empty() || !tokens[0].text.eq_ignore_ascii_case("perform") {
            return;
        }

        let mut parameters = Vec::new();
        let mut arguments = Vec::new();
        let mut section: Option<PerformParameterSection> = None;
        let mut highest_section_rank = 0u8;
        let mut section_order_invalid = false;
        let mut tables_ordinal = 0usize;
        let mut using_ordinal = 0usize;
        let mut changing_ordinal = 0usize;
        let mut idx = target.arguments_start_idx;

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
            self.collector
                .collect_token_expression_refs_infos(&tokens[idx..next_idx], scope, true);
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

        self.collector.emit_perform_call(PerformCallData {
            scope,
            range: stmt_range,
            routine_name: target.routine_name,
            routine_range: target.routine_range,
            is_dynamic: target.is_dynamic,
            program: target.program,
            has_if_found: target.has_if_found,
            parameters,
            arguments,
            section_order_invalid,
        });
    }

    pub(super) fn declare_function_parameters_from_header(
        &mut self,
        function_node: NodeId,
        function_scope: ScopeId,
        function_symbol: crate::ids::SymbolId,
    ) -> FunctionModuleData {
        let Some(function_decl) = FunctionDecl::cast(self.collector.syntax(function_node)) else {
            return FunctionModuleData {
                symbol: function_symbol,
                parameters: Vec::new(),
                exceptions: Vec::new(),
            };
        };
        if function_decl.name_token().is_none() {
            return FunctionModuleData {
                symbol: function_symbol,
                parameters: Vec::new(),
                exceptions: Vec::new(),
            };
        }

        let mut parameter_infos = Vec::new();
        let mut exceptions = Vec::new();
        for section in function_decl.param_sections() {
            match section.kind(self.collector.source) {
                Some(AstFunctionParamSectionKind::Importing)
                | Some(AstFunctionParamSectionKind::Exporting)
                | Some(AstFunctionParamSectionKind::Changing)
                | Some(AstFunctionParamSectionKind::Tables) => {
                    let section_kind = match section.kind(self.collector.source) {
                        Some(AstFunctionParamSectionKind::Importing) => {
                            FunctionModuleParameterSection::Importing
                        }
                        Some(AstFunctionParamSectionKind::Exporting) => {
                            FunctionModuleParameterSection::Exporting
                        }
                        Some(AstFunctionParamSectionKind::Changing) => {
                            FunctionModuleParameterSection::Changing
                        }
                        Some(AstFunctionParamSectionKind::Tables) => {
                            FunctionModuleParameterSection::Tables
                        }
                        _ => continue,
                    };
                    for param in section.params() {
                        let Some(name_node) = param.name_token() else {
                            continue;
                        };
                        let Some(name) = name_node.name(self.collector.source) else {
                            continue;
                        };
                        let declared_type = match param.type_clause_kind(self.collector.source) {
                            Some(TypeClauseKind::Type) => param.type_ref().and_then(|type_ref| {
                                self.collector.field_type_ref_from_node(
                                    type_ref.syntax().id(),
                                    Namespace::Type,
                                )
                            }),
                            Some(TypeClauseKind::Like) => param.type_ref().and_then(|type_ref| {
                                self.collector.field_type_ref_from_node(
                                    type_ref.syntax().id(),
                                    Namespace::Value,
                                )
                            }),
                            Some(TypeClauseKind::For) => None,
                            None if section_kind == FunctionModuleParameterSection::Tables => {
                                param.type_ref().and_then(|type_ref| {
                                    self.collector.field_type_ref_from_node(
                                        type_ref.syntax().id(),
                                        Namespace::Type,
                                    )
                                })
                            }
                            None => None,
                        };
                        let type_clause_display = Self::function_table_parameter_type_display(
                            section_kind,
                            param
                                .type_ref()
                                .and_then(|type_ref| type_ref.display_text(self.collector.source))
                                .map(Arc::from),
                        );
                        parameter_infos.push(FunctionModuleParameterData {
                            section: section_kind,
                            name,
                            range: name_node.range(),
                            declared_type,
                            type_clause_display,
                            is_untyped: self.function_param_has_adt_untyped_pragma(param),
                            is_optional: param.is_optional(self.collector.source),
                            has_default_value: param.has_default_value(self.collector.source),
                        });
                    }
                }
                Some(AstFunctionParamSectionKind::Exceptions) => {
                    for param in section.params() {
                        let Some(name_node) = param.name_token() else {
                            continue;
                        };
                        let Some(name) = name_node.name(self.collector.source) else {
                            continue;
                        };
                        exceptions.push(FunctionModuleExceptionData {
                            name,
                            range: name_node.range(),
                        });
                    }
                }
                None => {}
            }
        }

        for parameter in &parameter_infos {
            self.collector.declare_symbol(
                function_scope,
                Arc::clone(&parameter.name),
                SymbolKind::Parameter,
                parameter.range.clone(),
                None,
                parameter.declared_type.clone(),
                parameter.type_clause_display.clone(),
                None,
            );
        }

        FunctionModuleData {
            symbol: function_symbol,
            parameters: parameter_infos,
            exceptions,
        }
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
