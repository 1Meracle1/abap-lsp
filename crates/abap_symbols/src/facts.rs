use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::compatibility::positional_parameter_section;
use crate::def_map::{
    ExpressionFactData, ExpressionFactKind, FieldAccess, FieldAccessSegment, FieldTypeRefData,
    MethodParameterSection, NamedArgumentTarget, Resolution, TypeFactData, UnitAnalysis,
    ValueFlowEdgeData, ValueFlowKind, ValueFlowTargetData,
};
use crate::ids::{ScopeId, SymbolHandle, SymbolId};
use crate::resolver::{ScopeIndex, build_scope_index};
use crate::scope::{Namespace, ScopeKind};

#[derive(Debug, Clone, Default)]
struct InferredUnitFacts {
    expression_facts: Vec<ExpressionFactData>,
    value_flow_edges: Vec<ValueFlowEdgeData>,
}

#[derive(Debug, Clone)]
struct CallParameterInfo {
    name: Option<Arc<str>>,
    decl_unit: Option<crate::ids::UnitId>,
    decl_range: Option<abap_lexer::TextRange>,
    type_fact: TypeFactData,
    positional: bool,
}

pub(crate) fn infer_semantic_facts(units: &mut [UnitAnalysis]) {
    let scope_indexes: Vec<_> = units.iter().map(build_scope_index).collect();
    let builder = FactBuilder::new(units, scope_indexes);
    let inferred: Vec<_> = (0..units.len())
        .map(|unit_idx| builder.infer_unit(unit_idx))
        .collect();

    for (unit, facts) in units.iter_mut().zip(inferred) {
        unit.expression_facts = facts.expression_facts;
        let mut value_flow_edges = facts.value_flow_edges;
        value_flow_edges.extend(
            unit.value_flow_edges
                .iter()
                .filter(|edge| edge.kind == ValueFlowKind::FieldSymbolAssignment)
                .cloned(),
        );
        unit.value_flow_edges = value_flow_edges;
    }
}

struct FactBuilder<'a> {
    units: &'a [UnitAnalysis],
    scope_indexes: Vec<ScopeIndex>,
    inline_sql_table_symbols: Vec<HashSet<SymbolId>>,
    unit_indexes: HashMap<crate::ids::UnitId, usize>,
    root_type_symbols: HashMap<Arc<str>, SymbolHandle>,
    function_modules_by_name: HashMap<Arc<str>, (usize, usize)>,
}

impl<'a> FactBuilder<'a> {
    fn new(units: &'a [UnitAnalysis], scope_indexes: Vec<ScopeIndex>) -> Self {
        let inline_sql_table_symbols = units
            .iter()
            .enumerate()
            .map(|(unit_idx, unit)| {
                let mut symbols = HashSet::new();
                for target in unit
                    .sql_targets
                    .iter()
                    .filter(|target| target.is_inline && target.is_table)
                {
                    let Some(target_name) = target.target_name.as_ref() else {
                        continue;
                    };
                    if let Some(symbol_id) = lookup_scope_chain(
                        unit,
                        &scope_indexes[unit_idx],
                        target.scope,
                        Namespace::Value,
                        target_name,
                    ) {
                        symbols.insert(symbol_id);
                    }
                }
                symbols
            })
            .collect();
        let unit_indexes = units
            .iter()
            .enumerate()
            .map(|(idx, unit)| (unit.unit_id, idx))
            .collect();
        let mut root_type_symbols = HashMap::new();
        let mut function_modules_by_name = HashMap::new();
        for (unit_idx, unit) in units.iter().enumerate() {
            for symbol in unit
                .symbols
                .iter()
                .filter(|symbol| symbol.scope == unit.root_scope)
            {
                if symbol.kind.occupies(Namespace::Type) {
                    root_type_symbols
                        .entry(Arc::clone(&symbol.name))
                        .or_insert(SymbolHandle {
                            unit: unit.unit_id,
                            symbol: symbol.id,
                        });
                }
            }
            for (function_idx, function_module) in unit.function_modules.iter().enumerate() {
                let symbol = unit.symbol(function_module.symbol);
                function_modules_by_name
                    .entry(Arc::clone(&symbol.name))
                    .or_insert((unit_idx, function_idx));
            }
        }

        Self {
            units,
            scope_indexes,
            inline_sql_table_symbols,
            unit_indexes,
            root_type_symbols,
            function_modules_by_name,
        }
    }

    fn unit_index(&self, unit_id: crate::ids::UnitId) -> Option<usize> {
        self.unit_indexes.get(&unit_id).copied()
    }

    fn infer_unit(&self, unit_idx: usize) -> InferredUnitFacts {
        let unit = &self.units[unit_idx];
        let mut out = InferredUnitFacts::default();

        for reference in &unit.references {
            if reference.namespace != Namespace::Value {
                continue;
            }
            let type_fact = match reference.resolution {
                Some(Resolution::Symbol(handle)) => {
                    self.symbol_type_fact_for_site(unit_idx, reference.scope, handle)
                }
                _ => TypeFactData::default(),
            };
            out.expression_facts.push(ExpressionFactData {
                scope: reference.scope,
                range: reference.range.clone(),
                kind: ExpressionFactKind::Reference,
                type_fact,
            });
        }

        for access in unit
            .field_accesses
            .iter()
            .filter(|access| !access.in_type_position)
        {
            out.expression_facts
                .extend(self.selector_expression_facts(unit_idx, access));
        }

        for call_site in &unit.call_sites {
            out.expression_facts.push(ExpressionFactData {
                scope: call_site.scope,
                range: call_site.range.clone(),
                kind: ExpressionFactKind::CallResult,
                type_fact: self.call_result_type_fact(unit_idx, call_site.scope, &call_site.target),
            });
            out.value_flow_edges
                .extend(self.call_argument_flow_edges(unit_idx, call_site));
        }

        for assignment in &unit.assignment_sites {
            out.value_flow_edges.push(ValueFlowEdgeData {
                scope: assignment.scope,
                kind: ValueFlowKind::Assignment,
                source_range: assignment.rhs_range.clone(),
                source_type: self.enrich_existing_type_fact(
                    unit_idx,
                    assignment.scope,
                    &assignment.rhs,
                ),
                target: ValueFlowTargetData::Assignment {
                    range: assignment.lhs_range.clone(),
                },
                target_type: self.enrich_existing_type_fact(
                    unit_idx,
                    assignment.scope,
                    &assignment.lhs,
                ),
            });
        }

        dedup_expression_facts(&mut out.expression_facts);
        dedup_value_flow_edges(&mut out.value_flow_edges);
        out
    }

    fn selector_expression_facts(
        &self,
        unit_idx: usize,
        access: &FieldAccess,
    ) -> Vec<ExpressionFactData> {
        let mut out = Vec::new();
        let mut current_unit_idx = unit_idx;
        let Some(mut current_fact) = self.base_access_type_fact(
            unit_idx,
            access.scope,
            access.base_namespace,
            &access.base_name,
        ) else {
            for segment in &access.field_path {
                out.push(ExpressionFactData {
                    scope: access.scope,
                    range: segment.range.clone(),
                    kind: ExpressionFactKind::Selector,
                    type_fact: TypeFactData::default(),
                });
            }
            return out;
        };

        for segment in &access.field_path {
            let (next_unit_idx, next_fact) = self.resolve_selector_segment(
                unit_idx,
                access.scope,
                current_unit_idx,
                current_fact.clone(),
                access.base_namespace,
                segment,
            );
            out.push(ExpressionFactData {
                scope: access.scope,
                range: segment.range.clone(),
                kind: ExpressionFactKind::Selector,
                type_fact: next_fact.clone(),
            });
            current_unit_idx = next_unit_idx;
            current_fact = next_fact;
        }

        out
    }

    fn call_argument_flow_edges(
        &self,
        unit_idx: usize,
        call_site: &crate::CallSiteData,
    ) -> Vec<ValueFlowEdgeData> {
        let mut out = Vec::new();
        let Some(parameters) =
            self.call_parameter_infos(unit_idx, call_site.scope, &call_site.target)
        else {
            return out;
        };
        let mut positional_idx = 0usize;

        for argument in &call_site.arguments {
            let parameter = if let Some(argument_name) = argument.name.as_ref() {
                parameters
                    .iter()
                    .find(|parameter| parameter.name.as_ref() == Some(argument_name))
            } else {
                let parameter = parameters
                    .iter()
                    .filter(|parameter| parameter.positional)
                    .nth(positional_idx);
                positional_idx += 1;
                parameter
            };
            let Some(parameter) = parameter else {
                continue;
            };
            out.push(ValueFlowEdgeData {
                scope: call_site.scope,
                kind: ValueFlowKind::CallArgument,
                source_range: argument.range.clone(),
                source_type: self.enrich_existing_type_fact(
                    unit_idx,
                    call_site.scope,
                    &argument.type_fact,
                ),
                target: ValueFlowTargetData::CallParameter {
                    call_range: call_site.range.clone(),
                    target: call_site.target.clone(),
                    parameter_name: parameter.name.clone(),
                    parameter_decl_unit: parameter.decl_unit,
                    parameter_decl_range: parameter.decl_range.clone(),
                },
                target_type: parameter.type_fact.clone(),
            });
        }

        out
    }

    fn call_parameter_infos(
        &self,
        unit_idx: usize,
        scope: ScopeId,
        target: &NamedArgumentTarget,
    ) -> Option<Vec<CallParameterInfo>> {
        match target {
            NamedArgumentTarget::Method {
                base_namespace,
                base_name,
                method_name,
            } => {
                let class_handle = self.resolve_method_target_class_symbol(
                    unit_idx,
                    scope,
                    *base_namespace,
                    base_name,
                )?;
                let (member_unit_idx, member) =
                    self.resolve_class_member_in_hierarchy(class_handle, method_name.as_ref())?;
                Some(
                    member
                        .parameters
                        .iter()
                        .map(|parameter| CallParameterInfo {
                            name: Some(Arc::clone(&parameter.name)),
                            decl_unit: Some(self.units[member_unit_idx].unit_id),
                            decl_range: Some(parameter.range.clone()),
                            type_fact: parameter
                                .declared_type
                                .clone()
                                .map(|declared_type| {
                                    self.type_fact_from_declared_type(
                                        unit_idx,
                                        scope,
                                        member_unit_idx,
                                        declared_type,
                                        parameter.type_clause_display.clone(),
                                    )
                                })
                                .unwrap_or_default(),
                            positional: positional_parameter_section(method_parameter_section(
                                parameter.section,
                            )),
                        })
                        .collect(),
                )
            }
            NamedArgumentTarget::ImplicitMethod { method_name } => {
                let class_symbol = enclosing_class_owner(&self.units[unit_idx], scope)?;
                let class_handle = SymbolHandle {
                    unit: self.units[unit_idx].unit_id,
                    symbol: class_symbol,
                };
                let (member_unit_idx, member) =
                    self.resolve_class_member_in_hierarchy(class_handle, method_name.as_ref())?;
                Some(
                    member
                        .parameters
                        .iter()
                        .map(|parameter| CallParameterInfo {
                            name: Some(Arc::clone(&parameter.name)),
                            decl_unit: Some(self.units[member_unit_idx].unit_id),
                            decl_range: Some(parameter.range.clone()),
                            type_fact: parameter
                                .declared_type
                                .clone()
                                .map(|declared_type| {
                                    self.type_fact_from_declared_type(
                                        unit_idx,
                                        scope,
                                        member_unit_idx,
                                        declared_type,
                                        parameter.type_clause_display.clone(),
                                    )
                                })
                                .unwrap_or_default(),
                            positional: positional_parameter_section(method_parameter_section(
                                parameter.section,
                            )),
                        })
                        .collect(),
                )
            }
            NamedArgumentTarget::Function { function_name } => {
                let (function_unit_idx, function_module) =
                    self.resolve_function_module(function_name.as_ref())?;
                Some(
                    function_module
                        .parameters
                        .iter()
                        .map(|parameter| CallParameterInfo {
                            name: Some(Arc::clone(&parameter.name)),
                            decl_unit: Some(self.units[function_unit_idx].unit_id),
                            decl_range: Some(parameter.range.clone()),
                            type_fact: parameter
                                .declared_type
                                .clone()
                                .map(|declared_type| {
                                    self.type_fact_from_declared_type(
                                        unit_idx,
                                        scope,
                                        function_unit_idx,
                                        declared_type,
                                        parameter.type_clause_display.clone(),
                                    )
                                })
                                .unwrap_or_default(),
                            positional: matches!(
                                parameter.section,
                                crate::FunctionModuleParameterSection::Importing
                                    | crate::FunctionModuleParameterSection::Changing
                                    | crate::FunctionModuleParameterSection::Tables
                            ),
                        })
                        .collect(),
                )
            }
            NamedArgumentTarget::Constructor { .. } | NamedArgumentTarget::Routine { .. } => None,
        }
    }

    fn call_result_type_fact(
        &self,
        unit_idx: usize,
        scope: ScopeId,
        target: &NamedArgumentTarget,
    ) -> TypeFactData {
        match target {
            NamedArgumentTarget::Method {
                base_namespace,
                base_name,
                method_name,
            } => {
                let Some(class_handle) = self.resolve_method_target_class_symbol(
                    unit_idx,
                    scope,
                    *base_namespace,
                    base_name,
                ) else {
                    return TypeFactData::default();
                };
                let Some((member_unit_idx, member)) =
                    self.resolve_class_member_in_hierarchy(class_handle, method_name.as_ref())
                else {
                    return TypeFactData::default();
                };
                self.method_return_type_fact(unit_idx, scope, member_unit_idx, member)
            }
            NamedArgumentTarget::ImplicitMethod { method_name } => {
                let Some(class_symbol) = enclosing_class_owner(&self.units[unit_idx], scope) else {
                    return TypeFactData::default();
                };
                let Some((member_unit_idx, member)) = self.resolve_class_member_in_hierarchy(
                    SymbolHandle {
                        unit: self.units[unit_idx].unit_id,
                        symbol: class_symbol,
                    },
                    method_name.as_ref(),
                ) else {
                    return TypeFactData::default();
                };
                self.method_return_type_fact(unit_idx, scope, member_unit_idx, member)
            }
            _ => TypeFactData::default(),
        }
    }

    fn method_return_type_fact(
        &self,
        unit_idx: usize,
        scope: ScopeId,
        member_unit_idx: usize,
        member: &crate::ClassMemberData,
    ) -> TypeFactData {
        let Some(parameter) = member.parameters.iter().find(|parameter| {
            matches!(
                parameter.section,
                crate::MethodParameterSection::Returning | crate::MethodParameterSection::Receiving
            )
        }) else {
            return TypeFactData::default();
        };
        let Some(declared_type) = parameter.declared_type.clone() else {
            return TypeFactData::default();
        };
        self.type_fact_from_declared_type(
            unit_idx,
            scope,
            member_unit_idx,
            declared_type,
            parameter.type_clause_display.clone(),
        )
    }

    fn resolve_selector_segment(
        &self,
        site_unit_idx: usize,
        scope: ScopeId,
        current_unit_idx: usize,
        current_fact: TypeFactData,
        base_namespace: Namespace,
        segment: &FieldAccessSegment,
    ) -> (usize, TypeFactData) {
        if segment.is_deref() {
            return (
                current_unit_idx,
                self.portable_fact(
                    site_unit_idx,
                    current_unit_idx,
                    self.dereference_type_fact(
                        site_unit_idx,
                        scope,
                        current_unit_idx,
                        &current_fact,
                    ),
                ),
            );
        }

        if let Some(class_handle) =
            self.class_handle_from_fact(current_unit_idx, scope, base_namespace, &current_fact)
            && let Some((member_unit_idx, member)) =
                self.resolve_class_member_in_hierarchy(class_handle, segment.name.as_ref())
        {
            let fact = self.class_member_type_fact(
                site_unit_idx,
                scope,
                member_unit_idx,
                member,
                segment.name.as_ref(),
            );
            return (
                member_unit_idx,
                self.portable_fact(site_unit_idx, member_unit_idx, fact),
            );
        }

        let Some(structure_id) = current_fact.structure else {
            return (current_unit_idx, TypeFactData::default());
        };
        let Some(field) =
            self.units[current_unit_idx].structure_field_info(structure_id, segment.name.as_ref())
        else {
            return (current_unit_idx, TypeFactData::default());
        };
        let mut fact = field
            .type_ref
            .clone()
            .map(|declared_type| {
                self.type_fact_from_declared_type(
                    site_unit_idx,
                    scope,
                    current_unit_idx,
                    declared_type,
                    None,
                )
            })
            .unwrap_or_default();
        if fact.structure.is_none() {
            fact.structure = match field.shape {
                crate::StructureFieldShape::Structured { structure } => Some(structure),
                crate::StructureFieldShape::Scalar => None,
            };
        }
        (
            current_unit_idx,
            self.portable_fact(site_unit_idx, current_unit_idx, fact),
        )
    }

    fn class_member_type_fact(
        &self,
        site_unit_idx: usize,
        scope: ScopeId,
        member_unit_idx: usize,
        member: &crate::ClassMemberData,
        member_name: &str,
    ) -> TypeFactData {
        let unit = &self.units[member_unit_idx];
        if let Some(symbol) = unit.symbols.iter().find(|symbol| {
            symbol.name.as_ref() == member_name
                && unit.scope(symbol.scope).kind == ScopeKind::Class
                && unit.scope(symbol.scope).owner == Some(member.class_symbol)
        }) {
            return self.symbol_type_fact_for_site(
                site_unit_idx,
                scope,
                SymbolHandle {
                    unit: unit.unit_id,
                    symbol: symbol.id,
                },
            );
        }
        TypeFactData {
            structure: (site_unit_idx == member_unit_idx)
                .then_some(member.structure)
                .flatten(),
            declared_type: None,
            type_clause_display: None,
            table_line: None,
        }
    }

    fn base_access_type_fact(
        &self,
        unit_idx: usize,
        scope: ScopeId,
        namespace: Namespace,
        base_name: &Arc<str>,
    ) -> Option<TypeFactData> {
        match namespace {
            Namespace::Value => {
                let symbol_id = lookup_scope_chain(
                    &self.units[unit_idx],
                    &self.scope_indexes[unit_idx],
                    scope,
                    Namespace::Value,
                    base_name,
                )?;
                Some(self.symbol_type_fact_for_site(
                    unit_idx,
                    scope,
                    SymbolHandle {
                        unit: self.units[unit_idx].unit_id,
                        symbol: symbol_id,
                    },
                ))
            }
            Namespace::Type => {
                let handle =
                    self.resolve_type_symbol_handle(unit_idx, scope, base_name.as_ref())?;
                Some(self.symbol_type_fact_for_site(unit_idx, scope, handle))
            }
            Namespace::Routine => None,
        }
    }

    fn symbol_type_fact_for_site(
        &self,
        site_unit_idx: usize,
        scope: ScopeId,
        handle: SymbolHandle,
    ) -> TypeFactData {
        let Some(symbol_unit_idx) = self.unit_index(handle.unit) else {
            return TypeFactData::default();
        };
        let symbol = self.units[symbol_unit_idx].symbol(handle.symbol);
        let mut fact = TypeFactData {
            structure: (site_unit_idx == symbol_unit_idx)
                .then_some(symbol.structure)
                .flatten(),
            declared_type: symbol.declared_type.clone(),
            type_clause_display: symbol.type_clause_display.clone(),
            table_line: None,
        };
        if self.symbol_is_table(handle, symbol) {
            fact.table_line = Some(Box::new(TypeFactData {
                structure: (site_unit_idx == symbol_unit_idx)
                    .then_some(symbol.structure)
                    .flatten(),
                declared_type: symbol.declared_type.clone(),
                type_clause_display: None,
                table_line: None,
            }));
        }
        self.enrich_existing_type_fact(site_unit_idx, scope, &fact)
    }

    fn type_fact_from_declared_type(
        &self,
        site_unit_idx: usize,
        scope: ScopeId,
        current_unit_idx: usize,
        declared_type: FieldTypeRefData,
        type_clause_display: Option<Arc<str>>,
    ) -> TypeFactData {
        let structure = if declared_type.namespace == Namespace::Type
            && !declared_type.is_ref
            && declared_type.field_path.is_empty()
        {
            self.resolve_type_symbol_handle(
                current_unit_idx,
                scope,
                declared_type.base_name.as_ref(),
            )
            .and_then(|handle| {
                self.unit_index(handle.unit)
                    .filter(|&idx| idx == site_unit_idx)
                    .and_then(|idx| self.units[idx].symbol(handle.symbol).structure)
            })
        } else {
            None
        };
        let mut fact = TypeFactData {
            structure,
            declared_type: Some(declared_type),
            type_clause_display,
            table_line: None,
        };
        if fact
            .type_clause_display
            .as_deref()
            .is_some_and(is_internal_table_type_display)
        {
            fact.table_line = Some(Box::new(TypeFactData {
                structure: fact.structure,
                declared_type: fact.declared_type.clone(),
                type_clause_display: None,
                table_line: None,
            }));
        }
        self.enrich_existing_type_fact(site_unit_idx, scope, &fact)
    }

    fn enrich_existing_type_fact(
        &self,
        site_unit_idx: usize,
        scope: ScopeId,
        fact: &TypeFactData,
    ) -> TypeFactData {
        let mut fact = fact.clone();
        if fact.table_line.is_none()
            && fact
                .type_clause_display
                .as_deref()
                .is_some_and(is_internal_table_type_display)
        {
            fact.table_line = Some(Box::new(TypeFactData {
                structure: fact.structure,
                declared_type: fact.declared_type.clone(),
                type_clause_display: None,
                table_line: None,
            }));
        }
        if fact.structure.is_none()
            && let Some(declared_type) = fact.declared_type.as_ref()
            && declared_type.namespace == Namespace::Type
            && !declared_type.is_ref
            && declared_type.field_path.is_empty()
            && let Some(handle) = self.resolve_type_symbol_handle(
                site_unit_idx,
                scope,
                declared_type.base_name.as_ref(),
            )
            && self.unit_index(handle.unit) == Some(site_unit_idx)
        {
            fact.structure = self.units[site_unit_idx].symbol(handle.symbol).structure;
        }
        if let Some(line_fact) = fact.table_line.take() {
            fact.table_line = Some(Box::new(self.enrich_existing_type_fact(
                site_unit_idx,
                scope,
                &line_fact,
            )));
        }
        fact
    }

    fn dereference_type_fact(
        &self,
        site_unit_idx: usize,
        scope: ScopeId,
        current_unit_idx: usize,
        fact: &TypeFactData,
    ) -> TypeFactData {
        let Some(declared_type) = fact.declared_type.as_ref() else {
            return TypeFactData::default();
        };
        if !declared_type.is_ref {
            return TypeFactData::default();
        }
        let structure = self
            .resolve_type_symbol_handle(current_unit_idx, scope, declared_type.base_name.as_ref())
            .and_then(|handle| {
                self.unit_index(handle.unit)
                    .filter(|&idx| idx == site_unit_idx)
                    .and_then(|idx| self.units[idx].symbol(handle.symbol).structure)
            });
        TypeFactData {
            structure,
            declared_type: Some(FieldTypeRefData {
                namespace: declared_type.namespace,
                is_ref: false,
                base_name: Arc::clone(&declared_type.base_name),
                field_path: declared_type.field_path.clone(),
            }),
            type_clause_display: None,
            table_line: None,
        }
    }

    fn class_handle_from_fact(
        &self,
        current_unit_idx: usize,
        scope: ScopeId,
        base_namespace: Namespace,
        fact: &TypeFactData,
    ) -> Option<SymbolHandle> {
        if base_namespace == Namespace::Type
            && let Some(declared_type) = fact.declared_type.as_ref()
        {
            return self.resolve_type_symbol_handle(
                current_unit_idx,
                scope,
                declared_type.base_name.as_ref(),
            );
        }
        let declared_type = fact.declared_type.as_ref()?;
        if !declared_type.is_ref || !declared_type.field_path.is_empty() {
            return None;
        }
        self.resolve_type_symbol_handle(current_unit_idx, scope, declared_type.base_name.as_ref())
    }

    fn resolve_method_target_class_symbol(
        &self,
        unit_idx: usize,
        scope: ScopeId,
        base_namespace: Namespace,
        base_name: &Arc<str>,
    ) -> Option<SymbolHandle> {
        match base_namespace {
            Namespace::Type => self.resolve_type_symbol_handle(unit_idx, scope, base_name.as_ref()),
            Namespace::Value => {
                let symbol_id = lookup_scope_chain(
                    &self.units[unit_idx],
                    &self.scope_indexes[unit_idx],
                    scope,
                    Namespace::Value,
                    base_name,
                )?;
                let symbol = self.units[unit_idx].symbol(symbol_id);
                let declared_type = symbol.declared_type.as_ref()?;
                if !declared_type.is_ref || !declared_type.field_path.is_empty() {
                    return None;
                }
                self.resolve_type_symbol_handle(unit_idx, scope, declared_type.base_name.as_ref())
            }
            Namespace::Routine => None,
        }
    }

    fn resolve_class_member_in_hierarchy(
        &self,
        mut class_handle: SymbolHandle,
        member_name: &str,
    ) -> Option<(usize, &'a crate::ClassMemberData)> {
        for _ in 0..16 {
            let unit_idx = self.unit_index(class_handle.unit)?;
            let unit = &self.units[unit_idx];
            if let Some(member) = unit.class_member(class_handle.symbol, member_name) {
                return Some((unit_idx, member));
            }
            let inheritance = unit.class_superclass(class_handle.symbol)?;
            class_handle = self.resolve_type_symbol_handle(
                unit_idx,
                unit.root_scope,
                inheritance.superclass_name.as_ref(),
            )?;
        }
        None
    }

    fn resolve_function_module(
        &self,
        function_name: &str,
    ) -> Option<(usize, &'a crate::FunctionModuleData)> {
        let &(unit_idx, function_idx) = self.function_modules_by_name.get(function_name)?;
        Some((
            unit_idx,
            &self.units[unit_idx].function_modules[function_idx],
        ))
    }

    fn resolve_type_symbol_handle(
        &self,
        unit_idx: usize,
        scope: ScopeId,
        name: &str,
    ) -> Option<SymbolHandle> {
        let local_name = Arc::<str>::from(name.to_ascii_lowercase());
        if let Some(symbol) = lookup_scope_chain(
            &self.units[unit_idx],
            &self.scope_indexes[unit_idx],
            scope,
            Namespace::Type,
            &local_name,
        ) {
            return Some(SymbolHandle {
                unit: self.units[unit_idx].unit_id,
                symbol,
            });
        }
        self.root_type_symbols.get(name).copied()
    }

    fn symbol_is_table(&self, handle: SymbolHandle, symbol: &crate::SymbolData) -> bool {
        symbol
            .type_clause_display
            .as_deref()
            .is_some_and(is_internal_table_type_display)
            || self
                .unit_index(handle.unit)
                .is_some_and(|idx| self.inline_sql_table_symbols[idx].contains(&handle.symbol))
    }

    fn portable_fact(
        &self,
        site_unit_idx: usize,
        current_unit_idx: usize,
        mut fact: TypeFactData,
    ) -> TypeFactData {
        if let Some(line_fact) = fact.table_line.take() {
            fact.table_line = Some(Box::new(self.portable_fact(
                site_unit_idx,
                current_unit_idx,
                *line_fact,
            )));
        }
        if site_unit_idx != current_unit_idx {
            fact.structure = None;
        }
        fact
    }
}

fn lookup_scope_chain(
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
) -> Option<SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if let Some(symbols) = scope_index
            .get(scope_id.as_usize())
            .and_then(|scope_map| scope_map.get(&(namespace, Arc::clone(name))))
            && let Some(symbol) = symbols.last().copied()
        {
            return Some(symbol);
        }
        current = unit
            .scopes
            .get(scope_id.as_usize())
            .and_then(|scope| scope.parent);
    }
    None
}

fn enclosing_class_owner(unit: &UnitAnalysis, scope: ScopeId) -> Option<SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        let scope = unit.scopes.get(scope_id.as_usize())?;
        if scope.kind == ScopeKind::Class {
            return scope.owner;
        }
        current = scope.parent;
    }
    None
}

fn is_internal_table_type_display(display: &str) -> bool {
    let upper = display.trim().to_ascii_uppercase();
    upper.contains(" TABLE OF ")
}

fn method_parameter_section(section: crate::MethodParameterSection) -> MethodParameterSection {
    section
}

fn dedup_expression_facts(facts: &mut Vec<ExpressionFactData>) {
    facts.sort_by(|left, right| {
        left.range
            .start
            .cmp(&right.range.start)
            .then(left.range.end.cmp(&right.range.end))
            .then((left.kind as u8).cmp(&(right.kind as u8)))
    });
    facts.dedup_by(|left, right| {
        left.scope == right.scope
            && left.range == right.range
            && left.kind == right.kind
            && left.type_fact == right.type_fact
    });
}

fn dedup_value_flow_edges(edges: &mut Vec<ValueFlowEdgeData>) {
    edges.sort_by(|left, right| {
        left.source_range
            .start
            .cmp(&right.source_range.start)
            .then(left.source_range.end.cmp(&right.source_range.end))
            .then((left.kind as u8).cmp(&(right.kind as u8)))
    });
    edges.dedup();
}
