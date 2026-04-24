use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::compatibility::positional_parameter_section;
use crate::def_map::{
    ExpressionFactData, ExpressionFactKind, FieldAccess, FieldAccessSegment, FieldTypeRefData,
    MethodParameterSection, NamedArgumentSection, NamedArgumentTarget, Resolution,
    RoutineControlRegionData, RoutineLoopKind, TypeFactData, UnitAnalysis, ValueFlowEdgeData,
    ValueFlowKind, ValueFlowTargetData,
};
use crate::ids::{ScopeId, SymbolHandle, SymbolId};
use crate::resolver::{ScopeIndex, build_scope_index};
use crate::scope::{Namespace, ScopeKind};

#[derive(Debug, Clone, Default)]
struct InferredUnitFacts {
    expression_facts: Vec<ExpressionFactData>,
    value_flow_edges: Vec<ValueFlowEdgeData>,
    symbol_type_facts: Vec<SymbolTypeFactUpdate>,
    assignment_type_facts: Vec<(usize, TypeFactData, TypeFactData)>,
}

#[derive(Debug, Clone)]
struct SymbolTypeFactUpdate {
    symbol_id: SymbolId,
    type_fact: TypeFactData,
    overwrite_existing: bool,
}

#[derive(Debug, Clone)]
struct CallParameterInfo {
    name: Option<Arc<str>>,
    decl_unit: Option<crate::ids::UnitId>,
    decl_range: Option<abap_lexer::TextRange>,
    type_fact: TypeFactData,
    positional: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CallArgumentObservationKey {
    target: NamedArgumentTarget,
    name: Arc<str>,
    section: Option<NamedArgumentSection>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ObservedCallArgumentType {
    declared_type: FieldTypeRefData,
    type_clause_display: Option<Arc<str>>,
}

pub(crate) fn infer_semantic_facts(units: &mut [UnitAnalysis]) {
    let rerun = apply_inferred_unit_facts(units, infer_all_unit_facts(units));
    if rerun {
        apply_inferred_unit_facts(units, infer_all_unit_facts(units));
    }
}

fn infer_all_unit_facts(units: &[UnitAnalysis]) -> Vec<InferredUnitFacts> {
    let scope_indexes: Vec<_> = units.iter().map(build_scope_index).collect();
    let builder = FactBuilder::new(units, scope_indexes);
    (0..units.len())
        .map(|unit_idx| builder.infer_unit(unit_idx))
        .collect()
}

fn apply_inferred_unit_facts(units: &mut [UnitAnalysis], inferred: Vec<InferredUnitFacts>) -> bool {
    let mut rerun = false;
    for (unit, facts) in units.iter_mut().zip(inferred) {
        unit.expression_facts = facts.expression_facts;
        let mut value_flow_edges = facts.value_flow_edges;
        value_flow_edges.extend(
            unit.value_flow_edges
                .iter()
                .filter(|edge| {
                    matches!(
                        edge.kind,
                        ValueFlowKind::FieldSymbolAssignment
                            | ValueFlowKind::ConditionalFieldSymbolAssignment
                    )
                })
                .cloned(),
        );
        unit.value_flow_edges = value_flow_edges;
        for update in facts.symbol_type_facts {
            let symbol = &mut unit.symbols[update.symbol_id.as_usize()];
            if update.overwrite_existing {
                rerun |= symbol.structure != update.type_fact.structure
                    || symbol.declared_type != update.type_fact.declared_type
                    || symbol.type_clause_display != update.type_fact.type_clause_display;
                symbol.structure = update.type_fact.structure;
                symbol.declared_type = update.type_fact.declared_type;
                symbol.type_clause_display = update.type_fact.type_clause_display;
            } else if symbol.declared_type.is_none() {
                let structure = symbol.structure.or(update.type_fact.structure);
                rerun |= symbol.structure != structure
                    || symbol.declared_type != update.type_fact.declared_type;
                symbol.structure = structure;
                symbol.declared_type = update.type_fact.declared_type;
            }
        }
        for (assignment_idx, lhs, rhs) in facts.assignment_type_facts {
            if let Some(assignment) = unit.assignment_sites.get_mut(assignment_idx) {
                assignment.lhs = lhs;
                assignment.rhs = rhs;
            }
        }
    }
    rerun
}

struct FactBuilder<'a> {
    units: &'a [UnitAnalysis],
    scope_indexes: Vec<ScopeIndex>,
    inline_sql_table_symbols: Vec<HashSet<SymbolId>>,
    unit_indexes: HashMap<crate::ids::UnitId, usize>,
    root_type_symbols: HashMap<Arc<str>, SymbolHandle>,
    function_modules_by_name: HashMap<Arc<str>, (usize, usize)>,
    observed_call_argument_types: HashMap<CallArgumentObservationKey, ObservedCallArgumentType>,
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

        let mut builder = Self {
            units,
            scope_indexes,
            inline_sql_table_symbols,
            unit_indexes,
            root_type_symbols,
            function_modules_by_name,
            observed_call_argument_types: HashMap::new(),
        };
        builder.observed_call_argument_types = builder.build_observed_call_argument_types();
        builder
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

        out.symbol_type_facts
            .extend(self.loop_inline_target_type_facts(unit_idx));
        out.symbol_type_facts
            .extend(self.inline_call_argument_type_facts(unit_idx));

        for assignment in &unit.assignment_sites {
            let lhs_type = self.assignment_target_type_fact(unit_idx, assignment);
            let rhs_type = self.assignment_source_type_fact(unit_idx, assignment);
            out.assignment_type_facts.push((
                out.assignment_type_facts.len(),
                lhs_type.clone(),
                rhs_type.clone(),
            ));
            out.value_flow_edges.push(ValueFlowEdgeData {
                scope: assignment.scope,
                kind: ValueFlowKind::Assignment,
                source_range: assignment.rhs_range.clone(),
                source_type: rhs_type,
                target: ValueFlowTargetData::Assignment {
                    range: assignment.lhs_range.clone(),
                },
                target_type: lhs_type,
            });
            if let Some(update) = self.infer_inline_assignment_symbol_type(unit_idx, assignment) {
                out.symbol_type_facts.push(update);
            }
        }

        dedup_expression_facts(&mut out.expression_facts);
        dedup_value_flow_edges(&mut out.value_flow_edges);
        out
    }

    fn infer_inline_assignment_symbol_type(
        &self,
        unit_idx: usize,
        assignment: &crate::AssignmentSiteData,
    ) -> Option<SymbolTypeFactUpdate> {
        if !assignment.rhs_is_top_level_sum {
            return None;
        }
        let unit = &self.units[unit_idx];
        let symbol = unit.symbols.iter().find(|symbol| {
            symbol.decl_range == assignment.lhs_range
                && symbol.kind == crate::SymbolKind::Variable
                && symbol.declared_type.is_none()
        })?;

        let refs: Vec<_> = unit
            .references
            .iter()
            .filter(|reference| {
                reference.namespace == Namespace::Value
                    && reference.range.start >= assignment.rhs_range.start
                    && reference.range.end <= assignment.rhs_range.end
            })
            .collect();
        if refs.is_empty() {
            return None;
        }

        let mut inferred_fact: Option<TypeFactData> = None;
        for reference in refs {
            let Some(Resolution::Symbol(handle)) = reference.resolution else {
                return None;
            };
            let fact = self.symbol_type_fact_for_site(unit_idx, assignment.scope, handle);
            let declared_type = fact.declared_type.as_ref()?;
            if declared_type.namespace != Namespace::Type
                || declared_type.is_ref
                || !declared_type.field_path.is_empty()
            {
                return None;
            }
            if let Some(existing_fact) = inferred_fact.as_ref() {
                if existing_fact.declared_type.as_ref()? != declared_type {
                    return None;
                }
            } else {
                inferred_fact = Some(fact);
            }
        }

        inferred_fact.map(|fact| SymbolTypeFactUpdate {
            symbol_id: symbol.id,
            type_fact: fact,
            overwrite_existing: false,
        })
    }

    fn build_observed_call_argument_types(
        &self,
    ) -> HashMap<CallArgumentObservationKey, ObservedCallArgumentType> {
        let mut observations = HashMap::new();
        let mut ambiguous = HashSet::new();

        for (unit_idx, unit) in self.units.iter().enumerate() {
            for call_site in &unit.call_sites {
                for argument in &call_site.arguments {
                    let Some(key) = call_argument_observation_key(call_site, argument) else {
                        continue;
                    };
                    let Some(observed) = observed_type_from_call_argument(argument).or_else(|| {
                        self.direct_call_argument_declared_type(unit_idx, call_site, argument)
                    }) else {
                        continue;
                    };
                    if let Some(existing) = observations.get(&key) {
                        if existing != &observed {
                            ambiguous.insert(key);
                        }
                    } else {
                        observations.insert(key, observed);
                    }
                }
            }
        }

        for key in ambiguous {
            observations.remove(&key);
        }
        observations
    }

    fn direct_call_argument_declared_type(
        &self,
        unit_idx: usize,
        call_site: &crate::CallSiteData,
        argument: &crate::CallArgumentData,
    ) -> Option<ObservedCallArgumentType> {
        let unit = &self.units[unit_idx];
        let mut refs = unit.references.iter().filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.range.start >= argument.range.start
                && reference.range.end <= argument.range.end
        });
        let reference = refs.next()?;
        if refs.next().is_some() {
            return None;
        }
        let Some(Resolution::Symbol(handle)) = reference.resolution else {
            return None;
        };
        let fact = self.symbol_type_fact_for_site(unit_idx, call_site.scope, handle);
        let declared_type = fact.declared_type?;
        Some(ObservedCallArgumentType {
            declared_type,
            type_clause_display: fact.type_clause_display,
        })
    }

    fn inline_call_argument_type_facts(&self, unit_idx: usize) -> Vec<SymbolTypeFactUpdate> {
        let unit = &self.units[unit_idx];
        let mut updates = Vec::new();
        for call_site in &unit.call_sites {
            for argument in &call_site.arguments {
                let Some(key) = call_argument_observation_key(call_site, argument) else {
                    continue;
                };
                let Some(observed) = self.observed_call_argument_types.get(&key) else {
                    continue;
                };
                let Some(symbol_id) = inline_variable_symbol_for_call_argument(unit, argument)
                else {
                    continue;
                };
                if unit.symbol(symbol_id).declared_type.is_some() {
                    continue;
                }
                let fact = self.type_fact_from_declared_type(
                    unit_idx,
                    call_site.scope,
                    unit_idx,
                    observed.declared_type.clone(),
                    observed.type_clause_display.clone(),
                );
                updates.push(SymbolTypeFactUpdate {
                    symbol_id,
                    type_fact: fact,
                    overwrite_existing: false,
                });
            }
        }
        updates
    }

    fn loop_inline_target_type_facts(&self, unit_idx: usize) -> Vec<SymbolTypeFactUpdate> {
        let unit = &self.units[unit_idx];
        let mut updates = Vec::new();
        for region in &unit.routine_control_regions {
            let RoutineControlRegionData::Loop(region) = region else {
                continue;
            };
            if region.kind != RoutineLoopKind::Loop {
                continue;
            }
            let (Some(source_access), Some(target_access)) =
                (region.source_access.as_ref(), region.target_access.as_ref())
            else {
                continue;
            };
            let Some(symbol_id) = self.inline_symbol_for_target_access(unit_idx, target_access)
            else {
                continue;
            };
            if unit
                .symbol(symbol_id)
                .type_clause_display
                .as_deref()
                .is_some_and(is_line_of_type_display)
            {
                continue;
            }
            let source_fact = self.type_fact_for_access(unit_idx, source_access);
            let Some(line_fact) = source_fact.table_line.as_deref() else {
                continue;
            };
            if !line_fact.is_known() {
                continue;
            }
            updates.push(SymbolTypeFactUpdate {
                symbol_id,
                type_fact: line_fact.clone(),
                overwrite_existing: true,
            });
        }
        updates
    }

    fn inline_symbol_for_target_access(
        &self,
        unit_idx: usize,
        access: &FieldAccess,
    ) -> Option<SymbolId> {
        if access.base_namespace != Namespace::Value || !access.field_path.is_empty() {
            return None;
        }
        let symbol_id = lookup_scope_chain(
            &self.units[unit_idx],
            &self.scope_indexes[unit_idx],
            access.scope,
            Namespace::Value,
            &access.base_name,
        )?;
        let symbol = self.units[unit_idx].symbol(symbol_id);
        (symbol.decl_range == access.base_range
            && matches!(
                symbol.kind,
                crate::SymbolKind::Variable | crate::SymbolKind::FieldSymbol
            ))
        .then_some(symbol_id)
    }

    fn assignment_source_type_fact(
        &self,
        unit_idx: usize,
        assignment: &crate::AssignmentSiteData,
    ) -> TypeFactData {
        if is_append_assignment(&self.units[unit_idx], &assignment.range) {
            return self.enrich_existing_type_fact(
                unit_idx,
                assignment.scope,
                unit_idx,
                &assignment.rhs,
            );
        }
        self.assignment_rhs_access_type_fact(unit_idx, assignment)
            .unwrap_or_else(|| {
                self.enrich_existing_type_fact(
                    unit_idx,
                    assignment.scope,
                    unit_idx,
                    &assignment.rhs,
                )
            })
    }

    fn assignment_rhs_access_type_fact(
        &self,
        unit_idx: usize,
        assignment: &crate::AssignmentSiteData,
    ) -> Option<TypeFactData> {
        let unit = &self.units[unit_idx];
        let exact_accesses = unit
            .field_accesses
            .iter()
            .filter(|access| !access.in_type_position && access.scope == assignment.scope)
            .filter(|access| field_access_range(access) == assignment.rhs_range)
            .collect::<Vec<_>>();
        if exact_accesses.len() == 1 {
            return Some(self.type_fact_for_access(unit_idx, exact_accesses[0]));
        }

        let exact_refs = unit
            .references
            .iter()
            .filter(|reference| {
                reference.namespace == Namespace::Value
                    && reference.scope == assignment.scope
                    && reference.range == assignment.rhs_range
            })
            .collect::<Vec<_>>();
        if exact_refs.len() != 1 {
            return None;
        }
        let Some(Resolution::Symbol(handle)) = exact_refs[0].resolution else {
            return None;
        };
        Some(self.symbol_type_fact_for_site(unit_idx, assignment.scope, handle))
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
                    unit_idx,
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

    fn assignment_target_type_fact(
        &self,
        unit_idx: usize,
        assignment: &crate::AssignmentSiteData,
    ) -> TypeFactData {
        if let Some(access) = assignment.lhs_target_access.as_ref()
            && is_append_assignment(&self.units[unit_idx], &assignment.range)
        {
            let access_fact = self.type_fact_for_access(unit_idx, access);
            if let Some(line_fact) = access_fact.table_line.as_deref() {
                return line_fact.clone();
            }
        }
        let fact =
            self.enrich_existing_type_fact(unit_idx, assignment.scope, unit_idx, &assignment.lhs);
        if fact.is_known() {
            return fact;
        }
        let Some(access) = assignment.lhs_target_access.as_ref() else {
            return fact;
        };
        self.type_fact_for_access(unit_idx, access)
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
            NamedArgumentTarget::Constructor { .. }
            | NamedArgumentTarget::Report { .. }
            | NamedArgumentTarget::Routine { .. } => None,
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

    fn type_fact_for_access(&self, unit_idx: usize, access: &FieldAccess) -> TypeFactData {
        let Some(mut current_fact) = self.base_access_type_fact(
            unit_idx,
            access.scope,
            access.base_namespace,
            &access.base_name,
        ) else {
            return TypeFactData::default();
        };
        let mut current_unit_idx = unit_idx;

        for segment in &access.field_path {
            let (next_unit_idx, next_fact) = self.resolve_selector_segment(
                unit_idx,
                access.scope,
                current_unit_idx,
                current_fact,
                access.base_namespace,
                segment,
            );
            current_unit_idx = next_unit_idx;
            current_fact = next_fact;
        }

        current_fact
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
            fact.table_line = Some(Box::new(
                self.synthesized_table_line_fact(TypeFactData {
                    structure: (site_unit_idx == symbol_unit_idx)
                        .then_some(symbol.structure)
                        .flatten(),
                    declared_type: symbol.declared_type.clone(),
                    type_clause_display: symbol.type_clause_display.clone(),
                    table_line: None,
                }),
            ));
        }
        self.enrich_existing_type_fact(site_unit_idx, scope, symbol_unit_idx, &fact)
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
            .is_some_and(is_table_like_type_display)
        {
            fact.table_line = Some(Box::new(self.synthesized_table_line_fact(TypeFactData {
                structure: fact.structure,
                declared_type: fact.declared_type.clone(),
                type_clause_display: fact.type_clause_display.clone(),
                table_line: None,
            })));
        }
        self.enrich_existing_type_fact(site_unit_idx, scope, current_unit_idx, &fact)
    }

    fn enrich_existing_type_fact(
        &self,
        site_unit_idx: usize,
        scope: ScopeId,
        current_unit_idx: usize,
        fact: &TypeFactData,
    ) -> TypeFactData {
        let mut fact = fact.clone();
        if fact.table_line.is_none()
            && let Some(line_fact) =
                self.resolve_table_line_fact(site_unit_idx, scope, current_unit_idx, &fact, 0)
        {
            fact.table_line = Some(Box::new(line_fact));
        }
        if fact.structure.is_none()
            && fact
                .type_clause_display
                .as_deref()
                .is_some_and(is_line_of_type_display)
            && let Some(declared_type) = fact.declared_type.as_ref()
            && declared_type.namespace == Namespace::Value
            && !declared_type.is_ref
            && declared_type.field_path.is_empty()
            && let Some(symbol_id) = lookup_scope_chain(
                &self.units[site_unit_idx],
                &self.scope_indexes[site_unit_idx],
                scope,
                Namespace::Value,
                &declared_type.base_name,
            )
        {
            let symbol_fact = self.symbol_type_fact_for_site(
                site_unit_idx,
                scope,
                SymbolHandle {
                    unit: self.units[site_unit_idx].unit_id,
                    symbol: symbol_id,
                },
            );
            if let Some(line_fact) = symbol_fact.table_line.as_deref() {
                fact.structure = line_fact.structure;
                fact.declared_type = line_fact.declared_type.clone();
            }
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
                current_unit_idx,
                &line_fact,
            )));
        }
        fact
    }

    fn resolve_table_line_fact(
        &self,
        site_unit_idx: usize,
        scope: ScopeId,
        current_unit_idx: usize,
        fact: &TypeFactData,
        depth: usize,
    ) -> Option<TypeFactData> {
        if depth >= 8 {
            return None;
        }
        if fact
            .type_clause_display
            .as_deref()
            .is_some_and(is_table_like_type_display)
        {
            return Some(self.synthesized_table_line_fact(fact.clone()));
        }

        let declared_type = fact.declared_type.as_ref()?;
        if declared_type.namespace != Namespace::Type
            || declared_type.is_ref
            || !declared_type.field_path.is_empty()
        {
            return None;
        }
        let handle = self.resolve_type_symbol_handle(
            current_unit_idx,
            scope,
            declared_type.base_name.as_ref(),
        )?;
        let type_unit_idx = self.unit_index(handle.unit)?;
        let type_symbol = self.units[type_unit_idx].symbol(handle.symbol);
        if !type_symbol
            .type_clause_display
            .as_deref()
            .is_some_and(is_table_like_type_display)
        {
            return None;
        }

        let line_fact = self.resolve_table_line_fact(
            site_unit_idx,
            scope,
            type_unit_idx,
            &TypeFactData {
                structure: type_symbol.structure,
                declared_type: type_symbol.declared_type.clone(),
                type_clause_display: type_symbol.type_clause_display.clone(),
                table_line: None,
            },
            depth + 1,
        )?;
        Some(self.portable_fact(site_unit_idx, type_unit_idx, line_fact))
    }

    fn synthesized_table_line_fact(&self, fact: TypeFactData) -> TypeFactData {
        if fact
            .type_clause_display
            .as_deref()
            .is_some_and(is_range_table_type_display)
        {
            return TypeFactData {
                structure: fact.structure,
                declared_type: None,
                type_clause_display: None,
                table_line: None,
            };
        }

        TypeFactData {
            structure: fact.structure,
            declared_type: fact.declared_type,
            type_clause_display: None,
            table_line: None,
        }
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

fn field_access_range(access: &FieldAccess) -> std::ops::Range<usize> {
    access.base_range.start
        ..access
            .field_path
            .last()
            .map(|segment| segment.range.end)
            .unwrap_or(access.base_range.end)
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

fn is_range_table_type_display(display: &str) -> bool {
    display.trim().to_ascii_uppercase().starts_with("RANGE OF ")
}

fn is_table_like_type_display(display: &str) -> bool {
    is_internal_table_type_display(display) || is_range_table_type_display(display)
}

fn is_line_of_type_display(display: &str) -> bool {
    display.trim().to_ascii_uppercase().starts_with("LINE OF ")
}

fn is_append_assignment(unit: &UnitAnalysis, range: &abap_lexer::TextRange) -> bool {
    unit.system_field_updates.iter().any(|update| {
        update.statement == crate::SystemFieldStatementKind::Append && &update.range == range
    })
}

fn method_parameter_section(section: crate::MethodParameterSection) -> MethodParameterSection {
    section
}

fn call_argument_observation_key(
    call_site: &crate::CallSiteData,
    argument: &crate::CallArgumentData,
) -> Option<CallArgumentObservationKey> {
    if argument.section == Some(NamedArgumentSection::Exceptions) {
        return None;
    }
    Some(CallArgumentObservationKey {
        target: call_site.target.clone(),
        name: Arc::clone(argument.name.as_ref()?),
        section: argument.section,
    })
}

fn observed_type_from_call_argument(
    argument: &crate::CallArgumentData,
) -> Option<ObservedCallArgumentType> {
    Some(ObservedCallArgumentType {
        declared_type: argument.type_fact.declared_type.clone()?,
        type_clause_display: argument.type_fact.type_clause_display.clone(),
    })
}

fn inline_variable_symbol_for_call_argument(
    unit: &UnitAnalysis,
    argument: &crate::CallArgumentData,
) -> Option<SymbolId> {
    let mut matches = unit.symbols.iter().filter(|symbol| {
        symbol.kind == crate::SymbolKind::Variable
            && symbol.decl_range.start >= argument.range.start
            && symbol.decl_range.end <= argument.range.end
    });
    let symbol = matches.next()?;
    matches.next().is_none().then_some(symbol.id)
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
