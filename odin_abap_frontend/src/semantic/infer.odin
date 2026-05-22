package abap_frontend_semantic

import "../tokenizer"

import "core:mem"
import "core:slice"

Inferred_Symbol_Type_Update :: struct {
	symbol:             Symbol_Id,
	type_fact:          Type_Fact_Data,
	overwrite_existing: bool,
}

Inferred_Assignment_Update :: struct {
	index: int,
	lhs:   Type_Fact_Data,
	rhs:   Type_Fact_Data,
}

Inferred_Concatenate_Update :: struct {
	index:  int,
	source: Type_Fact_Data,
}

Range_Type_Fact :: struct {
	range:     tokenizer.Range,
	type_fact: Type_Fact_Data,
}

Range_Type_Fact_Index :: struct {
	facts: [dynamic]Range_Type_Fact,
}

Inline_Symbol_Range :: struct {
	range:  tokenizer.Range,
	symbol: Symbol_Id,
}

Inline_Symbol_Index :: struct {
	symbols: [dynamic]Inline_Symbol_Range,
}

Inferred_Unit_Facts :: struct {
	expression_facts: [dynamic]Expression_Fact_Data,
	value_flow_edges: [dynamic]Value_Flow_Edge_Data,
	symbol_updates:   [dynamic]Inferred_Symbol_Type_Update,
	assignments:      [dynamic]Inferred_Assignment_Update,
	concatenates:     [dynamic]Inferred_Concatenate_Update,
}

inferred_unit_facts_make :: proc(unit: ^Unit_Analysis, allocator: mem.Allocator) -> Inferred_Unit_Facts {
	value_flow_cap := len(unit.assignment_sites) + len(unit.named_arguments) + len(unit.call_sites)
	return Inferred_Unit_Facts {
		expression_facts = make(
			[dynamic]Expression_Fact_Data,
			0,
			len(unit.references) + len(unit.field_accesses) + len(unit.call_sites),
			allocator,
		),
		value_flow_edges = make([dynamic]Value_Flow_Edge_Data, 0, value_flow_cap, allocator),
		symbol_updates = make(
			[dynamic]Inferred_Symbol_Type_Update,
			0,
			len(unit.assignment_sites),
			allocator,
		),
		assignments = make(
			[dynamic]Inferred_Assignment_Update,
			0,
			len(unit.assignment_sites),
			allocator,
		),
		concatenates = make(
			[dynamic]Inferred_Concatenate_Update,
			0,
			len(unit.concatenate_lines_of_sites),
			allocator,
		),
	}
}

infer_unit_semantic_facts :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	allocator: mem.Allocator,
) -> Inferred_Unit_Facts {
	unit := &project.units[unit_index]
	out := inferred_unit_facts_make(unit, allocator)
	range_facts := range_type_fact_index_make(project, unit_index, allocator)
	defer delete(range_facts.facts)
	inline_symbols := inline_symbol_index_make(unit, allocator)
	defer delete(inline_symbols.symbols)

	for ref in unit.references {
		if ref.namespace != .Value {
			continue
		}
		fact := unknown_type_fact()
		if ref.has_resolution && ref.resolution.kind == .Symbol {
			fact = type_fact_from_symbol_handle(project, unit_index, ref.resolution.symbol)
		}
		push_expression_fact(&out.expression_facts, ref.scope, ref.range, .Reference, fact)
	}

	for access in unit.field_accesses {
		if access.in_type_position {
			continue
		}
		if fact, ok := resolve_field_access_tail(project, lookup, unit_index, access); ok {
			range := field_access_range(access)
			push_expression_fact(&out.expression_facts, access.scope, range, .Selector, fact)
		}
	}

	for site in unit.call_sites {
		fact := call_result_type_fact(project, lookup, unit_index, site)
		push_expression_fact(&out.expression_facts, site.scope, site.range, .Call_Result, fact)
		for arg in site.arguments {
			if !type_fact_known(arg.type_fact) {
				continue
			}
			target := Value_Flow_Target_Data {
				kind = .Call_Parameter,
				range = arg.range,
				call_range = site.range,
				target = site.target,
				parameter_name = arg.name,
			}
			append(
				&out.value_flow_edges,
				Value_Flow_Edge_Data {
					scope = site.scope,
					kind = .Call_Argument,
					source_range = arg.range,
					source_type = arg.type_fact,
					target = target,
					target_type = unknown_type_fact(),
				},
			)
		}
	}

	for assignment, i in unit.assignment_sites {
		lhs := assignment.lhs
		rhs := assignment.rhs
		if .Has_Lhs_Target_Access in assignment.flags {
			if fact, ok := type_fact_for_access(project, lookup, unit_index, assignment.lhs_target_access);
			   ok {
				lhs = fact
			}
		} else if fact, ok := type_fact_for_range_indexed(&range_facts, assignment.lhs_range);
		          ok {
			lhs = fact
		}
		if fact, ok := type_fact_for_range_indexed(&range_facts, assignment.rhs_range); ok {
			rhs = fact
		}
		append(&out.assignments, Inferred_Assignment_Update{index = i, lhs = lhs, rhs = rhs})
		append(
			&out.value_flow_edges,
			Value_Flow_Edge_Data {
				scope = assignment.scope,
				kind = .Assignment,
				source_range = assignment.rhs_range,
				source_type = rhs,
				target = Value_Flow_Target_Data {
					kind = .Assignment,
					range = assignment.lhs_range,
				},
				target_type = lhs,
			},
		)
		if symbol_id, ok := inline_symbol_at_range_indexed(&inline_symbols, assignment.lhs_range);
		   ok && type_fact_known(rhs) {
			append(
				&out.symbol_updates,
				Inferred_Symbol_Type_Update {
					symbol = symbol_id,
					type_fact = rhs,
					overwrite_existing = false,
				},
			)
		}
	}

	for site, i in unit.concatenate_lines_of_sites {
		if fact, ok := type_fact_for_range_indexed(&range_facts, site.source_range); ok {
			append(&out.concatenates, Inferred_Concatenate_Update{index = i, source = fact})
		}
	}

	return out
}

apply_inferred_project_facts :: proc(
	project: ^Project_Analysis,
	inferred: []Inferred_Unit_Facts,
) -> bool {
	rerun := false
	for facts, unit_index in inferred {
		if unit_index >= len(project.units) {
			continue
		}
		unit := &project.units[unit_index]
		unit.expression_facts = facts.expression_facts
		unit.value_flow_edges = facts.value_flow_edges
		for update in facts.symbol_updates {
			idx := symbol_id_index(update.symbol)
			if idx < 0 || idx >= len(unit.symbols) {
				continue
			}
			s := &unit.symbols[idx]
			if update.overwrite_existing || !s.has_declared_type {
				rerun = rerun || s.structure != update.type_fact.structure ||
				        s.has_declared_type != update.type_fact.has_declared_type ||
				        !field_type_refs_equal(s.declared_type, update.type_fact.declared_type)
				if update.type_fact.structure != INVALID_STRUCTURE_ID {
					s.structure = update.type_fact.structure
				}
				if update.type_fact.has_declared_type {
					s.declared_type = update.type_fact.declared_type
					s.has_declared_type = true
					s.type_clause_display = update.type_fact.type_clause_display
				}
			}
		}
		for update in facts.assignments {
			if update.index >= 0 && update.index < len(unit.assignment_sites) {
				unit.assignment_sites[update.index].lhs = update.lhs
				unit.assignment_sites[update.index].rhs = update.rhs
			}
		}
		for update in facts.concatenates {
			if update.index >= 0 && update.index < len(unit.concatenate_lines_of_sites) {
				unit.concatenate_lines_of_sites[update.index].source = update.source
			}
		}
	}
	return rerun
}

field_type_refs_equal :: proc(a, b: Field_Type_Ref_Data) -> bool {
	if a.namespace != b.namespace ||
	   a.is_ref != b.is_ref ||
	   a.base_name != b.base_name ||
	   len(a.field_path) != len(b.field_path) {
		return false
	}
	for i in 0 ..< len(a.field_path) {
		if a.field_path[i] != b.field_path[i] {
			return false
		}
	}
	return true
}

type_fact_known :: proc(fact: Type_Fact_Data) -> bool {
	return fact.structure != INVALID_STRUCTURE_ID ||
	       fact.has_declared_type ||
	       fact.type_clause_display != "" ||
	       fact.table_line != nil
}

range_type_fact_index_make :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	allocator: mem.Allocator,
) -> Range_Type_Fact_Index {
	unit := &project.units[unit_index]
	index := Range_Type_Fact_Index {
		facts = make(
			[dynamic]Range_Type_Fact,
			0,
			len(unit.references) + len(unit.expression_facts),
			allocator,
		),
	}
	for ref in unit.references {
		if ref.namespace != .Value || !ref.has_resolution || ref.resolution.kind != .Symbol {
			continue
		}
		fact := type_fact_from_symbol_handle(project, unit_index, ref.resolution.symbol)
		if type_fact_known(fact) {
			append(&index.facts, Range_Type_Fact{range = ref.range, type_fact = fact})
		}
	}
	for fact in unit.expression_facts {
		if type_fact_known(fact.type_fact) {
			append(
				&index.facts,
				Range_Type_Fact{range = fact.range, type_fact = fact.type_fact},
			)
		}
	}
	slice.sort_by(index.facts[:], range_type_fact_less)
	return index
}

range_type_fact_less :: proc(a, b: Range_Type_Fact) -> bool {
	if a.range.start != b.range.start {
		return a.range.start < b.range.start
	}
	return a.range.end > b.range.end
}

inline_symbol_index_make :: proc(
	unit: ^Unit_Analysis,
	allocator: mem.Allocator,
) -> Inline_Symbol_Index {
	index := Inline_Symbol_Index {
		symbols = make([dynamic]Inline_Symbol_Range, 0, len(unit.symbols), allocator),
	}
	for s in unit.symbols {
		if s.kind == .Variable || s.kind == .Field_Symbol {
			append(&index.symbols, Inline_Symbol_Range{range = s.decl_range, symbol = s.id})
		}
	}
	slice.sort_by(index.symbols[:], inline_symbol_range_less)
	return index
}

inline_symbol_range_less :: proc(a, b: Inline_Symbol_Range) -> bool {
	if a.range.start != b.range.start {
		return a.range.start < b.range.start
	}
	return a.range.end < b.range.end
}

type_fact_from_symbol_handle :: proc(
	project: ^Project_Analysis,
	site_unit_index: int,
	handle: Symbol_Handle,
) -> Type_Fact_Data {
	unit_index := unit_id_index(handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return unknown_type_fact()
	}
	s := symbol(&project.units[unit_index], handle.symbol)
	if s == nil {
		return unknown_type_fact()
	}
	return Type_Fact_Data {
		structure = s.structure if unit_index == site_unit_index else INVALID_STRUCTURE_ID,
		declared_type = s.declared_type,
		has_declared_type = s.has_declared_type,
		type_clause_display = s.type_clause_display,
	}
}

type_fact_for_access :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	access: Field_Access,
) -> (Type_Fact_Data, bool) {
	return resolve_field_access_tail(project, lookup, unit_index, access)
}

type_fact_for_range_indexed :: proc(
	index: ^Range_Type_Fact_Index,
	range: tokenizer.Range,
) -> (Type_Fact_Data, bool) {
	if !range_valid(range) {
		return unknown_type_fact(), false
	}
	best := unknown_type_fact()
	best_width := 0
	found := false
	for i := range_type_fact_lower_bound(index.facts[:], range.start);
	    i < len(index.facts) && index.facts[i].range.start <= range.end;
	    i += 1 {
		fact := index.facts[i]
		if fact.range.end > range.end || !type_fact_known(fact.type_fact) {
			continue
		}
		width := fact.range.end - fact.range.start
		if !found || width > best_width {
			best = fact.type_fact
			best_width = width
			found = true
		}
	}
	if found {
		return best, true
	}
	return unknown_type_fact(), false
}

range_type_fact_lower_bound :: proc(facts: []Range_Type_Fact, start: int) -> int {
	left, right := 0, len(facts)
	for left < right {
		mid := int(uint(left + right) >> 1)
		if facts[mid].range.start < start {
			left = mid + 1
		} else {
			right = mid
		}
	}
	return left
}

call_result_type_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	site: Call_Site_Data,
) -> Type_Fact_Data {
	if site.target.kind != .Method || site.target.method_name == "" {
		return unknown_type_fact()
	}
	class_handle, ok := class_handle_for_call_target(project, lookup, unit_index, site)
	if !ok {
		return unknown_type_fact()
	}
	member, member_ok := class_member_in_hierarchy(project, lookup, class_handle, site.target.method_name, false)
	if !member_ok || member.kind != .Method {
		return unknown_type_fact()
	}
	return class_member_type_fact(member)
}

inline_symbol_at_range_indexed :: proc(
	index: ^Inline_Symbol_Index,
	range: tokenizer.Range,
) -> (Symbol_Id, bool) {
	for i := inline_symbol_lower_bound(index.symbols[:], range.start);
	    i < len(index.symbols) && index.symbols[i].range.start == range.start;
	    i += 1 {
		if index.symbols[i].range == range {
			return index.symbols[i].symbol, true
		}
	}
	return INVALID_SYMBOL_ID, false
}

inline_symbol_lower_bound :: proc(symbols: []Inline_Symbol_Range, start: int) -> int {
	left, right := 0, len(symbols)
	for left < right {
		mid := int(uint(left + right) >> 1)
		if symbols[mid].range.start < start {
			left = mid + 1
		} else {
			right = mid
		}
	}
	return left
}

field_access_range :: proc(access: Field_Access) -> tokenizer.Range {
	out := access.base_range
	for segment in access.field_path {
		if !range_valid(out) {
			out = segment.range
		} else {
			if segment.range.start < out.start {out.start = segment.range.start}
			if segment.range.end > out.end {out.end = segment.range.end}
		}
	}
	return out
}

push_expression_fact :: proc(
	facts: ^[dynamic]Expression_Fact_Data,
	scope_id: Scope_Id,
	range: tokenizer.Range,
	kind: Expression_Fact_Kind,
	type_fact: Type_Fact_Data,
) {
	append(
		facts,
		Expression_Fact_Data{scope = scope_id, range = range, kind = kind, type_fact = type_fact},
	)
}
