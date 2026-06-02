#+private
package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

import "core:mem"
import "core:slice"
import "core:strings"

Inferred_Symbol_Type_Update :: struct {
	symbol:             Symbol_Id,
	type_fact:          Type_Fact_Data,
	overwrite_existing: bool,
	sql_star_query_id:  int,
	sql_star_name:      string,
	is_sql_star:        bool,
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
	rank:      int,
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
	operands:         [dynamic]Operand_Data,
	symbol_updates:   [dynamic]Inferred_Symbol_Type_Update,
	assignments:      [dynamic]Inferred_Assignment_Update,
	concatenates:     [dynamic]Inferred_Concatenate_Update,
}

inferred_unit_facts_destroy_updates :: proc(facts: ^Inferred_Unit_Facts) {
	delete(facts.symbol_updates)
	delete(facts.assignments)
	delete(facts.concatenates)
}

inferred_unit_facts_make :: proc(unit: ^Unit_Analysis, allocator: mem.Allocator) -> Inferred_Unit_Facts {
	return Inferred_Unit_Facts {
		expression_facts = make(
			[dynamic]Expression_Fact_Data,
			0,
			len(unit.references) + len(unit.field_accesses) + len(unit.table_exprs) + len(unit.call_sites),
			allocator,
		),
		operands = make(
			[dynamic]Operand_Data,
			0,
			len(unit.operands) + len(unit.references) + len(unit.field_accesses) + len(unit.table_exprs) +
				len(unit.call_sites) +
				len(unit.assignment_sites) * 2,
			allocator,
		),
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
	lookup: ^Project_Index,
	unit_index: int,
	allocator: mem.Allocator,
) -> Inferred_Unit_Facts {
	unit := &project.units[unit_index]
	out := inferred_unit_facts_make(unit, allocator)
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	inline_symbols := inline_symbol_index_make(unit, context.temp_allocator)

	for operand in unit.operands {
		if !(.Syntax in operand.flags) {
			continue
		}
		next := operand
		if next.has_symbol && next.symbol.unit == unit.unit_id {
			if symbol(unit, next.symbol.symbol) != nil {
				next.type_fact = type_fact_from_symbol_handle(project, unit_index, next.symbol)
			}
		}
		append(&out.operands, next)
	}

	for ref in unit.references {
		if ref.namespace != .Value {
			append(&out.operands, reference_operand(project, unit_index, ref))
			continue
		}
		fact := unknown_type_fact()
		if ref.has_resolution && ref.resolution.kind == .Symbol {
			fact = type_fact_from_symbol_handle(project, unit_index, ref.resolution.symbol)
		}
		push_expression_fact(&out.expression_facts, ref.scope, ref.range, .Reference, fact)
		append(&out.operands, reference_operand(project, unit_index, ref))
	}

	for access in unit.field_accesses {
		if access.in_type_position {
			continue
		}
		if fact, ok := resolve_field_access_tail(project, lookup, unit_index, access); ok {
			if !field_access_fact_is_high_confidence(project, lookup, unit_index, access, fact) {
				fact = type_fact_with_confidence(fact, .Low)
			}
			range := field_access_range(access)
			push_expression_fact(&out.expression_facts, access.scope, range, .Selector, fact)
			push_operand(&out.operands, access.scope, range, .Field, fact, assignable = true)
		}
	}

	for site in unit.table_exprs {
		if fact, ok := table_expr_source_fact(project, lookup, unit_index, site.table_access);
		   ok {
			if row, row_ok := typecheck_table_row_fact(project, fact); row_ok {
				push_expression_fact(&out.expression_facts, site.scope, site.range, .Selector, row)
				push_operand(&out.operands, site.scope, site.range, .Value, row)
			}
		}
	}

	for site in unit.call_sites {
		fact := call_result_type_fact(project, lookup, unit_index, site)
		push_expression_fact(&out.expression_facts, site.scope, site.range, .Call_Result, fact)
		if type_fact_is_known(fact) {
			push_operand(&out.operands, site.scope, site.range, .Value, fact)
		}
		signature, signature_ok := typecheck_call_signature(project, lookup, unit_index, site)
		if !signature_ok || signature.info == nil {
			continue
		}
		for arg, arg_index in site.arguments {
			if !typecheck_argument_requires_writable(site.target.kind, arg.section) {
				continue
			}
			symbol_id, symbol_ok := inline_symbol_at_range_indexed(&inline_symbols, arg.value_range)
			if !symbol_ok {
				continue
			}
			param, param_ok := typecheck_call_parameter(signature.info, site.target.kind, site, arg_index)
			if !param_ok {
				continue
			}
			param_fact := typecheck_parameter_fact(project, lookup, signature.unit_index, signature.info, param^)
			if type_fact_known(param_fact) {
				append(&out.symbol_updates, Inferred_Symbol_Type_Update{symbol = symbol_id, type_fact = param_fact})
			}
		}
	}

	range_facts := range_type_fact_index_make(
		project,
		unit_index,
		out.expression_facts[:],
		context.temp_allocator,
	)

	for target in unit.sql_targets {
		if !(.Is_Table in target.flags && .Is_Inline in target.flags) {
			continue
		}
		symbol_id, symbol_ok := inline_symbol_at_range_indexed(&inline_symbols, target.target_range)
		s := symbol(unit, symbol_id) if symbol_ok else nil
		if s == nil || s.structure != INVALID_STRUCTURE_ID {
			continue
		}
		if open_sql_star_table_target(unit, target.query_id) {
			append(
				&out.symbol_updates,
				Inferred_Symbol_Type_Update {
					symbol            = symbol_id,
					sql_star_query_id = target.query_id,
					sql_star_name     = target.target_name,
					is_sql_star       = true,
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
		if .Assigns_Table_Line in assignment.flags {
			if row, ok := typecheck_table_row_fact(project, rhs); ok {
				rhs = row
			} else {
				rhs = unknown_type_fact()
			}
		}
		if assignment.lhs_range.end > assignment.lhs_range.start {
			push_operand(&out.operands, assignment.scope, assignment.lhs_range, .Variable, lhs, assignable = true)
		}
		if assignment.rhs_range.end > assignment.rhs_range.start {
			push_operand(&out.operands, assignment.scope, assignment.rhs_range, .Value, rhs)
		}
		append(&out.assignments, Inferred_Assignment_Update{index = i, lhs = lhs, rhs = rhs})
		if symbol_id, ok := inline_symbol_at_range_indexed(&inline_symbols, assignment.lhs_range);
		   ok && type_fact_known(rhs) {
			append(
				&out.symbol_updates,
				Inferred_Symbol_Type_Update {
					symbol = symbol_id,
					type_fact = rhs,
					overwrite_existing = rhs.has_declared_type,
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

table_expr_source_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	access: Field_Access,
) -> (Type_Fact_Data, bool) {
	if len(access.field_path) == 0 {
		if base, ok := value_handle_for_name(project, lookup, unit_index, access.scope, access.base_name); ok {
			return type_fact_from_symbol_handle(project, unit_index, base), true
		}
	}
	return type_fact_for_access(project, lookup, unit_index, access)
}

open_sql_star_table_target :: proc(unit: ^Unit_Analysis, query_id: int) -> bool {
	count := 0
	for projection in unit.sql_projections {
		if projection.query_id != query_id {
			continue
		}
		if !(projection.kind == .Star || projection.kind == .Qualified_Star) {
			return false
		}
		count += 1
	}
	return count > 0
}

field_access_fact_is_high_confidence :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	access: Field_Access,
	fact: Type_Fact_Data,
) -> bool {
	if !type_fact_is_high_confidence(fact) {
		return false
	}
	arrow_index := -1
	for segment, i in access.field_path {
		if segment.selector == .Arrow {
			if arrow_index >= 0 {
				return false
			}
			arrow_index = i
		}
	}
	if arrow_index < 0 {
		return true
	}
	if arrow_index != 0 {
		return false
	}
	for segment, i in access.field_path {
		if segment.deref {
			return false
		}
		if i == 0 {
			if segment.selector != .Arrow {
				return false
			}
		} else if segment.selector != .Dash {
			return false
		}
	}
	class_handle, ok := class_handle_for_field_access_base(project, lookup, unit_index, access)
	if !ok {
		return false
	}
	member, _, member_ok := class_member_for_path_segment(
		project,
		lookup,
		class_handle,
		access.field_path[0],
		unit_index,
		access.scope,
	)
	if !member_ok {
		return false
	}
	member_unit_index := unit_id_index(member.unit)
	if member_unit_index < 0 || member_unit_index >= len(project.units) {
		return false
	}
	info := entity_decl_info(&project.units[member_unit_index], member.symbol)
	return info != nil && info.member_kind == .Attribute
}

class_handle_for_field_access_base :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	access: Field_Access,
) -> (Symbol_Handle, bool) {
	if access.base_namespace == .Type {
		return resolve_type_name_in_project_lookup(project, lookup, unit_index, access.base_name)
	}
	if access.base_name == "super" {
		class_symbol, ok := enclosing_instance_method_class_owner_unit(&project.units[unit_index], access.scope)
		if !ok {
			return {}, false
		}
		return direct_superclass_handle_lookup(
			project,
			lookup,
			Symbol_Handle{unit = project.units[unit_index].unit_id, symbol = class_symbol},
		)
	}
	base, ok := value_handle_for_name(project, lookup, unit_index, access.scope, access.base_name)
	if !ok {
		return {}, false
	}
	return class_handle_from_symbol(project, lookup, unit_index, base)
}

apply_inferred_project_facts :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	inferred: []Inferred_Unit_Facts,
) -> bool {
	rerun := false
	for &facts, unit_index in inferred {
		unit := &project.units[unit_index]
		delete(unit.expression_facts)
		unit.expression_facts = facts.expression_facts
		delete(unit.operands)
		unit.operands = facts.operands
		for update in facts.symbol_updates {
			idx := symbol_id_index(update.symbol)
			assert(idx >= 0 && idx < len(unit.symbols))
			s := &unit.symbols[idx]
			if update.overwrite_existing || !s.has_declared_type {
				update_structure := symbol_update_structure_for_unit(project, lookup, unit_index, unit, update)
				rerun = rerun || s.structure != update_structure ||
				        s.has_declared_type != update.type_fact.has_declared_type ||
				        !field_type_refs_equal(s.declared_type, update.type_fact.declared_type)
				if update_structure != INVALID_STRUCTURE_ID {
					s.structure = update_structure
				}
				if update.type_fact.has_declared_type {
					s.declared_type = update.type_fact.declared_type
					s.has_declared_type = true
					s.type_clause_display = update.type_fact.type_clause_display
				}
				s.type_id = type_id_from_symbol_data(unit, s)
			}
		}
		for update in facts.assignments {
			assert(update.index >= 0 && update.index < len(unit.assignment_sites))
			unit.assignment_sites[update.index].lhs = update.lhs
			unit.assignment_sites[update.index].rhs = update.rhs
		}
		for update in facts.concatenates {
			assert(update.index >= 0 && update.index < len(unit.concatenate_lines_of_sites))
			unit.concatenate_lines_of_sites[update.index].source = update.source
		}
		inferred_unit_facts_destroy_updates(&facts)
	}
	return rerun
}

apply_inferred_project_facts_for_indices :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	inferred: []Inferred_Unit_Facts,
	indices: []int,
) -> bool {
	rerun := false
	for unit_index, i in indices {
		facts := &inferred[i]
		unit := &project.units[unit_index]
		delete(unit.expression_facts)
		unit.expression_facts = facts.expression_facts
		delete(unit.operands)
		unit.operands = facts.operands
		for update in facts.symbol_updates {
			idx := symbol_id_index(update.symbol)
			assert(idx >= 0 && idx < len(unit.symbols))
			s := &unit.symbols[idx]
			if update.overwrite_existing || !s.has_declared_type {
				update_structure := symbol_update_structure_for_unit(project, lookup, unit_index, unit, update)
				rerun = rerun || s.structure != update_structure ||
				        s.has_declared_type != update.type_fact.has_declared_type ||
				        !field_type_refs_equal(s.declared_type, update.type_fact.declared_type)
				if update_structure != INVALID_STRUCTURE_ID {
					s.structure = update_structure
				}
				if update.type_fact.has_declared_type {
					s.declared_type = update.type_fact.declared_type
					s.has_declared_type = true
					s.type_clause_display = update.type_fact.type_clause_display
				}
				s.type_id = type_id_from_symbol_data(unit, s)
			}
		}
		for update in facts.assignments {
			assert(update.index >= 0 && update.index < len(unit.assignment_sites))
			unit.assignment_sites[update.index].lhs = update.lhs
			unit.assignment_sites[update.index].rhs = update.rhs
		}
		for update in facts.concatenates {
			assert(update.index >= 0 && update.index < len(unit.concatenate_lines_of_sites))
			unit.concatenate_lines_of_sites[update.index].source = update.source
		}
		inferred_unit_facts_destroy_updates(facts)
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
		a_deref := i < len(a.field_derefs) && a.field_derefs[i]
		b_deref := i < len(b.field_derefs) && b.field_derefs[i]
		if a_deref != b_deref {
			return false
		}
		a_selector := a.field_selectors[i] if i < len(a.field_selectors) else ast.Selector_Op.Dash
		b_selector := b.field_selectors[i] if i < len(b.field_selectors) else ast.Selector_Op.Dash
		if a_selector != b_selector {
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

type_fact_local_structure :: proc(fact: Type_Fact_Data, unit_id: Unit_Id) -> Structure_Id {
	if fact.structure == INVALID_STRUCTURE_ID {
		return INVALID_STRUCTURE_ID
	}
	if fact.structure_unit == INVALID_UNIT_ID || fact.structure_unit == unit_id {
		return fact.structure
	}
	return INVALID_STRUCTURE_ID
}

symbol_update_structure_for_unit :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	unit: ^Unit_Analysis,
	update: Inferred_Symbol_Type_Update,
) -> Structure_Id {
	if update.is_sql_star {
		return open_sql_star_table_target_structure_for_unit(
			project,
			lookup,
			unit_index,
			unit,
			update.sql_star_query_id,
			update.sql_star_name,
		)
	}
	return type_fact_structure_for_unit(project, unit, update.type_fact)
}

open_sql_star_table_target_structure_for_unit :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	unit: ^Unit_Analysis,
	query_id: int,
	target_name: string,
) -> Structure_Id {
	source_count, single, ok := open_sql_star_source_count(project, lookup, unit_index, query_id)
	if !ok {
		return INVALID_STRUCTURE_ID
	}
	if source_count == 1 {
		return type_fact_structure_for_unit(project, unit, single)
	}
	name := open_sql_star_structure_name(target_name)
	if st := find_structure(unit, name); st != nil {
		return st.id
	}
	fields := make([dynamic]Structure_Field_Data, 0, 8, context.allocator)
	for projection in unit.sql_projections {
		if projection.query_id != query_id {
			continue
		}
		for source in unit.sql_sources {
			if !sql_star_projection_selects_source(projection, source) {
				continue
			}
			fact, fact_ok := sql_source_structure_fact(project, lookup, unit_index, source)
			assert(fact_ok)
			source_unit_index := unit_id_index(fact.structure_unit)
			source_structure := structure(&project.units[source_unit_index], fact.structure)
			assert(source_structure != nil)
			for field in source_structure.fields {
				append(&fields, field)
			}
		}
	}
	if len(fields) == 0 {
		return INVALID_STRUCTURE_ID
	}
	return push_structure(unit, name, fields)
}

open_sql_star_source_count :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	query_id: int,
) -> (int, Type_Fact_Data, bool) {
	unit := &project.units[unit_index]
	count := 0
	single := Type_Fact_Data{}
	for projection in unit.sql_projections {
		if projection.query_id != query_id {
			continue
		}
		for source in unit.sql_sources {
			if !sql_star_projection_selects_source(projection, source) {
				continue
			}
			fact, ok := sql_source_structure_fact(project, lookup, unit_index, source)
			if !ok {
				return 0, {}, false
			}
			count += 1
			single = fact
		}
	}
	return count, single, count > 0
}

sql_star_projection_selects_source :: proc(projection: Sql_Projection_Data, source: Sql_Source_Data) -> bool {
	if source.query_id != projection.query_id {
		return false
	}
	if projection.kind == .Star {
		return true
	}
	return projection.kind == .Qualified_Star &&
	       (source.alias == projection.source_alias || source.name == projection.source_alias)
}

sql_source_structure_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	source: Sql_Source_Data,
) -> (Type_Fact_Data, bool) {
	if source.resolution != .External {
		return {}, false
	}
	handle, ok := resolve_type_name_in_project_lookup(project, lookup, unit_index, source.name)
	if !ok {
		return {}, false
	}
	source_unit_index := unit_id_index(handle.unit)
	if source_unit_index < 0 || source_unit_index >= len(project.units) {
		return {}, false
	}
	source_symbol := symbol(&project.units[source_unit_index], handle.symbol)
	if source_symbol == nil || source_symbol.structure == INVALID_STRUCTURE_ID {
		return {}, false
	}
	return Type_Fact_Data {
		structure = source_symbol.structure,
		structure_unit = handle.unit,
		confidence = .High if project.units[source_unit_index].source_mode == .Full else .Low,
	}, true
}

open_sql_star_structure_name :: proc(target_name: string) -> string {
	out := strings.builder_make(context.allocator)
	strings.write_string(&out, "<open_sql_star:")
	strings.write_string(&out, target_name)
	strings.write_byte(&out, '>')
	return strings.to_string(out)
}

type_fact_structure_for_unit :: proc(
	project: ^Project_Analysis,
	unit: ^Unit_Analysis,
	fact: Type_Fact_Data,
) -> Structure_Id {
	local := type_fact_local_structure(fact, unit.unit_id)
	if local != INVALID_STRUCTURE_ID || fact.structure_unit == INVALID_UNIT_ID {
		return local
	}
	source_index := unit_id_index(fact.structure_unit)
	if source_index < 0 || source_index >= len(project.units) {
		return INVALID_STRUCTURE_ID
	}
	source := &project.units[source_index]
	source_structure := structure(source, fact.structure)
	if source_structure == nil {
		return INVALID_STRUCTURE_ID
	}
	for &st in unit.structures {
		if st.origin_unit == source_structure.origin_unit &&
		   st.origin_structure == source_structure.origin_structure {
			return st.id
		}
	}
	fields := make([dynamic]Structure_Field_Data, 0, len(source_structure.fields), context.allocator)
	for &field in source_structure.fields {
		append(&fields, field)
	}
	id := push_structure(unit, source_structure.name, fields)
	st := &unit.structures[structure_id_index(id)]
	st.origin_unit = source_structure.origin_unit
	st.origin_structure = source_structure.origin_structure
	return id
}

range_type_fact_index_make :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	expression_facts: []Expression_Fact_Data,
	allocator: mem.Allocator,
) -> Range_Type_Fact_Index {
	unit := &project.units[unit_index]
	index := Range_Type_Fact_Index {
		facts = make(
			[dynamic]Range_Type_Fact,
			0,
			len(unit.references) + len(unit.expression_facts) + len(expression_facts),
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
			append(&index.facts, Range_Type_Fact{range = fact.range, type_fact = fact.type_fact, rank = 1})
		}
	}
	for fact in expression_facts {
		if type_fact_known(fact.type_fact) {
			append(&index.facts, Range_Type_Fact{range = fact.range, type_fact = fact.type_fact, rank = 2})
		}
	}
	slice.sort_by(index.facts[:], range_type_fact_less)
	return index
}

range_type_fact_less :: proc(a, b: Range_Type_Fact) -> bool {
	if a.range.start != b.range.start {
		return a.range.start < b.range.start
	}
	if a.range.end == b.range.end {
		return a.rank > b.rank
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
	_ = site_unit_index
	unit_index := unit_id_index(handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return unknown_type_fact()
	}
	s := symbol(&project.units[unit_index], handle.symbol)
	if s == nil {
		return unknown_type_fact()
	}
	return Type_Fact_Data {
		type_id = s.type_id,
		type_unit = handle.unit if type_id_is_known(s.type_id) else INVALID_UNIT_ID,
		structure = s.structure,
		structure_unit = handle.unit if s.structure != INVALID_STRUCTURE_ID else INVALID_UNIT_ID,
		declared_type = s.declared_type,
		has_declared_type = s.has_declared_type,
		type_clause_display = s.type_clause_display,
		confidence = .High if project.units[unit_index].source_mode == .Full else .Low,
	}
}

type_fact_for_access :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
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
	lookup: ^Project_Index,
	unit_index: int,
	site: Call_Site_Data,
) -> Type_Fact_Data {
	method_name := site.target.method_name
	class_handle: Symbol_Handle
	ok := false
	#partial switch site.target.kind {
	case .Routine:
		return builtin_routine_result_type_fact(&project.units[unit_index], site.target.routine_name)
	case .Method:
		if method_name == "" {
			return unknown_type_fact()
		}
		class_handle, ok = class_handle_for_call_target(project, lookup, unit_index, site)
	case .Implicit_Method:
		if method_name == "" {
			return unknown_type_fact()
		}
		class_symbol, class_ok := enclosing_class_owner_unit(&project.units[unit_index], site.scope)
		if class_ok {
			class_handle = Symbol_Handle{unit = project.units[unit_index].unit_id, symbol = class_symbol}
			ok = true
		}
	case:
		return unknown_type_fact()
	}
	if !ok {
		return unknown_type_fact()
	}
	if fact, trusted := direct_call_result_type_fact(project, lookup, unit_index, site, class_handle, method_name);
	   trusted {
		return fact
	}
	member, member_unit_index, member_ok := class_member_in_hierarchy_with_unit(
		project,
		lookup,
		class_handle,
		site.target.method_name,
		false,
		unit_index,
		site.scope,
	)
	member_info := entity_decl_info(&project.units[member_unit_index], member.symbol) if member_ok else nil
	if !member_ok || member_info == nil || member_info.member_kind != .Method {
		return unknown_type_fact()
	}
	if fact, fact_ok := method_signature_result_type_fact(
		project,
		lookup,
		member,
		member_unit_index,
		member_info,
		.Low,
	); fact_ok {
		return fact
	}
	return type_fact_with_confidence(class_member_type_fact(project, member, member_unit_index), .Low)
}

builtin_routine_result_type_fact :: proc(unit: ^Unit_Analysis, name: string) -> Type_Fact_Data {
	spec := builtin_routine_spec(name)
	if spec == nil || spec.return_type == "" {
		return unknown_type_fact()
	}
	return Type_Fact_Data {
		type_id = type_builtin(unit, spec.return_type),
		type_unit = unit.unit_id,
		structure = INVALID_STRUCTURE_ID,
		structure_unit = INVALID_UNIT_ID,
		declared_type = builtin_type_ref(spec.return_type),
		has_declared_type = true,
		confidence = .High,
	}
}

direct_call_result_type_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	site: Call_Site_Data,
	class_handle: Symbol_Handle,
	method_name: string,
) -> (Type_Fact_Data, bool) {
	if site.target.kind == .Method &&
	   (len(site.target.receiver_path) > 0 || site.target.interface_qualified) {
		return unknown_type_fact(), false
	}
	owner := symbol_for_project_handle(project, class_handle)
	if owner == nil || owner.kind != .Class {
		return unknown_type_fact(), false
	}
	member, member_ok := class_member_handle_lookup(project, lookup, class_handle, method_name)
	if !member_ok {
		return unknown_type_fact(), false
	}
	member_unit_index := unit_id_index(member.unit)
	if member_unit_index < 0 ||
	   member_unit_index >= len(project.units) ||
	   project.units[member_unit_index].source_mode != .Full {
		return unknown_type_fact(), false
	}
	info := entity_decl_info(&project.units[member_unit_index], member.symbol)
	if info == nil || info.member_kind != .Method || .Is_Redefinition in info.flags {
		return unknown_type_fact(), false
	}
	return method_signature_result_type_fact(project, lookup, member, member_unit_index, info, .High)
}

method_signature_result_type_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	member: Symbol_Handle,
	member_unit_index: int,
	info: ^Decl_Info_Data,
	confidence: Type_Fact_Confidence,
) -> (Type_Fact_Data, bool) {
	signature := member
	signature_unit_index := member_unit_index
	signature_info := info
	if info.effective_signature.unit != INVALID_UNIT_ID &&
	   info.effective_signature.symbol != INVALID_SYMBOL_ID {
		unit_index := unit_id_index(info.effective_signature.unit)
		if unit_index >= 0 && unit_index < len(project.units) {
			if effective := entity_decl_info(&project.units[unit_index], info.effective_signature.symbol);
			   effective != nil {
				signature = info.effective_signature
				signature_unit_index = unit_index
				signature_info = effective
			}
		}
	}
	for param in signature_info.signature_parameters {
		if param.section != .Method_Returning && param.section != .Method_Receiving {
			continue
		}
		fact := typecheck_parameter_fact(project, lookup, signature_unit_index, signature_info, param)
		impl_scope, impl_unit_index, has_impl := method_implementation_scope(
			project,
			signature,
			signature_unit_index,
			signature_info,
		)
		if inferred, inferred_ok := method_return_assignment_type_fact(
			project,
			impl_unit_index if has_impl else signature_unit_index,
			impl_scope if has_impl else signature_info.body_scope,
			param,
		); inferred_ok &&
		   (!type_fact_has_resolved_shape(project, fact) ||
		    type_fact_has_richer_structure(project, inferred, fact)) {
			return type_fact_with_confidence(inferred, .Low), true
		}
		return type_fact_with_confidence(fact, confidence), type_fact_is_known(fact)
	}
	return unknown_type_fact(), false
}

method_implementation_scope :: proc(
	project: ^Project_Analysis,
	signature: Symbol_Handle,
	signature_unit_index: int,
	signature_info: ^Decl_Info_Data,
) -> (Scope_Id, int, bool) {
	if signature_info.body_scope != INVALID_SCOPE_ID {
		return signature_info.body_scope, signature_unit_index, true
	}
	if signature_info.implementation_unit != INVALID_UNIT_ID {
		unit_index := unit_id_index(signature_info.implementation_unit)
		if scope, ok := method_implementation_scope_in_unit(
			project,
			unit_index,
			signature,
			signature_info.implementation_range,
		); ok {
			return scope, unit_index, true
		}
	}
	if scope, ok := method_implementation_scope_in_unit(
		project,
		signature_unit_index,
		signature,
		signature_info.implementation_range,
	); ok {
		return scope, signature_unit_index, true
	}
	return INVALID_SCOPE_ID, -1, false
}

method_implementation_scope_in_unit :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	signature: Symbol_Handle,
	implementation_range: tokenizer.Range,
) -> (Scope_Id, bool) {
	if unit_index < 0 || unit_index >= len(project.units) {
		return INVALID_SCOPE_ID, false
	}
	unit := &project.units[unit_index]
	for &info in unit.decl_infos {
		if info.body_scope != INVALID_SCOPE_ID &&
		   info.kind == .Method &&
		   (info.effective_signature == signature ||
		    (range_valid(implementation_range) &&
		     info.decl_range == implementation_range)) {
			return info.body_scope, true
		}
	}
	return INVALID_SCOPE_ID, false
}

type_fact_has_resolved_shape :: proc(project: ^Project_Analysis, fact: Type_Fact_Data) -> bool {
	if fact.structure != INVALID_STRUCTURE_ID {
		return true
	}
	if t := typecheck_type_data(project, fact); t != nil {
		if t.kind == .Table {
			row, ok := typecheck_table_row_fact(project, fact)
			return ok && type_fact_has_resolved_shape(project, row)
		}
		return t.kind != .Unknown
	}
	return false
}

type_fact_has_richer_structure :: proc(project: ^Project_Analysis, a, b: Type_Fact_Data) -> bool {
	a_count, a_ok := type_fact_structure_field_count(project, a)
	b_count, b_ok := type_fact_structure_field_count(project, b)
	return a_ok && (!b_ok || a_count > b_count)
}

type_fact_structure_field_count :: proc(project: ^Project_Analysis, fact: Type_Fact_Data) -> (int, bool) {
	structure_unit := fact.structure_unit
	structure_id := fact.structure
	if structure_id == INVALID_STRUCTURE_ID {
		if row, ok := typecheck_table_row_fact(project, fact); ok {
			structure_unit = row.structure_unit
			structure_id = row.structure
		}
	}
	unit_index := unit_id_index(structure_unit)
	if structure_id == INVALID_STRUCTURE_ID || unit_index < 0 || unit_index >= len(project.units) {
		return 0, false
	}
	st := structure(&project.units[unit_index], structure_id)
	if st == nil {
		return 0, false
	}
	return len(st.fields), true
}

method_return_assignment_type_fact :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	body_scope: Scope_Id,
	param: Decl_Signature_Parameter_Data,
) -> (Type_Fact_Data, bool) {
	if param.name == "" ||
	   body_scope == INVALID_SCOPE_ID ||
	   unit_index < 0 ||
	   unit_index >= len(project.units) {
		return {}, false
	}
	unit := &project.units[unit_index]
	out := Type_Fact_Data{}
	found := false
	for site in unit.assignment_sites {
		if !(.Has_Lhs_Target_Access in site.flags) ||
		   len(site.lhs_target_access.field_path) > 0 ||
		   site.lhs_target_access.base_name != param.name ||
		   !scope_is_or_child(unit, site.scope, body_scope) ||
		   !type_fact_has_resolved_shape(project, site.rhs) {
			continue
		}
		if found &&
		   (out.type_id != site.rhs.type_id ||
		    out.type_unit != site.rhs.type_unit ||
		    out.structure != site.rhs.structure ||
		    out.structure_unit != site.rhs.structure_unit) {
			return {}, false
		}
		out = site.rhs
		found = true
	}
	return out, found
}

scope_is_or_child :: proc(unit: ^Unit_Analysis, scope_id, parent: Scope_Id) -> bool {
	for current := scope_id; current != INVALID_SCOPE_ID; {
		if current == parent {
			return true
		}
		s := scope(unit, current)
		if s == nil {
			break
		}
		current = s.parent
	}
	return false
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

push_operand :: proc(
	operands: ^[dynamic]Operand_Data,
	scope: Scope_Id,
	range: tokenizer.Range,
	mode: Operand_Mode,
	type_fact: Type_Fact_Data,
	symbol := Symbol_Handle{},
	has_symbol := false,
	assignable := false,
) {
	flags := Operand_Flags{}
	if assignable {
		flags += {.Assignable}
	}
	append(
		operands,
		Operand_Data {
			scope = scope,
			range = range,
			mode = mode,
			type_fact = type_fact,
			symbol = symbol,
			has_symbol = has_symbol,
			flags = flags,
		},
	)
}
