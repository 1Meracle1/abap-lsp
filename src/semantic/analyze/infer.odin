#+private
package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

import "core:mem"
import "core:slice"
import "core:strings"
import base_runtime "base:runtime"

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
	symbol_updates: [dynamic]Inferred_Symbol_Type_Update,
	assignments:    [dynamic]Inferred_Assignment_Update,
	concatenates:   [dynamic]Inferred_Concatenate_Update,
}

Range_Type_Fact_Ast_Walker :: struct {
	unit:  ^Source_File_Provider,
	facts: ^[dynamic]Range_Type_Fact,
}

inferred_unit_facts_destroy_updates :: proc(facts: ^Inferred_Unit_Facts) {
	delete(facts.symbol_updates)
	delete(facts.assignments)
	delete(facts.concatenates)
}

inferred_unit_facts_make :: proc(unit: ^Source_File_Provider, update_allocator: mem.Allocator) -> Inferred_Unit_Facts {
	return Inferred_Unit_Facts {
		symbol_updates = make(
			[dynamic]Inferred_Symbol_Type_Update,
			0,
			len(unit.assignment_sites),
			update_allocator,
		),
		assignments = make(
			[dynamic]Inferred_Assignment_Update,
			0,
			len(unit.assignment_sites),
			update_allocator,
		),
		concatenates = make(
			[dynamic]Inferred_Concatenate_Update,
			0,
			len(unit.concatenate_lines_of_sites),
			update_allocator,
		),
	}
}

infer_unit_semantic_facts :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	allocator: mem.Allocator,
) -> Inferred_Unit_Facts {
	_ = allocator
	unit := &project.providers.source_files[source_file_index]
	out := inferred_unit_facts_make(unit, base_runtime.heap_allocator())
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	extra_range_facts := make(
		[dynamic]Range_Type_Fact,
		0,
		len(unit.references) + len(unit.field_accesses) + len(unit.table_exprs) + len(unit.call_sites),
		context.temp_allocator,
	)
	inline_symbols := inline_symbol_index_make(unit, context.temp_allocator)

	for &ref in unit.references {
		if ref.namespace != .Value {
			_ = reference_operand(project, source_file_index, ref)
			continue
		}
		fact := unknown_type_fact()
		if ref.has_resolution && ref.resolution.kind == .Symbol {
			fact = type_fact_from_symbol_handle(project, source_file_index, ref.resolution.symbol)
		}
		append_extra_range_type_fact(&extra_range_facts, ref.range, fact)
		_ = reference_operand(project, source_file_index, ref)
	}

	for &access in unit.field_accesses {
		if access.in_type_position {
			continue
		}
		if fact, ok := resolve_field_access_tail(project, lookup, source_file_index, access); ok {
			if !field_access_fact_is_high_confidence(project, lookup, source_file_index, access, fact) {
				fact = type_fact_with_confidence(fact, .Low)
			}
			if access.node != nil {
				add_type_and_value(unit, access.node, access.scope, .Field, fact, assignable = true)
			}
			append_extra_range_type_fact(&extra_range_facts, field_access_range(access), fact)
		}
	}

	for &site in unit.table_exprs {
		if fact, ok := table_expr_source_fact(project, lookup, source_file_index, site.table_access);
		   ok {
			if row, row_ok := typecheck_table_row_fact(project, fact); row_ok {
				if site.node != nil {
					add_type_and_value(unit, site.node, site.scope, .Value, row)
				}
				append_extra_range_type_fact(&extra_range_facts, site.range, row)
			}
		}
	}

	for &site in unit.call_sites {
		fact := call_result_type_fact(project, lookup, source_file_index, site)
		if site.node != nil {
			add_type_and_value(unit, site.node, site.scope, .Value, fact)
		}
		append_extra_range_type_fact(&extra_range_facts, site.range, fact)
		signature, signature_ok := typecheck_call_signature(project, lookup, source_file_index, site)
		if !signature_ok || signature.info == nil {
			continue
		}
		for &arg, arg_index in site.arguments {
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
			param_fact := typecheck_parameter_fact(project, lookup, signature.source_file_index, signature.info, param^)
			if type_fact_known(param_fact) {
				append(&out.symbol_updates, Inferred_Symbol_Type_Update{symbol = symbol_id, type_fact = param_fact})
			}
		}
	}

	range_facts := range_type_fact_index_make(
		project,
		source_file_index,
		extra_range_facts[:],
		context.temp_allocator,
	)

	for &target in unit.sql_targets {
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

	for &assignment, i in unit.assignment_sites {
		lhs := assignment.lhs
		rhs := assignment.rhs
		if .Has_Lhs_Target_Access in assignment.flags {
			if fact, ok := type_fact_for_access(project, lookup, source_file_index, assignment.lhs_target_access);
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

	for &site, i in unit.concatenate_lines_of_sites {
		if fact, ok := type_fact_for_range_indexed(&range_facts, site.source_range); ok {
			append(&out.concatenates, Inferred_Concatenate_Update{index = i, source = fact})
		}
	}

	return out
}

table_expr_source_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
) -> (Type_Fact_Data, bool) {
	if len(access.field_path) == 0 {
		if base, ok := value_handle_for_name(project, lookup, source_file_index, access.scope, access.base_name); ok {
			return type_fact_from_symbol_handle(project, source_file_index, base), true
		}
	}
	return type_fact_for_access(project, lookup, source_file_index, access)
}

open_sql_star_table_target :: proc(unit: ^Source_File_Provider, query_id: int) -> bool {
	count := 0
	for &projection in unit.sql_projections {
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
	source_file_index: int,
	access: Field_Access,
	fact: Type_Fact_Data,
) -> bool {
	if !type_fact_is_high_confidence(fact) {
		return false
	}
	arrow_index := -1
	for &segment, i in access.field_path {
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
	for &segment, i in access.field_path {
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
	class_handle, ok := class_handle_for_field_access_base(project, lookup, source_file_index, access)
	if !ok {
		return false
	}
	member, _, member_ok := class_member_for_path_segment(
		project,
		lookup,
		class_handle,
		access.field_path[0],
		source_file_index,
		access.scope,
	)
	if !member_ok {
		return false
	}
	member_source_file_index := source_file_id_index(member.unit)
	if member_source_file_index < 0 || member_source_file_index >= len(project.providers.source_files) {
		return false
	}
	info := entity_decl_info(&project.providers.source_files[member_source_file_index], member.symbol)
	return info != nil && info.member_kind == .Attribute
}

class_handle_for_field_access_base :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
) -> (Symbol_Link, bool) {
	if access.base_namespace == .Type {
		return resolve_type_name_in_project_lookup(project, lookup, source_file_index, access.base_name)
	}
	if access.base_name == "super" {
		class_symbol, ok := enclosing_instance_method_class_owner_unit(&project.providers.source_files[source_file_index], access.scope)
		if !ok {
			return {}, false
		}
		return direct_superclass_handle_lookup(
			project,
			lookup,
			Symbol_Link{unit = project.providers.source_files[source_file_index].source_file_id, symbol = class_symbol},
		)
	}
	base, ok := value_handle_for_name(project, lookup, source_file_index, access.scope, access.base_name)
	if !ok {
		return {}, false
	}
	return class_handle_from_symbol(project, lookup, source_file_index, base)
}

apply_inferred_project_facts :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	inferred: []Inferred_Unit_Facts,
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) -> bool {
	rerun := false
	for &facts, source_file_index in inferred {
		unit := &project.providers.source_files[source_file_index]
		unit_alloc := unit_allocator(source_file_allocators, source_file_index, allocator)
		for &update in facts.symbol_updates {
			idx := symbol_id_index(update.symbol)
			assert(idx >= 0 && idx < len(unit.symbols))
			s := &unit.symbols[idx]
			if update.overwrite_existing || !s.has_declared_type {
				update_structure := symbol_update_structure_for_unit(
					project,
					lookup,
					source_file_index,
					unit,
					update,
					unit_alloc,
				)
				rerun = rerun || symbol_type_shape_differs(s, update_structure, update.type_fact)
				symbol_apply_inferred_type_fact(unit, s, update_structure, update.type_fact)
				update_symbol_node_type_and_value(project, source_file_index, s)
			}
		}
		for &update in facts.assignments {
			assert(update.index >= 0 && update.index < len(unit.assignment_sites))
			unit.assignment_sites[update.index].lhs = update.lhs
			unit.assignment_sites[update.index].rhs = update.rhs
		}
		for &update in facts.concatenates {
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
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) -> bool {
	rerun := false
	for source_file_index, i in indices {
		facts := &inferred[i]
		unit := &project.providers.source_files[source_file_index]
		unit_alloc := unit_allocator(source_file_allocators, source_file_index, allocator)
		for &update in facts.symbol_updates {
			idx := symbol_id_index(update.symbol)
			assert(idx >= 0 && idx < len(unit.symbols))
			s := &unit.symbols[idx]
			if update.overwrite_existing || !s.has_declared_type {
				update_structure := symbol_update_structure_for_unit(
					project,
					lookup,
					source_file_index,
					unit,
					update,
					unit_alloc,
				)
				rerun = rerun || symbol_type_shape_differs(s, update_structure, update.type_fact)
				symbol_apply_inferred_type_fact(unit, s, update_structure, update.type_fact)
				update_symbol_node_type_and_value(project, source_file_index, s)
			}
		}
		for &update in facts.assignments {
			assert(update.index >= 0 && update.index < len(unit.assignment_sites))
			unit.assignment_sites[update.index].lhs = update.lhs
			unit.assignment_sites[update.index].rhs = update.rhs
		}
		for &update in facts.concatenates {
			assert(update.index >= 0 && update.index < len(unit.concatenate_lines_of_sites))
			unit.concatenate_lines_of_sites[update.index].source = update.source
		}
		inferred_unit_facts_destroy_updates(facts)
	}
	return rerun
}

update_symbol_node_type_and_value :: proc(
	project: ^Project_Analysis,
	source_file_index: int,
	s: ^Symbol_Data,
) {
	if s == nil || s.node == nil {
		return
	}
	unit := &project.providers.source_files[source_file_index]
	handle := Symbol_Link{unit = unit.source_file_id, symbol = s.id}
	mode, assignable := operand_mode_from_symbol(project, handle)
	fact := type_fact_from_symbol_handle(project, source_file_index, handle)
	add_type_and_value(unit, s.node, s.scope, mode, fact, assignable = assignable)
}

type_fact_known :: proc(fact: Type_Fact_Data) -> bool {
	return fact.structure != INVALID_STRUCTURE_ID ||
	       fact.has_declared_type ||
	       fact.type_clause_display != "" ||
	       fact.table_line != nil
}

type_fact_local_structure :: proc(fact: Type_Fact_Data, source_file_id: Source_File_Id) -> Structure_Id {
	if fact.structure == INVALID_STRUCTURE_ID {
		return INVALID_STRUCTURE_ID
	}
	if fact.structure_unit == INVALID_SOURCE_FILE_ID || fact.structure_unit == source_file_id {
		return fact.structure
	}
	return INVALID_STRUCTURE_ID
}

symbol_update_structure_for_unit :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	unit: ^Source_File_Provider,
	update: Inferred_Symbol_Type_Update,
	allocator: mem.Allocator,
) -> Structure_Id {
	if update.is_sql_star {
		return open_sql_star_table_target_structure_for_unit(
			project,
			lookup,
			source_file_index,
			unit,
			update.sql_star_query_id,
			update.sql_star_name,
			allocator,
		)
	}
	return type_fact_structure_for_unit(project, unit, update.type_fact, allocator)
}

open_sql_star_table_target_structure_for_unit :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	unit: ^Source_File_Provider,
	query_id: int,
	target_name: string,
	allocator: mem.Allocator,
) -> Structure_Id {
	source_count, single, ok := open_sql_star_source_count(project, lookup, source_file_index, query_id)
	if !ok {
		return INVALID_STRUCTURE_ID
	}
	if source_count == 1 {
		return type_fact_structure_for_unit(project, unit, single, allocator)
	}
	name := open_sql_star_structure_name(target_name, context.temp_allocator)
	if st := find_structure(unit, name); st != nil {
		return st.id
	}
	name = strings.clone(name, allocator)
	fields := make([dynamic]Structure_Field_Data, 0, 8, allocator)
	for &projection in unit.sql_projections {
		if projection.query_id != query_id {
			continue
		}
		for &source in unit.sql_sources {
			if !sql_star_projection_selects_source(projection, source) {
				continue
			}
			fact, fact_ok := sql_source_structure_fact(project, lookup, source_file_index, source)
			assert(fact_ok)
			source_source_file_index := source_file_id_index(fact.structure_unit)
			source_structure := structure(&project.providers.source_files[source_source_file_index], fact.structure)
			assert(source_structure != nil)
			for &field in source_structure.fields {
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
	source_file_index: int,
	query_id: int,
) -> (int, Type_Fact_Data, bool) {
	unit := &project.providers.source_files[source_file_index]
	count := 0
	single := Type_Fact_Data{}
	for &projection in unit.sql_projections {
		if projection.query_id != query_id {
			continue
		}
		for &source in unit.sql_sources {
			if !sql_star_projection_selects_source(projection, source) {
				continue
			}
			fact, ok := sql_source_structure_fact(project, lookup, source_file_index, source)
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
	source_file_index: int,
	source: Sql_Source_Data,
) -> (Type_Fact_Data, bool) {
	if source.resolution != .External {
		return {}, false
	}
	handle, ok := resolve_type_name_in_project_lookup(project, lookup, source_file_index, source.name)
	if !ok {
		return {}, false
	}
	source_source_file_index := source_file_id_index(handle.unit)
	if source_source_file_index < 0 || source_source_file_index >= len(project.providers.source_files) {
		return {}, false
	}
	source_symbol := symbol(&project.providers.source_files[source_source_file_index], handle.symbol)
	if source_symbol == nil || source_symbol.structure == INVALID_STRUCTURE_ID {
		return {}, false
	}
	return Type_Fact_Data {
		structure = source_symbol.structure,
		structure_unit = handle.unit,
		confidence = .High if project.providers.source_files[source_source_file_index].role == .Full_Source else .Low,
	}, true
}

open_sql_star_structure_name :: proc(target_name: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "<open_sql_star:")
	strings.write_string(&out, target_name)
	strings.write_byte(&out, '>')
	return strings.to_string(out)
}

type_fact_structure_for_unit :: proc(
	project: ^Project_Analysis,
	unit: ^Source_File_Provider,
	fact: Type_Fact_Data,
	allocator: mem.Allocator,
) -> Structure_Id {
	local := type_fact_local_structure(fact, unit.source_file_id)
	if local != INVALID_STRUCTURE_ID || fact.structure_unit == INVALID_SOURCE_FILE_ID {
		return local
	}
	source_index := source_file_id_index(fact.structure_unit)
	if source_index < 0 || source_index >= len(project.providers.source_files) {
		return INVALID_STRUCTURE_ID
	}
	source := &project.providers.source_files[source_index]
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
	fields := make([dynamic]Structure_Field_Data, 0, len(source_structure.fields), allocator)
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
	source_file_index: int,
	extra_range_facts: []Range_Type_Fact,
	allocator: mem.Allocator,
) -> Range_Type_Fact_Index {
	unit := &project.providers.source_files[source_file_index]
	index := Range_Type_Fact_Index {
		facts = make(
			[dynamic]Range_Type_Fact,
			0,
			len(unit.references) + len(extra_range_facts),
			allocator,
		),
	}
	for &ref in unit.references {
		if ref.namespace != .Value || !ref.has_resolution || ref.resolution.kind != .Symbol {
			continue
		}
		fact := type_fact_from_symbol_handle(project, source_file_index, ref.resolution.symbol)
		if type_fact_known(fact) {
			append(&index.facts, Range_Type_Fact{range = ref.range, type_fact = fact})
		}
	}
	range_type_fact_index_append_ast(unit, &index.facts)
	for fact in extra_range_facts {
		if type_fact_known(fact.type_fact) {
			append(&index.facts, fact)
		}
	}
	slice.sort_by(index.facts[:], range_type_fact_less)
	return index
}

range_type_fact_index_append_ast :: proc(
	unit: ^Source_File_Provider,
	facts: ^[dynamic]Range_Type_Fact,
) {
	if unit == nil || unit.root == nil {
		return
	}
	collector := Range_Type_Fact_Ast_Walker{unit = unit, facts = facts}
	visitor := ast.Visitor{visit = range_type_fact_ast_visit, data = &collector}
	ast.walk(&visitor, unit.root)
}

range_type_fact_ast_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil || !(.Has_Type_And_Value in node.sem.flags) {
		return v
	}
	collector := cast(^Range_Type_Fact_Ast_Walker)v.data
	fact := type_fact_from_type_and_value(node.sem.tav)
	if !type_fact_known(fact) {
		return v
	}
	rank := 1
	if node.sem.tav.mode == .Field || node.sem.tav.mode == .Method {
		rank = 3
	} else if node.sem.tav.mode != .No_Value && node.sem.tav.mode != .Invalid {
		rank = 2
	}
	append(collector.facts, Range_Type_Fact{range = node.range, type_fact = fact, rank = rank})
	return v
}

append_extra_range_type_fact :: proc(
	facts: ^[dynamic]Range_Type_Fact,
	range: tokenizer.Range,
	fact: Type_Fact_Data,
) {
	append(facts, Range_Type_Fact{range = range, type_fact = fact, rank = 2})
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
	unit: ^Source_File_Provider,
	allocator: mem.Allocator,
) -> Inline_Symbol_Index {
	index := Inline_Symbol_Index {
		symbols = make([dynamic]Inline_Symbol_Range, 0, len(unit.symbols), allocator),
	}
	for &s in unit.symbols {
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
	site_source_file_index: int,
	handle: Symbol_Link,
) -> Type_Fact_Data {
	_ = site_source_file_index
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return unknown_type_fact()
	}
	s := symbol(&project.providers.source_files[source_file_index], handle.symbol)
	if s == nil {
		return unknown_type_fact()
	}
	return Type_Fact_Data {
		type_id = s.type_id,
		type_unit = handle.unit if type_id_is_known(s.type_id) else INVALID_SOURCE_FILE_ID,
		structure = s.structure,
		structure_unit = handle.unit if s.structure != INVALID_STRUCTURE_ID else INVALID_SOURCE_FILE_ID,
		declared_type = s.declared_type,
		has_declared_type = s.has_declared_type,
		type_clause_display = s.type_clause_display,
		confidence = .High if project.providers.source_files[source_file_index].role == .Full_Source else .Low,
	}
}

type_fact_for_access :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
) -> (Type_Fact_Data, bool) {
	return resolve_field_access_tail(project, lookup, source_file_index, access)
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
		fact := &index.facts[i]
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
	source_file_index: int,
	site: Call_Site_Data,
) -> Type_Fact_Data {
	method_name := site.target.method_name
	class_handle: Symbol_Link
	ok := false
	#partial switch site.target.kind {
	case .Routine:
		return builtin_routine_result_type_fact(&project.providers.source_files[source_file_index], site.target.routine_name)
	case .Method:
		if method_name == "" {
			return unknown_type_fact()
		}
		class_handle, ok = class_handle_for_call_target(project, lookup, source_file_index, site)
	case .Implicit_Method:
		if method_name == "" {
			return unknown_type_fact()
		}
		class_symbol, class_ok := enclosing_class_owner_unit(&project.providers.source_files[source_file_index], site.scope)
		if class_ok {
			class_handle = Symbol_Link{unit = project.providers.source_files[source_file_index].source_file_id, symbol = class_symbol}
			ok = true
		}
	case:
		return unknown_type_fact()
	}
	if !ok {
		return unknown_type_fact()
	}
	if fact, trusted := direct_call_result_type_fact(project, lookup, source_file_index, site, class_handle, method_name);
	   trusted {
		return fact
	}
	member, member_source_file_index, member_ok := class_member_in_hierarchy_with_unit(
		project,
		lookup,
		class_handle,
		site.target.method_name,
		false,
		source_file_index,
		site.scope,
	)
	member_info := entity_decl_info(&project.providers.source_files[member_source_file_index], member.symbol) if member_ok else nil
	if !member_ok || member_info == nil || member_info.member_kind != .Method {
		return unknown_type_fact()
	}
	if fact, fact_ok := method_signature_result_type_fact(
		project,
		lookup,
		member,
		member_source_file_index,
		member_info,
		.Low,
	); fact_ok {
		return fact
	}
	return type_fact_with_confidence(class_member_type_fact(project, member, member_source_file_index), .Low)
}

builtin_routine_result_type_fact :: proc(unit: ^Source_File_Provider, name: string) -> Type_Fact_Data {
	spec := builtin_routine_spec(name)
	if spec == nil || spec.return_type == "" {
		return unknown_type_fact()
	}
	return Type_Fact_Data {
		type_id = type_builtin(unit, spec.return_type),
		type_unit = unit.source_file_id,
		structure = INVALID_STRUCTURE_ID,
		structure_unit = INVALID_SOURCE_FILE_ID,
		declared_type = builtin_type_ref(spec.return_type),
		has_declared_type = true,
		confidence = .High,
	}
}

direct_call_result_type_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	site: Call_Site_Data,
	class_handle: Symbol_Link,
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
	member_source_file_index := source_file_id_index(member.unit)
	if member_source_file_index < 0 ||
	   member_source_file_index >= len(project.providers.source_files) ||
	   project.providers.source_files[member_source_file_index].role != .Full_Source {
		return unknown_type_fact(), false
	}
	info := entity_decl_info(&project.providers.source_files[member_source_file_index], member.symbol)
	if info == nil || info.member_kind != .Method || .Is_Redefinition in info.flags {
		return unknown_type_fact(), false
	}
	return method_signature_result_type_fact(project, lookup, member, member_source_file_index, info, .High)
}

method_signature_result_type_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	member: Symbol_Link,
	member_source_file_index: int,
	info: ^Decl_Info_Data,
	confidence: Type_Fact_Confidence,
) -> (Type_Fact_Data, bool) {
	signature := member
	signature_source_file_index := member_source_file_index
	signature_info := info
	if info.effective_signature.unit != INVALID_SOURCE_FILE_ID &&
	   info.effective_signature.symbol != INVALID_SYMBOL_ID {
		source_file_index := source_file_id_index(info.effective_signature.unit)
		if source_file_index >= 0 && source_file_index < len(project.providers.source_files) {
			if effective := entity_decl_info(&project.providers.source_files[source_file_index], info.effective_signature.symbol);
			   effective != nil {
				signature = info.effective_signature
				signature_source_file_index = source_file_index
				signature_info = effective
			}
		}
	}
	for &param in signature_info.signature_parameters {
		if param.section != .Method_Returning && param.section != .Method_Receiving {
			continue
		}
		fact := typecheck_parameter_fact(project, lookup, signature_source_file_index, signature_info, param)
		impl_scope, impl_source_file_index, has_impl := method_implementation_scope(
			project,
			signature,
			signature_source_file_index,
			signature_info,
		)
		if inferred, inferred_ok := method_return_assignment_type_fact(
			project,
			impl_source_file_index if has_impl else signature_source_file_index,
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
	signature: Symbol_Link,
	signature_source_file_index: int,
	signature_info: ^Decl_Info_Data,
) -> (Scope_Id, int, bool) {
	if signature_info.body_scope != INVALID_SCOPE_ID {
		return signature_info.body_scope, signature_source_file_index, true
	}
	if signature_info.implementation_unit != INVALID_SOURCE_FILE_ID {
		source_file_index := source_file_id_index(signature_info.implementation_unit)
		if scope, ok := method_implementation_scope_in_unit(
			project,
			source_file_index,
			signature,
			signature_info.implementation_range,
		); ok {
			return scope, source_file_index, true
		}
	}
	if scope, ok := method_implementation_scope_in_unit(
		project,
		signature_source_file_index,
		signature,
		signature_info.implementation_range,
	); ok {
		return scope, signature_source_file_index, true
	}
	return INVALID_SCOPE_ID, -1, false
}

method_implementation_scope_in_unit :: proc(
	project: ^Project_Analysis,
	source_file_index: int,
	signature: Symbol_Link,
	implementation_range: tokenizer.Range,
) -> (Scope_Id, bool) {
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return INVALID_SCOPE_ID, false
	}
	unit := &project.providers.source_files[source_file_index]
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
	source_file_index := source_file_id_index(structure_unit)
	if structure_id == INVALID_STRUCTURE_ID || source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return 0, false
	}
	st := structure(&project.providers.source_files[source_file_index], structure_id)
	if st == nil {
		return 0, false
	}
	return len(st.fields), true
}

method_return_assignment_type_fact :: proc(
	project: ^Project_Analysis,
	source_file_index: int,
	body_scope: Scope_Id,
	param: Decl_Signature_Parameter_Data,
) -> (Type_Fact_Data, bool) {
	if param.name == "" ||
	   body_scope == INVALID_SCOPE_ID ||
	   source_file_index < 0 ||
	   source_file_index >= len(project.providers.source_files) {
		return {}, false
	}
	unit := &project.providers.source_files[source_file_index]
	out := Type_Fact_Data{}
	found := false
	for &site in unit.assignment_sites {
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

scope_is_or_child :: proc(unit: ^Source_File_Provider, scope_id, parent: Scope_Id) -> bool {
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
	for &segment in access.field_path {
		if !range_valid(out) {
			out = segment.range
		} else {
			if segment.range.start < out.start {out.start = segment.range.start}
			if segment.range.end > out.end {out.end = segment.range.end}
		}
	}
	return out
}
