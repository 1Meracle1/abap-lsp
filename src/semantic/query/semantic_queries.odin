package abap_frontend_semantic_query

import analyze "src:semantic/analyze"
import "src:tokenizer"

import "core:mem"
import "core:strings"

// Pointer-returning query procedures borrow storage from the queried Unit_Analysis.
// The returned pointers are invalid after the unit is analyzed or mutated again.
Semantic_Queries :: struct {
	unit: ^analyze.Unit_Analysis,
}

Decl_Queries :: struct {
	unit: ^analyze.Unit_Analysis,
}

Ref_Queries :: struct {
	unit: ^analyze.Unit_Analysis,
}

Sql_Queries :: struct {
	unit: ^analyze.Unit_Analysis,
}

Fact_Queries :: struct {
	unit: ^analyze.Unit_Analysis,
}

semantic :: proc(unit: ^analyze.Unit_Analysis) -> Semantic_Queries {
	return Semantic_Queries{unit = unit}
}

decls :: proc(q: Semantic_Queries) -> Decl_Queries {
	return Decl_Queries{unit = q.unit}
}

refs :: proc(q: Semantic_Queries) -> Ref_Queries {
	return Ref_Queries{unit = q.unit}
}

sql :: proc(q: Semantic_Queries) -> Sql_Queries {
	return Sql_Queries{unit = q.unit}
}

facts :: proc(q: Semantic_Queries) -> Fact_Queries {
	return Fact_Queries{unit = q.unit}
}

decl_symbol_at_offset :: proc(q: Decl_Queries, offset: int) -> ^analyze.Symbol_Data {
	id, ok := analyze.semantic_index_symbol_at_offset(&q.unit.semantic_index, offset)
	if !ok {
		return nil
	}
	sem := q.unit.semantic_index.symbols[analyze.sem_symbol_index(id)]
	return analyze.symbol(q.unit, sem.symbol_id)
}

decl_symbol_handle_at_offset :: proc(
	q: Decl_Queries,
	offset: int,
) -> (
	analyze.Symbol_Handle,
	bool,
) {
	s := decl_symbol_at_offset(q, offset)
	if s == nil {
		return {}, false
	}
	return analyze.Symbol_Handle{unit = q.unit.unit_id, symbol = s.id}, true
}

decl_symbol_copy_at_offset :: proc(
	q: Decl_Queries,
	offset: int,
) -> (
	analyze.Symbol_Data,
	bool,
) {
	s := decl_symbol_at_offset(q, offset)
	if s == nil {
		return {}, false
	}
	return s^, true
}

decl_symbol_with_kind_and_decl_range :: proc(
	q: Decl_Queries,
	kind: analyze.Symbol_Kind,
	range: tokenizer.Range,
) -> ^analyze.Symbol_Data {
	id, ok := analyze.semantic_index_symbol_with_kind_and_decl_range(&q.unit.semantic_index, kind, range)
	if !ok {
		return nil
	}
	sem := q.unit.semantic_index.symbols[analyze.sem_symbol_index(id)]
	return analyze.symbol(q.unit, sem.symbol_id)
}

decl_class_member_at_offset :: proc(q: Decl_Queries, offset: int) -> ^analyze.Class_Member_Data {
	id, ok := analyze.semantic_index_class_member_at_offset(&q.unit.semantic_index, offset)
	if !ok {
		return nil
	}
	sem := q.unit.semantic_index.class_members[analyze.sem_class_member_index(id)]
	if sem.raw_index < 0 || sem.raw_index >= len(q.unit.class_members) {
		return nil
	}
	return &q.unit.class_members[sem.raw_index]
}

decl_class_member :: proc(
	q: Decl_Queries,
	class_symbol: analyze.Symbol_Id,
	name: string,
) -> ^analyze.Class_Member_Data {
	for &member in q.unit.class_members {
		if member.class_symbol == class_symbol && strings.equal_fold(member.name, name) {
			return &member
		}
	}
	return nil
}

decl_structure_field :: proc(
	q: Decl_Queries,
	structure_id: analyze.Structure_Id,
	field_name: string,
) -> ^analyze.Structure_Field_Data {
	return analyze.structure_field(q.unit, structure_id, field_name)
}

decl_structure_field_info :: proc(
	q: Decl_Queries,
	structure_id: analyze.Structure_Id,
	field_name: string,
) -> (
	analyze.Structure_Field_Info,
	bool,
) {
	return analyze.structure_field_info(q.unit, structure_id, field_name)
}

decl_structure_field_infos :: proc(
	q: Decl_Queries,
	structure_id: analyze.Structure_Id,
	allocator: mem.Allocator,
) -> [dynamic]analyze.Structure_Field_Info {
	out := make([dynamic]analyze.Structure_Field_Info, 0, 4, allocator)
	st := analyze.structure(q.unit, structure_id)
	if st == nil {
		return out
	}
	for field in st.fields {
		if info, ok := analyze.structure_field_info(q.unit, structure_id, field.name); ok {
			append(&out, info)
		}
	}
	return out
}

decl_resolve_structure_field_path :: proc(
	q: Decl_Queries,
	structure_id: analyze.Structure_Id,
	field_path: []string,
) -> (
	analyze.Structure_Field_Info,
	bool,
) {
	current := structure_id
	info := analyze.Structure_Field_Info{}
	for field_name, i in field_path {
		next, ok := analyze.structure_field_info(q.unit, current, field_name)
		if !ok {
			return analyze.Structure_Field_Info{}, false
		}
		info = next
		if i + 1 < len(field_path) {
			if next.shape != .Structured {
				return analyze.Structure_Field_Info{}, false
			}
			current = next.structure
		}
	}
	return info, len(field_path) > 0
}

decl_structure_field_at_offset :: proc(
	q: Decl_Queries,
	offset: int,
) -> (
	analyze.Structure_Field_Info,
	bool,
) {
	id, ok := analyze.semantic_index_structure_field_at_offset(&q.unit.semantic_index, offset)
	if !ok {
		return analyze.Structure_Field_Info{}, false
	}
	sem := q.unit.semantic_index.structure_fields[analyze.sem_structure_field_index(id)]
	info, info_ok := analyze.structure_field_info(q.unit, sem.structure_id, sem.name)
	if !info_ok || info.decl_range != sem.decl_range {
		return analyze.Structure_Field_Info{}, false
	}
	return info, true
}

ref_reference_at_offset :: proc(q: Ref_Queries, offset: int) -> ^analyze.Reference_Data {
	id, ok := analyze.semantic_index_reference_at_offset(&q.unit.semantic_index, offset)
	if !ok {
		return nil
	}
	sem := q.unit.semantic_index.references[analyze.sem_reference_index(id)]
	if analyze.reference_id_index(sem.reference_id) >= len(q.unit.references) {
		return nil
	}
	return &q.unit.references[analyze.reference_id_index(sem.reference_id)]
}

ref_reference_id_at_offset :: proc(
	q: Ref_Queries,
	offset: int,
) -> (
	analyze.Reference_Id,
	bool,
) {
	ref := ref_reference_at_offset(q, offset)
	if ref == nil {
		return analyze.INVALID_REFERENCE_ID, false
	}
	return ref.id, true
}

ref_reference_copy_at_offset :: proc(
	q: Ref_Queries,
	offset: int,
) -> (
	analyze.Reference_Data,
	bool,
) {
	ref := ref_reference_at_offset(q, offset)
	if ref == nil {
		return {}, false
	}
	return ref^, true
}

ref_reference_at_range :: proc(q: Ref_Queries, range: tokenizer.Range) -> ^analyze.Reference_Data {
	id, ok := analyze.semantic_index_reference_at_range(&q.unit.semantic_index, range)
	if !ok {
		return nil
	}
	sem := q.unit.semantic_index.references[analyze.sem_reference_index(id)]
	if analyze.reference_id_index(sem.reference_id) >= len(q.unit.references) {
		return nil
	}
	return &q.unit.references[analyze.reference_id_index(sem.reference_id)]
}

ref_reference_copy_at_range :: proc(
	q: Ref_Queries,
	range: tokenizer.Range,
) -> (
	analyze.Reference_Data,
	bool,
) {
	ref := ref_reference_at_range(q, range)
	if ref == nil {
		return {}, false
	}
	return ref^, true
}

ref_type_reference_at_offset :: proc(q: Ref_Queries, offset: int) -> ^analyze.Reference_Data {
	ref := ref_reference_at_offset(q, offset)
	if ref == nil || !(ref.kind == .Type_Ref || ref.kind == .Interface_Use) {
		return nil
	}
	return ref
}

ref_resolving_to :: proc(
	q: Ref_Queries,
	handle: analyze.Symbol_Handle,
	allocator: mem.Allocator,
) -> [dynamic]^analyze.Reference_Data {
	out := make([dynamic]^analyze.Reference_Data, 0, 4, allocator)
	for &reference in q.unit.references {
		if reference.has_resolution &&
		   reference.resolution.kind == .Symbol &&
		   reference.resolution.symbol == handle {
			append(&out, &reference)
		}
	}
	return out
}

ref_type_named :: proc(
	q: Ref_Queries,
	name: string,
	allocator: mem.Allocator,
) -> [dynamic]^analyze.Reference_Data {
	out := make([dynamic]^analyze.Reference_Data, 0, 2, allocator)
	for &reference in q.unit.references {
		if (reference.kind == .Type_Ref || reference.kind == .Interface_Use) &&
		   strings.equal_fold(reference.name, name) {
			append(&out, &reference)
		}
	}
	return out
}

ref_in_scope :: proc(
	q: Ref_Queries,
	scope_id: analyze.Scope_Id,
	allocator: mem.Allocator,
) -> [dynamic]^analyze.Reference_Data {
	out := make([dynamic]^analyze.Reference_Data, 0, 4, allocator)
	for &reference in q.unit.references {
		if reference.scope == scope_id {
			append(&out, &reference)
		}
	}
	return out
}

sql_name_ref_at_offset :: proc(q: Sql_Queries, offset: int) -> ^analyze.Sql_Name_Ref_Data {
	id, ok := analyze.semantic_index_sql_name_ref_at_offset(&q.unit.semantic_index, offset)
	if !ok {
		return nil
	}
	sem := q.unit.semantic_index.sql_name_refs[analyze.sem_sql_name_ref_index(id)]
	if sem.raw_index < 0 || sem.raw_index >= len(q.unit.sql_name_refs) {
		return nil
	}
	return &q.unit.sql_name_refs[sem.raw_index]
}

sql_source_name_refs_named :: proc(
	q: Sql_Queries,
	name: string,
	allocator: mem.Allocator,
) -> [dynamic]^analyze.Sql_Name_Ref_Data {
	out := make([dynamic]^analyze.Sql_Name_Ref_Data, 0, 2, allocator)
	for &reference in q.unit.sql_name_refs {
		if reference.kind == .Source && strings.equal_fold(reference.name, name) {
			append(&out, &reference)
		}
	}
	return out
}

sql_has_source_named :: proc(q: Sql_Queries, name: string) -> bool {
	for source in q.unit.sql_sources {
		if strings.equal_fold(source.name, name) {
			return true
		}
	}
	return false
}

fact_expression_fact_at_offset :: proc(q: Fact_Queries, offset: int) -> ^analyze.Expression_Fact_Data {
	best := -1
	best_priority := 0
	best_width := 0
	for fact, i in q.unit.expression_facts {
		if !analyze.range_contains_offset(fact.range, offset) {
			continue
		}
		priority := expression_fact_priority(fact.kind)
		width := fact.range.end - fact.range.start
		if best < 0 ||
		   priority < best_priority ||
		   (priority == best_priority && width < best_width) {
			best = i
			best_priority = priority
			best_width = width
		}
	}
	if best < 0 {
		return nil
	}
	return &q.unit.expression_facts[best]
}

fact_expression_fact_copy_at_offset :: proc(
	q: Fact_Queries,
	offset: int,
) -> (
	analyze.Expression_Fact_Data,
	bool,
) {
	fact := fact_expression_fact_at_offset(q, offset)
	if fact == nil {
		return {}, false
	}
	return fact^, true
}

fact_value_flow_edges_touching_offset :: proc(
	q: Fact_Queries,
	offset: int,
	allocator: mem.Allocator,
) -> [dynamic]^analyze.Value_Flow_Edge_Data {
	out := make([dynamic]^analyze.Value_Flow_Edge_Data, 0, 2, allocator)
	for &edge in q.unit.value_flow_edges {
		if analyze.range_contains_offset(edge.source_range, offset) ||
		   value_flow_target_contains_offset(edge.target, offset) {
			append(&out, &edge)
		}
	}
	return out
}

value_flow_target_contains_offset :: proc(target: analyze.Value_Flow_Target_Data, offset: int) -> bool {
	switch target.kind {
	case .Assignment, .Field_Symbol:
		return analyze.range_contains_offset(target.range, offset)
	case .Call_Parameter:
		return(
			target.has_parameter_decl_range &&
			analyze.range_contains_offset(target.parameter_decl_range, offset) \
		)
	}
	return false
}

expression_fact_priority :: proc(kind: analyze.Expression_Fact_Kind) -> int {
	switch kind {
	case .Call_Result:
		return 0
	case .Selector:
		return 1
	case .Reference:
		return 2
	}
	return 3
}
