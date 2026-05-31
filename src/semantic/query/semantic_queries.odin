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
	best := -1
	best_width := 0
	for symbol, i in q.unit.symbols {
		if !analyze.range_contains_offset(symbol.decl_range, offset) {
			continue
		}
		width := symbol.decl_range.end - symbol.decl_range.start
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return &q.unit.symbols[best] if best >= 0 else nil
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
	for &symbol in q.unit.symbols {
		if symbol.kind == kind && symbol.decl_range == range {
			return &symbol
		}
	}
	return nil
}

decl_class_member_at_offset :: proc(q: Decl_Queries, offset: int) -> ^analyze.Class_Member_Data {
	best := -1
	best_width := 0
	for member, i in q.unit.class_members {
		width := 0
		if analyze.range_contains_offset(member.decl_range, offset) {
			width = member.decl_range.end - member.decl_range.start
		} else if analyze.range_contains_offset(member.implementation_range, offset) {
			width = member.implementation_range.end - member.implementation_range.start
		} else {
			continue
		}
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return &q.unit.class_members[best] if best >= 0 else nil
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

decl_structure_field_at_offset :: proc(
	q: Decl_Queries,
	offset: int,
) -> (
	analyze.Structure_Field_Info,
	bool,
) {
	best_structure := analyze.INVALID_STRUCTURE_ID
	best_name := ""
	best_range := tokenizer.Range{}
	best_width := 0
	for st in q.unit.structures {
		for field in st.fields {
			if !analyze.range_contains_offset(field.decl_range, offset) {
				continue
			}
			width := field.decl_range.end - field.decl_range.start
			if best_structure == analyze.INVALID_STRUCTURE_ID || width < best_width {
				best_structure = st.id
				best_name = field.name
				best_range = field.decl_range
				best_width = width
			}
		}
	}
	if best_structure == analyze.INVALID_STRUCTURE_ID {
		return analyze.Structure_Field_Info{}, false
	}
	info, ok := analyze.structure_field_info(q.unit, best_structure, best_name)
	if !ok || info.decl_range != best_range {
		return analyze.Structure_Field_Info{}, false
	}
	return info, true
}

ref_reference_at_offset :: proc(q: Ref_Queries, offset: int) -> ^analyze.Reference_Data {
	best := -1
	best_width := 0
	for reference, i in q.unit.references {
		if !analyze.range_contains_offset(reference.range, offset) {
			continue
		}
		width := reference.range.end - reference.range.start
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return &q.unit.references[best] if best >= 0 else nil
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
	for &reference in q.unit.references {
		if reference.range == range {
			return &reference
		}
	}
	return nil
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

sql_name_ref_at_offset :: proc(q: Sql_Queries, offset: int) -> ^analyze.Sql_Name_Ref_Data {
	best := -1
	best_width := 0
	for sql_ref, i in q.unit.sql_name_refs {
		if !analyze.range_contains_offset(sql_ref.range, offset) {
			continue
		}
		width := sql_ref.range.end - sql_ref.range.start
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return &q.unit.sql_name_refs[best] if best >= 0 else nil
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

fact_operand_at_offset :: proc(q: Fact_Queries, offset: int) -> ^analyze.Operand_Data {
	best := -1
	best_priority := 0
	best_width := 0
	for operand, i in q.unit.operands {
		if !analyze.range_contains_offset(operand.range, offset) {
			continue
		}
		priority := operand_priority(operand.mode)
		width := operand.range.end - operand.range.start
		if best < 0 ||
		   width < best_width ||
		   (width == best_width && priority < best_priority) {
			best = i
			best_priority = priority
			best_width = width
		}
	}
	if best < 0 {
		return nil
	}
	return &q.unit.operands[best]
}

fact_operand_copy_at_offset :: proc(
	q: Fact_Queries,
	offset: int,
) -> (
	analyze.Operand_Data,
	bool,
) {
	operand := fact_operand_at_offset(q, offset)
	if operand == nil {
		return {}, false
	}
	return operand^, true
}

operand_priority :: proc(mode: analyze.Operand_Mode) -> int {
	switch mode {
	case .Field, .Method:
		return 0
	case .Variable, .Constant, .Type, .Routine:
		return 1
	case .Value:
		return 2
	case .Unknown:
		return 3
	case .Invalid:
		return 4
	}
	return 5
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
