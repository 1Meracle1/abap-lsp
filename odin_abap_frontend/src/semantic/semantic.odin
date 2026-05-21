package abap_frontend_semantic

import "../tokenizer"

import "core:mem"

Sem_Symbol_Id :: distinct u32
Sem_Reference_Id :: distinct u32
Sem_Scope_Id :: distinct u32
Sem_Sql_Query_Id :: distinct u32
Sem_Sql_Name_Ref_Id :: distinct u32
Sem_Class_Member_Id :: distinct u32
Sem_Structure_Field_Id :: distinct u32

sem_symbol_index :: #force_inline proc(id: Sem_Symbol_Id) -> int {return int(id)}
sem_reference_index :: #force_inline proc(id: Sem_Reference_Id) -> int {return int(id)}
sem_sql_name_ref_index :: #force_inline proc(id: Sem_Sql_Name_Ref_Id) -> int {return int(id)}
sem_class_member_index :: #force_inline proc(id: Sem_Class_Member_Id) -> int {return int(id)}
sem_structure_field_index :: #force_inline proc(id: Sem_Structure_Field_Id) -> int {return int(id)}

Sem_Symbol :: struct {
	symbol_id:  Symbol_Id,
	scope:      Scope_Id,
	decl_range: tokenizer.Range,
	kind:       Symbol_Kind,
}

Sem_Reference :: struct {
	reference_id: Reference_Id,
	scope:        Scope_Id,
	range:        tokenizer.Range,
}

Sem_Scope :: struct {
	scope_id: Scope_Id,
	parent:   Scope_Id,
	range:    tokenizer.Range,
}

Sem_Sql_Query :: struct {
	query_id: int,
	scope:    Scope_Id,
	range:    tokenizer.Range,
}

Sem_Sql_Name_Ref :: struct {
	raw_index: int,
	query_id:  int,
	range:     tokenizer.Range,
	kind:      Sql_Name_Ref_Kind,
}

Sem_Class_Member :: struct {
	raw_index:            int,
	class_symbol:         Symbol_Id,
	kind:                 Class_Member_Kind,
	decl_range:           tokenizer.Range,
	implementation_range: tokenizer.Range,
}

Sem_Structure_Field :: struct {
	structure_id: Structure_Id,
	raw_index:    int,
	name:         string,
	decl_range:   tokenizer.Range,
}

Semantic_Index :: struct {
	symbols:          [dynamic]Sem_Symbol,
	references:       [dynamic]Sem_Reference,
	scopes:           [dynamic]Sem_Scope,
	sql_queries:      [dynamic]Sem_Sql_Query,
	sql_name_refs:    [dynamic]Sem_Sql_Name_Ref,
	class_members:    [dynamic]Sem_Class_Member,
	structure_fields: [dynamic]Sem_Structure_Field,
}

semantic_index_make :: proc(allocator: mem.Allocator) -> Semantic_Index {
	return Semantic_Index {
		symbols = make([dynamic]Sem_Symbol, 0, 0, allocator),
		references = make([dynamic]Sem_Reference, 0, 0, allocator),
		scopes = make([dynamic]Sem_Scope, 0, 0, allocator),
		sql_queries = make([dynamic]Sem_Sql_Query, 0, 0, allocator),
		sql_name_refs = make([dynamic]Sem_Sql_Name_Ref, 0, 0, allocator),
		class_members = make([dynamic]Sem_Class_Member, 0, 0, allocator),
		structure_fields = make([dynamic]Sem_Structure_Field, 0, 0, allocator),
	}
}

build_semantic_index :: proc(unit: ^Unit_Analysis, allocator: mem.Allocator) -> Semantic_Index {
	index := Semantic_Index {
		symbols          = make([dynamic]Sem_Symbol, 0, len(unit.symbols), allocator),
		references       = make([dynamic]Sem_Reference, 0, len(unit.references), allocator),
		scopes           = make([dynamic]Sem_Scope, 0, len(unit.scopes), allocator),
		sql_queries      = make([dynamic]Sem_Sql_Query, 0, len(unit.sql_queries), allocator),
		sql_name_refs    = make([dynamic]Sem_Sql_Name_Ref, 0, len(unit.sql_name_refs), allocator),
		class_members    = make([dynamic]Sem_Class_Member, 0, len(unit.class_members), allocator),
		structure_fields = make([dynamic]Sem_Structure_Field, 0, 8, allocator),
	}
	for s in unit.symbols {
		append(
			&index.symbols,
			Sem_Symbol {
				symbol_id = s.id,
				scope = s.scope,
				decl_range = s.decl_range,
				kind = s.kind,
			},
		)
	}
	for r in unit.references {
		append(
			&index.references,
			Sem_Reference{reference_id = r.id, scope = r.scope, range = r.range},
		)
	}
	for s in unit.scopes {
		append(&index.scopes, Sem_Scope{scope_id = s.id, parent = s.parent, range = s.range})
	}
	for query, i in unit.sql_queries {
		append(
			&index.sql_queries,
			Sem_Sql_Query{query_id = i, scope = query.scope, range = query.range},
		)
	}
	for sql_ref, i in unit.sql_name_refs {
		append(
			&index.sql_name_refs,
			Sem_Sql_Name_Ref {
				raw_index = i,
				query_id = sql_ref.query_id,
				range = sql_ref.range,
				kind = sql_ref.kind,
			},
		)
	}
	for member, i in unit.class_members {
		append(
			&index.class_members,
			Sem_Class_Member {
				raw_index = i,
				class_symbol = member.class_symbol,
				kind = member.kind,
				decl_range = member.decl_range,
				implementation_range = member.implementation_range,
			},
		)
	}
	for st in unit.structures {
		for field, i in st.fields {
			if range_valid(field.decl_range) {
				append(
					&index.structure_fields,
					Sem_Structure_Field {
						structure_id = st.id,
						raw_index = i,
						name = field.name,
						decl_range = field.decl_range,
					},
				)
			}
		}
	}
	return index
}

semantic_index_symbol_at_offset :: proc(
	index: ^Semantic_Index,
	offset: int,
) -> (
	Sem_Symbol_Id,
	bool,
) {
	best := -1
	best_width := 0
	for item, i in index.symbols {
		if !range_contains_offset(item.decl_range, offset) {
			continue
		}
		width := item.decl_range.end - item.decl_range.start
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return Sem_Symbol_Id(u32(best)), best >= 0
}

semantic_index_symbol_with_kind_and_decl_range :: proc(
	index: ^Semantic_Index,
	kind: Symbol_Kind,
	range: tokenizer.Range,
) -> (
	Sem_Symbol_Id,
	bool,
) {
	for item, i in index.symbols {
		if item.kind == kind && item.decl_range == range {
			return Sem_Symbol_Id(u32(i)), true
		}
	}
	return Sem_Symbol_Id(0), false
}

semantic_index_reference_at_offset :: proc(
	index: ^Semantic_Index,
	offset: int,
) -> (
	Sem_Reference_Id,
	bool,
) {
	best := -1
	best_width := 0
	for item, i in index.references {
		if !range_contains_offset(item.range, offset) {
			continue
		}
		width := item.range.end - item.range.start
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return Sem_Reference_Id(u32(best)), best >= 0
}

semantic_index_reference_at_range :: proc(
	index: ^Semantic_Index,
	range: tokenizer.Range,
) -> (
	Sem_Reference_Id,
	bool,
) {
	for item, i in index.references {
		if item.range == range {
			return Sem_Reference_Id(u32(i)), true
		}
	}
	return Sem_Reference_Id(0), false
}

semantic_index_sql_name_ref_at_offset :: proc(
	index: ^Semantic_Index,
	offset: int,
) -> (
	Sem_Sql_Name_Ref_Id,
	bool,
) {
	best := -1
	best_width := 0
	for item, i in index.sql_name_refs {
		if !range_contains_offset(item.range, offset) {
			continue
		}
		width := item.range.end - item.range.start
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return Sem_Sql_Name_Ref_Id(u32(best)), best >= 0
}

semantic_index_class_member_at_offset :: proc(
	index: ^Semantic_Index,
	offset: int,
) -> (
	Sem_Class_Member_Id,
	bool,
) {
	best := -1
	best_width := 0
	for item, i in index.class_members {
		width := 0
		if range_contains_offset(item.decl_range, offset) {
			width = item.decl_range.end - item.decl_range.start
		} else if range_contains_offset(item.implementation_range, offset) {
			width = item.implementation_range.end - item.implementation_range.start
		} else {
			continue
		}
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return Sem_Class_Member_Id(u32(best)), best >= 0
}

semantic_index_structure_field_at_offset :: proc(
	index: ^Semantic_Index,
	offset: int,
) -> (
	Sem_Structure_Field_Id,
	bool,
) {
	best := -1
	best_width := 0
	for item, i in index.structure_fields {
		if !range_contains_offset(item.decl_range, offset) {
			continue
		}
		width := item.decl_range.end - item.decl_range.start
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return Sem_Structure_Field_Id(u32(best)), best >= 0
}

range_contains_offset :: #force_inline proc(range: tokenizer.Range, offset: int) -> bool {
	return range.start <= offset && offset < range.end
}
