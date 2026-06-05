package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

import "core:strings"

Structure_Field_Flag :: enum {
	Has_Decl_Range,
	Has_Type_Ref,
	Is_Key,
	Is_Include,
}
Structure_Field_Flags :: bit_set[Structure_Field_Flag]

Structure_Field_Data :: struct {
	name:                    string,
	decl_range:              tokenizer.Range,
	decl_unit:               Source_File_Id,
	type_id:                 Type_Id,
	structure:               Structure_Id,
	type_ref:                Field_Type_Ref_Data,
	type_clause_form:        ast.Data_Type_Form,
	has_type_clause_form:    bool,
	value_clause_display:    string,
	description:             string,
	include_renaming_suffix: string,
	flags:                   Structure_Field_Flags,
}

Structure_Field_Shape_Kind :: enum {
	Scalar,
	Structured,
}

Structure_Field_Info :: struct {
	owner:                Structure_Id,
	owner_unit:           Source_File_Id,
	name:                 string,
	decl_range:           tokenizer.Range,
	decl_unit:            Source_File_Id,
	shape:                Structure_Field_Shape_Kind,
	type_id:              Type_Id,
	structure:            Structure_Id,
	type_ref:             Field_Type_Ref_Data,
	type_clause_form:     ast.Data_Type_Form,
	has_type_clause_form: bool,
	value_clause_display: string,
	description:          string,
	flags:                Structure_Field_Flags,
}

Structure_Data :: struct {
	id:               Structure_Id,
	origin_unit:      Source_File_Id,
	origin_structure: Structure_Id,
	name:             string,
	scope:            Scope_Id,
	fields:           [dynamic]Structure_Field_Data,
}

push_structure :: proc(
	unit: ^Source_File_Provider,
	name: string,
	fields: [dynamic]Structure_Field_Data,
	scope := INVALID_SCOPE_ID,
) -> Structure_Id {
	id := Structure_Id(u32(len(unit.structures)))
	append(
		&unit.structures,
		Structure_Data {
			id = id,
			origin_unit = unit.source_file_id,
			origin_structure = id,
			name = name,
			scope = scope,
			fields = fields,
		},
	)
	_ = type_structure(unit, id)
	return id
}

structure :: proc(unit: ^Source_File_Provider, id: Structure_Id) -> ^Structure_Data {
	if id == INVALID_STRUCTURE_ID {
		return nil
	}
	if index, builtin := builtin_structure_index(id); builtin {
		provider := shared_builtin_provider()
		if index >= 0 && index < len(provider.structures) {
			return &provider.structures[index]
		}
		return nil
	}
	if structure_id_index(id) >= len(unit.structures) {
		return nil
	}
	return &unit.structures[structure_id_index(id)]
}

find_structure :: proc(unit: ^Source_File_Provider, name: string) -> ^Structure_Data {
	for &s in unit.structures {
		if strings.equal_fold(s.name, name) {
			return &s
		}
	}
	if !builtin_provider_is_shared(unit) {
		builtin := shared_builtin_provider()
		for &s in builtin.structures {
			if strings.equal_fold(s.name, name) {
				return &s
			}
		}
	}
	return nil
}

structure_field :: proc(
	unit: ^Source_File_Provider,
	structure_id: Structure_Id,
	field_name: string,
) -> ^Structure_Field_Data {
	s := structure(unit, structure_id)
	if s == nil {
		return nil
	}
	for &field in s.fields {
		if strings.equal_fold(field.name, field_name) {
			return &field
		}
	}
	return nil
}

structure_field_info :: proc(
	unit: ^Source_File_Provider,
	structure_id: Structure_Id,
	field_name: string,
) -> (
	Structure_Field_Info,
	bool,
) {
	field := structure_field(unit, structure_id, field_name)
	if field == nil {
		return {}, false
	}
	owner := structure(unit, structure_id)
	assert(owner != nil)
	info := Structure_Field_Info {
		owner                = structure_id,
		owner_unit           = owner.origin_unit,
		name                 = field.name,
		decl_range           = field.decl_range,
		decl_unit            = field.decl_unit,
		shape                = .Scalar,
		type_id              = field.type_id,
		structure            = field.structure,
		type_ref             = field.type_ref,
		type_clause_form     = field.type_clause_form,
		has_type_clause_form = field.has_type_clause_form,
		value_clause_display = field.value_clause_display,
		description          = field.description,
		flags                = field.flags,
	}
	if field.structure != INVALID_STRUCTURE_ID {
		info.shape = .Structured
	}
	return info, true
}
