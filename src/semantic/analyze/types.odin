package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

import "core:mem"
import "core:strings"

Type_Kind :: enum {
	Unknown,
	Builtin,
	Named,
	Structure,
	Table,
	Ref,
	Class,
	Interface,
}

Type_Data :: struct {
	id:         Type_Id,
	kind:       Type_Kind,
	name:       string,
	symbol:     Symbol_Id,
	structure:  Structure_Id,
	base:       Type_Id,
	table_form: ast.Data_Type_Form,
}

Field_Type_Ref_Data :: struct {
	namespace:       Namespace,
	is_ref:          bool,
	base_name:       string,
	base_range:      tokenizer.Range,
	field_path:      [dynamic]string,
	field_ranges:    [dynamic]tokenizer.Range,
	field_derefs:    [dynamic]bool,
	field_selectors: [dynamic]ast.Selector_Op,
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

Type_Fact_Data :: struct {
	type_id:             Type_Id,
	type_unit:           Source_File_Id,
	structure:           Structure_Id,
	structure_unit:      Source_File_Id,
	declared_type:       Field_Type_Ref_Data,
	has_declared_type:   bool,
	type_clause_display: string,
	table_line:          ^Type_Fact_Data,
	confidence:          Type_Fact_Confidence,
}

Type_Fact_Confidence :: enum {
	Low,
	High,
}

unknown_type_fact :: #force_inline proc() -> Type_Fact_Data {
	return Type_Fact_Data {
		type_id = UNKNOWN_TYPE_ID,
		type_unit = INVALID_SOURCE_FILE_ID,
		structure = INVALID_STRUCTURE_ID,
		structure_unit = INVALID_SOURCE_FILE_ID,
	}
}

type_fact_is_known :: #force_inline proc(fact: Type_Fact_Data) -> bool {
	return(
		type_id_is_known(fact.type_id) ||
		fact.structure != INVALID_STRUCTURE_ID ||
		fact.has_declared_type ||
		fact.type_clause_display != "" ||
		fact.table_line != nil \
	)
}

type_fact_is_high_confidence :: #force_inline proc(fact: Type_Fact_Data) -> bool {
	return type_fact_is_known(fact) && fact.confidence == .High
}

type_fact_with_confidence :: #force_inline proc(
	fact: Type_Fact_Data,
	confidence: Type_Fact_Confidence,
) -> Type_Fact_Data {
	out := fact
	out.confidence = confidence
	return out
}

type_arena_init :: proc(unit: ^Source_File_Provider, allocator: mem.Allocator) {
	unit.types = make([dynamic]Type_Data, 0, 32, allocator)
	append(
		&unit.types,
		Type_Data {
			id = UNKNOWN_TYPE_ID,
			kind = .Unknown,
			symbol = INVALID_SYMBOL_ID,
			structure = INVALID_STRUCTURE_ID,
			base = UNKNOWN_TYPE_ID,
		},
	)
}

type_id_is_known :: #force_inline proc(id: Type_Id) -> bool {
	return id != UNKNOWN_TYPE_ID && id != INVALID_TYPE_ID
}

type_data :: proc(unit: ^Source_File_Provider, id: Type_Id) -> ^Type_Data {
	if id == INVALID_TYPE_ID || type_id_index(id) >= len(unit.types) {
		return nil
	}
	return &unit.types[type_id_index(id)]
}

type_intern :: proc(unit: ^Source_File_Provider, item: Type_Data) -> Type_Id {
	assert(len(unit.types) > 0)
	for &existing in unit.types {
		if type_data_equal(existing, item) {
			if existing.kind == .Named && type_id_is_known(item.base) && existing.base != item.base {
				existing.base = item.base
			}
			return existing.id
		}
	}
	id := Type_Id(u32(len(unit.types)))
	next := item
	next.id = id
	append(&unit.types, next)
	return id
}

type_data_equal :: proc(a, b: Type_Data) -> bool {
	if a.kind != b.kind {
		return false
	}
	#partial switch a.kind {
	case .Unknown:
		return true
	case .Builtin:
		return strings.equal_fold(a.name, b.name)
	case .Named:
		if a.symbol != INVALID_SYMBOL_ID || b.symbol != INVALID_SYMBOL_ID {
			return a.symbol == b.symbol
		}
		return strings.equal_fold(a.name, b.name) && a.base == b.base
	case .Structure:
		return a.structure == b.structure
	case .Table:
		return a.base == b.base && a.table_form == b.table_form
	case .Ref:
		return a.base == b.base
	case .Class, .Interface:
		if a.symbol != INVALID_SYMBOL_ID || b.symbol != INVALID_SYMBOL_ID {
			return a.symbol == b.symbol
		}
		return strings.equal_fold(a.name, b.name)
	}
	return false
}

type_builtin :: proc(unit: ^Source_File_Provider, name: string) -> Type_Id {
	if name == "" {
		return UNKNOWN_TYPE_ID
	}
	return type_intern(
		unit,
		Type_Data {
			kind = .Builtin,
			name = name,
			symbol = INVALID_SYMBOL_ID,
			structure = INVALID_STRUCTURE_ID,
			base = UNKNOWN_TYPE_ID,
		},
	)
}

type_named :: proc(unit: ^Source_File_Provider, name: string, symbol_id: Symbol_Id, base := UNKNOWN_TYPE_ID) -> Type_Id {
	if name == "" {
		return UNKNOWN_TYPE_ID
	}
	return type_intern(
		unit,
		Type_Data {
			kind = .Named,
			name = name,
			symbol = symbol_id,
			structure = INVALID_STRUCTURE_ID,
			base = base,
		},
	)
}

type_structure :: proc(unit: ^Source_File_Provider, structure_id: Structure_Id) -> Type_Id {
	st := structure(unit, structure_id)
	if st == nil {
		return UNKNOWN_TYPE_ID
	}
	return type_intern(
		unit,
		Type_Data {
			kind = .Structure,
			name = st.name,
			symbol = INVALID_SYMBOL_ID,
			structure = structure_id,
			base = UNKNOWN_TYPE_ID,
		},
	)
}

type_table :: proc(unit: ^Source_File_Provider, row: Type_Id, form: ast.Data_Type_Form) -> Type_Id {
	return type_intern(
		unit,
		Type_Data {
			kind = .Table,
			symbol = INVALID_SYMBOL_ID,
			structure = INVALID_STRUCTURE_ID,
			base = row,
			table_form = form,
		},
	)
}

type_ref :: proc(unit: ^Source_File_Provider, target: Type_Id) -> Type_Id {
	return type_intern(
		unit,
		Type_Data {
			kind = .Ref,
			symbol = INVALID_SYMBOL_ID,
			structure = INVALID_STRUCTURE_ID,
			base = target,
		},
	)
}

type_class_or_interface :: proc(
	unit: ^Source_File_Provider,
	name: string,
	symbol_id: Symbol_Id,
	kind: Symbol_Kind,
) -> Type_Id {
	type_kind := Type_Kind.Class if kind == .Class else Type_Kind.Interface
	return type_intern(
		unit,
		Type_Data {
			kind = type_kind,
			name = name,
			symbol = symbol_id,
			structure = INVALID_STRUCTURE_ID,
			base = UNKNOWN_TYPE_ID,
		},
	)
}

type_id_from_symbol_data :: proc(unit: ^Source_File_Provider, s: ^Symbol_Data, depth := 0) -> Type_Id {
	if s == nil {
		return UNKNOWN_TYPE_ID
	}
	return type_id_from_symbol_fields(
		unit,
		s.id,
		s.scope,
		s.name,
		s.kind,
		s.structure,
		s.declared_type,
		s.has_declared_type,
		s.type_clause_form,
		s.has_type_clause_form,
		depth,
	)
}

type_id_from_symbol_fields :: proc(
	unit: ^Source_File_Provider,
	symbol_id: Symbol_Id,
	scope_id: Scope_Id,
	name: string,
	kind: Symbol_Kind,
	structure_id: Structure_Id,
	declared_type: Field_Type_Ref_Data,
	has_declared_type: bool,
	type_form := ast.Data_Type_Form{},
	has_type_form := false,
	depth := 0,
) -> Type_Id {
	base := UNKNOWN_TYPE_ID
	if has_declared_type {
		base = type_id_from_declared_type(unit, scope_id, declared_type, type_form, has_type_form, depth + 1)
	}
	if structure_id != INVALID_STRUCTURE_ID {
		structure_type := type_structure(unit, structure_id)
		if has_type_form && type_form == .Range_Of {
			base = type_table(unit, structure_type, type_form)
		} else if has_type_form && type_form_is_table_category(type_form) && !type_id_is_known(base) {
			base = type_table(unit, structure_type, type_form)
		} else if !type_id_is_known(base) {
			base = structure_type
		}
	}
	if has_type_form && type_form_is_table_category(type_form) && !type_id_is_known(base) {
		base = type_table(unit, UNKNOWN_TYPE_ID, type_form)
	}
	#partial switch kind {
	case .Builtin_Type:
		return type_builtin(unit, name)
	case .Type_Def:
		return type_named(unit, name, symbol_id, base)
	case .Class, .Interface:
		return type_class_or_interface(unit, name, symbol_id, kind)
	}
	return base
}

type_id_from_declared_type :: proc(
	unit: ^Source_File_Provider,
	scope_id: Scope_Id,
	type_ref_data: Field_Type_Ref_Data,
	type_form := ast.Data_Type_Form{},
	has_type_form := false,
	depth := 0,
) -> Type_Id {
	if depth > 16 {
		return UNKNOWN_TYPE_ID
	}
	base := type_id_from_type_ref_path(unit, scope_id, type_ref_data, depth + 1)
	if has_type_form {
		#partial switch type_form {
		case .Type_Line_Of, .Like_Line_Of:
			return type_row_type(unit, base, depth + 1)
		case .Range_Of:
			return type_table(unit, base, type_form)
		case .Any_Table,
		     .Table,
		     .Like_Table,
		     .Index_Table,
		     .Standard_Table,
		     .Sorted_Table,
		     .Hashed_Table,
		     .Like_Standard_Table,
		     .Like_Sorted_Table,
		     .Like_Hashed_Table:
			return type_table(unit, base, type_form)
		}
	}
	return base
}

type_id_from_type_ref_path :: proc(
	unit: ^Source_File_Provider,
	scope_id: Scope_Id,
	type_ref_data: Field_Type_Ref_Data,
	depth: int,
) -> Type_Id {
	if depth > 16 || type_ref_data.base_name == "" {
		return UNKNOWN_TYPE_ID
	}
	current := UNKNOWN_TYPE_ID
	symbol_id, has_symbol := type_symbol_for_ref(unit, scope_id, type_ref_data)
	path_start := 0
	if has_symbol {
		current = type_id_from_symbol(unit, symbol_id, depth + 1)
		if s := symbol(unit, symbol_id);
		   s != nil &&
		   (s.kind == .Class || s.kind == .Interface) &&
		   len(type_ref_data.field_path) > 0 &&
		   type_selector_at(type_ref_data.field_selectors[:], 0) != .Dash {
			if nested, ok := type_class_symbol(unit, symbol_id, type_ref_data.field_path[0]); ok {
				current = type_id_from_symbol(unit, nested, depth + 1)
				path_start = 1
			}
		}
	} else if is_builtin_type_name(type_ref_data.base_name) {
		current = type_builtin(unit, type_ref_data.base_name)
	} else if type_ref_data.namespace == .Type {
		current = type_named(unit, type_ref_data.base_name, INVALID_SYMBOL_ID)
	}
	for i := path_start; i < len(type_ref_data.field_path); i += 1 {
		if i < len(type_ref_data.field_derefs) && type_ref_data.field_derefs[i] {
			current = type_ref_target(unit, current, depth + 1)
			continue
		}
		selector := type_selector_at(type_ref_data.field_selectors[:], i)
		name := type_ref_data.field_path[i]
		if selector == .Arrow {
			target := type_ref_target(unit, current, depth + 1)
			if class_symbol, ok := type_class_symbol_from_type(unit, target, depth + 1); ok {
				if member := unit_class_member_symbol_canonical(unit, class_symbol, name); member != nil {
					current = member.type_id
					continue
				}
			}
			return UNKNOWN_TYPE_ID
		}
		if selector == .Fat_Arrow || selector == .Tilde {
			if class_symbol, ok := type_class_symbol_from_type(unit, current, depth + 1); ok {
				if nested, nested_ok := type_class_symbol(unit, class_symbol, name); nested_ok {
					current = type_id_from_symbol(unit, nested, depth + 1)
					continue
				}
			}
			return UNKNOWN_TYPE_ID
		}
		if structure_id, ok := type_structure_id(unit, current, depth + 1); ok {
			if field := structure_field(unit, structure_id, name); field != nil {
				current = field.type_id
				if !type_id_is_known(current) && field.structure != INVALID_STRUCTURE_ID {
					current = type_structure(unit, field.structure)
				}
				continue
			}
		}
		return UNKNOWN_TYPE_ID
	}
	if type_ref_data.is_ref {
		return type_ref(unit, current)
	}
	return current
}

type_id_from_symbol :: proc(unit: ^Source_File_Provider, symbol_id: Symbol_Id, depth := 0) -> Type_Id {
	if depth > 16 {
		return UNKNOWN_TYPE_ID
	}
	s := symbol(unit, symbol_id)
	if s == nil {
		return UNKNOWN_TYPE_ID
	}
	if type_id_is_known(s.type_id) {
		return s.type_id
	}
	return type_id_from_symbol_data(unit, s, depth + 1)
}

type_row_type :: proc(unit: ^Source_File_Provider, id: Type_Id, depth := 0) -> Type_Id {
	if depth > 16 {
		return UNKNOWN_TYPE_ID
	}
	if t := type_data(unit, id); t != nil {
		#partial switch t.kind {
		case .Table:
			return t.base
		case .Named:
			return type_row_type(unit, t.base, depth + 1)
		}
	}
	return UNKNOWN_TYPE_ID
}

type_ref_target :: proc(unit: ^Source_File_Provider, id: Type_Id, depth := 0) -> Type_Id {
	if depth > 16 {
		return UNKNOWN_TYPE_ID
	}
	if t := type_data(unit, id); t != nil {
		#partial switch t.kind {
		case .Ref:
			return t.base
		case .Named:
			return type_ref_target(unit, t.base, depth + 1)
		}
	}
	return UNKNOWN_TYPE_ID
}

type_structure_id :: proc(unit: ^Source_File_Provider, id: Type_Id, depth := 0) -> (Structure_Id, bool) {
	if depth > 16 {
		return INVALID_STRUCTURE_ID, false
	}
	if t := type_data(unit, id); t != nil {
		#partial switch t.kind {
		case .Structure:
			return t.structure, true
		case .Named:
			return type_structure_id(unit, t.base, depth + 1)
		}
	}
	return INVALID_STRUCTURE_ID, false
}

type_class_symbol_from_type :: proc(unit: ^Source_File_Provider, id: Type_Id, depth := 0) -> (Symbol_Id, bool) {
	if depth > 16 {
		return INVALID_SYMBOL_ID, false
	}
	if t := type_data(unit, id); t != nil {
		#partial switch t.kind {
		case .Class, .Interface:
			return t.symbol, t.symbol != INVALID_SYMBOL_ID
		case .Named, .Ref:
			return type_class_symbol_from_type(unit, t.base, depth + 1)
		}
	}
	return INVALID_SYMBOL_ID, false
}

type_symbol_for_ref :: proc(
	unit: ^Source_File_Provider,
	scope_id: Scope_Id,
	type_ref_data: Field_Type_Ref_Data,
) -> (Symbol_Id, bool) {
	if symbol_id, ok := type_lookup_scope_chain(unit, scope_id, type_ref_data.namespace, type_ref_data.base_name);
	   ok {
		return symbol_id, true
	}
	if type_ref_data.namespace == .Type {
		if symbol_id, ok := type_lookup_scope_chain(unit, scope_id, .Value, type_ref_data.base_name);
		   ok {
			return symbol_id, true
		}
		if class_symbol, ok := type_enclosing_owner(unit, scope_id, .Class); ok {
			return type_class_symbol(unit, class_symbol, type_ref_data.base_name)
		}
		if interface_symbol, ok := type_enclosing_owner(unit, scope_id, .Interface); ok {
			return type_class_symbol(unit, interface_symbol, type_ref_data.base_name)
		}
	}
	return INVALID_SYMBOL_ID, false
}

type_lookup_scope_chain :: proc(
	unit: ^Source_File_Provider,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (Symbol_Id, bool) {
	current := scope_id
	for current != INVALID_SCOPE_ID {
		if id, ok := scope_lookup_declaration(unit, current, namespace, name); ok {
			return id, true
		}
		s := scope(unit, current)
		if s == nil {
			break
		}
		current = s.parent
	}
	return INVALID_SYMBOL_ID, false
}

type_enclosing_owner :: proc(
	unit: ^Source_File_Provider,
	scope_id: Scope_Id,
	kind: Scope_Kind,
) -> (Symbol_Id, bool) {
	current := scope_id
	for current != INVALID_SCOPE_ID {
		s := scope(unit, current)
		if s == nil {
			break
		}
		if s.kind == kind && s.owner != INVALID_SYMBOL_ID {
			return s.owner, true
		}
		current = s.parent
	}
	return INVALID_SYMBOL_ID, false
}

type_class_symbol :: proc(unit: ^Source_File_Provider, owner: Symbol_Id, name: string) -> (Symbol_Id, bool) {
	if len(unit.scope_index.enclosing_classes) == len(unit.scopes) {
		key := Class_Scope_Index_Key{class_symbol = owner, namespace = .Type, name = name}
		if id, ok := unit.scope_index.class_symbols[key]; ok {
			return id, true
		}
	}
	if scope_id := class_definition_scope(unit, owner); scope_id != INVALID_SCOPE_ID {
		return scope_lookup_declaration(unit, scope_id, .Type, name)
	}
	return INVALID_SYMBOL_ID, false
}

type_selector_at :: #force_inline proc(selectors: []ast.Selector_Op, index: int) -> ast.Selector_Op {
	return selectors[index] if index < len(selectors) else .Dash
}

refresh_unit_type_ids :: proc(unit: ^Source_File_Provider) {
	assert(len(unit.scope_index.enclosing_classes) == len(unit.scopes))
	for i in 0 ..< len(unit.symbols) {
		s := &unit.symbols[i]
		symbol_refresh_type_id(unit, s)
	}
	for i in 0 ..< len(unit.structures) {
		st := &unit.structures[i]
		scope_id := st.scope
		if scope_id == INVALID_SCOPE_ID {
			scope_id = unit.root_scope
		}
		for j in 0 ..< len(st.fields) {
			field := &st.fields[j]
			field.type_id = type_id_from_structure_field(unit, scope_id, field^)
		}
	}
	for i in 0 ..< len(unit.decl_infos) {
		info := &unit.decl_infos[i]
		scope_id := info.signature_scope if info.signature_scope != INVALID_SCOPE_ID else info.scope
		if info.event_source_type.base_name != "" {
			info.event_source_type_id = type_id_from_declared_type(unit, scope_id, info.event_source_type)
		}
		for j in 0 ..< len(info.signature_parameters) {
			param := &info.signature_parameters[j]
			param.type_id = type_id_from_parameter_symbol_or_ref(
				unit,
				param.symbol,
				scope_id,
				param.declared_type,
				param.type_clause_form,
				param.has_type_clause_form,
				.Has_Declared_Type in param.flags,
			)
		}
	}
}

type_id_from_structure_field :: proc(
	unit: ^Source_File_Provider,
	scope_id: Scope_Id,
	field: Structure_Field_Data,
) -> Type_Id {
	if field.structure != INVALID_STRUCTURE_ID {
		return type_structure(unit, field.structure)
	}
	if .Has_Type_Ref in field.flags {
		return type_id_from_declared_type(
			unit,
			scope_id,
			field.type_ref,
			field.type_clause_form,
			field.has_type_clause_form,
		)
	}
	return UNKNOWN_TYPE_ID
}

type_id_from_parameter_symbol_or_ref :: proc(
	unit: ^Source_File_Provider,
	symbol_id: Symbol_Id,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	type_form: ast.Data_Type_Form,
	has_type_form: bool,
	has_type: bool,
) -> Type_Id {
	if s := symbol(unit, symbol_id); s != nil {
		return s.type_id
	}
	if has_type {
		return type_id_from_declared_type(unit, scope_id, type_ref, type_form, has_type_form)
	}
	return UNKNOWN_TYPE_ID
}

builtin_type_ref :: #force_inline proc(name: string) -> Field_Type_Ref_Data {
	return Field_Type_Ref_Data{namespace = .Type, base_name = name}
}
