#+private
package abap_frontend_semantic_analyze

import "src:tokenizer"
import "src:ast"

import "core:mem"

Validation_Lookup :: struct {
	visible:               [][dynamic]Unit_Id,
	predecessors:          [][dynamic]Unit_Id,
	root_by_unit:          map[Root_Symbol_Key]Symbol_Handle,
	global_roots:          map[Root_Name_Key]Symbol_Handle,
	class_members:         map[Class_Member_Lookup_Key]int,
	sql_predicate_columns: map[Sql_Predicate_Column_Key]bool,
}

Root_Symbol_Key :: struct {
	unit:      Unit_Id,
	namespace: Namespace,
	name:      string,
}

Root_Name_Key :: struct {
	namespace: Namespace,
	name:      string,
}

Class_Member_Lookup_Key :: struct {
	unit:         Unit_Id,
	class_symbol: Symbol_Id,
	name:         string,
}

Sql_Predicate_Column_Key :: struct {
	unit:        Unit_Id,
	range_start: int,
	range_end:   int,
	name:        string,
}

build_validation_lookup :: proc(
	project: ^Project_Analysis,
	allocator: mem.Allocator,
) -> Validation_Lookup {
	roots := build_project_root_index(project.units[:], allocator)
	member_hint, sql_hint := 0, 0
	for unit in project.units {
		member_hint += len(unit.class_members)
		sql_hint += len(unit.sql_name_refs)
	}
	lookup := Validation_Lookup {
		visible = include_visible_units_for_units(project.units[:], allocator),
		predecessors = include_predecessor_units_for_units(project.units[:], allocator),
		root_by_unit = make(map[Root_Symbol_Key]Symbol_Handle, len(roots), allocator),
		global_roots = make(map[Root_Name_Key]Symbol_Handle, len(roots), allocator),
		class_members = make(map[Class_Member_Lookup_Key]int, member_hint, allocator),
		sql_predicate_columns = make(map[Sql_Predicate_Column_Key]bool, sql_hint, allocator),
	}
	for entry in roots {
		handle := Symbol_Handle{unit = entry.unit, symbol = entry.symbol}
		root_key := Root_Symbol_Key{unit = entry.unit, namespace = entry.namespace, name = entry.name}
		_, slot, inserted, _ := map_entry(&lookup.root_by_unit, root_key)
		if inserted {
			slot^ = handle
		}
		if entry.visible_by_default {
			global_key := Root_Name_Key{namespace = entry.namespace, name = entry.name}
			_, global_slot, global_inserted, _ := map_entry(&lookup.global_roots, global_key)
			if global_inserted {
				global_slot^ = handle
			}
		}
	}
	for unit in project.units {
		for member, i in unit.class_members {
			key := Class_Member_Lookup_Key {
				unit = unit.unit_id,
				class_symbol = member.class_symbol,
				name = member.name,
			}
			if !(key in lookup.class_members) {
				lookup.class_members[key] = i
			}
		}
		for ref in unit.sql_name_refs {
			if ref.kind == .Column {
				lookup.sql_predicate_columns[sql_predicate_column_key(unit.unit_id, ref.range, ref.name)] = true
			}
		}
	}
	return lookup
}

validate_unit_diagnostics :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	allocator: mem.Allocator,
) -> [dynamic]Diagnostic {
	unit := &project.units[unit_index]
	out := make([dynamic]Diagnostic, 0, 8, allocator)
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	seen := make(map[Diagnostic_Key]bool, 8, context.temp_allocator)
	for diagnostic in unit.diagnostics {
		if retained_collector_diagnostic(diagnostic.kind) {
			append(&out, diagnostic)
			seen[diagnostic_key(diagnostic)] = true
		}
	}
	validate_later_include_type_refs(project, lookup, unit_index, &out, &seen, allocator)
	validate_unresolved_references(project, lookup, unit_index, &out, &seen, allocator)
	validate_create_data_type_handles(project, lookup, unit_index, &out, &seen, allocator)
	validate_object_type_refs(project, lookup, unit_index, &out, &seen, allocator)
	validate_missing_method_implementations(project, unit_index, &out, &seen, allocator)
	validate_generic_table_types(project, unit_index, &out, &seen, allocator)
	validate_parameter_types(project, unit_index, &out, &seen, allocator)
	validate_field_accesses(project, lookup, unit_index, &out, &seen, allocator)
	validate_call_sites(project, lookup, unit_index, &out, &seen, allocator)
	validate_open_sql(project, lookup, unit_index, &out, &seen, allocator)
	return out
}

Diagnostic_Key :: struct {
	kind:        Diagnostic_Kind,
	range_start: int,
	range_end:   int,
	message:     string,
}

diagnostic_key :: #force_inline proc(diagnostic: Diagnostic) -> Diagnostic_Key {
	return diagnostic_key_from_values(diagnostic.kind, diagnostic.range, diagnostic.message)
}

diagnostic_key_from_values :: #force_inline proc(
	kind: Diagnostic_Kind,
	range: tokenizer.Range,
	message: string,
) -> Diagnostic_Key {
	return Diagnostic_Key {
		kind = kind,
		range_start = range.start,
		range_end = range.end,
		message = message,
	}
}

retained_collector_diagnostic :: proc(kind: Diagnostic_Kind) -> bool {
	return(
		kind == .Syntax_Error ||
		kind == .Duplicate_Declaration ||
		kind == .Shadowed_Symbol ||
		kind == .Mismatched_Structured_Declaration ||
		kind == .Unresolved_Include ||
		kind == .Include_Cycle ||
		kind == .Invalid_Open_Sql_Syntax \
	)
}

validate_later_include_type_refs :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	for ref in unit.references {
		if ref.kind != .Type_Ref || ref.namespace != .Type || !ref.has_resolution ||
		   ref.resolution.kind != .Symbol {
			continue
		}
		handle := ref.resolution.symbol
		target_index := unit_id_index(handle.unit)
		if target_index < 0 || target_index >= len(project.units) {
			continue
		}
		target := symbol(&project.units[target_index], handle.symbol)
		if target == nil || !(target.kind == .Type_Def || target.kind == .Class) {
			continue
		}
		if type_decl_after_reference(project, lookup, unit_index, ref.range.start, handle) {
			append_diag(
				out,
				seen,
				.Unresolved_Reference,
				ref.range,
				diagnostic_message("type declared after use: ", ref.name, allocator),
			)
		}
	}
}

type_decl_after_reference :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	ref_start: int,
	target: Symbol_Handle,
) -> bool {
	target_index := unit_id_index(target.unit)
	if target_index == unit_index {
		s := symbol(&project.units[target_index], target.symbol)
		return s != nil && s.decl_range.start > ref_start
	}
	target_unit := &project.units[target_index]
	if s := symbol(target_unit, target.symbol); s != nil && root_symbol_visible_by_default(target_unit, s) {
		return false
	}
	if unit_list_contains(lookup.predecessors[unit_index][:], target.unit) {
		return false
	}
	unit := &project.units[unit_index]
	for edge in unit.include_edges {
		if !edge.has_target {
			continue
		}
		expansion := lookup.visible[unit_id_index(edge.target)]
		if edge.target == target.unit || unit_list_contains(expansion[:], target.unit) {
			return edge.range.start > ref_start
		}
	}
	return true
}

validate_unresolved_references :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	for ref in unit.references {
		if ref.has_resolution || ref.kind == .Include {
			continue
		}
		if ref.namespace == .Value &&
		   sql_predicate_column_name(lookup, unit.unit_id, ref.range, ref.name) {
			continue
		}
		if symbol_exists_in_other_namespace(project, lookup, unit_index, ref) {
			append_diag(
				out,
				seen,
				.Wrong_Namespace,
				ref.range,
				diagnostic_message("wrong namespace for ", ref.name, allocator),
			)
		} else {
			append_diag(
				out,
				seen,
				.Unresolved_Reference,
				ref.range,
				diagnostic_message("unknown symbol ", ref.name, allocator),
			)
		}
	}
}

symbol_exists_in_other_namespace :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	ref: Reference_Data,
) -> bool {
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if namespace == ref.namespace {
			continue
		}
		if _, ok := lookup_scope_chain(
			&project.units[unit_index],
			&project.units[unit_index].scope_index,
			ref.scope,
			namespace,
			ref.name,
		); ok {
			return true
		}
		if _, ok := root_symbol_in_visible_units_lookup(lookup, namespace, ref.name, lookup.visible[unit_index]); ok {
			return true
		}
		if _, ok := global_visible_root_symbol_lookup(lookup, namespace, ref.name); ok {
			return true
		}
	}
	return false
}

validate_create_data_type_handles :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	for site in unit.create_data_type_handles {
		if site.target_name != "" {
			if handle, ok := value_handle_for_name(project, lookup, unit_index, site.scope, site.target_name);
			   ok && create_data_target_is_invalid(project, lookup, unit_index, handle) {
				append_diag(
					out,
					seen,
					.Invalid_Create_Data_Target,
					site.target_range,
					diagnostic_message("CREATE DATA target must be a data reference: ", site.target_name, allocator),
				)
			}
		}
		if handle, ok := value_handle_for_name(project, lookup, unit_index, site.scope, site.handle_name);
		   ok && !create_data_handle_is_datadescr_ref(project, lookup, unit_index, handle) {
			append_diag(
				out,
				seen,
				.Invalid_Create_Data_Type_Handle,
				site.handle_range,
				diagnostic_message("TYPE HANDLE operand must be REF TO cl_abap_datadescr or subclass: ", site.handle_name, allocator),
			)
		}
	}
}

create_data_target_is_invalid :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	handle: Symbol_Handle,
) -> bool {
	s := symbol_for_project_handle(project, handle)
	if s == nil || !s.has_declared_type {
		return false
	}
	return !s.declared_type.is_ref || type_ref_is_object_ref(project, lookup, unit_index, s.declared_type)
}

create_data_handle_is_datadescr_ref :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	handle: Symbol_Handle,
) -> bool {
	s := symbol_for_project_handle(project, handle)
	if s == nil || !s.has_declared_type || !s.declared_type.is_ref {
		return false
	}
	if s.declared_type.base_name == "cl_abap_datadescr" {
		return true
	}
	if is_builtin_type_name(s.declared_type.base_name) {
		return false
	}
	type_handle, ok := resolve_type_ref_handle_project_lookup(project, lookup, unit_index, s.declared_type)
	return !ok || class_is_or_inherits_from_name(project, lookup, type_handle, "cl_abap_datadescr")
}

type_ref_is_object_ref :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	type_ref: Field_Type_Ref_Data,
) -> bool {
	if type_ref.base_name == "object" {
		return true
	}
	handle, ok := resolve_type_ref_handle_project_lookup(project, lookup, unit_index, type_ref)
	if !ok {
		return false
	}
	s := symbol_for_project_handle(project, handle)
	return s != nil && (s.kind == .Class || s.kind == .Interface)
}

class_is_or_inherits_from_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	handle: Symbol_Handle,
	name: string,
) -> bool {
	current := handle
	for {
		s := symbol_for_project_handle(project, current)
		if s != nil && s.name == name {
			return true
		}
		next, ok := direct_superclass_handle_lookup(project, lookup, current)
		if !ok {
			return false
		}
		current = next
	}
}

symbol_for_project_handle :: proc(project: ^Project_Analysis, handle: Symbol_Handle) -> ^Symbol_Data {
	unit_index := unit_id_index(handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return nil
	}
	return symbol(&project.units[unit_index], handle.symbol)
}

validate_object_type_refs :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	for s in unit.symbols {
		if !s.has_declared_type ||
		   s.declared_type.namespace != .Type ||
		   s.declared_type.is_ref ||
		   len(s.declared_type.field_path) > 0 {
			continue
		}
		handle, ok := resolve_type_ref_handle_project_lookup(project, lookup, unit_index, s.declared_type)
		if !ok {
			continue
		}
		target := symbol(&project.units[unit_id_index(handle.unit)], handle.symbol)
		if target != nil && (target.kind == .Class || target.kind == .Interface) {
			append_diag(
				out,
				seen,
				.Invalid_Object_Type_Reference,
				s.decl_range,
				diagnostic_message("object type needs REF TO: ", s.declared_type.base_name, allocator),
			)
		}
	}
}

validate_missing_method_implementations :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	if unit.source_mode == .Dependency_Interface {
		return
	}
	for definition in unit.class_definitions {
		class_symbol := symbol(unit, definition.class_symbol)
		if class_symbol == nil || class_symbol.kind != .Class || definition.is_abstract {
			continue
		}
		for member in unit.class_members {
			if member.class_symbol == definition.class_symbol &&
			   member.kind == .Method &&
			   !(.Has_Implementation in member.flags) {
				append_diag(
					out,
					seen,
					.Missing_Method_Implementation,
					member.decl_range,
					diagnostic_message("missing method implementation: ", member.name, allocator),
				)
			}
		}
	}
}

validate_generic_table_types :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	for s in unit.symbols {
		if !generic_table_category_type(s) ||
		   s.kind == .Parameter ||
		   s.kind == .Field_Symbol {
			continue
		}
		append_diag(
			out,
			seen,
			.Invalid_Generic_Table_Type,
			s.decl_range,
			diagnostic_message("generic table type only allowed for parameters and field symbols: ", s.name, allocator),
		)
	}
}

generic_table_category_type :: #force_inline proc "contextless" (s: Symbol_Data) -> bool {
	return s.has_type_clause_form &&
	       (s.type_clause_form == .Any_Table || s.type_clause_form == .Index_Table)
}

validate_parameter_types :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	for s in unit.symbols {
		if s.kind == .Parameter && parameter_type_uses_inline_table_type(s.type_clause_form) {
			append_diag(
				out,
				seen,
				.Invalid_Parameter_Type,
				s.decl_range,
				diagnostic_message("invalid inline table parameter type: ", s.name, allocator),
			)
		}
	}
}

parameter_type_uses_inline_table_type :: #force_inline proc "contextless" (form: ast.Data_Type_Form) -> bool {
	return form == .Table || form == .Like_Table
}

validate_field_accesses :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	for access in project.units[unit_index].field_accesses {
		if len(access.field_path) == 0 || access.in_type_position {
			continue
		}
		if _, ok := resolve_field_access_tail(project, lookup, unit_index, access); !ok {
			field := access.field_path[len(access.field_path) - 1]
			append_diag(
				out,
				seen,
				.Unknown_Field,
				field.range,
				diagnostic_message("unknown field ", field.name, allocator),
			)
		}
	}
}

validate_call_sites :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	for site in project.units[unit_index].call_sites {
		if site.target.kind != .Method || site.target.method_name == "" {
			continue
		}
		class_handle, ok := class_handle_for_call_target(project, lookup, unit_index, site)
		if !ok {
			continue
		}
		member, member_ok := class_member_in_hierarchy(
			project,
			lookup,
			class_handle,
			site.target.method_name,
			false,
		)
		if !member_ok {
			if implicit_super_constructor_call(site) {
				continue
			}
			append_diag(
				out,
				seen,
				.Unknown_Field,
				method_target_range(site),
				diagnostic_message("unknown method ", site.target.method_name, allocator),
			)
			continue
		}
		if member.kind != .Method {
			append_diag(
				out,
				seen,
				.Unknown_Field,
				method_target_range(site),
				diagnostic_message("member is not a method: ", site.target.method_name, allocator),
			)
		}
	}
}

implicit_super_constructor_call :: proc(site: Call_Site_Data) -> bool {
	return site.target.base_name == "super" && site.target.method_name == "constructor"
}

method_target_range :: proc(site: Call_Site_Data) -> tokenizer.Range {
	if site.target.method_range.start < site.target.method_range.end {
		return site.target.method_range
	}
	return site.range
}

validate_open_sql :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	for source_data in unit.sql_sources {
		if source_data.resolution != .External {
			continue
		}
		if _, ok := resolve_type_name_in_project_lookup(project, lookup, unit_index, source_data.name); !ok {
			append_diag(
				out,
				seen,
				.Unverified_Open_Sql_Source,
				source_data.range,
				diagnostic_message("unverified Open SQL source ", source_data.name, allocator),
			)
		}
	}
	for name_ref in unit.sql_name_refs {
		if !(name_ref.kind == .Column || name_ref.kind == .Qualified_Column) {
			continue
		}
		source_data, ok := sql_source_for_name_ref(unit, name_ref)
		if !ok {
			continue
		}
		source_handle, source_ok := resolve_type_name_in_project_lookup(project, lookup, unit_index, source_data.name)
		if !source_ok {
			continue
		}
		source_unit := &project.units[unit_id_index(source_handle.unit)]
		source_symbol := symbol(source_unit, source_handle.symbol)
		if source_symbol == nil || source_symbol.structure == INVALID_STRUCTURE_ID {
			continue
		}
		if structure_field(source_unit, source_symbol.structure, name_ref.name) == nil {
			append_diag(
				out,
				seen,
				.Unknown_Field,
				name_ref.range,
				diagnostic_message("unknown Open SQL field ", name_ref.name, allocator),
			)
		}
	}
}

sql_source_for_name_ref :: proc(
	unit: ^Unit_Analysis,
	ref: Sql_Name_Ref_Data,
) -> (^Sql_Source_Data, bool) {
	for &source_data in unit.sql_sources {
		if ref.qualifier != "" {
			if source_data.alias == ref.qualifier || source_data.name == ref.qualifier {
				return &source_data, true
			}
			continue
		}
		return &source_data, true
	}
	return nil, false
}

sql_predicate_column_name :: proc(
	lookup: ^Validation_Lookup,
	unit_id: Unit_Id,
	range: tokenizer.Range,
	name: string,
) -> bool {
	return sql_predicate_column_key(unit_id, range, name) in lookup.sql_predicate_columns
}

sql_predicate_column_key :: #force_inline proc(
	unit_id: Unit_Id,
	range: tokenizer.Range,
	name: string,
) -> Sql_Predicate_Column_Key {
	return Sql_Predicate_Column_Key {
		unit = unit_id,
		range_start = range.start,
		range_end = range.end,
		name = name,
	}
}

class_handle_for_call_target :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	site: Call_Site_Data,
) -> (Symbol_Handle, bool) {
	if len(site.target.receiver_path) > 0 {
		fact, ok := resolve_field_access_tail(
			project,
			lookup,
			unit_index,
			Field_Access {
				scope = site.scope,
				base_namespace = site.target.base_namespace,
				base_name = site.target.base_name,
				field_path = site.target.receiver_path,
			},
		)
		if !ok {
			return {}, false
		}
		return class_handle_from_type_fact(project, lookup, unit_index, fact)
	}
	if site.target.base_namespace == .Type {
		return resolve_type_name_in_project_lookup(project, lookup, unit_index, site.target.base_name)
	}
	if site.target.base_name == "super" {
		class_symbol, ok := enclosing_instance_method_class_owner_unit(&project.units[unit_index], site.scope)
		if !ok {
			return {}, false
		}
		return direct_superclass_handle_lookup(
			project,
			lookup,
			Symbol_Handle{unit = project.units[unit_index].unit_id, symbol = class_symbol},
		)
	}
	handle, ok := value_handle_for_name(
		project,
		lookup,
		unit_index,
		site.scope,
		site.target.base_name,
	)
	if !ok {
		return {}, false
	}
	return class_handle_from_symbol(project, lookup, unit_index, handle)
}

class_handle_from_symbol :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	site_unit_index: int,
	handle: Symbol_Handle,
) -> (Symbol_Handle, bool) {
	unit_index := unit_id_index(handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return {}, false
	}
	s := symbol(&project.units[unit_index], handle.symbol)
	if s == nil || !s.has_declared_type {
		return {}, false
	}
	line_of := s.has_type_clause_form && type_form_is_line_of(s.type_clause_form)
	return class_handle_from_declared_type(project, lookup, site_unit_index, s.declared_type, line_of, 0)
}

class_handle_from_type_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	fact: Type_Fact_Data,
) -> (Symbol_Handle, bool) {
	if !fact.has_declared_type {
		return {}, false
	}
	return class_handle_from_declared_type(project, lookup, unit_index, fact.declared_type, false, 0)
}

class_handle_from_declared_type :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	type_ref: Field_Type_Ref_Data,
	line_of: bool,
	depth: int,
) -> (Symbol_Handle, bool) {
	if depth > len(project.units) + 16 {
		return {}, false
	}
	handle, ok := resolve_type_ref_leaf_handle_project_lookup(project, lookup, unit_index, type_ref)
	if !ok {
		return {}, false
	}
	s := symbol_for_project_handle(project, handle)
	if s == nil {
		return {}, false
	}
	if line_of {
		if !s.has_declared_type {
			return {}, false
		}
		next_line_of := !(s.has_type_clause_form && type_form_is_table(s.type_clause_form))
		return class_handle_from_declared_type(
			project,
			lookup,
			unit_index,
			s.declared_type,
			next_line_of,
			depth + 1,
		)
	}
	if s.kind == .Class || s.kind == .Interface {
		return handle, true
	}
	if !s.has_declared_type {
		return {}, false
	}
	next_line_of := s.has_type_clause_form && type_form_is_line_of(s.type_clause_form)
	return class_handle_from_declared_type(
		project,
		lookup,
		unit_index,
		s.declared_type,
		next_line_of,
		depth + 1,
	)
}

resolve_type_ref_leaf_handle_project_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	type_ref: Field_Type_Ref_Data,
) -> (Symbol_Handle, bool) {
	handle, ok := resolve_type_ref_handle_project_lookup(project, lookup, unit_index, type_ref)
	if !ok {
		return {}, false
	}
	for name in type_ref.field_path {
		next, next_ok := class_type_symbol_handle(project.units[:], handle, name)
		if !next_ok {
			return {}, false
		}
		handle = next
	}
	return handle, true
}

type_form_is_line_of :: proc(form: ast.Data_Type_Form) -> bool {
	return form == .Like_Line_Of || form == .Type_Line_Of
}

type_form_is_table :: proc(form: ast.Data_Type_Form) -> bool {
	#partial switch form {
	case .Any_Table,
	     .Table,
	     .Like_Table,
	     .Index_Table,
	     .Standard_Table,
	     .Sorted_Table,
	     .Hashed_Table,
	     .Like_Standard_Table,
	     .Like_Sorted_Table,
	     .Like_Hashed_Table,
	     .Range_Of:
		return true
	}
	return false
}

value_handle_for_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	scope_id: Scope_Id,
	name: string,
) -> (Symbol_Handle, bool) {
	if symbol_id, ok := lookup_scope_chain(
		&project.units[unit_index],
		&project.units[unit_index].scope_index,
		scope_id,
		.Value,
		name,
	); ok {
		return Symbol_Handle{unit = project.units[unit_index].unit_id, symbol = symbol_id}, true
	}
	if symbol_id, ok := current_class_value_symbol(project, unit_index, scope_id, name); ok {
		return Symbol_Handle{unit = project.units[unit_index].unit_id, symbol = symbol_id}, true
	}
	if handle, ok := value_alias_handle_for_name(project, lookup, unit_index, scope_id, name); ok {
		return handle, true
	}
	if handle, ok := inherited_value_handle_for_name(project, lookup, unit_index, scope_id, name);
	   ok {
		return handle, true
	}
	if handle, ok := inherited_value_alias_handle_for_name(project, lookup, unit_index, scope_id, name);
	   ok {
		return handle, true
	}
	if handle, ok := root_symbol_in_visible_units_lookup(lookup, .Value, name, lookup.visible[unit_index]); ok {
		return handle, true
	}
	return global_visible_root_symbol_lookup(lookup, .Value, name)
}

current_class_value_symbol :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	scope_id: Scope_Id,
	name: string,
) -> (Symbol_Id, bool) {
	class_symbol, ok := enclosing_class_owner_unit(&project.units[unit_index], scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	return class_scope_symbol(&project.units[unit_index].scope_index, class_symbol, .Value, name)
}

inherited_value_handle_for_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	scope_id: Scope_Id,
	name: string,
) -> (Symbol_Handle, bool) {
	class_symbol, ok := enclosing_class_owner_unit(&project.units[unit_index], scope_id)
	if !ok {
		return {}, false
	}
	current := Symbol_Handle{unit = project.units[unit_index].unit_id, symbol = class_symbol}
	for _ in 0 ..< len(project.units) + 8 {
		next, next_ok := direct_superclass_handle_lookup(project, lookup, current)
		if !next_ok {
			return {}, false
		}
		next_unit_index := unit_id_index(next.unit)
		if next_unit_index < 0 || next_unit_index >= len(project.units) {
			return {}, false
		}
		member := unit_class_member_lookup(project, lookup, next, name)
		if member != nil && member.kind == .Attribute && member.visibility != .Private {
			if symbol_id, symbol_ok := class_scope_symbol(
				&project.units[next_unit_index].scope_index,
				next.symbol,
				.Value,
				name,
			); symbol_ok {
				return Symbol_Handle{unit = next.unit, symbol = symbol_id}, true
			}
		}
		current = next
	}
	return {}, false
}

value_alias_handle_for_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	scope_id: Scope_Id,
	name: string,
) -> (Symbol_Handle, bool) {
	class_symbol, ok := enclosing_class_owner_unit(&project.units[unit_index], scope_id)
	if !ok {
		return {}, false
	}
	class_handle := Symbol_Handle{unit = project.units[unit_index].unit_id, symbol = class_symbol}
	return class_alias_symbol_by_handle_lookup(project, lookup, unit_index, class_handle, .Value, name)
}

inherited_value_alias_handle_for_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	scope_id: Scope_Id,
	name: string,
) -> (Symbol_Handle, bool) {
	class_symbol, ok := enclosing_class_owner_unit(&project.units[unit_index], scope_id)
	if !ok {
		return {}, false
	}
	current := Symbol_Handle{unit = project.units[unit_index].unit_id, symbol = class_symbol}
	for _ in 0 ..< len(project.units) + 8 {
		next, next_ok := direct_superclass_handle_lookup(project, lookup, current)
		if !next_ok {
			return {}, false
		}
		next_unit_index := unit_id_index(next.unit)
		if next_unit_index < 0 || next_unit_index >= len(project.units) {
			return {}, false
		}
		if handle, found := class_alias_symbol_by_handle_lookup(
			project,
			lookup,
			next_unit_index,
			next,
			.Value,
			name,
		); found {
			return handle, true
		}
		current = next
	}
	return {}, false
}

class_alias_symbol_by_handle_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	class_handle: Symbol_Handle,
	namespace: Namespace,
	name: string,
) -> (Symbol_Handle, bool) {
	for alias in project.units[unit_index].member_aliases {
		if alias.owner_symbol != class_handle.symbol || alias.alias_name != name {
			continue
		}
		interface_handle, interface_ok := resolve_type_name_in_project_lookup(
			project,
			lookup,
			unit_index,
			alias.target_interface_name,
		)
		if !interface_ok {
			continue
		}
		interface_unit_index := unit_id_index(interface_handle.unit)
		if interface_unit_index < 0 || interface_unit_index >= len(project.units) {
			continue
		}
		member_name := alias.target_member_name
		if member_name == "" {
			member_name = name
		}
		if symbol_id, member_ok := class_scope_symbol(
			&project.units[interface_unit_index].scope_index,
			interface_handle.symbol,
			namespace,
			member_name,
		); member_ok {
			return Symbol_Handle{unit = interface_handle.unit, symbol = symbol_id}, true
		}
	}
	return {}, false
}

resolve_field_access_tail :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	access: Field_Access,
) -> (Type_Fact_Data, bool) {
	if access.base_namespace == .Type {
		class_handle, ok := resolve_type_name_in_project_lookup(project, lookup, unit_index, access.base_name)
		if !ok {
			return {}, false
		}
		return type_fact_from_class_member_path(project, lookup, class_handle, access.field_path[:])
	}
	if access.base_name == "super" {
		class_symbol, class_ok := enclosing_instance_method_class_owner_unit(
			&project.units[unit_index],
			access.scope,
		)
		if !class_ok {
			return {}, false
		}
		super_handle, super_ok := direct_superclass_handle_lookup(
			project,
			lookup,
			Symbol_Handle{unit = project.units[unit_index].unit_id, symbol = class_symbol},
		)
		if !super_ok {
			return {}, false
		}
		return type_fact_from_class_member_path(project, lookup, super_handle, access.field_path[:])
	}
	base, ok := value_handle_for_name(project, lookup, unit_index, access.scope, access.base_name)
	if !ok {
		return {}, false
	}
	base_unit := &project.units[unit_id_index(base.unit)]
	base_symbol := symbol(base_unit, base.symbol)
	if base_symbol == nil {
		return {}, false
	}
	if base_symbol.structure != INVALID_STRUCTURE_ID {
		return type_fact_from_structure_path(project, base_unit, base_symbol.structure, access.field_path[:])
	}
	if base_symbol.has_declared_type {
		if class_handle, class_ok := class_handle_from_symbol(project, lookup, unit_index, base);
		   class_ok {
			return type_fact_from_class_member_path(project, lookup, class_handle, access.field_path[:])
		}
	}
	return {}, false
}

type_fact_from_structure_path :: proc(
	project: ^Project_Analysis,
	start_unit: ^Unit_Analysis,
	start_structure: Structure_Id,
	path: []Field_Access_Segment,
) -> (Type_Fact_Data, bool) {
	current_unit := start_unit
	current_structure := start_structure
	fact := Type_Fact_Data{structure = current_structure}
	for segment in path {
		field := structure_field(current_unit, current_structure, segment.name)
		if field == nil {
			return {}, false
		}
		fact = Type_Fact_Data {
			structure = field.structure,
			declared_type = field.type_ref,
			has_declared_type = .Has_Type_Ref in field.flags,
			type_clause_display = field.type_ref.base_name,
		}
		if field.structure == INVALID_STRUCTURE_ID {
			continue
		}
		next_unit_index := unit_id_index(field.decl_unit)
		if next_unit_index >= 0 && next_unit_index < len(project.units) {
			current_unit = &project.units[next_unit_index]
		}
		current_structure = field.structure
	}
	return fact, true
}

type_fact_from_class_member_path :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	class_handle: Symbol_Handle,
	path: []Field_Access_Segment,
) -> (Type_Fact_Data, bool) {
	if len(path) == 0 {
		return unknown_type_fact(), true
	}
	member, ok := class_member_in_hierarchy(project, lookup, class_handle, path[0].name, true)
	if !ok {
		if fact, builtin_ok := builtin_class_member_type_fact(project, class_handle, path[0].name);
		   builtin_ok {
			if len(path) == 1 {
				return fact, true
			}
			if fact.structure == INVALID_STRUCTURE_ID {
				return {}, false
			}
			member_unit := &project.units[unit_id_index(class_handle.unit)]
			return type_fact_from_structure_path(project, member_unit, fact.structure, path[1:])
		}
		return {}, false
	}
	fact := class_member_type_fact(member)
	if len(path) == 1 {
		return fact, true
	}
	member_unit := &project.units[unit_id_index(class_handle.unit)]
	if fact.structure == INVALID_STRUCTURE_ID {
		return {}, false
	}
	return type_fact_from_structure_path(project, member_unit, fact.structure, path[1:])
}

class_member_in_hierarchy :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	class_handle: Symbol_Handle,
	name: string,
	inherited: bool,
) -> (^Class_Member_Data, bool) {
	if member := unit_class_member_lookup(project, lookup, class_handle, name); member != nil {
		if inherited && member.visibility == .Private {
			return nil, false
		}
		return member, true
	}
	if member, ok := interface_member_in_class(project, lookup, class_handle, name); ok {
		return member, true
	}
	next, ok := direct_superclass_handle_lookup(project, lookup, class_handle)
	if !ok {
		return nil, false
	}
	return class_member_in_hierarchy(project, lookup, next, name, true)
}

interface_member_in_class :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	class_handle: Symbol_Handle,
	name: string,
) -> (^Class_Member_Data, bool) {
	unit_index := unit_id_index(class_handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return nil, false
	}
	unit := &project.units[unit_index]
	for alias in unit.member_aliases {
		if alias.owner_symbol != class_handle.symbol || alias.alias_name != name {
			continue
		}
		if member, ok := interface_member_by_name(
			project,
			lookup,
			unit_index,
			alias.target_interface_name,
			alias.target_member_name,
		); ok {
			return member, true
		}
	}
	for implemented in unit.implemented_interfaces {
		if implemented.owner_symbol != class_handle.symbol {
			continue
		}
		if member, ok := interface_member_by_name(
			project,
			lookup,
			unit_index,
			implemented.interface_name,
			name,
		); ok {
			return member, true
		}
	}
	return nil, false
}

interface_member_by_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	interface_name, member_name: string,
) -> (^Class_Member_Data, bool) {
	handle, ok := resolve_type_name_in_project_lookup(project, lookup, unit_index, interface_name)
	if !ok {
		return nil, false
	}
	member := unit_class_member_lookup(project, lookup, handle, member_name)
	if member == nil {
		return nil, false
	}
	return member, true
}

resolve_type_name_in_project_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	name: string,
) -> (Symbol_Handle, bool) {
	unit_id := project.units[unit_index].unit_id
	if handle, ok := root_symbol_in_unit_lookup(lookup, unit_id, .Type, name); ok {
		return handle, true
	}
	if handle, ok := root_symbol_in_visible_units_lookup(lookup, .Type, name, lookup.visible[unit_index]); ok {
		return handle, true
	}
	return global_visible_root_symbol_lookup(lookup, .Type, name)
}

resolve_function_module_in_project_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	name: string,
) -> (Symbol_Handle, bool) {
	unit_id := project.units[unit_index].unit_id
	if handle, ok := root_symbol_in_unit_lookup(lookup, unit_id, .Routine, name); ok && symbol_handle_is_kind(project, handle, .Module) {
		return handle, true
	}
	for visible in lookup.visible[unit_index] {
		if handle, ok := root_symbol_in_unit_lookup(lookup, visible, .Routine, name); ok && symbol_handle_is_kind(project, handle, .Module) {
			return handle, true
		}
	}
	if handle, ok := global_visible_root_symbol_lookup(lookup, .Routine, name); ok && symbol_handle_is_kind(project, handle, .Module) {
		return handle, true
	}
	return {}, false
}

symbol_handle_is_kind :: proc(project: ^Project_Analysis, handle: Symbol_Handle, kind: Symbol_Kind) -> bool {
	unit_index := unit_id_index(handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return false
	}
	s := symbol(&project.units[unit_index], handle.symbol)
	return s != nil && s.kind == kind
}

resolve_type_ref_handle_project_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	type_ref: Field_Type_Ref_Data,
) -> (Symbol_Handle, bool) {
	all_namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in all_namespaces {
		if !(namespace == type_ref.namespace ||
		     (type_ref.namespace == .Value && namespace == .Type)) {
			continue
		}
		unit_id := project.units[unit_index].unit_id
		if handle, ok := root_symbol_in_unit_lookup(lookup, unit_id, namespace, type_ref.base_name);
		   ok {
			return handle, true
		}
		if handle, ok := root_symbol_in_visible_units_lookup(
			lookup,
			namespace,
			type_ref.base_name,
			lookup.visible[unit_index],
		); ok {
			return handle, true
		}
		if handle, ok := global_visible_root_symbol_lookup(lookup, namespace, type_ref.base_name);
		   ok {
			return handle, true
		}
	}
	return {}, false
}

direct_superclass_handle_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	current: Symbol_Handle,
) -> (Symbol_Handle, bool) {
	unit_index := unit_id_index(current.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return {}, false
	}
	super_name, ok := class_superclass_name(&project.units[unit_index], current.symbol)
	if !ok {
		return {}, false
	}
	return resolve_type_name_in_project_lookup(project, lookup, unit_index, super_name)
}

root_symbol_in_unit_lookup :: #force_inline proc(
	lookup: ^Validation_Lookup,
	unit_id: Unit_Id,
	namespace: Namespace,
	name: string,
) -> (Symbol_Handle, bool) {
	key := Root_Symbol_Key{unit = unit_id, namespace = namespace, name = name}
	if handle, ok := lookup.root_by_unit[key]; ok {
		return handle, true
	}
	return {}, false
}

root_symbol_in_visible_units_lookup :: proc(
	lookup: ^Validation_Lookup,
	namespace: Namespace,
	name: string,
	visible: [dynamic]Unit_Id,
) -> (Symbol_Handle, bool) {
	for unit_id in visible {
		if handle, ok := root_symbol_in_unit_lookup(lookup, unit_id, namespace, name); ok {
			return handle, true
		}
	}
	return {}, false
}

global_visible_root_symbol_lookup :: #force_inline proc(
	lookup: ^Validation_Lookup,
	namespace: Namespace,
	name: string,
) -> (Symbol_Handle, bool) {
	key := Root_Name_Key{namespace = namespace, name = name}
	if handle, ok := lookup.global_roots[key]; ok {
		return handle, true
	}
	return {}, false
}

unit_class_member_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	class_handle: Symbol_Handle,
	name: string,
) -> ^Class_Member_Data {
	unit_index := unit_id_index(class_handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return nil
	}
	key := Class_Member_Lookup_Key {
		unit = class_handle.unit,
		class_symbol = class_handle.symbol,
		name = name,
	}
	if member_index, ok := lookup.class_members[key]; ok {
		return &project.units[unit_index].class_members[member_index]
	}
	return nil
}

builtin_class_member_type_fact :: proc(
	project: ^Project_Analysis,
	class_handle: Symbol_Handle,
	member_name: string,
) -> (Type_Fact_Data, bool) {
	unit_index := unit_id_index(class_handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return {}, false
	}
	s := symbol(&project.units[unit_index], class_handle.symbol)
	if s == nil {
		return {}, false
	}
	return builtin_class_attribute_type_fact(s.name, member_name)
}

class_member_type_fact :: proc(member: ^Class_Member_Data) -> Type_Fact_Data {
	if member == nil {
		return unknown_type_fact()
	}
	if member.kind == .Attribute {
		return Type_Fact_Data{structure = member.structure}
	}
	for param in member.parameters {
		if param.section == .Returning || param.section == .Receiving {
			return Type_Fact_Data {
				structure = INVALID_STRUCTURE_ID,
				declared_type = param.declared_type,
				has_declared_type = .Has_Declared_Type in param.flags,
				type_clause_display = param.type_clause_display,
			}
		}
	}
	return unknown_type_fact()
}

append_diag :: proc(
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	kind: Diagnostic_Kind,
	range: tokenizer.Range,
	message: string,
) {
	key := diagnostic_key_from_values(kind, range, message)
	if key in seen^ {
		return
	}
	seen^[key] = true
	append(out, Diagnostic{kind = kind, range = range, message = message})
}
