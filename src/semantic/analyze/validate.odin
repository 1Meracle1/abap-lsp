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
	validate_generic_builtin_types(project, unit_index, &out, &seen, allocator)
	validate_generic_table_types(project, unit_index, &out, &seen, allocator)
	validate_parameter_types(project, unit_index, &out, &seen, allocator)
	validate_field_accesses(project, lookup, unit_index, &out, &seen, allocator)
	validate_call_sites(project, lookup, unit_index, &out, &seen, allocator)
	validate_open_sql(project, lookup, unit_index, &out, &seen, allocator)
	validate_at_groups(project, unit_index, &out, &seen)
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
	if s := symbol(target_unit, target.symbol); s != nil {
		if owner_scope := scope(target_unit, s.scope);
		   owner_scope != nil &&
		   (owner_scope.kind == .Class || owner_scope.kind == .Interface) &&
		   owner_scope.owner != INVALID_SYMBOL_ID {
			if owner := symbol(target_unit, owner_scope.owner);
			   owner != nil && root_symbol_visible_by_default(target_unit, owner) {
				return false
			}
		}
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
	handle, ok := resolve_type_ref_leaf_handle_project_lookup(project, lookup, unit_index, type_ref)
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

declared_type_has_unknown_shape :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	depth := 0,
) -> bool {
	if depth > len(project.units) + 16 {
		return false
	}
	if type_ref.base_name == "" {
		return false
	}
	if is_generic_builtin_type_name(type_ref.base_name) {
		return true
	}
	if is_builtin_type_name(type_ref.base_name) {
		return false
	}
	handle, ok := type_ref_symbol_handle(project, lookup, unit_index, scope_id, type_ref)
	if !ok {
		return true
	}
	if len(type_ref.field_path) == 0 {
		return type_ref_handle_has_unknown_shape(project, lookup, handle, depth + 1)
	}
	return type_ref_path_has_unknown_shape(
		project,
		lookup,
		handle,
		type_ref.field_path[:],
		type_ref.field_selectors[:],
		type_ref.field_derefs[:],
		depth + 1,
	)
}

type_ref_handle_has_unknown_shape :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	handle: Symbol_Handle,
	depth: int,
) -> bool {
	unit_index := unit_id_index(handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return true
	}
	s := symbol(&project.units[unit_index], handle.symbol)
	if s == nil {
		return true
	}
	if s.structure != INVALID_STRUCTURE_ID || s.kind == .Class || s.kind == .Interface {
		return false
	}
	if s.has_declared_type {
		return declared_type_has_unknown_shape(project, lookup, unit_index, s.scope, s.declared_type, depth + 1)
	}
	return s.kind == .Field_Symbol
}

type_ref_path_has_unknown_shape :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	handle: Symbol_Handle,
	path: []string,
	selectors: []ast.Selector_Op,
	derefs: []bool,
	depth: int,
) -> bool {
	unit_index := unit_id_index(handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return true
	}
	unit := &project.units[unit_index]
	s := symbol(unit, handle.symbol)
	if s == nil {
		return true
	}
	if s.kind == .Class || s.kind == .Interface {
		current := handle
		for name in path {
			next, ok := class_type_symbol_handle(project.units[:], current, name)
			if !ok {
				return true
			}
			current = next
		}
		return false
	}
	if s.structure == INVALID_STRUCTURE_ID {
		return type_ref_handle_has_unknown_shape(project, lookup, handle, depth + 1)
	}
	current_unit := unit
	current_structure := s.structure
	for name, i in path {
		if i < len(derefs) && derefs[i] {
			continue
		}
		if selector_at(selectors, i) != .Dash {
			return false
		}
		field := structure_field(current_unit, current_structure, name)
		if field == nil {
			return true
		}
		if field.structure == INVALID_STRUCTURE_ID {
			next_unit_index := unit_id_index(field.decl_unit)
			if .Has_Type_Ref in field.flags &&
			   next_unit_index >= 0 &&
			   next_unit_index < len(project.units) &&
			   declared_type_has_unknown_shape(
				   project,
				   lookup,
				   next_unit_index,
				   INVALID_SCOPE_ID,
				   field.type_ref,
				   depth + 1,
			   ) {
				return true
			}
		}
		next_unit_index := unit_id_index(field.decl_unit)
		if next_unit_index >= 0 && next_unit_index < len(project.units) {
			current_unit = &project.units[next_unit_index]
		}
		current_structure = field.structure
	}
	return false
}

type_ref_symbol_handle :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
) -> (Symbol_Handle, bool) {
	if scope_id != INVALID_SCOPE_ID {
		namespaces := [?]Namespace{.Value, .Type, .Routine}
		for namespace in namespaces {
			if !type_ref_namespace_matches(type_ref.namespace, namespace) {
				continue
			}
			if symbol_id, ok := lookup_scope_chain(
				&project.units[unit_index],
				&project.units[unit_index].scope_index,
				scope_id,
				namespace,
				type_ref.base_name,
			); ok {
				return Symbol_Handle{unit = project.units[unit_index].unit_id, symbol = symbol_id}, true
			}
		}
	}
	return resolve_type_ref_handle_project_lookup(project, lookup, unit_index, type_ref)
}

type_ref_namespace_matches :: #force_inline proc "contextless" (want, got: Namespace) -> bool {
	return want == got || (want == .Value && got == .Type) || (want == .Type && got == .Value)
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

validate_generic_builtin_types :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	for s in unit.symbols {
		if symbol_kind_is_builtin(s.kind) ||
		   !s.has_declared_type ||
		   s.declared_type.namespace != .Type ||
		   len(s.declared_type.field_path) > 0 {
			continue
		}
		if s.declared_type.base_name == "object" && !s.declared_type.is_ref {
			append_diag(
				out,
				seen,
				.Invalid_Object_Type_Reference,
				type_ref_or_decl_range(s),
				diagnostic_message("object type needs REF TO: ", s.declared_type.base_name, allocator),
			)
			continue
		}
		if !invalid_generic_builtin_type_use(s) {
			continue
		}
		message := "generic type only allowed for parameters and field symbols: "
		if s.declared_type.is_ref {
			message = "generic type not allowed after REF TO: "
		}
		append_diag(
			out,
			seen,
			.Invalid_Generic_Builtin_Type,
			type_ref_or_decl_range(s),
			diagnostic_message(message, s.declared_type.base_name, allocator),
		)
	}
}

invalid_generic_builtin_type_use :: proc "contextless" (s: Symbol_Data) -> bool {
	if !is_generic_builtin_type_name(s.declared_type.base_name) {
		return false
	}
	if s.declared_type.is_ref {
		return !is_generic_builtin_ref_type_name(s.declared_type.base_name)
	}
	return s.kind != .Parameter && s.kind != .Field_Symbol
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
	       (s.type_clause_form == .Any_Table ||
	        s.type_clause_form == .Index_Table ||
	        (!s.has_declared_type &&
	         (s.type_clause_form == .Table ||
	          s.type_clause_form == .Standard_Table ||
	          s.type_clause_form == .Sorted_Table ||
	          s.type_clause_form == .Hashed_Table)))
}

type_ref_or_decl_range :: #force_inline proc "contextless" (s: Symbol_Data) -> tokenizer.Range {
	return s.declared_type.base_range if s.declared_type.base_range.start != s.declared_type.base_range.end else s.decl_range
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
		if s.kind == .Parameter && parameter_type_uses_inline_table_type(s) {
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

parameter_type_uses_inline_table_type :: #force_inline proc "contextless" (s: Symbol_Data) -> bool {
	return s.type_clause_table_has_of && (s.type_clause_form == .Table || s.type_clause_form == .Like_Table)
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
			skip_table_line_diag := false
			if access.base_namespace == .Value &&
			   len(access.field_path) == 1 &&
			   access.field_path[0].selector == .Dash &&
			   access.field_path[0].name == "table_line" {
				for query in project.units[unit_index].sql_queries {
					if !(.Has_For_All_Entries in query.flags) ||
					   query.for_all_entries_name != access.base_name ||
					   !range_valid(query.where_clause) ||
					   access.base_range.start < query.where_clause.start ||
					   access.field_path[0].range.end > query.where_clause.end {
						continue
					}
					handle, handle_ok := value_handle_for_name(
						project,
						lookup,
						unit_index,
						access.scope,
						access.base_name,
					)
					if !handle_ok {
						break
					}
					for depth := 0; depth <= len(project.units) + 16; depth += 1 {
						handle_unit_index := unit_id_index(handle.unit)
						assert(handle_unit_index >= 0 && handle_unit_index < len(project.units))
						s := symbol(&project.units[handle_unit_index], handle.symbol)
						assert(s != nil)
						if s.has_type_clause_form && type_form_is_table(s.type_clause_form) {
							skip_table_line_diag =
								s.type_clause_table_has_of &&
								(s.structure == INVALID_STRUCTURE_ID ||
								 len(s.declared_type.field_path) > 0 ||
								 (s.has_declared_type && is_builtin_type_name(s.declared_type.base_name)))
							break
						}
						if !s.has_declared_type {
							break
						}
						next, next_ok := type_ref_leaf_handle(
							project,
							lookup,
							handle_unit_index,
							s.scope,
							s.declared_type,
						)
						if !next_ok {
							skip_table_line_diag = s.structure == INVALID_STRUCTURE_ID
							break
						}
						handle = next
					}
					break
				}
			}
			if skip_table_line_diag {
				continue
			}
			if !field_access_base_resolves(project, lookup, unit_index, access) {
				continue
			}
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

field_access_base_resolves :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	access: Field_Access,
) -> bool {
	if access.base_namespace == .Type {
		_, ok := resolve_type_name_in_project_lookup(project, lookup, unit_index, access.base_name)
		return ok
	}
	if access.base_name == "super" {
		_, ok := enclosing_instance_method_class_owner_unit(&project.units[unit_index], access.scope)
		return ok
	}
	_, ok := value_handle_for_name(project, lookup, unit_index, access.scope, access.base_name)
	return ok
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
		if source_data.query_id != ref.query_id {
			continue
		}
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

validate_at_groups :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
) {
	unit := &project.units[unit_index]
	for region in unit.routine_control_regions {
		if region.kind != .At || at_group_has_loop_context(unit, region.at.scope) {
			continue
		}
		append_diag(
			out,
			seen,
			.Invalid_Control_Break,
			region.at.range,
			"AT group requires LOOP AT context",
		)
	}
}

at_group_has_loop_context :: proc(unit: ^Unit_Analysis, scope_id: Scope_Id) -> bool {
	current := scope_id
	for current != INVALID_SCOPE_ID {
		s := scope(unit, current)
		if s == nil {
			return false
		}
		if s.kind == .Loop_Block {
			return true
		}
		current = s.parent
	}
	return false
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
	return class_handle_from_declared_type(project, lookup, site_unit_index, s.declared_type, line_of, 0, s.scope)
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
	scope_id := INVALID_SCOPE_ID,
) -> (Symbol_Handle, bool) {
	if depth > len(project.units) + 16 {
		return {}, false
	}
	handle, ok := type_ref_leaf_handle(project, lookup, unit_index, scope_id, type_ref)
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
			s.scope,
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
		s.scope,
	)
}

type_ref_leaf_handle :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
) -> (Symbol_Handle, bool) {
	handle, ok := type_ref_symbol_handle(project, lookup, unit_index, scope_id, type_ref)
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

resolve_type_ref_leaf_handle_project_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	type_ref: Field_Type_Ref_Data,
) -> (Symbol_Handle, bool) {
	return type_ref_leaf_handle(project, lookup, unit_index, INVALID_SCOPE_ID, type_ref)
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
		return type_fact_from_class_member_path(project, lookup, class_handle, access.field_path[:], unit_index, access.scope)
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
		return type_fact_from_class_member_path(project, lookup, super_handle, access.field_path[:], unit_index, access.scope)
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
	if base_symbol.kind == .Field_Symbol && !base_symbol.has_declared_type {
		return unknown_type_fact(), true
	}
	if len(access.field_path) > 0 && access.field_path[0].deref {
		if base_symbol.has_declared_type &&
		   !base_symbol.declared_type.is_ref &&
		   len(base_symbol.declared_type.field_path) == 0 &&
		   base_symbol.declared_type.base_name != "object" &&
		   is_generic_builtin_type_name(base_symbol.declared_type.base_name) {
			return unknown_type_fact(), true
		}
		if !base_symbol.has_declared_type ||
		   !base_symbol.declared_type.is_ref ||
		   type_ref_is_object_ref(project, lookup, unit_index, base_symbol.declared_type) {
			return {}, false
		}
		if len(access.field_path) == 1 {
			fact := Type_Fact_Data {
				structure = base_symbol.structure,
				declared_type = base_symbol.declared_type,
				has_declared_type = true,
				type_clause_display = base_symbol.type_clause_display,
			}
			fact.declared_type.is_ref = false
			return fact, true
		}
		if base_symbol.structure == INVALID_STRUCTURE_ID {
			return unknown_type_fact(), true
		}
		fact := Type_Fact_Data {
			structure = base_symbol.structure,
			declared_type = base_symbol.declared_type,
			has_declared_type = true,
			type_clause_display = base_symbol.type_clause_display,
		}
		fact.declared_type.is_ref = false
		return type_fact_from_structure_path(
			project,
			lookup,
			unit_index,
			base_unit,
			base_symbol.structure,
			access.field_path[1:],
			fact,
		)
	}
	if base_symbol.structure != INVALID_STRUCTURE_ID {
		fact := Type_Fact_Data {
			structure = base_symbol.structure,
			declared_type = base_symbol.declared_type,
			has_declared_type = base_symbol.has_declared_type,
			type_clause_display = base_symbol.type_clause_display,
		}
		return type_fact_from_structure_path(
			project,
			lookup,
			unit_index,
			base_unit,
			base_symbol.structure,
			access.field_path[:],
			fact,
		)
	}
	if base_symbol.has_declared_type {
		if len(access.field_path) > 0 &&
		   access.field_path[0].selector == .Arrow {
			if class_handle, class_ok := class_handle_from_symbol(project, lookup, unit_index, base);
			   class_ok {
				return type_fact_from_class_member_path(project, lookup, class_handle, access.field_path[:], unit_index, access.scope)
			}
		}
		base_unit_index := unit_id_index(base.unit)
		if base_unit_index >= 0 &&
		   base_unit_index < len(project.units) &&
		   declared_type_has_unknown_shape(
			   project,
			   lookup,
			   base_unit_index,
			   base_symbol.scope,
			   base_symbol.declared_type,
		   ) {
			return unknown_type_fact(), true
		}
	}
	return {}, false
}

type_fact_from_structure_path :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	start_unit: ^Unit_Analysis,
	start_structure: Structure_Id,
	path: []Field_Access_Segment,
	start_fact := Type_Fact_Data{structure = INVALID_STRUCTURE_ID},
) -> (Type_Fact_Data, bool) {
	current_unit := start_unit
	current_structure := start_structure
	fact := start_fact
	if !type_fact_is_known(fact) {
		fact = Type_Fact_Data{structure = current_structure}
	} else if fact.structure == INVALID_STRUCTURE_ID {
		fact.structure = current_structure
	}
	unknown_after_deref := false
	for segment, i in path {
		if segment.deref {
			if fact.has_declared_type &&
			   !fact.declared_type.is_ref &&
			   len(fact.declared_type.field_path) == 0 &&
			   fact.declared_type.base_name != "object" &&
			   is_generic_builtin_type_name(fact.declared_type.base_name) {
				return unknown_type_fact(), true
			}
			if !fact.has_declared_type ||
			   !fact.declared_type.is_ref ||
			   type_ref_is_object_ref(project, lookup, unit_index, fact.declared_type) {
				return {}, false
			}
			fact.declared_type.is_ref = false
			current_structure = fact.structure
			unknown_after_deref = current_structure == INVALID_STRUCTURE_ID
			continue
		}
		if segment.selector != .Dash {
			if segment.selector != .Arrow ||
			   !fact.has_declared_type ||
			   !fact.declared_type.is_ref {
				return {}, false
			}
			if current_structure == INVALID_STRUCTURE_ID {
				if class_handle, class_ok := class_handle_from_declared_type(
					project,
					lookup,
					unit_index,
					fact.declared_type,
					false,
					0,
				); class_ok {
					return type_fact_from_class_member_path(
						project,
						lookup,
						class_handle,
						path[i:],
						unit_index,
					)
				}
				current_unit_index := unit_id_index(current_unit.unit_id)
				if current_unit_index >= 0 &&
				   current_unit_index < len(project.units) &&
				   declared_type_has_unknown_shape(
					   project,
					   lookup,
					   current_unit_index,
					   INVALID_SCOPE_ID,
					   fact.declared_type,
				   ) {
					return unknown_type_fact(), true
				}
				return {}, false
			}
			if type_ref_is_object_ref(project, lookup, unit_index, fact.declared_type) {
				return {}, false
			}
		}
		if current_structure == INVALID_STRUCTURE_ID {
			if unknown_after_deref {
				return unknown_type_fact(), true
			}
			current_unit_index := unit_id_index(current_unit.unit_id)
			if fact.has_declared_type &&
			   current_unit_index >= 0 &&
			   current_unit_index < len(project.units) &&
			   declared_type_has_unknown_shape(
				   project,
				   lookup,
				   current_unit_index,
				   INVALID_SCOPE_ID,
				   fact.declared_type,
			   ) {
				return unknown_type_fact(), true
			}
			return {}, false
		}
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
		unknown_after_deref = false
		current_structure = field.structure
		if field.structure == INVALID_STRUCTURE_ID {
			next_unit_index := unit_id_index(field.decl_unit)
			if next_unit_index >= 0 && next_unit_index < len(project.units) {
				current_unit = &project.units[next_unit_index]
			}
			continue
		}
	}
	return fact, true
}

type_fact_from_class_member_path :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	class_handle: Symbol_Handle,
	path: []Field_Access_Segment,
	access_unit_index := -1,
	access_scope := INVALID_SCOPE_ID,
) -> (Type_Fact_Data, bool) {
	if len(path) == 0 {
		return unknown_type_fact(), true
	}
	member, member_unit_index, ok := class_member_for_path_segment(
		project,
		lookup,
		class_handle,
		path[0],
		access_unit_index,
		access_scope,
	)
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
			return type_fact_from_structure_path(
				project,
				lookup,
				unit_id_index(class_handle.unit),
				member_unit,
				fact.structure,
				path[1:],
				fact,
			)
		}
		return {}, false
	}
	fact := class_member_type_fact(member)
	if len(path) == 1 {
		return fact, true
	}
	member_unit := &project.units[member_unit_index]
	if fact.structure == INVALID_STRUCTURE_ID {
		return {}, false
	}
	return type_fact_from_structure_path(
		project,
		lookup,
		member_unit_index,
		member_unit,
		fact.structure,
		path[1:],
		fact,
	)
}

class_member_for_path_segment :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	class_handle: Symbol_Handle,
	segment: Field_Access_Segment,
	access_unit_index := -1,
	access_scope := INVALID_SCOPE_ID,
) -> (^Class_Member_Data, int, bool) {
	if segment.selector == .Dash {
		return nil, -1, false
	}
	if segment.interface_qualified {
		if !type_exposes_interface(project, lookup, class_handle, segment.interface_name, 0) {
			return nil, -1, false
		}
		unit_index := unit_id_index(class_handle.unit)
		if unit_index < 0 || unit_index >= len(project.units) {
			return nil, -1, false
		}
		return interface_member_by_name_with_unit(
			project,
			lookup,
			unit_index,
			segment.interface_name,
			segment.name,
		)
	}
	return class_member_in_hierarchy_with_unit(
		project,
		lookup,
		class_handle,
		segment.name,
		true,
		access_unit_index,
		access_scope,
	)
}

class_member_in_hierarchy :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	class_handle: Symbol_Handle,
	name: string,
	inherited: bool,
	access_unit_index := -1,
	access_scope := INVALID_SCOPE_ID,
) -> (^Class_Member_Data, bool) {
	member, _, ok := class_member_in_hierarchy_with_unit(
		project,
		lookup,
		class_handle,
		name,
		inherited,
		access_unit_index,
		access_scope,
	)
	return member, ok
}

class_member_in_hierarchy_with_unit :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	class_handle: Symbol_Handle,
	name: string,
	inherited: bool,
	access_unit_index := -1,
	access_scope := INVALID_SCOPE_ID,
) -> (^Class_Member_Data, int, bool) {
	unit_index := unit_id_index(class_handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return nil, -1, false
	}
	if member := unit_class_member_lookup(project, lookup, class_handle, name); member != nil {
		if inherited && member.visibility == .Private &&
		   !class_private_member_visible(project, class_handle, access_unit_index, access_scope) {
			return nil, -1, false
		}
		return member, unit_index, true
	}
	if member, member_unit_index, ok := interface_member_in_class_with_unit(project, lookup, class_handle, name);
	   ok {
		return member, member_unit_index, true
	}
	next, ok := direct_superclass_handle_lookup(project, lookup, class_handle)
	if !ok {
		return nil, -1, false
	}
	return class_member_in_hierarchy_with_unit(
		project,
		lookup,
		next,
		name,
		true,
		access_unit_index,
		access_scope,
	)
}

class_private_member_visible :: proc(
	project: ^Project_Analysis,
	class_handle: Symbol_Handle,
	access_unit_index: int,
	access_scope: Scope_Id,
) -> bool {
	if access_unit_index < 0 || access_unit_index >= len(project.units) {
		return false
	}
	access_unit := &project.units[access_unit_index]
	caller_symbol, ok := enclosing_class_owner_unit(access_unit, access_scope)
	if !ok {
		return false
	}
	if access_unit.unit_id == class_handle.unit && caller_symbol == class_handle.symbol {
		return true
	}
	caller := symbol(access_unit, caller_symbol)
	if caller == nil {
		return false
	}
	return class_has_friend(project, class_handle, caller.name)
}

class_has_friend :: proc(
	project: ^Project_Analysis,
	class_handle: Symbol_Handle,
	friend_name: string,
) -> bool {
	unit_index := unit_id_index(class_handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return false
	}
	for friend in project.units[unit_index].class_friends {
		if friend.class_symbol == class_handle.symbol && friend.friend_name == friend_name {
			return true
		}
	}
	return false
}

type_exposes_interface :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	handle: Symbol_Handle,
	interface_name: string,
	depth: int,
) -> bool {
	if depth > len(project.units) + 8 {
		return false
	}
	unit_index := unit_id_index(handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return false
	}
	unit := &project.units[unit_index]
	s := symbol(unit, handle.symbol)
	if s != nil && s.kind == .Interface && s.name == interface_name {
		return true
	}
	for implemented in unit.implemented_interfaces {
		if implemented.owner_symbol != handle.symbol {
			continue
		}
		if implemented.interface_name == interface_name {
			return true
		}
		next, ok := resolve_type_name_in_project_lookup(
			project,
			lookup,
			unit_index,
			implemented.interface_name,
		)
		if ok && type_exposes_interface(project, lookup, next, interface_name, depth + 1) {
			return true
		}
	}
	if s != nil && s.kind == .Class {
		if next, ok := direct_superclass_handle_lookup(project, lookup, handle); ok {
			return type_exposes_interface(project, lookup, next, interface_name, depth + 1)
		}
	}
	return false
}

interface_member_in_class :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	class_handle: Symbol_Handle,
	name: string,
) -> (^Class_Member_Data, bool) {
	member, _, ok := interface_member_in_class_with_unit(project, lookup, class_handle, name)
	return member, ok
}

interface_member_in_class_with_unit :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	class_handle: Symbol_Handle,
	name: string,
) -> (^Class_Member_Data, int, bool) {
	unit_index := unit_id_index(class_handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return nil, -1, false
	}
	unit := &project.units[unit_index]
	for alias in unit.member_aliases {
		if alias.owner_symbol != class_handle.symbol || alias.alias_name != name {
			continue
		}
		if member, member_unit_index, ok := interface_member_by_name_with_unit(
			project,
			lookup,
			unit_index,
			alias.target_interface_name,
			alias.target_member_name,
		); ok {
			return member, member_unit_index, true
		}
	}
	for implemented in unit.implemented_interfaces {
		if implemented.owner_symbol != class_handle.symbol {
			continue
		}
		if member, member_unit_index, ok := interface_member_by_name_with_unit(
			project,
			lookup,
			unit_index,
			implemented.interface_name,
			name,
		); ok {
			return member, member_unit_index, true
		}
	}
	return nil, -1, false
}

interface_member_by_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	interface_name, member_name: string,
) -> (^Class_Member_Data, bool) {
	member, _, ok := interface_member_by_name_with_unit(
		project,
		lookup,
		unit_index,
		interface_name,
		member_name,
	)
	return member, ok
}

interface_member_by_name_with_unit :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	interface_name, member_name: string,
) -> (^Class_Member_Data, int, bool) {
	handle, ok := resolve_type_name_in_project_lookup(project, lookup, unit_index, interface_name)
	if !ok {
		return nil, -1, false
	}
	return interface_member_by_handle_with_unit(project, lookup, handle, member_name, 0)
}

interface_member_by_handle :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	handle: Symbol_Handle,
	member_name: string,
	depth: int,
) -> (^Class_Member_Data, bool) {
	member, _, ok := interface_member_by_handle_with_unit(
		project,
		lookup,
		handle,
		member_name,
		depth,
	)
	return member, ok
}

interface_member_by_handle_with_unit :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	handle: Symbol_Handle,
	member_name: string,
	depth: int,
) -> (^Class_Member_Data, int, bool) {
	if depth > len(project.units) + 8 {
		return nil, -1, false
	}
	unit_index := unit_id_index(handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return nil, -1, false
	}
	if member := unit_class_member_lookup(project, lookup, handle, member_name); member != nil {
		return member, unit_index, true
	}
	unit := &project.units[unit_index]
	for alias in unit.member_aliases {
		if alias.owner_symbol != handle.symbol || alias.alias_name != member_name {
			continue
		}
		target, ok := resolve_type_name_in_project_lookup(
			project,
			lookup,
			unit_index,
			alias.target_interface_name,
		)
		if !ok {
			continue
		}
		if aliased, aliased_unit_index, aliased_ok := interface_member_by_handle_with_unit(
			project,
			lookup,
			target,
			alias.target_member_name,
			depth + 1,
		); aliased_ok {
			return aliased, aliased_unit_index, true
		}
	}
	for implemented in unit.implemented_interfaces {
		if implemented.owner_symbol != handle.symbol {
			continue
		}
		next, ok := resolve_type_name_in_project_lookup(
			project,
			lookup,
			unit_index,
			implemented.interface_name,
		)
		if !ok {
			continue
		}
		if inherited, inherited_unit_index, inherited_ok := interface_member_by_handle_with_unit(
			project,
			lookup,
			next,
			member_name,
			depth + 1,
		); inherited_ok {
			return inherited, inherited_unit_index, true
		}
	}
	return nil, -1, false
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
