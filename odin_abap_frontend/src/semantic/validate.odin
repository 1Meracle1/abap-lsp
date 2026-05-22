package abap_frontend_semantic

import "../tokenizer"

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
		if !(root_key in lookup.root_by_unit) {
			lookup.root_by_unit[root_key] = handle
		}
		unit := &project.units[unit_id_index(entry.unit)]
		s := symbol(unit, entry.symbol)
		global_key := Root_Name_Key{namespace = entry.namespace, name = entry.name}
		if s != nil && root_symbol_visible_by_default(unit, s^) && !(global_key in lookup.global_roots) {
			lookup.global_roots[global_key] = handle
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
	hint := validation_diagnostic_hint(unit)
	out := make([dynamic]Diagnostic, 0, hint, allocator)
	seen := make(map[Diagnostic_Key]bool, hint, allocator)
	defer delete(seen)
	for diagnostic in unit.diagnostics {
		if retained_collector_diagnostic(diagnostic.kind) {
			append(&out, diagnostic)
			seen[diagnostic_key(diagnostic)] = true
		}
	}
	validate_later_include_type_refs(project, lookup, unit_index, &out, &seen, allocator)
	validate_unresolved_references(project, lookup, unit_index, &out, &seen, allocator)
	validate_object_type_refs(project, lookup, unit_index, &out, &seen, allocator)
	validate_missing_method_implementations(project, unit_index, &out, &seen, allocator)
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

validation_diagnostic_hint :: proc(unit: ^Unit_Analysis) -> int {
	hint := len(unit.diagnostics) +
	        len(unit.references) +
	        len(unit.field_accesses) +
	        len(unit.call_sites) +
	        len(unit.sql_sources) +
	        len(unit.sql_name_refs)
	if hint < 8 {
		return 8
	}
	return hint
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
	if s := symbol(target_unit, target.symbol); s != nil && root_symbol_visible_by_default(target_unit, s^) {
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

validate_parameter_types :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	for member in unit.class_members {
		for param in member.parameters {
			if parameter_type_uses_inline_table_type(param.type_clause_display) {
				append_diag(
					out,
					seen,
					.Invalid_Parameter_Type,
					param.range,
					diagnostic_message("invalid inline table parameter type: ", param.name, allocator),
				)
			}
		}
	}
	for routine in unit.form_routines {
		for param in routine.parameters {
			s := symbol(unit, param.symbol)
			if s != nil && parameter_type_uses_inline_table_type(s.type_clause_display) {
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
}

parameter_type_uses_inline_table_type :: proc(display: string) -> bool {
	return ascii_contains_ignore_case(display, "table of") &&
	       !ascii_contains_ignore_case(display, "standard table of") &&
	       !ascii_contains_ignore_case(display, "sorted table of") &&
	       !ascii_contains_ignore_case(display, "hashed table of")
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
			append_diag(
				out,
				seen,
				.Unknown_Field,
				site.range,
				diagnostic_message("unknown method ", site.target.method_name, allocator),
			)
			continue
		}
		if member.kind != .Method {
			append_diag(
				out,
				seen,
				.Unknown_Field,
				site.range,
				diagnostic_message("member is not a method: ", site.target.method_name, allocator),
			)
		}
	}
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
	if site.target.base_namespace == .Type {
		return resolve_type_name_in_project_lookup(project, lookup, unit_index, site.target.base_name)
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
	return resolve_type_ref_handle_project_lookup(project, lookup, site_unit_index, s.declared_type)
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
	if handle, ok := root_symbol_in_visible_units_lookup(lookup, .Value, name, lookup.visible[unit_index]); ok {
		return handle, true
	}
	return global_visible_root_symbol_lookup(lookup, .Value, name)
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
