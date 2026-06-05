#+private
package abap_frontend_semantic_analyze

import "src:tokenizer"
import "src:ast"

import "core:mem"

max_type_lookup_depth :: 64

validate_unit_diagnostics :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	allocator: mem.Allocator,
) -> [dynamic]Diagnostic {
	unit := &project.providers.source_files[source_file_index]
	out := make([dynamic]Diagnostic, 0, 8, allocator)
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	seen := make(map[Diagnostic_Key]bool, 8, context.temp_allocator)
	for diagnostic in unit.diagnostics {
		if retained_source_diagnostic(diagnostic.kind) {
			append(&out, diagnostic)
			seen[diagnostic_key(diagnostic)] = true
		}
	}
	validate_core_diagnostics(project, lookup, source_file_index, &out, &seen, allocator)
	return out
}

validate_core_diagnostics :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	validate_later_include_type_refs(project, lookup, source_file_index, out, seen, allocator)
	validate_unresolved_references(project, lookup, source_file_index, out, seen, allocator)
	validate_create_data_type_handles(project, lookup, source_file_index, out, seen, allocator)
	validate_object_type_refs(project, lookup, source_file_index, out, seen, allocator)
	validate_missing_method_implementations(project, source_file_index, out, seen, allocator)
	validate_generic_builtin_types(project, source_file_index, out, seen, allocator)
	validate_generic_table_types(project, lookup, source_file_index, out, seen, allocator)
	validate_parameter_types(project, source_file_index, out, seen, allocator)
	validate_field_accesses(project, lookup, source_file_index, out, seen, allocator)
	validate_call_sites(project, lookup, source_file_index, out, seen, allocator)
	validate_open_sql(project, lookup, source_file_index, out, seen, allocator)
	validate_typecheck_diagnostics(project, lookup, source_file_index, out, seen, allocator)
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

retained_source_diagnostic :: proc(kind: Diagnostic_Kind) -> bool {
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
	lookup: ^Project_Index,
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.providers.source_files[source_file_index]
	for ref in unit.references {
		if ref.kind != .Type_Ref || ref.namespace != .Type || !ref.has_resolution ||
		   ref.resolution.kind != .Symbol {
			continue
		}
		handle := ref.resolution.symbol
		target_index := source_file_id_index(handle.unit)
		if target_index < 0 || target_index >= len(project.providers.source_files) {
			continue
		}
		target := symbol(&project.providers.source_files[target_index], handle.symbol)
		if target == nil || !(target.kind == .Type_Def || target.kind == .Class) {
			continue
		}
		if type_decl_after_reference(project, lookup, source_file_index, ref.range.start, handle) {
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
	lookup: ^Project_Index,
	source_file_index: int,
	ref_start: int,
	target: Symbol_Link,
) -> bool {
	target_index := source_file_id_index(target.unit)
	if target_index == source_file_index {
		if typepool_dependency_unit(project.providers.source_files[source_file_index].uri) {
			return false
		}
		s := symbol(&project.providers.source_files[target_index], target.symbol)
		return s != nil && s.decl_range.start > ref_start
	}
	target_unit := &project.providers.source_files[target_index]
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
	if unit_list_contains(lookup.predecessors[source_file_index][:], target.unit) {
		return false
	}
	unit := &project.providers.source_files[source_file_index]
	for edge in unit.include_edges {
		if !edge.has_target {
			continue
		}
		expansion := lookup.visible[source_file_id_index(edge.target)]
		if edge.target == target.unit || unit_list_contains(expansion[:], target.unit) {
			return edge.range.start > ref_start
		}
	}
	return true
}

validate_unresolved_references :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.providers.source_files[source_file_index]
	for ref in unit.references {
		if ref.has_resolution || ref.kind == .Include {
			continue
		}
		if ref.namespace == .Value &&
		   sql_predicate_column_name(unit, ref.range, ref.name) {
			continue
		}
		if symbol_exists_in_other_namespace(project, lookup, source_file_index, ref) {
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
	lookup: ^Project_Index,
	source_file_index: int,
	ref: Reference_Data,
) -> bool {
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if namespace == ref.namespace {
			continue
		}
		if _, ok := lookup_scope_chain(
			&project.providers.source_files[source_file_index],
			&project.providers.source_files[source_file_index].scope_index,
			ref.scope,
			namespace,
			ref.name,
		); ok {
			return true
		}
		if _, ok := root_symbol_in_visible_units_lookup(project, namespace, ref.name, lookup.visible[source_file_index]); ok {
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
	lookup: ^Project_Index,
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.providers.source_files[source_file_index]
	for site in unit.create_data_type_handles {
		if site.target_name != "" {
			handle, ok := value_handle_for_site(project, lookup, source_file_index, site.scope, site.target_range, site.target_name)
			if ok && create_data_target_is_invalid(project, lookup, source_file_index, handle) {
				append_diag(
					out,
					seen,
					.Invalid_Create_Data_Target,
					site.target_range,
					diagnostic_message("CREATE DATA target must be a data reference: ", site.target_name, allocator),
				)
			}
		}
		handle, ok := value_handle_for_site(project, lookup, source_file_index, site.scope, site.handle_range, site.handle_name)
		if ok && !create_data_handle_is_datadescr_ref(project, lookup, source_file_index, handle) {
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
	lookup: ^Project_Index,
	source_file_index: int,
	handle: Symbol_Link,
) -> bool {
	s := symbol_for_project_handle(project, handle)
	if s == nil || !s.has_declared_type {
		return false
	}
	return !s.declared_type.is_ref || type_ref_is_object_ref(project, lookup, source_file_index, s.declared_type)
}

create_data_handle_is_datadescr_ref :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	handle: Symbol_Link,
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
	type_handle, ok := resolve_type_ref_handle_project_lookup(project, lookup, source_file_index, s.declared_type)
	return !ok || class_is_or_inherits_from_name(project, lookup, type_handle, "cl_abap_datadescr")
}

type_ref_is_object_ref :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	type_ref: Field_Type_Ref_Data,
) -> bool {
	if type_ref.base_name == "object" {
		return true
	}
	if _, _, summary_ok := summary_provider_for_type_name_lookup(project, lookup, .Type, type_ref.base_name); summary_ok {
		return true
	}
	handle, ok := resolve_type_ref_leaf_handle_project_lookup(project, lookup, source_file_index, type_ref)
	if !ok {
		return false
	}
	s := symbol_for_project_handle(project, handle)
	return s != nil && (s.kind == .Class || s.kind == .Interface)
}

class_is_or_inherits_from_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	handle: Symbol_Link,
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

symbol_for_project_handle :: proc(project: ^Project_Analysis, handle: Symbol_Link) -> ^Symbol_Data {
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return nil
	}
	return symbol(&project.providers.source_files[source_file_index], handle.symbol)
}

declared_type_has_unknown_shape :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	depth := 0,
) -> bool {
	if depth > max_type_lookup_depth {
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
	handle, ok := type_ref_symbol_handle(project, lookup, source_file_index, scope_id, type_ref)
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
	lookup: ^Project_Index,
	handle: Symbol_Link,
	depth: int,
) -> bool {
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return true
	}
	s := symbol(&project.providers.source_files[source_file_index], handle.symbol)
	if s == nil {
		return true
	}
	if s.structure != INVALID_STRUCTURE_ID || s.kind == .Class || s.kind == .Interface {
		return false
	}
	if s.has_declared_type {
		return declared_type_has_unknown_shape(project, lookup, source_file_index, s.scope, s.declared_type, depth + 1)
	}
	return s.kind == .Field_Symbol
}

type_ref_path_has_unknown_shape :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	handle: Symbol_Link,
	path: []string,
	selectors: []ast.Selector_Op,
	derefs: []bool,
	depth: int,
) -> bool {
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return true
	}
	unit := &project.providers.source_files[source_file_index]
	s := symbol(unit, handle.symbol)
	if s == nil {
		return true
	}
	if s.kind == .Class || s.kind == .Interface {
		current := handle
		for name in path {
			next, ok := class_type_symbol_handle(project.providers.source_files[:], current, name)
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
			if .Has_Type_Ref in field.flags {
				field_source_file_index := source_file_id_index(field.decl_unit)
				if field_source_file_index < 0 || field_source_file_index >= len(project.providers.source_files) {
					field_source_file_index = source_file_id_index(current_unit.source_file_id)
				}
				scope_id := current_unit.root_scope
				if field_source_file_index == source_file_id_index(current_unit.source_file_id) {
					if owner := structure(current_unit, current_structure);
					   owner != nil && owner.scope != INVALID_SCOPE_ID {
						scope_id = owner.scope
					}
				}
				if structure_unit, structure_id, ok := project_structure_for_type_ref_lookup(
					project,
					lookup,
					field_source_file_index,
					scope_id,
					field.type_ref,
					depth + 1,
				); ok {
					next_source_file_index := source_file_id_index(structure_unit)
					if next_source_file_index >= 0 && next_source_file_index < len(project.providers.source_files) {
						current_unit = &project.providers.source_files[next_source_file_index]
						current_structure = structure_id
						continue
					}
				}
			}
			next_source_file_index := source_file_id_index(field.decl_unit)
			if .Has_Type_Ref in field.flags &&
			   next_source_file_index >= 0 &&
			   next_source_file_index < len(project.providers.source_files) &&
			   declared_type_has_unknown_shape(
				   project,
				   lookup,
				   next_source_file_index,
				   INVALID_SCOPE_ID,
				   field.type_ref,
				   depth + 1,
			   ) {
				return true
			}
		}
		next_source_file_index := source_file_id_index(field.decl_unit)
		if next_source_file_index >= 0 && next_source_file_index < len(project.providers.source_files) {
			current_unit = &project.providers.source_files[next_source_file_index]
		}
		current_structure = field.structure
	}
	return false
}

type_ref_symbol_handle :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
) -> (Symbol_Link, bool) {
	if scope_id != INVALID_SCOPE_ID {
		namespaces := [?]Namespace{.Value, .Type, .Routine}
		if type_ref.namespace == .Type {
			namespaces = [?]Namespace{.Type, .Value, .Routine}
		}
		for namespace in namespaces {
			if !type_ref_namespace_matches(type_ref.namespace, namespace) {
				continue
			}
			if symbol_id, ok := lookup_scope_chain(
				&project.providers.source_files[source_file_index],
				&project.providers.source_files[source_file_index].scope_index,
				scope_id,
				namespace,
				type_ref.base_name,
			); ok {
				if namespace == .Value &&
				   source_file_index < len(lookup.visible) &&
				   source_file_index < len(lookup.predecessors) {
					if handle, effective_ok := effective_project_method_parameter_handle(
						project.providers.source_files[:],
						source_file_index,
						scope_id,
						type_ref.base_name,
						symbol_id,
					); effective_ok {
						return handle, true
					}
				}
				return Symbol_Link{unit = project.providers.source_files[source_file_index].source_file_id, symbol = symbol_id}, true
			}
		}
		if type_ref.namespace == .Value {
			if source_file_index < len(lookup.visible) &&
			   source_file_index < len(lookup.predecessors) {
				if handle, ok := effective_project_method_parameter_handle(
					project.providers.source_files[:],
					source_file_index,
					scope_id,
					type_ref.base_name,
				); ok {
					return handle, true
				}
			}
			if symbol_id, ok := current_class_value_symbol(
				project,
				source_file_index,
				scope_id,
				type_ref.base_name,
			); ok {
				return Symbol_Link{unit = project.providers.source_files[source_file_index].source_file_id, symbol = symbol_id}, true
			}
			if handle, ok := inherited_value_handle_for_name(
				project,
				lookup,
				source_file_index,
				scope_id,
				type_ref.base_name,
			); ok {
				return handle, true
			}
		}
	}
	return resolve_type_ref_handle_project_lookup(project, lookup, source_file_index, type_ref)
}

type_ref_namespace_matches :: #force_inline proc "contextless" (want, got: Namespace) -> bool {
	return want == got || (want == .Value && got == .Type)
}

validate_object_type_refs :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.providers.source_files[source_file_index]
	for s in unit.symbols {
		if !s.has_declared_type ||
		   s.declared_type.namespace != .Type ||
		   s.declared_type.is_ref ||
		   len(s.declared_type.field_path) > 0 {
			continue
		}
		handle, ok := resolve_type_ref_handle_project_lookup(project, lookup, source_file_index, s.declared_type)
		if !ok {
			continue
		}
		target := symbol(&project.providers.source_files[source_file_id_index(handle.unit)], handle.symbol)
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
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.providers.source_files[source_file_index]
	if unit.role == .Dependency_Interface_Source {
		return
	}
	for definition in unit.class_definitions {
		class_symbol := symbol(unit, definition.class_symbol)
		if class_symbol == nil || class_symbol.kind != .Class || definition.is_abstract {
			continue
		}
		for member in unit.symbols {
			info := entity_decl_info(unit, member.id)
			scope_data := scope(unit, member.scope)
			if info != nil &&
			   scope_data != nil &&
			   scope_data.owner == definition.class_symbol &&
			   (scope_data.kind == .Class || scope_data.kind == .Interface) &&
			   info.member_kind == .Method &&
			   !(.Has_Implementation in info.flags) {
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
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.providers.source_files[source_file_index]
	for s in unit.symbols {
		if symbol_kind_is_builtin(s.kind) ||
		   !s.has_declared_type ||
		   s.declared_type.namespace != .Type ||
		   len(s.declared_type.field_path) > 0 {
			continue
		}
		if !symbol_has_source_type_clause(unit, s) && s.kind != .Parameter && s.kind != .Field_Symbol {
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

symbol_has_source_type_clause :: proc(unit: ^Source_File_Provider, s: Symbol_Data) -> bool {
	info := decl_info(unit, s.decl_info)
	return info != nil && info.type_clause != nil
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
	lookup: ^Project_Index,
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.providers.source_files[source_file_index]
	for s in unit.symbols {
		if inline_sql_table_target_symbol(unit, s) {
			continue
		}
		if generic_table_category_type(s) {
			if s.kind == .Parameter ||
			   s.kind == .Field_Symbol ||
			   (s.kind == .Type_Def && s.type_clause_table_has_of && s.has_declared_type) {
				continue
			}
		} else {
			if !s.has_declared_type ||
			   s.kind == .Parameter ||
			   s.kind == .Field_Symbol ||
			   s.kind == .Type_Def ||
			   (s.has_type_clause_form && type_form_is_line_of(s.type_clause_form)) {
				continue
			}
			handle, ok := type_ref_leaf_handle(project, lookup, source_file_index, s.scope, s.declared_type)
			if !ok || !symbol_handle_is_generic_table_type(project, lookup, handle, 0) {
				continue
			}
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

inline_sql_table_target_symbol :: proc(unit: ^Source_File_Provider, s: Symbol_Data) -> bool {
	for target in unit.sql_targets {
		if .Is_Table in target.flags &&
		   .Is_Inline in target.flags &&
		   target.target_name == s.name &&
		   target.target_range == s.decl_range {
			return true
		}
	}
	return false
}

symbol_handle_is_generic_table_type :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	handle: Symbol_Link,
	depth: int,
) -> bool {
	if depth > max_type_lookup_depth {
		return false
	}
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return false
	}
	s := symbol(&project.providers.source_files[source_file_index], handle.symbol)
	if s == nil {
		return false
	}
	if generic_table_category_type(s^) {
		return true
	}
	if s.kind != .Type_Def || !s.has_declared_type {
		return false
	}
	next, ok := type_ref_leaf_handle(project, lookup, source_file_index, s.scope, s.declared_type)
	return ok && symbol_handle_is_generic_table_type(project, lookup, next, depth + 1)
}

generic_table_category_type :: #force_inline proc "contextless" (s: Symbol_Data) -> bool {
	return s.has_type_clause_form &&
	       (s.type_clause_form == .Any_Table ||
	        s.type_clause_form == .Index_Table ||
	        (!s.has_declared_type &&
	         s.structure == INVALID_STRUCTURE_ID &&
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
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.providers.source_files[source_file_index]
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
	lookup: ^Project_Index,
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.providers.source_files[source_file_index]
	resolved_selectors := make(map[tokenizer.Range]bool, len(unit.field_accesses), context.temp_allocator)
	for access in unit.field_accesses {
		if access.in_type_position || len(access.field_path) == 0 {
			continue
		}
		if _, ok := resolve_field_access_tail(project, lookup, source_file_index, access); ok {
			resolved_selectors[field_access_range(access)] = true
		}
	}
	for access in unit.field_accesses {
		if len(access.field_path) == 0 {
			continue
		}
		if access.in_type_position {
			validate_type_position_field_access(project, lookup, source_file_index, access, out, seen, allocator)
			continue
		}
		if access.base_namespace == .Value &&
		   access.base_name == "text" &&
		   len(access.field_path) == 1 &&
		   access.field_path[0].selector == .Dash {
			text_pool_id := len(access.field_path[0].name) > 0
			for r in access.field_path[0].name {
				if !(('0' <= r && r <= '9') || ('a' <= r && r <= 'z') || ('A' <= r && r <= 'Z')) {
					text_pool_id = false
					break
				}
			}
			if text_pool_id {
				continue
			}
		}
		if resolved_selectors[field_access_range(access)] {
			continue
		}
		if _, ok := resolve_field_access_tail(project, lookup, source_file_index, access); !ok {
			skip_table_line_diag := false
			if access.base_namespace == .Value &&
			   len(access.field_path) == 1 &&
			   access.field_path[0].selector == .Dash &&
			   access.field_path[0].name == "table_line" {
				for query in unit.sql_queries {
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
						source_file_index,
						access.scope,
						access.base_name,
					)
					if !handle_ok {
						break
					}
					for depth := 0; depth <= max_type_lookup_depth; depth += 1 {
						handle_source_file_index := source_file_id_index(handle.unit)
						assert(handle_source_file_index >= 0 && handle_source_file_index < len(project.providers.source_files))
						s := symbol(&project.providers.source_files[handle_source_file_index], handle.symbol)
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
							handle_source_file_index,
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
			if field_access_base_has_unknown_inline_table_line_shape(project, lookup, source_file_index, access) {
				continue
			}
			if !field_access_base_resolves(project, lookup, source_file_index, access) {
				continue
			}
			if access.requires_known_base_shape &&
			   !internal_table_where_field_candidate_is_valid(project, lookup, source_file_index, access) {
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

Type_Position_Field_Path_Status :: enum {
	Unknown,
	Valid,
	Missing,
}

validate_type_position_field_access :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	if _, ok := resolve_field_access_tail(project, lookup, source_file_index, access); ok {
		return
	}
	status, segment := type_position_field_path_status(project, lookup, source_file_index, access)
	if status != .Missing {
		return
	}
	append_diag(
		out,
		seen,
		.Unknown_Field,
		segment.range,
		diagnostic_message("unknown field ", segment.name, allocator),
	)
}

type_position_field_path_status :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
) -> (Type_Position_Field_Path_Status, Field_Access_Segment) {
	start_unit, start_structure, path_start, base_status, segment := type_position_base_structure(
		project,
		lookup,
		source_file_index,
		access,
	)
	if base_status != .Valid {
		return base_status, segment
	}
	return type_position_structure_path_status(
		project,
		lookup,
		start_unit,
		start_structure,
		access.field_path[path_start:],
		0,
	)
}

type_position_base_structure :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
) -> (Source_File_Id, Structure_Id, int, Type_Position_Field_Path_Status, Field_Access_Segment) {
	if is_generic_builtin_type_name(access.base_name) {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, 0, .Unknown, {}
	}
	if is_builtin_type_name(access.base_name) {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, 0, .Missing, access.field_path[0]
	}
	base_ref := Field_Type_Ref_Data {
		namespace  = access.base_namespace,
		base_name  = access.base_name,
		base_range = access.base_range,
	}
	handle, ok := type_ref_symbol_handle(project, lookup, source_file_index, access.scope, base_ref)
	if !ok {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, 0, .Unknown, {}
	}
	path_start := 0
	for {
		handle_source_file_index := source_file_id_index(handle.unit)
		if handle_source_file_index < 0 || handle_source_file_index >= len(project.providers.source_files) {
			return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, 0, .Unknown, {}
		}
		source_symbol := symbol(&project.providers.source_files[handle_source_file_index], handle.symbol)
		if source_symbol == nil {
			return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, 0, .Unknown, {}
		}
		if (source_symbol.kind == .Class || source_symbol.kind == .Interface) &&
		   path_start < len(access.field_path) &&
		   access.field_path[path_start].selector != .Dash {
			next, next_ok := class_type_symbol_handle(
				project.providers.source_files[:],
				handle,
				access.field_path[path_start].name,
			)
			if !next_ok {
				return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, path_start, .Missing, access.field_path[path_start]
			}
			handle = next
			path_start += 1
			continue
		}
		if source_symbol.structure != INVALID_STRUCTURE_ID {
			return handle.unit, source_symbol.structure, path_start, .Valid, {}
		}
		if source_symbol.has_declared_type {
			if structure_unit, structure_id, structure_ok := project_structure_for_type_ref_lookup(
				project,
				lookup,
				handle_source_file_index,
				source_symbol.scope,
				source_symbol.declared_type,
				0,
			); structure_ok {
				return structure_unit, structure_id, path_start, .Valid, {}
			}
			if declared_type_has_unknown_shape(
				project,
				lookup,
				handle_source_file_index,
				source_symbol.scope,
				source_symbol.declared_type,
			) {
				return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, path_start, .Unknown, {}
			}
		}
		if path_start < len(access.field_path) {
			return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, path_start, .Missing, access.field_path[path_start]
		}
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, path_start, .Valid, {}
	}
}

type_position_structure_path_status :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	start_unit: Source_File_Id,
	start_structure: Structure_Id,
	path: []Field_Access_Segment,
	depth: int,
) -> (Type_Position_Field_Path_Status, Field_Access_Segment) {
	if depth > max_type_lookup_depth {
		return .Unknown, {}
	}
	if len(path) == 0 {
		return .Valid, {}
	}
	current_unit := start_unit
	current_structure := start_structure
	for &segment, i in path {
		if segment.deref || segment.selector != .Dash {
			return .Unknown, {}
		}
		field, field_unit, owner_structure, field_ok, field_unknown := project_structure_field_lookup_deep(
			project,
			lookup,
			current_unit,
			current_structure,
			segment.name,
			depth + 1,
		)
		if !field_ok {
			if field_unknown {
				return .Unknown, {}
			}
			return .Missing, segment
		}
		if i == len(path) - 1 {
			return .Valid, {}
		}
		next_unit, next_structure, next_ok, next_unknown := structure_field_next_structure(
			project,
			lookup,
			field,
			field_unit,
			owner_structure,
			depth + 1,
		)
		if next_ok {
			current_unit = next_unit
			current_structure = next_structure
			continue
		}
		if next_unknown {
			return .Unknown, {}
		}
		return .Missing, path[i + 1]
	}
	return .Valid, {}
}

project_structure_field_lookup_deep :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	structure_unit: Source_File_Id,
	structure_id: Structure_Id,
	field_name: string,
	depth: int,
) -> (^Structure_Field_Data, Source_File_Id, Structure_Id, bool, bool) {
	if depth > max_type_lookup_depth {
		return nil, INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false, true
	}
	source_file_index := source_file_id_index(structure_unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return nil, INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false, true
	}
	unit := &project.providers.source_files[source_file_index]
	if field := structure_field(unit, structure_id, field_name); field != nil {
		return field, unit.source_file_id, structure_id, true, false
	}
	owner := structure(unit, structure_id)
	if owner == nil {
		return nil, INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false, true
	}
	unknown_include := false
	for &include in owner.fields {
		if !(.Is_Include in include.flags) || !(.Has_Type_Ref in include.flags) {
			continue
		}
		include_source_file_index := source_file_id_index(include.decl_unit)
		if include_source_file_index < 0 || include_source_file_index >= len(project.providers.source_files) {
			include_source_file_index = source_file_index
		}
		scope_id := unit.root_scope
		if include_source_file_index == source_file_index && owner.scope != INVALID_SCOPE_ID {
			scope_id = owner.scope
		}
		include_unit, include_structure, include_ok := project_structure_for_type_ref_lookup(
			project,
			lookup,
			include_source_file_index,
			scope_id,
			include.type_ref,
			depth + 1,
		)
		if !include_ok {
			unknown_include = true
			continue
		}
		field, field_unit, field_owner, field_ok, field_unknown := project_structure_field_lookup_deep(
			project,
			lookup,
			include_unit,
			include_structure,
			field_name,
			depth + 1,
		)
		if field_ok {
			return field, field_unit, field_owner, true, false
		}
		if field_unknown {
			unknown_include = true
		}
	}
	return nil, INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false, unknown_include
}

structure_field_next_structure :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	field: ^Structure_Field_Data,
	field_unit: Source_File_Id,
	owner_structure: Structure_Id,
	depth: int,
) -> (Source_File_Id, Structure_Id, bool, bool) {
	if field == nil {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false, true
	}
	if field.structure != INVALID_STRUCTURE_ID {
		return field_unit, field.structure, true, false
	}
	if !(.Has_Type_Ref in field.flags) {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false, false
	}
	field_source_file_index := source_file_id_index(field.decl_unit)
	if field_source_file_index < 0 || field_source_file_index >= len(project.providers.source_files) {
		field_source_file_index = source_file_id_index(field_unit)
	}
	if field_source_file_index < 0 || field_source_file_index >= len(project.providers.source_files) {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false, true
	}
	scope_id := project.providers.source_files[field_source_file_index].root_scope
	if field.decl_unit == field_unit {
		if owner := structure(&project.providers.source_files[field_source_file_index], owner_structure);
		   owner != nil && owner.scope != INVALID_SCOPE_ID {
			scope_id = owner.scope
		}
	}
	next_unit, next_structure, next_ok := project_structure_for_type_ref_lookup(
		project,
		lookup,
		field_source_file_index,
		scope_id,
		field.type_ref,
		depth + 1,
	)
	if next_ok {
		return next_unit, next_structure, true, false
	}
	if declared_type_has_unknown_shape(project, lookup, field_source_file_index, scope_id, field.type_ref) {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false, true
	}
	return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false, false
}

field_access_base_has_unknown_inline_table_line_shape :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
) -> bool {
	if access.base_namespace != .Value {
		return false
	}
	handle, ok := value_handle_for_name(project, lookup, source_file_index, access.scope, access.base_name)
	if !ok || handle.unit != project.providers.source_files[source_file_index].source_file_id {
		return false
	}
	unit := &project.providers.source_files[source_file_index]
	base := symbol(unit, handle.symbol)
	if base == nil {
		return false
	}
	found := false
	for assignment in unit.assignment_sites {
		if .Assigns_Table_Line in assignment.flags && assignment.lhs_range == base.decl_range {
			found = true
			break
		}
	}
	if !found || base.structure != INVALID_STRUCTURE_ID {
		return false
	}
	if !base.has_declared_type {
		return true
	}
	return declared_type_has_unknown_shape(project, lookup, source_file_index, base.scope, base.declared_type)
}

field_access_base_resolves :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
) -> bool {
	if access.base_namespace == .Type {
		_, ok := resolve_type_name_in_project_lookup(project, lookup, source_file_index, access.base_name)
		return ok
	}
	if access.base_name == "super" {
		_, ok := enclosing_instance_method_class_owner_unit(&project.providers.source_files[source_file_index], access.scope)
		return ok
	}
	_, ok := value_handle_for_name(project, lookup, source_file_index, access.scope, access.base_name)
	return ok
}

internal_table_where_field_candidate_is_valid :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
) -> bool {
	if access.where_candidate_name == "table_line" {
		return false
	}
	if access.where_candidate_name != "" {
		if _, ok := value_handle_for_name(
			project,
			lookup,
			source_file_index,
			access.scope,
			access.where_candidate_name,
		); ok {
			return false
		}
	}
	return field_access_base_has_known_shape(project, lookup, source_file_index, access)
}

field_access_base_has_known_shape :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
) -> bool {
	if access.base_namespace == .Type {
		return true
	}
	base, ok := value_handle_for_name(project, lookup, source_file_index, access.scope, access.base_name)
	if !ok {
		return false
	}
	base_source_file_index := source_file_id_index(base.unit)
	if base_source_file_index < 0 || base_source_file_index >= len(project.providers.source_files) {
		return false
	}
	s := symbol(&project.providers.source_files[base_source_file_index], base.symbol)
	if s == nil {
		return false
	}
	if s.structure != INVALID_STRUCTURE_ID {
		return true
	}
	if !s.has_declared_type {
		return false
	}
	if fact, _, fact_ok := type_fact_from_declared_type(
		project,
		lookup,
		base_source_file_index,
		s.scope,
		s.declared_type,
		s.type_clause_form,
		s.has_type_clause_form,
		0,
	); fact_ok {
		return fact.structure != INVALID_STRUCTURE_ID
	}
	return false
}

validate_call_sites :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	for site in project.providers.source_files[source_file_index].call_sites {
		if site.target.kind != .Method || site.target.method_name == "" {
			continue
		}
		class_handle, ok := class_handle_for_call_target(project, lookup, source_file_index, site)
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
		member_info := entity_decl_info(&project.providers.source_files[source_file_id_index(member.unit)], member.symbol)
		if member_info == nil || member_info.member_kind != .Method {
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
	lookup: ^Project_Index,
	source_file_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.providers.source_files[source_file_index]
	for source_data in unit.sql_sources {
		if source_data.resolution != .External {
			continue
		}
		if _, ok := resolve_type_name_in_project_lookup(project, lookup, source_file_index, source_data.name); !ok {
			append_diag(
				out,
				seen,
				.Unresolved_Open_Sql_Source,
				source_data.range,
				diagnostic_message("unresolved Open SQL source ", source_data.name, allocator),
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
		source_handle, source_ok := resolve_type_name_in_project_lookup(project, lookup, source_file_index, source_data.name)
		if !source_ok {
			continue
		}
		source_unit := &project.providers.source_files[source_file_id_index(source_handle.unit)]
		source_symbol := symbol(source_unit, source_handle.symbol)
		if source_symbol == nil || source_symbol.structure == INVALID_STRUCTURE_ID {
			continue
		}
		if _, _, field_ok := project_structure_field_lookup(
			project,
			source_handle.unit,
			source_symbol.structure,
			name_ref.name,
		); !field_ok {
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
	unit: ^Source_File_Provider,
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

sql_predicate_column_name :: proc(unit: ^Source_File_Provider, range: tokenizer.Range, name: string) -> bool {
	for ref in unit.sql_name_refs {
		if ref.kind == .Column &&
		   ref.name == name &&
		   ref.range.start == range.start &&
		   ref.range.end == range.end {
			return true
		}
	}
	return false
}

class_handle_for_call_target :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	site: Call_Site_Data,
) -> (Symbol_Link, bool) {
	if len(site.target.receiver_path) > 0 {
		fact, ok := resolve_field_access_tail(
			project,
			lookup,
			source_file_index,
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
		return class_handle_from_type_fact(project, lookup, source_file_index, fact)
	}
	if site.target.base_namespace == .Type {
		return resolve_type_name_in_project_lookup(project, lookup, source_file_index, site.target.base_name)
	}
	if site.target.base_name == "super" {
		class_symbol, ok := enclosing_instance_method_class_owner_unit(&project.providers.source_files[source_file_index], site.scope)
		if !ok {
			return {}, false
		}
		return direct_superclass_handle_lookup(
			project,
			lookup,
			Symbol_Link{unit = project.providers.source_files[source_file_index].source_file_id, symbol = class_symbol},
		)
	}
	handle, ok := value_handle_for_name(
		project,
		lookup,
		source_file_index,
		site.scope,
		site.target.base_name,
	)
	if !ok {
		return {}, false
	}
	return class_handle_from_symbol(project, lookup, source_file_index, handle)
}

class_handle_from_symbol :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	site_source_file_index: int,
	handle: Symbol_Link,
) -> (Symbol_Link, bool) {
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return {}, false
	}
	s := symbol(&project.providers.source_files[source_file_index], handle.symbol)
	if s == nil || !s.has_declared_type {
		return {}, false
	}
	line_of := s.has_type_clause_form && type_form_is_line_of(s.type_clause_form)
	_ = site_source_file_index
	return class_handle_from_declared_type(project, lookup, source_file_index, s.declared_type, line_of, 0, s.scope)
}

class_handle_from_type_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	fact: Type_Fact_Data,
) -> (Symbol_Link, bool) {
	if !fact.has_declared_type {
		return {}, false
	}
	return class_handle_from_declared_type(project, lookup, source_file_index, fact.declared_type, false, 0)
}

class_handle_from_declared_type :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	type_ref: Field_Type_Ref_Data,
	line_of: bool,
	depth: int,
	scope_id := INVALID_SCOPE_ID,
) -> (Symbol_Link, bool) {
	if depth > max_type_lookup_depth {
		return {}, false
	}
	handle, ok := type_ref_leaf_handle(project, lookup, source_file_index, scope_id, type_ref)
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
			source_file_index,
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
		source_file_index,
		s.declared_type,
		next_line_of,
		depth + 1,
		s.scope,
	)
}

type_fact_from_declared_type :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	type_form: ast.Data_Type_Form,
	has_type_form: bool,
	depth: int,
) -> (Type_Fact_Data, int, bool) {
	if depth > max_type_lookup_depth {
		return {}, -1, false
	}
	if has_type_form && type_form_is_line_of(type_form) {
		return line_of_type_fact_from_declared_type(project, lookup, source_file_index, scope_id, type_ref, depth + 1)
	}
	if structure_unit, structure_id, ok := project_structure_for_type_ref_lookup(
		project,
		lookup,
		source_file_index,
		scope_id,
		type_ref,
		depth + 1,
	); ok {
		structure_source_file_index := source_file_id_index(structure_unit)
		return Type_Fact_Data {
			structure = structure_id,
			structure_unit = structure_unit,
			declared_type = type_ref,
			has_declared_type = true,
			type_clause_display = type_ref.base_name,
			confidence = .High if structure_source_file_index >= 0 &&
			                     structure_source_file_index < len(project.providers.source_files) &&
			                     project.providers.source_files[structure_source_file_index].role == .Full_Source else .Low,
		}, structure_source_file_index, true
	}
	if is_builtin_type_name(type_ref.base_name) {
		return Type_Fact_Data {
			type_id = type_builtin(&project.providers.source_files[source_file_index], type_ref.base_name),
			type_unit = project.providers.source_files[source_file_index].source_file_id,
			structure = INVALID_STRUCTURE_ID,
			structure_unit = INVALID_SOURCE_FILE_ID,
			declared_type = type_ref,
			has_declared_type = true,
			type_clause_display = type_ref.base_name,
			confidence = .High,
		}, source_file_index, true
	}
	if handle, ok := type_ref_leaf_handle(project, lookup, source_file_index, scope_id, type_ref);
	   ok {
		return type_fact_from_symbol_handle(project, source_file_index, handle), source_file_id_index(handle.unit), true
	}
	return {}, -1, false
}

project_structure_for_type_ref_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	depth: int,
) -> (Source_File_Id, Structure_Id, bool) {
	if depth > max_type_lookup_depth ||
	   type_ref.base_name == "" ||
	   is_builtin_type_name(type_ref.base_name) {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
	}
	if structure_id, ok := local_structure_for_type_ref(&project.providers.source_files[source_file_index], scope_id, type_ref);
	   ok {
		return project.providers.source_files[source_file_index].source_file_id, structure_id, true
	}
	if structure_name, builtin_ok := builtin_root_structure_name(.Type, type_ref.base_name); builtin_ok {
		provider := shared_builtin_provider()
		if st := find_structure(provider, structure_name); st != nil {
			return project.providers.source_files[source_file_index].source_file_id, st.id, true
		}
	}
	if structure_unit, structure_id, ok := project_attribute_structure_for_type_ref_lookup(
		project,
		lookup,
		source_file_index,
		scope_id,
		type_ref,
		depth + 1,
	); ok {
		return structure_unit, structure_id, true
	}
	handle, ok := type_ref_symbol_handle(project, lookup, source_file_index, scope_id, type_ref)
	if !ok {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
	}
	path := type_ref.field_path[:]
	derefs := type_ref.field_derefs[:]
	selectors := type_ref.field_selectors[:]
	handle_source_file_index := source_file_id_index(handle.unit)
	if handle_source_file_index < 0 || handle_source_file_index >= len(project.providers.source_files) {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
	}
	source_symbol := symbol(&project.providers.source_files[handle_source_file_index], handle.symbol)
	if source_symbol != nil &&
	   (source_symbol.kind == .Class || source_symbol.kind == .Interface) &&
	   len(path) > 0 &&
	   selector_at(selectors, 0) != .Dash {
		nested, nested_ok := class_type_symbol_handle(project.providers.source_files[:], handle, path[0])
		if !nested_ok {
			return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
		}
		handle = nested
		path = path[1:]
		if len(derefs) > 0 {derefs = derefs[1:]}
		if len(selectors) > 0 {selectors = selectors[1:]}
		handle_source_file_index = source_file_id_index(handle.unit)
		if handle_source_file_index < 0 || handle_source_file_index >= len(project.providers.source_files) {
			return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
		}
		source_symbol = symbol(&project.providers.source_files[handle_source_file_index], handle.symbol)
	}
	if source_symbol == nil {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
	}
	if source_symbol.structure == INVALID_STRUCTURE_ID {
		if source_symbol.has_declared_type {
			structure_unit, structure_id, structure_ok := project_structure_for_type_ref_lookup(
				project,
				lookup,
				handle_source_file_index,
				source_symbol.scope,
				source_symbol.declared_type,
				depth + 1,
			)
			if !structure_ok {
				return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
			}
			if len(path) == 0 {
				return structure_unit, structure_id, true
			}
			return project_structure_path_lookup(
				project,
				lookup,
				structure_unit,
				structure_id,
				path,
				selectors,
				derefs,
				depth + 1,
			)
		}
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
	}
	return project_structure_path_lookup(
		project,
		lookup,
		handle.unit,
		source_symbol.structure,
		path,
		selectors,
		derefs,
		depth + 1,
	)
}

project_attribute_structure_for_type_ref_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	depth: int,
) -> (Source_File_Id, Structure_Id, bool) {
	if depth > max_type_lookup_depth ||
	   len(type_ref.field_path) == 0 ||
	   selector_at(type_ref.field_selectors[:], 0) != .Arrow {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
	}
	base, ok := value_handle_for_name(project, lookup, source_file_index, scope_id, type_ref.base_name)
	if !ok {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
	}
	class_handle, class_ok := class_handle_from_symbol(project, lookup, source_file_index, base)
	if !class_ok {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
	}
	member, member_source_file_index, member_ok := class_member_for_path_segment(
		project,
		lookup,
		class_handle,
		Field_Access_Segment{name = type_ref.field_path[0], selector = .Arrow},
		source_file_index,
		scope_id,
	)
	member_symbol := symbol(&project.providers.source_files[member_source_file_index], member.symbol) if member_ok else nil
	member_info := entity_decl_info(&project.providers.source_files[member_source_file_index], member.symbol) if member_ok else nil
	if !member_ok ||
	   member_symbol == nil ||
	   member_info == nil ||
	   member_info.member_kind != .Attribute ||
	   member_symbol.structure == INVALID_STRUCTURE_ID {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
	}
	next_selectors := type_ref.field_selectors[:]
	next_derefs := type_ref.field_derefs[:]
	if len(next_selectors) > 0 {next_selectors = next_selectors[1:]}
	if len(next_derefs) > 0 {next_derefs = next_derefs[1:]}
	return project_structure_path_lookup(
		project,
		lookup,
		project.providers.source_files[member_source_file_index].source_file_id,
		member_symbol.structure,
		type_ref.field_path[1:],
		next_selectors,
		next_derefs,
		depth + 1,
	)
}

project_structure_path_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	start_unit: Source_File_Id,
	start_structure: Structure_Id,
	path: []string,
	selectors: []ast.Selector_Op,
	derefs: []bool,
	depth: int,
) -> (Source_File_Id, Structure_Id, bool) {
	if depth > max_type_lookup_depth {
		return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
	}
	current_unit := start_unit
	current_structure := start_structure
	for field_name, i in path {
		if i < len(derefs) && derefs[i] {
			continue
		}
		if selector_at(selectors, i) != .Dash {
			return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
		}
		source_file_index := source_file_id_index(current_unit)
		if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
			return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
		}
		unit := &project.providers.source_files[source_file_index]
		field := structure_field(unit, current_structure, field_name)
		if field == nil {
			return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
		}
		if field.structure != INVALID_STRUCTURE_ID {
			current_structure = field.structure
			continue
		}
		if !(.Has_Type_Ref in field.flags) {
			return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
		}
		field_source_file_index := source_file_id_index(field.decl_unit)
		if field_source_file_index < 0 || field_source_file_index >= len(project.providers.source_files) {
			field_source_file_index = source_file_index
		}
		scope_id := unit.root_scope
		if field_source_file_index == source_file_index {
			if owner := structure(unit, current_structure); owner != nil && owner.scope != INVALID_SCOPE_ID {
				scope_id = owner.scope
			}
		}
		next_unit, next_structure, next_ok := project_structure_for_type_ref_lookup(
			project,
			lookup,
			field_source_file_index,
			scope_id,
			field.type_ref,
			depth + 1,
		)
		if !next_ok {
			return INVALID_SOURCE_FILE_ID, INVALID_STRUCTURE_ID, false
		}
		current_unit = next_unit
		current_structure = next_structure
	}
	return current_unit, current_structure, true
}

project_structure_field_lookup :: proc(
	project: ^Project_Analysis,
	structure_unit: Source_File_Id,
	structure_id: Structure_Id,
	field_name: string,
) -> (^Structure_Field_Data, int, bool) {
	source_file_index := source_file_id_index(structure_unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return nil, -1, false
	}
	field := structure_field(&project.providers.source_files[source_file_index], structure_id, field_name)
	return field, source_file_index, field != nil
}

line_of_type_fact_from_symbol :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	handle: Symbol_Link,
) -> (Type_Fact_Data, int, bool) {
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return {}, -1, false
	}
	s := symbol(&project.providers.source_files[source_file_index], handle.symbol)
	if s == nil ||
	   !s.has_declared_type ||
	   !(s.has_type_clause_form && type_form_is_line_of(s.type_clause_form)) {
		return {}, -1, false
	}
	return line_of_type_fact_from_declared_type(project, lookup, source_file_index, s.scope, s.declared_type, 0)
}

like_type_fact_from_symbol :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	handle: Symbol_Link,
	depth := 0,
) -> (Type_Fact_Data, int, bool) {
	if depth > max_type_lookup_depth {
		return {}, -1, false
	}
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return {}, -1, false
	}
	s := symbol(&project.providers.source_files[source_file_index], handle.symbol)
	if s == nil ||
	   !s.has_declared_type ||
	   !(s.has_type_clause_form && s.type_clause_form == .Like) {
		return {}, -1, false
	}
	next, ok := type_ref_leaf_handle(project, lookup, source_file_index, s.scope, s.declared_type)
	if !ok {
		if declared_type_has_unknown_shape(project, lookup, source_file_index, s.scope, s.declared_type) {
			return unknown_type_fact(), source_file_index, true
		}
		return {}, -1, false
	}
	next_source_file_index := source_file_id_index(next.unit)
	if next_source_file_index < 0 || next_source_file_index >= len(project.providers.source_files) {
		return {}, -1, false
	}
	next_symbol := symbol(&project.providers.source_files[next_source_file_index], next.symbol)
	if next_symbol == nil {
		return {}, -1, false
	}
	if next_symbol.has_type_clause_form && next_symbol.type_clause_form == .Like {
		return like_type_fact_from_symbol(project, lookup, next, depth + 1)
	}
	return Type_Fact_Data {
		type_id = next_symbol.type_id,
		type_unit = next.unit if type_id_is_known(next_symbol.type_id) else INVALID_SOURCE_FILE_ID,
		structure = next_symbol.structure,
		structure_unit = next.unit if next_symbol.structure != INVALID_STRUCTURE_ID else INVALID_SOURCE_FILE_ID,
		declared_type = next_symbol.declared_type,
		has_declared_type = next_symbol.has_declared_type,
		type_clause_display = next_symbol.type_clause_display,
		confidence = .High if project.providers.source_files[next_source_file_index].role == .Full_Source else .Low,
	}, next_source_file_index, true
}

line_of_type_fact_from_declared_type :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	depth: int,
) -> (Type_Fact_Data, int, bool) {
	if depth > max_type_lookup_depth {
		return {}, -1, false
	}
	handle, ok := type_ref_leaf_handle(project, lookup, source_file_index, scope_id, type_ref)
	if !ok {
		if structure_unit, structure_id, structure_ok := project_structure_for_type_ref_lookup(
			project,
			lookup,
			source_file_index,
			scope_id,
			type_ref,
			depth + 1,
		); structure_ok {
			structure_source_file_index := source_file_id_index(structure_unit)
			return Type_Fact_Data {
				structure = structure_id,
				structure_unit = structure_unit,
				declared_type = type_ref,
				has_declared_type = true,
				type_clause_display = type_ref.base_name,
				confidence = .High if structure_source_file_index >= 0 &&
				                     structure_source_file_index < len(project.providers.source_files) &&
				                     project.providers.source_files[structure_source_file_index].role == .Full_Source else .Low,
			}, structure_source_file_index, true
		}
		if declared_type_has_unknown_shape(project, lookup, source_file_index, scope_id, type_ref) {
			return unknown_type_fact(), source_file_index, true
		}
		return {}, -1, false
	}
	handle_source_file_index := source_file_id_index(handle.unit)
	if handle_source_file_index < 0 || handle_source_file_index >= len(project.providers.source_files) {
		return {}, -1, false
	}
	s := symbol(&project.providers.source_files[handle_source_file_index], handle.symbol)
	if s == nil {
		return {}, -1, false
	}
	if s.has_type_clause_form && type_form_is_table(s.type_clause_form) {
		return table_line_type_fact_from_symbol(project, lookup, handle_source_file_index, s)
	}
	if !s.has_declared_type {
		return {}, -1, false
	}
	return line_of_type_fact_from_declared_type(
		project,
		lookup,
		handle_source_file_index,
		s.scope,
		s.declared_type,
		depth + 1,
	)
}

table_line_type_fact_from_symbol :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	s: ^Symbol_Data,
) -> (Type_Fact_Data, int, bool) {
	if s == nil || !s.has_declared_type {
		return unknown_type_fact(), source_file_index, true
	}
	fact := Type_Fact_Data {
		structure = s.structure,
		structure_unit = project.providers.source_files[source_file_index].source_file_id if s.structure != INVALID_STRUCTURE_ID else INVALID_SOURCE_FILE_ID,
		declared_type = s.declared_type,
		has_declared_type = true,
		type_clause_display = s.type_clause_display,
		confidence = .High if project.providers.source_files[source_file_index].role == .Full_Source else .Low,
	}
	if fact.structure == INVALID_STRUCTURE_ID {
		if handle, ok := type_ref_leaf_handle(project, lookup, source_file_index, s.scope, s.declared_type);
		   ok {
			handle_source_file_index := source_file_id_index(handle.unit)
			if handle_source_file_index >= 0 && handle_source_file_index < len(project.providers.source_files) {
				if target := symbol(&project.providers.source_files[handle_source_file_index], handle.symbol);
				   target != nil && target.structure != INVALID_STRUCTURE_ID {
					fact.structure = target.structure
					fact.structure_unit = handle.unit
					fact.type_id = target.type_id
					fact.type_unit = handle.unit if type_id_is_known(target.type_id) else INVALID_SOURCE_FILE_ID
					fact.confidence = .High if project.providers.source_files[handle_source_file_index].role == .Full_Source else .Low
					return fact, handle_source_file_index, true
				}
			}
		}
	}
	return fact, source_file_index, true
}

type_fact_from_data_ref_path :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	fact: Type_Fact_Data,
	path: []Field_Access_Segment,
) -> (Type_Fact_Data, bool) {
	if !fact.has_declared_type ||
	   !fact.declared_type.is_ref ||
	   type_ref_is_object_ref(project, lookup, source_file_index, fact.declared_type) {
		return {}, false
	}
	if fact.structure == INVALID_STRUCTURE_ID {
		if declared_type_has_unknown_shape(
			project,
			lookup,
			source_file_index,
			INVALID_SCOPE_ID,
			fact.declared_type,
		) {
			return unknown_type_fact(), true
		}
		return {}, false
	}
	structure_source_file_index := source_file_index
	if fact.structure_unit != INVALID_SOURCE_FILE_ID {
		structure_source_file_index = source_file_id_index(fact.structure_unit)
		if structure_source_file_index < 0 || structure_source_file_index >= len(project.providers.source_files) {
			return {}, false
		}
	}
	return type_fact_from_structure_path(
		project,
		lookup,
		source_file_index,
		&project.providers.source_files[structure_source_file_index],
		fact.structure,
		path,
		fact,
	)
}

type_ref_leaf_handle :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
) -> (Symbol_Link, bool) {
	handle, ok := type_ref_symbol_handle(project, lookup, source_file_index, scope_id, type_ref)
	if !ok {
		return {}, false
	}
	for name in type_ref.field_path {
		next, next_ok := class_type_symbol_handle(project.providers.source_files[:], handle, name)
		if !next_ok {
			return {}, false
		}
		handle = next
	}
	return handle, true
}

resolve_type_ref_leaf_handle_project_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	type_ref: Field_Type_Ref_Data,
) -> (Symbol_Link, bool) {
	return type_ref_leaf_handle(project, lookup, source_file_index, INVALID_SCOPE_ID, type_ref)
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

value_handle_for_site :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	range: tokenizer.Range,
	name: string,
) -> (Symbol_Link, bool) {
	_ = range
	return value_handle_for_name(project, lookup, source_file_index, scope_id, name)
}

value_handle_for_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	name: string,
) -> (Symbol_Link, bool) {
	if symbol_id, ok := lookup_scope_chain(
		&project.providers.source_files[source_file_index],
		&project.providers.source_files[source_file_index].scope_index,
		scope_id,
		.Value,
		name,
	); ok {
		if source_file_index < len(lookup.visible) &&
		   source_file_index < len(lookup.predecessors) {
			if handle, effective_ok := effective_project_method_parameter_handle(
				project.providers.source_files[:],
				source_file_index,
				scope_id,
				name,
				symbol_id,
			); effective_ok {
				return handle, true
			}
		}
		return Symbol_Link{unit = project.providers.source_files[source_file_index].source_file_id, symbol = symbol_id}, true
	}
	if source_file_index < len(lookup.visible) &&
	   source_file_index < len(lookup.predecessors) {
		if handle, ok := effective_project_method_parameter_handle(
			project.providers.source_files[:],
			source_file_index,
			scope_id,
			name,
		); ok {
			return handle, true
		}
	}
	if symbol_id, ok := current_class_value_symbol(project, source_file_index, scope_id, name); ok {
		return Symbol_Link{unit = project.providers.source_files[source_file_index].source_file_id, symbol = symbol_id}, true
	}
	if handle, ok := value_alias_handle_for_name(project, lookup, source_file_index, scope_id, name); ok {
		return handle, true
	}
	if handle, ok := inherited_value_handle_for_name(project, lookup, source_file_index, scope_id, name);
	   ok {
		return handle, true
	}
	if handle, ok := inherited_value_alias_handle_for_name(project, lookup, source_file_index, scope_id, name);
	   ok {
		return handle, true
	}
	if handle, ok := root_symbol_in_visible_units_lookup(project, .Value, name, lookup.visible[source_file_index]); ok {
		return handle, true
	}
	return global_visible_root_symbol_lookup(lookup, .Value, name)
}

current_class_value_symbol :: proc(
	project: ^Project_Analysis,
	source_file_index: int,
	scope_id: Scope_Id,
	name: string,
) -> (Symbol_Id, bool) {
	class_symbol, ok := enclosing_class_owner_unit(&project.providers.source_files[source_file_index], scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	return class_scope_symbol(&project.providers.source_files[source_file_index].scope_index, class_symbol, .Value, name)
}

inherited_value_handle_for_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	name: string,
) -> (Symbol_Link, bool) {
	class_symbol, ok := enclosing_class_owner_unit(&project.providers.source_files[source_file_index], scope_id)
	if !ok {
		return {}, false
	}
	current := Symbol_Link{unit = project.providers.source_files[source_file_index].source_file_id, symbol = class_symbol}
	for _ in 0 ..< len(project.providers.source_files) + 8 {
		next, next_ok := direct_superclass_handle_lookup(project, lookup, current)
		if !next_ok {
			return {}, false
		}
		next_source_file_index := source_file_id_index(next.unit)
		if next_source_file_index < 0 || next_source_file_index >= len(project.providers.source_files) {
			return {}, false
		}
		member, member_ok := class_member_handle_lookup(project, lookup, next, name)
		info := entity_decl_info(&project.providers.source_files[next_source_file_index], member.symbol) if member_ok else nil
		if member_ok && info != nil && info.member_kind == .Attribute && info.visibility != .Private {
			if symbol_id, symbol_ok := class_scope_symbol(
				&project.providers.source_files[next_source_file_index].scope_index,
				next.symbol,
				.Value,
				name,
			); symbol_ok {
				return Symbol_Link{unit = next.unit, symbol = symbol_id}, true
			}
		}
		current = next
	}
	return {}, false
}

value_alias_handle_for_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	name: string,
) -> (Symbol_Link, bool) {
	class_symbol, ok := enclosing_class_owner_unit(&project.providers.source_files[source_file_index], scope_id)
	if !ok {
		return {}, false
	}
	class_handle := Symbol_Link{unit = project.providers.source_files[source_file_index].source_file_id, symbol = class_symbol}
	return class_alias_symbol_by_handle_lookup(project, lookup, source_file_index, class_handle, .Value, name)
}

inherited_value_alias_handle_for_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	scope_id: Scope_Id,
	name: string,
) -> (Symbol_Link, bool) {
	class_symbol, ok := enclosing_class_owner_unit(&project.providers.source_files[source_file_index], scope_id)
	if !ok {
		return {}, false
	}
	current := Symbol_Link{unit = project.providers.source_files[source_file_index].source_file_id, symbol = class_symbol}
	for _ in 0 ..< len(project.providers.source_files) + 8 {
		next, next_ok := direct_superclass_handle_lookup(project, lookup, current)
		if !next_ok {
			return {}, false
		}
		next_source_file_index := source_file_id_index(next.unit)
		if next_source_file_index < 0 || next_source_file_index >= len(project.providers.source_files) {
			return {}, false
		}
		if handle, found := class_alias_symbol_by_handle_lookup(
			project,
			lookup,
			next_source_file_index,
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
	lookup: ^Project_Index,
	source_file_index: int,
	class_handle: Symbol_Link,
	namespace: Namespace,
	name: string,
) -> (Symbol_Link, bool) {
	for alias in project.providers.source_files[source_file_index].member_aliases {
		if alias.owner_symbol != class_handle.symbol || alias.alias_name != name {
			continue
		}
		interface_handle, interface_ok := resolve_type_name_in_project_lookup(
			project,
			lookup,
			source_file_index,
			alias.target_interface_name,
		)
		if !interface_ok {
			continue
		}
		interface_source_file_index := source_file_id_index(interface_handle.unit)
		if interface_source_file_index < 0 || interface_source_file_index >= len(project.providers.source_files) {
			continue
		}
		member_name := alias.target_member_name
		if member_name == "" {
			member_name = name
		}
		if symbol_id, member_ok := class_scope_symbol(
			&project.providers.source_files[interface_source_file_index].scope_index,
			interface_handle.symbol,
			namespace,
			member_name,
		); member_ok {
			return Symbol_Link{unit = interface_handle.unit, symbol = symbol_id}, true
		}
	}
	return {}, false
}

resolve_field_access_tail :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
) -> (Type_Fact_Data, bool) {
	if access.base_namespace == .Type {
		class_handle, ok := resolve_type_name_in_project_lookup(project, lookup, source_file_index, access.base_name)
		if !ok {
			if fact, builtin_ok := type_fact_from_builtin_structure_access(project, lookup, source_file_index, access);
			   builtin_ok {
				return fact, true
			}
			return {}, false
		}
		return type_fact_from_class_member_path(project, lookup, class_handle, access.field_path[:], source_file_index, access.scope)
	}
	if fact, builtin_ok := type_fact_from_builtin_structure_access(project, lookup, source_file_index, access);
	   builtin_ok {
		return fact, true
	}
	if access.base_name == "super" {
		class_symbol, class_ok := enclosing_instance_method_class_owner_unit(
			&project.providers.source_files[source_file_index],
			access.scope,
		)
		if !class_ok {
			return {}, false
		}
		super_handle, super_ok := direct_superclass_handle_lookup(
			project,
			lookup,
			Symbol_Link{unit = project.providers.source_files[source_file_index].source_file_id, symbol = class_symbol},
		)
		if !super_ok {
			return {}, false
		}
		return type_fact_from_class_member_path(project, lookup, super_handle, access.field_path[:], source_file_index, access.scope)
	}
	base, ok := value_handle_for_name(project, lookup, source_file_index, access.scope, access.base_name)
	if !ok {
		return {}, false
	}
	base_unit := &project.providers.source_files[source_file_id_index(base.unit)]
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
		   type_ref_is_object_ref(project, lookup, source_file_index, base_symbol.declared_type) {
			return {}, false
		}
		if len(access.field_path) == 1 {
			fact := Type_Fact_Data {
				structure = base_symbol.structure,
				structure_unit = base.unit if base_symbol.structure != INVALID_STRUCTURE_ID else INVALID_SOURCE_FILE_ID,
				declared_type = base_symbol.declared_type,
				has_declared_type = true,
				type_clause_display = base_symbol.type_clause_display,
				confidence = .High if base_unit.role == .Full_Source else .Low,
			}
			fact.declared_type.is_ref = false
			return fact, true
		}
		if base_symbol.structure == INVALID_STRUCTURE_ID {
			return unknown_type_fact(), true
		}
		fact := Type_Fact_Data {
			structure = base_symbol.structure,
			structure_unit = base.unit if base_symbol.structure != INVALID_STRUCTURE_ID else INVALID_SOURCE_FILE_ID,
			declared_type = base_symbol.declared_type,
			has_declared_type = true,
			type_clause_display = base_symbol.type_clause_display,
			confidence = .High if base_unit.role == .Full_Source else .Low,
		}
		fact.declared_type.is_ref = false
		return type_fact_from_structure_path(
			project,
			lookup,
			source_file_index,
			base_unit,
			base_symbol.structure,
			access.field_path[1:],
			fact,
		)
	}
	if base_symbol.has_declared_type &&
	   len(access.field_path) > 0 &&
	   access.field_path[0].selector == .Arrow {
		base_source_file_index := source_file_id_index(base.unit)
		if fact, fact_source_file_index, fact_ok := line_of_type_fact_from_declared_type(
			project,
			lookup,
			base_source_file_index,
			base_symbol.scope,
			base_symbol.declared_type,
			0,
		); fact_ok {
			if resolved, resolved_ok := type_fact_from_data_ref_path(
				project,
				lookup,
				fact_source_file_index,
				fact,
				access.field_path[:],
			); resolved_ok {
				return resolved, true
			}
		}
		if fact, fact_source_file_index, fact_ok := like_type_fact_from_symbol(
			project,
			lookup,
			base,
		); fact_ok {
			if resolved, resolved_ok := type_fact_from_data_ref_path(
				project,
				lookup,
				fact_source_file_index,
				fact,
				access.field_path[:],
			); resolved_ok {
				return resolved, true
			}
		}
		if fact, fact_source_file_index, fact_ok := line_of_type_fact_from_symbol(
			project,
			lookup,
			base,
		); fact_ok {
			if resolved, resolved_ok := type_fact_from_data_ref_path(
				project,
				lookup,
				fact_source_file_index,
				fact,
				access.field_path[:],
			); resolved_ok {
				return resolved, true
			}
		}
	}
	if base_symbol.structure != INVALID_STRUCTURE_ID {
		fact := Type_Fact_Data {
			structure = base_symbol.structure,
			structure_unit = base.unit if base_symbol.structure != INVALID_STRUCTURE_ID else INVALID_SOURCE_FILE_ID,
			declared_type = base_symbol.declared_type,
			has_declared_type = base_symbol.has_declared_type,
			type_clause_display = base_symbol.type_clause_display,
			confidence = .High if base_unit.role == .Full_Source else .Low,
		}
		return type_fact_from_structure_path(
			project,
			lookup,
			source_file_index,
			base_unit,
			base_symbol.structure,
			access.field_path[:],
			fact,
		)
	}
	if base_symbol.has_declared_type {
		if len(access.field_path) > 0 &&
		   access.field_path[0].selector == .Arrow {
			if fact, fact_ok := type_fact_from_summary_member_path(
				project,
				lookup,
				base_symbol.declared_type,
				access.field_path[:],
			); fact_ok {
				return fact, true
			}
			if class_handle, class_ok := class_handle_from_symbol(project, lookup, source_file_index, base);
			   class_ok {
				return type_fact_from_class_member_path(project, lookup, class_handle, access.field_path[:], source_file_index, access.scope)
			}
		}
		base_source_file_index := source_file_id_index(base.unit)
		if base_source_file_index >= 0 && base_source_file_index < len(project.providers.source_files) {
			line_fact_found := false
			if fact, fact_source_file_index, fact_ok := type_fact_from_declared_type(
				project,
				lookup,
				base_source_file_index,
				base_symbol.scope,
				base_symbol.declared_type,
				base_symbol.type_clause_form,
				base_symbol.has_type_clause_form,
				0,
			); fact_ok {
				line_fact_found = true
				if fact.structure != INVALID_STRUCTURE_ID {
					start_unit := shared_builtin_provider()
					if fact_source_file_index >= 0 && fact_source_file_index < len(project.providers.source_files) {
						start_unit = &project.providers.source_files[fact_source_file_index]
					}
					return type_fact_from_structure_path(
						project,
						lookup,
						source_file_index,
						start_unit,
						fact.structure,
						access.field_path[:],
						fact,
					)
				}
				if base_symbol.has_type_clause_form &&
				   type_form_is_line_of(base_symbol.type_clause_form) &&
				   !(fact.has_declared_type &&
				     fact.declared_type.is_ref &&
				     type_ref_is_object_ref(project, lookup, source_file_index, fact.declared_type)) {
					return unknown_type_fact(), true
				}
			}
			if !line_fact_found &&
			   base_symbol.has_type_clause_form &&
			   type_form_is_line_of(base_symbol.type_clause_form) &&
			   len(base_symbol.declared_type.field_path) == 0 {
				return unknown_type_fact(), true
			}
		}
		if base_source_file_index >= 0 &&
		   base_source_file_index < len(project.providers.source_files) &&
		   declared_type_has_unknown_shape(
			   project,
			   lookup,
			   base_source_file_index,
			   base_symbol.scope,
			   base_symbol.declared_type,
		   ) {
			return unknown_type_fact(), true
		}
	}
	return {}, false
}

type_fact_from_builtin_structure_access :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	access: Field_Access,
) -> (Type_Fact_Data, bool) {
	structure_name, ok := builtin_root_structure_name(access.base_namespace, access.base_name)
	if !ok {
		return {}, false
	}
	provider := shared_builtin_provider()
	st := find_structure(provider, structure_name)
	if st == nil {
		return {}, false
	}
	return type_fact_from_structure_path(
		project,
		lookup,
		source_file_index,
		provider,
		st.id,
		access.field_path[:],
	)
}

type_fact_from_structure_path :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	start_unit: ^Source_File_Provider,
	start_structure: Structure_Id,
	path: []Field_Access_Segment,
	start_fact := Type_Fact_Data{structure = INVALID_STRUCTURE_ID},
) -> (Type_Fact_Data, bool) {
	current_unit := start_unit
	current_structure := start_structure
	fact := start_fact
	if fact.structure != INVALID_STRUCTURE_ID && fact.structure_unit != INVALID_SOURCE_FILE_ID {
		if fact_source_file_index := source_file_id_index(fact.structure_unit);
		   fact_source_file_index >= 0 && fact_source_file_index < len(project.providers.source_files) {
			current_unit = &project.providers.source_files[fact_source_file_index]
			current_structure = fact.structure
		}
	}
	if !type_fact_is_known(fact) {
		fact = Type_Fact_Data {
			structure = current_structure,
			structure_unit = current_unit.source_file_id if current_structure != INVALID_STRUCTURE_ID else INVALID_SOURCE_FILE_ID,
			confidence = .High if current_unit.role == .Full_Source else .Low,
		}
	} else if fact.structure == INVALID_STRUCTURE_ID {
		fact.structure = current_structure
		fact.structure_unit = current_unit.source_file_id if current_structure != INVALID_STRUCTURE_ID else INVALID_SOURCE_FILE_ID
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
			   type_ref_is_object_ref(project, lookup, source_file_index, fact.declared_type) {
				return {}, false
			}
			fact.declared_type.is_ref = false
			current_structure = fact.structure
			if fact.structure_unit != INVALID_SOURCE_FILE_ID {
				if fact_source_file_index := source_file_id_index(fact.structure_unit);
				   fact_source_file_index >= 0 && fact_source_file_index < len(project.providers.source_files) {
					current_unit = &project.providers.source_files[fact_source_file_index]
				}
			}
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
					source_file_index,
					fact.declared_type,
					false,
					0,
				); class_ok {
					return type_fact_from_class_member_path(
						project,
						lookup,
						class_handle,
						path[i:],
						source_file_index,
					)
				}
				current_source_file_index := source_file_id_index(current_unit.source_file_id)
				if current_source_file_index >= 0 &&
				   current_source_file_index < len(project.providers.source_files) &&
				   declared_type_has_unknown_shape(
					   project,
					   lookup,
					   current_source_file_index,
					   INVALID_SCOPE_ID,
					   fact.declared_type,
				   ) {
					return unknown_type_fact(), true
				}
				return {}, false
			}
			if type_ref_is_object_ref(project, lookup, source_file_index, fact.declared_type) {
				return {}, false
			}
		}
		if current_structure == INVALID_STRUCTURE_ID {
			if unknown_after_deref {
				return unknown_type_fact(), true
			}
			current_source_file_index := source_file_id_index(current_unit.source_file_id)
			if fact.has_declared_type &&
			   current_source_file_index >= 0 &&
			   current_source_file_index < len(project.providers.source_files) &&
			   declared_type_has_unknown_shape(
				   project,
				   lookup,
				   current_source_file_index,
				   INVALID_SCOPE_ID,
				   fact.declared_type,
			   ) {
				return unknown_type_fact(), true
			}
			return {}, false
		}
		field := structure_field(current_unit, current_structure, segment.name)
		field_unit := current_unit
		field_owner_structure := current_structure
		if field == nil {
			if owner := structure(current_unit, current_structure); owner != nil {
				for include in owner.fields {
					if !(.Is_Include in include.flags) || !(.Has_Type_Ref in include.flags) {
						continue
					}
					include_source_file_index := source_file_id_index(include.decl_unit)
					if include_source_file_index < 0 || include_source_file_index >= len(project.providers.source_files) {
						include_source_file_index = source_file_id_index(current_unit.source_file_id)
					}
					scope_id := current_unit.root_scope
					if include_source_file_index == source_file_id_index(current_unit.source_file_id) &&
					   owner.scope != INVALID_SCOPE_ID {
						scope_id = owner.scope
					}
					include_unit, include_structure, include_ok := project_structure_for_type_ref_lookup(
						project,
						lookup,
						include_source_file_index,
						scope_id,
						include.type_ref,
						0,
					)
					if !include_ok {
						continue
					}
					resolved_source_file_index := source_file_id_index(include_unit)
					if resolved_source_file_index < 0 || resolved_source_file_index >= len(project.providers.source_files) {
						continue
					}
					resolved_unit := &project.providers.source_files[resolved_source_file_index]
					if resolved_field := structure_field(resolved_unit, include_structure, segment.name);
					   resolved_field != nil {
						field = resolved_field
						field_unit = resolved_unit
						field_owner_structure = include_structure
						break
					}
				}
			}
			if field == nil {
				return {}, false
			}
		}
		field_scope := field_unit.root_scope
		if owner := structure(field_unit, field_owner_structure); owner != nil && owner.scope != INVALID_SCOPE_ID {
			field_scope = owner.scope
		}
		fact = Type_Fact_Data {
			type_id = field.type_id,
			type_unit = field_unit.source_file_id if type_id_is_known(field.type_id) else INVALID_SOURCE_FILE_ID,
			structure = field.structure,
			structure_unit = field_unit.source_file_id if field.structure != INVALID_STRUCTURE_ID else INVALID_SOURCE_FILE_ID,
			declared_type = field.type_ref,
			has_declared_type = .Has_Type_Ref in field.flags,
			type_clause_display = field.type_ref.base_name,
			confidence = .High if field_unit.role == .Full_Source else .Low,
		}
		unknown_after_deref = false
		current_unit = field_unit
		current_structure = field.structure
		if field.structure == INVALID_STRUCTURE_ID {
			if .Has_Type_Ref in field.flags {
				field_source_file_index := source_file_id_index(field.decl_unit)
				if field_source_file_index < 0 || field_source_file_index >= len(project.providers.source_files) {
					field_source_file_index = source_file_index
				}
				scope_id := field_scope if current_unit.source_file_id != INVALID_SOURCE_FILE_ID &&
				                         project.providers.source_files[field_source_file_index].source_file_id == current_unit.source_file_id else INVALID_SCOPE_ID
				if resolved, resolved_source_file_index, resolved_ok := type_fact_from_declared_type(
					project,
					lookup,
					field_source_file_index,
					scope_id,
					field.type_ref,
					field.type_clause_form,
					field.has_type_clause_form,
					0,
				); resolved_ok && resolved.structure != INVALID_STRUCTURE_ID {
					fact.type_id = resolved.type_id
					fact.type_unit = resolved.type_unit
					fact.structure = resolved.structure
					fact.structure_unit = resolved.structure_unit
					if resolved_source_file_index >= 0 && resolved_source_file_index < len(project.providers.source_files) {
						current_unit = &project.providers.source_files[resolved_source_file_index]
					} else {
						current_unit = shared_builtin_provider()
					}
					current_structure = resolved.structure
					continue
				} else if resolved_ok && type_fact_is_known(resolved) {
					fact = resolved
				}
			}
			next_source_file_index := source_file_id_index(field.decl_unit)
			if next_source_file_index >= 0 && next_source_file_index < len(project.providers.source_files) {
				current_unit = &project.providers.source_files[next_source_file_index]
			}
			continue
		}
	}
	return fact, true
}

type_fact_from_class_member_path :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	class_handle: Symbol_Link,
	path: []Field_Access_Segment,
	access_source_file_index := -1,
	access_scope := INVALID_SCOPE_ID,
) -> (Type_Fact_Data, bool) {
	if len(path) == 0 {
		return unknown_type_fact(), true
	}
	member, member_source_file_index, ok := class_member_for_path_segment(
		project,
		lookup,
		class_handle,
		path[0],
		access_source_file_index,
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
			member_unit := &project.providers.source_files[source_file_id_index(class_handle.unit)]
			return type_fact_from_structure_path(
				project,
				lookup,
				source_file_id_index(class_handle.unit),
				member_unit,
				fact.structure,
				path[1:],
				fact,
			)
		}
		return {}, false
	}
	fact := class_member_type_fact(project, member, member_source_file_index)
	if len(path) == 1 {
		return fact, true
	}
	member_unit := &project.providers.source_files[member_source_file_index]
	if fact.structure == INVALID_STRUCTURE_ID {
		member_symbol := symbol(member_unit, member.symbol)
		member_info := entity_decl_info(member_unit, member.symbol)
		scope_id := member_symbol.scope if member_symbol != nil else member_unit.root_scope
		if member_info != nil && member_info.signature_scope != INVALID_SCOPE_ID {
			scope_id = member_info.signature_scope
		}
		type_form := ast.Data_Type_Form{}
		has_type_form := false
		if member_info != nil && member_info.member_kind == .Attribute {
			type_form = member_symbol.type_clause_form if member_symbol != nil else ast.Data_Type_Form{}
			has_type_form = member_symbol.has_type_clause_form if member_symbol != nil else false
		} else if member_info != nil {
			for param in member_info.signature_parameters {
				if param.section == .Method_Returning || param.section == .Method_Receiving {
					type_form = param.type_clause_form
					has_type_form = param.has_type_clause_form
					break
				}
			}
		}
		if fact.has_declared_type {
			if resolved, resolved_source_file_index, resolved_ok := type_fact_from_declared_type(
				project,
				lookup,
				member_source_file_index,
				scope_id,
				fact.declared_type,
				type_form,
				has_type_form,
				0,
			); resolved_ok && resolved.structure != INVALID_STRUCTURE_ID {
				fact = resolved
				member_unit = &project.providers.source_files[resolved_source_file_index]
			} else {
				if access_source_file_index >= 0 && member_source_file_index != access_source_file_index {
					return unknown_type_fact(), true
				}
				return {}, false
			}
		} else {
			if access_source_file_index >= 0 && member_source_file_index != access_source_file_index {
				return unknown_type_fact(), true
			}
			return {}, false
		}
	}
	return type_fact_from_structure_path(
		project,
		lookup,
		member_source_file_index,
		member_unit,
		fact.structure,
		path[1:],
		fact,
	)
}

class_member_for_path_segment :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	class_handle: Symbol_Link,
	segment: Field_Access_Segment,
	access_source_file_index := -1,
	access_scope := INVALID_SCOPE_ID,
) -> (Symbol_Link, int, bool) {
	if segment.selector == .Dash {
		return {}, -1, false
	}
	if segment.interface_qualified {
		if !type_exposes_interface(project, lookup, class_handle, segment.interface_name, 0) {
			return {}, -1, false
		}
		source_file_index := source_file_id_index(class_handle.unit)
		if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
			return {}, -1, false
		}
		return interface_member_by_name_with_unit(
			project,
			lookup,
			source_file_index,
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
		access_source_file_index,
		access_scope,
	)
}

class_member_in_hierarchy :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	class_handle: Symbol_Link,
	name: string,
	inherited: bool,
	access_source_file_index := -1,
	access_scope := INVALID_SCOPE_ID,
) -> (Symbol_Link, bool) {
	member, _, ok := class_member_in_hierarchy_with_unit(
		project,
		lookup,
		class_handle,
		name,
		inherited,
		access_source_file_index,
		access_scope,
	)
	return member, ok
}

class_member_in_hierarchy_with_unit :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	class_handle: Symbol_Link,
	name: string,
	inherited: bool,
	access_source_file_index := -1,
	access_scope := INVALID_SCOPE_ID,
) -> (Symbol_Link, int, bool) {
	source_file_index := source_file_id_index(class_handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return {}, -1, false
	}
	if member, member_ok := class_member_handle_lookup(project, lookup, class_handle, name); member_ok {
		info := entity_decl_info(&project.providers.source_files[source_file_id_index(member.unit)], member.symbol)
		if inherited && info != nil && info.visibility == .Private &&
		   !class_private_member_visible(project, class_handle, access_source_file_index, access_scope) {
			return {}, -1, false
		}
		return member, source_file_index, true
	}
	if member, member_source_file_index, ok := interface_member_in_class_with_unit(project, lookup, class_handle, name);
	   ok {
		return member, member_source_file_index, true
	}
	next, ok := direct_superclass_handle_lookup(project, lookup, class_handle)
	if !ok {
		return {}, -1, false
	}
	return class_member_in_hierarchy_with_unit(
		project,
		lookup,
		next,
		name,
		true,
		access_source_file_index,
		access_scope,
	)
}

class_private_member_visible :: proc(
	project: ^Project_Analysis,
	class_handle: Symbol_Link,
	access_source_file_index: int,
	access_scope: Scope_Id,
) -> bool {
	if access_source_file_index < 0 || access_source_file_index >= len(project.providers.source_files) {
		return false
	}
	access_unit := &project.providers.source_files[access_source_file_index]
	caller_symbol, ok := enclosing_class_owner_unit(access_unit, access_scope)
	if !ok {
		return false
	}
	if access_unit.source_file_id == class_handle.unit && caller_symbol == class_handle.symbol {
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
	class_handle: Symbol_Link,
	friend_name: string,
) -> bool {
	source_file_index := source_file_id_index(class_handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return false
	}
	for friend in project.providers.source_files[source_file_index].class_friends {
		if friend.class_symbol == class_handle.symbol && friend.friend_name == friend_name {
			return true
		}
	}
	return false
}

type_exposes_interface :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	handle: Symbol_Link,
	interface_name: string,
	depth: int,
) -> bool {
	if depth > len(project.providers.source_files) + 8 {
		return false
	}
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return false
	}
	unit := &project.providers.source_files[source_file_index]
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
			source_file_index,
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
	lookup: ^Project_Index,
	class_handle: Symbol_Link,
	name: string,
) -> (Symbol_Link, bool) {
	member, _, ok := interface_member_in_class_with_unit(project, lookup, class_handle, name)
	return member, ok
}

interface_member_in_class_with_unit :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	class_handle: Symbol_Link,
	name: string,
) -> (Symbol_Link, int, bool) {
	source_file_index := source_file_id_index(class_handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return {}, -1, false
	}
	unit := &project.providers.source_files[source_file_index]
	for alias in unit.member_aliases {
		if alias.owner_symbol != class_handle.symbol || alias.alias_name != name {
			continue
		}
		if member, member_source_file_index, ok := interface_member_by_name_with_unit(
			project,
			lookup,
			source_file_index,
			alias.target_interface_name,
			alias.target_member_name,
		); ok {
			return member, member_source_file_index, true
		}
	}
	for implemented in unit.implemented_interfaces {
		if implemented.owner_symbol != class_handle.symbol {
			continue
		}
		if member, member_source_file_index, ok := interface_member_by_name_with_unit(
			project,
			lookup,
			source_file_index,
			implemented.interface_name,
			name,
		); ok {
			return member, member_source_file_index, true
		}
	}
	return {}, -1, false
}

interface_member_by_name :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	interface_name, member_name: string,
) -> (Symbol_Link, bool) {
	member, _, ok := interface_member_by_name_with_unit(
		project,
		lookup,
		source_file_index,
		interface_name,
		member_name,
	)
	return member, ok
}

interface_member_by_name_with_unit :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	interface_name, member_name: string,
) -> (Symbol_Link, int, bool) {
	handle, ok := resolve_type_name_in_project_lookup(project, lookup, source_file_index, interface_name)
	if !ok {
		return {}, -1, false
	}
	return interface_member_by_handle_with_unit(project, lookup, handle, member_name, 0)
}

interface_member_by_handle :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	handle: Symbol_Link,
	member_name: string,
	depth: int,
) -> (Symbol_Link, bool) {
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
	lookup: ^Project_Index,
	handle: Symbol_Link,
	member_name: string,
	depth: int,
) -> (Symbol_Link, int, bool) {
	if depth > len(project.providers.source_files) + 8 {
		return {}, -1, false
	}
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return {}, -1, false
	}
	if member, member_ok := class_member_handle_lookup(project, lookup, handle, member_name); member_ok {
		return member, source_file_index, true
	}
	unit := &project.providers.source_files[source_file_index]
	for alias in unit.member_aliases {
		if alias.owner_symbol != handle.symbol || alias.alias_name != member_name {
			continue
		}
		target, ok := resolve_type_name_in_project_lookup(
			project,
			lookup,
			source_file_index,
			alias.target_interface_name,
		)
		if !ok {
			continue
		}
		if aliased, aliased_source_file_index, aliased_ok := interface_member_by_handle_with_unit(
			project,
			lookup,
			target,
			alias.target_member_name,
			depth + 1,
		); aliased_ok {
			return aliased, aliased_source_file_index, true
		}
	}
	for implemented in unit.implemented_interfaces {
		if implemented.owner_symbol != handle.symbol {
			continue
		}
		next, ok := resolve_type_name_in_project_lookup(
			project,
			lookup,
			source_file_index,
			implemented.interface_name,
		)
		if !ok {
			continue
		}
		if inherited, inherited_source_file_index, inherited_ok := interface_member_by_handle_with_unit(
			project,
			lookup,
			next,
			member_name,
			depth + 1,
		); inherited_ok {
			return inherited, inherited_source_file_index, true
		}
	}
	return {}, -1, false
}

resolve_type_name_in_project_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	name: string,
) -> (Symbol_Link, bool) {
	source_file_id := project.providers.source_files[source_file_index].source_file_id
	if handle, ok := root_symbol_in_source_file_lookup(project, source_file_id, .Type, name); ok {
		return handle, true
	}
	if handle, ok := root_symbol_in_visible_units_lookup(project, .Type, name, lookup.visible[source_file_index]); ok {
		return handle, true
	}
	return global_visible_root_symbol_lookup(lookup, .Type, name)
}

resolve_function_module_in_project_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	name: string,
) -> (Symbol_Link, bool) {
	source_file_id := project.providers.source_files[source_file_index].source_file_id
	if handle, ok := root_symbol_in_source_file_lookup(project, source_file_id, .Routine, name); ok && symbol_handle_is_kind(project, handle, .Module) {
		return handle, true
	}
	for visible in lookup.visible[source_file_index] {
		if handle, ok := root_symbol_in_source_file_lookup(project, visible, .Routine, name); ok && symbol_handle_is_kind(project, handle, .Module) {
			return handle, true
		}
	}
	if handle, ok := global_visible_root_symbol_lookup(lookup, .Routine, name); ok && symbol_handle_is_kind(project, handle, .Module) {
		return handle, true
	}
	if _, ok := global_visible_summary_entity_lookup(lookup, .Routine, name); ok {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, true
	}
	return {}, false
}

symbol_handle_is_kind :: proc(project: ^Project_Analysis, handle: Symbol_Link, kind: Symbol_Kind) -> bool {
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return false
	}
	s := symbol(&project.providers.source_files[source_file_index], handle.symbol)
	return s != nil && s.kind == kind
}

resolve_type_ref_handle_project_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
	type_ref: Field_Type_Ref_Data,
) -> (Symbol_Link, bool) {
	all_namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in all_namespaces {
		if !(namespace == type_ref.namespace ||
		     (type_ref.namespace == .Value && namespace == .Type)) {
			continue
		}
		source_file_id := project.providers.source_files[source_file_index].source_file_id
		if handle, ok := root_symbol_in_source_file_lookup(project, source_file_id, namespace, type_ref.base_name);
		   ok {
			return handle, true
		}
		if handle, ok := root_symbol_in_visible_units_lookup(
			project,
			namespace,
			type_ref.base_name,
			lookup.visible[source_file_index],
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
	lookup: ^Project_Index,
	current: Symbol_Link,
) -> (Symbol_Link, bool) {
	source_file_index := source_file_id_index(current.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return {}, false
	}
	super_name, ok := class_superclass_name(&project.providers.source_files[source_file_index], current.symbol)
	if !ok {
		return {}, false
	}
	return resolve_type_name_in_project_lookup(project, lookup, source_file_index, super_name)
}

root_symbol_in_source_file_lookup :: #force_inline proc(
	project: ^Project_Analysis,
	source_file_id: Source_File_Id,
	namespace: Namespace,
	name: string,
) -> (Symbol_Link, bool) {
	return root_symbol_in_source_file(project.providers.source_files[:], source_file_id, namespace, name)
}

root_symbol_in_visible_units_lookup :: proc(
	project: ^Project_Analysis,
	namespace: Namespace,
	name: string,
	visible: [dynamic]Source_File_Id,
) -> (Symbol_Link, bool) {
	for source_file_id in visible {
		if handle, ok := root_symbol_in_source_file_lookup(project, source_file_id, namespace, name); ok {
			return handle, true
		}
	}
	return {}, false
}

global_visible_root_symbol_lookup :: #force_inline proc(
	lookup: ^Project_Index,
	namespace: Namespace,
	name: string,
) -> (Symbol_Link, bool) {
	key := Root_Name_Key{namespace = namespace, name = name}
	if handle, ok := lookup.root_lookup.global[key]; ok {
		return handle, true
	}
	return {}, false
}

global_visible_summary_entity_lookup :: proc(
	lookup: ^Project_Index,
	namespace: Namespace,
	name: string,
) -> (Entity_Handle, bool) {
	if lookup == nil {
		return {}, false
	}
	return global_visible_summary_entity(&lookup.root_lookup, namespace, name)
}

summary_provider_for_type_name_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	namespace: Namespace,
	name: string,
) -> (summary: ^Summary_Provider_Input, provider: Provider_Handle, ok: bool) {
	entity, entity_ok := global_visible_summary_entity_lookup(lookup, namespace, name)
	if !entity_ok || entity.provider.kind != .Summary_Provider {
		return nil, {}, false
	}
	summary_input, summary_ok := summary_provider_slot(project, entity.provider)
	if !summary_ok {
		return nil, {}, false
	}
	return summary_input, entity.provider, true
}

type_fact_from_summary_member_path :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	type_ref: Field_Type_Ref_Data,
	path: []Field_Access_Segment,
) -> (Type_Fact_Data, bool) {
	if len(path) == 0 || !type_ref.is_ref {
		return {}, false
	}
	summary, _, summary_ok := summary_provider_for_type_name_lookup(project, lookup, .Type, type_ref.base_name)
	if !summary_ok {
		return {}, false
	}
	member, member_ok := summary_provider_class_member_lookup(summary, type_ref.base_name, path[0].name)
	if !member_ok {
		return {}, false
	}
	if len(path) > 1 {
		return unknown_type_fact(), true
	}
	fact := type_fact_from_summary_type_ref(project, member.type_ref)
	fact.confidence = .Low
	return fact, true
}

type_fact_from_summary_type_ref :: proc(
	project: ^Project_Analysis,
	ref: Summary_Provider_Type_Ref_Input,
) -> Type_Fact_Data {
	type_ref := summary_provider_field_type_ref(ref, context.allocator)
	fact := Type_Fact_Data {
		declared_type = type_ref,
		has_declared_type = type_ref.base_name != "",
		type_clause_display = ref.display,
		structure = INVALID_STRUCTURE_ID,
		structure_unit = INVALID_SOURCE_FILE_ID,
		confidence = .Low,
	}
	if is_builtin_type_name(type_ref.base_name) && len(project.providers.source_files) > 0 {
		fact.type_id = type_builtin(&project.providers.source_files[0], type_ref.base_name)
		fact.type_unit = project.providers.source_files[0].source_file_id
	}
	return fact
}

class_member_handle_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	class_handle: Symbol_Link,
	name: string,
) -> (Symbol_Link, bool) {
	source_file_index := source_file_id_index(class_handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return {}, false
	}
	namespaces := [?]Namespace{.Value, .Routine, .Type}
	for namespace in namespaces {
		key := Project_Class_Member_Key {
			class_unit = class_handle.unit,
			class_symbol = class_handle.symbol,
			namespace = namespace,
			name = name,
		}
		if entry, ok := lookup.class_scope_entries[key]; ok {
			return Symbol_Link{unit = entry.unit, symbol = entry.symbol}, true
		}
	}
	return {}, false
}

builtin_class_member_type_fact :: proc(
	project: ^Project_Analysis,
	class_handle: Symbol_Link,
	member_name: string,
) -> (Type_Fact_Data, bool) {
	source_file_index := source_file_id_index(class_handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return {}, false
	}
	s := symbol(&project.providers.source_files[source_file_index], class_handle.symbol)
	if s == nil {
		return {}, false
	}
	return builtin_class_attribute_type_fact(s.name, member_name)
}

class_member_type_fact :: proc(
	project: ^Project_Analysis,
	member: Symbol_Link,
	member_source_file_index: int,
) -> Type_Fact_Data {
	if member.symbol == INVALID_SYMBOL_ID ||
	   member_source_file_index < 0 ||
	   member_source_file_index >= len(project.providers.source_files) {
		return unknown_type_fact()
	}
	unit := &project.providers.source_files[member_source_file_index]
	info := entity_decl_info(unit, member.symbol)
	s := symbol(unit, member.symbol)
	if info == nil || s == nil {
		return unknown_type_fact()
	}
	if info.member_kind == .Attribute {
		source_file_id := unit.source_file_id
		return Type_Fact_Data {
			type_id = s.type_id,
			type_unit = source_file_id if type_id_is_known(s.type_id) else INVALID_SOURCE_FILE_ID,
			structure = s.structure,
			structure_unit = source_file_id if s.structure != INVALID_STRUCTURE_ID else INVALID_SOURCE_FILE_ID,
			declared_type = s.declared_type,
			has_declared_type = s.has_declared_type,
			type_clause_display = s.type_clause_display,
			confidence = .High if unit.role == .Full_Source else .Low,
		}
	}
	for param in info.signature_parameters {
		if param.section == .Method_Returning || param.section == .Method_Receiving {
			return Type_Fact_Data {
				type_id = param.type_id,
				type_unit = unit.source_file_id if type_id_is_known(param.type_id) else INVALID_SOURCE_FILE_ID,
				structure = INVALID_STRUCTURE_ID,
				structure_unit = INVALID_SOURCE_FILE_ID,
				declared_type = param.declared_type,
				has_declared_type = .Has_Declared_Type in param.flags,
				type_clause_display = param.type_clause_display,
				confidence = .High if unit.role == .Full_Source else .Low,
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
