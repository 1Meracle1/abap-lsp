package abap_frontend_semantic_query

import "src:ast"
import analyze "src:semantic/analyze"
import "src:tokenizer"

import "core:mem"
import "core:strings"

// Pointer-returning query procedures borrow storage from the queried Source_File_Provider.
// The returned pointers are invalid after the unit is analyzed or mutated again.
Semantic_Queries :: struct {
	unit: ^analyze.Source_File_Provider,
}

Decl_Queries :: struct {
	unit: ^analyze.Source_File_Provider,
}

Ref_Queries :: struct {
	unit: ^analyze.Source_File_Provider,
}

Sql_Queries :: struct {
	unit: ^analyze.Source_File_Provider,
}

Fact_Queries :: struct {
	unit: ^analyze.Source_File_Provider,
}

Project_Queries :: struct {
	project: ^analyze.Project_Analysis,
}

Diagnostic_Queries :: struct {
	project: ^analyze.Project_Analysis,
}

Project_Ref_Queries :: struct {
	project: ^analyze.Project_Analysis,
}

Completion_Queries :: struct {
	project: ^analyze.Project_Analysis,
	unit:    ^analyze.Source_File_Provider,
}

Completion_Item_Source :: enum {
	Lexical_Scope,
	Include_Visible,
	Summary_Provider,
	Dependency_Catalog,
}

Completion_Item :: struct {
	name:      string,
	kind:      analyze.Symbol_Kind,
	namespace: analyze.Namespace,
	provider:  analyze.Provider_Handle,
	symbol:    analyze.Symbol_Link,
	source:    Completion_Item_Source,
	range:     tokenizer.Range,
}

Completion_Item_Key :: struct {
	name:      string,
	namespace: analyze.Namespace,
	source:    Completion_Item_Source,
}

Ast_Query_Semantic_Search :: struct {
	offset:        int,
	best:          ^ast.Node,
	best_kind:     analyze.Ast_Expression_Info_Kind,
	best_priority: int,
	best_width:    int,
}

semantic :: proc(unit: ^analyze.Source_File_Provider) -> Semantic_Queries {
	return Semantic_Queries{unit = unit}
}

project_semantic :: proc(project: ^analyze.Project_Analysis) -> Project_Queries {
	return Project_Queries{project = project}
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

diagnostics :: proc(q: Project_Queries) -> Diagnostic_Queries {
	return Diagnostic_Queries{project = q.project}
}

project_refs :: proc(q: Project_Queries) -> Project_Ref_Queries {
	return Project_Ref_Queries{project = q.project}
}

completion :: proc(q: Project_Queries, unit: ^analyze.Source_File_Provider) -> Completion_Queries {
	return Completion_Queries{project = q.project, unit = unit}
}

diagnostic_project_copies :: proc(
	q: Diagnostic_Queries,
	allocator: mem.Allocator,
) -> [dynamic]analyze.Diagnostic {
	count := len(q.project.diagnostics) if q.project != nil else 0
	out := make([dynamic]analyze.Diagnostic, 0, count, allocator)
	if q.project == nil {
		return out
	}
	for diagnostic in q.project.diagnostics {
		append(&out, diagnostic)
	}
	return out
}

diagnostic_unit_copies :: proc(
	q: Diagnostic_Queries,
	unit: ^analyze.Source_File_Provider,
	allocator: mem.Allocator,
) -> [dynamic]analyze.Diagnostic {
	count := len(unit.diagnostics) if unit != nil else 0
	out := make([dynamic]analyze.Diagnostic, 0, count, allocator)
	if unit == nil {
		return out
	}
	for diagnostic in unit.diagnostics {
		append(&out, diagnostic)
	}
	return out
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
	analyze.Symbol_Link,
	bool,
) {
	s := decl_symbol_at_offset(q, offset)
	if s == nil {
		return {}, false
	}
	return analyze.Symbol_Link{unit = q.unit.source_file_id, symbol = s.id}, true
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

decl_class_member_at_offset :: proc(q: Decl_Queries, offset: int) -> ^analyze.Symbol_Data {
	best := -1
	best_width := 0
	for symbol, i in q.unit.symbols {
		info := analyze.entity_decl_info(q.unit, symbol.id)
		scope_data := analyze.scope(q.unit, symbol.scope)
		if info == nil ||
		   info.owner == analyze.INVALID_SYMBOL_ID ||
		   scope_data == nil ||
		   !(scope_data.kind == .Class || scope_data.kind == .Interface) ||
		   scope_data.owner != info.owner {
			continue
		}
		width := 0
		if analyze.range_contains_offset(symbol.decl_range, offset) {
			width = symbol.decl_range.end - symbol.decl_range.start
		} else if analyze.range_contains_offset(info.implementation_range, offset) {
			width = info.implementation_range.end - info.implementation_range.start
		} else {
			continue
		}
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return &q.unit.symbols[best] if best >= 0 else nil
}

decl_class_member :: proc(
	q: Decl_Queries,
	class_symbol: analyze.Symbol_Id,
	name: string,
) -> ^analyze.Symbol_Data {
	return analyze.unit_class_member_symbol(q.unit, class_symbol, name)
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
	handle: analyze.Symbol_Link,
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

project_ref_resolving_to :: proc(
	q: Project_Ref_Queries,
	handle: analyze.Symbol_Link,
	allocator: mem.Allocator,
) -> [dynamic]analyze.Project_Entity_Use {
	out := make([dynamic]analyze.Project_Entity_Use, 0, 4, allocator)
	if q.project == nil {
		return out
	}
	if entity, ok := analyze.entity_handle_from_symbol_handle(q.project, handle); ok {
		project_ref_append_uses_for_entity(q, entity, &out)
	}
	for &unit in q.project.providers.source_files {
		for ref in unit.references {
			if project_ref_matches_symbol(q.project, &unit, ref, handle) {
				entity, entity_ok := analyze.entity_handle_from_symbol_handle(q.project, handle)
				if !entity_ok {
					continue
				}
				project_ref_append_unique_use(
					&out,
					analyze.Project_Entity_Use {
						entity    = entity,
						provider  = analyze.source_file_provider_handle(&unit),
						reference = ref.id,
						range     = ref.range,
					},
				)
			}
		}
	}
	return out
}

project_ref_resolving_to_entity :: proc(
	q: Project_Ref_Queries,
	entity: analyze.Entity_Handle,
	allocator: mem.Allocator,
) -> [dynamic]analyze.Project_Entity_Use {
	out := make([dynamic]analyze.Project_Entity_Use, 0, 4, allocator)
	if q.project == nil {
		return out
	}
	project_ref_append_uses_for_entity(q, entity, &out)
	for &unit in q.project.providers.source_files {
		for ref in unit.references {
			if project_ref_matches_entity(q.project, &unit, ref, entity) {
				project_ref_append_unique_use(
					&out,
					analyze.Project_Entity_Use {
						entity    = entity,
						provider  = analyze.source_file_provider_handle(&unit),
						reference = ref.id,
						range     = ref.range,
					},
				)
			}
		}
	}
	return out
}

project_ref_reference_for_use :: proc(
	q: Project_Ref_Queries,
	use: analyze.Project_Entity_Use,
) -> (
	^analyze.Reference_Data,
	bool,
) {
	if q.project == nil {
		return nil, false
	}
	source_file_id, ok := project_ref_provider_source_file_id(use.provider)
	if !ok {
		return nil, false
	}
	unit := analyze.project_source_file_by_id(q.project, source_file_id)
	if unit == nil {
		return nil, false
	}
	for &ref in unit.references {
		if ref.id == use.reference {
			return &ref, true
		}
	}
	return nil, false
}

ref_entity_handle_at_offset :: proc(q: Ref_Queries, offset: int) -> (ast.Entity_Handle, bool) {
	ref := ref_reference_at_offset(q, offset)
	if ref == nil || ref.node == nil || !(.Has_Entity in ref.node.sem.flags) {
		return {}, false
	}
	return ref.node.sem.entity, true
}

ref_use_handle_at_offset :: proc(q: Ref_Queries, offset: int) -> (ast.Use_Handle, bool) {
	ref := ref_reference_at_offset(q, offset)
	if ref == nil || ref.node == nil || !(.Has_Use in ref.node.sem.flags) {
		return {}, false
	}
	return ref.node.sem.use, true
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

completion_items_at_offset :: proc(
	q: Completion_Queries,
	offset: int,
	prefix: string,
	allocator: mem.Allocator,
) -> [dynamic]Completion_Item {
	out := make([dynamic]Completion_Item, 0, 32, allocator)
	if q.unit == nil {
		return out
	}
	seen := make(map[Completion_Item_Key]bool, 64, allocator)
	canonical_prefix := analyze.canonical_name(prefix, context.temp_allocator)

	scope_id := completion_scope_at_offset(q.unit, offset)
	completion_append_scope_chain_items(
		q.unit,
		scope_id,
		.Lexical_Scope,
		canonical_prefix,
		&seen,
		&out,
	)
	if q.project != nil {
		completion_append_include_visible_items(q, canonical_prefix, &seen, &out)
		completion_append_dependency_summary_items(q, canonical_prefix, &seen, &out)
		completion_append_dependency_catalog_items(q, canonical_prefix, &seen, &out)
	}
	return out
}

completion_scope_at_offset :: proc(
	unit: ^analyze.Source_File_Provider,
	offset: int,
) -> analyze.Scope_Id {
	if unit == nil {
		return analyze.INVALID_SCOPE_ID
	}
	best := unit.root_scope
	best_width := 0
	for scope_data in unit.scopes {
		if !analyze.range_contains_offset(scope_data.range, offset) {
			continue
		}
		width := scope_data.range.end - scope_data.range.start
		if best == unit.root_scope || width < best_width {
			best = scope_data.id
			best_width = width
		}
	}
	return best
}

completion_append_scope_chain_items :: proc(
	unit: ^analyze.Source_File_Provider,
	scope_id: analyze.Scope_Id,
	source: Completion_Item_Source,
	prefix: string,
	seen: ^map[Completion_Item_Key]bool,
	out: ^[dynamic]Completion_Item,
) {
	current := scope_id
	for current != analyze.INVALID_SCOPE_ID {
		scope_data := analyze.scope(unit, current)
		if scope_data == nil {
			break
		}
		for symbol_id in scope_data.declarations {
			if symbol_data := analyze.symbol(unit, symbol_id); symbol_data != nil {
				completion_append_symbol(
					unit,
					symbol_data^,
					source,
					prefix,
					seen,
					out,
				)
			}
		}
		current = scope_data.parent
	}
}

completion_append_include_visible_items :: proc(
	q: Completion_Queries,
	prefix: string,
	seen: ^map[Completion_Item_Key]bool,
	out: ^[dynamic]Completion_Item,
) {
	if q.project == nil || q.unit == nil {
		return
	}
	source_file_index := analyze.source_file_id_index(q.unit.source_file_id)
	if source_file_index < 0 || source_file_index >= len(q.project.providers.source_files) {
		return
	}
	visible := analyze.include_visible_source_files_for_project_graph(q.project, context.temp_allocator)
	if source_file_index >= len(visible) {
		return
	}
	for source_file_id in visible[source_file_index] {
		unit := analyze.project_source_file_by_id(q.project, source_file_id)
		if unit == nil {
			continue
		}
		scope_data := analyze.scope(unit, unit.root_scope)
		if scope_data == nil {
			continue
		}
		for symbol_id in scope_data.declarations {
			if symbol_data := analyze.symbol(unit, symbol_id); symbol_data != nil {
				completion_append_symbol(
					unit,
					symbol_data^,
					.Include_Visible,
					prefix,
					seen,
					out,
				)
			}
		}
	}
}

completion_append_dependency_summary_items :: proc(
	q: Completion_Queries,
	prefix: string,
	seen: ^map[Completion_Item_Key]bool,
	out: ^[dynamic]Completion_Item,
) {
	for summary, index in q.project.providers.summaries {
		provider := analyze.provider_handle_for_dependency_summary(analyze.Provider_Id(u32(index)))
		for export, symbol_index in summary.exports {
			completion_append_summary_export(
				provider,
				analyze.Symbol_Id(u32(symbol_index)),
				export,
				.Summary_Provider,
				prefix,
				seen,
				out,
			)
		}
		for class, class_index in summary.classes {
			completion_append_summary_name(
				provider,
				analyze.Symbol_Id(u32(class_index)),
				class.name,
				.Class,
				analyze.Namespace.Type,
				.Summary_Provider,
				prefix,
				seen,
				out,
			)
		}
		for function, function_index in summary.functions {
			completion_append_summary_name(
				provider,
				analyze.Symbol_Id(u32(function_index)),
				function.name,
				.Module,
				analyze.Namespace.Routine,
				.Summary_Provider,
				prefix,
				seen,
				out,
			)
		}
		for typ, type_index in summary.types {
			completion_append_summary_name(
				provider,
				analyze.Symbol_Id(u32(type_index)),
				typ.name,
				.Type_Def,
				analyze.Namespace.Type,
				.Summary_Provider,
				prefix,
				seen,
				out,
			)
		}
		for symbol_name, symbol_index in summary.type_pool_symbols {
			kind := completion_kind_for_summary_typepool_symbol(summary, symbol_name)
			namespace := analyze.Namespace.Type
			if kind == .Constant {
				namespace = .Value
			}
			completion_append_summary_name(
				provider,
				analyze.Symbol_Id(u32(symbol_index)),
				symbol_name,
				kind,
				namespace,
				.Summary_Provider,
				prefix,
				seen,
				out,
			)
		}
	}
}

completion_append_dependency_catalog_items :: proc(
	q: Completion_Queries,
	prefix: string,
	seen: ^map[Completion_Item_Key]bool,
	out: ^[dynamic]Completion_Item,
) {
	for summary, index in q.project.providers.summaries {
		provider := analyze.provider_handle_for_dependency_summary(analyze.Provider_Id(u32(index)))
		for name in summary.provided_names {
			completion_append_summary_catalog_name(provider, name, prefix, seen, out)
		}
		if summary.object_name != "" {
			completion_append_summary_catalog_name(provider, summary.object_name, prefix, seen, out)
		}
	}
}

completion_append_summary_export :: proc(
	provider: analyze.Provider_Handle,
	symbol_id: analyze.Symbol_Id,
	export: analyze.Summary_Provider_Export_Input,
	source: Completion_Item_Source,
	prefix: string,
	seen: ^map[Completion_Item_Key]bool,
	out: ^[dynamic]Completion_Item,
) {
	if !completion_name_matches_prefix(export.name, prefix) {
		return
	}
	kind, namespace, ok := completion_summary_export_kind(export.kind)
	if !ok {
		return
	}
	completion_append_item(
		Completion_Item {
			name      = export.name,
			kind      = kind,
			namespace = namespace,
			provider  = provider,
			symbol    = analyze.Symbol_Link{unit = analyze.INVALID_SOURCE_FILE_ID, symbol = symbol_id},
			source    = source,
		},
		seen,
		out,
	)
}

completion_append_summary_name :: proc(
	provider: analyze.Provider_Handle,
	symbol_id: analyze.Symbol_Id,
	name: string,
	kind: analyze.Symbol_Kind,
	namespace: analyze.Namespace,
	source: Completion_Item_Source,
	prefix: string,
	seen: ^map[Completion_Item_Key]bool,
	out: ^[dynamic]Completion_Item,
) {
	if !completion_name_matches_prefix(name, prefix) {
		return
	}
	completion_append_item(
		Completion_Item {
			name      = name,
			kind      = kind,
			namespace = namespace,
			provider  = provider,
			symbol    = analyze.Symbol_Link{unit = analyze.INVALID_SOURCE_FILE_ID, symbol = symbol_id},
			source    = source,
		},
		seen,
		out,
	)
}

completion_append_summary_catalog_name :: proc(
	provider: analyze.Provider_Handle,
	name: string,
	prefix: string,
	seen: ^map[Completion_Item_Key]bool,
	out: ^[dynamic]Completion_Item,
) {
	if !completion_name_matches_prefix(name, prefix) {
		return
	}
	completion_append_item(
		Completion_Item {
			name      = name,
			kind      = .Class,
			namespace = .Type,
			provider  = provider,
			symbol    = analyze.Symbol_Link {
				unit   = analyze.INVALID_SOURCE_FILE_ID,
				symbol = analyze.INVALID_SYMBOL_ID,
			},
			source = .Dependency_Catalog,
		},
		seen,
		out,
	)
}

completion_summary_export_kind :: proc(kind: string) -> (analyze.Symbol_Kind, analyze.Namespace, bool) {
	if strings.equal_fold(kind, "class") {
		return .Class, .Type, true
	}
	if strings.equal_fold(kind, "interface") {
		return .Interface, .Type, true
	}
	if strings.equal_fold(kind, "type") {
		return .Type_Def, .Type, true
	}
	if strings.equal_fold(kind, "function-module") {
		return .Module, .Routine, true
	}
	if strings.equal_fold(kind, "constant") {
		return .Constant, .Value, true
	}
	if strings.equal_fold(kind, "variable") {
		return .Variable, .Value, true
	}
	return {}, {}, false
}

completion_kind_for_summary_typepool_symbol :: proc(
	summary: analyze.Summary_Provider_Input,
	name: string,
) -> analyze.Symbol_Kind {
	for export in summary.exports {
		if export.name != name {
			continue
		}
		if strings.equal_fold(export.kind, "constant") {
			return .Constant
		}
	}
	return .Type_Def
}

completion_append_symbol :: proc(
	unit: ^analyze.Source_File_Provider,
	symbol_data: analyze.Symbol_Data,
	source: Completion_Item_Source,
	prefix: string,
	seen: ^map[Completion_Item_Key]bool,
	out: ^[dynamic]Completion_Item,
) {
	if !completion_name_matches_prefix(symbol_data.name, prefix) {
		return
	}
	namespaces := [?]analyze.Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if !analyze.symbol_kind_occupies(symbol_data.kind, namespace) {
			continue
		}
		completion_append_item(
			Completion_Item {
				name      = symbol_data.name,
				kind      = symbol_data.kind,
				namespace = namespace,
				provider  = analyze.source_file_provider_handle(unit),
				symbol    = analyze.Symbol_Link {
					unit   = unit.source_file_id,
					symbol = symbol_data.id,
				},
				source = source,
				range  = symbol_data.decl_range,
			},
			seen,
			out,
		)
	}
}

completion_append_catalog_name :: proc(
	unit: ^analyze.Source_File_Provider,
	provider: analyze.Provider_Handle,
	name: string,
	prefix: string,
	seen: ^map[Completion_Item_Key]bool,
	out: ^[dynamic]Completion_Item,
) {
	if !completion_name_matches_prefix(name, prefix) {
		return
	}
	kind := analyze.Symbol_Kind.Class
	namespace := analyze.Namespace.Type
	if symbol_data, ok := completion_catalog_symbol_for_name(unit, name); ok {
		kind = symbol_data.kind
		namespaces := [?]analyze.Namespace{.Value, .Type, .Routine}
		for candidate in namespaces {
			if analyze.symbol_kind_occupies(kind, candidate) {
				namespace = candidate
				break
			}
		}
	}
	completion_append_item(
		Completion_Item {
			name      = name,
			kind      = kind,
			namespace = namespace,
			provider  = provider,
			symbol    = analyze.Symbol_Link {
				unit   = analyze.INVALID_SOURCE_FILE_ID,
				symbol = analyze.INVALID_SYMBOL_ID,
			},
			source = .Dependency_Catalog,
		},
		seen,
		out,
	)
}

completion_append_item :: proc(
	item: Completion_Item,
	seen: ^map[Completion_Item_Key]bool,
	out: ^[dynamic]Completion_Item,
) {
	if item.name == "" {
		return
	}
	key := Completion_Item_Key{name = item.name, namespace = item.namespace, source = item.source}
	if key in seen^ {
		return
	}
	seen^[key] = true
	append(out, item)
}

completion_catalog_symbol_for_name :: proc(
	unit: ^analyze.Source_File_Provider,
	name: string,
) -> (
	analyze.Symbol_Data,
	bool,
) {
	scope_data := analyze.scope(unit, unit.root_scope)
	if scope_data == nil {
		return {}, false
	}
	for symbol_id in scope_data.declarations {
		symbol_data := analyze.symbol(unit, symbol_id)
		if symbol_data != nil &&
		   !analyze.symbol_kind_is_builtin(symbol_data.kind) &&
		   strings.equal_fold(symbol_data.name, name) {
			return symbol_data^, true
		}
	}
	return {}, false
}

completion_name_matches_prefix :: proc(name, prefix: string) -> bool {
	if prefix == "" {
		return true
	}
	canonical := analyze.canonical_name(name, context.temp_allocator)
	return strings.has_prefix(canonical, prefix)
}

fact_expression_info_copy_at_offset :: proc(
	q: Fact_Queries,
	offset: int,
) -> (
	analyze.Ast_Expression_Info,
	bool,
) {
	if fact, ok := fact_expression_info_copy_from_ast_at_offset(q, offset); ok {
		return fact, true
	}
	return {}, false
}

fact_expression_info_copy_from_ast_at_offset :: proc(
	q: Fact_Queries,
	offset: int,
) -> (
	analyze.Ast_Expression_Info,
	bool,
) {
	if node, kind, ok := fact_ast_semantic_node_at_offset(q, offset); ok {
		scope := analyze.INVALID_SCOPE_ID
		if .Has_Scope in node.sem.flags {
			scope = semantic_scope_from_ast(q.unit, node.sem.scope)
		}
		return analyze.Ast_Expression_Info {
				scope     = scope,
				range     = node.range,
				node      = node,
				kind      = kind,
				type_fact = analyze.type_fact_from_type_and_value(node.sem.tav),
			},
			true
	}

	return {}, false
}

fact_operand_info_copy_at_offset :: proc(
	q: Fact_Queries,
	offset: int,
) -> (
	analyze.Ast_Operand_Info,
	bool,
) {
	if operand, ok := fact_operand_info_copy_from_ast_at_offset(q, offset); ok {
		return operand, true
	}
	return {}, false
}

fact_operand_info_copy_from_ast_at_offset :: proc(
	q: Fact_Queries,
	offset: int,
) -> (
	analyze.Ast_Operand_Info,
	bool,
) {
	if node, _, ok := fact_ast_semantic_node_at_offset(q, offset); ok {
		scope := analyze.INVALID_SCOPE_ID
		if .Has_Scope in node.sem.flags {
			scope = semantic_scope_from_ast(q.unit, node.sem.scope)
		}
		out := analyze.Ast_Operand_Info {
			scope     = scope,
			range     = node.range,
			node      = node,
			mode      = analyze.operand_mode_from_addressing_mode(node.sem.tav.mode),
			type_fact = analyze.type_fact_from_type_and_value(node.sem.tav),
		}
		if .Assignable in node.sem.flags {
			out.flags += {.Assignable}
		}
		return out, true
	}

	return {}, false
}

fact_type_and_value_at_offset :: proc(q: Fact_Queries, offset: int) -> (ast.Type_And_Value, bool) {
	if node, _, ok := fact_ast_semantic_node_at_offset(q, offset); ok {
		return node.sem.tav, true
	}
	return {}, false
}

fact_ast_semantic_node_at_offset :: proc(
	q: Fact_Queries,
	offset: int,
) -> (
	^ast.Node,
	analyze.Ast_Expression_Info_Kind,
	bool,
) {
	if q.unit == nil || q.unit.root == nil {
		return nil, {}, false
	}
	search := Ast_Query_Semantic_Search{offset = offset, best_priority = 0, best_width = 0}
	visitor := ast.Visitor{visit = fact_ast_semantic_search_visit, data = &search}
	ast.walk(&visitor, q.unit.root)
	if search.best == nil {
		return nil, {}, false
	}
	return search.best, search.best_kind, true
}

fact_ast_semantic_search_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil ||
	   !(.Has_Type_And_Value in node.sem.flags) ||
	   !analyze.range_contains_offset(node.range, (cast(^Ast_Query_Semantic_Search)v.data).offset) {
		return v
	}
	search := cast(^Ast_Query_Semantic_Search)v.data
	kind := expression_info_kind_from_ast_node(node)
	priority := expression_info_priority(kind)
	width := node.range.end - node.range.start
	if search.best == nil ||
	   priority < search.best_priority ||
	   (priority == search.best_priority && width < search.best_width) {
		search.best = node
		search.best_kind = kind
		search.best_priority = priority
		search.best_width = width
	}
	return v
}

expression_info_kind_from_ast_node :: proc(node: ^ast.Node) -> analyze.Ast_Expression_Info_Kind {
	#partial switch _ in node.derived {
	case ^ast.Call_Expr,
	     ^ast.Dynamic_Call_Method_Target_Expr,
	     ^ast.Ole_Call_Method_Target_Expr:
		return .Call_Result
	case ^ast.Selector_Expr,
	     ^ast.Interface_Qualified_Selector_Expr,
	     ^ast.Sql_Column_Expr:
		return .Selector
	case:
	}
	return .Reference
}

project_ref_append_uses_for_entity :: proc(
	q: Project_Ref_Queries,
	entity: analyze.Entity_Handle,
	out: ^[dynamic]analyze.Project_Entity_Use,
) {
	for use in analyze.project_graph_uses_of_entity(&q.project.graph, entity) {
		project_ref_append_unique_use(out, use)
	}
}

project_ref_append_unique_use :: proc(
	out: ^[dynamic]analyze.Project_Entity_Use,
	use: analyze.Project_Entity_Use,
) {
	for existing in out^ {
		if existing.entity == use.entity &&
		   existing.provider == use.provider &&
		   existing.reference == use.reference {
			return
		}
	}
	append(out, use)
}

project_ref_matches_symbol :: proc(
	project: ^analyze.Project_Analysis,
	unit: ^analyze.Source_File_Provider,
	ref: analyze.Reference_Data,
	handle: analyze.Symbol_Link,
) -> bool {
	if ref.has_resolution &&
	   ref.resolution.kind == .Symbol &&
	   ref.resolution.symbol == handle {
		return true
	}
	entity, ok := analyze.entity_handle_from_symbol_handle(project, handle)
	return ok && project_ref_matches_entity(project, unit, ref, entity)
}

project_ref_matches_entity :: proc(
	project: ^analyze.Project_Analysis,
	unit: ^analyze.Source_File_Provider,
	ref: analyze.Reference_Data,
	entity: analyze.Entity_Handle,
) -> bool {
	if ref.node != nil && .Has_Entity in ref.node.sem.flags {
		if node_entity, ok := semantic_entity_from_ast(ref.node.sem.entity); ok {
			return node_entity == entity
		}
	}
	if !ref.has_resolution {
		return false
	}
	#partial switch ref.resolution.kind {
	case .Symbol:
		if resolved, ok := analyze.entity_handle_from_symbol_handle(project, ref.resolution.symbol); ok {
			return resolved == entity
		}
	case .Provider_Entity:
		return ref.resolution.entity == entity
	case .Builtin_Type:
		if resolved, ok := analyze.builtin_entity_handle(.Type, ref.name); ok {
			return resolved == entity
		}
	case .Builtin_Routine:
		if resolved, ok := analyze.builtin_entity_handle(.Routine, ref.name); ok {
			return resolved == entity
		}
	case:
	}
	return false
}

project_ref_provider_source_file_id :: proc(provider: analyze.Provider_Handle) -> (analyze.Source_File_Id, bool) {
	#partial switch provider.kind {
	case .File:
		return analyze.Source_File_Id(u32(provider.id)), true
	case:
	}
	return analyze.INVALID_SOURCE_FILE_ID, false
}

semantic_provider_from_ast :: proc(provider: ast.Provider_Handle) -> (analyze.Provider_Handle, bool) {
	#partial switch provider.kind {
	case .Builtin:
		return analyze.Provider_Handle {
				kind     = .Builtin,
				id       = analyze.Provider_Id(u32(provider.id)),
				revision = provider.revision,
			},
			true
	case .File:
		return analyze.Provider_Handle {
				kind     = .File,
				id       = analyze.Provider_Id(u32(provider.id)),
				revision = provider.revision,
			},
			true
	case .Summary_Provider:
		return analyze.Provider_Handle {
				kind     = .Summary_Provider,
				id       = analyze.Provider_Id(u32(provider.id)),
				revision = provider.revision,
			},
			true
	case:
	}
	return {}, false
}

semantic_entity_from_ast :: proc(handle: ast.Entity_Handle) -> (analyze.Entity_Handle, bool) {
	provider, ok := semantic_provider_from_ast(handle.provider)
	if !ok || handle.id == ast.INVALID_ENTITY_ID {
		return {}, false
	}
	return analyze.Entity_Handle{provider = provider, id = analyze.Entity_Id(u32(handle.id))}, true
}

semantic_scope_from_ast :: proc(
	unit: ^analyze.Source_File_Provider,
	handle: ast.Scope_Handle,
) -> analyze.Scope_Id {
	if unit == nil || handle.id == ast.INVALID_SCOPE_ID {
		return analyze.INVALID_SCOPE_ID
	}
	provider, ok := semantic_provider_from_ast(handle.provider)
	if !ok {
		return analyze.INVALID_SCOPE_ID
	}
	expected := analyze.source_file_provider_handle(unit)
	if provider != expected {
		return analyze.INVALID_SCOPE_ID
	}
	id := analyze.Scope_Id(u32(handle.id))
	if analyze.scope(unit, id) == nil {
		return analyze.INVALID_SCOPE_ID
	}
	return id
}

expression_info_priority :: proc(kind: analyze.Ast_Expression_Info_Kind) -> int {
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
