package abap_frontend_lsp

import "src:ast"
import dep_store "src:dependency_store"
import remote_deps "src:remote_dependencies"
import "src:semantic"
import workspace "src:workspace"

import json "core:encoding/json"
import "core:fmt"
import "core:mem"
import net "core:net"
import "core:os"
import "core:strings"

handle_hover :: proc(ctx: ^Request_Context, params: json.Value) {
	found := entity_at_position(ctx.state, params)
	if !found.ok {
		send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
		return
	}
	text := entity_hover_text(found.snapshot.project, found.entity)
	hover := Hover {
		contents = Hover_Markup{kind = "markdown", value = text},
		range = range_from_offsets(found.snapshot.source, found.range.start, found.range.end),
	}
	send_success(ctx.output, ctx.id, hover, ctx.state.allocator)
}

handle_definition :: proc(ctx: ^Request_Context, params: json.Value) {
	found := entity_at_position(ctx.state, params)
	if !found.ok {
		send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
		return
	}
	location, location_ok := location_for_project_file_range(
		ctx.state,
		found.snapshot,
		found.entity.source_file,
		found.entity.name_range,
	)
	if !location_ok {
		send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
		return
	}
	send_success(ctx.output, ctx.id, location, ctx.state.allocator)
}

handle_implementation :: proc(ctx: ^Request_Context, params: json.Value) {
	location, ok := implementation_location_for_params(ctx.state, params)
	if !ok {
		send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
		return
	}
	send_success(ctx.output, ctx.id, location, ctx.state.allocator)
}

implementation_location_for_params :: proc(
	state: ^Server_State,
	params: json.Value,
) -> (
	Location,
	bool,
) {
	found := entity_at_position(state, params)
	if !found.ok {
		return {}, false
	}
	payload, payload_ok := found.entity.payload.(^semantic.Entity_Routine_Payload)
	if !payload_ok || payload == nil || payload.implementation_unit == nil {
		return {}, false
	}
	return location_for_project_file_range(
		state,
		found.snapshot,
		payload.implementation_unit,
		payload.implementation_name_range,
	)
}

handle_references :: proc(ctx: ^Request_Context, params: json.Value) {
	locations := reference_locations_for_params(ctx.state, params, context.temp_allocator)
	send_success(ctx.output, ctx.id, locations[:], context.temp_allocator)
}

reference_locations_for_params :: proc(
	state: ^Server_State,
	params: json.Value,
	allocator: mem.Allocator,
) -> [dynamic]Location {
	locations := make([dynamic]Location, 0, 4, allocator)
	found := entity_at_position(state, params)
	if !found.ok {
		return locations
	}
	append_entity_reference_locations(state, &locations, found.snapshot, found.entity)
	return locations
}

append_entity_reference_locations :: proc(
	state: ^Server_State,
	locations: ^[dynamic]Location,
	snapshot: Snapshot_Lookup,
	entity: ^semantic.Entity,
) {
	if entity == nil {
		return
	}
	append_reference_location(state, locations, snapshot, entity.source_file, entity.name_range)

	if payload, ok := entity.payload.(^semantic.Entity_Routine_Payload);
	   ok && payload != nil && payload.implementation_unit != nil {
		append_reference_location(
			state,
			locations,
			snapshot,
			payload.implementation_unit,
			payload.implementation_name_range,
		)
	}

	query := semantic.semantic_query(snapshot.project, snapshot.checker)
	refs := semantic.semantic_ref_resolving_to_entity(
		semantic.semantic_query_refs(query),
		entity,
		context.temp_allocator,
	)
	for ref in refs {
		if ref == nil || ref.file == nil {
			continue
		}
		append_reference_location(
			state,
			locations,
			snapshot,
			ref.file,
			semantic.semantic_entity_use_range(ref^),
		)
	}
}

append_reference_location :: proc(
	state: ^Server_State,
	locations: ^[dynamic]Location,
	snapshot: Snapshot_Lookup,
	file: ^semantic.Project_File,
	range: semantic.Range,
) {
	location, ok := location_for_project_file_range(state, snapshot, file, range)
	if !ok || location_present(locations[:], location) {
		return
	}
	append(locations, location)
}

entity_label :: proc(project: ^semantic.Project, entity: ^semantic.Entity) -> string {
	if entity == nil {
		return ""
	}
	return fmt.tprintf("`%s` %s", entity.name, entity_kind_label(entity.kind))
}

entity_detail :: proc(project: ^semantic.Project, entity: ^semantic.Entity) -> string {
	if entity == nil {
		return ""
	}
	type_text := type_label(project, entity.type)
	if type_text == "" || type_text == "unknown" {
		if declared := declared_entity_type_label(entity); declared != "" {
			type_text = declared
		}
	}
	if type_text == "" {
		return ""
	}
	return fmt.tprintf("type: `%s`", type_text)
}

entity_hover_text :: proc(project: ^semantic.Project, entity: ^semantic.Entity) -> string {
	signature := entity_hover_signature(entity)
	documentation := entity_documentation(project, entity)
	out := strings.builder_make(context.temp_allocator)
	if signature != "" {
		write_markdown_code_block(&out, "abap", signature)
	} else {
		label := entity_label(project, entity)
		detail := entity_detail(project, entity)
		strings.write_string(&out, label)
		if detail != "" {
			strings.write_string(&out, "\n\n")
			strings.write_string(&out, detail)
		}
	}
	if documentation != "" {
		strings.write_string(&out, "\n\n")
		strings.write_string(&out, documentation)
	}
	return strings.to_string(out)
}

entity_hover_signature :: proc(entity: ^semantic.Entity) -> string {
	if entity == nil {
		return ""
	}
	if entity.kind == .Method {
		return method_hover_signature(entity)
	}
	if payload, ok := entity.payload.(^semantic.Entity_Routine_Payload);
	   ok && payload != nil && payload.signature != "" {
		return payload.signature
	}
	return ""
}

method_hover_signature :: proc(entity: ^semantic.Entity) -> string {
	payload, ok := entity.payload.(^semantic.Entity_Routine_Payload)
	if !ok || payload == nil || payload.signature_scope == nil {
		return ""
	}
	if signature := method_hover_signature_from_decl(entity, payload.signature_scope.decl_info);
	   signature != "" {
		return signature
	}
	return method_hover_signature_from_decl(entity, entity.decl_info)
}

method_hover_signature_from_decl :: proc(
	entity: ^semantic.Entity,
	info: ^semantic.Decl_Info,
) -> string {
	if entity == nil || info == nil || info.decl_node == nil {
		return ""
	}
	oop, ok := info.decl_node.derived.(^ast.Oop_Simple_Stmt)
	if !ok || (oop.kind != .Methods && oop.kind != .Class_Methods) {
		return ""
	}
	for member in oop.members {
		if member.name.range == entity.name_range {
			return ast.print_oop_member_signature(
				oop.kind,
				member,
				context.temp_allocator,
				ast.Print_Options{newline = "\n", indent = "  "},
			)
		}
	}
	return ""
}

write_markdown_code_block :: proc(out: ^strings.Builder, language, text: string) {
	strings.write_string(out, "```")
	strings.write_string(out, language)
	strings.write_byte(out, '\n')
	strings.write_string(out, text)
	if len(text) == 0 || text[len(text) - 1] != '\n' {
		strings.write_byte(out, '\n')
	}
	strings.write_string(out, "```")
}

entity_documentation :: proc(project: ^semantic.Project, entity: ^semantic.Entity) -> string {
	if entity == nil {
		return ""
	}
	if .Builtin in entity.flags {
		if payload, ok := entity.payload.(^semantic.Entity_Builtin_Payload);
		   ok && payload != nil && payload.docs != "" {
			return payload.docs
		}
		if entity.kind == .Field && entity.owner != nil {
			if docs := semantic.checker_builtin_structure_field_description(
				entity.owner.name,
				entity.name,
			); docs != "" {
				return docs
			}
		}
		if docs := semantic.checker_builtin_symbol_description(entity.name, entity.kind);
		   docs != "" {
			return docs
		}
	}
	if entity.decl_info == nil {
		return ""
	}
	return decl_info_documentation(entity.decl_info, context.temp_allocator)
}

decl_info_documentation :: proc(info: ^semantic.Decl_Info, allocator: mem.Allocator) -> string {
	if info == nil {
		return ""
	}
	out := strings.builder_make(allocator)
	write_comment_documentation(&out, info.docs)
	if len(info.docs) > 0 && len(info.comment) > 0 {
		strings.write_byte(&out, '\n')
	}
	write_comment_documentation(&out, info.comment)
	return strings.to_string(out)
}

write_comment_documentation :: proc(out: ^strings.Builder, trivia: []ast.Ast_Trivia) {
	wrote := false
	for item in trivia {
		if item.kind != .Comment {
			continue
		}
		text := clean_comment_documentation(item.text)
		if text == "" {
			continue
		}
		if wrote {
			strings.write_byte(out, '\n')
		}
		strings.write_string(out, text)
		wrote = true
	}
}

clean_comment_documentation :: proc(text: string) -> string {
	trimmed := strings.trim_space(text)
	if len(trimmed) > 0 && (trimmed[0] == '"' || trimmed[0] == '*') {
		trimmed = strings.trim_space(trimmed[1:])
	}
	return trimmed
}

entity_kind_label :: proc(kind: semantic.Entity_Kind) -> string {
	#partial switch kind {
	case .Type_Def:
		return "type"
	case .Field_Symbol:
		return "field-symbol"
	case .Invalid:
		return "symbol"
	}
	raw := fmt.tprintf("%v", kind)
	lower := strings.to_lower(raw, context.temp_allocator)
	label, _ := strings.replace_all(lower, "_", " ", context.temp_allocator)
	return label
}

declared_entity_type_label :: proc(entity: ^semantic.Entity) -> string {
	if entity == nil || entity.decl_info == nil || entity.decl_info.type_clause == nil {
		return ""
	}
	return declared_type_clause_label(entity.decl_info.type_clause)
}

declared_type_clause_label :: proc(clause: ^ast.Data_Type_Clause) -> string {
	if clause == nil {
		return ""
	}
	base := declared_type_ref_label(clause.type_ref)
	#partial switch clause.form {
	case .Ref_To:
		return fmt.tprintf("ref to %s", base) if base != "" else "ref"
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
		return fmt.tprintf("table of %s", base) if base != "" else "table"
	case .Like_Line_Of, .Type_Line_Of:
		return fmt.tprintf("line of %s", base) if base != "" else ""
	case .Range_Of:
		return fmt.tprintf("range of %s", base) if base != "" else "range"
	}
	return base
}

declared_type_ref_label :: proc(expr: ^ast.Expr) -> string {
	if expr == nil {
		return ""
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Type_Ref_Expr:
		if n.name.text != "" {
			return n.name.text
		}
		return n.source.text
	case ^ast.Ident_Expr:
		return n.name
	case ^ast.Literal_Expr:
		return n.value
	}
	return ""
}

type_label :: proc(project: ^semantic.Project, typ: ^semantic.Type) -> string {
	if typ == nil {
		return ""
	}
	switch typ.kind {
	case .Builtin, .Named, .Class, .Interface:
		if typ.name != "" {
			return typ.name
		}
	case .Structure:
		if typ.structure != nil && typ.structure.name != "" {
			return typ.structure.name
		}
		return "structure"
	case .Table:
		base := type_label(project, typ.base)
		if !table_type_form_has_row_type(typ.table_form) && (base == "" || base == "unknown") {
			return table_type_form_label(typ.table_form)
		}
		if base == "" {
			base = "unknown"
		}
		return fmt.tprintf("%s OF %s", table_type_form_label(typ.table_form), base)
	case .Ref:
		base := type_label(project, typ.base)
		return fmt.tprintf("ref to %s", base) if base != "" else "ref"
	case .Routine:
		return "routine"
	case .Unknown:
		return "unknown"
	}
	return ""
}

table_type_form_label :: proc(form: ast.Data_Type_Form) -> string {
	#partial switch form {
	case .Any_Table:
		return "ANY TABLE"
	case .Index_Table:
		return "INDEX TABLE"
	case .Sorted_Table, .Like_Sorted_Table:
		return "SORTED TABLE"
	case .Hashed_Table, .Like_Hashed_Table:
		return "HASHED TABLE"
	case .Standard_Table, .Like_Standard_Table:
		return "STANDARD TABLE"
	case .Range_Of:
		return "RANGE"
	case .Table, .Like_Table:
	}
	return "TABLE"
}

table_type_form_has_row_type :: proc(form: ast.Data_Type_Form) -> bool {
	#partial switch form {
	case .Any_Table, .Index_Table:
		return false
	}
	return true
}

location_for_project_file_range :: proc(
	state: ^Server_State,
	snapshot: Snapshot_Lookup,
	file: ^semantic.Project_File,
	range: semantic.Range,
) -> (
	Location,
	bool,
) {
	if file == nil || range.start >= range.end {
		return {}, false
	}
	source := source_for_project_file(state, file)
	if source == "" && file == snapshot.file {
		source = snapshot.source
	}
	if source == "" {
		return {}, false
	}
	uri := file.path
	if state.materialize_dependency_documents {
		if materialized_uri, materialized_ok := materialize_dependency_document_uri(
			state,
			file.path,
			source,
			context.temp_allocator,
		); materialized_ok {
			uri = materialized_uri
		}
	}
	return Location{uri = uri, range = range_from_offsets(source, range.start, range.end)}, true
}

source_for_project_file :: proc(state: ^Server_State, file: ^semantic.Project_File) -> string {
	if file == nil {
		return ""
	}
	if doc, ok := state.documents[file.path]; ok {
		return doc.text
	}
	path, path_ok := file_uri_to_path(file.path, context.temp_allocator)
	if path_ok {
		source, source_ok := workspace.read_text_file(path, context.temp_allocator)
		if source_ok {
			return source
		}
	}
	if source, ok := dependency_source_for_uri(state, file.path, context.temp_allocator); ok {
		return source
	}
	if file.root != nil {
		return ast.print_node(file.root, context.temp_allocator)
	}
	return ""
}

handle_read_dependency_document :: proc(ctx: ^Request_Context, params: json.Value) {
	uri, ok := read_dependency_document_uri_from_params(params)
	if !ok {
		send_error(
			ctx.output,
			ctx.id,
			RPC_INVALID_PARAMS,
			"abapls/readDependencyDocument requires uri",
			ctx.state.allocator,
		)
		return
	}
	if source, source_ok := read_dependency_document_source(
		ctx.state,
		uri,
		context.temp_allocator,
	); source_ok {
		send_success(
			ctx.output,
			ctx.id,
			Read_Dependency_Document_Result{source_text = source},
			context.temp_allocator,
		)
		return
	}
	send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
}

read_dependency_document_uri_from_params :: proc(params: json.Value) -> (string, bool) {
	object, ok := params.(json.Object)
	if !ok {
		return "", false
	}
	uri, uri_ok := object_string(object, "uri")
	if !uri_ok || strings.trim_space(uri) == "" {
		return "", false
	}
	return normalize_lsp_uri(uri, context.temp_allocator), true
}

read_dependency_document_source :: proc(
	state: ^Server_State,
	uri: string,
	allocator: mem.Allocator,
) -> (
	string,
	bool,
) {
	if doc, ok := state.documents[uri]; ok {
		return strings.clone(doc.text, allocator), true
	}
	if source, ok := dependency_source_for_uri(state, uri, allocator); ok {
		return source, true
	}
	return dependency_ast_source_for_uri(state, uri, allocator)
}

dependency_source_for_uri :: proc(
	state: ^Server_State,
	uri: string,
	allocator: mem.Allocator,
) -> (
	string,
	bool,
) {
	object_kind, object_name, ok := dependency_object_from_virtual_uri(uri, allocator)
	if !ok {
		return "", false
	}
	for &slot in state.workspaces {
		config := workspace.remote_dependency_config_from_workspace(&slot.root)
		source, source_ok, _ := remote_deps.open_source(
			&config,
			object_kind,
			object_name,
			allocator,
		)
		if source_ok && source.source_text != "" {
			return source.source_text, true
		}
	}
	return "", false
}

dependency_ast_source_for_uri :: proc(
	state: ^Server_State,
	uri: string,
	allocator: mem.Allocator,
) -> (
	string,
	bool,
) {
	for &slot in state.workspaces {
		if !slot.has_analysis {
			continue
		}
		analysis := semantic.semantic_graph_session_current_analysis(&slot.analysis.session)
		if analysis == nil {
			continue
		}
		for &result in analysis.project_results {
			file := project_file_for_uri(&result, uri)
			if file != nil && file.root != nil {
				return ast.print_node(file.root, allocator), true
			}
		}
	}
	return "", false
}

materialize_dependency_document_uri :: proc(
	state: ^Server_State,
	uri: string,
	source: string,
	allocator: mem.Allocator,
) -> (
	string,
	bool,
) {
	if source == "" {
		return "", false
	}
	path, path_ok := materialized_dependency_document_path(state, uri, allocator)
	if !path_ok {
		return "", false
	}
	parent := os.dir(path)
	if parent != "" && parent != "." {
		if os.make_directory_all(parent) != nil {
			return "", false
		}
	}
	if os.write_entire_file(path, source) != nil {
		return "", false
	}
	return file_uri_from_path(path, allocator)
}

materialized_dependency_document_path :: proc(
	state: ^Server_State,
	uri: string,
	allocator: mem.Allocator,
) -> (
	string,
	bool,
) {
	object_kind, object_name, ok := dependency_object_from_virtual_uri(uri, context.temp_allocator)
	if !ok {
		return "", false
	}
	store_path, store_path_ok := dep_store.resolve_dependency_store_path(
		state.options.dependency_store_path,
		context.temp_allocator,
	)
	if !store_path_ok {
		return "", false
	}
	cache_root, root_err := os.join_path(
		{os.dir(store_path), "dependency-documents"},
		context.temp_allocator,
	)
	if root_err != nil {
		return "", false
	}
	kind := dependency_document_path_segment(object_kind, "dependency", context.temp_allocator)
	stem := dependency_document_path_segment(object_name, "document", context.temp_allocator)
	file_name := strings.concatenate({stem, ".abap"}, context.temp_allocator)
	path, path_err := os.join_path({cache_root, kind, file_name}, allocator)
	return path, path_err == nil
}

dependency_document_path_segment :: proc(
	value: string,
	fallback: string,
	allocator: mem.Allocator,
) -> string {
	lower := strings.to_lower(strings.trim_space(value), context.temp_allocator)
	out := strings.builder_make(allocator)
	wrote := false
	last_separator := false
	for ch in lower {
		if (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '_' || ch == '-' {
			strings.write_rune(&out, ch)
			wrote = true
			last_separator = false
			continue
		}
		if wrote && !last_separator {
			strings.write_byte(&out, '_')
			last_separator = true
		}
	}
	segment := strings.to_string(out)
	for len(segment) > 0 && segment[len(segment) - 1] == '_' {
		segment = segment[:len(segment) - 1]
	}
	if segment == "" {
		return strings.clone(fallback, allocator)
	}
	return segment
}

dependency_object_from_virtual_uri :: proc(
	uri: string,
	allocator: mem.Allocator,
) -> (
	object_kind, object_name: string,
	ok: bool,
) {
	lower := strings.to_lower(uri, context.temp_allocator)
	if strings.has_prefix(lower, "abapls-cache:") {
		return dependency_object_from_cache_uri(uri[len("abapls-cache:"):], allocator)
	}
	return "", "", false
}

dependency_object_from_cache_uri :: proc(
	raw_uri: string,
	allocator: mem.Allocator,
) -> (
	object_kind, object_name: string,
	ok: bool,
) {
	path := raw_uri
	if query_start := strings.index_byte(path, '?'); query_start >= 0 {
		path = path[:query_start]
	}
	if fragment_start := strings.index_byte(path, '#'); fragment_start >= 0 {
		path = path[:fragment_start]
	}
	for len(path) > 0 && path[0] == '/' {
		path = path[1:]
	}
	separator := strings.index_byte(path, '/')
	if separator < 0 {
		return "", "", false
	}
	kind := strings.trim_space(path[:separator])
	name := strings.trim_space(path[separator + 1:])
	if kind == "" || name == "" {
		return "", "", false
	}
	lower_name := strings.to_lower(name, context.temp_allocator)
	if strings.has_suffix(lower_name, ".abap") {
		name = name[:len(name) - len(".abap")]
	} else if strings.has_suffix(lower_name, ".xml") {
		name = name[:len(name) - len(".xml")]
	}
	if decoded, decode_ok := net.percent_decode(name, allocator); decode_ok {
		name = decoded
	} else {
		name = strings.clone(name, allocator)
	}
	return strings.clone(kind, allocator), name, true
}

location_present :: proc(locations: []Location, location: Location) -> bool {
	for existing in locations {
		if existing.uri == location.uri && existing.range == location.range {
			return true
		}
	}
	return false
}
