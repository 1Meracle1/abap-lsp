package abap_frontend_lsp

import "src:ast"
import "src:semantic"
import "src:utils"

import json "core:encoding/json"
import "core:fmt"
import "core:mem"
import "core:strings"

handle_code_action :: proc(ctx: ^Request_Context, params: json.Value) {
	actions := code_actions_for_params(ctx.state, params, context.temp_allocator)
	send_success(ctx.output, ctx.id, actions[:], context.temp_allocator)
}

code_actions_for_params :: proc(
	state: ^Server_State,
	params: json.Value,
	allocator: mem.Allocator,
) -> [dynamic]Code_Action {
	out := make([dynamic]Code_Action, 0, 2, allocator)
	if !code_action_context_allows_quickfix(params) {
		return out
	}
	uri := uri_from_text_document_params(params)
	if uri == "" {
		return out
	}
	snapshot := snapshot_for_uri(state, uri)
	if !snapshot.ok {
		return out
	}
	request_range, range_ok := code_action_request_offset_range(params, snapshot.source)
	if !range_ok {
		return out
	}

	query := semantic.semantic_query(snapshot.project, snapshot.checker, snapshot.file)
	if action, action_ok := code_action_fill_value_constructor_fields(
		state,
		snapshot,
		query,
		request_range,
		allocator,
	); action_ok {
		append(&out, action)
	}
	diagnostics := semantic.semantic_diagnostic_copies(
		semantic.semantic_query_diagnostics(query),
		context.temp_allocator,
	)
	for diagnostic in diagnostics {
		if diagnostic.kind != .Missing_Method_Implementation ||
		   !code_action_range_applies(request_range, diagnostic.range) {
			continue
		}
		method := diagnostic.entity
		if method == nil || method.kind != .Method {
			continue
		}
		action, action_ok := code_action_add_method_implementation(
			state,
			snapshot,
			method,
			allocator,
		)
		if action_ok {
			append(&out, action)
		}
	}
	return out
}

code_action_context_allows_quickfix :: proc(params: json.Value) -> bool {
	object, ok := params.(json.Object)
	if !ok {
		return false
	}
	context_object, context_ok := object_object(object, "context")
	if !context_ok {
		return true
	}
	only, only_ok := object_array(context_object, "only")
	if !only_ok || len(only) == 0 {
		return true
	}
	for value in only {
		kind, kind_ok := value.(json.String)
		if kind_ok && string(kind) == "quickfix" {
			return true
		}
	}
	return false
}

code_action_request_offset_range :: proc(
	params: json.Value,
	source: string,
) -> (
	semantic.Range,
	bool,
) {
	object, ok := params.(json.Object)
	if !ok {
		return {}, false
	}
	range_object, range_ok := object_object(object, "range")
	if !range_ok {
		return {}, false
	}
	start_object, start_ok := object_object(range_object, "start")
	end_object, end_ok := object_object(range_object, "end")
	if !start_ok || !end_ok {
		return {}, false
	}
	start, start_pos_ok := code_action_position_from_object(start_object)
	end, end_pos_ok := code_action_position_from_object(end_object)
	if !start_pos_ok || !end_pos_ok {
		return {}, false
	}
	start_offset := position_to_offset(source, start)
	end_offset := position_to_offset(source, end)
	if end_offset < start_offset {
		end_offset = start_offset
	}
	return semantic.Range{start = start_offset, end = end_offset}, true
}

code_action_position_from_object :: proc(object: json.Object) -> (Position, bool) {
	line, line_ok := object_integer(object, "line")
	character, character_ok := object_integer(object, "character")
	if !line_ok || !character_ok {
		return {}, false
	}
	return Position{line = line, character = character}, true
}

code_action_range_applies :: proc(request, diagnostic: semantic.Range) -> bool {
	if request.start == request.end {
		return diagnostic.start <= request.start && request.start <= diagnostic.end
	}
	return request.start < diagnostic.end && diagnostic.start < request.end
}

code_action_fill_value_constructor_fields :: proc(
	state: ^Server_State,
	snapshot: Snapshot_Lookup,
	query: semantic.Semantic_Query,
	request_range: semantic.Range,
	allocator: mem.Allocator,
) -> (
	Code_Action,
	bool,
) {
	info, info_ok := semantic.semantic_fact_value_constructor_structure_at_range(
		semantic.semantic_query_facts(query),
		request_range,
	)
	if !info_ok || info.node == nil || info.structure == nil {
		return {}, false
	}
	body_start, body_end, body_ok := code_action_value_constructor_body_range(
		snapshot.source,
		info,
	)
	if !body_ok || !code_action_source_range_is_blank(snapshot.source, body_start, body_end) {
		return {}, false
	}
	field_names := code_action_structure_field_names(state, info.structure, allocator)
	if len(field_names) == 0 {
		return {}, false
	}
	new_text := code_action_value_constructor_fields_text(
		snapshot.source,
		info.range.start,
		field_names[:],
		allocator,
	)
	if new_text == "" {
		return {}, false
	}

	changes := make(map[string][]Text_Edit, 1, allocator)
	edits := make([]Text_Edit, 1, allocator)
	edits[0] = Text_Edit {
		range = range_from_offsets(snapshot.source, body_start, body_end),
		new_text = new_text,
	}
	changes[strings.clone(snapshot.file.path, allocator)] = edits

	return Code_Action {
			title = "Fill VALUE with structure fields",
			kind = "quickfix",
			edit = Workspace_Edit{changes = changes},
			is_preferred = false,
		},
		true
}

code_action_value_constructor_body_range :: proc(
	source: string,
	info: semantic.Semantic_Value_Constructor_Info,
) -> (
	start: int,
	end: int,
	ok: bool,
) {
	if info.expr != nil {
		return code_action_constructor_body_range(source, info.expr)
	}
	if info.node == nil {
		return
	}
	if _, row_ok := info.node.derived.(^ast.Call_Arg_List_Expr); row_ok {
		return code_action_parenthesized_body_range(source, info.node.range.start, info.node.range.end)
	}
	return
}

code_action_constructor_body_range :: proc(
	source: string,
	expr: ^ast.Constructor_Expr,
) -> (
	start: int,
	end: int,
	ok: bool,
) {
	if expr == nil || expr.type_ref == nil {
		return
	}
	range_start := clamp(expr.type_ref.range.end, 0, len(source))
	range_end := clamp(expr.range.end, range_start, len(source))
	return code_action_parenthesized_body_range(source, range_start, range_end)
}

code_action_parenthesized_body_range :: proc(
	source: string,
	range_start: int,
	range_end: int,
) -> (
	start: int,
	end: int,
	ok: bool,
) {
	start_bound := clamp(range_start, 0, len(source))
	end_bound := clamp(range_end, start_bound, len(source))
	open := -1
	for i := start_bound; i < end_bound; i += 1 {
		if source[i] == '(' {
			open = i
			break
		}
	}
	if open < 0 {
		return
	}
	close := end_bound - 1
	for close > open && source[close] != ')' {
		close -= 1
	}
	if close <= open {
		return
	}
	return open + 1, close, true
}

code_action_source_range_is_blank :: proc(source: string, start, end: int) -> bool {
	lo := clamp(start, 0, len(source))
	hi := clamp(end, lo, len(source))
	for i := lo; i < hi; i += 1 {
		if !code_action_space_char(source[i]) {
			return false
		}
	}
	return true
}

code_action_space_char :: proc "contextless" (ch: u8) -> bool {
	return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'
}

code_action_structure_field_names :: proc(
	state: ^Server_State,
	structure: ^semantic.Structure,
	allocator: mem.Allocator,
) -> [dynamic]string {
	out := make([dynamic]string, 0, len(structure.fields), allocator)
	for field in structure.fields {
		if field == nil || field.kind != .Field || field.name == "" {
			continue
		}
		if payload, payload_ok := field.payload.(^semantic.Entity_Field_Payload);
		   payload_ok && payload != nil && .Is_Include in payload.flags {
			continue
		}
		source := source_for_project_file(state, field.source_file)
		name := code_action_source_text_or_name(source, field.name_range, field.name, allocator)
		append(&out, name)
	}
	return out
}

code_action_value_constructor_fields_text :: proc(
	source: string,
	constructor_start: int,
	field_names: []string,
	allocator: mem.Allocator,
) -> string {
	if len(field_names) == 0 {
		return ""
	}
	out := strings.builder_make(allocator)
	newline := code_action_newline(source)
	indent := code_action_line_indent_at_offset(source, constructor_start, allocator)
	for field_name in field_names {
		strings.write_string(&out, newline)
		strings.write_string(&out, indent)
		strings.write_string(&out, "  ")
		strings.write_string(&out, field_name)
		strings.write_string(&out, " = VALUE #( )")
	}
	strings.write_string(&out, newline)
	strings.write_string(&out, indent)
	return strings.to_string(out)
}

code_action_line_indent_at_offset :: proc(
	source: string,
	offset: int,
	allocator: mem.Allocator,
) -> string {
	line_start := code_action_line_start(source, offset)
	indent_end := line_start
	for indent_end < len(source) && (source[indent_end] == ' ' || source[indent_end] == '\t') {
		indent_end += 1
	}
	return strings.clone(source[line_start:indent_end], allocator)
}

code_action_add_method_implementation :: proc(
	state: ^Server_State,
	snapshot: Snapshot_Lookup,
	method: ^semantic.Entity,
	allocator: mem.Allocator,
) -> (
	Code_Action,
	bool,
) {
	edit_uri, edit_source, insert_offset, new_text, edit_ok :=
		code_action_method_implementation_edit(state, snapshot, method, allocator)
	if !edit_ok {
		return {}, false
	}

	changes := make(map[string][]Text_Edit, 1, allocator)
	edits := make([]Text_Edit, 1, allocator)
	edits[0] = Text_Edit {
		range = range_from_offsets(edit_source, insert_offset, insert_offset),
		new_text = new_text,
	}
	changes[strings.clone(edit_uri, allocator)] = edits

	return Code_Action {
			title = fmt.tprintf("Add implementation for method '%s'", method.name),
			kind = "quickfix",
			edit = Workspace_Edit{changes = changes},
			is_preferred = true,
		},
		true
}

code_action_method_implementation_edit :: proc(
	state: ^Server_State,
	snapshot: Snapshot_Lookup,
	method: ^semantic.Entity,
	allocator: mem.Allocator,
) -> (
	uri: string,
	source: string,
	insert_offset: int,
	new_text: string,
	ok: bool,
) {
	if method == nil || method.owner == nil || method.owner.kind != .Class {
		return
	}
	method_source := source_for_project_file(state, method.source_file)
	if method_source == "" {
		return
	}
	method_name := code_action_source_text_or_name(method_source, method.name_range, method.name, allocator)
	class_name := code_action_source_text_or_name(
		source_for_project_file(state, method.owner.source_file),
		method.owner.name_range,
		method.owner.name,
		allocator,
	)

	if impl_file, impl_source, impl_decl, impl_ok :=
	   code_action_find_class_implementation(state, snapshot, method.owner); impl_ok {
		offset, offset_ok := code_action_class_implementation_insert_offset(impl_source, impl_decl)
		if !offset_ok {
			return
		}
		return impl_file.path,
			impl_source,
			offset,
			code_action_method_implementation_stub(
				impl_source[:offset],
				method_name,
				code_action_newline(impl_source),
				allocator,
			),
			true
	}

	if method.owner.source_file == nil || method.owner.node == nil {
		return
	}
	source = source_for_project_file(state, method.owner.source_file)
	if source == "" {
		return
	}
	newline := code_action_newline(source)
	return method.owner.source_file.path,
		source,
		method.owner.node.range.end,
		code_action_class_implementation_block(class_name, method_name, newline, allocator),
		true
}

code_action_find_class_implementation :: proc(
	state: ^Server_State,
	snapshot: Snapshot_Lookup,
	class_entity: ^semantic.Entity,
) -> (
	^semantic.Project_File,
	string,
	^ast.Class_Decl,
	bool,
) {
	if snapshot.project_result == nil || class_entity == nil {
		return nil, "", nil, false
	}
	for file in snapshot.project_result.files {
		if file == nil || file.root == nil {
			continue
		}
		source := source_for_project_file(state, file)
		if source == "" {
			continue
		}
		for stmt in file.root.stmts {
			class_decl, class_ok := stmt.derived_stmt.(^ast.Class_Decl)
			if !class_ok || !(.Implementation in class_decl.flags) {
				continue
			}
			if utils.to_lower_ascii(class_decl.name.text, context.temp_allocator) == class_entity.name {
				return file, source, class_decl, true
			}
		}
	}
	return nil, "", nil, false
}

code_action_class_implementation_insert_offset :: proc(
	source: string,
	decl: ^ast.Class_Decl,
) -> (
	int,
	bool,
) {
	if decl == nil {
		return 0, false
	}
	endclass := code_action_last_index_ascii_fold(source, decl.range.start, decl.range.end, "endclass")
	if endclass < 0 {
		return decl.range.end, true
	}
	return code_action_line_start(source, endclass), true
}

code_action_last_index_ascii_fold :: proc(source: string, start, end: int, needle: string) -> int {
	if needle == "" {
		return -1
	}
	lo := clamp(start, 0, len(source))
	hi := clamp(end, lo, len(source))
	if hi - lo < len(needle) {
		return -1
	}
	for i := hi - len(needle); i >= lo; i -= 1 {
		if code_action_ascii_fold_match_at(source, i, needle) {
			return i
		}
		if i == lo {
			break
		}
	}
	return -1
}

code_action_ascii_fold_match_at :: proc(source: string, offset: int, needle: string) -> bool {
	if offset < 0 || offset + len(needle) > len(source) {
		return false
	}
	for i in 0 ..< len(needle) {
		if code_action_ascii_lower(source[offset + i]) != needle[i] {
			return false
		}
	}
	return true
}

code_action_ascii_lower :: proc "contextless" (ch: u8) -> u8 {
	if 'A' <= ch && ch <= 'Z' {
		return ch + ('a' - 'A')
	}
	return ch
}

code_action_line_start :: proc(source: string, offset: int) -> int {
	i := clamp(offset, 0, len(source))
	for i > 0 && source[i - 1] != '\n' {
		i -= 1
	}
	return i
}

code_action_source_text_or_name :: proc(
	source: string,
	range: semantic.Range,
	fallback: string,
	allocator: mem.Allocator,
) -> string {
	if source != "" && range.start >= 0 && range.start < range.end && range.end <= len(source) {
		return strings.clone(source[range.start:range.end], allocator)
	}
	return strings.clone(fallback, allocator)
}

code_action_newline :: proc(source: string) -> string {
	return "\r\n" if strings.contains(source, "\r\n") else "\n"
}

code_action_method_implementation_stub :: proc(
	source: string,
	method_name: string,
	newline: string,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	if len(source) > 0 && source[len(source) - 1] != '\n' {
		strings.write_string(&out, newline)
	}
	strings.write_string(&out, "  METHOD ")
	strings.write_string(&out, method_name)
	strings.write_byte(&out, '.')
	strings.write_string(&out, newline)
	strings.write_string(&out, "  ENDMETHOD.")
	strings.write_string(&out, newline)
	return strings.to_string(out)
}

code_action_class_implementation_block :: proc(
	class_name: string,
	method_name: string,
	newline: string,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, newline)
	strings.write_string(&out, newline)
	strings.write_string(&out, "CLASS ")
	strings.write_string(&out, class_name)
	strings.write_string(&out, " IMPLEMENTATION.")
	strings.write_string(&out, newline)
	strings.write_string(&out, "  METHOD ")
	strings.write_string(&out, method_name)
	strings.write_byte(&out, '.')
	strings.write_string(&out, newline)
	strings.write_string(&out, "  ENDMETHOD.")
	strings.write_string(&out, newline)
	strings.write_string(&out, "ENDCLASS.")
	return strings.to_string(out)
}
