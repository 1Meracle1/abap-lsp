package abap_frontend_lsp

import "src:semantic"
import "src:tokenizer"

import json "core:encoding/json"
import "core:mem"
import "core:slice"
import "core:strings"

Rename_Target_Edit :: struct {
	uri:   string,
	range: semantic.Range,
}

Rename_Plan :: struct {
	snapshot:    Snapshot_Lookup,
	entity:      ^semantic.Entity,
	range:       semantic.Range,
	placeholder: string,
	locations:   [dynamic]Rename_Target_Edit,
	ok:          bool,
}

handle_prepare_rename :: proc(ctx: ^Request_Context, params: json.Value) {
	response, ok := prepare_rename_for_params(ctx.state, params, ctx.state.allocator)
	if !ok {
		send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
		return
	}
	send_success(ctx.output, ctx.id, response, ctx.state.allocator)
}

handle_rename :: proc(ctx: ^Request_Context, params: json.Value) {
	edit, ok, error := rename_for_params(ctx.state, params, ctx.state.allocator)
	if error != "" {
		send_error(ctx.output, ctx.id, RPC_INVALID_PARAMS, error, ctx.state.allocator)
		return
	}
	if !ok {
		send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
		return
	}
	send_success(ctx.output, ctx.id, edit, ctx.state.allocator)
}

prepare_rename_for_params :: proc(
	state: ^Server_State,
	params: json.Value,
	allocator: mem.Allocator,
) -> (Prepare_Rename_Response, bool) {
	plan := rename_plan_for_params(state, params, allocator)
	if !plan.ok {
		return {}, false
	}
	return Prepare_Rename_Response {
			range = range_from_offsets(plan.snapshot.source, plan.range.start, plan.range.end),
			placeholder = plan.placeholder,
		},
		true
}

rename_for_params :: proc(
	state: ^Server_State,
	params: json.Value,
	allocator: mem.Allocator,
) -> (Workspace_Edit, bool, string) {
	new_name, new_name_ok := rename_new_name_from_params(params)
	if !new_name_ok {
		return {}, false, "textDocument/rename requires newName"
	}
	plan := rename_plan_for_params(state, params, allocator)
	if !plan.ok {
		return {}, false, ""
	}
	if error := validate_rename_new_name(plan.placeholder, new_name); error != "" {
		return {}, false, error
	}
	return workspace_edit_from_rename_plan(state, plan, new_name, allocator), true, ""
}

rename_plan_for_params :: proc(
	state: ^Server_State,
	params: json.Value,
	allocator: mem.Allocator,
) -> Rename_Plan {
	found := entity_at_position(state, params)
	if !found.ok || !rename_entity_is_editable(state, found.entity) {
		return {}
	}
	placeholder, placeholder_ok := source_text_for_range(found.snapshot.source, found.range)
	if !placeholder_ok {
		return {}
	}

	locations := make([dynamic]Rename_Target_Edit, 0, 8, allocator)
	append_entity_rename_locations(state, &locations, found.snapshot, found.entity)
	sort_rename_locations(locations[:])
	locations = dedupe_rename_locations(locations[:], allocator)
	if len(locations) == 0 {
		return {}
	}

	return Rename_Plan {
		snapshot = found.snapshot,
		entity = found.entity,
		range = found.range,
		placeholder = strings.clone(placeholder, allocator),
		locations = locations,
		ok = true,
	}
}

append_entity_rename_locations :: proc(
	state: ^Server_State,
	locations: ^[dynamic]Rename_Target_Edit,
	snapshot: Snapshot_Lookup,
	entity: ^semantic.Entity,
) {
	if entity == nil {
		return
	}
	if entity.source_file != nil {
		append_rename_location(locations, state, entity.source_file, entity.name_range)
	}
	if payload, ok := entity.payload.(^semantic.Entity_Routine_Payload); ok && payload != nil {
		if payload.implementation_unit != nil {
			append_rename_location(
				locations,
				state,
				payload.implementation_unit,
				payload.implementation_name_range,
			)
		}
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
		append_rename_location(
			locations,
			state,
			ref.file,
			semantic.semantic_entity_use_range(ref^),
		)
	}
}

append_rename_location :: proc(
	locations: ^[dynamic]Rename_Target_Edit,
	state: ^Server_State,
	file: ^semantic.Project_File,
	range: semantic.Range,
) {
	if file == nil || range.start >= range.end {
		return
	}
	if _, ok := state.documents[file.path]; !ok {
		return
	}
	append(locations, Rename_Target_Edit{uri = file.path, range = range})
}

workspace_edit_from_rename_plan :: proc(
	state: ^Server_State,
	plan: Rename_Plan,
	new_name: string,
	allocator: mem.Allocator,
) -> Workspace_Edit {
	changes := make(map[string][]Text_Edit, 4, allocator)
	for i := 0; i < len(plan.locations); {
		uri := plan.locations[i].uri
		source, source_ok := document_text_for_uri(state, uri)
		if !source_ok {
			i += 1
			continue
		}
		end := i + 1
		for end < len(plan.locations) && plan.locations[end].uri == uri {
			end += 1
		}
		edits := make([]Text_Edit, end - i, allocator)
		for j := i; j < end; j += 1 {
			edits[j - i] = Text_Edit {
				range = range_from_offsets(
					source,
					plan.locations[j].range.start,
					plan.locations[j].range.end,
				),
				new_text = new_name,
			}
		}
		changes[strings.clone(uri, allocator)] = edits
		i = end
	}
	return Workspace_Edit{changes = changes}
}

rename_new_name_from_params :: proc(params: json.Value) -> (string, bool) {
	object, ok := params.(json.Object)
	if !ok {
		return "", false
	}
	return object_string(object, "newName")
}

validate_rename_new_name :: proc(current, new_name: string) -> string {
	if new_name == "" {
		return "new name must not be empty"
	}
	if strings.trim_space(new_name) != new_name {
		return "new name must not contain leading or trailing whitespace"
	}
	current_is_field_symbol := rename_name_is_field_symbol(current)
	new_is_field_symbol := rename_name_is_field_symbol(new_name)
	if current_is_field_symbol && !new_is_field_symbol {
		return "field-symbol rename must keep angle brackets"
	}
	if !current_is_field_symbol && new_is_field_symbol {
		return "new name is not a valid ABAP identifier"
	}

	tokenized := tokenizer.tokenize(new_name, context.temp_allocator)
	if len(tokenized.errors) != 0 ||
	   len(tokenized.tokens) != 2 ||
	   tokenized.tokens[0].kind != .Ident ||
	   tokenized.tokens[0].range.start != 0 ||
	   tokenized.tokens[0].range.end != len(new_name) ||
	   tokenized.tokens[1].kind != .Eof {
		return "new name is not a valid ABAP identifier"
	}
	return ""
}

rename_name_is_field_symbol :: proc "contextless" (name: string) -> bool {
	return len(name) >= 2 && name[0] == '<' && name[len(name) - 1] == '>'
}

rename_entity_is_editable :: proc(state: ^Server_State, entity: ^semantic.Entity) -> bool {
	if entity == nil || .Builtin in entity.flags || entity.source_file == nil {
		return false
	}
	_, ok := state.documents[entity.source_file.path]
	return ok
}

source_text_for_range :: proc(source: string, range: semantic.Range) -> (string, bool) {
	if range.start < 0 || range.end < range.start || range.end > len(source) {
		return "", false
	}
	return source[range.start:range.end], true
}

document_text_for_uri :: proc(state: ^Server_State, uri: string) -> (string, bool) {
	if doc, ok := state.documents[uri]; ok {
		return doc.text, true
	}
	return "", false
}

sort_rename_locations :: proc(locations: []Rename_Target_Edit) {
	slice.sort_by(locations, rename_location_less)
}

rename_location_less :: proc(left, right: Rename_Target_Edit) -> bool {
	if cmp := strings.compare(left.uri, right.uri); cmp != 0 {
		return cmp < 0
	}
	if left.range.start != right.range.start {
		return left.range.start < right.range.start
	}
	return left.range.end < right.range.end
}

dedupe_rename_locations :: proc(
	locations: []Rename_Target_Edit,
	allocator: mem.Allocator,
) -> [dynamic]Rename_Target_Edit {
	out := make([dynamic]Rename_Target_Edit, 0, len(locations), allocator)
	for location in locations {
		if len(out) == 0 || out[len(out) - 1].uri != location.uri ||
		   out[len(out) - 1].range != location.range {
			append(&out, location)
		}
	}
	return out
}
