package abap_frontend_lsp

import execution "src:execution"
import "src:parser"
import "src:semantic"
import string_interner "src:string_interner"
import workspace "src:workspace"

import json "core:encoding/json"
import "core:fmt"
import "core:mem"
import net "core:net"
import "core:os"
import "core:slice"
import "core:strings"

Document :: struct {
	uri:     string,
	text:    string,
	version: int,
}

Parse_Diagnostic_Bucket :: struct {
	uri:    string,
	errors: [dynamic]parser.Parse_Error,
}

Server_State :: struct {
	allocator:          mem.Allocator,
	options:            workspace.Options,
	pool:               execution.Pool,
	documents:          map[string]Document,
	parse_diagnostics:  [dynamic]Parse_Diagnostic_Bucket,
	opened_workspace:   workspace.Workspace,
	has_workspace:      bool,
	analysis:           workspace.Analysis_Result,
	has_analysis:       bool,
	initialized:        bool,
	shutdown_requested: bool,
}

Request_Context :: struct {
	state:  ^Server_State,
	output: ^os.File,
	id:     json.Value,
}

Snapshot_Lookup :: struct {
	project_result: ^semantic.Workspace_Project_Result,
	project:        ^semantic.Project,
	checker:        ^semantic.Checker,
	file:           ^semantic.Project_File,
	source:         string,
	ok:             bool,
}

Entity_Lookup :: struct {
	snapshot: Snapshot_Lookup,
	entity:   ^semantic.Entity,
	range:    semantic.Range,
	ok:       bool,
}

Pending_Token :: struct {
	start:      int,
	end:        int,
	token_type: u32,
	modifiers:  u32,
	priority:   int,
}

Semantic_Token_Type_Indices :: struct {
	type_:       u32,
	class:       u32,
	interface_:  u32,
	parameter:   u32,
	variable:    u32,
	property:    u32,
	function:    u32,
	method:      u32,
	event:       u32,
	namespace:   u32,
	enum_member: u32,
}

TOKEN_TYPE_INDICES :: Semantic_Token_Type_Indices {
	type_       = 0,
	class       = 1,
	interface_  = 2,
	parameter   = 3,
	variable    = 4,
	property    = 5,
	function    = 6,
	method      = 7,
	event       = 8,
	namespace   = 9,
	enum_member = 10,
}

serve_stdio :: proc(allocator: mem.Allocator) -> int {
	state: Server_State
	server_init(&state, allocator)
	defer server_destroy(&state)

	for {
		frame := read_frame(os.stdin, allocator)
		switch frame.status {
		case .Closed:
			return 0
		case .Error:
			fmt.eprintf("abap_language_server: JSON-RPC frame error: %s\n", frame.error)
			return 1
		case .Ok:
		}

		should_exit := server_handle_payload(&state, frame.payload, os.stdout)
		if should_exit {
			return 0
		}
	}
}

server_init :: proc(state: ^Server_State, allocator: mem.Allocator) {
	state^ = Server_State {
		allocator         = allocator,
		options           = workspace.Options{},
		documents         = make(map[string]Document, 16, allocator),
		parse_diagnostics = make([dynamic]Parse_Diagnostic_Bucket, 0, 16, allocator),
	}
	execution.pool_init(
		&state.pool,
		execution.Options {
			worker_count = execution.AUTO_WORKER_COUNT,
			queue_capacity = 128,
			deque_capacity = 128,
		},
		allocator,
	)
	if state.pool.options.worker_count > 0 {
		execution.pool_start(&state.pool)
	}
}

server_destroy :: proc(state: ^Server_State) {
	if state.has_analysis {
		workspace.analysis_result_destroy(&state.analysis, state.allocator)
	}
	if state.has_workspace {
		workspace.workspace_destroy(&state.opened_workspace, state.allocator)
	}
	execution.pool_destroy(&state.pool)
}

server_handle_payload :: proc(state: ^Server_State, payload: []byte, output: ^os.File) -> bool {
	message := parse_rpc_message(payload, state.allocator)
	if !message.ok {
		send_error(output, json.Null(nil), RPC_INVALID_REQUEST, message.error, state.allocator)
		return false
	}
	if message.method == METHOD_EXIT {
		return true
	}
	if message.has_id {
		ctx := Request_Context {
			state  = state,
			output = output,
			id     = message.id,
		}
		handle_request(&ctx, message.method, message.params)
	} else {
		handle_notification(state, output, message.method, message.params)
	}
	return false
}

handle_request :: proc(ctx: ^Request_Context, method: string, params: json.Value) {
	switch method {
	case METHOD_INITIALIZE:
		handle_initialize(ctx, params)
	case METHOD_SHUTDOWN:
		ctx.state.shutdown_requested = true
		send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
	case METHOD_COMPLETION:
		handle_completion(ctx, params)
	case METHOD_HOVER:
		handle_hover(ctx, params)
	case METHOD_DEFINITION:
		handle_definition(ctx, params)
	case METHOD_REFERENCES:
		handle_references(ctx, params)
	case METHOD_SEMANTIC_TOKENS_FULL:
		handle_semantic_tokens(ctx, params)
	case METHOD_FOLDING_RANGE:
		handle_folding_ranges(ctx, params)
	case:
		send_error(
			ctx.output,
			ctx.id,
			RPC_METHOD_NOT_FOUND,
			"method not found",
			ctx.state.allocator,
		)
	}
}

handle_notification :: proc(
	state: ^Server_State,
	output: ^os.File,
	method: string,
	params: json.Value,
) {
	switch method {
	case METHOD_INITIALIZED:
		state.initialized = true
	case METHOD_DID_OPEN:
		if update_document_from_open(state, params) {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_CHANGE:
		if update_document_from_change(state, params) {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_CLOSE:
		if close_document(state, params) {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case:
	}
}

handle_initialize :: proc(ctx: ^Request_Context, params: json.Value) {
	state := ctx.state
	if object, ok := json_object(params); ok {
		if init_options, init_ok := object_object(object, "initializationOptions"); init_ok {
			if path, path_ok := object_string(init_options, "dependencyCachePath"); path_ok {
				state.options.dependency_store_path = strings.clone(path, state.allocator)
			}
		}
		root_uri := initialize_root_uri(object)
		if root_uri != "" {
			open_workspace_for_uri(state, root_uri)
		}
	}
	state.initialized = true
	send_success(ctx.output, ctx.id, initialize_result(state.allocator), state.allocator)
}

initialize_root_uri :: proc(object: json.Object) -> string {
	if folders, ok := object_array(object, "workspaceFolders"); ok && len(folders) > 0 {
		if folder, folder_ok := json_object(folders[0]); folder_ok {
			if uri, uri_ok := object_string(folder, "uri"); uri_ok {
				return uri
			}
		}
	}
	if uri, ok := object_string(object, "rootUri"); ok {
		return uri
	}
	return ""
}

initialize_result :: proc(allocator: mem.Allocator) -> Initialize_Result_JSON {
	_ = allocator
	return Initialize_Result_JSON {
		capabilities = Server_Capabilities_JSON {
			text_document_sync = TEXT_DOCUMENT_SYNC_FULL,
			hover_provider = true,
			definition_provider = true,
			references_provider = true,
			completion_provider = Completion_Options_JSON {
				trigger_characters = initialize_trigger_characters(allocator),
			},
			semantic_tokens_provider = Semantic_Tokens_Options_JSON {
				legend = Semantic_Tokens_Legend_JSON {
					token_types = initialize_semantic_token_types(allocator),
					token_modifiers = initialize_semantic_token_modifiers(allocator),
				},
				full = true,
			},
			folding_range_provider = true,
		},
		server_info = Server_Info_JSON{name = "abap-lsp-odin", version = "0.1.0"},
	}
}

initialize_trigger_characters :: proc(allocator: mem.Allocator) -> []string {
	out := make([]string, 3, allocator)
	out[0] = "-"
	out[1] = ">"
	out[2] = "~"
	return out
}

initialize_semantic_token_types :: proc(allocator: mem.Allocator) -> []string {
	out := make([]string, 11, allocator)
	out[0] = "type"
	out[1] = "class"
	out[2] = "interface"
	out[3] = "parameter"
	out[4] = "variable"
	out[5] = "property"
	out[6] = "function"
	out[7] = "method"
	out[8] = "event"
	out[9] = "namespace"
	out[10] = "enumMember"
	return out
}

initialize_semantic_token_modifiers :: proc(allocator: mem.Allocator) -> []string {
	out := make([]string, 2, allocator)
	out[0] = "declaration"
	out[1] = "readonly"
	return out
}

Initialize_Result_JSON :: struct {
	capabilities: Server_Capabilities_JSON `json:"capabilities"`,
	server_info:  Server_Info_JSON `json:"serverInfo"`,
}

Server_Info_JSON :: struct {
	name:    string `json:"name"`,
	version: string `json:"version"`,
}

Server_Capabilities_JSON :: struct {
	text_document_sync:       int `json:"textDocumentSync"`,
	hover_provider:           bool `json:"hoverProvider"`,
	definition_provider:      bool `json:"definitionProvider"`,
	references_provider:      bool `json:"referencesProvider"`,
	completion_provider:      Completion_Options_JSON `json:"completionProvider"`,
	semantic_tokens_provider: Semantic_Tokens_Options_JSON `json:"semanticTokensProvider"`,
	folding_range_provider:   bool `json:"foldingRangeProvider"`,
}

Completion_Options_JSON :: struct {
	trigger_characters: []string `json:"triggerCharacters"`,
}

Semantic_Tokens_Options_JSON :: struct {
	legend: Semantic_Tokens_Legend_JSON `json:"legend"`,
	full:   bool `json:"full"`,
}

Semantic_Tokens_Legend_JSON :: struct {
	token_types:     []string `json:"tokenTypes"`,
	token_modifiers: []string `json:"tokenModifiers"`,
}

open_workspace_for_uri :: proc(state: ^Server_State, uri: string) -> bool {
	path := file_uri_to_path(uri, state.allocator) or_return
	if state.has_workspace {
		workspace.workspace_destroy(&state.opened_workspace, state.allocator)
		state.has_workspace = false
	}
	opened, ok, _ := workspace.open_workspace(path, state.options, state.allocator)
	if !ok {
		opened, ok, _ = workspace.open_standalone_workspace(path, state.options, state.allocator)
	}
	if !ok {
		return false
	}
	state.opened_workspace = opened
	state.has_workspace = true
	return true
}

ensure_workspace_for_document :: proc(state: ^Server_State, uri: string) {
	if state.has_workspace {
		return
	}
	path, path_ok := file_uri_to_path(uri, context.temp_allocator)
	root := "."
	if path_ok {
		root = os.dir(path)
	}
	opened, ok, _ := workspace.open_standalone_workspace(root, state.options, state.allocator)
	if ok {
		state.opened_workspace = opened
		state.has_workspace = true
	}
}

update_document_from_open :: proc(state: ^Server_State, params: json.Value) -> bool {
	object := json_object(params) or_return
	text_document := object_object(object, "textDocument") or_return
	uri := object_string(text_document, "uri") or_return
	text := object_string(text_document, "text") or_return
	version := object_integer(text_document, "version") or_return
	uri = normalize_lsp_uri(uri, state.allocator)
	ensure_workspace_for_document(state, uri)
	state.documents[uri] = Document {
		uri     = uri,
		text    = strings.clone(text, state.allocator),
		version = version,
	}
	return true
}

update_document_from_change :: proc(state: ^Server_State, params: json.Value) -> bool {
	object := json_object(params) or_return
	text_document := object_object(object, "textDocument") or_return
	changes := object_array(object, "contentChanges") or_return
	uri := object_string(text_document, "uri") or_return
	version := object_integer(text_document, "version") or_return
	last_change := json_object(changes[len(changes) - 1]) or_return
	text := object_string(last_change, "text") or_return
	uri = normalize_lsp_uri(uri, state.allocator)
	ensure_workspace_for_document(state, uri)
	state.documents[uri] = Document {
		uri     = uri,
		text    = strings.clone(text, state.allocator),
		version = version,
	}
	return true
}

close_document :: proc(state: ^Server_State, params: json.Value) -> bool {
	object := json_object(params) or_return
	text_document := object_object(object, "textDocument") or_return
	uri := object_string(text_document, "uri") or_return
	uri = normalize_lsp_uri(uri, context.temp_allocator)
	delete_key(&state.documents, uri)
	return true
}

server_reanalyze :: proc(state: ^Server_State) {
	if state.has_analysis {
		workspace.analysis_result_destroy(&state.analysis, state.allocator)
		state.has_analysis = false
	}
	clear(&state.parse_diagnostics)
	if len(state.documents) == 0 || !state.has_workspace {
		return
	}
	inputs := make(
		[dynamic]semantic.Workspace_File_Input,
		0,
		len(state.documents),
		state.allocator,
	)
	for _, doc in state.documents {
		parsed := parser.parse(doc.text, doc.uri, state.allocator)
		append_parse_diagnostics(state, doc.uri, parsed.errors)
		append(
			&inputs,
			semantic.Workspace_File_Input {
				path = strings.clone(doc.uri, state.allocator),
				root = parsed.root,
				kind = .Unknown,
			},
		)
	}
	state.analysis = workspace.analyze_inputs(
		&state.opened_workspace,
		inputs[:],
		&state.pool,
		state.allocator,
	)
	state.has_analysis = state.analysis.ok
}

append_parse_diagnostics :: proc(state: ^Server_State, uri: string, errors: []parser.Parse_Error) {
	bucket := Parse_Diagnostic_Bucket {
		uri    = strings.clone(uri, state.allocator),
		errors = make([dynamic]parser.Parse_Error, 0, len(errors), state.allocator),
	}
	for err in errors {
		append(&bucket.errors, err)
	}
	append(&state.parse_diagnostics, bucket)
}

publish_all_diagnostics :: proc(state: ^Server_State, output: ^os.File) {
	for uri, _ in state.documents {
		diagnostics := diagnostics_for_uri(state, uri, state.allocator)
		params := Publish_Diagnostics_Params {
			uri         = uri,
			diagnostics = diagnostics,
		}
		send_notification(output, METHOD_PUBLISH_DIAGNOSTICS, params, state.allocator)
	}
}

diagnostics_for_uri :: proc(
	state: ^Server_State,
	uri: string,
	allocator: mem.Allocator,
) -> []Diagnostic {
	doc, doc_ok := state.documents[uri]
	if !doc_ok {
		return nil
	}
	out := make([dynamic]Diagnostic, 0, 8, allocator)
	for bucket in state.parse_diagnostics {
		if bucket.uri != uri {
			continue
		}
		for err in bucket.errors {
			append(
				&out,
				Diagnostic {
					range = range_from_offsets(doc.text, err.range.start, err.range.end),
					severity = DIAGNOSTIC_ERROR,
					code = "syntax",
					source = "abap-lsp",
					message = err.message,
				},
			)
		}
	}
	if state.has_analysis {
		if analysis := semantic.semantic_graph_session_current_analysis(&state.analysis.session);
		   analysis != nil {
			for &result in analysis.project_results {
				if result.project == nil || result.checker == nil {
					continue
				}
				file := project_file_for_uri(&result, uri)
				if file == nil {
					continue
				}
				query := semantic.semantic_query(result.project, result.checker, file)
				semantic_diags := semantic.semantic_diagnostic_copies(
					semantic.semantic_query_diagnostics(query),
					context.temp_allocator,
				)
				for diagnostic in semantic_diags {
					item := diagnostic_to_lsp(doc.text, diagnostic)
					if !diagnostic_present(out[:], item) {
						append(&out, item)
					}
				}
			}
		}
	}
	return out[:]
}

diagnostic_to_lsp :: proc(source: string, diagnostic: semantic.Checker_Diagnostic) -> Diagnostic {
	return Diagnostic {
		range = range_from_offsets(source, diagnostic.range.start, diagnostic.range.end),
		severity = diagnostic_severity(diagnostic.severity),
		code = fmt.tprintf("%v", diagnostic.kind),
		source = "abap-lsp",
		message = diagnostic.message,
	}
}

diagnostic_present :: proc(items: []Diagnostic, item: Diagnostic) -> bool {
	for existing in items {
		if existing.range == item.range &&
		   existing.severity == item.severity &&
		   existing.code == item.code &&
		   existing.message == item.message {
			return true
		}
	}
	return false
}

diagnostic_severity :: proc(severity: semantic.Checker_Diagnostic_Severity) -> int {
	switch severity {
	case .Error:
		return DIAGNOSTIC_ERROR
	case .Warning:
		return DIAGNOSTIC_WARNING
	case .Note:
		return DIAGNOSTIC_INFORMATION
	}
	return DIAGNOSTIC_ERROR
}

project_file_for_uri :: proc(
	result: ^semantic.Workspace_Project_Result,
	uri: string,
) -> ^semantic.Project_File {
	for file in result.files {
		if file != nil && file.path == uri {
			return file
		}
	}
	return nil
}

snapshot_for_position :: proc(
	state: ^Server_State,
	params: json.Value,
) -> (
	Snapshot_Lookup,
	int,
	bool,
) {
	pos := text_document_position_from_params(params)
	if !pos.ok {
		return {}, 0, false
	}
	snapshot := snapshot_for_uri(state, pos.uri)
	if !snapshot.ok {
		return {}, 0, false
	}
	offset := position_to_offset(snapshot.source, pos.position)
	return snapshot, offset, true
}

snapshot_for_uri :: proc(state: ^Server_State, uri: string) -> Snapshot_Lookup {
	doc, doc_ok := state.documents[uri]
	if !doc_ok || !state.has_analysis {
		return {}
	}
	analysis := semantic.semantic_graph_session_current_analysis(&state.analysis.session)
	if analysis == nil {
		return {}
	}
	for &result in analysis.project_results {
		if result.project == nil || result.checker == nil {
			continue
		}
		file := project_file_for_uri(&result, uri)
		if file == nil {
			continue
		}
		return Snapshot_Lookup {
			project_result = &result,
			project = result.project,
			checker = result.checker,
			file = file,
			source = doc.text,
			ok = true,
		}
	}
	return {}
}

entity_at_position :: proc(state: ^Server_State, params: json.Value) -> Entity_Lookup {
	snapshot, offset, ok := snapshot_for_position(state, params)
	if !ok {
		return {}
	}
	query := semantic.semantic_query(snapshot.project, snapshot.checker, snapshot.file)
	decl_query := semantic.semantic_query_decls(query)
	if entity := semantic.semantic_decl_entity_at_offset(decl_query, offset); entity != nil {
		return Entity_Lookup {
			snapshot = snapshot,
			entity = entity,
			range = entity.name_range,
			ok = true,
		}
	}
	if entity := semantic.semantic_decl_class_member_at_offset(decl_query, offset); entity != nil {
		return Entity_Lookup {
			snapshot = snapshot,
			entity = entity,
			range = entity.name_range,
			ok = true,
		}
	}
	if entity := semantic.semantic_decl_structure_field_at_offset(decl_query, offset);
	   entity != nil {
		return Entity_Lookup {
			snapshot = snapshot,
			entity = entity,
			range = entity.name_range,
			ok = true,
		}
	}
	ref_query := semantic.semantic_query_refs(query)
	if use := semantic.semantic_ref_use_at_offset(ref_query, offset);
	   use != nil && use.entity != nil {
		range := use.node.range if use.node != nil else use.entity.name_range
		return Entity_Lookup{snapshot = snapshot, entity = use.entity, range = range, ok = true}
	}
	return {}
}

handle_hover :: proc(ctx: ^Request_Context, params: json.Value) {
	found := entity_at_position(ctx.state, params)
	if !found.ok {
		send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
		return
	}
	label := entity_label(found.snapshot.project, found.entity)
	detail := entity_detail(found.snapshot.project, found.entity)
	text := label
	if detail != "" {
		text = fmt.tprintf("%s\n\n%s", label, detail)
	}
	hover := Hover {
		contents = Hover_Markup{kind = "markdown", value = text},
		range = range_from_offsets(found.snapshot.source, found.range.start, found.range.end),
	}
	send_success(ctx.output, ctx.id, hover, ctx.state.allocator)
}

handle_definition :: proc(ctx: ^Request_Context, params: json.Value) {
	found := entity_at_position(ctx.state, params)
	if !found.ok || found.entity.source_file == nil {
		send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
		return
	}
	source := source_for_project_file(ctx.state, found.entity.source_file)
	if source == "" {
		source = found.snapshot.source
	}
	location := Location {
		uri   = found.entity.source_file.path,
		range = range_from_offsets(
			source,
			found.entity.name_range.start,
			found.entity.name_range.end,
		),
	}
	send_success(ctx.output, ctx.id, location, ctx.state.allocator)
}

handle_references :: proc(ctx: ^Request_Context, params: json.Value) {
	found := entity_at_position(ctx.state, params)
	if !found.ok {
		send_success(ctx.output, ctx.id, []Location{}, ctx.state.allocator)
		return
	}
	query := semantic.semantic_query(found.snapshot.project, found.snapshot.checker)
	refs := semantic.semantic_ref_resolving_to_entity(
		semantic.semantic_query_refs(query),
		found.entity,
		context.temp_allocator,
	)
	locations := make([dynamic]Location, 0, len(refs) + 1, ctx.state.allocator)
	if found.entity.source_file != nil {
		source := source_for_project_file(ctx.state, found.entity.source_file)
		append(
			&locations,
			Location {
				uri = found.entity.source_file.path,
				range = range_from_offsets(
					source,
					found.entity.name_range.start,
					found.entity.name_range.end,
				),
			},
		)
	}
	for ref in refs {
		if ref == nil || ref.node == nil || ref.file == nil {
			continue
		}
		source := source_for_project_file(ctx.state, ref.file)
		location := Location {
			uri   = ref.file.path,
			range = range_from_offsets(source, ref.node.range.start, ref.node.range.end),
		}
		if !location_present(locations[:], location) {
			append(&locations, location)
		}
	}
	send_success(ctx.output, ctx.id, locations[:], ctx.state.allocator)
}

handle_completion :: proc(ctx: ^Request_Context, params: json.Value) {
	snapshot, offset, ok := snapshot_for_position(ctx.state, params)
	if !ok {
		send_success(ctx.output, ctx.id, Completion_List{}, ctx.state.allocator)
		return
	}
	prefix := completion_prefix(snapshot.source, offset, context.temp_allocator)
	query := semantic.semantic_query(snapshot.project, snapshot.checker, snapshot.file)
	items := semantic.semantic_completion_items_at_offset(
		semantic.semantic_query_completion(query),
		offset,
		prefix,
		context.temp_allocator,
	)
	out := make([]Completion_Item, len(items), ctx.state.allocator)
	for item, i in items {
		name := string_interner.load(snapshot.project.interner, item.name)
		out[i] = Completion_Item {
			label = name,
			kind  = completion_kind(item.entity),
		}
	}
	send_success(
		ctx.output,
		ctx.id,
		Completion_List{is_incomplete = false, items = out},
		ctx.state.allocator,
	)
}

completion_prefix :: proc(source: string, offset: int, allocator: mem.Allocator) -> string {
	end := clamp(offset, 0, len(source))
	start := end
	for start > 0 {
		ch := source[start - 1]
		if !(('a' <= ch && ch <= 'z') ||
			   ('A' <= ch && ch <= 'Z') ||
			   ('0' <= ch && ch <= '9') ||
			   ch == '_' ||
			   ch == '/') {
			break
		}
		start -= 1
	}
	if start == end {
		return ""
	}
	return strings.clone(source[start:end], allocator)
}

handle_semantic_tokens :: proc(ctx: ^Request_Context, params: json.Value) {
	uri := uri_from_text_document_params(params)
	if uri == "" {
		send_success(ctx.output, ctx.id, Semantic_Tokens{}, ctx.state.allocator)
		return
	}
	snapshot := snapshot_for_uri(ctx.state, uri)
	if !snapshot.ok {
		send_success(ctx.output, ctx.id, Semantic_Tokens{}, ctx.state.allocator)
		return
	}
	tokens := semantic_tokens_for_snapshot(snapshot, ctx.state.allocator)
	send_success(ctx.output, ctx.id, Semantic_Tokens{data = tokens}, ctx.state.allocator)
}

handle_folding_ranges :: proc(ctx: ^Request_Context, params: json.Value) {
	uri := uri_from_text_document_params(params)
	if uri == "" {
		send_success(ctx.output, ctx.id, []Folding_Range{}, ctx.state.allocator)
		return
	}
	if doc, ok := ctx.state.documents[uri]; ok {
		ranges := folding_ranges_for_source(doc.text, ctx.state.allocator)
		send_success(ctx.output, ctx.id, ranges, ctx.state.allocator)
		return
	}
	send_success(ctx.output, ctx.id, []Folding_Range{}, ctx.state.allocator)
}

uri_from_text_document_params :: proc(params: json.Value) -> string {
	object, ok := json_object(params)
	if !ok {
		return ""
	}
	text_document, doc_ok := object_object(object, "textDocument")
	if !doc_ok {
		return ""
	}
	uri, uri_ok := object_string(text_document, "uri")
	if !uri_ok {
		return ""
	}
	return normalize_lsp_uri(uri, context.allocator)
}

semantic_tokens_for_snapshot :: proc(
	snapshot: Snapshot_Lookup,
	allocator: mem.Allocator,
) -> []u32 {
	pending := make([dynamic]Pending_Token, 0, 128, context.temp_allocator)
	for entity in snapshot.checker.info.definitions {
		if entity == nil || entity.source_file != snapshot.file {
			continue
		}
		push_pending_token(&pending, entity.name_range, entity, true)
	}
	for use in snapshot.checker.info.uses {
		if use.file != snapshot.file || use.entity == nil || use.node == nil {
			continue
		}
		push_pending_token(&pending, use.node.range, use.entity, false)
	}
	slice.sort_by(pending[:], pending_token_less)
	merged := make([dynamic]Pending_Token, 0, len(pending), context.temp_allocator)
	last_end := 0
	for token in pending {
		if token.start < last_end {
			continue
		}
		append(&merged, token)
		last_end = token.end
	}
	return encode_semantic_token_deltas(snapshot.source, merged[:], allocator)
}

push_pending_token :: proc(
	pending: ^[dynamic]Pending_Token,
	range: semantic.Range,
	entity: ^semantic.Entity,
	declaration: bool,
) {
	if range.start >= range.end {
		return
	}
	token_type := semantic_token_type(entity)

	TOKEN_MOD_DECLARATION :: u32(1 << 0)
	TOKEN_MOD_READONLY :: u32(1 << 1)
	modifiers: u32
	if declaration {
		modifiers |= TOKEN_MOD_DECLARATION
	}
	if entity != nil &&
	   (entity.kind == .Constant || entity.kind == .Enum_Member || .Read_Only in entity.flags) {
		modifiers |= TOKEN_MOD_READONLY
	}
	append(
		pending,
		Pending_Token {
			start = range.start,
			end = range.end,
			token_type = token_type,
			modifiers = modifiers,
			priority = 0 if declaration else 1,
		},
	)
}

pending_token_less :: proc(a, b: Pending_Token) -> bool {
	if a.start != b.start {
		return a.start < b.start
	}
	a_width := a.end - a.start
	b_width := b.end - b.start
	if a_width != b_width {
		return a_width < b_width
	}
	return a.priority < b.priority
}

encode_semantic_token_deltas :: proc(
	source: string,
	tokens: []Pending_Token,
	allocator: mem.Allocator,
) -> []u32 {
	out := make([dynamic]u32, 0, len(tokens) * 5, allocator)
	prev_line := 0
	prev_character := 0
	for token in tokens {
		start := offset_to_position(source, token.start)
		end := offset_to_position(source, token.end)
		if end.line != start.line || end.character <= start.character {
			continue
		}
		delta_line := start.line - prev_line
		delta_start := start.character - prev_character if delta_line == 0 else start.character
		append(&out, u32(delta_line))
		append(&out, u32(delta_start))
		append(&out, u32(end.character - start.character))
		append(&out, token.token_type)
		append(&out, token.modifiers)
		prev_line = start.line
		prev_character = start.character
	}
	return out[:]
}

folding_ranges_for_source :: proc(source: string, allocator: mem.Allocator) -> []Folding_Range {
	ranges := make([dynamic]Folding_Range, 0, 16, allocator)
	stack := make([dynamic]int, 0, 16, context.temp_allocator)
	line_start := 0
	line := 0
	for i := 0; i <= len(source); i += 1 {
		if i < len(source) && source[i] != '\n' {
			continue
		}
		line_text := source[line_start:i]
		if len(line_text) > 0 && line_text[len(line_text) - 1] == '\r' {
			line_text = line_text[:len(line_text) - 1]
		}
		keyword := leading_keyword(line_text, context.temp_allocator)
		if folding_start_keyword(keyword) {
			append(&stack, line)
		} else if folding_end_keyword(keyword) && len(stack) > 0 {
			start_line := pop(&stack)
			if line > start_line {
				append(
					&ranges,
					Folding_Range {
						start_line = start_line,
						start_character = 0,
						end_line = line,
						end_character = len(line_text),
					},
				)
			}
		}
		line += 1
		line_start = i + 1
	}
	return ranges[:]
}

leading_keyword :: proc(line: string, allocator: mem.Allocator) -> string {
	trimmed := strings.trim_space(line)
	if trimmed == "" || strings.has_prefix(trimmed, "*") || strings.has_prefix(trimmed, "\"") {
		return ""
	}
	end := 0
	for end < len(trimmed) {
		ch := trimmed[end]
		if !(('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '-') {
			break
		}
		end += 1
	}
	return strings.to_upper(trimmed[:end], allocator) if end > 0 else ""
}

folding_start_keyword :: proc(keyword: string) -> bool {
	switch keyword {
	case "CLASS",
	     "INTERFACE",
	     "METHOD",
	     "FORM",
	     "FUNCTION",
	     "IF",
	     "CASE",
	     "DO",
	     "WHILE",
	     "LOOP",
	     "TRY",
	     "SELECT":
		return true
	}
	return false
}

folding_end_keyword :: proc(keyword: string) -> bool {
	switch keyword {
	case "ENDCLASS",
	     "ENDINTERFACE",
	     "ENDMETHOD",
	     "ENDFORM",
	     "ENDFUNCTION",
	     "ENDIF",
	     "ENDCASE",
	     "ENDDO",
	     "ENDWHILE",
	     "ENDLOOP",
	     "ENDTRY",
	     "ENDSELECT":
		return true
	}
	return false
}

semantic_token_type :: proc(entity: ^semantic.Entity) -> u32 {
	if entity == nil {
		return TOKEN_TYPE_INDICES.variable
	}
	#partial switch entity.kind {
	case .Type_Def:
		return TOKEN_TYPE_INDICES.type_
	case .Class:
		return TOKEN_TYPE_INDICES.class
	case .Interface:
		return TOKEN_TYPE_INDICES.type_
	case .Parameter:
		return TOKEN_TYPE_INDICES.parameter
	case .Field:
		return TOKEN_TYPE_INDICES.property
	case .Form, .Module, .Builtin:
		return TOKEN_TYPE_INDICES.function
	case .Method:
		return TOKEN_TYPE_INDICES.method
	case .Event:
		return TOKEN_TYPE_INDICES.event
	case .Enum_Member:
		return TOKEN_TYPE_INDICES.enum_member
	case .Variable, .Constant, .Field_Symbol, .Exception, .Include, .Control, .Report:
		return TOKEN_TYPE_INDICES.variable
	case .Alias, .Invalid:
	}
	return TOKEN_TYPE_INDICES.variable
}

completion_kind :: proc(entity: ^semantic.Entity) -> int {
	if entity == nil {
		return COMPLETION_VARIABLE
	}
	#partial switch entity.kind {
	case .Class:
		return COMPLETION_CLASS
	case .Interface:
		return COMPLETION_INTERFACE
	case .Type_Def:
		return COMPLETION_STRUCT
	case .Field:
		return COMPLETION_FIELD
	case .Form:
		return COMPLETION_FUNCTION
	case .Module:
		return COMPLETION_MODULE
	case .Method:
		return COMPLETION_METHOD
	case .Event:
		return COMPLETION_EVENT
	case .Constant:
		return COMPLETION_CONSTANT
	case .Enum_Member:
		return COMPLETION_ENUM_MEMBER
	case:
	}
	return COMPLETION_VARIABLE
}

entity_label :: proc(project: ^semantic.Project, entity: ^semantic.Entity) -> string {
	if entity == nil {
		return ""
	}
	name := string_interner.load(project.interner, entity.name)
	return fmt.tprintf("`%s` %s", name, entity_kind_label(entity.kind))
}

entity_detail :: proc(project: ^semantic.Project, entity: ^semantic.Entity) -> string {
	if entity == nil || entity.type == nil {
		return ""
	}
	type_text := type_label(project, entity.type)
	if type_text == "" {
		return ""
	}
	return fmt.tprintf("type: `%s`", type_text)
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

type_label :: proc(project: ^semantic.Project, typ: ^semantic.Type) -> string {
	if typ == nil {
		return ""
	}
	switch typ.kind {
	case .Builtin, .Named, .Class, .Interface:
		if string_interner.is_valid(typ.name) {
			return string_interner.load(project.interner, typ.name)
		}
	case .Structure:
		if typ.structure != nil && string_interner.is_valid(typ.structure.name) {
			return string_interner.load(project.interner, typ.structure.name)
		}
		return "structure"
	case .Table:
		base := type_label(project, typ.base)
		return fmt.tprintf("table of %s", base) if base != "" else "table"
	case .Ref:
		base := type_label(project, typ.base)
		return fmt.tprintf("ref to %s", base) if base != "" else "ref"
	case .Routine:
		return "routine"
	case .Unknown:
	}
	return ""
}

source_for_project_file :: proc(state: ^Server_State, file: ^semantic.Project_File) -> string {
	if file == nil {
		return ""
	}
	if doc, ok := state.documents[file.path]; ok {
		return doc.text
	}
	return ""
}

location_present :: proc(locations: []Location, location: Location) -> bool {
	for existing in locations {
		if existing.uri == location.uri && existing.range == location.range {
			return true
		}
	}
	return false
}

send_success :: proc(output: ^os.File, id: json.Value, result: any, allocator: mem.Allocator) {
	if payload, ok := rpc_success_payload(id, result, allocator); ok {
		_ = write_frame(output, payload)
	}
}

send_error :: proc(
	output: ^os.File,
	id: json.Value,
	code: int,
	message: string,
	allocator: mem.Allocator,
) {
	if payload, ok := rpc_error_payload(id, code, message, allocator); ok {
		_ = write_frame(output, payload)
	}
}

send_notification :: proc(
	output: ^os.File,
	method: string,
	params: any,
	allocator: mem.Allocator,
) {
	if payload, ok := notification_payload(method, params, allocator); ok {
		_ = write_frame(output, payload)
	}
}

file_uri_to_path :: proc(uri: string, allocator: mem.Allocator) -> (string, bool) {
	if !strings.has_prefix(strings.to_lower(uri, context.temp_allocator), "file://") {
		return "", false
	}
	raw := uri[len("file://"):]
	if strings.has_prefix(raw, "/") && len(raw) >= 4 && raw[2] == ':' {
		raw = raw[1:]
	}
	decoded, ok := net.percent_decode(raw, allocator)
	if !ok {
		return "", false
	}
	path, path_err := os.replace_path_separators(decoded, os.Path_Separator, allocator)
	if path_err != nil {
		return "", false
	}
	return path, true
}
