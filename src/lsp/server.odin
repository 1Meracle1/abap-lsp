package abap_frontend_lsp

import execution "src:execution"
import "src:parser"
import "src:semantic"
import string_interner "src:string_interner"
import uri_key "src:uri_key"
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
	dirty:   bool,
}

Parse_Diagnostic_Bucket :: struct {
	uri:    string,
	errors: [dynamic]parser.Parse_Error,
}

Server_Workspace :: struct {
	root:         workspace.Workspace,
	analysis:     workspace.Analysis_Result,
	has_analysis: bool,
}

Server_State :: struct {
	allocator:            mem.Allocator,
	options:              workspace.Options,
	pool:                 execution.Pool,
	documents:            map[string]Document,
	parse_diagnostics:    [dynamic]Parse_Diagnostic_Bucket,
	workspaces:           [dynamic]Server_Workspace,
	pending_removed_uris: [dynamic]string,
	initialized:          bool,
	shutdown_requested:   bool,
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
		workspaces        = make([dynamic]Server_Workspace, 0, 4, allocator),
		pending_removed_uris = make([dynamic]string, 0, 8, allocator),
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
	for &slot in state.workspaces {
		if slot.has_analysis {
			workspace.analysis_result_destroy(&slot.analysis, state.allocator)
		}
		workspace.workspace_destroy(&slot.root, state.allocator)
	}
	delete(state.workspaces)
	if state.pending_removed_uris.allocator.procedure != nil {
		delete(state.pending_removed_uris)
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
	case METHOD_DID_CHANGE_WATCHED_FILES:
		changed := false
		object, object_ok := params.(json.Object)
		if !object_ok {
			return
		}
		changes, changes_ok := object_array(object, "changes")
		if !changes_ok {
			return
		}
		if state.pending_removed_uris.allocator.procedure == nil {
			state.pending_removed_uris = make([dynamic]string, 0, len(changes), state.allocator)
		}
		for value in changes {
			event, event_ok := value.(json.Object)
			if !event_ok {
				continue
			}
			uri, uri_ok := object_string(event, "uri")
			if !uri_ok {
				continue
			}
			change_type, change_type_ok := object_integer(event, "type")
			if !change_type_ok {
				continue
			}
			uri = normalize_lsp_uri(uri, context.temp_allocator)
			switch change_type {
			case FILE_CHANGE_DELETED:
				append(&state.pending_removed_uris, uri)
				to_delete := make([dynamic]string, 0, 4, context.temp_allocator)
				for doc_uri, _ in state.documents {
					if lsp_uri_matches_or_under(doc_uri, uri) {
						append(&to_delete, doc_uri)
					}
				}
				for doc_uri in to_delete {
					delete_key(&state.documents, doc_uri)
				}
				changed = true
			case FILE_CHANGE_CHANGED, FILE_CHANGE_CREATED:
				for doc_uri, &doc in state.documents {
					if !lsp_uri_matches_or_under(doc_uri, uri) {
						continue
					}
					doc.dirty = true
					changed = true
				}
			case:
			}
		}
		if changed {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_CREATE_FILES:
		changed := false
		object, object_ok := params.(json.Object)
		if !object_ok {
			return
		}
		files, files_ok := object_array(object, "files")
		if !files_ok {
			return
		}
		for value in files {
			file, file_ok := value.(json.Object)
			if !file_ok {
				continue
			}
			uri, uri_ok := object_string(file, "uri")
			if !uri_ok {
				continue
			}
			uri = normalize_lsp_uri(uri, context.temp_allocator)
			for doc_uri, &doc in state.documents {
				if !lsp_uri_matches_or_under(doc_uri, uri) {
					continue
				}
				doc.dirty = true
				changed = true
			}
		}
		if changed {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_DELETE_FILES:
		changed := false
		object, object_ok := params.(json.Object)
		if !object_ok {
			return
		}
		files, files_ok := object_array(object, "files")
		if !files_ok {
			return
		}
		if state.pending_removed_uris.allocator.procedure == nil {
			state.pending_removed_uris = make([dynamic]string, 0, len(files), state.allocator)
		}
		for value in files {
			file, file_ok := value.(json.Object)
			if !file_ok {
				continue
			}
			uri, uri_ok := object_string(file, "uri")
			if !uri_ok {
				continue
			}
			uri = normalize_lsp_uri(uri, context.temp_allocator)
			append(&state.pending_removed_uris, uri)
			to_delete := make([dynamic]string, 0, 4, context.temp_allocator)
			for doc_uri, _ in state.documents {
				if lsp_uri_matches_or_under(doc_uri, uri) {
					append(&to_delete, doc_uri)
				}
			}
			for doc_uri in to_delete {
				delete_key(&state.documents, doc_uri)
			}
			changed = true
		}
		if changed {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_RENAME_FILES:
		changed := false
		object, object_ok := params.(json.Object)
		if !object_ok {
			return
		}
		files, files_ok := object_array(object, "files")
		if !files_ok {
			return
		}
		if state.pending_removed_uris.allocator.procedure == nil {
			state.pending_removed_uris = make([dynamic]string, 0, len(files), state.allocator)
		}
		for value in files {
			file, file_ok := value.(json.Object)
			if !file_ok {
				continue
			}
			old_uri, old_uri_ok := object_string(file, "oldUri")
			new_uri, new_uri_ok := object_string(file, "newUri")
			if !old_uri_ok || !new_uri_ok {
				continue
			}
			old_uri = normalize_lsp_uri(old_uri, context.temp_allocator)
			new_uri = normalize_lsp_uri(new_uri, context.temp_allocator)
			append(&state.pending_removed_uris, old_uri)
			to_delete := make([dynamic]string, 0, 4, context.temp_allocator)
			for doc_uri, _ in state.documents {
				if lsp_uri_matches_or_under(doc_uri, old_uri) {
					append(&to_delete, doc_uri)
				}
			}
			for doc_uri in to_delete {
				delete_key(&state.documents, doc_uri)
			}
			for doc_uri, &doc in state.documents {
				if !lsp_uri_matches_or_under(doc_uri, new_uri) {
					continue
				}
				doc.dirty = true
			}
			changed = true
		}
		if changed {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_CHANGE_WORKSPACE_FOLDERS:
		changed := false
		object, object_ok := params.(json.Object)
		if !object_ok {
			return
		}
		event, event_ok := object_object(object, "event")
		if !event_ok {
			return
		}
		if removed, removed_ok := object_array(event, "removed"); removed_ok {
			for value in removed {
				folder, folder_ok := value.(json.Object)
				if !folder_ok {
					continue
				}
				uri, uri_ok := object_string(folder, "uri")
				if !uri_ok {
					continue
				}
				path, path_ok := file_uri_to_path(uri, context.temp_allocator)
				if !path_ok {
					continue
				}
				root_key := lsp_path_key(path, context.temp_allocator)
				for i := 0; i < len(state.workspaces); {
					if lsp_path_key(state.workspaces[i].root.root_path, context.temp_allocator) != root_key {
						i += 1
						continue
					}
					if state.workspaces[i].has_analysis {
						workspace.analysis_result_destroy(&state.workspaces[i].analysis, state.allocator)
					}
					workspace.workspace_destroy(&state.workspaces[i].root, state.allocator)
					ordered_remove(&state.workspaces, i)
					changed = true
				}
			}
		}
		if added, added_ok := object_array(event, "added"); added_ok {
			for value in added {
				folder, folder_ok := value.(json.Object)
				if !folder_ok {
					continue
				}
				uri, uri_ok := object_string(folder, "uri")
				if !uri_ok {
					continue
				}
				changed = open_workspace_for_uri(state, uri) || changed
			}
		}
		if changed {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case:
	}
}

handle_initialize :: proc(ctx: ^Request_Context, params: json.Value) {
	state := ctx.state
	if object, ok := params.(json.Object); ok {
		if init_options, init_ok := object_object(object, "initializationOptions"); init_ok {
			if path, path_ok := object_string(init_options, "dependencyCachePath"); path_ok {
				state.options.dependency_store_path = strings.clone(path, state.allocator)
			}
		}
		opened := false
		if folders, folders_ok := object_array(object, "workspaceFolders");
		   folders_ok && len(folders) > 0 {
			for value in folders {
				folder, folder_ok := value.(json.Object)
				if !folder_ok {
					continue
				}
				uri, uri_ok := object_string(folder, "uri")
				if !uri_ok {
					continue
				}
				opened = open_workspace_for_uri(state, uri) || opened
			}
		}
		if !opened {
			uri, uri_ok := object_string(object, "rootUri")
			if uri_ok {
				open_workspace_for_uri(state, uri)
			}
		}
	}
	state.initialized = true
	send_success(ctx.output, ctx.id, initialize_result(state.allocator), state.allocator)
}

initialize_result :: proc(allocator: mem.Allocator) -> Initialize_Result {
	token_modifiers := make([]string, 2, allocator)
	token_modifiers[0] = "declaration"
	token_modifiers[1] = "readonly"

	trigger_characters := make([]string, 3, allocator)
	trigger_characters[0] = "-"
	trigger_characters[1] = ">"
	trigger_characters[2] = "~"

	file_operation_filters := make([]Workspace_File_Operation_Filter, 1, allocator)
	file_operation_filters[0] = Workspace_File_Operation_Filter {
		scheme = "file",
		pattern = Workspace_File_Operation_Pattern{glob = "**/*"},
	}
	file_operation_options := Workspace_File_Operation_Registration_Options {
		filters = file_operation_filters,
	}

	return Initialize_Result {
		capabilities = Server_Capabilities {
			text_document_sync = TEXT_DOCUMENT_SYNC_FULL,
			hover_provider = true,
			definition_provider = true,
			references_provider = true,
			completion_provider = Completion_Options{trigger_characters = trigger_characters},
			semantic_tokens_provider = Semantic_Tokens_Options {
				legend = Semantic_Tokens_Legend {
					token_types = initialize_semantic_token_types(allocator),
					token_modifiers = token_modifiers,
				},
				full = true,
			},
			folding_range_provider = true,
			workspace = Workspace_Server_Capabilities {
				workspace_folders = Workspace_Folders_Server_Capability {
					supported = true,
					change_notifications = true,
				},
				file_operations = Workspace_File_Operation_Server_Capabilities {
					did_create = file_operation_options,
					did_rename = file_operation_options,
					did_delete = file_operation_options,
				},
			},
		},
		server_info = Server_Info{name = "abap-lsp-odin", version = "0.1.0"},
	}
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

Initialize_Result :: struct {
	capabilities: Server_Capabilities `json:"capabilities"`,
	server_info:  Server_Info `json:"serverInfo"`,
}

Server_Info :: struct {
	name:    string `json:"name"`,
	version: string `json:"version"`,
}

Server_Capabilities :: struct {
	text_document_sync:       int `json:"textDocumentSync"`,
	hover_provider:           bool `json:"hoverProvider"`,
	definition_provider:      bool `json:"definitionProvider"`,
	references_provider:      bool `json:"referencesProvider"`,
	completion_provider:      Completion_Options `json:"completionProvider"`,
	semantic_tokens_provider: Semantic_Tokens_Options `json:"semanticTokensProvider"`,
	folding_range_provider:   bool `json:"foldingRangeProvider"`,
	workspace:                Workspace_Server_Capabilities `json:"workspace"`,
}

Completion_Options :: struct {
	trigger_characters: []string `json:"triggerCharacters"`,
}

Workspace_Server_Capabilities :: struct {
	workspace_folders: Workspace_Folders_Server_Capability `json:"workspaceFolders"`,
	file_operations:   Workspace_File_Operation_Server_Capabilities `json:"fileOperations"`,
}

Workspace_Folders_Server_Capability :: struct {
	supported:            bool `json:"supported"`,
	change_notifications: bool `json:"changeNotifications"`,
}

Workspace_File_Operation_Server_Capabilities :: struct {
	did_create: Workspace_File_Operation_Registration_Options `json:"didCreate"`,
	did_rename: Workspace_File_Operation_Registration_Options `json:"didRename"`,
	did_delete: Workspace_File_Operation_Registration_Options `json:"didDelete"`,
}

Workspace_File_Operation_Registration_Options :: struct {
	filters: []Workspace_File_Operation_Filter `json:"filters"`,
}

Workspace_File_Operation_Filter :: struct {
	scheme:  string `json:"scheme"`,
	pattern: Workspace_File_Operation_Pattern `json:"pattern"`,
}

Workspace_File_Operation_Pattern :: struct {
	glob: string `json:"glob"`,
}

Semantic_Tokens_Options :: struct {
	legend: Semantic_Tokens_Legend `json:"legend"`,
	full:   bool `json:"full"`,
}

Semantic_Tokens_Legend :: struct {
	token_types:     []string `json:"tokenTypes"`,
	token_modifiers: []string `json:"tokenModifiers"`,
}

open_workspace_for_uri :: proc(state: ^Server_State, uri: string) -> bool {
	path := file_uri_to_path(uri, state.allocator) or_return
	return open_workspace_for_path(state, path)
}

open_workspace_for_path :: proc(state: ^Server_State, path: string) -> bool {
	opened, ok, _ := workspace.open(path, state.options, state.allocator)
	if !ok {
		opened, ok, _ = workspace.open_standalone(path, state.options, state.allocator)
	}
	if !ok {
		return false
	}
	root_key := lsp_path_key(opened.root_path, context.temp_allocator)
	for &existing in state.workspaces {
		if lsp_path_key(existing.root.root_path, context.temp_allocator) != root_key {
			continue
		}
		workspace.workspace_destroy(&opened, state.allocator)
		return true
	}
	append(&state.workspaces, Server_Workspace{root = opened})
	return true
}

ensure_workspace_for_document :: proc(state: ^Server_State, uri: string) {
	if _, ok := workspace_index_for_uri(state, uri); ok {
		return
	}
	path, path_ok := file_uri_to_path(uri, context.temp_allocator)
	root := "."
	if path_ok {
		root = os.dir(path)
	}
	open_workspace_for_path(state, root)
}

update_document_from_open :: proc(state: ^Server_State, params: json.Value) -> bool {
	object := params.(json.Object) or_return
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
		dirty   = true,
	}
	return true
}

update_document_from_change :: proc(state: ^Server_State, params: json.Value) -> bool {
	object := params.(json.Object) or_return
	text_document := object_object(object, "textDocument") or_return
	changes := object_array(object, "contentChanges") or_return
	uri := object_string(text_document, "uri") or_return
	version := object_integer(text_document, "version") or_return
	last_change := changes[len(changes) - 1].(json.Object) or_return
	text := object_string(last_change, "text") or_return
	uri = normalize_lsp_uri(uri, state.allocator)
	ensure_workspace_for_document(state, uri)
	state.documents[uri] = Document {
		uri     = uri,
		text    = strings.clone(text, state.allocator),
		version = version,
		dirty   = true,
	}
	return true
}

close_document :: proc(state: ^Server_State, params: json.Value) -> bool {
	object := params.(json.Object) or_return
	text_document := object_object(object, "textDocument") or_return
	uri := object_string(text_document, "uri") or_return
	uri = normalize_lsp_uri(uri, context.temp_allocator)
	delete_key(&state.documents, uri)
	return true
}

server_reanalyze :: proc(state: ^Server_State) {
	clear(&state.parse_diagnostics)
	for _, doc in state.documents {
		ensure_workspace_for_document(state, doc.uri)
	}
	for &slot, workspace_index in state.workspaces {
		inputs := make(
			[dynamic]semantic.Workspace_File_Input,
			0,
			len(state.documents),
			state.allocator,
		)
		removed_paths := make([dynamic]string, 0, 4, context.temp_allocator)
		for _, workspace_doc in state.documents {
			doc_workspace_index, doc_workspace_ok := workspace_index_for_uri(state, workspace_doc.uri)
			if !doc_workspace_ok || doc_workspace_index != workspace_index {
				continue
			}
			parsed := parser.parse(workspace_doc.text, workspace_doc.uri, state.allocator)
			append_parse_diagnostics(state, workspace_doc.uri, parsed.errors)
			if slot.has_analysis && !workspace_doc.dirty {
				continue
			}
			append(
				&inputs,
				semantic.Workspace_File_Input {
					path = strings.clone(workspace_doc.uri, state.allocator),
					root = parsed.root,
					kind = .Unknown,
				},
			)
		}
		if slot.has_analysis {
			for file in slot.analysis.session.editable_files {
				_, still_open := state.documents[file.path]
				doc_workspace_index, doc_workspace_ok := workspace_index_for_uri(state, file.path)
				removed := !(still_open && doc_workspace_ok && doc_workspace_index == workspace_index)
				if !removed {
					for removed_uri in state.pending_removed_uris {
						if lsp_uri_matches_or_under(file.path, removed_uri) {
							removed = true
							break
						}
					}
				}
				if removed {
					append(&removed_paths, file.path)
				}
			}
		}
		if len(inputs) == 0 && len(removed_paths) == 0 {
			continue
		}
		slot.has_analysis = workspace.analysis_result_update_inputs(
			&slot.analysis,
			&slot.root,
			inputs[:],
			removed_paths[:],
			&state.pool,
			state.allocator,
		)
	}
	for _, &doc in state.documents {
		doc.dirty = false
	}
	if state.pending_removed_uris.allocator.procedure != nil {
		clear(&state.pending_removed_uris)
	}
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
	for &slot in state.workspaces {
		if !slot.has_analysis {
			continue
		}
		if analysis := semantic.semantic_graph_session_current_analysis(&slot.analysis.session);
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

diagnostic_severity :: proc "contextless" (severity: semantic.Checker_Diagnostic_Severity) -> int {
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

project_file_for_uri :: proc "contextless" (
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
	if !doc_ok {
		return {}
	}
	candidate_indices := make([dynamic]int, 0, len(state.workspaces), context.temp_allocator)
	if workspace_index, workspace_ok := workspace_index_for_uri(state, uri); workspace_ok {
		append(&candidate_indices, workspace_index)
	}
	for i in 0 ..< len(state.workspaces) {
		if len(candidate_indices) > 0 && candidate_indices[0] == i {
			continue
		}
		append(&candidate_indices, i)
	}
	for workspace_index in candidate_indices {
		slot := &state.workspaces[workspace_index]
		if !slot.has_analysis {
			continue
		}
		analysis := semantic.semantic_graph_session_current_analysis(&slot.analysis.session)
		if analysis == nil {
			continue
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
	}
	return {}
}

workspace_index_for_uri :: proc(state: ^Server_State, uri: string) -> (int, bool) {
	path, ok := file_uri_to_path(uri, context.temp_allocator)
	if !ok {
		if len(state.workspaces) > 0 {
			return 0, true
		}
		return -1, false
	}
	return workspace_index_for_path(state, path)
}

workspace_index_for_path :: proc(state: ^Server_State, path: string) -> (int, bool) {
	path_key := lsp_path_key(path, context.temp_allocator)
	best_index := -1
	best_len := -1
	for &slot, i in state.workspaces { 
		root_key := lsp_path_key(slot.root.root_path, context.temp_allocator)
		under_root := path_key == root_key ||
		              (len(path_key) > len(root_key) &&
		               strings.has_prefix(path_key, root_key) &&
		               path_key[len(root_key)] == '/')
		if under_root && len(root_key) > best_len {
			best_index = i
			best_len = len(root_key)
		}
	}
	return best_index, best_index >= 0
}

lsp_path_key :: proc(path: string, allocator: mem.Allocator) -> string {
	cleaned, clean_err := os.clean_path(path, allocator)
	if clean_err == nil {
		return uri_key.normalized_uri_path_key(cleaned, allocator)
	}
	return uri_key.normalized_uri_path_key(path, allocator)
}

lsp_uri_matches_or_under :: proc(candidate_uri, root_uri: string) -> bool {
	if candidate_uri == root_uri {
		return true
	}
	candidate_path, candidate_path_ok := file_uri_to_path(candidate_uri, context.temp_allocator)
	root_path, root_path_ok := file_uri_to_path(root_uri, context.temp_allocator)
	if candidate_path_ok || root_path_ok {
		candidate_location := candidate_uri
		root_location := root_uri
		if candidate_path_ok {
			candidate_location = candidate_path
		}
		if root_path_ok {
			root_location = root_path
		}
		candidate_key := lsp_path_key(candidate_location, context.temp_allocator)
		root_key := lsp_path_key(root_location, context.temp_allocator)
		return candidate_key == root_key ||
		       (len(candidate_key) > len(root_key) &&
		        strings.has_prefix(candidate_key, root_key) &&
		        candidate_key[len(root_key)] == '/')
	}
	return len(candidate_uri) > len(root_uri) &&
	       strings.has_prefix(candidate_uri, root_uri) &&
	       candidate_uri[len(root_uri)] == '/'
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
		range := semantic.semantic_entity_use_range(use^)
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
		if ref == nil || ref.file == nil {
			continue
		}
		source := source_for_project_file(ctx.state, ref.file)
		range := semantic.semantic_entity_use_range(ref^)
		if range.start >= range.end {
			continue
		}
		location := Location {
			uri   = ref.file.path,
			range = range_from_offsets(source, range.start, range.end),
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
	object, ok := params.(json.Object)
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
		if use.file != snapshot.file || use.entity == nil {
			continue
		}
		push_pending_token(&pending, semantic.semantic_entity_use_range(use), use.entity, false)
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

folding_start_keyword :: proc "contextless" (keyword: string) -> bool {
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

folding_end_keyword :: proc "contextless" (keyword: string) -> bool {
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

semantic_token_type :: proc "contextless" (entity: ^semantic.Entity) -> u32 {
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

completion_kind :: proc "contextless" (entity: ^semantic.Entity) -> int {
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
