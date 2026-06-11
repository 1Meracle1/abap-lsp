package abap_frontend_lsp

import execution "src:execution"
import "src:parser"
import workspace "src:workspace"

import json "core:encoding/json"
import "core:fmt"
import "core:mem"
import net "core:net"
import "core:os"
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
	pending_disk_refresh_uris: [dynamic]string,
	materialize_dependency_documents: bool,
	initialized:          bool,
	shutdown_requested:   bool,
}

Request_Context :: struct {
	state:  ^Server_State,
	output: ^os.File,
	id:     json.Value,
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
		pending_disk_refresh_uris = make([dynamic]string, 0, 8, allocator),
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
	if state.pending_disk_refresh_uris.allocator.procedure != nil {
		delete(state.pending_disk_refresh_uris)
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
	case METHOD_IMPLEMENTATION:
		handle_implementation(ctx, params)
	case METHOD_REFERENCES:
		handle_references(ctx, params)
	case METHOD_PREPARE_RENAME:
		handle_prepare_rename(ctx, params)
	case METHOD_RENAME:
		handle_rename(ctx, params)
	case METHOD_SEMANTIC_TOKENS_FULL:
		handle_semantic_tokens(ctx, params)
	case METHOD_FOLDING_RANGE:
		handle_folding_ranges(ctx, params)
	case METHOD_READ_DEPENDENCY_DOCUMENT:
		handle_read_dependency_document(ctx, params)
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
