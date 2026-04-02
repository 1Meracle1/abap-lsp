package lsp

import "../cache"
import "../jsonrpc"
import "../thread_pool"
import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:strings"

Server_Loop_Action :: enum {
	Continue,
	Shutdown,
}

Server :: struct {
	stream:                         jsonrpc.Stream,
	storage:                        ^cache.Cache,
	worker_pool:                    ^thread_pool.Thread_Pool,
	// Client capabilities (after initialize)
	client_work_done_progress:      bool,
	// Outbound JSON-RPC to client (workDoneProgress/create)
	next_jsonrpc_out_id:            i64,
	pending_outgoing_rpc_id:        i64,
	pending_outgoing_rpc_done:      bool,
	pending_outgoing_create_failed: bool,
	// Filled by server_start so nested read/dispatch (progress wait) can handle other requests
	dispatch_initialized:           ^bool,
	dispatch_request_handlers:      ^map[string]Request_Handler,
	dispatch_notif_handlers:        ^map[string]Notification_Handler,
}

Request_Handler :: #type proc(srv: ^Server, id: json.Value, params: json.Value)
Notification_Handler :: #type proc(srv: ^Server, params: json.Value)

server_start :: proc(stream: jsonrpc.Stream) {
	srv: Server
	srv.stream = stream
	srv.storage = cache.cache_init()
	defer cache.cache_deinit(srv.storage)
	srv.worker_pool = thread_pool.init(thread_pool.recommended_worker_count())
	defer thread_pool.deinit(srv.worker_pool)

	request_handlers := make(map[string]Request_Handler)
	request_handlers["initialize"] = handle_initialize
	request_handlers["textDocument/hover"] = handle_hover
	request_handlers["textDocument/diagnostic"] = handle_diagnostic
	request_handlers["textDocument/semanticTokens/full"] = handle_semantic_tokens
	request_handlers["textDocument/completion"] = handle_completion

	notif_handlers := make(map[string]Notification_Handler)
	notif_handlers["textDocument/didOpen"] = handle_document_open
	notif_handlers["textDocument/didChange"] = handle_document_change
	notif_handlers[Remote_Dependencies_Updated_Notification] = handle_remote_dependencies_updated
	notif_handlers[Workspace_Manifest_Updated_Notification] = handle_workspace_manifest_updated
	notif_handlers[Dependency_Cache_Cleared_Notification] = handle_dependency_cache_cleared

	initialized: bool

	srv.dispatch_initialized = &initialized
	srv.dispatch_request_handlers = &request_handlers
	srv.dispatch_notif_handlers = &notif_handlers

	log.infof("starting server with %d worker threads...", srv.worker_pool.worker_count)

	for {
		defer free_all(context.temp_allocator)
		_ = thread_pool.run_pending_completions(srv.worker_pool)

		data, err := jsonrpc.read(&srv.stream)
		if err != nil {
			log_trace(&srv, fmt.tprintf("stream read error: %v", err))
			break
		}
		if len(data) == 0 {
			log_trace(&srv, "stream read error: corrupted message")
			break
		}
		log.infof("raw message data: %s", data)

		action := server_process_raw_message_bytes(
			&srv,
			data,
			&initialized,
			request_handlers,
			notif_handlers,
		)
		if action == .Shutdown {
			break
		}

		_ = thread_pool.run_pending_completions(srv.worker_pool)
	}
}

server_process_raw_message_bytes :: proc(
	srv: ^Server,
	data: []byte,
	initialized: ^bool,
	request_handlers: map[string]Request_Handler,
	notif_handlers: map[string]Notification_Handler,
) -> Server_Loop_Action {
	value, parse_err := json.parse(data, allocator = context.temp_allocator)
	if parse_err != nil {
		log_trace(srv, fmt.tprintf("json parse error: %v", parse_err))
		return .Continue
	}

	obj, obj_ok := value.(json.Object)
	if !obj_ok {
		log_trace(srv, fmt.tprintf("received message that is not a json Object: %s", data))
		return .Continue
	}

	if _, has_method := obj["method"]; !has_method {
		if id_val, id_ok := obj["id"]; id_ok {
			_, has_res := obj["result"]
			_, has_err := obj["error"]
			if has_res || has_err {
				if srv.pending_outgoing_rpc_id != 0 &&
				   jsonrpc_response_id_matches(srv.pending_outgoing_rpc_id, id_val) {
					if has_err {
						srv.pending_outgoing_create_failed = true
					}
					srv.pending_outgoing_rpc_done = true
					srv.pending_outgoing_rpc_id = 0
				} else {
					log_trace(
						srv,
						fmt.tprintf("unhandled JSON-RPC response from client: %s", data),
					)
				}
				return .Continue
			}
		}
		log_trace(srv, fmt.tprintf("JSON-RPC object without method: %s", data))
		return .Continue
	}

	method, method_ok := obj["method"].(json.String)
	if !method_ok {
		log_trace(srv, fmt.tprintf("received request with invalid 'method' field: %s", data))
		return .Continue
	}

	if id, id_ok := obj["id"]; id_ok {
		if !initialized^ && method != "initialize" {
			reply_error(srv, id, .ServerNotInitialized)
			return .Continue
		}

		if handler, ok := request_handlers[method]; ok {
			handler(srv, id, obj["params"])
		} else {
			if method == "shutdown" {
				log_trace(srv, "shutdown request received")
				return .Shutdown
			}
			reply_error(srv, id, .MethodNotFound)
		}
	} else {
		if !initialized^ {
			if method == "initialized" {
				initialized^ = true
			}
			return .Continue
		}

		if handler, ok := notif_handlers[method]; ok {
			handler(srv, obj["params"])
		}
	}

	return .Continue
}

handle_initialize :: proc(srv: ^Server, id: json.Value, params: json.Value) {
	initialize_params: InitializeParams
	if err := unmarshal(params, initialize_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("initialize request unmarshal failed: %v", err)
		log_trace(srv, descr)
		reply_error(srv, id, .ParseError, descr)
		return
	}

	for wspace in initialize_params.workspaceFolders {
		cache.cache_add_workspace(srv.storage, wspace.uri, wspace.name)
	}

	srv.client_work_done_progress = initialize_params.capabilities_.window_.workDoneProgress

	result := InitializeResult {
		capabilities = ServerCapabilities {
			textDocumentSync = .Full,
			workDoneProgressProvider = true,
			hoverProvider = true,
			completionProvider = CompletionOptions {
				triggerCharacters = {"-", ">"},
				resolveProvider = false,
			},
			definitionProvider = false,
			diagnosticProvider = nil,
			// diagnosticProvider = DiagnosticOptions {
			// 	interFileDependencies = false,
			// 	workspaceDiagnostics  = false,
			// },
			semanticTokensProvider = SemanticTokensOptions {
				legend = SemanticTokensLegend {
					tokenTypes = {
						"namespace",
						"type",
						"class",
						"enum",
						"interface",
						"struct",
						"typeParameter",
						"parameter",
						"variable",
						"property",
						"enumMember",
						"event",
						"function",
						"method",
						"macro",
						"keyword",
						"modifier",
						"comment",
						"string",
						"number",
						"regexp",
						"operator",
					},
					tokenModifiers = {
						"declaration",
						"definition",
						"readonly",
						"static",
						"deprecated",
						"abstract",
						"async",
						"modification",
						"documentation",
						"defaultLibrary",
					},
				},
				range = false,
				full = true,
			},
		},
	}
	reply(srv, id, result)
}

log_trace :: proc(srv: ^Server, message: string) {
	log.infof("log_trace: %s", message)
	message_escaped, _ := strings.replace_all(message, "\"", "\\\"", context.temp_allocator)
	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)
	strings.write_string(&b, `{"jsonrpc": "2.0","method":"$/logTrace","params":{"message":"`)
	strings.write_string(&b, message_escaped)
	strings.write_string(&b, `"}}`)
	jsonrpc.write(&srv.stream, transmute([]byte)strings.to_string(b))
}

reply :: proc(srv: ^Server, id: json.Value, params: $T) {
	response := jsonrpc.Response(T) {
		jsonrpc = "2.0",
		id      = id,
		result  = params,
	}
	data, err := json.marshal(response)
	if err != nil {
		log.errorf("failed to marshal reply params to json: %v", err)
	}
	log.infof("reply - id: %v, params: %s", id, string(data))
	jsonrpc.write(&srv.stream, transmute([]byte)data)
}

notify :: proc(srv: ^Server, method: string, params: $T) {
	notification := struct {
		jsonrpc: string,
		method:  string,
		params:  T,
	} {
		jsonrpc = "2.0",
		method  = method,
		params  = params,
	}
	data, err := json.marshal(notification, allocator = context.temp_allocator)
	if err != nil {
		log.errorf("failed to marshal notification: %v", err)
		return
	}
	log.infof("notify - method: %s, params: %s", method, string(data))
	jsonrpc.write(&srv.stream, data)
}

reply_error :: proc(srv: ^Server, id: json.Value, error_code: ErrorCodes, message: string = "") {
	response := jsonrpc.Response(any) {
		jsonrpc = "2.0",
		id = id,
		error = jsonrpc.ResponseError{code = cast(i32)error_code, message = message},
	}
	data, _ := json.marshal(response)
	log.infof("reply_error - id: %v, params: %s", id, string(data))
	jsonrpc.write(&srv.stream, transmute([]byte)data)
}
