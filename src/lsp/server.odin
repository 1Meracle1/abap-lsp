package lsp

import "../cache"
import "../jsonrpc"
import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:strings"

Server :: struct {
	stream:     jsonrpc.Stream,
	workspaces: []WorkspaceFolder,
	storage:    ^cache.Cache,
}

handle_request_t :: #type proc(srv: ^Server, id: json.Value, params: json.Value)
handle_notification_t :: #type proc(srv: ^Server, params: json.Value)

server_start :: proc(stream: jsonrpc.Stream) {
	srv: Server
	srv.stream = stream
	srv.storage = cache.init()

	request_handlers := make(map[string]handle_request_t)
	request_handlers["initialize"] = handle_initialize
	request_handlers["textDocument/hover"] = handle_hover
	request_handlers["textDocument/diagnostic"] = handle_diagnostic
	request_handlers["textDocument/semanticTokens/full"] = handle_semantic_tokens
	request_handlers["textDocument/completion"] = handle_completion

	notif_handlers := make(map[string]handle_notification_t)
	notif_handlers["textDocument/didOpen"] = handle_document_open
	notif_handlers["textDocument/didChange"] = handle_document_change

	initialized: bool

	log.info("starting server...")

	for {
		defer free_all(context.temp_allocator)

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

		value, parse_err := json.parse(data, allocator = context.temp_allocator)
		if parse_err != nil {
			log_trace(&srv, fmt.tprintf("json parse error: %v", parse_err))
			continue
		}

		if obj, obj_ok := value.(json.Object); obj_ok {
			method_value, method_value_ok := obj["method"]
			if !method_value_ok {
				log_trace(&srv, fmt.tprintf("received request without 'method' field: %s", data))
				continue
			}
			method, method_ok := obj["method"].(json.String)
			if !method_ok {
				log_trace(
					&srv,
					fmt.tprintf("received request with invalid 'method' field: %s", data),
				)
				continue
			}

			if id, id_ok := obj["id"]; id_ok {
				if !initialized && method != "initialize" {
					reply_error(&srv, id, .ServerNotInitialized)
					continue
				}

				if handler, ok := request_handlers[method]; ok {
					handler(&srv, id, obj["params"])
				} else {
					if method == "shutdown" {
						log_trace(&srv, "shutdown request received")
						break
					}
					reply_error(&srv, id, .MethodNotFound)
				}
			} else {
				if !initialized {
					if method == "initialized" {
						initialized = true
					}
					continue
				}

				if handler, ok := notif_handlers[method]; ok {
					handler(&srv, obj["params"])
				}
			}
		} else {
			log_trace(&srv, fmt.tprintf("received request that is not a json Object: %s", data))
		}
	}
}

handle_initialize :: proc(srv: ^Server, id: json.Value, params: json.Value) {
	initialize_params: InitializeParams
	if err := unmarshal(params, initialize_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("initialize request unmarshal failed: %v", err)
		log_trace(srv, descr)
		reply_error(srv, id, .ParseError, descr)
		return
	}

	srv.workspaces = initialize_params.workspaceFolders

	result := InitializeResult {
		capabilities = ServerCapabilities {
			textDocumentSync = .Full,
			hoverProvider = true,
			completionProvider = CompletionOptions {
				triggerCharacters = {".", "-", ">"},
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
	}{
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
