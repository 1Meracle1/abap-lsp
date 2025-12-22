package lsp

import "../jsonrpc"
import "../lang/ast"
import "../lang/parser"
import "core:encoding/json"
import "core:fmt"
import "core:log"

Server :: struct {
	stream:     jsonrpc.Stream,
	workspaces: []WorkspaceFolder,
}

handle_request_t :: #type proc(srv: ^Server, id: json.Value, params: json.Value)
handle_notification_t :: #type proc(srv: ^Server, params: json.Value)

server_start :: proc(stream: jsonrpc.Stream) {
	srv: Server
	srv.stream = stream

	request_handlers := make(map[string]handle_request_t)
	request_handlers["initialize"] = handle_initialize

	notif_handlers := make(map[string]handle_notification_t)
	notif_handlers["textDocument/didOpen"] = handle_document_open

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
			completionProvider = CompletionOptions {
				triggerCharacters = {"."},
				resolveProvider = false,
			},
			definitionProvider = false,
		},
	}
	reply(srv, id, result)
}

handle_document_open :: proc(srv: ^Server, params: json.Value) {
	document_open_params: DidOpenTextDocumentParams
	if err := unmarshal(params, document_open_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("document/didOpen request unmarshal failed: %v", err)
		log_trace(srv, descr)
		return
	}

	file := ast.new(ast.File, {})
    file.fullpath = document_open_params.textDocument.uri
    file.src = document_open_params.textDocument.text
	p: parser.Parser
	parser.parse_file(&p, file)
}

log_trace :: proc(srv: ^Server, message: string) {
	log.infof("log_trace: %s", message)
	data := fmt.tprintf(`{{"jsonrpc": "2.0","method":"$/logTrace","message":"%s"}`, message)
	jsonrpc.write(&srv.stream, transmute([]byte)data)
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
