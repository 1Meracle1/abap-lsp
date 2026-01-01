package lsp

import "../cache"
import "../jsonrpc"
import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:strings"

import "../lang/ast"
import "../lang/symbols"

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

	cache.refresh_document(
		srv.storage,
		document_open_params.textDocument.uri,
		document_open_params.textDocument.text,
		document_open_params.textDocument.version,
	)
}

handle_document_change :: proc(srv: ^Server, params: json.Value) {
	document_change_params: DidChangeTextDocumentParams
	if err := unmarshal(params, document_change_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("document/didChange request unmarshal failed: %v", err)
		log_trace(srv, descr)
		return
	}

	for change in document_change_params.contentChanges {
		cache.refresh_document(
			srv.storage,
			document_change_params.textDocument.uri,
			change.text,
			document_change_params.textDocument.version,
		)
	}
}

handle_hover :: proc(srv: ^Server, id: json.Value, params: json.Value) {
	hover_params: HoverParams
	if err := unmarshal(params, hover_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("hover request unmarshal failed: %v", err)
		log_trace(srv, descr)
		reply_error(srv, id, .ParseError, descr)
		return
	}
	log_trace(srv, fmt.tprintf("hover_params: %v", hover_params))

	snap := cache.get_snapshot(srv.storage, hover_params.textDocument.uri)
	if snap == nil {
		reply_error(srv, id, .InvalidParams, "Document not found")
		return
	}
	defer cache.release_snapshot(snap)

	offset := position_to_offset(snap.text, hover_params.position)
	if offset < 0 {
		reply(srv, id, json.Null(nil))
		return
	}

	log_trace(srv, fmt.tprintf("hover at offset: %d", offset))

	node := ast.find_node_at_offset(&snap.ast.node, offset)
	if node == nil {
		log_trace(srv, "no node found at offset")
		reply(srv, id, json.Null(nil))
		return
	}

	hover_text := ""

	#partial switch n in node.derived {
	case ^ast.Ident:
		log_trace(srv, fmt.tprintf("found ident: %s", n.name))
		if sym, ok := snap.symbol_table.symbols[n.name]; ok {
			hover_text = fmt.tprintf("(%v) %s", sym.kind, sym.name)
		} else {
			hover_text = fmt.tprintf("(unknown) %s", n.name)
		}
	case:
	// For other nodes, maybe just show the type of node?
	// or nothing
	}

	if hover_text != "" {
		result := Hover {
			contents = MarkupContent{kind = MarkupKind_Markdown, value = hover_text},
		}
		reply(srv, id, result)
	} else {
		reply(srv, id, json.Null(nil))
	}
}

position_to_offset :: proc(text: string, pos: Position) -> int {
	line := 0
	offset := 0
	len_text := len(text)

	// fast forward to line
	for line < int(pos.line) && offset < len_text {
		if text[offset] == '\n' {
			line += 1
		}
		offset += 1
	}

	if line != int(pos.line) {
		return -1
	}

	// add character offset
	// TODO: handle utf-16 conversion properly if needed
	target_offset := offset + int(pos.character)

	// ensure we don't cross line boundary or EOF
	for i := offset; i < target_offset && i < len_text; i += 1 {
		if text[i] == '\n' {
			return i // stop at newline
		}
	}

	if target_offset > len_text {
		return len_text
	}

	return target_offset
}

log_trace :: proc(srv: ^Server, message: string) {
	log.infof("log_trace: %s", message)
	message, _ := strings.replace_all(message, "\"", "\\\"", context.temp_allocator)
	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)
	strings.write_string(&b, `{"jsonrpc": "2.0","method":"$/logTrace","message":"`)
	strings.write_string(&b, message)
	strings.write_string(&b, `"}`)
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
