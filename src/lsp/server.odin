package lsp

import "../cache"
import "../jsonrpc"
import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:strings"

import "../lang/ast"
import "../lang/lexer"
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
	request_handlers["textDocument/diagnostic"] = handle_diagnostic

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
			diagnosticProvider = nil,
			// diagnosticProvider = DiagnosticOptions {
			// 	interFileDependencies = false,
			// 	workspaceDiagnostics  = false,
			// },
		},
	}
	reply(srv, id, result)
}

handle_document_open :: proc(srv: ^Server, params: json.Value) {
	document_open_params: DidOpenTextDocumentParams
	if err := unmarshal(params, document_open_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("textDocument/didOpen request unmarshal failed: %v", err)
		log_trace(srv, descr)
		return
	}

	uri := document_open_params.textDocument.uri
	cache.refresh_document(
		srv.storage,
		uri,
		document_open_params.textDocument.text,
		document_open_params.textDocument.version,
	)

	// Publish diagnostics after refresh
	snap := cache.get_snapshot(srv.storage, uri)
	if snap != nil {
		defer cache.release_snapshot(snap)
		publish_diagnostics(srv, uri, snap)
	}
}

handle_document_change :: proc(srv: ^Server, params: json.Value) {
	document_change_params: DidChangeTextDocumentParams
	if err := unmarshal(params, document_change_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("textDocument/didChange request unmarshal failed: %v", err)
		log_trace(srv, descr)
		return
	}

	uri := document_change_params.textDocument.uri
	for change in document_change_params.contentChanges {
		cache.refresh_document(
			srv.storage,
			uri,
			change.text,
			document_change_params.textDocument.version,
		)
	}

	// Publish diagnostics after refresh
	snap := cache.get_snapshot(srv.storage, uri)
	if snap != nil {
		defer cache.release_snapshot(snap)
		publish_diagnostics(srv, uri, snap)
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
		// Look up symbol in the correct scope chain
		if sym, ok := lookup_symbol_at_offset(snap, n.name, offset); ok {
			if sym.kind == .Form {
				// For FORM symbols, show the full signature
				hover_text = format_form_signature(sym)
			} else {
				type_str := symbols.format_type(sym.type_info)
				hover_text = fmt.tprintf("%s: %s", sym.name, type_str)
			}
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

// format_form_signature formats a complete FORM signature from the Form symbol.
// Output example:
//   FORM process_data TABLES it_input
//                     USING p_mode TYPE string
//                     CHANGING c_count TYPE i
format_form_signature :: proc(sym: symbols.Symbol) -> string {
	if sym.kind != .Form || sym.child_scope == nil {
		return sym.name
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)

	// Wrap in code block for proper Markdown rendering with preserved newlines
	strings.write_string(&b, "```abap\n")

	// Write FORM name
	strings.write_string(&b, "FORM ")
	strings.write_string(&b, sym.name)

	// Calculate indent for continuation lines (align to after "FORM ")
	indent := 5 + len(sym.name) // "FORM " is 5 chars

	// Collect parameters by kind
	tables_params := make([dynamic]symbols.Symbol, context.temp_allocator)
	using_params := make([dynamic]symbols.Symbol, context.temp_allocator)
	changing_params := make([dynamic]symbols.Symbol, context.temp_allocator)

	for _, param_sym in sym.child_scope.symbols {
		if param_sym.kind == .FormParameter {
			switch param_sym.form_param_kind {
			case .Tables:
				append(&tables_params, param_sym)
			case .Using:
				append(&using_params, param_sym)
			case .Changing:
				append(&changing_params, param_sym)
			case .None:
			// Skip non-parameters (like local variables)
			}
		}
	}

	// Helper to write parameter list
	write_params :: proc(b: ^strings.Builder, keyword: string, params: []symbols.Symbol, indent: int, is_first: ^bool) {
		if len(params) == 0 {
			return
		}

		if is_first^ {
			strings.write_byte(b, ' ')
			is_first^ = false
		} else {
			strings.write_byte(b, '\n')
			for _ in 0 ..< indent {
				strings.write_byte(b, ' ')
			}
		}

		strings.write_string(b, keyword)
		for param, i in params {
			if i > 0 {
				strings.write_byte(b, ' ')
			}
			strings.write_byte(b, ' ')
			strings.write_string(b, param.name)
			if param.type_info != nil && param.type_info.kind != .Unknown {
				strings.write_string(b, " TYPE ")
				strings.write_string(b, symbols.format_type(param.type_info))
			}
		}
	}

	is_first := true
	write_params(&b, "TABLES", tables_params[:], indent, &is_first)
	write_params(&b, "USING", using_params[:], indent, &is_first)
	write_params(&b, "CHANGING", changing_params[:], indent, &is_first)

	// Close the code block
	strings.write_string(&b, "\n```")

	return strings.to_string(b)
}

// lookup_symbol_at_offset looks up a symbol by name, considering the scope at the given offset.
// It first checks if the offset is inside a form/function and looks in the local scope,
// then falls back to the global scope.
lookup_symbol_at_offset :: proc(snap: ^cache.Snapshot, name: string, offset: int) -> (symbols.Symbol, bool) {
	// Check if we're inside a form declaration
	if enclosing_form := ast.find_enclosing_form(snap.ast, offset); enclosing_form != nil {
		// Get the form's name to find its symbol (which has the child_scope)
		form_name := enclosing_form.ident.name
		if form_sym, ok := snap.symbol_table.symbols[form_name]; ok {
			// Look up in the form's local scope first
			if form_sym.child_scope != nil {
				if sym, found := form_sym.child_scope.symbols[name]; found {
					return sym, true
				}
			}
		}
	}

	// Fall back to global scope
	if sym, ok := snap.symbol_table.symbols[name]; ok {
		return sym, true
	}

	return {}, false
}

handle_diagnostic :: proc(srv: ^Server, id: json.Value, params: json.Value) {
	diagnostic_params: DocumentDiagnosticParams
	if err := unmarshal(params, diagnostic_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("diagnostic request unmarshal failed: %v", err)
		log_trace(srv, descr)
		reply_error(srv, id, .ParseError, descr)
		return
	}

	snap := cache.get_snapshot(srv.storage, diagnostic_params.textDocument.uri)
	if snap == nil {
		// Return empty diagnostics if document not found
		result := FullDocumentDiagnosticReport {
			kind  = DocumentDiagnosticReportKind_Full,
			items = {},
		}
		reply(srv, id, result)
		return
	}
	defer cache.release_snapshot(snap)

	diagnostics := make([dynamic]Diagnostic, context.temp_allocator)

	for err in snap.ast.syntax_errors {
		append(&diagnostics, Diagnostic{
			range    = text_range_to_lsp_range(snap.text, err.range),
			severity = .Error,
			source   = "abap-lsp",
			message  = err.message,
		})
	}

	result := FullDocumentDiagnosticReport {
		kind  = DocumentDiagnosticReportKind_Full,
		items = diagnostics[:],
	}
	reply(srv, id, result)
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

offset_to_position :: proc(text: string, offset: int) -> Position {
	line := 0
	col := 0
	for i := 0; i < offset && i < len(text); i += 1 {
		if text[i] == '\n' {
			line += 1
			col = 0
		} else {
			col += 1
		}
	}
	return Position{line = line, character = col}
}

text_range_to_lsp_range :: proc(text: string, range: lexer.TextRange) -> Range {
	return Range{
		start = offset_to_position(text, range.start),
		end   = offset_to_position(text, range.end),
	}
}

publish_diagnostics :: proc(srv: ^Server, uri: string, snap: ^cache.Snapshot) {
	diagnostics := make([dynamic]Diagnostic, context.temp_allocator)

	for err in snap.ast.syntax_errors {
		append(&diagnostics, Diagnostic{
			range    = text_range_to_lsp_range(snap.text, err.range),
			severity = .Error,
			source   = "abap-lsp",
			message  = err.message,
		})
	}

	params := PublishDiagnosticsParams{
		uri         = uri,
		version     = snap.version,
		diagnostics = diagnostics[:],
	}

	notify(srv, "textDocument/publishDiagnostics", params)
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
