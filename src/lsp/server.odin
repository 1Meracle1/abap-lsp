package lsp

import "../cache"
import "../jsonrpc"
import "core:container/queue"
import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:mem"
import "core:strings"
import "core:sync"
import "core:thread"

// ============================================================================
// Server Types
// ============================================================================

Server :: struct {
	stream:            jsonrpc.Stream,
	workspaces:        []WorkspaceFolder,
	storage:           ^cache.Cache,

	// Thread synchronization
	stream_write_mu:   sync.Mutex,         // Protects writes to the stream
	cancelled_ids_mu:  sync.Mutex,         // Protects the cancelled_ids set
	cancelled_ids:     map[i64]bool,       // Set of cancelled request IDs

	// Thread pool
	workers:           [THREAD_POOL_SIZE]^thread.Thread,
	task_queue:        queue.Queue(^Request_Task),
	task_queue_mu:     sync.Mutex,         // Protects task_queue
	task_queue_cond:   sync.Cond,          // Signals when tasks are available
	shutdown:          bool,
}

// Number of worker threads in the pool
THREAD_POOL_SIZE :: 4

// Request_Task represents a request to be processed by a worker thread
Request_Task :: struct {
	srv:      ^Server,
	id:       json.Value,          // Request ID
	id_int:   i64,                 // Numeric ID for cancellation tracking
	method:   string,
	handler:  handle_request_t,
	raw_data: []byte,              // Raw JSON data for parsing in worker thread
}

handle_request_t :: #type proc(srv: ^Server, id: json.Value, params: json.Value)
handle_notification_t :: #type proc(srv: ^Server, params: json.Value)

// ============================================================================
// Server Initialization and Main Loop
// ============================================================================

server_start :: proc(stream: jsonrpc.Stream) {
	srv: Server
	srv.stream = stream
	srv.storage = cache.init()
	srv.cancelled_ids = make(map[i64]bool)
	queue.init(&srv.task_queue)

	// Start worker threads
	for i := 0; i < THREAD_POOL_SIZE; i += 1 {
		srv.workers[i] = thread.create(worker_thread_proc)
		srv.workers[i].data = &srv
		thread.start(srv.workers[i])
	}

	request_handlers := make(map[string]handle_request_t)
	request_handlers["initialize"] = handle_initialize
	request_handlers["textDocument/hover"] = handle_hover
	request_handlers["textDocument/diagnostic"] = handle_diagnostic
	request_handlers["textDocument/semanticTokens/full"] = handle_semantic_tokens
	request_handlers["textDocument/completion"] = handle_completion

	notif_handlers := make(map[string]handle_notification_t)
	notif_handlers["textDocument/didOpen"] = handle_document_open
	notif_handlers["textDocument/didChange"] = handle_document_change
	notif_handlers["$/cancelRequest"] = handle_cancel_request

	initialized: bool

	log.info("starting server...")

	main_loop: for {
		defer free_all(context.temp_allocator)

		// Check for shutdown (though in normal operation this comes from shutdown request)
		if srv.shutdown {
			break main_loop
		}

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
					// Handle initialize request synchronously on the main thread
					// (required by LSP spec - must complete before other requests)
					if method == "initialize" {
						handler(&srv, id, obj["params"])
					} else {
						// Dispatch other requests to worker threads
						dispatch_request(&srv, id, method, obj["params"], handler, data)
					}
				} else {
					if method == "shutdown" {
						log_trace(&srv, "shutdown request received")
						// Signal shutdown and break the main loop
						srv.shutdown = true
						break main_loop
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

	// Shutdown: finish pending tasks and cleanup
	shutdown_server(&srv)
}

// ============================================================================
// Thread Pool and Worker Functions
// ============================================================================

// dispatch_request sends a request to the worker thread pool
dispatch_request :: proc(
	srv: ^Server,
	id: json.Value,
	method: string,
	params: json.Value,
	handler: handle_request_t,
	raw_data: []byte,
) {
	// Extract numeric ID for cancellation tracking
	id_int: i64
	#partial switch v in id {
	case json.Integer:
		id_int = v
	case:
		id_int = -1 // Non-numeric IDs can't be cancelled (string IDs not common in practice)
	}

	// Allocate task data that will persist until the task completes
	task := new(Request_Task)
	task.srv = srv
	task.id = clone_json_value(id)
	task.id_int = id_int
	task.method = strings.clone(method)
	task.handler = handler
	task.raw_data = slice_clone(raw_data)

	// Add task to queue and signal workers
	sync.mutex_lock(&srv.task_queue_mu)
	queue.push_back(&srv.task_queue, task)
	sync.cond_signal(&srv.task_queue_cond)
	sync.mutex_unlock(&srv.task_queue_mu)
}

// worker_thread_proc is the main procedure for worker threads
worker_thread_proc :: proc(t: ^thread.Thread) {
	srv := cast(^Server)t.data
	if srv == nil {
		return
	}

	for {
		// Wait for a task
		sync.mutex_lock(&srv.task_queue_mu)
		
		for queue.len(srv.task_queue) == 0 && !srv.shutdown {
			sync.cond_wait(&srv.task_queue_cond, &srv.task_queue_mu)
		}

		// Check if we're shutting down with no tasks
		if srv.shutdown && queue.len(srv.task_queue) == 0 {
			sync.mutex_unlock(&srv.task_queue_mu)
			break
		}

		// Get the next task
		task := queue.pop_front(&srv.task_queue)
		sync.mutex_unlock(&srv.task_queue_mu)

		// Check if this request was cancelled before processing
		if is_request_cancelled(srv, task.id_int) {
			log.infof("request %v was cancelled, skipping", task.id)
			cleanup_task(task)
			continue
		}

		// Process the request
		process_request_task(srv, task)

		// Cleanup task resources
		cleanup_task(task)
	}
}

// process_request_task handles a single request in a worker thread
process_request_task :: proc(srv: ^Server, task: ^Request_Task) {
	// Create a thread-local temp allocator for this request
	arena: mem.Arena
	arena_buffer := make([]byte, 1024 * 1024) // 1MB per request
	defer delete(arena_buffer)
	mem.arena_init(&arena, arena_buffer)

	context.temp_allocator = mem.arena_allocator(&arena)

	// Re-parse the JSON in this thread's context to get proper allocations
	value, parse_err := json.parse(task.raw_data, allocator = context.temp_allocator)
	if parse_err != nil {
		log.errorf("worker thread JSON parse error: %v", parse_err)
		reply_error(srv, task.id, .ParseError)
		return
	}

	params: json.Value
	if obj, obj_ok := value.(json.Object); obj_ok {
		params = obj["params"]
	}

	// Check cancellation one more time before processing
	if is_request_cancelled(srv, task.id_int) {
		log.infof("request %v was cancelled before processing", task.id)
		return
	}

	// Call the handler
	task.handler(srv, task.id, params)

	// After handling, remove from cancelled set if it was there
	remove_cancelled_id(srv, task.id_int)
}

// cleanup_task frees resources allocated for a task
cleanup_task :: proc(task: ^Request_Task) {
	if task == nil {
		return
	}
	if len(task.method) > 0 {
		delete(task.method)
	}
	if len(task.raw_data) > 0 {
		delete(task.raw_data)
	}
	free_json_value(task.id)
	free(task)
}

// shutdown_server cleanly shuts down the server and worker threads
shutdown_server :: proc(srv: ^Server) {
	log.info("shutting down server...")

	// Signal shutdown
	srv.shutdown = true

	// Wake up all waiting workers
	sync.mutex_lock(&srv.task_queue_mu)
	sync.cond_broadcast(&srv.task_queue_cond)
	sync.mutex_unlock(&srv.task_queue_mu)

	// Wait for all worker threads to finish
	for i := 0; i < THREAD_POOL_SIZE; i += 1 {
		if srv.workers[i] != nil {
			thread.join(srv.workers[i])
			thread.destroy(srv.workers[i])
		}
	}

	// Cleanup remaining tasks in queue
	sync.mutex_lock(&srv.task_queue_mu)
	for queue.len(srv.task_queue) > 0 {
		task := queue.pop_front(&srv.task_queue)
		cleanup_task(task)
	}
	queue.destroy(&srv.task_queue)
	sync.mutex_unlock(&srv.task_queue_mu)

	// Cleanup
	sync.mutex_lock(&srv.cancelled_ids_mu)
	delete(srv.cancelled_ids)
	sync.mutex_unlock(&srv.cancelled_ids_mu)

	cache.destroy(srv.storage)
	log.info("server shutdown complete")
}

// ============================================================================
// Request Cancellation
// ============================================================================

// CancelParams represents the parameters for $/cancelRequest notification
CancelParams :: struct {
	id: json.Value,
}

// handle_cancel_request handles the $/cancelRequest notification
handle_cancel_request :: proc(srv: ^Server, params: json.Value) {
	// Extract the ID to cancel
	if obj, ok := params.(json.Object); ok {
		if id, id_ok := obj["id"]; id_ok {
			#partial switch v in id {
			case json.Integer:
				mark_request_cancelled(srv, v)
				log.infof("marked request %v as cancelled", v)
			case json.Float:
				// Some clients send floats
				mark_request_cancelled(srv, i64(v))
				log.infof("marked request %v as cancelled", i64(v))
			case:
				log.warnf("cancel request with non-numeric id: %v", id)
			}
		}
	}
}

// mark_request_cancelled adds an ID to the cancelled set
mark_request_cancelled :: proc(srv: ^Server, id: i64) {
	sync.mutex_lock(&srv.cancelled_ids_mu)
	defer sync.mutex_unlock(&srv.cancelled_ids_mu)
	srv.cancelled_ids[id] = true
}

// is_request_cancelled checks if a request ID has been cancelled
is_request_cancelled :: proc(srv: ^Server, id: i64) -> bool {
	if id < 0 {
		return false // Non-numeric IDs can't be cancelled
	}
	sync.mutex_lock(&srv.cancelled_ids_mu)
	defer sync.mutex_unlock(&srv.cancelled_ids_mu)
	return srv.cancelled_ids[id] or_else false
}

// remove_cancelled_id removes an ID from the cancelled set
remove_cancelled_id :: proc(srv: ^Server, id: i64) {
	if id < 0 {
		return
	}
	sync.mutex_lock(&srv.cancelled_ids_mu)
	defer sync.mutex_unlock(&srv.cancelled_ids_mu)
	delete_key(&srv.cancelled_ids, id)
}

// ============================================================================
// Request Handlers
// ============================================================================

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

// ============================================================================
// Logging and Communication
// ============================================================================

log_trace :: proc(srv: ^Server, message: string) {
	log.infof("log_trace: %s", message)
	message_escaped, _ := strings.replace_all(message, "\"", "\\\"", context.temp_allocator)
	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)
	strings.write_string(&b, `{"jsonrpc": "2.0","method":"$/logTrace","params":{"message":"`)
	strings.write_string(&b, message_escaped)
	strings.write_string(&b, `"}}`)
	write_synchronized(srv, transmute([]byte)strings.to_string(b))
}

reply :: proc(srv: ^Server, id: json.Value, params: $T) {
	response := jsonrpc.Response(T) {
		jsonrpc = "2.0",
		id      = id,
		result  = params,
	}
	data, err := json.marshal(response, allocator = context.temp_allocator)
	if err != nil {
		log.errorf("failed to marshal reply params to json: %v", err)
		return
	}
	log.infof("reply - id: %v, params: %s", id, string(data))
	write_synchronized(srv, data)
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
	write_synchronized(srv, data)
}

reply_error :: proc(srv: ^Server, id: json.Value, error_code: ErrorCodes, message: string = "") {
	response := jsonrpc.Response(any) {
		jsonrpc = "2.0",
		id = id,
		error = jsonrpc.ResponseError{code = cast(i32)error_code, message = message},
	}
	data, _ := json.marshal(response, allocator = context.temp_allocator)
	log.infof("reply_error - id: %v, params: %s", id, string(data))
	write_synchronized(srv, data)
}

// write_synchronized performs a thread-safe write to the stream
write_synchronized :: proc(srv: ^Server, data: []byte) {
	sync.mutex_lock(&srv.stream_write_mu)
	defer sync.mutex_unlock(&srv.stream_write_mu)
	jsonrpc.write(&srv.stream, data)
}

// ============================================================================
// Utility Functions
// ============================================================================

// slice_clone creates a copy of a byte slice
slice_clone :: proc(src: []byte, allocator := context.allocator) -> []byte {
	if len(src) == 0 {
		return nil
	}
	dst := make([]byte, len(src), allocator)
	copy(dst, src)
	return dst
}

// clone_json_value creates a deep copy of a json.Value
clone_json_value :: proc(val: json.Value, allocator := context.allocator) -> json.Value {
	#partial switch v in val {
	case json.Null:
		return json.Null(nil)
	case json.Integer:
		return v
	case json.Float:
		return v
	case json.Boolean:
		return v
	case json.String:
		return json.String(strings.clone(string(v), allocator))
	case:
		// For complex types, just return the value (ID is usually simple)
		return val
	}
}

// free_json_value frees a cloned json.Value
free_json_value :: proc(val: json.Value, allocator := context.allocator) {
	#partial switch v in val {
	case json.String:
		delete(string(v), allocator)
	case:
		// Other types don't need freeing
	}
}
