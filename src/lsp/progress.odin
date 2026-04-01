package lsp

import "../cache"
import "../jsonrpc"
import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:path/filepath"
import "core:strings"

Work_Done_Session :: struct {
	srv:          ^Server,
	token:        string,
	active:       bool,
	files_loaded: int,
}

Work_Done_Create_Params :: struct {
	token: string,
}

Work_Done_Create_Request :: struct {
	jsonrpc: string `json:"jsonrpc"`,
	id:      i64 `json:"id"`,
	method:  string `json:"method"`,
	params:  Work_Done_Create_Params `json:"params"`,
}

Progress_Notification_Params_Begin :: struct {
	token: string `json:"token"`,
	value: Progress_Value_Begin `json:"value"`,
}

Progress_Value_Begin :: struct {
	kind:        string `json:"kind"`,
	title:       string `json:"title"`,
	cancellable: bool `json:"cancellable"`,
}

Progress_Notification_Params_Report :: struct {
	token: string `json:"token"`,
	value: Progress_Value_Report `json:"value"`,
}

Progress_Value_Report :: struct {
	kind:    string `json:"kind"`,
	message: string `json:"message"`,
}

Progress_Notification_Params_End :: struct {
	token: string `json:"token"`,
	value: Progress_Value_End `json:"value"`,
}

Progress_Value_End :: struct {
	kind:    string `json:"kind"`,
	message: string `json:"message"`,
}

jsonrpc_response_id_matches :: proc(expected: i64, id_val: json.Value) -> bool {
	#partial switch v in id_val {
	case json.Integer:
		return v == expected
	case json.Float:
		return i64(v) == expected
	case:
		return false
	}
}

// Blocks on the JSON-RPC stream until the client answers window/workDoneProgress/create, dispatching
// other inbound messages in the meantime. Returns false on I/O failure or shutdown.
work_done_create_and_wait :: proc(srv: ^Server) -> bool {
	for !srv.pending_outgoing_rpc_done {
		data, err := jsonrpc.read(&srv.stream)
		if err != nil {
			log_trace(srv, fmt.tprintf("work_done wait read error: %v", err))
			srv.pending_outgoing_rpc_id = 0
			return false
		}
		if len(data) == 0 {
			log_trace(srv, "work_done wait: empty message")
			srv.pending_outgoing_rpc_id = 0
			return false
		}

		action := server_process_raw_message_bytes(
			srv,
			data,
			srv.dispatch_initialized,
			srv.dispatch_request_handlers^,
			srv.dispatch_notif_handlers^,
		)
		#partial switch action {
		case .Shutdown:
			srv.pending_outgoing_rpc_id = 0
			return false
		case .Continue:
		}

		if srv.pending_outgoing_rpc_done {
			return true
		}
	}
	return true
}

work_done_session_begin :: proc(srv: ^Server, title: string) -> (session: Work_Done_Session, ok: bool) {
	session.srv = srv
	if !srv.client_work_done_progress {
		return session, false
	}

	srv.next_jsonrpc_out_id += 1
	rid := srv.next_jsonrpc_out_id
	session.token = fmt.tprintf("abapls-wd-%d", rid)

	srv.pending_outgoing_rpc_id = rid
	srv.pending_outgoing_rpc_done = false
	srv.pending_outgoing_create_failed = false

	req := Work_Done_Create_Request {
		jsonrpc = "2.0",
		id      = rid,
		method  = "window/workDoneProgress/create",
		params  = Work_Done_Create_Params{token = session.token},
	}
	data, m_err := json.marshal(req, allocator = context.temp_allocator)
	if m_err != nil {
		log.errorf("marshal workDoneProgress/create failed: %v", m_err)
		srv.pending_outgoing_rpc_id = 0
		return session, false
	}
	if w_err := jsonrpc.write(&srv.stream, data); w_err != nil {
		log_trace(srv, fmt.tprintf("write workDoneProgress/create failed: %v", w_err))
		srv.pending_outgoing_rpc_id = 0
		return session, false
	}

	if !work_done_create_and_wait(srv) {
		return session, false
	}
	if srv.pending_outgoing_create_failed {
		srv.pending_outgoing_create_failed = false
		return session, false
	}

	session.active = true
	notify(
		srv,
		"$/progress",
		Progress_Notification_Params_Begin {
			token = session.token,
			value = Progress_Value_Begin{kind = "begin", title = title, cancellable = false},
		},
	)
	return session, true
}

work_done_session_report :: proc(session: ^Work_Done_Session, message: string) {
	if session == nil || !session.active || session.srv == nil {
		return
	}
	if len(strings.trim_space(message)) == 0 {
		return
	}
	notify(
		session.srv,
		"$/progress",
		Progress_Notification_Params_Report {
			token = session.token,
			value = Progress_Value_Report{kind = "report", message = message},
		},
	)
}

work_done_session_end :: proc(session: ^Work_Done_Session, message: string = "") {
	if session == nil || !session.active || session.srv == nil {
		return
	}
	session.active = false
	kind_msg := message
	if len(strings.trim_space(kind_msg)) == 0 {
		kind_msg = "Done."
	}
	notify(
		session.srv,
		"$/progress",
		Progress_Notification_Params_End {
			token = session.token,
			value = Progress_Value_End{kind = "end", message = kind_msg},
		},
	)
}

work_done_analysis_on_phase :: proc(user: rawptr, message: string) {
	session := cast(^Work_Done_Session)user
	if session == nil {
		return
	}
	work_done_session_report(session, message)
}

work_done_analysis_on_parsed_file :: proc(user: rawptr, path: string) {
	session := cast(^Work_Done_Session)user
	if session == nil {
		return
	}
	session.files_loaded += 1
	base := filepath.base(path)
	if len(base) == 0 {
		base = path
	}
	work_done_session_report(
		session,
		fmt.tprintf("Parsed %s (%d file(s))", base, session.files_loaded),
	)
}

work_done_fill_analysis_progress :: proc(session: ^Work_Done_Session, progress: ^cache.Analysis_Progress) {
	if session == nil || progress == nil || !session.active {
		return
	}
	progress.user = session
	progress.on_phase = work_done_analysis_on_phase
	progress.on_parsed_file = work_done_analysis_on_parsed_file
}
