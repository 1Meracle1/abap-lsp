package abap_frontend_lsp

import json "core:encoding/json"
import "core:fmt"
import "core:mem"
import "core:os"
import "core:strconv"
import "core:strings"
import "core:sync"

MAX_HEADER_LINE_BYTES :: 8192

@(private = "file")
write_frame_mutex: sync.Mutex

Frame_Status :: enum {
	Ok,
	Closed,
	Error,
}

Frame :: struct {
	status:  Frame_Status,
	payload: []byte,
	error:   string,
}

read_frame :: proc(input: ^os.File, allocator: mem.Allocator) -> Frame {
	content_length := -1
	for {
		line, ok, err := read_header_line(input, allocator)
		if err != nil {
			if err == .Invalid_Command {
				return Frame{status = .Error, error = "header line too long"}
			}
			return Frame{status = .Error, error = "failed to read header"}
		}
		if !ok {
			if content_length < 0 {
				return Frame{status = .Closed}
			}
			return Frame{status = .Error, error = "truncated header"}
		}
		if line == "\r\n" || line == "\n" || line == "" {
			break
		}
		if separator := strings.index_byte(line, ':');
		   separator != -1 &&
		   strings.equal_fold(strings.trim_space(line[:separator]), "Content-Length") {
			value := line[separator + 1:]
			parsed, parse_ok := strconv.parse_int(strings.trim_space(value), 10)
			if !parse_ok || parsed < 0 {
				return Frame{status = .Error, error = "invalid Content-Length header"}
			}
			content_length = parsed
		}
	}
	if content_length < 0 {
		return Frame{status = .Error, error = "missing Content-Length header"}
	}
	payload := make([]byte, content_length, allocator)
	if content_length > 0 {
		n, err := os.read_full(input, payload)
		if err != nil || n != content_length {
			return Frame{status = .Error, error = "truncated payload"}
		}
	}
	return Frame{status = .Ok, payload = payload}
}

write_frame :: proc(output: ^os.File, payload: []byte) -> bool {
	sync.mutex_lock(&write_frame_mutex)
	defer sync.mutex_unlock(&write_frame_mutex)

	header := fmt.tprintf("Content-Length: %d\r\n\r\n", len(payload))
	return(
		write_all(output, transmute([]byte)header) &&
		write_all(output, payload) &&
		os.flush(output) == nil \
	)
}

rpc_success_payload :: proc(
	id: json.Value,
	result: any,
	allocator: mem.Allocator,
) -> (
	[]byte,
	bool,
) {
	id_bytes, id_err := json.marshal(id, json.Marshal_Options{spec = .JSON}, allocator)
	if id_err != nil {
		return nil, false
	}
	result_bytes, result_err := json.marshal(result, json.Marshal_Options{spec = .JSON}, allocator)
	if result_err != nil {
		return nil, false
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, `{"jsonrpc":"2.0","id":`)
	strings.write_string(&out, string(id_bytes))
	strings.write_string(&out, `,"result":`)
	strings.write_string(&out, string(result_bytes))
	strings.write_byte(&out, '}')
	return transmute([]byte)strings.to_string(out), true
}

rpc_error_payload :: proc(
	id: json.Value,
	code: int,
	message: string,
	allocator: mem.Allocator,
) -> (
	[]byte,
	bool,
) {
	id_bytes, id_err := json.marshal(id, json.Marshal_Options{spec = .JSON}, allocator)
	if id_err != nil {
		return nil, false
	}
	error_value := Rpc_Error_JSON {
		code    = code,
		message = message,
	}
	error_bytes, error_err := json.marshal(
		error_value,
		json.Marshal_Options{spec = .JSON},
		allocator,
	)
	if error_err != nil {
		return nil, false
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, `{"jsonrpc":"2.0","id":`)
	strings.write_string(&out, string(id_bytes))
	strings.write_string(&out, `,"error":`)
	strings.write_string(&out, string(error_bytes))
	strings.write_byte(&out, '}')
	return transmute([]byte)strings.to_string(out), true
}

notification_payload :: proc(
	method: string,
	params: any,
	allocator: mem.Allocator,
) -> (
	[]byte,
	bool,
) {
	method_bytes, method_err := json.marshal(method, json.Marshal_Options{spec = .JSON}, allocator)
	if method_err != nil {
		return nil, false
	}
	params_bytes, params_err := json.marshal(params, json.Marshal_Options{spec = .JSON}, allocator)
	if params_err != nil {
		return nil, false
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, `{"jsonrpc":"2.0","method":`)
	strings.write_string(&out, string(method_bytes))
	strings.write_string(&out, `,"params":`)
	strings.write_string(&out, string(params_bytes))
	strings.write_byte(&out, '}')
	return transmute([]byte)strings.to_string(out), true
}

parse_rpc_message :: proc(payload: []byte, allocator: mem.Allocator) -> Rpc_Message {
	root_value, parse_err := json.parse(payload, .JSON, true, allocator)
	if parse_err != nil {
		return Rpc_Message{ok = false, error = "invalid JSON"}
	}
	object, object_ok := root_value.(json.Object)
	if !object_ok {
		return Rpc_Message{ok = false, error = "JSON-RPC message must be an object"}
	}
	version, version_ok := object_string(object, "jsonrpc")
	if !version_ok || version != JSON_RPC_VERSION {
		return Rpc_Message{ok = false, error = "invalid jsonrpc version"}
	}
	method, method_ok := object_string(object, "method")
	if !method_ok || method == "" {
		return Rpc_Message{ok = false, error = "missing method"}
	}
	id := json.Value(json.Null(nil))
	has_id := false
	if id_value, ok := object["id"]; ok {
		id = id_value
		has_id = true
	}
	params := json.Value(json.Null(nil))
	if params_value, ok := object["params"]; ok {
		params = params_value
	}
	return Rpc_Message{id = id, method = method, params = params, has_id = has_id, ok = true}
}

Rpc_Error_JSON :: struct {
	code:    int `json:"code"`,
	message: string `json:"message"`,
}

read_header_line :: proc(
	input: ^os.File,
	allocator: mem.Allocator,
) -> (
	line: string,
	ok: bool,
	err: os.Error,
) {
	out := strings.builder_make(allocator)
	buffer: [1]byte
	for {
		n, read_err := os.read(input, buffer[:])
		if read_err != nil {
			err = read_err
			return
		}
		if n == 0 {
			ok = false
			line = strings.to_string(out)
			return
		}
		strings.write_byte(&out, buffer[0])
		if buffer[0] == '\n' {
			ok = true
			line = strings.to_string(out)
			return
		}
		// LSP header lines are small; keep a framing cap so a peer cannot force
		// unbounded allocation before sending a newline.
		if len(out.buf) > MAX_HEADER_LINE_BYTES {
			err = .Invalid_Command
			return
		}
	}
}

write_all :: proc(output: ^os.File, payload: []byte) -> bool {
	written := 0
	for written < len(payload) {
		n, err := os.write(output, payload[written:])
		if err != nil || n <= 0 {
			return false
		}
		written += n
	}
	return true
}
