package http

import "core:mem"
import "core:net"
import "core:strconv"
import "core:bytes"
import "core:strings"
import "core:time"

DEFAULT_MAX_RESPONSE_BYTES :: 16 * 1024 * 1024

Method :: enum u8 {
	Get,
	Head,
	Post,
	Put,
	Patch,
	Delete,
	Options,
}

Status :: distinct u16

HTTP_STATUS_OK                    :: Status(200)
HTTP_STATUS_CREATED               :: Status(201)
HTTP_STATUS_NO_CONTENT            :: Status(204)
HTTP_STATUS_BAD_REQUEST           :: Status(400)
HTTP_STATUS_UNAUTHORIZED          :: Status(401)
HTTP_STATUS_FORBIDDEN             :: Status(403)
HTTP_STATUS_NOT_FOUND             :: Status(404)
HTTP_STATUS_INTERNAL_SERVER_ERROR :: Status(500)

Error :: enum u8 {
	None,
	Invalid_Url,
	Unsupported_Scheme,
	Network,
	Response_Too_Large,
	Bad_Response,
	Invalid_Content_Length,
	Unsupported_Transfer_Encoding,
	Invalid_Chunk,
}

Header :: struct {
	name:  string,
	value: string,
}

Headers :: [dynamic]Header

Client :: struct {
	timeout:            time.Duration,
	max_response_bytes: int,
}

Request :: struct {
	method:  Method,
	url:     string,
	headers: Headers,
	body:    []u8,
}

Response :: struct {
	status:         string,
	status_code:    Status,
	proto:          string,
	headers:        Headers,
	body:           []u8,
	content_length: int,
}

URL :: struct {
	scheme:    string,
	authority: string,
	host:      string,
	port:      int,
	path:      string,
	query:     string,
}

default_client :: proc() -> Client {
	return Client{timeout = 30 * time.Second, max_response_bytes = DEFAULT_MAX_RESPONSE_BYTES}
}

request_init :: proc(request: ^Request, method: Method, url: string, allocator: mem.Allocator) {
	request^ = Request{method = method, url = url}
	request.headers = make([dynamic]Header, 0, 8, allocator)
}

request_destroy :: proc(request: ^Request, allocator: mem.Allocator) {
	headers_destroy(&request.headers, allocator)
	request^ = {}
}

response_destroy :: proc(response: ^Response, allocator: mem.Allocator) {
	if response.status != "" {
		delete(response.status, allocator)
	}
	if response.proto != "" {
		delete(response.proto, allocator)
	}
	headers_destroy(&response.headers, allocator)
	delete(response.body, allocator)
	response^ = {}
}

headers_destroy :: proc(headers: ^Headers, allocator: mem.Allocator) {
	for h in headers^ {
		delete(h.name, allocator)
		delete(h.value, allocator)
	}
	delete(headers^)
	headers^ = nil
}

header_set :: proc(headers: ^Headers, name, value: string, allocator: mem.Allocator) {
	if i := header_index(headers^, name); i >= 0 {
		delete(headers[i].value, allocator)
		headers[i].value = strings.clone(strings.trim_space(value), allocator)
		return
	}
	append(headers, Header{
		name = strings.to_lower(name, allocator),
		value = strings.clone(strings.trim_space(value), allocator),
	})
}

header_get :: proc(headers: Headers, name: string) -> (string, bool) {
	if i := header_index(headers, name); i >= 0 {
		return headers[i].value, true
	}
	return "", false
}

header_has :: proc(headers: Headers, name: string) -> bool {
	_, ok := header_get(headers, name)
	return ok
}

get :: proc(url: string, allocator: mem.Allocator) -> (Response, Error) {
	client := default_client()
	request: Request
	request_init(&request, .Get, url, allocator)
	defer request_destroy(&request, allocator)
	return client_do(&client, &request, allocator)
}

post :: proc(url, content_type: string, body: []u8, allocator: mem.Allocator) -> (Response, Error) {
	client := default_client()
	request: Request
	request_init(&request, .Post, url, allocator)
	request.body = body
	header_set(&request.headers, "Content-Type", content_type, allocator)
	defer request_destroy(&request, allocator)
	return client_do(&client, &request, allocator)
}

client_do :: proc(client: ^Client, request: ^Request, allocator: mem.Allocator) -> (Response, Error) {
	url, err := parse_url(request.url)
	if err != .None {
		return {}, err
	}
	if !strings.equal_fold(url.scheme, "http") {
		return {}, .Unsupported_Scheme
	}

	socket, net_err := net.dial_tcp_from_hostname_with_port_override(url.host, url.port)
	if net_err != nil {
		return {}, .Network
	}
	defer net.close(socket)

	if client.timeout > 0 {
		if net.set_option(socket, .Receive_Timeout, client.timeout) != nil ||
		   net.set_option(socket, .Send_Timeout, client.timeout) != nil {
			return {}, .Network
		}
	}

	head := format_request_head(request, &url, allocator)
	defer delete(head, allocator)
	if _, send_err := net.send_tcp(socket, transmute([]u8)head); send_err != nil {
		return {}, .Network
	}
	if len(request.body) > 0 {
		if _, send_err := net.send_tcp(socket, request.body); send_err != nil {
			return {}, .Network
		}
	}

	limit := client.max_response_bytes
	if limit <= 0 {
		limit = DEFAULT_MAX_RESPONSE_BYTES
	}
	raw, read_err := read_all(socket, limit, allocator)
	if read_err != .None {
		return {}, read_err
	}
	defer delete(raw, allocator)
	return parse_response(raw, request.method, allocator)
}

parse_url :: proc(raw: string) -> (URL, Error) {
	scheme_end := strings.index(raw, "://")
	if scheme_end <= 0 {
		return {}, .Invalid_Url
	}
	scheme := raw[:scheme_end]
	rest := raw[scheme_end + 3:]
	if hash := strings.index_byte(rest, '#'); hash >= 0 {
		rest = rest[:hash]
	}
	path_start := strings.index_byte(rest, '/')
	authority := rest
	path := "/"
	if path_start >= 0 {
		authority = rest[:path_start]
		path = rest[path_start:]
	}
	if authority == "" {
		return {}, .Invalid_Url
	}
	query := ""
	if query_start := strings.index_byte(path, '?'); query_start >= 0 {
		query = path[query_start + 1:]
		path = path[:query_start]
	}
	host, port, ok := net.split_port(authority)
	if !ok {
		return {}, .Invalid_Url
	}
	if port == 0 {
		port = 80
	}
	return URL{scheme = scheme, authority = authority, host = host, port = port, path = path, query = query}, .None
}

format_request_head :: proc(request: ^Request, url: ^URL, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, method_string(request.method))
	strings.write_byte(&out, ' ')
	if url.path == "" {
		strings.write_byte(&out, '/')
	} else {
		strings.write_string(&out, url.path)
	}
	if url.query != "" {
		strings.write_byte(&out, '?')
		strings.write_string(&out, url.query)
	}
	strings.write_string(&out, " HTTP/1.1\r\n")

	if !header_has(request.headers, "Host") {
		strings.write_string(&out, "Host: ")
		strings.write_string(&out, url.authority)
		strings.write_string(&out, "\r\n")
	}
	if !header_has(request.headers, "User-Agent") {
		strings.write_string(&out, "User-Agent: abap-lsp-odin-http/0.1\r\n")
	}
	for h in request.headers {
		if strings.equal_fold(h.name, "content-length") || strings.equal_fold(h.name, "connection") {
			continue
		}
		strings.write_string(&out, h.name)
		strings.write_string(&out, ": ")
		strings.write_string(&out, h.value)
		strings.write_string(&out, "\r\n")
	}
	strings.write_string(&out, "Content-Length: ")
	strings.write_int(&out, len(request.body))
	strings.write_string(&out, "\r\n")
	if !header_has(request.headers, "Connection") {
		strings.write_string(&out, "Connection: close\r\n")
	}
	strings.write_string(&out, "\r\n")
	return strings.to_string(out)
}

parse_response :: proc(raw: []u8, method: Method, allocator: mem.Allocator) -> (Response, Error) {
	head_end := bytes.index(raw, transmute([]u8)string("\r\n\r\n"))
	if head_end < 0 {
		return {}, .Bad_Response
	}
	head := string(raw[:head_end])
	body_bytes := raw[head_end + 4:]
	line_end := strings.index(head, "\r\n")
	if line_end < 0 {
		return {}, .Bad_Response
	}
	status_line := head[:line_end]
	first_space := strings.index_byte(status_line, ' ')
	if first_space <= 0 || first_space + 4 > len(status_line) {
		return {}, .Bad_Response
	}
	code, ok := strconv.parse_int(status_line[first_space + 1:first_space + 4], 10)
	if !ok {
		return {}, .Bad_Response
	}

	res := Response{
		status = strings.clone(status_line[first_space + 1:], allocator),
		status_code = Status(code),
		proto = strings.clone(status_line[:first_space], allocator),
		content_length = -1,
	}
	res.headers = make([dynamic]Header, 0, 8, allocator)
	defer if res.status_code == 0 {
		response_destroy(&res, allocator)
	}

	lines := head[line_end + 2:]
	for len(lines) > 0 {
		next := strings.index(lines, "\r\n")
		line := lines
		if next >= 0 {
			line = lines[:next]
			lines = lines[next + 2:]
		} else {
			lines = ""
		}
		if line == "" {
			continue
		}
		colon := strings.index_byte(line, ':')
		if colon <= 0 {
			res.status_code = 0
			return {}, .Bad_Response
		}
		header_set(&res.headers, line[:colon], line[colon + 1:], allocator)
	}

	if method == .Head {
		res.body = make([]u8, 0, allocator)
		return res, .None
	}

	if te, te_ok := header_get(res.headers, "Transfer-Encoding"); te_ok {
		te_lower := strings.to_lower(te, allocator)
		defer delete(te_lower, allocator)
		if strings.contains(te_lower, "chunked") {
			body, chunk_err := decode_chunked(body_bytes, allocator)
			if chunk_err != .None {
				res.status_code = 0
				return {}, chunk_err
			}
			res.body = body
			res.content_length = len(body)
			return res, .None
		}
		if !strings.equal_fold(strings.trim_space(te), "identity") {
			res.status_code = 0
			return {}, .Unsupported_Transfer_Encoding
		}
	}
	if length_text, length_header_ok := header_get(res.headers, "Content-Length"); length_header_ok {
		length, length_ok := strconv.parse_int(length_text, 10)
		if !length_ok || length < 0 {
			res.status_code = 0
			return {}, .Invalid_Content_Length
		}
		if length > len(body_bytes) {
			res.status_code = 0
			return {}, .Bad_Response
		}
		res.body = bytes.clone(body_bytes[:length], allocator)
		res.content_length = length
		return res, .None
	}
	res.body = bytes.clone(body_bytes, allocator)
	res.content_length = len(res.body)
	return res, .None
}

read_all :: proc(socket: net.TCP_Socket, max_bytes: int, allocator: mem.Allocator) -> ([]u8, Error) {
	data := make([dynamic]u8, 0, 4096, allocator)
	defer delete(data)
	buf: [4096]u8
	for {
		n, err := net.recv_tcp(socket, buf[:])
		if err != nil {
			return nil, .Network
		}
		if n == 0 {
			break
		}
		if max_bytes > 0 && len(data) + n > max_bytes {
			return nil, .Response_Too_Large
		}
		old_len := len(data)
		resize(&data, old_len + n)
		copy(data[old_len:], buf[:n])
	}
	return bytes.clone(data[:], allocator), .None
}

decode_chunked :: proc(input: []u8, allocator: mem.Allocator) -> ([]u8, Error) {
	out := make([dynamic]u8, 0, len(input), allocator)
	defer delete(out)
	pos := 0
	for {
		line_end := bytes.index(input[pos:], transmute([]u8)string("\r\n"))
		if line_end < 0 {
			return nil, .Invalid_Chunk
		}
		size_line := string(input[pos:pos + line_end])
		if semi := strings.index_byte(size_line, ';'); semi >= 0 {
			size_line = size_line[:semi]
		}
		size, ok := strconv.parse_int(strings.trim_space(size_line), 16)
		if !ok || size < 0 {
			return nil, .Invalid_Chunk
		}
		pos += line_end + 2
		if size == 0 {
			return bytes.clone(out[:], allocator), .None
		}
		if pos + size + 2 > len(input) ||
		   input[pos + size] != '\r' ||
		   input[pos + size + 1] != '\n' {
			return nil, .Invalid_Chunk
		}
		old_len := len(out)
		resize(&out, old_len + size)
		copy(out[old_len:], input[pos:pos + size])
		pos += size + 2
	}
}

method_string :: proc(method: Method) -> string {
	switch method {
	case .Get:
		return "GET"
	case .Head:
		return "HEAD"
	case .Post:
		return "POST"
	case .Put:
		return "PUT"
	case .Patch:
		return "PATCH"
	case .Delete:
		return "DELETE"
	case .Options:
		return "OPTIONS"
	}
	return "GET"
}

status_text :: proc(status: Status) -> string {
	switch status {
	case HTTP_STATUS_OK:
		return "OK"
	case HTTP_STATUS_CREATED:
		return "Created"
	case HTTP_STATUS_NO_CONTENT:
		return "No Content"
	case HTTP_STATUS_BAD_REQUEST:
		return "Bad Request"
	case HTTP_STATUS_UNAUTHORIZED:
		return "Unauthorized"
	case HTTP_STATUS_FORBIDDEN:
		return "Forbidden"
	case HTTP_STATUS_NOT_FOUND:
		return "Not Found"
	case HTTP_STATUS_INTERNAL_SERVER_ERROR:
		return "Internal Server Error"
	}
	return ""
}

header_index :: proc(headers: Headers, name: string) -> int {
	for h, i in headers {
		if strings.equal_fold(h.name, name) {
			return i
		}
	}
	return -1
}
