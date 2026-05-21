package http

import "base:runtime"
import c "core:c/libc"
import "core:mem"
import "core:strconv"
import "core:bytes"
import "core:strings"
import "core:time"
import curl "vendor:curl"

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

Curl_Buffer :: struct {
	data:      [dynamic]u8,
	max_bytes: int,
	too_large: bool,
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
	if !strings.equal_fold(url.scheme, "http") && !strings.equal_fold(url.scheme, "https") {
		return {}, .Unsupported_Scheme
	}
	return client_do_curl(client, request, allocator)
}

client_do_curl :: proc(client: ^Client, request: ^Request, allocator: mem.Allocator) -> (Response, Error) {
	if curl.global_init(c.long(curl.GLOBAL_DEFAULT)) != curl.code.E_OK {
		return {}, .Network
	}
	handle := curl.easy_init()
	if handle == nil {
		return {}, .Network
	}
	defer curl.easy_cleanup(handle)

	limit := client.max_response_bytes
	if limit <= 0 {
		limit = DEFAULT_MAX_RESPONSE_BYTES
	}
	body := Curl_Buffer{data = make([dynamic]u8, 0, 4096, allocator), max_bytes = limit}
	head := Curl_Buffer{data = make([dynamic]u8, 0, 1024, allocator)}
	defer delete(body.data)
	defer delete(head.data)

	url := cstring_buffer(request.url, allocator)
	defer delete(url, allocator)
	if curl.easy_setopt(handle, curl.option.URL, cstring(raw_data(url))) != curl.code.E_OK ||
	   curl.easy_setopt(handle, curl.option.HTTP_VERSION, c.long(curl.HTTP_VERSION_1_1)) != curl.code.E_OK ||
	   curl.easy_setopt(handle, curl.option.NOSIGNAL, c.long(1)) != curl.code.E_OK ||
	   curl.easy_setopt(handle, curl.option.FORBID_REUSE, c.long(1)) != curl.code.E_OK ||
	   curl.easy_setopt(handle, curl.option.WRITEFUNCTION, curl.write_callback(curl_write_callback)) != curl.code.E_OK ||
	   curl.easy_setopt(handle, curl.option.WRITEDATA, &body) != curl.code.E_OK ||
	   curl.easy_setopt(handle, curl.option.HEADERFUNCTION, curl.write_callback(curl_header_callback)) != curl.code.E_OK ||
	   curl.easy_setopt(handle, curl.option.HEADERDATA, &head) != curl.code.E_OK {
		return {}, .Network
	}
	if client.timeout > 0 {
		timeout_ms := c.long(client.timeout / time.Millisecond)
		if timeout_ms <= 0 {
			timeout_ms = 1
		}
		if curl.easy_setopt(handle, curl.option.TIMEOUT_MS, timeout_ms) != curl.code.E_OK {
			return {}, .Network
		}
	}

	method_buf: []byte
	if request.method == .Head {
		if curl.easy_setopt(handle, curl.option.NOBODY, c.long(1)) != curl.code.E_OK {
			return {}, .Network
		}
	} else if request.method == .Post {
		if curl.easy_setopt(handle, curl.option.POST, c.long(1)) != curl.code.E_OK {
			return {}, .Network
		}
	} else if request.method != .Get || len(request.body) > 0 {
		method_buf = cstring_buffer(method_string(request.method), allocator)
		defer delete(method_buf, allocator)
		if curl.easy_setopt(handle, curl.option.CUSTOMREQUEST, cstring(raw_data(method_buf))) != curl.code.E_OK {
			return {}, .Network
		}
	}
	if request.method == .Post || len(request.body) > 0 {
		if curl.easy_setopt(handle, curl.option.POSTFIELDSIZE, c.long(len(request.body))) != curl.code.E_OK {
			return {}, .Network
		}
		if len(request.body) > 0 &&
		   curl.easy_setopt(handle, curl.option.POSTFIELDS, raw_data(request.body)) != curl.code.E_OK {
			return {}, .Network
		}
	}

	if !header_has(request.headers, "User-Agent") {
		ua := cstring_buffer("abap-lsp-odin-http/0.1", allocator)
		defer delete(ua, allocator)
		if curl.easy_setopt(handle, curl.option.USERAGENT, cstring(raw_data(ua))) != curl.code.E_OK {
			return {}, .Network
		}
	}
	header_list: ^curl.slist
	defer if header_list != nil {
		curl.slist_free_all(header_list)
	}
	for h in request.headers {
		if strings.equal_fold(h.name, "content-length") || strings.equal_fold(h.name, "connection") {
			continue
		}
		line := strings.builder_make(allocator)
		strings.write_string(&line, h.name)
		strings.write_string(&line, ": ")
		strings.write_string(&line, h.value)
		text := strings.to_string(line)
		ctext := cstring_buffer(text, allocator)
		next := curl.slist_append(header_list, cstring(raw_data(ctext)))
		delete(text, allocator)
		delete(ctext, allocator)
		if next == nil {
			return {}, .Network
		}
		header_list = next
	}
	if header_list != nil &&
	   curl.easy_setopt(handle, curl.option.HTTPHEADER, header_list) != curl.code.E_OK {
		return {}, .Network
	}

	code := curl.easy_perform(handle)
	if body.too_large {
		return {}, .Response_Too_Large
	}
	if code != curl.code.E_OK {
		return {}, .Network
	}
	return response_from_curl(head.data[:], body.data[:], request.method, allocator)
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
	host, port, ok := split_authority(authority)
	if !ok {
		return {}, .Invalid_Url
	}
	if port == 0 {
		port = 443 if strings.equal_fold(scheme, "https") else 80
	}
	return URL{scheme = scheme, authority = authority, host = host, port = port, path = path, query = query}, .None
}

split_authority :: proc(authority: string) -> (string, int, bool) {
	if i := strings.last_index(authority, "]:"); i > 0 && authority[0] == '[' {
		port, ok := strconv.parse_int(authority[i + 2:], 10)
		return authority[1:i], port, ok && port <= 65535
	}
	if strings.count(authority, ":") == 1 {
		i := strings.last_index(authority, ":")
		port, ok := strconv.parse_int(authority[i + 1:], 10)
		return authority[:i], port, ok && port <= 65535
	}
	return authority, 0, true
}

parse_response_head :: proc(head: string, allocator: mem.Allocator) -> (Response, Error) {
	text := head
	if strings.has_suffix(text, "\r\n\r\n") {
		text = text[:len(text) - 4]
	} else if strings.has_suffix(text, "\r\n") {
		text = text[:len(text) - 2]
	}
	line_end := strings.index(text, "\r\n")
	status_line := text
	lines := ""
	if line_end >= 0 {
		status_line = text[:line_end]
		lines = text[line_end + 2:]
	}
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
	return res, .None
}

response_from_curl :: proc(head: []u8, body: []u8, method: Method, allocator: mem.Allocator) -> (Response, Error) {
	res, err := parse_response_head(string(head), allocator)
	if err != .None {
		return {}, err
	}
	if method == .Head {
		res.body = make([]u8, 0, allocator)
		return res, .None
	}
	res.body = bytes.clone(body, allocator)
	res.content_length = len(res.body)
	return res, .None
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

curl_write_callback :: proc "c" (buffer: [^]byte, size, nitems: c.size_t, userdata: rawptr) -> c.size_t {
	context = runtime.default_context()
	sink := cast(^Curl_Buffer)userdata
	n := int(size) * int(nitems)
	if sink.max_bytes > 0 && len(sink.data) + n > sink.max_bytes {
		sink.too_large = true
		return c.size_t(curl.WRITEFUNC_ERROR)
	}
	old_len := len(sink.data)
	resize(&sink.data, old_len + n)
	copy(sink.data[old_len:], buffer[:n])
	return size * nitems
}

curl_header_callback :: proc "c" (buffer: [^]byte, size, nitems: c.size_t, userdata: rawptr) -> c.size_t {
	context = runtime.default_context()
	sink := cast(^Curl_Buffer)userdata
	n := int(size) * int(nitems)
	line := buffer[:n]
	if n >= 5 && string(line[:5]) == "HTTP/" {
		resize(&sink.data, 0)
	}
	old_len := len(sink.data)
	resize(&sink.data, old_len + n)
	copy(sink.data[old_len:], line)
	return size * nitems
}

cstring_buffer :: proc(value: string, allocator: mem.Allocator) -> []byte {
	buf := make([]byte, len(value) + 1, allocator)
	copy(buf, value)
	buf[len(value)] = 0
	return buf
}
