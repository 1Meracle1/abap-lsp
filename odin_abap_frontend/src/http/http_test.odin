package http

import "core:net"
import "core:strings"
import "core:testing"
import "core:thread"

@(test)
headers_are_case_insensitive :: proc(t: ^testing.T) {
	headers: Headers
	headers_init(&headers, context.allocator)
	defer headers_destroy(&headers, context.allocator)

	header_set(&headers, "Content-Type", "application/json", context.allocator)
	value, ok := header_get(headers, "content-type")
	testing.expect(t, ok)
	testing.expect_value(t, value, "application/json")

	header_set(&headers, "content-type", "text/plain", context.allocator)
	value, ok = header_get(headers, "CONTENT-TYPE")
	testing.expect(t, ok)
	testing.expect_value(t, value, "text/plain")
	testing.expect_value(t, len(headers), 1)
}

@(test)
request_head_writes_http_11_wire_shape :: proc(t: ^testing.T) {
	req: Request
	request_init(&req, .Post, "http://example.test/api?x=1", context.allocator)
	req.body = transmute([]u8)string("abc")
	header_set(&req.headers, "X-Test", "yes", context.allocator)
	defer request_destroy(&req, context.allocator)

	url, err := parse_url(req.url)
	testing.expect_value(t, err, Error.None)
	head := format_request_head(&req, &url, context.allocator)
	defer delete(head, context.allocator)

	testing.expect(t, strings.has_prefix(head, "POST /api?x=1 HTTP/1.1\r\n"))
	testing.expect(t, strings.contains(head, "\r\nHost: example.test\r\n"))
	testing.expect(t, strings.contains(head, "\r\nx-test: yes\r\n"))
	testing.expect(t, strings.contains(head, "\r\nContent-Length: 3\r\n"))
	testing.expect(t, strings.has_suffix(head, "\r\n\r\n"))
}

@(test)
parse_response_reads_content_length_body :: proc(t: ^testing.T) {
	raw := transmute([]u8)string("HTTP/1.1 200 OK\r\nContent-Length: 5\r\nX-Test: one\r\n\r\nhelloextra")
	res, err := parse_response(raw, .Get, context.allocator)
	testing.expect_value(t, err, Error.None)
	defer response_destroy(&res, context.allocator)

	testing.expect_value(t, res.status, "200 OK")
	testing.expect_value(t, res.status_code, HTTP_STATUS_OK)
	testing.expect_value(t, string(res.body), "hello")
	value, ok := header_get(res.headers, "x-test")
	testing.expect(t, ok)
	testing.expect_value(t, value, "one")
}

@(test)
parse_response_decodes_chunked_body :: proc(t: ^testing.T) {
	raw := transmute([]u8)string("HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n")
	res, err := parse_response(raw, .Get, context.allocator)
	testing.expect_value(t, err, Error.None)
	defer response_destroy(&res, context.allocator)

	testing.expect_value(t, string(res.body), "Wikipedia")
	testing.expect_value(t, res.content_length, 9)
}

Test_Server :: struct {
	listener:    net.TCP_Socket,
	response:    string,
	request_buf: [1024]u8,
	request_len: int,
}

test_server_run :: proc(t: ^thread.Thread) {
	server := cast(^Test_Server)t.data
	client, _, err := net.accept_tcp(server.listener)
	if err != nil {
		net.close(server.listener)
		return
	}
	defer net.close(client)
	defer net.close(server.listener)

	n, recv_err := net.recv_tcp(client, server.request_buf[:])
	if recv_err == nil {
		server.request_len = n
	}
	_, _ = net.send_tcp(client, transmute([]u8)server.response)
}

@(test)
client_get_talks_to_loopback_http_server :: proc(t: ^testing.T) {
	listener, listen_err := net.listen_tcp(net.Endpoint{address = net.IP4_Loopback, port = 0})
	testing.expect(t, listen_err == nil)
	ep, ep_err := net.bound_endpoint(listener)
	testing.expect(t, ep_err == nil)

	server := Test_Server{
		listener = listener,
		response = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nX-Test: one\r\nConnection: close\r\n\r\nhello",
	}
	worker := thread.create(test_server_run)
	worker.data = &server
	thread.start(worker)

	url := strings.builder_make(context.allocator)
	strings.write_string(&url, "http://127.0.0.1:")
	strings.write_int(&url, ep.port)
	strings.write_string(&url, "/hello?x=1")
	target := strings.to_string(url)
	defer delete(target, context.allocator)

	res, err := get(target, context.allocator)
	testing.expect_value(t, err, Error.None)
	defer response_destroy(&res, context.allocator)

	thread.join(worker)
	thread.destroy(worker)

	testing.expect_value(t, res.status_code, HTTP_STATUS_OK)
	testing.expect_value(t, string(res.body), "hello")
	value, ok := header_get(res.headers, "X-Test")
	testing.expect(t, ok)
	testing.expect_value(t, value, "one")
	testing.expect(t, strings.has_prefix(string(server.request_buf[:server.request_len]), "GET /hello?x=1 HTTP/1.1\r\n"))
}

@(test)
https_is_explicitly_unsupported_until_tls_is_added :: proc(t: ^testing.T) {
	res, err := get("https://example.com/", context.allocator)
	testing.expect_value(t, err, Error.Unsupported_Scheme)
	testing.expect_value(t, len(res.body), 0)
}
