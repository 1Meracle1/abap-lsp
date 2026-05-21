package http

import "core:net"
import "core:strings"
import "core:testing"
import "core:thread"
import "core:time"

@(test)
headers_are_case_insensitive :: proc(t: ^testing.T) {
	headers: Headers
	headers = make([dynamic]Header, 0, 8, context.allocator)
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
parse_url_defaults_ports_by_scheme :: proc(t: ^testing.T) {
	url, err := parse_url("http://example.test/api")
	testing.expect_value(t, err, Error.None)
	testing.expect_value(t, url.port, 80)
	url, err = parse_url("https://example.test/api")
	testing.expect_value(t, err, Error.None)
	testing.expect_value(t, url.port, 443)
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
https_scheme_reaches_transport :: proc(t: ^testing.T) {
	client := default_client()
	client.timeout = 2 * time.Second
	req: Request
	request_init(&req, .Get, "https://127.0.0.1:1/", context.allocator)
	defer request_destroy(&req, context.allocator)

	res, err := client_do(&client, &req, context.allocator)
	testing.expect_value(t, err, Error.Network)
	testing.expect_value(t, len(res.body), 0)
}
