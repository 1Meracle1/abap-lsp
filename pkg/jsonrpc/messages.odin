package jsonrpc

import "core:encoding/json"

Request :: struct {
	jsonrpc: string,
	id:      json.Value,
	method:  string,
	params:  json.Value,
}

Response :: struct($T: typeid) {
	jsonrpc: string,
	id:      json.Value,
	result:  Maybe(T),
	error:   Maybe(ResponseError),
}

ResponseError :: struct {
	code:    i32,
	message: string,
}

Notification :: struct {
	jsonrpc: string,
	method:  string,
	params:  AnyNotification,
}

AnyNotification :: union {}
