package main

import "../lsp"
import "../jsonrpc"
import "core:os"
import "core:os/os2"
import "core:mem"
import "core:log"

main :: proc() {
	init_global_temporary_allocator(mem.Megabyte * 100)
	logs_file, logs_err := os2.open("D:/dev/odin/abap-lsp/logs.txt", {.Write, .Create, .Trunc})
	if logs_err != nil {
		panic("failed to open logs file for writing")
	}
	context.logger = log.create_file_logger(cast(os.Handle)os2.fd(logs_file))

	handle, handle_ok := create_pipe(`\\.\pipe\abap-ls`)
	if !handle_ok {
		panic("failed to create named pipe")
	}
	io_stream := stream_from_handle(handle)
	stream := jsonrpc.init(io_stream, io_stream)
	// stream := jsonrpc.init(os2.stdin.stream, os2.stdout.stream)
	lsp.server_start(stream)
}
