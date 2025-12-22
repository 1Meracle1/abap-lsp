package main

import "../../pkg/lsp"
import "../../pkg/jsonrpc"
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

	stream := jsonrpc.init(os2.stdin.stream, os2.stdout.stream)
	lsp.server_start(stream)
}
