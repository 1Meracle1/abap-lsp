package main

import lsp "src:lsp"
import stack_trace "src:stack_trace"

import "core:mem"
import "core:mem/virtual"
import "core:os"

main :: proc() {
	stack_trace.install_debug_crash_trace()

	arena: virtual.Arena
	_ = virtual.arena_init_growing(&arena, mem.Gigabyte)
	allocator := virtual.arena_allocator(&arena)
	context.allocator = allocator

	temp_arena: virtual.Arena
	_ = virtual.arena_init_growing(&temp_arena, mem.Gigabyte)
	temp_allocator := virtual.arena_allocator(&temp_arena)
	context.temp_allocator = temp_allocator

	exit_code := lsp.serve_stdio(allocator)
	os.exit(exit_code)
}
