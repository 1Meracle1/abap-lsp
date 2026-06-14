package main

import lsp "src:lsp"
import stack_trace "src:stack_trace"
import workspace "src:workspace"

import base_runtime "base:runtime"
import "core:fmt"
import "core:mem"
import "core:mem/virtual"
import "core:os"

main :: proc() {
	stack_trace.install_debug_crash_trace()

	allocator := base_runtime.heap_allocator()
	context.allocator = allocator

	temp_arena: virtual.Arena
	_ = virtual.arena_init_growing(&temp_arena, mem.Gigabyte)
	temp_allocator := virtual.arena_allocator(&temp_arena)
	context.temp_allocator = temp_allocator

	if len(os.args) == 2 && os.args[1] == "--help" {
		print_usage()
		virtual.arena_destroy(&temp_arena)
		return
	}

	options, options_ok := parse_options(os.args[1:])
	if !options_ok {
		virtual.arena_destroy(&temp_arena)
		os.exit(1)
	}

	exit_code := lsp.serve_stdio_with_options(allocator, options)
	virtual.arena_destroy(&temp_arena)
	os.exit(exit_code)
}

parse_options :: proc(args: []string) -> (workspace.Options, bool) {
	options := lsp.server_default_workspace_options()
	for arg in args {
		switch arg {
		case "--disable-adt-dependency-fetch":
			options.flags += {.Disable_ADT_Dependency_Fetch}
		case:
			fmt.eprintf("unknown option %q\n", arg)
			print_usage_error()
			return options, false
		}
	}
	return options, true
}

print_usage :: proc() {
	fmt.println("abap_language_server")
	fmt.println("usage: abap_language_server [--disable-adt-dependency-fetch]")
}

print_usage_error :: proc() {
	fmt.eprintln("usage: abap_language_server [--disable-adt-dependency-fetch]")
}
