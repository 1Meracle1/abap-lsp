package stack_trace

when ODIN_OS != .Windows && ODIN_OS != .Darwin {
	install_debug_crash_trace :: proc() {}
}
