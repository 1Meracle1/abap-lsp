package cache

// Optional hooks for long-running analysis (parsing, project resolution). The LSP layer
// wires these to window/workDoneProgress; all pointers may be nil.
Analysis_Progress :: struct {
	user:           rawptr,
	on_parsed_file: proc(user: rawptr, path: string),
	on_phase:       proc(user: rawptr, message: string),
}

analysis_progress_phase :: proc(progress: ^Analysis_Progress, message: string) {
	if progress == nil || progress.on_phase == nil {
		return
	}
	progress.on_phase(progress.user, message)
}

analysis_progress_parsed_file :: proc(progress: ^Analysis_Progress, path: string) {
	if progress == nil || progress.on_parsed_file == nil {
		return
	}
	progress.on_parsed_file(progress.user, path)
}
