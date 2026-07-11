package abap_frontend_cli

import execution "src:execution"
import ir "src:ir"
import "src:semantic"
import workspace "src:workspace"

import json "core:encoding/json"
import "core:fmt"
import "core:mem"
import "core:os"
import "core:slice"
import "core:strings"
import "core:terminal"
import ansi "core:terminal/ansi"

Source_Cache_Entry :: struct {
	path:   string,
	source: string,
}

Analysis_Diagnostic_Output :: struct {
	diagnostic:    semantic.Checker_Diagnostic,
	fallback_path: string,
}

Source_Position :: struct {
	line:   int,
	column: int,
}

SGR_RESET :: ansi.CSI + ansi.RESET + ansi.SGR
SGR_RED :: ansi.CSI + ansi.FG_RED + ansi.SGR
SGR_YELLOW :: ansi.CSI + ansi.FG_YELLOW + ansi.SGR

emit_json :: proc(value: any, pretty: bool, allocator: mem.Allocator) {
	options := json.Marshal_Options {
		spec = .JSON,
	}
	if pretty {
		options.pretty = true
		options.use_spaces = true
		options.spaces = 2
	}
	bytes, err := json.marshal(value, options, allocator)
	if err != nil {
		fmt.eprintf("error: failed to serialize JSON: %v\n", err)
		os.exit(1)
	}
	fmt.println(string(bytes))
}

diagnostic_prefix :: proc(
	uri: string,
	line, column: int,
	label: string,
	color: string,
	allocator: mem.Allocator = context.temp_allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, uri)
	strings.write_byte(&out, '(')
	strings.write_string(&out, fmt.tprintf("%d", line))
	strings.write_byte(&out, ':')
	strings.write_string(&out, fmt.tprintf("%d", column))
	strings.write_string(&out, ") ")
	if color != "" {
		strings.write_string(&out, color)
	}
	strings.write_string(&out, label)
	if color != "" {
		strings.write_string(&out, SGR_RESET)
	}
	return strings.to_string(out)
}

analyze_path :: proc(
	path: string,
	include_paths: []string,
	pool: ^execution.Pool,
	options: workspace.Options,
	allocator: mem.Allocator,
) -> workspace.Analysis_Result {
	abs_path, ok := workspace.absolute_clean_path(path, allocator)
	if !ok {
		return workspace.Analysis_Result{ok = false, error = "invalid path"}
	}
	info, err := os.stat(abs_path, allocator)
	if err != nil {
		return workspace.Analysis_Result{ok = false, error = "invalid path"}
	}
	if info.type == .Directory {
		opened, workspace_ok, workspace_error := workspace.open(abs_path, options, allocator)
		if !workspace_ok {
			return workspace.Analysis_Result{ok = false, error = workspace_error}
		}
		defer workspace.workspace_destroy(&opened, allocator)
		return workspace.analyze_workspace(&opened, include_paths, pool, options, allocator)
	}

	opened, workspace_ok, workspace_error := workspace.open_standalone(
		os.dir(abs_path),
		options,
		allocator,
	)
	if !workspace_ok {
		return workspace.Analysis_Result{ok = false, error = workspace_error}
	}
	defer workspace.workspace_destroy(&opened, allocator)
	return workspace.analyze_path(&opened, abs_path, include_paths, pool, options, allocator)
}

diagnostic_path_filter :: proc(path: string, allocator: mem.Allocator) -> string {
	abs_path, ok := workspace.absolute_clean_path(path, allocator)
	if !ok {
		return ""
	}
	info, err := os.stat(abs_path, allocator)
	if err != nil || info.type != .Regular {
		return ""
	}
	return abs_path
}

analysis_error_count :: proc(
	result: ^workspace.Analysis_Result,
	path_filter: string,
	allocator: mem.Allocator,
) -> int {
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	if analysis == nil {
		return 0
	}
	count := 0
	for project_result in analysis.project_results {
		if project_result.project == nil || project_result.checker == nil {
			continue
		}
		query := semantic.semantic_query(project_result.project, project_result.checker)
		diagnostics := semantic.semantic_diagnostic_copies(
			semantic.semantic_query_diagnostics(query),
			allocator,
		)
		for diagnostic in diagnostics {
			item := Analysis_Diagnostic_Output {
				diagnostic = diagnostic,
				fallback_path = project_result.root_path,
			}
			if diagnostic.severity == .Error &&
			   analysis_diagnostic_output_matches_filter(item, path_filter, allocator) {
				count += 1
			}
		}
	}
	return count
}

print_analysis_diagnostics :: proc(
	result: ^workspace.Analysis_Result,
	warnings_as_errors: bool,
	path_filter: string = "",
	allocator: mem.Allocator = context.temp_allocator,
) -> (
	count_errors: int,
) {
	use_color := terminal.color_enabled && terminal.is_terminal(os.stdout)
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	if analysis == nil {
		return
	}
	source_cache := make([dynamic]Source_Cache_Entry, 0, 16, allocator)
	diagnostics := make([dynamic]Analysis_Diagnostic_Output, 0, 16, allocator)
	for project_result in analysis.project_results {
		if project_result.project == nil || project_result.checker == nil {
			continue
		}
		query := semantic.semantic_query(project_result.project, project_result.checker)
		project_diagnostics := semantic.semantic_diagnostic_copies(
			semantic.semantic_query_diagnostics(query),
			allocator,
		)
		for diagnostic in project_diagnostics {
			append(
				&diagnostics,
				Analysis_Diagnostic_Output {
					diagnostic = diagnostic,
					fallback_path = project_result.root_path,
				},
			)
		}
	}
	slice.sort_by(diagnostics[:], analysis_diagnostic_output_less)
	for item in diagnostics {
		if !analysis_diagnostic_output_matches_filter(item, path_filter, allocator) {
			continue
		}
		if print_semantic_diagnostic(
			item.diagnostic,
			item.fallback_path,
			warnings_as_errors,
			use_color,
			&source_cache,
			allocator,
		) {
			count_errors += 1
		}
	}
	return
}

analysis_diagnostic_output_matches_filter :: proc(
	item: Analysis_Diagnostic_Output,
	path_filter: string,
	allocator: mem.Allocator,
) -> bool {
	if path_filter == "" {
		return true
	}
	path := analysis_diagnostic_output_path(item)
	return(
		workspace.normalized_uri_path_key(path, allocator) ==
		workspace.normalized_uri_path_key(path_filter, allocator) \
	)
}

analysis_diagnostic_output_less :: proc(left, right: Analysis_Diagnostic_Output) -> bool {
	return semantic.semantic_diagnostic_less_with_paths(
		left.diagnostic,
		analysis_diagnostic_output_path(left),
		right.diagnostic,
		analysis_diagnostic_output_path(right),
	)
}

analysis_diagnostic_output_path :: proc(item: Analysis_Diagnostic_Output) -> string {
	if item.diagnostic.file != nil && item.diagnostic.file.path != "" {
		return item.diagnostic.file.path
	}
	return item.fallback_path
}

print_semantic_diagnostic :: proc(
	diagnostic: semantic.Checker_Diagnostic,
	fallback_path: string,
	warnings_as_errors: bool,
	use_color: bool,
	source_cache: ^[dynamic]Source_Cache_Entry,
	allocator: mem.Allocator,
) -> bool {
	path := fallback_path
	if diagnostic.file != nil && diagnostic.file.path != "" {
		path = diagnostic.file.path
	}
	source := cached_source(path, source_cache, allocator)
	line_starts := build_line_starts(source, allocator)
	uri := display_uri(path, allocator)
	is_warning := diagnostic.severity == .Warning
	is_note := diagnostic.severity == .Note
	label := "error"
	had_error := true
	if is_note {
		label = "note"
		had_error = false
	} else if is_warning && !warnings_as_errors {
		label = "warning"
		had_error = false
	}
	color := ""
	if use_color {
		color = SGR_RED if had_error else SGR_YELLOW
	}
	start := source_position(source, line_starts[:], diagnostic.range.start)
	end := source_position(source, line_starts[:], diagnostic.range.end)
	prefix := diagnostic_prefix(uri, start.line, start.column, label, color, allocator)
	fmt.printf("%s %v: %s\n", prefix, diagnostic.kind, diagnostic.message)
	print_source_highlight(source, line_starts[:], start, end, color)
	fmt.println()
	return had_error
}

print_ir_verify_diagnostics :: proc(
	fallback_path: string,
	diagnostics: []ir.Verify_Diagnostic,
	allocator: mem.Allocator = context.temp_allocator,
) {
	source_cache := make([dynamic]Source_Cache_Entry, 0, 4, allocator)
	for diagnostic in diagnostics {
		path := fallback_path
		if diagnostic.source.file != nil && diagnostic.source.file.path != "" {
			path = diagnostic.source.file.path
		}
		uri := display_uri(path, allocator)
		if diagnostic.source.range.end > diagnostic.source.range.start {
			source := cached_source(path, &source_cache, allocator)
			line_starts := build_line_starts(source, allocator)
			pos := source_position(source, line_starts[:], diagnostic.source.range.start)
			fmt.eprintf(
				"%s(%d:%d) error IR_%v: %s",
				uri,
				pos.line,
				pos.column,
				diagnostic.kind,
				diagnostic.message,
			)
		} else {
			fmt.eprintf("%s error IR_%v: %s", uri, diagnostic.kind, diagnostic.message)
		}
		if diagnostic.function != ir.INVALID_FUNCTION_ID {
			fmt.eprintf(" function=%d", int(diagnostic.function))
		}
		if diagnostic.block != ir.INVALID_BLOCK_ID {
			fmt.eprintf(" block=%d", int(diagnostic.block))
		}
		if diagnostic.op != ir.INVALID_OP_ID {
			fmt.eprintf(" op=%d", int(diagnostic.op))
		}
		if diagnostic.value != ir.INVALID_VALUE_ID {
			fmt.eprintf(" value=%d", int(diagnostic.value))
		}
		fmt.eprintln()
	}
}

cached_source :: proc(
	path: string,
	cache: ^[dynamic]Source_Cache_Entry,
	allocator: mem.Allocator,
) -> string {
	for entry in cache^ {
		if entry.path == path {
			return entry.source
		}
	}
	source := ""
	if path != "" {
		if data, err := os.read_entire_file(path, allocator); err == nil {
			source = string(data)
		}
	}
	append(cache, Source_Cache_Entry{path = path, source = source})
	return source
}

build_line_starts :: proc(source: string, allocator: mem.Allocator) -> [dynamic]int {
	starts := make([dynamic]int, 0, 128, allocator)
	append(&starts, 0)
	for r, i in source {
		if r == '\n' {
			append(&starts, i + 1)
		}
	}
	return starts
}

source_position :: proc(source: string, starts: []int, offset: int) -> Source_Position {
	pos := clamp(offset, 0, len(source)) 
	line_index := 0
	lo, hi := 0, len(starts) - 1
	for lo <= hi {
		mid := (lo + hi) / 2
		if starts[mid] <= pos {
			line_index = mid
			lo = mid + 1
		} else {
			hi = mid - 1
		}
	}
	return Source_Position{line = line_index + 1, column = pos - starts[line_index] + 1}
}

source_line_text :: proc(source: string, starts: []int, line: int) -> string {
	index := line - 1
	if index < 0 || index >= len(starts) {
		return ""
	}
	start := starts[index]
	end := len(source)
	if index + 1 < len(starts) {
		end = starts[index + 1] - 1
	}
	if end > start && source[end - 1] == '\r' {
		end -= 1
	}
	return source[start:end]
}

display_uri :: proc(uri: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	for i in 0 ..< len(uri) {
		ch := uri[i]
		if ch == '\\' {
			ch = '/'
		}
		strings.write_byte(&out, ch)
	}
	return strings.to_string(out)
}

parse_display_uri :: proc(path: string, allocator: mem.Allocator) -> string {
	if abs_path, ok := workspace.absolute_clean_path(path, allocator); ok {
		return display_uri(abs_path, allocator)
	}
	return display_uri(path, allocator)
}

print_source_highlight :: proc(
	source: string,
	starts: []int,
	start, end: Source_Position,
	color: string,
) {
	range_end := normalized_highlight_end(source, starts, start, end)
	for line := start.line; line <= range_end.line; line += 1 {
		line_text := source_line_text(source, starts, line)
		caret_start := 1
		caret_end := len(line_text) + 1
		if line == start.line {
			caret_start = start.column
		}
		if line == range_end.line {
			caret_end = range_end.column
		}
		fmt.printf("    %s\n", line_text)
		print_caret_line(caret_start, caret_end - caret_start, color)
	}
}

print_source_highlight_stderr :: proc(
	source: string,
	starts: []int,
	start, end: Source_Position,
) {
	range_end := normalized_highlight_end(source, starts, start, end)
	for line := start.line; line <= range_end.line; line += 1 {
		line_text := source_line_text(source, starts, line)
		caret_start := 1
		caret_end := len(line_text) + 1
		if line == start.line {
			caret_start = start.column
		}
		if line == range_end.line {
			caret_end = range_end.column
		}
		fmt.eprintf("    %s\n", line_text)
		print_caret_line_stderr(caret_start, caret_end - caret_start)
	}
}

normalized_highlight_end :: proc(
	source: string,
	starts: []int,
	start, end: Source_Position,
) -> Source_Position {
	range_end := end
	if range_end.line > start.line && range_end.column == 1 {
		range_end.line -= 1
		range_end.column = len(source_line_text(source, starts, range_end.line)) + 1
	}
	if range_end.line < start.line ||
	   (range_end.line == start.line && range_end.column < start.column) {
		range_end = start
	}
	return range_end
}

print_caret_line :: proc(start_column, width: int, color: string) {
	sb: strings.Builder
	strings.builder_init(&sb, context.temp_allocator)
	strings.write_string(&sb, "    ")
	spaces := max(0, start_column - 1)
	for _ in 0 ..< spaces {
		strings.write_byte(&sb, ' ')
	}
	w := max(1, width)
	if color != "" {
		strings.write_string(&sb, color)
	}
	for _ in 0 ..< w {
		strings.write_byte(&sb, '^')
	}
	if color != "" {
		strings.write_string(&sb, SGR_RESET)
	}
	fmt.println(strings.to_string(sb))
}

print_caret_line_stderr :: proc(start_column, width: int) {
	sb: strings.Builder
	strings.builder_init(&sb, context.temp_allocator)
	strings.write_string(&sb, "    ")
	spaces := max(0, start_column - 1)
	for _ in 0 ..< spaces {
		strings.write_byte(&sb, ' ')
	}
	w := max(1, width)
	for _ in 0 ..< w {
		strings.write_byte(&sb, '^')
	}
	fmt.eprintln(strings.to_string(sb))
}
