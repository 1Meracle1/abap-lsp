package main

import "src:ast"
import execution "src:execution"
import "src:parser"
import "src:semantic"
import stack_trace "src:stack_trace"
import "src:tokenizer"
import trace "src:trace"
import workspace "src:workspace"

import "base:runtime"
import "core:container/xar"
import "core:fmt"
import "core:mem"
import "core:mem/virtual"
import "core:os"
import "core:slice"
import "core:strings"
import "core:terminal"
import ansi "core:terminal/ansi"
import "core:time"

Node_Count :: struct {
	name:  string,
	count: int,
}

Node_Counts :: struct {
	items: [dynamic]Node_Count,
}

Allocation_Location_Total :: struct {
	location: runtime.Source_Code_Location,
	bytes:    i64,
	count:    int,
}

Source_Cache_Entry :: struct {
	path:   string,
	source: string,
}

Analyze_Diagnostic_Output :: struct {
	diagnostic:    semantic.Checker_Diagnostic,
	fallback_path: string,
}

Tree_State :: struct {
	index: int,
}

SGR_RESET :: ansi.CSI + ansi.RESET + ansi.SGR
SGR_RED :: ansi.CSI + ansi.FG_RED + ansi.SGR
SGR_YELLOW :: ansi.CSI + ansi.FG_YELLOW + ansi.SGR

main :: proc() {
	stack_trace.install_debug_crash_trace()

	args := os.args
	if len(args) == 2 && args[1] == "--help" {
		print_usage()
		return
	}
	if len(args) < 2 {
		print_usage()
		os.exit(1)
	}

	arena: virtual.Arena
	_ = virtual.arena_init_growing(&arena, mem.Gigabyte)
	allocator := virtual.arena_allocator(&arena)
	context.allocator = allocator

	temp_arena: virtual.Arena
	_ = virtual.arena_init_growing(&temp_arena, mem.Gigabyte)
	temp_allocator := virtual.arena_allocator(&temp_arena)
	context.temp_allocator = temp_allocator

	command := args[1]
	if command == "analyze" {
		if len(args) < 3 {
			print_usage()
			os.exit(1)
		}
		run_analyze(args, allocator)
		return
	}
	if len(args) != 3 {
		print_usage()
		os.exit(1)
	}

	path := args[2]
	if command == "parse" {
		run_parse(path, false, allocator)
		return
	}
	if command == "tree" {
		run_parse(path, true, allocator)
		return
	}

	print_usage()
	os.exit(1)
}

print_usage :: proc() {
	fmt.println("abap_frontend")
	fmt.println("usage: abap_frontend --help")
	fmt.println("       abap_frontend parse <file>")
	fmt.println("       abap_frontend tree <file>")
	fmt.println(
		"       abap_frontend analyze <file-or-folder> [--include <file>...] [--warnings-as-errors] [--enable-dependency-diagnostics]",
	)
}

read_source :: proc(path: string, allocator: mem.Allocator) -> (string, bool) {
	data, err := os.read_entire_file(path, allocator)
	if err != nil {
		fmt.printf("error\tread\t%s\t%v\n", path, err)
		return "", false
	}
	return string(data), true
}

run_tokens :: proc(path: string, allocator: mem.Allocator) {
	source, ok := read_source(path, allocator)
	if !ok {
		os.exit(1)
	}

	lexed := tokenizer.tokenize(source, allocator)
	fmt.printf("file\t%s\n", path)
	fmt.printf("tokens\t%d\n", len(lexed.tokens))
	for tok in lexed.tokens {
		fmt.printf("token\t%d\t%v\t%d\t%d\n", tok.index, tok.kind, tok.range.start, tok.range.end)
	}
	print_lex_errors(lexed.errors)
}

run_parse :: proc(path: string, dump_tree: bool, allocator: mem.Allocator) {
	source, ok := read_source(path, allocator)
	if !ok {
		os.exit(1)
	}
	start_time := time.now()
	{
		context.temp_allocator = allocator
		parsed := parser.parse(source, path, allocator)
		had_errors := print_parse_errors(path, source, parsed.errors)
		if dump_tree {
			print_node_counts(parsed.root, allocator)
			print_tree(parsed.root)
		}
		if had_errors {
			os.exit(1)
		} else {
			fmt.printf(
				"run_parse - finished with no errors - elapsed_ms=%.3f\n",
				time.duration_milliseconds(time.since(start_time)),
			)
		}
	}
}

run_analyze :: proc(args: []string, allocator: mem.Allocator) {
	assert(
		context.temp_allocator.procedure == virtual.arena_allocator_proc &&
		context.temp_allocator.data != nil,
	)
	temp_arena := virtual.arena_temp_begin(cast(^virtual.Arena)context.temp_allocator.data)
	defer virtual.arena_temp_end(temp_arena)

	context.allocator = allocator
	start_time := time.now()
	when trace.ENABLED {
		tracker: mem.Tracking_Allocator
		mem.tracking_allocator_init(&tracker, allocator, allocator)
		tracked_allocator := mem.tracking_allocator(&tracker)
		context.allocator = tracked_allocator
	}

	target_path := args[2]

	warnings_as_errors := false
	workspace_flags := workspace.Option_Flags{.Enable_ADT}
	include_paths := make([dynamic]string, 0, 4, context.temp_allocator)
	for i := 3; i < len(args); {
		if args[i] == "--warnings-as-errors" {
			warnings_as_errors = true
			i += 1
		} else if args[i] == "--enable-dependency-diagnostics" {
			workspace_flags += {.Enable_Dependency_Diagnostics}
			i += 1
		} else if args[i] == "--include" && i + 1 < len(args) {
			append(&include_paths, args[i + 1])
			i += 2
		} else {
			print_usage()
			os.exit(1)
		}
	}

	pool: execution.Pool
	execution.pool_init(
		&pool,
		execution.Options {
			worker_count = execution.AUTO_WORKER_COUNT,
			queue_capacity = 128,
			deque_capacity = 128,
		},
		context.allocator,
	)
	if pool.options.worker_count > 0 {
		execution.pool_start(&pool)
	}

	result := analyze_cli_path(
		target_path,
		include_paths[:],
		&pool,
		workspace.Options{flags = workspace_flags},
		context.allocator,
	)
	if !result.ok {
		fmt.printf("error\tanalyze\t%s\n", result.error)
		execution.pool_destroy(&pool)
		os.exit(1)
	}
	when trace.ENABLED {
		print_analyze_counts(&result)
	}
	diagnostic_path_filter := analyze_diagnostic_path_filter(target_path, context.temp_allocator)
	count_errors := print_analyze_diagnostics(&result, warnings_as_errors, diagnostic_path_filter)
	workspace.analysis_result_destroy(&result, context.allocator)
	execution.pool_destroy(&pool)
	when trace.ENABLED {
		trace.eprintf(
			"[trace - main] run_analyze - elapsed_ms=%.3f\n",
			trace.duration_ms_since(start_time),
		)
		print_analyze_memory_report(&tracker)
		mem.tracking_allocator_destroy(&tracker)
	}
	if count_errors > 0 {
		fmt.printf(
			"run_analyze - finished with %d errors - elapsed_ms=%.3f\n",
			count_errors,
			time.duration_milliseconds(time.since(start_time)),
		)
		os.exit(1)
	} else {
		fmt.printf(
			"run_analyze - finished with no errors - elapsed_ms=%.3f\n",
			time.duration_milliseconds(time.since(start_time)),
		)
	}
}

analyze_diagnostic_path_filter :: proc(path: string, allocator: mem.Allocator) -> string {
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

analyze_cli_path :: proc(
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

print_analyze_memory_report :: proc(tracker: ^mem.Tracking_Allocator) {
	assert(
		context.temp_allocator.procedure == virtual.arena_allocator_proc &&
		context.temp_allocator.data != nil,
	)
	temp_arena := virtual.arena_temp_begin(cast(^virtual.Arena)context.temp_allocator.data)
	defer virtual.arena_temp_end(temp_arena)

	totals := make([dynamic]Allocation_Location_Total, 0, 64, context.temp_allocator)
	for _, entry in tracker.allocation_map {
		found := false
		for &total in totals {
			if source_location_equal(total.location, entry.location) {
				total.bytes += i64(entry.size)
				total.count += 1
				found = true
				break
			}
		}
		if !found {
			append(&totals, Allocation_Location_Total{entry.location, i64(entry.size), 1})
		}
	}
	slice.sort_by(totals[:], allocation_location_total_less)

	fmt.printf(
		"[trace - main] Memory summary: used=%d KB, peak=%d KB, total allocated=%d KB, allocations=%d, locations=%d\n",
		tracker.current_memory_allocated / mem.Kilobyte,
		tracker.peak_memory_allocated / mem.Kilobyte,
		tracker.total_memory_allocated / mem.Kilobyte,
		tracker.total_allocation_count,
		len(totals),
	)
	for total in totals {
		fmt.printf(
			"[trace - main] Memory location: %d KB in %d allocation(s) at %s(%d:%d), proc=%s\n",
			total.bytes / mem.Kilobyte,
			total.count,
			total.location.file_path,
			total.location.line,
			total.location.column,
			total.location.procedure,
		)
	}
}

source_location_equal :: proc(a, b: runtime.Source_Code_Location) -> bool {
	return(
		a.file_path == b.file_path &&
		a.line == b.line &&
		a.column == b.column &&
		a.procedure == b.procedure \
	)
}

allocation_location_total_less :: proc(a, b: Allocation_Location_Total) -> bool {
	return a.bytes > b.bytes
}

print_analyze_counts :: proc(result: ^workspace.Analysis_Result) {
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	if analysis == nil {
		fmt.println(
			"[trace - main] Analysis summary: projects=0, files=0, symbols=0, scopes=0, uses=0, diagnostics=0, unresolved=0, external requests=0",
		)
		return
	}
	files, symbols, scopes, uses, diagnostics: int
	for project_result in analysis.project_results {
		files += len(project_result.files)
		if project_result.project != nil {
			symbols += xar.len(project_result.project.entities)
			scopes += xar.len(project_result.project.scopes)
		}
		if project_result.checker != nil {
			uses += len(project_result.checker.info.uses)
			diagnostics += len(project_result.checker.info.diagnostics)
		}
	}
	fmt.printf(
		"[trace - main] Analysis summary: projects=%d, files=%d, symbols=%d, scopes=%d, uses=%d, diagnostics=%d, unresolved=%d, external requests=%d\n",
		len(analysis.project_results),
		files,
		symbols,
		scopes,
		uses,
		diagnostics,
		len(analysis.unresolved),
		len(analysis.external_requests),
	)
}

Source_Position :: struct {
	line:   int,
	column: int,
}

build_line_starts :: proc(source: string, allocator: mem.Allocator) -> [dynamic]int {
	starts := make([dynamic]int, 0, 128, allocator)
	append(&starts, 0)
	for i in 0 ..< len(source) {
		if source[i] == '\n' {
			append(&starts, i + 1)
		}
	}
	return starts
}

source_position :: proc(source: string, starts: []int, offset: int) -> Source_Position {
	pos := offset
	if pos < 0 {
		pos = 0
	}
	if pos > len(source) {
		pos = len(source)
	}
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

print_caret_line :: proc(start_column, width: int, color: string) {
	fmt.print("    ")
	spaces := start_column - 1
	for _ in 0 ..< spaces {
		fmt.print(" ")
	}
	w := width
	if w < 1 {
		w = 1
	}
	if color != "" {
		fmt.print(color)
	}
	for _ in 0 ..< w {
		fmt.print("^")
	}
	if color != "" {
		fmt.print(SGR_RESET)
	}
	fmt.println()
}

print_source_highlight :: proc(
	source: string,
	starts: []int,
	start, end: Source_Position,
	color: string,
) {
	range_end := end
	if range_end.line > start.line && range_end.column == 1 {
		range_end.line -= 1
		range_end.column = len(source_line_text(source, starts, range_end.line)) + 1
	}
	if range_end.line < start.line ||
	   (range_end.line == start.line && range_end.column < start.column) {
		range_end = start
	}

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

print_analyze_diagnostics :: proc(
	result: ^workspace.Analysis_Result,
	warnings_as_errors: bool,
	path_filter: string = "",
) -> (
	count_errors: int,
) {
	use_color := terminal.color_enabled && terminal.is_terminal(os.stdout)
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	if analysis == nil {
		return
	}
	source_cache := make([dynamic]Source_Cache_Entry, 0, 16, context.temp_allocator)
	diagnostics := make([dynamic]Analyze_Diagnostic_Output, 0, 16, context.temp_allocator)
	for project_result in analysis.project_results {
		if project_result.project == nil || project_result.checker == nil {
			continue
		}
		query := semantic.semantic_query(project_result.project, project_result.checker)
		project_diagnostics := semantic.semantic_diagnostic_copies(
			semantic.semantic_query_diagnostics(query),
			context.temp_allocator,
		)
		for diagnostic in project_diagnostics {
			append(
				&diagnostics,
				Analyze_Diagnostic_Output {
					diagnostic = diagnostic,
					fallback_path = project_result.root_path,
				},
			)
		}
	}
	slice.sort_by(diagnostics[:], analyze_diagnostic_output_less)
	for item in diagnostics {
		if !analyze_diagnostic_output_matches_filter(item, path_filter) {
			continue
		}
		if print_semantic_diagnostic(
			item.diagnostic,
			item.fallback_path,
			warnings_as_errors,
			use_color,
			&source_cache,
		) {
			count_errors += 1
		}
	}
	return
}

analyze_diagnostic_output_matches_filter :: proc(
	item: Analyze_Diagnostic_Output,
	path_filter: string,
) -> bool {
	if path_filter == "" {
		return true
	}
	path := analyze_diagnostic_output_path(item)
	return(
		workspace.normalized_uri_path_key(path, context.temp_allocator) ==
		workspace.normalized_uri_path_key(path_filter, context.temp_allocator) \
	)
}

analyze_diagnostic_output_less :: proc(left, right: Analyze_Diagnostic_Output) -> bool {
	return semantic.semantic_diagnostic_less_with_paths(
		left.diagnostic,
		analyze_diagnostic_output_path(left),
		right.diagnostic,
		analyze_diagnostic_output_path(right),
	)
}

analyze_diagnostic_output_path :: proc(item: Analyze_Diagnostic_Output) -> string {
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
) -> bool {
	path := fallback_path
	if diagnostic.file != nil && diagnostic.file.path != "" {
		path = diagnostic.file.path
	}
	source := cached_source(path, source_cache, context.temp_allocator)
	line_starts := build_line_starts(source, context.temp_allocator)
	uri := display_uri(path, context.temp_allocator)
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
	fmt.printf("%s(%d:%d) ", uri, start.line, start.column)
	if color != "" {
		fmt.print(color)
	}
	fmt.print(label)
	if color != "" {
		fmt.print(SGR_RESET)
	}
	fmt.printf(" %v: %s\n", diagnostic.kind, diagnostic.message)
	print_source_highlight(source, line_starts[:], start, end, color)
	fmt.println()
	return had_error
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

print_lex_errors :: proc(errors: []tokenizer.Lex_Error) {
	fmt.printf("lex_errors\t%d\n", len(errors))
	for err, i in errors {
		fmt.printf("lex_error\t%d\t%d\t%d\t%s\n", i, err.range.start, err.range.end, err.message)
	}
}

print_parse_errors :: proc(path, source: string, errors: []parser.Parse_Error) -> bool {
	if len(errors) == 0 {
		return false
	}
	line_starts := build_line_starts(source, context.temp_allocator)
	uri := parse_display_uri(path, context.temp_allocator)
	color := ""
	if terminal.color_enabled && terminal.is_terminal(os.stdout) {
		color = SGR_RED
	}
	for err in errors {
		start := source_position(source, line_starts[:], err.range.start)
		end := source_position(source, line_starts[:], err.range.end)
		fmt.printf("%s(%d:%d) ", uri, start.line, start.column)
		if color != "" {
			fmt.print(color)
		}
		fmt.print("error")
		if color != "" {
			fmt.print(SGR_RESET)
		}
		fmt.printf(" Syntax_Error: %s\n", err.message)
		print_source_highlight(source, line_starts[:], start, end, color)
		fmt.println()
	}
	return true
}

parse_display_uri :: proc(path: string, allocator: mem.Allocator) -> string {
	if abs_path, ok := workspace.absolute_clean_path(path, allocator); ok {
		return display_uri(abs_path, allocator)
	}
	return display_uri(path, allocator)
}

print_node_counts :: proc(root: ^ast.Node, allocator: runtime.Allocator) {
	counts := Node_Counts {
		items = make([dynamic]Node_Count, 0, 64, allocator),
	}
	visitor := ast.Visitor {
		visit = count_visit,
		data  = rawptr(&counts),
	}
	ast.walk(&visitor, root)
	fmt.printf("node_counts\t%d\n", len(counts.items))
	for item in counts.items {
		fmt.printf("node_count\t%s\t%d\n", item.name, item.count)
	}
}

count_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	counts := cast(^Node_Counts)v.data
	count_node(counts, node_type_name(node))
	return v
}

count_node :: proc(counts: ^Node_Counts, name: string) {
	for &item in counts.items {
		if item.name == name {
			item.count += 1
			return
		}
	}
	append(&counts.items, Node_Count{name, 1})
}

print_tree :: proc(root: ^ast.Node) {
	state := Tree_State{}
	visitor := ast.Visitor {
		visit = tree_visit,
		data  = rawptr(&state),
	}
	ast.walk(&visitor, root)
}

tree_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	state := cast(^Tree_State)v.data
	fmt.printf(
		"tree_node\t%d\t%s\t%d\t%d\n",
		state.index,
		node_type_name(node),
		node.range.start,
		node.range.end,
	)
	state.index += 1
	return v
}

node_type_name :: proc(node: ^ast.Node) -> string {
	if node == nil {
		return "nil"
	}
	return fmt.tprintf("%s", typeid_of(type_of(node.derived)))
}
