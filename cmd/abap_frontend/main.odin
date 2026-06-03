package main

import "src:ast"
import execution "src:execution"
import "src:parser"
import semantic_analyze "src:semantic/analyze"
import stack_trace "src:stack_trace"
import "src:tokenizer"
import workspace "src:workspace"

import "base:runtime"
import "core:fmt"
import "core:mem"
import "core:mem/virtual"
import "core:os"
import "core:slice"
import "core:strings"
import "core:terminal"
import ansi "core:terminal/ansi"

DEPENDENCY_FETCH_TRACE :: #config(ABAP_FRONTEND_TRACE_ADT_FETCH, false)

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

Tree_State :: struct {
	index: int,
}

SGR_RESET  :: ansi.CSI + ansi.RESET + ansi.SGR
SGR_RED    :: ansi.CSI + ansi.FG_RED + ansi.SGR
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
	fmt.println("       abap_frontend analyze <file-or-folder> [--include <file>...] [--warnings-as-errors]")
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
		}
	}
}

run_analyze :: proc(args: []string, allocator: mem.Allocator) {
	assert(context.temp_allocator.procedure == virtual.arena_allocator_proc && context.temp_allocator.data != nil)
	temp_arena := virtual.arena_temp_begin(cast(^virtual.Arena)context.temp_allocator.data)
	defer virtual.arena_temp_end(temp_arena)

	context.allocator = allocator
	when DEPENDENCY_FETCH_TRACE {
		tracker: mem.Tracking_Allocator
		mem.tracking_allocator_init(&tracker, allocator, allocator)
		tracked_allocator := mem.tracking_allocator(&tracker)
		context.allocator = tracked_allocator
	}

	target_path := args[2]

	warnings_as_errors := false
	include_paths := make([dynamic]string, 0, 4, context.temp_allocator)
	for i := 3; i < len(args); {
		if args[i] == "--warnings-as-errors" {
			warnings_as_errors = true
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
		workspace.Options{pool = &pool, enable_adt = true},
		context.allocator,
	)
	if !result.ok {
		fmt.printf("error\tanalyze\t%s\n", result.error)
		execution.pool_destroy(&pool)
		os.exit(1)
	}
	when DEPENDENCY_FETCH_TRACE {
		print_analyze_counts(&result.project)
	}
	had_error := print_analyze_diagnostics(&result.project, warnings_as_errors)
	execution.pool_destroy(&pool)
	when DEPENDENCY_FETCH_TRACE {
		print_analyze_memory_report(&tracker)
		mem.tracking_allocator_destroy(&tracker)
	}
	if had_error {
		os.exit(1)
	}
}

analyze_cli_path :: proc(
	path: string,
	include_paths: []string,
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
		opened, workspace_ok, workspace_error := workspace.open_workspace(
			abs_path,
			options,
			allocator,
		)
		if !workspace_ok {
			return workspace.Analysis_Result{ok = false, error = workspace_error}
		}
		defer workspace.workspace_destroy(&opened, allocator)
		return workspace.analyze_workspace(&opened, include_paths, options, allocator)
	}

	opened, workspace_ok, workspace_error := workspace.open_standalone_workspace(
		os.dir(abs_path),
		options,
		allocator,
	)
	if !workspace_ok {
		return workspace.Analysis_Result{ok = false, error = workspace_error}
	}
	defer workspace.workspace_destroy(&opened, allocator)
	return workspace.analyze_path(&opened, abs_path, include_paths, options, allocator)
}

print_analyze_memory_report :: proc(tracker: ^mem.Tracking_Allocator) {
	assert(context.temp_allocator.procedure == virtual.arena_allocator_proc && context.temp_allocator.data != nil)
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
		"memory\tused: %d KB\tpeak: %d KB\ttotal_allocated: %d KB\tallocations\t%d\tlocations\t%d\n",
		tracker.current_memory_allocated / mem.Kilobyte,
		tracker.peak_memory_allocated / mem.Kilobyte,
		tracker.total_memory_allocated / mem.Kilobyte,
		tracker.total_allocation_count,
		len(totals),
	)
	for total in totals {
		fmt.printf(
			"memory_location\t%d KB\tallocations: %d\t%s(%d:%d)\tproc: %s\n",
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

print_analyze_counts :: proc(project: ^semantic_analyze.Project_Analysis) {
	symbols, scopes, references, structures, diagnostics, include_edges, unresolved_refs: int
	for unit in project.units {
		symbols += len(unit.symbols)
		scopes += len(unit.scopes)
		references += len(unit.references)
		structures += len(unit.structures)
		diagnostics += len(unit.diagnostics)
		include_edges += len(unit.include_edges)
		for ref in unit.references {
			if !ref.has_resolution {
				unresolved_refs += 1
			}
		}
	}
	fmt.printf(
		"counts\tunits\t%d\tsymbols\t%d\tscopes\t%d\treferences\t%d\tstructures\t%d\tdiagnostics\t%d\tinclude_edges\t%d\tunresolved_refs\t%d\n",
		len(project.units),
		symbols,
		scopes,
		references,
		structures,
		diagnostics,
		include_edges,
		unresolved_refs,
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

print_analyze_diagnostics :: proc(
	project: ^semantic_analyze.Project_Analysis,
	warnings_as_errors: bool,
) -> bool {
	had_error := false
	use_color := terminal.color_enabled && terminal.is_terminal(os.stdout)
	for unit in project.units {
		assert(context.temp_allocator.procedure == virtual.arena_allocator_proc && context.temp_allocator.data != nil)
		temp_arena := virtual.arena_temp_begin(cast(^virtual.Arena)context.temp_allocator.data)
		defer virtual.arena_temp_end(temp_arena)

		line_starts := build_line_starts(unit.source, context.temp_allocator)
		uri := display_uri(unit.uri, context.temp_allocator)
		for diagnostic in unit.diagnostics {
			warning := diagnostic_is_warning(diagnostic.kind) && !warnings_as_errors
			color := ""
			label := "error"
			if warning {
				label = "warning"
			} else {
				had_error = true
			}
			if use_color {
				color = SGR_YELLOW if warning else SGR_RED
			}
			start := source_position(unit.source, line_starts[:], diagnostic.range.start)
			end := source_position(unit.source, line_starts[:], diagnostic.range.end)
			line_text := source_line_text(unit.source, line_starts[:], start.line)
			width := end.column - start.column
			if end.line != start.line {
				width = len(line_text) - start.column + 2
			}
			fmt.printf("%s(%d:%d) ", uri, start.line, start.column)
			if color != "" {
				fmt.print(color)
			}
			fmt.print(label)
			if color != "" {
				fmt.print(SGR_RESET)
			}
			fmt.printf(" %v: %s\n", diagnostic.kind, diagnostic.message)
			fmt.printf("    %s\n", line_text)
			print_caret_line(start.column, width, color)
			fmt.println()
		}
	}
	return had_error
}

diagnostic_is_warning :: proc(kind: semantic_analyze.Diagnostic_Kind) -> bool {
	#partial switch kind {
	case .Shadowed_Symbol,
	     .Unreachable_Code:
		return true
	}
	return false
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
		line_text := source_line_text(source, line_starts[:], start.line)
		width := end.column - start.column
		if end.line != start.line {
			width = len(line_text) - start.column + 2
		}
		fmt.printf("%s(%d:%d) ", uri, start.line, start.column)
		if color != "" {
			fmt.print(color)
		}
		fmt.print("error")
		if color != "" {
			fmt.print(SGR_RESET)
		}
		fmt.printf(" Syntax_Error: %s\n", err.message)
		fmt.printf("    %s\n", line_text)
		print_caret_line(start.column, width, color)
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
	#partial switch _ in node.derived {
	case ^ast.File:
		return "File"
	case ^ast.Bad_Expr:
		return "Bad_Expr"
	case ^ast.Char_String_Template_Expr:
		return "Char_String_Template_Expr"
	case ^ast.Template_Literal_Expr:
		return "Template_Literal_Expr"
	case ^ast.Template_Interpolation_Expr:
		return "Template_Interpolation_Expr"
	case ^ast.Template_Expr:
		return "Template_Expr"
	case ^ast.Template_Format_Spec_Expr:
		return "Template_Format_Spec_Expr"
	case ^ast.Binary_Expr:
		return "Binary_Expr"
	case ^ast.Unary_Expr:
		return "Unary_Expr"
	case ^ast.Paren_Expr:
		return "Paren_Expr"
	case ^ast.Ident_Expr:
		return "Ident_Expr"
	case ^ast.Literal_Expr:
		return "Literal_Expr"
	case ^ast.Type_Ref_Expr:
		return "Type_Ref_Expr"
	case ^ast.Host_Expr:
		return "Host_Expr"
	case ^ast.Table_Expr:
		return "Table_Expr"
	case ^ast.Selector_Expr:
		return "Selector_Expr"
	case ^ast.Substring_Expr:
		return "Substring_Expr"
	case ^ast.Call_Expr:
		return "Call_Expr"
	case ^ast.Call_Arg_List_Expr:
		return "Call_Arg_List_Expr"
	case ^ast.Call_Arg_Section_Expr:
		return "Call_Arg_Section_Expr"
	case ^ast.Call_Named_Arg_Expr:
		return "Call_Named_Arg_Expr"
	case ^ast.Call_Positional_Arg_Expr:
		return "Call_Positional_Arg_Expr"
	case ^ast.Constructor_Expr:
		return "Constructor_Expr"
	case ^ast.Is_Predicate_Expr:
		return "Is_Predicate_Expr"
	case ^ast.Instance_Of_Predicate_Expr:
		return "Instance_Of_Predicate_Expr"
	case ^ast.Between_Expr:
		return "Between_Expr"
	case ^ast.Sql_Case_When_Expr:
		return "Sql_Case_When_Expr"
	case ^ast.Sql_Case_Expr:
		return "Sql_Case_Expr"
	case ^ast.Let_Expr:
		return "Let_Expr"
	case ^ast.Constructor_Let_Binding_Expr:
		return "Constructor_Let_Binding_Expr"
	case ^ast.Constructor_When_Clause_Expr:
		return "Constructor_When_Clause_Expr"
	case ^ast.Constructor_Else_Clause_Expr:
		return "Constructor_Else_Clause_Expr"
	case ^ast.Constructor_For_Clause_Expr:
		return "Constructor_For_Clause_Expr"
	case ^ast.Constructor_Where_Clause_Expr:
		return "Constructor_Where_Clause_Expr"
	case ^ast.Constructor_Init_Clause_Expr:
		return "Constructor_Init_Clause_Expr"
	case ^ast.Constructor_Next_Clause_Expr:
		return "Constructor_Next_Clause_Expr"
	case ^ast.Constructor_Named_Assignment_Expr:
		return "Constructor_Named_Assignment_Expr"
	case ^ast.Constructor_Base_Clause_Expr:
		return "Constructor_Base_Clause_Expr"
	case ^ast.Constructor_Lines_Of_Clause_Expr:
		return "Constructor_Lines_Of_Clause_Expr"
	case ^ast.Constructor_Optional_Expr:
		return "Constructor_Optional_Expr"
	case ^ast.Constructor_Corresponding_Mapping_Clause_Expr:
		return "Constructor_Corresponding_Mapping_Clause_Expr"
	case ^ast.Constructor_Corresponding_Mapping_Assignment_Expr:
		return "Constructor_Corresponding_Mapping_Assignment_Expr"
	case ^ast.Constructor_Corresponding_Except_Clause_Expr:
		return "Constructor_Corresponding_Except_Clause_Expr"
	case ^ast.Data_Inline_Name_Expr:
		return "Data_Inline_Name_Expr"
	case ^ast.Field_Symbol_Inline_Name_Expr:
		return "Field_Symbol_Inline_Name_Expr"
	case ^ast.Data_Decl:
		return "Data_Decl"
	case ^ast.Data_Chained_Decl:
		return "Data_Chained_Decl"
	case ^ast.Data_Inline_Decl:
		return "Data_Inline_Decl"
	case ^ast.Types_Decl:
		return "Types_Decl"
	case ^ast.Constants_Decl:
		return "Constants_Decl"
	case ^ast.Field_Symbols_Decl:
		return "Field_Symbols_Decl"
	case ^ast.Statics_Decl:
		return "Statics_Decl"
	case ^ast.Tables_Decl:
		return "Tables_Decl"
	case ^ast.Ranges_Decl:
		return "Ranges_Decl"
	case ^ast.Parameters_Decl:
		return "Parameters_Decl"
	case ^ast.Select_Options_Decl:
		return "Select_Options_Decl"
	case ^ast.Controls_Decl:
		return "Controls_Decl"
	case ^ast.Class_Data_Decl:
		return "Class_Data_Decl"
	case ^ast.Type_Pools_Decl:
		return "Type_Pools_Decl"
	case ^ast.Function_Pool_Decl:
		return "Function_Pool_Decl"
	case ^ast.Include_Stmt:
		return "Include_Stmt"
	case ^ast.Assign_Stmt:
		return "Assign_Stmt"
	case ^ast.Downcast_Assign_Stmt:
		return "Downcast_Assign_Stmt"
	case ^ast.Expr_Stmt:
		return "Expr_Stmt"
	case ^ast.Clear_Stmt:
		return "Clear_Stmt"
	case ^ast.Refresh_Stmt:
		return "Refresh_Stmt"
	case ^ast.Free_Stmt:
		return "Free_Stmt"
	case ^ast.Unassign_Stmt:
		return "Unassign_Stmt"
	case ^ast.Move_Stmt:
		return "Move_Stmt"
	case ^ast.Add_Stmt:
		return "Add_Stmt"
	case ^ast.Subtract_Stmt:
		return "Subtract_Stmt"
	case ^ast.Multiply_Stmt:
		return "Multiply_Stmt"
	case ^ast.Divide_Stmt:
		return "Divide_Stmt"
	case ^ast.Compute_Stmt:
		return "Compute_Stmt"
	case ^ast.Concatenate_Stmt:
		return "Concatenate_Stmt"
	case ^ast.Split_Stmt:
		return "Split_Stmt"
	case ^ast.Condense_Stmt:
		return "Condense_Stmt"
	case ^ast.Replace_Stmt:
		return "Replace_Stmt"
	case ^ast.Translate_Stmt:
		return "Translate_Stmt"
	case ^ast.Shift_Stmt:
		return "Shift_Stmt"
	case ^ast.Find_Stmt:
		return "Find_Stmt"
	case ^ast.Search_Stmt:
		return "Search_Stmt"
	case ^ast.Perform_Stmt:
		return "Perform_Stmt"
	case ^ast.Call_Stmt:
		return "Call_Stmt"
	case ^ast.Submit_Stmt:
		return "Submit_Stmt"
	case ^ast.Message_Stmt:
		return "Message_Stmt"
	case ^ast.Write_Stmt:
		return "Write_Stmt"
	case ^ast.Write_To_Stmt:
		return "Write_To_Stmt"
	case ^ast.Assert_Stmt:
		return "Assert_Stmt"
	case ^ast.Check_Stmt:
		return "Check_Stmt"
	case ^ast.Flow_Stmt:
		return "Flow_Stmt"
	case ^ast.Transaction_Stmt:
		return "Transaction_Stmt"
	case ^ast.Describe_Stmt:
		return "Describe_Stmt"
	case ^ast.Runtime_Stmt:
		return "Runtime_Stmt"
	case ^ast.Raise_Stmt:
		return "Raise_Stmt"
	case ^ast.Authority_Check_Stmt:
		return "Authority_Check_Stmt"
	case ^ast.Field_Groups_Stmt:
		return "Field_Groups_Stmt"
	case ^ast.Insert_Dummy_Stmt:
		return "Insert_Dummy_Stmt"
	case ^ast.Field_Stmt:
		return "Field_Stmt"
	case ^ast.Assign_Field_Stmt:
		return "Assign_Field_Stmt"
	case ^ast.Create_Object_Stmt:
		return "Create_Object_Stmt"
	case ^ast.Text_Transform_Stmt:
		return "Text_Transform_Stmt"
	case ^ast.List_Control_Stmt:
		return "List_Control_Stmt"
	case ^ast.Line_Stmt:
		return "Line_Stmt"
	case ^ast.Macro_Def_Stmt:
		return "Macro_Def_Stmt"
	case ^ast.Macro_Call_Stmt:
		return "Macro_Call_Stmt"
	case ^ast.Oop_Simple_Stmt:
		return "Oop_Simple_Stmt"
	case ^ast.If_Stmt:
		return "If_Stmt"
	case ^ast.Case_Stmt:
		return "Case_Stmt"
	case ^ast.While_Stmt:
		return "While_Stmt"
	case ^ast.Do_Stmt:
		return "Do_Stmt"
	case ^ast.Loop_Stmt:
		return "Loop_Stmt"
	case ^ast.At_Stmt:
		return "At_Stmt"
	case ^ast.Try_Stmt:
		return "Try_Stmt"
	case ^ast.Class_Decl:
		return "Class_Decl"
	case ^ast.Interface_Decl:
		return "Interface_Decl"
	case ^ast.Method_Decl:
		return "Method_Decl"
	case ^ast.Form_Decl:
		return "Form_Decl"
	case ^ast.Function_Decl:
		return "Function_Decl"
	case ^ast.Module_Decl:
		return "Module_Decl"
	case ^ast.Event_Block_Stmt:
		return "Event_Block_Stmt"
	case ^ast.Enhancement_Stmt:
		return "Enhancement_Stmt"
	case ^ast.Enhancement_Section_Stmt:
		return "Enhancement_Section_Stmt"
	case ^ast.Test_Seam_Stmt:
		return "Test_Seam_Stmt"
	case ^ast.Test_Injection_Stmt:
		return "Test_Injection_Stmt"
	case ^ast.Select_Stmt:
		return "Select_Stmt"
	case ^ast.Open_Cursor_Stmt:
		return "Open_Cursor_Stmt"
	case ^ast.Fetch_Stmt:
		return "Fetch_Stmt"
	case ^ast.Close_Cursor_Stmt:
		return "Close_Cursor_Stmt"
	case ^ast.Insert_Stmt:
		return "Insert_Stmt"
	case ^ast.Append_Stmt:
		return "Append_Stmt"
	case ^ast.Modify_Stmt:
		return "Modify_Stmt"
	case ^ast.Sort_Stmt:
		return "Sort_Stmt"
	case ^ast.Update_Stmt:
		return "Update_Stmt"
	case ^ast.Delete_Stmt:
		return "Delete_Stmt"
	case ^ast.Read_Table_Stmt:
		return "Read_Table_Stmt"
	case ^ast.Dataset_Stmt:
		return "Dataset_Stmt"
	case ^ast.Report_Stmt:
		return "Report_Stmt"
	case ^ast.Textpool_Stmt:
		return "Textpool_Stmt"
	case ^ast.Exec_Sql_Stmt:
		return "Exec_Sql_Stmt"
	case ^ast.Generate_Stmt:
		return "Generate_Stmt"
	case ^ast.Invalid_Stmt:
		return "Invalid_Stmt"
	}
	return "Unknown"
}
