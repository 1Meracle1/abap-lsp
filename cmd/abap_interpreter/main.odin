package main

import cli "src:cli"
import execution "src:execution"
import ir "src:ir"
import bytecode "src:ir/bytecode"
import runtime "src:runtime"
import "src:semantic"
import stack_trace "src:stack_trace"
import vm "src:vm"
import workspace "src:workspace"

import "core:fmt"
import "core:mem"
import "core:mem/virtual"
import "core:os"
import "core:strconv"
import "core:strings"

print_usage :: proc() {
	fmt.println("abap_interpreter")
	fmt.println("usage: abap_interpreter --help")
	fmt.println(
		"       abap_interpreter run <file-or-folder> [--include <file>...] [--disable-adt-dependency-fetch] [--step-limit N] [--json] [--pretty]",
	)
}

Run_Cli_Options :: struct {
	path:              string,
	include_paths:     [dynamic]string,
	disable_adt_fetch: bool,
	step_limit:        u64,
	json_output:       bool,
	pretty:            bool,
}

Run_Cli_Event :: struct {
	kind:         string `json:"kind"`,
	text:         string `json:"text"`,
	message_type: string `json:"message_type,omitempty"`,
	path:         string `json:"path,omitempty"`,
	range:        [2]int `json:"range"`,
}

Run_Cli_Trap :: struct {
	kind:    string `json:"kind"`,
	message: string `json:"message"`,
	path:    string `json:"path,omitempty"`,
	range:   [2]int `json:"range"`,
}

Run_Cli_Value :: struct {
	scope:      string `json:"scope"`,
	name:       string `json:"name"`,
	kind:       string `json:"kind"`,
	int_value:  i64 `json:"int_value,omitempty"`,
	text_value: string `json:"text_value,omitempty"`,
}

Run_Cli_Report :: struct {
	schema:            string `json:"schema"`,
	version:           int `json:"version"`,
	phase:             string `json:"phase"`,
	status:            string `json:"status"`,
	instruction_count: u64 `json:"instruction_count"`,
	events:            []Run_Cli_Event `json:"events"`,
	trap:              Maybe(Run_Cli_Trap) `json:"trap"`,
	final_values:      []Run_Cli_Value `json:"final_values"`,
}

Runtime_Prepare_Result :: struct {
	ok:      bool,
	message: string,
	module:  bytecode.Module,
}

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
	if command == "run" {
		run_runtime(args, allocator)
		return
	}

	print_usage()
	os.exit(1)
}

run_runtime :: proc(args: []string, allocator: mem.Allocator) {
	assert(
		context.temp_allocator.procedure == virtual.arena_allocator_proc &&
		context.temp_allocator.data != nil,
	)
	temp_arena := virtual.arena_temp_begin(cast(^virtual.Arena)context.temp_allocator.data)
	defer virtual.arena_temp_end(temp_arena)

	options, options_ok := runtime_parse_options(args, context.temp_allocator)
	if !options_ok {
		print_usage()
		os.exit(1)
	}

	context.allocator = allocator
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
	defer execution.pool_destroy(&pool)
	if pool.options.worker_count > 0 {
		execution.pool_start(&pool)
	}

	workspace_flags := workspace.Option_Flags{.Enable_ADT}
	if options.disable_adt_fetch {
		workspace_flags += {.Disable_ADT_Dependency_Fetch}
	}
	result := cli.analyze_path(
		options.path,
		options.include_paths[:],
		&pool,
		workspace.Options{flags = workspace_flags},
		context.allocator,
	)
	if !result.ok {
		fmt.eprintf("error\trun\t%s\n", result.error)
		os.exit(1)
	}
	defer workspace.analysis_result_destroy(&result, context.allocator)

	path_filter := cli.diagnostic_path_filter(options.path, context.temp_allocator)
	if cli.analysis_error_count(&result, path_filter, context.temp_allocator) > 0 {
		cli.print_analysis_diagnostics(&result, false, path_filter)
		os.exit(1)
	}

	target_file_path := runtime_target_file_path(options.path, context.temp_allocator)
	prepared := runtime_prepare_bytecode(&result, target_file_path, context.allocator)
	if !prepared.ok {
		fmt.eprintf("error: run: %s\n", prepared.message)
		os.exit(1)
	}
	defer bytecode.module_destroy(&prepared.module)

	run_options := vm.Run_Options {
		step_limit = options.step_limit,
		io_policy = runtime.io_policy_captured(),
	}
	run := vm.execute_module(&prepared.module, run_options, context.allocator)
	defer vm.run_result_destroy(&run)

	if options.json_output {
		report := runtime_cli_report(&run, context.temp_allocator)
		cli.emit_json(report, options.pretty, context.temp_allocator)
	} else {
		print_runtime_output(&run)
		if run.status == .Trapped {
			print_runtime_trap(&run.trap, options.path)
		}
	}
	if run.status == .Trapped {
		os.exit(1)
	}
}

runtime_parse_options :: proc(args: []string, allocator: mem.Allocator) -> (Run_Cli_Options, bool) {
	options := Run_Cli_Options {
		include_paths = make([dynamic]string, 0, 4, allocator),
	}
	had_path := false
	for i := 2; i < len(args); {
		arg := args[i]
		switch arg {
		case "--include":
			if i + 1 >= len(args) {
				return {}, false
			}
			append(&options.include_paths, args[i + 1])
			i += 2
			continue
		case "--disable-adt-dependency-fetch":
			options.disable_adt_fetch = true
		case "--step-limit":
			if i + 1 >= len(args) {
				return {}, false
			}
			value, ok := strconv.parse_u64(args[i + 1], 10)
			if !ok {
				fmt.eprintf("invalid value for --step-limit: %q\n", args[i + 1])
				return {}, false
			}
			options.step_limit = value
			i += 2
			continue
		case "--json":
			options.json_output = true
		case "--pretty":
			options.pretty = true
		case:
			if strings.has_prefix(arg, "-") || had_path {
				return {}, false
			}
			options.path = arg
			had_path = true
		}
		i += 1
	}
	return options, had_path
}

runtime_prepare_bytecode :: proc(
	result: ^workspace.Analysis_Result,
	target_file_path: string,
	allocator: mem.Allocator,
) -> Runtime_Prepare_Result {
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	if analysis == nil {
		return Runtime_Prepare_Result{message = "semantic analysis produced no snapshot"}
	}

	prepared: Runtime_Prepare_Result
	had_error := false
	entry_count := 0
	for &project_result in analysis.project_results {
		if project_result.project == nil || project_result.checker == nil {
			continue
		}

		lowered := runtime_lower_project_for_target(
			project_result.project,
			project_result.checker,
			target_file_path,
			allocator,
		)
		verify := ir.verify_module(&lowered.module, allocator)
		if !verify.ok {
			cli.print_ir_verify_diagnostics(project_result.root_path, verify.diagnostics[:])
			had_error = true
			ir.verify_result_destroy(&verify)
			ir.lower_result_destroy(&lowered)
			continue
		}
		ir.verify_result_destroy(&verify)

		emitted := bytecode.lower_module(&lowered.module, allocator)
		if !emitted.ok {
			cli.print_bytecode_lower_error(project_result.root_path, &emitted)
			had_error = true
			bytecode.module_destroy(&emitted.module)
			ir.lower_result_destroy(&lowered)
			continue
		}

		entry_count += len(emitted.module.entries)
		if entry_count > 1 {
			bytecode.module_destroy(&emitted.module)
			ir.lower_result_destroy(&lowered)
			if prepared.ok {
				bytecode.module_destroy(&prepared.module)
			}
			return Runtime_Prepare_Result {
				message = "runtime requires exactly one executable bytecode entry",
			}
		}
		if len(emitted.module.entries) == 1 {
			prepared.ok = true
			prepared.module = emitted.module
		} else {
			bytecode.module_destroy(&emitted.module)
		}
		ir.lower_result_destroy(&lowered)
	}

	if had_error {
		if prepared.ok {
			bytecode.module_destroy(&prepared.module)
		}
		return Runtime_Prepare_Result{message = "bytecode preparation failed"}
	}
	if !prepared.ok {
		return Runtime_Prepare_Result{message = "semantic analysis produced no executable entry"}
	}
	return prepared
}

runtime_target_file_path :: proc(path: string, allocator: mem.Allocator) -> string {
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

runtime_lower_project_for_target :: proc(
	project: ^semantic.Project,
	checker: ^semantic.Checker,
	target_file_path: string,
	allocator: mem.Allocator,
) -> ir.Lower_Result {
	if target_file_path == "" {
		return ir.lower_project(project, checker, allocator)
	}
	result := ir.Lower_Result {
		module = ir.module_make(allocator),
		diagnostics = make([dynamic]ir.Lower_Diagnostic, 0, 4, allocator),
		ok = true,
	}
	query := semantic.semantic_query(project, checker)
	for file in semantic.semantic_query_files(query) {
		if runtime_same_path(file.path, target_file_path) {
			ir.lower_file(&result.module, project, checker, file)
		}
	}
	return result
}

runtime_same_path :: proc(a, b: string) -> bool {
	return(
		workspace.normalized_uri_path_key(a, context.temp_allocator) ==
		workspace.normalized_uri_path_key(b, context.temp_allocator) \
	)
}

print_runtime_output :: proc(result: ^vm.Run_Result) {
	for event in result.events {
		switch event.kind {
		case .Write, .Message:
			fmt.println(event.text)
		}
	}
}

print_runtime_trap :: proc(trap: ^runtime.Trap, fallback_path: string) {
	path := runtime_source_path(trap.source, fallback_path)
	label := runtime_trap_kind_text(trap.kind)
	if trap.source.range.end > trap.source.range.start && path != "" {
		source_cache := make([dynamic]cli.Source_Cache_Entry, 0, 1, context.temp_allocator)
		source := cli.cached_source(path, &source_cache, context.temp_allocator)
		line_starts := cli.build_line_starts(source, context.temp_allocator)
		start := cli.source_position(source, line_starts[:], trap.source.range.start)
		end := cli.source_position(source, line_starts[:], trap.source.range.end)
		uri := cli.display_uri(path, context.temp_allocator)
		fmt.eprintf("%s(%d:%d) error RUNTIME_%s: %s\n", uri, start.line, start.column, label, trap.message)
		cli.print_source_highlight_stderr(source, line_starts[:], start, end)
		return
	}
	fmt.eprintf("error RUNTIME_%s: %s\n", label, trap.message)
}

runtime_cli_report :: proc(result: ^vm.Run_Result, allocator: mem.Allocator) -> Run_Cli_Report {
	events := make([]Run_Cli_Event, len(result.events), allocator)
	for event, i in result.events {
		events[i] = Run_Cli_Event {
			kind = runtime_event_kind_text(event.kind),
			text = event.text,
			message_type = event.message_type,
			path = runtime_source_path(event.source, ""),
			range = [2]int{event.source.range.start, event.source.range.end},
		}
	}
	values := make([]Run_Cli_Value, len(result.final_values), allocator)
	for value, i in result.final_values {
		values[i] = Run_Cli_Value {
			scope = value.scope,
			name = value.name,
			kind = runtime_value_kind_text(runtime.value_kind(value.value)),
			int_value = runtime.value_int(value.value),
			text_value = runtime.value_text(value.value),
		}
	}

	trap := Maybe(Run_Cli_Trap)(nil)
	if result.status == .Trapped {
		trap = Run_Cli_Trap {
			kind = runtime_trap_kind_text(result.trap.kind),
			message = result.trap.message,
			path = runtime_source_path(result.trap.source, ""),
			range = [2]int{result.trap.source.range.start, result.trap.source.range.end},
		}
	}
	return Run_Cli_Report {
		schema = "abap-lsp.runtime-run",
		version = 1,
		phase = "run",
		status = runtime_status_text(result.status),
		instruction_count = result.instruction_count,
		events = events,
		trap = trap,
		final_values = values,
	}
}

runtime_source_path :: proc(source: ir.Source_Loc, fallback_path: string = "") -> string {
	if source.file != nil && source.file.path != "" {
		return source.file.path
	}
	return fallback_path
}

runtime_status_text :: proc(status: runtime.Run_Status) -> string {
	switch status {
	case .Completed:
		return "completed"
	case .Trapped:
		return "trapped"
	}
	return "trapped"
}

runtime_trap_kind_text :: proc(kind: runtime.Trap_Kind) -> string {
	switch kind {
	case .None:
		return "none"
	case .Invalid_Module:
		return "invalid-module"
	case .Invalid_Function:
		return "invalid-function"
	case .Invalid_Instruction:
		return "invalid-instruction"
	case .Unsupported:
		return "unsupported"
	case .Type:
		return "type"
	case .Divide_By_Zero:
		return "divide-by-zero"
	case .Step_Limit:
		return "step-limit"
	}
	return "unsupported"
}

runtime_event_kind_text :: proc(kind: runtime.IO_Event_Kind) -> string {
	switch kind {
	case .Write:
		return "write"
	case .Message:
		return "message"
	}
	return "write"
}

runtime_value_kind_text :: proc(kind: runtime.Value_Kind) -> string {
	switch kind {
	case .Initial:
		return "initial"
	case .World:
		return "world"
	case .Integer:
		return "integer"
	case .String:
		return "string"
	case .Structure:
		return "structure"
	case .Object:
		return "object"
	case .Predicate:
		return "predicate"
	case .Table:
		return "table"
	case .Table_Iterator:
		return "table-iterator"
	}
	return "initial"
}
