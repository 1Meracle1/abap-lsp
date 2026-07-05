package main

import "src:ast"
import cli "src:cli"
import execution "src:execution"
import ir "src:ir"
import bytecode "src:ir/bytecode"
import lints "src:lints"
import "src:parser"
import "src:semantic"
import stack_trace "src:stack_trace"
import "src:tokenizer"
import trace "src:trace"
import workspace "src:workspace"

import base_runtime "base:runtime"
import "core:container/xar"
import "core:fmt"
import "core:mem"
import "core:mem/virtual"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"
import "core:terminal"
import "core:time"

print_usage :: proc() {
	fmt.println("abap_frontend")
	fmt.println("usage: abap_frontend --help")
	fmt.println("       abap_frontend parse <file>")
	fmt.println("       abap_frontend tree <file>")
	fmt.println(
		"       abap_frontend ir <file-or-folder> [--include <file>...] [--disable-adt-dependency-fetch]",
	)
	fmt.println(
		"       abap_frontend bytecode <file-or-folder> [--include <file>...] [--raw] [--source] [--disable-adt-dependency-fetch]",
	)
	fmt.println(
		"       abap_frontend analyze <file-or-folder> [--include <file>...] [--warnings-as-errors] [--enable-dependency-diagnostics] [--enable-lints] [--disable-adt-dependency-fetch]",
	)
	fmt.println(
		"       abap_frontend perf-edit <workspace-folder> <file> [--iterations <count>] [--enable-lints]",
	)
	fmt.println(
		"       abap_frontend lint [--json] [--pretty] [--with-project] [--all-files] [--show-suppressed] [--fail-on-warnings] [--disable-adt-dependency-fetch] [file-or-folder]",
	)
}

Node_Count :: struct {
	name:  string,
	count: int,
}

Node_Counts :: struct {
	items: [dynamic]Node_Count,
}

Allocation_Location_Total :: struct {
	location: base_runtime.Source_Code_Location,
	bytes:    i64,
	count:    int,
}

Analyze_Lint_Diagnostic_Output :: struct {
	diagnostic:    lints.Diagnostic,
	fallback_path: string,
}

Lint_Cli_Options :: struct {
	json_output:       bool,
	pretty:            bool,
	with_project:      bool,
	all_files:         bool,
	show_suppressed:   bool,
	fail_on_warnings:  bool,
	disable_adt_fetch: bool,
	path:              string,
}

Perf_Edit_Options :: struct {
	workspace_path: string,
	file_path:      string,
	iterations:     int,
	enable_lints:   bool,
}

Lint_Cli_Target :: struct {
	uri:           string `json:"uri"`,
	path:          Maybe(string) `json:"path"`,
	object_name:   Maybe(string) `json:"object_name"`,
	is_dependency: bool `json:"is_dependency"`,
}

Lint_Cli_Workspace :: struct {
	with_project:          bool `json:"with_project"`,
	all_files:             bool `json:"all_files,omitempty"`,
	root_uri:              Maybe(string) `json:"root_uri"`,
	manifest_present:      bool `json:"manifest_present"`,
	project_unit_count:    Maybe(int) `json:"project_unit_count"`,
	dependency_unit_count: Maybe(int) `json:"dependency_unit_count"`,
	editable_file_count:   Maybe(int) `json:"editable_file_count,omitempty"`,
}

Lint_Cli_Suppression :: struct {
	kind:  string `json:"kind"`,
	range: [2]int `json:"range"`,
	token: string `json:"token"`,
}

Lint_Cli_Finding :: struct {
	uri:         string `json:"uri"`,
	lint_id:     string `json:"lint_id"`,
	level:       string `json:"level"`,
	group:       string `json:"group"`,
	origin:      string `json:"origin"`,
	message:     string `json:"message"`,
	range:       [2]int `json:"range"`,
	suppressed:  bool `json:"suppressed"`,
	suppression: Maybe(Lint_Cli_Suppression) `json:"suppression"`,
}

Lint_Cli_Hard_Error :: struct {
	uri:     string `json:"uri,omitempty"`,
	path:    string `json:"path,omitempty"`,
	phase:   string `json:"phase"`,
	message: string `json:"message"`,
	range:   [2]int `json:"range"`,
}

Lint_Cli_Level_Summary :: struct {
	allow: int `json:"allow"`,
	info:  int `json:"info"`,
	warn:  int `json:"warn"`,
	deny:  int `json:"deny"`,
}

Lint_Cli_Group_Summary :: struct {
	correctness:   int `json:"correctness"`,
	performance:   int `json:"performance"`,
	security:      int `json:"security"`,
	style:         int `json:"style"`,
	modernization: int `json:"modernization"`,
	package_count: int `json:"package"`,
	experimental:  int `json:"experimental"`,
}

Lint_Cli_Summary :: struct {
	total:            int `json:"total"`,
	suppressed:       int `json:"suppressed"`,
	by_level:         Lint_Cli_Level_Summary `json:"by_level"`,
	by_group:         Lint_Cli_Group_Summary `json:"by_group"`,
	file_count:       int `json:"file_count,omitempty"`,
	hard_error_count: int `json:"hard_error_count,omitempty"`,
}

Lint_Cli_File :: struct {
	uri:              string `json:"uri"`,
	path:             Maybe(string) `json:"path"`,
	object_name:      Maybe(string) `json:"object_name"`,
	is_dependency:    bool `json:"is_dependency"`,
	finding_count:    int `json:"finding_count"`,
	hard_error_count: int `json:"hard_error_count"`,
	summary:          Lint_Cli_Summary `json:"summary"`,
}

Lint_Cli_Report :: struct {
	schema:      string `json:"schema"`,
	version:     int `json:"version"`,
	phase:       string `json:"phase"`,
	target:      Lint_Cli_Target `json:"target"`,
	workspace:   Lint_Cli_Workspace `json:"workspace"`,
	files:       []Lint_Cli_File `json:"files,omitempty"`,
	findings:    []Lint_Cli_Finding `json:"findings"`,
	hard_errors: []Lint_Cli_Hard_Error `json:"hard_errors"`,
	summary:     Lint_Cli_Summary `json:"summary"`,
}

Tree_State :: struct {
	index: int,
}

Ir_Cli_Output :: enum {
	Module,
	Bytecode,
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
	if command == "analyze" {
		if len(args) < 3 {
			print_usage()
			os.exit(1)
		}
		run_analyze(args, allocator)
		return
	}
	if command == "ir" {
		run_ir(args, allocator, .Module)
		return
	}
	if command == "bytecode" {
		run_ir(args, allocator, .Bytecode)
		return
	}
	if command == "perf-edit" {
		run_perf_edit(args, allocator)
		return
	}
	if command == "lint" {
		run_lint(args, allocator)
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
		} else if args[i] == "--enable-lints" {
			workspace_flags += {.Enable_Lints}
			i += 1
		} else if args[i] == "--disable-adt-dependency-fetch" {
			workspace_flags += {.Disable_ADT_Dependency_Fetch}
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

	result := cli.analyze_path(
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
	diagnostic_path_filter := cli.diagnostic_path_filter(target_path, context.temp_allocator)
	count_errors := cli.print_analysis_diagnostics(&result, warnings_as_errors, diagnostic_path_filter)
	if .Enable_Lints in workspace_flags {
		lint_analysis := run_analyze_lints(&result, &pool)
		count_errors += print_lint_diagnostics(
			&lint_analysis,
			warnings_as_errors,
			diagnostic_path_filter,
		)
		lints.analysis_destroy(&lint_analysis)
	}
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

run_lint :: proc(args: []string, allocator: mem.Allocator) {
	assert(
		context.temp_allocator.procedure == virtual.arena_allocator_proc &&
		context.temp_allocator.data != nil,
	)
	temp_arena := virtual.arena_temp_begin(cast(^virtual.Arena)context.temp_allocator.data)
	defer virtual.arena_temp_end(temp_arena)

	options, options_ok := lint_parse_options(args)
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
	if pool.options.worker_count > 0 {
		execution.pool_start(&pool)
	}

	result, root_path, target_path := lint_analyze_cli_path(&options, &pool, context.allocator)
	if !result.ok {
		fmt.printf("error\tlint\t%s\n", result.error)
		execution.pool_destroy(&pool)
		os.exit(1)
	}
	defer workspace.analysis_result_destroy(&result, context.allocator)
	defer execution.pool_destroy(&pool)

	path_filter :=
		"" if options.all_files else cli.diagnostic_path_filter(target_path, context.temp_allocator)
	hard_errors := lint_cli_hard_errors(&result, path_filter, context.temp_allocator)
	policy := result.lint_policy
	if options.show_suppressed {
		policy.report_suppressed = true
	}

	lint_analysis: lints.Analysis
	have_lint_analysis := false
	if len(hard_errors) == 0 {
		lint_analysis = run_analyze_lints(&result, &pool, &policy)
		have_lint_analysis = true
	}
	if have_lint_analysis {
		defer lints.analysis_destroy(&lint_analysis)
	}

	if options.json_output {
		report := lint_cli_report(
			&options,
			&result,
			&lint_analysis,
			hard_errors[:],
			target_path,
			root_path,
			path_filter,
			context.temp_allocator,
		)
		cli.emit_json(report, options.pretty, context.temp_allocator)
	} else if len(hard_errors) > 0 {
		cli.print_analysis_diagnostics(&result, false, path_filter)
	} else {
		print_lint_diagnostics(&lint_analysis, options.fail_on_warnings, path_filter)
	}

	if len(hard_errors) > 0 ||
	   (have_lint_analysis &&
			   lint_cli_findings_should_fail(
				   &lint_analysis,
				   options.fail_on_warnings,
				   path_filter,
			   )) {
		os.exit(1)
	}
}

run_ir :: proc(args: []string, allocator: mem.Allocator, output: Ir_Cli_Output) {
	assert(
		context.temp_allocator.procedure == virtual.arena_allocator_proc &&
		context.temp_allocator.data != nil,
	)
	temp_arena := virtual.arena_temp_begin(cast(^virtual.Arena)context.temp_allocator.data)
	defer virtual.arena_temp_end(temp_arena)

	if len(args) < 3 {
		print_usage()
		os.exit(1)
	}

	target_path := args[2]
	workspace_flags := workspace.Option_Flags{.Enable_ADT}
	include_paths := make([dynamic]string, 0, 4, context.temp_allocator)
	bytecode_options := bytecode.Print_Options{}
	for i := 3; i < len(args); {
		if args[i] == "--include" && i + 1 < len(args) {
			append(&include_paths, args[i + 1])
			i += 2
		} else if args[i] == "--disable-adt-dependency-fetch" {
			workspace_flags += {.Disable_ADT_Dependency_Fetch}
			i += 1
		} else if output == .Bytecode && args[i] == "--raw" {
			bytecode_options.raw = true
			i += 1
		} else if output == .Bytecode && args[i] == "--source" {
			bytecode_options.show_source = true
			i += 1
		} else {
			print_usage()
			os.exit(1)
		}
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

	result := cli.analyze_path(
		target_path,
		include_paths[:],
		&pool,
		workspace.Options{flags = workspace_flags},
		context.allocator,
	)
	if !result.ok {
		fmt.printf("error\t%s\t%s\n", ir_cli_output_name(output), result.error)
		os.exit(1)
	}
	defer workspace.analysis_result_destroy(&result, context.allocator)

	print_ok := false
	switch output {
	case .Module:
		print_ok = print_ir_for_analysis(&result, context.allocator)
	case .Bytecode:
		print_ok = print_emitted_bytecode_for_analysis(&result, context.allocator, bytecode_options)
	}
	if !print_ok {
		os.exit(1)
	}
}

ir_cli_output_name :: proc "contextless" (output: Ir_Cli_Output) -> string {
	switch output {
	case .Module:
		return "ir"
	case .Bytecode:
		return "bytecode"
	}
	unreachable()
}

print_ir_for_analysis :: proc(
	result: ^workspace.Analysis_Result,
	allocator: mem.Allocator,
) -> bool {
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	if analysis == nil {
		fmt.eprintln("error: ir: semantic analysis produced no snapshot")
		return false
	}

	printed := false
	ok := true
	for &project_result in analysis.project_results {
		if project_result.project == nil || project_result.checker == nil {
			continue
		}

		lowered := ir.lower_project(project_result.project, project_result.checker, allocator)
		verify := ir.verify_module(&lowered.module, allocator)
		if verify.ok {
			if printed {
				fmt.println()
			}
			text := ir.print_module(&lowered.module, allocator)
			fmt.print(text)
			delete(text, allocator)
			printed = true
		} else {
			cli.print_ir_verify_diagnostics(project_result.root_path, verify.diagnostics[:])
			ok = false
		}
		ir.verify_result_destroy(&verify)
		ir.lower_result_destroy(&lowered)
	}

	if !printed && ok {
		fmt.eprintln("error: ir: semantic analysis produced no lowerable projects")
		return false
	}
	return ok
}

print_emitted_bytecode_for_analysis :: proc(
	result: ^workspace.Analysis_Result,
	allocator: mem.Allocator,
	options: bytecode.Print_Options = {},
) -> bool {
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	if analysis == nil {
		fmt.eprintln("error: bytecode: semantic analysis produced no snapshot")
		return false
	}

	printed := false
	ok := true
	for &project_result in analysis.project_results {
		if project_result.project == nil || project_result.checker == nil {
			continue
		}

		lowered := ir.lower_project(project_result.project, project_result.checker, allocator)
		emitted_bytecode := bytecode.lower_module(&lowered.module, allocator)
		if emitted_bytecode.ok {
			if printed {
				fmt.println()
			}
			text := bytecode.print_module(&emitted_bytecode.module, allocator, options)
			fmt.print(text)
			delete(text, allocator)
			printed = true
		} else {
			cli.print_bytecode_lower_error(project_result.root_path, &emitted_bytecode)
			ok = false
		}
		bytecode.module_destroy(&emitted_bytecode.module)
		ir.lower_result_destroy(&lowered)
	}

	if !printed && ok {
		fmt.eprintln("error: bytecode: semantic analysis produced no lowerable projects")
		return false
	}
	return ok
}

lint_parse_options :: proc(args: []string) -> (Lint_Cli_Options, bool) {
	options := Lint_Cli_Options {
		path = ".",
	}
	had_path := false
	for i := 2; i < len(args); {
		arg := args[i]
		switch arg {
		case "--json":
			options.json_output = true
		case "--pretty":
			options.pretty = true
		case "--with-project":
			options.with_project = true
		case "--all-files":
			options.all_files = true
			options.with_project = true
		case "--show-suppressed":
			options.show_suppressed = true
		case "--fail-on-warnings":
			options.fail_on_warnings = true
		case "--disable-adt-dependency-fetch":
			options.disable_adt_fetch = true
		case:
			if strings.has_prefix(arg, "-") || had_path {
				return {}, false
			}
			options.path = arg
			had_path = true
		}
		i += 1
	}
	return options, true
}

lint_analyze_cli_path :: proc(
	options: ^Lint_Cli_Options,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> (
	result: workspace.Analysis_Result,
	root_path: string,
	target_path: string,
) {
	abs_path, ok := workspace.absolute_clean_path(options.path, allocator)
	if !ok {
		return workspace.Analysis_Result{ok = false, error = "invalid path"}, "", ""
	}
	info, err := os.stat(abs_path, allocator)
	if err != nil {
		return workspace.Analysis_Result{ok = false, error = "invalid path"}, "", abs_path
	}

	workspace_flags := workspace.Option_Flags{.Enable_Lints, .Enable_ADT}
	if options.disable_adt_fetch {
		workspace_flags += {.Disable_ADT_Dependency_Fetch}
	}
	workspace_options := workspace.Options {
		flags = workspace_flags,
	}

	if info.type == .Directory {
		opened, workspace_ok, workspace_error := workspace.open(
			abs_path,
			workspace_options,
			allocator,
		)
		if !workspace_ok {
			return workspace.Analysis_Result{ok = false, error = workspace_error},
				abs_path,
				abs_path
		}
		defer workspace.workspace_destroy(&opened, allocator)
		return workspace.analyze_workspace(&opened, nil, pool, workspace_options, allocator),
			abs_path,
			abs_path
	}

	if options.with_project || options.all_files {
		root := os.dir(abs_path)
		if nearest_root, _, found := workspace.find_nearest_manifest(
			abs_path,
			context.temp_allocator,
		); found {
			root = nearest_root
		}
		opened, workspace_ok, workspace_error := workspace.open(root, workspace_options, allocator)
		if !workspace_ok {
			return workspace.Analysis_Result{ok = false, error = workspace_error}, root, abs_path
		}
		defer workspace.workspace_destroy(&opened, allocator)
		if options.all_files {
			return workspace.analyze_workspace(&opened, nil, pool, workspace_options, allocator),
				root,
				abs_path
		}
		return workspace.analyze_path(&opened, abs_path, nil, pool, workspace_options, allocator),
			root,
			abs_path
	}

	opened, workspace_ok, workspace_error := workspace.open_standalone(
		os.dir(abs_path),
		workspace_options,
		allocator,
	)
	if !workspace_ok {
		return workspace.Analysis_Result{ok = false, error = workspace_error}, "", abs_path
	}
	defer workspace.workspace_destroy(&opened, allocator)
	input, input_ok := workspace.workspace_file_input_from_path(abs_path, allocator)
	if !input_ok {
		return workspace.Analysis_Result{ok = false, error = "failed to read target file"},
			"",
			abs_path
	}
	files := make([dynamic]semantic.Workspace_File_Input, 0, 1, allocator)
	append(&files, input)
	return workspace.analyze_inputs(&opened, files[:], pool, allocator), "", abs_path
}

run_analyze_lints :: proc(
	result: ^workspace.Analysis_Result,
	pool: ^execution.Pool,
	policy: ^lints.Policy = nil,
) -> lints.Analysis {
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	defer execution.graph_destroy(&graph)

	policy_to_use := policy
	if policy_to_use == nil {
		policy_to_use = &result.lint_policy
	}
	task := lints.submit_analysis(&graph, execution.worker_executor(pool), analysis, policy_to_use)
	execution.graph_start(&graph)
	lint_analysis := execution.wait(task)
	execution.graph_wait(&graph)
	return lint_analysis
}

lint_cli_hard_errors :: proc(
	result: ^workspace.Analysis_Result,
	path_filter: string,
	allocator: mem.Allocator,
) -> [dynamic]Lint_Cli_Hard_Error {
	out := make([dynamic]Lint_Cli_Hard_Error, 0, 4, allocator)
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	if analysis == nil {
		return out
	}
	diagnostics := make([dynamic]cli.Analysis_Diagnostic_Output, 0, 16, allocator)
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
				cli.Analysis_Diagnostic_Output {
					diagnostic = diagnostic,
					fallback_path = project_result.root_path,
				},
			)
		}
	}
	slice.sort_by(diagnostics[:], cli.analysis_diagnostic_output_less)
	for item in diagnostics {
		if item.diagnostic.kind != .Syntax_Error || item.diagnostic.severity != .Error {
			continue
		}
		if !cli.analysis_diagnostic_output_matches_filter(item, path_filter, allocator) {
			continue
		}
		path := cli.analysis_diagnostic_output_path(item)
		append(
			&out,
			Lint_Cli_Hard_Error {
				uri = lint_cli_file_uri(path, allocator),
				path = path,
				phase = "parse",
				message = item.diagnostic.message,
				range = [2]int{item.diagnostic.range.start, item.diagnostic.range.end},
			},
		)
	}
	return out
}

lint_cli_lint_outputs :: proc(
	analysis: ^lints.Analysis,
	path_filter: string,
	allocator: mem.Allocator,
) -> [dynamic]Analyze_Lint_Diagnostic_Output {
	out := make([dynamic]Analyze_Lint_Diagnostic_Output, 0, 16, allocator)
	if analysis == nil {
		return out
	}
	for diagnostic in analysis.diagnostics {
		item := Analyze_Lint_Diagnostic_Output {
			diagnostic    = diagnostic,
			fallback_path = diagnostic.file.path if diagnostic.file != nil else "",
		}
		if analyze_lint_diagnostic_output_matches_filter(item, path_filter) {
			append(&out, item)
		}
	}
	slice.sort_by(out[:], analyze_lint_diagnostic_output_less)
	return out
}

lint_cli_findings_should_fail :: proc(
	analysis: ^lints.Analysis,
	fail_on_warnings: bool,
	path_filter: string,
) -> bool {
	outputs := lint_cli_lint_outputs(analysis, path_filter, context.temp_allocator)
	for item in outputs {
		diagnostic := item.diagnostic
		if diagnostic.suppressed {
			continue
		}
		if diagnostic.level == .Deny || (fail_on_warnings && diagnostic.level == .Warn) {
			return true
		}
	}
	return false
}

lint_cli_report :: proc(
	options: ^Lint_Cli_Options,
	result: ^workspace.Analysis_Result,
	analysis: ^lints.Analysis,
	hard_errors: []Lint_Cli_Hard_Error,
	target_path: string,
	root_path: string,
	path_filter: string,
	allocator: mem.Allocator,
) -> Lint_Cli_Report {
	outputs := lint_cli_lint_outputs(analysis, path_filter, allocator)
	findings := make([]Lint_Cli_Finding, len(outputs), allocator)
	for item, i in outputs {
		findings[i] = lint_cli_finding_json(item, allocator)
	}
	files := make([]Lint_Cli_File, 0, allocator)
	if options.all_files {
		files = lint_cli_file_reports(result, outputs[:], hard_errors, target_path, allocator)
	}

	summary := lint_cli_summary(outputs[:])
	if options.all_files {
		summary.file_count = len(files)
		summary.hard_error_count = len(hard_errors)
	}

	project_count := lint_cli_project_unit_count(result)
	project_count_value := lint_cli_optional_int(
		project_count,
		options.with_project || options.all_files,
	)
	dependency_count_value := lint_cli_optional_int(0, options.with_project || options.all_files)
	editable_count_value := lint_cli_optional_int(
		len(result.session.editable_files),
		options.all_files,
	)
	root_uri_value := lint_cli_optional_string(lint_cli_file_uri(root_path, allocator))

	return Lint_Cli_Report {
		schema = "abap-lsp.lint",
		version = 1,
		phase = "lint",
		target = Lint_Cli_Target {
			uri = lint_cli_file_uri(target_path, allocator),
			path = lint_cli_optional_string(target_path),
			object_name = lint_cli_optional_string(""),
			is_dependency = false,
		},
		workspace = Lint_Cli_Workspace {
			with_project = options.with_project || options.all_files,
			all_files = options.all_files,
			root_uri = root_uri_value,
			manifest_present = result.used_manifest,
			project_unit_count = project_count_value,
			dependency_unit_count = dependency_count_value,
			editable_file_count = editable_count_value,
		},
		files = files,
		findings = findings,
		hard_errors = hard_errors,
		summary = summary,
	}
}

lint_cli_file_reports :: proc(
	result: ^workspace.Analysis_Result,
	findings: []Analyze_Lint_Diagnostic_Output,
	hard_errors: []Lint_Cli_Hard_Error,
	target_path: string,
	allocator: mem.Allocator,
) -> []Lint_Cli_File {
	paths := make([dynamic]string, 0, len(result.session.editable_files), allocator)
	for input in result.session.editable_files {
		lint_cli_add_report_path(&paths, input.path)
	}
	for item in findings {
		lint_cli_add_report_path(&paths, analyze_lint_diagnostic_output_path(item))
	}
	for error in hard_errors {
		lint_cli_add_report_path(&paths, error.path)
	}
	if len(paths) == 0 {
		lint_cli_add_report_path(&paths, target_path)
	}
	slice.sort_by(paths[:], lint_cli_path_less)

	files := make([]Lint_Cli_File, len(paths), allocator)
	for path, i in paths {
		finding_count := 0
		for item in findings {
			if lint_cli_same_path(path, analyze_lint_diagnostic_output_path(item)) {
				finding_count += 1
			}
		}
		hard_error_count := 0
		for error in hard_errors {
			if lint_cli_same_path(path, error.path) {
				hard_error_count += 1
			}
		}
		files[i] = Lint_Cli_File {
			uri              = lint_cli_file_uri(path, allocator),
			path             = lint_cli_optional_string(path),
			object_name      = lint_cli_optional_string(""),
			is_dependency    = false,
			finding_count    = finding_count,
			hard_error_count = hard_error_count,
			summary          = lint_cli_summary_for_path(findings, path),
		}
	}
	return files
}

lint_cli_add_report_path :: proc(paths: ^[dynamic]string, path: string) {
	if path == "" {
		return
	}
	for existing in paths^ {
		if lint_cli_same_path(existing, path) {
			return
		}
	}
	append(paths, path)
}

lint_cli_path_less :: proc(a, b: string) -> bool {
	return strings.compare(a, b) < 0
}

lint_cli_same_path :: proc(a, b: string) -> bool {
	return(
		workspace.normalized_uri_path_key(a, context.temp_allocator) ==
		workspace.normalized_uri_path_key(b, context.temp_allocator) \
	)
}

lint_cli_finding_json :: proc(
	item: Analyze_Lint_Diagnostic_Output,
	allocator: mem.Allocator,
) -> Lint_Cli_Finding {
	diagnostic := item.diagnostic
	return Lint_Cli_Finding {
		uri = lint_cli_file_uri(analyze_lint_diagnostic_output_path(item), allocator),
		lint_id = diagnostic.id,
		level = lints.level_string(diagnostic.level),
		group = lints.group_string(diagnostic.group),
		origin = lints.origin_string(diagnostic.origin),
		message = diagnostic.message,
		range = [2]int{diagnostic.range.start, diagnostic.range.end},
		suppressed = diagnostic.suppressed,
		suppression = lint_cli_suppression_json(diagnostic),
	}
}

lint_cli_suppression_json :: proc(diagnostic: lints.Diagnostic) -> Maybe(Lint_Cli_Suppression) {
	if !diagnostic.has_suppression {
		return nil
	}
	return Lint_Cli_Suppression {
		kind = lints.suppression_kind_string(diagnostic.suppression.kind),
		range = [2]int{diagnostic.suppression.range.start, diagnostic.suppression.range.end},
		token = diagnostic.suppression.token,
	}
}

lint_cli_summary :: proc(findings: []Analyze_Lint_Diagnostic_Output) -> Lint_Cli_Summary {
	summary: Lint_Cli_Summary
	summary.total = len(findings)
	for item in findings {
		lint_cli_summary_add(&summary, item.diagnostic)
	}
	return summary
}

lint_cli_summary_for_path :: proc(
	findings: []Analyze_Lint_Diagnostic_Output,
	path: string,
) -> Lint_Cli_Summary {
	summary: Lint_Cli_Summary
	for item in findings {
		if !lint_cli_same_path(path, analyze_lint_diagnostic_output_path(item)) {
			continue
		}
		summary.total += 1
		lint_cli_summary_add(&summary, item.diagnostic)
	}
	return summary
}

lint_cli_summary_add :: proc(summary: ^Lint_Cli_Summary, diagnostic: lints.Diagnostic) {
	if diagnostic.suppressed {
		summary.suppressed += 1
	}
	switch diagnostic.level {
	case .Allow:
		summary.by_level.allow += 1
	case .Info:
		summary.by_level.info += 1
	case .Warn:
		summary.by_level.warn += 1
	case .Deny:
		summary.by_level.deny += 1
	}
	switch diagnostic.group {
	case .Correctness:
		summary.by_group.correctness += 1
	case .Performance:
		summary.by_group.performance += 1
	case .Security:
		summary.by_group.security += 1
	case .Style:
		summary.by_group.style += 1
	case .Modernization:
		summary.by_group.modernization += 1
	case .Package:
		summary.by_group.package_count += 1
	case .Experimental:
		summary.by_group.experimental += 1
	}
}

lint_cli_project_unit_count :: proc(result: ^workspace.Analysis_Result) -> int {
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	if analysis == nil {
		return 0
	}
	return len(analysis.project_results)
}

lint_cli_file_uri :: proc(path: string, allocator: mem.Allocator) -> string {
	if path == "" {
		return ""
	}
	display := cli.display_uri(path, allocator)
	lower := strings.to_lower(display, context.temp_allocator)
	if strings.has_prefix(lower, "file://") {
		return display
	}
	if len(display) >= 2 && display[1] == ':' {
		return strings.concatenate({"file:///", display}, allocator)
	}
	if strings.has_prefix(display, "/") {
		return strings.concatenate({"file://", display}, allocator)
	}
	return display
}

lint_cli_optional_string :: proc(value: string) -> Maybe(string) {
	if value == "" {
		return nil
	}
	return value
}

lint_cli_optional_int :: proc(value: int, present: bool) -> Maybe(int) {
	if !present {
		return nil
	}
	return value
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

source_location_equal :: proc(a, b: base_runtime.Source_Code_Location) -> bool {
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

run_perf_edit :: proc(args: []string, allocator: mem.Allocator) {
	assert(
		context.temp_allocator.procedure == virtual.arena_allocator_proc &&
		context.temp_allocator.data != nil,
	)
	temp_arena := virtual.arena_temp_begin(cast(^virtual.Arena)context.temp_allocator.data)
	defer virtual.arena_temp_end(temp_arena)

	options, options_ok := perf_edit_parse_options(args)
	if !options_ok {
		print_usage()
		os.exit(1)
	}
	context.allocator = allocator

	root_path, root_ok := workspace.absolute_clean_path(options.workspace_path, context.temp_allocator)
	target_path, target_ok := workspace.absolute_clean_path(options.file_path, context.temp_allocator)
	if !root_ok || !target_ok {
		fmt.eprintln("error: invalid perf-edit path")
		os.exit(1)
	}
	source, source_ok := read_source(target_path, allocator)
	if !source_ok {
		os.exit(1)
	}

	workspace_flags := workspace.Option_Flags {
		.Enable_Dependency_Diagnostics,
		.Disable_ADT_Dependency_Fetch,
	}
	if options.enable_lints {
		workspace_flags += {.Enable_Lints}
	}
	workspace_options := workspace.Options{flags = workspace_flags}
	opened, workspace_ok, workspace_error := workspace.open(
		root_path,
		workspace_options,
		allocator,
	)
	if !workspace_ok {
		fmt.printf("error\tperf-edit\t%s\n", workspace_error)
		os.exit(1)
	}
	defer workspace.workspace_destroy(&opened, allocator)

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

	initial_start := time.now()
	result := workspace.analyze_workspace(
		&opened,
		nil,
		&pool,
		workspace_options,
		context.allocator,
	)
	if !result.ok {
		fmt.printf("error\tperf-edit\t%s\n", result.error)
		os.exit(1)
	}
	defer workspace.analysis_result_destroy(&result, context.allocator)
	fmt.printf(
		"perf-edit\tworkspace\t%s\tfile\t%s\titerations\t%d\tinitial_ms\t%.3f\tinitial_files\t%d\n",
		root_path,
		target_path,
		options.iterations,
		time.duration_milliseconds(time.since(initial_start)),
		result.last_stats.semantic.workspace_rebuild_input_files,
	)
	fmt.println(
		"iteration\ttotal_ms\tparse_ms\tupdate_ms\tlint_ms\tchanged_files\tremote_requests\tremote_sources\tworkspace_rebuilds\tincremental_rebuilds\trebuild_input_files\tfull_rebuilds\trebuilt_editable\tnew_fetch_requests\tlint_diagnostics",
	)

	for iteration in 1 ..= options.iterations {
		iteration_start := time.now()
		parse_start := time.now()
		edited_source := perf_edit_source(source, iteration, allocator)
		input := perf_workspace_input_from_source(target_path, edited_source, allocator)
		changed := [?]semantic.Workspace_File_Input{input}
		parse_ms := time.duration_milliseconds(time.since(parse_start))

		update_start := time.now()
		update_ok := workspace.analysis_result_update_inputs(
			&result,
			&opened,
			changed[:],
			nil,
			&pool,
			context.allocator,
			workspace.Analysis_Update_Options {
				suspend_external_dependency_acquisition = true,
			},
		)
		update_ms := time.duration_milliseconds(time.since(update_start))
		if !update_ok {
			fmt.eprintln("error: perf-edit update failed")
			os.exit(1)
		}

		lint_ms := 0.0
		lint_diagnostics := -1
		if options.enable_lints {
			lint_start := time.now()
			lint_analysis := run_analyze_lints(&result, &pool)
			lint_ms = time.duration_milliseconds(time.since(lint_start))
			lint_diagnostics = len(lint_analysis.diagnostics)
			lints.analysis_destroy(&lint_analysis)
		}

		stats := result.last_stats
		fmt.printf(
			"%d\t%.3f\t%.3f\t%.3f\t%.3f\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n",
			iteration,
			time.duration_milliseconds(time.since(iteration_start)),
			parse_ms,
			update_ms,
			lint_ms,
			stats.changed_files,
			stats.remote_resolution_requests,
			stats.remote_sources,
			stats.semantic.workspace_rebuilds,
			stats.semantic.incremental_workspace_rebuilds,
			stats.semantic.workspace_rebuild_input_files,
			stats.semantic.full_editable_rebuilds,
			stats.semantic.rebuilt_editable_projects,
			stats.semantic.new_fetch_requests,
			lint_diagnostics,
		)
	}
}

perf_edit_parse_options :: proc(args: []string) -> (Perf_Edit_Options, bool) {
	if len(args) < 4 {
		return {}, false
	}
	options := Perf_Edit_Options {
		workspace_path = args[2],
		file_path = args[3],
		iterations = 5,
	}
	for i := 4; i < len(args); {
		if args[i] == "--iterations" && i + 1 < len(args) {
			parsed, parse_ok := strconv.parse_int(args[i + 1], 10)
			if !parse_ok || parsed < 1 {
				fmt.eprintf("invalid value for --iterations: %q\n", args[i + 1])
				return {}, false
			}
			options.iterations = parsed
			i += 2
		} else if args[i] == "--enable-lints" {
			options.enable_lints = true
			i += 1
		} else {
			return {}, false
		}
	}
	return options, true
}

perf_edit_source :: proc(source: string, iteration: int, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, source)
	if len(source) == 0 || source[len(source) - 1] != '\n' {
		strings.write_byte(&out, '\n')
	}
	strings.write_string(&out, `" perf-edit iteration `)
	strings.write_string(&out, fmt.tprintf("%d", iteration))
	strings.write_byte(&out, '\n')
	return strings.to_string(out)
}

perf_workspace_input_from_source :: proc(
	path: string,
	source: string,
	allocator: mem.Allocator,
) -> semantic.Workspace_File_Input {
	parsed := parser.parse(source, path, allocator)
	return semantic.Workspace_File_Input {
		path = strings.clone(path, allocator),
		root = parsed.root,
		kind = .Unknown,
		syntax_diagnostics = workspace.syntax_diagnostics_from_parse_errors(
			parsed.errors,
			allocator,
		),
		has_syntax_errors = len(parsed.errors) > 0,
	}
}

print_lint_diagnostics :: proc(
	analysis: ^lints.Analysis,
	warnings_as_errors: bool,
	path_filter: string = "",
) -> (
	count_errors: int,
) {
	if analysis == nil {
		return
	}
	use_color := terminal.color_enabled && terminal.is_terminal(os.stdout)
	source_cache := make([dynamic]cli.Source_Cache_Entry, 0, 16, context.temp_allocator)
	diagnostics := make(
		[dynamic]Analyze_Lint_Diagnostic_Output,
		0,
		len(analysis.diagnostics),
		context.temp_allocator,
	)
	for diagnostic in analysis.diagnostics {
		append(
			&diagnostics,
			Analyze_Lint_Diagnostic_Output {
				diagnostic = diagnostic,
				fallback_path = diagnostic.file.path if diagnostic.file != nil else "",
			},
		)
	}
	slice.sort_by(diagnostics[:], analyze_lint_diagnostic_output_less)
	for item in diagnostics {
		if !analyze_lint_diagnostic_output_matches_filter(item, path_filter) {
			continue
		}
		if print_lint_diagnostic(
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

analyze_lint_diagnostic_output_matches_filter :: proc(
	item: Analyze_Lint_Diagnostic_Output,
	path_filter: string,
) -> bool {
	if path_filter == "" {
		return true
	}
	path := analyze_lint_diagnostic_output_path(item)
	return(
		workspace.normalized_uri_path_key(path, context.temp_allocator) ==
		workspace.normalized_uri_path_key(path_filter, context.temp_allocator) \
	)
}

analyze_lint_diagnostic_output_less :: proc(left, right: Analyze_Lint_Diagnostic_Output) -> bool {
	left_path := analyze_lint_diagnostic_output_path(left)
	right_path := analyze_lint_diagnostic_output_path(right)
	path_cmp := strings.compare(left_path, right_path)
	if path_cmp != 0 {
		return path_cmp < 0
	}
	if left.diagnostic.range.start != right.diagnostic.range.start {
		return left.diagnostic.range.start < right.diagnostic.range.start
	}
	if left.diagnostic.range.end != right.diagnostic.range.end {
		return left.diagnostic.range.end < right.diagnostic.range.end
	}
	if left.diagnostic.severity != right.diagnostic.severity {
		return int(left.diagnostic.severity) < int(right.diagnostic.severity)
	}
	id_cmp := strings.compare(left.diagnostic.id, right.diagnostic.id)
	if id_cmp != 0 {
		return id_cmp < 0
	}
	return strings.compare(left.diagnostic.message, right.diagnostic.message) < 0
}

analyze_lint_diagnostic_output_path :: proc(item: Analyze_Lint_Diagnostic_Output) -> string {
	if item.diagnostic.file != nil && item.diagnostic.file.path != "" {
		return item.diagnostic.file.path
	}
	return item.fallback_path
}

print_lint_diagnostic :: proc(
	diagnostic: lints.Diagnostic,
	fallback_path: string,
	warnings_as_errors: bool,
	use_color: bool,
	source_cache: ^[dynamic]cli.Source_Cache_Entry,
) -> bool {
	path := fallback_path
	if diagnostic.file != nil && diagnostic.file.path != "" {
		path = diagnostic.file.path
	}
	source := cli.cached_source(path, source_cache, context.temp_allocator)
	line_starts := cli.build_line_starts(source, context.temp_allocator)
	uri := cli.display_uri(path, context.temp_allocator)
	is_warning := diagnostic.severity == .Warning
	is_note := diagnostic.severity == .Information || diagnostic.severity == .Hint
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
		color = cli.SGR_RED if had_error else cli.SGR_YELLOW
	}
	start := cli.source_position(source, line_starts[:], diagnostic.range.start)
	end := cli.source_position(source, line_starts[:], diagnostic.range.end)
	prefix := cli.diagnostic_prefix(uri, start.line, start.column, label, color)
	fmt.printf("%s %s: %s\n", prefix, diagnostic.id, diagnostic.message)
	cli.print_source_highlight(source, line_starts[:], start, end, color)
	fmt.println()
	return had_error
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
	line_starts := cli.build_line_starts(source, context.temp_allocator)
	uri := cli.parse_display_uri(path, context.temp_allocator)
	color := ""
	if terminal.color_enabled && terminal.is_terminal(os.stdout) {
		color = cli.SGR_RED
	}
	for err in errors {
		start := cli.source_position(source, line_starts[:], err.range.start)
		end := cli.source_position(source, line_starts[:], err.range.end)
		prefix := cli.diagnostic_prefix(uri, start.line, start.column, "error", color)
		fmt.printf("%s Syntax_Error: %s\n", prefix, err.message)
		cli.print_source_highlight(source, line_starts[:], start, end, color)
		fmt.println()
	}
	return true
}

print_node_counts :: proc(root: ^ast.Node, allocator: base_runtime.Allocator) {
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
