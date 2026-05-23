package abap_frontend_semantic

import runtime "../runtime"
import "../parser"

import "core:mem"
import "core:strings"

Source_Input :: struct {
	uri:    string,
	source: string,
}

Analyze_Options :: struct {
	pool:                  ^runtime.Pool,
	dependency_store_path: string,
	enable_standalone_adt: bool,
}

Project_Analysis :: struct {
	units:       [dynamic]Unit_Analysis,
	diagnostics: [dynamic]Diagnostic,
}

Candidate_Name :: struct {
	name:            string,
	candidate_index: int,
}

Project_Candidate_Input :: struct {
	input:       Source_Input,
	object_name: string,
}

Project_Work_State :: struct {
	units:           [dynamic]Unit_Analysis,
	inputs:          [dynamic]Source_Input,
	unit_allocators: []mem.Allocator,
	allocator:       mem.Allocator,
}

Project_Task_Payload :: struct {
	state:      ^Project_Work_State,
	unit_index: int,
}

Project_Infer_State :: struct {
	project:         ^Project_Analysis,
	lookup:          ^Validation_Lookup,
	inferred:        []Inferred_Unit_Facts,
	unit_allocators: []mem.Allocator,
	allocator:       mem.Allocator,
}

Project_Validate_State :: struct {
	project:         ^Project_Analysis,
	lookup:          ^Validation_Lookup,
	diagnostics:     [][dynamic]Diagnostic,
	unit_allocators: []mem.Allocator,
	allocator:       mem.Allocator,
}

Project_Infer_Payload :: struct {
	state:      ^Project_Infer_State,
	unit_index: int,
}

Project_Validate_Payload :: struct {
	state:      ^Project_Validate_State,
	unit_index: int,
}

analyze_target :: proc(
	target: Source_Input,
	candidates: []Source_Input,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	wrapped := make([dynamic]Project_Candidate_Input, 0, len(candidates), allocator)
	for candidate in candidates {
		append(&wrapped, Project_Candidate_Input{input = candidate})
	}
	return analyze_target_with_candidate_inputs(target, wrapped[:], {}, options, allocator)
}

analyze_target_with_candidate_inputs :: proc(
	target: Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return analyze_target_with_candidate_inputs_allocators(target, candidates, dependencies, options, {}, allocator)
}

analyze_target_with_candidate_inputs_allocators :: proc(
	target: Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	options: Analyze_Options,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) -> Project_Analysis {
	assert(options.pool != nil)
	state := Project_Work_State {
		units           = make([dynamic]Unit_Analysis, 0, 1 + len(dependencies), allocator),
		inputs          = make([dynamic]Source_Input, 0, 1 + len(dependencies), allocator),
		unit_allocators = unit_allocators,
		allocator       = allocator,
	}
	unit_dirs := make([dynamic]string, 0, 1 + len(dependencies), allocator)
	unit_candidate_indices := make([dynamic]int, 0, 1 + len(dependencies), allocator)

	append(&state.inputs, target)
	append(&state.units, parse_collect_input(Unit_Id(0), target, unit_allocator(unit_allocators, 0, allocator)))
	append(&unit_dirs, uri_parent_dir_key(target.uri, allocator))
	append(&unit_candidate_indices, -1)
	dependency_unit_indices := make([dynamic]int, 0, len(dependencies), allocator)
	for dependency in dependencies {
		unit_index := len(state.units)
		append(&state.inputs, dependency)
		append(&state.units, Unit_Analysis{})
		append(&unit_dirs, uri_parent_dir_key(dependency.uri, allocator))
		append(&unit_candidate_indices, -1)
		append(&dependency_unit_indices, unit_index)
	}
	if len(dependency_unit_indices) > 0 {
		run_project_tasks(options.pool, dependency_unit_indices[:], &state, parse_collect_task, allocator)
	}

	candidate_dirs := make([]string, len(candidates), allocator)
	candidate_to_unit := make([]Unit_Id, len(candidates), allocator)
	candidate_names := make([dynamic]Candidate_Name, 0, len(candidates), allocator)
	for candidate, i in candidates {
		candidate_dirs[i] = uri_parent_dir_key(candidate.input.uri, allocator)
		candidate_to_unit[i] = INVALID_UNIT_ID
		add_candidate_name(&candidate_names, uri_file_stem(candidate.input.uri), i, allocator)
		add_candidate_name(&candidate_names, candidate.object_name, i, allocator)
	}

	for {
		new_units := make([dynamic]int, 0, 4, allocator)
		for unit_index in 0 ..< len(state.units) {
			resolve_reachable_include_edges(
				&state,
				unit_index,
				candidates,
				candidate_dirs,
				&candidate_names,
				candidate_to_unit,
				&unit_dirs,
				&unit_candidate_indices,
				&new_units,
			)
		}
		if len(new_units) == 0 {
			break
		}
		run_project_tasks(options.pool, new_units[:], &state, parse_collect_task, allocator)
		for unit_index in new_units {
			candidate_index := unit_candidate_indices[unit_index]
			if candidate_index < 0 {
				continue
			}
			for name in state.units[unit_index].provided_names {
				add_candidate_name(&candidate_names, name, candidate_index, allocator)
			}
		}
	}

	add_unresolved_include_diagnostics(state.units[:], allocator)
	diagnose_include_cycles(state.units[:], allocator)
	run_all_unit_tasks(options.pool, &state, build_scope_index_task, allocator)
	for i in 0 ..< len(state.units) {
		resolve_unit_with_index(&state.units[i], &state.units[i].scope_index)
	}
	project := project_analysis_from_units(state.units, allocator)
	finish_project_analysis(&project, options.pool, unit_allocators, allocator)
	return project
}

unit_allocator :: proc(unit_allocators: []mem.Allocator, unit_index: int, fallback: mem.Allocator) -> mem.Allocator {
	if 0 <= unit_index && unit_index < len(unit_allocators) && unit_allocators[unit_index].procedure != nil {
		return unit_allocators[unit_index]
	}
	return fallback
}

project_analysis_from_units :: proc(
	units: [dynamic]Unit_Analysis,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return Project_Analysis {
		units = units,
		diagnostics = make([dynamic]Diagnostic, 0, 8, allocator),
	}
}

finish_project_analysis :: proc(
	project: ^Project_Analysis,
	pool: ^runtime.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	resolve_project_cross_unit(project.units[:], allocator)
	link_class_member_implementations(project.units[:], allocator)
	reclassify_project_open_sql_predicate_host_variables(project.units[:], allocator)
	lookup := build_validation_lookup(project, allocator)
	infer_project_semantic_facts(project, &lookup, pool, unit_allocators, allocator)
	validate_project_units(project, &lookup, pool, unit_allocators, allocator)
	rebuild_project_semantic_indexes(project, pool, unit_allocators, allocator)
	collect_project_diagnostics(project)
}

project_unit_by_uri :: proc(project: ^Project_Analysis, uri: string) -> ^Unit_Analysis {
	for &unit in project.units {
		if unit.uri == uri {
			return &unit
		}
	}
	return nil
}

parse_collect_input :: proc(
	unit_id: Unit_Id,
	input: Source_Input,
	allocator: mem.Allocator,
) -> Unit_Analysis {
	parsed: parser.Parsed_File
	parsed = parser.parse(input.source, input.uri, allocator)
	return collect_unit(unit_id, input.uri, input.source, parsed, allocator)
}

resolve_reachable_include_edges :: proc(
	state: ^Project_Work_State,
	unit_index: int,
	candidates: []Project_Candidate_Input,
	candidate_dirs: []string,
	candidate_names: ^[dynamic]Candidate_Name,
	candidate_to_unit: []Unit_Id,
	unit_dirs: ^[dynamic]string,
	unit_candidate_indices: ^[dynamic]int,
	new_units: ^[dynamic]int,
) {
	source_dir := unit_dirs[unit_index]
	for &edge in state.units[unit_index].include_edges {
		if edge.has_target {
			continue
		}
		candidate_index, ok := resolve_include_candidate(
			edge.name,
			source_dir,
			candidate_dirs,
			candidate_names^[:],
		)
		if !ok {
			continue
		}
		target_unit := candidate_to_unit[candidate_index]
		if target_unit == INVALID_UNIT_ID {
			target_unit = Unit_Id(u32(len(state.units)))
			candidate_to_unit[candidate_index] = target_unit
			append(&state.inputs, candidates[candidate_index].input)
			append(&state.units, Unit_Analysis{})
			append(unit_dirs, candidate_dirs[candidate_index])
			append(unit_candidate_indices, candidate_index)
			append(new_units, unit_id_index(target_unit))
		}
		edge.target = target_unit
		edge.has_target = true
	}
}

resolve_include_candidate :: proc(
	name, source_dir: string,
	candidate_dirs: []string,
	candidate_names: []Candidate_Name,
) -> (
	int,
	bool,
) {
	if source_dir != "" {
		if candidate, ok := find_candidate_in_dir(name, source_dir, candidate_dirs, candidate_names);
		   ok {
			return candidate, true
		}
		if candidate, ok := find_candidate_in_child_dir(
			name,
			source_dir,
			"includes",
			candidate_dirs,
			candidate_names,
		);
		   ok {
			return candidate, true
		}
	}
	for i in 0 ..< len(candidate_dirs) {
		if candidate_has_name(candidate_names, i, name) {
			return i, true
		}
	}
	return -1, false
}

find_candidate_in_dir :: proc(
	name, dir: string,
	candidate_dirs: []string,
	candidate_names: []Candidate_Name,
) -> (
	int,
	bool,
) {
	for candidate_dir, i in candidate_dirs {
		if candidate_dir == dir && candidate_has_name(candidate_names, i, name) {
			return i, true
		}
	}
	return -1, false
}

find_candidate_in_child_dir :: proc(
	name, parent, child: string,
	candidate_dirs: []string,
	candidate_names: []Candidate_Name,
) -> (
	int,
	bool,
) {
	for candidate_dir, i in candidate_dirs {
		if dir_is_child(candidate_dir, parent, child) && candidate_has_name(candidate_names, i, name) {
			return i, true
		}
	}
	return -1, false
}

dir_is_child :: proc(candidate, parent, child: string) -> bool {
	if len(candidate) != len(parent) + 1 + len(child) {
		return false
	}
	return candidate[:len(parent)] == parent &&
	       candidate[len(parent)] == '/' &&
	       candidate[len(parent) + 1:] == child
}

candidate_has_name :: proc(names: []Candidate_Name, candidate_index: int, name: string) -> bool {
	for candidate_name in names {
		if candidate_name.candidate_index == candidate_index && candidate_name.name == name {
			return true
		}
	}
	return false
}

add_candidate_name :: proc(
	names: ^[dynamic]Candidate_Name,
	name: string,
	candidate_index: int,
	allocator: mem.Allocator,
) {
	if name == "" {
		return
	}
	canonical := canonical_name(name, allocator)
	for existing in names^ {
		if existing.candidate_index == candidate_index && existing.name == canonical {
			return
		}
	}
	append(names, Candidate_Name{name = canonical, candidate_index = candidate_index})
}

run_all_unit_tasks :: proc(
	pool: ^runtime.Pool,
	state: ^Project_Work_State,
	work: proc(Project_Task_Payload) -> runtime.No_Result,
	allocator: mem.Allocator,
) {
	indices := make([dynamic]int, 0, len(state.units), allocator)
	for _, i in state.units {
		append(&indices, i)
	}
	run_project_tasks(pool, indices[:], state, work, allocator)
}

run_project_tasks :: proc(
	pool: ^runtime.Pool,
	unit_indices: []int,
	state: ^Project_Work_State,
	work: proc(Project_Task_Payload) -> runtime.No_Result,
	allocator: mem.Allocator,
) {
	batch_size := pool.options.task_capacity
	for start := 0; start < len(unit_indices); {
		end := start + batch_size
		if end > len(unit_indices) {
			end = len(unit_indices)
		}
		tasks := make([dynamic]runtime.Task(runtime.No_Result), 0, end - start, allocator)
		for unit_index in unit_indices[start:end] {
			payload := Project_Task_Payload{state = state, unit_index = unit_index}
			task, err := runtime.submit_value(pool, payload, work)
			assert(err == .None)
			append(&tasks, task)
		}
		for task in tasks {
			_, _ = runtime.wait(task)
		}
		delete(tasks)
		start = end
	}
}

parse_collect_task :: proc(payload: Project_Task_Payload) -> runtime.No_Result {
	input := payload.state.inputs[payload.unit_index]
	payload.state.units[payload.unit_index] = parse_collect_input(
		Unit_Id(u32(payload.unit_index)),
		input,
		unit_allocator(payload.state.unit_allocators, payload.unit_index, payload.state.allocator),
	)
	return runtime.No_Result{}
}

build_scope_index_task :: proc(payload: Project_Task_Payload) -> runtime.No_Result {
	unit := &payload.state.units[payload.unit_index]
	unit.scope_index = build_scope_index(
		unit,
		unit_allocator(payload.state.unit_allocators, payload.unit_index, payload.state.allocator),
	)
	return runtime.No_Result{}
}

rebuild_semantic_index_task :: proc(payload: Project_Task_Payload) -> runtime.No_Result {
	rebuild_semantic_index(
		&payload.state.units[payload.unit_index],
		unit_allocator(payload.state.unit_allocators, payload.unit_index, payload.state.allocator),
	)
	return runtime.No_Result{}
}

add_unresolved_include_diagnostics :: proc(units: []Unit_Analysis, allocator: mem.Allocator) {
	for &unit in units {
		for edge in unit.include_edges {
			if !edge.has_target && !edge.if_found {
				append(
					&unit.diagnostics,
					Diagnostic {
						kind = .Unresolved_Include,
						range = edge.range,
						message = diagnostic_message("unresolved include ", edge.name, allocator),
					},
				)
			}
		}
	}
}

diagnose_include_cycles :: proc(units: []Unit_Analysis, allocator: mem.Allocator) {
	stack := make([dynamic]Unit_Id, 0, len(units), allocator)
	done := make([]bool, len(units), allocator)
	for unit, i in units {
		if !done[i] {
			diagnose_include_cycles_from(units, unit.unit_id, &stack, done, allocator)
		}
	}
}

diagnose_include_cycles_from :: proc(
	units: []Unit_Analysis,
	unit_id: Unit_Id,
	stack: ^[dynamic]Unit_Id,
	done: []bool,
	allocator: mem.Allocator,
) {
	unit_index := unit_id_index(unit_id)
	if unit_index < 0 || unit_index >= len(units) {
		return
	}
	if done[unit_index] {
		return
	}
	if unit_in_stack(stack^[:], unit_id) {
		return
	}
	append(stack, unit_id)
	for edge in units[unit_index].include_edges {
		if !edge.has_target || edge.target == INVALID_UNIT_ID {
			continue
		}
		if unit_in_stack(stack^[:], edge.target) {
			append(
				&units[unit_index].diagnostics,
				Diagnostic {
					kind = .Include_Cycle,
					range = edge.range,
					message = diagnostic_message("include cycle at ", edge.name, allocator),
				},
			)
			continue
		}
		diagnose_include_cycles_from(units, edge.target, stack, done, allocator)
	}
	resize(stack, len(stack^) - 1)
	done[unit_index] = true
}

unit_in_stack :: proc(stack: []Unit_Id, unit_id: Unit_Id) -> bool {
	for current in stack {
		if current == unit_id {
			return true
		}
	}
	return false
}

collect_project_diagnostics :: proc(project: ^Project_Analysis) {
	clear(&project.diagnostics)
	hint := 0
	for unit in project.units {
		hint += len(unit.diagnostics)
	}
	if hint < 8 {
		hint = 8
	}
	seen := make(map[Diagnostic_Key]bool, hint, context.temp_allocator)
	for unit in project.units {
		for diagnostic in unit.diagnostics {
			key := diagnostic_key(diagnostic)
			if !(key in seen) {
				seen[key] = true
				append(&project.diagnostics, diagnostic)
			}
		}
	}
}

infer_project_semantic_facts :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	pool: ^runtime.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	for _ in 0 ..< 4 {
		inferred := make([]Inferred_Unit_Facts, len(project.units), allocator)
		state := Project_Infer_State {
			project         = project,
			lookup          = lookup,
			inferred        = inferred,
			unit_allocators = unit_allocators,
			allocator       = allocator,
		}
		run_infer_tasks(pool, &state, allocator)
		if !apply_inferred_project_facts(project, inferred) {
			break
		}
	}
}

validate_project_units :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	pool: ^runtime.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	diagnostics := make([][dynamic]Diagnostic, len(project.units), allocator)
	state := Project_Validate_State {
		project         = project,
		lookup          = lookup,
		diagnostics     = diagnostics,
		unit_allocators = unit_allocators,
		allocator       = allocator,
	}
	run_validate_tasks(pool, &state, allocator)
	for i in 0 ..< len(project.units) {
		project.units[i].diagnostics = diagnostics[i]
	}
}

rebuild_project_semantic_indexes :: proc(
	project: ^Project_Analysis,
	pool: ^runtime.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	state := Project_Work_State {
		units           = project.units,
		inputs          = make([dynamic]Source_Input, 0, 0, allocator),
		unit_allocators = unit_allocators,
		allocator       = allocator,
	}
	run_all_unit_tasks(pool, &state, rebuild_semantic_index_task, allocator)
	project.units = state.units
}

run_infer_tasks :: proc(
	pool: ^runtime.Pool,
	state: ^Project_Infer_State,
	allocator: mem.Allocator,
) {
	batch_size := pool.options.task_capacity
	for start := 0; start < len(state.project.units); {
		end := start + batch_size
		if end > len(state.project.units) {
			end = len(state.project.units)
		}
		tasks := make([dynamic]runtime.Task(runtime.No_Result), 0, end - start, allocator)
		for unit_index in start ..< end {
			payload := Project_Infer_Payload{state = state, unit_index = unit_index}
			task, err := runtime.submit_value(pool, payload, infer_task)
			assert(err == .None)
			append(&tasks, task)
		}
		for task in tasks {
			_, _ = runtime.wait(task)
		}
		delete(tasks)
		start = end
	}
}

run_validate_tasks :: proc(
	pool: ^runtime.Pool,
	state: ^Project_Validate_State,
	allocator: mem.Allocator,
) {
	batch_size := pool.options.task_capacity
	for start := 0; start < len(state.project.units); {
		end := start + batch_size
		if end > len(state.project.units) {
			end = len(state.project.units)
		}
		tasks := make([dynamic]runtime.Task(runtime.No_Result), 0, end - start, allocator)
		for unit_index in start ..< end {
			payload := Project_Validate_Payload{state = state, unit_index = unit_index}
			task, err := runtime.submit_value(pool, payload, validate_task)
			assert(err == .None)
			append(&tasks, task)
		}
		for task in tasks {
			_, _ = runtime.wait(task)
		}
		delete(tasks)
		start = end
	}
}

infer_task :: proc(payload: Project_Infer_Payload) -> runtime.No_Result {
	payload.state.inferred[payload.unit_index] = infer_unit_semantic_facts(
		payload.state.project,
		payload.state.lookup,
		payload.unit_index,
		unit_allocator(payload.state.unit_allocators, payload.unit_index, payload.state.allocator),
	)
	return runtime.No_Result{}
}

validate_task :: proc(payload: Project_Validate_Payload) -> runtime.No_Result {
	payload.state.diagnostics[payload.unit_index] = validate_unit_diagnostics(
		payload.state.project,
		payload.state.lookup,
		payload.unit_index,
		unit_allocator(payload.state.unit_allocators, payload.unit_index, payload.state.allocator),
	)
	return runtime.No_Result{}
}

diagnostic_message :: proc(prefix, name: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, prefix)
	strings.write_string(&out, name)
	return strings.to_string(out)
}

link_class_member_implementations :: proc(units: []Unit_Analysis, allocator: mem.Allocator) {
	predecessors := include_predecessor_units_for_units(units, allocator)
	for unit_index in 0 ..< len(units) {
		for member_index in 0 ..< len(units[unit_index].class_members) {
			member := &units[unit_index].class_members[member_index]
			if .Has_Implementation_Range in member.flags {
				member.implementation = Class_Member_Implementation_Data {
					unit = units[unit_index].unit_id,
					range = member.implementation_range,
				}
				member.flags += {.Has_Implementation}
			}
		}
	}
	roots := build_project_root_index(units, allocator)
	root_lookup := build_project_root_lookup(units, roots[:], allocator)
	for impl_unit_index in 0 ..< len(units) {
		for method_symbol in units[impl_unit_index].symbols {
			if method_symbol.kind != .Method {
				continue
			}
			class_symbol, ok := enclosing_class_owner_unit(&units[impl_unit_index], method_symbol.scope)
			if !ok {
				continue
			}
			class_name := symbol(&units[impl_unit_index], class_symbol).name
			for i := len(predecessors[impl_unit_index]) - 1; i >= 0; i -= 1 {
				def_unit := predecessors[impl_unit_index][i]
				class_handle, class_ok := root_symbol_in_unit(&root_lookup, def_unit, .Type, class_name)
				if !class_ok || !unit_has_class_definition(&units[unit_id_index(def_unit)], class_handle.symbol) {
					continue
				}
				member := unit_class_member(
					&units[unit_id_index(def_unit)],
					class_handle.symbol,
					method_symbol.name,
				)
				if member != nil && member.kind == .Method && !(.Has_Implementation in member.flags) {
					member.implementation = Class_Member_Implementation_Data {
						unit = units[impl_unit_index].unit_id,
						range = method_symbol.decl_range,
					}
					member.implementation_range = method_symbol.decl_range
					member.flags += {.Has_Implementation, .Has_Implementation_Range}
					break
				}
			}
		}
	}
}

reclassify_project_open_sql_predicate_host_variables :: proc(
	units: []Unit_Analysis,
	allocator: mem.Allocator,
) {
	roots := make([dynamic]Symbol_Handle, 0, 8, allocator)
	names := make([dynamic]string, 0, 8, allocator)
	for unit in units {
		for s in unit.symbols {
			if s.scope == unit.root_scope && symbol_kind_occupies(s.kind, .Value) {
				if !string_list_contains(names[:], s.name) {
					append(&names, s.name)
					append(&roots, Symbol_Handle{unit = unit.unit_id, symbol = s.id})
				}
			}
		}
	}
	for unit_index in 0 ..< len(units) {
		next_refs := make([dynamic]Sql_Name_Ref_Data, 0, len(units[unit_index].sql_name_refs), allocator)
		for sql_ref in units[unit_index].sql_name_refs {
			if sql_ref.kind == .Column &&
			   sql_ref_in_predicate(units[unit_index].sql_predicates[:], sql_ref) {
				if root_index := string_list_index(names[:], sql_ref.name); root_index >= 0 {
					add_reclassified_sql_reference(&units[unit_index], sql_ref, roots[root_index])
					continue
				}
			}
			append(&next_refs, sql_ref)
		}
		units[unit_index].sql_name_refs = next_refs
	}
}

sql_ref_in_predicate :: proc(predicates: []Sql_Predicate_Data, ref: Sql_Name_Ref_Data) -> bool {
	for predicate in predicates {
		if predicate.query_id == ref.query_id &&
		   predicate.range.start <= ref.range.start &&
		   ref.range.end <= predicate.range.end {
			return true
		}
	}
	return false
}

add_reclassified_sql_reference :: proc(
	unit: ^Unit_Analysis,
	sql_ref: Sql_Name_Ref_Data,
	handle: Symbol_Handle,
) {
	for ref in unit.references {
		if ref.namespace == .Value &&
		   ref.kind == .Identifier &&
		   ref.range == sql_ref.range &&
		   ref.name == sql_ref.name {
			return
		}
	}
	id := Reference_Id(u32(len(unit.references)))
	append(
		&unit.references,
		Reference_Data {
			id = id,
			name = sql_ref.name,
			namespace = .Value,
			kind = .Identifier,
			scope = sql_ref.scope,
			range = sql_ref.range,
			resolution = Resolution{kind = .Symbol, symbol = handle},
			has_resolution = true,
		},
	)
}

string_list_contains :: proc(values: []string, name: string) -> bool {
	return string_list_index(values, name) >= 0
}

string_list_index :: proc(values: []string, name: string) -> int {
	for value, i in values {
		if value == name {
			return i
		}
	}
	return -1
}

uri_parent_dir_key :: proc(uri: string, allocator: mem.Allocator) -> string {
	normalized := normalized_uri_path_key(uri, allocator)
	for i := len(normalized) - 1; i >= 0; i -= 1 {
		if normalized[i] == '/' {
			return normalized[:i]
		}
	}
	return ""
}

normalized_uri_path_key :: proc(uri: string, allocator: mem.Allocator) -> string {
	end := len(uri)
	for end > 0 && (uri[end - 1] == '/' || uri[end - 1] == '\\') {
		end -= 1
	}
	out := make([]byte, end, allocator)
	for i in 0 ..< end {
		ch := uri[i]
		if ch == '\\' {
			ch = '/'
		}
		if 'A' <= ch && ch <= 'Z' {
			ch += 'a' - 'A'
		}
		out[i] = ch
	}
	return string(out)
}
