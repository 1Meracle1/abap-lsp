package abap_frontend_semantic2

import "src:ast"
import string_interner "src:string_interner"

import "core:mem"
import "core:strings"

Workspace_Input :: struct {
	files:               []Workspace_File_Input,
	external:            ^External_Semantics,
	external_sources:    []External_Source_Input,
	external_interfaces: []External_Interface_Input,
	interner:            ^string_interner.Interner,
}

Workspace_File_Project_Usage :: struct {
	path:     string,
	projects: [dynamic]^Project,
}

Workspace_Project_Result :: struct {
	project:   ^Project,
	checker:   ^Checker,
	root_path: string,
	kind:      Workspace_Project_Kind,
	files:     [dynamic]^Project_File,
	record_id: Semantic_Project_Id,
}

Workspace_Analysis :: struct {
	allocator:             mem.Allocator,
	interner:              ^string_interner.Interner,
	owns_interner:         bool,
	external_context:      ^External_Semantics,
	owns_external_context: bool,
	discovery:             Project_Discovery,
	projects:              [dynamic]^Project,
	project_results:       [dynamic]Workspace_Project_Result,
	external_index:        External_Semantic_Index,
	file_projects:         [dynamic]Workspace_File_Project_Usage,
	unresolved:            [dynamic]Checker_Unresolved_Candidate,
	workspace_diags:       [dynamic]Checker_Diagnostic,
	external_requests:     [dynamic]Checker_Unresolved_Candidate,
}

Workspace_Project_Build_State :: struct {
	analysis:       ^Workspace_Analysis,
	project_result: ^Workspace_Project_Result,
	discovery:      ^Project_Discovery,
	checker:        ^Checker,
	project:        ^Project,
	root_scope:     ^Scope,
	local_files:    [dynamic]^Project_File,
	external_files: [dynamic]^Project_File,
	stack:          [dynamic]int,
}

semantic_workspace_analyze :: proc(
	input: Workspace_Input,
	allocator: mem.Allocator = context.allocator,
) -> Workspace_Analysis {
	interner := input.interner
	owns_interner := false
	if interner == nil {
		interner = string_interner.create()
		owns_interner = true
	}
	external, owns_external := semantic_workspace_prepare_external_context(
		input,
		interner,
		allocator,
	)
	provider_context := external
	owns_provider_context := owns_external
	if input.external != nil || provider_context == nil {
		provider_context = semantic_workspace_provider_context(external, interner, allocator)
		owns_provider_context = true
	}
	analysis := Workspace_Analysis {
		allocator             = allocator,
		interner              = interner,
		owns_interner         = owns_interner,
		external_context      = provider_context,
		owns_external_context = owns_provider_context,
		projects              = make([dynamic]^Project, 0, len(input.files), allocator),
		project_results       = make(
			[dynamic]Workspace_Project_Result,
			0,
			len(input.files),
			allocator,
		),
		external_index        = external_semantic_index_make(interner, allocator),
		file_projects         = make(
			[dynamic]Workspace_File_Project_Usage,
			0,
			len(input.files),
			allocator,
		),
		unresolved            = make([dynamic]Checker_Unresolved_Candidate, 0, 8, allocator),
		workspace_diags       = make([dynamic]Checker_Diagnostic, 0, 4, allocator),
		external_requests     = make([dynamic]Checker_Unresolved_Candidate, 0, 8, allocator),
	}
	if external != nil {
		external_semantic_index_import_providers(&analysis.external_index, &external.index)
		semantic_workspace_import_external_interface_records(&analysis, external)
	}
	analysis.discovery = project_discovery_build(
		input.files,
		interner,
		provider_context,
		allocator,
	)
	semantic_workspace_add_local_root_providers(provider_context, &analysis.discovery)
	for diagnostic in analysis.discovery.diagnostics {
		copied := diagnostic
		copied.message = strings.clone(diagnostic.message, allocator) if diagnostic.message != "" else ""
		append(&analysis.workspace_diags, copied)
	}
	for plan in analysis.discovery.plans {
		semantic_workspace_build_project(&analysis, plan, provider_context)
	}
	return analysis
}

semantic_workspace_provider_context :: proc(
	external: ^External_Semantics,
	interner: ^string_interner.Interner,
	allocator: mem.Allocator,
) -> ^External_Semantics {
	provider := new(External_Semantics, allocator)
	assert(provider != nil)
	provider^ = external_semantics_make(interner, allocator)
	if external == nil {
		return provider
	}
	external_semantic_index_import_providers(&provider.index, &external.index)
	for source in external.source_files {
		names := make([dynamic]string_interner.String, 0, len(source.provided_names), allocator)
		for name in source.provided_names {
			append(&names, name)
		}
		append(
			&provider.source_files,
			External_Source_File {
				path = strings.clone(source.path, allocator) if source.path != "" else "",
				root = source.root,
				provided_names = names,
			},
		)
	}
	return provider
}

semantic_workspace_add_local_root_providers :: proc(
	external: ^External_Semantics,
	discovery: ^Project_Discovery,
) {
	assert(external != nil && discovery != nil)
	for plan in discovery.plans {
		if plan.kind != .Root {
			continue
		}
		facts := &discovery.facts[plan.root_index]
		if facts.root == nil {
			continue
		}
		role, role_ok := semantic_workspace_external_role_for_file_kind(facts.kind)
		if !role_ok {
			continue
		}
		key := semantic_workspace_root_object_key(facts, plan.kind)
		if !semantic_object_key_is_valid(key) {
			continue
		}
		_ = external_semantics_analyze_interface_input(
			external,
			External_Interface_Input{key = key, path = facts.path, root = facts.root, role = role},
		)
	}
}

semantic_workspace_external_role_for_file_kind :: proc(
	kind: Workspace_File_Kind,
) -> (
	External_Interface_Object_Role,
	bool,
) {
	#partial switch kind {
	case .Report:
		return .Report, true
	case .Class:
		return .Class, true
	case .Interface:
		return .Interface, true
	case .Type_Pool:
		return .Type_Pool, true
	case:
	}
	return .Unknown, false
}

semantic_workspace_prepare_external_context :: proc(
	input: Workspace_Input,
	interner: ^string_interner.Interner,
	allocator: mem.Allocator,
) -> (
	^External_Semantics,
	bool,
) {
	external := input.external
	owns_external := false
	if (len(input.external_sources) > 0 || len(input.external_interfaces) > 0) && external == nil {
		external = new(External_Semantics, allocator)
		assert(external != nil)
		external^ = external_semantics_make(interner, allocator)
		owns_external = true
	}
	if external != nil {
		for source_input in input.external_sources {
			_ = external_semantics_upsert_source_input(external, source_input)
		}
		for interface_input in input.external_interfaces {
			_ = external_semantics_analyze_interface_input(external, interface_input)
		}
	}
	return external, owns_external
}

semantic_workspace_import_external_interface_records :: proc(
	analysis: ^Workspace_Analysis,
	external: ^External_Semantics,
) {
	if analysis == nil || external == nil {
		return
	}
	external_semantic_index_import_external_project_records(
		&analysis.external_index,
		&external.index,
	)
	for record in external.index.projects {
		if record.role != .External_Interface || !semantic_object_key_is_valid(record.root_key) {
			continue
		}
		for candidate in record.unresolved {
			checker_add_unresolved_candidate_to_list(&analysis.unresolved, candidate)
			checker_add_unresolved_candidate_to_list(&analysis.external_requests, candidate)
		}
	}
}

semantic_workspace_analysis_destroy :: proc(analysis: ^Workspace_Analysis) {
	if analysis == nil {
		return
	}
	project_discovery_destroy(&analysis.discovery)
	for result in analysis.project_results {
		if result.project != nil {
			project_destroy(result.project)
			free(result.project, analysis.allocator)
		}
		if result.checker != nil {
			free(result.checker, analysis.allocator)
		}
		if result.root_path != "" {
			delete(result.root_path, analysis.allocator)
		}
		if result.files.allocator.procedure != nil {
			delete(result.files)
		}
	}
	if analysis.projects.allocator.procedure != nil {
		delete(analysis.projects)
	}
	if analysis.project_results.allocator.procedure != nil {
		delete(analysis.project_results)
	}
	external_semantic_index_destroy(&analysis.external_index)
	for &usage in analysis.file_projects {
		if usage.path != "" {
			delete(usage.path, analysis.allocator)
		}
		if usage.projects.allocator.procedure != nil {
			delete(usage.projects)
		}
	}
	if analysis.file_projects.allocator.procedure != nil {
		delete(analysis.file_projects)
	}
	if analysis.unresolved.allocator.procedure != nil {
		delete(analysis.unresolved)
	}
	checker_diagnostic_list_destroy(&analysis.workspace_diags, analysis.allocator)
	if analysis.external_requests.allocator.procedure != nil {
		delete(analysis.external_requests)
	}
	if analysis.owns_external_context && analysis.external_context != nil {
		external_semantics_destroy(analysis.external_context)
		free(analysis.external_context, analysis.allocator)
	}
	if analysis.owns_interner {
		string_interner.destroy(analysis.interner)
	}
	analysis^ = {}
}

checker_diagnostic_list_destroy :: proc(
	diagnostics: ^[dynamic]Checker_Diagnostic,
	allocator: mem.Allocator,
) {
	if diagnostics == nil || diagnostics.allocator.procedure == nil {
		return
	}
	for &diagnostic in diagnostics^ {
		if diagnostic.message != "" {
			delete(diagnostic.message, allocator)
		}
	}
	delete(diagnostics^)
	diagnostics^ = nil
}

semantic_workspace_projects_for_file :: proc(
	analysis: ^Workspace_Analysis,
	path: string,
) -> []^Project {
	for usage in analysis.file_projects {
		if usage.path == path {
			return usage.projects[:]
		}
	}
	return nil
}

semantic_workspace_build_project :: proc(
	analysis: ^Workspace_Analysis,
	plan: Workspace_Project_Plan,
	external: ^External_Semantics,
) {
	project := new(Project, analysis.allocator)
	assert(project != nil)
	project^ = project_make_with_interner(analysis.interner)

	checker := new(Checker, analysis.allocator)
	assert(checker != nil)
	checker_init_with_builtins(
		checker,
		project,
		external,
		external_semantics_builtin_scope(external) if external != nil else nil,
	)

	root_facts := &analysis.discovery.facts[plan.root_index]
	root_file := checker_add_file(checker, root_facts.path, root_facts.root)
	result := Workspace_Project_Result {
		project   = project,
		checker   = checker,
		root_path = strings.clone(root_facts.path, analysis.allocator) if root_facts.path != "" else "",
		kind      = plan.kind,
		files     = make([dynamic]^Project_File, 0, 4, analysis.allocator),
	}
	append(&result.files, root_file)
	append(&analysis.project_results, result)
	project_result := &analysis.project_results[len(analysis.project_results) - 1]

	state := Workspace_Project_Build_State {
		analysis       = analysis,
		project_result = project_result,
		discovery      = &analysis.discovery,
		checker        = checker,
		project        = project,
		root_scope     = root_file.root_scope,
		local_files    = make(
			[dynamic]^Project_File,
			len(analysis.discovery.facts),
			len(analysis.discovery.facts),
			project.allocator,
		),
		external_files = make(
			[dynamic]^Project_File,
			len(external.source_files) if external != nil else 0,
			len(external.source_files) if external != nil else 0,
			project.allocator,
		),
		stack          = make([dynamic]int, 0, 8, project.allocator),
	}
	state.local_files[plan.root_index] = root_file

	ctx := checker_context_make(checker, root_file)
	workspace_collect_expanded_file_entities(&state, &ctx, plan.root_index)
	checker_check_queued_entities(&ctx)
	workspace_check_expanded_file_stmts(&state, &ctx, plan.root_index)

	append(&analysis.projects, project)
	for file in project_result.files {
		semantic_workspace_add_file_project_usage(analysis, file.path, project)
	}
	for candidate in checker.info.unresolved {
		checker_add_unresolved_candidate_to_list(&analysis.unresolved, candidate)
		checker_add_unresolved_candidate_to_list(&analysis.external_requests, candidate)
	}
	semantic_workspace_record_project_dependencies(analysis, project_result, plan)
}

semantic_workspace_record_project_dependencies :: proc(
	analysis: ^Workspace_Analysis,
	project_result: ^Workspace_Project_Result,
	plan: Workspace_Project_Plan,
) {
	assert(
		analysis != nil &&
		project_result != nil &&
		project_result.project != nil &&
		project_result.checker != nil,
	)
	root_facts := &analysis.discovery.facts[plan.root_index]
	root_key := semantic_workspace_root_object_key(root_facts, plan.kind)
	record := semantic_project_record_make(
		&analysis.external_index,
		semantic_workspace_project_role(plan.kind),
		project_result.project,
		project_result.checker,
		root_key,
	)
	semantic_workspace_add_project_provides(&record, root_facts, plan.kind)
	for edge in project_result.checker.info.resolved_external_dependencies {
		semantic_project_record_add_dependency(&record, edge)
	}
	for edge in project_result.checker.info.unresolved_external_dependencies {
		semantic_project_record_add_dependency(&record, edge)
	}
	for candidate in project_result.checker.info.unresolved {
		checker_add_unresolved_candidate_to_list(&record.unresolved, candidate)
	}
	stored := external_semantic_index_add_project_record(&analysis.external_index, record)
	project_result.record_id = stored.id
	for edge in stored.resolved_dependencies {
		external_semantic_index_add_dependency(&analysis.external_index, stored.id, edge)
	}
	for edge in stored.unresolved_dependencies {
		external_semantic_index_add_dependency(&analysis.external_index, stored.id, edge)
	}
}

semantic_workspace_add_project_provides :: proc(
	record: ^Semantic_Project_Record,
	facts: ^Workspace_File_Facts,
	project_kind: Workspace_Project_Kind,
) {
	key_kind := semantic_workspace_object_kind_for_project(facts.kind, project_kind)
	for name in facts.provided_names {
		semantic_project_record_add_provide(
			record,
			Semantic_Object_Key{kind = key_kind, name = name},
		)
	}
}

semantic_workspace_root_object_key :: proc(
	facts: ^Workspace_File_Facts,
	project_kind: Workspace_Project_Kind,
) -> Semantic_Object_Key {
	if facts == nil || len(facts.provided_names) == 0 {
		return {}
	}
	return Semantic_Object_Key {
		kind = semantic_workspace_object_kind_for_project(facts.kind, project_kind),
		name = facts.provided_names[0],
	}
}

semantic_workspace_object_kind_for_project :: proc(
	file_kind: Workspace_File_Kind,
	project_kind: Workspace_Project_Kind,
) -> External_Candidate_Kind {
	if project_kind == .Include_Fragment {
		return .Include_Source
	}
	#partial switch file_kind {
	case .Report:
		return .Report
	case .Class:
		return .Class
	case .Interface:
		return .Interface
	case .Type_Pool:
		return .Type_Pool
	case:
	}
	return .Global_Symbol
}

semantic_workspace_project_role :: proc(kind: Workspace_Project_Kind) -> Semantic_Project_Role {
	if kind == .Include_Fragment {
		return .Include_Fragment
	}
	return .Editable_Root
}

workspace_collect_expanded_file_entities :: proc(
	state: ^Workspace_Project_Build_State,
	ctx: ^Checker_Context,
	file_index: int,
) {
	if workspace_stack_contains(state.stack[:], file_index) {
		workspace_add_project_diagnostic(state, ctx, .Include_Cycle, {}, "include cycle")
		return
	}
	append(&state.stack, file_index)
	file := workspace_project_file_for_local(state, file_index)
	previous_file := ctx.file
	previous_scope := ctx.scope
	checker_context_set_file(ctx, file)
	ctx.scope = state.root_scope
	defer {
		ctx.file = previous_file
		ctx.scope = previous_scope
		_ = pop(&state.stack)
	}

	facts := &state.discovery.facts[file_index]
	if facts.root == nil {
		return
	}
	for stmt in facts.root.stmts {
		if include, ok := stmt.derived_stmt.(^ast.Include_Stmt); ok {
			checker_collect_include_stmt(ctx, include)
			workspace_collect_include_targets(state, ctx, facts, include)
			continue
		}
		checker_collect_stmt_entities(ctx, stmt)
		workspace_collect_type_pool_candidates(state, ctx, stmt)
	}
}

workspace_collect_include_targets :: proc(
	state: ^Workspace_Project_Build_State,
	ctx: ^Checker_Context,
	facts: ^Workspace_File_Facts,
	include: ^ast.Include_Stmt,
) {
	for include_name in include.names {
		name := checker_intern_name(ctx.project, include_name.name.text)
		edge, edge_ok := project_discovery_edge_for_include(facts, name, include_name.name.range)
		if !edge_ok || !edge.has_target {
			workspace_note_unresolved_include(
				state,
				ctx,
				name,
				include_name.name.range,
				include,
				include.if_found,
			)
			continue
		}
		if edge.is_external {
			target := workspace_project_file_for_external(state, edge.external_index)
			workspace_set_include_entity_target(ctx, name, target)
			workspace_collect_external_source_entities(state, ctx, edge.external_index)
			continue
		}
		target := workspace_project_file_for_local(state, edge.target_index)
		workspace_set_include_entity_target(ctx, name, target)
		if state.discovery.facts[edge.target_index].explicit_root {
			workspace_add_project_diagnostic(
				state,
				ctx,
				.Root_File_Included,
				include_name.name.range,
				"include target is an explicit root",
			)
		}
		if workspace_stack_contains(state.stack[:], edge.target_index) {
			workspace_add_project_diagnostic(
				state,
				ctx,
				.Include_Cycle,
				include_name.name.range,
				"include cycle",
			)
			continue
		}
		workspace_collect_expanded_file_entities(state, ctx, edge.target_index)
	}
}

workspace_collect_external_source_entities :: proc(
	state: ^Workspace_Project_Build_State,
	ctx: ^Checker_Context,
	external_index: int,
) {
	file := workspace_project_file_for_external(state, external_index)
	previous_file := ctx.file
	previous_scope := ctx.scope
	checker_context_set_file(ctx, file)
	ctx.scope = state.root_scope
	defer {
		ctx.file = previous_file
		ctx.scope = previous_scope
	}
	if file.root == nil {
		return
	}
	for stmt in file.root.stmts {
		if include, ok := stmt.derived_stmt.(^ast.Include_Stmt); ok {
			checker_collect_include_stmt(ctx, include)
			for include_name in include.names {
				name := checker_intern_name(ctx.project, include_name.name.text)
				if target_index, local_ok := project_discovery_find_local_provided_name(
					state.discovery,
					name,
				); local_ok {
					target := workspace_project_file_for_local(state, target_index)
					workspace_set_include_entity_target(ctx, name, target)
					workspace_collect_expanded_file_entities(state, ctx, target_index)
					continue
				}
				if next_external, external_ok := project_discovery_find_external_source_name(
					state.discovery,
					name,
				); external_ok {
					target := workspace_project_file_for_external(state, next_external)
					workspace_set_include_entity_target(ctx, name, target)
					workspace_collect_external_source_entities(state, ctx, next_external)
					continue
				}
				workspace_note_unresolved_include(
					state,
					ctx,
					name,
					include_name.name.range,
					include,
					include.if_found,
				)
			}
			continue
		}
		checker_collect_stmt_entities(ctx, stmt)
		workspace_collect_type_pool_candidates(state, ctx, stmt)
	}
}

workspace_check_expanded_file_stmts :: proc(
	state: ^Workspace_Project_Build_State,
	ctx: ^Checker_Context,
	file_index: int,
) {
	file := workspace_project_file_for_local(state, file_index)
	previous_file := ctx.file
	previous_scope := ctx.scope
	checker_context_set_file(ctx, file)
	ctx.scope = state.root_scope
	defer {
		ctx.file = previous_file
		ctx.scope = previous_scope
	}

	facts := &state.discovery.facts[file_index]
	if facts.root == nil {
		return
	}
	for stmt in facts.root.stmts {
		if include, ok := stmt.derived_stmt.(^ast.Include_Stmt); ok {
			workspace_check_include_targets(state, ctx, facts, include)
			continue
		}
		checker_check_stmt(ctx, stmt, collect_declarations = false)
	}
}

workspace_check_include_targets :: proc(
	state: ^Workspace_Project_Build_State,
	ctx: ^Checker_Context,
	facts: ^Workspace_File_Facts,
	include: ^ast.Include_Stmt,
) {
	for include_name in include.names {
		name := checker_intern_name(ctx.project, include_name.name.text)
		edge, edge_ok := project_discovery_edge_for_include(facts, name, include_name.name.range)
		if !edge_ok || !edge.has_target {
			continue
		}
		if edge.is_external {
			workspace_check_external_source_stmts(state, ctx, edge.external_index)
			continue
		}
		if workspace_stack_contains(state.stack[:], edge.target_index) {
			continue
		}
		append(&state.stack, edge.target_index)
		workspace_check_expanded_file_stmts(state, ctx, edge.target_index)
		_ = pop(&state.stack)
	}
}

workspace_check_external_source_stmts :: proc(
	state: ^Workspace_Project_Build_State,
	ctx: ^Checker_Context,
	external_index: int,
) {
	file := workspace_project_file_for_external(state, external_index)
	previous_file := ctx.file
	previous_scope := ctx.scope
	checker_context_set_file(ctx, file)
	ctx.scope = state.root_scope
	defer {
		ctx.file = previous_file
		ctx.scope = previous_scope
	}
	if file.root == nil {
		return
	}
	for stmt in file.root.stmts {
		if include, ok := stmt.derived_stmt.(^ast.Include_Stmt); ok {
			for include_name in include.names {
				name := checker_intern_name(ctx.project, include_name.name.text)
				if target_index, local_ok := project_discovery_find_local_provided_name(
					state.discovery,
					name,
				); local_ok {
					workspace_check_expanded_file_stmts(state, ctx, target_index)
					continue
				}
				if next_external, external_ok := project_discovery_find_external_source_name(
					state.discovery,
					name,
				); external_ok {
					workspace_check_external_source_stmts(state, ctx, next_external)
				}
			}
			continue
		}
		checker_check_stmt(ctx, stmt, collect_declarations = false)
	}
}

workspace_collect_type_pool_candidates :: proc(
	state: ^Workspace_Project_Build_State,
	ctx: ^Checker_Context,
	stmt: ^ast.Stmt,
) {
	if pools, pools_ok := stmt.derived_stmt.(^ast.Type_Pools_Decl); pools_ok {
		for pool in pools.pools {
			name := checker_intern_name(ctx.project, pool.text)
			if !string_interner.is_valid(name) {
				continue
			}
			if ctx.info.external != nil {
				if _, external_ok := external_semantics_lookup(
					ctx.info.external,
					.Type,
					name,
					.Type_Pool,
				); external_ok {
					continue
				}
			}
			checker_add_unresolved_candidate(
				ctx,
				name,
				.Type,
				.Type_Pool,
				.Type_Pool_Statement,
				.Type_Pool_Import,
				stmt.range,
				&stmt.stmt_base,
			)
		}
	}
}

workspace_project_file_for_local :: proc(
	state: ^Workspace_Project_Build_State,
	file_index: int,
) -> ^Project_File {
	if existing := state.local_files[file_index]; existing != nil {
		return existing
	}
	facts := &state.discovery.facts[file_index]
	file := project_add_file(state.project, facts.path, facts.root)
	file.root_scope = state.root_scope
	checker_register_file(state.checker, file)
	state.local_files[file_index] = file
	append(&state.project_result.files, file)
	return file
}

workspace_project_file_for_external :: proc(
	state: ^Workspace_Project_Build_State,
	external_index: int,
) -> ^Project_File {
	if existing := state.external_files[external_index]; existing != nil {
		return existing
	}
	source := &state.discovery.external.source_files[external_index]
	file := project_add_file(state.project, source.path, source.root)
	file.root_scope = state.root_scope
	checker_register_file(state.checker, file)
	state.external_files[external_index] = file
	append(&state.project_result.files, file)
	return file
}

workspace_set_include_entity_target :: proc(
	ctx: ^Checker_Context,
	name: string_interner.String,
	target: ^Project_File,
) {
	if !string_interner.is_valid(name) || target == nil {
		return
	}
	if entity, ok := scope_lookup_declaration(ctx.scope, .Value, name);
	   ok && entity.kind == .Include {
		payload, payload_ok := entity.payload.(^Entity_Include_Payload)
		assert(payload_ok && payload != nil)
		payload.target = target
		payload.has_target = true
	}
}

workspace_note_unresolved_include :: proc(
	state: ^Workspace_Project_Build_State,
	ctx: ^Checker_Context,
	name: string_interner.String,
	range: Range,
	stmt: ^ast.Include_Stmt,
	if_found: bool,
) {
	kind := Checker_Diagnostic_Kind.Unresolved_Include
	severity := Checker_Diagnostic_Severity.Error
	if if_found {
		kind = .Unresolved_Include_If_Found
		severity = .Note
	}
	checker_add_diagnostic(ctx, kind, range, "unresolved include", severity = severity)
	checker_add_unresolved_candidate(
		ctx,
		name,
		.Value,
		.Include_Source,
		.Include_Statement,
		.Unresolved_Include,
		range,
		&stmt.node.stmt_base if stmt != nil else nil,
		if_found,
	)
}

workspace_add_project_diagnostic :: proc(
	state: ^Workspace_Project_Build_State,
	ctx: ^Checker_Context,
	kind: Checker_Diagnostic_Kind,
	range: Range,
	message: string,
	severity: Checker_Diagnostic_Severity = .Error,
) {
	checker_add_diagnostic(ctx, kind, range, message, severity = severity)
	append(
		&state.analysis.workspace_diags,
		Checker_Diagnostic {
			kind = kind,
			severity = severity,
			range = range,
			message = strings.clone(message, state.analysis.allocator) if message != "" else "",
		},
	)
}

semantic_workspace_add_file_project_usage :: proc(
	analysis: ^Workspace_Analysis,
	path: string,
	project: ^Project,
) {
	for &usage in analysis.file_projects {
		if usage.path != path {
			continue
		}
		for existing in usage.projects {
			if existing == project {
				return
			}
		}
		append(&usage.projects, project)
		return
	}
	projects := make([dynamic]^Project, 0, 2, analysis.allocator)
	append(&projects, project)
	append(
		&analysis.file_projects,
		Workspace_File_Project_Usage {
			path = strings.clone(path, analysis.allocator) if path != "" else "",
			projects = projects,
		},
	)
}

workspace_stack_contains :: proc(stack: []int, value: int) -> bool {
	for item in stack {
		if item == value {
			return true
		}
	}
	return false
}
