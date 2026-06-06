package abap_frontend_semantic2

import "src:parser"
import string_interner "src:string_interner"

import "core:testing"

workspace_test_file :: proc(
	t: ^testing.T,
	path: string,
	source: string,
	kind: Workspace_File_Kind = .Unknown,
	object_name: string = "",
) -> Workspace_File_Input {
	parsed := parser.parse(source, path, context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	return Workspace_File_Input {
		path        = path,
		root        = parsed.root,
		kind        = kind,
		object_name = object_name,
	}
}

workspace_test_lookup :: proc(
	t: ^testing.T,
	analysis: ^Workspace_Analysis,
	scope: ^Scope,
	namespace: Namespace,
	name: string,
	kind: Entity_Kind,
) -> ^Entity {
	interned := string_interner.insert(analysis.interner, name)
	_, entity, ok := checker_lookup_declaration_from_scope(scope, namespace, interned)
	testing.expect(t, ok)
	if ok {
		testing.expect_value(t, entity.kind, kind)
	}
	return entity
}

workspace_test_definition_index :: proc(scope: ^Scope, entity: ^Entity) -> int {
	for item, index in scope.declarations {
		if item == entity {
			return index
		}
	}
	return -1
}

workspace_test_candidate_count :: proc(
	analysis: ^Workspace_Analysis,
	kind: External_Candidate_Kind,
	name: string,
) -> int {
	interned := string_interner.insert(analysis.interner, name)
	count := 0
	for candidate in analysis.unresolved {
		if candidate.kind == kind && candidate.name == interned {
			count += 1
		}
	}
	return count
}

workspace_test_candidate_count_in :: proc(
	interner: ^string_interner.Interner,
	candidates: []Checker_Unresolved_Candidate,
	kind: External_Candidate_Kind,
	name: string,
) -> int {
	interned := string_interner.insert(interner, name)
	count := 0
	for candidate in candidates {
		if candidate.kind == kind && candidate.name == interned {
			count += 1
		}
	}
	return count
}

workspace_test_external_interface_input :: proc(
	t: ^testing.T,
	interner: ^string_interner.Interner,
	kind: External_Candidate_Kind,
	role: External_Interface_Object_Role,
	name: string,
	path: string,
	source: string,
	generation: u64 = 0,
) -> External_Interface_Input {
	parsed := parser.parse(source, path, context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	return External_Interface_Input {
		key        = Semantic_Object_Key{kind = kind, name = string_interner.insert(interner, name)},
		path       = path,
		root       = parsed.root,
		generation = generation,
		role       = role,
	}
}

workspace_test_external_source_input :: proc(
	t: ^testing.T,
	path: string,
	source: string,
	provided_names: []string,
	generation: u64 = 0,
) -> External_Source_Input {
	parsed := parser.parse_with_diagnostic_policy(
		source,
		path,
		context.allocator,
		.Include_Fragment,
	)
	testing.expect_value(t, len(parsed.errors), 0)
	return External_Source_Input {
		path           = path,
		root           = parsed.root,
		provided_names = provided_names,
		source_hash    = 1,
		generation     = generation,
	}
}

workspace_test_record_for_project :: proc(
	t: ^testing.T,
	analysis: ^Workspace_Analysis,
	project: ^Project,
) -> ^Semantic_Project_Record {
	for &record in analysis.external_index.projects {
		if record.project == project {
			return &record
		}
	}
	testing.expect(t, false)
	return nil
}

workspace_test_dependency_edge_count :: proc(
	interner: ^string_interner.Interner,
	edges: []Semantic_Dependency_Edge,
	kind: External_Candidate_Kind,
	name: string,
) -> int {
	key := Semantic_Object_Key{kind = kind, name = string_interner.insert(interner, name)}
	count := 0
	for edge in edges {
		if edge.key == key {
			count += 1
		}
	}
	return count
}

workspace_test_reverse_project_count :: proc(
	interner: ^string_interner.Interner,
	waiters: map[Semantic_Object_Key][dynamic]Semantic_Project_Id,
	kind: External_Candidate_Kind,
	name: string,
	project_id: Semantic_Project_Id,
) -> int {
	key := Semantic_Object_Key{kind = kind, name = string_interner.insert(interner, name)}
	count := 0
	if projects, ok := waiters[key]; ok {
		for id in projects {
			if id == project_id {
				count += 1
			}
		}
	}
	return count
}

workspace_test_reverse_total_count :: proc(
	interner: ^string_interner.Interner,
	waiters: map[Semantic_Object_Key][dynamic]Semantic_Project_Id,
	kind: External_Candidate_Kind,
	name: string,
) -> int {
	key := Semantic_Object_Key{kind = kind, name = string_interner.insert(interner, name)}
	if projects, ok := waiters[key]; ok {
		return len(projects)
	}
	return 0
}

workspace_test_graph_result_candidate_count :: proc(
	interner: ^string_interner.Interner,
	candidates: []Checker_Unresolved_Candidate,
	kind: External_Candidate_Kind,
	name: string,
) -> int {
	interned := string_interner.insert(interner, name)
	count := 0
	for candidate in candidates {
		if candidate.kind == kind && candidate.name == interned {
			count += 1
		}
	}
	return count
}

workspace_test_graph_project_path_count :: proc(
	projects: []Semantic_Graph_Project_Ref,
	path: string,
) -> int {
	count := 0
	for project in projects {
		if project.root_path == path {
			count += 1
		}
	}
	return count
}

workspace_test_graph_external_key_count :: proc(
	interner: ^string_interner.Interner,
	keys: []Semantic_Object_Key,
	kind: External_Candidate_Kind,
	name: string,
) -> int {
	key := Semantic_Object_Key{kind = kind, name = string_interner.insert(interner, name)}
	count := 0
	for existing in keys {
		if existing == key {
			count += 1
		}
	}
	return count
}

workspace_test_diagnostic_count :: proc(
	checker: ^Checker,
	kind: Checker_Diagnostic_Kind,
) -> int {
	count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == kind {
			count += 1
		}
	}
	return count
}

workspace_test_add_external_class_method_with_param :: proc(
	external: ^External_Semantics,
	class: ^Entity,
	method_name: string,
	param_name: string,
	param_type_name: string,
) -> ^Entity {
	assert(external != nil && class != nil && class.kind == .Class)
	class_payload, class_ok := class.payload.(^Entity_Object_Payload)
	assert(class_ok && class_payload != nil && class_payload.definition_scope != nil)

	method := external_new_entity(external, .Method)
	method.name = external_intern_name(external, method_name)
	method.state = .Resolved
	method.scope = class_payload.definition_scope
	method.owner = class
	method.member_kind = .Method
	method.visibility = .Public
	method.flags += {.Static}
	method_payload, method_ok := method.payload.(^Entity_Routine_Payload)
	assert(method_ok && method_payload != nil)
	signature_scope := external_new_scope(external, class_payload.definition_scope, .Method, method)
	method_payload.signature_scope = signature_scope
	method_payload.body_scope = signature_scope
	method_payload.visibility = .Public
	method_payload.is_static = true
	method.type = external_new_type(external, .Routine)
	method.type.routine.signature_scope = signature_scope
	_ = scope_insert_declaration(class_payload.definition_scope, method)

	param := external_new_entity(external, .Parameter)
	param.name = external_intern_name(external, param_name)
	param.state = .Resolved
	param.scope = signature_scope
	param.owner = method
	param.type = external_builtin_type(external, param_type_name)
	param.flags += {.Parameter, .Has_Declared_Type}
	param_payload, param_ok := param.payload.(^Entity_Variable_Payload)
	assert(param_ok && param_payload != nil)
	param_payload.section = .Method_Importing
	param_payload.passing = .Reference
	_ = scope_insert_declaration(signature_scope, param)
	append(&method_payload.parameters, param)
	append(&method.type.routine.parameters, param)
	return method
}

@(test)
semantic_graph_fetches_external_chain_before_rebuilding_editable_waiters :: proc(t: ^testing.T) {
	interner := string_interner.create()
	defer string_interner.destroy(interner)

	session := semantic_graph_session_make(interner)
	defer semantic_graph_session_destroy(&session)

	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", "REPORT zmain. DATA lo_a TYPE REF TO zcl_a."),
	}
	initial := semantic_graph_session_apply_update(
		&session,
		Semantic_Graph_Update {
			changed_files            = files[:],
			external_frontier_stable = false,
		},
	)
	defer semantic_graph_update_result_destroy(&initial)

	testing.expect_value(t, len(initial.rebuilt_editable_projects), 1)
	testing.expect_value(t, workspace_test_graph_result_candidate_count(interner, initial.new_fetch_requests[:], .Global_Symbol, "zcl_a"), 1)

	external_a := workspace_test_external_interface_input(
		t,
		interner,
		.Class,
		.Class,
		"zcl_a",
		"adt://zcl_a.class.abap",
		`CLASS zcl_a DEFINITION.
  PUBLIC SECTION.
    DATA child TYPE REF TO zcl_b.
ENDCLASS.`,
		2,
	)
	fetched_a_inputs := [?]External_Interface_Input{external_a}
	fetched_a := semantic_graph_session_apply_update(
		&session,
		Semantic_Graph_Update {
			fetched_external_objects = fetched_a_inputs[:],
			external_frontier_stable = false,
		},
	)
	defer semantic_graph_update_result_destroy(&fetched_a)

	analysis := semantic_graph_session_current_analysis(&session)
	testing.expect(t, analysis != nil)
	if analysis != nil {
		key_a := Semantic_Object_Key{kind = .Class, name = string_interner.insert(interner, "zcl_a")}
		_, provider_ok := analysis.external_index.providers[key_a]
		testing.expect(t, provider_ok)
		testing.expect_value(t, workspace_test_reverse_total_count(interner, analysis.external_index.unresolved_waiters_by_object, .Global_Symbol, "zcl_a"), 0)
	}
	testing.expect_value(t, workspace_test_graph_external_key_count(interner, fetched_a.rebuilt_external_projects[:], .Class, "zcl_a"), 1)
	testing.expect_value(t, workspace_test_graph_result_candidate_count(interner, fetched_a.new_fetch_requests[:], .Global_Symbol, "zcl_b"), 1)
	testing.expect_value(t, workspace_test_graph_project_path_count(fetched_a.dirty_editable_projects[:], "mem://zmain.report.abap"), 1)
	testing.expect_value(t, workspace_test_graph_project_path_count(fetched_a.deferred_editable_projects[:], "mem://zmain.report.abap"), 1)
	testing.expect_value(t, len(fetched_a.rebuilt_editable_projects), 0)

	external_b := workspace_test_external_interface_input(
		t,
		interner,
		.Class,
		.Class,
		"zcl_b",
		"adt://zcl_b.class.abap",
		"CLASS zcl_b DEFINITION. ENDCLASS.",
		3,
	)
	fetched_b_inputs := [?]External_Interface_Input{external_b}
	fetched_b := semantic_graph_session_apply_update(
		&session,
		Semantic_Graph_Update {
			fetched_external_objects = fetched_b_inputs[:],
			external_frontier_stable = true,
		},
	)
	defer semantic_graph_update_result_destroy(&fetched_b)

	testing.expect_value(t, workspace_test_graph_external_key_count(interner, fetched_b.rebuilt_external_projects[:], .Class, "zcl_b"), 1)
	testing.expect_value(t, workspace_test_graph_external_key_count(interner, fetched_b.rebuilt_external_projects[:], .Class, "zcl_a"), 1)
	testing.expect_value(t, workspace_test_graph_project_path_count(fetched_b.rebuilt_editable_projects[:], "mem://zmain.report.abap"), 1)
	testing.expect_value(t, len(fetched_b.deferred_editable_projects), 0)
	testing.expect_value(t, workspace_test_graph_result_candidate_count(interner, fetched_b.new_fetch_requests[:], .Global_Symbol, "zcl_b"), 0)

	analysis = semantic_graph_session_current_analysis(&session)
	testing.expect(t, analysis != nil)
	if analysis != nil {
		testing.expect_value(t, workspace_test_candidate_count(analysis, .Global_Symbol, "zcl_a"), 0)
		testing.expect_value(t, workspace_test_candidate_count(analysis, .Global_Symbol, "zcl_b"), 0)
		record := workspace_test_record_for_project(t, analysis, analysis.project_results[0].project)
		testing.expect(t, record != nil)
		if record != nil {
			testing.expect_value(t, workspace_test_dependency_edge_count(interner, record.resolved_dependencies[:], .Class, "zcl_a"), 1)
		}
	}
}

@(test)
semantic_graph_external_update_dirties_only_reverse_dependents :: proc(t: ^testing.T) {
	interner := string_interner.create()
	defer string_interner.destroy(interner)

	session := semantic_graph_session_make(interner)
	defer semantic_graph_session_destroy(&session)

	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", "REPORT zmain. DATA lo_a TYPE REF TO zcl_a."),
		workspace_test_file(t, "mem://zother.report.abap", "REPORT zother. DATA lo_other TYPE REF TO zcl_other."),
	}
	initial := semantic_graph_session_apply_update(
		&session,
		Semantic_Graph_Update {
			changed_files            = files[:],
			external_frontier_stable = false,
		},
	)
	defer semantic_graph_update_result_destroy(&initial)

	testing.expect_value(t, len(initial.rebuilt_editable_projects), 2)
	testing.expect_value(t, workspace_test_graph_result_candidate_count(interner, initial.new_fetch_requests[:], .Global_Symbol, "zcl_a"), 1)
	testing.expect_value(t, workspace_test_graph_result_candidate_count(interner, initial.new_fetch_requests[:], .Global_Symbol, "zcl_other"), 1)

	external_a := workspace_test_external_interface_input(
		t,
		interner,
		.Class,
		.Class,
		"zcl_a",
		"adt://zcl_a.class.abap",
		"CLASS zcl_a DEFINITION. ENDCLASS.",
		4,
	)
	fetched_a_inputs := [?]External_Interface_Input{external_a}
	fetched_a := semantic_graph_session_apply_update(
		&session,
		Semantic_Graph_Update {
			fetched_external_objects = fetched_a_inputs[:],
			external_frontier_stable = true,
		},
	)
	defer semantic_graph_update_result_destroy(&fetched_a)

	testing.expect_value(t, workspace_test_graph_project_path_count(fetched_a.dirty_editable_projects[:], "mem://zmain.report.abap"), 1)
	testing.expect_value(t, workspace_test_graph_project_path_count(fetched_a.dirty_editable_projects[:], "mem://zother.report.abap"), 0)
	testing.expect_value(t, workspace_test_graph_project_path_count(fetched_a.rebuilt_editable_projects[:], "mem://zmain.report.abap"), 1)
	testing.expect_value(t, workspace_test_graph_project_path_count(fetched_a.rebuilt_editable_projects[:], "mem://zother.report.abap"), 0)

	analysis := semantic_graph_session_current_analysis(&session)
	testing.expect(t, analysis != nil)
	if analysis != nil {
		testing.expect_value(t, workspace_test_candidate_count(analysis, .Global_Symbol, "zcl_a"), 0)
		testing.expect_value(t, workspace_test_candidate_count(analysis, .Global_Symbol, "zcl_other"), 1)
	}
}

@(test)
semantic_graph_fetched_external_source_expands_include_and_unblocks_frontier :: proc(t: ^testing.T) {
	interner := string_interner.create()
	defer string_interner.destroy(interner)

	session := semantic_graph_session_make(interner)
	defer semantic_graph_session_destroy(&session)

	files := [?]Workspace_File_Input {
		workspace_test_file(
			t,
			"mem://zmain.report.abap",
			"REPORT zmain. INCLUDE zinc_ext. WRITE gv_ext.",
		),
	}
	initial := semantic_graph_session_apply_update(
		&session,
		Semantic_Graph_Update {
			changed_files            = files[:],
			external_frontier_stable = false,
		},
	)
	defer semantic_graph_update_result_destroy(&initial)

	testing.expect_value(t, workspace_test_graph_result_candidate_count(interner, initial.new_fetch_requests[:], .Include_Source, "zinc_ext"), 1)

	source := workspace_test_external_source_input(
		t,
		"adt://zinc_ext.include.abap",
		"DATA gv_ext TYPE i.",
		{"zinc_ext"},
		2,
	)
	source_inputs := [?]External_Source_Input{source}
	fetched := semantic_graph_session_apply_update(
		&session,
		Semantic_Graph_Update {
			fetched_external_sources = source_inputs[:],
			external_frontier_stable = true,
		},
	)
	defer semantic_graph_update_result_destroy(&fetched)

	testing.expect_value(t, workspace_test_graph_project_path_count(fetched.rebuilt_editable_projects[:], "mem://zmain.report.abap"), 1)
	testing.expect_value(t, workspace_test_graph_result_candidate_count(interner, fetched.new_fetch_requests[:], .Include_Source, "zinc_ext"), 0)

	analysis := semantic_graph_session_current_analysis(&session)
	testing.expect(t, analysis != nil)
	if analysis != nil {
		testing.expect_value(t, workspace_test_candidate_count(analysis, .Include_Source, "zinc_ext"), 0)
	}
}

@(test)
semantic_external_summaries_publish_project_backed_provider_bindings :: proc(t: ^testing.T) {
	interner := string_interner.create()
	defer string_interner.destroy(interner)

	external := external_semantics_make(interner)
	defer external_semantics_destroy(&external)
	class := external_semantics_add_class_summary(&external, "zcl_dep")

	key, binding, ok := external_semantic_index_lookup(&external.index, .Type, class.name, .Class)
	testing.expect(t, ok)
	testing.expect_value(t, key.kind, External_Candidate_Kind.Class)
	testing.expect_value(t, binding.entity, class)
	testing.expect(t, semantic_project_id_is_valid(binding.project_id))
	testing.expect_value(t, len(external.index.projects), 1)
	testing.expect_value(t, external.index.projects[0].project, external.compat_project)
	testing.expect_value(t, class.source_file, external.compat_root_file)
}

@(test)
semantic_workspace_analyzes_external_class_interface_for_transitive_type_candidates :: proc(t: ^testing.T) {
	interner := string_interner.create()
	defer string_interner.destroy(interner)

	external_inputs := [?]External_Interface_Input {
		workspace_test_external_interface_input(
			t,
			interner,
			.Class,
			.Class,
			"zcl_dep",
			"adt://zcl_dep.class.abap",
			`CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING phase LIKE zif_base=>ty_phase value TYPE zmissing_domain.
ENDCLASS.`,
			7,
		),
	}
	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", "REPORT zmain. DATA lo_dep TYPE REF TO zcl_dep."),
	}

	analysis := semantic_workspace_analyze(Workspace_Input{files = files[:], external_interfaces = external_inputs[:], interner = interner})
	defer semantic_workspace_analysis_destroy(&analysis)

	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Global_Symbol, "zcl_dep"), 0)
	testing.expect(t, workspace_test_candidate_count(&analysis, .Global_Symbol, "zif_base") > 0)
	testing.expect(t, workspace_test_candidate_count(&analysis, .Global_Symbol, "zmissing_domain") > 0)

	key := Semantic_Object_Key{kind = .Class, name = string_interner.insert(interner, "zcl_dep")}
	binding, provider_ok := analysis.external_index.providers[key]
	testing.expect(t, provider_ok)
	if provider_ok {
		testing.expect_value(t, binding.generation, u64(7))
		testing.expect(t, binding.entity != nil && binding.entity.kind == .Class)
		record, record_ok := external_semantic_index_project_record(&analysis.external_index, binding.project_id)
		testing.expect(t, record_ok)
		if record_ok {
			testing.expect_value(t, record.role, Semantic_Project_Role.External_Interface)
			testing.expect_value(t, record.root_key, key)
			testing.expect(t, record.project != nil && record.checker != nil)
			testing.expect(t, workspace_test_candidate_count_in(interner, record.unresolved[:], .Global_Symbol, "zif_base") > 0)
			testing.expect(t, workspace_test_dependency_edge_count(interner, record.unresolved_dependencies[:], .Global_Symbol, "zmissing_domain") > 0)
		}
	}
}

@(test)
semantic_workspace_analyzes_external_ddic_table_for_field_backing_candidates :: proc(t: ^testing.T) {
	interner := string_interner.create()
	defer string_interner.destroy(interner)

	external_inputs := [?]External_Interface_Input {
		workspace_test_external_interface_input(
			t,
			interner,
			.DDIC_Table,
			.DDIC_Table,
			"zunknown",
			"adt://zunknown.ddic_table.abap",
			`TYPES: BEGIN OF zunknown,
         raw_value TYPE zmissing_domain,
       END OF zunknown.`,
			11,
		),
	}
	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", `REPORT zmain.
SELECT SINGLE raw_value FROM zunknown INTO @DATA(lv_raw).`),
	}

	analysis := semantic_workspace_analyze(Workspace_Input{files = files[:], external_interfaces = external_inputs[:], interner = interner})
	defer semantic_workspace_analysis_destroy(&analysis)

	checker := analysis.project_results[0].checker
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .DDIC_Table, "zunknown"), 0)
	testing.expect(t, workspace_test_candidate_count(&analysis, .Global_Symbol, "zmissing_domain") > 0)
	testing.expect_value(t, workspace_test_diagnostic_count(checker, .Unresolved_Open_Sql_Source), 0)

	key := Semantic_Object_Key{kind = .DDIC_Table, name = string_interner.insert(interner, "zunknown")}
	binding, provider_ok := analysis.external_index.providers[key]
	testing.expect(t, provider_ok)
	if provider_ok {
		testing.expect_value(t, binding.generation, u64(11))
		testing.expect(t, binding.entity != nil && binding.entity.kind == .Type_Def)
		record, record_ok := external_semantic_index_project_record(&analysis.external_index, binding.project_id)
		testing.expect(t, record_ok)
		if record_ok {
			testing.expect_value(t, record.role, Semantic_Project_Role.External_Interface)
			testing.expect_value(t, record.root_key, key)
			testing.expect(t, workspace_test_dependency_edge_count(interner, record.unresolved_dependencies[:], .Global_Symbol, "zmissing_domain") > 0)
		}
	}
}

@(test)
semantic_workspace_builds_per_root_projects_and_indexes_shared_include :: proc(t: ^testing.T) {
	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", "REPORT zmain. INCLUDE zshared. FORM run. gv_shared = 1. ENDFORM."),
		workspace_test_file(t, "mem://zother.report.abap", "REPORT zother. INCLUDE zshared. FORM run. gv_shared = 2. ENDFORM."),
		workspace_test_file(t, "mem://zshared.include.abap", "DATA gv_shared TYPE i."),
	}

	analysis := semantic_workspace_analyze(Workspace_Input{files = files[:]})
	defer semantic_workspace_analysis_destroy(&analysis)

	testing.expect_value(t, len(analysis.projects), 2)
	shared_projects := semantic_workspace_projects_for_file(&analysis, "mem://zshared.include.abap")
	testing.expect_value(t, len(shared_projects), 2)

	for result in analysis.project_results {
		testing.expect(t, len(result.files) == 2)
		gv_shared := workspace_test_lookup(t, &analysis, result.files[0].root_scope, .Value, "gv_shared", .Variable)
		testing.expect(t, gv_shared != nil)
		if gv_shared != nil {
			testing.expect_value(t, gv_shared.source_file.path, "mem://zshared.include.abap")
		}
	}
}

@(test)
semantic_workspace_keeps_external_ddic_fields_with_unresolved_types_soft :: proc(t: ^testing.T) {
	interner := string_interner.create()
	defer string_interner.destroy(interner)

	external := external_semantics_make(interner)
	defer external_semantics_destroy(&external)
	fields := [?]External_Field_Summary {
		{name = "raw_value", type_name = "zmissing_domain"},
	}
	_ = external_semantics_add_structure_summary(&external, "zunknown", fields[:])

	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", `REPORT zmain.
DATA lv_time TYPE t.
SELECT SINGLE raw_value FROM zunknown INTO @lv_time.`),
	}

	analysis := semantic_workspace_analyze(Workspace_Input{files = files[:], external = &external, interner = interner})
	defer semantic_workspace_analysis_destroy(&analysis)

	checker := analysis.project_results[0].checker
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .DDIC_Table, "zunknown"), 0)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Global_Symbol, "zmissing_domain"), 0)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .DDIC_Type, "zmissing_domain"), 0)
	testing.expect_value(t, workspace_test_diagnostic_count(checker, .Unresolved_Open_Sql_Source), 0)
	testing.expect_value(t, workspace_test_diagnostic_count(checker, .Invalid_Open_Sql_Into_Target), 0)
}

@(test)
semantic_workspace_keeps_external_class_parameters_with_unresolved_types_soft :: proc(t: ^testing.T) {
	interner := string_interner.create()
	defer string_interner.destroy(interner)

	external := external_semantics_make(interner)
	defer external_semantics_destroy(&external)
	class := external_semantics_add_class_summary(&external, "zcl_dep")
	_ = workspace_test_add_external_class_method_with_param(&external, class, "run", "iv_value", "zmissing_domain")

	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", `REPORT zmain.
DATA lv_num TYPE i.
zcl_dep=>run( EXPORTING iv_value = lv_num ).`),
	}

	analysis := semantic_workspace_analyze(Workspace_Input{files = files[:], external = &external, interner = interner})
	defer semantic_workspace_analysis_destroy(&analysis)

	checker := analysis.project_results[0].checker
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Global_Symbol, "zcl_dep"), 0)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Class, "zcl_dep"), 0)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Global_Symbol, "zmissing_domain"), 0)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .DDIC_Type, "zmissing_domain"), 0)
	testing.expect_value(t, workspace_test_diagnostic_count(checker, .Unknown_Named_Parameter), 0)
	testing.expect_value(t, workspace_test_diagnostic_count(checker, .Missing_Required_Parameter), 0)
	testing.expect_value(t, workspace_test_diagnostic_count(checker, .Incompatible_Argument_Type), 0)
}

@(test)
semantic_workspace_expands_external_include_source_in_lexical_order :: proc(t: ^testing.T) {
	interner := string_interner.create()
	defer string_interner.destroy(interner)

	external := external_semantics_make(interner)
	defer external_semantics_destroy(&external)
	parsed_external := parser.parse("DATA gv_ext TYPE i.", "adt://zext.include.abap", context.allocator)
	testing.expect_value(t, len(parsed_external.errors), 0)
	_ = external_semantics_add_source_file(&external, "adt://zext.include.abap", parsed_external.root, []string{"zext"})

	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", "REPORT zmain. DATA gv_before TYPE i. INCLUDE zext. DATA gv_after TYPE i."),
	}

	analysis := semantic_workspace_analyze(Workspace_Input{files = files[:], external = &external, interner = interner})
	defer semantic_workspace_analysis_destroy(&analysis)

	testing.expect_value(t, len(analysis.projects), 1)
	testing.expect_value(t, len(semantic_workspace_projects_for_file(&analysis, "adt://zext.include.abap")), 1)
	result := &analysis.project_results[0]
	gv_ext := workspace_test_lookup(t, &analysis, result.files[0].root_scope, .Value, "gv_ext", .Variable)
	gv_after := workspace_test_lookup(t, &analysis, result.files[0].root_scope, .Value, "gv_after", .Variable)
	testing.expect(t, workspace_test_definition_index(result.files[0].root_scope, gv_ext) < workspace_test_definition_index(result.files[0].root_scope, gv_after))
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Include_Source, "zext"), 0)
}

@(test)
semantic_workspace_emits_unresolved_include_candidates_and_if_found_notes :: proc(t: ^testing.T) {
	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", "REPORT zmain. INCLUDE zmissing IF FOUND. INCLUDE zabsent."),
	}

	analysis := semantic_workspace_analyze(Workspace_Input{files = files[:]})
	defer semantic_workspace_analysis_destroy(&analysis)

	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Include_Source, "zmissing"), 1)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Include_Source, "zabsent"), 1)
	checker := analysis.project_results[0].checker
	testing.expect_value(t, workspace_test_diagnostic_count(checker, .Unresolved_Include_If_Found), 1)
	testing.expect_value(t, workspace_test_diagnostic_count(checker, .Unresolved_Include), 1)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Unresolved_Include_If_Found {
			testing.expect_value(t, diagnostic.severity, Checker_Diagnostic_Severity.Note)
		}
	}
}

@(test)
semantic_workspace_emits_unresolved_candidates_for_checker_misses :: proc(t: ^testing.T) {
	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", `REPORT zmain.
CLASS lcl_child DEFINITION INHERITING FROM zcl_remote_parent.
  PUBLIC SECTION.
    INTERFACES zif_remote.
ENDCLASS.
DATA lr_remote TYPE REF TO zcl_remote.
CALL FUNCTION 'Z_REMOTE_FM'.
SELECT carrid FROM ztab INTO @DATA(lv_carrid).`),
	}

	analysis := semantic_workspace_analyze(Workspace_Input{files = files[:]})
	defer semantic_workspace_analysis_destroy(&analysis)

	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Global_Symbol, "zcl_remote"), 1)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Class, "zcl_remote_parent"), 1)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Interface, "zif_remote"), 1)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Function_Module, "z_remote_fm"), 1)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .DDIC_Table, "ztab"), 1)

	record := workspace_test_record_for_project(t, &analysis, analysis.project_results[0].project)
	testing.expect(t, record != nil)
	if record != nil {
		testing.expect_value(t, workspace_test_dependency_edge_count(analysis.interner, record.unresolved_dependencies[:], .Global_Symbol, "zcl_remote"), 1)
		testing.expect_value(t, workspace_test_dependency_edge_count(analysis.interner, record.unresolved_dependencies[:], .Class, "zcl_remote_parent"), 1)
		testing.expect_value(t, workspace_test_dependency_edge_count(analysis.interner, record.unresolved_dependencies[:], .Interface, "zif_remote"), 1)
		testing.expect_value(t, workspace_test_dependency_edge_count(analysis.interner, record.unresolved_dependencies[:], .Function_Module, "z_remote_fm"), 1)
		testing.expect_value(t, workspace_test_dependency_edge_count(analysis.interner, record.unresolved_dependencies[:], .DDIC_Table, "ztab"), 1)
		testing.expect_value(t, workspace_test_reverse_project_count(analysis.interner, analysis.external_index.unresolved_waiters_by_object, .DDIC_Table, "ztab", record.id), 1)
	}
}

@(test)
semantic_workspace_resolves_external_type_and_sql_summaries_without_candidates :: proc(t: ^testing.T) {
	interner := string_interner.create()
	defer string_interner.destroy(interner)

	external := external_semantics_make(interner)
	defer external_semantics_destroy(&external)
	_ = external_semantics_add_class_summary(&external, "zcl_remote")
	scarr_fields := [?]External_Field_Summary {
		{name = "carrid", type_name = "string"},
	}
	_ = external_semantics_add_structure_summary(&external, "scarr", scarr_fields[:])

	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", `REPORT zmain.
DATA lr_remote TYPE REF TO zcl_remote.
SELECT carrid FROM scarr INTO @DATA(lv_carrid).`),
	}

	analysis := semantic_workspace_analyze(Workspace_Input{files = files[:], external = &external, interner = interner})
	defer semantic_workspace_analysis_destroy(&analysis)

	checker := analysis.project_results[0].checker
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Global_Symbol, "zcl_remote"), 0)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Class, "zcl_remote"), 0)
	testing.expect_value(t, workspace_test_candidate_count(&analysis, .DDIC_Table, "scarr"), 0)
	testing.expect_value(t, workspace_test_diagnostic_count(checker, .Unresolved_Open_Sql_Source), 0)

	record := workspace_test_record_for_project(t, &analysis, analysis.project_results[0].project)
	testing.expect(t, record != nil)
	if record != nil {
		testing.expect_value(t, workspace_test_dependency_edge_count(analysis.interner, record.resolved_dependencies[:], .Class, "zcl_remote"), 1)
		testing.expect_value(t, workspace_test_dependency_edge_count(analysis.interner, record.resolved_dependencies[:], .DDIC_Table, "scarr"), 1)
		testing.expect_value(t, len(record.unresolved_dependencies), 0)
		testing.expect_value(t, workspace_test_reverse_project_count(analysis.interner, analysis.external_index.dependents_by_object, .Class, "zcl_remote", record.id), 1)
		testing.expect_value(t, workspace_test_reverse_project_count(analysis.interner, analysis.external_index.dependents_by_object, .DDIC_Table, "scarr", record.id), 1)
	}

	lr_remote := workspace_test_lookup(t, &analysis, analysis.project_results[0].files[0].root_scope, .Value, "lr_remote", .Variable)
	testing.expect(t, lr_remote != nil && lr_remote.type != nil)
	if lr_remote != nil && lr_remote.type != nil {
		testing.expect_value(t, lr_remote.type.kind, Type_Kind.Ref)
		testing.expect(t, lr_remote.type.base != nil)
		if lr_remote.type.base != nil {
			testing.expect_value(t, lr_remote.type.base.kind, Type_Kind.Class)
		}
	}
}

@(test)
semantic_workspace_resolves_external_function_modules_without_candidates :: proc(t: ^testing.T) {
	interner := string_interner.create()
	defer string_interner.destroy(interner)

	external := external_semantics_make(interner)
	defer external_semantics_destroy(&external)
	_ = external_semantics_add_routine_summary(&external, "z_remote_fm", .Module)

	files := [?]Workspace_File_Input {
		workspace_test_file(t, "mem://zmain.report.abap", "REPORT zmain. CALL FUNCTION 'Z_REMOTE_FM'."),
	}

	analysis := semantic_workspace_analyze(Workspace_Input{files = files[:], external = &external, interner = interner})
	defer semantic_workspace_analysis_destroy(&analysis)

	testing.expect_value(t, workspace_test_candidate_count(&analysis, .Function_Module, "z_remote_fm"), 0)
	record := workspace_test_record_for_project(t, &analysis, analysis.project_results[0].project)
	testing.expect(t, record != nil)
	if record != nil {
		testing.expect_value(t, workspace_test_dependency_edge_count(analysis.interner, record.resolved_dependencies[:], .Function_Module, "z_remote_fm"), 1)
		testing.expect_value(t, workspace_test_reverse_project_count(analysis.interner, analysis.external_index.dependents_by_object, .Function_Module, "z_remote_fm", record.id), 1)
	}
}
