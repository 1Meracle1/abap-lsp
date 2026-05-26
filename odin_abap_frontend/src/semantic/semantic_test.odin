package abap_frontend_semantic

import "../adt"
import dep_store "../dependency_store"
import ddic_xml "../ddic_xml"
import "../parser"
import "../tokenizer"
import frontend_runtime "../runtime"
import analyze "./analyze"
import deps "./dependencies"
import sem_query "./query"
import workspace "../workspace"

import "core:mem/virtual"
import "core:os"
import filepath "core:path/filepath"
import "core:strings"
import "core:testing"

contains_fold :: proc(source, needle: string) -> bool {
	lower := strings.to_lower(source, context.allocator)
	defer delete(lower, context.allocator)
	return strings.contains(lower, needle)
}

@(test)
symbol_kind_namespace_occupancy :: proc(t: ^testing.T) {
	testing.expect(t, analyze.symbol_kind_occupies(.Builtin_Type, .Type))
	testing.expect(t, !analyze.symbol_kind_occupies(.Builtin_Type, .Value))
	testing.expect(t, analyze.symbol_kind_occupies(.Builtin_Routine, .Routine))
	testing.expect(t, !analyze.symbol_kind_occupies(.Builtin_Routine, .Type))
	testing.expect(t, analyze.symbol_kind_occupies(.Variable, .Value))
	testing.expect(t, analyze.symbol_kind_occupies(.Report, .Value))
	testing.expect(t, analyze.symbol_kind_occupies(.Method, .Routine))
}

@(test)
creates_root_file_scope_and_builtins :: proc(t: ^testing.T) {
	unit := analyze.unit_analysis_make(
		analyze.Unit_Id(0),
		"mem://main.prog.abap",
		tokenizer.text_range(0, 10),
		context.allocator,
	)

	root := analyze.scope(&unit, unit.root_scope)
	testing.expect(t, root != nil)
	testing.expect_value(t, root.kind, analyze.Scope_Kind.File)
	testing.expect_value(t, root.range, tokenizer.text_range(0, 10))

	testing.expect(t, analyze.find_symbol(&unit, "i", .Builtin_Type) != nil)
	testing.expect(t, analyze.find_symbol(&unit, "abap_bool", .Builtin_Type) != nil)
	testing.expect(t, analyze.find_symbol(&unit, "abap_true", .Builtin_Constant) != nil)
	testing.expect(t, analyze.find_symbol(&unit, "sy", .Builtin_Variable) != nil)
	testing.expect(t, analyze.find_symbol(&unit, "syst", .Builtin_Type) != nil)
	strlen := analyze.builtin_routine_spec("strlen")
	testing.expect(t, strlen != nil)
	if strlen != nil {
		testing.expect_value(t, strlen.description, "Number of characters in a text value.")
	}
	testing.expect(t, analyze.find_symbol(&unit, "strlen", .Builtin_Routine) != nil)

	numeric_routines := [?]string {
		"abs",
		"sign",
		"ceil",
		"floor",
		"trunc",
		"frac",
		"ipow",
		"nmax",
		"nmin",
		"acos",
		"asin",
		"atan",
		"cos",
		"sin",
		"tan",
		"cosh",
		"sinh",
		"tanh",
		"exp",
		"log",
		"log10",
		"sqrt",
		"round",
		"rescale",
	}
	for name in numeric_routines {
		testing.expect(t, analyze.builtin_routine_spec(name) != nil)
		testing.expect(t, analyze.find_symbol(&unit, name, .Builtin_Routine) != nil)
	}
	nmin := analyze.builtin_routine_spec("nmin")
	testing.expect(t, nmin != nil)
	if nmin != nil {
		testing.expect_value(t, len(nmin.params), 9)
		testing.expect_value(t, nmin.params[0].name, "val1")
		testing.expect(t, nmin.supports_named_arguments)
	}
}

@(test)
interface_attribute_alias_resolves_in_method_body :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://alias_attr.abap",
		source = `INTERFACE zif_settings.
  TYPES: BEGIN OF ty_settings,
           block_commit TYPE abap_bool,
         END OF ty_settings.
  TYPES: BEGIN OF ty_repo,
           local_settings TYPE ty_settings,
         END OF ty_repo.
ENDINTERFACE.
INTERFACE zif_repo.
  DATA ms_data TYPE zif_settings=>ty_repo READ-ONLY.
ENDINTERFACE.
CLASS lcl_repo DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_repo.
    ALIASES ms_data FOR zif_repo~ms_data.
ENDCLASS.
CLASS lcl_repo_online DEFINITION INHERITING FROM lcl_repo.
  PUBLIC SECTION.
    METHODS push.
ENDCLASS.
CLASS lcl_repo_online IMPLEMENTATION.
  METHOD push.
    DATA ls_meta_slug TYPE zif_settings=>ty_repo.
    MOVE-CORRESPONDING ms_data TO ls_meta_slug.
    IF ms_data-local_settings-block_commit = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.`,
	}

	project := analyze_project_test(t, 0, target, nil)
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unknown_Field))
}

@(test)
dependency_interface_mode_keeps_declarations_and_drops_bodies :: proc(t: ^testing.T) {
	source := `REPORT zdep.
DATA gv_dep TYPE zglobal_type.
FORM helper USING iv_value TYPE zparam_type.
  DATA lv_body TYPE zbody_type.
  CALL FUNCTION 'Z_BODY'.
ENDFORM.
CLASS lcl_dep DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    DATA pub TYPE zpub_type.
    METHODS run IMPORTING iv_arg TYPE zmethod_type.
  PROTECTED SECTION.
    DATA prot TYPE zprot_type.
  PRIVATE SECTION.
    DATA priv TYPE zpriv_type.
ENDCLASS.
CLASS lcl_dep IMPLEMENTATION.
  METHOD run.
    DATA lv_impl TYPE zimpl_type.
  ENDMETHOD.
ENDCLASS.`

	parsed := parser.parse(source, "mem://dep.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	unit := analyze.collect_unit(analyze.Unit_Id(0), "mem://dep.abap", source, parsed, context.allocator, .Dependency_Interface)
	class := analyze.find_symbol(&unit, "lcl_dep", .Class)

	testing.expect(t, analyze.find_symbol(&unit, "zdep", .Report) != nil)
	testing.expect(t, analyze.find_symbol(&unit, "gv_dep", .Variable) != nil)
	testing.expect(t, analyze.find_symbol(&unit, "helper", .Form) != nil)
	testing.expect(t, class != nil)
	if class != nil {
		testing.expect(t, class_member_named(&unit, class.id, "pub", .Attribute) != nil)
		testing.expect(t, class_member_named(&unit, class.id, "prot", .Attribute) != nil)
		testing.expect(t, class_member_named(&unit, class.id, "run", .Method) != nil)
		testing.expect(t, class_member_named(&unit, class.id, "priv", .Attribute) == nil)
	}
	testing.expect(t, has_reference(&unit, "zglobal_type", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "zparam_type", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "zcl_parent", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "zpub_type", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "zmethod_type", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "zprot_type", .Type, .Type_Ref))
	testing.expect(t, !has_reference(&unit, "zbody_type", .Type, .Type_Ref))
	testing.expect(t, !has_reference(&unit, "zpriv_type", .Type, .Type_Ref))
	testing.expect(t, !has_reference(&unit, "zimpl_type", .Type, .Type_Ref))
	for call_site in unit.call_sites {
		testing.expect(t, call_site.target.function_name != "z_body")
	}
}

@(test)
dependency_interface_mode_collects_ast_declarations_only :: proc(t: ^testing.T) {
	source := `REPORT zdep.
INCLUDE zimpl.
INCLUDE TYPE ztyped_include.
IF abap_true = abap_true.
  DATA lv_hidden TYPE zhidden_type.
ENDIF.
CLASS zcl_forward DEFINITION DEFERRED.
INTERFACE zif_dep.
  TYPES ty_value TYPE zif_type.
  METHODS run IMPORTING iv_arg TYPE zif_arg.
ENDINTERFACE.
CLASS zcl_dep DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    METHODS public REDEFINITION.
  PROTECTED SECTION.
    EVENTS changed EXPORTING VALUE(ev_value) TYPE zchanged_type.
  PRIVATE SECTION.
    METHODS secret.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD public.
    DATA lv_impl TYPE zimpl_type.
  ENDMETHOD.
ENDCLASS.
FUNCTION z_dep IMPORTING iv_value TYPE zfunc_type.
  DATA lv_body TYPE zfunc_body.
ENDFUNCTION.`

	parsed := parser.parse(source, "mem://dep.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	unit := analyze.collect_unit(analyze.Unit_Id(0), "mem://dep.abap", source, parsed, context.allocator, .Dependency_Interface)
	iface := analyze.find_symbol(&unit, "zif_dep", .Interface)
	class := analyze.find_symbol(&unit, "zcl_dep", .Class)

	testing.expect(t, analyze.find_symbol(&unit, "zdep", .Report) != nil)
	testing.expect(t, analyze.find_symbol(&unit, "zcl_forward", .Class) != nil)
	testing.expect(t, iface != nil)
	testing.expect(t, class != nil)
	testing.expect(t, analyze.find_symbol(&unit, "z_dep", .Module) != nil)
	if iface != nil {
		testing.expect(t, class_member_named(&unit, iface.id, "run", .Method) != nil)
	}
	if class != nil {
		testing.expect(t, class_member_named(&unit, class.id, "public", .Method) != nil)
		testing.expect(t, class_member_named(&unit, class.id, "changed", .Event) != nil)
		testing.expect(t, class_member_named(&unit, class.id, "secret", .Method) == nil)
	}
	testing.expect_value(t, len(unit.include_edges), 0)
	testing.expect(t, has_reference(&unit, "ztyped_include", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "zif_type", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "zif_arg", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "zcl_parent", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "zchanged_type", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "zfunc_type", .Type, .Type_Ref))
	testing.expect(t, !has_reference(&unit, "zhidden_type", .Type, .Type_Ref))
	testing.expect(t, !has_reference(&unit, "zimpl_type", .Type, .Type_Ref))
	testing.expect(t, !has_reference(&unit, "zfunc_body", .Type, .Type_Ref))
}

@(test)
remote_dependency_candidates_ignore_plain_identifiers :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///remote_candidates.abap",
		source = `REPORT zmain.
TYPES ls_local_type TYPE string.
DATA lv_ref TYPE REF TO zcl_remote_type.
DATA ls_local TYPE ls_local_type.
DATA ls_client TYPE t000.
unknown_value = 1.
CALL FUNCTION 'Z_REMOTE_FM'.
lv_ref->get_url( ).
CLASS lcl_remote_impl DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_remote.
    METHODS zif_remote~run REDEFINITION.
ENDCLASS.
`,
	}

	project := analyze_project_test(t, 0, target, nil)
	candidates := analyze.collect_project_remote_dependency_candidates(&project, context.allocator)

	has_type := false
	has_type_hint := false
	has_function := false
	has_standard_type := false
	has_interface := false
	has_interface_hint := false
	has_symbol := false
	has_routine := false
	has_local_type := false
	for candidate in candidates {
		if candidate.name == "zcl_remote_type" && candidate.kind == .Type {
			has_type = true
			has_type_hint = candidate.hint == .Object_Type
		}
		if candidate.name == "z_remote_fm" && candidate.kind == .Function {
			has_function = true
		}
		if candidate.name == "t000" && candidate.kind == .Type {
			has_standard_type = true
		}
		if candidate.name == "zif_remote" && candidate.kind == .Type {
			has_interface = true
			has_interface_hint = candidate.hint == .Interface_Type
		}
		if candidate.name == "unknown_value" {
			has_symbol = true
		}
		if candidate.name == "get_url" {
			has_routine = true
		}
		if candidate.name == "ls_local_type" {
			has_local_type = true
		}
	}
	testing.expect(t, has_type)
	testing.expect(t, has_type_hint)
	testing.expect(t, has_function)
	testing.expect(t, has_standard_type)
	testing.expect(t, has_interface)
	testing.expect(t, has_interface_hint)
	testing.expect(t, !has_symbol)
	testing.expect(t, !has_routine)
	testing.expect(t, !has_local_type)
}

@(test)
remote_dependency_candidates_include_unresolved_static_targets :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/main.abap",
			source = `
REPORT zmain.
CLASS lcl_local DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS lcl_local IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
DATA c_generic_error_msg TYPE string.
cl_message_helper=>set_msg_vars_for_clike( c_generic_error_msg ).
lcl_local=>run( ).
`,
		},
	}
	project := analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		nil,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)

	_, static_pending := state.unresolved_candidates[analyze.Remote_Dependency_Key{name = "cl_message_helper", kind = .Static}]
	_, local_pending := state.unresolved_candidates[analyze.Remote_Dependency_Key{name = "lcl_local", kind = .Static}]
	static_found := false
	local_found := false
	for candidate in analyze.collect_project_remote_dependency_candidates(&project, context.allocator) {
		if candidate.name == "cl_message_helper" && candidate.kind == .Static {
			static_found = true
		}
		if candidate.name == "lcl_local" && candidate.kind == .Static {
			local_found = true
		}
	}

	testing.expect(t, static_pending)
	testing.expect(t, !local_pending)
	testing.expect(t, static_found)
	testing.expect(t, !local_found)
}

@(test)
function_remote_candidate_requires_function_module_symbol :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/main.abap",
			source = `
REPORT zmain.
FORM z_remote_fm.
ENDFORM.
CALL FUNCTION 'Z_REMOTE_FM'.
`,
		},
	}
	project := analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		nil,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	_, pending := state.unresolved_candidates[analyze.Remote_Dependency_Key{name = "z_remote_fm", kind = .Function}]
	found := false
	for candidate in analyze.collect_project_remote_dependency_candidates(&project, context.allocator) {
		if candidate.name == "z_remote_fm" && candidate.kind == .Function {
			found = true
		}
	}

	testing.expect(t, pending)
	testing.expect(t, found)
}

@(test)
standard_type_pool_symbols_are_installed :: proc(t: ^testing.T) {
	unit := analyze.unit_analysis_make(
		analyze.Unit_Id(0),
		"mem://type_pool_symbols.abap",
		tokenizer.text_range(0, 0),
		context.allocator,
	)

	type_names := [?]string {
		"syst_short",
		"syst_byte",
		"syst_long",
		"synt_errors",
		"synt_comment",
		"synt_map",
		"synt_it_trmsg_raw",
		"synt_includes",
		"synt_ext_check",
		"synt_interval",
		"synt_crossref",
		"synt_type_obj",
		"synt_type_childs",
		"synt_data_obj",
		"synt_dpar",
		"synt_env",
		"synt_comp_obj",
		"synt_xcross",
		"synt_xcross_level",
		"synt_xcross_stmnt",
		"synt_ext_obj_use",
		"synum01",
		"sychar68k",
		"abap_classname",
		"abap_compname",
		"abap_typename",
		"abap_keyname",
		"abap_keycompname",
		"abap_intfname",
		"abap_attrname",
		"abap_evntname",
		"abap_parmname",
		"abap_excpname",
		"abap_component_tab",
		"abap_func_parmbind_tab",
		"abap_func_excpbind_tab",
		"abap_func_parmbind",
		"abap_func_excpbind",
		"abap_trans_srcbind_tab",
		"abap_trans_resbind_tab",
		"abap_componentdescr",
		"abap_simple_componentdescr",
		"abap_abstypename",
		"abap_compdescr",
		"abap_keydescr",
		"abap_table_keydescr_tab",
		"abap_table_keycompdescr",
		"abap_table_keydescr",
		"abap_intfdescr_tab",
		"abap_typecategory",
		"abap_typekind",
		"abap_typepropkind",
		"abap_component_symbol_tab",
		"abap_component_view_tab",
		"abap_structkind",
		"abap_compdescr_tab",
		"abapsource",
		"abap_encoding",
		"abap_editmask",
		"abap_helpid",
		"abap_classkind",
		"abap_visibility",
		"abap_frndtypes_tab",
		"abap_tablekind",
		"abap_keydefkind",
		"abap_keydescr_tab",
		"abap_methname",
		"abap_methdescr",
		"abap_parmdescr",
		"abap_parmdescr_tab",
		"abap_excpdescr",
		"abap_excpdescr_tab",
		"abap_frnddescr",
		"abap_frnddescr_tab",
		"abap_intfdescr",
		"abap_typedef",
		"abap_attrdescr",
		"abap_evntdescr",
		"abap_endian",
		"abap_parmkind",
		"abap_typedef_tab",
		"abap_attrdescr_tab",
		"abap_methdescr_tab",
		"abap_evntdescr_tab",
		"abap_parmbind",
		"abap_parmbind_tab",
		"abap_excpbind",
		"abap_excpbind_tab",
		"abap_intfkind",
		"abap_char1",
		"abap_cr_lf",
		"abap_byte_order_mark",
		"abap_byte_order_utf8",
		"abap_trans_parmname",
		"abap_trans_parmvalue",
		"abap_trans_parmref",
		"abap_trans_parmbind",
		"abap_trans_parm_obj_bind",
		"abap_trans_parmbind_tab",
		"abap_trans_parm_obj_bind_tab",
		"abap_trans_objname",
		"abap_trans_objbind",
		"abap_trans_objbind_tab",
		"abap_trans_srcname",
		"abap_trans_srcbind",
		"abap_trans_srcbind_tab_sorted",
		"abap_trans_resname",
		"abap_trans_resbind",
		"abap_trans_resbind_tab_sorted",
		"abap_bool",
	}
	for name in type_names {
		testing.expect(t, analyze.find_symbol(&unit, name, .Builtin_Type) != nil)
	}

	constant_names := [?]string {
		"abap_true",
		"abap_false",
		"abap_undefined",
		"abap_on",
		"abap_off",
		"abap_max_abs_type_name_ln",
		"abap_max_class_name_ln",
		"abap_max_intf_name_ln",
		"abap_max_comp_name_ln",
		"abap_max_key_name_ln",
		"abap_max_class_comp_name_ln",
		"abap_max_edit_mask_ln",
		"abap_max_help_id_ln",
		"abap_max_db_string_ln",
		"abap_max_db_rawstring_ln",
		"abap_func_exporting",
		"abap_func_importing",
		"abap_func_tables",
		"abap_func_changing",
	}
	for name in constant_names {
		testing.expect(t, analyze.find_symbol(&unit, name, .Builtin_Constant) != nil)
	}
}

@(test)
structure_field_lookup_for_syst_and_screen :: proc(t: ^testing.T) {
	unit := analyze.unit_analysis_make(
		analyze.Unit_Id(0),
		"mem://main.prog.abap",
		tokenizer.text_range(0, 0),
		context.allocator,
	)

	syst := analyze.find_structure(&unit, "syst")
	screen := analyze.find_structure(&unit, "screen")
	testing.expect(t, syst != nil)
	testing.expect(t, screen != nil)

	subrc, ok := analyze.structure_field_info(&unit, syst.id, "subrc")
	testing.expect(t, ok)
	testing.expect_value(t, subrc.name, "subrc")
	testing.expect(t, .Has_Type_Ref in subrc.flags)
	testing.expect_value(t, subrc.type_ref.base_name, "i")
	testing.expect_value(t, subrc.description, "Return code set by many ABAP statements; 0 usually indicates success for the documented statement.")
	testing.expect_value(t, analyze.builtin_structure_field_description("syst", "subrc"), subrc.description)

	screen_name, ok2 := analyze.structure_field_info(&unit, screen.id, "name")
	testing.expect(t, ok2)
	testing.expect_value(t, screen_name.name, "name")
	testing.expect(t, .Has_Type_Ref in screen_name.flags)
	testing.expect_value(t, screen_name.type_ref.base_name, "c")
	testing.expect_value(t, screen_name.description, "Name of the current dynpro field or screen element.")
}

collect_test_unit :: proc(t: ^testing.T, uri, source: string) -> analyze.Unit_Analysis {
	parsed := parser.parse(source, uri, context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	return analyze.analyze_unit(analyze.Unit_Id(0), uri, source, parsed, context.allocator)
}

has_symbol :: proc(unit: ^analyze.Unit_Analysis, kind: analyze.Symbol_Kind, name: string) -> bool {
	for symbol in unit.symbols {
		if symbol.kind == kind && symbol.name == name {
			return true
		}
	}
	return false
}

has_scope_kind :: proc(unit: ^analyze.Unit_Analysis, kind: analyze.Scope_Kind) -> bool {
	for scope in unit.scopes {
		if scope.kind == kind {
			return true
		}
	}
	return false
}

has_diagnostic :: proc(unit: ^analyze.Unit_Analysis, kind: analyze.Diagnostic_Kind) -> bool {
	for diagnostic in unit.diagnostics {
		if diagnostic.kind == kind {
			return true
		}
	}
	return false
}

project_has_diagnostic :: proc(project: ^analyze.Project_Analysis, kind: analyze.Diagnostic_Kind) -> bool {
	for diagnostic in project.diagnostics {
		if diagnostic.kind == kind {
			return true
		}
	}
	return false
}

project_units_have_diagnostic :: proc(project: ^analyze.Project_Analysis, kind: analyze.Diagnostic_Kind) -> bool {
	for &unit in project.units {
		if has_diagnostic(&unit, kind) {
			return true
		}
	}
	return false
}

analyze_project_test :: proc(
	t: ^testing.T,
	worker_count: int,
	target: analyze.Source_Input,
	candidates: []analyze.Source_Input,
) -> analyze.Project_Analysis {
	pool: frontend_runtime.Pool
	options := frontend_runtime.Options {
		worker_count = worker_count,
		task_capacity = 128,
		queue_capacity = 32,
		deque_capacity = 32,
	}
	testing.expect_value(t, frontend_runtime.pool_init(&pool, options, context.allocator), frontend_runtime.Submit_Error.None)
	if pool.options.worker_count > 0 {
		testing.expect_value(t, frontend_runtime.pool_start(&pool), frontend_runtime.Submit_Error.None)
	}
	project := analyze.analyze_target(target, candidates, analyze.Analyze_Options{pool = &pool}, context.allocator)
	if pool.options.worker_count > 0 {
		frontend_runtime.pool_join(&pool)
	}
	frontend_runtime.pool_destroy(&pool)
	return project
}

analyze_project_dependencies_test :: proc(
	t: ^testing.T,
	target: analyze.Source_Input,
	dependencies: []analyze.Source_Input,
) -> analyze.Project_Analysis {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, 0, context.allocator)
	project := analyze.analyze_target_with_candidate_inputs(
		target,
		candidates[:],
		dependencies,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	frontend_runtime.pool_destroy(&pool)
	return project
}

@(test)
project_state_incremental_dependency_update_resolves_waiting_unit :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	target := analyze.Source_Input {
		uri = "mem://main.abap",
		source = "DATA lo_dep TYPE REF TO zcl_dep. DATA lo_other TYPE REF TO zcl_other.",
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, 0, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, 0, 1, context.allocator)

	project := analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	root := analyze.project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil)
	if root != nil {
		testing.expect(t, has_diagnostic(root, .Unresolved_Reference))
	}

	dep := analyze.Source_Input {
		uri = "abapls-cache:/global-class/zcl_dep.abap",
		source = "CLASS zcl_dep DEFINITION. ENDCLASS.",
		mode = .Dependency_Interface,
	}
	append(&dependencies, dep)
	project = analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	root = analyze.project_unit_by_uri(&project, target.uri)

	testing.expect_value(t, len(project.units), 2)
	testing.expect(t, root != nil)
	if root != nil {
		testing.expect(t, reference_resolves_to_uri(&project, root, "zcl_dep", .Type, .Type_Ref, dep.uri))
		testing.expect(t, has_diagnostic(root, .Unresolved_Reference))
	}
}

@(test)
project_state_dependency_private_change_keeps_interface_signature :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	target := analyze.Source_Input {
		uri = "mem://main.abap",
		source = "DATA lo_dep TYPE REF TO zcl_dep.",
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, 0, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, 0, 1, context.allocator)
	dep := analyze.Source_Input {
		uri = "abapls-cache:/global-class/zcl_dep.abap",
		source = `CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.`,
		mode = .Dependency_Interface,
	}
	append(&dependencies, dep)
	project := analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	root := analyze.project_unit_by_uri(&project, target.uri)
	dep_unit := analyze.project_unit_by_uri(&project, dep.uri)
	testing.expect(t, root != nil)
	testing.expect(t, dep_unit != nil)
	testing.expect(t, root != nil && reference_resolves_to_uri(&project, root, "zcl_dep", .Type, .Type_Ref, dep.uri))
	signature := ""
	if dep_unit != nil {
		signature = state.interface_signatures[analyze.unit_id_index(dep_unit.unit_id)]
	}

	dependencies[0].source = `CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
    DATA lv_private TYPE string.
  ENDMETHOD.
ENDCLASS.`
	project = analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	dep_unit = analyze.project_unit_by_uri(&project, dep.uri)
	root = analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, dep_unit != nil)
	if dep_unit != nil {
		testing.expect_value(t, state.interface_signatures[analyze.unit_id_index(dep_unit.unit_id)], signature)
	}
	testing.expect(t, root != nil && reference_resolves_to_uri(&project, root, "zcl_dep", .Type, .Type_Ref, dep.uri))
}

@(test)
project_state_batch_resolves_target_candidates :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	targets := [?]analyze.Source_Input {
		{uri = "file:///workspace/main.abap", source = "INCLUDE zshared. gv_shared = 1."},
		{uri = "file:///workspace/generated.abap", source = "REPORT zshared. DATA gv_shared TYPE i."},
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, len(targets), context.allocator)
	for target in targets {
		append(&candidates, analyze.Project_Candidate_Input{input = target})
	}
	state := analyze.project_state_make({}, context.allocator)
	project := analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		candidates[:],
		{},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	root := analyze.project_unit_by_uri(&project, targets[0].uri)

	testing.expect_value(t, len(project.units), 2)
	testing.expect(t, root != nil)
	if root != nil {
		testing.expect_value(t, include_target_uri(&project, root, "zshared"), targets[1].uri)
		testing.expect(t, reference_resolves_to_uri(&project, root, "gv_shared", .Value, .Identifier, targets[1].uri))
	}
}

@(test)
project_state_unresolved_candidates_keep_one_waiter_per_unit :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/first.abap",
			source = "DATA lo_a TYPE REF TO zcl_waiting. DATA lo_b TYPE REF TO zcl_waiting.",
		},
		{
			uri = "file:///workspace/second.abap",
			source = "DATA lo_c TYPE REF TO zcl_waiting. DATA lo_d TYPE REF TO zcl_waiting.",
		},
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, 0, context.allocator)
	state := analyze.project_state_make({}, context.allocator)
	project := analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		candidates[:],
		nil,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	units, ok := state.unresolved_candidates[analyze.Remote_Dependency_Key {
		name = "zcl_waiting",
		kind = .Type,
		hint = .Object_Type,
	}]

	testing.expect(t, ok)
	testing.expect_value(t, len(units), 2)
	testing.expect(t, units[0] == project.units[0].unit_id)
	testing.expect(t, units[1] == project.units[1].unit_id)
}

@(test)
project_state_unresolved_candidates_skip_resolved_open_sql_dependency :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/main.abap",
			source = "REPORT zmain. SELECT funcname FROM enlfdir INTO TABLE @DATA(lt_rows).",
		},
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table/enlfdir.abap",
			source = `
TYPES: BEGIN OF enlfdir,
         funcname TYPE string,
       END OF enlfdir.
`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	root := analyze.project_unit_by_uri(&project, targets[0].uri)
	_, pending := state.unresolved_candidates[analyze.Remote_Dependency_Key{name = "enlfdir", kind = .Type}]

	testing.expect(t, !pending)
	testing.expect(t, root != nil)
	testing.expect(t, root != nil && !has_diagnostic(root, .Unverified_Open_Sql_Source))
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))
}

@(test)
project_state_unresolved_candidates_skip_resolved_function_dependency :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/main.abap",
			source = "REPORT zmain. CALL FUNCTION 'Z_REMOTE_FM'.",
		},
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/function-module/Z_REMOTE_FM.abap",
			source = "FUNCTION z_remote_fm.\nENDFUNCTION.",
			mode = .Dependency_Interface,
		},
	}
	project := analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	_, pending := state.unresolved_candidates[analyze.Remote_Dependency_Key{name = "z_remote_fm", kind = .Function}]
	state_candidates := analyze.collect_project_state_remote_dependency_candidates(&state, false, context.allocator)
	found_state_candidate := false
	for candidate in state_candidates {
		if candidate.name == "z_remote_fm" && candidate.kind == .Function {
			found_state_candidate = true
		}
	}

	testing.expect(t, analyze.project_unit_by_uri(&project, dependencies[0].uri) != nil)
	testing.expect(t, !pending)
	testing.expect(t, !found_state_candidate)
}

@(test)
dependency_interface_static_like_type_ref_is_transitive_candidate :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/main.abap",
			source = "REPORT zmain. DATA lo_dep TYPE REF TO zcl_dep.",
		},
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/global-class/zcl_dep.abap",
			source = `CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING phase LIKE zif_base=>ty_phase.
ENDCLASS.`,
			mode = .Dependency_Interface,
		},
	}
	_ = analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	full_candidates := analyze.collect_project_state_remote_dependency_candidates(&state, true, context.allocator)
	root_only_candidates := analyze.collect_project_state_remote_dependency_candidates(&state, false, context.allocator)
	found_full := false
	found_root_only := false
	for candidate in full_candidates {
		if candidate.name == "zif_base" && candidate.kind == .Type {
			found_full = true
		}
	}
	for candidate in root_only_candidates {
		if candidate.name == "zif_base" && candidate.kind == .Type {
			found_root_only = true
		}
	}

	testing.expect(t, found_full)
	testing.expect(t, !found_root_only)
}

@(test)
project_state_retained_global_roots_keep_first_winner :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{uri = "file:///workspace/a/zcl_shared.abap", source = "CLASS zcl_shared DEFINITION. ENDCLASS."},
		{uri = "file:///workspace/b/zcl_shared.abap", source = "CLASS zcl_shared DEFINITION. ENDCLASS."},
		{uri = "file:///workspace/main.abap", source = "DATA lo_shared TYPE REF TO zcl_shared."},
	}
	project := analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		nil,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	consumer := analyze.project_unit_by_uri(&project, targets[2].uri)

	testing.expect(t, consumer != nil)
	testing.expect(t, consumer != nil && reference_resolves_to_uri(&project, consumer, "zcl_shared", .Type, .Type_Ref, targets[0].uri))

	targets[1].source = "CLASS zcl_shared DEFINITION. PUBLIC SECTION. DATA gv_second TYPE i. ENDCLASS."
	project = analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		nil,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	consumer = analyze.project_unit_by_uri(&project, targets[2].uri)

	testing.expect(t, consumer != nil)
	testing.expect(t, consumer != nil && reference_resolves_to_uri(&project, consumer, "zcl_shared", .Type, .Type_Ref, targets[0].uri))
}

@(test)
project_state_retained_global_root_removal_exposes_next_winner :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{uri = "file:///workspace/a/zcl_shared.abap", source = "CLASS zcl_shared DEFINITION. ENDCLASS."},
		{uri = "file:///workspace/b/zcl_shared.abap", source = "CLASS zcl_shared DEFINITION. ENDCLASS."},
		{uri = "file:///workspace/main.abap", source = "DATA lo_shared TYPE REF TO zcl_shared."},
	}
	_ = analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		nil,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)

	targets[0].source = "CLASS zcl_first DEFINITION. ENDCLASS."
	project := analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		nil,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	consumer := analyze.project_unit_by_uri(&project, targets[2].uri)

	testing.expect(t, consumer != nil)
	testing.expect(t, consumer != nil && reference_resolves_to_uri(&project, consumer, "zcl_shared", .Type, .Type_Ref, targets[1].uri))
}

@(test)
project_state_retained_provided_name_removal_keeps_other_owner :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{uri = "file:///workspace/first.abap", source = "REPORT zshared."},
		{uri = "file:///workspace/second.abap", source = "REPORT zshared."},
	}
	_ = analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		nil,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)

	targets[0].source = "REPORT zfirst."
	_ = analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		nil,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)

	testing.expect(t, "zshared" in state.index.root_lookup.provided_names)
}

@(test)
project_state_public_interface_change_revalidates_reverse_dependents :: proc(t: ^testing.T) {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	defer frontend_runtime.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	target := analyze.Source_Input {
		uri = "file:///workspace/main.abap",
		source = `
DATA lo_dep TYPE REF TO zcl_dep.
START-OF-SELECTION.
  CALL METHOD lo_dep->run.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/zcl_dep.abap",
			source = `CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.`,
			mode = .Dependency_Interface,
		},
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, 0, context.allocator)
	project := analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	root := analyze.project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))

	dependencies[0].source = `CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS stop.
ENDCLASS.`
	project = analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	root = analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil && has_diagnostic(root, .Unknown_Field))
}

@(test)
analyze_batches_more_units_than_task_capacity :: proc(t: ^testing.T) {
	target := analyze.Source_Input{uri = "mem://main.abap", source = "REPORT zmain."}
	dependencies := make([dynamic]analyze.Source_Input, 0, 5, context.allocator)
	append(&dependencies, analyze.Source_Input{uri = "mem://dep1.abap", source = "REPORT zdep1."})
	append(&dependencies, analyze.Source_Input{uri = "mem://dep2.abap", source = "REPORT zdep2."})
	append(&dependencies, analyze.Source_Input{uri = "mem://dep3.abap", source = "REPORT zdep3."})
	append(&dependencies, analyze.Source_Input{uri = "mem://dep4.abap", source = "REPORT zdep4."})
	append(&dependencies, analyze.Source_Input{uri = "mem://dep5.abap", source = "REPORT zdep5."})

	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 2}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	project := analyze.analyze_target_with_candidate_inputs(
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	frontend_runtime.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 6)
}

analyze_path_test :: proc(t: ^testing.T, root, target_path: string) -> workspace.Analysis_Result {
	return analyze_path_test_with_options(t, root, target_path, {})
}

analyze_path_test_with_options :: proc(
	t: ^testing.T,
	root: string,
	target_path: string,
	options: workspace.Options,
) -> workspace.Analysis_Result {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	run_options := options
	run_options.pool = &pool
	result := workspace.analyze_path(target_path, root, nil, run_options, context.allocator)
	frontend_runtime.pool_destroy(&pool)
	return result
}

analyze_file_path_test_with_options :: proc(
	t: ^testing.T,
	target_path: string,
	options: workspace.Options,
) -> workspace.Analysis_Result {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	run_options := options
	run_options.pool = &pool
	result := workspace.analyze_file_path(target_path, nil, run_options, context.allocator)
	frontend_runtime.pool_destroy(&pool)
	return result
}

manifest_workspace_path :: proc(name: string) -> string {
	package_dir := filepath.dir(#file)
	root, _ := filepath.join(
		{package_dir, "..", "..", "bin", "test-data", "manifest", name},
		context.allocator,
	)
	os.remove_all(root)
	os.make_directory_all(root)
	return root
}

external_export_workspace_path :: proc(name: string) -> string {
	package_dir := filepath.dir(#file)
	root, _ := filepath.join(
		{package_dir, "..", "..", "bin", "test-data", "local-export", name},
		context.allocator,
	)
	os.remove_all(root)
	os.make_directory_all(root)
	return root
}

manifest_test_file :: proc(t: ^testing.T, root, relative, source: string) -> string {
	path, _ := filepath.join({root, relative}, context.allocator)
	dir := filepath.dir(path)
	testing.expect(t, os.make_directory_all(dir) == nil)
	testing.expect(t, os.write_entire_file(path, source) == nil)
	cleaned, ok := workspace.absolute_clean_path(path, context.allocator)
	testing.expect(t, ok)
	return cleaned
}

analyze_units_project_test :: proc(t: ^testing.T, sources: []analyze.Source_Input) -> analyze.Project_Analysis {
	units := make([dynamic]analyze.Unit_Analysis, 0, len(sources), context.allocator)
	for source, i in sources {
		parsed := parser.parse(source.source, source.uri, context.allocator)
		testing.expect_value(t, len(parsed.errors), 0)
		unit := analyze.collect_unit(analyze.Unit_Id(u32(i)), source.uri, source.source, parsed, context.allocator, source.mode)
		analyze.resolve_unit_locally(&unit, context.allocator)
		append(&units, unit)
	}
	project := analyze.project_analysis_from_units(units, context.allocator)
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	analyze.finish_project_analysis(&project, &pool, {}, context.allocator)
	frontend_runtime.pool_destroy(&pool)
	return project
}

include_target_uri :: proc(project: ^analyze.Project_Analysis, unit: ^analyze.Unit_Analysis, name: string) -> string {
	for edge in unit.include_edges {
		if edge.name != name || !edge.has_target {
			continue
		}
		target_index := analyze.unit_id_index(edge.target)
		if target_index >= 0 && target_index < len(project.units) {
			return project.units[target_index].uri
		}
	}
	return ""
}

class_member_named :: proc(
	unit: ^analyze.Unit_Analysis,
	class_symbol: analyze.Symbol_Id,
	name: string,
	kind: analyze.Class_Member_Kind,
) -> ^analyze.Class_Member_Data {
	for &member in unit.class_members {
		if member.class_symbol == class_symbol && member.kind == kind && member.name == name {
			return &member
		}
	}
	return nil
}

reference_count :: proc(
	unit: ^analyze.Unit_Analysis,
	name: string,
	namespace: analyze.Namespace,
	kind: analyze.Reference_Kind,
) -> int {
	count := 0
	for reference in unit.references {
		if reference.name == name && reference.namespace == namespace && reference.kind == kind {
			count += 1
		}
	}
	return count
}

has_reference :: proc(
	unit: ^analyze.Unit_Analysis,
	name: string,
	namespace: analyze.Namespace,
	kind: analyze.Reference_Kind,
) -> bool {
	return reference_count(unit, name, namespace, kind) > 0
}

reference_resolves_to_uri :: proc(
	project: ^analyze.Project_Analysis,
	unit: ^analyze.Unit_Analysis,
	name: string,
	namespace: analyze.Namespace,
	kind: analyze.Reference_Kind,
	uri: string,
) -> bool {
	for reference in unit.references {
		if reference.name != name ||
		   reference.namespace != namespace ||
		   reference.kind != kind ||
		   !reference.has_resolution ||
		   reference.resolution.kind != .Symbol {
			continue
		}
		unit_index := analyze.unit_id_index(reference.resolution.symbol.unit)
		if unit_index >= 0 && unit_index < len(project.units) && project.units[unit_index].uri == uri {
			return true
		}
	}
	return false
}

has_named_argument :: proc(
	unit: ^analyze.Unit_Analysis,
	name: string,
	section: analyze.Named_Argument_Section,
	target_kind: analyze.Named_Argument_Target_Kind,
) -> bool {
	for arg in unit.named_arguments {
		if arg.name == name &&
		   arg.has_section &&
		   arg.section == section &&
		   arg.target.kind == target_kind {
			return true
		}
	}
	return false
}

has_method_named_argument :: proc(
	unit: ^analyze.Unit_Analysis,
	name: string,
	section: analyze.Named_Argument_Section,
	base_name: string,
	method_name: string,
) -> bool {
	for arg in unit.named_arguments {
		if arg.name == name &&
		   arg.has_section &&
		   arg.section == section &&
		   arg.target.kind == .Method &&
		   arg.target.base_name == base_name &&
		   arg.target.method_name == method_name {
			return true
		}
	}
	return false
}

field_names_match :: proc(structure: ^analyze.Structure_Data, names: []string) -> bool {
	if structure == nil || len(structure.fields) != len(names) {
		return false
	}
	for i in 0 ..< len(names) {
		if structure.fields[i].name != names[i] {
			return false
		}
	}
	return true
}

provided_name_present :: proc(unit: ^analyze.Unit_Analysis, name: string) -> bool {
	for provided in unit.provided_names {
		if provided == name {
			return true
		}
	}
	return false
}

sql_source_present :: proc(
	unit: ^analyze.Unit_Analysis,
	name: string,
	resolution: analyze.Sql_Resolution,
) -> bool {
	for source in unit.sql_sources {
		if source.name == name && source.resolution == resolution {
			return true
		}
	}
	return false
}

sql_source_alias_present :: proc(
	unit: ^analyze.Unit_Analysis,
	name, alias: string,
	kind: analyze.Sql_Source_Kind,
) -> bool {
	for source in unit.sql_sources {
		if source.name == name && source.alias == alias && source.source_kind == kind {
			return true
		}
	}
	return false
}

sql_projection_present :: proc(
	unit: ^analyze.Unit_Analysis,
	name: string,
	kind: analyze.Sql_Projection_Kind,
) -> bool {
	for projection in unit.sql_projections {
		if projection.name == name && projection.kind == kind {
			return true
		}
	}
	return false
}

sql_projection_alias_present :: proc(
	unit: ^analyze.Unit_Analysis,
	alias: string,
	kind: analyze.Sql_Projection_Kind,
) -> bool {
	for projection in unit.sql_projections {
		if projection.alias == alias && projection.kind == kind {
			return true
		}
	}
	return false
}

sql_name_ref_present :: proc(unit: ^analyze.Unit_Analysis, name: string, kind: analyze.Sql_Name_Ref_Kind) -> bool {
	for reference in unit.sql_name_refs {
		if reference.name == name && reference.kind == kind {
			return true
		}
	}
	return false
}

sql_qualified_ref_present :: proc(
	unit: ^analyze.Unit_Analysis,
	qualifier, name: string,
	kind: analyze.Sql_Name_Ref_Kind,
) -> bool {
	for reference in unit.sql_name_refs {
		if reference.qualifier == qualifier && reference.name == name && reference.kind == kind {
			return true
		}
	}
	return false
}

sql_predicate_present :: proc(unit: ^analyze.Unit_Analysis, kind: analyze.Sql_Predicate_Kind) -> bool {
	for predicate in unit.sql_predicates {
		if predicate.kind == kind {
			return true
		}
	}
	return false
}

sql_dynamic_present :: proc(unit: ^analyze.Unit_Analysis, kind: analyze.Sql_Dynamic_Fragment_Kind) -> bool {
	for fragment in unit.sql_dynamic_fragments {
		if fragment.kind == kind {
			return true
		}
	}
	return false
}

sql_target_present :: proc(
	unit: ^analyze.Unit_Analysis,
	name: string,
	kind: analyze.Sql_Target_Kind,
	flags: analyze.Sql_Target_Flags,
) -> bool {
	for target in unit.sql_targets {
		if target.target_name != name || target.kind != kind {
			continue
		}
		ok := true
		for flag in flags {
			if !(flag in target.flags) {
				ok = false
			}
		}
		if ok {
			return true
		}
	}
	return false
}

string_list_matches :: proc(values: [dynamic]string, expected: []string) -> bool {
	if len(values) != len(expected) {
		return false
	}
	for i in 0 ..< len(expected) {
		if values[i] != expected[i] {
			return false
		}
	}
	return true
}

internal_table_order_present :: proc(
	unit: ^analyze.Unit_Analysis,
	table_name: string,
	fields: []string,
) -> bool {
	for order in unit.internal_table_orders {
		if order.table_name == table_name && string_list_matches(order.key_fields, fields) {
			return true
		}
	}
	return false
}

binary_search_present :: proc(unit: ^analyze.Unit_Analysis, table_name: string, fields: []string) -> bool {
	for read in unit.read_table_binary_searches {
		if read.table_name == table_name && string_list_matches(read.key_fields, fields) {
			return true
		}
	}
	return false
}

system_update_present :: proc(
	unit: ^analyze.Unit_Analysis,
	statement: analyze.System_Field_Statement_Kind,
	field_name: string,
) -> bool {
	for update in unit.system_field_updates {
		if update.statement == statement && update.field_name == field_name {
			return true
		}
	}
	return false
}

find_text :: proc(source, needle: string) -> int {
	if needle == "" || len(needle) > len(source) {
		return -1
	}
	for i in 0 ..= len(source) - len(needle) {
		if source[i:i + len(needle)] == needle {
			return i
		}
	}
	return -1
}

find_text_last :: proc(source, needle: string) -> int {
	if needle == "" || len(needle) > len(source) {
		return -1
	}
	found := -1
	for i in 0 ..= len(source) - len(needle) {
		if source[i:i + len(needle)] == needle {
			found = i
		}
	}
	return found
}

@(test)
collects_top_level_and_nested_definitions :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lv_local TYPE i.
ENDFORM.

CLASS lcl_demo IMPLEMENTATION.
  METHOD execute.
    DATA lv_inner TYPE i.
  ENDMETHOD.
ENDCLASS.

DATA gv_value TYPE i.
TYPES ty_name TYPE string.
CONSTANTS gc_limit TYPE i VALUE 1.
FIELD-SYMBOLS <fs_row> TYPE any.
`
	unit := collect_test_unit(t, "file:///defs.abap", source)

	testing.expect(t, has_symbol(&unit, .Form, "run"))
	testing.expect(t, has_symbol(&unit, .Class, "lcl_demo"))
	testing.expect(t, has_symbol(&unit, .Method, "execute"))
	testing.expect(t, has_symbol(&unit, .Variable, "gv_value"))
	testing.expect(t, has_symbol(&unit, .Type_Def, "ty_name"))
	testing.expect(t, has_symbol(&unit, .Constant, "gc_limit"))
	testing.expect(t, has_symbol(&unit, .Field_Symbol, "<fs_row>"))
	testing.expect(t, has_scope_kind(&unit, .Form))
	testing.expect(t, has_scope_kind(&unit, .Method))
}

@(test)
collects_single_include_edge :: proc(t: ^testing.T) {
	unit := collect_test_unit(t, "file:///zmain.abap", "INCLUDE zinc.")

	testing.expect_value(t, len(unit.include_edges), 1)
	testing.expect_value(t, unit.include_edges[0].name, "zinc")
	testing.expect(t, !unit.include_edges[0].has_target)
	testing.expect(t, has_symbol(&unit, .Include, "zinc"))
	testing.expect_value(t, len(unit.references), 1)
	testing.expect_value(t, unit.references[0].kind, analyze.Reference_Kind.Include)
	testing.expect_value(t, unit.references[0].name, "zinc")
	testing.expect(t, unit.references[0].has_resolution)
}

@(test)
collects_chained_include_edges :: proc(t: ^testing.T) {
	unit := collect_test_unit(t, "file:///zmain.abap", "INCLUDE: ztop, zf01.")

	testing.expect_value(t, len(unit.include_edges), 2)
	testing.expect_value(t, unit.include_edges[0].name, "ztop")
	testing.expect_value(t, unit.include_edges[1].name, "zf01")
	testing.expect(t, has_symbol(&unit, .Include, "ztop"))
	testing.expect(t, has_symbol(&unit, .Include, "zf01"))
}

@(test)
provided_name_includes_uri_stem :: proc(t: ^testing.T) {
	unit := collect_test_unit(t, "file:///workspace/ZMAIN.abap", "REPORT zother.")

	testing.expect(t, provided_name_present(&unit, "zother"))
	testing.expect(t, provided_name_present(&unit, "zmain"))
}

@(test)
collector_reports_duplicate_and_shadowed_declarations :: proc(t: ^testing.T) {
	source := `
DATA lv_value TYPE i.
DATA lv_value TYPE i.
FORM run.
  DATA lv_value TYPE i.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///diagnostics.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Duplicate_Declaration))
	testing.expect(t, has_diagnostic(&unit, .Shadowed_Symbol))
}

@(test)
collects_tables_work_area_declarations :: proc(t: ^testing.T) {
	unit := collect_test_unit(t, "file:///tables.abap", "TABLES: tbtco, v_op.")

	table_names := [?]string{"tbtco", "v_op"}
	for name in table_names {
		s := analyze.find_symbol(&unit, name, .Variable)
		testing.expect(t, s != nil)
		testing.expect(t, s.has_declared_type)
		testing.expect_value(t, s.declared_type.namespace, analyze.Namespace.Type)
		testing.expect_value(t, s.declared_type.base_name, name)
		testing.expect_value(t, s.type_clause_display, name)
	}
	testing.expect_value(t, len(unit.table_work_areas), 2)
	testing.expect_value(t, unit.table_work_areas[0].name, "tbtco")
	testing.expect_value(t, unit.table_work_areas[1].name, "v_op")
}

@(test)
legacy_occurs_header_line_keeps_declared_type_clean :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///occurs_header.abap",
		"DATA int_eket LIKE beket OCCURS 0 WITH HEADER LINE.",
	)

	s := analyze.find_symbol(&unit, "int_eket", .Variable)
	testing.expect(t, s != nil)
	testing.expect(t, s.has_declared_type)
	testing.expect_value(t, s.declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, s.declared_type.base_name, "beket")
	testing.expect_value(t, s.type_clause_display, "beket")
}

@(test)
type_clause_displays_use_parser_bounded_type_refs :: proc(t: ^testing.T) {
	source := `DATA int_eket LIKE beket OCCURS 0 WITH HEADER LINE.
DATA lv_value TYPE i VALUE 1.
DATA lv_len TYPE c LENGTH 3.
DATA lv_dec TYPE p DECIMALS 2.
DATA mv_text TYPE string READ-ONLY.
PARAMETERS p_count TYPE i DEFAULT 1.
TYPES ty_def TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
TYPES ty_unique TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.`
	unit := collect_test_unit(t, "file:///type_ref_display_bounds.abap", source)

	int_eket := analyze.find_symbol(&unit, "int_eket", .Variable)
	lv_value := analyze.find_symbol(&unit, "lv_value", .Variable)
	lv_len := analyze.find_symbol(&unit, "lv_len", .Variable)
	lv_dec := analyze.find_symbol(&unit, "lv_dec", .Variable)
	mv_text := analyze.find_symbol(&unit, "mv_text", .Variable)
	p_count := analyze.find_symbol(&unit, "p_count", .Variable)
	ty_def := analyze.find_symbol(&unit, "ty_def", .Type_Def)
	ty_unique := analyze.find_symbol(&unit, "ty_unique", .Type_Def)
	testing.expect(t, int_eket != nil && lv_value != nil && lv_len != nil && lv_dec != nil)
	testing.expect(t, mv_text != nil && p_count != nil && ty_def != nil && ty_unique != nil)
	testing.expect_value(t, int_eket.type_clause_display, "beket")
	testing.expect_value(t, lv_value.type_clause_display, "i")
	testing.expect_value(t, lv_len.type_clause_display, "c")
	testing.expect_value(t, lv_dec.type_clause_display, "p")
	testing.expect_value(t, mv_text.type_clause_display, "string")
	testing.expect_value(t, p_count.type_clause_display, "i")
	testing.expect_value(t, ty_def.type_clause_display, "STANDARD TABLE OF string WITH DEFAULT KEY")
	testing.expect_value(t, ty_unique.type_clause_display, "SORTED TABLE OF string WITH UNIQUE KEY table_line")
}

@(test)
declaration_type_refs_use_ast_base_paths_and_ranges :: proc(t: ^testing.T) {
	source := `
INTERFACE lif_demo.
  TYPES ty_line TYPE i.
ENDINTERFACE.
DATA lv_date LIKE sy-datum.
DATA lr_item TYPE REF TO lif_demo=>ty_line.
`
	unit := collect_test_unit(t, "file:///type_ref_ast_paths.abap", source)

	lv_date := analyze.find_symbol(&unit, "lv_date", .Variable)
	lr_item := analyze.find_symbol(&unit, "lr_item", .Variable)
	testing.expect(t, lv_date != nil)
	testing.expect(t, lr_item != nil)
	testing.expect_value(t, lv_date.declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, lv_date.declared_type.base_name, "sy")
	testing.expect_value(t, lv_date.declared_type.field_path[0], "datum")
	testing.expect_value(t, lr_item.declared_type.namespace, analyze.Namespace.Type)
	testing.expect(t, lr_item.declared_type.is_ref)
	testing.expect_value(t, lr_item.declared_type.base_name, "lif_demo")
	testing.expect_value(t, lr_item.declared_type.field_path[0], "ty_line")

	found_sy_path := false
	found_class_path := false
	for access in unit.field_accesses {
		if !access.in_type_position || len(access.field_path) == 0 {
			continue
		}
		if access.base_name == "sy" && access.field_path[0].name == "datum" {
			found_sy_path = true
			testing.expect_value(t, source[access.base_range.start:access.base_range.end], "sy")
			testing.expect_value(t, source[access.field_path[0].range.start:access.field_path[0].range.end], "datum")
		}
		if access.base_name == "lif_demo" && access.field_path[0].name == "ty_line" {
			found_class_path = true
			testing.expect_value(t, source[access.base_range.start:access.base_range.end], "lif_demo")
			testing.expect_value(t, source[access.field_path[0].range.start:access.field_path[0].range.end], "ty_line")
		}
	}
	testing.expect(t, found_sy_path)
	testing.expect(t, found_class_path)
}

@(test)
common_part_delimiters_do_not_emit_bogus_symbols :: proc(t: ^testing.T) {
	source := `
DATA: BEGIN OF COMMON PART fm06lcbe.
DATA: BEGIN OF bet OCCURS 50.
        INCLUDE STRUCTURE ekbe.
DATA: END OF bet.
DATA: END OF COMMON PART.
`
	unit := collect_test_unit(t, "file:///common_part.abap", source)

	testing.expect(t, has_symbol(&unit, .Variable, "bet"))
	bogus := [?]string{"begin", "common", "end"}
	for name in bogus {
		testing.expect(t, !has_symbol(&unit, .Variable, name))
		testing.expect(t, !has_reference(&unit, name, .Value, .Identifier))
	}
}

@(test)
declarations_named_common_still_collect :: proc(t: ^testing.T) {
	source := `
DATA common TYPE i.
CLASS lcl_holder DEFINITION.
  PUBLIC SECTION.
    DATA common TYPE i.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///common_name.abap", source)

	testing.expect(t, has_symbol(&unit, .Variable, "common"))
	class := analyze.find_symbol(&unit, "lcl_holder", .Class)
	testing.expect(t, class != nil)
	testing.expect(t, class_member_named(&unit, class.id, "common", .Attribute) != nil)
}

@(test)
constant_structure_collects_numeric_prefixed_component_names :: proc(t: ^testing.T) {
	source := `
CONSTANTS: BEGIN OF gc_bapi_proc_mode,
             aip VALUE 'A',
             46c VALUE 'B',
           END OF gc_bapi_proc_mode.
`
	unit := collect_test_unit(t, "file:///constant_components.abap", source)

	s := analyze.find_symbol(&unit, "gc_bapi_proc_mode", .Constant)
	testing.expect(t, s != nil)
	testing.expect(t, s.structure != analyze.INVALID_STRUCTURE_ID)
	st := analyze.structure(&unit, s.structure)
	const_fields := [?]string{"aip", "46c"}
	testing.expect(t, field_names_match(st, const_fields[:]))
	testing.expect_value(t, st.fields[0].value_clause_display, "'A'")
	testing.expect_value(t, st.fields[1].value_clause_display, "'B'")
}

@(test)
structured_includes_expand_known_members :: proc(t: ^testing.T) {
	source := `
TYPES: BEGIN OF ty_base,
         a TYPE i,
       END OF ty_base.
TYPES: BEGIN OF ty_wrap,
         INCLUDE TYPE ty_base,
         b TYPE string,
       END OF ty_wrap.
`
	unit := collect_test_unit(t, "file:///structured_include.abap", source)

	wrap := analyze.find_symbol(&unit, "ty_wrap", .Type_Def)
	testing.expect(t, wrap != nil)
	st := analyze.structure(&unit, wrap.structure)
	wrap_fields := [?]string{"a", "b"}
	testing.expect(t, field_names_match(st, wrap_fields[:]))
	testing.expect_value(t, st.fields[0].type_ref.base_name, "i")
	testing.expect_value(t, st.fields[1].type_ref.base_name, "string")
}

@(test)
project_index_include_graph_rebuild_uses_index_allocator :: proc(t: ^testing.T) {
	arena: virtual.Arena
	_ = virtual.arena_init_growing(&arena)
	defer virtual.arena_destroy(&arena)

	index := analyze.project_index_make(virtual.arena_allocator(&arena))
	units := make([dynamic]analyze.Unit_Analysis, 0, 2, context.allocator)
	defer delete(units)

	append(&units, analyze.Unit_Analysis{unit_id = analyze.Unit_Id(0)})
	first := [?]analyze.Unit_Id{analyze.Unit_Id(0)}
	analyze.project_index_update_include_graph(&index, units[:], first[:])

	append(&units, analyze.Unit_Analysis{unit_id = analyze.Unit_Id(1)})
	second := [?]analyze.Unit_Id{analyze.Unit_Id(1)}
	analyze.project_index_update_include_graph(&index, units[:], second[:])

	testing.expect_value(t, len(index.visible), 2)
	testing.expect_value(t, len(index.predecessors), 2)
}

@(test)
multi_statement_structured_include_expands_known_members :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///multi_statement_structured_include.abap",
		source = `
INTERFACE lif_repo.
  TYPES: BEGIN OF ty_repo_xml,
           url TYPE string,
         END OF ty_repo_xml.
  TYPES: BEGIN OF ty_repo,
           key TYPE string.
      INCLUDE TYPE ty_repo_xml.
  TYPES: END OF ty_repo.
ENDINTERFACE.

DATA ls_repo TYPE lif_repo=>ty_repo.
ls_repo-url = 'https://example.invalid'.
`,
	}

	project := analyze_project_test(t, 0, target, nil)
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, !has_diagnostic(root, .Unknown_Field))
}

@(test)
selection_ranges_collect_range_structure :: proc(t: ^testing.T) {
	source := `
TYPES zattp_gln TYPE string.
DATA lv_rogln TYPE zattp_gln.
RANGES r_rogln FOR lv_rogln.
SELECT-OPTIONS s_rogln FOR lv_rogln.
`
	unit := collect_test_unit(t, "file:///ranges.abap", source)

	range_names := [?]string{"r_rogln", "s_rogln"}
	for name in range_names {
		s := analyze.find_symbol(&unit, name, .Variable)
		testing.expect(t, s != nil)
		testing.expect(t, s.structure != analyze.INVALID_STRUCTURE_ID)
		st := analyze.structure(&unit, s.structure)
		range_fields := [?]string{"sign", "option", "low", "high"}
		testing.expect(t, field_names_match(st, range_fields[:]))
		testing.expect_value(t, st.fields[2].type_ref.namespace, analyze.Namespace.Value)
		testing.expect_value(t, st.fields[2].type_ref.base_name, "lv_rogln")
	}
}

@(test)
collects_class_members_inheritance_interfaces_and_method_signature :: proc(t: ^testing.T) {
	source := `
INTERFACE lif_demo.
  METHODS base.
ENDINTERFACE.

CLASS zcl_base DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS base IMPORTING iv_code TYPE string.
ENDCLASS.

CLASS zcl_child DEFINITION INHERITING FROM zcl_base.
  PUBLIC SECTION.
    INTERFACES lif_demo.
    ALIASES alias_base FOR lif_demo~base.
    DATA mv_flag TYPE i.
    CLASS-DATA gv_count TYPE i.
    EVENTS changed EXPORTING VALUE(ev_flag) TYPE i.
    METHODS run IMPORTING iv_value TYPE i RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///class_members.abap", source)

	base := analyze.find_symbol(&unit, "zcl_base", .Class)
	child := analyze.find_symbol(&unit, "zcl_child", .Class)
	testing.expect(t, base != nil)
	testing.expect(t, child != nil)
	testing.expect(t, len(unit.class_definitions) >= 2)
	testing.expect(t, unit.class_definitions[0].is_abstract)
	testing.expect_value(t, unit.class_inheritance[0].class_symbol, child.id)
	testing.expect_value(t, unit.class_inheritance[0].superclass_name, "zcl_base")
	testing.expect_value(t, unit.implemented_interfaces[0].interface_name, "lif_demo")
	testing.expect_value(t, unit.member_aliases[0].alias_name, "alias_base")
	testing.expect_value(t, unit.member_aliases[0].target_interface_name, "lif_demo")
	testing.expect_value(t, unit.member_aliases[0].target_member_name, "base")

	attr := class_member_named(&unit, child.id, "mv_flag", .Attribute)
	static_attr := class_member_named(&unit, child.id, "gv_count", .Attribute)
	event := class_member_named(&unit, child.id, "changed", .Event)
	method := class_member_named(&unit, child.id, "run", .Method)
	testing.expect(t, attr != nil)
	testing.expect(t, static_attr != nil)
	testing.expect(t, .Is_Static in static_attr.flags)
	testing.expect(t, event != nil)
	testing.expect_value(t, event.parameters[0].name, "ev_flag")
	testing.expect_value(t, event.parameters[0].passing, analyze.Parameter_Passing_Kind.Value)
	testing.expect(t, method != nil)
	testing.expect(t, .Has_Implementation in method.flags)
	testing.expect_value(t, len(method.parameters), 2)
	testing.expect_value(t, method.parameters[0].section, analyze.Method_Parameter_Section.Importing)
	testing.expect_value(t, method.parameters[0].name, "iv_value")
	testing.expect_value(t, method.parameters[0].passing, analyze.Parameter_Passing_Kind.Direct)
	testing.expect_value(t, method.parameters[1].section, analyze.Method_Parameter_Section.Returning)
	testing.expect_value(t, method.parameters[1].name, "rv_text")
	testing.expect_value(t, method.parameters[1].passing, analyze.Parameter_Passing_Kind.Value)
}

@(test)
interface_method_implementation_resolves_me :: proc(t: ^testing.T) {
	source := `INTERFACE lif_log.
  METHODS merge RETURNING VALUE(ro_log) TYPE REF TO lif_log.
ENDINTERFACE.
CLASS lcl_log DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_log.
ENDCLASS.
CLASS lcl_log IMPLEMENTATION.
  METHOD lif_log~merge.
    ro_log = me.
  ENDMETHOD.
ENDCLASS.`
	unit := collect_test_unit(t, "file:///interface_method_me.abap", source)

	testing.expect(t, has_reference(&unit, "me", .Value, .Identifier))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
oop_signature_type_refs_use_ast_paths :: proc(t: ^testing.T) {
	source := `
INTERFACE lif_demo.
  TYPES scriptcallphase_enum TYPE i.
ENDINTERFACE.
CLASS lcl_date DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_date LIKE sy-datum phase LIKE lif_demo=>scriptcallphase_enum.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///oop_type_ref_paths.abap", source)

	class := analyze.find_symbol(&unit, "lcl_date", .Class)
	testing.expect(t, class != nil)
	method := class_member_named(&unit, class.id, "run", .Method)
	testing.expect(t, method != nil)
	testing.expect_value(t, len(method.parameters), 2)
	date := method.parameters[0]
	testing.expect(t, .Has_Declared_Type in date.flags)
	testing.expect_value(t, date.declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, date.declared_type.base_name, "sy")
	testing.expect_value(t, date.declared_type.field_path[0], "datum")
	phase := method.parameters[1]
	testing.expect(t, .Has_Declared_Type in phase.flags)
	testing.expect_value(t, phase.declared_type.namespace, analyze.Namespace.Type)
	testing.expect_value(t, phase.declared_type.base_name, "lif_demo")
	testing.expect_value(t, phase.declared_type.field_path[0], "scriptcallphase_enum")
	testing.expect(t, has_reference(&unit, "sy", .Value, .Type_Ref))
	testing.expect(t, has_reference(&unit, "lif_demo", .Type, .Type_Ref))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
collects_oop_section_visibility_from_ast :: proc(t: ^testing.T) {
	source := `CLASS lcl_vis DEFINITION.
  PUBLIC SECTION.
    METHODS pub.
  PROTECTED SECTION.
    METHODS prot.
  PRIVATE SECTION.
    METHODS priv.
ENDCLASS.
INTERFACE lif_vis.
  PUBLIC SECTION.
    METHODS if_pub.
ENDINTERFACE.`
	unit := collect_test_unit(t, "file:///oop_visibility.abap", source)

	class := analyze.find_symbol(&unit, "lcl_vis", .Class)
	iface := analyze.find_symbol(&unit, "lif_vis", .Interface)
	testing.expect(t, class != nil)
	testing.expect(t, iface != nil)

	pub := class_member_named(&unit, class.id, "pub", .Method)
	prot := class_member_named(&unit, class.id, "prot", .Method)
	priv := class_member_named(&unit, class.id, "priv", .Method)
	if_pub := class_member_named(&unit, iface.id, "if_pub", .Method)
	testing.expect(t, pub != nil)
	testing.expect(t, prot != nil)
	testing.expect(t, priv != nil)
	testing.expect(t, if_pub != nil)
	testing.expect_value(t, pub.visibility, analyze.Visibility.Public)
	testing.expect_value(t, prot.visibility, analyze.Visibility.Protected)
	testing.expect_value(t, priv.visibility, analyze.Visibility.Private)
	testing.expect_value(t, if_pub.visibility, analyze.Visibility.Public)
}

@(test)
collects_class_header_facts_from_ast :: proc(t: ^testing.T) {
	source := `CLASS zcl_abs DEFINITION ABSTRACT.
ENDCLASS.
CLASS zcl_super DEFINITION.
ENDCLASS.
CLASS zcl_child DEFINITION INHERITING FROM zcl_super.
ENDCLASS.
CLASS zcl_impl DEFINITION.
ENDCLASS.
CLASS zcl_impl IMPLEMENTATION.
ENDCLASS.
CLASS zcl_deferred DEFINITION DEFERRED.`
	unit := collect_test_unit(t, "file:///class_header_facts.abap", source)

	abs := analyze.find_symbol(&unit, "zcl_abs", .Class)
	child := analyze.find_symbol(&unit, "zcl_child", .Class)
	impl := analyze.find_symbol(&unit, "zcl_impl", .Class)
	deferred := analyze.find_symbol(&unit, "zcl_deferred", .Class)
	testing.expect(t, abs != nil)
	testing.expect(t, child != nil)
	testing.expect(t, impl != nil)
	testing.expect(t, deferred != nil)

	abs_is_abstract := false
	deferred_has_definition := false
	for definition in unit.class_definitions {
		if definition.class_symbol == abs.id {
			abs_is_abstract = definition.is_abstract
		}
		if definition.class_symbol == deferred.id {
			deferred_has_definition = true
		}
	}
	testing.expect(t, abs_is_abstract)
	testing.expect(t, !deferred_has_definition)

	child_inherits_super := false
	for inheritance in unit.class_inheritance {
		if inheritance.class_symbol == child.id && inheritance.superclass_name == "zcl_super" {
			child_inherits_super = true
		}
	}
	testing.expect(t, child_inherits_super)

	impl_symbol_count := 0
	for symbol in unit.symbols {
		if symbol.kind == .Class && symbol.name == "zcl_impl" {
			impl_symbol_count += 1
		}
	}
	testing.expect_value(t, impl_symbol_count, 1)
}

@(test)
deferred_class_and_interface_definitions_reuse_forward_symbol :: proc(t: ^testing.T) {
	source := `CLASS zcl_forward DEFINITION DEFERRED.
INTERFACE zif_forward DEFERRED.

CLASS zcl_forward DEFINITION.
ENDCLASS.
INTERFACE zif_forward.
ENDINTERFACE.`
	unit := collect_test_unit(t, "file:///forward_types.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Duplicate_Declaration))

	class_count := 0
	interface_count := 0
	for symbol in unit.symbols {
		if symbol.kind == .Class && symbol.name == "zcl_forward" {
			class_count += 1
		}
		if symbol.kind == .Interface && symbol.name == "zif_forward" {
			interface_count += 1
		}
	}
	testing.expect_value(t, class_count, 1)
	testing.expect_value(t, interface_count, 1)
}

@(test)
qualified_method_redefinitions_do_not_duplicate_interface_name :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS lif_demo~create REDEFINITION.
    METHODS lif_demo~delete REDEFINITION.
ENDCLASS.`
	unit := collect_test_unit(t, "file:///qualified_redefinitions.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Duplicate_Declaration))
	testing.expect(t, has_symbol(&unit, .Method, "create"))
	testing.expect(t, has_symbol(&unit, .Method, "delete"))
}

@(test)
qualified_method_redefinition_qualifier_is_not_order_checked_type_ref :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS lif_demo~run REDEFINITION.
ENDCLASS.

INTERFACE lif_demo.
  METHODS run.
ENDINTERFACE.`
	unit := collect_test_unit(t, "file:///qualified_method_late_interface.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, has_reference(&unit, "lif_demo", .Type, .Interface_Use))
	testing.expect(t, !has_reference(&unit, "lif_demo", .Type, .Type_Ref))
}

@(test)
interface_implementation_is_not_order_checked_type_ref :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_demo.
ENDCLASS.

INTERFACE lif_demo.
ENDINTERFACE.`
	unit := collect_test_unit(t, "file:///late_interface_implementation.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, has_reference(&unit, "lif_demo", .Type, .Interface_Use))
	testing.expect(t, !has_reference(&unit, "lif_demo", .Type, .Type_Ref))
}

@(test)
program_event_blocks_do_not_duplicate_event_symbols :: proc(t: ^testing.T) {
	source := `AT SELECTION-SCREEN ON EXIT-COMMAND.
  DATA lv_exit TYPE i.

AT SELECTION-SCREEN.
  DATA lv_screen TYPE i.`
	unit := collect_test_unit(t, "file:///selection_screen_events.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Duplicate_Declaration))
}

@(test)
inline_statement_targets_are_declared_once :: proc(t: ^testing.T) {
	source := `DATA lt_values TYPE STANDARD TABLE OF i.
DATA lv_text TYPE string.
READ TABLE lt_values INDEX 1 ASSIGNING FIELD-SYMBOL(<row>).
MESSAGE i001 INTO DATA(lv_message).
ASSIGN lv_text TO FIELD-SYMBOL(<text>).`
	unit := collect_test_unit(t, "file:///inline_statement_targets.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Duplicate_Declaration))
	testing.expect(t, has_symbol(&unit, .Field_Symbol, "<row>"))
	testing.expect(t, has_symbol(&unit, .Variable, "lv_message"))
	testing.expect(t, has_symbol(&unit, .Field_Symbol, "<text>"))
}

@(test)
constructor_for_iterators_are_expression_scoped :: proc(t: ^testing.T) {
	source := `DATA(lv_first) = REDUCE i( INIT x = 0 FOR i = 0 UNTIL i > 1 NEXT x = x + i ).
DATA(lv_second) = REDUCE i( INIT x = 0 FOR i = 0 UNTIL i > 1 NEXT x = x + i ).`
	unit := collect_test_unit(t, "file:///constructor_for_scope.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Duplicate_Declaration))
	testing.expect_value(t, len(unit.constructor_for_bindings), 2)
}

@(test)
collects_multiple_method_parameters_from_oop_ast :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING it_source TYPE STANDARD TABLE iv_state TYPE i OPTIONAL iv_text TYPE string
      RETURNING VALUE(rv_ok) TYPE abap_bool.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA lv_state TYPE i.
    lv_state = iv_state.
  ENDMETHOD.
ENDCLASS.`
	unit := collect_test_unit(t, "file:///oop_params.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	class := analyze.find_symbol(&unit, "lcl", .Class)
	testing.expect(t, class != nil)
	method := class_member_named(&unit, class.id, "run", .Method)
	testing.expect(t, method != nil)
	testing.expect_value(t, len(method.parameters), 4)
	testing.expect_value(t, method.parameters[0].name, "it_source")
	testing.expect_value(t, method.parameters[1].name, "iv_state")
	testing.expect(t, .Is_Optional in method.parameters[1].flags)
	testing.expect_value(t, method.parameters[2].name, "iv_text")
	testing.expect_value(t, method.parameters[3].section, analyze.Method_Parameter_Section.Returning)
	testing.expect_value(t, method.parameters[3].name, "rv_ok")
}

@(test)
resolves_old_style_exception_raise :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run EXCEPTIONS failed.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    RAISE failed.
  ENDMETHOD.
ENDCLASS.
FUNCTION z_demo
  EXCEPTIONS not_found.
  RAISE not_found.
ENDFUNCTION.`
	unit := collect_test_unit(t, "file:///old_style_exceptions.abap", source)

	class := analyze.find_symbol(&unit, "lcl", .Class)
	testing.expect(t, class != nil)
	method := class_member_named(&unit, class.id, "run", .Method)
	testing.expect(t, method != nil)
	testing.expect_value(t, len(method.exceptions), 1)
	testing.expect_value(t, method.exceptions[0].name, "failed")
	testing.expect(t, has_symbol(&unit, .Exception, "failed"))
	testing.expect(t, has_symbol(&unit, .Exception, "not_found"))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
collects_form_and_function_signatures :: proc(t: ^testing.T) {
	source := `
FORM run TABLES !ct_rows STRUCTURE mara USING VALUE(iv_text) TYPE string REFERENCE(iv_ref) LIKE sy-uname CHANGING cv_count TYPE i.
ENDFORM.

FUNCTION z_demo
  IMPORTING VALUE(iv_value) TYPE i OPTIONAL iv_text TYPE string DEFAULT 'x'
  EXPORTING ev_text LIKE sy-uname
  CHANGING REFERENCE(cv_any) TYPE REF TO object
  TABLES et_return STRUCTURE bapiret2
  EXCEPTIONS failed = 1 not_found.
ENDFUNCTION.
`
	unit := collect_test_unit(t, "file:///signatures.abap", source)

	testing.expect_value(t, len(unit.form_routines), 1)
	form := unit.form_routines[0]
	testing.expect_value(t, len(form.parameters), 4)
	ct_rows := unit.symbols[analyze.symbol_id_index(form.parameters[0].symbol)]
	iv_text := unit.symbols[analyze.symbol_id_index(form.parameters[1].symbol)]
	iv_ref := unit.symbols[analyze.symbol_id_index(form.parameters[2].symbol)]
	testing.expect_value(t, ct_rows.name, "ct_rows")
	testing.expect_value(t, ct_rows.declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, form.parameters[0].section, analyze.Form_Parameter_Section.Tables)
	testing.expect_value(t, form.parameters[1].section, analyze.Form_Parameter_Section.Using)
	testing.expect_value(t, form.parameters[1].passing, analyze.Form_Parameter_Passing_Kind.Value)
	testing.expect_value(t, iv_text.name, "iv_text")
	testing.expect_value(t, form.parameters[2].passing, analyze.Form_Parameter_Passing_Kind.Reference)
	testing.expect_value(t, iv_ref.declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, iv_ref.declared_type.base_name, "sy")
	testing.expect_value(t, iv_ref.declared_type.field_path[0], "uname")
	testing.expect_value(t, form.parameters[3].section, analyze.Form_Parameter_Section.Changing)

	testing.expect_value(t, len(unit.function_modules), 1)
	fm := unit.function_modules[0]
	testing.expect_value(t, len(fm.parameters), 5)
	testing.expect_value(t, fm.parameters[0].name, "iv_value")
	testing.expect_value(t, fm.parameters[0].passing, analyze.Parameter_Passing_Kind.Value)
	testing.expect(t, .Is_Optional in fm.parameters[0].flags)
	testing.expect_value(t, fm.parameters[1].name, "iv_text")
	testing.expect_value(t, fm.parameters[1].passing, analyze.Parameter_Passing_Kind.Direct)
	testing.expect(t, .Has_Default_Value in fm.parameters[1].flags)
	testing.expect_value(t, fm.parameters[2].section, analyze.Function_Module_Parameter_Section.Exporting)
	testing.expect_value(t, fm.parameters[2].declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, fm.parameters[2].declared_type.base_name, "sy")
	testing.expect_value(t, fm.parameters[2].declared_type.field_path[0], "uname")
	testing.expect_value(t, fm.parameters[3].section, analyze.Function_Module_Parameter_Section.Changing)
	testing.expect_value(t, fm.parameters[3].passing, analyze.Parameter_Passing_Kind.Reference)
	testing.expect_value(t, fm.parameters[3].declared_type.base_name, "object")
	testing.expect_value(t, fm.parameters[4].section, analyze.Function_Module_Parameter_Section.Tables)
	testing.expect_value(t, fm.parameters[4].declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, fm.parameters[4].declared_type.base_name, "bapiret2")
	testing.expect_value(t, len(fm.exceptions), 2)
	testing.expect_value(t, fm.exceptions[0].name, "failed")
	testing.expect_value(t, fm.exceptions[1].name, "not_found")
	testing.expect(t, has_reference(&unit, "string", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "sy", .Value, .Type_Ref))
	testing.expect(t, has_reference(&unit, "bapiret2", .Value, .Type_Ref))
}

@(test)
resolves_local_variable_references :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///local_refs.abap",
		`
FORM run.
  DATA lv_count TYPE i.
  lv_count = lv_count + 1.
ENDFORM.
`,
	)

	testing.expect_value(t, reference_count(&unit, "lv_count", .Value, .Identifier), 2)
	for reference in unit.references {
		if reference.name == "lv_count" && reference.kind == .Identifier {
			testing.expect(t, reference.has_resolution)
			testing.expect_value(t, reference.resolution.kind, analyze.Resolution_Kind.Symbol)
		}
	}
	testing.expect_value(t, len(unit.assignment_sites), 1)
}

@(test)
get_time_stamp_field_does_not_reference_keywords :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///get_time_stamp.abap",
		`
FORM run.
  DATA lv_timestamp TYPE timestamp.
  GET TIME STAMP FIELD lv_timestamp.
ENDFORM.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_reference(&unit, "time", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "stamp", .Value, .Identifier))
	testing.expect_value(t, reference_count(&unit, "lv_timestamp", .Value, .Identifier), 1)
}

@(test)
validates_unresolved_and_wrong_namespace_references :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///reference_diagnostics.abap",
		`
TYPES ty_value TYPE i.
ty_value = 1.
missing = 2.
`,
	)

	testing.expect(t, has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect(t, has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
create_object_type_clause_uses_type_namespace :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///create_object_type.abap",
		`
CLASS lcl_html DEFINITION.
ENDCLASS.

DATA ri_html TYPE REF TO lcl_html.

START-OF-SELECTION.
  CREATE OBJECT ri_html TYPE lcl_html.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect_value(t, reference_count(&unit, "lcl_html", .Type, .Type_Ref), 2)
	testing.expect_value(t, reference_count(&unit, "lcl_html", .Value, .Identifier), 0)
}

@(test)
create_data_dynamic_literal_type_is_not_remote_dependency :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///create_data_dynamic_literal.abap",
		source = `
DATA lr_xml_api TYPE REF TO data.
CREATE DATA lr_xml_api TYPE REF TO ('CL_W3_API_XML3').
`,
	}
	project := analyze_project_test(t, 0, target, nil)
	unit := &project.units[0]
	candidates := analyze.collect_project_remote_dependency_candidates(&project, context.allocator)

	testing.expect(t, !has_diagnostic(unit, .Unresolved_Reference))
	testing.expect(t, !has_reference(unit, "('cl_w3_api_xml3')", .Type, .Type_Ref))
	for candidate in candidates {
		testing.expect(t, candidate.name != "('cl_w3_api_xml3')")
	}
}

@(test)
create_data_dynamic_table_type_uses_runtime_name_expr :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///create_data_dynamic_table.abap",
		`
TYPES: BEGIN OF ty_table,
         tobj_name TYPE string,
       END OF ty_table.
FIELD-SYMBOLS <ls_table> TYPE ty_table.
DATA lv_primary TYPE string.
DATA lr_ref TYPE REF TO data.

CREATE DATA lr_ref TYPE STANDARD TABLE OF (<ls_table>-tobj_name).
CREATE DATA lr_ref TYPE STANDARD TABLE OF (lv_primary).
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_reference(&unit, "standard table of (<ls_table>-tobj_name)", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "<ls_table>", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_primary", .Value, .Identifier))
}

@(test)
resolves_form_changing_parameter_in_body :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///form_params.abap",
		`
FORM some_form CHANGING cv_result TYPE string.
  DATA lv_text TYPE string.
  cv_result = lv_text.
ENDFORM.
`,
	)

	param := analyze.find_symbol(&unit, "cv_result", .Parameter)
	testing.expect(t, param != nil)
	testing.expect(t, param.has_declared_type)
	testing.expect_value(t, param.declared_type.namespace, analyze.Namespace.Type)
	testing.expect_value(t, param.declared_type.base_name, "string")
	for reference in unit.references {
		if reference.name == "cv_result" {
			testing.expect(t, reference.has_resolution)
			testing.expect_value(t, reference.resolution.symbol.symbol, param.id)
		}
	}
}

@(test)
resolves_redefined_method_inherited_parameters :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///redefined_method_params.abap",
		`
CLASS lcl_root DEFINITION.
  PUBLIC SECTION.
    METHODS get_source_position
      EXPORTING
        program_name TYPE string
        include_name TYPE string
        source_line  TYPE i.
ENDCLASS.

CLASS lcl_child DEFINITION INHERITING FROM lcl_root.
  PUBLIC SECTION.
    METHODS get_source_position REDEFINITION.
ENDCLASS.

CLASS lcl_root IMPLEMENTATION.
  METHOD get_source_position.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_child IMPLEMENTATION.
  METHOD get_source_position.
    include_name = program_name.
    source_line = source_line.
  ENDMETHOD.
ENDCLASS.
`,
	)

	names := [?]string{"program_name", "include_name", "source_line"}
	for name in names {
		found := false
		for reference in unit.references {
			if reference.name == name && reference.kind == .Identifier {
				found = true
				testing.expect(t, reference.has_resolution)
			}
		}
		testing.expect(t, found)
	}
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
resolves_qualified_interface_method_parameters :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///qualified_interface_method_params.abap",
		`
INTERFACE lif_message.
  METHODS get_longtext
    IMPORTING preserve_newlines TYPE abap_bool.
ENDINTERFACE.

INTERFACE lif_t100_message.
  INTERFACES lif_message.
ENDINTERFACE.

CLASS lcl_exception DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_t100_message.
    METHODS lif_message~get_longtext REDEFINITION.
ENDCLASS.

CLASS lcl_exception IMPLEMENTATION.
  METHOD lif_message~get_longtext.
    DATA lv_keep TYPE abap_bool.
    lv_keep = preserve_newlines.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	resolved := false
	for reference in unit.references {
		if reference.name == "preserve_newlines" &&
		   reference.namespace == .Value &&
		   reference.kind == .Identifier &&
		   reference.has_resolution {
			resolved = true
		}
	}
	testing.expect(t, resolved)
}

@(test)
resolves_qualified_interface_method_table_line_fields :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/page.abap",
		source = `
CLASS lcl_page DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      zif_hotkeys.
    METHODS zif_hotkeys~get_hotkey_actions REDEFINITION.
ENDCLASS.

CLASS lcl_page IMPLEMENTATION.
  METHOD zif_hotkeys~get_hotkey_actions.
    DATA ls_hotkey_action LIKE LINE OF rt_hotkey_actions.
    ls_hotkey_action-description = 'Stage'.
  ENDMETHOD.
ENDCLASS.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/interfaces/zif_hotkeys.abap",
			source = `
INTERFACE zif_hotkeys DEFERRED.

CLASS cx_root DEFINITION.
ENDCLASS.

INTERFACE zif_hotkeys.
  TYPES:
    BEGIN OF ty_hotkey_action,
      ui_component TYPE string,
      action TYPE string,
      hotkey TYPE string,
      description TYPE string,
    END OF ty_hotkey_action.
  TYPES ty_hotkey_actions TYPE STANDARD TABLE OF ty_hotkey_action
    WITH DEFAULT KEY
    WITH UNIQUE SORTED KEY action COMPONENTS ui_component action.
  METHODS get_hotkey_actions
    RETURNING VALUE(rt_hotkey_actions) TYPE ty_hotkey_actions
    RAISING cx_root.
ENDINTERFACE.
`,
			mode = .Dependency_Interface,
		},
	}

	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, root != nil && !has_diagnostic(root, .Unresolved_Reference))
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))
}

@(test)
resolves_class_type_ref_to :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///class_ref_to.abap",
		`
CLASS c1 DEFINITION.
ENDCLASS.

DATA lo_c1 TYPE REF TO c1.
`,
	)

	c1 := analyze.find_symbol(&unit, "c1", .Class)
	lo_c1 := analyze.find_symbol(&unit, "lo_c1", .Variable)
	testing.expect(t, c1 != nil)
	testing.expect(t, lo_c1 != nil)
	testing.expect(t, lo_c1.has_declared_type)
	testing.expect(t, lo_c1.declared_type.is_ref)
	testing.expect_value(t, lo_c1.declared_type.base_name, "c1")
	for reference in unit.references {
		if reference.name == "c1" && reference.kind == .Type_Ref {
			testing.expect(t, reference.has_resolution)
			testing.expect_value(t, reference.resolution.symbol.symbol, c1.id)
		}
	}
}

@(test)
resolves_table_of_ref_to_interface_type :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///table_ref_to_interface.abap",
		`
INTERFACE lif_demo.
  TYPES ty_tab TYPE STANDARD TABLE OF REF TO lif_demo WITH KEY table_line.
ENDINTERFACE.
`,
	)

	ty_tab := analyze.find_symbol(&unit, "ty_tab", .Type_Def)
	testing.expect(t, ty_tab != nil)
	testing.expect(t, ty_tab.has_declared_type)
	testing.expect(t, ty_tab.declared_type.is_ref)
	testing.expect_value(t, ty_tab.declared_type.base_name, "lif_demo")
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Invalid_Object_Type_Reference))
}

@(test)
resolves_class_qualified_local_type_refs :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///class_qualified_type.abap",
		`
CLASS lcl_archive_connector DEFINITION.
  PUBLIC SECTION.
    TYPES tr_retriable_errs TYPE RANGE OF string.
ENDCLASS.

DATA(lr_retriable_errs) = VALUE lcl_archive_connector=>tr_retriable_errs( ).
`,
	)

	class_symbol := analyze.find_symbol(&unit, "lcl_archive_connector", .Class)
	testing.expect(t, class_symbol != nil)
	testing.expect(t, has_symbol(&unit, .Type_Def, "tr_retriable_errs"))
	testing.expect(t, has_symbol(&unit, .Variable, "lr_retriable_errs"))
	found_type_ref := false
	for reference in unit.references {
		if reference.name == "lcl_archive_connector" && reference.kind == .Type_Ref {
			found_type_ref = true
			testing.expect(t, reference.has_resolution)
			testing.expect_value(t, reference.resolution.symbol.symbol, class_symbol.id)
		}
	}
	testing.expect(t, found_type_ref)
}

@(test)
resolves_builtin_screen_and_syst_fields :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///builtins_fields.abap",
		`
IF sy-subrc = 0.
ENDIF.

LOOP AT SCREEN.
  IF screen-name = 'P_FOO'.
  ENDIF.
ENDLOOP.
`,
	)

	names := [?]string{"sy", "screen"}
	for name in names {
		for reference in unit.references {
			if reference.name == name && reference.kind == .Identifier {
				testing.expect(t, reference.has_resolution)
				testing.expect_value(t, reference.resolution.kind, analyze.Resolution_Kind.Symbol)
			}
		}
	}
	syst := analyze.find_structure(&unit, "syst")
	screen := analyze.find_structure(&unit, "screen")
	testing.expect(t, syst != nil)
	testing.expect(t, screen != nil)
	subrc, subrc_ok := analyze.structure_field_info(&unit, syst.id, "subrc")
	screen_name, screen_ok := analyze.structure_field_info(&unit, screen.id, "name")
	testing.expect(t, subrc_ok)
	testing.expect(t, screen_ok)
	testing.expect_value(t, subrc.type_ref.base_name, "i")
	testing.expect_value(t, screen_name.type_ref.base_name, "c")
}

@(test)
semantic_queries_find_symbols_references_sql_and_facts :: proc(t: ^testing.T) {
	source := `DATA lv_value TYPE i.
DATA lv_copy TYPE i.
lv_copy = lv_value.
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).`
	unit := collect_test_unit(t, "file:///semantic_queries.abap", source)
	query := sem_query.semantic(&unit)
	decl_query := sem_query.decls(query)
	ref_query := sem_query.refs(query)
	sql_query := sem_query.sql(query)
	fact_query := sem_query.facts(query)

	decl_offset := find_text(source, "lv_value")
	use_offset := find_text_last(source, "lv_value")
	testing.expect(t, decl_offset >= 0)
	testing.expect(t, use_offset > decl_offset)

	sym := sem_query.decl_symbol_at_offset(decl_query, decl_offset)
	testing.expect(t, sym != nil)
	testing.expect_value(t, sym.name, "lv_value")

	by_range := sem_query.decl_symbol_with_kind_and_decl_range(decl_query, .Variable, sym.decl_range)
	testing.expect(t, by_range != nil)
	testing.expect_value(t, by_range.id, sym.id)

	ref := sem_query.ref_reference_at_offset(ref_query, use_offset)
	testing.expect(t, ref != nil)
	testing.expect_value(t, ref.name, "lv_value")
	testing.expect(t, ref.has_resolution)

	exact_ref := sem_query.ref_reference_at_range(ref_query, ref.range)
	testing.expect(t, exact_ref != nil)
	testing.expect_value(t, exact_ref.id, ref.id)

	resolved := sem_query.ref_resolving_to(
		ref_query,
		analyze.Symbol_Handle{unit = unit.unit_id, symbol = sym.id},
		context.allocator,
	)
	testing.expect_value(t, len(resolved), 1)

	source_offset := find_text(source, "scarr")
	sql_ref := sem_query.sql_name_ref_at_offset(sql_query, source_offset)
	testing.expect(t, sql_ref != nil)
	testing.expect_value(t, sql_ref.kind, analyze.Sql_Name_Ref_Kind.Source)
	testing.expect(t, sem_query.sql_has_source_named(sql_query, "SCARR"))
	sql_sources := sem_query.sql_source_name_refs_named(sql_query, "scarr", context.allocator)
	testing.expect_value(t, len(sql_sources), 1)

	fact := sem_query.fact_expression_fact_at_offset(fact_query, use_offset)
	testing.expect(t, fact != nil)
	testing.expect_value(t, fact.kind, analyze.Expression_Fact_Kind.Reference)
}

@(test)
semantic_queries_find_class_members_and_structure_fields :: proc(t: ^testing.T) {
	source := `
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

TYPES: BEGIN OF ty_demo,
         comp TYPE i,
       END OF ty_demo.
`
	unit := collect_test_unit(t, "file:///decl_queries.abap", source)
	query := sem_query.semantic(&unit)
	decl_query := sem_query.decls(query)

	method_offset := find_text(source, "run")
	member := sem_query.decl_class_member_at_offset(decl_query, method_offset)
	testing.expect(t, member != nil)
	testing.expect_value(t, member.kind, analyze.Class_Member_Kind.Method)
	testing.expect_value(t, member.name, "run")

	class_symbol := analyze.find_symbol(&unit, "lcl_demo", .Class)
	testing.expect(t, class_symbol != nil)
	member_by_name := sem_query.decl_class_member(decl_query, class_symbol.id, "RUN")
	testing.expect(t, member_by_name != nil)
	testing.expect_value(t, member_by_name.name, "run")

	field_offset := find_text(source, "comp")
	field, ok := sem_query.decl_structure_field_at_offset(decl_query, field_offset)
	testing.expect(t, ok)
	testing.expect_value(t, field.name, "comp")
	direct, direct_ok := sem_query.decl_structure_field_info(decl_query, field.owner, "COMP")
	testing.expect(t, direct_ok)
	testing.expect_value(t, direct.name, "comp")
}

@(test)
collects_do_times_header_references :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///do_times_header.abap",
		`
FORM run.
  DATA lv_max_len TYPE i.
  DO lv_max_len TIMES.
  ENDDO.
ENDFORM.
`,
	)

	testing.expect_value(t, reference_count(&unit, "lv_max_len", .Value, .Identifier), 1)
	testing.expect_value(t, len(unit.routine_control_regions), 1)
	testing.expect_value(t, unit.routine_control_regions[0].kind, analyze.Routine_Control_Region_Kind.Loop)
}

@(test)
collects_case_when_header_references :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///case_when_header.abap",
		`
FORM run.
  CONSTANTS lc_rs_agg_op TYPE string VALUE 'SUM'.
  DATA lv_kind TYPE string.
  CASE lv_kind.
    WHEN lc_rs_agg_op.
      WRITE lc_rs_agg_op.
  ENDCASE.
ENDFORM.
`,
	)

	testing.expect_value(t, reference_count(&unit, "lv_kind", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "lc_rs_agg_op", .Value, .Identifier), 2)
	testing.expect_value(t, len(unit.routine_control_regions), 1)
	testing.expect_value(t, unit.routine_control_regions[0].kind, analyze.Routine_Control_Region_Kind.Case)
}

@(test)
collects_at_group_kinds_fields_and_loop_contexts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///at_groups.abap",
		`
FORM run.
  DATA itab TYPE TABLE OF i.
  LOOP AT itab.
    AT FIRST.
    ENDAT.
    AT LAST.
    ENDAT.
    AT NEW src_plant.
    ENDAT.
    AT END OF src_plant.
    ENDAT.
  ENDLOOP.
ENDFORM.
`,
	)

	first, last, new_, end_of := 0, 0, 0, 0
	for region in unit.routine_control_regions {
		if region.kind != .At {
			continue
		}
		switch region.at.kind {
		case .First:
			first += 1
		case .Last:
			last += 1
		case .New:
			new_ += 1
		case .End_Of:
			end_of += 1
		}
	}

	testing.expect_value(t, first, 1)
	testing.expect_value(t, last, 1)
	testing.expect_value(t, new_, 1)
	testing.expect_value(t, end_of, 1)
	testing.expect_value(t, reference_count(&unit, "itab", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "src_plant", .Value, .Identifier), 2)
	testing.expect_value(t, len(unit.loop_at_field_contexts), 2)
	testing.expect(t, system_update_present(&unit, .Loop_At, "subrc"))
	keywords := [?]string{"first", "last", "new", "end", "of", "endat"}
	for keyword in keywords {
		testing.expect(t, !has_reference(&unit, keyword, .Value, .Identifier))
	}
}

@(test)
catch_into_data_declares_inline_and_type_ref :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///catch_inline.abap",
		`
CLASS cx_demo DEFINITION.
ENDCLASS.

TRY.
  CATCH cx_demo INTO DATA(lo_error).
ENDTRY.
`,
	)

	testing.expect(t, has_symbol(&unit, .Variable, "lo_error"))
	testing.expect(t, has_reference(&unit, "cx_demo", .Type, .Type_Ref))
}

@(test)
raise_exception_targets_use_value_or_type_namespace :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///raise_exception_targets.abap",
		`
CLASS cx_demo DEFINITION.
ENDCLASS.

FORM run.
  DATA lx TYPE REF TO cx_demo.
  RAISE EXCEPTION lx.
  RAISE EXCEPTION TYPE cx_demo.
ENDFORM.
`,
	)

	testing.expect(t, has_reference(&unit, "lx", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "cx_demo", .Type, .Type_Ref))
	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
}

@(test)
collects_method_function_and_perform_argument_facts :: proc(t: ^testing.T) {
	// CALL METHOD/FUNCTION values are still raw Call_Stmt value ranges; this covers that fallback.
	unit := collect_test_unit(
		t,
		"file:///call_facts.abap",
		`
FORM process_data USING pv_mode TYPE string CHANGING cv_count TYPE i.
ENDFORM.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_value TYPE i EXPORTING ev_value TYPE i.
    METHODS exec.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD exec.
    DATA lv_value TYPE i.
    DATA lo_obj TYPE REF TO lcl_demo.
    DATA lt_rows TYPE TABLE OF i.
    DATA: BEGIN OF ls_row,
            field TYPE i,
          END OF ls_row.
    CALL METHOD run
      EXPORTING iv_value = lv_value
      IMPORTING ev_value = DATA(lv_out)
      CHANGING cv_value = lv_value
      RECEIVING rv_value = DATA(lv_recv)
      EXCEPTIONS failed = 1.
    run( EXPORTING iv_direct = ls_row-field IMPORTING ev_direct = DATA(lv_direct) ).
    lo_obj = NEW lcl_demo( ).
    CALL FUNCTION 'Z_DEMO'
      EXPORTING iv_value = lv_value is_row = ls_row-field
      IMPORTING ev_func = DATA(lv_func)
      CHANGING cv_func = FIELD-SYMBOL(<fs_func>)
      TABLES ct_rows = lt_rows
      EXCEPTIONS failed = 1.
    PERFORM process_data USING 'demo' CHANGING lv_value.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, has_named_argument(&unit, "iv_value", .Exporting, .Implicit_Method))
	testing.expect(t, has_named_argument(&unit, "ev_value", .Importing, .Implicit_Method))
	testing.expect(t, has_named_argument(&unit, "cv_value", .Changing, .Implicit_Method))
	testing.expect(t, has_named_argument(&unit, "rv_value", .Receiving, .Implicit_Method))
	testing.expect(t, has_named_argument(&unit, "failed", .Exceptions, .Implicit_Method))
	testing.expect(t, has_named_argument(&unit, "iv_direct", .Exporting, .Implicit_Method))
	testing.expect(t, has_named_argument(&unit, "ev_direct", .Importing, .Implicit_Method))
	testing.expect(t, has_named_argument(&unit, "iv_value", .Exporting, .Function))
	testing.expect(t, has_named_argument(&unit, "ev_func", .Importing, .Function))
	testing.expect(t, has_named_argument(&unit, "cv_func", .Changing, .Function))
	testing.expect(t, has_named_argument(&unit, "ct_rows", .Tables, .Function))
	testing.expect(t, has_named_argument(&unit, "failed", .Exceptions, .Function))
	testing.expect(t, has_symbol(&unit, .Variable, "lv_out"))
	testing.expect(t, has_symbol(&unit, .Variable, "lv_recv"))
	testing.expect(t, has_symbol(&unit, .Variable, "lv_direct"))
	testing.expect(t, has_symbol(&unit, .Variable, "lv_func"))
	testing.expect(t, has_symbol(&unit, .Field_Symbol, "<fs_func>"))
	testing.expect(t, !has_reference(&unit, "exporting", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "importing", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "changing", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "tables", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "exceptions", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "iv_value", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "failed", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "1", .Value, .Identifier))
	ls_row_field_accesses := 0
	for access in unit.field_accesses {
		if !access.in_type_position &&
		   access.base_name == "ls_row" &&
		   len(access.field_path) == 1 &&
		   access.field_path[0].name == "field" {
			ls_row_field_accesses += 1
		}
	}
	testing.expect_value(t, ls_row_field_accesses, 2)
	testing.expect(t, len(unit.call_sites) >= 2)
	testing.expect_value(t, len(unit.perform_calls), 1)
	testing.expect_value(t, len(unit.perform_calls[0].arguments), 2)
	testing.expect_value(
		t,
		unit.perform_calls[0].arguments[1].section,
		analyze.Perform_Parameter_Section.Changing,
	)
}

@(test)
receive_results_from_function_uses_call_argument_facts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///receive_results.abap",
		`
FORM run.
  DATA lt_rows TYPE TABLE OF i.
  RECEIVE RESULTS FROM FUNCTION 'Z_DEMO'
    IMPORTING ev_value = DATA(lv_value)
    TABLES et_rows = lt_rows
    EXCEPTIONS failed = 1.
ENDFORM.
`,
	)

	testing.expect(t, has_named_argument(&unit, "ev_value", .Importing, .Function))
	testing.expect(t, has_named_argument(&unit, "et_rows", .Tables, .Function))
	testing.expect(t, has_named_argument(&unit, "failed", .Exceptions, .Function))
	testing.expect(t, has_symbol(&unit, .Variable, "lv_value"))
	testing.expect_value(t, reference_count(&unit, "lt_rows", .Value, .Identifier), 1)
	testing.expect(t, !has_reference(&unit, "results", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "function", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "ev_value", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "failed", .Value, .Identifier))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))

	found_call_site := false
	for site in unit.call_sites {
		if site.target.kind == .Function && site.target.function_name == "z_demo" {
			found_call_site = true
		}
	}
	testing.expect(t, found_call_site)
}

@(test)
raw_call_method_target_facts_drive_refs_and_metadata :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///raw_call_method_target.abap",
		`
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_value TYPE i.
ENDCLASS.

FORM run.
  DATA lo_client TYPE REF TO lcl_demo.
  DATA lv_value TYPE i.
  CALL METHOD lo_client->run EXPORTING iv_value = lv_value.
  CALL METHOD lo_client->('RUN') EXPORTING iv_dyn = (lv_value).
ENDFORM.
`,
	)

	testing.expect(t, has_method_named_argument(&unit, "iv_value", .Exporting, "lo_client", "run"))
	testing.expect(t, has_method_named_argument(&unit, "iv_dyn", .Exporting, "lo_client", ""))
	testing.expect_value(t, reference_count(&unit, "lo_client", .Value, .Identifier), 2)
	testing.expect_value(t, reference_count(&unit, "lv_value", .Value, .Identifier), 1)
	testing.expect(t, !has_reference(&unit, "exporting", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "iv_value", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "iv_dyn", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "run", .Value, .Identifier))

	selector_accesses := 0
	for access in unit.field_accesses {
		if access.base_name == "lo_client" &&
		   len(access.field_path) == 1 &&
		   access.field_path[0].name == "run" {
			selector_accesses += 1
		}
	}
	testing.expect_value(t, selector_accesses, 1)
}

@(test)
dynamic_static_call_method_targets_do_not_reference_method_names :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///dynamic_static_call_method_target.abap",
		`
DATA lv_class TYPE string.
CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_in.
CALL METHOD (lv_class)=>create.
CALL METHOD (lv_class)=>if_demo~create_instance.
`,
	)

	testing.expect(t, !has_reference(&unit, "create_in", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "create", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "create_instance", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "if_demo", .Value, .Identifier))
	testing.expect_value(t, reference_count(&unit, "lv_class", .Value, .Identifier), 2)
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
method_call_chains_do_not_reference_members_after_call_results :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///method_call_chain.abap",
		`
INTERFACE if_node.
  METHODS get_first_child RETURNING VALUE(ro_child) TYPE REF TO if_node.
  METHODS append_child IMPORTING io_child TYPE REF TO if_node.
ENDINTERFACE.

INTERFACE if_doc.
  METHODS get_root RETURNING VALUE(ro_root) TYPE REF TO if_node.
ENDINTERFACE.

DATA mi_xml_doc TYPE REF TO if_doc.
DATA mi_child TYPE REF TO if_node.
mi_xml_doc->get_root( )->get_first_child( )->get_first_child( )->append_child( mi_child ).
`,
	)

	testing.expect(t, !has_reference(&unit, "get_first_child", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "append_child", .Value, .Identifier))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
interface_aliases_resolve_inherited_methods :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///interface_alias_method.abap",
		`
INTERFACE if_node.
  METHODS get_root RETURNING VALUE(ro_root) TYPE REF TO if_node.
ENDINTERFACE.

INTERFACE if_doc.
  INTERFACES if_node.
  ALIASES get_root FOR if_node~get_root.
ENDINTERFACE.

DATA li_doc TYPE REF TO if_doc.
li_doc->get_root( ).
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
call_transaction_collects_parser_operand_facts_without_keyword_refs :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///call_transaction_facts.abap",
		`
DATA tcode TYPE string.
DATA bdc_tab TYPE string.
DATA mode TYPE c.
DATA upd TYPE c.
DATA opt TYPE string.
DATA msg_tab TYPE string.
CALL TRANSACTION tcode WITH AUTHORITY-CHECK USING bdc_tab MODE mode UPDATE upd MESSAGES INTO msg_tab.
CALL TRANSACTION tcode WITHOUT AUTHORITY-CHECK USING bdc_tab OPTIONS FROM opt MESSAGES INTO msg_tab.
`,
	)

	testing.expect_value(t, reference_count(&unit, "tcode", .Value, .Identifier), 2)
	testing.expect_value(t, reference_count(&unit, "bdc_tab", .Value, .Identifier), 2)
	testing.expect_value(t, reference_count(&unit, "mode", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "upd", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "opt", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "msg_tab", .Value, .Identifier), 2)
	keywords := [?]string {
		"call",
		"transaction",
		"with",
		"without",
		"authority",
		"check",
		"using",
		"options",
		"from",
		"messages",
		"into",
	}
	for keyword in keywords {
		testing.expect(t, !has_reference(&unit, keyword, .Value, .Identifier))
	}
}

@(test)
collects_raw_operand_ast_facts_without_keyword_references :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///raw_operand_facts.abap",
		`
DATA ls_row TYPE i.
DATA lv_name TYPE string.
RAISE EVENT changed EXPORTING value = ls_row-field other = DATA(lv_raw).
ASSIGN COMPONENT lv_name OF STRUCTURE ls_row TO FIELD-SYMBOL(<fs_raw>).
`,
	)

	testing.expect(t, has_symbol(&unit, .Variable, "lv_raw"))
	testing.expect(t, has_symbol(&unit, .Field_Symbol, "<fs_raw>"))
	testing.expect(t, has_reference(&unit, "changed", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_name", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "ls_row", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "exporting", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "value", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "other", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "component", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "structure", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "to", .Value, .Identifier))
	ls_row_field_accesses := 0
	for access in unit.field_accesses {
		if !access.in_type_position &&
		   access.base_name == "ls_row" &&
		   len(access.field_path) == 1 &&
		   access.field_path[0].name == "field" {
			ls_row_field_accesses += 1
		}
	}
	testing.expect_value(t, ls_row_field_accesses, 1)
}

@(test)
collects_message_default_and_message_use_facts :: proc(t: ^testing.T) {
	source := `
REPORT zmain MESSAGE-ID zmsg.
DATA lv_text TYPE string.
DATA lv_like TYPE c.
DATA cx_msg TYPE string.
MESSAGE i001 WITH lv_text DISPLAY LIKE lv_like RAISING cx_msg.
`
	unit := collect_test_unit(t, "file:///message_facts.abap", source)

	testing.expect(t, unit.has_message_default_class)
	testing.expect_value(t, unit.message_default_class.name, "zmsg")
	testing.expect_value(t, source[unit.message_default_class.range.start:unit.message_default_class.range.end], "zmsg")
	testing.expect(t, has_reference(&unit, "zmsg", .Value, .Message_Class))
	testing.expect_value(t, len(unit.message_uses), 1)
	testing.expect_value(t, unit.message_uses[0].class_name, "zmsg")
	testing.expect_value(t, len(unit.message_uses[0].with_arg_ranges), 1)
	testing.expect(t, has_reference(&unit, "lv_text", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_like", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "cx_msg", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "i001", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "i001", .Value, .Message_Class))
	keywords := [?]string{"report", "message", "id", "message-id"}
	for keyword in keywords {
		testing.expect(t, !has_reference(&unit, keyword, .Value, .Identifier))
	}
}

@(test)
program_message_id_sets_default_message_class :: proc(t: ^testing.T) {
	source := `PROGRAM zmain MESSAGE-ID zmsg.
MESSAGE e001.`
	unit := collect_test_unit(t, "file:///program_message_default.abap", source)

	testing.expect(t, unit.has_message_default_class)
	testing.expect_value(t, unit.message_default_class.name, "zmsg")
	testing.expect_value(t, source[unit.message_default_class.range.start:unit.message_default_class.range.end], "zmsg")
	testing.expect_value(t, len(unit.message_uses), 1)
	testing.expect_value(t, unit.message_uses[0].class_name, "zmsg")
	testing.expect(t, has_reference(&unit, "zmsg", .Value, .Message_Class))
	testing.expect(t, !has_reference(&unit, "e001", .Value, .Identifier))
	keywords := [?]string{"program", "message", "id", "message-id"}
	for keyword in keywords {
		testing.expect(t, !has_reference(&unit, keyword, .Value, .Identifier))
	}
}

@(test)
message_uses_function_pool_default_message_class :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///message_function_pool.abap",
		`
FUNCTION-POOL zfg MESSAGE-ID zfgmsg.
MESSAGE e001.
`,
	)

	testing.expect(t, unit.has_message_default_class)
	testing.expect_value(t, unit.message_default_class.name, "zfgmsg")
	testing.expect_value(t, len(unit.message_uses), 1)
	testing.expect_value(t, unit.message_uses[0].class_name, "zfgmsg")
	testing.expect(t, !has_reference(&unit, "e001", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "e001", .Value, .Message_Class))
}

@(test)
message_compact_class_uses_parser_fact_range :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string.
DATA lv_like TYPE c.
DATA cx_msg TYPE string.
MESSAGE e001(zmsg) WITH lv_text DISPLAY LIKE lv_like RAISING cx_msg.`
	unit := collect_test_unit(t, "file:///message_compact.abap", source)
	use := unit.message_uses[0]

	testing.expect_value(t, len(unit.message_uses), 1)
	testing.expect_value(t, use.class_name, "zmsg")
	testing.expect(t, .Has_Class_Range in use.flags)
	testing.expect_value(t, source[use.class_range.start:use.class_range.end], "zmsg")
	testing.expect(t, has_reference(&unit, "zmsg", .Value, .Message_Class))
	testing.expect(t, has_reference(&unit, "lv_text", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_like", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "cx_msg", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "e001", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "e001", .Value, .Message_Class))
	testing.expect(t, !has_reference(&unit, "zmsg", .Value, .Identifier))
	keywords := [?]string{"with", "display", "like", "raising"}
	for keyword in keywords {
		testing.expect(t, !has_reference(&unit, keyword, .Value, .Identifier))
	}
}

@(test)
message_id_type_number_does_not_collect_bogus_refs :: proc(t: ^testing.T) {
	source := `DATA lv_type TYPE c.
DATA lv_no TYPE n.
DATA lv_text TYPE string.
MESSAGE ID zmsg TYPE lv_type NUMBER lv_no WITH lv_text.
MESSAGE ID zlit TYPE 'E' NUMBER '001'.`
	unit := collect_test_unit(t, "file:///message_id_form.abap", source)

	testing.expect_value(t, len(unit.message_uses), 2)
	testing.expect_value(t, unit.message_uses[0].class_name, "zmsg")
	testing.expect_value(t, unit.message_uses[1].class_name, "zlit")
	testing.expect(t, has_reference(&unit, "zmsg", .Value, .Message_Class))
	testing.expect(t, has_reference(&unit, "zlit", .Value, .Message_Class))
	testing.expect(t, has_reference(&unit, "lv_type", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_no", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_text", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "zmsg", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "zlit", .Value, .Identifier))
	names := [?]string{"id", "type", "number", "with", "e", "001"}
	for name in names {
		testing.expect(t, !has_reference(&unit, name, .Value, .Identifier))
	}
}

@(test)
no_bogus_references_for_transaction_and_type_pools :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///no_bogus_refs.abap",
		`
FORM run.
  COMMIT WORK.
  ROLLBACK WORK.
  TYPE-POOLS abap.
ENDFORM.
`,
	)

	testing.expect(t, !has_reference(&unit, "work", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "abap", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "abap", .Type, .Type_Ref))
}

@(test)
collects_open_sql_query_projection_source_predicate_and_inline_target :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql.abap",
		`
DATA iv_carrid TYPE c LENGTH 3.
SELECT carrid, carrname
  FROM scarr
  INTO TABLE @DATA(lt_scarr)
  WHERE carrid = @iv_carrid.
`,
	)

	testing.expect_value(t, len(unit.sql_queries), 1)
	query := unit.sql_queries[0]
	testing.expect(t, .Has_Projection_Clause in query.flags)
	testing.expect(t, .Has_From_Clause in query.flags)
	testing.expect(t, .Has_Into_Clause in query.flags)
	testing.expect(t, .Has_Where_Clause in query.flags)
	testing.expect(t, system_update_present(&unit, .Select, "subrc"))
	testing.expect(t, system_update_present(&unit, .Select, "dbcnt"))
	testing.expect(t, sql_source_present(&unit, "scarr", .External))
	testing.expect(t, sql_projection_present(&unit, "carrid", .Column))
	testing.expect(t, sql_projection_present(&unit, "carrname", .Column))
	testing.expect(t, sql_predicate_present(&unit, .Where))
	testing.expect(t, sql_target_present(&unit, "lt_scarr", .Into, {.Is_Table, .Is_Inline}))
	testing.expect(t, has_reference(&unit, "iv_carrid", .Value, .Identifier))

	target := analyze.find_symbol(&unit, "lt_scarr", .Variable)
	testing.expect(t, target != nil)
	testing.expect(t, target.structure != analyze.INVALID_STRUCTURE_ID)
	st := analyze.structure(&unit, target.structure)
	fields := [?]string{"carrid", "carrname"}
	testing.expect(t, field_names_match(st, fields[:]))
}

@(test)
collects_dynamic_open_sql_fragments :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///dynamic_sql.abap",
		`
DATA lv_fields TYPE string.
DATA lv_table TYPE string.
DATA lv_where TYPE string.
DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.

SELECT (lv_fields)
  FROM (lv_table)
  INTO TABLE @lt_rows
  WHERE (lv_where).
`,
	)

	testing.expect_value(t, len(unit.sql_queries), 1)
	testing.expect(t, sql_dynamic_present(&unit, .Projection))
	testing.expect(t, sql_dynamic_present(&unit, .Source))
	testing.expect(t, sql_dynamic_present(&unit, .Where))
	testing.expect_value(t, len(unit.sql_sources), 0)
	testing.expect(t, !sql_name_ref_present(&unit, "lv_fields", .Column))
	dynamic_names := [?]string{"lv_fields", "lv_table", "lv_where"}
	for name in dynamic_names {
		testing.expect(t, has_reference(&unit, name, .Value, .Identifier))
	}
	testing.expect(t, has_reference(&unit, "lt_rows", .Value, .Identifier))
}

@(test)
collects_common_table_expression_sql_facts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///cte_dossier.abap",
		`
DATA lv_carrid TYPE string.

WITH +filtered AS (
       SELECT carrid, connid
         FROM sflight
         WHERE carrid = @lv_carrid
     ),
     +joined AS (
       SELECT f~carrid
         FROM +filtered AS f
         INNER JOIN spfli AS p ON p~carrid = f~carrid
     )
SELECT carrid
  FROM +joined
  INTO TABLE @DATA(lt_flights).
`,
	)

	testing.expect_value(t, len(unit.sql_queries), 3)
	testing.expect(t, sql_source_present(&unit, "sflight", .External))
	testing.expect(t, sql_source_present(&unit, "spfli", .External))
	testing.expect(t, sql_source_present(&unit, "+filtered", .Local_Cte))
	testing.expect(t, sql_source_present(&unit, "+joined", .Local_Cte))
	testing.expect(t, sql_name_ref_present(&unit, "sflight", .Source))
	testing.expect(t, sql_name_ref_present(&unit, "spfli", .Source))
	testing.expect(t, !sql_name_ref_present(&unit, "+filtered", .Source))
	testing.expect(t, !sql_name_ref_present(&unit, "+joined", .Source))
	testing.expect(t, sql_target_present(&unit, "lt_flights", .Into, {.Is_Table, .Is_Inline}))
	testing.expect(t, has_reference(&unit, "lv_carrid", .Value, .Identifier))
}

@(test)
collects_join_alias_qualified_star_aggregate_and_set_operator_facts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql_join_set.abap",
		`
SELECT a~carrid AS carrier, COUNT( * ) AS total
  FROM scarr AS a
  INNER JOIN spfli AS b ON b~carrid = a~carrid
  INTO TABLE @DATA(lt_rows)
UNION ALL SELECT * FROM spfli INTO TABLE @lt_rows.
`,
	)

	testing.expect_value(t, len(unit.sql_queries), 1)
	testing.expect(t, .Has_Set_Operators in unit.sql_queries[0].flags)
	testing.expect(t, sql_source_alias_present(&unit, "scarr", "a", .From))
	testing.expect(t, sql_source_alias_present(&unit, "spfli", "b", .Join))
	testing.expect(t, sql_projection_alias_present(&unit, "carrier", .Column))
	testing.expect(t, sql_projection_alias_present(&unit, "total", .Aggregate))
	testing.expect(t, sql_name_ref_present(&unit, "count", .Aggregate))
	testing.expect(t, sql_qualified_ref_present(&unit, "a", "carrid", .Qualified_Column))
	testing.expect(t, sql_name_ref_present(&unit, "*", .Star))
	testing.expect(t, sql_predicate_present(&unit, .Join_On))
}

@(test)
collects_sql_for_all_entries_and_dynamic_where_predicates :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql_predicates.abap",
		`
DATA lt_keys TYPE string.
DATA lv_where TYPE string.

SELECT DISTINCT carrid
  FROM sflight
  INTO TABLE @DATA(lt_rows)
  FOR ALL ENTRIES IN lt_keys
  WHERE (lv_where).
`,
	)

	testing.expect_value(t, len(unit.sql_queries), 1)
	testing.expect(t, .Is_Distinct in unit.sql_queries[0].flags)
	testing.expect(t, .Has_Dynamic_Where in unit.sql_queries[0].flags)
	testing.expect(t, .Has_For_All_Entries in unit.sql_queries[0].flags)
	testing.expect(t, sql_predicate_present(&unit, .Dynamic_Where))
	testing.expect(t, sql_predicate_present(&unit, .For_All_Entries))
	testing.expect(t, sql_dynamic_present(&unit, .Where))
	testing.expect(t, has_reference(&unit, "lt_keys", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_where", .Value, .Identifier))
}

@(test)
validates_open_sql_source_and_field_diagnostics :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql_validation.abap",
		`
TYPES: BEGIN OF zflight,
         carrid TYPE string,
       END OF zflight.

SELECT connid FROM zflight INTO TABLE @DATA(lt_rows).
SELECT carrid FROM zmissing INTO TABLE @DATA(lt_missing).
`,
	)

	testing.expect(t, has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, has_diagnostic(&unit, .Unverified_Open_Sql_Source))
}

@(test)
collects_sort_order_and_read_table_binary_search_facts :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
           connid TYPE string,
         END OF ty_row.
  DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
  DATA ls_row TYPE ty_row.

  SORT lt_rows BY carrid connid.
  READ TABLE lt_rows INTO ls_row WITH KEY carrid = 'AA' connid = '001' BINARY SEARCH.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///read_table_binary_search.abap", source)
	keys := [?]string{"carrid", "connid"}
	testing.expect(t, internal_table_order_present(&unit, "lt_rows", keys[:]))
	testing.expect(t, binary_search_present(&unit, "lt_rows", keys[:]))
	testing.expect(t, system_update_present(&unit, .Read_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Read_Table, "tabix"))
	found := false
	for read in unit.read_table_binary_searches {
		if read.table_name == "lt_rows" && string_list_matches(read.key_fields, keys[:]) {
			testing.expect_value(t, source[read.range.start:read.range.end], "BINARY SEARCH")
			found = true
		}
	}
	testing.expect(t, found)
}

@(test)
sort_by_components_are_not_value_references :: proc(t: ^testing.T) {
	source := `
TYPES vepparamtype TYPE string.

FORM run.
  TYPES: BEGIN OF ty_row,
           vepname TYPE string,
           version TYPE string,
           function TYPE string,
           vepparam TYPE string,
           vepparamtype TYPE string,
         END OF ty_row.
  DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

  SORT lt_rows BY vepname version function vepparam vepparamtype.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///sort_components.abap", source)
	keys := [?]string{"vepname", "version", "function", "vepparam", "vepparamtype"}

	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect(t, !has_reference(&unit, "vepparamtype", .Value, .Identifier))
	testing.expect(t, internal_table_order_present(&unit, "lt_rows", keys[:]))
}

@(test)
collects_select_order_by_into_table_order_fact :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///select_order.abap",
		`
FORM run.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  SELECT carrid, connid
    FROM zflights
    INTO TABLE @lt_rows
    ORDER BY carrid, connid.
ENDFORM.
`,
	)
	keys := [?]string{"carrid", "connid"}
	testing.expect(t, .Has_Order_By_Clause in unit.sql_queries[0].flags)
	testing.expect(t, internal_table_order_present(&unit, "lt_rows", keys[:]))
}

@(test)
collects_classic_and_modern_select_ordering_facts :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lt_old TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lt_new TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lv_matnr TYPE string.

  SELECT matnr INTO TABLE @lt_old FROM mara WHERE matnr = @lv_matnr ORDER BY matnr.
  SELECT matnr FROM mara INTO TABLE @lt_new WHERE matnr = @lv_matnr ORDER BY matnr.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///select_classic_modern_order.abap", source)

	testing.expect_value(t, len(unit.sql_queries), 2)
	for query in unit.sql_queries {
		testing.expect(t, .Has_From_Clause in query.flags)
		testing.expect(t, .Has_Into_Clause in query.flags)
		testing.expect(t, .Has_Where_Clause in query.flags)
		testing.expect(t, .Has_Order_By_Clause in query.flags)
		testing.expect_value(t, source[query.from_clause.start:query.from_clause.end], "mara")
		testing.expect_value(t, source[query.where_clause.start:query.where_clause.end], "WHERE matnr = @lv_matnr")
		testing.expect_value(t, source[query.order_by_clause.start:query.order_by_clause.end], "ORDER BY matnr")
	}
	keys := [?]string{"matnr"}
	testing.expect(t, internal_table_order_present(&unit, "lt_old", keys[:]))
	testing.expect(t, internal_table_order_present(&unit, "lt_new", keys[:]))
}

@(test)
collects_parser_modeled_select_clause_flags_ranges_and_cursor_select :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lt_desc TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lv_matnr TYPE string.
  DATA lv_size TYPE i.
  DATA cv TYPE cursor.

  SELECT a~* FROM mara AS a INTO TABLE @lt_rows WHERE a~matnr = @lv_matnr GROUP BY a~matnr HAVING COUNT( * ) > 0 ORDER BY a~matnr, a~ersda UP TO 10 ROWS PACKAGE SIZE lv_size OFFSET 2 BYPASSING BUFFER CONNECTION con CLIENT SPECIFIED.
  SELECT * FROM zlock INTO @DATA(ls_lock) FOR UPDATE.
  SELECT * FROM zprimary INTO TABLE @lt_rows ORDER BY PRIMARY KEY.
  SELECT * FROM zdesc INTO TABLE @lt_desc ORDER BY carrid DESCENDING.
  OPEN CURSOR cv FOR SELECT matnr FROM mara WHERE matnr = @lv_matnr ORDER BY matnr.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///select_clause_facts.abap", source)

	testing.expect_value(t, len(unit.sql_queries), 5)
	query := unit.sql_queries[0]
	testing.expect(t, .Has_Group_By_Clause in query.flags)
	testing.expect(t, .Has_Having_Clause in query.flags)
	testing.expect(t, .Has_Order_By_Clause in query.flags)
	testing.expect(t, .Has_Up_To_Clause in query.flags)
	testing.expect(t, .Has_Package_Size_Clause in query.flags)
	testing.expect(t, .Has_Offset_Clause in query.flags)
	testing.expect(t, .Has_Abap_Options_Clause in query.flags)
	testing.expect_value(t, source[query.group_by_clause.start:query.group_by_clause.end], "GROUP BY a~matnr")
	testing.expect_value(t, source[query.having_clause.start:query.having_clause.end], "HAVING COUNT( * ) > 0")
	testing.expect_value(t, source[query.order_by_clause.start:query.order_by_clause.end], "ORDER BY a~matnr, a~ersda")
	testing.expect_value(t, source[query.up_to_clause.start:query.up_to_clause.end], "UP TO 10 ROWS")
	testing.expect_value(t, source[query.package_size_clause.start:query.package_size_clause.end], "PACKAGE SIZE lv_size")
	testing.expect_value(t, source[query.offset_clause.start:query.offset_clause.end], "OFFSET 2")
	testing.expect_value(t, source[query.abap_options_clause.start:query.abap_options_clause.end], "BYPASSING BUFFER CONNECTION con CLIENT SPECIFIED")
	testing.expect_value(t, len(query.order_by_fields), 2)
	testing.expect_value(t, query.order_by_fields[0], "matnr")
	testing.expect_value(t, query.order_by_fields[1], "ersda")
	testing.expect(t, sql_qualified_ref_present(&unit, "a", "*", .Qualified_Star))
	keys := [?]string{"matnr", "ersda"}
	testing.expect(t, internal_table_order_present(&unit, "lt_rows", keys[:]))

	testing.expect(t, .Has_For_Update in unit.sql_queries[1].flags)
	testing.expect(t, .Is_For_Update in unit.sql_queries[1].flags)
	testing.expect(t, .Order_By_Primary_Key in unit.sql_queries[2].flags)
	testing.expect_value(t, len(unit.sql_queries[3].order_by_fields), 0)
	desc_keys := [?]string{"carrid"}
	testing.expect(t, !internal_table_order_present(&unit, "lt_desc", desc_keys[:]))
	testing.expect(t, .Has_Order_By_Clause in unit.sql_queries[4].flags)
	testing.expect(t, has_reference(&unit, "cv", .Value, .Identifier))

	keywords := [?]string{"group", "by", "having", "order", "up", "package", "offset", "bypassing", "connection", "client", "for", "update"}
	for keyword in keywords {
		testing.expect(t, !has_reference(&unit, keyword, .Value, .Identifier))
	}
}

@(test)
open_sql_clause_keywords_are_not_sql_name_refs :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql_clause_keyword_refs.abap",
		`
FORM run.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lv_matnr TYPE string.

  SELECT a~matnr FROM mara AS a INTO TABLE @lt_rows WHERE a~matnr = @lv_matnr GROUP BY a~matnr HAVING COUNT( * ) > 0 ORDER BY a~matnr UP TO 1 ROWS.
  SELECT a~matnr FROM mara AS a INNER JOIN makt AS b ON b~matnr = a~matnr INTO TABLE @lt_rows.
  SELECT * FROM zlock INTO @DATA(ls_lock) FOR UPDATE.
ENDFORM.
`,
	)

	keywords := [?]string{"as", "from", "inner", "join", "on", "into", "table", "where", "group", "by", "having", "order", "up", "to", "rows", "for", "update"}
	for reference in unit.sql_name_refs {
		for keyword in keywords {
			testing.expect(t, reference.name != keyword)
		}
	}
}

@(test)
collects_sql_null_like_and_case_refs_without_keyword_refs :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql_null_like_case.abap",
		`
FORM run.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lv_pattern TYPE string.
  DATA lv_old_pattern TYPE string.

  SELECT CASE WHEN carrid LIKE @lv_pattern THEN LOWER( connid ) ELSE carrid END AS value
    FROM sflight
    INTO TABLE @lt_rows
    WHERE carrid IS NOT NULL
      AND connid NOT LIKE lv_old_pattern.
ENDFORM.
`,
	)

	testing.expect(t, sql_name_ref_present(&unit, "sflight", .Source))
	testing.expect(t, sql_name_ref_present(&unit, "carrid", .Column))
	testing.expect(t, sql_name_ref_present(&unit, "connid", .Column))
	testing.expect(t, sql_name_ref_present(&unit, "lower", .Function))
	testing.expect(t, has_reference(&unit, "lv_pattern", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_old_pattern", .Value, .Identifier))
	testing.expect(t, !sql_name_ref_present(&unit, "lv_old_pattern", .Column))
	keywords := [?]string{"case", "when", "then", "else", "end", "like", "null"}
	for reference in unit.sql_name_refs {
		for keyword in keywords {
			testing.expect(t, reference.name != keyword)
		}
	}
}

@(test)
collects_db_table_statement_sql_sources_and_host_refs :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///db_table_sql.abap",
		`
FORM run.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA ls_row TYPE string.
  DATA lr_objid TYPE string.

  INSERT zinsert_tab FROM TABLE lt_rows.
  MODIFY zmodify_tab FROM ls_row.
	UPDATE zupdate_tab SET status = ls_row WHERE objid = lr_objid.
  DELETE FROM zdelete_tab WHERE objid = lr_objid.
ENDFORM.
`,
	)

	db_names := [?]string{"zinsert_tab", "zmodify_tab", "zupdate_tab", "zdelete_tab"}
	for name in db_names {
		testing.expect(t, sql_source_present(&unit, name, .External))
		testing.expect(t, sql_name_ref_present(&unit, name, .Source))
		testing.expect(t, !has_reference(&unit, name, .Value, .Identifier))
	}
	testing.expect(t, sql_name_ref_present(&unit, "status", .Column))
	testing.expect(t, sql_name_ref_present(&unit, "objid", .Column))
	testing.expect(t, sql_predicate_present(&unit, .Where))
	testing.expect(t, has_reference(&unit, "lt_rows", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "ls_row", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lr_objid", .Value, .Identifier))
	testing.expect(t, system_update_present(&unit, .Insert_Db_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Modify_Db_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Update_Db_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Delete_Db_Table, "subrc"))
}

@(test)
collects_insert_parser_facts_without_keyword_refs :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///insert_facts.abap",
		`
FORM run.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA ls_row TYPE string.
  DATA lv_idx TYPE i.

  INSERT ls_row INTO TABLE lt_rows INDEX lv_idx.
  INSERT zinsert_tab FROM TABLE lt_rows ACCEPTING DUPLICATE KEYS.
  INSERT INTO zinto_tab VALUES ls_row.
ENDFORM.
`,
	)

	testing.expect(t, system_update_present(&unit, .Insert_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Insert_Db_Table, "subrc"))
	testing.expect(t, sql_source_present(&unit, "zinsert_tab", .External))
	testing.expect(t, sql_source_present(&unit, "zinto_tab", .External))
	testing.expect(t, !has_reference(&unit, "zinsert_tab", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "zinto_tab", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lt_rows", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "ls_row", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_idx", .Value, .Identifier))
	keywords := [?]string{"insert", "into", "table", "from", "values", "accepting", "duplicate", "keys"}
	for keyword in keywords {
		testing.expect(t, !has_reference(&unit, keyword, .Value, .Identifier))
	}
}

@(test)
collects_dml_parser_facts_without_keyword_refs :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///dml_parser_facts.abap",
		`
FORM run.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA ls_row TYPE string.
  DATA lv_id TYPE string.
  DATA lv_status TYPE string.
  DATA lv_where TYPE string.

  UPDATE zupdate_tab SET status = lv_status WHERE (lv_where).
  DELETE FROM zdelete_tab WHERE id = lv_id.
  DELETE lt_rows WHERE id = lv_id.
  MODIFY zmodify_tab FROM ls_row WHERE id = lv_id.
  MODIFY TABLE lt_rows FROM ls_row WHERE id = lv_id.
  INSERT zinsert_tab FROM ls_row ACCEPTING DUPLICATE KEYS.
  INSERT ls_row INTO TABLE lt_rows.
ENDFORM.
`,
	)

	db_names := [?]string{"zupdate_tab", "zdelete_tab", "zmodify_tab", "zinsert_tab"}
	for name in db_names {
		testing.expect(t, sql_source_present(&unit, name, .External))
		testing.expect(t, sql_name_ref_present(&unit, name, .Source))
		testing.expect(t, !has_reference(&unit, name, .Value, .Identifier))
	}
	testing.expect(t, sql_name_ref_present(&unit, "status", .Column))
	testing.expect(t, sql_name_ref_present(&unit, "id", .Column))
	testing.expect(t, sql_predicate_present(&unit, .Dynamic_Where))
	testing.expect(t, system_update_present(&unit, .Update_Db_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Delete_Db_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Delete_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Modify_Db_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Modify_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Insert_Db_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Insert_Table, "subrc"))
	testing.expect(t, has_reference(&unit, "lt_rows", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "ls_row", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_id", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_status", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_where", .Value, .Identifier))

	keywords := [?]string{"update", "set", "where", "delete", "from", "modify", "table", "insert", "into", "accepting", "duplicate", "keys"}
	for keyword in keywords {
		testing.expect(t, !has_reference(&unit, keyword, .Value, .Identifier))
		for reference in unit.sql_name_refs {
			testing.expect(t, reference.name != keyword)
		}
	}
}

@(test)
collects_surface_source_maintenance_and_string_operation_facts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///surface_facts.abap",
		`
FORM run.
  DATA lt_source TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lt_report TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lt_pool TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lv_prog TYPE string.
  DATA lv_msg TYPE string.
  DATA lv_line TYPE i.
  DATA lv_word TYPE string.
  DATA lv_offset TYPE i.
  DATA lv_file TYPE string.
  DATA lv_text TYPE string.
  DATA lv_len TYPE i.
  DATA lv_pos TYPE i.
  DATA lv_attr TYPE string.
  DATA lv_result TYPE string.
  DATA lv_off TYPE i.
  DATA lv_match_len TYPE i.
  DATA lv_cnt TYPE i.

  READ REPORT lv_prog INTO lt_report.
  INSERT REPORT lv_prog FROM lt_source.
  DELETE REPORT lv_prog.
  INSERT TEXTPOOL lv_prog FROM lt_pool LANGUAGE 'E'.
  GENERATE SUBROUTINE POOL lt_source NAME lv_prog MESSAGE lv_msg LINE lv_line WORD lv_word OFFSET lv_offset.
  OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_msg.
  READ DATASET lv_file INTO lv_text ACTUAL LENGTH lv_len.
  GET DATASET lv_file POSITION lv_pos ATTRIBUTES lv_attr.
  CONCATENATE LINES OF lt_source INTO lv_text IN BYTE MODE.
  FIND ALL OCCURRENCES OF 'A' IN lv_text MATCH OFFSET lv_off MATCH LENGTH lv_match_len MATCH COUNT lv_cnt RESULTS lv_result.
ENDFORM.
`,
	)

	testing.expect(t, system_update_present(&unit, .Read_Report, "subrc"))
	testing.expect(t, system_update_present(&unit, .Insert_Report, "subrc"))
	testing.expect(t, system_update_present(&unit, .Delete_Report, "subrc"))
	testing.expect(t, system_update_present(&unit, .Insert_Textpool, "subrc"))
	testing.expect(t, len(unit.concatenate_lines_of_sites) == 1)
	testing.expect(t, unit.concatenate_lines_of_sites[0].byte_mode)
	testing.expect(t, len(unit.find_sites) == 1)
	testing.expect(t, len(unit.find_sites[0].write_targets) == 4)
	testing.expect(t, unit.find_sites[0].write_targets[3].definitely_assigned)
	testing.expect(t, len(unit.assignment_sites) >= 12)
	testing.expect(t, has_reference(&unit, "lt_source", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lt_report", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lt_pool", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_attr", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "count", .Value, .Identifier))
}

@(test)
manifest_decoder_accepts_rust_compatible_units :: proc(t: ^testing.T) {
	source := `
connection = "SAP-DEV"

[dependency_store]
product_version = "S4-2023"
default_package_version = "base"

[dependency_store.packages]
SAP_BASIS = "basis"

[local_export]
roots = ["../exports", "D:\\adt\\export"]

[dependencies]
source = "local-first"

[[unit]]
name = "ZMAIN"
kind = "program"
root_file = "src\\ZMAIN.abap"
members = [
  "src/ZMAIN_TOP.abap",
  { file = "./src/forms/ZMAIN_F01.abap", role = "include", object_name = "ZMAIN_F01" },
]

[[unit]]
name = "ZCL_DEP"
kind = "class"
root_file = "src/ZCL_DEP.abap"
dependency_of = ["src/ZMAIN.abap"]
`
	manifest, ok, err := workspace.parse_workspace_manifest_text("D:/workspace", "D:/workspace/abapls.toml", source, context.allocator)

	testing.expect(t, ok)
	testing.expect_value(t, err, "")
	testing.expect_value(t, manifest.connection, "sap-dev")
	testing.expect(t, manifest.has_dependency_store)
	testing.expect_value(t, manifest.dependency_store.product_version, "S4-2023")
	testing.expect_value(t, manifest.dependency_store.default_package_version, "base")
	testing.expect_value(t, len(manifest.dependency_store.packages), 1)
	if len(manifest.dependency_store.packages) == 1 {
		testing.expect_value(t, manifest.dependency_store.packages[0].package_name, "SAP_BASIS")
		testing.expect_value(t, manifest.dependency_store.packages[0].version, "basis")
	}
	testing.expect_value(t, len(manifest.local_export_roots), 2)
	if len(manifest.local_export_roots) == 2 {
		testing.expect_value(t, manifest.local_export_roots[0], "../exports")
		testing.expect_value(t, manifest.local_export_roots[1], "D:/adt/export")
	}
	testing.expect_value(t, manifest.dependency_source, "local-first")
	testing.expect_value(t, len(manifest.units), 2)
	if !ok || len(manifest.units) != 2 {
		return
	}
	testing.expect_value(t, manifest.units[0].name, "ZMAIN")
	testing.expect_value(t, manifest.units[0].kind, "program")
	testing.expect_value(t, manifest.units[0].root_file, "src/ZMAIN.abap")
	testing.expect_value(t, len(manifest.units[0].members), 2)
	testing.expect_value(t, manifest.units[0].members[0].file, "src/ZMAIN_TOP.abap")
	testing.expect_value(t, manifest.units[0].members[1].file, "src/forms/ZMAIN_F01.abap")
	testing.expect_value(t, manifest.units[0].members[1].role, "include")
	testing.expect_value(t, manifest.units[0].members[1].object_name, "ZMAIN_F01")
	testing.expect_value(t, manifest.units[1].root_file, "src/ZCL_DEP.abap")
	testing.expect_value(t, len(manifest.units[1].dependency_of), 1)
	testing.expect_value(t, manifest.units[1].dependency_of[0].file, "src/ZMAIN.abap")
}

@(test)
manifest_root_resolves_explicit_member_include_name :: proc(t: ^testing.T) {
	root := manifest_workspace_path("explicit-member")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
kind = "program"
root_file = "src/ZMAIN.abap"
members = [{ file = "src/includes/generated.abap", role = "include", object_name = "ZTOP" }]
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain. INCLUDE ztop. lv_top = 1.")
	member_file := manifest_test_file(t, root, "src/includes/generated.abap", "DATA lv_top TYPE i.")

	result := analyze_path_test(t, root, root_file)

	testing.expect(t, result.ok)
	testing.expect(t, result.used_manifest)
	testing.expect_value(t, len(result.project.units), 2)
	root_unit := analyze.project_unit_by_uri(&result.project, root_file)
	testing.expect(t, root_unit != nil)
	testing.expect_value(t, include_target_uri(&result.project, root_unit, "ztop"), member_file)
	testing.expect(t, reference_resolves_to_uri(&result.project, root_unit, "lv_top", .Value, .Identifier, member_file))
	testing.expect(t, !project_has_diagnostic(&result.project, .Unresolved_Include))
}

@(test)
manifest_listed_unreferenced_member_is_not_visible :: proc(t: ^testing.T) {
	root := manifest_workspace_path("unreferenced-member")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
root_file = "src/ZMAIN.abap"
members = ["src/ZUNUSED.abap"]
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain. DATA lo_unused TYPE REF TO zcl_unused.")
	unused_file := manifest_test_file(t, root, "src/ZUNUSED.abap", "CLASS zcl_unused DEFINITION. ENDCLASS.")

	result := analyze_path_test(t, root, root_file)
	root_unit := analyze.project_unit_by_uri(&result.project, root_file)

	testing.expect(t, result.ok)
	testing.expect(t, result.used_manifest)
	testing.expect_value(t, len(result.project.units), 1)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, unused_file) == nil)
	testing.expect(t, root_unit != nil)
	testing.expect(t, has_diagnostic(root_unit, .Unresolved_Reference))
}

@(test)
manifest_dependency_store_drains_iteratively :: proc(t: ^testing.T) {
	root := manifest_workspace_path("dependency-store-drain")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	inputs := [?]dep_store.Stored_Artifact_Input {
		{
			package_name   = "ZPKG",
			object_kind    = "global-class",
			object_name    = "ZCL_OUTER",
			object_uri     = "/sap/bc/adt/oo/classes/ZCL_OUTER",
			object_type    = "CLAS/OC",
			description    = "Outer class",
			file_extension = "abap",
			source_text    = "CLASS zcl_outer DEFINITION. PUBLIC SECTION. DATA value TYPE zdep_type. ENDCLASS. CLASS zcl_outer IMPLEMENTATION. ENDCLASS.",
			fetched_at     = "2026-05-21T00:00:00Z",
		},
		{
			package_name   = "ZPKG",
			object_kind    = "ddic-data-element",
			object_name    = "ZDEP_TYPE",
			object_uri     = "/sap/bc/adt/ddic/dataelements/ZDEP_TYPE",
			object_type    = "DTEL/DE",
			description    = "Dependent type",
			file_extension = "xml",
			source_text    = `<blue:wbobj adtcore:name="ZDEP_TYPE" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements"><dtel:dataElement><dtel:typeKind>predefinedAbapType</dtel:typeKind><dtel:dataType>STRING</dtel:dataType></dtel:dataElement></blue:wbobj>`,
			fetched_at     = "2026-05-21T00:00:00Z",
		},
	}
	_, err = dep_store.put_artifacts(&store, &profile, inputs[:], context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[dependency_store]
product_version = "S4-2023"
default_package_version = "base"

[[unit]]
name = "ZMAIN"
root_file = "src/ZMAIN.abap"
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain. DATA lo_outer TYPE REF TO zcl_outer.")

	result := analyze_path_test_with_options(t, root, root_file, workspace.Options{dependency_store_path = store_path})

	testing.expect(t, result.ok)
	testing.expect(t, result.used_manifest)
	testing.expect_value(t, len(result.project.units), 3)
	testing.expect(t, !project_has_diagnostic(&result.project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&result.project, .Unresolved_Reference))
}

@(test)
standalone_file_drains_dependency_store :: proc(t: ^testing.T) {
	root := manifest_workspace_path("standalone-dependency-store-drain")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	artifact := dep_store.Stored_Artifact_Input {
		package_name   = "ZPKG",
		object_kind    = "global-class",
		object_name    = "ZCL_STANDALONE_CACHE",
		object_uri     = "/sap/bc/adt/oo/classes/ZCL_STANDALONE_CACHE",
		object_type    = "CLAS/OC",
		description    = "Standalone cache class",
		file_extension = "abap",
		source_text    = "CLASS zcl_standalone_cache DEFINITION. ENDCLASS. CLASS zcl_standalone_cache IMPLEMENTATION. ENDCLASS.",
		fetched_at     = "2026-05-21T00:00:00Z",
	}
	_, err = dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	root_file := manifest_test_file(
		t,
		root,
		"ZMAIN.abap",
		"REPORT zmain. DATA lo_dep TYPE REF TO zcl_standalone_cache.",
	)
	result := analyze_file_path_test_with_options(t, root_file, workspace.Options{dependency_store_path = store_path})

	testing.expect(t, result.ok)
	testing.expect(t, !result.used_manifest)
	testing.expect_value(t, len(result.project.units), 2)
	testing.expect(t, !project_has_diagnostic(&result.project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&result.project, .Unresolved_Reference))
}

@(test)
dependency_store_function_hit_clears_remote_candidate :: proc(t: ^testing.T) {
	root := manifest_workspace_path("dependency-store-function-hit")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	artifact := dep_store.Stored_Artifact_Input {
		package_name   = "ZPKG",
		object_kind    = "function-module",
		object_name    = "Z_REMOTE_FM",
		object_uri     = "/sap/bc/adt/functions/groups/ZFG/fmodules/Z_REMOTE_FM",
		object_type    = "FUGR/FF",
		description    = "Cached function module",
		file_extension = "abap",
		source_text    = "FUNCTION z_remote_fm\n  EXPORTING ev_value TYPE i.\nENDFUNCTION.",
		fetched_at     = "2026-05-21T00:00:00Z",
	}
	_, err = dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	project := deps.analyze_with_dependency_drain(
		analyze.Source_Input {
			uri    = "mem://ZMAIN.abap",
			source = "REPORT zmain. CALL FUNCTION 'Z_REMOTE_FM'.",
		},
		make([dynamic]analyze.Project_Candidate_Input, context.allocator),
		make([dynamic]analyze.Source_Input, context.allocator),
		deps.Dependency_Config{store = &store, profile = &profile, store_any_profile = true},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	frontend_runtime.pool_destroy(&pool)

	found_module := false
	for &unit in project.units {
		if analyze.find_symbol(&unit, "z_remote_fm", .Module) != nil {
			found_module = true
		}
	}
	testing.expect(t, found_module)
	for candidate in analyze.collect_project_remote_dependency_candidates(&project, context.allocator) {
		testing.expect(t, !(candidate.name == "z_remote_fm" && candidate.kind == .Function))
	}
}

@(test)
standalone_file_drains_dependency_store_iteratively :: proc(t: ^testing.T) {
	root := manifest_workspace_path("standalone-dependency-store-iterative")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	inputs := [?]dep_store.Stored_Artifact_Input {
		{
			package_name   = "ZPKG",
			object_kind    = "global-class",
			object_name    = "ZCL_STANDALONE_OUTER",
			object_uri     = "/sap/bc/adt/oo/classes/ZCL_STANDALONE_OUTER",
			object_type    = "CLAS/OC",
			description    = "Standalone outer class",
			file_extension = "abap",
			source_text    = "CLASS zcl_standalone_outer DEFINITION. PUBLIC SECTION. DATA value TYPE zstandalone_type. ENDCLASS. CLASS zcl_standalone_outer IMPLEMENTATION. ENDCLASS.",
			fetched_at     = "2026-05-21T00:00:00Z",
		},
		{
			package_name   = "ZPKG",
			object_kind    = "ddic-data-element",
			object_name    = "ZSTANDALONE_TYPE",
			object_uri     = "/sap/bc/adt/ddic/dataelements/ZSTANDALONE_TYPE",
			object_type    = "DTEL/DE",
			description    = "Standalone dependent type",
			file_extension = "xml",
			source_text    = `<blue:wbobj adtcore:name="ZSTANDALONE_TYPE" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements"><dtel:dataElement><dtel:typeKind>predefinedAbapType</dtel:typeKind><dtel:dataType>STRING</dtel:dataType></dtel:dataElement></blue:wbobj>`,
			fetched_at     = "2026-05-21T00:00:00Z",
		},
	}
	_, err = dep_store.put_artifacts(&store, &profile, inputs[:], context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	root_file := manifest_test_file(
		t,
		root,
		"ZMAIN.abap",
		"REPORT zmain. DATA lo_dep TYPE REF TO zcl_standalone_outer.",
	)
	result := analyze_file_path_test_with_options(t, root_file, workspace.Options{dependency_store_path = store_path})

	testing.expect(t, result.ok)
	testing.expect(t, !result.used_manifest)
	testing.expect_value(t, len(result.project.units), 3)
	testing.expect(t, !project_has_diagnostic(&result.project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&result.project, .Unresolved_Reference))
}

@(test)
standalone_file_drains_dependency_store_with_threaded_pool :: proc(t: ^testing.T) {
	root := manifest_workspace_path("standalone-dependency-store-threaded")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	artifacts := make([dynamic]dep_store.Stored_Artifact_Input, 0, 16, context.allocator)
	target_source := strings.builder_make(context.allocator)
	strings.write_string(&target_source, "REPORT zmain.")
	for i in 0 ..< 16 {
		name := strings.builder_make(context.allocator)
		strings.write_string(&name, "ZCL_THREADED_DRAIN_")
		strings.write_i64(&name, i64(i))
		object_name := strings.to_string(name)

		source := strings.builder_make(context.allocator)
		strings.write_string(&source, "CLASS ")
		strings.write_string(&source, object_name)
		strings.write_string(&source, " DEFINITION. ENDCLASS. CLASS ")
		strings.write_string(&source, object_name)
		strings.write_string(&source, " IMPLEMENTATION. ENDCLASS.")

		uri := strings.builder_make(context.allocator)
		strings.write_string(&uri, "/sap/bc/adt/oo/classes/")
		strings.write_string(&uri, object_name)

		append(
			&artifacts,
			dep_store.Stored_Artifact_Input {
				package_name   = "ZPKG",
				object_kind    = "global-class",
				object_name    = object_name,
				object_uri     = strings.to_string(uri),
				object_type    = "CLAS/OC",
				description    = "Threaded drain class",
				file_extension = "abap",
				source_text    = strings.to_string(source),
				fetched_at     = "2026-05-21T00:00:00Z",
			},
		)
		strings.write_string(&target_source, " DATA lo")
		strings.write_i64(&target_source, i64(i))
		strings.write_string(&target_source, " TYPE REF TO ")
		strings.write_string(&target_source, object_name)
		strings.write_string(&target_source, ".")
	}
	_, err = dep_store.put_artifacts(&store, &profile, artifacts[:], context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 2, task_capacity = 8}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	if pool.options.worker_count > 0 {
		testing.expect_value(t, frontend_runtime.pool_start(&pool), frontend_runtime.Submit_Error.None)
	}
	project := deps.analyze_with_dependency_drain(
		analyze.Source_Input{uri = "file:///ZMAIN.abap", source = strings.to_string(target_source)},
		make([dynamic]analyze.Project_Candidate_Input, context.allocator),
		make([dynamic]analyze.Source_Input, context.allocator),
		deps.Dependency_Config{store = &store, profile = &profile, store_any_profile = true},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	if pool.options.worker_count > 0 {
		frontend_runtime.pool_join(&pool)
	}
	frontend_runtime.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 17)
	testing.expect(t, !project_has_diagnostic(&project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
dependency_store_candidates_submit_lookup_tasks :: proc(t: ^testing.T) {
	root := manifest_workspace_path("dependency-store-task-lookup")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	artifacts := [?]dep_store.Stored_Artifact_Input {
		{
			package_name   = "ZPKG",
			object_kind    = "global-class",
			object_name    = "ZCL_STORE_TASK",
			object_uri     = "/sap/bc/adt/oo/classes/ZCL_STORE_TASK",
			object_type    = "CLAS/OC",
			description    = "Store task class",
			file_extension = "abap",
			source_text    = "CLASS zcl_store_task DEFINITION. ENDCLASS. CLASS zcl_store_task IMPLEMENTATION. ENDCLASS.",
			fetched_at     = "2026-05-21T00:00:00Z",
		},
		{
			package_name   = "ZPKG",
			object_kind    = "include",
			object_name    = "ZINC_STORE_TASK",
			object_uri     = "/sap/bc/adt/programs/includes/ZINC_STORE_TASK",
			object_type    = "PROG/I",
			description    = "Store task include",
			file_extension = "abap",
			source_text    = "DATA gv_store_task TYPE string.",
			fetched_at     = "2026-05-21T00:00:00Z",
		},
	}
	_, err = dep_store.put_artifacts(&store, &profile, artifacts[:], context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 2, task_capacity = 4}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	if pool.options.worker_count > 0 {
		testing.expect_value(t, frontend_runtime.pool_start(&pool), frontend_runtime.Submit_Error.None)
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	remote := [?]analyze.Remote_Dependency_Candidate {
		{name = "ZCL_STORE_TASK", kind = .Type},
		{name = "ZINC_STORE_TASK", kind = .Include},
	}
	seen := make(map[i64]bool, context.allocator)
	before := frontend_runtime.pool_stats(&pool)
	added := deps.add_dependency_store_any_profile_matches(
		&candidates,
		&dependencies,
		remote[:],
		&store,
		&seen,
		&pool,
		"file:///ZMAIN.abap",
		context.allocator,
	)
	after := frontend_runtime.pool_stats(&pool)
	if pool.options.worker_count > 0 {
		frontend_runtime.pool_join(&pool)
	}
	frontend_runtime.pool_destroy(&pool)

	testing.expect(t, added)
	testing.expect_value(t, len(dependencies), 1)
	testing.expect_value(t, len(candidates), 1)
	testing.expect_value(t, dependencies[0].mode, analyze.Source_Mode.Dependency_Interface)
	testing.expect_value(t, candidates[0].input.mode, analyze.Source_Mode.Dependency_Interface)
	testing.expect(t, after.submitted >= before.submitted + u64(len(remote)))
}

@(test)
manifest_local_export_fallback_resolves_remote_candidate :: proc(t: ^testing.T) {
	export_root := external_export_workspace_path("external-export-root")
	export_file := manifest_test_file(
		t,
		export_root,
		"source-code-library/classes/ZCL_LOCAL_EXPORT.abap",
		"CLASS zcl_local_export DEFINITION. ENDCLASS. CLASS zcl_local_export IMPLEMENTATION. ENDCLASS.",
	)
	roots := make([dynamic]string, 0, 1, context.allocator)
	append(&roots, export_root)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lo_dep TYPE REF TO zcl_local_export.",
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	project := deps.analyze_with_dependency_drain(
		target,
		candidates,
		dependencies,
		deps.Dependency_Config{local_export_roots = roots[:]},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	frontend_runtime.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 2)
	testing.expect(t, analyze.project_unit_by_uri(&project, export_file) != nil)
	testing.expect(t, !project_has_diagnostic(&project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
manifest_local_export_match_is_cached_under_manifest_profile :: proc(t: ^testing.T) {
	root := manifest_workspace_path("local-export-cache")
	export_root := external_export_workspace_path("local-export-cache")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	packages := make([dynamic]dep_store.Package_Version, 0, 1, context.allocator)
	append(&packages, dep_store.Package_Version{package_name = "ZPKG", version = "addon"})
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
		packages                = packages[:],
	}
	export_file := manifest_test_file(
		t,
		export_root,
		"ZPKG/source-code-library/classes/ZCL_LOCAL_CACHE.abap",
		"CLASS zcl_local_cache DEFINITION. ENDCLASS. CLASS zcl_local_cache IMPLEMENTATION. ENDCLASS.",
	)
	roots := make([dynamic]string, 0, 1, context.allocator)
	append(&roots, export_root)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lo_dep TYPE REF TO zcl_local_cache.",
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	project := deps.analyze_with_dependency_drain(
		target,
		candidates,
		dependencies,
		deps.Dependency_Config{store = &store, profile = &profile, local_export_roots = roots[:]},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	frontend_runtime.pool_destroy(&pool)

	testing.expect(t, analyze.project_unit_by_uri(&project, export_file) != nil)
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))

	record, ok, lookup_err := dep_store.find_artifact_for_candidate(
		&store,
		&profile,
		"zcl_local_cache",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, lookup_err, dep_store.Store_Error.None)
	testing.expect(t, ok)
	testing.expect_value(t, record.package_name, "zpkg")
	testing.expect_value(t, record.package_version, "addon")
	testing.expect_value(t, record.object_kind, "global-class")
}

@(test)
manifest_project_dotenv_gates_adt_dependency_fetch :: proc(t: ^testing.T) {
	root := manifest_workspace_path("adt-dotenv-gate")
	opened, opened_ok, _ := workspace.open_workspace(root, workspace.Options{enable_adt = true}, context.allocator)
	testing.expect(t, opened_ok)
	testing.expect(t, !opened.has_dotenv)

	manifest_test_file(
		t,
		root,
		".env",
		`
ABAP_ADT_URL=http://127.0.0.1:1
ABAP_ADT_USER=demo
ABAP_ADT_PASSWORD=secret
`,
	)
	opened, opened_ok, _ = workspace.open_workspace(root, workspace.Options{enable_adt = true}, context.allocator)
	testing.expect(t, opened_ok)
	testing.expect(t, opened.has_dotenv)
	workspace.workspace_destroy(&opened, context.allocator)
}

@(test)
adt_fetched_dependency_input_resolves_remote_candidate :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lo_dep TYPE REF TO zcl_adt_fetch.",
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	object_ref := adt.build_class_object_ref("ZCL_ADT_FETCH", "ZPKG", context.allocator)
	defer adt.object_ref_destroy(&object_ref, context.allocator)
	uri_keys := deps.project_input_uri_keys(target.uri, dependencies[:], candidates[:], 1, context.allocator)

	added := deps.add_adt_fetched_dependency_input(
		&candidates,
		&dependencies,
		analyze.Remote_Dependency_Candidate{name = "zcl_adt_fetch", kind = .Type},
		&object_ref,
		"global-class",
		"CLASS zcl_adt_fetch DEFINITION. ENDCLASS. CLASS zcl_adt_fetch IMPLEMENTATION. ENDCLASS.",
		"abap",
		&uri_keys,
		context.allocator,
		context.allocator,
	)
	testing.expect(t, added)
	testing.expect_value(t, len(dependencies), 1)
	testing.expect_value(t, dependencies[0].mode, analyze.Source_Mode.Dependency_Interface)

	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	project := analyze.analyze_target_with_candidate_inputs(
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	frontend_runtime.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 2)
	testing.expect(t, !project_has_diagnostic(&project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
generic_type_adt_fetch_uses_search_before_direct_ddic_probe :: proc(t: ^testing.T) {
	testing.expect(
		t,
		!deps.adt_candidate_direct_first(analyze.Remote_Dependency_Candidate{name = "scit_clas", kind = .Type}),
	)
	testing.expect(
		t,
		deps.adt_candidate_direct_first(
			analyze.Remote_Dependency_Candidate{name = "zcl_demo", kind = .Type, hint = .Object_Type},
		),
	)
}

@(test)
adt_fetched_function_module_input_is_dependency_interface :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://ZMAIN.abap",
		source = `
REPORT zmain.
CALL FUNCTION 'Z_REMOTE_FM'
  EXPORTING iv_value = 1
  IMPORTING ev_value = DATA(lv_value)
  EXCEPTIONS failed = 1.
`,
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	object_ref := adt.Object_Ref {
		uri = strings.clone("/sap/bc/adt/functions/groups/ZFG/fmodules/Z_REMOTE_FM", context.allocator),
		object_type = strings.clone("FUGR/FF", context.allocator),
		name = strings.clone("Z_REMOTE_FM", context.allocator),
		package_name = strings.clone("ZPKG", context.allocator),
		description = strings.clone("Function module", context.allocator),
	}
	defer adt.object_ref_destroy(&object_ref, context.allocator)
	uri_keys := deps.project_input_uri_keys(target.uri, dependencies[:], candidates[:], 1, context.allocator)

	added := deps.add_adt_fetched_dependency_input(
		&candidates,
		&dependencies,
		analyze.Remote_Dependency_Candidate{name = "z_remote_fm", kind = .Function},
		&object_ref,
		"function-module",
		`
FUNCTION z_remote_fm
  IMPORTING iv_value TYPE i OPTIONAL
  EXPORTING ev_value TYPE i
  EXCEPTIONS failed = 1.
  DATA lv_body TYPE zbody_type.
  CALL FUNCTION 'Z_BODY'.
ENDFUNCTION.
`,
		"abap",
		&uri_keys,
		context.allocator,
		context.allocator,
	)
	testing.expect(t, added)
	testing.expect_value(t, len(dependencies), 1)
	testing.expect_value(t, len(candidates), 0)
	testing.expect_value(t, dependencies[0].mode, analyze.Source_Mode.Dependency_Interface)
	testing.expect(t, strings.contains(dependencies[0].uri, "/functions/groups/ZFG/fmodules/Z_REMOTE_FM"))
	testing.expect(t, !strings.contains(dependencies[0].source, "FUNCTION-POOL"))

	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	project := analyze.analyze_target_with_candidate_inputs(
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	frontend_runtime.pool_destroy(&pool)

	dep_unit := analyze.project_unit_by_uri(&project, dependencies[0].uri)
	testing.expect(t, dep_unit != nil)
	if dep_unit != nil {
		testing.expect(t, analyze.find_symbol(dep_unit, "z_remote_fm", .Module) != nil)
		testing.expect(t, has_reference(dep_unit, "i", .Type, .Type_Ref))
		testing.expect(t, !has_reference(dep_unit, "zbody_type", .Type, .Type_Ref))
		for call_site in dep_unit.call_sites {
			testing.expect(t, call_site.target.function_name != "z_body")
		}
	}
	remote_after := analyze.collect_project_remote_dependency_candidates(&project, context.allocator)
	for remote in remote_after {
		testing.expect(t, !(remote.name == "z_body" && remote.kind == .Function))
		testing.expect(t, remote.name != "zbody_type")
	}
}

@(test)
adt_fetched_ddic_table_type_resolves_type_reference :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lt_e071 TYPE tr_objects.",
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	object_ref := adt.Object_Ref {
		uri = strings.clone("/sap/bc/adt/vit/wb/object_type/ttypda/object_name/TR_OBJECTS", context.allocator),
		object_type = strings.clone("TTYP/DA", context.allocator),
		name = strings.clone("TR_OBJECTS", context.allocator),
		package_name = strings.clone("SCTS_PRJ", context.allocator),
		description = strings.clone("Table Type", context.allocator),
	}
	defer adt.object_ref_destroy(&object_ref, context.allocator)
	uri_keys := deps.project_input_uri_keys(target.uri, dependencies[:], candidates[:], 1, context.allocator)

	added := deps.add_adt_fetched_dependency_input(
		&candidates,
		&dependencies,
		analyze.Remote_Dependency_Candidate{name = "tr_objects", kind = .Type},
		&object_ref,
		"ddic-table-type",
		`<abapsource:elementInfo adtcore:type="TTYP/DA" adtcore:name="TR_OBJECTS" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="DTEL/DE" adtcore:name="C">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicRowType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`,
		"xml",
		&uri_keys,
		context.allocator,
		context.allocator,
	)
	testing.expect(t, added)
	testing.expect_value(t, len(dependencies), 1)
	testing.expect_value(t, dependencies[0].mode, analyze.Source_Mode.Dependency_Interface)
	testing.expect(t, contains_fold(dependencies[0].source, "type standard table of c with default key"))

	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	project := analyze.analyze_target_with_candidate_inputs(
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	frontend_runtime.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 2)
	testing.expect(t, !project_has_diagnostic(&project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
ddic_reference_table_type_generates_ref_to_line_type :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TTYP/DA" adtcore:name="ZREFS" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="INTF/OI" adtcore:name="ZIF_REF">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicReferenceType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	table_source := ddic_xml.dependency_source("ZREFS", "ddic-table-type", xml, context.allocator)
	defer delete(table_source, context.allocator)
	testing.expect(t, contains_fold(table_source, "type standard table of ref to zif_ref with default key"))

	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lt_refs TYPE zrefs.",
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table-type/zrefs.abap",
			source = table_source,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/global-interface/zif_ref.abap",
			source = "INTERFACE zif_ref. ENDINTERFACE.",
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	table_unit := analyze.project_unit_by_uri(&project, dependencies[0].uri)

	testing.expect(t, table_unit != nil)
	testing.expect(t, reference_resolves_to_uri(&project, table_unit, "zif_ref", .Type, .Type_Ref, dependencies[1].uri))
	testing.expect(t, !project_units_have_diagnostic(&project, .Invalid_Object_Type_Reference))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
ddic_reference_data_element_generates_ref_to_type :: proc(t: ^testing.T) {
	xml := `<blue:wbobj adtcore:name="ZDE_REF" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>refToClifType</dtel:typeKind>
    <dtel:typeName>ZCL_REF</dtel:typeName>
    <dtel:dataType></dtel:dataType>
  </dtel:dataElement>
</blue:wbobj>`
	ref_source := ddic_xml.dependency_source("ZDE_REF", "ddic-data-element", xml, context.allocator)
	defer delete(ref_source, context.allocator)
	testing.expect(t, contains_fold(ref_source, "type ref to zcl_ref"))

	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lr_ref TYPE zde_ref.",
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-data-element/zde_ref.abap",
			source = ref_source,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/global-class/zcl_ref.abap",
			source = "CLASS zcl_ref DEFINITION. ENDCLASS.",
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	ref_unit := analyze.project_unit_by_uri(&project, dependencies[0].uri)

	testing.expect(t, ref_unit != nil)
	testing.expect(t, reference_resolves_to_uri(&project, ref_unit, "zcl_ref", .Type, .Type_Ref, dependencies[1].uri))
	testing.expect(t, !project_units_have_diagnostic(&project, .Invalid_Object_Type_Reference))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
dependency_xml_detection_prefers_metadata :: proc(t: ^testing.T) {
	testing.expect(t, deps.dependency_source_is_xml("ddic-table-type", "xml", "not xml"))
	testing.expect(t, deps.dependency_source_is_xml("ddic-table-type", "abap", "<ttyp/>"))
	testing.expect(t, !deps.dependency_source_is_xml("global-class", "abap", "<fs> = value."))
}

@(test)
ddic_xml_structure_dependency_resolves_fields :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="zddic_row" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="ID">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">CHAR</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="COUNT">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">INT4</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	source := ddic_xml.dependency_source("ZDDIC_ROW", "ddic-table", xml, context.allocator)
	defer delete(source, context.allocator)
	testing.expect(t, contains_fold(source, "types: begin of zddic_row"))
	testing.expect(t, contains_fold(source, "id type c"))
	testing.expect(t, contains_fold(source, "count type i"))

	target := analyze.Source_Input {
		uri = "mem://ZMAIN.abap",
		source = `
REPORT zmain.
DATA ls_row TYPE zddic_row.
ls_row-id = 'A'.
ls_row-count = 1.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table/zddic_row.abap",
			source = source,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, !has_diagnostic(root, .Unknown_Field))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
ddic_xml_table_type_dependency_uses_row_type :: proc(t: ^testing.T) {
	row_xml := `<abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name="zddic_row" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="ID">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">CHAR</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="TEXT">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">STRING</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	table_xml := `<abapsource:elementInfo adtcore:type="TTYP/DA" adtcore:name="zddic_rows" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name="ZDDIC_ROW">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicRowType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	row_source := ddic_xml.dependency_source("ZDDIC_ROW", "ddic-structure", row_xml, context.allocator)
	defer delete(row_source, context.allocator)
	table_source := ddic_xml.dependency_source("ZDDIC_ROWS", "ddic-table-type", table_xml, context.allocator)
	defer delete(table_source, context.allocator)
	testing.expect(t, contains_fold(row_source, "id type c"))
	testing.expect(t, contains_fold(row_source, "text type string"))
	testing.expect(t, contains_fold(table_source, "type standard table of zddic_row with default key"))

	target := analyze.Source_Input {
		uri = "mem://ZMAIN.abap",
		source = `
REPORT zmain.
DATA lt_rows TYPE zddic_rows.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table-type/zddic_rows.abap",
			source = table_source,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/ddic-structure/zddic_row.abap",
			source = row_source,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)
	table_unit := analyze.project_unit_by_uri(&project, dependencies[0].uri)

	testing.expect(t, root != nil)
	testing.expect(t, table_unit != nil)
	testing.expect(t, reference_resolves_to_uri(&project, root, "zddic_rows", .Type, .Type_Ref, dependencies[0].uri))
	testing.expect(t, reference_resolves_to_uri(&project, table_unit, "zddic_row", .Type, .Type_Ref, dependencies[1].uri))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
adt_fetch_task_result_applies_inputs_without_live_network :: proc(t: ^testing.T) {
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	uri_keys := deps.project_input_uri_keys("mem://ZMAIN.abap", dependencies[:], candidates[:], 2, context.allocator)

	result := new(deps.Adt_Fetch_Task_Result, context.allocator)
	result.fetched = make([dynamic]deps.Adt_Fetched_Object, 0, 1, context.allocator)
	shared := make([dynamic]adt.Dependency_Artifact, 0, 1, context.allocator)
	append(
		&shared,
		adt.Dependency_Artifact {
			object_ref     = adt.build_include_object_ref("ZINC_TASK_RESULT", "ZPKG", context.allocator),
			body           = strings.clone("DATA gv_shared TYPE string.", context.allocator),
			file_extension = strings.clone("abap", context.allocator),
			manifest_kind  = strings.clone("include", context.allocator),
		},
	)
	append(
		&result.fetched,
		deps.Adt_Fetched_Object {
			object_ref = adt.build_class_object_ref("ZCL_TASK_RESULT", "ZPKG", context.allocator),
			fetched = adt.Dependency_Fetch_Result {
				body                = strings.clone("CLASS zcl_task_result DEFINITION. ENDCLASS. CLASS zcl_task_result IMPLEMENTATION. ENDCLASS.", context.allocator),
				file_extension      = strings.clone("abap", context.allocator),
				manifest_kind       = strings.clone("global-class", context.allocator),
				shared_dependencies = shared,
			},
		},
	)
	defer deps.adt_fetch_task_result_destroy(result, context.allocator)

	added := deps.add_adt_fetch_task_result(
		&candidates,
		&dependencies,
		analyze.Remote_Dependency_Candidate{name = "zcl_task_result", kind = .Type},
		result,
		nil,
		nil,
		&uri_keys,
		context.allocator,
		context.allocator,
	)

	testing.expect(t, added)
	testing.expect_value(t, len(dependencies), 1)
	testing.expect_value(t, len(candidates), 1)
	testing.expect_value(t, dependencies[0].mode, analyze.Source_Mode.Dependency_Interface)
	testing.expect_value(t, candidates[0].input.mode, analyze.Source_Mode.Dependency_Interface)
	testing.expect(t, strings.contains(dependencies[0].uri, "/oo/classes/ZCL_TASK_RESULT"))
	testing.expect(t, strings.contains(candidates[0].input.uri, "/programs/includes/ZINC_TASK_RESULT"))
}

@(test)
adt_fetched_dependency_is_cached :: proc(t: ^testing.T) {
	root := manifest_workspace_path("adt-fetch-cache-write")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	object_ref := adt.Object_Ref {
		uri          = "/sap/bc/adt/vit/wb/object_type/ttypda/object_name/TR_OBJECTS",
		object_type  = "TTYP/DA",
		name         = "TR_OBJECTS",
		package_name = "SCTS_PRJ",
		description  = "Table Type",
	}
	shared_dependencies := make([dynamic]adt.Dependency_Artifact, 0, 1, context.allocator)
	append(
		&shared_dependencies,
		adt.Dependency_Artifact {
			object_ref = adt.Object_Ref {
				uri          = "/sap/bc/adt/programs/includes/ZINC_FETCHED",
				object_type  = "PROG/I",
				name         = "ZINC_FETCHED",
				package_name = "ZPKG",
				description  = "Include",
			},
			body           = "DATA gv_fetched TYPE string.",
			file_extension = "abap",
			manifest_kind  = "include",
		},
	)
	fetched := adt.Dependency_Fetch_Result {
		body                = `<abapsource:elementInfo adtcore:type="TTYP/DA" adtcore:name="TR_OBJECTS" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="DTEL/DE" adtcore:name="C">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicRowType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`,
		file_extension      = "xml",
		manifest_kind       = "ddic-table-type",
		shared_dependencies = shared_dependencies,
	}

	deps.store_adt_dependency_fetch(&store, &profile, &object_ref, &fetched, context.allocator)

	record, ok, lookup_err := dep_store.find_artifact_for_candidate(&store, &profile, "tr_objects", .Type, context.allocator)
	testing.expect_value(t, lookup_err, dep_store.Store_Error.None)
	testing.expect(t, ok)
	testing.expect_value(t, record.object_kind, "ddic-table-type")
	testing.expect_value(t, record.file_extension, "xml")
	testing.expect(t, contains_fold(record.source_text, "ddicrowtype"))

	shared, shared_ok, shared_err := dep_store.find_artifact_for_candidate(&store, &profile, "zinc_fetched", .Include, context.allocator)
	testing.expect_value(t, shared_err, dep_store.Store_Error.None)
	testing.expect(t, shared_ok)
	testing.expect_value(t, shared.object_kind, "include")
	testing.expect(t, strings.contains(shared.source_text, "gv_fetched"))
}

@(test)
adt_fetched_ddic_table_preserves_xml_in_cache :: proc(t: ^testing.T) {
	root := manifest_workspace_path("adt-fetch-cache-ddic-table")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	object_ref := adt.Object_Ref {
		uri          = "/sap/bc/adt/vit/wb/object_type/tabldt/object_name/T000",
		object_type  = "TABL/DT",
		name         = "T000",
		package_name = "SABAP",
		description  = "Clients",
	}
	fetched := adt.Dependency_Fetch_Result {
		body = `<abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="t000" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="mandt">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">clnt</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="mtext">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">char</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`,
		file_extension = "xml",
		manifest_kind  = "ddic-table",
	}

	deps.store_adt_dependency_fetch(&store, &profile, &object_ref, &fetched, context.allocator)

	record, ok, lookup_err := dep_store.find_artifact_for_candidate(&store, &profile, "t000", .Type, context.allocator)
	testing.expect_value(t, lookup_err, dep_store.Store_Error.None)
	testing.expect(t, ok)
	testing.expect_value(t, record.object_kind, "ddic-table")
	testing.expect_value(t, record.file_extension, "xml")
	testing.expect(t, contains_fold(record.source_text, "ddicdatatype"))
	testing.expect(t, contains_fold(record.source_text, "mandt"))
	testing.expect(t, contains_fold(record.source_text, "mtext"))
}

@(test)
manifest_unlisted_reachable_include_joins_root_and_selects_owner :: proc(t: ^testing.T) {
	root := manifest_workspace_path("unlisted-include")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
root_file = "src/ZMAIN.abap"
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain. INCLUDE zinc. lv_inc = 1.")
	include_file := manifest_test_file(t, root, "src/ZINC.abap", "DATA lv_inc TYPE i.")

	root_result := analyze_path_test(t, root, root_file)
	requested_include_result := analyze_path_test(t, root, include_file)

	testing.expect(t, root_result.ok)
	testing.expect(t, root_result.used_manifest)
	testing.expect(t, analyze.project_unit_by_uri(&root_result.project, include_file) != nil)
	testing.expect(t, requested_include_result.ok)
	testing.expect(t, requested_include_result.used_manifest)
	testing.expect(t, analyze.project_unit_by_uri(&requested_include_result.project, root_file) != nil)
	testing.expect(t, analyze.project_unit_by_uri(&requested_include_result.project, include_file) != nil)
	testing.expect_value(t, len(requested_include_result.project.units), 2)
}

@(test)
manifest_unclaimed_loose_file_analyzes_standalone :: proc(t: ^testing.T) {
	root := manifest_workspace_path("loose-file")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
root_file = "src/ZMAIN.abap"
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain.")
	loose_file := manifest_test_file(t, root, "src/ZLOOSE.abap", "DATA lv_loose TYPE i.")

	result := analyze_path_test(t, root, loose_file)

	testing.expect(t, result.ok)
	testing.expect(t, !result.used_manifest)
	testing.expect_value(t, len(result.project.units), 1)
	testing.expect_value(t, result.project.units[0].uri, loose_file)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, root_file) == nil)
}

@(test)
manifest_dependency_of_activates_only_selected_root_dependencies :: proc(t: ^testing.T) {
	root := manifest_workspace_path("dependency-of")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
kind = "program"
root_file = "src/ZMAIN.abap"

[[unit]]
name = "ZCL_DEP"
kind = "class"
root_file = "src/ZCL_DEP.abap"
dependency_of = ["src/ZMAIN.abap"]

[[unit]]
name = "ZCL_OTHER"
kind = "class"
root_file = "src/ZCL_OTHER.abap"
dependency_of = ["src/ZOTHER.abap"]
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain. DATA lo_dep TYPE REF TO zcl_dep.")
	dependency_file := manifest_test_file(t, root, "src/ZCL_DEP.abap", "CLASS zcl_dep DEFINITION. ENDCLASS.")
	other_file := manifest_test_file(t, root, "src/ZCL_OTHER.abap", "CLASS zcl_other DEFINITION. ENDCLASS.")

	result := analyze_path_test(t, root, root_file)
	root_unit := analyze.project_unit_by_uri(&result.project, root_file)

	testing.expect(t, result.ok)
	testing.expect(t, result.used_manifest)
	testing.expect(t, root_unit != nil)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, dependency_file) != nil)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, other_file) == nil)
	testing.expect(t, reference_resolves_to_uri(&result.project, root_unit, "zcl_dep", .Type, .Type_Ref, dependency_file))
	testing.expect(t, !has_diagnostic(root_unit, .Unresolved_Reference))
}

@(test)
analyze_target_inline_pool_discovers_reachable_includes_only :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: ztop, zf01.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/ztop.abap", source = "DATA gv_shared TYPE i."},
		{uri = "file:///workspace/zf01.abap", source = "FORM run. gv_shared = 1. ENDFORM."},
		{uri = "file:///workspace/zunused.abap", source = "DATA gv_unused TYPE i."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])

	testing.expect_value(t, len(project.units), 3)
	testing.expect_value(t, project.units[0].uri, target.uri)
	testing.expect_value(t, project.units[1].uri, candidates[0].uri)
	testing.expect_value(t, project.units[2].uri, candidates[1].uri)
	testing.expect(t, analyze.project_unit_by_uri(&project, candidates[2].uri) == nil)
	root := analyze.project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil)
	testing.expect_value(t, include_target_uri(&project, root, "ztop"), candidates[0].uri)
	testing.expect_value(t, include_target_uri(&project, root, "zf01"), candidates[1].uri)
	testing.expect(t, !project_has_diagnostic(&project, .Unresolved_Include))
}

@(test)
analyze_target_threaded_pool_prefers_includes_folder_candidate :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/src/ZREP/ZREP.abap",
		source = "REPORT zrep. INCLUDE zrep_top.",
	}
	includes_uri := "file:///workspace/src/ZREP/Includes/ZREP_TOP.abap"
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/src/includes/ZREP_TOP.abap", source = "DATA lv_global TYPE i."},
		{uri = includes_uri, source = "DATA lv_includes TYPE i."},
	}

	project := analyze_project_test(t, 2, target, candidates[:])
	root := analyze.project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil)
	testing.expect_value(t, include_target_uri(&project, root, "zrep_top"), includes_uri)
	testing.expect_value(t, len(project.units), 2)
}

@(test)
analyze_target_prefers_same_folder_before_includes_folder :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/src/ZREP/ZREP.abap",
		source = "REPORT zrep. INCLUDE zrep_top.",
	}
	same_folder_uri := "file:///workspace/src/ZREP/ZREP_TOP.abap"
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/src/ZREP/Includes/ZREP_TOP.abap", source = "DATA lv_includes TYPE i."},
		{uri = same_folder_uri, source = "DATA lv_same_folder TYPE i."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	root := analyze.project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil)
	testing.expect_value(t, include_target_uri(&project, root, "zrep_top"), same_folder_uri)
	testing.expect_value(t, len(project.units), 2)
}

@(test)
analyze_target_ignores_sibling_candidate_without_include_edge :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. gr_demo = 1.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/zmain_top.abap", source = "DATA gr_demo TYPE i."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])

	testing.expect_value(t, len(project.units), 1)
	testing.expect(t, analyze.project_unit_by_uri(&project, candidates[0].uri) == nil)
	testing.expect(t, len(project.units[0].include_edges) == 0)
}

@(test)
project_global_class_resolves_when_name_matches_unit_stem :: proc(t: ^testing.T) {
	sources := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/zcl_parent.abap",
			source = "CLASS zcl_parent DEFINITION. ENDCLASS.",
		},
		{
			uri = "file:///workspace/zconsumer.abap",
			source = "DATA lo_parent TYPE REF TO zcl_parent.",
		},
	}

	project := analyze_units_project_test(t, sources[:])
	consumer := analyze.project_unit_by_uri(&project, sources[1].uri)

	testing.expect(t, consumer != nil)
	testing.expect(t, reference_resolves_to_uri(&project, consumer, "zcl_parent", .Type, .Type_Ref, sources[0].uri))
	testing.expect(t, !has_diagnostic(consumer, .Unresolved_Reference))
}

@(test)
project_message_class_resolves_from_dependency_provided_name :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri    = "file:///workspace/zmain.abap",
		source = "REPORT zmain MESSAGE-ID zmsg.",
	}
	dependencies := [?]analyze.Source_Input {
		{uri = "abapls-cache:/message-class/zmsg.abap", source = ""},
	}

	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)
	resolved := false
	if root != nil {
		for ref in root.references {
			if ref.kind == .Message_Class && ref.name == "zmsg" {
				resolved = ref.has_resolution && ref.resolution.kind == .External
			}
		}
		testing.expect(t, !has_diagnostic(root, .Unresolved_Reference))
	}

	testing.expect(t, root != nil)
	testing.expect(t, resolved)
}

@(test)
project_program_local_class_without_prefix_stays_unit_local :: proc(t: ^testing.T) {
	sources := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/zprogram_top.abap",
			source = "CLASS zcl_helper DEFINITION. ENDCLASS.",
		},
		{
			uri = "file:///workspace/zconsumer.abap",
			source = "DATA lo_helper TYPE REF TO zcl_helper.",
		},
	}

	project := analyze_units_project_test(t, sources[:])
	consumer := analyze.project_unit_by_uri(&project, sources[1].uri)

	testing.expect(t, consumer != nil)
	testing.expect(t, !reference_resolves_to_uri(&project, consumer, "zcl_helper", .Type, .Type_Ref, sources[0].uri))
	testing.expect(t, has_diagnostic(consumer, .Unresolved_Reference))
}

@(test)
analyze_target_reports_unresolved_include :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "INCLUDE zmissing.",
	}

	project := analyze_project_test(t, 0, target, nil)

	testing.expect_value(t, len(project.units), 1)
	testing.expect(t, project_has_diagnostic(&project, .Unresolved_Include))
	testing.expect(t, has_diagnostic(&project.units[0], .Unresolved_Include))
}

@(test)
analyze_target_allows_missing_if_found_include :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "INCLUDE zmissing IF FOUND.",
	}

	project := analyze_project_test(t, 0, target, nil)

	testing.expect_value(t, len(project.units), 1)
	testing.expect(t, !project_has_diagnostic(&project, .Unresolved_Include))
	testing.expect(t, !has_diagnostic(&project.units[0], .Unresolved_Include))
}

@(test)
analyze_target_reports_include_cycle :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "INCLUDE za.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/za.abap", source = "INCLUDE zb."},
		{uri = "file:///workspace/zb.abap", source = "INCLUDE za."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])

	testing.expect_value(t, len(project.units), 3)
	testing.expect(t, project_has_diagnostic(&project, .Include_Cycle))
}

@(test)
analyze_target_resolves_symbols_from_included_units :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "INCLUDE zinc. lv_inc = 1.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/zinc.abap", source = "DATA lv_inc TYPE i."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect_value(t, include_target_uri(&project, root, "zinc"), candidates[0].uri)
	testing.expect(t, reference_resolves_to_uri(&project, root, "lv_inc", .Value, .Identifier, candidates[0].uri))
}

@(test)
analyze_target_closes_nested_explicit_includes :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "INCLUDE za. gv_leaf = 1.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/za.abap", source = "INCLUDE zb."},
		{uri = "file:///workspace/zb.abap", source = "DATA gv_leaf TYPE i."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	root := analyze.project_unit_by_uri(&project, target.uri)
	za := analyze.project_unit_by_uri(&project, candidates[0].uri)

	testing.expect_value(t, len(project.units), 3)
	testing.expect(t, root != nil)
	testing.expect(t, za != nil)
	testing.expect_value(t, include_target_uri(&project, root, "za"), candidates[0].uri)
	testing.expect_value(t, include_target_uri(&project, za, "zb"), candidates[1].uri)
	testing.expect(t, reference_resolves_to_uri(&project, root, "gv_leaf", .Value, .Identifier, candidates[1].uri))
}

@(test)
analyze_target_included_units_share_compilation_context :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: ztop, zf01.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/ztop.abap", source = "DATA gv_shared TYPE i."},
		{uri = "file:///workspace/zf01.abap", source = "FORM run. gv_shared = 1. ENDFORM."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	form := analyze.project_unit_by_uri(&project, candidates[1].uri)

	testing.expect(t, form != nil)
	testing.expect(t, reference_resolves_to_uri(&project, form, "gv_shared", .Value, .Identifier, candidates[0].uri))
}

@(test)
analyze_target_reports_type_declared_in_later_include :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: zdata, ztypes.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/zdata.abap", source = "DATA ls_object_src TYPE ts_obj_ids."},
		{uri = "file:///workspace/ztypes.abap", source = `
TYPES: BEGIN OF ts_obj_ids,
         owner TYPE c LENGTH 12,
       END OF ts_obj_ids.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	data := analyze.project_unit_by_uri(&project, candidates[0].uri)

	testing.expect(t, data != nil)
	testing.expect(t, has_diagnostic(data, .Unresolved_Reference))
}

@(test)
analyze_target_accepts_type_declared_in_prior_include :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE ztypes. DATA ls_object_src TYPE ts_obj_ids.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/ztypes.abap", source = `
TYPES: BEGIN OF ts_obj_ids,
         owner TYPE c LENGTH 12,
       END OF ts_obj_ids.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, !has_diagnostic(root, .Unresolved_Reference))
}

@(test)
analyze_target_links_class_definition_and_implementation_across_ordered_includes :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = `
REPORT zmain.
INCLUDE: ztop, zcls.
START-OF-SELECTION.
  CREATE OBJECT gr_demo.
  CALL METHOD gr_demo->get_data.
`,
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/ztop.abap", source = `
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS get_data.
ENDCLASS.
`},
		{uri = "file:///workspace/zcls.abap", source = `
DATA gr_demo TYPE REF TO lcl_demo.
CLASS lcl_demo IMPLEMENTATION.
  METHOD get_data.
  ENDMETHOD.
ENDCLASS.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	top := analyze.project_unit_by_uri(&project, candidates[0].uri)
	cls := analyze.project_unit_by_uri(&project, candidates[1].uri)
	class_symbol: ^analyze.Symbol_Data
	member: ^analyze.Class_Member_Data
	if top != nil {
		class_symbol = analyze.find_symbol(top, "lcl_demo", .Class)
		if class_symbol != nil {
			member = class_member_named(top, class_symbol.id, "get_data", .Method)
		}
	}

	testing.expect(t, top != nil)
	testing.expect(t, cls != nil)
	testing.expect(t, class_symbol != nil)
	testing.expect(t, member != nil)
	testing.expect(t, .Has_Implementation in member.flags)
	testing.expect_value(t, member.implementation.unit, cls.unit_id)
	testing.expect(t, !has_diagnostic(top, .Missing_Method_Implementation))
}

@(test)
analyze_target_imports_structure_types_across_includes :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: ztop, zf01.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/ztop.abap", source = `
TYPES: BEGIN OF ty_row,
         comp TYPE string,
       END OF ty_row.
DATA gs_row TYPE ty_row.
`},
		{uri = "file:///workspace/zf01.abap", source = `
FORM run.
  DATA lv_comp TYPE string.
  lv_comp = gs_row-comp.
ENDFORM.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	form := analyze.project_unit_by_uri(&project, candidates[1].uri)

	testing.expect(t, form != nil)
	testing.expect(t, reference_resolves_to_uri(&project, form, "gs_row", .Value, .Identifier, candidates[0].uri))
	testing.expect(t, !has_diagnostic(form, .Unknown_Field))
}

@(test)
analyze_target_propagates_cached_structure_through_class_type_table_component :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = `
INTERFACE lif_tabl.
  TYPES ty_dd03p_tt TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.
  TYPES: BEGIN OF ty_internal,
           dd03p TYPE ty_dd03p_tt,
         END OF ty_internal.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS fill CHANGING cs_data TYPE lif_tabl=>ty_internal.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD fill.
    DATA ls_dd03p LIKE LINE OF cs_data-dd03p.
    ls_dd03p-keyflag = abap_true.
  ENDMETHOD.
ENDCLASS.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-structure/dd03p.abap",
			source = `
TYPES: BEGIN OF dd03p,
         keyflag TYPE abap_bool,
       END OF dd03p.
`,
		},
	}

	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, !has_diagnostic(root, .Unknown_Field))
}

@(test)
analyze_target_reclassifies_open_sql_predicate_globals_from_prior_include :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: ztop, zf01.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/ztop.abap", source = `
DATA p_lgnum TYPE string.
DATA p_lgtyp TYPE string.
DATA p_lgpla TYPE string.
`},
		{uri = "file:///workspace/zf01.abap", source = `
FORM run.
  DATA lw_lgpla TYPE string.
  DATA lw_skzsi TYPE string.
  SELECT SINGLE lgpla skzsi
    FROM lagp
    INTO (lw_lgpla, lw_skzsi)
    WHERE lgnum = p_lgnum
      AND lgtyp = p_lgtyp
      AND lgpla = p_lgpla.
ENDFORM.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	form := analyze.project_unit_by_uri(&project, candidates[1].uri)

	testing.expect(t, form != nil)
	names := [?]string{"p_lgnum", "p_lgtyp", "p_lgpla"}
	for name in names {
		testing.expect(t, !sql_name_ref_present(form, name, .Column))
		testing.expect(t, reference_resolves_to_uri(&project, form, name, .Value, .Identifier, candidates[0].uri))
	}
}

@(test)
validates_project_object_type_method_implementation_and_inherited_visibility :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = `
CLASS lcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS run.
  PRIVATE SECTION.
    METHODS secret.
ENDCLASS.

CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
  PUBLIC SECTION.
    METHODS own.
ENDCLASS.

CLASS lcl_parent IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
  METHOD secret.
  ENDMETHOD.
ENDCLASS.

DATA lo_child TYPE REF TO lcl_child.
DATA lo_bad TYPE lcl_child.

START-OF-SELECTION.
  CALL METHOD lo_child->run.
  CALL METHOD lo_child->secret.
`,
	}

	project := analyze_project_test(t, 0, target, nil)
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, has_diagnostic(root, .Invalid_Object_Type_Reference))
	testing.expect(t, has_diagnostic(root, .Missing_Method_Implementation))
	testing.expect(t, has_diagnostic(root, .Unknown_Field))
}

@(test)
workspace_folder_path_analyzes_manifest_roots :: proc(t: ^testing.T) {
	root := manifest_workspace_path("folder-path")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
root_file = "src/ZMAIN.abap"

[[unit]]
name = "ZOTHER"
root_file = "src/ZOTHER.abap"
`,
	)
	main_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain.")
	other_file := manifest_test_file(t, root, "src/ZOTHER.abap", "REPORT zother.")

	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	result := workspace.analyze_filesystem_path(root, nil, workspace.Options{pool = &pool}, context.allocator)
	frontend_runtime.pool_destroy(&pool)

	testing.expect(t, result.ok)
	testing.expect(t, result.used_manifest)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, main_file) != nil)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, other_file) != nil)
}

@(test)
filesystem_file_path_does_not_open_parent_workspace :: proc(t: ^testing.T) {
	root := manifest_workspace_path("file-path-no-parent-workspace")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
root_file = "src/ZMAIN.abap"

[[unit]]
name = "ZOTHER"
root_file = "src/ZOTHER.abap"
`,
	)
	main_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain.")
	other_file := manifest_test_file(t, root, "src/ZOTHER.abap", "REPORT zother.")

	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	result := workspace.analyze_filesystem_path(main_file, nil, workspace.Options{pool = &pool}, context.allocator)
	frontend_runtime.pool_destroy(&pool)

	testing.expect(t, result.ok)
	testing.expect(t, !result.used_manifest)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, main_file) != nil)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, other_file) == nil)
}
