package abap_frontend_semantic

import "src:adt"
import "src:ast"
import dep_store "src:dependency_store"
import ddic_xml "src:ddic_xml"
import "src:parser"
import "src:tokenizer"
import execution "src:execution"
import lints "src:lints"
import analyze "src:semantic/analyze"
import deps "src:semantic/dependencies"
import remote_deps "src:semantic/remote_dependencies"
import sem_query "src:semantic/query"
import session "src:semantic/session"
import workspace "src:workspace"

import "core:mem"
import "core:mem/virtual"
import net "core:net"
import "core:os"
import filepath "core:path/filepath"
import "core:strings"
import "core:testing"
import "core:thread"
import "core:time"

contains_fold :: proc(source, needle: string) -> bool {
	lower := strings.to_lower(source, context.allocator)
	defer delete(lower, context.allocator)
	return strings.contains(lower, needle)
}

Semantic_Adt_Test_Route :: struct {
	request_contains: string,
	response:         string,
}

Semantic_Adt_Test_Server :: struct {
	listener:         net.TCP_Socket,
	session_response: string,
	search_response:  string,
	missing_response: string,
	fetch_routes:     []Semantic_Adt_Test_Route,
	typepool_owner_response:  string,
	typepool_source_response: string,
	request_buf:      [4096]u8,
	request_count:    int,
	search_count:     int,
	fetch_count:      int,
	typepool_owner_count:  int,
	typepool_source_count: int,
}

semantic_adt_test_server_run :: proc(t: ^thread.Thread) {
	server := cast(^Semantic_Adt_Test_Server)t.data
	for {
		client, _, err := net.accept_tcp(server.listener)
		if err != nil {
			return
		}
		n, recv_err := net.recv_tcp(client, server.request_buf[:])
		response := server.missing_response
		if recv_err == nil {
			server.request_count += 1
			request := string(server.request_buf[:n])
			if strings.contains(request, "/runtime/systemmessages") {
				response = server.session_response
			} else if strings.contains(request, "/sap/bc/zabapls/typepool") &&
			          strings.contains(request, "op=owner") {
				server.typepool_owner_count += 1
				response = server.typepool_owner_response
			} else if strings.contains(request, "/sap/bc/zabapls/typepool") &&
			          strings.contains(request, "op=source") {
				server.typepool_source_count += 1
				response = server.typepool_source_response
			} else if strings.contains(request, "/repository/informationsystem/search") {
				server.search_count += 1
				response = server.search_response
			} else {
				server.fetch_count += 1
				for route in server.fetch_routes {
					if strings.contains(request, route.request_contains) {
						response = route.response
						break
					}
				}
			}
		}
		_, _ = net.send_tcp(client, transmute([]u8)response)
		net.close(client)
	}
}

semantic_test_http_response :: proc(status, body, extra_headers: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "HTTP/1.1 ")
	strings.write_string(&out, status)
	strings.write_string(&out, "\r\nContent-Length: ")
	strings.write_int(&out, len(body))
	strings.write_string(&out, "\r\nConnection: close\r\n")
	strings.write_string(&out, extra_headers)
	strings.write_string(&out, "\r\n")
	strings.write_string(&out, body)
	return strings.to_string(out)
}

semantic_adt_client_for_test_server :: proc(
	t: ^testing.T,
	server: ^Semantic_Adt_Test_Server,
) -> (adt.Client, ^thread.Thread) {
	listener, listen_err := net.listen_tcp(net.Endpoint{address = net.IP4_Loopback, port = 0})
	testing.expect(t, listen_err == nil)
	ep, ep_err := net.bound_endpoint(listener)
	testing.expect(t, ep_err == nil)
	server.listener = listener
	worker := thread.create(semantic_adt_test_server_run)
	worker.data = server
	thread.start(worker)

	base_url := strings.builder_make(context.allocator)
	strings.write_string(&base_url, "http://127.0.0.1:")
	strings.write_int(&base_url, ep.port)
	strings.write_string(&base_url, "/sap/bc/adt")
	client: adt.Client
	adt.client_init(
		&client,
		adt.Connection_Config {
			base_url = strings.to_string(base_url),
			username = "demo",
			password = "secret",
		},
		context.allocator,
	)
	client.http.timeout = 2 * time.Second
	return client, worker
}

semantic_adt_client_for_typepool_test_server :: proc(
	t: ^testing.T,
	server: ^Semantic_Adt_Test_Server,
) -> (adt.Client, ^thread.Thread) {
	client, worker := semantic_adt_client_for_test_server(t, server)
	root := client.connection.base_url
	if strings.has_suffix(root, "/sap/bc/adt") {
		root = root[:len(root) - len("/sap/bc/adt")]
	}
	out := strings.builder_make(context.allocator)
	strings.write_string(&out, root)
	strings.write_string(&out, "/sap/bc/zabapls/typepool")
	client.connection.typepool_resolver_url = strings.to_string(out)
	return client, worker
}

semantic_adt_test_server_stop :: proc(server: ^Semantic_Adt_Test_Server, worker: ^thread.Thread) {
	net.close(server.listener)
	thread.join(worker)
	thread.destroy(worker)
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
	testing.expect(t, analyze.find_symbol(&unit, "%_c_pointer", .Builtin_Type) != nil)
	testing.expect(t, analyze.find_symbol(&unit, "simple", .Builtin_Type) != nil)
	testing.expect(t, analyze.find_symbol(&unit, "numeric", .Builtin_Type) != nil)
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
field_symbol_type_simple_resolves_as_builtin :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"mem://simple_type.abap",
		"FIELD-SYMBOLS <lv_version> TYPE simple.",
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
kernel_pointer_type_resolves_as_builtin :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"mem://kernel_pointer.abap",
		`CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA pointer TYPE %_C_POINTER.
    METHODS get RETURNING VALUE(result) TYPE %_C_POINTER.
ENDCLASS.`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Invalid_Generic_Builtin_Type))
}

@(test)
percent_names_resolve_in_typepool_like_declarations :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"abapls-typepool:/ole2_test.abap",
		`
CONSTANTS: OLE2_%_POINTER POINTER.
TYPES: BEGIN OF OLE2_PCB,
       DATACB LIKE OLE2_%_POINTER,
       END OF OLE2_PCB.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
kernel_percent_constants_resolve_as_builtins :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"mem://kernel_percent_constants.abap",
		`
CONSTANTS charsize TYPE i VALUE %_CHARSIZE.
CONSTANTS endian TYPE abap_endian VALUE %_ENDIAN.
CONSTANTS minchar TYPE abap_char1 VALUE %_MINCHAR.
CONSTANTS maxchar TYPE abap_char1 VALUE %_MAXCHAR.
CONSTANTS horizontal_tab TYPE abap_char1 VALUE %_HORIZONTAL_TAB.
CONSTANTS vertical_tab TYPE abap_char1 VALUE %_VERTICAL_TAB.
CONSTANTS newline TYPE abap_char1 VALUE %_NEWLINE.
CONSTANTS cr_lf TYPE abap_cr_lf VALUE %_CR_LF.
CONSTANTS formfeed TYPE abap_char1 VALUE %_FORMFEED.
CONSTANTS backspace TYPE abap_char1 VALUE %_BACKSPACE.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
generic_builtin_types_are_context_checked :: proc(t: ^testing.T) {
	valid := `FIELD-SYMBOLS <value> TYPE simple.
FORM demo USING iv_number TYPE numeric CHANGING cv_data TYPE data.
ENDFORM.
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_text TYPE csequence.
ENDCLASS.
DATA lr_data TYPE REF TO data.
DATA lr_object TYPE REF TO object.`
	valid_unit := collect_test_unit(t, "file:///generic_builtins_valid.abap", valid)

	testing.expect(t, !has_diagnostic(&valid_unit, .Invalid_Generic_Builtin_Type))
	testing.expect(t, !has_diagnostic(&valid_unit, .Invalid_Object_Type_Reference))
	testing.expect(t, !has_diagnostic(&valid_unit, .Unresolved_Reference))

	invalid := `DATA lv_simple TYPE simple.
TYPES ty_numeric TYPE numeric.
CONSTANTS c_any TYPE any VALUE IS INITIAL.
DATA lr_simple TYPE REF TO simple.
DATA lo_object TYPE object.`
	invalid_unit := collect_test_unit(t, "file:///generic_builtins_invalid.abap", invalid)

	testing.expect(t, has_diagnostic(&invalid_unit, .Invalid_Generic_Builtin_Type))
	testing.expect(t, has_diagnostic(&invalid_unit, .Invalid_Object_Type_Reference))
	testing.expect(t, !has_diagnostic(&invalid_unit, .Unresolved_Reference))
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
interface_type_alias_resolves_method_parameter_type :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://alias_type.abap",
		source = `INTERFACE if_query.
  INTERFACES if_range.
  ALIASES t_name_range FOR if_range~t_name_range.
  METHODS get
    IMPORTING !name_range TYPE t_name_range.
ENDINTERFACE.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/global-interface/if_range.abap",
			source = `INTERFACE if_range.
  TYPES t_name_range TYPE RANGE OF string.
ENDINTERFACE.`,
			mode = .Dependency_Interface,
		},
	}

	project := analyze_project_dependencies_test(t, target, dependencies[:])
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
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
remote_dependency_candidates_include_unresolved_value_identifiers :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///remote_candidates.abap",
		source = `REPORT zmain.
TYPES ls_local_type TYPE string.
DATA lv_ref TYPE REF TO zcl_remote_type.
DATA ls_local TYPE ls_local_type.
DATA ls_client TYPE t000.
unknownvalue = 1.
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
	has_builtin_backing_type := false
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
		if candidate.name == "unknownvalue" {
			has_symbol = true
		}
		if candidate.name == "get_url" {
			has_routine = true
		}
		if candidate.name == "ls_local_type" {
			has_local_type = true
		}
		if candidate.kind == .Type &&
		   (candidate.name == "abap_bool" ||
		    candidate.name == "abap_func_parmbind" ||
		    candidate.name == "icon_l2") {
			has_builtin_backing_type = true
		}
	}
	testing.expect(t, has_type)
	testing.expect(t, has_type_hint)
	testing.expect(t, has_function)
	testing.expect(t, has_standard_type)
	testing.expect(t, has_interface)
	testing.expect(t, has_interface_hint)
	testing.expect(t, has_symbol)
	testing.expect(t, !has_routine)
	testing.expect(t, !has_local_type)
	testing.expect(t, !has_builtin_backing_type)
}

@(test)
remote_dependency_candidates_include_unresolved_like_field_type_bases :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/dd_get_nametab_header.abap",
			source = `TYPES: BEGIN OF ty_local,
         modeflag TYPE c,
       END OF ty_local.
FUNCTION DD_GET_NAMETAB_HEADER
  IMPORTING
    VALUE(STATUS) LIKE DDXTT-MODEFLAG DEFAULT 'A'
    VALUE(UNAME) LIKE SY-UNAME
    VALUE(LOCAL_STATUS) LIKE TY_LOCAL-MODEFLAG
  EXPORTING
    VALUE(R_STATUS) LIKE DDXTT-MODEFLAG.
ENDFUNCTION.
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
	_, ddxtt_pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "ddxtt", kind = .Type}]
	_, sy_pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "sy", kind = .Type}]
	_, local_pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "ty_local", kind = .Type}]
	ddxtt_found := false
	sy_found := false
	local_found := false
	for candidate in analyze.collect_project_remote_dependency_candidates(&project, context.allocator) {
		if candidate.name == "ddxtt" && candidate.kind == .Type {
			ddxtt_found = true
		}
		if candidate.name == "sy" {
			sy_found = true
		}
		if candidate.name == "ty_local" {
			local_found = true
		}
	}

	testing.expect(t, ddxtt_pending)
	testing.expect(t, ddxtt_found)
	testing.expect(t, !sy_pending)
	testing.expect(t, !sy_found)
	testing.expect(t, !local_pending)
	testing.expect(t, !local_found)
}

@(test)
remote_dependency_candidates_include_like_occurs_table_type_bases :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/rsdbrunt.abap",
			source = `TYPES: BEGIN OF ldb_stack_line,
         dyns_fields LIKE rsdsfields OCCURS 0,
       END OF ldb_stack_line.`,
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
	_, pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "rsdsfields", kind = .Type}]
	found := false
	for candidate in analyze.collect_project_remote_dependency_candidates(&project, context.allocator) {
		if candidate.name == "rsdsfields" && candidate.kind == .Type {
			found = true
		}
	}

	testing.expect(t, pending)
	testing.expect(t, found)
}

@(test)
remote_dependency_candidates_include_unresolved_tables_structure_type :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/radmasdl.abap",
			source = `FORM get_non_deleted_objects TABLES resulttab STRUCTURE ddsymtab
                                    rangetab
                             USING par1 par2.
ENDFORM.
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
	_, pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "ddsymtab", kind = .Type}]
	found := false
	for candidate in analyze.collect_project_remote_dependency_candidates(&project, context.allocator) {
		if candidate.name == "ddsymtab" && candidate.kind == .Type {
			found = true
		}
		if candidate.name == "rangetab" {
			testing.expect(t, false)
		}
	}

	testing.expect(t, pending)
	testing.expect(t, found)
}

@(test)
remote_dependency_candidates_include_function_tables_like_row_type :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/rh_read_object.abap",
			source = `FUNCTION rh_read_object
  TABLES
    EXISTENCE LIKE HROEXIST OPTIONAL
  EXCEPTIONS
    NOT_FOUND.
ENDFUNCTION.
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
	_, pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "hroexist", kind = .Type}]
	found := false
	candidates := analyze.collect_project_remote_dependency_candidates(&project, context.allocator)
	for candidate in candidates {
		if candidate.name == "hroexist" && candidate.kind == .Type {
			found = true
		}
		testing.expect(t, candidate.name != "existence")
	}

	testing.expect(t, pending)
	testing.expect(t, found)
	fm := analyze.find_symbol(&project.units[0], "rh_read_object", .Module)
	testing.expect(t, fm != nil)
	if fm != nil {
		fm_info := analyze.entity_decl_info(&project.units[0], fm.id)
		testing.expect(t, fm_info != nil)
		if fm_info != nil {
			testing.expect_value(t, fm_info.signature_parameters[0].type_clause_display, "STANDARD TABLE OF HROEXIST")
		}
	}
}

@(test)
remote_dependency_candidates_include_unresolved_static_targets :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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

	_, static_pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "cl_message_helper", kind = .Static}]
	_, local_pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "lcl_local", kind = .Static}]
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
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
	_, pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "z_remote_fm", kind = .Function}]
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
external_perform_in_program_queues_report_dependency :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/main.abap",
			source = `
REPORT zmain.
DATA lv_protname TYPE string.
PERFORM logdelete IN PROGRAM rddu0001 USING lv_protname.
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
	unit := &project.units[0]
	_, report_pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "rddu0001", kind = .Report}]
	report_found := false
	logdelete_found := false
	for candidate in analyze.collect_project_remote_dependency_candidates(&project, context.allocator) {
		if candidate.name == "rddu0001" && candidate.kind == .Report {
			report_found = true
		}
		if candidate.name == "logdelete" {
			logdelete_found = true
		}
	}

	testing.expect(t, report_pending)
	testing.expect(t, report_found)
	testing.expect(t, !logdelete_found)
	testing.expect(t, !has_reference(unit, "logdelete", .Routine, .Routine_Call))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	testing.expect_value(t, lint_unit.perform_calls[0].program.name, "rddu0001")
}

@(test)
submit_static_report_queues_report_dependency :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/main.abap",
			source = `
REPORT zmain.
SUBMIT scpr3 AND RETURN.
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
	unit := &project.units[0]
	_, report_pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "scpr3", kind = .Report}]
	report_found := false
	for candidate in analyze.collect_project_remote_dependency_candidates(&project, context.allocator) {
		if candidate.name == "scpr3" && candidate.kind == .Report {
			report_found = true
		}
	}

	testing.expect(t, report_pending)
	testing.expect(t, report_found)
	testing.expect(t, !has_reference(unit, "scpr3", .Value, .Identifier))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
	testing.expect_value(t, unit.call_sites[0].target.kind, analyze.Named_Argument_Target_Kind.Report)
	testing.expect_value(t, unit.call_sites[0].target.report_name, "scpr3")
}

@(test)
submit_dynamic_literal_queues_report_dependency :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/main.abap",
			source = `
REPORT zmain.
SUBMIT ('SCPR3') AND RETURN.
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
	_, report_pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "scpr3", kind = .Report}]

	testing.expect(t, report_pending)
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
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

	type_specs := [?][3]string {
		{"abap_bool", "c", "c LENGTH 1"},
		{"abap_keycompname", "abap_keyname", "abap_keyname"},
		{"abap_func_parmbind_tab", "abap_func_parmbind", "SORTED TABLE OF abap_func_parmbind WITH UNIQUE KEY kind name"},
		{"abap_trans_parmname", "string", "string"},
		{"abap_trans_parmref", "data", "REF TO data"},
		{"abap_byte_order_utf8", "x", "x LENGTH 3"},
		{"abap_encoding", "abap_encod", "abap_encod"},
		{"progname", "c", "c LENGTH 40"},
		{"include", "c", "c LENGTH 40"},
	}
	for spec in type_specs {
		s := analyze.find_symbol(&unit, spec[0], .Builtin_Type)
		testing.expect(t, s != nil)
		testing.expect(t, s.has_declared_type)
		testing.expect_value(t, s.declared_type.base_name, spec[1])
		testing.expect_value(t, s.type_clause_display, spec[2])
	}
	trans_ref := analyze.find_symbol(&unit, "abap_trans_parmref", .Builtin_Type)
	testing.expect(t, trans_ref != nil && trans_ref.declared_type.is_ref)

	constant_specs := [?][3]string {
		{"%_charsize", "i", "%_CHARSIZE"},
		{"%_endian", "abap_endian", "%_ENDIAN"},
		{"%_minchar", "abap_char1", "%_MINCHAR"},
		{"%_maxchar", "abap_char1", "%_MAXCHAR"},
		{"%_horizontal_tab", "abap_char1", "%_HORIZONTAL_TAB"},
		{"%_vertical_tab", "abap_char1", "%_VERTICAL_TAB"},
		{"%_newline", "abap_char1", "%_NEWLINE"},
		{"%_cr_lf", "abap_cr_lf", "%_CR_LF"},
		{"%_formfeed", "abap_char1", "%_FORMFEED"},
		{"%_backspace", "abap_char1", "%_BACKSPACE"},
		{"abap_true", "abap_bool", "'X'"},
		{"abap_false", "abap_bool", "' '"},
		{"abap_undefined", "abap_bool", "'-'"},
		{"abap_on", "abap_bool", "'X'"},
		{"abap_off", "abap_bool", "' '"},
		{"abap_max_abs_type_name_ln", "i", "200"},
		{"abap_max_class_name_ln", "i", "30"},
		{"abap_max_intf_name_ln", "i", "30"},
		{"abap_max_comp_name_ln", "i", "30"},
		{"abap_max_key_name_ln", "i", "255"},
		{"abap_max_class_comp_name_ln", "i", "61"},
		{"abap_max_edit_mask_ln", "i", "7"},
		{"abap_max_help_id_ln", "i", "62"},
		{"abap_max_db_string_ln", "i", "536870912"},
		{"abap_max_db_rawstring_ln", "i", "1073741824"},
		{"abap_func_exporting", "abap_func_parmbind-kind", "10"},
		{"abap_func_importing", "abap_func_parmbind-kind", "20"},
		{"abap_func_tables", "abap_func_parmbind-kind", "30"},
		{"abap_func_changing", "abap_func_parmbind-kind", "40"},
		{"icon_led_red", "icon_l2", "'@5C@'"},
		{"icon_led_yellow", "icon_l2", "'@5D@'"},
		{"icon_led_green", "icon_l2", "'@5B@'"},
		{"icon_led_inactive", "icon_l2", "'@BZ@'"},
		{"icon_message_information", "icon_l4", "'@19@'"},
		{"icon_system_help", "icon_l2", "'@35@'"},
		{"icon_stack", "icon_l2", "'@3B@'"},
		{"icon_abap", "icon_l2", "'@9U@'"},
		{"icon_warning", "icon_l2", "'@AH@'"},
		{"icon_package_standard", "icon_l2", "'@QC@'"},
		{"icon_no_status", "icon_l2", "'@MG@'"},
		{"icon_create", "icon_l2", "'@0Y@'"},
		{"icon_delete", "icon_l2", "'@11@'"},
		{"icon_change", "icon_l2", "'@0Z@'"},
		{"icon_adopt", "icon_l2", "'@IL@'"},
		{"icon_okay", "icon_l2", "'@0V@'"},
		{"icon_set_state", "icon_l2", "'@3J@'"},
		{"col_background", "c", "'0'"},
		{"col_heading", "c", "'1'"},
		{"col_normal", "c", "'2'"},
		{"col_total", "c", "'3'"},
		{"col_key", "c", "'4'"},
		{"col_positive", "c", "'5'"},
		{"col_negative", "c", "'6'"},
		{"col_group", "c", "'7'"},
		{"space", "c", "' '"},
	}
	for spec in constant_specs {
		s := analyze.find_symbol(&unit, spec[0], .Builtin_Constant)
		testing.expect(t, s != nil)
		testing.expect(t, s.has_declared_type)
		testing.expect_value(t, s.type_clause_display, spec[1])
		testing.expect_value(t, s.value_clause_display, spec[2])
	}
	builtin_constant_count := 0
	for s in unit.symbols {
		if s.kind != .Builtin_Constant {
			continue
		}
		builtin_constant_count += 1
		testing.expect(t, s.has_declared_type)
		testing.expect(t, s.value_clause_display != "")
	}
	testing.expect_value(t, builtin_constant_count, len(constant_specs))

	abap_func_exporting := analyze.find_symbol(&unit, "abap_func_exporting", .Builtin_Constant)
	testing.expect(t, abap_func_exporting != nil)
	if abap_func_exporting != nil {
		testing.expect_value(t, abap_func_exporting.declared_type.base_name, "abap_func_parmbind")
		testing.expect_value(t, abap_func_exporting.declared_type.field_path[0], "kind")
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

@(test)
transformation_bind_table_line_fields_are_validated :: proc(t: ^testing.T) {
	valid := collect_test_unit(
		t,
		"file:///transformation_bind_valid.abap",
		`
DATA lt_source TYPE abap_trans_srcbind_tab.
DATA lt_result TYPE abap_trans_resbind_tab.
FIELD-SYMBOLS <ls_source> LIKE LINE OF lt_source.
FIELD-SYMBOLS <ls_result> LIKE LINE OF lt_result.
APPEND INITIAL LINE TO lt_source ASSIGNING <ls_source>.
APPEND INITIAL LINE TO lt_result ASSIGNING <ls_result>.
<ls_source>-name = 'ROOT'.
GET REFERENCE OF lt_source INTO <ls_source>-value.
<ls_result>-name = 'ROOT'.
GET REFERENCE OF lt_result INTO <ls_result>-value.
`,
	)
	invalid := collect_test_unit(
		t,
		"file:///transformation_bind_invalid.abap",
		`
DATA lt_result TYPE abap_trans_resbind_tab.
FIELD-SYMBOLS <ls_result> LIKE LINE OF lt_result.
<ls_result>-missing = 'ROOT'.
`,
	)

	testing.expect(t, !has_diagnostic(&valid, .Unknown_Field))
	testing.expect(t, has_diagnostic(&invalid, .Unknown_Field))
	srcbind := analyze.find_structure(&valid, "abap_trans_srcbind")
	resbind := analyze.find_structure(&valid, "abap_trans_resbind")
	testing.expect(t, srcbind != nil)
	testing.expect(t, resbind != nil)
	if srcbind != nil {
		value, ok := analyze.structure_field_info(&valid, srcbind.id, "value")
		testing.expect(t, ok && value.type_ref.is_ref)
	}
	if resbind != nil {
		value, ok := analyze.structure_field_info(&valid, resbind.id, "value")
		testing.expect(t, ok && value.type_ref.is_ref)
	}
}

@(test)
abap_component_descriptors_validate_known_fields :: proc(t: ^testing.T) {
	valid := collect_test_unit(
		t,
		"file:///abap_componentdescr_valid.abap",
		`
FIELD-SYMBOLS <ls_component> TYPE abap_componentdescr.
FIELD-SYMBOLS <ls_simple> TYPE abap_simple_componentdescr.
DATA lv_type TYPE string.
<ls_component>-name = 'FIELD'.
<ls_component>-type = lv_type.
<ls_component>-as_include = abap_false.
<ls_component>-suffix = '_S'.
<ls_simple>-name = 'FIELD'.
<ls_simple>-type = lv_type.
`,
	)
	invalid := collect_test_unit(
		t,
		"file:///abap_componentdescr_invalid.abap",
		`
FIELD-SYMBOLS <ls_component> TYPE abap_componentdescr.
FIELD-SYMBOLS <ls_simple> TYPE abap_simple_componentdescr.
<ls_component>-missing = 'FIELD'.
<ls_simple>-as_include = abap_false.
`,
	)

	testing.expect(t, !has_diagnostic(&valid, .Unknown_Field))
	testing.expect(t, has_diagnostic(&invalid, .Unknown_Field))
	expect_structure_fields(t, &valid, "abap_componentdescr", "name", "type", "as_include", "suffix")
	expect_structure_fields(t, &valid, "abap_simple_componentdescr", "name", "type")
}

@(test)
abap_type_pool_structures_validate_known_fields :: proc(t: ^testing.T) {
	valid := collect_test_unit(
		t,
		"file:///abap_type_pool_structures_valid.abap",
		`
DATA lr_data TYPE REF TO data.
DATA lr_object TYPE REF TO object.
DATA lt_components TYPE abap_component_tab.
DATA lt_symbols TYPE abap_component_symbol_tab.
DATA lt_views TYPE abap_component_view_tab.
FIELD-SYMBOLS <ls_comp> TYPE abap_compdescr.
FIELD-SYMBOLS <ls_key> TYPE abap_keydescr.
FIELD-SYMBOLS <ls_component> LIKE LINE OF lt_components.
FIELD-SYMBOLS <ls_symbol> LIKE LINE OF lt_symbols.
FIELD-SYMBOLS <ls_view> LIKE LINE OF lt_views.
FIELD-SYMBOLS <ls_table_key> TYPE abap_table_keydescr.
FIELD-SYMBOLS <ls_key_component> LIKE LINE OF <ls_table_key>-components.
FIELD-SYMBOLS <ls_func_parm> TYPE abap_func_parmbind.
FIELD-SYMBOLS <ls_func_excp> TYPE abap_func_excpbind.
FIELD-SYMBOLS <ls_parm_bind> TYPE abap_parmbind.
FIELD-SYMBOLS <ls_excp_bind> TYPE abap_excpbind.
FIELD-SYMBOLS <ls_method> TYPE abap_methdescr.
FIELD-SYMBOLS <ls_method_parm> LIKE LINE OF <ls_method>-parameters.
FIELD-SYMBOLS <ls_method_excp> LIKE LINE OF <ls_method>-exceptions.
FIELD-SYMBOLS <ls_attr> TYPE abap_attrdescr.
FIELD-SYMBOLS <ls_intf> TYPE abap_intfdescr.
FIELD-SYMBOLS <ls_type> TYPE abap_typedef.
FIELD-SYMBOLS <ls_event> TYPE abap_evntdescr.
FIELD-SYMBOLS <ls_event_parm> LIKE LINE OF <ls_event>-parameters.
FIELD-SYMBOLS <ls_friend> TYPE abap_frnddescr.
FIELD-SYMBOLS <ls_trans_parm> TYPE abap_trans_parmbind.
FIELD-SYMBOLS <ls_trans_parm_obj> TYPE abap_trans_parm_obj_bind.
FIELD-SYMBOLS <ls_trans_obj> TYPE abap_trans_objbind.
<ls_comp>-name = 'FIELD'.
<ls_key>-name = 'PRIMARY'.
<ls_component>-as_include = abap_false.
<ls_symbol>-name = 'FIELD'.
<ls_view>-type = lr_data.
<ls_table_key>-name = 'PRIMARY'.
<ls_table_key>-access_kind = 'S'.
<ls_key_component>-name = 'FIELD'.
<ls_func_parm>-value = lr_data.
<ls_func_parm>-tables_wa = lr_data.
<ls_func_excp>-message = lr_data.
<ls_func_excp>-name = 'ERROR'.
<ls_parm_bind>-kind = 'I'.
<ls_parm_bind>-value = lr_data.
<ls_excp_bind>-name = 'ERROR'.
<ls_method>-name = 'RUN'.
<ls_method>-is_raising_excps = abap_false.
<ls_method_parm>-name = 'INPUT'.
<ls_method_parm>-by_value = abap_true.
<ls_method_excp>-name = 'ERROR'.
<ls_method_excp>-is_resumable = abap_false.
<ls_attr>-name = 'ATTR'.
<ls_attr>-is_read_only = abap_false.
<ls_intf>-name = 'LIF_INT'.
<ls_intf>-is_inherited = abap_false.
<ls_type>-name = 'TY_DATA'.
<ls_type>-alias_for = 'TY_ALIAS'.
<ls_event>-name = 'EVENT'.
<ls_event>-alias_for = 'ALIAS'.
<ls_event_parm>-name = 'INPUT'.
<ls_friend>-name = 'LCL_FRIEND'.
<ls_trans_parm>-name = 'ROOT'.
<ls_trans_parm>-value = 'ROOT'.
<ls_trans_parm_obj>-name = 'ROOT'.
<ls_trans_parm_obj>-value = lr_data.
<ls_trans_obj>-name = 'ROOT'.
<ls_trans_obj>-value = lr_object.
`,
	)
	invalid := collect_test_unit(
		t,
		"file:///abap_type_pool_structures_invalid.abap",
		`
FIELD-SYMBOLS <ls_table_key> TYPE abap_table_keydescr.
FIELD-SYMBOLS <ls_method> TYPE abap_methdescr.
FIELD-SYMBOLS <ls_func_parm> TYPE abap_func_parmbind.
<ls_table_key>-missing = abap_true.
<ls_method>-missing = abap_true.
<ls_func_parm>-missing = 1.
`,
	)

	testing.expect(t, !has_diagnostic(&valid, .Unknown_Field))
	testing.expect(t, has_diagnostic(&invalid, .Unknown_Field))
	expect_structure_fields(t, &valid, "abap_compdescr", "length", "decimals", "type_kind", "name")
	expect_structure_fields(t, &valid, "abap_keydescr", "name")
	expect_structure_fields(t, &valid, "abap_table_keycompdescr", "name")
	expect_structure_fields(t, &valid, "abap_table_keydescr", "components", "name", "is_primary", "access_kind", "is_unique", "key_kind")
	expect_structure_fields(t, &valid, "abap_parmdescr", "length", "decimals", "type_kind", "name", "parm_kind", "by_value", "is_optional")
	expect_structure_fields(t, &valid, "abap_excpdescr", "name", "is_resumable")
	expect_structure_fields(t, &valid, "abap_frnddescr", "name")
	expect_structure_fields(t, &valid, "abap_intfdescr", "name", "is_inherited")
	expect_structure_fields(t, &valid, "abap_typedef", "name", "alias_for", "visibility", "is_interface", "is_inherited")
	expect_structure_fields(t, &valid, "abap_attrdescr", "length", "decimals", "name", "type_kind", "visibility", "is_interface", "is_inherited", "is_class", "is_constant", "is_virtual", "is_read_only", "alias_for")
	expect_structure_fields(t, &valid, "abap_methdescr", "parameters", "exceptions", "name", "for_event", "of_class", "visibility", "is_interface", "is_inherited", "is_redefined", "is_abstract", "is_final", "is_class", "alias_for", "is_raising_excps")
	expect_structure_fields(t, &valid, "abap_evntdescr", "parameters", "name", "visibility", "is_interface", "is_inherited", "is_class", "alias_for")
	expect_structure_fields(t, &valid, "abap_func_parmbind", "value", "tables_wa", "kind", "name")
	expect_structure_fields(t, &valid, "abap_func_excpbind", "message", "value", "name")
	expect_structure_fields(t, &valid, "abap_parmbind", "name", "kind", "value")
	expect_structure_fields(t, &valid, "abap_excpbind", "name", "value")
	expect_structure_fields(t, &valid, "abap_trans_parmbind", "name", "value")
	expect_structure_fields(t, &valid, "abap_trans_parm_obj_bind", "name", "value")
	expect_structure_fields(t, &valid, "abap_trans_objbind", "name", "value")
}

collect_test_unit :: proc(t: ^testing.T, uri, source: string) -> analyze.Unit_Analysis {
	parsed := parser.parse(source, uri, context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 64}, context.allocator)
	defer execution.pool_destroy(&pool)
	return analyze.analyze_unit(analyze.Unit_Id(0), uri, source, parsed, &pool, context.allocator)
}

expect_type_kind :: proc(
	t: ^testing.T,
	unit: ^analyze.Unit_Analysis,
	id: analyze.Type_Id,
	kind: analyze.Type_Kind,
) -> ^analyze.Type_Data {
	type_data := analyze.type_data(unit, id)
	testing.expect(t, type_data != nil)
	if type_data != nil {
		testing.expect_value(t, type_data.kind, kind)
	}
	return type_data
}

expect_operand :: proc(
	t: ^testing.T,
	unit: ^analyze.Unit_Analysis,
	operand: ^analyze.Operand_Data,
	mode: analyze.Operand_Mode,
	builtin_type_name: string,
) {
	testing.expect(t, operand != nil)
	if operand == nil {
		return
	}
	testing.expect_value(t, operand.mode, mode)
	type_data := expect_type_kind(t, unit, operand.type_fact.type_id, .Builtin)
	if type_data != nil {
		testing.expect_value(t, type_data.name, builtin_type_name)
	}
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

diagnostic_count :: proc(unit: ^analyze.Unit_Analysis, kind: analyze.Diagnostic_Kind) -> int {
	count := 0
	for diagnostic in unit.diagnostics {
		if diagnostic.kind == kind {
			count += 1
		}
	}
	return count
}

diagnostic_message_for_kind :: proc(unit: ^analyze.Unit_Analysis, kind: analyze.Diagnostic_Kind) -> (string, bool) {
	for diagnostic in unit.diagnostics {
		if diagnostic.kind == kind {
			return diagnostic.message, true
		}
	}
	return "", false
}

diagnostic_present :: proc(diagnostics: []analyze.Diagnostic, kind: analyze.Diagnostic_Kind) -> bool {
	for diagnostic in diagnostics {
		if diagnostic.kind == kind {
			return true
		}
	}
	return false
}

@(test)
parse_errors_are_analysis_diagnostics :: proc(t: ^testing.T) {
	source := `CLASS lcl_owner DEFINITION.
  PUBLIC SECTION.
    METHODS get_generic.
ENDCLASS.
CLASS lcl_owner IMPLEMENTATION.
  METHOD get_generic.
    DATA ro_generic TYPE REF TO object.
    CREATE OBJECT ro_generic EXPORTING io_field_rules = get_field_rules( )1.
  ENDMETHOD.
ENDCLASS.`
	parsed := parser.parse(source, "file:///syntax_diagnostic.abap", context.allocator)
	testing.expect(t, len(parsed.errors) > 0)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 64}, context.allocator)
	defer execution.pool_destroy(&pool)
	unit := analyze.analyze_unit(
		analyze.Unit_Id(0),
		"file:///syntax_diagnostic.abap",
		source,
		parsed,
		&pool,
		context.allocator,
	)

	testing.expect(t, has_diagnostic(&unit, .Syntax_Error))
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
	pool: execution.Pool
	options := execution.Options {
		worker_count = worker_count,
		task_capacity = 128,
		queue_capacity = 32,
		deque_capacity = 32,
	}
	execution.pool_init(&pool, options, context.allocator)
	if pool.options.worker_count > 0 {
		execution.pool_start(&pool)
	}
	project := analyze.analyze_target(target, candidates, analyze.Analyze_Options{pool = &pool}, context.allocator)
	if pool.options.worker_count > 0 {
		execution.pool_join(&pool)
	}
	execution.pool_destroy(&pool)
	return project
}

analyze_project_dependencies_test :: proc(
	t: ^testing.T,
	target: analyze.Source_Input,
	dependencies: []analyze.Source_Input,
) -> analyze.Project_Analysis {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, 0, context.allocator)
	project := analyze.analyze_target_with_candidate_inputs(
		target,
		candidates[:],
		dependencies,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)
	return project
}

@(test)
project_state_incremental_dependency_update_resolves_waiting_unit :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
project_state_dependency_structure_update_revalidates_dependents :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	target := analyze.Source_Input {
		uri = "mem://main.abap",
		source = `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run CHANGING !ct_rows TYPE zrows.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA lv_name TYPE string.
    FIELD-SYMBOLS <row> LIKE LINE OF ct_rows.
    lv_name = <row>-full_name.
  ENDMETHOD.
ENDCLASS.`,
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, 0, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, 0, 2, context.allocator)
	append(
		&dependencies,
		analyze.Source_Input {
			uri = "abapls-cache:/ddic-table-type/zrows.abap",
			source = "TYPES zrows TYPE STANDARD TABLE OF zrow WITH DEFAULT KEY.",
			mode = .Dependency_Interface,
		},
	)

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
	testing.expect(t, project_units_have_diagnostic(&project, .Unresolved_Reference))
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))

	append(
		&dependencies,
		analyze.Source_Input {
			uri = "abapls-cache:/ddic-structure/zrow.abap",
			source = `TYPES: BEGIN OF zrow,
         full_name TYPE string,
       END OF zrow.`,
			mode = .Dependency_Interface,
		},
	)
	project = analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	root = analyze.project_unit_by_uri(&project, target.uri)

	testing.expect_value(t, len(project.units), 3)
	testing.expect(t, root != nil)
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
project_state_dependency_nested_structure_update_revalidates_dependents :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	target := analyze.Source_Input {
		uri = "mem://main.abap",
		source = `TYPES: BEGIN OF ty_dummy,
         value TYPE string,
       END OF ty_dummy.
TYPES: BEGIN OF ty_other,
         value TYPE string,
       END OF ty_other.
DATA ls_outer TYPE zouter.
ls_outer-inner-leaf = 'x'.`,
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, 0, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, 0, 2, context.allocator)
	append(
		&dependencies,
		analyze.Source_Input {
			uri = "abapls-cache:/ddic-structure/zouter.abap",
			source = `TYPES: BEGIN OF zouter,
         inner TYPE zinner,
       END OF zouter.`,
			mode = .Dependency_Interface,
		},
	)

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
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))

	append(
		&dependencies,
		analyze.Source_Input {
			uri = "abapls-cache:/ddic-structure/zinner.abap",
			source = `TYPES: BEGIN OF zinner,
         leaf TYPE string,
       END OF zinner.`,
			mode = .Dependency_Interface,
		},
	)
	project = analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	root = analyze.project_unit_by_uri(&project, target.uri)

	testing.expect_value(t, len(project.units), 3)
	testing.expect(t, root != nil)
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
project_state_dependency_private_change_keeps_interface_signature :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
project_state_ignores_unparsed_candidate_provided_names :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	target := analyze.Source_Input {
		uri = "file:///workspace/main.abap",
		source = "REPORT zmain. INCLUDE ztop.",
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, 1, context.allocator)
	project := analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		{},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	root := analyze.project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil)
	if root != nil {
		testing.expect_value(t, len(root.include_edges), 1)
		testing.expect(t, !root.include_edges[0].has_target)
	}

	append(
		&candidates,
		analyze.Project_Candidate_Input {
			input = analyze.Source_Input {
				uri = "file:///workspace/zother.abap",
				source = "DATA lv_other TYPE i.",
			},
			object_name = "zother",
		},
	)
	project = analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		{},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	root = analyze.project_unit_by_uri(&project, target.uri)

	testing.expect_value(t, len(project.units), 1)
	testing.expect(t, root != nil)
	if root != nil {
		testing.expect_value(t, len(root.include_edges), 1)
		testing.expect(t, !root.include_edges[0].has_target)
	}
}

@(test)
analysis_session_keeps_targets_that_are_also_candidates :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	targets := [?]analyze.Source_Input {
		{uri = "file:///workspace/main.abap", source = "INCLUDE zshared. gv_shared = 1."},
		{uri = "file:///workspace/generated.abap", source = "REPORT zshared. DATA gv_shared TYPE i."},
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, len(targets), context.allocator)
	for target in targets {
		append(&candidates, analyze.Project_Candidate_Input{input = target})
	}
	project := session.analysis_session_analyze_once(
		targets[:],
		candidates[:],
		{},
		remote_deps.Dependency_Config{},
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
analysis_session_keeps_unchanged_inputs_clean :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	target := analyze.Source_Input {
		uri = "file:///workspace/main.abap",
		source = "REPORT zmain. INCLUDE ztop. lv_top = 1.",
	}
	include := analyze.Source_Input {
		uri = "file:///workspace/ztop.abap",
		source = "DATA lv_top TYPE i.",
	}
	changes := [?]session.Input_Change {
		{kind = .Upsert, role = .Target, input = target},
		{kind = .Upsert, role = .Candidate, input = include, object_name = "ztop"},
	}
	analysis_session := session.analysis_session_make(
		remote_deps.Dependency_Config{},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	defer session.analysis_session_destroy(&analysis_session)

	first := session.analysis_session_apply_changes(&analysis_session, changes[:])
	second := session.analysis_session_apply_changes(&analysis_session, changes[:])
	root := analyze.project_unit_by_uri(&second.project, target.uri)

	testing.expect_value(t, len(first.project.units), 2)
	testing.expect_value(t, second.dirty_count, 0)
	testing.expect_value(t, len(second.project.units), 2)
	testing.expect(t, root != nil)
	if root != nil {
		testing.expect_value(t, include_target_uri(&second.project, root, "ztop"), include.uri)
	}
}

@(test)
analysis_session_ignores_editor_change_to_immutable_dependency :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	target := analyze.Source_Input {
		uri = "file:///workspace/main.abap",
		source = "DATA lo_dep TYPE REF TO zcl_dep.",
	}
	dependency := analyze.Source_Input {
		uri = "file:///workspace/zcl_dep.abap",
		source = "CLASS zcl_dep DEFINITION. ENDCLASS.",
		mode = .Dependency_Interface,
	}
	initial := [?]session.Input_Change {
		{kind = .Upsert, role = .Target, input = target},
		{kind = .Upsert, role = .Dependency, input = dependency, immutable = true},
	}
	editor_change := [?]session.Input_Change {
		{
			kind = .Upsert,
			role = .Dependency,
			input = analyze.Source_Input {
				uri = dependency.uri,
				source = "",
				mode = .Dependency_Interface,
			},
		},
	}
	analysis_session := session.analysis_session_make(
		remote_deps.Dependency_Config{},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	defer session.analysis_session_destroy(&analysis_session)

	_ = session.analysis_session_apply_changes(&analysis_session, initial[:])
	result := session.analysis_session_apply_changes(&analysis_session, editor_change[:])
	root := analyze.project_unit_by_uri(&result.project, target.uri)

	testing.expect_value(t, result.dirty_count, 0)
	testing.expect(t, root != nil)
	if root != nil {
		testing.expect(t, reference_resolves_to_uri(&result.project, root, "zcl_dep", .Type, .Type_Ref, dependency.uri))
	}
}

@(test)
project_state_unresolved_candidates_keep_one_waiter_per_unit :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
	units, ok := state.unresolved_candidates[deps.Remote_Dependency_Key {
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
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
	_, pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "enlfdir", kind = .Type}]

	testing.expect(t, !pending)
	testing.expect(t, root != nil)
	testing.expect(t, root != nil && !has_diagnostic(root, .Unresolved_Open_Sql_Source))
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))
}

@(test)
project_state_unresolved_candidates_skip_resolved_function_dependency :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
	_, pending := state.unresolved_candidates[deps.Remote_Dependency_Key{name = "z_remote_fm", kind = .Function}]
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
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
project_state_root_namespace_change_revalidates_type_reference :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	targets := [?]analyze.Source_Input {
		{uri = "file:///workspace/zfoo.abap", source = "REPORT zfoo."},
		{uri = "file:///workspace/zconsumer.abap", source = "DATA lo_foo TYPE REF TO zfoo."},
	}
	project := analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		nil,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	consumer := analyze.project_unit_by_uri(&project, targets[1].uri)
	testing.expect(t, consumer != nil)
	if consumer != nil {
		testing.expect(t, has_diagnostic(consumer, .Wrong_Namespace))
	}

	targets[0].source = "CLASS zfoo DEFINITION. ENDCLASS."
	project = analyze.project_state_analyze_targets_with_candidate_inputs(
		&state,
		targets[:],
		nil,
		nil,
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	consumer = analyze.project_unit_by_uri(&project, targets[1].uri)
	testing.expect(t, consumer != nil)
	if consumer != nil {
		testing.expect(t, reference_resolves_to_uri(&project, consumer, "zfoo", .Type, .Type_Ref, targets[0].uri))
		testing.expect(t, !has_diagnostic(consumer, .Wrong_Namespace))
	}
}

@(test)
project_state_public_interface_change_revalidates_reverse_dependents :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

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
project_state_parent_signature_change_updates_effective_method_parameter :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	target := analyze.Source_Input {
		uri = "file:///workspace/zcl_child.abap",
		source = `CLASS zcl_child DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    METHODS run REDEFINITION.
ENDCLASS.
CLASS zcl_child IMPLEMENTATION.
  METHOD run.
    DATA lv_text TYPE string.
    lv_text = iv_text.
  ENDMETHOD.
ENDCLASS.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/zcl_parent.abap",
			source = `CLASS zcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_text TYPE string.
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
	child := analyze.project_unit_by_uri(&project, target.uri)
	testing.expect(t, child != nil && !has_diagnostic(child, .Unresolved_Reference))

	dependencies[0].source = `CLASS zcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_other TYPE string.
ENDCLASS.`
	project = analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	child = analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, child != nil && has_diagnostic(child, .Unresolved_Reference))
	testing.expect(t, child != nil && unresolved_reference_count(child, "iv_text", .Value, .Identifier) == 1)
}

@(test)
project_state_event_parameter_type_change_recalculates_handler_type :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	target := analyze.Source_Input {
		uri = "file:///workspace/zhandler.abap",
		source = `CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_saved FOR EVENT saved OF zcl_source IMPORTING ex_object.
ENDCLASS.
CLASS lcl_handler IMPLEMENTATION.
  METHOD on_saved.
    DATA lv_type TYPE string.
    lv_type = ex_object->object_type.
  ENDMETHOD.
ENDCLASS.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/zcl_source.abap",
			source = `CLASS zcl_source DEFINITION.
  PUBLIC SECTION.
    EVENTS saved EXPORTING VALUE(ex_object) TYPE REF TO zcl_object_a.
ENDCLASS.`,
			mode = .Dependency_Interface,
		},
		{
			uri = "file:///workspace/zcl_object_a.abap",
			source = `CLASS zcl_object_a DEFINITION.
  PUBLIC SECTION.
    DATA object_type TYPE string.
ENDCLASS.`,
			mode = .Dependency_Interface,
		},
		{
			uri = "file:///workspace/zcl_object_b.abap",
			source = `CLASS zcl_object_b DEFINITION.
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
	handler := analyze.project_unit_by_uri(&project, target.uri)
	testing.expect(t, handler != nil && !has_diagnostic(handler, .Unknown_Field))

	dependencies[0].source = `CLASS zcl_source DEFINITION.
  PUBLIC SECTION.
    EVENTS saved EXPORTING VALUE(ex_object) TYPE REF TO zcl_object_b.
ENDCLASS.`
	project = analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	handler = analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, handler != nil && has_diagnostic(handler, .Unknown_Field))
}

@(test)
analyze_handles_more_units_than_initial_task_capacity :: proc(t: ^testing.T) {
	target := analyze.Source_Input{uri = "mem://main.abap", source = "REPORT zmain."}
	dependencies := make([dynamic]analyze.Source_Input, 0, 5, context.allocator)
	append(&dependencies, analyze.Source_Input{uri = "mem://dep1.abap", source = "REPORT zdep1."})
	append(&dependencies, analyze.Source_Input{uri = "mem://dep2.abap", source = "REPORT zdep2."})
	append(&dependencies, analyze.Source_Input{uri = "mem://dep3.abap", source = "REPORT zdep3."})
	append(&dependencies, analyze.Source_Input{uri = "mem://dep4.abap", source = "REPORT zdep4."})
	append(&dependencies, analyze.Source_Input{uri = "mem://dep5.abap", source = "REPORT zdep5."})

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 2}, context.allocator)
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	project := analyze.analyze_target_with_candidate_inputs(
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

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
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	run_options := options
	run_options.pool = &pool
	opened, workspace_ok, workspace_error := workspace.open_workspace(root, run_options, context.allocator)
	testing.expect(t, workspace_ok)
	if !workspace_ok {
		execution.pool_destroy(&pool)
		return workspace.Analysis_Result{ok = false, error = workspace_error}
	}
	result := workspace.analyze_path(&opened, target_path, nil, run_options, context.allocator)
	workspace.workspace_destroy(&opened, context.allocator)
	execution.pool_destroy(&pool)
	return result
}

analyze_standalone_path_test_with_options :: proc(
	t: ^testing.T,
	target_path: string,
	options: workspace.Options,
) -> workspace.Analysis_Result {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	run_options := options
	run_options.pool = &pool
	target_abs, target_ok := workspace.absolute_clean_path(target_path, context.allocator)
	testing.expect(t, target_ok)
	if !target_ok {
		execution.pool_destroy(&pool)
		return workspace.Analysis_Result{ok = false, error = "invalid target path"}
	}
	opened, workspace_ok, workspace_error := workspace.open_standalone_workspace(
		filepath.dir(target_abs),
		run_options,
		context.allocator,
	)
	testing.expect(t, workspace_ok)
	if !workspace_ok {
		execution.pool_destroy(&pool)
		return workspace.Analysis_Result{ok = false, error = workspace_error}
	}
	result := workspace.analyze_path(&opened, target_abs, nil, run_options, context.allocator)
	workspace.workspace_destroy(&opened, context.allocator)
	execution.pool_destroy(&pool)
	return result
}

manifest_workspace_path :: proc(name: string) -> string {
	package_dir := filepath.dir(#file)
	root, _ := filepath.join(
		{package_dir, "..", "..", "bin", "test-data", "manifest", semantic_test_workspace_name(name)},
		context.allocator,
	)
	os.make_directory_all(root)
	return root
}

external_export_workspace_path :: proc(name: string) -> string {
	package_dir := filepath.dir(#file)
	root, _ := filepath.join(
		{package_dir, "..", "..", "bin", "test-data", "local-export", semantic_test_workspace_name(name)},
		context.allocator,
	)
	os.make_directory_all(root)
	return root
}

semantic_test_workspace_name :: proc(name: string) -> string {
	out := strings.builder_make(context.allocator)
	strings.write_string(&out, name)
	strings.write_byte(&out, '-')
	strings.write_int(&out, os.get_pid())
	strings.write_byte(&out, '-')
	strings.write_i64(&out, time.time_to_unix_nano(time.now()))
	return strings.to_string(out)
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
	for unit_index in 0 ..< len(units) {
		for &edge in units[unit_index].include_edges {
			for &target in units {
				if target.unit_id != units[unit_index].unit_id &&
				   provided_name_present(&target, edge.name) {
					edge.target = target.unit_id
					edge.has_target = true
					break
				}
			}
		}
	}
	project := analyze.project_analysis_from_units(units, context.allocator)
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	analyze.finish_project_analysis(&project, &pool, {}, context.allocator)
	execution.pool_destroy(&pool)
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
) -> ^analyze.Symbol_Data {
	if member := analyze.unit_class_member_symbol(unit, class_symbol, name); member != nil {
		if info := analyze.entity_decl_info(unit, member.id); info != nil && info.member_kind == kind {
			return member
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

unresolved_reference_count :: proc(
	unit: ^analyze.Unit_Analysis,
	name: string,
	namespace: analyze.Namespace,
	kind: analyze.Reference_Kind,
) -> int {
	count := 0
	for reference in unit.references {
		if reference.name == name &&
		   reference.namespace == namespace &&
		   reference.kind == kind &&
		   !reference.has_resolution {
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

expect_structure_fields :: proc(
	t: ^testing.T,
	unit: ^analyze.Unit_Analysis,
	name: string,
	fields: ..string,
) {
	st := analyze.find_structure(unit, name)
	testing.expect(t, st != nil)
	if st != nil {
		testing.expect(t, field_names_match(st, fields))
	}
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
	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	for order in lint_unit.internal_table_orders {
		if order.table_name == table_name && string_list_matches(order.key_fields, fields) {
			return true
		}
	}
	return false
}

binary_search_present :: proc(unit: ^analyze.Unit_Analysis, table_name: string, fields: []string) -> bool {
	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	for read in lint_unit.read_table_binary_searches {
		if read.table_name == table_name && string_list_matches(read.key_fields, fields) {
			return true
		}
	}
	return false
}

system_update_present :: proc(
	unit: ^analyze.Unit_Analysis,
	statement: lints.System_Field_Statement_Kind,
	field_name: string,
) -> bool {
	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	for update in lint_unit.system_field_updates {
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
collected_symbols_have_persistent_decl_info :: proc(t: ^testing.T) {
	source := `
DATA gv_value TYPE i.
CONSTANTS gc_limit TYPE i VALUE 1.
`
	unit := collect_test_unit(t, "file:///decl_info.abap", source)
	gv := analyze.find_symbol(&unit, "gv_value", .Variable)
	gc := analyze.find_symbol(&unit, "gc_limit", .Constant)

	testing.expect(t, gv != nil)
	testing.expect(t, gc != nil)
	if gv != nil {
		testing.expect(t, gv.decl_info != analyze.INVALID_DECL_INFO_ID)
		info := analyze.decl_info(&unit, gv.decl_info)
		testing.expect(t, info != nil)
		if info != nil {
			testing.expect_value(t, info.entity, gv.id)
			testing.expect_value(t, info.kind, analyze.Symbol_Kind.Variable)
			testing.expect(t, info.type_clause != nil)
			testing.expect_value(t, info.state, analyze.Decl_Info_State.Unresolved)
		}
	}
	if gc != nil {
		info := analyze.decl_info(&unit, gc.decl_info)
		testing.expect(t, info != nil)
		if info != nil {
			testing.expect_value(t, info.entity, gc.id)
			testing.expect(t, info.value_clause != nil)
		}
	}
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
TYPES ty_unique TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
TYPES ty_initial TYPE STANDARD TABLE OF string INITIAL SIZE 5.`
	unit := collect_test_unit(t, "file:///type_ref_display_bounds.abap", source)

	int_eket := analyze.find_symbol(&unit, "int_eket", .Variable)
	lv_value := analyze.find_symbol(&unit, "lv_value", .Variable)
	lv_len := analyze.find_symbol(&unit, "lv_len", .Variable)
	lv_dec := analyze.find_symbol(&unit, "lv_dec", .Variable)
	mv_text := analyze.find_symbol(&unit, "mv_text", .Variable)
	p_count := analyze.find_symbol(&unit, "p_count", .Variable)
	ty_def := analyze.find_symbol(&unit, "ty_def", .Type_Def)
	ty_unique := analyze.find_symbol(&unit, "ty_unique", .Type_Def)
	ty_initial := analyze.find_symbol(&unit, "ty_initial", .Type_Def)
	testing.expect(t, int_eket != nil && lv_value != nil && lv_len != nil && lv_dec != nil)
	testing.expect(t, mv_text != nil && p_count != nil && ty_def != nil && ty_unique != nil && ty_initial != nil)
	testing.expect_value(t, int_eket.type_clause_display, "beket")
	testing.expect_value(t, lv_value.type_clause_display, "i")
	testing.expect_value(t, lv_len.type_clause_display, "c")
	testing.expect_value(t, lv_dec.type_clause_display, "p")
	testing.expect_value(t, mv_text.type_clause_display, "string")
	testing.expect_value(t, p_count.type_clause_display, "i")
	testing.expect_value(t, ty_def.type_clause_display, "STANDARD TABLE OF string WITH DEFAULT KEY")
	testing.expect_value(t, ty_unique.type_clause_display, "SORTED TABLE OF string WITH UNIQUE KEY table_line")
	testing.expect_value(t, ty_initial.type_clause_display, "STANDARD TABLE OF string INITIAL SIZE 5")
}

@(test)
canonical_type_ids_cover_declared_types_and_structures :: proc(t: ^testing.T) {
	source := `
CLASS lcl_demo DEFINITION.
ENDCLASS.
INTERFACE lif_demo.
ENDINTERFACE.
TYPES: BEGIN OF ty_line,
         text TYPE string,
       END OF ty_line.
TYPES ty_lines TYPE STANDARD TABLE OF ty_line WITH DEFAULT KEY.
DATA ls_line TYPE ty_line.
DATA lt_lines TYPE ty_lines.
DATA lr_demo TYPE REF TO lcl_demo.
DATA lr_if TYPE REF TO lif_demo.
`
	unit := collect_test_unit(t, "file:///canonical_types.abap", source)

	ty_line := analyze.find_symbol(&unit, "ty_line", .Type_Def)
	ty_lines := analyze.find_symbol(&unit, "ty_lines", .Type_Def)
	ls_line := analyze.find_symbol(&unit, "ls_line", .Variable)
	lt_lines := analyze.find_symbol(&unit, "lt_lines", .Variable)
	lr_demo := analyze.find_symbol(&unit, "lr_demo", .Variable)
	lr_if := analyze.find_symbol(&unit, "lr_if", .Variable)
	text_field := analyze.structure_field(&unit, ty_line.structure, "text") if ty_line != nil else nil

	testing.expect(t, ty_line != nil && ty_lines != nil && ls_line != nil && lt_lines != nil)
	testing.expect(t, lr_demo != nil && lr_if != nil && text_field != nil)

	line_type := expect_type_kind(t, &unit, ty_line.type_id, .Named)
	line_base := expect_type_kind(t, &unit, line_type.base, .Structure)
	testing.expect_value(t, line_base.structure, ty_line.structure)
	text_type := expect_type_kind(t, &unit, text_field.type_id, .Builtin)
	testing.expect_value(t, text_type.name, "string")
	testing.expect_value(t, ls_line.type_id, ty_line.type_id)

	lines_type := expect_type_kind(t, &unit, ty_lines.type_id, .Named)
	lines_table := expect_type_kind(t, &unit, lines_type.base, .Table)
	testing.expect_value(t, lines_table.base, ty_line.type_id)
	testing.expect_value(t, lines_table.table_form, ast.Data_Type_Form.Standard_Table)
	testing.expect_value(t, lt_lines.type_id, ty_lines.type_id)

	demo_ref := expect_type_kind(t, &unit, lr_demo.type_id, .Ref)
	testing.expect_value(t, expect_type_kind(t, &unit, demo_ref.base, .Class).name, "lcl_demo")
	if_ref := expect_type_kind(t, &unit, lr_if.type_id, .Ref)
	testing.expect_value(t, expect_type_kind(t, &unit, if_ref.base, .Interface).name, "lif_demo")
}

@(test)
recursive_type_alias_resolution_is_bounded :: proc(t: ^testing.T) {
	source := `TYPES ty_self TYPE ty_self.`
	unit := collect_test_unit(t, "file:///recursive_type_alias.abap", source)

	ty_self := analyze.find_symbol(&unit, "ty_self", .Type_Def)
	testing.expect(t, ty_self != nil)
	if ty_self == nil {
		return
	}
	ty_self.type_id = analyze.UNKNOWN_TYPE_ID
	resolved := analyze.type_id_from_symbol(&unit, ty_self.id)
	testing.expect(t, analyze.type_id_is_known(resolved))
}

@(test)
structured_type_components_named_begin_and_end_resolve :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_code_range,
  begin TYPE i,
  end TYPE i,
END OF ty_code_range.
TYPES ty_code_ranges TYPE SORTED TABLE OF ty_code_range WITH UNIQUE KEY begin.
DATA ls_range TYPE ty_code_range.
DATA lt_ranges TYPE ty_code_ranges.`
	unit := collect_test_unit(t, "file:///keyword_component_type_refs.abap", source)

	testing.expect(t, analyze.find_symbol(&unit, "ty_code_range", .Type_Def) != nil)
	testing.expect(t, analyze.find_symbol(&unit, "ty_code_ranges", .Type_Def) != nil)
	expect_structure_fields(t, &unit, "ty_code_range", "begin", "end")
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
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
method_local_include_type_expands_private_class_type :: proc(t: ^testing.T) {
	source := `
CLASS lcl_version DEFINITION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_requirement_status,
             met TYPE abap_bool,
           END OF ty_requirement_status.
    CLASS-METHODS show.
ENDCLASS.

CLASS lcl_version IMPLEMENTATION.
  METHOD show.
    TYPES: BEGIN OF ty_color_line,
             color TYPE string.
             INCLUDE TYPE ty_requirement_status.
    TYPES: END OF ty_color_line.

    FIELD-SYMBOLS <ls_line> TYPE ty_color_line.
    IF <ls_line>-met = abap_false.
      <ls_line>-color = 'x'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///method_local_include_private_type.abap", source)

	color_line := analyze.find_symbol(&unit, "ty_color_line", .Type_Def)
	testing.expect(t, color_line != nil)
	st := analyze.structure(&unit, color_line.structure)
	fields := [?]string{"color", "met"}
	testing.expect(t, field_names_match(st, fields[:]))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
structured_component_named_include_resolves_as_field :: proc(t: ^testing.T) {
	source := `
TYPES: BEGIN OF ty_src_info,
         program TYPE progname,
         include TYPE progname,
         line    TYPE i,
       END OF ty_src_info.
DATA ms_src_info TYPE ty_src_info.
DATA include_name TYPE progname.
include_name = ms_src_info-include.
`
	unit := collect_test_unit(t, "file:///component_include.abap", source)

	src_info := analyze.find_symbol(&unit, "ty_src_info", .Type_Def)
	testing.expect(t, src_info != nil)
	st := analyze.structure(&unit, src_info.structure)
	fields := [?]string{"program", "include", "line"}
	testing.expect(t, field_names_match(st, fields[:]))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
like_line_of_table_with_include_component_resolves_field :: proc(t: ^testing.T) {
	source := `
TYPES: BEGIN OF abap_callstack_line,
         mainprogram TYPE progname,
         include     TYPE include,
         line        TYPE i,
       END OF abap_callstack_line.
TYPES abap_callstack TYPE STANDARD TABLE OF abap_callstack_line WITH DEFAULT KEY.
DATA mt_callstack TYPE abap_callstack.
FIELD-SYMBOLS <ls_callstack> LIKE LINE OF mt_callstack.
DATA include_name TYPE progname.
include_name = <ls_callstack>-include.
`
	unit := collect_test_unit(t, "file:///callstack_include.abap", source)

	line := analyze.find_symbol(&unit, "abap_callstack_line", .Type_Def)
	testing.expect(t, line != nil)
	st := analyze.structure(&unit, line.structure)
	fields := [?]string{"mainprogram", "include", "line"}
	testing.expect(t, field_names_match(st, fields[:]))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
structured_include_type_does_not_add_include_field :: proc(t: ^testing.T) {
	source := `
TYPES: BEGIN OF ty_base,
         a TYPE i,
       END OF ty_base.
TYPES: BEGIN OF ty_wrap,
         INCLUDE TYPE ty_base,
       END OF ty_wrap.
DATA ls_wrap TYPE ty_wrap.
ls_wrap-include = 1.
`
	unit := collect_test_unit(t, "file:///real_structured_include.abap", source)

	wrap := analyze.find_symbol(&unit, "ty_wrap", .Type_Def)
	testing.expect(t, wrap != nil)
	st := analyze.structure(&unit, wrap.structure)
	fields := [?]string{"a"}
	testing.expect(t, field_names_match(st, fields[:]))
	testing.expect(t, has_diagnostic(&unit, .Unknown_Field))
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
structured_include_from_interface_component_type_expands_fields :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///interface_component_include.abap",
		source = `
INTERFACE lif_asset_manager.
  TYPES: BEGIN OF ty_web_asset,
           url          TYPE string,
           content      TYPE xstring,
           is_cacheable TYPE abap_bool,
         END OF ty_web_asset.
ENDINTERFACE.

CLASS lcl_asset_manager DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_asset_manager.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_asset_entry.
        INCLUDE TYPE lif_asset_manager~ty_web_asset.
    TYPES: mime_name TYPE string,
      END OF ty_asset_entry.
    TYPES ty_asset_register TYPE STANDARD TABLE OF ty_asset_entry WITH KEY url.
    DATA mt_asset_register TYPE ty_asset_register.
    METHODS load_asset
      IMPORTING is_asset_entry TYPE ty_asset_entry
                iv_url         TYPE string.
ENDCLASS.

CLASS lcl_asset_manager IMPLEMENTATION.
  METHOD load_asset.
    FIELD-SYMBOLS <ls_asset> LIKE LINE OF mt_asset_register.
    DATA lv_message TYPE string.
    lv_message = |failed: { is_asset_entry-url }|.
    READ TABLE mt_asset_register WITH KEY url = iv_url ASSIGNING <ls_asset>.
  ENDMETHOD.
ENDCLASS.
`,
	}

	project := analyze_project_test(t, 0, target, nil)
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	if root != nil {
		asset_entry := analyze.find_symbol(root, "ty_asset_entry", .Type_Def)
		testing.expect(t, asset_entry != nil)
		testing.expect(t, asset_entry.structure != analyze.INVALID_STRUCTURE_ID)
		st := analyze.structure(root, asset_entry.structure)
		fields := [?]string{"url", "content", "is_cacheable", "mime_name"}
		testing.expect(t, field_names_match(st, fields[:]))
		testing.expect(t, !has_diagnostic(root, .Unknown_Field))
	}
}

@(test)
inherited_structured_attribute_resolves_in_method_body :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///inherited_structured_attribute.abap",
		source = `
INTERFACE lif_defs.
  TYPES: BEGIN OF ty_sig,
           obj_name TYPE string,
         END OF ty_sig.
  TYPES: BEGIN OF ty_item.
      INCLUDE TYPE ty_sig.
  TYPES: END OF ty_item.
ENDINTERFACE.

CLASS lcl_parent DEFINITION.
  PROTECTED SECTION.
    DATA ms_item TYPE lif_defs=>ty_item.
ENDCLASS.

CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_child IMPLEMENTATION.
  METHOD run.
    DATA ls_item LIKE ms_item.
    DATA lv_name TYPE string.
    ls_item-obj_name = ms_item-obj_name.
    me->ms_item-obj_name = lv_name.
    lv_name = ms_item-obj_name.
  ENDMETHOD.
ENDCLASS.
`,
	}

	project := analyze_project_test(t, 0, target, nil)
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, !has_diagnostic(root, .Unknown_Field))
}

@(test)
private_structured_class_type_resolves_in_method_implementation :: proc(t: ^testing.T) {
	source := `
CLASS lcl_error DEFINITION.
  PUBLIC SECTION.
    DATA a1 TYPE string.
    METHODS run.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_message_parts,
             a1 LIKE a1,
             a2 LIKE a1,
             a3 LIKE a1,
             a4 LIKE a1,
           END OF ty_message_parts.
ENDCLASS.

CLASS lcl_error IMPLEMENTATION.
  METHOD run.
    DATA ls_msg TYPE ty_message_parts.
    DATA lv_text TYPE string.
    ls_msg = lv_text.
    lv_text = ls_msg-a1.
    lv_text = ls_msg-a2.
    lv_text = ls_msg-a3.
    lv_text = ls_msg-a4.
  ENDMETHOD.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///private_structured_class_type.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
inherited_structured_class_type_resolves_in_redefinition_body :: proc(t: ^testing.T) {
	source := `
CLASS lcl_parent DEFINITION.
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_row,
             id TYPE string,
           END OF ty_row.
    TYPES ty_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
    METHODS prune CHANGING ct_rows TYPE ty_rows.
ENDCLASS.

CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
  PROTECTED SECTION.
    CONSTANTS: BEGIN OF c_token,
                 id TYPE string VALUE 'A',
               END OF c_token.
    METHODS prune REDEFINITION.
ENDCLASS.

CLASS lcl_child IMPLEMENTATION.
  METHOD prune.
    FIELD-SYMBOLS <row> TYPE ty_row.
    LOOP AT ct_rows ASSIGNING <row>.
      DELETE ct_rows WHERE id = <row>-id AND id = c_token-id.
      <row>-id = 'A'.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///inherited_redefinition_type.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Open_Sql_Source))
}

@(test)
internal_delete_where_validates_row_fields :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_row,
           id TYPE string,
         END OF ty_row.
  DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
  DATA lv_id TYPE string.
  DELETE lt_rows WHERE missing = lv_id.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///internal_delete_where.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Open_Sql_Source))
}

@(test)
internal_table_where_keeps_values_out_of_row_field_diagnostics :: proc(t: ^testing.T) {
	source := `
FORM run USING iv_id TYPE string.
  TYPES: BEGIN OF ty_row,
           id TYPE string,
         END OF ty_row.
  DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
  CONSTANTS c_id TYPE string VALUE 'A'.
  DELETE lt_rows WHERE id = c_id.
  DELETE lt_rows WHERE id = iv_id.
  DELETE lt_rows WHERE table_line IS INITIAL.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///internal_table_where_values.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
table_expr_key_names_are_row_fields :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_status,
           exist_attp TYPE abap_bool,
         END OF ty_status.
  DATA lt_status TYPE STANDARD TABLE OF ty_status WITH EMPTY KEY.

  IF line_exists( lt_status[ exist_attp = abap_undefined ] ).
  ENDIF.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///table_expr_key_fields.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, !has_reference(&unit, "exist_attp", .Value, .Identifier))
}

@(test)
constructor_for_where_names_are_row_fields :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_status,
           exist_attp TYPE abap_bool,
           epc_id_uri TYPE string,
         END OF ty_status.
  DATA lt_status TYPE STANDARD TABLE OF ty_status WITH EMPTY KEY.
  DATA lt_serno_all TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  lt_serno_all = VALUE #( FOR lv_stat IN lt_status WHERE ( exist_attp = abap_undefined ) ( lv_stat-epc_id_uri ) ).
ENDFORM.
`
	unit := collect_test_unit(t, "file:///constructor_for_where_fields.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, !has_reference(&unit, "exist_attp", .Value, .Identifier))
}

@(test)
loop_where_table_body_uses_row_fields :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_row,
           type TYPE string,
         END OF ty_row.
  DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
  LOOP AT lt_rows[] ASSIGNING FIELD-SYMBOL(<row>) WHERE type CS 'A'.
  ENDLOOP.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///loop_where_table_body.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
loop_where_table_body_suppresses_row_fields_for_unknown_shape :: proc(t: ^testing.T) {
	source := `
FORM run.
  FIELD-SYMBOLS <choice>.
  LOOP AT <choice>-destination[] ASSIGNING FIELD-SYMBOL(<destination>) WHERE type CS 'A'.
  ENDLOOP.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///loop_where_unknown_table_body.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
selection_ranges_collect_range_structure :: proc(t: ^testing.T) {
	source := `
TYPES zattp_gln TYPE string.
DATA lv_rogln TYPE zattp_gln.
RANGES r_rogln FOR lv_rogln.
SELECT-OPTIONS s_rogln FOR lv_rogln.
LOOP AT s_rogln INTO DATA(ls_rogln).
  lv_rogln = ls_rogln-low.
ENDLOOP.
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
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
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
	alias_info := analyze.entity_decl_info(&unit, unit.member_aliases[0].symbol)
	testing.expect(t, alias_info != nil)
	testing.expect_value(t, alias_info.owner, child.id)
	testing.expect_value(t, alias_info.visibility, analyze.Visibility.Public)
	testing.expect_value(t, alias_info.alias_target_interface_name, "lif_demo")

	attr := class_member_named(&unit, child.id, "mv_flag", .Attribute)
	static_attr := class_member_named(&unit, child.id, "gv_count", .Attribute)
	event := class_member_named(&unit, child.id, "changed", .Event)
	method := class_member_named(&unit, child.id, "run", .Method)
	testing.expect(t, attr != nil)
	attr_info := analyze.entity_decl_info(&unit, attr.id)
	testing.expect(t, attr_info != nil)
	testing.expect_value(t, attr_info.owner, child.id)
	testing.expect_value(t, attr_info.member_kind, analyze.Class_Member_Kind.Attribute)
	testing.expect(t, static_attr != nil)
	static_attr_info := analyze.entity_decl_info(&unit, static_attr.id)
	testing.expect(t, static_attr_info != nil && .Is_Static in static_attr_info.flags)
	testing.expect(t, event != nil)
	event_info := analyze.entity_decl_info(&unit, event.id)
	testing.expect(t, event_info != nil)
	testing.expect_value(t, len(event_info.signature_parameters), 1)
	testing.expect_value(t, event_info.signature_parameters[0].name, "ev_flag")
	testing.expect_value(t, event_info.signature_parameters[0].section, analyze.Decl_Parameter_Section.Method_Exporting)
	testing.expect_value(t, event_info.signature_parameters[0].passing, analyze.Decl_Parameter_Passing.Value)
	testing.expect(t, method != nil)
	method_info := analyze.entity_decl_info(&unit, method.id)
	testing.expect(t, method_info != nil)
	testing.expect_value(t, method_info.owner, child.id)
	testing.expect_value(t, method_info.member_kind, analyze.Class_Member_Kind.Method)
	testing.expect(t, method_info.signature_scope != analyze.INVALID_SCOPE_ID)
	testing.expect(t, .Has_Implementation in method_info.flags)
	testing.expect_value(t, len(method_info.signature_parameters), 2)
	testing.expect_value(t, method_info.signature_parameters[0].name, "iv_value")
	testing.expect_value(t, method_info.signature_parameters[0].section, analyze.Decl_Parameter_Section.Method_Importing)
	testing.expect_value(t, method_info.signature_parameters[0].passing, analyze.Decl_Parameter_Passing.Direct)
	param_info := analyze.entity_decl_info(&unit, method_info.signature_parameters[0].symbol)
	testing.expect(t, param_info != nil)
	testing.expect_value(t, param_info.owner, method.id)
	testing.expect_value(t, param_info.parameter_section, analyze.Decl_Parameter_Section.Method_Importing)
	testing.expect_value(t, method_info.signature_parameters[1].section, analyze.Decl_Parameter_Section.Method_Returning)
	testing.expect_value(t, method_info.signature_parameters[1].name, "rv_text")
	testing.expect_value(t, method_info.signature_parameters[1].passing, analyze.Decl_Parameter_Passing.Value)
}

@(test)
event_handler_importing_parameter_uses_event_type :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///event_handler_parameter_type.abap",
		`
CLASS lcl_source DEFINITION.
  PUBLIC SECTION.
    DATA object_type TYPE string.
    EVENTS saved EXPORTING VALUE(ex_object) TYPE REF TO lcl_source.
ENDCLASS.

CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_saved FOR EVENT saved OF lcl_source IMPORTING ex_object.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD on_saved.
    DATA lv_type TYPE string.
    lv_type = ex_object->object_type.
  ENDMETHOD.
ENDCLASS.
`,
	)

	handler_class := analyze.find_symbol(&unit, "lcl_handler", .Class)
	handler_member: ^analyze.Symbol_Data
	if handler_class != nil {
		handler_member = class_member_named(&unit, handler_class.id, "on_saved", .Method)
	}
	handler_info := analyze.entity_decl_info(&unit, handler_member.id) if handler_member != nil else nil
	testing.expect(t, handler_info != nil)
	testing.expect(t, handler_info != nil && len(handler_info.signature_parameters) == 1)
	if handler_info != nil && len(handler_info.signature_parameters) == 1 {
		param := handler_info.signature_parameters[0]
		param_symbol := analyze.symbol(&unit, param.symbol)
		testing.expect(t, .Has_Declared_Type in param.flags)
		testing.expect_value(t, param.declared_type.base_name, "lcl_source")
		testing.expect(t, param.declared_type.is_ref)
		testing.expect(t, .Has_Event_Derived_Type in param.flags)
		testing.expect(t, param_symbol != nil && param_symbol.has_declared_type)
		testing.expect(t, param_symbol != nil && param_symbol.declared_type.is_ref)
	}
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
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
	method_info := analyze.entity_decl_info(&unit, method.id)
	testing.expect(t, method_info != nil)
	testing.expect_value(t, len(method_info.signature_parameters), 2)
	date := method_info.signature_parameters[0]
	testing.expect(t, .Has_Declared_Type in date.flags)
	testing.expect_value(t, date.declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, date.declared_type.base_name, "sy")
	testing.expect_value(t, date.declared_type.field_path[0], "datum")
	phase := method_info.signature_parameters[1]
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
	pub_info := analyze.entity_decl_info(&unit, pub.id)
	prot_info := analyze.entity_decl_info(&unit, prot.id)
	priv_info := analyze.entity_decl_info(&unit, priv.id)
	if_pub_info := analyze.entity_decl_info(&unit, if_pub.id)
	testing.expect_value(t, pub_info.visibility, analyze.Visibility.Public)
	testing.expect_value(t, prot_info.visibility, analyze.Visibility.Protected)
	testing.expect_value(t, priv_info.visibility, analyze.Visibility.Private)
	testing.expect_value(t, if_pub_info.visibility, analyze.Visibility.Public)
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
qualified_method_redefinitions_keep_qualified_symbol_names :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS lif_demo~create REDEFINITION.
    METHODS lif_demo~delete REDEFINITION.
ENDCLASS.`
	unit := collect_test_unit(t, "file:///qualified_redefinitions.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Duplicate_Declaration))
	testing.expect(t, has_symbol(&unit, .Method, "lif_demo~create"))
	testing.expect(t, has_symbol(&unit, .Method, "lif_demo~delete"))
}

@(test)
qualified_interface_redefinition_can_share_local_method_name :: proc(t: ^testing.T) {
	source := `INTERFACE lif_object.
  METHODS copy IMPORTING iv_value TYPE i.
  METHODS rename.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_object.
    CLASS-METHODS copy.
    METHODS lif_object~copy REDEFINITION.
    METHODS lif_object~rename REDEFINITION.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD copy.
  ENDMETHOD.
  METHOD lif_object~copy.
    DATA lv_value TYPE i.
    lv_value = iv_value.
  ENDMETHOD.
  METHOD lif_object~rename.
  ENDMETHOD.
ENDCLASS.`
	unit := collect_test_unit(t, "file:///qualified_redefinition_local_name_collision.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Duplicate_Declaration))
	testing.expect(t, !has_diagnostic(&unit, .Missing_Method_Implementation))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))

	class := analyze.find_symbol(&unit, "lcl", .Class)
	testing.expect(t, class != nil)
	local_copy := class_member_named(&unit, class.id, "copy", .Method)
	interface_copy := class_member_named(&unit, class.id, "lif_object~copy", .Method)
	interface_rename := class_member_named(&unit, class.id, "lif_object~rename", .Method)
	testing.expect(t, local_copy != nil)
	testing.expect(t, interface_copy != nil)
	testing.expect(t, interface_rename != nil)
	local_copy_info := analyze.entity_decl_info(&unit, local_copy.id)
	interface_copy_info := analyze.entity_decl_info(&unit, interface_copy.id)
	interface_rename_info := analyze.entity_decl_info(&unit, interface_rename.id)
	testing.expect(t, local_copy_info != nil && .Is_Static in local_copy_info.flags)
	testing.expect(t, interface_copy_info != nil && !(.Is_Static in interface_copy_info.flags))
	testing.expect(t, local_copy_info != nil && .Has_Implementation in local_copy_info.flags)
	testing.expect(t, interface_copy_info != nil && .Has_Implementation in interface_copy_info.flags)
	testing.expect(t, interface_rename_info != nil && .Has_Implementation in interface_rename_info.flags)
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
	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect_value(t, len(unit.constructor_for_bindings), 2)
}

@(test)
constructor_let_binding_infers_optional_table_expr_row :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_event,
         objid TYPE string,
         status_rep_evt TYPE i,
       END OF ty_event.
TYPES ty_events TYPE STANDARD TABLE OF ty_event WITH EMPTY KEY.
DATA lt_events TYPE ty_events.

FORM run.
  DATA(lv_text) = COND string(
    LET ls_evt_obj = VALUE #( lt_events[ objid = '1' ] OPTIONAL )
    IN WHEN ls_evt_obj-status_rep_evt = 1 THEN ls_evt_obj-objid ELSE '' ).
ENDFORM.`
	unit := collect_test_unit(t, "file:///constructor_let_optional_table_expr.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
inline_data_statement_infers_optional_table_expr_row :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_item,
         objid TYPE string,
         gtin TYPE string,
         uom TYPE string,
       END OF ty_item.
TYPES ty_items TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.
DATA it_obj_itm TYPE ty_items.
DATA is_obj_ids TYPE ty_item.

FORM run.
  DATA(ls_obj_itm) = VALUE #( it_obj_itm[ objid = is_obj_ids-objid ] OPTIONAL ).
  IF ls_obj_itm-uom IS INITIAL.
    ls_obj_itm-uom = ls_obj_itm-gtin.
  ENDIF.
ENDFORM.`
	unit := collect_test_unit(t, "file:///inline_data_optional_table_expr.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
inline_data_statement_from_ddic_cache_row_allows_incomplete_append_fields :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///inline_data_ddic_cache_row.abap",
		source = `DATA it_obj_itm TYPE /sttp/t_dm_obj_itm.
DATA is_obj_ids TYPE /sttp/dm_obj_itm.

FORM run.
  DATA(ls_obj_itm) = VALUE #( it_obj_itm[ objid = is_obj_ids-objid ] OPTIONAL ).
  IF ls_obj_itm-uom IS INITIAL.
    ls_obj_itm-uom = ls_obj_itm-gtin.
  ENDIF.
ENDFORM.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table-type/sttp_t_dm_obj_itm.abap",
			source = "TYPES /sttp/t_dm_obj_itm TYPE STANDARD TABLE OF /sttp/dm_obj_itm WITH DEFAULT KEY.",
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/ddic-table/sttp_dm_obj_itm.abap",
			source = `TYPES: BEGIN OF /sttp/dm_obj_itm,
         objid TYPE string,
         gtin TYPE string,
       END OF /sttp/dm_obj_itm.`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))
}

@(test)
open_sql_from_adt_ddic_table_allows_incomplete_append_fields :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///open_sql_adt_ddic_append_fields.abap",
		source = `DATA lv_evtid TYPE string.
SELECT SINGLE rep_evtid
  FROM /sttp/rep_evt
  INTO @lv_evtid
  WHERE recall_status = space
    AND response_code <> space.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-adt:/sap/bc/adt/vit/wb/object_type/tabldt/object_name/%2fSTTP%2fREP_EVT.xml",
			source = `TYPES: BEGIN OF /sttp/rep_evt,
         rep_evtid TYPE string,
       END OF /sttp/rep_evt.`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))
}

@(test)
constructor_let_binding_unknown_table_expr_row_shape_is_not_invalid :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS get RETURNING VALUE(rt_events) TYPE zmissing_tt.
    METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA(lt_events) = get( ).
    DATA(lv_text) = COND string(
      LET ls_evt_obj = VALUE #( lt_events[ objid = '1' ] OPTIONAL )
      IN WHEN ls_evt_obj-status_rep_evt = 1 THEN ls_evt_obj-objid ELSE '' ).
  ENDMETHOD.
ENDCLASS.`
	unit := collect_test_unit(t, "file:///constructor_let_unknown_table_expr.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
method_return_assignment_richer_than_declared_table_shape :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_declared,
         objid TYPE string,
       END OF ty_declared.
TYPES tt_declared TYPE STANDARD TABLE OF ty_declared WITH EMPTY KEY.
TYPES: BEGIN OF ty_actual,
         objid TYPE string,
         status_rep_evt TYPE i,
       END OF ty_actual.
TYPES tt_actual TYPE STANDARD TABLE OF ty_actual WITH EMPTY KEY.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS get RETURNING VALUE(rt_events) TYPE tt_declared.
    METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD get.
    DATA lt_events TYPE tt_actual.
    rt_events = lt_events.
  ENDMETHOD.

  METHOD run.
    DATA(lt_events) = get( ).
    DATA(lv_text) = COND string(
      LET ls_evt_obj = VALUE #( lt_events[ objid = '1' ] OPTIONAL )
      IN WHEN ls_evt_obj-status_rep_evt = 1 THEN ls_evt_obj-objid ELSE '' ).
ENDMETHOD.
ENDCLASS.`
	unit := collect_test_unit(t, "file:///method_return_assignment_richer_table.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
method_return_assignment_infers_unresolved_declared_table_shape :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS get RETURNING VALUE(rt_events) TYPE zmissing_tt.
    METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD get.
    TYPES: BEGIN OF lty_evt,
             objid TYPE string,
             status_rep_evt TYPE i,
           END OF lty_evt.
    DATA lt_events TYPE TABLE OF lty_evt.
    IF lt_events IS NOT INITIAL.
      rt_events = lt_events.
    ENDIF.
  ENDMETHOD.

  METHOD run.
    DATA(lt_events) = get( ).
    DATA(lv_text) = COND string(
      LET ls_evt_obj = VALUE #( lt_events[ objid = '1' ] OPTIONAL )
      IN WHEN ls_evt_obj-status_rep_evt = 1 THEN ls_evt_obj-objid ELSE '' ).
  ENDMETHOD.
ENDCLASS.`
	unit := collect_test_unit(t, "file:///method_return_assignment_unresolved_table.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
collects_multiple_method_parameters_from_oop_ast :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING it_source TYPE STANDARD TABLE it_any TYPE ANY TABLE it_index TYPE INDEX TABLE iv_state TYPE i OPTIONAL iv_text TYPE string
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
	testing.expect(t, !has_diagnostic(&unit, .Invalid_Parameter_Type))
	testing.expect(t, !has_diagnostic(&unit, .Invalid_Generic_Table_Type))
	class := analyze.find_symbol(&unit, "lcl", .Class)
	testing.expect(t, class != nil)
	method := class_member_named(&unit, class.id, "run", .Method)
	testing.expect(t, method != nil)
	method_info := analyze.entity_decl_info(&unit, method.id)
	testing.expect(t, method_info != nil)
	testing.expect_value(t, len(method_info.signature_parameters), 6)
	testing.expect_value(t, method_info.signature_parameters[0].name, "it_source")
	testing.expect_value(t, method_info.signature_parameters[1].name, "it_any")
	testing.expect_value(t, method_info.signature_parameters[2].name, "it_index")
	testing.expect_value(t, method_info.signature_parameters[3].name, "iv_state")
	testing.expect(t, .Is_Optional in method_info.signature_parameters[3].flags)
	testing.expect_value(t, method_info.signature_parameters[4].name, "iv_text")
	testing.expect_value(t, method_info.signature_parameters[5].section, analyze.Decl_Parameter_Section.Method_Returning)
	testing.expect_value(t, method_info.signature_parameters[5].name, "rv_ok")
}

@(test)
bare_table_method_parameters_are_generic :: proc(t: ^testing.T) {
	source := `INTERFACE if_swf_exp_expression.
  METHODS value_to_html
    EXPORTING
      !HTML_TABLE TYPE TABLE
      !EXCEPTION_RETURN TYPE REF TO cx_swf_exp_expression.
ENDINTERFACE.`
	unit := collect_test_unit(t, "file:///bare_table_method_parameter.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Invalid_Parameter_Type))
	html_table := analyze.find_symbol(&unit, "html_table", .Parameter)
	testing.expect(t, html_table != nil)
	if html_table != nil {
		testing.expect_value(t, html_table.type_clause_form, ast.Data_Type_Form.Table)
		testing.expect(t, !html_table.type_clause_table_has_of)
		testing.expect(t, !html_table.has_declared_type)
	}
}

@(test)
inline_table_of_method_parameters_are_invalid :: proc(t: ^testing.T) {
	source := `INTERFACE lif.
  METHODS run EXPORTING !ROWS TYPE TABLE OF string.
ENDINTERFACE.`
	unit := collect_test_unit(t, "file:///inline_table_method_parameter.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Invalid_Parameter_Type))
}

@(test)
generic_table_categories_are_context_checked :: proc(t: ^testing.T) {
	valid := `TYPES ty_any_rows TYPE ANY TABLE OF string.
FIELD-SYMBOLS <typed> TYPE ty_any_rows.
FIELD-SYMBOLS <any> TYPE ANY TABLE.
FIELD-SYMBOLS <index> TYPE INDEX TABLE.
FORM demo USING it_typed TYPE ty_any_rows it_any TYPE ANY TABLE it_any_rows TYPE ANY TABLE OF string CHANGING ct_index TYPE INDEX TABLE.
ENDFORM.`
	valid_unit := collect_test_unit(t, "file:///generic_tables_valid.abap", valid)

	testing.expect(t, !has_diagnostic(&valid_unit, .Invalid_Generic_Table_Type))
	testing.expect(t, !has_diagnostic(&valid_unit, .Invalid_Parameter_Type))
	testing.expect(t, !has_diagnostic(&valid_unit, .Unresolved_Reference))

	invalid := `DATA lt_index TYPE INDEX TABLE.
DATA lt_index_rows TYPE INDEX TABLE OF string.
TYPES ty_any TYPE ANY TABLE.
TYPES ty_any_rows TYPE ANY TABLE OF string.
DATA lt_any_rows TYPE ty_any_rows.`
	invalid_unit := collect_test_unit(t, "file:///generic_tables_invalid.abap", invalid)

	testing.expect(t, has_diagnostic(&invalid_unit, .Invalid_Generic_Table_Type))
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
	method_info := analyze.entity_decl_info(&unit, method.id)
	testing.expect(t, method_info != nil)
	testing.expect_value(t, len(method_info.signature_exceptions), 1)
	testing.expect_value(t, method_info.signature_exceptions[0].name, "failed")
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

	form := analyze.find_symbol(&unit, "run", .Form)
	testing.expect(t, form != nil)
	form_info := analyze.entity_decl_info(&unit, form.id) if form != nil else nil
	testing.expect(t, form_info != nil)
	testing.expect(t, form_info.body_scope != analyze.INVALID_SCOPE_ID)
	testing.expect_value(t, form_info.signature, "FORM run TABLES !ct_rows STRUCTURE mara USING VALUE(iv_text) TYPE string REFERENCE(iv_ref) LIKE sy-uname CHANGING cv_count TYPE i")
	testing.expect_value(t, len(form_info.signature_parameters), 4)
	ct_rows := unit.symbols[analyze.symbol_id_index(form_info.signature_parameters[0].symbol)]
	iv_text := unit.symbols[analyze.symbol_id_index(form_info.signature_parameters[1].symbol)]
	iv_ref := unit.symbols[analyze.symbol_id_index(form_info.signature_parameters[2].symbol)]
	form_param_info := analyze.entity_decl_info(&unit, form_info.signature_parameters[0].symbol)
	testing.expect(t, form_param_info != nil)
	testing.expect_value(t, form_param_info.owner, form.id)
	testing.expect_value(t, form_param_info.parameter_section, analyze.Decl_Parameter_Section.Form_Tables)
	testing.expect_value(t, form_info.signature_parameters[0].section, analyze.Decl_Parameter_Section.Form_Tables)
	testing.expect_value(t, ct_rows.name, "ct_rows")
	testing.expect_value(t, ct_rows.declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, form_info.signature_parameters[1].section, analyze.Decl_Parameter_Section.Form_Using)
	testing.expect_value(t, form_info.signature_parameters[1].passing, analyze.Decl_Parameter_Passing.Value)
	testing.expect_value(t, iv_text.name, "iv_text")
	testing.expect_value(t, form_info.signature_parameters[2].passing, analyze.Decl_Parameter_Passing.Reference)
	testing.expect_value(t, iv_ref.declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, iv_ref.declared_type.base_name, "sy")
	testing.expect_value(t, iv_ref.declared_type.field_path[0], "uname")
	testing.expect_value(t, form_info.signature_parameters[3].section, analyze.Decl_Parameter_Section.Form_Changing)

	fm := analyze.find_symbol(&unit, "z_demo", .Module)
	testing.expect(t, fm != nil)
	fm_info := analyze.entity_decl_info(&unit, fm.id) if fm != nil else nil
	testing.expect(t, fm_info != nil)
	testing.expect(t, fm_info.body_scope != analyze.INVALID_SCOPE_ID)
	testing.expect_value(t, len(fm_info.signature_parameters), 5)
	testing.expect_value(t, fm_info.signature_parameters[0].name, "iv_value")
	fm_param_info := analyze.entity_decl_info(&unit, fm_info.signature_parameters[0].symbol)
	testing.expect(t, fm_param_info != nil)
	testing.expect_value(t, fm_param_info.owner, fm.id)
	testing.expect_value(t, fm_param_info.parameter_section, analyze.Decl_Parameter_Section.Function_Importing)
	testing.expect_value(t, fm_info.signature_parameters[0].section, analyze.Decl_Parameter_Section.Function_Importing)
	testing.expect_value(t, fm_info.signature_parameters[0].passing, analyze.Decl_Parameter_Passing.Value)
	testing.expect(t, .Is_Optional in fm_info.signature_parameters[0].flags)
	testing.expect_value(t, fm_info.signature_parameters[1].name, "iv_text")
	testing.expect_value(t, fm_info.signature_parameters[1].passing, analyze.Decl_Parameter_Passing.Direct)
	testing.expect(t, .Has_Default_Value in fm_info.signature_parameters[1].flags)
	testing.expect_value(t, fm_info.signature_parameters[2].section, analyze.Decl_Parameter_Section.Function_Exporting)
	testing.expect_value(t, fm_info.signature_parameters[2].declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, fm_info.signature_parameters[2].declared_type.base_name, "sy")
	testing.expect_value(t, fm_info.signature_parameters[2].declared_type.field_path[0], "uname")
	testing.expect_value(t, fm_info.signature_parameters[3].section, analyze.Decl_Parameter_Section.Function_Changing)
	testing.expect_value(t, fm_info.signature_parameters[3].passing, analyze.Decl_Parameter_Passing.Reference)
	testing.expect_value(t, fm_info.signature_parameters[3].declared_type.base_name, "object")
	testing.expect_value(t, fm_info.signature_parameters[4].section, analyze.Decl_Parameter_Section.Function_Tables)
	testing.expect_value(t, fm_info.signature_parameters[4].declared_type.namespace, analyze.Namespace.Value)
	testing.expect_value(t, fm_info.signature_parameters[4].declared_type.base_name, "bapiret2")
	testing.expect_value(t, len(fm_info.signature_exceptions), 2)
	testing.expect_value(t, fm_info.signature_exceptions[0].name, "failed")
	testing.expect_value(t, fm_info.signature_exceptions[1].name, "not_found")
	testing.expect(t, has_reference(&unit, "string", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "sy", .Value, .Type_Ref))
	testing.expect(t, has_reference(&unit, "bapiret2", .Value, .Type_Ref))
}

@(test)
semantic_typecheck_reports_assignment_conversion_failures :: proc(t: ^testing.T) {
	source := `REPORT z_tc_assign_ref.
TYPES: BEGIN OF e070,
         trkorr TYPE c,
         as4date TYPE d,
       END OF e070.
DATA lv_date TYPE d.
DATA lv_time TYPE t.
DATA lr_data TYPE REF TO data.
DATA lr_e070 TYPE REF TO e070.
DATA lo_obj  TYPE REF TO object.
DATA ls_e070 TYPE e070.

lv_date = lv_time.
lr_data = lr_e070.
ls_e070 = lo_obj.`
	unit := collect_test_unit(t, "file:///tc_assign_ref.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Assignment_Type), 1)
	message, ok := diagnostic_message_for_kind(&unit, .Incompatible_Assignment_Type)
	testing.expect(t, ok)
	testing.expect_value(
		t,
		message,
		"The type of 'lv_time' cannot be converted to the type of 'lv_date' (current type 't', expected type 'd')",
	)
}

@(test)
semantic_typecheck_reports_date_time_scalar_assignment_failure :: proc(t: ^testing.T) {
	source := `REPORT z_tc_dt_scalar_bad.
DATA lv_date TYPE d.
DATA lv_time TYPE t.

lv_date = lv_time.`
	unit := collect_test_unit(t, "file:///tc_dt_scalar_bad.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Assignment_Type), 1)
}

@(test)
semantic_typecheck_accepts_exact_and_widening_ref_assignments :: proc(t: ^testing.T) {
	source := `REPORT z_tc_ref_ok.
INTERFACE lif_named.
ENDINTERFACE.
CLASS lcl_base DEFINITION.
ENDCLASS.
CLASS lcl_child DEFINITION INHERITING FROM lcl_base.
  PUBLIC SECTION.
    INTERFACES lif_named.
ENDCLASS.
DATA lo_child TYPE REF TO lcl_child.
DATA lo_same TYPE REF TO lcl_child.
DATA lo_base TYPE REF TO lcl_base.
DATA li_named TYPE REF TO lif_named.

lo_same = lo_child.
lo_base = lo_child.
li_named = lo_child.`
	unit := collect_test_unit(t, "file:///tc_ref_ok.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Incompatible_Assignment_Type))
}

@(test)
semantic_typecheck_handles_data_and_object_generic_refs :: proc(t: ^testing.T) {
	source := `REPORT z_tc_ref_generic.
CLASS lcl_demo DEFINITION.
ENDCLASS.
DATA lr_i TYPE REF TO i.
DATA lr_data TYPE REF TO data.
DATA lo_demo TYPE REF TO lcl_demo.
DATA lo_object TYPE REF TO object.
DATA lo_down TYPE REF TO lcl_demo.

lr_data = lr_i.
lo_object = lo_demo.
lo_down ?= lo_object.`
	unit := collect_test_unit(t, "file:///tc_ref_generic.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Incompatible_Assignment_Type))
}

@(test)
semantic_typecheck_reports_generic_data_ref_narrowing_failure :: proc(t: ^testing.T) {
	source := `REPORT z_tc_ref_data_bad.
DATA lr_i TYPE REF TO i.
DATA lr_data TYPE REF TO data.

lr_i = lr_data.`
	unit := collect_test_unit(t, "file:///tc_ref_data_bad.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Assignment_Type), 1)
}

@(test)
semantic_typecheck_reports_impossible_ref_assignment :: proc(t: ^testing.T) {
	source := `REPORT z_tc_ref_bad.
CLASS lcl_a DEFINITION.
ENDCLASS.
CLASS lcl_b DEFINITION.
ENDCLASS.
DATA lo_a TYPE REF TO lcl_a.
DATA lo_b TYPE REF TO lcl_b.

lo_b = lo_a.`
	unit := collect_test_unit(t, "file:///tc_ref_bad.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Assignment_Type), 1)
}

@(test)
semantic_typecheck_skips_dependency_interface_units :: proc(t: ^testing.T) {
	source := `REPORT z_remote_dep.
DATA lv_date TYPE d.
DATA lv_time TYPE t.
lv_date = lv_time.`
	parsed := parser.parse(source, "file:///remote_dep.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	unit := analyze.collect_unit(analyze.Unit_Id(0), "file:///remote_dep.abap", source, parsed, context.allocator)
	unit.source_mode = .Dependency_Interface

	units := make([dynamic]analyze.Unit_Analysis, 0, 1, context.allocator)
	append(&units, unit)
	project := analyze.project_analysis_from_units(units, context.allocator)
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 64}, context.allocator)
	defer execution.pool_destroy(&pool)
	analyze.finish_project_analysis(&project, &pool, {}, context.allocator)

	testing.expect(t, !has_diagnostic(&project.units[0], .Incompatible_Assignment_Type))
}

@(test)
semantic_typecheck_trusts_complete_dependency_interface_signatures :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://tc_external_complete.abap",
		source = `REPORT z_tc_external_complete.
TYPES: BEGIN OF ty_row,
         value TYPE c,
       END OF ty_row.
DATA lv_num TYPE i.
DATA ls_row TYPE ty_row.
zcl_dep=>run( EXPORTING method_unknown = lv_num ).
zcl_dep=>run( EXPORTING value = ls_row required = lv_num ).
zcl_dep=>run( EXPORTING value = lv_num ).
CALL FUNCTION 'Z_DEP_FM' EXPORTING function_unknown = lv_num.
CALL FUNCTION 'Z_DEP_FM' EXPORTING iv_num = ls_row iv_required = lv_num.
CALL FUNCTION 'Z_DEP_FM' EXPORTING iv_num = lv_num.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/global-class/zcl_dep.abap",
			source = `CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING value TYPE numeric required TYPE i.
ENDCLASS.`,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/function/z_dep_fm.abap",
			source = `FUNCTION z_dep_fm
  IMPORTING iv_num TYPE numeric iv_required TYPE i.
ENDFUNCTION.`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	if root != nil {
		testing.expect_value(t, diagnostic_count(root, .Unknown_Named_Parameter), 2)
		testing.expect_value(t, diagnostic_count(root, .Incompatible_Argument_Type), 2)
		testing.expect_value(t, diagnostic_count(root, .Missing_Required_Parameter), 2)
	}
}

@(test)
semantic_typecheck_keeps_incomplete_dependency_interface_signatures_silent :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://tc_external_incomplete.abap",
		source = `REPORT z_tc_external_incomplete.
TYPES: BEGIN OF ty_row,
         value TYPE c,
       END OF ty_row.
DATA lv_num TYPE i.
DATA ls_row TYPE ty_row.
zcl_dep=>run( EXPORTING method_unknown = lv_num ).
zcl_dep=>run( EXPORTING value = ls_row required = lv_num ).
zcl_dep=>run( EXPORTING value = lv_num ).
CALL FUNCTION 'Z_DEP_FM' EXPORTING function_unknown = lv_num.
CALL FUNCTION 'Z_DEP_FM' EXPORTING iv_num = ls_row iv_required = lv_num.
CALL FUNCTION 'Z_DEP_FM' EXPORTING iv_num = lv_num.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/global-class/zcl_dep.abap",
			source = `CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING value TYPE TABLE required TYPE i.
ENDCLASS.`,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/function/z_dep_fm.abap",
			source = `FUNCTION z_dep_fm
  IMPORTING iv_num TYPE TABLE iv_required TYPE i.
ENDFUNCTION.`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	if root != nil {
		testing.expect(t, !has_diagnostic(root, .Unknown_Named_Parameter))
		testing.expect(t, !has_diagnostic(root, .Incompatible_Argument_Type))
		testing.expect(t, !has_diagnostic(root, .Missing_Required_Parameter))
	}
}

@(test)
semantic_typecheck_reports_method_argument_failures :: proc(t: ^testing.T) {
	source := `REPORT z_tc_method_args.
TYPES: BEGIN OF e070,
         trkorr TYPE c,
       END OF e070.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING iv_trkorr TYPE e070-trkorr iv_count TYPE i
      EXPORTING ev_trkorr TYPE e070-trkorr
      CHANGING  cv_count  TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl_demo.
  DATA lv_trkorr TYPE e070-trkorr.
  DATA lv_text TYPE string.
  CREATE OBJECT lo.

  lo->run(
    EXPORTING iv_trkorr = sy-uzeit unknown = lv_trkorr
    IMPORTING ev_trkorr = 'literal'
    CHANGING  cv_count  = lv_text ).`
	unit := collect_test_unit(t, "file:///tc_method_args.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Unknown_Named_Parameter))
	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Argument_Type), 1)
	testing.expect(t, !has_diagnostic(&unit, .Missing_Required_Parameter))
}

@(test)
semantic_typecheck_reports_missing_required_method_and_function_parameters :: proc(t: ^testing.T) {
	source := `REPORT z_tc_missing_required.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING iv_required TYPE i iv_optional TYPE i OPTIONAL iv_default TYPE i DEFAULT 1.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
FUNCTION z_required
  IMPORTING iv_required TYPE i iv_default TYPE i DEFAULT 1.
ENDFUNCTION.
START-OF-SELECTION.
  lcl_demo=>run( ).
  CALL FUNCTION 'Z_REQUIRED'.`
	unit := collect_test_unit(t, "file:///tc_missing_required.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Missing_Required_Parameter), 2)
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Named_Parameter))
}

@(test)
semantic_typecheck_maps_positional_method_arguments :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_count TYPE i.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  DATA lo TYPE REF TO lcl_demo.
  DATA lv_count TYPE i.
  CREATE OBJECT lo.
  lo->run( lv_count ).`
	unit := collect_test_unit(t, "file:///tc_positional_method_arg.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Named_Parameter))
	testing.expect(t, !has_diagnostic(&unit, .Missing_Required_Parameter))
	testing.expect(t, !has_diagnostic(&unit, .Incompatible_Argument_Type))
}

@(test)
semantic_typecheck_accepts_positional_required_parameter_for_missing_check :: proc(t: ^testing.T) {
	source := `REPORT z_tc_positional_required.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING iv_value TYPE i.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  DATA lv_value TYPE i.
  lcl_demo=>run( lv_value ).`
	unit := collect_test_unit(t, "file:///tc_positional_required.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Missing_Required_Parameter))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Named_Parameter))
}

@(test)
semantic_typecheck_reports_positional_method_argument_failure :: proc(t: ^testing.T) {
	source := `REPORT z_tc_positional_method_bad.
TYPES: BEGIN OF e070,
         trkorr TYPE c,
       END OF e070.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING value TYPE numeric.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  DATA ls_e070 TYPE e070.
  lcl_demo=>run( ls_e070 ).`
	unit := collect_test_unit(t, "file:///tc_positional_method_bad.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Argument_Type), 1)
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Named_Parameter))
	testing.expect(t, !has_diagnostic(&unit, .Missing_Required_Parameter))
}

@(test)
semantic_typecheck_maps_sectioned_method_and_function_arguments :: proc(t: ^testing.T) {
	source := `REPORT z_tc_sectioned_call_args.
TYPES: BEGIN OF e070,
         trkorr TYPE c,
       END OF e070.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING iv_num TYPE numeric.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
FUNCTION z_demo
  IMPORTING iv_num TYPE numeric.
ENDFUNCTION.
START-OF-SELECTION.
  DATA ls_e070 TYPE e070.
  lcl_demo=>run( EXPORTING iv_num = ls_e070 ).
  CALL FUNCTION 'Z_DEMO' EXPORTING iv_num = ls_e070.`
	unit := collect_test_unit(t, "file:///tc_sectioned_call_args.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Argument_Type), 2)
	testing.expect(t, !has_diagnostic(&unit, .Missing_Required_Parameter))
}

@(test)
semantic_typecheck_maps_optional_default_positional_arguments :: proc(t: ^testing.T) {
	source := `REPORT z_tc_optional_default_positional.
TYPES: BEGIN OF e070,
         trkorr TYPE c,
       END OF e070.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS optional IMPORTING value TYPE numeric OPTIONAL.
    CLASS-METHODS defaulted IMPORTING value TYPE numeric DEFAULT 1.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD optional.
  ENDMETHOD.
  METHOD defaulted.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  DATA ls_e070 TYPE e070.
  lcl_demo=>optional( ls_e070 ).
  lcl_demo=>defaulted( ls_e070 ).`
	unit := collect_test_unit(t, "file:///tc_optional_default_positional.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Argument_Type), 2)
	testing.expect(t, !has_diagnostic(&unit, .Missing_Required_Parameter))
}

@(test)
semantic_typecheck_skips_ambiguous_positional_method_mapping :: proc(t: ^testing.T) {
	source := `REPORT z_tc_positional_unknown.
TYPES: BEGIN OF e070,
         trkorr TYPE c,
       END OF e070.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING first TYPE numeric second TYPE numeric.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  DATA ls_e070 TYPE e070.
  lcl_demo=>run( ls_e070 ).`
	unit := collect_test_unit(t, "file:///tc_positional_unknown.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Incompatible_Argument_Type))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Named_Parameter))
	testing.expect(t, !has_diagnostic(&unit, .Missing_Required_Parameter))
}

@(test)
semantic_typecheck_skips_missing_required_after_uncertain_argument_mapping :: proc(t: ^testing.T) {
	source := `REPORT z_tc_missing_uncertain.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING first TYPE i second TYPE i.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  DATA lv_value TYPE i.
  lcl_demo=>run( unknown = lv_value ).
  lcl_demo=>run( lv_value ).`
	unit := collect_test_unit(t, "file:///tc_missing_uncertain.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Unknown_Named_Parameter), 1)
	testing.expect(t, !has_diagnostic(&unit, .Missing_Required_Parameter))
}

@(test)
semantic_typecheck_reports_clike_argument_family_failures :: proc(t: ^testing.T) {
	source := `REPORT z_tc_clike_args.
TYPES: BEGIN OF ty_row,
         value TYPE c,
       END OF ty_row.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS needs_clike IMPORTING value TYPE clike.
    CLASS-METHODS needs_csequence IMPORTING value TYPE csequence.
    CLASS-METHODS text RETURNING VALUE(result) TYPE string.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD needs_clike.
  ENDMETHOD.
  METHOD needs_csequence.
  ENDMETHOD.
  METHOD text.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  DATA lv_char TYPE c.
  DATA lv_num TYPE n.
  DATA lv_string TYPE string.
  DATA lv_date TYPE d.
  DATA lv_time TYPE t.
  DATA lv_bool TYPE abap_bool.
  DATA lv_x TYPE x.
  DATA lv_ddic TYPE rs38l_fnam.
  DATA ls_row TYPE ty_row.
  lcl_demo=>needs_clike( value = lv_char ).
  lcl_demo=>needs_clike( value = lv_num ).
  lcl_demo=>needs_clike( value = lv_string ).
  lcl_demo=>needs_clike( value = lv_date ).
  lcl_demo=>needs_clike( value = lv_time ).
  lcl_demo=>needs_clike( value = lv_bool ).
  lcl_demo=>needs_clike( value = lv_x ).
  lcl_demo=>needs_clike( value = ls_row ).
  lcl_demo=>needs_clike( value = lv_ddic ).
  lcl_demo=>needs_clike( value = lcl_demo=>text( ) ).
  lcl_demo=>needs_csequence( value = lv_date ).`
	unit := collect_test_unit(t, "file:///tc_clike_args.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Argument_Type), 2)
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Named_Parameter))
	testing.expect(t, !has_diagnostic(&unit, .Missing_Required_Parameter))
}

@(test)
semantic_typecheck_skips_missing_required_redefinition_signature :: proc(t: ^testing.T) {
	source := `REPORT z_tc_missing_redefinition.
CLASS lcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_value TYPE i.
ENDCLASS.
CLASS lcl_parent IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
  PUBLIC SECTION.
    METHODS run REDEFINITION.
ENDCLASS.
CLASS lcl_child IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  DATA lo_child TYPE REF TO lcl_child.
  CREATE OBJECT lo_child.
  lo_child->run( ).`
	unit := collect_test_unit(t, "file:///tc_missing_redefinition.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Missing_Required_Parameter))
}

@(test)
semantic_typecheck_accepts_common_assignment_conversions :: proc(t: ^testing.T) {
	source := `REPORT z_tc_assignment_conversions.
TYPES: BEGIN OF ty_key,
         attr1 TYPE c,
       END OF ty_key.
TYPES ty_ints TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lv_tabix TYPE syst-tabix.
DATA ls_key TYPE ty_key.
DATA rv_valid TYPE abap_bool.
DATA lv_char TYPE c.
DATA lv_count TYPE i.
DATA lv_key TYPE string.
DATA iv_key TYPE string.
DATA lt_ints TYPE ty_ints.

lv_tabix = sy-tabix.
ls_key-attr1 = 'MSGV1'.
lv_char = 'MSGV1'.
rv_valid = boolc( sy-subrc = 0 ).
lv_count = lines( lt_ints ).
lv_count = strlen( lv_key ).
lv_key = to_upper( iv_key ).`
	unit := collect_test_unit(t, "file:///tc_assignment_conversions.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Incompatible_Assignment_Type))
	seen_boolc, seen_lines, seen_strlen := false, false, false
	for site in unit.assignment_sites {
		rhs_text := source[site.rhs_range.start:site.rhs_range.end]
		expected := ""
		switch rhs_text {
		case "boolc( sy-subrc = 0 )":
			expected = "string"
			seen_boolc = true
		case "lines( lt_ints )":
			expected = "i"
			seen_lines = true
		case "strlen( lv_key )":
			expected = "i"
			seen_strlen = true
		}
		if expected != "" {
			type_data := expect_type_kind(t, &unit, site.rhs.type_id, .Builtin)
			if type_data != nil {
				testing.expect_value(t, type_data.name, expected)
			}
		}
	}
	testing.expect(t, seen_boolc)
	testing.expect(t, seen_lines)
	testing.expect(t, seen_strlen)
}

@(test)
semantic_typecheck_skips_unknown_ddic_alias_assignment :: proc(t: ^testing.T) {
	source := `REPORT z_tc_unknown_ddic_alias.
DATA lv_domain TYPE zmissing_domain.
DATA lv_date TYPE d.

lv_date = lv_domain.`
	unit := collect_test_unit(t, "file:///tc_unknown_ddic_alias.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Incompatible_Assignment_Type))
}

@(test)
semantic_typecheck_accepts_high_confidence_call_result_assignment :: proc(t: ^testing.T) {
	source := `REPORT z_tc_call_result_ok.
CLASS lcl_clock DEFINITION.
  PUBLIC SECTION.
    METHODS get_time RETURNING VALUE(rv_time) TYPE t.
ENDCLASS.
CLASS lcl_clock IMPLEMENTATION.
  METHOD get_time.
  ENDMETHOD.
ENDCLASS.
DATA lo_clock TYPE REF TO lcl_clock.
DATA lv_time TYPE t.

lv_time = lo_clock->get_time( ).`
	unit := collect_test_unit(t, "file:///tc_call_result_ok.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Incompatible_Assignment_Type))
}

@(test)
semantic_typecheck_reports_high_confidence_call_result_assignment_failure :: proc(t: ^testing.T) {
	source := `REPORT z_tc_call_result_bad.
CLASS lcl_clock DEFINITION.
  PUBLIC SECTION.
    METHODS get_time RETURNING VALUE(rv_time) TYPE t.
ENDCLASS.
CLASS lcl_clock IMPLEMENTATION.
  METHOD get_time.
  ENDMETHOD.
ENDCLASS.
DATA lo_clock TYPE REF TO lcl_clock.
DATA lv_date TYPE d.

lv_date = lo_clock->get_time( ).`
	unit := collect_test_unit(t, "file:///tc_call_result_bad.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Assignment_Type), 1)
}

@(test)
semantic_typecheck_reports_direct_attribute_selector_assignment_failure :: proc(t: ^testing.T) {
	source := `REPORT z_tc_attr_selector_bad.
CLASS lcl_clock DEFINITION.
  PUBLIC SECTION.
    DATA mv_time TYPE t.
ENDCLASS.
DATA lo_clock TYPE REF TO lcl_clock.
DATA lv_date TYPE d.

lv_date = lo_clock->mv_time.`
	unit := collect_test_unit(t, "file:///tc_attr_selector_bad.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Assignment_Type), 1)
}

@(test)
semantic_typecheck_accepts_zabapgit_like_object_selector_conversion :: proc(t: ^testing.T) {
	source := `REPORT z_tc_object_selector_conversion.
TYPES: BEGIN OF trwbo_request_header,
         trkorr TYPE c,
       END OF trwbo_request_header.
TYPES: BEGIN OF ty_range,
         low TYPE string,
       END OF ty_range.
DATA lr_request TYPE REF TO trwbo_request_header.
DATA ls_r_trkorr TYPE ty_range.

ls_r_trkorr-low = lr_request->trkorr.`
	unit := collect_test_unit(t, "file:///tc_object_selector_conversion.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, !has_diagnostic(&unit, .Incompatible_Assignment_Type))
}

@(test)
semantic_typecheck_accepts_known_table_line_assignments :: proc(t: ^testing.T) {
	source := `REPORT z_tc_table_line_ok.
TYPES ty_times TYPE STANDARD TABLE OF t WITH DEFAULT KEY.
DATA lt_times TYPE ty_times.
DATA lv_time TYPE t.
FIELD-SYMBOLS <lv_time> TYPE t.
FIELD-SYMBOLS <any> TYPE any.

LOOP AT lt_times INTO lv_time.
ENDLOOP.
LOOP AT lt_times ASSIGNING <lv_time>.
ENDLOOP.
LOOP AT lt_times ASSIGNING <any>.
ENDLOOP.
READ TABLE lt_times INTO lv_time INDEX 1.
READ TABLE lt_times ASSIGNING <lv_time> INDEX 1.`
	unit := collect_test_unit(t, "file:///tc_table_line_ok.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Incompatible_Assignment_Type))
}

@(test)
semantic_typecheck_reports_known_table_line_assignment_failure :: proc(t: ^testing.T) {
	source := `REPORT z_tc_table_line_bad.
TYPES ty_times TYPE STANDARD TABLE OF t WITH DEFAULT KEY.
DATA lt_times TYPE ty_times.
FIELD-SYMBOLS <lv_date> TYPE d.

LOOP AT lt_times ASSIGNING <lv_date>.
ENDLOOP.`
	unit := collect_test_unit(t, "file:///tc_table_line_bad.abap", source)

	testing.expect_value(t, diagnostic_count(&unit, .Incompatible_Assignment_Type), 1)
}

@(test)
semantic_typecheck_skips_unknown_table_line_assignment_rows :: proc(t: ^testing.T) {
	source := `REPORT z_tc_table_line_unknown.
DATA lt_unknown TYPE zmissing_tab.
DATA lv_date TYPE d.

READ TABLE lt_unknown INTO lv_date INDEX 1.`
	unit := collect_test_unit(t, "file:///tc_table_line_unknown.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Incompatible_Assignment_Type))
}

@(test)
semantic_typecheck_treats_oop_defaults_as_optional :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_req TYPE i iv_default TYPE i DEFAULT 1.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  DATA lo TYPE REF TO lcl_demo.
  DATA lv_count TYPE i.
  CREATE OBJECT lo.
  lo->run( iv_req = lv_count ).`
	unit := collect_test_unit(t, "file:///tc_oop_default_arg.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Missing_Required_Parameter))
}

@(test)
semantic_typecheck_reports_generic_and_sql_target_failures :: proc(t: ^testing.T) {
	source := `REPORT z_tc_generic_sql.
TYPES: BEGIN OF e070,
         as4date TYPE d,
         trkorr TYPE c,
       END OF e070.
CLASS lcl_generic DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS needs_numeric IMPORTING value TYPE numeric.
ENDCLASS.

CLASS lcl_generic IMPLEMENTATION.
  METHOD needs_numeric.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ls_e070 TYPE e070.
  DATA lv_time TYPE t.

  lcl_generic=>needs_numeric( value = ls_e070 ).
  SELECT SINGLE as4date FROM e070 INTO @lv_time.`
	unit := collect_test_unit(t, "file:///tc_generic_sql.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Incompatible_Argument_Type))
	testing.expect(t, has_diagnostic(&unit, .Invalid_Open_Sql_Into_Target))
	arg_message, arg_ok := diagnostic_message_for_kind(&unit, .Incompatible_Argument_Type)
	sql_message, sql_ok := diagnostic_message_for_kind(&unit, .Invalid_Open_Sql_Into_Target)
	testing.expect(t, arg_ok)
	testing.expect(t, sql_ok)
	testing.expect_value(
		t,
		arg_message,
		"'ls_e070' is not type-compatible with formal parameter 'value' (current type 'e070', expected type 'numeric')",
	)
	testing.expect_value(
		t,
		sql_message,
		"Open SQL target is not compatible: 'lv_time' (current type 'd', expected type 't')",
	)
}

@(test)
semantic_typecheck_accepts_sql_alias_scalar_conversion :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://tc_sql_alias_ok.abap",
		source = `REPORT z_tc_sql_alias_ok.
DATA lv_text TYPE string.
SELECT SINGLE comp FROM zkeys INTO @lv_text.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table/zkeys.abap",
			source = `TYPES: BEGIN OF zkeys,
         comp TYPE abap_keycompname,
       END OF zkeys.`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	if root != nil {
		testing.expect(t, !has_diagnostic(root, .Invalid_Open_Sql_Into_Target))
	}
}

@(test)
semantic_typecheck_reports_sql_dependency_date_time_target_failure :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://tc_sql_dt_bad.abap",
		source = `REPORT z_tc_sql_dt_bad.
DATA lv_time TYPE t.
SELECT SINGLE as4date FROM e070 INTO @lv_time.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table/e070.abap",
			source = `TYPES: BEGIN OF e070,
         as4date TYPE d,
       END OF e070.`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	if root != nil {
		testing.expect_value(t, diagnostic_count(root, .Invalid_Open_Sql_Into_Target), 1)
	}
}

@(test)
semantic_typecheck_skips_unknown_ddic_sql_source_target :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://tc_sql_unknown_ddic.abap",
		source = `REPORT z_tc_sql_unknown_ddic.
DATA lv_time TYPE t.
SELECT SINGLE raw_value FROM zunknown INTO @lv_time.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table/zunknown.abap",
			source = `TYPES: BEGIN OF zunknown,
         raw_value TYPE zmissing_domain,
       END OF zunknown.`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	if root != nil {
		testing.expect(t, !has_diagnostic(root, .Invalid_Open_Sql_Into_Target))
	}
}

@(test)
function_module_importing_exporting_parameter_name_reuse_is_one_local_symbol :: proc(t: ^testing.T) {
	source := `FUNCTION read_style
  IMPORTING VALUE(OLANGUAGE) LIKE SY-LANGU DEFAULT SPACE
  EXPORTING VALUE(OLANGUAGE) LIKE SY-LANGU.
  OLANGUAGE = SY-LANGU.
ENDFUNCTION.`
	unit := collect_test_unit(t, "file:///read_style.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Duplicate_Declaration))
	fm := analyze.find_symbol(&unit, "read_style", .Module)
	testing.expect(t, fm != nil)
	fm_info := analyze.entity_decl_info(&unit, fm.id) if fm != nil else nil
	testing.expect(t, fm_info != nil)
	testing.expect_value(t, len(fm_info.signature_parameters), 2)
	testing.expect_value(t, fm_info.signature_parameters[0].section, analyze.Decl_Parameter_Section.Function_Importing)
	testing.expect_value(t, fm_info.signature_parameters[1].section, analyze.Decl_Parameter_Section.Function_Exporting)
	parameter_symbols := 0
	for symbol in unit.symbols {
		if symbol.kind == .Parameter && symbol.name == "olanguage" {
			parameter_symbols += 1
		}
	}
	testing.expect_value(t, parameter_symbols, 1)
}

@(test)
function_module_rejects_non_import_export_duplicate_parameter_names :: proc(t: ^testing.T) {
	source := `FUNCTION z_bad
  IMPORTING iv_value TYPE i
  CHANGING iv_value TYPE i.
ENDFUNCTION.`
	unit := collect_test_unit(t, "file:///bad_function_params.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Duplicate_Declaration))
}

@(test)
function_module_exception_can_reuse_parameter_name :: proc(t: ^testing.T) {
	source := `FUNCTION function_include_split
  EXPORTING VALUE(NO_FUNCTION_INCLUDE) TYPE c
  EXCEPTIONS NO_FUNCTION_INCLUDE.
  RAISE NO_FUNCTION_INCLUDE.
ENDFUNCTION.`
	unit := collect_test_unit(t, "file:///function_include_split.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Duplicate_Declaration))
	testing.expect(t, has_symbol(&unit, .Parameter, "no_function_include"))
	testing.expect(t, has_symbol(&unit, .Exception, "no_function_include"))
}

@(test)
function_module_rejects_duplicate_exceptions_after_parameter_name_reuse :: proc(t: ^testing.T) {
	source := `FUNCTION z_bad
  EXPORTING ev_failed TYPE c
  EXCEPTIONS ev_failed ev_failed.
ENDFUNCTION.`
	unit := collect_test_unit(t, "file:///bad_function_exceptions.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Duplicate_Declaration))
}

@(test)
form_tables_structure_resolves_loaded_ddic_type_dependency :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///radmasdl.abap",
		source = `FORM get_non_deleted_objects TABLES resulttab STRUCTURE ddsymtab
                                    rangetab
                             USING par1 par2.
ENDFORM.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "file:///ddsymtab.abap",
			source = `TYPES: BEGIN OF ddsymtab,
         name TYPE string,
       END OF ddsymtab.
`,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil)
	if root != nil {
		testing.expect(t, !has_diagnostic(root, .Unresolved_Reference))
		testing.expect(t, reference_resolves_to_uri(&project, root, "ddsymtab", .Value, .Type_Ref, dependencies[0].uri))
		form := analyze.find_symbol(root, "get_non_deleted_objects", .Form)
		testing.expect(t, form != nil)
		form_info := analyze.entity_decl_info(root, form.id) if form != nil else nil
		testing.expect(t, form_info != nil)
		testing.expect_value(t, len(form_info.signature_parameters), 4)
		resulttab := root.symbols[analyze.symbol_id_index(form_info.signature_parameters[0].symbol)]
		testing.expect_value(t, resulttab.type_clause_display, "STANDARD TABLE OF ddsymtab")
	}
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
string_section_does_not_reference_section_keyword :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///find_section.abap",
		`
FORM run.
  DATA lc_null TYPE string.
  DATA lv_cursor TYPE i.
  DATA iv_data TYPE string.

  FIND FIRST OCCURRENCE OF lc_null IN SECTION OFFSET lv_cursor OF iv_data.
  REPLACE SECTION OFFSET lv_cursor LENGTH 1 OF iv_data WITH lc_null.
ENDFORM.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_reference(&unit, "section", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_cursor", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "iv_data", .Value, .Identifier))
}

@(test)
find_in_table_match_line_does_not_reference_line_keyword :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///find_match_line.abap",
		`
FORM run.
  DATA ct_source TYPE STANDARD TABLE OF string.
  DATA iv_from_interface TYPE string.
  DATA lv_tabix TYPE i.

  FIND REGEX '^\s*INTERFACES(:| )\s*' && iv_from_interface && '\s*.' IN TABLE ct_source MATCH LINE lv_tabix ##REGEX_POSIX.
ENDFORM.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_reference(&unit, "line", .Value, .Identifier))
	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	testing.expect_value(t, len(lint_unit.find_sites), 1)
	testing.expect_value(t, len(lint_unit.find_sites[0].write_targets), 1)
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
create_object_exporting_method_result_uses_routine_namespace :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///create_object_exporting_method.abap",
		`
CLASS lcl_owner DEFINITION.
  PUBLIC SECTION.
    METHODS get_generic.
  PRIVATE SECTION.
    METHODS get_field_rules RETURNING VALUE(ro_rules) TYPE REF TO object.
ENDCLASS.

CLASS lcl_owner IMPLEMENTATION.
  METHOD get_generic.
    DATA ro_generic TYPE REF TO object.
    DATA ms_item TYPE i.
    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item.
  ENDMETHOD.

  METHOD get_field_rules.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect_value(t, reference_count(&unit, "get_field_rules", .Routine, .Routine_Call), 1)
	testing.expect_value(t, reference_count(&unit, "get_field_rules", .Value, .Identifier), 0)
}

@(test)
call_method_sections_method_results_use_routine_namespace :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///call_method_sections_method.abap",
		`
CLASS lcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS consume
      IMPORTING iv_value TYPE i
      EXPORTING ev_value TYPE i
      CHANGING cv_value TYPE i.
ENDCLASS.

CLASS lcl_dep IMPLEMENTATION.
  METHOD consume.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_owner DEFINITION.
  PUBLIC SECTION.
    METHODS run.
  PRIVATE SECTION.
    METHODS get_field_rules RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.

CLASS lcl_owner IMPLEMENTATION.
  METHOD run.
    DATA lo_dep TYPE REF TO lcl_dep.
    CALL METHOD lo_dep->consume
      EXPORTING iv_value = get_field_rules( )
      IMPORTING ev_value = get_field_rules( )
      CHANGING cv_value = get_field_rules( ).
  ENDMETHOD.

  METHOD get_field_rules.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect_value(t, reference_count(&unit, "get_field_rules", .Routine, .Routine_Call), 3)
	testing.expect_value(t, reference_count(&unit, "get_field_rules", .Value, .Identifier), 0)
}

@(test)
parenthesized_call_method_arguments_see_method_locals :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///parenthesized_call_method_args.abap",
		`
CLASS /sttp/cl_dm_query DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS query_objectdata_item
      IMPORTING iv_objcode TYPE string
      CHANGING co_messages TYPE REF TO object.
ENDCLASS.

CLASS /sttp/cl_dm_query IMPLEMENTATION.
  METHOD query_objectdata_item.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_owner DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_owner IMPLEMENTATION.
  METHOD run.
    DATA lv_objcode TYPE string.
    DATA lo_messages TYPE REF TO object.
    CALL METHOD /sttp/cl_dm_query=>query_objectdata_item(
      EXPORTING
        iv_objcode  = lv_objcode
      CHANGING
        co_messages = lo_messages
    ).
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect_value(t, reference_count(&unit, "lv_objcode", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "lo_messages", .Value, .Identifier), 1)
}

@(test)
ole_call_method_of_uses_value_namespace :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///ole_call_method_of.abap",
		`
CLASS lcl_owner DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_owner IMPLEMENTATION.
  METHOD run.
    DATA lv_excel TYPE i.
    DATA lv_cell TYPE i.
    DATA lv_row TYPE i.
    DATA lv_col TYPE i.
    CALL METHOD OF lv_excel 'Cells' = lv_cell
      EXPORTING #1 = lv_row #2 = lv_col.
    CALL METHOD OF lv_excel 'Quit'.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect_value(t, reference_count(&unit, "lv_excel", .Value, .Identifier), 2)
	testing.expect_value(t, reference_count(&unit, "lv_cell", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "lv_row", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "lv_col", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "lv_excel", .Routine, .Routine_Call), 0)
}

@(test)
call_method_positional_args_do_not_enter_target_namespace :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///call_method_positional_arg.abap",
		`
CLASS lcl_document DEFINITION.
ENDCLASS.

CLASS lcl_sender DEFINITION.
  PUBLIC SECTION.
    METHODS set_document IMPORTING io_document TYPE REF TO lcl_document.
ENDCLASS.

CLASS lcl_sender IMPLEMENTATION.
  METHOD set_document.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_owner DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_owner IMPLEMENTATION.
  METHOD run.
    DATA lo_send_mail TYPE REF TO lcl_sender.
    DATA lo_document TYPE REF TO lcl_document.
    CALL METHOD lo_send_mail->set_document( lo_document ).
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect_value(t, reference_count(&unit, "lo_document", .Routine, .Routine_Call), 0)
}

@(test)
new_call_and_constructor_forms_use_routine_namespace :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///new_call_and_constructor_forms.abap",
		`
CLASS lcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_value TYPE i.
    METHODS consume
      EXPORTING ev_value TYPE i
      CHANGING cv_value TYPE i.
ENDCLASS.

CLASS lcl_dep IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD consume.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_owner DEFINITION.
  PUBLIC SECTION.
    METHODS run.
  PRIVATE SECTION.
    METHODS get_field_rules RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.

CLASS lcl_owner IMPLEMENTATION.
  METHOD run.
    DATA lo_dep TYPE REF TO lcl_dep.
    CREATE OBJECT lo_dep EXPORTING iv_value = get_field_rules( ).
    lo_dep = NEW lcl_dep( iv_value = get_field_rules( ) ).
    lo_dep->consume( IMPORTING ev_value = get_field_rules( ) CHANGING cv_value = get_field_rules( ) ).
    NEW lcl_dep( )->consume( IMPORTING ev_value = get_field_rules( ) CHANGING cv_value = get_field_rules( ) ).
  ENDMETHOD.

  METHOD get_field_rules.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect_value(t, reference_count(&unit, "get_field_rules", .Routine, .Routine_Call), 6)
	testing.expect_value(t, reference_count(&unit, "get_field_rules", .Value, .Identifier), 0)
}

@(test)
text_symbols_and_inferred_constructors_do_not_require_global_names :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///text_symbols_and_hash_constructors.abap",
		`
CLASS lcl_dep DEFINITION.
ENDCLASS.

DATA lv_text TYPE string.
DATA lo_dep TYPE REF TO lcl_dep.
DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.

START-OF-SELECTION.
  lv_text = 'Doc. Num.'(008).
  lo_dep = NEW #( ).
  APPEND VALUE #( ) TO lt_text.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
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
create_data_type_handle_uses_value_reference :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///create_data_type_handle.abap",
		`
DATA rr_data TYPE REF TO data.
DATA lo_table TYPE REF TO data.

CREATE DATA rr_data TYPE HANDLE lo_table.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, has_reference(&unit, "lo_table", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "handle lo_table", .Type, .Type_Ref))
}

@(test)
validates_create_data_type_handle_static_types :: proc(t: ^testing.T) {
	valid := collect_test_unit(
		t,
		"file:///create_data_type_handle_valid.abap",
		`
CLASS cl_abap_datadescr DEFINITION.
ENDCLASS.
CLASS cl_abap_elemdescr DEFINITION INHERITING FROM cl_abap_datadescr.
ENDCLASS.

DATA lr_i TYPE REF TO i.
DATA lo_descr TYPE REF TO cl_abap_elemdescr.

CREATE DATA lr_i TYPE HANDLE lo_descr.
`,
	)
	invalid := collect_test_unit(
		t,
		"file:///create_data_type_handle_invalid.abap",
		`
CLASS cl_abap_typedescr DEFINITION.
ENDCLASS.
CLASS cl_abap_datadescr DEFINITION INHERITING FROM cl_abap_typedescr.
ENDCLASS.

DATA lr_data TYPE REF TO data.
DATA lo_descr TYPE REF TO cl_abap_typedescr.

CREATE DATA lr_data TYPE HANDLE lo_descr.
`,
	)

	testing.expect(t, !has_diagnostic(&valid, .Invalid_Create_Data_Type_Handle))
	testing.expect(t, !has_diagnostic(&valid, .Invalid_Create_Data_Target))
	testing.expect(t, has_diagnostic(&invalid, .Invalid_Create_Data_Type_Handle))
}

@(test)
validates_create_data_type_handle_target_is_data_ref :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///create_data_type_handle_target.abap",
		`
CLASS cl_abap_datadescr DEFINITION.
ENDCLASS.

DATA lv_i TYPE i.
DATA lo_descr TYPE REF TO cl_abap_datadescr.

CREATE DATA lv_i TYPE HANDLE lo_descr.
`,
	)

	testing.expect(t, has_diagnostic(&unit, .Invalid_Create_Data_Target))
	testing.expect(t, !has_diagnostic(&unit, .Invalid_Create_Data_Type_Handle))
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
resolves_multi_level_redefined_method_parameters_in_call_args :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///multi_level_redefined_method_params.abap",
		`
CLASS lcl_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS show_object
      IMPORTING
        im_obj_type TYPE string
        im_name     TYPE string.
ENDCLASS.

CLASS lcl_root DEFINITION.
  PUBLIC SECTION.
    METHODS download
      IMPORTING
        im_object_type TYPE string
        im_object_name TYPE string.
ENDCLASS.

CLASS lcl_mid DEFINITION INHERITING FROM lcl_root.
  PUBLIC SECTION.
    METHODS download REDEFINITION.
ENDCLASS.

CLASS lcl_leaf DEFINITION INHERITING FROM lcl_mid.
  PUBLIC SECTION.
    METHODS download REDEFINITION.
ENDCLASS.

CLASS lcl_root IMPLEMENTATION.
  METHOD download.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_mid IMPLEMENTATION.
  METHOD download.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_leaf IMPLEMENTATION.
  METHOD download.
    DATA lv_type TYPE string.
    lv_type = im_object_type.
    lcl_helper=>show_object(
      EXPORTING
        im_obj_type = im_object_type
        im_name     = im_object_name ).
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect_value(t, reference_count(&unit, "im_object_type", .Value, .Identifier), 2)
	testing.expect_value(t, reference_count(&unit, "im_object_name", .Value, .Identifier), 1)
	testing.expect(t, !has_reference(&unit, "im_obj_type", .Value, .Identifier))
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
redefined_alias_method_uses_inherited_interface_signature :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///alias_redefinition_target.abap",
		source = `
CLASS zcx_base DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS zcx_child DEFINITION INHERITING FROM zcx_base.
  PUBLIC SECTION.
    METHODS get_text REDEFINITION.
ENDCLASS.

CLASS zcx_child IMPLEMENTATION.
  METHOD get_text.
    result = 'x'.
  ENDMETHOD.
ENDCLASS.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/global-class/cx_static_check.abap",
			source = `
CLASS cx_static_check DEFINITION INHERITING FROM cx_root.
ENDCLASS.
`,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/global-class/cx_root.abap",
			source = `
CLASS cx_root DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_message.
    ALIASES get_text FOR if_message~get_text.
ENDCLASS.
`,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/global-interface/if_message.abap",
			source = `
INTERFACE if_message.
  METHODS get_text RETURNING VALUE(result) TYPE string.
ENDINTERFACE.
`,
			mode = .Dependency_Interface,
		},
	}

	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, root != nil && !has_diagnostic(root, .Unresolved_Reference))
	if root != nil {
		testing.expect(t, has_reference(root, "result", .Value, .Identifier))
	}
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
class_definition_load_does_not_shadow_cached_class :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "abapls-cache:/global-interface/if_salv_c_keys.abap",
		source = `
INTERFACE if_salv_c_keys.
  CLASS cl_gui_column_tree DEFINITION LOAD.
  CONSTANTS paste TYPE i VALUE cl_gui_column_tree=>key_paste.
ENDINTERFACE.
`,
		mode = .Dependency_Interface,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/global-class/cl_tree_control_base.abap",
			source = `
CLASS cl_tree_control_base DEFINITION.
  PUBLIC SECTION.
    CONSTANTS key_paste TYPE i VALUE 8.
ENDCLASS.
`,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/global-class/cl_gui_column_tree.abap",
			source = `
CLASS cl_gui_column_tree DEFINITION
  INHERITING FROM cl_tree_control_base.
ENDCLASS.
`,
			mode = .Dependency_Interface,
		},
	}

	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, root != nil && analyze.find_symbol(root, "cl_gui_column_tree", .Class) == nil)
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
IF sy-pfkey = 'SAVE'.
ENDIF.
IF sy-dynnr = '1001'.
ENDIF.
IF sy-datlo = sy-datum.
ENDIF.
IF sy-host <> ''.
ENDIF.
IF sy-sysid = 'ABC'.
ENDIF.
IF sy-mandt = '100'.
ENDIF.
IF sy-saprl = '757'.
ENDIF.
IF sy-scols > 0.
ENDIF.
IF sy-srows > 0.
ENDIF.
sy-tcode = 'SE41' ##WRITE_OK.

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
	host, host_ok := analyze.structure_field_info(&unit, syst.id, "host")
	subrc, subrc_ok := analyze.structure_field_info(&unit, syst.id, "subrc")
	saprl, saprl_ok := analyze.structure_field_info(&unit, syst.id, "saprl")
	tcode, tcode_ok := analyze.structure_field_info(&unit, syst.id, "tcode")
	screen_name, screen_ok := analyze.structure_field_info(&unit, screen.id, "name")
	testing.expect(t, host_ok)
	testing.expect(t, subrc_ok)
	testing.expect(t, saprl_ok)
	testing.expect(t, tcode_ok)
	testing.expect(t, screen_ok)
	testing.expect_value(t, host.type_ref.base_name, "c")
	testing.expect_value(t, subrc.type_ref.base_name, "i")
	testing.expect_value(t, saprl.type_ref.base_name, "c")
	testing.expect_value(t, tcode.type_ref.base_name, "c")
	testing.expect_value(t, screen_name.type_ref.base_name, "c")
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
textpool_builtin_type_validates_known_fields :: proc(t: ^testing.T) {
	valid := collect_test_unit(
		t,
		"file:///textpool_builtin_valid.abap",
		`
TYPES:
  BEGIN OF ty_tpool.
    INCLUDE TYPE textpool.
TYPES: split TYPE c LENGTH 8,
  END OF ty_tpool,
  ty_tpool_tt TYPE STANDARD TABLE OF ty_tpool WITH DEFAULT KEY.
DATA lt_raw TYPE textpool_table.
DATA rt_tpool TYPE ty_tpool_tt.
FIELD-SYMBOLS <ls_raw> LIKE LINE OF lt_raw.
FIELD-SYMBOLS <ls_tpool_out> LIKE LINE OF rt_tpool.
<ls_tpool_out>-split = <ls_tpool_out>-entry.
<ls_tpool_out>-entry = <ls_tpool_out>-entry+8.
IF <ls_tpool_out>-id = 'S'.
ENDIF.
<ls_raw>-entry = <ls_tpool_out>-entry.
`,
	)
	invalid := collect_test_unit(
		t,
		"file:///textpool_builtin_invalid.abap",
		`
DATA lt_raw TYPE textpool_table.
FIELD-SYMBOLS <ls_raw> LIKE LINE OF lt_raw.
<ls_raw>-missing = 'x'.
`,
	)

	testing.expect(t, !has_diagnostic(&valid, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&valid, .Unknown_Field))
	testing.expect(t, has_diagnostic(&invalid, .Unknown_Field))
	expect_structure_fields(t, &valid, "textpool", "id", "key", "entry", "length")
}

@(test)
resolves_known_cl_abap_char_utilities_attributes :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/main.abap",
		source = `
DATA lv_text TYPE string.
DATA lv_size TYPE i.
lv_text = cl_abap_char_utilities=>newline.
lv_text = cl_abap_char_utilities=>cr_lf.
lv_text = cl_abap_char_utilities=>form_feed.
lv_text = cl_abap_char_utilities=>horizontal_tab.
lv_text = cl_abap_char_utilities=>minchar.
lv_text = cl_abap_char_utilities=>endian.
lv_size = cl_abap_char_utilities=>charsize.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/global-class/cl_abap_char_utilities.abap",
			source = "CLASS cl_abap_char_utilities DEFINITION. ENDCLASS.",
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil)
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))
}

@(test)
friend_class_can_access_private_static_attribute :: proc(t: ^testing.T) {
	allowed := collect_test_unit(
		t,
		"file:///friend_static_attribute.abap",
		`
CLASS lcl_target DEFINITION FRIENDS lcl_friend.
  PRIVATE SECTION.
    CLASS-DATA gv_value TYPE i.
ENDCLASS.
CLASS lcl_friend DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS lcl_friend IMPLEMENTATION.
  METHOD run.
    lcl_target=>gv_value = 1.
  ENDMETHOD.
ENDCLASS.
`,
	)
	denied := collect_test_unit(
		t,
		"file:///non_friend_static_attribute.abap",
		`
CLASS lcl_target DEFINITION.
  PRIVATE SECTION.
    CLASS-DATA gv_value TYPE i.
ENDCLASS.
CLASS lcl_other DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS lcl_other IMPLEMENTATION.
  METHOD run.
    lcl_target=>gv_value = 1.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&allowed, .Unknown_Field))
	testing.expect(t, has_diagnostic(&denied, .Unknown_Field))
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

	handle, handle_ok := sem_query.decl_symbol_handle_at_offset(decl_query, decl_offset)
	testing.expect(t, handle_ok)
	testing.expect_value(t, handle.unit, unit.unit_id)
	testing.expect_value(t, handle.symbol, sym.id)

	sym_copy, sym_copy_ok := sem_query.decl_symbol_copy_at_offset(decl_query, decl_offset)
	testing.expect(t, sym_copy_ok)
	testing.expect_value(t, sym_copy.name, "lv_value")

	by_range := sem_query.decl_symbol_with_kind_and_decl_range(decl_query, .Variable, sym.decl_range)
	testing.expect(t, by_range != nil)
	testing.expect_value(t, by_range.id, sym.id)

	ref := sem_query.ref_reference_at_offset(ref_query, use_offset)
	testing.expect(t, ref != nil)
	testing.expect_value(t, ref.name, "lv_value")
	testing.expect(t, ref.has_resolution)

	ref_id, ref_id_ok := sem_query.ref_reference_id_at_offset(ref_query, use_offset)
	testing.expect(t, ref_id_ok)
	testing.expect_value(t, ref_id, ref.id)

	ref_copy, ref_copy_ok := sem_query.ref_reference_copy_at_offset(ref_query, use_offset)
	testing.expect(t, ref_copy_ok)
	testing.expect_value(t, ref_copy.id, ref.id)

	exact_ref := sem_query.ref_reference_at_range(ref_query, ref.range)
	testing.expect(t, exact_ref != nil)
	testing.expect_value(t, exact_ref.id, ref.id)

	exact_ref_copy, exact_ref_copy_ok := sem_query.ref_reference_copy_at_range(ref_query, ref.range)
	testing.expect(t, exact_ref_copy_ok)
	testing.expect_value(t, exact_ref_copy.id, ref.id)

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

	fact_copy, fact_copy_ok := sem_query.fact_expression_fact_copy_at_offset(fact_query, use_offset)
	testing.expect(t, fact_copy_ok)
	testing.expect_value(t, fact_copy.kind, analyze.Expression_Fact_Kind.Reference)

	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	flows := make([dynamic]^lints.Value_Flow_Edge_Data, 0, 1, context.allocator)
	for &edge in lint_unit.value_flow_edges {
		if analyze.range_contains_offset(edge.source_range, use_offset) ||
		   analyze.range_contains_offset(edge.target.range, use_offset) {
			append(&flows, &edge)
		}
	}
	testing.expect_value(t, len(flows), 1)
	testing.expect_value(t, flows[0].kind, lints.Value_Flow_Kind.Assignment)
	testing.expect_value(t, flows[0].target.kind, lints.Value_Flow_Target_Kind.Assignment)
}

@(test)
operand_annotations_cover_core_body_expressions :: proc(t: ^testing.T) {
	source := `
TYPES: BEGIN OF ty_row,
         field TYPE string,
       END OF ty_row.
DATA ls_row TYPE ty_row.

CLASS lcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS get_text RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS lcl_dep IMPLEMENTATION.
  METHOD get_text.
    rv_text = 'x'.
  ENDMETHOD.
ENDCLASS.

DATA lo_dep TYPE REF TO lcl_dep.
DATA(lv_text) = lo_dep->get_text( ).
ls_row-field = lv_text.
DATA(lv_num) = VALUE i( ).
`
	unit := collect_test_unit(t, "file:///operand_annotations.abap", source)
	fact_query := sem_query.facts(sem_query.semantic(&unit))

	inline_offset := find_text(source, "lv_text")
	inline_operand := sem_query.fact_operand_at_offset(fact_query, inline_offset)
	expect_operand(t, &unit, inline_operand, .Variable, "string")
	if inline_operand != nil {
		testing.expect(t, .Assignable in inline_operand.flags)
	}

	call_offset := find_text_last(source, "get_text")
	call_operand := sem_query.fact_operand_at_offset(fact_query, call_offset)
	expect_operand(t, &unit, call_operand, .Value, "string")

	selector_offset := find_text(source, "ls_row-field") + len("ls_row-")
	selector_operand := sem_query.fact_operand_at_offset(fact_query, selector_offset)
	expect_operand(t, &unit, selector_operand, .Field, "string")

	identifier_offset := find_text_last(source, "lv_text")
	identifier_operand := sem_query.fact_operand_at_offset(fact_query, identifier_offset)
	expect_operand(t, &unit, identifier_operand, .Variable, "string")

	literal_offset := find_text(source, "'x'")
	literal_operand := sem_query.fact_operand_at_offset(fact_query, literal_offset)
	expect_operand(t, &unit, literal_operand, .Constant, "string")

	constructor_offset := find_text(source, "VALUE i")
	constructor_operand, constructor_ok := sem_query.fact_operand_copy_at_offset(fact_query, constructor_offset)
	testing.expect(t, constructor_ok)
	expect_operand(t, &unit, &constructor_operand, .Value, "i")
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
	member_info := analyze.entity_decl_info(&unit, member.id)
	testing.expect(t, member_info != nil)
	testing.expect_value(t, member_info.member_kind, analyze.Class_Member_Kind.Method)
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
	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	testing.expect_value(t, len(lint_unit.routine_control_regions), 1)
	testing.expect_value(t, lint_unit.routine_control_regions[0].kind, lints.Routine_Control_Region_Kind.Loop)
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
	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	testing.expect_value(t, len(lint_unit.routine_control_regions), 1)
	testing.expect_value(t, lint_unit.routine_control_regions[0].kind, lints.Routine_Control_Region_Kind.Case)
}

@(test)
collects_write_to_target_without_keyword_reference :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///write_to.abap",
		`
FORM run.
  DATA lv_date TYPE d.
  DATA lv_date_string TYPE string.
  WRITE lv_date TO lv_date_string.
ENDFORM.
`,
	)

	testing.expect_value(t, reference_count(&unit, "lv_date", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "lv_date_string", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "to", .Value, .Identifier), 0)
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
collects_at_group_kinds_fields_and_loop_contexts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///at_groups.abap",
		`
FORM run.
  TYPES: BEGIN OF ty_row,
           src_plant TYPE i,
         END OF ty_row.
  DATA itab TYPE TABLE OF ty_row.
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
	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	for region in lint_unit.routine_control_regions {
		if region.kind != .At {
			continue
		}
		switch region.at_kind {
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
	testing.expect_value(t, reference_count(&unit, "src_plant", .Value, .Identifier), 0)
	testing.expect_value(t, len(unit.loop_at_field_contexts), 2)
	testing.expect(t, system_update_present(&unit, .Loop_At, "subrc"))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	keywords := [?]string{"first", "last", "new", "end", "of", "endat"}
	for keyword in keywords {
		testing.expect(t, !has_reference(&unit, keyword, .Value, .Identifier))
	}
}

@(test)
validates_at_group_fields_against_loop_source :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///at_group_field.abap",
		`
FORM run.
  TYPES: BEGIN OF ty_row,
           good TYPE i,
         END OF ty_row.
  DATA itab TYPE TABLE OF ty_row.
  LOOP AT itab.
    AT NEW missing.
    ENDAT.
  ENDLOOP.
ENDFORM.
`,
	)

	testing.expect(t, has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
rejects_at_groups_outside_loop_at :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///at_group_context.abap",
		`
FORM run.
  AT FIRST.
  ENDAT.
  AT NEW any_field.
  ENDAT.
ENDFORM.
`,
	)

	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	testing.expect(t, diagnostic_present(lint_unit.diagnostics[:], .Invalid_Control_Break))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
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
	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	testing.expect_value(t, len(lint_unit.perform_calls), 1)
	testing.expect_value(t, len(lint_unit.perform_calls[0].arguments), 2)
	testing.expect_value(
		t,
		lint_unit.perform_calls[0].arguments[1].section,
		lints.Perform_Parameter_Section.Changing,
	)
}

@(test)
inline_call_importing_declaration_is_visible_after_block :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///inline_call_importing_scope.abap",
		`
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS compose EXPORTING ev_value TYPE i.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD compose.
  ENDMETHOD.
  METHOD run.
    DO 1 TIMES.
      CALL METHOD me->compose
        IMPORTING ev_value = DATA(lv_value).
    ENDDO.
    lv_value = lv_value.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, has_symbol(&unit, .Variable, "lv_value"))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
inline_call_importing_target_infers_formal_structure :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///inline_call_importing_structure.abap",
		`
CLASS lcl_rules DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_event,
             readpnt_gln TYPE string,
           END OF ty_event.
    CLASS-METHODS get_event_data EXPORTING es_evt TYPE ty_event.
ENDCLASS.

CLASS lcl_rules IMPLEMENTATION.
  METHOD get_event_data.
  ENDMETHOD.
ENDCLASS.

FORM run.
  lcl_rules=>get_event_data( IMPORTING es_evt = DATA(ls_evt) ).
  DATA(lv_gln) = ls_evt-readpnt_gln.
  SELECT SINGLE * FROM ztab INTO @DATA(ls_row) WHERE cmo_gln = @ls_evt-readpnt_gln.
ENDFORM.
`,
	)

	ls_evt := analyze.find_symbol(&unit, "ls_evt", .Variable)
	testing.expect(t, ls_evt != nil)
	if ls_evt != nil {
		testing.expect(t, ls_evt.has_declared_type)
		testing.expect_value(t, ls_evt.declared_type.base_name, "ty_event")
	}
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
classic_data_declaration_is_visible_after_block :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///classic_data_scope.abap",
		`
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DO 1 TIMES.
      DATA lv_value TYPE i.
      lv_value = 1.
    ENDDO.
    lv_value = lv_value.
  ENDMETHOD.
ENDCLASS.
`,
	)

	found := false
	for symbol in unit.symbols {
		if symbol.kind == .Variable && symbol.name == "lv_value" {
			found = true
			scope := analyze.scope(&unit, symbol.scope)
			testing.expect(t, scope != nil)
			if scope != nil {
				testing.expect_value(t, scope.kind, analyze.Scope_Kind.Method)
			}
		}
	}
	testing.expect(t, found)
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
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
	testing.expect_value(t, reference_count(&unit, "lv_value", .Value, .Identifier), 2)
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
	testing.expect_value(t, selector_accesses, 0)
}

@(test)
dynamic_call_method_component_receivers_resolve_base_only :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///dynamic_call_method_component_receiver.abap",
		`
CLASS lcl_mapper DEFINITION.
ENDCLASS.

TYPES: BEGIN OF ty_pair,
         file_name_mapper TYPE REF TO lcl_mapper,
       END OF ty_pair.

FORM run.
  FIELD-SYMBOLS <ls_extension_mapper_pair> TYPE ty_pair.
  DATA lv_object TYPE string.
  CALL METHOD <ls_extension_mapper_pair>-file_name_mapper->('IF_AFF_FILE_NAME_MAPPER~GET_FILE_NAME_FROM_OBJECT')
    EXPORTING iv_object = lv_object.
ENDFORM.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	testing.expect_value(t, reference_count(&unit, "<ls_extension_mapper_pair>", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "lv_object", .Value, .Identifier), 1)
	testing.expect(t, !has_reference(&unit, "<ls_extension_mapper_pair>-file_name_mapper", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "file_name_mapper", .Value, .Identifier))

	found_target := false
	for arg in unit.named_arguments {
		if arg.name == "iv_object" &&
		   arg.target.kind == .Method &&
		   arg.target.base_name == "<ls_extension_mapper_pair>" &&
		   arg.target.method_name == "" &&
		   len(arg.target.receiver_path) == 1 &&
		   arg.target.receiver_path[0].name == "file_name_mapper" {
			found_target = true
		}
	}
	testing.expect(t, found_target)
}

@(test)
super_constructor_calls_are_not_field_accesses :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///super_constructor.abap",
		`
CLASS lcl_parent DEFINITION.
ENDCLASS.

CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
  PUBLIC SECTION.
    METHODS constructor IMPORTING previous TYPE i OPTIONAL.
ENDCLASS.

CLASS lcl_child IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    super->constructor( previous = previous ).
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	constructor_calls := 0
	for site in unit.call_sites {
		if site.target.kind == .Method &&
		   site.target.base_name == "super" &&
		   site.target.method_name == "constructor" &&
		   site.target.method_range.start < site.target.method_range.end {
			constructor_calls += 1
		}
	}
	testing.expect_value(t, constructor_calls, 2)
}

@(test)
me_and_super_method_receivers_resolve_in_instance_methods :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///me_super_receivers.abap",
		`
CLASS lcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS base.
ENDCLASS.

CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
  PUBLIC SECTION.
    METHODS own.
ENDCLASS.

CLASS lcl_parent IMPLEMENTATION.
  METHOD base.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_child IMPLEMENTATION.
  METHOD own.
    me->own( ).
    CALL METHOD me->own.
    super->base( ).
    CALL METHOD super->base.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	me_calls, super_calls := 0, 0
	for site in unit.call_sites {
		if site.target.kind != .Method {
			continue
		}
		if site.target.base_name == "me" && site.target.method_name == "own" {
			me_calls += 1
		}
		if site.target.base_name == "super" && site.target.method_name == "base" {
			super_calls += 1
		}
	}
	testing.expect_value(t, me_calls, 2)
	testing.expect_value(t, super_calls, 2)
}

@(test)
method_receiver_can_be_structure_component_object_ref :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///component_method_receiver.abap",
		`
INTERFACE lif_repo.
  METHODS get_selected_branch RETURNING VALUE(rv_branch) TYPE string.
ENDINTERFACE.

TYPES: BEGIN OF ty_merge,
         repo_online TYPE REF TO lif_repo,
       END OF ty_merge.

DATA ms_merge TYPE ty_merge.
DATA lv_branch TYPE string.

START-OF-SELECTION.
  lv_branch = ms_merge-repo_online->get_selected_branch( ).
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	found := false
	for site in unit.call_sites {
		if site.target.kind == .Method &&
		   site.target.base_name == "ms_merge" &&
		   site.target.method_name == "get_selected_branch" &&
		   len(site.target.receiver_path) == 1 &&
		   site.target.receiver_path[0].name == "repo_online" {
			found = true
		}
	}
	testing.expect(t, found)
}

@(test)
inline_selector_assignment_infers_field_type_same_pass :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///inline_selector_assignment.abap",
		`
TYPES: BEGIN OF ty_row,
         field TYPE string,
       END OF ty_row.

DATA ls_row TYPE ty_row.
DATA(lv_field) = ls_row-field.
`,
	)

	lv_field := analyze.find_symbol(&unit, "lv_field", .Variable)
	testing.expect(t, lv_field != nil)
	if lv_field != nil {
		testing.expect(t, lv_field.has_declared_type)
		testing.expect_value(t, lv_field.declared_type.base_name, "string")
	}
}

@(test)
inline_method_result_assignment_infers_return_type_same_pass :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///inline_method_result_assignment.abap",
		`
CLASS lcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS get_text RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS lcl_dep IMPLEMENTATION.
  METHOD get_text.
  ENDMETHOD.
ENDCLASS.

DATA lo_dep TYPE REF TO lcl_dep.
DATA(lv_text) = lo_dep->get_text( ).
`,
	)

	lv_text := analyze.find_symbol(&unit, "lv_text", .Variable)
	testing.expect(t, lv_text != nil)
	if lv_text != nil {
		testing.expect(t, lv_text.has_declared_type)
		testing.expect_value(t, lv_text.declared_type.base_name, "string")
	}
}

@(test)
method_result_call_argument_value_flow_uses_same_pass_return_type :: proc(t: ^testing.T) {
	source := `
CLASS lcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS get_text RETURNING VALUE(rv_text) TYPE string.
    METHODS consume IMPORTING iv_text TYPE string.
ENDCLASS.

CLASS lcl_dep IMPLEMENTATION.
  METHOD get_text.
  ENDMETHOD.
  METHOD consume.
  ENDMETHOD.
ENDCLASS.

DATA lo_dep TYPE REF TO lcl_dep.
lo_dep->consume( iv_text = lo_dep->get_text( ) ).
`
	unit := collect_test_unit(t, "file:///method_result_call_arg_flow.abap", source)
	arg_offset := find_text_last(source, "get_text")
	query := sem_query.facts(sem_query.semantic(&unit))
	operand := sem_query.fact_operand_at_offset(query, arg_offset)

	testing.expect(t, arg_offset >= 0)
	expect_operand(t, &unit, operand, .Value, "string")
}

@(test)
method_argument_can_be_object_attribute_structure_component :: proc(t: ^testing.T) {
	source := `
INTERFACE lif_user.
  METHODS is_favorite_repo
    IMPORTING iv_repo_key TYPE string
    RETURNING VALUE(rv_favorite) TYPE abap_bool.
ENDINTERFACE.

INTERFACE lif_repo.
  TYPES: BEGIN OF ty_repo,
           key TYPE string,
         END OF ty_repo.
  DATA ms_data TYPE ty_repo READ-ONLY.
ENDINTERFACE.

CLASS lcl_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_user RETURNING VALUE(ro_user) TYPE REF TO lif_user.
ENDCLASS.

TYPES ty_repo_list TYPE STANDARD TABLE OF REF TO lif_repo WITH DEFAULT KEY.

FORM run.
  DATA lt_repo_obj_list TYPE ty_repo_list.
  DATA lv_favorite TYPE abap_bool.
  FIELD-SYMBOLS <ls_repo> LIKE LINE OF lt_repo_obj_list.

  LOOP AT lt_repo_obj_list ASSIGNING <ls_repo>.
    lv_favorite = lcl_factory=>get_user( )->is_favorite_repo( <ls_repo>->ms_data-key ).
  ENDLOOP.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///method_arg_object_attr_component.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
method_argument_rejects_object_attribute_with_dash_selector :: proc(t: ^testing.T) {
	source := `
INTERFACE lif_user.
  METHODS is_favorite_repo
    IMPORTING iv_repo_key TYPE string
    RETURNING VALUE(rv_favorite) TYPE abap_bool.
ENDINTERFACE.

INTERFACE lif_repo.
  TYPES: BEGIN OF ty_repo,
           key TYPE string,
         END OF ty_repo.
  DATA ms_data TYPE ty_repo READ-ONLY.
ENDINTERFACE.

CLASS lcl_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_user RETURNING VALUE(ro_user) TYPE REF TO lif_user.
ENDCLASS.

TYPES ty_repo_list TYPE STANDARD TABLE OF REF TO lif_repo WITH DEFAULT KEY.

FORM run.
  DATA lt_repo_obj_list TYPE ty_repo_list.
  DATA lv_favorite TYPE abap_bool.
  FIELD-SYMBOLS <ls_repo> LIKE LINE OF lt_repo_obj_list.

  LOOP AT lt_repo_obj_list ASSIGNING <ls_repo>.
    lv_favorite = lcl_factory=>get_user( )->is_favorite_repo( <ls_repo>-ms_data-key ).
  ENDLOOP.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///method_arg_object_attr_bad_selector.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Unknown_Field))
}

@(test)
method_receiver_can_be_line_of_nested_table_ref_type :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///line_of_nested_table_ref_type.abap",
		`
INTERFACE lif_repo.
  METHODS get_package RETURNING VALUE(rv_package) TYPE string.
ENDINTERFACE.

INTERFACE lif_repo_srv.
  TYPES ty_repo_list TYPE STANDARD TABLE OF REF TO lif_repo WITH DEFAULT KEY.
ENDINTERFACE.

FIELD-SYMBOLS <repo> TYPE LINE OF lif_repo_srv=>ty_repo_list.
DATA lv_package TYPE string.

START-OF-SELECTION.
  lv_package = <repo>->get_package( ).
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
me_and_super_are_not_valid_outside_instance_methods :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///me_super_scope.abap",
		`
CLASS lcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS base.
ENDCLASS.

CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
  PUBLIC SECTION.
    METHODS own.
    CLASS-METHODS stat.
ENDCLASS.

CLASS lcl_parent IMPLEMENTATION.
  METHOD base.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_child IMPLEMENTATION.
  METHOD own.
  ENDMETHOD.

  METHOD stat.
    me->stat( ).
    super->base( ).
    CALL METHOD super->base.
  ENDMETHOD.
ENDCLASS.

FORM run.
  me->own( ).
  super->base( ).
ENDFORM.
`,
	)

	testing.expect(t, has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect_value(t, unresolved_reference_count(&unit, "me", .Value, .Identifier), 2)
	testing.expect_value(t, unresolved_reference_count(&unit, "super", .Value, .Identifier), 3)
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
interface_inherited_methods_resolve_without_alias :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///interface_inherited_method.abap",
		`
INTERFACE if_node.
  METHODS set_value IMPORTING value TYPE string.
ENDINTERFACE.

INTERFACE if_text.
  INTERFACES if_node.
ENDINTERFACE.

DATA li_text TYPE REF TO if_text.
li_text->set_value( value = 'x' ).
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
interface_qualified_instance_calls_do_not_treat_interface_as_field :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///interface_qualified_instance_call.abap",
		`
INTERFACE if_node.
  METHODS set_value IMPORTING value TYPE string.
ENDINTERFACE.

INTERFACE if_attr.
  INTERFACES if_node.
ENDINTERFACE.

DATA li_attr TYPE REF TO if_attr.
li_attr->if_node~set_value( value = 'x' ).
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
interface_qualified_attribute_access_resolves_member_structure :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///qualified_attribute_access.abap",
		source = `
CLASS lcl_exception DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_t100_message.
ENDCLASS.

DATA lx_error TYPE REF TO lcl_exception.
DATA lv_msgid TYPE string.
lv_msgid = lx_error->lif_t100_message~t100key-msgid.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "file:///lif_t100_message.abap",
			source = `
INTERFACE lif_t100_message.
  DATA t100key TYPE scx_t100key.
ENDINTERFACE.
`,
			mode = .Dependency_Interface,
		},
		{
			uri = "file:///scx_t100key.abap",
			source = `
TYPES: BEGIN OF scx_t100key,
         msgid TYPE string,
         msgno TYPE string,
       END OF scx_t100key.
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
interface_qualified_attribute_access_requires_exposed_interface :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///qualified_attribute_access_invalid.abap",
		source = `
CLASS lcl_exception DEFINITION.
ENDCLASS.

DATA lx_error TYPE REF TO lcl_exception.
DATA lv_msgid TYPE string.
lv_msgid = lx_error->lif_t100_message~t100key-msgid.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "file:///lif_t100_message.abap",
			source = `
INTERFACE lif_t100_message.
  DATA t100key TYPE scx_t100key.
ENDINTERFACE.
`,
			mode = .Dependency_Interface,
		},
		{
			uri = "file:///scx_t100key.abap",
			source = `
TYPES: BEGIN OF scx_t100key,
         msgid TYPE string,
       END OF scx_t100key.
`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, root != nil && has_diagnostic(root, .Unknown_Field))
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
assign_dereferenced_data_reference_does_not_validate_star_field :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///assign_deref_data_ref.abap",
		`
DATA lr_data TYPE REF TO data.
FIELD-SYMBOLS <fs> TYPE any.
ASSIGN lr_data->* TO <fs>.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	deref_seen := false
	for access in unit.field_accesses {
		if access.base_name == "lr_data" &&
		   len(access.field_path) == 1 &&
		   access.field_path[0].name == "*" &&
		   access.field_path[0].deref {
			deref_seen = true
		}
	}
	testing.expect(t, deref_seen)
}

@(test)
like_line_of_dereferenced_ref_table_keeps_line_structure :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///like_line_of_ref_table.abap",
		`
INTERFACE lif_types.
  TYPES: BEGIN OF ty_node,
           path TYPE string,
           name TYPE string,
         END OF ty_node.
  TYPES ty_nodes_ts TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY.
ENDINTERFACE.

DATA mr_source_tree TYPE REF TO lif_types=>ty_nodes_ts.

FORM run.
  FIELD-SYMBOLS <item> LIKE LINE OF mr_source_tree->*.
  DATA ls_renamed_node LIKE <item>.

  LOOP AT mr_source_tree->* ASSIGNING <item> WHERE path = 'x'.
    ls_renamed_node = <item>.
    ls_renamed_node-path = 'y'.
    ls_renamed_node-name = <item>-name.
  ENDLOOP.
ENDFORM.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
like_line_of_table_ref_line_keeps_reference_structure :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///like_line_of_table_ref_line.abap",
		`
INTERFACE lif_types.
  TYPES: BEGIN OF ty_node,
           type TYPE string,
           children TYPE i,
         END OF ty_node.
  TYPES ty_stack_tt TYPE STANDARD TABLE OF REF TO ty_node WITH DEFAULT KEY.
ENDINTERFACE.

DATA mt_stack TYPE lif_types=>ty_stack_tt.

FORM run.
  DATA lr_stack_top LIKE LINE OF mt_stack.
  lr_stack_top->children = lr_stack_top->children + 1.
  IF lr_stack_top->type = 'array'.
  ENDIF.
ENDFORM.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
like_line_of_attribute_table_ref_line_keeps_reference_structure :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///like_line_of_attribute_table_ref_line.abap",
		`
INTERFACE lif_types.
  TYPES: BEGIN OF ty_node,
           type TYPE string,
           children TYPE i,
         END OF ty_node.
ENDINTERFACE.

CLASS lcl_parser DEFINITION.
  PRIVATE SECTION.
    TYPES ty_stack_tt TYPE STANDARD TABLE OF REF TO lif_types=>ty_node WITH DEFAULT KEY.
    DATA mt_stack TYPE ty_stack_tt.
    METHODS run.
ENDCLASS.

CLASS lcl_parser IMPLEMENTATION.
  METHOD run.
    DATA lr_stack_top LIKE LINE OF mt_stack.
    lr_stack_top->children = lr_stack_top->children + 1.
    IF lr_stack_top->type = 'array'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
like_data_reference_parameter_keeps_reference_structure :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///like_data_reference_parameter.abap",
		`
INTERFACE lif_types.
  TYPES: BEGIN OF ty_node,
           type TYPE string,
           children TYPE i,
         END OF ty_node.
ENDINTERFACE.

CLASS lcl_json DEFINITION.
  PRIVATE SECTION.
    METHODS delete_subtree
      IMPORTING ir_parent TYPE REF TO lif_types=>ty_node OPTIONAL.
    METHODS prove_path_exists
      RETURNING VALUE(rr_end_node) TYPE REF TO lif_types=>ty_node.
ENDCLASS.

CLASS lcl_json IMPLEMENTATION.
  METHOD delete_subtree.
    DATA lr_parent LIKE ir_parent.
    ir_parent->children = ir_parent->children - 1.
    lr_parent = ir_parent.
    lr_parent->children = lr_parent->children - 1.
  ENDMETHOD.
  METHOD prove_path_exists.
    DATA lr_node_parent LIKE rr_end_node.
    lr_node_parent = rr_end_node.
    lr_node_parent->children = lr_node_parent->children + 1.
    IF lr_node_parent->type = 'array'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
like_line_of_object_table_attribute_keeps_line_structure :: proc(t: ^testing.T) {
	valid_source := `
CLASS lcl_map DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_entry,
             k TYPE string,
             v TYPE string,
           END OF ty_entry.
    TYPES ty_entries TYPE SORTED TABLE OF ty_entry WITH UNIQUE KEY k.
    DATA mt_entries TYPE ty_entries.
ENDCLASS.

DATA lo_map TYPE REF TO lcl_map.
FIELD-SYMBOLS <entry> LIKE LINE OF lo_map->mt_entries.
<entry>-k = 'x'.
`
	valid := collect_test_unit(t, "file:///like_line_of_object_table_attr.abap", valid_source)
	invalid := collect_test_unit(
		t,
		"file:///like_line_of_object_table_attr_dash.abap",
		`
CLASS lcl_map DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_entry,
             k TYPE string,
           END OF ty_entry.
    TYPES ty_entries TYPE STANDARD TABLE OF ty_entry WITH DEFAULT KEY.
    DATA mt_entries TYPE ty_entries.
ENDCLASS.

DATA lo_map TYPE REF TO lcl_map.
FIELD-SYMBOLS <entry> LIKE LINE OF lo_map-mt_entries.
<entry>-k = 'x'.
`,
	)

	testing.expect(t, !has_diagnostic(&valid, .Unknown_Field))
	testing.expect(t, has_diagnostic(&invalid, .Unknown_Field))

	fact_query := sem_query.facts(sem_query.semantic(&valid))
	field_offset := find_text(valid_source, "<entry>-k")
	testing.expect(t, field_offset >= 0)
	field_fact := sem_query.fact_expression_fact_at_offset(fact_query, field_offset + len("<entry>-"))
	testing.expect(t, field_fact != nil)
	testing.expect_value(t, field_fact.kind, analyze.Expression_Fact_Kind.Selector)
	testing.expect(t, field_fact.type_fact.has_declared_type)
	testing.expect_value(t, field_fact.type_fact.declared_type.base_name, "string")
}

@(test)
like_line_of_dependency_object_table_attribute_keeps_line_structure :: proc(t: ^testing.T) {
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/global-class/cl_abap_zip.abap",
			mode = .Dependency_Interface,
			source = `
CLASS cl_abap_zip DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_file,
             name TYPE string,
           END OF t_file.
    TYPES t_files TYPE STANDARD TABLE OF t_file WITH DEFAULT KEY.
    DATA files TYPE t_files READ-ONLY.
ENDCLASS.
`,
		},
	}
	valid_target := analyze.Source_Input {
		uri = "file:///zip_line_valid.abap",
		source = `
DATA lo_zip TYPE REF TO cl_abap_zip.
FIELD-SYMBOLS <file> LIKE LINE OF lo_zip->files.
<file>-name = 'x'.
`,
	}
	invalid_target := analyze.Source_Input {
		uri = "file:///zip_line_invalid.abap",
		source = `
DATA lo_zip TYPE REF TO cl_abap_zip.
FIELD-SYMBOLS <file> LIKE LINE OF lo_zip->files.
<file>-missing = 'x'.
`,
	}

	valid_project := analyze_project_dependencies_test(t, valid_target, dependencies[:])
	invalid_project := analyze_project_dependencies_test(t, invalid_target, dependencies[:])
	valid := analyze.project_unit_by_uri(&valid_project, valid_target.uri)
	invalid := analyze.project_unit_by_uri(&invalid_project, invalid_target.uri)

	testing.expect(t, valid != nil && !has_diagnostic(valid, .Unknown_Field))
	testing.expect(t, invalid != nil && has_diagnostic(invalid, .Unknown_Field))
}

@(test)
unknown_receiver_type_suppresses_field_cascade :: proc(t: ^testing.T) {
	missing_base := collect_test_unit(
		t,
		"file:///unknown_receiver_base.abap",
		`
FORM run.
  zmissing_node-path = 'x'.
ENDFORM.
`,
	)
	missing_type_pool_constant := collect_test_unit(
		t,
		"file:///unknown_type_pool_constant.abap",
		`
DATA lv_version TYPE c.
lv_version = sews_c_vif_version-all.
`,
	)
	missing := collect_test_unit(
		t,
		"file:///unknown_receiver_type.abap",
		`
FORM run.
  DATA ls_node TYPE zmissing_node.
  ls_node-path = 'x'.
ENDFORM.
`,
	)
	generic := collect_test_unit(
		t,
		"file:///generic_receiver_type.abap",
		`
FORM run.
  FIELD-SYMBOLS <node> TYPE any.
  <node>-path = 'x'.
ENDFORM.
`,
	)
	unknown_line_type := collect_test_unit(
		t,
		"file:///unknown_line_type_component.abap",
		`
FORM run.
  DATA ls_data TYPE zmissing_db_data.
  FIELD-SYMBOLS <row> LIKE LINE OF ls_data-sproxhdr.
  CLEAR <row>-created_by.
ENDFORM.
`,
	)
	unknown_table_line_type := collect_test_unit(
		t,
		"file:///unknown_table_line_type.abap",
		`
FORM run.
  DATA lt_messages TYPE zmissing_message_tab.
  DATA ls_message LIKE LINE OF lt_messages.
  FIELD-SYMBOLS <message> LIKE LINE OF lt_messages.
  CLEAR ls_message-mtext.
  CLEAR <message>-mtext.
ENDFORM.
`,
	)

	testing.expect(t, has_diagnostic(&missing_base, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&missing_base, .Unknown_Field))
	testing.expect(t, has_diagnostic(&missing_type_pool_constant, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&missing_type_pool_constant, .Unknown_Field))
	testing.expect(t, has_diagnostic(&missing, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&missing, .Unknown_Field))
	testing.expect(t, !has_diagnostic(&generic, .Unknown_Field))
	testing.expect(t, has_diagnostic(&unknown_line_type, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unknown_line_type, .Unknown_Field))
	testing.expect(t, has_diagnostic(&unknown_table_line_type, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unknown_table_line_type, .Unknown_Field))
}

@(test)
loop_assigning_inline_field_symbol_infers_line_shape :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_row,
           created_by TYPE string,
         END OF ty_row.
  DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
  LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row>) WHERE created_by IS NOT INITIAL.
    CLEAR <row>-created_by.
  ENDLOOP.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///loop_inline_field_symbol.abap", source)

	testing.expect(t, has_symbol(&unit, .Field_Symbol, "<row>"))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
dereference_operator_requires_data_reference :: proc(t: ^testing.T) {
	generic_any := collect_test_unit(
		t,
		"file:///assign_deref_generic_any.abap",
		`
FORM run USING iv_data TYPE any.
  FIELD-SYMBOLS <fs> TYPE any.
  ASSIGN iv_data->* TO <fs>.
ENDFORM.
`,
	)
	nested_type_ref := collect_test_unit(
		t,
		"file:///assign_deref_nested_type_ref.abap",
		`
INTERFACE lif_types.
  TYPES: BEGIN OF ty_row,
           name TYPE string,
         END OF ty_row.
ENDINTERFACE.
DATA lr_row TYPE REF TO lif_types=>ty_row.
FIELD-SYMBOLS <fs> TYPE any.
ASSIGN lr_row->* TO <fs>.
`,
	)
	non_ref := collect_test_unit(
		t,
		"file:///assign_deref_non_ref.abap",
		`
TYPES: BEGIN OF ty_row,
         name TYPE string,
       END OF ty_row.
DATA ls_row TYPE ty_row.
FIELD-SYMBOLS <fs> TYPE any.
ASSIGN ls_row->* TO <fs>.
`,
	)
	object_ref := collect_test_unit(
		t,
		"file:///assign_deref_object_ref.abap",
		`
CLASS lcl DEFINITION.
ENDCLASS.
DATA lo_obj TYPE REF TO lcl.
FIELD-SYMBOLS <fs> TYPE any.
ASSIGN lo_obj->* TO <fs>.
`,
	)

	testing.expect(t, !has_diagnostic(&generic_any, .Unknown_Field))
	testing.expect(t, !has_diagnostic(&nested_type_ref, .Unknown_Field))
	testing.expect(t, has_diagnostic(&non_ref, .Unknown_Field))
	testing.expect(t, has_diagnostic(&object_ref, .Unknown_Field))
}

@(test)
delete_adjacent_duplicates_comparing_uses_table_line_fields :: proc(t: ^testing.T) {
	source := `
CLASS lcl_table DEFINITION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_nested,
             part TYPE string,
           END OF ty_nested.
    TYPES: BEGIN OF ty_row,
             tobj_name TYPE string,
             tobjkey TYPE string,
             nested TYPE ty_nested,
           END OF ty_row.
    DATA mt_object_table TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
    METHODS run.
ENDCLASS.
CLASS lcl_table IMPLEMENTATION.
  METHOD run.
    FIELD-SYMBOLS <row> LIKE LINE OF mt_object_table.
    DELETE ADJACENT DUPLICATES FROM mt_object_table COMPARING tobj_name tobjkey nested-part.
    LOOP AT mt_object_table ASSIGNING <row>.
      DATA lv_name TYPE string.
      lv_name = <row>-tobj_name.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///delete_adjacent_duplicates_comparing.abap", source)

	testing.expect(t, has_reference(&unit, "mt_object_table", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "tobj_name", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "tobjkey", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "nested", .Value, .Identifier))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))

	tobj_name_seen := false
	tobjkey_seen := false
	nested_seen := false
	for access in unit.field_accesses {
		if access.base_name != "mt_object_table" {
			continue
		}
		if len(access.field_path) == 1 && access.field_path[0].name == "tobj_name" {
			tobj_name_seen = true
		}
		if len(access.field_path) == 1 && access.field_path[0].name == "tobjkey" {
			tobjkey_seen = true
		}
		if len(access.field_path) == 2 &&
		   access.field_path[0].name == "nested" &&
		   access.field_path[1].name == "part" {
			nested_seen = true
		}
	}
	testing.expect(t, tobj_name_seen)
	testing.expect(t, tobjkey_seen)
	testing.expect(t, nested_seen)
}

@(test)
read_table_table_line_key_is_pseudo_component :: proc(t: ^testing.T) {
	source := `
INTERFACE lif_defs.
  TYPES ty_languages TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
ENDINTERFACE.
CLASS lcl_table DEFINITION.
  PRIVATE SECTION.
    METHODS run.
ENDCLASS.
CLASS lcl_table IMPLEMENTATION.
  METHOD run.
    DATA lt_i18n_langs TYPE lif_defs=>ty_languages.
    READ TABLE lt_i18n_langs WITH KEY table_line = '*' TRANSPORTING NO FIELDS.
  ENDMETHOD.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///read_table_table_line_key.abap", source)

	testing.expect(t, has_reference(&unit, "lt_i18n_langs", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "table_line", .Value, .Identifier))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
read_table_nested_key_components_resolve_against_table_line :: proc(t: ^testing.T) {
	source := `
INTERFACE lif_defs.
  TYPES: BEGIN OF ty_item,
           obj_type TYPE string,
           obj_name TYPE string,
         END OF ty_item.
  TYPES: BEGIN OF ty_status,
           item TYPE ty_item,
         END OF ty_status.
  TYPES ty_statuses TYPE SORTED TABLE OF ty_status WITH UNIQUE KEY item-obj_type item-obj_name.
ENDINTERFACE.
CLASS lcl_table DEFINITION.
  PRIVATE SECTION.
    METHODS run.
ENDCLASS.
CLASS lcl_table IMPLEMENTATION.
  METHOD run.
    DATA rt_item_status TYPE lif_defs=>ty_statuses.
    DATA ls_item_status TYPE lif_defs=>ty_status.
    DATA lr_item_status TYPE REF TO lif_defs=>ty_status.
    READ TABLE rt_item_status REFERENCE INTO lr_item_status
      WITH KEY item-obj_type = ls_item_status-item-obj_type
               item-obj_name = ls_item_status-item-obj_name.
  ENDMETHOD.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///read_table_nested_key.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	obj_name_seen := false
	for access in unit.field_accesses {
		if access.base_name == "rt_item_status" &&
		   len(access.field_path) == 2 &&
		   access.field_path[0].name == "item" &&
		   access.field_path[1].name == "obj_name" {
			obj_name_seen = true
		}
	}
	testing.expect(t, obj_name_seen)
}

@(test)
read_table_table_line_reference_key_resolves_line_attribute :: proc(t: ^testing.T) {
	source := `
INTERFACE lif_permission.
  DATA package_interface_name TYPE string READ-ONLY.
ENDINTERFACE.
TYPES ty_permission_ref TYPE REF TO lif_permission.
TYPES ty_permissions TYPE STANDARD TABLE OF ty_permission_ref WITH DEFAULT KEY.
DATA lt_permissions TYPE ty_permissions.
DATA lv_name TYPE string.
READ TABLE lt_permissions WITH KEY table_line->package_interface_name = lv_name TRANSPORTING NO FIELDS.
`
	unit := collect_test_unit(t, "file:///read_table_table_line_ref_key.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	attribute_seen := false
	for access in unit.field_accesses {
		if access.base_name == "lt_permissions" &&
		   len(access.field_path) == 1 &&
		   access.field_path[0].name == "package_interface_name" &&
		   access.field_path[0].selector == .Arrow {
			attribute_seen = true
		}
	}
	testing.expect(t, attribute_seen)
}

@(test)
insert_into_table_data_ref_component_resolves_struct_field :: proc(t: ^testing.T) {
	valid_source := `
INTERFACE lif_log.
  TYPES: BEGIN OF ty_msg,
           text TYPE string,
         END OF ty_msg.
  TYPES ty_msgs TYPE STANDARD TABLE OF ty_msg WITH DEFAULT KEY.
  TYPES: BEGIN OF ty_status,
           messages TYPE ty_msgs,
         END OF ty_status.
ENDINTERFACE.
DATA ls_msg TYPE lif_log=>ty_msg.
DATA lr_status TYPE REF TO lif_log=>ty_status.
INSERT ls_msg INTO TABLE lr_status->messages.
`
	invalid_source := `
INTERFACE lif_log.
  TYPES: BEGIN OF ty_msg,
           text TYPE string,
         END OF ty_msg.
  TYPES ty_msgs TYPE STANDARD TABLE OF ty_msg WITH DEFAULT KEY.
  TYPES: BEGIN OF ty_status,
           messages TYPE ty_msgs,
         END OF ty_status.
ENDINTERFACE.
DATA ls_msg TYPE lif_log=>ty_msg.
DATA lr_status TYPE REF TO lif_log=>ty_status.
INSERT ls_msg INTO TABLE lr_status->missing.
`
	valid := collect_test_unit(t, "file:///insert_data_ref_component.abap", valid_source)
	invalid := collect_test_unit(t, "file:///insert_data_ref_component_invalid.abap", invalid_source)

	testing.expect(t, !has_diagnostic(&valid, .Unknown_Field))
	testing.expect(t, has_diagnostic(&invalid, .Unknown_Field))
	messages_seen := false
	for access in valid.field_accesses {
		if access.base_name == "lr_status" &&
		   len(access.field_path) == 1 &&
		   access.field_path[0].name == "messages" &&
		   access.field_path[0].selector == .Arrow {
			messages_seen = true
		}
	}
	testing.expect(t, messages_seen)
}

@(test)
read_table_nested_key_on_table_component_keeps_source_path :: proc(t: ^testing.T) {
	source := `
INTERFACE lif_defs.
  TYPES: BEGIN OF ty_item,
           obj_type TYPE string,
           obj_name TYPE string,
         END OF ty_item.
  TYPES: BEGIN OF ty_file,
           filename TYPE string,
         END OF ty_file.
  TYPES: BEGIN OF ty_file_item,
           item TYPE ty_item,
           file TYPE ty_file,
         END OF ty_file_item.
  TYPES ty_file_items TYPE STANDARD TABLE OF ty_file_item WITH DEFAULT KEY.
  TYPES: BEGIN OF ty_stage,
           local TYPE ty_file_items,
         END OF ty_stage.
ENDINTERFACE.
CLASS lcl_table DEFINITION.
  PRIVATE SECTION.
    METHODS run IMPORTING is_stage TYPE lif_defs=>ty_stage.
ENDCLASS.
CLASS lcl_table IMPLEMENTATION.
  METHOD run.
    DATA ls_local TYPE lif_defs=>ty_file_item.
    DATA lv_name TYPE string.
    READ TABLE is_stage-local INTO ls_local
      WITH KEY item-obj_name = lv_name
               file-filename = lv_name.
  ENDMETHOD.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///read_table_nested_key_on_component.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	obj_name_seen := false
	for access in unit.field_accesses {
		if access.base_name == "is_stage" &&
		   len(access.field_path) == 3 &&
		   access.field_path[0].name == "local" &&
		   access.field_path[1].name == "item" &&
		   access.field_path[2].name == "obj_name" {
			obj_name_seen = true
		}
	}
	testing.expect(t, obj_name_seen)
}

@(test)
delete_adjacent_duplicates_comparing_all_fields_is_clause :: proc(t: ^testing.T) {
	source := `
CLASS lcl_table DEFINITION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_row,
             component TYPE string,
           END OF ty_row.
    DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
    METHODS run.
ENDCLASS.
CLASS lcl_table IMPLEMENTATION.
  METHOD run.
    DELETE ADJACENT DUPLICATES FROM lt_rows COMPARING ALL FIELDS.
  ENDMETHOD.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///delete_adjacent_duplicates_all_fields.abap", source)

	testing.expect(t, has_reference(&unit, "lt_rows", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "all", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "fields", .Value, .Identifier))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
}

@(test)
data_cluster_media_collect_refs_without_keyword_refs :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///data_cluster_media.abap",
		`
REPORT zmain.
DATA lv_xstr TYPE xstring.
DATA lt_cluster TYPE TABLE OF string.
DATA lv_id TYPE string.
DATA lv_client TYPE string.
DATA ls_payload TYPE string.
DATA ls_indx TYPE string.
IMPORT payload TO ls_payload FROM MEMORY ID lv_id.
EXPORT payload FROM ls_payload TO MEMORY ID lv_id.
IMPORT payload = ls_payload FROM DATA BUFFER lv_xstr.
EXPORT payload = ls_payload TO DATA BUFFER lv_xstr.
IMPORT payload = ls_payload FROM INTERNAL TABLE lt_cluster.
EXPORT payload = ls_payload TO INTERNAL TABLE lt_cluster.
IMPORT payload = ls_payload FROM DATABASE demo_indx_blob(sc) TO ls_indx CLIENT lv_client ID lv_id.
EXPORT payload = ls_payload TO DATABASE demo_indx_blob(sc) FROM ls_indx CLIENT lv_client ID lv_id.
IMPORT payload = ls_payload FROM SHARED MEMORY demo_indx_blob(sc) TO ls_indx CLIENT lv_client ID lv_id.
EXPORT payload = ls_payload TO SHARED BUFFER demo_indx_blob(sc) FROM ls_indx CLIENT lv_client ID lv_id.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, has_reference(&unit, "lv_xstr", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lt_cluster", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_id", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_client", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "ls_indx", .Value, .Identifier))
	keywords := [?]string{"from", "to", "data", "buffer", "internal", "table", "memory", "id", "database", "shared", "client"}
	for keyword in keywords {
		testing.expect(t, !has_reference(&unit, keyword, .Value, .Identifier))
	}
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
inline_open_sql_table_target_infers_row_fields :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql_inline_table_row.abap",
		`
DATA lv_carrid TYPE string.
SELECT carrid, carrname
  FROM scarr
  INTO TABLE @DATA(lt_scarr).
READ TABLE lt_scarr INTO DATA(ls_scarr) INDEX 1.
lv_carrid = ls_scarr-carrid.
`,
	)

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, !has_diagnostic(&unit, .Invalid_Generic_Table_Type))
}

@(test)
inline_open_sql_scalar_target_infers_aggregate_alias_fields :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///sql_inline_aggregate_row.abap",
		source = `
TYPES: BEGIN OF ty_obj_ids,
         objid TYPE string,
       END OF ty_obj_ids.
DATA ls_obj_ids TYPE ty_obj_ids.
SELECT COUNT( DISTINCT rel~evtid ) AS total_events,
       COUNT( DISTINCT evt~evtid ) AS active_events
  FROM zrel AS rel
  LEFT OUTER JOIN zevt AS evt ON rel~evtid = evt~evtid
  WHERE rel~objid = @ls_obj_ids-objid
  INTO @DATA(ls_event_summary).
IF ls_event_summary-total_events > 0 AND
   ls_event_summary-total_events > ls_event_summary-active_events.
ENDIF.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{uri = "abapls-cache:/ddic-table/zrel.abap", source = `
TYPES: BEGIN OF zrel,
         evtid TYPE string,
         objid TYPE string,
       END OF zrel.
`},
		{uri = "abapls-cache:/ddic-table/zevt.abap", source = `
TYPES: BEGIN OF zevt,
         evtid TYPE string,
       END OF zevt.
`},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	unit := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, unit != nil)
	if unit == nil {
		return
	}
	testing.expect(t, !has_diagnostic(unit, .Unknown_Field))
	result := analyze.find_symbol(unit, "ls_event_summary", .Variable)
	testing.expect(t, result != nil && result.structure != analyze.INVALID_STRUCTURE_ID)
	if result != nil && result.structure != analyze.INVALID_STRUCTURE_ID {
		st := analyze.structure(unit, result.structure)
		fields := [?]string{"total_events", "active_events"}
		testing.expect(t, field_names_match(st, fields[:]))
	}
}

@(test)
inline_open_sql_star_table_target_uses_source_structure :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://sql_inline_star_table.abap",
		source = `REPORT z_sql_inline_star_table.
SELECT * FROM zrows INTO TABLE @DATA(lt_rows).
READ TABLE lt_rows INTO DATA(ls_row) INDEX 1.
DATA lv_text TYPE string.
lv_text = ls_row-text.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table/zrows.abap",
			source = `TYPES: BEGIN OF zrows,
         id TYPE i,
         text TYPE string,
       END OF zrows.`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	if root != nil {
		testing.expect(t, !has_diagnostic(root, .Invalid_Generic_Table_Type))
		testing.expect(t, !has_diagnostic(root, .Unknown_Field))

		rows := analyze.find_symbol(root, "lt_rows", .Variable)
		testing.expect(t, rows != nil && rows.structure != analyze.INVALID_STRUCTURE_ID)
		if rows != nil && rows.structure != analyze.INVALID_STRUCTURE_ID {
			st := analyze.structure(root, rows.structure)
			fields := [?]string{"id", "text"}
			testing.expect(t, field_names_match(st, fields[:]))
		}
	}
}

@(test)
inline_open_sql_join_star_table_target_combines_source_structures :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://sql_inline_join_star_table.abap",
		source = `REPORT z_sql_inline_join_star_table.
SELECT *
  FROM zhead AS h
  INNER JOIN zitem AS i ON i~head_id = h~id
  INTO TABLE @DATA(lt_rows).
READ TABLE lt_rows INTO DATA(ls_row) INDEX 1.
DATA lv_text TYPE string.
DATA lv_qty TYPE i.
lv_text = ls_row-text.
lv_qty = ls_row-qty.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table/zhead.abap",
			source = `TYPES: BEGIN OF zhead,
         id TYPE i,
         text TYPE string,
       END OF zhead.`,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/ddic-table/zitem.abap",
			source = `TYPES: BEGIN OF zitem,
         head_id TYPE i,
         qty TYPE i,
       END OF zitem.`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	if root != nil {
		testing.expect(t, !has_diagnostic(root, .Invalid_Generic_Table_Type))
		testing.expect(t, !has_diagnostic(root, .Unknown_Field))

		rows := analyze.find_symbol(root, "lt_rows", .Variable)
		testing.expect(t, rows != nil && rows.structure != analyze.INVALID_STRUCTURE_ID)
		if rows != nil && rows.structure != analyze.INVALID_STRUCTURE_ID {
			st := analyze.structure(root, rows.structure)
			fields := [?]string{"id", "text", "head_id", "qty"}
			testing.expect(t, field_names_match(st, fields[:]))
		}
	}
}

@(test)
inline_open_sql_table_target_with_unresolved_source_is_not_generic :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql_inline_unknown_table.abap",
		`TYPES string_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.
SELECT * FROM zmissing INTO TABLE @DATA(lt_rows).
DATA(lt_out) = VALUE string_table( FOR ls_row IN lt_rows ( '' ) ).`,
	)

	testing.expect(t, has_diagnostic(&unit, .Unresolved_Open_Sql_Source))
	testing.expect(t, !has_diagnostic(&unit, .Invalid_Generic_Table_Type))
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
SELECT a~carrid AS carrier,
       COUNT( DISTINCT b~connid ) AS total,
       COUNT( ALL b~connid ) AS all_total
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
	testing.expect(t, sql_projection_alias_present(&unit, "all_total", .Aggregate))
	testing.expect(t, sql_name_ref_present(&unit, "count", .Aggregate))
	testing.expect(t, sql_qualified_ref_present(&unit, "a", "carrid", .Qualified_Column))
	testing.expect(t, sql_qualified_ref_present(&unit, "b", "connid", .Qualified_Column))
	testing.expect(t, sql_name_ref_present(&unit, "*", .Star))
	testing.expect(t, !sql_name_ref_present(&unit, "distinct", .Column))
	testing.expect(t, !sql_name_ref_present(&unit, "all", .Column))
	testing.expect(t, sql_predicate_present(&unit, .Join_On))
}

@(test)
collects_parenthesized_open_sql_where_as_static_predicate :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql_parenthesized_where.abap",
		`
DATA rv_transport TYPE string.
DATA iv_program_id TYPE string.
DATA iv_object_type TYPE string.
DATA iv_object_name TYPE string.

SELECT SINGLE a~trkorr FROM e070 AS a JOIN e071 AS b ON a~trkorr = b~trkorr
  INTO rv_transport
  WHERE ( a~trstatus = 'D' OR a~trstatus = 'L' )
    AND a~trfunction <> 'G'
    AND NOT ( a~trfunction = 'F' AND ( a~tarsystem = '' OR a~tarsystem = 'SAP' ) )
    AND b~pgmid = iv_program_id AND b~object = iv_object_type AND b~obj_name = iv_object_name.
`,
	)

	testing.expect_value(t, len(unit.sql_queries), 1)
	testing.expect(t, !(.Has_Dynamic_Where in unit.sql_queries[0].flags))
	testing.expect(t, sql_predicate_present(&unit, .Where))
	testing.expect(t, !sql_predicate_present(&unit, .Dynamic_Where))
	testing.expect(t, sql_source_alias_present(&unit, "e070", "a", .From))
	testing.expect(t, sql_source_alias_present(&unit, "e071", "b", .Join))
	testing.expect(t, sql_qualified_ref_present(&unit, "a", "trstatus", .Qualified_Column))
	testing.expect(t, sql_qualified_ref_present(&unit, "b", "obj_name", .Qualified_Column))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
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
open_sql_for_all_entries_table_line_is_scalar_driver_pseudo_component :: proc(t: ^testing.T) {
	valid := `
TYPES: BEGIN OF dokil,
         id TYPE string,
         object TYPE string,
       END OF dokil.
DATA c_longtext_id_wc TYPE string.
TYPES ty_object_tt TYPE STANDARD TABLE OF dokil-object WITH DEFAULT KEY.
DATA lt_object TYPE STANDARD TABLE OF dokil-object WITH DEFAULT KEY.
DATA lt_alias TYPE ty_object_tt.
DATA rt_list TYPE STANDARD TABLE OF dokil-object WITH DEFAULT KEY.
DATA lt_list LIKE rt_list.
DATA lt_dokil TYPE STANDARD TABLE OF dokil WITH DEFAULT KEY.
SELECT * FROM dokil INTO TABLE lt_dokil
  FOR ALL ENTRIES IN lt_object
  WHERE id = c_longtext_id_wc AND object = lt_object-table_line.
SELECT * FROM dokil INTO TABLE lt_dokil
  FOR ALL ENTRIES IN lt_alias
  WHERE object = lt_alias-table_line.
SELECT * FROM dokil INTO TABLE lt_dokil
  FOR ALL ENTRIES IN lt_list
  WHERE object = lt_list-table_line.
`
	invalid := `
TYPES: BEGIN OF dokil,
         id TYPE string,
         object TYPE string,
       END OF dokil.
DATA c_longtext_id_wc TYPE string.
DATA lt_object TYPE STANDARD TABLE OF dokil-object WITH DEFAULT KEY.
DATA lt_other TYPE STANDARD TABLE OF dokil-object WITH DEFAULT KEY.
DATA lt_dokil TYPE STANDARD TABLE OF dokil WITH DEFAULT KEY.
SELECT * FROM dokil INTO TABLE lt_dokil
  FOR ALL ENTRIES IN lt_object
  WHERE object = lt_other-table_line.
SELECT * FROM dokil INTO TABLE lt_dokil
  WHERE object = lt_object-table_line.
`
	interface_like_driver := `
TYPES: BEGIN OF dokil,
         id TYPE string,
         object TYPE string,
       END OF dokil.
INTERFACE lif_package.
  TYPES ty_object_tt TYPE STANDARD TABLE OF dokil-object WITH DEFAULT KEY.
  METHODS list RETURNING VALUE(rt_list) TYPE ty_object_tt.
ENDINTERFACE.
CLASS lcl_package DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_package.
ENDCLASS.
CLASS lcl_package IMPLEMENTATION.
  METHOD lif_package~list.
    DATA lt_list LIKE rt_list.
    DATA lt_dokil TYPE STANDARD TABLE OF dokil WITH DEFAULT KEY.
    SELECT * FROM dokil INTO TABLE lt_dokil
      FOR ALL ENTRIES IN lt_list
      WHERE object = lt_list-table_line.
  ENDMETHOD.
ENDCLASS.
`

	valid_unit := collect_test_unit(t, "file:///sql_fae_table_line.abap", valid)
	invalid_unit := collect_test_unit(t, "file:///sql_bad_fae_table_line.abap", invalid)
	interface_like_driver_unit := collect_test_unit(t, "file:///sql_fae_interface_like_table_line.abap", interface_like_driver)

	testing.expect(t, !has_reference(&valid_unit, "table_line", .Value, .Identifier))
	testing.expect(t, !has_diagnostic(&valid_unit, .Unknown_Field))
	testing.expect(t, !has_diagnostic(&interface_like_driver_unit, .Unknown_Field))
	testing.expect(t, has_diagnostic(&invalid_unit, .Unknown_Field))
}

@(test)
open_sql_delete_where_classic_host_attribute_is_not_column :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://ZMAIN.abap",
		source = `
CLASS lcl_repo DEFINITION.
  PRIVATE SECTION.
    DATA mv_object TYPE string.
    METHODS delete_object.
ENDCLASS.

CLASS lcl_repo IMPLEMENTATION.
  METHOD delete_object.
    DELETE FROM tcdobs WHERE object = mv_object.
  ENDMETHOD.
ENDCLASS.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table/tcdobs.abap",
			source = `TYPES: BEGIN OF tcdobs,
         object TYPE string,
       END OF tcdobs.`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	if root != nil {
		testing.expect(t, !sql_name_ref_present(root, "mv_object", .Column))
		testing.expect(t, has_reference(root, "mv_object", .Value, .Identifier))
		testing.expect(t, !has_diagnostic(root, .Unknown_Field))
		testing.expect(t, !has_diagnostic(root, .Unresolved_Reference))
	}
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
	testing.expect(t, has_diagnostic(&unit, .Unresolved_Open_Sql_Source))
}

@(test)
validates_open_sql_fields_against_query_local_source :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="wdy_config_data" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="config_id">
    <abapsource:properties><abapsource:entry abapsource:key="ddicDataType">CHAR</abapsource:entry></abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="config_type">
    <abapsource:properties><abapsource:entry abapsource:key="ddicDataType">NUMC</abapsource:entry></abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="config_var">
    <abapsource:properties><abapsource:entry abapsource:key="ddicDataType">CHAR</abapsource:entry></abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="author">
    <abapsource:properties><abapsource:entry abapsource:key="ddicDataType">CHAR</abapsource:entry></abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="createdon">
    <abapsource:properties><abapsource:entry abapsource:key="ddicDataType">DATS</abapsource:entry></abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	wdy_source := ddic_xml.dependency_source("WDY_CONFIG_DATA", "ddic-table", xml, context.allocator)
	defer delete(wdy_source, context.allocator)
	target := analyze.Source_Input {
		uri = "mem://ZMAIN.abap",
		source = `
REPORT zmain.
DATA lv_id TYPE string.
DATA lv_author TYPE string.
DATA lv_createdon TYPE string.
DATA lv_config_id TYPE string.
DATA lv_config_type TYPE string.
DATA lv_config_var TYPE string.

SELECT id FROM zfirst INTO lv_id.
SELECT SINGLE author createdon FROM wdy_config_data INTO (lv_author, lv_createdon)
  WHERE config_id = lv_config_id
    AND config_type = lv_config_type
    AND config_var = lv_config_var.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table/zfirst.abap",
			source = `TYPES: BEGIN OF zfirst,
         id TYPE string,
       END OF zfirst.`,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/ddic-table/wdy_config_data.abap",
			source = wdy_source,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, !has_diagnostic(root, .Unknown_Field))
	testing.expect(t, !has_diagnostic(root, .Unresolved_Open_Sql_Source))
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
	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	found := false
	for read in lint_unit.read_table_binary_searches {
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
sort_target_resolves_class_local_type_component :: proc(t: ^testing.T) {
	source := `
CLASS lcl DEFINITION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_header,
             vepname TYPE string,
             version TYPE string,
           END OF ty_header.
    TYPES: BEGIN OF ty_webi,
             pvepheader TYPE STANDARD TABLE OF ty_header WITH DEFAULT KEY,
           END OF ty_webi.
    METHODS sort CHANGING cs_webi TYPE ty_webi.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD sort.
    SORT cs_webi-pvepheader BY vepname version.
  ENDMETHOD.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///sort_class_type_component.abap", source)
	keys := [?]string{"vepname", "version"}

	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, internal_table_order_present(&unit, "cs_webi-pvepheader", keys[:]))
}

@(test)
sort_by_nested_components_are_table_fields_not_value_refs :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_definition,
           component_name TYPE string,
           view_name TYPE string,
         END OF ty_definition.
  TYPES: BEGIN OF ty_view,
           definition TYPE ty_definition,
         END OF ty_view.
  DATA lt_view TYPE STANDARD TABLE OF ty_view WITH EMPTY KEY.

  SORT lt_view BY definition-component_name ASCENDING definition-view_name ASCENDING.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///sort_nested_components.abap", source)
	keys := [?]string{"definition-component_name", "definition-view_name"}

	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, !has_reference(&unit, "definition", .Value, .Identifier))
	testing.expect(t, internal_table_order_present(&unit, "lt_view", keys[:]))
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
delete_db_table_from_table_uses_sql_source_namespace :: proc(t: ^testing.T) {
	source := `
TYPES zdelete_tab TYPE string.
FORM run.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  DELETE zdelete_tab FROM TABLE lt_rows ##SUBRC_OK.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///delete_db_from_table.abap", source)

	testing.expect(t, sql_source_present(&unit, "zdelete_tab", .External))
	testing.expect(t, sql_name_ref_present(&unit, "zdelete_tab", .Source))
	testing.expect(t, has_reference(&unit, "lt_rows", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "zdelete_tab", .Value, .Identifier))
	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
}

@(test)
modify_transporting_and_where_are_table_fields :: proc(t: ^testing.T) {
	source := `
TYPES dokstate TYPE string.
FORM run.
  TYPES: BEGIN OF ty_dokil,
           dokstate TYPE string,
         END OF ty_dokil.
  DATA lt_dokil TYPE STANDARD TABLE OF ty_dokil WITH EMPTY KEY.
  DATA ls_dokil TYPE ty_dokil.

  MODIFY lt_dokil FROM ls_dokil TRANSPORTING dokstate WHERE dokstate IS NOT INITIAL.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///modify_transporting_fields.abap", source)

	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect(t, !has_diagnostic(&unit, .Unresolved_Reference))
	testing.expect(t, !has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, has_reference(&unit, "lt_dokil", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "ls_dokil", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "dokstate", .Value, .Identifier))

	field_seen := false
	for access in unit.field_accesses {
		if access.base_name == "lt_dokil" &&
		   len(access.field_path) == 1 &&
		   access.field_path[0].name == "dokstate" {
			field_seen = true
		}
	}
	testing.expect(t, field_seen)
}

@(test)
modify_transporting_validates_table_fields :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_row,
           id TYPE string,
         END OF ty_row.
  DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
  DATA ls_row TYPE ty_row.

  MODIFY lt_rows FROM ls_row TRANSPORTING missing.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///modify_transporting_missing.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, !has_diagnostic(&unit, .Wrong_Namespace))
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
	lint_unit := lints.collect_source(unit.uri, unit.source, context.allocator)
	testing.expect(t, len(lint_unit.find_sites) == 1)
	testing.expect(t, len(lint_unit.find_sites[0].write_targets) == 4)
	testing.expect(t, lint_unit.find_sites[0].write_targets[3].definitely_assigned)
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
dependency_store_resolves_formatted_decfloat_data_element_in_structure :: proc(t: ^testing.T) {
	root := manifest_workspace_path("dependency-store-ddic-decfloat")
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
			object_kind    = "ddic-structure",
			object_name    = "ZQUANTITY",
			object_uri     = "/sap/bc/adt/ddic/structures/ZQUANTITY",
			object_type    = "TABL/DS",
			description    = "Quantity structure",
			file_extension = "xml",
			source_text    = `<abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name="ZQUANTITY" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="NUMBER">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">
        ZNUMBER_DECFLOAT
      </abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">
        DF34_RAW
      </abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`,
			fetched_at = "2026-05-21T00:00:00Z",
		},
		{
			package_name   = "ZPKG",
			object_kind    = "ddic-data-element",
			object_name    = "ZNUMBER_DECFLOAT",
			object_uri     = "/sap/bc/adt/ddic/dataelements/ZNUMBER_DECFLOAT",
			object_type    = "DTEL/DE",
			description    = "Decimal number",
			file_extension = "xml",
			source_text    = `<blue:wbobj adtcore:name="ZNUMBER_DECFLOAT" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>
      predefinedAbapType
    </dtel:typeKind>
    <dtel:dataType>
      DF34_RAW
    </dtel:dataType>
  </dtel:dataElement>
</blue:wbobj>`,
			fetched_at = "2026-05-21T00:00:00Z",
		},
	}
	_, err = dep_store.put_artifacts(&store, &profile, inputs[:], context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	targets := [?]analyze.Source_Input {
		{uri = "mem://ZMAIN.abap", source = "REPORT zmain. DATA quantity TYPE zquantity."},
	}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 3)
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
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
	result := analyze_standalone_path_test_with_options(t, root_file, workspace.Options{dependency_store_path = store_path})

	testing.expect(t, result.ok)
	testing.expect(t, !result.used_manifest)
	testing.expect_value(t, len(result.project.units), 2)
	testing.expect(t, !project_has_diagnostic(&result.project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&result.project, .Unresolved_Reference))
}

@(test)
dependency_store_ddic_data_elements_resolve_transitive_table_fields :: proc(t: ^testing.T) {
	root := manifest_workspace_path("dependency-store-ddic-lchr")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	artifacts := [?]dep_store.Stored_Artifact_Input {
		{
			package_name   = "SUSR",
			object_kind    = "ddic-table",
			object_name    = "USR12",
			object_uri     = "/sap/bc/adt/vit/wb/object_type/tabldt/object_name/USR12",
			object_type    = "TABL/DT",
			description    = "User Master Authorization Values",
			file_extension = "xml",
			source_text    = `<abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="USR12" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="VALS">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">XUVALS</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">LCHR</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`,
			fetched_at     = "2026-05-21T00:00:00Z",
		},
		{
			package_name   = "SUSR",
			object_kind    = "ddic-data-element",
			object_name    = "XUVALS",
			object_uri     = "/sap/bc/adt/vit/wb/object_type/dtelde/object_name/XUVALS",
			object_type    = "DTEL/DE",
			description    = "Authorization values",
			file_extension = "xml",
			source_text    = `<blue:wbobj adtcore:name="XUVALS" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>domain</dtel:typeKind>
    <dtel:dataType>LCHR</dtel:dataType>
  </dtel:dataElement>
</blue:wbobj>`,
			fetched_at     = "2026-05-21T00:00:00Z",
		},
		{
			package_name   = "SWWW",
			object_kind    = "ddic-structure",
			object_name    = "W3TEMPATTR",
			object_uri     = "/sap/bc/adt/vit/wb/object_type/tablds/object_name/W3TEMPATTR",
			object_type    = "TABL/DS",
			description    = "WWW temporary attributes",
			file_extension = "xml",
			source_text    = `<abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name="W3TEMPATTR" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="CLUSTD">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">INDX_CLUST</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">LRAW</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`,
			fetched_at     = "2026-05-21T00:00:00Z",
		},
		{
			package_name   = "SWWW",
			object_kind    = "ddic-data-element",
			object_name    = "INDX_CLUST",
			object_uri     = "/sap/bc/adt/vit/wb/object_type/dtelde/object_name/INDX_CLUST",
			object_type    = "DTEL/DE",
			description    = "Cluster data",
			file_extension = "xml",
			source_text    = `<blue:wbobj adtcore:name="INDX_CLUST" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>domain</dtel:typeKind>
    <dtel:dataType>LRAW</dtel:dataType>
  </dtel:dataElement>
</blue:wbobj>`,
			fetched_at     = "2026-05-21T00:00:00Z",
		},
	}
	_, err = dep_store.put_artifacts(&store, &profile, artifacts[:], context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	targets := [?]analyze.Source_Input {
		{
			uri    = "mem://ZMAIN.abap",
			source = "REPORT zmain. DATA ls_user TYPE usr12. DATA ls_cluster TYPE w3tempattr.",
		},
	}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, cache_any_profile = true},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 5)
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
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

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. CALL FUNCTION 'Z_REMOTE_FM'.",
	}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, cache_any_profile = true},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

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
cache_hit_resolves_before_unreachable_adt :: proc(t: ^testing.T) {
	root := manifest_workspace_path("cache-hit-before-adt")
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
		object_name    = "ZCL_CACHE_FIRST",
		object_uri     = "/sap/bc/adt/oo/classes/ZCL_CACHE_FIRST",
		object_type    = "CLAS/OC",
		description    = "Cached class",
		file_extension = "abap",
		source_text    = "CLASS zcl_cache_first DEFINITION. ENDCLASS. CLASS zcl_cache_first IMPLEMENTATION. ENDCLASS.",
		fetched_at     = "2026-05-21T00:00:00Z",
	}
	_, err = dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	client: adt.Client
	adt.client_init(
		&client,
		adt.Connection_Config {
			base_url = "http://127.0.0.1:1/sap/bc/adt",
			username = "demo",
			password = "secret",
		},
		context.allocator,
	)
	client.http.timeout = 50 * time.Millisecond
	defer adt.client_destroy(&client, context.allocator)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lo_dep TYPE REF TO zcl_cache_first.",
	}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, adt_client = &client},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 2)
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
adt_runs_for_cache_miss_after_cache_hit_reanalysis :: proc(t: ^testing.T) {
	root := manifest_workspace_path("cache-hit-then-adt")
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
		object_name    = "ZCL_CACHE_THEN_ADT",
		object_uri     = "/sap/bc/adt/oo/classes/ZCL_CACHE_THEN_ADT",
		object_type    = "CLAS/OC",
		description    = "Cached class",
		file_extension = "abap",
		source_text    = "CLASS zcl_cache_then_adt DEFINITION. ENDCLASS. CLASS zcl_cache_then_adt IMPLEMENTATION. ENDCLASS.",
		fetched_at     = "2026-05-21T00:00:00Z",
	}
	_, err = dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	server := Semantic_Adt_Test_Server {
		session_response = semantic_test_http_response("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		search_response  = semantic_test_http_response("200 OK", `<feed xmlns:adtcore="http://www.sap.com/adt/core"></feed>`, "", context.allocator),
		missing_response = semantic_test_http_response(
			"200 OK",
			"CLASS zcl_adt_after_cache DEFINITION. ENDCLASS. CLASS zcl_adt_after_cache IMPLEMENTATION. ENDCLASS.",
			"",
			context.allocator,
		),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.search_response, context.allocator)
	defer delete(server.missing_response, context.allocator)
	client, worker := semantic_adt_client_for_test_server(t, &server)
	defer adt.client_destroy(&client, context.allocator)
	defer semantic_adt_test_server_stop(&server, worker)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lo_cache TYPE REF TO zcl_cache_then_adt. DATA lo_adt TYPE REF TO zcl_adt_after_cache.",
	}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, adt_client = &client},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect(t, server.fetch_count > 0)
	_, cached, cache_err := dep_store.find_artifact_for_candidate(
		&store,
		&profile,
		"zcl_adt_after_cache",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, cache_err, dep_store.Store_Error.None)
	testing.expect(t, cached)
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
cache_negative_skips_adt_and_allows_local_export_fallback :: proc(t: ^testing.T) {
	root := manifest_workspace_path("cache-negative-local-fallback")
	export_root := external_export_workspace_path("cache-negative-local-fallback")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	export_file := manifest_test_file(
		t,
		export_root,
		"source-code-library/includes/ZINC_NEG_LOCAL.abap",
		"DATA gv_neg_local TYPE i.",
	)
	connection_key := "test-connection"
	err = dep_store.record_negative_lookup(
		&store,
		&profile,
		connection_key,
		"zinc_neg_local",
		.Include,
		"2026-05-21T00:00:00Z",
		context.allocator,
	)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	status, status_err := dep_store.find_cached_candidate(
		&store,
		&profile,
		connection_key,
		"zinc_neg_local",
		.Include,
		context.allocator,
	)
	testing.expect_value(t, status_err, dep_store.Store_Error.None)
	testing.expect_value(t, status, dep_store.Candidate_Cache_Status.Negative)
	probe_candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	probe_dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	probe_seen := make(map[i64]bool, context.allocator)
	probe_remote := [?]deps.Remote_Dependency_Candidate {
		{name = "zinc_neg_local", kind = .Include},
	}
	pool_for_probe: execution.Pool
	execution.pool_init(&pool_for_probe, execution.Options{worker_count = 0, task_capacity = 8}, context.allocator)
	probe_temp_arena: virtual.Arena
	_ = virtual.arena_init_growing(&probe_temp_arena)
	defer virtual.arena_destroy(&probe_temp_arena)
	previous_temp_allocator := context.temp_allocator
	context.temp_allocator = virtual.arena_allocator(&probe_temp_arena)
	defer context.temp_allocator = previous_temp_allocator
	probe := remote_deps.add_dependency_cache_matches(
		&probe_candidates,
		&probe_dependencies,
		probe_remote[:],
		&store,
		&profile,
		false,
		connection_key,
		&probe_seen,
		&pool_for_probe,
		"mem://ZMAIN.abap",
		"cache",
	)
	execution.pool_destroy(&pool_for_probe)
	testing.expect(t, !probe.added)
	testing.expect_value(t, len(probe.adt_candidates), 0)
	testing.expect_value(t, len(probe.local_candidates), 1)

	roots := make([dynamic]string, 0, 1, context.allocator)
	append(&roots, export_root)
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. INCLUDE zinc_neg_local.",
	}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config {
			cache = &store,
			profile = &profile,
			local_export_roots = roots[:],
		},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect(t, analyze.project_unit_by_uri(&project, export_file) != nil)
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
adt_miss_records_negative_cache_lookup :: proc(t: ^testing.T) {
	root := manifest_workspace_path("adt-miss-negative-cache")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	server := Semantic_Adt_Test_Server {
		session_response = semantic_test_http_response("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		search_response  = semantic_test_http_response("200 OK", `<feed xmlns:adtcore="http://www.sap.com/adt/core"></feed>`, "", context.allocator),
		missing_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.search_response, context.allocator)
	defer delete(server.missing_response, context.allocator)
	client, worker := semantic_adt_client_for_test_server(t, &server)
	defer adt.client_destroy(&client, context.allocator)
	defer semantic_adt_test_server_stop(&server, worker)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lo_dep TYPE REF TO zcl_missing_remote.",
	}
	targets := [?]analyze.Source_Input{target}
	_ = session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, adt_client = &client},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	status, lookup_err := dep_store.find_cached_candidate(
		&store,
		&profile,
		adt.client_connection_key(&client, context.allocator),
		"zcl_missing_remote",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, lookup_err, dep_store.Store_Error.None)
	testing.expect_value(t, status, dep_store.Candidate_Cache_Status.Negative)
	testing.expect(t, server.request_count > 0)
}

@(test)
typepool_resolver_fetches_pool_source_after_adt_miss :: proc(t: ^testing.T) {
	root := manifest_workspace_path("typepool-resolver-fetch")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	pool_source := `TYPE-POOL tpak.
TYPES: BEGIN OF tpak_permission_to_use,
         name TYPE string,
       END OF tpak_permission_to_use.
TYPES tpak_permission_to_use_list TYPE STANDARD TABLE OF tpak_permission_to_use WITH DEFAULT KEY.`
	server := Semantic_Adt_Test_Server {
		session_response = semantic_test_http_response("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		search_response  = semantic_test_http_response("200 OK", `<feed xmlns:adtcore="http://www.sap.com/adt/core"></feed>`, "", context.allocator),
		missing_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
		typepool_owner_response = semantic_test_http_response("200 OK", "TPAK", "", context.allocator),
		typepool_source_response = semantic_test_http_response("200 OK", pool_source, "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.search_response, context.allocator)
	defer delete(server.missing_response, context.allocator)
	defer delete(server.typepool_owner_response, context.allocator)
	defer delete(server.typepool_source_response, context.allocator)
	client, worker := semantic_adt_client_for_typepool_test_server(t, &server)
	defer adt.client_destroy(&client, context.allocator)
	defer semantic_adt_test_server_stop(&server, worker)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lt_permissions TYPE tpak_permission_to_use_list.",
	}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, adt_client = &client},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
	testing.expect_value(t, server.typepool_source_count, 1)
	record, ok, lookup_err := dep_store.find_artifact_by_kind_name(
		&store,
		&profile,
		"type-pool",
		"tpak",
		context.allocator,
	)
	testing.expect_value(t, lookup_err, dep_store.Store_Error.None)
	testing.expect(t, ok)
	testing.expect(t, !strings.contains(record.source_text, "TYPE-POOL"))
}

@(test)
cached_typepool_resolves_from_dependency_store_symbol :: proc(t: ^testing.T) {
	root := manifest_workspace_path("typepool-resolver-symbol-cache")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	pool_source := `TYPE-POOL gfw.
TYPES gfw_boolean TYPE c LENGTH 1.
CONSTANTS gfw_false TYPE gfw_boolean VALUE ' '.`
	server := Semantic_Adt_Test_Server {
		session_response = semantic_test_http_response("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		search_response  = semantic_test_http_response("200 OK", `<feed xmlns:adtcore="http://www.sap.com/adt/core"></feed>`, "", context.allocator),
		missing_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
		typepool_owner_response = semantic_test_http_response("200 OK", "GFW", "", context.allocator),
		typepool_source_response = semantic_test_http_response("200 OK", pool_source, "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.search_response, context.allocator)
	defer delete(server.missing_response, context.allocator)
	defer delete(server.typepool_owner_response, context.allocator)
	defer delete(server.typepool_source_response, context.allocator)
	client, worker := semantic_adt_client_for_typepool_test_server(t, &server)
	defer adt.client_destroy(&client, context.allocator)
	defer semantic_adt_test_server_stop(&server, worker)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	targets := [?]analyze.Source_Input {
		{uri = "mem://FETCH.abap", source = "REPORT zmain. DATA flag TYPE gfw_boolean."},
	}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, adt_client = &client},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
	testing.expect_value(t, server.typepool_source_count, 1)

	targets = [?]analyze.Source_Input {
		{uri = "mem://CACHE.abap", source = "REPORT zmain. DATA flag TYPE c LENGTH 1. flag = gfw_false."},
	}
	project = session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, cache_any_profile = true},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
	testing.expect_value(t, server.typepool_source_count, 1)
}

@(test)
typepool_dependency_revalidates_waiting_units :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	defer execution.pool_destroy(&pool)

	state := analyze.project_state_make({}, context.allocator)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA proxy TYPE sprx_s_proxy.",
	}
	if_proxy := analyze.Source_Input {
		uri = "abapls-cache:/global-interface/if_proxy_name_proposal.abap",
		source = `INTERFACE if_proxy_name_proposal.
  CONSTANTS co_enum_value TYPE string VALUE sprx_const_enumval_wsdl.
ENDINTERFACE.`,
		mode = .Dependency_Interface,
	}
	typepool := analyze.Source_Input {
		uri = "abapls-typepool:/sprx.abap",
		source = `CONSTANTS sprx_const_enumval_wsdl TYPE string VALUE 'enumerationvalue'.
TYPES: BEGIN OF sprx_s_proxy,
         actor1 TYPE sprx_s_contract_actor,
       END OF sprx_s_proxy.
TYPES sprx_s_contract_actor TYPE string.`,
		mode = .Dependency_Interface,
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, 0, 2, context.allocator)
	append(&dependencies, if_proxy)

	project := analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	testing.expect(t, project_units_have_diagnostic(&project, .Unresolved_Reference))

	append(&dependencies, typepool)
	project = analyze.project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
typepool_resolver_expands_pool_macros :: proc(t: ^testing.T) {
	root := manifest_workspace_path("typepool-resolver-macro")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	pool_source := `TYPE-POOL trsel.
DEFINE trsel_def_range_tab.
  TYPES: BEGIN OF trsel_trs_&1,
           sign TYPE c LENGTH 1,
           option TYPE c LENGTH 2,
           low TYPE &2,
           high TYPE &2,
         END OF trsel_trs_&1.
END-OF-DEFINITION.

trsel_def_range_tab trkorr c.

TYPES: BEGIN OF trsel_ts_ranges,
         trkorr TYPE trsel_trs_trkorr OCCURS 0,
       END OF trsel_ts_ranges.`
	server := Semantic_Adt_Test_Server {
		session_response = semantic_test_http_response("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		search_response  = semantic_test_http_response("200 OK", `<feed xmlns:adtcore="http://www.sap.com/adt/core"></feed>`, "", context.allocator),
		missing_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
		typepool_owner_response = semantic_test_http_response("200 OK", "TRSEL", "", context.allocator),
		typepool_source_response = semantic_test_http_response("200 OK", pool_source, "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.search_response, context.allocator)
	defer delete(server.missing_response, context.allocator)
	defer delete(server.typepool_owner_response, context.allocator)
	defer delete(server.typepool_source_response, context.allocator)
	client, worker := semantic_adt_client_for_typepool_test_server(t, &server)
	defer adt.client_destroy(&client, context.allocator)
	defer semantic_adt_test_server_stop(&server, worker)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA ranges TYPE trsel_ts_ranges.",
	}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, adt_client = &client},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
	record, ok, lookup_err := dep_store.find_artifact_by_kind_name(
		&store,
		&profile,
		"type-pool",
		"trsel",
		context.allocator,
	)
	testing.expect_value(t, lookup_err, dep_store.Store_Error.None)
	testing.expect(t, ok)
	testing.expect(t, strings.contains(record.source_text, "trsel_trs_trkorr"))
	testing.expect(t, !strings.contains(record.source_text, "trsel_def_range_tab trkorr"))
}

@(test)
typepool_resolver_fetches_one_pool_for_multiple_symbols :: proc(t: ^testing.T) {
	pool_source := `TYPE-POOL gfw.
TYPES gfw_boolean TYPE c LENGTH 1.
CONSTANTS gfw_false TYPE gfw_boolean VALUE ' '.`
	server := Semantic_Adt_Test_Server {
		session_response = semantic_test_http_response("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		search_response  = semantic_test_http_response("200 OK", `<feed xmlns:adtcore="http://www.sap.com/adt/core"></feed>`, "", context.allocator),
		missing_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
		typepool_owner_response = semantic_test_http_response("200 OK", "GFW", "", context.allocator),
		typepool_source_response = semantic_test_http_response("200 OK", pool_source, "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.search_response, context.allocator)
	defer delete(server.missing_response, context.allocator)
	defer delete(server.typepool_owner_response, context.allocator)
	defer delete(server.typepool_source_response, context.allocator)
	client, worker := semantic_adt_client_for_typepool_test_server(t, &server)
	defer adt.client_destroy(&client, context.allocator)
	defer semantic_adt_test_server_stop(&server, worker)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri = "mem://ZMAIN.abap",
		source = `REPORT zmain.
DATA lv_flag TYPE gfw_boolean.
lv_flag = gfw_false.`,
	}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{adt_client = &client},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
	testing.expect_value(t, server.typepool_source_count, 1)
}

@(test)
typepool_resolver_submits_owner_and_source_tasks :: proc(t: ^testing.T) {
	pool_source := `TYPE-POOL gfw.
TYPES gfw_boolean TYPE c LENGTH 1.
CONSTANTS gfw_false TYPE gfw_boolean VALUE ' '.`
	server := Semantic_Adt_Test_Server {
		session_response = semantic_test_http_response("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		missing_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
		typepool_owner_response = semantic_test_http_response("200 OK", "GFW", "", context.allocator),
		typepool_source_response = semantic_test_http_response("200 OK", pool_source, "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.missing_response, context.allocator)
	defer delete(server.typepool_owner_response, context.allocator)
	defer delete(server.typepool_source_response, context.allocator)
	client, worker := semantic_adt_client_for_typepool_test_server(t, &server)
	defer adt.client_destroy(&client, context.allocator)
	defer semantic_adt_test_server_stop(&server, worker)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 2, task_capacity = 8}, context.allocator)
	if pool.options.worker_count > 0 {
		execution.pool_start(&pool)
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	remote := [?]deps.Remote_Dependency_Candidate {
		{name = "gfw_boolean", kind = .Type},
		{name = "gfw_false", kind = .Symbol},
	}
	before := execution.pool_stats(&pool)
	added := remote_deps.add_typepool_resolver_matches(
		&candidates,
		&dependencies,
		remote[:],
		nil,
		nil,
		&client,
		&pool,
		"mem://ZMAIN.abap",
	)
	after := execution.pool_stats(&pool)
	if pool.options.worker_count > 0 {
		execution.pool_join(&pool)
	}
	execution.pool_destroy(&pool)

	testing.expect(t, added)
	testing.expect_value(t, len(dependencies), 1)
	testing.expect_value(t, server.typepool_owner_count, 2)
	testing.expect_value(t, server.typepool_source_count, 1)
	testing.expect(t, after.submitted >= before.submitted + 3)
}

@(test)
cached_typepool_source_skips_source_endpoint :: proc(t: ^testing.T) {
	root := manifest_workspace_path("typepool-resolver-cache")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	artifact := dep_store.Stored_Artifact_Input {
		package_name   = "GFW",
		object_kind    = "type-pool",
		object_name    = "GFW",
		object_uri     = "type-pool:GFW",
		object_type    = "TYPEPOOL",
		description    = "Cached type-pool",
		file_extension = "abap",
		source_text    = "TYPES gfw_boolean TYPE c LENGTH 1.",
		fetched_at     = "2026-05-30T00:00:00Z",
	}
	_, err = dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	server := Semantic_Adt_Test_Server {
		session_response = semantic_test_http_response("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		search_response  = semantic_test_http_response("200 OK", `<feed xmlns:adtcore="http://www.sap.com/adt/core"></feed>`, "", context.allocator),
		missing_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
		typepool_owner_response = semantic_test_http_response("200 OK", "GFW", "", context.allocator),
		typepool_source_response = semantic_test_http_response("500 Internal Server Error", "unexpected", "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.search_response, context.allocator)
	defer delete(server.missing_response, context.allocator)
	defer delete(server.typepool_owner_response, context.allocator)
	defer delete(server.typepool_source_response, context.allocator)
	client, worker := semantic_adt_client_for_typepool_test_server(t, &server)
	defer adt.client_destroy(&client, context.allocator)
	defer semantic_adt_test_server_stop(&server, worker)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lv_flag TYPE gfw_boolean.",
	}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, adt_client = &client},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
	testing.expect_value(t, server.typepool_source_count, 0)
}

@(test)
cached_typepool_like_dependency_is_fetched :: proc(t: ^testing.T) {
	root := manifest_workspace_path("typepool-resolver-like-dependency")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	artifacts := [?]dep_store.Stored_Artifact_Input {
		{
			package_name   = "RSD",
			object_kind    = "type-pool",
			object_name    = "RSD",
			object_uri     = "type-pool:RSD",
			object_type    = "TYPEPOOL",
			description    = "Cached type-pool",
			file_extension = "abap",
			source_text    = "TYPES rsd_s_area LIKE rsdareav.",
			fetched_at     = "2026-05-30T00:00:00Z",
		},
		{
			package_name   = "RSD",
			object_kind    = "ddic-view",
			object_name    = "RSDAREAV",
			object_uri     = "/sap/bc/adt/vit/wb/object_type/viewdv/object_name/RSDAREAV",
			object_type    = "VIEW/DV",
			description    = "InfoArea view",
			file_extension = "abap",
			source_text    = "TYPES: BEGIN OF rsdareav, area TYPE string, END OF rsdareav.",
			fetched_at     = "2026-05-30T00:00:00Z",
		},
	}
	_, err = dep_store.put_artifacts(&store, &profile, artifacts[:], context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	server := Semantic_Adt_Test_Server {
		session_response = semantic_test_http_response("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		search_response  = semantic_test_http_response("200 OK", `<feed xmlns:adtcore="http://www.sap.com/adt/core"></feed>`, "", context.allocator),
		missing_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
		typepool_owner_response = semantic_test_http_response("200 OK", "RSD", "", context.allocator),
		typepool_source_response = semantic_test_http_response("500 Internal Server Error", "unexpected", "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.search_response, context.allocator)
	defer delete(server.missing_response, context.allocator)
	defer delete(server.typepool_owner_response, context.allocator)
	defer delete(server.typepool_source_response, context.allocator)
	client, worker := semantic_adt_client_for_typepool_test_server(t, &server)
	defer adt.client_destroy(&client, context.allocator)
	defer semantic_adt_test_server_stop(&server, worker)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA ls_area TYPE rsd_s_area.",
	}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, adt_client = &client},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
	testing.expect_value(t, server.typepool_source_count, 0)
}

@(test)
cached_typepool_include_source_is_refetched_and_expanded :: proc(t: ^testing.T) {
	root := manifest_workspace_path("typepool-resolver-cache-include")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	artifact := dep_store.Stored_Artifact_Input {
		package_name   = "SVRS2",
		object_kind    = "type-pool",
		object_name    = "SVRS2",
		object_uri     = "type-pool:SVRS2",
		object_type    = "TYPEPOOL",
		description    = "Cached type-pool",
		file_extension = "abap",
		source_text    = "INCLUDE LSVRXPIN.",
		fetched_at     = "2026-05-30T00:00:00Z",
	}
	_, err = dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	pool_source := `TYPE-POOL svrs2.
INCLUDE LSVRXPIN.`
	include_source := `INCLUDE DDIC_LSVRXPIN.`
	nested_include_source := `TYPES svrs2_versionable_object TYPE c LENGTH 1.`
	include_response := semantic_test_http_response("200 OK", include_source, "", context.allocator)
	nested_include_response := semantic_test_http_response(
		"200 OK",
		nested_include_source,
		"",
		context.allocator,
	)
	fetch_routes := [?]Semantic_Adt_Test_Route {
		{request_contains = "/programs/includes/LSVRXPIN", response = include_response},
		{request_contains = "/programs/includes/DDIC_LSVRXPIN", response = nested_include_response},
	}
	server := Semantic_Adt_Test_Server {
		session_response = semantic_test_http_response("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		search_response  = semantic_test_http_response("200 OK", `<feed xmlns:adtcore="http://www.sap.com/adt/core"></feed>`, "", context.allocator),
		missing_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
		fetch_routes     = fetch_routes[:],
		typepool_owner_response = semantic_test_http_response("200 OK", "SVRS2", "", context.allocator),
		typepool_source_response = semantic_test_http_response("200 OK", pool_source, "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.search_response, context.allocator)
	defer delete(server.missing_response, context.allocator)
	defer delete(include_response, context.allocator)
	defer delete(nested_include_response, context.allocator)
	defer delete(server.typepool_owner_response, context.allocator)
	defer delete(server.typepool_source_response, context.allocator)
	client, worker := semantic_adt_client_for_typepool_test_server(t, &server)
	defer adt.client_destroy(&client, context.allocator)
	defer semantic_adt_test_server_stop(&server, worker)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA ls_obj TYPE svrs2_versionable_object.",
	}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, adt_client = &client},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
	testing.expect_value(t, server.typepool_source_count, 1)
	testing.expect_value(t, server.fetch_count, 2)
	record, ok, lookup_err := dep_store.find_artifact_by_kind_name(
		&store,
		&profile,
		"type-pool",
		"svrs2",
		context.allocator,
	)
	testing.expect_value(t, lookup_err, dep_store.Store_Error.None)
	testing.expect(t, ok)
	testing.expect(t, strings.contains(record.source_text, "svrs2_versionable_object"))
	testing.expect(t, !strings.contains(record.source_text, "INCLUDE LSVRXPIN"))
	testing.expect(t, !strings.contains(record.source_text, "INCLUDE DDIC_LSVRXPIN"))
}

@(test)
typepool_resolver_miss_keeps_unresolved_diagnostic :: proc(t: ^testing.T) {
	server := Semantic_Adt_Test_Server {
		session_response = semantic_test_http_response("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		search_response  = semantic_test_http_response("200 OK", `<feed xmlns:adtcore="http://www.sap.com/adt/core"></feed>`, "", context.allocator),
		missing_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
		typepool_owner_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
		typepool_source_response = semantic_test_http_response("404 Not Found", "missing", "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.search_response, context.allocator)
	defer delete(server.missing_response, context.allocator)
	defer delete(server.typepool_owner_response, context.allocator)
	defer delete(server.typepool_source_response, context.allocator)
	client, worker := semantic_adt_client_for_typepool_test_server(t, &server)
	defer adt.client_destroy(&client, context.allocator)
	defer semantic_adt_test_server_stop(&server, worker)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lt_missing TYPE unknown_typepool_symbol.",
	}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{adt_client = &client},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect(t, project_units_have_diagnostic(&project, .Unresolved_Reference))
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
	result := analyze_standalone_path_test_with_options(t, root_file, workspace.Options{dependency_store_path = store_path})

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

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 2, task_capacity = 8}, context.allocator)
	if pool.options.worker_count > 0 {
		execution.pool_start(&pool)
	}
	target := analyze.Source_Input{uri = "file:///ZMAIN.abap", source = strings.to_string(target_source)}
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		make([dynamic]analyze.Project_Candidate_Input, context.allocator)[:],
		make([dynamic]analyze.Source_Input, context.allocator)[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, cache_any_profile = true},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	if pool.options.worker_count > 0 {
		execution.pool_join(&pool)
	}
	execution.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 17)
	testing.expect(t, !project_has_diagnostic(&project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
dependency_store_cache_misses_keep_fallback_candidates :: proc(t: ^testing.T) {
	root := manifest_workspace_path("dependency-store-cache-miss-fallback")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	remote := [?]deps.Remote_Dependency_Candidate {
		{name = "tpak_package_interface_list", kind = .Type},
		{name = "zinc_missing", kind = .Include},
		{name = "zmissing_report", kind = .Report},
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	seen := make(map[i64]bool, context.allocator)
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 8}, context.allocator)
	defer execution.pool_destroy(&pool)

	temp_arena: virtual.Arena
	_ = virtual.arena_init_growing(&temp_arena)
	defer virtual.arena_destroy(&temp_arena)
	previous_temp_allocator := context.temp_allocator
	context.temp_allocator = virtual.arena_allocator(&temp_arena)
	defer context.temp_allocator = previous_temp_allocator

	cache_result := remote_deps.add_dependency_cache_matches(
		&candidates,
		&dependencies,
		remote[:],
		&store,
		&profile,
		false,
		"test-connection",
		&seen,
		&pool,
		"file:///ZMAIN.abap",
		"cache",
	)
	clobber := make([dynamic]deps.Remote_Dependency_Candidate, 0, 32, context.temp_allocator)
	for _ in 0 ..< 32 {
		append(&clobber, deps.Remote_Dependency_Candidate{name = "clobber", kind = .Report})
	}

	testing.expect(t, !cache_result.added)
	testing.expect_value(t, len(cache_result.adt_candidates), len(remote))
	testing.expect_value(t, len(cache_result.local_candidates), len(remote))
	testing.expect_value(t, cache_result.local_candidates[0].name, "tpak_package_interface_list")
	testing.expect_value(t, cache_result.local_candidates[0].kind, deps.Remote_Dependency_Kind.Type)
}

@(test)
dependency_store_candidates_reuse_reader_without_lookup_tasks :: proc(t: ^testing.T) {
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

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 2, task_capacity = 4}, context.allocator)
	if pool.options.worker_count > 0 {
		execution.pool_start(&pool)
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	remote := [?]deps.Remote_Dependency_Candidate {
		{name = "ZCL_STORE_TASK", kind = .Type},
		{name = "ZINC_STORE_TASK", kind = .Include},
		{name = "ZMISS_STORE_TASK", kind = .Report},
	}
	seen := make(map[i64]bool, context.allocator)
	temp_arena: virtual.Arena
	_ = virtual.arena_init_growing(&temp_arena)
	defer virtual.arena_destroy(&temp_arena)
	previous_temp_allocator := context.temp_allocator
	context.temp_allocator = virtual.arena_allocator(&temp_arena)
	defer context.temp_allocator = previous_temp_allocator
	worker_limit := max(pool.options.worker_count, 1)
	before := execution.pool_stats(&pool)
	cache_result := remote_deps.add_dependency_cache_matches(
		&candidates,
		&dependencies,
		remote[:],
		&store,
		nil,
		true,
		"",
		&seen,
		&pool,
		"file:///ZMAIN.abap",
		"store_any",
	)
	after := execution.pool_stats(&pool)
	if pool.options.worker_count > 0 {
		execution.pool_join(&pool)
	}
	execution.pool_destroy(&pool)

	testing.expect(t, cache_result.added)
	testing.expect_value(t, len(dependencies), 1)
	testing.expect_value(t, len(candidates), 1)
	testing.expect_value(t, dependencies[0].mode, analyze.Source_Mode.Dependency_Interface)
	testing.expect_value(t, candidates[0].input.mode, analyze.Source_Mode.Dependency_Interface)
	lookup_tasks := after.submitted - before.submitted
	testing.expect(t, lookup_tasks < u64(len(remote)))
	testing.expect(t, lookup_tasks <= u64(worker_limit))
}

@(test)
dependency_store_symbol_hit_for_include_adds_include_candidate :: proc(t: ^testing.T) {
	root := manifest_workspace_path("dependency-store-symbol-include")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	symbols := [?]dep_store.Stored_Symbol_Input {
		{symbol_name = "gv_cancel", symbol_kind = "variable", priority = 1},
	}
	artifact := dep_store.Stored_Artifact_Input {
		package_name   = "STTP",
		object_kind    = "include",
		object_name    = "/STTP/INT_GLOBAL",
		object_uri     = "/sap/bc/adt/programs/includes/%2FSTTP%2FINT_GLOBAL",
		object_type    = "PROG/I",
		description    = "Namespaced include",
		file_extension = "abap",
		source_text    = "DATA gv_cancel TYPE abap_bool.",
		fetched_at     = "2026-05-21T00:00:00Z",
		symbols        = symbols[:],
	}
	_, err = dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 8}, context.allocator)
	defer execution.pool_destroy(&pool)
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	remote := [?]deps.Remote_Dependency_Candidate {
		{name = "gv_cancel", kind = .Symbol},
		{name = "/sttp/int_global", kind = .Include},
	}
	seen := make(map[i64]bool, context.allocator)
	temp_arena: virtual.Arena
	_ = virtual.arena_init_growing(&temp_arena)
	defer virtual.arena_destroy(&temp_arena)
	previous_temp_allocator := context.temp_allocator
	context.temp_allocator = virtual.arena_allocator(&temp_arena)
	defer context.temp_allocator = previous_temp_allocator

	cache_result := remote_deps.add_dependency_cache_matches(
		&candidates,
		&dependencies,
		remote[:],
		&store,
		&profile,
		false,
		"test-connection",
		&seen,
		&pool,
		"file:///ZMAIN.abap",
		"cache",
	)

	testing.expect(t, cache_result.added)
	testing.expect_value(t, len(candidates), 1)
	testing.expect_value(t, len(dependencies), 0)
	testing.expect_value(t, candidates[0].object_name, "/sttp/int_global")
	testing.expect_value(t, candidates[0].input.mode, analyze.Source_Mode.Dependency_Interface)
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
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		candidates[:],
		dependencies[:],
		remote_deps.Dependency_Config{local_export_roots = roots[:]},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

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
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	targets := [?]analyze.Source_Input{target}
	project := session.analysis_session_analyze_once(
		targets[:],
		candidates[:],
		dependencies[:],
		remote_deps.Dependency_Config{cache = &store, profile = &profile, local_export_roots = roots[:]},
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

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
manifest_project_loads_dotenv_for_adt_dependency_fetch :: proc(t: ^testing.T) {
	root := manifest_workspace_path("adt-dotenv-gate")
	manifest_test_file(t, root, ".git/keep", "")
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
	opened, opened_ok, _ := workspace.open_workspace(root, workspace.Options{enable_adt = true}, context.allocator)
	testing.expect(t, opened_ok)
	testing.expect(t, opened.has_dotenv)
	testing.expect(t, opened.has_adt)
	workspace.workspace_destroy(&opened, context.allocator)
}

@(test)
standalone_workspace_loads_adt_from_dotenv :: proc(t: ^testing.T) {
	root := manifest_workspace_path("standalone-adt-dotenv")
	manifest_test_file(t, root, ".git/keep", "")
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
	opened, opened_ok, _ := workspace.open_standalone_workspace(
		root,
		workspace.Options{enable_adt = true},
		context.allocator,
	)
	testing.expect(t, opened_ok)
	testing.expect(t, opened.has_adt)
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
	uri_keys := remote_deps.project_input_uri_keys(target.uri, dependencies[:], candidates[:], 1, context.allocator)

	added := remote_deps.add_adt_fetched_dependency_input(
		&candidates,
		&dependencies,
		deps.Remote_Dependency_Candidate{name = "zcl_adt_fetch", kind = .Type},
		&object_ref,
		"global-class",
		"CLASS zcl_adt_fetch DEFINITION. ENDCLASS. CLASS zcl_adt_fetch IMPLEMENTATION. ENDCLASS.",
		"abap",
		&uri_keys,
		context.allocator,
	)
	testing.expect(t, added)
	testing.expect_value(t, len(dependencies), 1)
	testing.expect_value(t, dependencies[0].mode, analyze.Source_Mode.Dependency_Interface)

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	project := analyze.analyze_target_with_candidate_inputs(
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 2)
	testing.expect(t, !project_has_diagnostic(&project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
generic_type_adt_fetch_uses_search_before_direct_ddic_probe :: proc(t: ^testing.T) {
	testing.expect(
		t,
		!remote_deps.adt_candidate_direct_first(deps.Remote_Dependency_Candidate{name = "scit_clas", kind = .Type}),
	)
	testing.expect(
		t,
		remote_deps.adt_candidate_direct_first(
			deps.Remote_Dependency_Candidate{name = "zcl_demo", kind = .Type, hint = .Object_Type},
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
	uri_keys := remote_deps.project_input_uri_keys(target.uri, dependencies[:], candidates[:], 1, context.allocator)

	added := remote_deps.add_adt_fetched_dependency_input(
		&candidates,
		&dependencies,
		deps.Remote_Dependency_Candidate{name = "z_remote_fm", kind = .Function},
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
	)
	testing.expect(t, added)
	testing.expect_value(t, len(dependencies), 1)
	testing.expect_value(t, len(candidates), 0)
	testing.expect_value(t, dependencies[0].mode, analyze.Source_Mode.Dependency_Interface)
	testing.expect(t, strings.contains(dependencies[0].uri, "/functions/groups/ZFG/fmodules/Z_REMOTE_FM"))
	testing.expect(t, !strings.contains(dependencies[0].source, "FUNCTION-POOL"))

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	project := analyze.analyze_target_with_candidate_inputs(
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

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
	uri_keys := remote_deps.project_input_uri_keys(target.uri, dependencies[:], candidates[:], 1, context.allocator)

	added := remote_deps.add_adt_fetched_dependency_input(
		&candidates,
		&dependencies,
		deps.Remote_Dependency_Candidate{name = "tr_objects", kind = .Type},
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
	)
	testing.expect(t, added)
	testing.expect_value(t, len(dependencies), 1)
	testing.expect_value(t, dependencies[0].mode, analyze.Source_Mode.Dependency_Interface)
	testing.expect(t, contains_fold(dependencies[0].source, "type standard table of c with default key"))

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	project := analyze.analyze_target_with_candidate_inputs(
		target,
		candidates[:],
		dependencies[:],
		analyze.Analyze_Options{pool = &pool},
		context.allocator,
	)
	execution.pool_destroy(&pool)

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
ddic_structure_field_resolves_dictionary_reference_data_element :: proc(t: ^testing.T) {
	structure_xml := `<abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name="zrow" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="VALUE">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">ZDE_DATAREF</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType"></abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	data_element_xml := `<blue:wbobj adtcore:name="ZDE_DATAREF" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>refToDictionaryType</dtel:typeKind>
    <dtel:typeName>DATA</dtel:typeName>
    <dtel:dataType/>
  </dtel:dataElement>
</blue:wbobj>`
	structure_source := ddic_xml.dependency_source("ZROW", "ddic-structure", structure_xml, context.allocator)
	defer delete(structure_source, context.allocator)
	data_element_source := ddic_xml.dependency_source("ZDE_DATAREF", "ddic-data-element", data_element_xml, context.allocator)
	defer delete(data_element_source, context.allocator)
	testing.expect(t, contains_fold(structure_source, "value type zde_dataref"))
	testing.expect(t, contains_fold(data_element_source, "type ref to data"))

	target := analyze.Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA ls_row TYPE zrow.",
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-structure/zrow.abap",
			source = structure_source,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/ddic-data-element/zde_dataref.abap",
			source = data_element_source,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	structure_unit := analyze.project_unit_by_uri(&project, dependencies[0].uri)

	testing.expect(t, structure_unit != nil)
	testing.expect(t, reference_resolves_to_uri(&project, structure_unit, "zde_dataref", .Type, .Type_Ref, dependencies[1].uri))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
dependency_xml_detection_prefers_metadata :: proc(t: ^testing.T) {
	testing.expect(t, remote_deps.dependency_source_is_xml("ddic-table-type", "xml", "not xml"))
	testing.expect(t, remote_deps.dependency_source_is_xml("ddic-table-type", "abap", "<ttyp/>"))
	testing.expect(t, !remote_deps.dependency_source_is_xml("global-class", "abap", "<fs> = value."))
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
ddic_xml_direct_include_component_resolves_backing_structure :: proc(t: ^testing.T) {
	ddic_source := `
@EndUserText.label : 'Event Processing Structure: Transaction Event'
define type /sttp/s_proc_evtt {
  proc_evt  : include /sttp/s_proc_evt;
  evtaction : abap.char(10);
}`
	structure_source := ddic_xml.dependency_source("/sttp/s_proc_evtt", "ddic-structure", ddic_source, context.allocator)
	defer delete(structure_source, context.allocator)
	testing.expect(t, contains_fold(structure_source, "include type /sttp/s_proc_evt as proc_evt"))

	target := analyze.Source_Input {
		uri = "mem://ZMAIN.abap",
		source = `
REPORT zmain.
DATA ls_obj_evt TYPE /sttp/s_proc_evt.
DATA ls_obj_event TYPE /sttp/s_proc_evtt.
ls_obj_event-proc_evt = ls_obj_evt.
ls_obj_event-proc_evt-evtaction = ls_obj_evt-evtaction.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-structure/sttp_s_proc_evtt.abap",
			source = structure_source,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/ddic-structure/sttp_s_proc_evt.abap",
			source = `
TYPES: BEGIN OF /sttp/s_proc_evt,
         evtaction TYPE c,
       END OF /sttp/s_proc_evt.
`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, !has_diagnostic(root, .Unknown_Field))
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
	table_type := analyze.find_symbol(table_unit, "zddic_rows", .Type_Def)
	testing.expect(t, table_type != nil)
	if table_type != nil {
		named := expect_type_kind(t, table_unit, table_type.type_id, .Named)
		table := expect_type_kind(t, table_unit, named.base, .Table)
		row := expect_type_kind(t, table_unit, table.base, .Named)
		testing.expect_value(t, row.name, "zddic_row")
	}
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
ddic_table_type_line_of_parameter_resolves_row_fields :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://ZMAIN.abap",
		source = `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS add_sources CHANGING !ct_enhancements TYPE enh_hook_impl_it.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD add_sources.
    DATA lv_source TYPE string.
    FIELD-SYMBOLS <ls_enhancement> LIKE LINE OF ct_enhancements.
    LOOP AT ct_enhancements ASSIGNING <ls_enhancement>.
      lv_source = <ls_enhancement>-full_name.
      INSERT lv_source INTO <ls_enhancement>-source INDEX 1.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-table-type/enh_hook_impl_it.abap",
			source = "TYPES enh_hook_impl_it TYPE STANDARD TABLE OF enh_hook_impl WITH DEFAULT KEY.",
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/ddic-structure/enh_hook_impl.abap",
			source = `TYPES: BEGIN OF enh_hook_impl,
         full_name TYPE string,
         source TYPE rswsourcet,
       END OF enh_hook_impl.`,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/ddic-table-type/rswsourcet.abap",
			source = "TYPES rswsourcet TYPE STANDARD TABLE OF string WITH DEFAULT KEY.",
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
typepool_occurs_table_type_line_of_uses_dependency_include_without_copy :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "mem://ZMAIN.abap",
		source = `REPORT zmain.
DATA lt_request_headers TYPE trwbo_request_headers.
DATA ls_row LIKE LINE OF lt_request_headers.
ls_row-trkorr = 'A'.
ls_row-as4user = sy-uname.
ls_row-as4date = sy-datum.
ls_row-as4time = sy-uzeit.`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-typepool:/trwbo.abap",
			source = `TYPES: BEGIN OF trwbo_request_header.
INCLUDE STRUCTURE e070.
TYPES:    as4text TYPE string,
       END OF trwbo_request_header.
TYPES trwbo_request_headers TYPE trwbo_request_header OCCURS 0.`,
			mode = .Dependency_Interface,
		},
		{
			uri = "abapls-cache:/ddic-table/e070.abap",
			source = `TYPES: BEGIN OF e070,
         trkorr  TYPE string,
         as4user TYPE string,
         as4date TYPE d,
         as4time TYPE t,
       END OF e070.`,
			mode = .Dependency_Interface,
		},
	}
	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)
	typepool := analyze.project_unit_by_uri(&project, dependencies[0].uri)

	testing.expect(t, root != nil)
	testing.expect(t, typepool != nil)
	if typepool != nil {
		headers := analyze.find_symbol(typepool, "trwbo_request_headers", .Type_Def)
		header := analyze.find_symbol(typepool, "trwbo_request_header", .Type_Def)
		testing.expect(t, headers != nil && headers.type_clause_form == .Standard_Table)
		testing.expect(t, header != nil && analyze.structure_field(typepool, header.structure, "trkorr") == nil)
	}
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
adt_fetch_task_result_applies_inputs_without_live_network :: proc(t: ^testing.T) {
	candidates := make([dynamic]analyze.Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]analyze.Source_Input, context.allocator)
	uri_keys := remote_deps.project_input_uri_keys("mem://ZMAIN.abap", dependencies[:], candidates[:], 2, context.allocator)

	result_arena: virtual.Arena
	_ = virtual.arena_init_growing(&result_arena)
	defer virtual.arena_destroy(&result_arena)
	result_allocator := virtual.arena_allocator(&result_arena)
	result := new(remote_deps.Adt_Fetch_Task_Result, result_allocator)
	result.fetched = make([dynamic]remote_deps.Adt_Fetched_Object, 0, 1, result_allocator)
	object_ref := adt.build_class_object_ref("ZCL_TASK_RESULT", "ZPKG", result_allocator)
	candidate := deps.Remote_Dependency_Candidate{name = "zcl_task_result", kind = .Type}
	remote_deps.append_prepared_adt_input(
		result,
		candidate,
		&object_ref,
		"global-class",
		"abap",
		"CLASS zcl_task_result DEFINITION. ENDCLASS. CLASS zcl_task_result IMPLEMENTATION. ENDCLASS.",
		false,
		result_allocator,
		context.temp_allocator,
	)
	include_ref := adt.build_include_object_ref("ZINC_TASK_RESULT", "ZPKG", result_allocator)
	remote_deps.append_prepared_adt_input(
		result,
		deps.Remote_Dependency_Candidate{name = "ZINC_TASK_RESULT", kind = .Include},
		&include_ref,
		"include",
		"abap",
		"DATA gv_shared TYPE string.",
		true,
		result_allocator,
		context.temp_allocator,
	)

	added := remote_deps.add_adt_fetch_task_result(
		&candidates,
		&dependencies,
		candidate,
		result,
		&uri_keys,
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

	remote_deps.store_adt_dependency_fetch(&store, &profile, &object_ref, &fetched, context.allocator)

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

	remote_deps.store_adt_dependency_fetch(&store, &profile, &object_ref, &fetched, context.allocator)

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
cached_ddic_table_with_include_is_stale :: proc(t: ^testing.T) {
	record := dep_store.Stored_Artifact_Record {
		object_kind    = "ddic-table",
		file_extension = "xml",
		source_text    = `<abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="/sttp/rep_evt" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name=".include">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicIncludeName">/sttp/s_rep_evt_att</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`,
	}
	candidate := deps.Remote_Dependency_Candidate{name = "/sttp/rep_evt", kind = .Type}

	testing.expect(t, remote_deps.cached_dependency_record_is_stale(&record, candidate))
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
project_visible_value_root_does_not_satisfy_type_reference :: proc(t: ^testing.T) {
	sources := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/zfoo.abap",
			source = "REPORT zfoo.",
		},
		{
			uri = "file:///workspace/zconsumer.abap",
			source = "DATA lo_foo TYPE REF TO zfoo.",
		},
	}

	project := analyze_units_project_test(t, sources[:])
	consumer := analyze.project_unit_by_uri(&project, sources[1].uri)

		testing.expect(t, consumer != nil)
	if consumer != nil {
		testing.expect(t, !reference_resolves_to_uri(&project, consumer, "zfoo", .Type, .Type_Ref, sources[0].uri))
		testing.expect(t, has_diagnostic(consumer, .Wrong_Namespace))
	}
}

@(test)
project_visible_type_root_does_not_satisfy_routine_reference :: proc(t: ^testing.T) {
	sources := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/zfoo.abap",
			source = "TYPES zfoo TYPE i.",
		},
		{
			uri = "file:///workspace/zconsumer.abap",
			source = "PERFORM zfoo.",
		},
	}

	project := analyze_units_project_test(t, sources[:])
	consumer := analyze.project_unit_by_uri(&project, sources[1].uri)

	testing.expect(t, consumer != nil)
	if consumer != nil {
		testing.expect(t, !reference_resolves_to_uri(&project, consumer, "zfoo", .Routine, .Routine_Call, sources[0].uri))
		testing.expect(t, has_diagnostic(consumer, .Wrong_Namespace))
	}
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
finish_project_analysis_respects_include_predecessor_order :: proc(t: ^testing.T) {
	prior := [?]analyze.Source_Input {
		{uri = "file:///workspace/zmain.abap", source = "REPORT zmain. INCLUDE: ztypes, zdata."},
		{uri = "file:///workspace/ztypes.abap", source = `
TYPES: BEGIN OF ts_obj_ids,
         owner TYPE c LENGTH 12,
       END OF ts_obj_ids.
`},
		{uri = "file:///workspace/zdata.abap", source = "DATA ls_object_src TYPE ts_obj_ids."},
	}
	later := [?]analyze.Source_Input {
		{uri = "file:///workspace/zmain.abap", source = "REPORT zmain. INCLUDE: zdata, ztypes."},
		{uri = "file:///workspace/zdata.abap", source = "DATA ls_object_src TYPE ts_obj_ids."},
		{uri = "file:///workspace/ztypes.abap", source = `
TYPES: BEGIN OF ts_obj_ids,
         owner TYPE c LENGTH 12,
       END OF ts_obj_ids.
`},
	}

	prior_project := analyze_units_project_test(t, prior[:])
	prior_data := analyze.project_unit_by_uri(&prior_project, prior[2].uri)
	later_project := analyze_units_project_test(t, later[:])
	later_data := analyze.project_unit_by_uri(&later_project, later[1].uri)

	testing.expect(t, prior_data != nil)
	testing.expect(t, prior_data != nil && !has_diagnostic(prior_data, .Unresolved_Reference))
	testing.expect(t, prior_data != nil && reference_resolves_to_uri(&prior_project, prior_data, "ts_obj_ids", .Type, .Type_Ref, prior[1].uri))
	testing.expect(t, later_data != nil)
	testing.expect(t, later_data != nil && has_diagnostic(later_data, .Unresolved_Reference))
}

@(test)
finish_project_analysis_links_class_implementation_across_ordered_includes :: proc(t: ^testing.T) {
	sources := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/zmain.abap",
			source = `
REPORT zmain.
INCLUDE: ztop, zcls.
START-OF-SELECTION.
  CREATE OBJECT gr_demo.
  CALL METHOD gr_demo->get_data.
`,
		},
		{
			uri = "file:///workspace/ztop.abap",
			source = `
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS get_data.
ENDCLASS.
`,
		},
		{
			uri = "file:///workspace/zcls.abap",
			source = `
DATA gr_demo TYPE REF TO lcl_demo.
CLASS lcl_demo IMPLEMENTATION.
  METHOD get_data.
  ENDMETHOD.
ENDCLASS.
`,
		},
	}

	project := analyze_units_project_test(t, sources[:])
	top := analyze.project_unit_by_uri(&project, sources[1].uri)
	cls := analyze.project_unit_by_uri(&project, sources[2].uri)
	class_symbol: ^analyze.Symbol_Data
	member: ^analyze.Symbol_Data
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
	member_info := analyze.entity_decl_info(top, member.id) if top != nil && member != nil else nil
	testing.expect(t, member_info != nil && .Has_Implementation in member_info.flags)
	testing.expect(t, member_info != nil && member_info.implementation_unit == cls.unit_id)
	testing.expect(t, top != nil && !has_diagnostic(top, .Missing_Method_Implementation))
}

@(test)
finish_project_analysis_resolves_redefined_method_parameters_from_parent_unit :: proc(t: ^testing.T) {
	sources := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/zcl_parent.abap",
			source = `
CLASS zcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_text TYPE string.
ENDCLASS.
CLASS zcl_parent IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
`,
		},
		{
			uri = "file:///workspace/zcl_child.abap",
			source = `
CLASS zcl_child DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    METHODS run REDEFINITION.
ENDCLASS.
CLASS zcl_child IMPLEMENTATION.
  METHOD run.
    DATA lv_text TYPE string.
    lv_text = iv_text.
  ENDMETHOD.
ENDCLASS.
`,
		},
	}

	project := analyze_units_project_test(t, sources[:])
	child := analyze.project_unit_by_uri(&project, sources[1].uri)

	testing.expect(t, child != nil)
	testing.expect(t, child != nil && !has_diagnostic(child, .Unresolved_Reference))
	testing.expect(t, child != nil && has_reference(child, "iv_text", .Value, .Identifier))
}

@(test)
finish_project_analysis_derives_event_handler_signature_parameters_from_event_unit :: proc(t: ^testing.T) {
	sources := [?]analyze.Source_Input {
		{
			uri = "file:///workspace/zcl_source.abap",
			source = `
CLASS zcl_source DEFINITION.
  PUBLIC SECTION.
    DATA object_type TYPE string.
    EVENTS saved EXPORTING VALUE(ex_object) TYPE REF TO zcl_source.
ENDCLASS.
`,
		},
		{
			uri = "file:///workspace/zhandler.abap",
			source = `
CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_saved FOR EVENT saved OF zcl_source IMPORTING ex_object.
ENDCLASS.
CLASS lcl_handler IMPLEMENTATION.
  METHOD on_saved.
    DATA lv_type TYPE string.
    lv_type = ex_object->object_type.
  ENDMETHOD.
ENDCLASS.
`,
		},
	}

	project := analyze_units_project_test(t, sources[:])
	handler := analyze.project_unit_by_uri(&project, sources[1].uri)
	testing.expect(t, handler != nil)
	testing.expect(t, handler != nil && !has_diagnostic(handler, .Unknown_Field))
	handler_class: ^analyze.Symbol_Data
	if handler != nil {
		handler_class = analyze.find_symbol(handler, "lcl_handler", .Class)
	}
	handler_member: ^analyze.Symbol_Data
	if handler_class != nil {
		handler_member = class_member_named(handler, handler_class.id, "on_saved", .Method)
	}
	handler_info: ^analyze.Decl_Info_Data
	if handler_member != nil {
		handler_info = analyze.entity_decl_info(handler, handler_member.id)
	}
	testing.expect(t, handler_info != nil)
	testing.expect(t, handler_info != nil && len(handler_info.signature_parameters) == 1)
	testing.expect(t, handler_info != nil && len(handler_info.signature_parameters) == 1 && .Has_Declared_Type in handler_info.signature_parameters[0].flags)
	testing.expect(t, handler_info != nil && len(handler_info.signature_parameters) == 1 && handler_info.signature_parameters[0].declared_type.base_name == "zcl_source")
	testing.expect(t, handler_info != nil && len(handler_info.signature_parameters) == 1 && .Has_Event_Derived_Type in handler_info.signature_parameters[0].flags)
}

@(test)
finish_project_analysis_imports_structure_types_across_includes :: proc(t: ^testing.T) {
	sources := [?]analyze.Source_Input {
		{uri = "file:///workspace/zmain.abap", source = "REPORT zmain. INCLUDE: ztop, zf01."},
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

	project := analyze_units_project_test(t, sources[:])
	form := analyze.project_unit_by_uri(&project, sources[2].uri)

	testing.expect(t, form != nil)
	testing.expect(t, form != nil && reference_resolves_to_uri(&project, form, "gs_row", .Value, .Identifier, sources[1].uri))
	testing.expect(t, form != nil && !has_diagnostic(form, .Unknown_Field))
}

@(test)
finish_project_analysis_reclassifies_open_sql_predicate_globals_from_prior_include :: proc(t: ^testing.T) {
	sources := [?]analyze.Source_Input {
		{uri = "file:///workspace/zmain.abap", source = "REPORT zmain. INCLUDE: ztop, zf01."},
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

	project := analyze_units_project_test(t, sources[:])
	form := analyze.project_unit_by_uri(&project, sources[2].uri)
	names := [?]string{"p_lgnum", "p_lgtyp", "p_lgpla"}

	testing.expect(t, form != nil)
	for name in names {
		testing.expect(t, form != nil && !sql_name_ref_present(form, name, .Column))
		testing.expect(t, form != nil && reference_resolves_to_uri(&project, form, name, .Value, .Identifier, sources[1].uri))
	}
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
analyze_target_infers_select_options_row_across_includes :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: zsel, zf01.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/zsel.abap", source = `
DATA gv_doc TYPE string.
SELECT-OPTIONS so_dels FOR gv_doc.
`},
		{uri = "file:///workspace/zf01.abap", source = `
FORM run.
  LOOP AT so_dels INTO DATA(ls_doc).
    gv_doc = ls_doc-low.
  ENDLOOP.
ENDFORM.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	form := analyze.project_unit_by_uri(&project, candidates[1].uri)

	testing.expect(t, form != nil)
	testing.expect(t, !has_diagnostic(form, .Unknown_Field))
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
DATA gr_demo TYPE REF TO lcl_demo.
`},
		{uri = "file:///workspace/zcls.abap", source = `
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
	member: ^analyze.Symbol_Data
	if top != nil {
		class_symbol = analyze.find_symbol(top, "lcl_demo", .Class)
		if class_symbol != nil {
			member = class_member_named(top, class_symbol.id, "get_data", .Method)
		}
	}

	testing.expect(t, top != nil)
	testing.expect(t, cls != nil)
	root := analyze.project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil && !has_diagnostic(root, .Unknown_Field))
	testing.expect(t, class_symbol != nil)
	testing.expect(t, member != nil)
	member_info := analyze.entity_decl_info(top, member.id) if top != nil && member != nil else nil
	testing.expect(t, member_info != nil && .Has_Implementation in member_info.flags)
	testing.expect_value(t, member_info.implementation_unit, cls.unit_id)
	testing.expect(t, !has_diagnostic(top, .Missing_Method_Implementation))
}

@(test)
analyze_target_uses_provider_structure_across_includes_without_copy :: proc(t: ^testing.T) {
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
	testing.expect(t, form != nil && analyze.find_structure(form, "ty_row") == nil)
}

@(test)
cross_unit_field_fact_keeps_provider_structure_without_local_copy :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: ztop, zf01.",
	}
	candidates := [?]analyze.Source_Input {
		{uri = "file:///workspace/ztop.abap", source = `
TYPES: BEGIN OF ty_child,
         name TYPE string,
       END OF ty_child.
TYPES: BEGIN OF ty_wrap,
         child TYPE ty_child,
       END OF ty_wrap.
DATA gs_wrap TYPE ty_wrap.
`},
		{uri = "file:///workspace/zf01.abap", source = `
FORM run.
  DATA ls_child TYPE ty_child.
  ls_child = gs_wrap-child.
  ls_child-name = gs_wrap-child-name.
ENDFORM.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	top := analyze.project_unit_by_uri(&project, candidates[0].uri)
	form := analyze.project_unit_by_uri(&project, candidates[1].uri)
	testing.expect(t, top != nil)
	testing.expect(t, form != nil)
	testing.expect(t, form != nil && !has_diagnostic(form, .Unknown_Field))
	if top == nil || form == nil {
		return
	}

	field_offset := find_text(candidates[1].source, "gs_wrap-child") + len("gs_wrap-")
	fact := sem_query.fact_expression_fact_at_offset(sem_query.facts(sem_query.semantic(form)), field_offset)
	testing.expect(t, fact != nil)
	if fact != nil {
		testing.expect_value(t, fact.type_fact.structure_unit, top.unit_id)
		testing.expect(t, fact.type_fact.structure != analyze.INVALID_STRUCTURE_ID)
	}
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
like_line_of_provider_structure_table_component_keeps_component_path :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS fill CHANGING cs_data TYPE zcomponent.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD fill.
    DATA ls_item LIKE LINE OF cs_data-items.
    ls_item-name = 'x'.
  ENDMETHOD.
ENDCLASS.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{uri = "abapls-cache:/ddic-structure/zitem.abap", source = `
TYPES: BEGIN OF zitem,
         name TYPE string,
       END OF zitem.
`},
		{uri = "abapls-cache:/ddic-table-type/zitem_tab.abap", source = `
TYPES zitem_tab TYPE STANDARD TABLE OF zitem WITH DEFAULT KEY.
`},
		{uri = "abapls-cache:/ddic-structure/zcomponent.abap", source = `
TYPES: BEGIN OF zcomponent,
         items TYPE zitem_tab,
       END OF zcomponent.
`},
	}

	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, !has_diagnostic(root, .Unknown_Field))
}

@(test)
tables_work_area_type_ref_prefers_ddic_type_over_value_symbol :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = `
TABLES sscrfields.

FORM run.
  sscrfields-ucomm = 'RUN'.
ENDFORM.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{uri = "abapls-cache:/ddic-structure/sscrfields.abap", source = `
TYPES: BEGIN OF sscrfields,
         ucomm TYPE string,
       END OF sscrfields.
`},
	}

	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, !has_diagnostic(root, .Unknown_Field))
}

@(test)
like_line_of_parameter_typed_as_class_structure_component_uses_provider_structure :: proc(t: ^testing.T) {
	target := analyze.Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = `
INTERFACE lif_descr.
  TYPES ty_texts TYPE STANDARD TABLE OF ztext WITH DEFAULT KEY.
ENDINTERFACE.

CLASS lcl_obj DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_data,
             texts TYPE lif_descr=>ty_texts,
           END OF ty_data.
ENDCLASS.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS fill
      EXPORTING et_texts TYPE lcl_obj=>ty_data-texts.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD fill.
    DATA ls_text LIKE LINE OF et_texts.
    ls_text-clsname = 'X'.
  ENDMETHOD.
ENDCLASS.
`,
	}
	dependencies := [?]analyze.Source_Input {
		{
			uri = "abapls-cache:/ddic-structure/ztext.abap",
			source = `
TYPES: BEGIN OF ztext,
         clsname TYPE string,
       END OF ztext.
`,
		},
	}

	project := analyze_project_dependencies_test(t, target, dependencies[:])
	root := analyze.project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, !has_diagnostic(root, .Unknown_Field))
	ls_text := analyze.find_symbol(root, "ls_text", .Variable)
	testing.expect(t, ls_text != nil && ls_text.structure == analyze.INVALID_STRUCTURE_ID)
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
analyze_workspace_uses_manifest_roots :: proc(t: ^testing.T) {
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

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	opened, workspace_ok, _ := workspace.open_workspace(
		root,
		workspace.Options{pool = &pool},
		context.allocator,
	)
	testing.expect(t, workspace_ok)
	if !workspace_ok {
		execution.pool_destroy(&pool)
		return
	}
	result := workspace.analyze_workspace(&opened, nil, workspace.Options{pool = &pool}, context.allocator)
	workspace.workspace_destroy(&opened, context.allocator)
	execution.pool_destroy(&pool)

	testing.expect(t, result.ok)
	testing.expect(t, result.used_manifest)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, main_file) != nil)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, other_file) != nil)
}

@(test)
analyze_workspace_with_empty_manifest_analyzes_files :: proc(t: ^testing.T) {
	root := manifest_workspace_path("folder-path-empty-manifest")
	manifest_test_file(t, root, "abapls.toml", "version = 1")
	main_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain. INCLUDE zinc. lv_inc = 1.")
	include_file := manifest_test_file(t, root, "src/ZINC.abap", "DATA lv_inc TYPE i.")

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	opened, workspace_ok, _ := workspace.open_workspace(
		root,
		workspace.Options{pool = &pool},
		context.allocator,
	)
	testing.expect(t, workspace_ok)
	if !workspace_ok {
		execution.pool_destroy(&pool)
		return
	}
	result := workspace.analyze_workspace(&opened, nil, workspace.Options{pool = &pool}, context.allocator)
	workspace.workspace_destroy(&opened, context.allocator)
	execution.pool_destroy(&pool)
	root_unit := analyze.project_unit_by_uri(&result.project, main_file)

	testing.expect(t, result.ok)
	testing.expect_value(t, len(result.project.units), 2)
	testing.expect(t, root_unit != nil)
	if root_unit != nil {
		testing.expect_value(t, include_target_uri(&result.project, root_unit, "zinc"), include_file)
	}
}

@(test)
standalone_workspace_does_not_open_parent_manifest :: proc(t: ^testing.T) {
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

	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 128}, context.allocator)
	opened, workspace_ok, _ := workspace.open_standalone_workspace(
		filepath.dir(main_file),
		workspace.Options{pool = &pool},
		context.allocator,
	)
	testing.expect(t, workspace_ok)
	if !workspace_ok {
		execution.pool_destroy(&pool)
		return
	}
	result := workspace.analyze_path(&opened, main_file, nil, workspace.Options{pool = &pool}, context.allocator)
	workspace.workspace_destroy(&opened, context.allocator)
	execution.pool_destroy(&pool)

	testing.expect(t, result.ok)
	testing.expect(t, !result.used_manifest)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, main_file) != nil)
	testing.expect(t, analyze.project_unit_by_uri(&result.project, other_file) == nil)
}
