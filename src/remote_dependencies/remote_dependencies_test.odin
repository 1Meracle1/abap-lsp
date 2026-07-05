package abap_frontend_remote_dependencies

import adt "src:adt"
import "src:ast"
import dep_store "src:dependency_store"
import execution "src:execution"

import "core:mem"
import "core:net"
import "core:os"
import filepath "core:path/filepath"
import "core:strings"
import "core:testing"
import "core:thread"
import "core:time"

Remote_ADT_Test_Server :: struct {
	listener:         net.TCP_Socket,
	session_response: string,
	source_response:  string,
	request_buf:      [4096]u8,
	session_count:    int,
	source_count:     int,
}

remote_adt_test_server_run :: proc(t: ^thread.Thread) {
	server := cast(^Remote_ADT_Test_Server)t.data
	for {
		client, _, err := net.accept_tcp(server.listener)
		if err != nil {
			return
		}
		n, recv_err := net.recv_tcp(client, server.request_buf[:])
		response := server.session_response
		if recv_err == nil {
			request := string(server.request_buf[:n])
			if strings.contains(request, "/runtime/systemmessages") {
				server.session_count += 1
			} else {
				server.source_count += 1
				response = server.source_response
			}
		}
		net.send_tcp(client, transmute([]u8)response)
		net.close(client)
	}
}

remote_adt_test_client :: proc(
	t: ^testing.T,
	server: ^Remote_ADT_Test_Server,
) -> (adt.Client, ^thread.Thread) {
	listener, listen_err := net.listen_tcp(net.Endpoint{address = net.IP4_Loopback, port = 0})
	testing.expect(t, listen_err == nil)
	net.set_option(listener, .Receive_Timeout, 500 * time.Millisecond)
	ep, ep_err := net.bound_endpoint(listener)
	testing.expect(t, ep_err == nil)
	server.listener = listener
	worker := thread.create(remote_adt_test_server_run)
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

remote_adt_test_client_destroy :: proc(client: ^adt.Client, allocator: mem.Allocator) {
	delete(client.connection.base_url, allocator)
	adt.client_destroy(client, allocator)
}

remote_adt_test_server_stop :: proc(
	server: ^Remote_ADT_Test_Server,
	worker: ^thread.Thread,
) {
	net.close(server.listener)
	thread.join(worker)
	thread.destroy(worker)
}

remote_test_http_response_status :: proc(
	status, body, extra_headers: string,
	allocator: mem.Allocator,
) -> string {
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

@(test)
remote_dependency_requests_are_normalized_and_deduped :: proc(t: ^testing.T) {
	requests := [?]Request {
		{name = " ZCL_DEMO ", kind = .Class},
		{name = "zcl_demo", kind = .Class},
		{name = "I", kind = .Type},
	}

	normalized := normalize_requests(requests[:], context.allocator)

	testing.expect_value(t, len(normalized), 2)
	testing.expect_value(t, normalized[0].name, "zcl_demo")
	testing.expect_value(t, normalized[1].name, "i")

	unseen := unseen_requests(normalized[:], nil, context.allocator)
	testing.expect_value(t, len(unseen), 2)
	testing.expect_value(t, unseen[0].name, "zcl_demo")
	testing.expect_value(t, unseen[1].name, "i")
}

@(test)
typepool_macro_expansion_uses_ast_macro_expander :: proc(t: ^testing.T) {
	source := `TYPE-POOL zfoo.
DEFINE set_field.
  &1 = &2.
END-OF-DEFINITION.
set_field lv_a 'B'.`

	expanded := expanded_typepool_dependency_source(nil, source, context.allocator)

	testing.expect_value(t, expanded, "lv_a = 'B'.")
}

@(test)
remote_dependency_class_interface_prunes_private_and_implementation :: proc(t: ^testing.T) {
	source := `CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS public_method.
  PROTECTED SECTION.
    METHODS protected_method.
  PRIVATE SECTION.
    METHODS private_method.
ENDCLASS.
CLASS zcl_demo IMPLEMENTATION.
  METHOD public_method.
  ENDMETHOD.
ENDCLASS.`
	artifact := Artifact {
		request        = Request{name = "zcl_demo", kind = .Class},
		source_kind    = .Cache,
		object_kind    = "global-class",
		object_name    = "zcl_demo",
		object_uri     = "/sap/bc/adt/oo/classes/zcl_demo",
		object_type    = "CLAS/OC",
		file_extension = "abap",
		source_text    = source,
	}
	result := result_make(context.allocator)
	state := state_make(context.allocator)

	added := result_add_artifact(&result, &artifact, &state, context.allocator)

	testing.expect(t, added)
	testing.expect_value(t, len(result.interfaces), 1)
	root := result.interfaces[0].root
	testing.expect_value(t, len(root.stmts), 1)
	class_decl := root.stmts[0].derived_stmt.(^ast.Class_Decl)
	testing.expect_value(t, len(class_decl.body), 4)
	testing.expect_value(t, class_decl.body[0].derived_stmt.(^ast.Oop_Simple_Stmt).visibility, ast.Oop_Visibility.Public)
	testing.expect_value(t, class_decl.body[2].derived_stmt.(^ast.Oop_Simple_Stmt).visibility, ast.Oop_Visibility.Protected)
	for stmt in class_decl.body {
		if oop, ok := stmt.derived_stmt.(^ast.Oop_Simple_Stmt); ok {
			testing.expect(t, oop.visibility != ast.Oop_Visibility.Private)
			if len(oop.members) > 0 {
				testing.expect(t, !strings.equal_fold(oop.members[0].name.text, "private_method"))
			}
		}
	}
}

@(test)
remote_dependency_include_returns_full_source_with_provided_names :: proc(t: ^testing.T) {
	artifact := Artifact {
		request        = Request{name = "zinc_demo", kind = .Include},
		source_kind    = .Cache,
		object_kind    = "include",
		object_name    = "zinc_demo",
		object_uri     = "/sap/bc/adt/programs/includes/zinc_demo",
		object_type    = "PROG/I",
		file_extension = "abap",
		source_text    = "DATA gv_value TYPE i.",
	}
	result := result_make(context.allocator)
	state := state_make(context.allocator)

	added := result_add_artifact(&result, &artifact, &state, context.allocator)

	testing.expect(t, added)
	testing.expect_value(t, len(result.sources), 1)
	testing.expect_value(t, len(result.interfaces), 0)
	testing.expect_value(t, result.sources[0].provided_names[0], "zinc_demo")
}

@(test)
remote_dependency_local_export_resolves_to_interface_ast :: proc(t: ^testing.T) {
	root := remote_dependency_test_root("local_export")
	os.remove_all(root)
	testing.expect(t, os.make_directory_all(root) == nil)
	path, _ := filepath.join({root, "zcl_local.abap"}, context.allocator)
	source := "CLASS zcl_local DEFINITION. PUBLIC SECTION. METHODS run. ENDCLASS."
	testing.expect(t, os.write_entire_file(path, source) == nil)

	config := Config {
		local_export_roots = {root},
		source_order       = .Local_First,
	}
	state := state_make(context.allocator)
	requests := [?]Request {
		{name = "zcl_local", kind = .Class},
	}

	result := resolve_requests(requests[:], &config, &state, nil, context.allocator)

	testing.expect_value(t, len(result.interfaces), 1)
	testing.expect_value(t, len(result.misses), 0)
}

@(test)
remote_dependency_local_export_uses_pool_for_multiple_requests :: proc(t: ^testing.T) {
	root := remote_dependency_test_root("local_export_pool")
	os.remove_all(root)
	testing.expect(t, os.make_directory_all(root) == nil)
	first_path, _ := filepath.join({root, "zcl_pool_a.abap"}, context.allocator)
	second_path, _ := filepath.join({root, "zcl_pool_b.abap"}, context.allocator)
	include_path, _ := filepath.join({root, "zinc_pool.abap"}, context.allocator)
	testing.expect(
		t,
		os.write_entire_file(
			first_path,
			"CLASS zcl_pool_a DEFINITION. PUBLIC SECTION. METHODS run. ENDCLASS.",
		) == nil,
	)
	testing.expect(
		t,
		os.write_entire_file(
			second_path,
			"CLASS zcl_pool_b DEFINITION. PUBLIC SECTION. METHODS run. ENDCLASS.",
		) == nil,
	)
	testing.expect(
		t,
		os.write_entire_file(
			include_path,
			"DATA gv_pool TYPE i.",
		) == nil,
	)

	pool: execution.Pool
	execution.pool_init(
		&pool,
		execution.Options{worker_count = 2, task_capacity = 8, edge_capacity = 8},
		context.allocator,
	)
	defer execution.pool_destroy(&pool)
	execution.pool_start(&pool)

	config := Config {
		local_export_roots = {root},
		source_order       = .Local_First,
	}
	state := state_make(context.allocator)
	requests := [?]Request {
		{name = "zcl_pool_a", kind = .Class},
		{name = "zcl_pool_b", kind = .Class},
		{name = "zinc_pool", kind = .Include},
	}

	result := resolve_requests(requests[:], &config, &state, &pool, context.allocator)

	testing.expect_value(t, len(result.interfaces), 2)
	testing.expect_value(t, len(result.sources), 1)
	testing.expect_value(t, len(result.misses), 0)
	for input in result.interfaces {
		testing.expect(t, input.path != "")
		testing.expect(t, input.root != nil)
		testing.expect_value(t, len(input.root.stmts), 1)
	}
	testing.expect(t, result.sources[0].path != "")
	testing.expect(t, result.sources[0].root != nil)
	testing.expect_value(t, len(result.sources[0].root.stmts), 1)
	testing.expect_value(t, result.sources[0].provided_names[0], "zinc_pool")
}

@(test)
remote_dependency_cache_uses_pool_for_multiple_requests :: proc(t: ^testing.T) {
	path := remote_dependency_test_store_path("cache_pool.sqlite3")
	store, store_err := dep_store.dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, store_err, dep_store.Store_Error.None)
	profile := standalone_dependency_profile()
	artifacts := [?]dep_store.Stored_Artifact_Input {
		{
			package_name   = "zcl_cache_pool_a",
			object_kind    = "global-class",
			object_name    = "zcl_cache_pool_a",
			object_uri     = "/sap/bc/adt/oo/classes/zcl_cache_pool_a",
			object_type    = "CLAS/OC",
			description    = "Cache pool A",
			file_extension = "abap",
			source_text    = "CLASS zcl_cache_pool_a DEFINITION. PUBLIC SECTION. METHODS run. ENDCLASS.",
			fetched_at     = "2026-06-07T00:00:00Z",
		},
		{
			package_name   = "zcl_cache_pool_b",
			object_kind    = "global-class",
			object_name    = "zcl_cache_pool_b",
			object_uri     = "/sap/bc/adt/oo/classes/zcl_cache_pool_b",
			object_type    = "CLAS/OC",
			description    = "Cache pool B",
			file_extension = "abap",
			source_text    = "CLASS zcl_cache_pool_b DEFINITION. PUBLIC SECTION. METHODS run. ENDCLASS.",
			fetched_at     = "2026-06-07T00:00:00Z",
		},
		{
			package_name   = "zcache_pool_row",
			object_kind    = "ddic-structure",
			object_name    = "zcache_pool_row",
			object_uri     = "/sap/bc/adt/ddic/structures/zcache_pool_row",
			object_type    = "TABL/DS",
			description    = "Cache pool DDIC row",
			file_extension = "ddic",
			source_text    = "define type zcache_pool_row { value : abap.int4; }",
			fetched_at     = "2026-06-07T00:00:00Z",
		},
	}
	_, put_err := dep_store.put_artifacts(&store, &profile, artifacts[:], context.allocator)
	testing.expect_value(t, put_err, dep_store.Store_Error.None)

	pool: execution.Pool
	execution.pool_init(
		&pool,
		execution.Options{worker_count = 2, task_capacity = 8, edge_capacity = 8},
		context.allocator,
	)
	defer execution.pool_destroy(&pool)
	execution.pool_start(&pool)

	config := Config{cache = &store, profile = &profile}
	state := state_make(context.allocator)
	requests := [?]Request {
		{name = "zcl_cache_pool_a", kind = .Class},
		{name = "zcl_cache_pool_b", kind = .Class},
		{name = "zcache_pool_row", kind = .Type},
	}
	stats_before := execution.pool_stats(&pool)

	result := resolve_requests(requests[:], &config, &state, &pool, context.allocator)

	stats_after := execution.pool_stats(&pool)
	testing.expect_value(t, len(result.interfaces), 3)
	testing.expect_value(t, len(result.misses), 0)
	for input in result.interfaces {
		testing.expect(t, input.path != "")
		testing.expect(t, input.root != nil)
		testing.expect_value(t, len(input.root.stmts), 1)
	}
	testing.expect(t, stats_after.submitted > stats_before.submitted)
}

@(test)
remote_dependency_cache_hit_does_not_probe_adt :: proc(t: ^testing.T) {
	bad_session := remote_test_http_response_status("503 Service Unavailable", "down", "", context.allocator)
	source_response := remote_test_http_response_status(
		"200 OK",
		"CLASS zcl_cache_no_probe DEFINITION. ENDCLASS.",
		"",
		context.allocator,
	)
	defer delete(bad_session, context.allocator)
	defer delete(source_response, context.allocator)
	server := Remote_ADT_Test_Server {
		session_response = bad_session,
		source_response  = source_response,
	}
	client, worker := remote_adt_test_client(t, &server)
	defer remote_adt_test_client_destroy(&client, context.allocator)
	defer remote_adt_test_server_stop(&server, worker)

	path := remote_dependency_test_store_path("cache_hit_does_not_probe_adt.sqlite3")
	store, store_err := dep_store.dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, store_err, dep_store.Store_Error.None)
	profile := standalone_dependency_profile()
	artifact := dep_store.Stored_Artifact_Input {
		package_name   = "zpkg",
		object_kind    = "global-class",
		object_name    = "zcl_cache_no_probe",
		object_uri     = "/sap/bc/adt/oo/classes/zcl_cache_no_probe",
		object_type    = "CLAS/OC",
		description    = "Cache hit",
		file_extension = "abap",
		source_text    = "CLASS zcl_cache_no_probe DEFINITION. ENDCLASS.",
		fetched_at     = "2026-06-07T00:00:00Z",
	}
	_, put_err := dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, put_err, dep_store.Store_Error.None)

	availability: ADT_Availability
	config := Config {
		cache            = &store,
		profile          = &profile,
		adt_client       = &client,
		adt_availability = &availability,
		source_order     = .ADT_First,
	}
	state := state_make(context.allocator)
	request := Request{name = "zcl_cache_no_probe", kind = .Class}

	result := resolve_requests({request}, &config, &state, nil, context.allocator)

	testing.expect_value(t, len(result.interfaces), 1)
	testing.expect_value(t, len(result.misses), 0)
	testing.expect_value(t, server.session_count, 0)
	testing.expect_value(t, server.source_count, 0)
	testing.expect_value(t, availability.status, ADT_Availability_Status.Unknown)
}

@(test)
remote_dependency_local_export_hit_does_not_probe_adt :: proc(t: ^testing.T) {
	bad_session := remote_test_http_response_status("503 Service Unavailable", "down", "", context.allocator)
	source_response := remote_test_http_response_status(
		"200 OK",
		"CLASS zcl_local_no_probe DEFINITION. ENDCLASS.",
		"",
		context.allocator,
	)
	defer delete(bad_session, context.allocator)
	defer delete(source_response, context.allocator)
	server := Remote_ADT_Test_Server {
		session_response = bad_session,
		source_response  = source_response,
	}
	client, worker := remote_adt_test_client(t, &server)
	defer remote_adt_test_client_destroy(&client, context.allocator)
	defer remote_adt_test_server_stop(&server, worker)

	root := remote_dependency_test_root("local_export_no_adt_probe")
	os.remove_all(root)
	testing.expect(t, os.make_directory_all(root) == nil)
	path, _ := filepath.join({root, "zcl_local_no_probe.abap"}, context.allocator)
	source := "CLASS zcl_local_no_probe DEFINITION. ENDCLASS."
	testing.expect(t, os.write_entire_file(path, source) == nil)

	availability: ADT_Availability
	config := Config {
		local_export_roots = {root},
		adt_client         = &client,
		adt_availability   = &availability,
		source_order       = .Local_First,
	}
	state := state_make(context.allocator)
	request := Request{name = "zcl_local_no_probe", kind = .Class}

	result := resolve_requests({request}, &config, &state, nil, context.allocator)

	testing.expect_value(t, len(result.interfaces), 1)
	testing.expect_value(t, len(result.misses), 0)
	testing.expect_value(t, server.session_count, 0)
	testing.expect_value(t, server.source_count, 0)
	testing.expect_value(t, availability.status, ADT_Availability_Status.Unknown)
}

@(test)
remote_dependency_disabled_adt_candidate_fetch_does_not_probe_adt :: proc(t: ^testing.T) {
	bad_session := remote_test_http_response_status("503 Service Unavailable", "down", "", context.allocator)
	source_response := remote_test_http_response_status(
		"200 OK",
		"CLASS zcl_no_fetch DEFINITION. ENDCLASS.",
		"",
		context.allocator,
	)
	defer delete(bad_session, context.allocator)
	defer delete(source_response, context.allocator)
	server := Remote_ADT_Test_Server {
		session_response = bad_session,
		source_response  = source_response,
	}
	client, worker := remote_adt_test_client(t, &server)
	defer remote_adt_test_client_destroy(&client, context.allocator)
	defer remote_adt_test_server_stop(&server, worker)

	availability: ADT_Availability
	config := Config {
		adt_client                  = &client,
		adt_availability            = &availability,
		source_order                = .ADT_First,
		disable_adt_candidate_fetch = true,
	}
	state := state_make(context.allocator)
	request := Request{name = "zcl_no_fetch", kind = .Class}

	result := resolve_requests({request}, &config, &state, nil, context.allocator)

	testing.expect_value(t, len(result.interfaces), 0)
	testing.expect_value(t, len(result.misses), 1)
	testing.expect_value(t, len(result.blocked_requests), 0)
	testing.expect_value(t, server.session_count, 0)
	testing.expect_value(t, server.source_count, 0)
	testing.expect_value(t, availability.status, ADT_Availability_Status.Unknown)
}

@(test)
remote_dependency_failed_adt_probe_disables_session :: proc(t: ^testing.T) {
	bad_session := remote_test_http_response_status("503 Service Unavailable", "down", "", context.allocator)
	good_session := remote_test_http_response_status("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator)
	source_response := remote_test_http_response_status(
		"200 OK",
		"CLASS zcl_after_probe DEFINITION. ENDCLASS.",
		"",
		context.allocator,
	)
	defer delete(bad_session, context.allocator)
	defer delete(good_session, context.allocator)
	defer delete(source_response, context.allocator)
	server := Remote_ADT_Test_Server {
		session_response = bad_session,
		source_response  = source_response,
	}
	client, worker := remote_adt_test_client(t, &server)
	defer remote_adt_test_client_destroy(&client, context.allocator)
	defer remote_adt_test_server_stop(&server, worker)

	availability: ADT_Availability
	config := Config {
		adt_client       = &client,
		adt_availability = &availability,
		source_order     = .ADT_First,
	}
	first_state := state_make(context.allocator)
	first_request := Request{name = "zcl_down", kind = .Class}

	first := resolve_requests({first_request}, &config, &first_state, nil, context.allocator)

	testing.expect_value(t, len(first.interfaces), 0)
	testing.expect_value(t, len(first.blocked_requests), 1)
	testing.expect_value(t, len(first.diagnostics), 1)
	testing.expect_value(t, availability.status, ADT_Availability_Status.Unavailable)
	testing.expect_value(t, availability.error, adt.Error.Bad_Status)
	testing.expect_value(t, server.session_count, 1)
	testing.expect_value(t, server.source_count, 0)

	server.session_response = good_session
	second_state := state_make(context.allocator)
	second_request := Request{name = "zcl_after_probe", kind = .Class}

	second := resolve_requests({second_request}, &config, &second_state, nil, context.allocator)

	testing.expect_value(t, len(second.interfaces), 0)
	testing.expect_value(t, len(second.blocked_requests), 1)
	testing.expect_value(t, server.session_count, 1)
	testing.expect_value(t, server.source_count, 0)
}

@(test)
remote_dependency_cache_miss_is_seen_for_analysis_run :: proc(t: ^testing.T) {
	path := remote_dependency_test_store_path("cache_miss_seen_for_run.sqlite3")
	store, store_err := dep_store.dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, store_err, dep_store.Store_Error.None)
	profile := standalone_dependency_profile()
	config := Config{cache = &store, profile = &profile}
	state := state_make(context.allocator)
	request := Request{name = "zcl_cache_miss_seen", kind = .Class}

	first := cache_artifacts({request}, &config, &state, nil, context.allocator)
	testing.expect_value(t, len(first), 0)
	testing.expect(t, remote_dependency_key(request) in state.seen_cache_requests)

	artifact := dep_store.Stored_Artifact_Input {
		package_name   = "zpkg",
		object_kind    = "global-class",
		object_name    = "zcl_cache_miss_seen",
		object_uri     = "/sap/bc/adt/oo/classes/zcl_cache_miss_seen",
		object_type    = "CLAS/OC",
		description    = "Cache miss seen",
		file_extension = "abap",
		source_text    = "CLASS zcl_cache_miss_seen DEFINITION. ENDCLASS.",
		fetched_at     = "2026-06-07T00:00:00Z",
	}
	_, put_err := dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, put_err, dep_store.Store_Error.None)

	second := cache_artifacts({request}, &config, &state, nil, context.allocator)
	testing.expect_value(t, len(second), 0)

	fresh_state := state_make(context.allocator)
	third := cache_artifacts({request}, &config, &fresh_state, nil, context.allocator)
	testing.expect_value(t, len(third), 1)
}

@(test)
remote_dependency_ddic_generated_abap_cache_entry_is_stale :: proc(t: ^testing.T) {
	record := dep_store.Stored_Artifact_Record {
		object_kind    = "ddic-table",
		object_name    = "ztab",
		file_extension = "abap",
		source_text     = "TYPES: BEGIN OF ztab, value TYPE i, END OF ztab.",
	}
	request := Request{name = "ztab", kind = .Type}

	testing.expect(t, cached_artifact_is_stale(&record, request))
}

@(test)
remote_dependency_ddic_xml_with_include_cache_entry_is_not_stale :: proc(t: ^testing.T) {
	record := dep_store.Stored_Artifact_Record {
		object_kind    = "ddic-table",
		object_name    = "/sttp/rep_evt",
		file_extension = "xml",
		source_text    = `<abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="/sttp/rep_evt" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name=".include">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicIncludeName">/sttp/s_rep_evt_att</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`,
	}
	request := Request{name = "/sttp/rep_evt", kind = .Type}

	testing.expect(t, !cached_artifact_is_stale(&record, request))
}

@(test)
remote_dependency_cached_ddic_domain_preserves_char_length_source :: proc(t: ^testing.T) {
	artifact := Artifact {
		request        = Request{name = "trkorr", kind = .Type},
		source_kind    = .Cache,
		object_kind    = "ddic-domain",
		object_name    = "trkorr",
		object_uri     = "/sap/bc/adt/ddic/domains/trkorr",
		object_type    = "DOMA/D",
		file_extension = "xml",
		source_text    = `<blue:wbobj adtcore:name="trkorr" adtcore:type="DOMA/D" xmlns:blue="http://www.sap.com/wbobj/dictionary/doma" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:doma="http://www.sap.com/adt/dictionary/domains">
  <doma:domain>
    <doma:dataType>CHAR</doma:dataType>
    <doma:dataTypeLength>20</doma:dataTypeLength>
    <doma:dataTypeDecimals>0</doma:dataTypeDecimals>
  </doma:domain>
</blue:wbobj>`,
	}

	source := open_source_from_artifact(&artifact, context.allocator)
	lower := strings.to_lower(source.source_text, context.allocator)
	defer delete(lower, context.allocator)

	testing.expect(t, strings.contains(lower, "types trkorr type c length 20"))
	testing.expect(t, !source.has_parse_errors)
}

@(test)
remote_dependency_ddic_source_cache_entry_is_not_stale_and_converts :: proc(t: ^testing.T) {
	source := `define type zrow {
  value : abap.int4;
}`
	record := dep_store.Stored_Artifact_Record {
		object_kind    = "ddic-structure",
		object_name    = "zrow",
		file_extension = "ddic",
		source_text     = source,
	}
	request := Request{name = "zrow", kind = .Type}

	testing.expect(t, !cached_artifact_is_stale(&record, request))

	artifact := Artifact {
		request        = request,
		source_kind    = .Cache,
		object_kind    = record.object_kind,
		object_name    = record.object_name,
		object_uri     = "/sap/bc/adt/ddic/structures/zrow",
		object_type    = "TABL/DS",
		file_extension = record.file_extension,
		source_text    = record.source_text,
	}
	result := result_make(context.allocator)
	state := state_make(context.allocator)

	added := result_add_artifact(&result, &artifact, &state, context.allocator)

	testing.expect(t, added)
	testing.expect_value(t, len(result.interfaces), 1)
	testing.expect_value(t, len(result.misses), 0)
	testing.expect_value(t, len(result.diagnostics), 0)
	testing.expect_value(t, result.interfaces[0].key.name, "zrow")
}

@(test)
remote_dependency_ddic_source_key_fields_convert_without_diagnostics :: proc(t: ^testing.T) {
	source := `define type dd03p {
  key tabname    : tabname
    with foreign key [1..*,1] dd02l
      where tabname = dd03p.tabname;
  key fieldname  : fieldname;
}`
	artifact := Artifact {
		request        = Request{name = "dd03p", kind = .Type},
		source_kind    = .Cache,
		object_kind    = "ddic-structure",
		object_name    = "dd03p",
		object_uri     = "/sap/bc/adt/ddic/structures/dd03p",
		object_type    = "TABL/DS",
		file_extension = "ddic",
		source_text    = source,
	}
	result := result_make(context.allocator)
	state := state_make(context.allocator)

	added := result_add_artifact(&result, &artifact, &state, context.allocator)

	testing.expect(t, added)
	testing.expect_value(t, len(result.interfaces), 1)
	testing.expect_value(t, len(result.diagnostics), 0)
	testing.expect_value(t, result.interfaces[0].key.name, "dd03p")
}

@(test)
remote_dependency_cache_resolves_artifact_provided_key :: proc(t: ^testing.T) {
	path := remote_dependency_test_store_path("cache_resolves_artifact_provided_key.sqlite3")
	store, store_err := dep_store.dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, store_err, dep_store.Store_Error.None)
	profile := standalone_dependency_profile()
	artifact := dep_store.Stored_Artifact_Input {
		package_name   = "sdic",
		object_kind    = "ddic-structure",
		object_name    = "dd03p",
		object_uri     = "/sap/bc/adt/ddic/structures/dd03p",
		object_type    = "TABL/DS",
		description    = "Structure",
		file_extension = "ddic",
		source_text    = `define type dd03p {
  key tabname    : tabname
    with foreign key [1..*,1] dd02l
      where tabname = dd03p.tabname;
  key fieldname  : fieldname;
}`,
		fetched_at     = "2026-06-07T00:00:00Z",
	}
	_, put_err := dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, put_err, dep_store.Store_Error.None)

	config := Config{cache = &store, profile = &profile}
	state := state_make(context.allocator)
	requests := [?]Request {
		{name = "dd03p", kind = .Symbol},
		{name = "dd03p", kind = .Type},
	}

	result := resolve_requests(requests[:], &config, &state, nil, context.allocator)

	testing.expect_value(t, len(result.interfaces), 1)
	testing.expect_value(t, len(result.misses), 0)
	testing.expect_value(t, len(result.diagnostics), 0)
	testing.expect_value(t, result.interfaces[0].key, Remote_Dependency_Key{name = "dd03p", kind = .Type})
}

@(test)
remote_dependency_typepool_cache_keeps_original_request_key :: proc(t: ^testing.T) {
	path := remote_dependency_test_store_path("typepool_cache_keeps_original_request_key.sqlite3")
	store, store_err := dep_store.dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, store_err, dep_store.Store_Error.None)
	profile := standalone_dependency_profile()
	source := "TYPES ztp_cached_type TYPE i."
	artifact := dep_store.Stored_Artifact_Input {
		package_name     = "ztp_cached",
		object_kind      = TYPEPOOL_OBJECT_KIND,
		object_name      = "ztp_cached",
		object_uri       = typepool_object_uri("ztp_cached", context.allocator),
		object_type      = TYPEPOOL_OBJECT_TYPE,
		description      = "Type-pool",
		file_extension   = "abap",
		source_text      = source,
		fetched_at       = "2026-06-06T00:00:00Z",
		typepool_symbols = {"ztp_cached_type"},
	}
	_, put_err := dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, put_err, dep_store.Store_Error.None)

	config := Config{cache = &store, profile = &profile}
	state := state_make(context.allocator)
	request := Request{name = "ztp_cached_type", kind = .Type}
	artifacts := cache_artifacts({request}, &config, &state, nil, context.allocator)

	testing.expect_value(t, len(artifacts), 1)
	testing.expect_value(t, artifacts[0].request.name, "ztp_cached_type")
	testing.expect_value(t, artifacts[0].object_name, "ztp_cached")
}

@(test)
typepool_source_analysis_detects_pending_expansion_and_symbols :: proc(t: ^testing.T) {
	include_analysis := typepool_source_analysis("include ztp_include.", context.allocator)
	name_analysis := typepool_source_analysis("TYPES ztp_include_name TYPE i.", context.allocator)
	testing.expect(t, include_analysis.pending_expansion)
	testing.expect(t, !name_analysis.pending_expansion)

	analysis := typepool_source_analysis(
		"TYPES ztp_type TYPE i.\nCONSTANTS ztp_const TYPE i VALUE 1.\nTYPES ztp_type TYPE i.",
		context.allocator,
	)
	testing.expect_value(t, len(analysis.symbols), 2)
	testing.expect(t, "ztp_type" in analysis.symbol_set)
	testing.expect(t, "ztp_const" in analysis.symbol_set)
}

@(test)
remote_dependency_typepool_cache_uses_pool_for_multiple_records :: proc(t: ^testing.T) {
	path := remote_dependency_test_store_path("typepool_cache_pool.sqlite3")
	store, store_err := dep_store.dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, store_err, dep_store.Store_Error.None)
	profile := standalone_dependency_profile()
	artifacts := [?]dep_store.Stored_Artifact_Input {
		{
			package_name     = "ztp_parallel_a",
			object_kind      = TYPEPOOL_OBJECT_KIND,
			object_name      = "ztp_parallel_a",
			object_uri       = typepool_object_uri("ztp_parallel_a", context.allocator),
			object_type      = TYPEPOOL_OBJECT_TYPE,
			description      = "Type-pool A",
			file_extension   = "abap",
			source_text      = "TYPES ztp_parallel_one TYPE i.",
			fetched_at       = "2026-06-06T00:00:00Z",
			typepool_symbols = {"ztp_parallel_one"},
		},
		{
			package_name     = "ztp_parallel_b",
			object_kind      = TYPEPOOL_OBJECT_KIND,
			object_name      = "ztp_parallel_b",
			object_uri       = typepool_object_uri("ztp_parallel_b", context.allocator),
			object_type      = TYPEPOOL_OBJECT_TYPE,
			description      = "Type-pool B",
			file_extension   = "abap",
			source_text      = "CONSTANTS ztp_parallel_two TYPE i VALUE 1.",
			fetched_at       = "2026-06-06T00:00:00Z",
			typepool_symbols = {"ztp_parallel_two"},
		},
	}
	_, put_err := dep_store.put_artifacts(&store, &profile, artifacts[:], context.allocator)
	testing.expect_value(t, put_err, dep_store.Store_Error.None)

	pool: execution.Pool
	execution.pool_init(
		&pool,
		execution.Options{worker_count = 2, task_capacity = 8, edge_capacity = 8},
		context.allocator,
	)
	defer execution.pool_destroy(&pool)
	execution.pool_start(&pool)

	config := Config{cache = &store, profile = &profile}
	state := state_make(context.allocator)
	requests := [?]Request {
		{name = "ztp_parallel_one", kind = .Type},
		{name = "ztp_parallel_two", kind = .Type},
	}
	stats_before := execution.pool_stats(&pool)

	resolved := cache_artifacts(requests[:], &config, &state, &pool, context.allocator)

	stats_after := execution.pool_stats(&pool)
	testing.expect_value(t, len(resolved), 2)
	expected_tasks := min(max(pool.options.worker_count, 1), len(requests))
	if pool.options.worker_count > 0 {
		expected_tasks += len(artifacts)
	}
	testing.expect(t, stats_after.submitted >= stats_before.submitted + u64(expected_tasks))
}

@(test)
remote_dependency_store_paths_clear_stale_summary_payload :: proc(t: ^testing.T) {
	path := remote_dependency_test_store_path("store_paths_clear_stale_summary_payload.sqlite3")
	store, store_err := dep_store.dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, store_err, dep_store.Store_Error.None)
	profile := standalone_dependency_profile()
	old := dep_store.Stored_Artifact_Input {
		package_name     = "zcl_summary_clear",
		object_kind      = "global-class",
		object_name      = "zcl_summary_clear",
		object_uri       = "/sap/bc/adt/oo/classes/zcl_summary_clear",
		object_type      = "CLAS/OC",
		description      = "Stale summary",
		file_extension   = "abap",
		source_text      = "CLASS zcl_summary_clear DEFINITION. ENDCLASS.",
		fetched_at       = "2026-06-05T00:00:00Z",
		summary_payload  = "abapls-summary-v1\nexport\tclass\tzcl_summary_clear\t\n",
	}
	old_id, put_err := dep_store.put_artifact(&store, &profile, &old, context.allocator)
	testing.expect_value(t, put_err, dep_store.Store_Error.None)
	_, old_payload_ok, old_payload_err := dep_store.read_artifact_summary_payload(
		&store,
		old_id,
		context.allocator,
	)
	testing.expect_value(t, old_payload_err, dep_store.Store_Error.None)
	testing.expect(t, old_payload_ok)

	request := Request{name = "zcl_summary_clear", kind = .Class}
	source := "CLASS zcl_summary_clear DEFINITION. PUBLIC SECTION. METHODS run. ENDCLASS."
	store_local_export_dependency(
		&store,
		&profile,
		request,
		"/sap/bc/adt/oo/classes/zcl_summary_clear",
		source,
		"global-class",
		"CLAS/OC",
		"abap",
	)
	record, record_ok, record_err := dep_store.find_artifact_for_candidate(
		&store,
		&profile,
		request.name,
		.Static,
		context.allocator,
	)
	testing.expect_value(t, record_err, dep_store.Store_Error.None)
	testing.expect(t, record_ok)
	_, payload_ok, payload_err := dep_store.read_artifact_summary_payload(
		&store,
		record.artifact_id,
		context.allocator,
	)
	testing.expect_value(t, payload_err, dep_store.Store_Error.None)
	testing.expect(t, !payload_ok)
}

remote_dependency_test_root :: proc(name: string) -> string {
	package_dir := filepath.dir(#file)
	root, _ := filepath.join(
		{package_dir, "..", "..", "bin", "test-data", "remote_dependencies", name},
		context.allocator,
	)
	return root
}

remote_dependency_test_store_path :: proc(name: string) -> string {
	root := remote_dependency_test_root("stores")
	os.make_directory_all(root)
	path, _ := filepath.join({root, name}, context.allocator)
	os.remove(path)
	os.remove(strings.concatenate({path, "-wal"}, context.allocator))
	os.remove(strings.concatenate({path, "-shm"}, context.allocator))
	return path
}
