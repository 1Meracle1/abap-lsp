package adt

import "core:mem"
import "core:net"
import "core:strings"
import "core:testing"
import "core:thread"
import "core:time"

@(test)
normalizes_base_url_to_adt_root :: proc(t: ^testing.T) {
	value := normalize_base_url("https://host.example.com/", context.allocator)
	defer delete(value, context.allocator)
	testing.expect_value(t, value, "https://host.example.com/sap/bc/adt")

	already := normalize_base_url("https://host.example.com/sap/bc/adt/", context.allocator)
	defer delete(already, context.allocator)
	testing.expect_value(t, already, "https://host.example.com/sap/bc/adt")
}

@(test)
encodes_adt_path_segment :: proc(t: ^testing.T) {
	namespaced := encode_path_segment("/STTP/DEMO", context.allocator)
	defer delete(namespaced, context.allocator)
	testing.expect_value(t, namespaced, "%2FSTTP%2FDEMO")

	plain := encode_path_segment("ZCL_DEMO", context.allocator)
	defer delete(plain, context.allocator)
	testing.expect_value(t, plain, "ZCL_DEMO")
}

@(test)
parses_search_object_references :: proc(t: ^testing.T) {
	xml := `<feed xmlns:adtcore="http://www.sap.com/adt/core">
  <adtcore:objectReference adtcore:uri="/sap/bc/adt/oo/classes/ZCL_DEMO" adtcore:type="CLAS/OC" adtcore:name="ZCL_DEMO" adtcore:packageName="ZPKG" adtcore:description="Demo &amp; Test"/>
</feed>`
	refs := parse_object_references(xml, context.allocator)
	defer object_refs_destroy(&refs, context.allocator)

	testing.expect_value(t, len(refs), 1)
	testing.expect_value(t, refs[0].name, "ZCL_DEMO")
	testing.expect_value(t, refs[0].object_type, "CLAS/OC")
	testing.expect_value(t, refs[0].description, "Demo & Test")
}

@(test)
parses_repository_node_structure :: proc(t: ^testing.T) {
	xml := `<asx:values>
<SEU_ADT_OBJECT_TYPE_INFO><OBJECT_TYPE>FUGR/FF</OBJECT_TYPE><CATEGORY_TAG>FUNC</CATEGORY_TAG><OBJECT_TYPE_LABEL>Function Modules</OBJECT_TYPE_LABEL><NODE_ID>000001</NODE_ID></SEU_ADT_OBJECT_TYPE_INFO>
<SEU_ADT_REPOSITORY_OBJ_NODE><OBJECT_TYPE>FUGR/FF</OBJECT_TYPE><OBJECT_NAME>ZFM</OBJECT_NAME><OBJECT_URI>/sap/bc/adt/functions/groups/ZFG/fmodules/ZFM</OBJECT_URI><OBJECT_VIT_URI>vit</OBJECT_VIT_URI><EXPANDABLE>X</EXPANDABLE></SEU_ADT_REPOSITORY_OBJ_NODE>
</asx:values>`
	structure := parse_repository_node_structure(xml, context.allocator)
	defer repository_node_structure_destroy(&structure, context.allocator)

	testing.expect_value(t, len(structure.object_types), 1)
	testing.expect_value(t, structure.tree_content[0].object_name, "ZFM")
	testing.expect(t, structure.tree_content[0].expandable)
}

@(test)
dotenv_and_connection_sources_accept_environment_aliases :: proc(t: ^testing.T) {
	dotenv, parse_err, ok := parse_dotenv_contents(
		`
export ABAP_ADT_URL = https://host.example.com/
SAPUSER= demo
SAPPASS='secret'
ABAP_ADT_CLIENT=100 # inline comment
ABAP_TYPEPOOL_RESOLVER_URL=/sap/bc/zabapls/typepool
`,
		context.allocator,
	)
	defer dotenv_defaults_destroy(&dotenv, context.allocator)
	testing.expect(t, ok)
	testing.expect_value(t, parse_err.line, 0)

	overrides := Connection_Overrides {
		username = "override_user",
	}
	config, err := connection_config_from_sources(&overrides, &dotenv, context.allocator)
	defer connection_config_destroy(&config, context.allocator)
	testing.expect_value(t, err, Error.None)
	testing.expect_value(t, config.base_url, "https://host.example.com/sap/bc/adt")
	testing.expect_value(t, config.username, "override_user")
	testing.expect_value(t, config.password, "secret")
	testing.expect_value(t, config.sap_client, "100")
	testing.expect_value(t, config.typepool_resolver_url, "/sap/bc/zabapls/typepool")
}

@(test)
typepool_resolver_url_uses_configured_endpoint_and_client :: proc(t: ^testing.T) {
	client: Client
	client_init(
		&client,
		Connection_Config {
			base_url = "http://host/sap/bc/adt",
			typepool_resolver_url = "/sap/bc/zabapls/typepool",
			sap_client = "100",
		},
		context.allocator,
	)
	defer client_destroy(&client, context.allocator)

	url := typepool_resolver_url(&client, "owner", "name", "TPAK PERMISSION/TO+USE", context.allocator)
	defer delete(url, context.allocator)
	testing.expect_value(
		t,
		url,
		"http://host/sap/bc/zabapls/typepool?op=owner&name=TPAK%20PERMISSION%2FTO%2BUSE&sap-client=100",
	)
}

Typepool_Resolver_Test_Server :: struct {
	listener:        net.TCP_Socket,
	session_response: string,
	owner_response:  string,
	source_response: string,
	request_buf:     [2048]u8,
	owner_count:     int,
	source_count:    int,
}

typepool_resolver_test_server_run :: proc(t: ^thread.Thread) {
	server := cast(^Typepool_Resolver_Test_Server)t.data
	for {
		client, _, err := net.accept_tcp(server.listener)
		if err != nil {
			return
		}
		n, recv_err := net.recv_tcp(client, server.request_buf[:])
		response := server.session_response
		if recv_err == nil {
			request := string(server.request_buf[:n])
			if strings.contains(request, "op=owner") {
				server.owner_count += 1
				response = server.owner_response
			} else if strings.contains(request, "op=source") {
				server.source_count += 1
				response = server.source_response
			}
		}
		net.send_tcp(client, transmute([]u8)response)
		net.close(client)
	}
}

test_http_response_status :: proc(
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

typepool_resolver_test_client :: proc(
	t: ^testing.T,
	server: ^Typepool_Resolver_Test_Server,
) -> (Client, ^thread.Thread) {
	listener, listen_err := net.listen_tcp(net.Endpoint{address = net.IP4_Loopback, port = 0})
	testing.expect(t, listen_err == nil)
	net.set_option(listener, .Receive_Timeout, 500 * time.Millisecond)
	ep, ep_err := net.bound_endpoint(listener)
	testing.expect(t, ep_err == nil)
	server.listener = listener
	worker := thread.create(typepool_resolver_test_server_run)
	worker.data = server
	thread.start(worker)

	base_url := strings.builder_make(context.allocator)
	strings.write_string(&base_url, "http://127.0.0.1:")
	strings.write_int(&base_url, ep.port)
	strings.write_string(&base_url, "/sap/bc/adt")
	resolver_url := strings.builder_make(context.allocator)
	strings.write_string(&resolver_url, "http://127.0.0.1:")
	strings.write_int(&resolver_url, ep.port)
	strings.write_string(&resolver_url, "/sap/bc/zabapls/typepool")
	client: Client
	client_init(
		&client,
		Connection_Config {
			base_url = strings.to_string(base_url),
			typepool_resolver_url = strings.to_string(resolver_url),
			username = "demo",
			password = "secret",
		},
		context.allocator,
	)
	client.http.timeout = 2 * time.Second
	return client, worker
}

typepool_resolver_test_server_stop :: proc(
	server: ^Typepool_Resolver_Test_Server,
	worker: ^thread.Thread,
) {
	net.close(server.listener)
	thread.join(worker)
	thread.destroy(worker)
}

typepool_resolver_test_client_destroy :: proc(client: ^Client, allocator: mem.Allocator) {
	delete(client.connection.base_url, allocator)
	delete(client.connection.typepool_resolver_url, allocator)
	client_destroy(client, allocator)
}

@(test)
bootstrap_session_returns_token_and_cookie :: proc(t: ^testing.T) {
	server := Typepool_Resolver_Test_Server {
		session_response = test_http_response_status(
			"200 OK",
			"ok",
			"x-csrf-token: token\r\nset-cookie: sap-contextid=abc; path=/\r\n",
			context.allocator,
		),
		owner_response = test_http_response_status("200 OK", "", "", context.allocator),
		source_response = test_http_response_status("200 OK", "", "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.owner_response, context.allocator)
	defer delete(server.source_response, context.allocator)
	client, worker := typepool_resolver_test_client(t, &server)
	defer typepool_resolver_test_client_destroy(&client, context.allocator)
	defer typepool_resolver_test_server_stop(&server, worker)

	bootstrap, err := bootstrap_session(&client, context.allocator)
	defer session_bootstrap_destroy(&bootstrap, context.allocator)

	testing.expect_value(t, err, Error.None)
	testing.expect_value(t, bootstrap.csrf_token, "token")
	testing.expect_value(t, bootstrap.cookie, "sap-contextid=abc")
	testing.expect_value(t, client.csrf_token, "")
}

@(test)
bootstrap_session_reports_bad_status :: proc(t: ^testing.T) {
	server := Typepool_Resolver_Test_Server {
		session_response = test_http_response_status("503 Service Unavailable", "down", "", context.allocator),
		owner_response   = test_http_response_status("200 OK", "", "", context.allocator),
		source_response  = test_http_response_status("200 OK", "", "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.owner_response, context.allocator)
	defer delete(server.source_response, context.allocator)
	client, worker := typepool_resolver_test_client(t, &server)
	defer typepool_resolver_test_client_destroy(&client, context.allocator)
	defer typepool_resolver_test_server_stop(&server, worker)

	_, err := bootstrap_session(&client, context.allocator)

	testing.expect_value(t, err, Error.Bad_Status)
	testing.expect_value(t, client.csrf_token, "")
}

@(test)
typepool_resolver_http_fetches_owner_and_source :: proc(t: ^testing.T) {
	server := Typepool_Resolver_Test_Server {
		session_response = test_http_response_status("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		owner_response   = test_http_response_status("200 OK", "TPAK", "", context.allocator),
		source_response  = test_http_response_status("200 OK", "TYPE-POOL tpak.", "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.owner_response, context.allocator)
	defer delete(server.source_response, context.allocator)
	client, worker := typepool_resolver_test_client(t, &server)
	defer typepool_resolver_test_client_destroy(&client, context.allocator)
	defer typepool_resolver_test_server_stop(&server, worker)

	owner, owner_err := resolve_typepool_owner(&client, "tpak_permission_to_use_list", context.allocator)
	source, source_err := fetch_typepool_source(&client, "TPAK", context.allocator)

	testing.expect_value(t, owner_err, Error.None)
	testing.expect_value(t, owner, "TPAK")
	testing.expect_value(t, source_err, Error.None)
	testing.expect_value(t, source, "TYPE-POOL tpak.")
	testing.expect_value(t, server.owner_count, 1)
	testing.expect_value(t, server.source_count, 1)
}

@(test)
typepool_resolver_http_misses_return_bad_status :: proc(t: ^testing.T) {
	server := Typepool_Resolver_Test_Server {
		session_response = test_http_response_status("200 OK", "ok", "x-csrf-token: token\r\n", context.allocator),
		owner_response   = test_http_response_status("404 Not Found", "", "", context.allocator),
		source_response  = test_http_response_status("500 Internal Server Error", "", "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.owner_response, context.allocator)
	defer delete(server.source_response, context.allocator)
	client, worker := typepool_resolver_test_client(t, &server)
	defer typepool_resolver_test_client_destroy(&client, context.allocator)
	defer typepool_resolver_test_server_stop(&server, worker)

	_, owner_err := resolve_typepool_owner(&client, "unknown_typepool_symbol", context.allocator)
	_, source_err := fetch_typepool_source(&client, "UNKNOWN", context.allocator)

	testing.expect_value(t, owner_err, Error.Bad_Status)
	testing.expect_value(t, source_err, Error.Bad_Status)
}

@(test)
dependency_object_selection_prefers_exact_supported_refs :: proc(t: ^testing.T) {
	objects := [?]Object_Ref {
		build_message_class_object_ref("zmsg", context.allocator),
		build_report_object_ref("zmsg", "ZPKG", context.allocator),
		build_include_object_ref("zinc", "ZPKG", context.allocator),
	}
	defer for &entry in objects {
		object_ref_destroy(&entry, context.allocator)
	}

	selected := select_dependency_objects("zmsg", objects[:], "report", context.allocator)
	defer object_refs_destroy(&selected, context.allocator)
	testing.expect_value(t, len(selected), 1)
	testing.expect_value(t, selected[0].object_type, "PROG/P")
}

@(test)
type_dependency_selection_ignores_same_named_function_group :: proc(t: ^testing.T) {
	objects := [?]Object_Ref {
		{
			uri = strings.clone("/sap/bc/adt/functions/groups/TR_OBJECTS", context.allocator),
			object_type = strings.clone("FUGR/F", context.allocator),
			name = strings.clone("TR_OBJECTS", context.allocator),
			package_name = strings.clone("SCTS_OBJ", context.allocator),
			description = strings.clone("Function Group", context.allocator),
		},
		{
			uri = strings.clone(
				"/sap/bc/adt/vit/wb/object_type/ttypda/object_name/TR_OBJECTS",
				context.allocator,
			),
			object_type = strings.clone("TTYP/DA", context.allocator),
			name = strings.clone("TR_OBJECTS", context.allocator),
			package_name = strings.clone("SCTS_PRJ", context.allocator),
			description = strings.clone("Table Type", context.allocator),
		},
	}
	defer for &entry in objects {
		object_ref_destroy(&entry, context.allocator)
	}

	selected := select_dependency_objects("tr_objects", objects[:], "type", context.allocator)
	defer object_refs_destroy(&selected, context.allocator)
	testing.expect_value(t, len(selected), 1)
	testing.expect_value(t, selected[0].object_type, "TTYP/DA")
}

@(test)
direct_dependency_refs_use_global_name_shape :: proc(t: ^testing.T) {
	class_refs := direct_dependency_object_refs("zcl_demo", "type", context.allocator)
	defer object_refs_destroy(&class_refs, context.allocator)
	testing.expect_value(t, len(class_refs), 1)
	testing.expect_value(t, class_refs[0].object_type, "CLAS/OC")

	static_refs := direct_dependency_object_refs("demo", "static", context.allocator)
	defer object_refs_destroy(&static_refs, context.allocator)
	testing.expect_value(t, len(static_refs), 2)

	object_refs := direct_dependency_object_refs("demo", "object-type", context.allocator)
	defer object_refs_destroy(&object_refs, context.allocator)
	testing.expect_value(t, len(object_refs), 2)

	interface_refs := direct_dependency_object_refs("demo", "interface-type", context.allocator)
	defer object_refs_destroy(&interface_refs, context.allocator)
	testing.expect_value(t, len(interface_refs), 1)
	testing.expect_value(t, interface_refs[0].object_type, "INTF/OI")

	ddic_refs := direct_dependency_object_refs("demo", "ddic-type", context.allocator)
	defer object_refs_destroy(&ddic_refs, context.allocator)
	testing.expect_value(t, len(ddic_refs), 2)
	testing.expect_value(t, ddic_refs[0].object_type, "DTEL/DE")
	testing.expect_value(t, ddic_refs[1].object_type, "DDIC/EI")
}

@(test)
ddic_elementinfo_type_is_read_from_xml :: proc(t: ^testing.T) {
	object_type := ddic_object_type_from_xml(
		`<?xml version="1.0"?><elementInfo type="TTYP/DA"></elementInfo>`,
	)
	testing.expect_value(t, object_type, "TTYP/DA")
	testing.expect_value(
		t,
		infer_ddic_manifest_kind_from_object_type(object_type),
		"ddic-table-type",
	)
}

Function_Module_Fetch_Test_Server :: struct {
	listener:         net.TCP_Socket,
	session_response: string,
	module_response:  string,
	group_response:   string,
	request_buf:      [2048]u8,
	request_count:    int,
	group_requested:  bool,
}

function_module_fetch_test_server_run :: proc(t: ^thread.Thread) {
	server := cast(^Function_Module_Fetch_Test_Server)t.data
	for {
		client, _, err := net.accept_tcp(server.listener)
		if err != nil {
			return
		}
		n, recv_err := net.recv_tcp(client, server.request_buf[:])
		response := server.session_response
		if recv_err == nil {
			server.request_count += 1
			request := string(server.request_buf[:n])
			if ascii_contains_ignore_case(request, "/functions/groups/zfg/source/main") {
				server.group_requested = true
				response = server.group_response
			} else if !ascii_contains_ignore_case(request, "/runtime/systemmessages") {
				response = server.module_response
			}
		}
		net.send_tcp(client, transmute([]u8)response)
		net.close(client)
	}
}

test_http_response :: proc(body, extra_headers: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "HTTP/1.1 200 OK\r\nContent-Length: ")
	strings.write_int(&out, len(body))
	strings.write_string(&out, "\r\nConnection: close\r\n")
	strings.write_string(&out, extra_headers)
	strings.write_string(&out, "\r\n")
	strings.write_string(&out, body)
	return strings.to_string(out)
}

@(test)
function_module_dependency_fetch_uses_only_module_source :: proc(t: ^testing.T) {
	listener, listen_err := net.listen_tcp(net.Endpoint{address = net.IP4_Loopback, port = 0})
	testing.expect(t, listen_err == nil)
	net.set_option(listener, .Receive_Timeout, 500 * time.Millisecond)
	ep, ep_err := net.bound_endpoint(listener)
	testing.expect(t, ep_err == nil)

	module_source := "FUNCTION zfm.\n  DATA lv_body TYPE zbody_type.\nENDFUNCTION."
	group_source := "FUNCTION-POOL zfg.\nINCLUDE lzfgtop.\n"
	server := Function_Module_Fetch_Test_Server {
		listener         = listener,
		session_response = test_http_response("ok", "x-csrf-token: token\r\n", context.allocator),
		module_response  = test_http_response(module_source, "", context.allocator),
		group_response   = test_http_response(group_source, "", context.allocator),
	}
	defer delete(server.session_response, context.allocator)
	defer delete(server.module_response, context.allocator)
	defer delete(server.group_response, context.allocator)
	worker := thread.create(function_module_fetch_test_server_run)
	worker.data = &server
	thread.start(worker)

	base_url := strings.builder_make(context.allocator)
	strings.write_string(&base_url, "http://127.0.0.1:")
	strings.write_int(&base_url, ep.port)
	strings.write_string(&base_url, "/sap/bc/adt")
	base := strings.to_string(base_url)
	defer delete(base, context.allocator)
	client: Client
	client_init(
		&client,
		Connection_Config{base_url = base, username = "demo", password = "secret"},
		context.allocator,
	)
	client.http.timeout = 2 * time.Second
	defer client_destroy(&client, context.allocator)
	object_ref := Object_Ref {
		uri          = "/sap/bc/adt/functions/groups/ZFG/fmodules/ZFM",
		object_type  = "FUGR/FF",
		name         = "ZFM",
		package_name = "ZPKG",
		description  = "Function module",
	}

	result, err := fetch_dependency_object(&client, &object_ref, context.allocator)
	net.close(listener)
	thread.join(worker)
	thread.destroy(worker)
	defer dependency_fetch_result_destroy(&result, context.allocator)

	testing.expect_value(t, err, Error.None)
	testing.expect_value(t, result.manifest_kind, "function-module")
	testing.expect_value(t, result.file_extension, "abap")
	testing.expect_value(t, result.body, module_source)
	testing.expect_value(t, len(result.shared_dependencies), 0)
	testing.expect_value(t, server.request_count, 2)
	testing.expect(t, !server.group_requested)
	testing.expect(t, !strings.contains(result.body, "FUNCTION-POOL"))
	testing.expect(t, !strings.contains(result.body, "lzfgtop"))
}

@(test)
formats_ddic_xml_lines :: proc(t: ^testing.T) {
	rendered := format_ddic_xml("<A><B>x</B></A>", context.allocator)
	defer delete(rendered, context.allocator)
	testing.expect_value(t, rendered, "<A>\n  <B>\n    x\n  </B>\n</A>\n")
}

@(test)
absolute_url_strips_duplicate_adt_root_and_adds_client :: proc(t: ^testing.T) {
	config := Connection_Config {
		base_url   = "http://host/sap/bc/adt",
		sap_client = "100",
	}
	url := absolute_url(&config, "/sap/bc/adt/programs/includes/ZINC", context.allocator)
	defer delete(url, context.allocator)
	testing.expect_value(t, url, "http://host/sap/bc/adt/programs/includes/ZINC?sap-client=100")
}

@(test)
adt_request_accepts_https_scheme_and_reports_network_failures :: proc(t: ^testing.T) {
	client: Client
	client_init(
		&client,
		Connection_Config {
			base_url = "https://127.0.0.1:1/sap/bc/adt",
			username = "demo",
			password = "secret",
		},
		context.allocator,
	)
	client.http.timeout = 2 * time.Second
	_, err := search_repository_objects(&client, "demo", 1, context.allocator)
	testing.expect_value(t, err, Error.Http_Network)
}
