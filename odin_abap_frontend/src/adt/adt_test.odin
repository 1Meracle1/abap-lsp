package adt

import "core:strings"
import "core:testing"
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
dotenv_and_connection_sources_match_rust_keys :: proc(t: ^testing.T) {
	dotenv, parse_err, ok := parse_dotenv_contents(`
export ABAP_ADT_URL = https://host.example.com/
SAPUSER= demo
SAPPASS='secret'
ABAP_ADT_CLIENT=100 # inline comment
`, context.allocator)
	defer dotenv_defaults_destroy(&dotenv, context.allocator)
	testing.expect(t, ok)
	testing.expect_value(t, parse_err.line, 0)

	overrides := Connection_Overrides{username = "override_user"}
	config, err := connection_config_from_sources(&overrides, &dotenv, context.allocator)
	defer connection_config_destroy(&config, context.allocator)
	testing.expect_value(t, err, Error.None)
	testing.expect_value(t, config.base_url, "https://host.example.com/sap/bc/adt")
	testing.expect_value(t, config.username, "override_user")
	testing.expect_value(t, config.password, "secret")
	testing.expect_value(t, config.sap_client, "100")
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
			uri = strings.clone("/sap/bc/adt/vit/wb/object_type/ttypda/object_name/TR_OBJECTS", context.allocator),
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
}

@(test)
extracts_active_includes_and_shapes_function_module_source :: proc(t: ^testing.T) {
	source := "FUNCTION-POOL zfg.\r\nINCLUDE lzfguxx.\n* INCLUDE skipped.\nINCLUDE lzfgtop. \" comment\n"
	names := extract_active_top_level_include_names(source, context.allocator)
	defer for name in names {
		delete(name, context.allocator)
	}
	defer delete(names)
	testing.expect_value(t, len(names), 2)
	testing.expect_value(t, names[0], "LZFGUXX")
	testing.expect_value(t, names[1], "LZFGTOP")

	combined := build_function_module_dependency_source(source, "FUNCTION zfm.\nENDFUNCTION.", context.allocator)
	defer delete(combined, context.allocator)
	testing.expect(t, strings.contains(combined, "Omitted in dependency cache"))
	testing.expect(t, strings.contains(combined, "FUNCTION zfm."))
}

@(test)
formats_ddic_xml_lines :: proc(t: ^testing.T) {
	rendered := format_ddic_xml("<A><B>x</B></A>", context.allocator)
	defer delete(rendered, context.allocator)
	testing.expect_value(t, rendered, "<A>\n  <B>\n    x\n  </B>\n</A>\n")
}

@(test)
absolute_url_strips_duplicate_adt_root_and_adds_client :: proc(t: ^testing.T) {
	config := Connection_Config{base_url = "http://host/sap/bc/adt", sap_client = "100"}
	url := absolute_url(&config, "/sap/bc/adt/programs/includes/ZINC", context.allocator)
	defer delete(url, context.allocator)
	testing.expect_value(t, url, "http://host/sap/bc/adt/programs/includes/ZINC?sap-client=100")
}

@(test)
adt_request_accepts_https_scheme_and_reports_network_failures :: proc(t: ^testing.T) {
	client: Client
	client_init(&client, Connection_Config{
		base_url = "https://127.0.0.1:1/sap/bc/adt",
		username = "demo",
		password = "secret",
	})
	client.http.timeout = 2 * time.Second
	_, err := search_repository_objects(&client, "demo", 1, context.allocator)
	testing.expect_value(t, err, Error.Http_Network)
}
