package abap_frontend_encoding_toml

import "core:strings"
import "core:testing"

@(test)
decodes_workspace_manifest_shapes :: proc(t: ^testing.T) {
	source := `
version = 1
connection = "default"

[dependency_store]
product_version = "SAP NETWEAVER"
default_package_version = "7.50"

[dependency_store.packages]
SABAP = "7.57"

[local_export]
roots = ["D:/dev/abap/sap_system_export"]

[lints]
profile = "recommended"
report_suppressed = false

[lints.rules]
"abap-lsp.dead-store" = "info"

[[unit]]
name = "ZREP"
members = ["main.abap", { role = "include", file = "top.abap" }]

[[unit]]
name = "ZCL_DEMO"
`
	result := parse_string(source, context.allocator)
	defer destroy_parse_result(result, context.allocator)

	testing.expect_value(t, len(result.errors), 0)
	{
		version, ok := table_get_int(result.root, "version")
		testing.expect(t, ok)
		testing.expect_value(t, version, i64(1))
	}
	{
		connection, ok := table_get_string(result.root, "connection")
		testing.expect(t, ok)
		testing.expect_value(t, connection, "default")
	}
	{
		dependency_store, ok := table_get_table(result.root, "dependency_store")
		testing.expect(t, ok)
		product_version, product_ok := table_get_string(dependency_store, "product_version")
		testing.expect(t, product_ok)
		testing.expect_value(t, product_version, "SAP NETWEAVER")
		packages, packages_ok := table_get_table(dependency_store, "packages")
		testing.expect(t, packages_ok)
		sabap_version, sabap_ok := table_get_string(packages, "SABAP")
		testing.expect(t, sabap_ok)
		testing.expect_value(t, sabap_version, "7.57")
	}
	{
		lints, ok := table_get_table(result.root, "lints")
		testing.expect(t, ok)
		report_suppressed, report_ok := table_get_bool(lints, "report_suppressed")
		testing.expect(t, report_ok)
		testing.expect_value(t, report_suppressed, false)
		rules, rules_ok := table_get_table(lints, "rules")
		testing.expect(t, rules_ok)
		dead_store, dead_store_ok := table_get_string(rules, "abap-lsp.dead-store")
		testing.expect(t, dead_store_ok)
		testing.expect_value(t, dead_store, "info")
	}
	{
		units, ok := table_get_array(result.root, "unit")
		testing.expect(t, ok)
		testing.expect_value(t, len(units), 2)
		first_unit, first_unit_ok := array_get_table(units, 0)
		testing.expect(t, first_unit_ok)
		first_name, first_name_ok := table_get_string(first_unit, "name")
		testing.expect(t, first_name_ok)
		testing.expect_value(t, first_name, "ZREP")
		members, members_ok := table_get_array(first_unit, "members")
		testing.expect(t, members_ok)
		first_member, first_member_ok := array_get_string(members, 0)
		testing.expect(t, first_member_ok)
		testing.expect_value(t, first_member, "main.abap")
		second_member, second_member_ok := array_get_table(members, 1)
		testing.expect(t, second_member_ok)
		second_member_file, second_member_file_ok := table_get_string(second_member, "file")
		testing.expect(t, second_member_file_ok)
		testing.expect_value(t, second_member_file, "top.abap")
	}
}

@(test)
decodes_sidecar_inline_tables_and_multiline_arrays :: proc(t: ^testing.T) {
	source := `
members = [
  "forms/ZREP_F01.abap",
]

includes = { "ZREP_TOP" = "forms/ZREP_TOP.abap" }

[dependencies]
source = "local-only"
`
	result := parse_string(source, context.allocator)
	defer destroy_parse_result(result, context.allocator)

	testing.expect_value(t, len(result.errors), 0)
	{
		members, ok := table_get_array(result.root, "members")
		testing.expect(t, ok)
		member, member_ok := array_get_string(members, 0)
		testing.expect(t, member_ok)
		testing.expect_value(t, member, "forms/ZREP_F01.abap")
	}
	{
		includes, ok := table_get_table(result.root, "includes")
		testing.expect(t, ok)
		include_file, include_ok := table_get_string(includes, "ZREP_TOP")
		testing.expect(t, include_ok)
		testing.expect_value(t, include_file, "forms/ZREP_TOP.abap")
	}
	{
		dependencies, ok := table_get_table(result.root, "dependencies")
		testing.expect(t, ok)
		source_mode, source_ok := table_get_string(dependencies, "source")
		testing.expect(t, source_ok)
		testing.expect_value(t, source_mode, "local-only")
	}
}

@(test)
encodes_basic_value_tree :: proc(t: ^testing.T) {
	result := parse_string("version = 1\nconnection = \"default\"\n", context.allocator)
	defer destroy_parse_result(result, context.allocator)

	text := encode_string(result.root, context.allocator)
	defer delete(text, context.allocator)

	testing.expect(t, strings.contains(text, "version = 1"))
	testing.expect(t, strings.contains(text, `connection = "default"`))
}
