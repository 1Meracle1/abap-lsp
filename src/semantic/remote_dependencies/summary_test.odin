package abap_frontend_semantic_remote_dependencies

import dep_store "src:dependency_store"

import "core:os"
import filepath "core:path/filepath"
import "core:strings"
import "core:testing"

summary_test_store_path :: proc(name: string) -> string {
	package_dir := filepath.dir(#file)
	root, _ := filepath.join(
		{package_dir, "..", "..", "..", "bin", "test-data", "dependency_store"},
		context.allocator,
	)
	os.make_directory_all(root)
	path, _ := filepath.join({root, name}, context.allocator)
	os.remove(path)
	os.remove(strings.concatenate({path, "-wal"}, context.allocator))
	os.remove(strings.concatenate({path, "-shm"}, context.allocator))
	return path
}

summary_test_profile :: proc() -> dep_store.Dependency_Profile {
	return dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "v1",
	}
}

summary_export_exists :: proc(
	exports: []Dependency_Interface_Export_Summary,
	kind, name, owner: string,
) -> bool {
	for export in exports {
		if export.kind == kind &&
		   export.name == name &&
		   export.owner == owner {
			return true
		}
	}
	return false
}

summary_type_by_name :: proc(
	summary: ^Dependency_Interface_Summary,
	name: string,
) -> ^Dependency_Interface_Type_Summary {
	for &typ in summary.types {
		if typ.name == name {
			return &typ
		}
	}
	return nil
}

@(test)
stored_summary_payload_lists_exports_without_source :: proc(t: ^testing.T) {
	source := `CLASS zcl_dep DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS get_data IMPORTING iv_id TYPE string RETURNING VALUE(rv_text) TYPE string.
  PROTECTED SECTION.
    CLASS-DATA gv_state TYPE string.
  PRIVATE SECTION.
    METHODS hidden.
ENDCLASS.`
	payload := dependency_interface_summary_payload_from_artifact(
		"global-class",
		"ZCL_DEP",
		"/sap/bc/adt/oo/classes/ZCL_DEP",
		"CLAS/OC",
		"abap",
		source,
		context.allocator,
	)
	testing.expect(t, payload != "")

	path := summary_test_store_path("stored_summary_payload_lists_exports_without_source.sqlite3")
	store, err := dep_store.dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := summary_test_profile()
	artifact := dep_store.Stored_Artifact_Input {
		package_name    = "ZPKG",
		object_kind     = "global-class",
		object_name     = "ZCL_DEP",
		object_uri      = "/sap/bc/adt/oo/classes/ZCL_DEP",
		object_type     = "CLAS/OC",
		description     = "Class dependency",
		file_extension  = "abap",
		source_text     = source,
		fetched_at      = "2026-06-04T00:00:00Z",
		summary_payload = payload,
	}
	artifact_id: i64
	artifact_id, err = dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	loaded_payload: string
	ok: bool
	loaded_payload, ok, err = dep_store.read_artifact_summary_payload(
		&store,
		artifact_id,
		context.allocator,
	)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	testing.expect(t, ok)

	exports := dependency_interface_summary_exports_from_payload(loaded_payload, context.allocator)
	testing.expect(t, summary_export_exists(exports[:], "class", "zcl_dep", ""))
	testing.expect(t, summary_export_exists(exports[:], "method", "get_data", "zcl_dep"))
	testing.expect(t, summary_export_exists(exports[:], "attribute", "gv_state", "zcl_dep"))
	testing.expect(t, !summary_export_exists(exports[:], "method", "hidden", "zcl_dep"))
}

@(test)
summary_payload_round_trips_typepool_exports :: proc(t: ^testing.T) {
	source := `TYPE-POOL ztp.
TYPES ztp_text TYPE string.
CONSTANTS ztp_flag TYPE abap_bool VALUE abap_true.`
	payload := dependency_interface_summary_payload_from_artifact(
		TYPEPOOL_OBJECT_KIND,
		"ZTP",
		"type-pool:ZTP",
		TYPEPOOL_OBJECT_TYPE,
		"abap",
		source,
		context.allocator,
	)
	summary, ok := dependency_interface_summary_from_payload(payload, context.allocator)
	testing.expect(t, ok)
	testing.expect_value(t, summary.type_pool.name, "ztp")
	testing.expect_value(t, len(summary.type_pool.symbols), 2)
	exports := dependency_interface_summary_exports(&summary, context.allocator)
	testing.expect(t, summary_export_exists(exports[:], "type-pool", "ztp", ""))
	testing.expect(t, summary_export_exists(exports[:], "type", "ztp_text", ""))
	testing.expect(t, summary_export_exists(exports[:], "constant", "ztp_flag", ""))
}

@(test)
summary_payload_round_trips_function_signature_exports :: proc(t: ^testing.T) {
	source := `FUNCTION z_demo
  IMPORTING VALUE(iv_value) TYPE i OPTIONAL iv_text TYPE string DEFAULT 'x'
  EXPORTING ev_text LIKE sy-uname
  CHANGING REFERENCE(cv_any) TYPE REF TO object
  TABLES et_return STRUCTURE bapiret2
  EXCEPTIONS failed = 1 not_found.
ENDFUNCTION.`
	payload := dependency_interface_summary_payload_from_artifact(
		"function-module",
		"Z_DEMO",
		"/sap/bc/adt/functions/groups/ZFG/fmodules/Z_DEMO",
		"FUGR/FF",
		"abap",
		source,
		context.allocator,
	)
	summary, ok := dependency_interface_summary_from_payload(payload, context.allocator)
	testing.expect(t, ok)
	testing.expect_value(t, len(summary.functions), 1)
	testing.expect_value(t, summary.functions[0].name, "z_demo")
	testing.expect_value(t, len(summary.functions[0].parameters), 5)
	testing.expect_value(t, len(summary.functions[0].exceptions), 2)
	exports := dependency_interface_summary_exports(&summary, context.allocator)
	testing.expect(t, summary_export_exists(exports[:], "function-module", "z_demo", ""))
}

@(test)
summary_payload_round_trips_ddic_structure_shape :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="ztab" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="mandt">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">MANDT</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">clnt</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="counter">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">int4</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	payload := dependency_interface_summary_payload_from_artifact(
		"ddic-table",
		"ZTAB",
		"/sap/bc/adt/ddic/tables/ZTAB",
		"TABL/DT",
		"xml",
		xml,
		context.allocator,
	)
	summary, ok := dependency_interface_summary_from_payload(payload, context.allocator)
	testing.expect(t, ok)
	typ := summary_type_by_name(&summary, "ztab")
	testing.expect(t, typ != nil)
	if typ != nil {
		testing.expect_value(t, typ.shape_kind, "structure")
		testing.expect_value(t, len(typ.fields), 2)
		testing.expect_value(t, typ.fields[0].name, "mandt")
		testing.expect_value(t, typ.fields[1].type_ref.base_name, "i")
	}
	exports := dependency_interface_summary_exports(&summary, context.allocator)
	testing.expect(t, summary_export_exists(exports[:], "type", "ztab", ""))
}
