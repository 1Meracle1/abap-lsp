package abap_frontend_dependency_store

import "core:os"
import filepath "core:path/filepath"
import "core:strings"
import "core:testing"

workspace_store_path :: proc(name: string) -> string {
	package_dir := filepath.dir(#file)
	root, _ := filepath.join(
		{package_dir, "..", "..", "bin", "test-data", "dependency_store"},
		context.allocator,
	)
	os.make_directory_all(root)
	path, _ := filepath.join({root, name}, context.allocator)
	os.remove(path)
	os.remove(strings.concatenate({path, "-wal"}, context.allocator))
	os.remove(strings.concatenate({path, "-shm"}, context.allocator))
	return path
}

sample_profile :: proc() -> Dependency_Profile {
	packages := make([dynamic]Package_Version, 0, 1, context.allocator)
	append(&packages, Package_Version{package_name = "sabap", version = "v2"})
	return Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "v1",
		packages                = packages[:],
	}
}

sample_artifact :: proc() -> Stored_Artifact_Input {
	symbols := make([dynamic]Stored_Symbol_Input, 0, 1, context.allocator)
	append(
		&symbols,
		Stored_Symbol_Input {
			symbol_name = "CL_ABAP_TYPEDESCR",
			symbol_kind = "class",
			range_start = 6,
			range_end   = 23,
			priority    = 100,
		},
	)
	return Stored_Artifact_Input {
		package_name   = "SABAP",
		object_kind    = "global-class",
		object_name    = "CL_ABAP_TYPEDESCR",
		object_uri     = "/sap/bc/adt/oo/classes/CL_ABAP_TYPEDESCR",
		object_type    = "CLAS/OC",
		description    = "Global class",
		file_extension = "abap",
		source_text    = "CLASS cl_abap_typedescr DEFINITION. ENDCLASS.",
		fetched_at     = "2026-04-23T10:00:00Z",
		symbols        = symbols[:],
	}
}

@(test)
sqlite_file_uri_uses_read_only_uri_form_for_workspace_path :: proc(t: ^testing.T) {
	path := workspace_store_path("dependency cache.sqlite3")
	uri := sqlite_file_uri(path, "mode=ro&immutable=1", context.allocator)
	testing.expect(t, strings.has_prefix(uri, "file://"))
	testing.expect(t, strings.contains(uri, "bin/test-data/dependency_store"))
	testing.expect(t, strings.contains(uri, "dependency%20cache.sqlite3"))
	testing.expect(t, strings.has_suffix(uri, "?mode=ro&immutable=1"))
}

@(test)
override_store_path_is_absolute :: proc(t: ^testing.T) {
	path, ok := resolve_dependency_store_path(
		"bin/test-data/dependency_store/cache.sqlite3",
		context.allocator,
	)
	testing.expect(t, ok)
	testing.expect(t, filepath.is_abs(path))
	testing.expect(t, strings.contains(path, "test-data"))
}

@(test)
stores_and_looks_up_artifacts :: proc(t: ^testing.T) {
	path := workspace_store_path("stores_and_looks_up_artifacts.sqlite3")
	store, err := dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	profile := sample_profile()
	artifact := sample_artifact()

	artifact_id: i64
	artifact_id, err = put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	status: Candidate_Cache_Status
	status, err = find_cached_candidate(
		&store,
		&profile,
		"https://sap.example|100|demo",
		"cl_abap_typedescr",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect_value(t, status, Candidate_Cache_Status.Artifact)

	lookup: Symbol_Lookup_Result
	ok: bool
	lookup, ok, err = lookup_symbol(&store, &profile, "cl_abap_typedescr", .Type, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect(t, ok)
	testing.expect_value(t, lookup.artifact_id, artifact_id)
	testing.expect_value(t, lookup.range_start, 6)

	stored: Stored_Artifact_Record
	stored, ok, err = read_artifact_source(&store, artifact_id, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect(t, ok)
	testing.expect_value(t, stored.object_name, "cl_abap_typedescr")
}

@(test)
stores_artifact_with_empty_text_fields :: proc(t: ^testing.T) {
	path := workspace_store_path("stores_artifact_with_empty_text_fields.sqlite3")
	store, err := dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	profile := sample_profile()
	artifact := sample_artifact()
	artifact.package_name = ""
	artifact.description = ""

	_, err = put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
}

@(test)
candidate_lookup_returns_highest_priority_artifact_kind :: proc(t: ^testing.T) {
	path := workspace_store_path("candidate_lookup_kind_priority.sqlite3")
	store, err := dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	profile := sample_profile()
	data_element := sample_artifact()
	data_element.object_kind = "ddic-data-element"
	data_element.object_uri = "/sap/bc/adt/ddic/dataelements/cl_abap_typedescr"
	data_element.object_type = "DTEL/DE"
	data_element.source_text = "TYPES cl_abap_typedescr TYPE c LENGTH 10."
	data_element.symbols = nil
	artifact := sample_artifact()

	inputs := [?]Stored_Artifact_Input{data_element, artifact}
	ids: [dynamic]i64
	ids, err = put_artifacts(&store, &profile, inputs[:], context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect_value(t, len(ids), 2)

	record: Stored_Artifact_Record
	ok: bool
	record, ok, err = find_artifact_for_candidate(
		&store,
		&profile,
		"CL_ABAP_TYPEDESCR",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect(t, ok)
	testing.expect_value(t, record.object_kind, "global-class")
}

@(test)
candidate_lookup_any_profile_reads_central_cache_artifact :: proc(t: ^testing.T) {
	path := workspace_store_path("candidate_lookup_any_profile.sqlite3")
	store, err := dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	profile := sample_profile()
	artifact := sample_artifact()

	_, err = put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, Store_Error.None)

	record: Stored_Artifact_Record
	ok: bool
	record, ok, err = find_artifact_for_candidate_any_profile(
		&store,
		"CL_ABAP_TYPEDESCR",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect(t, ok)
	testing.expect_value(t, record.object_kind, "global-class")
}

@(test)
candidate_lookup_filters_type_cache_hits_by_object_type :: proc(t: ^testing.T) {
	path := workspace_store_path("candidate_lookup_filters_type_cache_hits_by_object_type.sqlite3")
	store, err := dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	profile := sample_profile()

	report := sample_artifact()
	report.object_kind = "report"
	report.object_name = "RSPARAM"
	report.object_uri = "/sap/bc/adt/programs/programs/RSPARAM"
	report.object_type = "PROG/P"
	report.source_text = "REPORT rsparam."
	report.symbols = nil

	_, err = put_artifact(&store, &profile, &report, context.allocator)
	testing.expect_value(t, err, Store_Error.None)

	status: Candidate_Cache_Status
	status, err = find_cached_candidate(
		&store,
		&profile,
		"https://sap.example|100|demo",
		"rsparam",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect_value(t, status, Candidate_Cache_Status.Missing)

	ok: bool
	_, ok, err = find_artifact_for_candidate_any_profile(
		&store,
		"rsparam",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect(t, !ok)

	structure := sample_artifact()
	structure.object_kind = "ddic-structure"
	structure.object_name = "RSPARAM"
	structure.object_uri = "/sap/bc/adt/ddic/structures/RSPARAM"
	structure.object_type = "TABL/DS"
	structure.source_text = "TYPES rsparam TYPE string."
	structure.symbols = nil

	_, err = put_artifact(&store, &profile, &structure, context.allocator)
	testing.expect_value(t, err, Store_Error.None)

	record: Stored_Artifact_Record
	record, ok, err = find_artifact_for_candidate_any_profile(
		&store,
		"rsparam",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect(t, ok)
	testing.expect_value(t, record.object_kind, "ddic-structure")
}

@(test)
lists_artifacts_by_kind_in_profile_scope :: proc(t: ^testing.T) {
	path := workspace_store_path("lists_artifacts_by_kind_in_profile_scope.sqlite3")
	store, err := dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	profile := sample_profile()
	data_element := sample_artifact()
	data_element.object_kind = "ddic-data-element"
	data_element.object_name = "ZDEMO"
	data_element.object_uri = "/sap/bc/adt/ddic/dataelements/ZDEMO"
	data_element.object_type = "DTEL/DE"
	data_element.source_text = "TYPES zdemo TYPE c LENGTH 10."
	data_element.symbols = nil

	inputs := [?]Stored_Artifact_Input{sample_artifact(), data_element}
	_, err = put_artifacts(
		&store,
		&profile,
		inputs[:],
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)

	records: [dynamic]Stored_Artifact_Record
	records, err = list_artifacts_by_kind(&store, &profile, "ddic-data-element", context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect_value(t, len(records), 1)
	testing.expect_value(t, records[0].object_name, "zdemo")
}

@(test)
type_candidates_include_ddic_domains :: proc(t: ^testing.T) {
	kinds := make([dynamic]string, 0, 8, context.allocator)
	candidate_artifact_kinds(.Type, &kinds)
	found := false
	for kind in kinds {
		found = found || kind == "ddic-domain"
	}
	testing.expect(t, found)
}

@(test)
records_negative_candidates_by_profile_scope :: proc(t: ^testing.T) {
	path := workspace_store_path("records_negative_candidates_by_profile_scope.sqlite3")
	store, err := dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	profile := sample_profile()

	err = record_negative_lookup(
		&store,
		&profile,
		"https://sap.example|100|demo",
		"boolean",
		.Type,
		"2026-04-23T10:00:00Z",
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)

	status: Candidate_Cache_Status
	status, err = find_cached_candidate(
		&store,
		&profile,
		"https://sap.example|100|demo",
		"boolean",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect_value(t, status, Candidate_Cache_Status.Negative)
}

@(test)
clears_profile_scope :: proc(t: ^testing.T) {
	path := workspace_store_path("clears_profile_scope.sqlite3")
	store, err := dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	profile := sample_profile()
	artifact := sample_artifact()
	_, err = put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	err = record_negative_lookup(
		&store,
		&profile,
		"https://sap.example|100|demo",
		"boolean",
		.Type,
		"2026-04-23T10:00:00Z",
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)

	err = clear_profile_scope(&store, &profile, context.allocator)
	testing.expect_value(t, err, Store_Error.None)
	status: Candidate_Cache_Status
	status, err = find_cached_candidate(
		&store,
		&profile,
		"https://sap.example|100|demo",
		"cl_abap_typedescr",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect_value(t, status, Candidate_Cache_Status.Missing)
	status, err = find_cached_candidate(
		&store,
		&profile,
		"https://sap.example|100|demo",
		"boolean",
		.Type,
		context.allocator,
	)
	testing.expect_value(t, err, Store_Error.None)
	testing.expect_value(t, status, Candidate_Cache_Status.Missing)
}
