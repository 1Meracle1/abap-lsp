package abap_frontend_remote_dependencies

import "src:ast"
import dep_store "src:dependency_store"
import execution "src:execution"

import "core:os"
import filepath "core:path/filepath"
import "core:strings"
import "core:testing"

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
				testing.expect(t, !strings.equal_fold(oop.members[0].name, "private_method"))
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
	}

	result := resolve_requests(requests[:], &config, &state, &pool, context.allocator)

	testing.expect_value(t, len(result.interfaces), 2)
	testing.expect_value(t, len(result.misses), 0)
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
	}
	stats_before := execution.pool_stats(&pool)

	result := resolve_requests(requests[:], &config, &state, &pool, context.allocator)

	stats_after := execution.pool_stats(&pool)
	testing.expect_value(t, len(result.interfaces), 2)
	testing.expect_value(t, len(result.misses), 0)
	testing.expect(t, stats_after.submitted > stats_before.submitted)
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
remote_dependency_store_paths_clear_legacy_summary_payload :: proc(t: ^testing.T) {
	path := remote_dependency_test_store_path("store_paths_clear_legacy_summary_payload.sqlite3")
	store, store_err := dep_store.dependency_store_from_override_path(path, context.allocator)
	testing.expect_value(t, store_err, dep_store.Store_Error.None)
	profile := standalone_dependency_profile()
	old := dep_store.Stored_Artifact_Input {
		package_name     = "zcl_summary_clear",
		object_kind      = "global-class",
		object_name      = "zcl_summary_clear",
		object_uri       = "/sap/bc/adt/oo/classes/zcl_summary_clear",
		object_type      = "CLAS/OC",
		description      = "Legacy summary",
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
