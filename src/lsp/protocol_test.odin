package abap_frontend_lsp

import "src:parser"
import "src:semantic"
import workspace "src:workspace"

import json "core:encoding/json"
import "core:mem"
import "core:os"
import "core:strings"
import "core:testing"

@(test)
file_uri_to_path_decodes_windows_paths :: proc(t: ^testing.T) {
	path, ok := file_uri_to_path("file:///D:/dev/rust/abap%20lsp/demo.abap", context.allocator)

	testing.expect(t, ok)
	testing.expect_value(t, path, `D:\dev\rust\abap lsp\demo.abap`)
}

@(test)
initialize_opens_all_workspace_folders :: proc(t: ^testing.T) {
	root_a := `tmp\lsp_multi_workspace_a`
	if absolute, abs_err := os.get_absolute_path(root_a, context.allocator); abs_err == nil {
		root_a = absolute
	}
	if cleaned, clean_err := os.clean_path(root_a, context.allocator); clean_err == nil {
		root_a = cleaned
	}
	root_b := `tmp\lsp_multi_workspace_b`
	if absolute, abs_err := os.get_absolute_path(root_b, context.allocator); abs_err == nil {
		root_b = absolute
	}
	if cleaned, clean_err := os.clean_path(root_b, context.allocator); clean_err == nil {
		root_b = cleaned
	}
	os.remove_all(root_a)
	os.remove_all(root_b)
	testing.expect(t, os.make_directory_all(root_a) == nil)
	testing.expect(t, os.make_directory_all(root_b) == nil)
	defer os.remove_all(root_a)
	defer os.remove_all(root_b)
	output_path := `tmp\lsp_initialize_response.out`
	os.remove(output_path)
	output, output_err := os.create(output_path)
	testing.expect(t, output_err == nil)
	if output_err != nil {
		return
	}
	defer os.close(output)
	defer os.remove(output_path)

	state := Server_State {
		allocator  = context.allocator,
		options    = workspace.Options{},
		workspaces = make([dynamic]Server_Workspace, 0, 2, context.allocator),
	}
	defer {
		for i in 0 ..< len(state.workspaces) {
			slot := &state.workspaces[i]
			if slot.has_analysis {
				workspace.analysis_result_destroy(&slot.analysis, state.allocator)
			}
			workspace.workspace_destroy(&slot.root, state.allocator)
		}
		delete(state.workspaces)
	}

	uri_a := normalize_lsp_uri(root_a, context.allocator)
	if len(uri_a) >= 2 && uri_a[1] == ':' {
		uri_a = strings.concatenate({"file:///", uri_a}, context.allocator)
	} else {
		uri_a = strings.concatenate({"file://", uri_a}, context.allocator)
	}
	uri_b := normalize_lsp_uri(root_b, context.allocator)
	if len(uri_b) >= 2 && uri_b[1] == ':' {
		uri_b = strings.concatenate({"file:///", uri_b}, context.allocator)
	} else {
		uri_b = strings.concatenate({"file://", uri_b}, context.allocator)
	}
	params := make(json.Object, 1, context.allocator)
	folders := make(json.Array, 0, 2, context.allocator)
	folder_a := make(json.Object, 1, context.allocator)
	folder_b := make(json.Object, 1, context.allocator)
	folder_a["uri"] = json.String(uri_a)
	folder_b["uri"] = json.String(uri_b)
	append(&folders, folder_a)
	append(&folders, folder_b)
	params["workspaceFolders"] = folders

	ctx := Request_Context {
		state  = &state,
		output = output,
		id     = json.Integer(1),
	}
	handle_initialize(&ctx, params)

	testing.expect_value(t, len(state.workspaces), 2)
}

@(test)
workspace_index_for_uri_prefers_most_specific_workspace :: proc(t: ^testing.T) {
	state := Server_State {
		workspaces = make([dynamic]Server_Workspace, 0, 2, context.allocator),
	}
	defer delete(state.workspaces)
	append(&state.workspaces, Server_Workspace{root = workspace.Workspace{root_path = `D:\repo`}})
	append(&state.workspaces, Server_Workspace{root = workspace.Workspace{root_path = `D:\repo\package`}})

	index, ok := workspace_index_for_uri(&state, "file:///D:/repo/package/src/zmain.abap")

	testing.expect(t, ok)
	testing.expect_value(t, index, 1)
}

@(test)
workspace_index_for_non_file_uri_uses_first_workspace :: proc(t: ^testing.T) {
	state := Server_State {
		workspaces = make([dynamic]Server_Workspace, 0, 1, context.allocator),
	}
	defer delete(state.workspaces)
	append(&state.workspaces, Server_Workspace{root = workspace.Workspace{root_path = `D:\repo`}})

	index, ok := workspace_index_for_uri(&state, "untitled:Untitled-1")

	testing.expect(t, ok)
	testing.expect_value(t, index, 0)
}

@(test)
lsp_reanalysis_uses_disk_workspace_files_for_include_resolution :: proc(t: ^testing.T) {
	root := lsp_test_temp_root(t, `tmp\lsp_include_seed`)
	defer os.remove_all(root)
	src := lsp_test_join_path(t, root, "src")
	testing.expect(t, os.make_directory_all(src) == nil)
	report_path := lsp_test_join_path(t, src, "zmain.abap")
	include_path := lsp_test_join_path(t, src, "zinc.abap")
	report_source := `REPORT zmain.
INCLUDE zinc.
WRITE gv_value.`
	include_source := "DATA gv_value TYPE i."
	testing.expect(t, os.write_entire_file(report_path, report_source) == nil)
	testing.expect(t, os.write_entire_file(include_path, include_source) == nil)

	uri, uri_ok := file_uri_from_path(report_path, context.allocator)
	testing.expect(t, uri_ok)
	if !uri_ok {
		return
	}
	opened, workspace_ok, _ := workspace.open(root, workspace.Options{}, context.allocator)
	testing.expect(t, workspace_ok)
	if !workspace_ok {
		return
	}
	state := lsp_test_empty_state()
	append(&state.workspaces, Server_Workspace{root = opened})
	defer lsp_test_state_destroy(&state)

	testing.expect(t, update_document_from_open(&state, lsp_test_did_open_params(uri, report_source)))
	server_reanalyze(&state)
	diagnostics := diagnostics_for_uri(&state, uri, context.allocator)

	for diagnostic in diagnostics {
		testing.expect(t, diagnostic.code != "Unresolved_Include")
	}
	testing.expect(t, state.workspaces[0].has_analysis)
	testing.expect(t, len(state.workspaces[0].analysis.session.editable_files) >= 2)
}

@(test)
lsp_uri_matches_or_under_accepts_file_uris_and_paths :: proc(t: ^testing.T) {
	testing.expect(t, lsp_uri_matches_or_under(`D:\repo\pkg\zmain.abap`, "file:///D:/repo/pkg"))
	testing.expect(t, lsp_uri_matches_or_under("file:///D:/repo/pkg/zmain.abap", `D:\repo\pkg`))
	testing.expect(t, !lsp_uri_matches_or_under(`D:\repo\pkg2\zmain.abap`, "file:///D:/repo/pkg"))
}

@(test)
initialize_result_exposes_rename_prepare_provider :: proc(t: ^testing.T) {
	result := initialize_result(context.allocator)

	testing.expect(t, result.capabilities.rename_provider.prepare_provider)
}

@(test)
initialize_result_exposes_implementation_provider :: proc(t: ^testing.T) {
	result := initialize_result(context.allocator)

	testing.expect(t, result.capabilities.implementation_provider)
}

@(test)
lsp_default_workspace_options_enable_dependency_resolution_diagnostics :: proc(t: ^testing.T) {
	options := server_default_workspace_options()

	testing.expect(t, .Enable_ADT in options.flags)
	testing.expect(t, .Enable_Dependency_Diagnostics in options.flags)
}

@(test)
initialize_honors_materialized_dependency_documents_option :: proc(t: ^testing.T) {
	output_path := `tmp\lsp_initialize_materialized_dependency_documents.out`
	os.remove(output_path)
	output, output_err := os.create(output_path)
	testing.expect(t, output_err == nil)
	if output_err != nil {
		return
	}
	defer os.close(output)
	defer os.remove(output_path)

	state := lsp_test_empty_state()
	defer lsp_test_state_destroy(&state)
	params := make(json.Object, 1, context.allocator)
	init_options := make(json.Object, 1, context.allocator)
	init_options["materializeDependencyDocuments"] = json.Boolean(true)
	params["initializationOptions"] = init_options

	ctx := Request_Context {
		state  = &state,
		output = output,
		id     = json.Integer(1),
	}
	handle_initialize(&ctx, params)

	testing.expect(t, state.materialize_dependency_documents)
}

@(test)
initialize_honors_completion_snippet_support_false :: proc(t: ^testing.T) {
	params := make(json.Object, 1, context.allocator)
	capabilities := make(json.Object, 1, context.allocator)
	text_document := make(json.Object, 1, context.allocator)
	completion := make(json.Object, 1, context.allocator)
	completion_item := make(json.Object, 1, context.allocator)
	completion_item["snippetSupport"] = json.Boolean(false)
	completion["completionItem"] = completion_item
	text_document["completion"] = completion
	capabilities["textDocument"] = text_document
	params["capabilities"] = capabilities

	snippets, ok := initialize_completion_snippet_support(params)

	testing.expect(t, ok)
	testing.expect(t, !snippets)
}

@(test)
lsp_reanalysis_preserves_workspace_analysis_session :: proc(t: ^testing.T) {
	state := Server_State {
		allocator         = context.allocator,
		documents         = make(map[string]Document, 1, context.allocator),
		parse_diagnostics = make([dynamic]Parse_Diagnostic_Bucket, 0, 2, context.allocator),
		workspaces        = make([dynamic]Server_Workspace, 0, 1, context.allocator),
	}
	defer {
		for &slot in state.workspaces {
			if slot.has_analysis {
				workspace.analysis_result_destroy(&slot.analysis, state.allocator)
			}
			workspace.workspace_destroy(&slot.root, state.allocator)
		}
		delete(state.workspaces)
		delete(state.parse_diagnostics)
		delete(state.documents)
	}
	append(&state.workspaces, Server_Workspace{root = workspace.Workspace{root_path = `D:\repo`}})

	uri := "file:///D:/repo/zmain.abap"
	state.documents[uri] = Document {
		uri = uri,
		text = "REPORT zmain. DATA lv_value TYPE i.",
		version = 1,
		dirty = true,
	}
	server_reanalyze(&state)
	testing.expect(t, state.workspaces[0].has_analysis)
	first_generation := state.workspaces[0].analysis.session.generation

	state.documents[uri] = Document {
		uri = uri,
		text = "REPORT zmain. DATA lv_value TYPE string.",
		version = 2,
		dirty = true,
	}
	server_reanalyze(&state)

	testing.expect(t, state.workspaces[0].has_analysis)
	testing.expect(t, state.workspaces[0].analysis.session.generation > first_generation)
}

@(test)
lsp_completion_selector_method_uses_full_call_snippet :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_method_snippet.abap"
	source := `CLASS lcl_repo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_instance RETURNING VALUE(ro_repo) TYPE REF TO lcl_repo.
    METHODS execute
      IMPORTING iv_input TYPE string
      EXPORTING ev_result TYPE string
      CHANGING cv_state TYPE string.
ENDCLASS.
CLASS lcl_repo IMPLEMENTATION.
  METHOD get_instance.
  ENDMETHOD.
  METHOD execute.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  lcl_repo=>get_instance( )->execute( ).`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, ")->execute") + len(")->")
	testing.expect(t, offset >= len(")->"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "execute")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(
		t,
		item.insert_text,
		`execute(
    EXPORTING
      iv_input = $1
    IMPORTING
      ev_result = $2
    CHANGING
      cv_state = $3
  )$0`,
	)
}

@(test)
lsp_completion_selector_method_omits_exporting_for_only_exporting_args :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_method_exporting_shorthand.abap"
	source := `CLASS lcl_repo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_instance RETURNING VALUE(ro_repo) TYPE REF TO lcl_repo.
    METHODS execute
      IMPORTING iv_input TYPE string
                iv_other TYPE i.
ENDCLASS.
CLASS lcl_repo IMPLEMENTATION.
  METHOD get_instance.
  ENDMETHOD.
  METHOD execute.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  lcl_repo=>get_instance( )->execute( ).`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, ")->execute") + len(")->")
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "execute")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(
		t,
		item.insert_text,
		`execute(
    iv_input = $1
    iv_other = $2
  )$0`,
	)
	testing.expect(t, !strings.contains(item.insert_text, "EXPORTING"))
}

@(test)
lsp_completion_selector_method_falls_back_to_plain_text_without_snippet_support :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_method_plain.abap"
	source := `CLASS lcl_repo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_instance RETURNING VALUE(ro_repo) TYPE REF TO lcl_repo.
    METHODS execute IMPORTING iv_input TYPE string.
ENDCLASS.
CLASS lcl_repo IMPLEMENTATION.
  METHOD get_instance.
  ENDMETHOD.
  METHOD execute.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  lcl_repo=>get_instance( )->execute( ).`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, ")->execute") + len(")->")
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, false, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "execute")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(t, item.insert_text, "execute")
}

@(test)
lsp_completion_if_template_expands_from_if_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_if_template.abap"
	source := "REPORT zmain.\nFORM run.\n  i"
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "IF ... ENDIF")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
	testing.expect_value(t, item.sort_text, "2:if ... endif")
	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(t, item.insert_text, "IF ${1:condition}.\n    $0\n  ENDIF.")
}

@(test)
lsp_completion_loop_templates_expand_from_loop_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_loop_template.abap"
	source := "REPORT zmain.\nFORM run.\n  lo"
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	assigning, assigning_ok := lsp_test_find_completion_item(items, "LOOP AT ... ASSIGNING")
	into, into_ok := lsp_test_find_completion_item(items, "LOOP AT ... INTO")
	testing.expect(t, assigning_ok)
	testing.expect(t, into_ok)
	if !assigning_ok || !into_ok {
		return
	}

	testing.expect_value(t, assigning.kind, COMPLETION_SNIPPET)
	testing.expect_value(t, assigning.sort_text, "2:loop at ... assigning")
	testing.expect_value(t, assigning.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(
		t,
		assigning.insert_text,
		"LOOP AT ${1:itab} ASSIGNING FIELD-SYMBOL(<${2:row}>).\n    $0\n  ENDLOOP.",
	)
	testing.expect_value(t, into.kind, COMPLETION_SNIPPET)
	testing.expect_value(t, into.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(
		t,
		into.insert_text,
		"LOOP AT ${1:itab} INTO DATA(${2:row}).\n    $0\n  ENDLOOP.",
	)
}

@(test)
lsp_completion_get_time_stamp_field_template_expands_from_get_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_get_time_stamp_template.abap"
	source := "REPORT zmain.\nFORM run.\n  ge"
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "GET TIME STAMP FIELD")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
	testing.expect_value(t, item.sort_text, "2:get time stamp field")
	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(t, item.insert_text, "GET TIME STAMP FIELD ${1:lv_timestamp}.$0")
}

@(test)
lsp_completion_get_time_stamp_field_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_get_time_stamp_template_plain.abap"
	source := "REPORT zmain.\ng"
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, false, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "GET TIME STAMP FIELD")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(t, item.insert_text, "GET TIME STAMP FIELD lv_timestamp.")
}

@(test)
lsp_completion_if_template_falls_back_to_plain_text_without_snippet_support :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_if_template_plain.abap"
	source := "REPORT zmain.\ni"
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, false, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "IF ... ENDIF")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(t, item.insert_text, "IF condition.\n  \nENDIF.")
}

@(test)
lsp_completion_get_time_stamp_field_template_sorts_after_matching_symbols :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_get_time_stamp_template_priority.abap"
	source := `DATA get_candidate TYPE i.
ge`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	symbol_index := lsp_test_completion_item_index(items, "get_candidate")
	template_index := lsp_test_completion_item_index(items, "GET TIME STAMP FIELD")
	testing.expect(t, symbol_index >= 0)
	testing.expect(t, template_index >= 0)
	if symbol_index < 0 || template_index < 0 {
		return
	}

	testing.expect(t, symbol_index < template_index)
	testing.expect_value(t, items[symbol_index].sort_text, "1:get_candidate")
	testing.expect_value(t, items[template_index].sort_text, "2:get time stamp field")
}

@(test)
lsp_completion_if_template_sorts_after_matching_symbols :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_if_template_priority.abap"
	source := `DATA if_candidate TYPE i.
i`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	symbol_index := lsp_test_completion_item_index(items, "if_candidate")
	template_index := lsp_test_completion_item_index(items, "IF ... ENDIF")
	testing.expect(t, symbol_index >= 0)
	testing.expect(t, template_index >= 0)
	if symbol_index < 0 || template_index < 0 {
		return
	}

	testing.expect(t, symbol_index < template_index)
	testing.expect_value(t, items[symbol_index].sort_text, "1:if_candidate")
	testing.expect_value(t, items[template_index].sort_text, "2:if ... endif")
}

@(test)
lsp_completion_get_time_stamp_field_template_does_not_match_expression_prefixes :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_get_time_stamp_template_expression.abap"
	source := `DATA get_value TYPE i.
WRITE ge`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	_, item_ok := lsp_test_find_completion_item(items, "GET TIME STAMP FIELD")

	testing.expect(t, !item_ok)
}

@(test)
lsp_completion_if_template_does_not_match_expression_prefixes :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_if_template_expression.abap"
	source := `DATA if_value TYPE i.
WRITE i`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	_, item_ok := lsp_test_find_completion_item(items, "IF ... ENDIF")

	testing.expect(t, !item_ok)
}

@(test)
lsp_completion_loop_templates_fall_back_to_plain_text_without_snippet_support :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_loop_template_plain.abap"
	source := "REPORT zmain.\nlo"
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, false, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "LOOP AT ... INTO")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(t, item.insert_text, "LOOP AT itab INTO DATA(row).\n  \nENDLOOP.")
}

@(test)
lsp_completion_loop_templates_sort_after_matching_symbols :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_loop_template_priority.abap"
	source := `DATA lo_candidate TYPE i.
lo`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	symbol_index := lsp_test_completion_item_index(items, "lo_candidate")
	template_index := lsp_test_completion_item_index(items, "LOOP AT ... ASSIGNING")
	testing.expect(t, symbol_index >= 0)
	testing.expect(t, template_index >= 0)
	if symbol_index < 0 || template_index < 0 {
		return
	}

	testing.expect(t, symbol_index < template_index)
	testing.expect_value(t, items[symbol_index].sort_text, "1:lo_candidate")
	testing.expect_value(t, items[template_index].sort_text, "2:loop at ... assigning")
}

@(test)
lsp_completion_loop_templates_do_not_match_other_prefixes :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_loop_template_unmatched.abap"
	source := `DATA lv_value TYPE i.
lv`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	_, assigning_ok := lsp_test_find_completion_item(items, "LOOP AT ... ASSIGNING")
	_, into_ok := lsp_test_find_completion_item(items, "LOOP AT ... INTO")

	testing.expect(t, !assigning_ok)
	testing.expect(t, !into_ok)
}

@(test)
lsp_completion_loop_templates_do_not_match_expression_prefixes :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_loop_template_expression.abap"
	source := `DATA lo_value TYPE i.
WRITE lo`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	_, assigning_ok := lsp_test_find_completion_item(items, "LOOP AT ... ASSIGNING")
	_, into_ok := lsp_test_find_completion_item(items, "LOOP AT ... INTO")

	testing.expect(t, !assigning_ok)
	testing.expect(t, !into_ok)
}

@(test)
watched_folder_delete_removes_analysis_inputs_under_folder :: proc(t: ^testing.T) {
	output_path := `tmp\lsp_watched_folder_delete.out`
	os.remove(output_path)
	output, output_err := os.create(output_path)
	testing.expect(t, output_err == nil)
	if output_err != nil {
		return
	}
	defer os.close(output)
	defer os.remove(output_path)

	state := Server_State {
		allocator            = context.allocator,
		documents            = make(map[string]Document, 1, context.allocator),
		parse_diagnostics    = make([dynamic]Parse_Diagnostic_Bucket, 0, 2, context.allocator),
		workspaces           = make([dynamic]Server_Workspace, 0, 1, context.allocator),
		pending_removed_uris = make([dynamic]string, 0, 1, context.allocator),
	}
	defer {
		for &slot in state.workspaces {
			if slot.has_analysis {
				workspace.analysis_result_destroy(&slot.analysis, state.allocator)
			}
			workspace.workspace_destroy(&slot.root, state.allocator)
		}
		delete(state.pending_removed_uris)
		delete(state.workspaces)
		delete(state.parse_diagnostics)
		delete(state.documents)
	}
	append(&state.workspaces, Server_Workspace{root = workspace.Workspace{root_path = `D:\repo`}})

	uri := "file:///D:/repo/pkg/zmain.abap"
	state.documents[uri] = Document {
		uri = uri,
		text = "REPORT zmain. DATA lv_value TYPE i.",
		version = 1,
		dirty = true,
	}
	server_reanalyze(&state)

	testing.expect(t, state.workspaces[0].has_analysis)
	testing.expect_value(t, len(state.workspaces[0].analysis.session.editable_files), 1)

	params := make(json.Object, 1, context.allocator)
	changes := make(json.Array, 0, 1, context.allocator)
	event := make(json.Object, 2, context.allocator)
	event["uri"] = json.String("file:///D:/repo/pkg")
	event["type"] = json.Integer(FILE_CHANGE_DELETED)
	append(&changes, event)
	params["changes"] = changes

	handle_notification(&state, output, METHOD_DID_CHANGE_WATCHED_FILES, params)

	_, still_open := state.documents[uri]
	testing.expect(t, !still_open)
	testing.expect_value(t, len(state.workspaces[0].analysis.session.editable_files), 0)
}

@(test)
file_operation_folder_rename_removes_old_analysis_inputs :: proc(t: ^testing.T) {
	output_path := `tmp\lsp_folder_rename.out`
	os.remove(output_path)
	output, output_err := os.create(output_path)
	testing.expect(t, output_err == nil)
	if output_err != nil {
		return
	}
	defer os.close(output)
	defer os.remove(output_path)

	state := Server_State {
		allocator            = context.allocator,
		documents            = make(map[string]Document, 1, context.allocator),
		parse_diagnostics    = make([dynamic]Parse_Diagnostic_Bucket, 0, 2, context.allocator),
		workspaces           = make([dynamic]Server_Workspace, 0, 1, context.allocator),
		pending_removed_uris = make([dynamic]string, 0, 1, context.allocator),
	}
	defer {
		for &slot in state.workspaces {
			if slot.has_analysis {
				workspace.analysis_result_destroy(&slot.analysis, state.allocator)
			}
			workspace.workspace_destroy(&slot.root, state.allocator)
		}
		delete(state.pending_removed_uris)
		delete(state.workspaces)
		delete(state.parse_diagnostics)
		delete(state.documents)
	}
	append(&state.workspaces, Server_Workspace{root = workspace.Workspace{root_path = `D:\repo`}})

	uri := "file:///D:/repo/old/zmain.abap"
	state.documents[uri] = Document {
		uri = uri,
		text = "REPORT zmain. DATA lv_value TYPE i.",
		version = 1,
		dirty = true,
	}
	server_reanalyze(&state)

	testing.expect(t, state.workspaces[0].has_analysis)
	testing.expect_value(t, len(state.workspaces[0].analysis.session.editable_files), 1)

	params := make(json.Object, 1, context.allocator)
	files := make(json.Array, 0, 1, context.allocator)
	event := make(json.Object, 2, context.allocator)
	event["oldUri"] = json.String("file:///D:/repo/old")
	event["newUri"] = json.String("file:///D:/repo/new")
	append(&files, event)
	params["files"] = files

	handle_notification(&state, output, METHOD_DID_RENAME_FILES, params)

	_, still_open := state.documents[uri]
	testing.expect(t, !still_open)
	testing.expect_value(t, len(state.workspaces[0].analysis.session.editable_files), 0)
}

@(test)
lsp_positions_use_utf16_columns :: proc(t: ^testing.T) {
	source := "WRITE 'a😀b'.\n"
	b_offset := strings.index(source, "b")

	position := offset_to_position(source, b_offset)

	testing.expect_value(t, position.line, 0)
	testing.expect_value(t, position.character, 10)
	testing.expect_value(t, position_to_offset(source, position), b_offset)
	testing.expect_value(t, position_to_offset(source, Position{line = 0, character = 11}), b_offset + 1)
}

@(test)
prepare_rename_returns_placeholder_and_range_for_variable_use :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/prepare_rename.abap"
	source := "DATA lv TYPE i.\nlv = 1."
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	params := lsp_test_rename_position_params(uri, Position{line = 1, character = 1}, "")
	response, ok := prepare_rename_for_params(&state, params, context.allocator)

	testing.expect(t, ok)
	testing.expect_value(t, response.placeholder, "lv")
	testing.expect_value(t, response.range.start.line, 1)
	testing.expect_value(t, response.range.start.character, 0)
	testing.expect_value(t, response.range.end.character, 2)
}

@(test)
rename_returns_workspace_edit_for_method_declaration_implementation_and_call :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/rename_method.abap"
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
    METHODS caller.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
  METHOD caller.
    run( ).
  ENDMETHOD.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "run( )") + 1
	testing.expect(t, offset > 0)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "execute")
	edit, ok, error := rename_for_params(&state, params, context.allocator)

	testing.expect_value(t, error, "")
	testing.expect(t, ok)
	edits, edits_ok := edit.changes[uri]
	testing.expect(t, edits_ok)
	if !edits_ok {
		return
	}
	testing.expect_value(t, len(edits), 3)
	if len(edits) == 3 {
		testing.expect_value(t, edits[0].range.start.line, 2)
		testing.expect_value(t, edits[1].range.start.line, 7)
		testing.expect_value(t, edits[2].range.start.line, 10)
	}
	for item in edits {
		testing.expect_value(t, item.new_text, "execute")
	}
}

@(test)
lsp_implementation_returns_method_body_for_definition_method_name :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/implementation_method.abap"
	source := `CLASS lcl_demo DEFINITION.
  PRIVATE SECTION.
    METHODS run
      IMPORTING iv_value TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "METHODS run") + len("METHODS ") + 1
	testing.expect(t, offset > len("METHODS "))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	location, ok := implementation_location_for_params(&state, params)

	testing.expect(t, ok)
	testing.expect_value(t, location.uri, uri)
	testing.expect_value(t, location.range.start.line, 7)
	testing.expect_value(t, location.range.start.character, 9)
	testing.expect_value(t, location.range.end.line, 7)
	testing.expect_value(t, location.range.end.character, 12)
	if ok {
		start := position_to_offset(source, location.range.start)
		end := position_to_offset(source, location.range.end)
		testing.expect_value(t, source[start:end], "run")
	}
}

@(test)
lsp_references_include_method_declaration_implementation_and_call :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/references_method.abap"
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
    METHODS caller.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
  METHOD caller.
    run( ).
  ENDMETHOD.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "METHODS run") + len("METHODS ")
	testing.expect(t, offset >= len("METHODS "))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	locations := reference_locations_for_params(&state, params, context.allocator)

	testing.expect_value(t, len(locations), 3)
	if len(locations) == 3 {
		testing.expect_value(t, locations[0].range.start.line, 2)
		testing.expect_value(t, locations[1].range.start.line, 7)
		testing.expect_value(t, locations[2].range.start.line, 10)
		for location in locations {
			testing.expect_value(t, location.uri, uri)
			start := position_to_offset(source, location.range.start)
			end := position_to_offset(source, location.range.end)
			testing.expect_value(t, source[start:end], "run")
		}
	}
}

@(test)
lsp_definition_location_uses_virtual_dependency_ast_source :: proc(t: ^testing.T) {
	uri := "abapls-cache:/global-interface/zif_filter.abap"
	source := `INTERFACE zif_filter.
  METHODS convert_select_option.
ENDINTERFACE.`
	parsed := parser.parse(source, uri, context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)

	project := semantic.project_make()
	defer semantic.project_destroy(&project)
	file := semantic.project_add_file(&project, uri, parsed.root)

	state := lsp_test_empty_state()
	defer lsp_test_state_destroy(&state)

	name := "convert_select_option"
	offset := strings.index(source, name)
	testing.expect(t, offset >= 0)
	if offset < 0 {
		return
	}
	location, ok := location_for_project_file_range(
		&state,
		Snapshot_Lookup{},
		file,
		semantic.Range{start = offset, end = offset + len(name)},
	)

	testing.expect(t, ok)
	testing.expect_value(t, location.uri, uri)
	testing.expect_value(t, location.range.start.line, 1)
	testing.expect_value(t, location.range.start.character, 10)
	testing.expect_value(t, location.range.end.character, 31)
}

@(test)
lsp_definition_location_materializes_virtual_dependency_when_requested :: proc(t: ^testing.T) {
	root := lsp_test_temp_root(t, `tmp\lsp_materialized_dependency_document`)
	defer os.remove_all(root)
	store_path := lsp_test_join_path(t, root, "cache.sqlite3")
	uri := "abapls-cache:/global-interface/zif_filter.abap"
	source := `INTERFACE zif_filter.
  METHODS convert_select_option.
ENDINTERFACE.`
	parsed := parser.parse(source, uri, context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)

	project := semantic.project_make()
	defer semantic.project_destroy(&project)
	file := semantic.project_add_file(&project, uri, parsed.root)

	state := lsp_test_empty_state()
	defer lsp_test_state_destroy(&state)
	state.materialize_dependency_documents = true
	state.options.dependency_store_path = store_path
	state.documents[uri] = Document{uri = uri, text = source, version = 1}

	name := "convert_select_option"
	offset := strings.index(source, name)
	testing.expect(t, offset >= 0)
	if offset < 0 {
		return
	}
	location, ok := location_for_project_file_range(
		&state,
		Snapshot_Lookup{},
		file,
		semantic.Range{start = offset, end = offset + len(name)},
	)

	testing.expect(t, ok)
	testing.expect(t, strings.has_prefix(location.uri, "file://"))
	testing.expect_value(t, location.range.start.line, 1)
	testing.expect_value(t, location.range.start.character, 10)
	testing.expect_value(t, location.range.end.character, 31)

	path, path_ok := file_uri_to_path(location.uri, context.allocator)
	testing.expect(t, path_ok)
	if !path_ok {
		return
	}
	testing.expect(t, strings.contains(path, "dependency-documents"))
	written, written_ok := workspace.read_text_file(path, context.allocator)
	testing.expect(t, written_ok)
	testing.expect_value(t, written, source)
}

@(test)
read_dependency_document_source_returns_open_virtual_document :: proc(t: ^testing.T) {
	uri := "abapls-cache:/global-interface/zif_filter.abap"
	source := "INTERFACE zif_filter.\nENDINTERFACE."

	state := lsp_test_empty_state()
	defer lsp_test_state_destroy(&state)
	state.documents[uri] = Document{uri = uri, text = source, version = 1}

	result, ok := read_dependency_document_source(&state, uri, context.allocator)

	testing.expect(t, ok)
	testing.expect_value(t, result, source)
}

@(test)
rename_updates_type_structure_begin_and_end_names :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/rename_type_group.abap"
	source := `TYPES: BEGIN OF ty_input_po,
         sort_idx  TYPE i,
         ebeln     TYPE string,
         vendor_po TYPE string,
       END OF ty_input_po.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	begin_offset := strings.index(source, "BEGIN OF ty_input_po") + len("BEGIN OF ")
	end_offset := strings.last_index(source, "ty_input_po")
	testing.expect(t, begin_offset >= len("BEGIN OF "))
	testing.expect(t, end_offset > begin_offset)

	begin_params := lsp_test_rename_position_params(
		uri,
		offset_to_position(source, begin_offset),
		"ty_purchase_order",
	)
	begin_edit, begin_ok, begin_error := rename_for_params(&state, begin_params, context.allocator)

	testing.expect_value(t, begin_error, "")
	testing.expect(t, begin_ok)
	lsp_test_expect_type_structure_rename_edits(
		t,
		source,
		begin_edit,
		uri,
		"ty_input_po",
		"ty_purchase_order",
	)

	end_params := lsp_test_rename_position_params(
		uri,
		offset_to_position(source, end_offset),
		"ty_po_input",
	)
	end_edit, end_ok, end_error := rename_for_params(&state, end_params, context.allocator)

	testing.expect_value(t, end_error, "")
	testing.expect(t, end_ok)
	lsp_test_expect_type_structure_rename_edits(
		t,
		source,
		end_edit,
		uri,
		"ty_input_po",
		"ty_po_input",
	)
}

lsp_test_expect_type_structure_rename_edits :: proc(
	t: ^testing.T,
	source: string,
	edit: Workspace_Edit,
	uri: string,
	old_name: string,
	new_name: string,
) {
	testing.expect_value(t, len(edit.changes), 1)
	edits, edits_ok := edit.changes[uri]
	testing.expect(t, edits_ok)
	if !edits_ok {
		return
	}
	testing.expect_value(t, len(edits), 2)
	if len(edits) == 2 {
		testing.expect_value(t, edits[0].range.start.line, 0)
		testing.expect_value(t, edits[0].range.start.character, 16)
		testing.expect_value(t, edits[0].range.end.line, 0)
		testing.expect_value(t, edits[0].range.end.character, 27)
		testing.expect_value(t, edits[1].range.start.line, 4)
		testing.expect_value(t, edits[1].range.start.character, 14)
		testing.expect_value(t, edits[1].range.end.line, 4)
		testing.expect_value(t, edits[1].range.end.character, 25)
	}
	for item in edits {
		testing.expect_value(t, item.new_text, new_name)
	}
	applied := lsp_test_apply_text_edits(t, source, edits, context.allocator)
	testing.expect(t, strings.contains(applied, strings.concatenate({"BEGIN OF ", new_name}, context.temp_allocator)))
	testing.expect(t, strings.contains(applied, strings.concatenate({"END OF ", new_name}, context.temp_allocator)))
	testing.expect(t, !strings.contains(applied, old_name))
}

@(test)
rename_rejects_field_symbol_name_without_angle_brackets :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/rename_field_symbol.abap"
	source := "FIELD-SYMBOLS <fs> TYPE any.\nASSIGN 1 TO <fs>."
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "ASSIGN 1 TO <fs>") + len("ASSIGN 1 TO ") + 1
	testing.expect(t, offset > 0)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "fs2")
	_, ok, error := rename_for_params(&state, params, context.allocator)

	testing.expect(t, !ok)
	testing.expect(t, strings.contains(error, "angle brackets"))
}

@(test)
semantic_tokens_include_multiline_class_header_names :: proc(t: ^testing.T) {
	source := `CLASS lcl_parent DEFINITION.
ENDCLASS.
CLASS lcl_child DEFINITION
  INHERITING FROM lcl_parent
  CREATE PUBLIC.
ENDCLASS.`

	parsed := parser.parse(source, "mem://semantic_tokens_class_header.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)

	project := semantic.project_make()
	defer semantic.project_destroy(&project)

	checker := semantic.checker_make(&project)
	file := semantic.checker_add_file(&checker, parsed.path, parsed.root)
	semantic.checker_check_file(&checker, file)

	snapshot := Snapshot_Lookup {
		project = &project,
		checker = &checker,
		file    = file,
		source  = source,
		ok      = true,
	}
	tokens := semantic_tokens_for_snapshot(snapshot, context.allocator)

	testing.expect(
		t,
		semantic_token_data_has_token(source, tokens, "lcl_child", "lcl_child", TOKEN_TYPE_INDICES.class),
	)
	testing.expect(
		t,
		semantic_token_data_has_token(
			source,
			tokens,
			"INHERITING FROM lcl_parent",
			"lcl_parent",
			TOKEN_TYPE_INDICES.class,
		),
	)
}

@(test)
semantic_tokens_include_multiline_table_key_type_names :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_order_map,
    odata_property TYPE string,
  END OF ty_order_map,
  tt_order_map TYPE HASHED TABLE OF ty_order_map
    WITH UNIQUE KEY odata_property.`

	parsed := parser.parse(source, "mem://semantic_tokens_table_key.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)

	project := semantic.project_make()
	defer semantic.project_destroy(&project)

	checker := semantic.checker_make(&project)
	file := semantic.checker_add_file(&checker, parsed.path, parsed.root)
	semantic.checker_check_file(&checker, file)

	snapshot := Snapshot_Lookup {
		project = &project,
		checker = &checker,
		file    = file,
		source  = source,
		ok      = true,
	}
	tokens := semantic_tokens_for_snapshot(snapshot, context.allocator)

	testing.expect(
		t,
		semantic_token_data_has_token(
			source,
			tokens,
			"HASHED TABLE OF ty_order_map",
			"ty_order_map",
			TOKEN_TYPE_INDICES.type_,
		),
	)
	testing.expect(
		t,
		semantic_token_data_has_token(
			source,
			tokens,
			"WITH UNIQUE KEY odata_property",
			"odata_property",
			TOKEN_TYPE_INDICES.property,
		),
	)
}

@(test)
lsp_hover_lookup_reports_table_key_field_declared_type :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/table_key_hover.abap"
	source := `TYPES:
  BEGIN OF ty_seen_po,
    ebeln TYPE ekpo-ebeln,
  END OF ty_seen_po.
DATA lt_seen_po TYPE HASHED TABLE OF ty_seen_po WITH UNIQUE KEY ebeln.`

	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "WITH UNIQUE KEY ebeln")
	testing.expect(t, offset >= 0)
	if offset < 0 {
		return
	}
	offset += len("WITH UNIQUE KEY ")
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	found := entity_at_position(&state, params)

	testing.expect(t, found.ok)
	if !found.ok {
		return
	}
	testing.expect_value(t, found.entity.kind, semantic.Entity_Kind.Field)
	testing.expect_value(t, entity_label(found.snapshot.project, found.entity), "`ebeln` field")
	testing.expect_value(t, entity_detail(found.snapshot.project, found.entity), "type: `ekpo-ebeln`")
	testing.expect_value(t, source[found.range.start:found.range.end], "ebeln")
}

@(test)
lsp_hover_lookup_uses_precise_structured_type_field_ranges :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/structured_type_field_hover.abap"
	source := `CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_customer_ref,
        kunnr TYPE kunnr,
        gln   TYPE char13,
      END OF ty_customer_ref,
      BEGIN OF ty_delivery_summary,
        destination_owner_customer TYPE ty_customer_ref,
      END OF ty_delivery_summary,
      BEGIN OF ty_delivery_detail,
        INCLUDE TYPE ty_delivery_summary.
    TYPES:
        fetched_at TYPE timestampl,
      END OF ty_delivery_detail.
ENDCLASS.`

	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	field_offset := strings.index(source, "kunnr TYPE")
	testing.expect(t, field_offset >= 0)
	if field_offset < 0 {
		return
	}
	field_params := lsp_test_rename_position_params(uri, offset_to_position(source, field_offset), "")
	field_found := entity_at_position(&state, field_params)
	testing.expect(t, field_found.ok)
	if field_found.ok {
		testing.expect_value(t, field_found.entity.kind, semantic.Entity_Kind.Field)
		testing.expect_value(t, source[field_found.range.start:field_found.range.end], "kunnr")
	}

	type_offset := strings.index(source, "char13")
	testing.expect(t, type_offset >= 0)
	if type_offset < 0 {
		return
	}
	type_params := lsp_test_rename_position_params(uri, offset_to_position(source, type_offset), "")
	type_found := entity_at_position(&state, type_params)
	testing.expect(t, !type_found.ok)
	if type_found.ok {
		testing.expect(t, type_found.range.end <= type_found.range.start)
	}
}

@(test)
lsp_hover_lookup_uses_precise_raise_exporting_value_range :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/raise_exporting_hover.abap"
	source := `DATA lo_msg TYPE REF TO object.
RAISE EXCEPTION TYPE cx_demo
  EXPORTING
    message_container = lo_msg.`

	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "message_container = lo_msg")
	testing.expect(t, offset >= 0)
	if offset < 0 {
		return
	}
	offset += len("message_container = ")
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	found := entity_at_position(&state, params)

	testing.expect(t, found.ok)
	if !found.ok {
		return
	}
	testing.expect_value(t, found.entity.kind, semantic.Entity_Kind.Variable)
	testing.expect_value(t, source[found.range.start:found.range.end], "lo_msg")
}

@(test)
lsp_hover_reports_builtin_procedure_documentation :: proc(t: ^testing.T) {
	source := `DATA lv_len TYPE i.
lv_len = strlen( 'abc' ).`

	text := lsp_test_hover_text(t, source, "strlen( 'abc' )", "strlen")

	testing.expect(t, strings.contains(text, "`strlen` builtin"))
	testing.expect(t, strings.contains(text, "Number of characters in a text value."))
}

@(test)
lsp_hover_reports_builtin_structure_field_documentation :: proc(t: ^testing.T) {
	source := `DATA lv_subrc TYPE i.
lv_subrc = sy-subrc.`

	text := lsp_test_hover_text(t, source, "sy-subrc", "subrc")

	testing.expect(t, strings.contains(text, "`subrc` field"))
	testing.expect(t, strings.contains(text, "Return code set by many ABAP statements"))
}

@(test)
lsp_hover_reports_unknown_open_sql_inline_table_type :: proc(t: ^testing.T) {
	source := `SELECT *
  FROM zmissing_jobs
  INTO TABLE @DATA(lt_jobs).`

	text := lsp_test_hover_text(t, source, "@DATA(lt_jobs)", "lt_jobs")

	testing.expect(t, strings.contains(text, "`lt_jobs` variable"))
	testing.expect(t, strings.contains(text, "type: `STANDARD TABLE OF unknown`"))
}

@(test)
lsp_hover_reports_leading_declaration_comment_documentation :: proc(t: ^testing.T) {
	source := `DATA lv_seed TYPE i.
" local accumulator
DATA lv_total TYPE i.
lv_total = 1.`

	text := lsp_test_hover_text(t, source, "lv_total = 1", "lv_total")

	testing.expect(t, strings.contains(text, "local accumulator"))
	testing.expect(t, !strings.contains(text, `" local accumulator`))
}

@(test)
lsp_hover_reports_same_line_declaration_comment_documentation :: proc(t: ^testing.T) {
	source := `DATA lv_total TYPE i. " inline accumulator
lv_total = 1.`

	text := lsp_test_hover_text(t, source, "lv_total = 1", "lv_total")

	testing.expect(t, strings.contains(text, "inline accumulator"))
	testing.expect(t, !strings.contains(text, `" inline accumulator`))
}

@(test)
lsp_hover_reports_method_signature_and_documentation :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    " Build instance from display name.
    CLASS-METHODS create
      IMPORTING !iv_name TYPE string
                !iv_id TYPE i
      RETURNING VALUE(ro_result) TYPE REF TO lcl_demo
      RAISING zcx_demo.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD create.
  ENDMETHOD.
ENDCLASS.`

	text := lsp_test_hover_text(t, source, "METHOD create", "create")

	testing.expect(t, strings.contains(text, "```abap"))
	testing.expect(
		t,
		strings.contains(
			text,
			`CLASS-METHODS create
  IMPORTING
    !iv_name TYPE string
    !iv_id TYPE i
  RETURNING
    VALUE(ro_result) TYPE REF TO lcl_demo
  RAISING
    zcx_demo.`,
		),
	)
	testing.expect(t, strings.contains(text, "Build instance from display name."))
	testing.expect(t, !strings.contains(text, `" Build instance`))
}

@(test)
lsp_hover_does_not_apply_leading_comment_to_chained_declarations :: proc(t: ^testing.T) {
	source := `" shared heading
DATA: lv_a TYPE i, lv_b TYPE i.
lv_a = lv_b.`

	first := lsp_test_hover_text(t, source, "lv_a = lv_b", "lv_a")
	second := lsp_test_hover_text(t, source, "lv_a = lv_b", "lv_b")

	testing.expect(t, !strings.contains(first, "shared heading"))
	testing.expect(t, !strings.contains(second, "shared heading"))
}

semantic_token_data_has_token :: proc(
	source: string,
	data: []u32,
	occurrence: string,
	target: string,
	token_type: u32,
) -> bool {
	base := strings.index(source, occurrence)
	inside := strings.index(occurrence, target)
	if base < 0 || inside < 0 {
		return false
	}
	position := offset_to_position(source, base + inside)
	line := 0
	character := 0
	for i := 0; i + 4 < len(data); i += 5 {
		line += int(data[i])
		if data[i] == 0 {
			character += int(data[i + 1])
		} else {
			character = int(data[i + 1])
		}
		if line == position.line &&
		   character == position.character &&
		   int(data[i + 2]) == len(target) &&
		   data[i + 3] == token_type {
			return true
		}
	}
	return false
}

lsp_test_hover_text :: proc(
	t: ^testing.T,
	source: string,
	occurrence: string,
	target: string,
) -> string {
	uri := "file:///D:/repo/hover_documentation.abap"
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	base := strings.index(source, occurrence)
	inside := strings.index(occurrence, target)
	testing.expect(t, base >= 0 && inside >= 0)
	if base < 0 || inside < 0 {
		return ""
	}
	params := lsp_test_rename_position_params(uri, offset_to_position(source, base + inside), "")
	found := entity_at_position(&state, params)
	testing.expect(t, found.ok)
	if !found.ok {
		return ""
	}
	return entity_hover_text(found.snapshot.project, found.entity)
}

lsp_test_find_completion_item :: proc(
	items: []Completion_Item,
	label: string,
) -> (Completion_Item, bool) {
	for item in items {
		if item.label == label {
			return item, true
		}
	}
	return {}, false
}

lsp_test_completion_item_index :: proc(items: []Completion_Item, label: string) -> int {
	for item, i in items {
		if item.label == label {
			return i
		}
	}
	return -1
}

lsp_test_empty_state :: proc() -> Server_State {
	return Server_State {
		allocator         = context.allocator,
		documents         = make(map[string]Document, 1, context.allocator),
		parse_diagnostics = make([dynamic]Parse_Diagnostic_Bucket, 0, 2, context.allocator),
		workspaces        = make([dynamic]Server_Workspace, 0, 1, context.allocator),
		pending_removed_uris = make([dynamic]string, 0, 2, context.allocator),
		pending_disk_refresh_uris = make([dynamic]string, 0, 2, context.allocator),
		completion_snippets_supported = true,
	}
}

lsp_test_state_with_open_document :: proc(uri, source: string) -> Server_State {
	state := lsp_test_empty_state()
	append(&state.workspaces, Server_Workspace{root = workspace.Workspace{root_path = `D:\repo`}})
	params := lsp_test_did_open_params(uri, source)
	testing_ok := update_document_from_open(&state, params)
	assert(testing_ok)
	server_reanalyze(&state)
	return state
}

lsp_test_state_destroy :: proc(state: ^Server_State) {
	for &slot in state.workspaces {
		if slot.has_analysis {
			workspace.analysis_result_destroy(&slot.analysis, state.allocator)
		}
		workspace.workspace_destroy(&slot.root, state.allocator)
	}
	delete(state.workspaces)
	if state.pending_disk_refresh_uris.allocator.procedure != nil {
		delete(state.pending_disk_refresh_uris)
	}
	if state.pending_removed_uris.allocator.procedure != nil {
		delete(state.pending_removed_uris)
	}
	delete(state.parse_diagnostics)
	delete(state.documents)
}

lsp_test_temp_root :: proc(t: ^testing.T, path: string) -> string {
	root := path
	if absolute, abs_err := os.get_absolute_path(root, context.allocator); abs_err == nil {
		root = absolute
	}
	if cleaned, clean_err := os.clean_path(root, context.allocator); clean_err == nil {
		root = cleaned
	}
	os.remove_all(root)
	testing.expect(t, os.make_directory_all(root) == nil)
	return root
}

lsp_test_join_path :: proc(t: ^testing.T, a, b: string) -> string {
	path, err := os.join_path({a, b}, context.allocator)
	testing.expect(t, err == nil)
	if err != nil {
		return ""
	}
	return path
}

lsp_test_rename_position_params :: proc(
	uri: string,
	position: Position,
	new_name: string,
) -> json.Object {
	params := make(json.Object, 3, context.allocator)
	text_document := make(json.Object, 1, context.allocator)
	text_document["uri"] = json.String(uri)
	params["textDocument"] = text_document

	position_object := make(json.Object, 2, context.allocator)
	position_object["line"] = json.Integer(position.line)
	position_object["character"] = json.Integer(position.character)
	params["position"] = position_object

	if new_name != "" {
		params["newName"] = json.String(new_name)
	}
	return params
}

lsp_test_apply_text_edits :: proc(
	t: ^testing.T,
	source: string,
	edits: []Text_Edit,
	allocator: mem.Allocator,
) -> string {
	current := strings.clone(source, allocator)
	for i := len(edits); i > 0; i -= 1 {
		edit := edits[i - 1]
		start := position_to_offset(current, edit.range.start)
		end := position_to_offset(current, edit.range.end)
		testing.expect(t, start >= 0 && start <= end && end <= len(current))
		if start < 0 || start > end || end > len(current) {
			return current
		}
		current = strings.concatenate({current[:start], edit.new_text, current[end:]}, allocator)
	}
	return current
}

lsp_test_did_open_params :: proc(uri, source: string) -> json.Object {
	params := make(json.Object, 1, context.allocator)
	text_document := make(json.Object, 3, context.allocator)
	text_document["uri"] = json.String(uri)
	text_document["text"] = json.String(source)
	text_document["version"] = json.Integer(1)
	params["textDocument"] = text_document
	return params
}
