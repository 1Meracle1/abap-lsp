package abap_frontend_lsp

import "src:parser"
import "src:semantic"
import workspace "src:workspace"

import json "core:encoding/json"
import "core:fmt"
import "core:mem"
import virtual "core:mem/virtual"
import "core:os"
import "core:strings"
import "core:testing"

Completion_Template_Prefix_Test_Case :: struct {
	prefix:      string,
	label:       string,
	insert_text: string,
}

Completion_Template_Source_Test_Case :: struct {
	source:      string,
	label:       string,
	insert_text: string,
}

Completion_Template_Priority_Test_Case :: struct {
	source:         string,
	symbol_label:   string,
	template_label: string,
	symbol_sort:    string,
	template_sort:  string,
}

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
	testing.expect(t, !(.Disable_ADT_Dependency_Fetch in options.flags))
}

@(test)
server_init_accepts_custom_workspace_options :: proc(t: ^testing.T) {
	options := server_default_workspace_options()
	options.flags += {.Disable_ADT_Dependency_Fetch}

	state: Server_State
	server_init_with_options(&state, context.allocator, options)
	defer server_destroy(&state)

	testing.expect(t, .Disable_ADT_Dependency_Fetch in state.options.flags)
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
		clear_parse_diagnostics(&state)
		if state.parse_diagnostics.allocator.procedure != nil {
			delete(state.parse_diagnostics)
		}
		for _, &doc in state.documents {
			document_destroy(&doc, state.allocator)
		}
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
lsp_reanalysis_suspends_dependency_fetches_for_unsaved_document :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/zmain.abap"
	initial_source := "REPORT zmain."
	changed_source := "REPORT zmain. DATA lo_remote TYPE REF TO zcl_remote."
	state := lsp_test_empty_state()
	append(&state.workspaces, Server_Workspace{root = workspace.Workspace{root_path = `D:\repo`}})
	defer lsp_test_state_destroy(&state)

	testing.expect(t, update_document_from_open(&state, lsp_test_did_open_params(uri, initial_source)))
	server_reanalyze(&state)
	testing.expect(t, state.workspaces[0].has_analysis)

	testing.expect(t, update_document_from_change(&state, lsp_test_did_change_params(uri, changed_source, 2)))
	server_reanalyze(&state)

	doc := state.documents[uri]
	testing.expect(t, doc.has_unsaved_changes)
	testing.expect_value(t, len(state.workspaces[0].analysis.remote_result.misses), 0)
	analysis := semantic.semantic_graph_session_current_analysis(&state.workspaces[0].analysis.session)
	testing.expect(t, analysis != nil)
	if analysis != nil {
		testing.expect_value(t, lsp_test_unresolved_count(analysis, .Global_Symbol, "zcl_remote"), 1)
	}

	testing.expect(t, update_document_from_save(&state, lsp_test_did_save_params(uri, changed_source)))
	server_reanalyze(&state)

	doc = state.documents[uri]
	testing.expect(t, !doc.has_unsaved_changes)
	testing.expect(t, len(state.workspaces[0].analysis.remote_result.misses) > 0)
}

@(test)
lsp_did_change_applies_incremental_text_ranges :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/change.abap"
	source := "REPORT zmain.\nDATA lv_value TYPE i."
	state := lsp_test_empty_state()
	defer lsp_test_state_destroy(&state)

	testing.expect(t, update_document_from_open(&state, lsp_test_did_open_params(uri, source)))

	start := strings.index(source, "value")
	testing.expect(t, start >= 0)
	if start < 0 {
		return
	}
	edit_range := range_from_offsets(source, start, start + len("value"))
	testing.expect(t, update_document_from_change(
		&state,
		lsp_test_did_incremental_change_params(uri, edit_range, "other", 2),
	))

	doc, doc_ok := state.documents[uri]
	testing.expect(t, doc_ok)
	if !doc_ok {
		return
	}
	testing.expect_value(t, doc.text, "REPORT zmain.\nDATA lv_other TYPE i.")
	testing.expect(t, doc.has_unsaved_changes)
}

@(test)
lsp_completion_after_incremental_selector_change_returns_inline_new_members :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_inline_new_selector_incremental.abap"
	before := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_param TYPE string
        iv_param1 TYPE i OPTIONAL.

    METHODS method_name
      IMPORTING
        !iv_value TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD method_name.
  ENDMETHOD.
ENDCLASS.

DATA(lo_inst) = NEW lcl_class( 'iv_param11111' ).
DATA lo_inst1 TYPE REF TO lcl_class.
lo_inst1 = NEW #( iv_param = 'hello' ).

lo_inst->method_name( 'hello' ).
`
	typed := "lo_inst->"
	source := strings.concatenate({before, typed}, context.allocator)
	state := lsp_test_state_with_open_document(uri, before)
	defer lsp_test_state_destroy(&state)

	insert_range := range_from_offsets(before, len(before), len(before))
	testing.expect(t, update_document_from_change(
		&state,
		lsp_test_did_incremental_change_params(uri, insert_range, typed, 2),
	))
	server_reanalyze(&state)

	offset := len(source)
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "method_name")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}
	_, constructor_ok := lsp_test_find_completion_item(items, "constructor")
	testing.expect(t, !constructor_ok)

	filter_text, filter_text_ok := item.filter_text.?
	testing.expect(t, filter_text_ok)
	if filter_text_ok {
		testing.expect_value(t, filter_text, "lo_inst->method_name")
	}
	edit, edit_ok := item.text_edit.?
	testing.expect(t, edit_ok)
	if edit_ok {
		expected_range := range_from_offsets(source, len(before), len(source))
		testing.expect_value(t, edit.range.start.line, expected_range.start.line)
		testing.expect_value(t, edit.range.start.character, expected_range.start.character)
		testing.expect_value(t, edit.range.end.line, expected_range.end.line)
		testing.expect_value(t, edit.range.end.character, expected_range.end.character)
	}
}

@(test)
lsp_completion_after_pending_instance_arrow_inserts_arrow_selector :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_pending_instance_arrow.abap"
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_param TYPE string
        iv_param1 TYPE i OPTIONAL.

    METHODS method_name
      IMPORTING
        !iv_value TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD method_name.
  ENDMETHOD.
ENDCLASS.

DATA(lo_inst) = NEW lcl_class( 'iv_param11111' ).
lo_inst-`
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
	item, item_ok := lsp_test_find_completion_item(items, "method_name")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}
	_, constructor_ok := lsp_test_find_completion_item(items, "constructor")
	testing.expect(t, !constructor_ok)

	filter_text, filter_text_ok := item.filter_text.?
	testing.expect(t, filter_text_ok)
	if filter_text_ok {
		testing.expect_value(t, filter_text, "lo_inst->method_name")
	}
	edit, edit_ok := item.text_edit.?
	testing.expect(t, edit_ok)
	if edit_ok {
		replace_start := offset - len("lo_inst-")
		replace_start_position := offset_to_position(source, replace_start)
		replace_end_position := offset_to_position(source, offset)
		testing.expect_value(t, edit.range.start.line, replace_start_position.line)
		testing.expect_value(t, edit.range.start.character, replace_start_position.character)
		testing.expect_value(t, edit.range.end.line, replace_end_position.line)
		testing.expect_value(t, edit.range.end.character, replace_end_position.character)
		testing.expect_value(
			t,
			edit.new_text,
			`lo_inst->method_name(
  iv_value = $1
)$0`,
		)
	}
}

@(test)
lsp_completion_after_instance_arrow_uses_aliases_and_interface_names :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_interface_alias_selector.abap"
	prefix := `INTERFACE lif_interface.
  METHODS method_name
    IMPORTING
      iv_value TYPE string.
ENDINTERFACE.

CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_interface.
    ALIASES short_name FOR lif_interface~method_name.
    METHODS local_method.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD lif_interface~method_name.
  ENDMETHOD.
  METHOD local_method.
  ENDMETHOD.
ENDCLASS.

DATA(lo_inst) = NEW lcl_class( ).
`
	source := strings.concatenate({prefix, "lo_inst->"}, context.allocator)
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
	_, method_name_ok := lsp_test_find_completion_item(items, "method_name")
	testing.expect(t, !method_name_ok)

	short_name, short_name_ok := lsp_test_find_completion_item(items, "short_name")
	testing.expect(t, short_name_ok)
	if short_name_ok {
		testing.expect_value(t, short_name.kind, COMPLETION_METHOD)
		testing.expect(t, strings.contains(short_name.insert_text, "short_name("))
	}

	interface_item, interface_ok := lsp_test_find_completion_item(items, "lif_interface")
	testing.expect(t, interface_ok)
	if interface_ok {
		testing.expect_value(t, interface_item.kind, COMPLETION_INTERFACE)
		filter_text, filter_text_ok := interface_item.filter_text.?
		testing.expect(t, filter_text_ok)
		if filter_text_ok {
			testing.expect_value(t, filter_text, "lo_inst->lif_interface")
		}
	}

	qualified_source := strings.concatenate({prefix, "lo_inst->lif_interface~"}, context.allocator)
	qualified_state := lsp_test_state_with_open_document(
		"file:///D:/repo/completion_interface_qualified_selector.abap",
		qualified_source,
	)
	defer lsp_test_state_destroy(&qualified_state)

	qualified_offset := len(qualified_source)
	qualified_params := lsp_test_rename_position_params(
		"file:///D:/repo/completion_interface_qualified_selector.abap",
		offset_to_position(qualified_source, qualified_offset),
		"",
	)
	qualified_snapshot, qualified_completion_offset, qualified_snapshot_ok := snapshot_for_position(
		&qualified_state,
		qualified_params,
	)
	testing.expect(t, qualified_snapshot_ok)
	if !qualified_snapshot_ok {
		return
	}
	qualified_items := completion_items_for_snapshot(
		qualified_snapshot,
		qualified_completion_offset,
		true,
		context.allocator,
	)
	method_name, qualified_method_ok := lsp_test_find_completion_item(qualified_items, "method_name")
	testing.expect(t, qualified_method_ok)
	if qualified_method_ok {
		filter_text, filter_text_ok := method_name.filter_text.?
		testing.expect(t, filter_text_ok)
		if filter_text_ok {
			testing.expect_value(t, filter_text, "lo_inst->lif_interface~method_name")
		}
		testing.expect(t, strings.contains(method_name.insert_text, "method_name("))
	}
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
lsp_completion_me_selector_uses_selector_text_edit_and_filter :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_me_selector_text_edit.abap"
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS do_something
      IMPORTING
        iv_param TYPE string
      RETURNING
        VALUE(rv_res) TYPE string.

    METHODS method_name
      IMPORTING
        iv_input TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD do_something.
    me->
  ENDMETHOD.
  METHOD method_name.
  ENDMETHOD.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "me->") + len("me->")
	testing.expect(t, offset >= len("me->"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "method_name")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(
		t,
		item.insert_text,
		`method_name(
  iv_input = $1
)$0`,
	)
	filter_text, filter_text_ok := item.filter_text.?
	testing.expect(t, filter_text_ok)
	if filter_text_ok {
		testing.expect_value(t, filter_text, "me->method_name")
	}
	edit, edit_ok := item.text_edit.?
	testing.expect(t, edit_ok)
	if edit_ok {
		replace_start := offset - len("me->")
		replace_start_position := offset_to_position(source, replace_start)
		replace_end_position := offset_to_position(source, offset)
		testing.expect_value(t, edit.range.start.line, replace_start_position.line)
		testing.expect_value(t, edit.range.start.character, replace_start_position.character)
		testing.expect_value(t, edit.range.end.line, replace_end_position.line)
		testing.expect_value(t, edit.range.end.character, replace_end_position.character)
		testing.expect_value(
			t,
			edit.new_text,
			`me->method_name(
  iv_input = $1
)$0`,
		)
	}
}

@(test)
lsp_completion_me_selector_text_edit_replaces_typed_member_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_me_selector_typed_prefix_text_edit.abap"
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS method_name
      IMPORTING
        iv_input TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD method_name.
    me->meth
  ENDMETHOD.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "me->meth") + len("me->meth")
	testing.expect(t, offset >= len("me->meth"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "method_name")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	filter_text, filter_text_ok := item.filter_text.?
	testing.expect(t, filter_text_ok)
	if filter_text_ok {
		testing.expect_value(t, filter_text, "me->method_name")
	}
	edit, edit_ok := item.text_edit.?
	testing.expect(t, edit_ok)
	if edit_ok {
		replace_start := offset - len("me->meth")
		replace_start_position := offset_to_position(source, replace_start)
		replace_end_position := offset_to_position(source, offset)
		testing.expect_value(t, edit.range.start.line, replace_start_position.line)
		testing.expect_value(t, edit.range.start.character, replace_start_position.character)
		testing.expect_value(t, edit.range.end.line, replace_end_position.line)
		testing.expect_value(t, edit.range.end.character, replace_end_position.character)
		testing.expect_value(
			t,
			edit.new_text,
			`me->method_name(
  iv_input = $1
)$0`,
		)
	}
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
lsp_completion_method_body_unqualified_method_uses_full_call_snippet :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_method_body_call_snippet.abap"
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS do_something.
    METHODS method_name
      IMPORTING
        iv_input TYPE string
        iv_other TYPE i.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD do_something.
    meth
  ENDMETHOD.
  METHOD method_name.
  ENDMETHOD.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "\n    meth") + len("\n    meth")
	testing.expect(t, offset >= len("\n    meth"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "method_name")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(
		t,
		item.insert_text,
		`method_name(
  iv_input = $1
  iv_other = $2
)$0`,
	)
	testing.expect(t, !strings.contains(item.insert_text, "EXPORTING"))
}

@(test)
lsp_completion_method_body_unqualified_method_without_parameters_inserts_empty_call :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_method_body_empty_call.abap"
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS do_something.
    METHODS no_parameters.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD do_something.
    no_
  ENDMETHOD.
  METHOD no_parameters.
  ENDMETHOD.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "\n    no_") + len("\n    no_")
	testing.expect(t, offset >= len("\n    no_"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "no_parameters")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(t, item.insert_text, "no_parameters( )$0")
}

@(test)
lsp_completion_method_implementation_header_keeps_unqualified_method_name_plain :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_method_header_plain.abap"
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS method_name IMPORTING iv_input TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD meth.
  ENDMETHOD.
  METHOD method_name.
  ENDMETHOD.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "\n  METHOD meth.") + len("\n  METHOD meth")
	testing.expect(t, offset >= len("\n  METHOD meth"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "method_name")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(t, item.insert_text, "method_name")
}

@(test)
lsp_completion_in_incomplete_method_implementation_reads_signature_scope :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_method_body_signature.abap"
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS do_something
      IMPORTING
        iv_param TYPE string
      RETURNING
        VALUE(rv_res) TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD do_something.
    rv_
  ENDMETHOD.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "\n    rv_") + len("\n    rv_")
	testing.expect(t, offset >= len("\n    rv_"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "rv_res")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text, "rv_res")
}

@(test)
lsp_completion_in_incomplete_method_implementation_works_after_server_init :: proc(t: ^testing.T) {
	root := lsp_test_temp_root(t, `tmp\lsp_completion_method_body_signature`)
	defer os.remove_all(root)
	file_path := lsp_test_join_path(t, root, "completion_method_body_signature.abap")
	uri, uri_ok := file_uri_from_path(file_path, context.allocator)
	testing.expect(t, uri_ok)
	if !uri_ok {
		return
	}
	root_uri, root_uri_ok := file_uri_from_path(root, context.allocator)
	testing.expect(t, root_uri_ok)
	if !root_uri_ok {
		return
	}

	output_path := `tmp\lsp_completion_method_body_signature.out`
	os.remove(output_path)
	output, output_err := os.create(output_path)
	testing.expect(t, output_err == nil)
	if output_err != nil {
		return
	}
	defer os.close(output)
	defer os.remove(output_path)

	options := server_default_workspace_options()
	options.flags += {.Disable_ADT_Dependency_Fetch}
	state: Server_State
	server_init_with_options(&state, context.allocator, options)
	defer server_destroy(&state)

	params := make(json.Object, 1, context.allocator)
	params["rootUri"] = json.String(root_uri)
	ctx := Request_Context {
		state  = &state,
		output = output,
		id     = json.Integer(1),
	}
	handle_initialize(&ctx, params)

	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS do_something
      IMPORTING
        iv_param TYPE string
      RETURNING
        VALUE(rv_res) TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD do_something.
    rv_
  ENDMETHOD.
ENDCLASS.`
	testing.expect(t, update_document_from_open(&state, lsp_test_did_open_params(uri, source)))
	server_reanalyze(&state)

	lsp_test_reset_temp_allocator()

	offset := strings.index(source, "\n    rv_") + len("\n    rv_")
	params = lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "rv_res")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text, "rv_res")
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
	labels := [?]string {
		"IF ... ENDIF",
		"IF sy-subrc = 0",
		"IF sy-subrc <> 0",
		"IF ... IS INITIAL",
		"IF ... IS NOT INITIAL",
	}
	for label in labels {
		item, item_ok := lsp_test_find_completion_item(items, label)
		testing.expect(t, item_ok)
		if item_ok {
			testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
			testing.expect_value(t, item.sort_text, completion_sort_text("2", label, context.temp_allocator))
			testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		}
	}

	generic, generic_ok := lsp_test_find_completion_item(items, "IF ... ENDIF")
	subrc_zero, subrc_zero_ok := lsp_test_find_completion_item(items, "IF sy-subrc = 0")
	subrc_not_zero, subrc_not_zero_ok := lsp_test_find_completion_item(items, "IF sy-subrc <> 0")
	is_initial, is_initial_ok := lsp_test_find_completion_item(items, "IF ... IS INITIAL")
	is_not_initial, is_not_initial_ok := lsp_test_find_completion_item(items, "IF ... IS NOT INITIAL")
	testing.expect(t, generic_ok)
	testing.expect(t, subrc_zero_ok)
	testing.expect(t, subrc_not_zero_ok)
	testing.expect(t, is_initial_ok)
	testing.expect(t, is_not_initial_ok)
	if !generic_ok || !subrc_zero_ok || !subrc_not_zero_ok || !is_initial_ok || !is_not_initial_ok {
		return
	}

	testing.expect_value(t, generic.insert_text, "IF ${1:condition}.\n  $0\nENDIF.")
	testing.expect_value(t, subrc_zero.insert_text, "IF sy-subrc = 0.\n  $0\nENDIF.")
	testing.expect_value(t, subrc_not_zero.insert_text, "IF sy-subrc <> 0.\n  $0\nENDIF.")
	testing.expect_value(t, is_initial.insert_text, "IF ${1:lv_value} IS INITIAL.\n  $0\nENDIF.")
	testing.expect_value(
		t,
		is_not_initial.insert_text,
		"IF ${1:lv_value} IS NOT INITIAL.\n  $0\nENDIF.",
	)
}

@(test)
lsp_completion_inside_incomplete_nested_if_condition_keeps_enclosing_declarations :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_incomplete_nested_if.abap"
	source := `DATA lt_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.
READ TABLE lt_table INDEX 1 INTO DATA(ls_row).
IF sy-subrc = 0.
  DATA(lv_some_value) = 10.
  IF lv_
ENDIF.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "IF lv_")
	testing.expect(t, offset >= 0)
	if offset < 0 {
		return
	}
	offset += len("IF lv_")
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	_, local_ok := lsp_test_find_completion_item(items, "lv_some_value")

	testing.expect(t, local_ok)
}

@(test)
lsp_completion_class_templates_expand_from_class_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_class_template.abap"
	source := "cla"
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
	labels := [?]string {
		"CLASS ... DEFINITION / IMPLEMENTATION",
		"CLASS ... DEFINITION PUBLIC FINAL CREATE PUBLIC",
		"CLASS ... DEFINITION INHERITING FROM",
		"CLASS ... DEFINITION FINAL CREATE PUBLIC",
		"CLASS ... DEFINITION ABSTRACT",
		"CLASS ... DEFINITION FOR TESTING",
	}
	for label in labels {
		item, item_ok := lsp_test_find_completion_item(items, label)
		testing.expect(t, item_ok)
		if item_ok {
			testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
			testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		}
	}

	basic, basic_ok := lsp_test_find_completion_item(
		items,
		"CLASS ... DEFINITION / IMPLEMENTATION",
	)
	public, public_ok := lsp_test_find_completion_item(
		items,
		"CLASS ... DEFINITION PUBLIC FINAL CREATE PUBLIC",
	)
	inheriting, inheriting_ok := lsp_test_find_completion_item(
		items,
		"CLASS ... DEFINITION INHERITING FROM",
	)
	testing_class, testing_ok := lsp_test_find_completion_item(
		items,
		"CLASS ... DEFINITION FOR TESTING",
	)
	testing.expect(t, basic_ok)
	testing.expect(t, public_ok)
	testing.expect(t, inheriting_ok)
	testing.expect(t, testing_ok)
	if !basic_ok || !public_ok || !inheriting_ok || !testing_ok {
		return
	}

	testing.expect_value(t, basic.sort_text, "2:class ... definition / implementation")
	testing.expect_value(
		t,
		basic.insert_text,
		`CLASS ${1:lcl_class} DEFINITION.
  PUBLIC SECTION.
    $0
ENDCLASS.

CLASS ${1:lcl_class} IMPLEMENTATION.
ENDCLASS.`,
	)
	testing.expect(
		t,
		strings.contains(public.insert_text, "DEFINITION PUBLIC FINAL CREATE PUBLIC."),
	)
	testing.expect(
		t,
		strings.contains(inheriting.insert_text, "INHERITING FROM ${2:lcl_parent}."),
	)
	testing.expect(t, strings.contains(testing_class.insert_text, "METHOD ${2:test_method}."))
}

@(test)
lsp_completion_interface_template_expands_from_interface_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_interface_template.abap"
	source := "int"
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
	item, item_ok := lsp_test_find_completion_item(items, "INTERFACE ... ENDINTERFACE")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
	testing.expect_value(t, item.sort_text, "2:interface ... endinterface")
	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(
		t,
		item.insert_text,
		`INTERFACE ${1:lif_interface}.
  $0
ENDINTERFACE.`,
	)
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
		"LOOP AT ${1:itab} ASSIGNING FIELD-SYMBOL(<${2:row}>).\n  $0\nENDLOOP.",
	)
	testing.expect_value(t, into.kind, COMPLETION_SNIPPET)
	testing.expect_value(t, into.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(
		t,
		into.insert_text,
		"LOOP AT ${1:itab} INTO DATA(${2:row}).\n  $0\nENDLOOP.",
	)
}

@(test)
lsp_completion_select_templates_expand_from_select_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_select_template.abap"
	source := "REPORT zmain.\nFORM run.\n  se"
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
	labels := [?]string {
		"SELECT ... WHERE",
		"SELECT SINGLE ... WHERE",
		"SELECT ... UP TO ... OFFSET",
		"SELECT ... FOR ALL ENTRIES",
		"SELECT ... JOIN",
		"SELECT ... PACKAGE SIZE",
		"SELECT ... CURSOR PACKAGE",
	}
	for label in labels {
		item, item_ok := lsp_test_find_completion_item(items, label)
		testing.expect(t, item_ok)
		if item_ok {
			testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
			testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		}
	}

	basic, basic_ok := lsp_test_find_completion_item(items, "SELECT ... WHERE")
	cursor, cursor_ok := lsp_test_find_completion_item(items, "SELECT ... CURSOR PACKAGE")
	testing.expect(t, basic_ok)
	testing.expect(t, cursor_ok)
	if !basic_ok || !cursor_ok {
		return
	}

	testing.expect_value(t, basic.sort_text, "2:select ... where")
	testing.expect_value(
		t,
		basic.insert_text,
		`SELECT ${1:fields}
  FROM ${2:table}
  INTO TABLE @DATA(${3:lt_rows})
  WHERE ${4:field} = @${5:lv_value}.$0`,
	)
	testing.expect(t, strings.contains(cursor.insert_text, "OPEN CURSOR WITH HOLD @${1:lv_cursor} FOR"))
	testing.expect(t, strings.contains(cursor.insert_text, "FETCH NEXT CURSOR @${1:lv_cursor}"))
	testing.expect(t, strings.contains(cursor.insert_text, "CLOSE CURSOR @${1:lv_cursor}."))
}

@(test)
lsp_completion_try_template_expands_from_try_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_try_template.abap"
	source := "REPORT zmain.\nFORM run.\n  tr"
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
	item, item_ok := lsp_test_find_completion_item(items, "TRY ... CATCH ... ENDTRY")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
	testing.expect_value(t, item.sort_text, "2:try ... catch ... endtry")
	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(
		t,
		item.insert_text,
		"TRY.\n  ${1}\nCATCH ${2:cx_root} INTO DATA(${3:lx_error}).\n  $0\nENDTRY.",
	)
}

@(test)
lsp_completion_commit_and_continue_templates_expand_from_co_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_commit_continue_template.abap"
	source := "REPORT zmain.\nFORM run.\n  co"
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
	commit, commit_ok := lsp_test_find_completion_item(items, "COMMIT WORK")
	commit_wait, commit_wait_ok := lsp_test_find_completion_item(items, "COMMIT WORK AND WAIT")
	continue_item, continue_ok := lsp_test_find_completion_item(items, "CONTINUE")
	testing.expect(t, commit_ok)
	testing.expect(t, commit_wait_ok)
	testing.expect(t, continue_ok)
	if !commit_ok || !commit_wait_ok || !continue_ok {
		return
	}

	testing.expect_value(t, commit.kind, COMPLETION_SNIPPET)
	testing.expect_value(t, commit.sort_text, "2:commit work")
	testing.expect_value(t, commit.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(t, commit.insert_text, "COMMIT WORK.$0")
	testing.expect_value(t, commit_wait.insert_text, "COMMIT WORK AND WAIT.$0")
	testing.expect_value(t, continue_item.sort_text, "2:continue")
	testing.expect_value(t, continue_item.insert_text, "CONTINUE.$0")
}

@(test)
lsp_completion_read_table_templates_expand_from_read_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_read_table_template.abap"
	source := "REPORT zmain.\nFORM run.\n  re"
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
	labels := [?]string {
		"READ TABLE ... INDEX ... INTO",
		"READ TABLE ... INDEX ... ASSIGNING",
		"READ TABLE ... INDEX ... USING KEY ... INTO",
		"READ TABLE ... WITH KEY ... INTO",
		"READ TABLE ... WITH KEY ... ASSIGNING",
		"READ TABLE ... WITH KEY ... REFERENCE INTO",
		"READ TABLE ... WITH KEY ... TRANSPORTING NO FIELDS",
		"READ TABLE ... WITH KEY ... BINARY SEARCH",
		"READ TABLE ... WITH TABLE KEY ... COMPONENTS ... INTO",
		"READ TABLE ... WITH TABLE KEY ... COMPONENTS ... ASSIGNING",
		"READ TABLE ... WITH TABLE KEY ... COMPONENTS ... TRANSPORTING NO FIELDS",
	}
	for label in labels {
		item, item_ok := lsp_test_find_completion_item(items, label)
		testing.expect(t, item_ok)
		if item_ok {
			testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
			testing.expect_value(t, item.sort_text, completion_sort_text("2", label, context.temp_allocator))
			testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		}
	}

	index_into, index_into_ok := lsp_test_find_completion_item(
		items,
		"READ TABLE ... INDEX ... INTO",
	)
	index_assigning, index_assigning_ok := lsp_test_find_completion_item(
		items,
		"READ TABLE ... INDEX ... ASSIGNING",
	)
	table_key, table_key_ok := lsp_test_find_completion_item(
		items,
		"READ TABLE ... WITH TABLE KEY ... COMPONENTS ... ASSIGNING",
	)
	testing.expect(t, index_into_ok)
	testing.expect(t, index_assigning_ok)
	testing.expect(t, table_key_ok)
	if !index_into_ok || !index_assigning_ok || !table_key_ok {
		return
	}

	testing.expect_value(
		t,
		index_into.insert_text,
		"READ TABLE ${1:itab} INDEX ${2:lv_index} INTO DATA(${3:ls_row}).$0",
	)
	testing.expect_value(
		t,
		index_assigning.insert_text,
		"READ TABLE ${1:itab} INDEX ${2:lv_index} ASSIGNING FIELD-SYMBOL(<${3:ls_row}>).$0",
	)
	testing.expect_value(
		t,
		table_key.insert_text,
		"READ TABLE ${1:itab} WITH TABLE KEY ${2:key_name} COMPONENTS ${3:id} = ${4:lv_id} ASSIGNING FIELD-SYMBOL(<${5:ls_row}>).$0",
	)
}

@(test)
lsp_completion_case_template_expands_from_case_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_case_template.abap"
	source := "REPORT zmain.\nFORM run.\n  ca"
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
	item, item_ok := lsp_test_find_completion_item(items, "CASE ... WHEN ... WHEN OTHERS")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
	testing.expect_value(t, item.sort_text, "2:case ... when ... when others")
	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(
		t,
		item.insert_text,
		"CASE ${1:lv_value}.\n  WHEN ${2:value_1}.\n    ${3}\n  WHEN ${4:value_2}.\n    ${5}\n  WHEN OTHERS.\n    $0\nENDCASE.",
	)
	edit, edit_ok := item.text_edit.?
	testing.expect(t, edit_ok)
	if edit_ok {
		testing.expect_value(t, edit.new_text, item.insert_text)
		testing.expect_value(t, edit.range.start.line, 2)
		testing.expect_value(t, edit.range.start.character, 2)
		testing.expect_value(t, edit.range.end.line, 2)
		testing.expect_value(t, edit.range.end.character, 4)
	}
}

@(test)
lsp_completion_expression_templates_expand_from_keyword_prefixes :: proc(t: ^testing.T) {
	cases := [?]Completion_Template_Prefix_Test_Case {
		{
			prefix = "con",
			label = "COND #( WHEN ... THEN ... ELSE ... )",
			insert_text = "COND #( WHEN ${1:condition} THEN ${2:value} ELSE ${3:default} )$0",
		},
		{
			prefix = "con",
			label = "COND ...",
			insert_text = "COND ${1:string}( WHEN ${2:condition} THEN ${3:value} ELSE ${4:default} )$0",
		},
		{
			prefix = "con",
			label = "COND ... LET ... IN",
			insert_text = "COND ${1:string}( LET ${2:lv_value} = ${3:value} IN WHEN ${4:condition} THEN ${2:lv_value} ELSE ${5:default} )$0",
		},
		{
			prefix = "conde",
			label = "condense( val = ... )",
			insert_text = "condense( val = ${1:lv_text} )$0",
		},
		{
			prefix = "conde",
			label = "condense( val = ... del = ... )",
			insert_text = "condense( val = ${1:lv_text} del = ${2:space} )$0",
		},
		{
			prefix = "conde",
			label = "condense( val = ... from = ... to = ... )",
			insert_text = "condense( val = ${1:lv_text} from = ${2:'_'} to = ${3:space} )$0",
		},
		{
			prefix = "fin",
			label = "find( val = ... sub = ... )",
			insert_text = "find( val = ${1:lv_text} sub = ${2:'text'} )$0",
		},
		{
			prefix = "fin",
			label = "find( val = ... regex = ... )",
			insert_text = "find( val = ${1:lv_text} regex = ${2:'pattern'} )$0",
		},
		{
			prefix = "fin",
			label = "find( val = ... sub = ... occ = ... )",
			insert_text = "find( val = ${1:lv_text} sub = ${2:'text'} occ = ${3:1} )$0",
		},
		{
			prefix = "fi",
			label = "FILTER #( ... WHERE ... )",
			insert_text = "FILTER #( ${1:itab} WHERE ${2:field} = ${3:lv_value} )$0",
		},
		{
			prefix = "fi",
			label = "FILTER #( ... USING KEY ... WHERE ... )",
			insert_text = "FILTER #( ${1:itab} USING KEY ${2:key_name} WHERE ${3:field} = ${4:lv_value} )$0",
		},
		{
			prefix = "fi",
			label = "FILTER #( ... EXCEPT WHERE ... )",
			insert_text = "FILTER #( ${1:itab} EXCEPT WHERE ${2:field} = ${3:lv_value} )$0",
		},
		{
			prefix = "re",
			label = "REDUCE ... FOR ... IN",
			insert_text = "REDUCE ${1:i}( INIT ${2:result} = ${3:0} FOR ${4:row} IN ${5:itab} NEXT ${2:result} = ${2:result} + ${4:row}-${6:amount} )$0",
		},
		{
			prefix = "re",
			label = "REDUCE ... FOR ... IN ... WHERE",
			insert_text = "REDUCE ${1:i}( INIT ${2:result} = ${3:0} FOR ${4:row} IN ${5:itab} WHERE ( ${6:field} = ${7:lv_value} ) NEXT ${2:result} = ${2:result} + ${4:row}-${8:amount} )$0",
		},
		{
			prefix = "re",
			label = "REDUCE ... FOR ... THEN ... UNTIL",
			insert_text = "REDUCE ${1:i}( INIT ${2:result} = ${3:0} FOR ${4:index} = ${5:1} THEN ${4:index} + ${6:1} UNTIL ${4:index} > ${7:limit} NEXT ${2:result} = ${2:result} + ${4:index} )$0",
		},
		{
			prefix = "re",
			label = "REDUCE ... FOR ... THEN ... WHILE",
			insert_text = "REDUCE ${1:i}( INIT ${2:result} = ${3:0} FOR ${4:index} = ${5:1} THEN ${4:index} + ${6:1} WHILE ${4:index} <= ${7:limit} NEXT ${2:result} = ${2:result} + ${4:index} )$0",
		},
		{
			prefix = "fo",
			label = "FOR ... IN",
			insert_text = "FOR ${1:row} IN ${2:itab} ( ${1:row} )$0",
		},
		{
			prefix = "fo",
			label = "FOR ... IN ... WHERE",
			insert_text = "FOR ${1:row} IN ${2:itab} WHERE ( ${3:field} = ${4:lv_value} ) ( ${1:row} )$0",
		},
		{
			prefix = "fo",
			label = "FOR ... THEN ... UNTIL",
			insert_text = "FOR ${1:index} = ${2:1} THEN ${1} + ${3:1} UNTIL ${1:index} > ${4:limit} ( ${1:index} )$0",
		},
		{
			prefix = "fo",
			label = "FOR ... THEN ... WHILE",
			insert_text = "FOR ${1:index} = ${2:1} THEN ${1:index} + ${3:1} WHILE ${1:index} <= ${4:limit} ( ${1:index} )$0",
		},
	}

	for test_case, i in cases {
		uri := strings.concatenate(
			{"file:///D:/repo/completion_expression_template_", fmt.tprintf("%d", i), ".abap"},
			context.temp_allocator,
		)
		source := strings.concatenate({"DATA dummy TYPE i.\nWRITE ", test_case.prefix}, context.temp_allocator)
		state := lsp_test_state_with_open_document(uri, source)
		defer lsp_test_state_destroy(&state)

		offset := len(source)
		params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
		snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
		testing.expect(t, snapshot_ok)
		if !snapshot_ok {
			continue
		}

		items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
		item, item_ok := lsp_test_find_completion_item(items, test_case.label)
		testing.expect(t, item_ok)
		if !item_ok {
			continue
		}

		testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
		testing.expect_value(
			t,
			item.sort_text,
			completion_sort_text("2", test_case.label, context.temp_allocator),
		)
		testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		testing.expect_value(t, item.insert_text, test_case.insert_text)
		edit, edit_ok := item.text_edit.?
		testing.expect(t, edit_ok)
		if edit_ok {
			testing.expect_value(t, edit.new_text, item.insert_text)
			testing.expect_value(t, edit.range.start.line, 1)
			testing.expect_value(t, edit.range.start.character, len("WRITE "))
			testing.expect_value(t, edit.range.end.line, 1)
			testing.expect_value(
				t,
				edit.range.end.character,
				len("WRITE ") + len(test_case.prefix),
			)
		}
	}
}

@(test)
lsp_completion_common_statement_templates_expand_from_keyword_prefixes :: proc(t: ^testing.T) {
	cases := [?]Completion_Template_Prefix_Test_Case {
		{
			prefix = "me",
			label = "MESSAGE ... TYPE",
			insert_text = "MESSAGE ${1:'Text'} TYPE ${2:'S'}.$0",
		},
		{
			prefix = "de",
			label = "DESCRIBE TABLE ... LINES",
			insert_text = "DESCRIBE TABLE ${1:itab} LINES ${2:lv_lines}.$0",
		},
		{
			prefix = "ex",
			label = "EXPORT ... TO MEMORY ID",
			insert_text = "EXPORT ${1:name} = ${2:value} TO MEMORY ID ${3:'id'}.$0",
		},
		{
			prefix = "im",
			label = "IMPORT ... FROM MEMORY ID",
			insert_text = "IMPORT ${1:name} = ${2:value} FROM MEMORY ID ${3:'id'}.$0",
		},
		{
			prefix = "ra",
			label = "RAISE EXCEPTION TYPE",
			insert_text = "RAISE EXCEPTION TYPE ${1:cx_static_check}.$0",
		},
		{
			prefix = "in",
			label = "INSERT ... INTO TABLE",
			insert_text = "INSERT ${1:wa} INTO TABLE ${2:itab}.$0",
		},
		{
			prefix = "in",
			label = "INSERT ... FROM VALUE #( ... )",
			insert_text = "INSERT ${1:dbtab} FROM VALUE #( ${2} ).$0",
		},
		{
			prefix = "del",
			label = "DELETE ... INDEX",
			insert_text = "DELETE ${1:itab} INDEX ${2:lv_index}.$0",
		},
		{
			prefix = "up",
			label = "UPDATE ... SET ... WHERE",
			insert_text = "UPDATE ${1:dbtab} SET ${2:field} = @${3:lv_value} WHERE ${4:key_field} = @${5:lv_key}.$0",
		},
		{
			prefix = "cond",
			label = "CONDENSE ...",
			insert_text = "CONDENSE ${1:lv_text}.$0",
		},
		{
			prefix = "cond",
			label = "CONDENSE ... NO-GAPS",
			insert_text = "CONDENSE ${1:lv_text} NO-GAPS.$0",
		},
		{
			prefix = "conv",
			label = "CONVERT DATE ... TIME ... INTO TIME STAMP",
			insert_text = "CONVERT DATE ${1:lv_date}\n        TIME ${2:lv_time}\n        INTO TIME STAMP DATA(${3:lv_timestamp})\n        TIME ZONE ${4:lv_time_zone}.$0",
		},
		{
			prefix = "find",
			label = "FIND ... IN",
			insert_text = "FIND ${1:'text'} IN ${2:lv_text}.$0",
		},
		{
			prefix = "find",
			label = "FIND FIRST OCCURRENCE OF ... IN",
			insert_text = "FIND FIRST OCCURRENCE OF ${1:'text'} IN ${2:lv_text} MATCH OFFSET ${3:lv_offset} MATCH LENGTH ${4:lv_length}.$0",
		},
		{
			prefix = "find",
			label = "FIND ALL OCCURRENCES OF ... IN",
			insert_text = "FIND ALL OCCURRENCES OF ${1:'text'} IN ${2:lv_text} MATCH COUNT ${3:lv_count}.$0",
		},
		{
			prefix = "find",
			label = "FIND REGEX ... IN",
			insert_text = "FIND REGEX ${1:'pattern'} IN ${2:lv_text} MATCH OFFSET ${3:lv_offset} MATCH LENGTH ${4:lv_length}.$0",
		},
		{
			prefix = "find",
			label = "FIND REGEX ... IN TABLE",
			insert_text = "FIND REGEX ${1:'pattern'} IN TABLE ${2:lt_text} MATCH LINE ${3:lv_line} MATCH OFFSET ${4:lv_offset} SUBMATCHES ${5:lv_match}.$0",
		},
		{
			prefix = "find",
			label = "FIND ALL OCCURRENCES OF REGEX ... IN TABLE ... RESULTS",
			insert_text = "FIND ALL OCCURRENCES OF REGEX ${1:'pattern'} IN TABLE ${2:lt_text} RESULTS ${3:lt_results}.$0",
		},
		{
			prefix = "find",
			label = "FIND ... IN SECTION OFFSET ... LENGTH ... OF",
			insert_text = "FIND ${1:'text'} IN SECTION OFFSET ${2:lv_offset} LENGTH ${3:lv_length} OF ${4:lv_text} MATCH OFFSET ${5:lv_match_offset}.$0",
		},
		{
			prefix = "type-",
			label = "TYPE-POOLS ...",
			insert_text = "TYPE-POOLS ${1:abap}.$0",
		},
		{
			prefix = "type-",
			label = "TYPE-POOLS: ...",
			insert_text = "TYPE-POOLS:\n  ${1:abap},\n  ${2:icon}.$0",
		},
		{
			prefix = "fi",
			label = "FIELD-SYMBOLS ... TYPE",
			insert_text = "FIELD-SYMBOLS <${1:fs}> TYPE ${2:any}.$0",
		},
		{
			prefix = "mo",
			label = "MOVE-CORRESPONDING ... TO",
			insert_text = "MOVE-CORRESPONDING ${1:source} TO ${2:target}.$0",
		},
		{
			prefix = "co",
			label = "CONCATENATE ... INTO",
			insert_text = "CONCATENATE ${1:lv_a} ${2:lv_b} INTO ${3:lv_text}.$0",
		},
		{
			prefix = "sp",
			label = "SPLIT ... AT ... INTO TABLE",
			insert_text = "SPLIT ${1:lv_text} AT ${2:','} INTO TABLE ${3:lt_parts}.$0",
		},
		{
			prefix = "ap",
			label = "APPEND ... TO",
			insert_text = "APPEND ${1:wa} TO ${2:itab}.$0",
		},
		{
			prefix = "so",
			label = "SORT ... BY",
			insert_text = "SORT ${1:itab} BY ${2:field}.$0",
		},
	}

	for test_case, i in cases {
		uri := strings.concatenate(
			{"file:///D:/repo/completion_common_template_", fmt.tprintf("%d", i), ".abap"},
			context.temp_allocator,
		)
		source := strings.concatenate({"REPORT zmain.\nFORM run.\n  ", test_case.prefix}, context.temp_allocator)
		state := lsp_test_state_with_open_document(uri, source)
		defer lsp_test_state_destroy(&state)

		offset := len(source)
		params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
		snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
		testing.expect(t, snapshot_ok)
		if !snapshot_ok {
			continue
		}

		items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
		item, item_ok := lsp_test_find_completion_item(items, test_case.label)
		testing.expect(t, item_ok)
		if !item_ok {
			continue
		}

		testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
		testing.expect_value(
			t,
			item.sort_text,
			completion_sort_text("2", test_case.label, context.temp_allocator),
		)
		testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		testing.expect_value(t, item.insert_text, test_case.insert_text)
		edit, edit_ok := item.text_edit.?
		testing.expect(t, edit_ok)
		if edit_ok {
			testing.expect_value(t, edit.new_text, item.insert_text)
			testing.expect_value(t, edit.range.start.line, 2)
			testing.expect_value(t, edit.range.start.character, 2)
			testing.expect_value(t, edit.range.end.line, 2)
			testing.expect_value(t, edit.range.end.character, 2 + len(test_case.prefix))
		}
		if i == 0 {
			payload, payload_err := json.marshal(
				item,
				json.Marshal_Options{spec = .JSON},
				context.allocator,
			)
			testing.expect(t, payload_err == nil)
			if payload_err == nil {
				testing.expect(t, strings.contains(string(payload), `"textEdit"`))
				testing.expect(t, strings.contains(string(payload), `"newText"`))
			}
		}
	}
}

@(test)
lsp_completion_convert_time_stamp_templates_expand_from_convert_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_convert_template.abap"
	source := "REPORT zmain.\nFORM run.\n  conv"
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
	labels := [?]string {
		"CONVERT DATE ... TIME ... INTO TIME STAMP",
		"CONVERT DATE ... TIME ... DAYLIGHT SAVING TIME ... INTO TIME STAMP",
		"CONVERT TIME STAMP ... INTO DATE ... TIME",
	}
	for label in labels {
		item, item_ok := lsp_test_find_completion_item(items, label)
		testing.expect(t, item_ok)
		if item_ok {
			testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
			testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		}
	}

	with_dst, with_dst_ok := lsp_test_find_completion_item(
		items,
		"CONVERT DATE ... TIME ... DAYLIGHT SAVING TIME ... INTO TIME STAMP",
	)
	testing.expect(t, with_dst_ok)
	if with_dst_ok {
		testing.expect(t, strings.contains(with_dst.insert_text, "DAYLIGHT SAVING TIME ${3:lv_dst}"))
		testing.expect(t, strings.contains(with_dst.insert_text, "INTO TIME STAMP DATA(${4:lv_timestamp})"))
	}
}

@(test)
lsp_completion_method_definition_templates_expand_from_keyword_prefixes :: proc(t: ^testing.T) {
	method_uri := "file:///D:/repo/completion_methods_template.abap"
	method_source := "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\n    meth\nENDCLASS."
	method_state := lsp_test_state_with_open_document(method_uri, method_source)
	defer lsp_test_state_destroy(&method_state)

	method_offset := strings.index(method_source, "meth") + len("meth")
	testing.expect(t, method_offset >= len("meth"))
	method_params := lsp_test_rename_position_params(
		method_uri,
		offset_to_position(method_source, method_offset),
		"",
	)
	method_snapshot, method_completion_offset, method_snapshot_ok := snapshot_for_position(
		&method_state,
		method_params,
	)
	testing.expect(t, method_snapshot_ok)
	if !method_snapshot_ok {
		return
	}

	method_items := completion_items_for_snapshot(
		method_snapshot,
		method_completion_offset,
		true,
		context.allocator,
	)
	method_labels := [?]string {
		"METHODS ...",
		"METHODS ... IMPORTING",
		"METHODS ... EXPORTING",
		"METHODS ... CHANGING",
		"METHODS ... RECEIVING",
		"METHODS ... RETURNING",
		"METHODS ... IMPORTING RETURNING",
		"METHODS ... IMPORTING EXPORTING",
		"METHODS ... IMPORTING CHANGING",
		"METHODS ... IMPORTING EXPORTING CHANGING",
		"METHODS ... RAISING",
		"METHODS ... IMPORTING RAISING",
		"METHODS ... IMPORTING RETURNING RAISING",
		"METHODS ... EXCEPTIONS",
		"METHODS ... FOR EVENT",
		"METHODS ... FOR TESTING",
		"METHODS ... REDEFINITION",
		"METHODS ... ABSTRACT",
		"METHODS ... FINAL",
	}
	for label in method_labels {
		item, item_ok := lsp_test_find_completion_item(method_items, label)
		testing.expect(t, item_ok)
		if item_ok {
			testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
			testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
			testing.expect_value(
				t,
				item.sort_text,
				completion_sort_text("2", label, context.temp_allocator),
			)
		}
	}

	full_method, full_method_ok := lsp_test_find_completion_item(
		method_items,
		"METHODS ... IMPORTING RETURNING RAISING",
	)
	event_method, event_method_ok := lsp_test_find_completion_item(
		method_items,
		"METHODS ... FOR EVENT",
	)
	basic_method, basic_method_ok := lsp_test_find_completion_item(method_items, "METHODS ...")
	testing.expect(t, full_method_ok)
	testing.expect(t, event_method_ok)
	testing.expect(t, basic_method_ok)
	if full_method_ok {
		testing.expect_value(
			t,
			full_method.insert_text,
			"METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  RETURNING\n    VALUE(${4:rv_result}) TYPE ${5:string}\n  RAISING\n    ${6:cx_static_check}.$0",
		)
		edit, edit_ok := full_method.text_edit.?
		testing.expect(t, edit_ok)
		if edit_ok {
			testing.expect_value(t, edit.new_text, full_method.insert_text)
			testing.expect_value(t, edit.range.start.line, 2)
			testing.expect_value(t, edit.range.start.character, 4)
			testing.expect_value(t, edit.range.end.line, 2)
			testing.expect_value(t, edit.range.end.character, 8)
		}
	}
	if event_method_ok {
		testing.expect_value(
			t,
			event_method.insert_text,
			"METHODS ${1:on_event}\n  FOR EVENT ${2:event_name} OF ${3:lcl_source}\n  IMPORTING\n    !${4:sender}.$0",
		)
	}
	if basic_method_ok {
		testing.expect_value(t, basic_method.insert_text, "METHODS ${1:method_name}.$0")
	}

	interfaces_uri := "file:///D:/repo/completion_interfaces_template.abap"
	interfaces_source := "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\n    interfaces\nENDCLASS."
	interfaces_state := lsp_test_state_with_open_document(interfaces_uri, interfaces_source)
	defer lsp_test_state_destroy(&interfaces_state)

	interfaces_offset := strings.index(interfaces_source, "interfaces") + len("interfaces")
	testing.expect(t, interfaces_offset >= len("interfaces"))
	interfaces_params := lsp_test_rename_position_params(
		interfaces_uri,
		offset_to_position(interfaces_source, interfaces_offset),
		"",
	)
	interfaces_snapshot, interfaces_completion_offset, interfaces_snapshot_ok := snapshot_for_position(
		&interfaces_state,
		interfaces_params,
	)
	testing.expect(t, interfaces_snapshot_ok)
	if !interfaces_snapshot_ok {
		return
	}

	interfaces_items := completion_items_for_snapshot(
		interfaces_snapshot,
		interfaces_completion_offset,
		true,
		context.allocator,
	)
	interfaces_item, interfaces_item_ok := lsp_test_find_completion_item(
		interfaces_items,
		"INTERFACES ...",
	)
	testing.expect(t, interfaces_item_ok)
	if interfaces_item_ok {
		testing.expect_value(t, interfaces_item.kind, COMPLETION_SNIPPET)
		testing.expect_value(
			t,
			interfaces_item.sort_text,
			"2:interfaces ...",
		)
		testing.expect_value(
			t,
			interfaces_item.insert_text_format,
			COMPLETION_INSERT_TEXT_FORMAT_SNIPPET,
		)
		testing.expect_value(
			t,
			interfaces_item.insert_text,
			"INTERFACES ${1:lif_interface}.$0",
		)
		edit, edit_ok := interfaces_item.text_edit.?
		testing.expect(t, edit_ok)
		if edit_ok {
			testing.expect_value(t, edit.new_text, interfaces_item.insert_text)
			testing.expect_value(t, edit.range.start.line, 2)
			testing.expect_value(t, edit.range.start.character, 4)
			testing.expect_value(t, edit.range.end.line, 2)
			testing.expect_value(t, edit.range.end.character, 14)
		}
	}

	aliases_uri := "file:///D:/repo/completion_aliases_template.abap"
	aliases_source := "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\n    aliases\nENDCLASS."
	aliases_state := lsp_test_state_with_open_document(aliases_uri, aliases_source)
	defer lsp_test_state_destroy(&aliases_state)

	aliases_offset := strings.index(aliases_source, "aliases") + len("aliases")
	testing.expect(t, aliases_offset >= len("aliases"))
	aliases_params := lsp_test_rename_position_params(
		aliases_uri,
		offset_to_position(aliases_source, aliases_offset),
		"",
	)
	aliases_snapshot, aliases_completion_offset, aliases_snapshot_ok := snapshot_for_position(
		&aliases_state,
		aliases_params,
	)
	testing.expect(t, aliases_snapshot_ok)
	if !aliases_snapshot_ok {
		return
	}

	aliases_items := completion_items_for_snapshot(
		aliases_snapshot,
		aliases_completion_offset,
		true,
		context.allocator,
	)
	aliases_item, aliases_item_ok := lsp_test_find_completion_item(
		aliases_items,
		"ALIASES ... FOR ...",
	)
	testing.expect(t, aliases_item_ok)
	if aliases_item_ok {
		testing.expect_value(t, aliases_item.kind, COMPLETION_SNIPPET)
		testing.expect_value(t, aliases_item.sort_text, "2:aliases ... for ...")
		testing.expect_value(
			t,
			aliases_item.insert_text_format,
			COMPLETION_INSERT_TEXT_FORMAT_SNIPPET,
		)
		testing.expect_value(
			t,
			aliases_item.insert_text,
			"ALIASES ${1:alias_name} FOR ${2:lif_interface}~${3:member_name}.$0",
		)
		edit, edit_ok := aliases_item.text_edit.?
		testing.expect(t, edit_ok)
		if edit_ok {
			testing.expect_value(t, edit.new_text, aliases_item.insert_text)
			testing.expect_value(t, edit.range.start.line, 2)
			testing.expect_value(t, edit.range.start.character, 4)
			testing.expect_value(t, edit.range.end.line, 2)
			testing.expect_value(t, edit.range.end.character, 11)
		}
	}

	class_uri := "file:///D:/repo/completion_class_methods_template.abap"
	class_source := "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\n    class-m\nENDCLASS."
	class_state := lsp_test_state_with_open_document(class_uri, class_source)
	defer lsp_test_state_destroy(&class_state)

	class_offset := strings.index(class_source, "class-m") + len("class-m")
	testing.expect(t, class_offset >= len("class-m"))
	class_params := lsp_test_rename_position_params(
		class_uri,
		offset_to_position(class_source, class_offset),
		"",
	)
	class_snapshot, class_completion_offset, class_snapshot_ok := snapshot_for_position(
		&class_state,
		class_params,
	)
	testing.expect(t, class_snapshot_ok)
	if !class_snapshot_ok {
		return
	}

	class_items := completion_items_for_snapshot(
		class_snapshot,
		class_completion_offset,
		true,
		context.allocator,
	)
	class_labels := [?]string {
		"CLASS-METHODS ...",
		"CLASS-METHODS ... IMPORTING",
		"CLASS-METHODS ... EXPORTING",
		"CLASS-METHODS ... CHANGING",
		"CLASS-METHODS ... RECEIVING",
		"CLASS-METHODS ... RETURNING",
		"CLASS-METHODS ... IMPORTING RETURNING",
		"CLASS-METHODS ... IMPORTING EXPORTING",
		"CLASS-METHODS ... IMPORTING CHANGING",
		"CLASS-METHODS ... IMPORTING EXPORTING CHANGING",
		"CLASS-METHODS ... RAISING",
		"CLASS-METHODS ... IMPORTING RAISING",
		"CLASS-METHODS ... IMPORTING RETURNING RAISING",
		"CLASS-METHODS ... EXCEPTIONS",
		"CLASS-METHODS ... FOR EVENT",
		"CLASS-METHODS ... ABSTRACT",
		"CLASS-METHODS ... FINAL",
	}
	for label in class_labels {
		item, item_ok := lsp_test_find_completion_item(class_items, label)
		testing.expect(t, item_ok)
		if item_ok {
			testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
			testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		}
	}
	_, class_testing_ok := lsp_test_find_completion_item(
		class_items,
		"CLASS-METHODS ... FOR TESTING",
	)
	testing.expect(t, !class_testing_ok)

	class_full, class_full_ok := lsp_test_find_completion_item(
		class_items,
		"CLASS-METHODS ... IMPORTING EXPORTING CHANGING",
	)
	testing.expect(t, class_full_ok)
	if class_full_ok {
		testing.expect_value(
			t,
			class_full.insert_text,
			"CLASS-METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  EXPORTING\n    !${4:ev_value} TYPE ${5:string}\n  CHANGING\n    !${6:cv_value} TYPE ${7:string}.$0",
		)
		edit, edit_ok := class_full.text_edit.?
		testing.expect(t, edit_ok)
		if edit_ok {
			testing.expect_value(t, edit.new_text, class_full.insert_text)
			testing.expect_value(t, edit.range.start.line, 2)
			testing.expect_value(t, edit.range.start.character, 4)
			testing.expect_value(t, edit.range.end.line, 2)
			testing.expect_value(t, edit.range.end.character, 11)
		}
	}
}

@(test)
lsp_completion_begin_end_statement_templates_expand_from_keyword_prefixes :: proc(t: ^testing.T) {
	cases := [?]Completion_Template_Prefix_Test_Case {
		{
			prefix = "ty",
			label = "TYPES: BEGIN OF ... END OF",
			insert_text = "TYPES:\n  BEGIN OF ${1:ty_line},\n    ${2:field} TYPE ${3:string},\n  END OF ${1:ty_line}.$0",
		},
		{
			prefix = "ty",
			label = "TYPES ... TYPE",
			insert_text = "TYPES ${1:ty_value} TYPE ${2:string}.$0",
		},
		{
			prefix = "ty",
			label = "TYPES ... LIKE",
			insert_text = "TYPES ${1:ty_value} LIKE ${2:sy-datum}.$0",
		},
		{
			prefix = "ty",
			label = "TYPES ... TYPE c LENGTH",
			insert_text = "TYPES ${1:ty_text} TYPE c LENGTH ${2:10}.$0",
		},
		{
			prefix = "ty",
			label = "TYPES ... TYPE p LENGTH DECIMALS",
			insert_text = "TYPES ${1:ty_amount} TYPE p LENGTH ${2:8} DECIMALS ${3:2}.$0",
		},
		{
			prefix = "ty",
			label = "TYPES ... TYPE REF TO",
			insert_text = "TYPES ${1:ty_ref} TYPE REF TO ${2:object}.$0",
		},
		{
			prefix = "ty",
			label = "TYPES ... TYPE STANDARD TABLE OF",
			insert_text = "TYPES ${1:ty_table} TYPE STANDARD TABLE OF ${2:string} WITH EMPTY KEY.$0",
		},
		{
			prefix = "ty",
			label = "TYPES ... TYPE SORTED TABLE OF",
			insert_text = "TYPES ${1:ty_table} TYPE SORTED TABLE OF ${2:string} WITH UNIQUE KEY ${3:table_line}.$0",
		},
		{
			prefix = "ty",
			label = "TYPES ... TYPE HASHED TABLE OF",
			insert_text = "TYPES ${1:ty_table} TYPE HASHED TABLE OF ${2:string} WITH UNIQUE KEY ${3:table_line}.$0",
		},
		{
			prefix = "ty",
			label = "TYPES ... TYPE RANGE OF",
			insert_text = "TYPES ${1:ty_range} TYPE RANGE OF ${2:sy-datum}.$0",
		},
		{
			prefix = "da",
			label = "DATA: BEGIN OF ... END OF",
			insert_text = "DATA:\n  BEGIN OF ${1:ls_row},\n    ${2:field} TYPE ${3:string},\n  END OF ${1:ls_row}.$0",
		},
		{
			prefix = "da",
			label = "DATA: BEGIN OF COMMON PART ... END OF COMMON PART",
			insert_text = "DATA:\n  BEGIN OF COMMON PART ${1:common_part}.\nDATA:\n  END OF COMMON PART.$0",
		},
		{
			prefix = "const",
			label = "CONSTANTS: BEGIN OF ... END OF",
			insert_text = "CONSTANTS:\n  BEGIN OF ${1:c_values},\n    ${2:name} TYPE ${3:string} VALUE ${4:''},\n  END OF ${1:c_values}.$0",
		},
		{
			prefix = "sta",
			label = "STATICS: BEGIN OF ... END OF",
			insert_text = "STATICS:\n  BEGIN OF ${1:s_state},\n    ${2:field} TYPE ${3:string},\n  END OF ${1:s_state}.$0",
		},
		{
			prefix = "class-da",
			label = "CLASS-DATA: BEGIN OF ... END OF",
			insert_text = "CLASS-DATA:\n  BEGIN OF ${1:gs_row},\n    ${2:field} TYPE ${3:string},\n  END OF ${1:gs_row}.$0",
		},
		{
			prefix = "se",
			label = "SELECTION-SCREEN BEGIN OF SCREEN ... END OF SCREEN",
			insert_text = "SELECTION-SCREEN BEGIN OF SCREEN ${1:1000} TITLE ${2:sy-title}.\n  $0\nSELECTION-SCREEN END OF SCREEN ${1:1000}.",
		},
		{
			prefix = "se",
			label = "SELECTION-SCREEN BEGIN OF BLOCK ... END OF BLOCK",
			insert_text = "SELECTION-SCREEN BEGIN OF BLOCK ${1:b1} WITH FRAME TITLE ${2:text-001}.\n  $0\nSELECTION-SCREEN END OF BLOCK ${1:b1}.",
		},
		{
			prefix = "se",
			label = "SELECTION-SCREEN BEGIN OF LINE ... END OF LINE",
			insert_text = "SELECTION-SCREEN BEGIN OF LINE.\n  $0\nSELECTION-SCREEN END OF LINE.",
		},
	}

	for test_case, i in cases {
		uri := strings.concatenate(
			{"file:///D:/repo/completion_begin_end_template_", fmt.tprintf("%d", i), ".abap"},
			context.temp_allocator,
		)
		source := strings.concatenate({"REPORT zmain.\n  ", test_case.prefix}, context.temp_allocator)
		state := lsp_test_state_with_open_document(uri, source)
		defer lsp_test_state_destroy(&state)

		offset := len(source)
		params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
		snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
		testing.expect(t, snapshot_ok)
		if !snapshot_ok {
			continue
		}

		items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
		item, item_ok := lsp_test_find_completion_item(items, test_case.label)
		testing.expect(t, item_ok)
		if !item_ok {
			continue
		}

		testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
		testing.expect_value(
			t,
			item.sort_text,
			completion_sort_text("2", test_case.label, context.temp_allocator),
		)
		testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		testing.expect_value(t, item.insert_text, test_case.insert_text)
		edit, edit_ok := item.text_edit.?
		testing.expect(t, edit_ok)
		if edit_ok {
			testing.expect_value(t, edit.new_text, item.insert_text)
			testing.expect_value(t, edit.range.start.line, 1)
			testing.expect_value(t, edit.range.start.character, 2)
			testing.expect_value(t, edit.range.end.line, 1)
			testing.expect_value(t, edit.range.end.character, 2 + len(test_case.prefix))
		}
	}
}

@(test)
lsp_completion_types_chained_begin_end_template_expands_from_begin_prefix :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_types_chained_begin_end_template.abap"
	source := `TYPES: BEGIN OF ty_line,
         field TYPE string,
       END OF ty_line,

       BEG.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "BEG.") + len("BEG")
	testing.expect(t, offset >= len("BEG"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "BEGIN OF ... END OF")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
	testing.expect_value(t, item.sort_text, "2:begin of ... end of")
	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
	testing.expect_value(
		t,
		item.insert_text,
		"BEGIN OF ${1:ty_line},\n  ${2:field} TYPE ${3:string},\nEND OF ${1:ty_line}$0",
	)
	edit, edit_ok := item.text_edit.?
	testing.expect(t, edit_ok)
	if edit_ok {
		testing.expect_value(t, edit.new_text, item.insert_text)
		testing.expect_value(t, edit.range.start.line, 4)
		testing.expect_value(t, edit.range.start.character, 7)
		testing.expect_value(t, edit.range.end.line, 4)
		testing.expect_value(t, edit.range.end.character, 10)

		applied := lsp_test_apply_text_edits(t, source, []Text_Edit{edit}, context.allocator)
		testing.expect(t, strings.has_suffix(applied, "END OF ${1:ty_line}$0."))
	}
}

@(test)
lsp_completion_type_addition_templates_expand_in_types_clause :: proc(t: ^testing.T) {
	cases := [?]Completion_Template_Prefix_Test_Case {
		{
			prefix = "type",
			label = "TYPE ...",
			insert_text = "TYPE ${1:string}$0",
		},
		{
			prefix = "type",
			label = "TYPE c LENGTH",
			insert_text = "TYPE c LENGTH ${1:10}$0",
		},
		{
			prefix = "type",
			label = "TYPE p LENGTH DECIMALS",
			insert_text = "TYPE p LENGTH ${1:8} DECIMALS ${2:2}$0",
		},
		{
			prefix = "type",
			label = "TYPE REF TO ...",
			insert_text = "TYPE REF TO ${1:object}$0",
		},
		{
			prefix = "type",
			label = "TYPE LINE OF ...",
			insert_text = "TYPE LINE OF ${1:itab}$0",
		},
		{
			prefix = "type",
			label = "TYPE TABLE OF ...",
			insert_text = "TYPE TABLE OF ${1:string}$0",
		},
		{
			prefix = "type",
			label = "TYPE ANY TABLE",
			insert_text = "TYPE ANY TABLE$0",
		},
		{
			prefix = "type",
			label = "TYPE INDEX TABLE",
			insert_text = "TYPE INDEX TABLE$0",
		},
		{
			prefix = "type",
			label = "TYPE STANDARD TABLE",
			insert_text = "TYPE STANDARD TABLE$0",
		},
		{
			prefix = "type",
			label = "TYPE STANDARD TABLE OF ... WITH EMPTY KEY",
			insert_text = "TYPE STANDARD TABLE OF ${1:string} WITH EMPTY KEY$0",
		},
		{
			prefix = "type",
			label = "TYPE STANDARD TABLE OF ... WITH DEFAULT KEY",
			insert_text = "TYPE STANDARD TABLE OF ${1:string} WITH DEFAULT KEY$0",
		},
		{
			prefix = "type",
			label = "TYPE SORTED TABLE OF ... WITH UNIQUE KEY",
			insert_text = "TYPE SORTED TABLE OF ${1:string} WITH UNIQUE KEY ${2:table_line}$0",
		},
		{
			prefix = "type",
			label = "TYPE SORTED TABLE OF ... WITH NON-UNIQUE KEY",
			insert_text = "TYPE SORTED TABLE OF ${1:string} WITH NON-UNIQUE KEY ${2:table_line}$0",
		},
		{
			prefix = "type",
			label = "TYPE HASHED TABLE OF ... WITH UNIQUE KEY",
			insert_text = "TYPE HASHED TABLE OF ${1:string} WITH UNIQUE KEY ${2:table_line}$0",
		},
		{
			prefix = "type",
			label = "TYPE RANGE OF ...",
			insert_text = "TYPE RANGE OF ${1:sy-datum}$0",
		},
	}

	for test_case, i in cases {
		uri := strings.concatenate(
			{"file:///D:/repo/completion_type_addition_template_", fmt.tprintf("%d", i), ".abap"},
			context.temp_allocator,
		)
		source_prefix := "TYPES: ty_value "
		source := strings.concatenate({source_prefix, test_case.prefix, "."}, context.temp_allocator)
		state := lsp_test_state_with_open_document(uri, source)
		defer lsp_test_state_destroy(&state)

		offset := len(source_prefix) + len(test_case.prefix)
		params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
		snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
		testing.expect(t, snapshot_ok)
		if !snapshot_ok {
			continue
		}

		items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
		item, item_ok := lsp_test_find_completion_item(items, test_case.label)
		testing.expect(t, item_ok)
		if !item_ok {
			continue
		}

		testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
		testing.expect_value(
			t,
			item.sort_text,
			completion_sort_text("2", test_case.label, context.temp_allocator),
		)
		testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		testing.expect_value(t, item.insert_text, test_case.insert_text)
		edit, edit_ok := item.text_edit.?
		testing.expect(t, edit_ok)
		if edit_ok {
			testing.expect_value(t, edit.new_text, item.insert_text)
			testing.expect_value(t, edit.range.start.line, 0)
			testing.expect_value(t, edit.range.start.character, len(source_prefix))
			testing.expect_value(t, edit.range.end.line, 0)
			testing.expect_value(
				t,
				edit.range.end.character,
				len(source_prefix) + len(test_case.prefix),
			)

			applied := lsp_test_apply_text_edits(t, source, []Text_Edit{edit}, context.allocator)
			testing.expect(
				t,
				strings.has_suffix(
					applied,
					strings.concatenate({test_case.insert_text, "."}, context.temp_allocator),
				),
			)
		}
	}
}

@(test)
lsp_completion_type_addition_templates_expand_in_later_types_clause :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_later_type_addition_template.abap"
	source := `TYPES: ty_first TYPE string,
       ty_second typ.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "typ.") + len("typ")
	testing.expect(t, offset >= len("typ"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "TYPE TABLE OF ...")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text, "TYPE TABLE OF ${1:string}$0")
	edit, edit_ok := item.text_edit.?
	testing.expect(t, edit_ok)
	if edit_ok {
		testing.expect_value(t, edit.new_text, item.insert_text)
		testing.expect_value(t, edit.range.start.line, 1)
		testing.expect_value(t, edit.range.start.character, len("       ty_second "))
		testing.expect_value(t, edit.range.end.line, 1)
		testing.expect_value(t, edit.range.end.character, len("       ty_second typ"))
	}
}

@(test)
lsp_completion_type_addition_templates_require_types_clause_name :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_type_addition_without_clause_name.abap"
	source := "TYPES: type"
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
	_, item_ok := lsp_test_find_completion_item(items, "TYPE REF TO ...")
	testing.expect(t, !item_ok)
}

@(test)
lsp_completion_type_addition_templates_expand_in_declaration_and_parameter_clauses :: proc(
	t: ^testing.T,
) {
	cases := [?]Completion_Template_Source_Test_Case {
		{
			source = "DATA lv_value type.",
			label = "TYPE REF TO ...",
			insert_text = "TYPE REF TO ${1:object}$0",
		},
		{
			source = "DATA: lv_first TYPE string,\n      lv_second type.",
			label = "TYPE TABLE OF ...",
			insert_text = "TYPE TABLE OF ${1:string}$0",
		},
		{
			source = "CLASS-DATA gv_value type.",
			label = "TYPE STANDARD TABLE OF ... WITH EMPTY KEY",
			insert_text = "TYPE STANDARD TABLE OF ${1:string} WITH EMPTY KEY$0",
		},
		{
			source = "CONSTANTS gc_value type.",
			label = "TYPE c LENGTH",
			insert_text = "TYPE c LENGTH ${1:10}$0",
		},
		{
			source = "STATICS sv_value type.",
			label = "TYPE p LENGTH DECIMALS",
			insert_text = "TYPE p LENGTH ${1:8} DECIMALS ${2:2}$0",
		},
		{
			source = "FIELD-SYMBOLS <fs_value> type.",
			label = "TYPE ANY TABLE",
			insert_text = "TYPE ANY TABLE$0",
		},
		{
			source = "PARAMETERS p_value type.",
			label = "TYPE RANGE OF ...",
			insert_text = "TYPE RANGE OF ${1:sy-datum}$0",
		},
		{
			source = `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_value type.
ENDCLASS.`,
			label = "TYPE REF TO ...",
			insert_text = "TYPE REF TO ${1:object}$0",
		},
		{
			source = `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS get RETURNING VALUE(rv_value) type.
ENDCLASS.`,
			label = "TYPE STANDARD TABLE",
			insert_text = "TYPE STANDARD TABLE$0",
		},
		{
			source = `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run CHANGING REFERENCE(cv_value) type.
ENDCLASS.`,
			label = "TYPE LINE OF ...",
			insert_text = "TYPE LINE OF ${1:itab}$0",
		},
		{
			source = "FORM run USING iv_value type.\nENDFORM.",
			label = "TYPE SORTED TABLE OF ... WITH UNIQUE KEY",
			insert_text = "TYPE SORTED TABLE OF ${1:string} WITH UNIQUE KEY ${2:table_line}$0",
		},
		{
			source = "FUNCTION z_demo\n  IMPORTING VALUE(iv_value) type\nENDFUNCTION.",
			label = "TYPE HASHED TABLE OF ... WITH UNIQUE KEY",
			insert_text = "TYPE HASHED TABLE OF ${1:string} WITH UNIQUE KEY ${2:table_line}$0",
		},
	}

	for test_case, i in cases {
		uri := strings.concatenate(
			{"file:///D:/repo/completion_type_addition_context_", fmt.tprintf("%d", i), ".abap"},
			context.temp_allocator,
		)
		state := lsp_test_state_with_open_document(uri, test_case.source)
		defer lsp_test_state_destroy(&state)

		prefix_start := strings.index(test_case.source, "type")
		testing.expect(t, prefix_start >= 0)
		if prefix_start < 0 {
			continue
		}
		offset := prefix_start + len("type")
		params := lsp_test_rename_position_params(
			uri,
			offset_to_position(test_case.source, offset),
			"",
		)
		snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
		testing.expect(t, snapshot_ok)
		if !snapshot_ok {
			continue
		}

		items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
		item, item_ok := lsp_test_find_completion_item(items, test_case.label)
		testing.expect(t, item_ok)
		if !item_ok {
			continue
		}
		testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
		testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		testing.expect_value(t, item.insert_text, test_case.insert_text)
		edit, edit_ok := item.text_edit.?
		testing.expect(t, edit_ok)
		if edit_ok {
			expected := strings.concatenate(
				{
					test_case.source[:prefix_start],
					test_case.insert_text,
					test_case.source[offset:],
				},
				context.temp_allocator,
			)
			applied := lsp_test_apply_text_edits(
				t,
				test_case.source,
				[]Text_Edit{edit},
				context.allocator,
			)
			testing.expect_value(t, applied, expected)
		}
	}
}

@(test)
lsp_completion_type_addition_templates_skip_complex_definitions_in_oop_parameter_clauses :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_oop_parameter_type_addition.abap"
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get RETURNING VALUE(rt_rows) type.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	prefix_start := strings.index(source, "type")
	testing.expect(t, prefix_start >= 0)
	if prefix_start < 0 {
		return
	}
	offset := prefix_start + len("type")
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	_, generic_ok := lsp_test_find_completion_item(items, "TYPE STANDARD TABLE")
	testing.expect(t, generic_ok)

	complex_labels := [?]string {
		"TYPE TABLE OF ...",
		"TYPE STANDARD TABLE OF ... WITH EMPTY KEY",
		"TYPE STANDARD TABLE OF ... WITH DEFAULT KEY",
		"TYPE SORTED TABLE OF ... WITH UNIQUE KEY",
		"TYPE SORTED TABLE OF ... WITH NON-UNIQUE KEY",
		"TYPE HASHED TABLE OF ... WITH UNIQUE KEY",
		"TYPE RANGE OF ...",
	}
	for label in complex_labels {
		_, item_ok := lsp_test_find_completion_item(items, label)
		testing.expect(t, !item_ok)
	}
}

@(test)
lsp_completion_type_addition_templates_do_not_repeat_typed_declaration_clause :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_type_addition_repeated_decl.abap"
	source := "DATA lv_value TYPE string type."
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	prefix_start := strings.last_index(source, "type")
	testing.expect(t, prefix_start >= 0)
	if prefix_start < 0 {
		return
	}
	offset := prefix_start + len("type")
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	_, item_ok := lsp_test_find_completion_item(items, "TYPE REF TO ...")
	testing.expect(t, !item_ok)
}

@(test)
lsp_completion_dml_statement_templates_expand_from_keyword_prefixes :: proc(
	t: ^testing.T,
) {
	cases := [?]Completion_Template_Prefix_Test_Case {
		{
			prefix = "de",
			label = "DELETE ... INDEX",
			insert_text = "DELETE ${1:itab} INDEX ${2:lv_index}.$0",
		},
		{
			prefix = "de",
			label = "DELETE ... WHERE",
			insert_text = "DELETE ${1:itab} WHERE ${2:field} = ${3:lv_value}.$0",
		},
		{
			prefix = "de",
			label = "DELETE TABLE ... WITH TABLE KEY",
			insert_text = "DELETE TABLE ${1:itab} WITH TABLE KEY ${2:field} = ${3:lv_value}.$0",
		},
		{
			prefix = "de",
			label = "DELETE ADJACENT DUPLICATES ... COMPARING",
			insert_text = "DELETE ADJACENT DUPLICATES FROM ${1:itab} COMPARING ${2:field}.$0",
		},
		{
			prefix = "de",
			label = "DELETE FROM ... WHERE",
			insert_text = "DELETE FROM ${1:dbtab} WHERE ${2:field} = @${3:lv_value}.$0",
		},
		{
			prefix = "de",
			label = "DELETE ... FROM TABLE",
			insert_text = "DELETE ${1:dbtab} FROM TABLE ${2:itab}.$0",
		},
		{
			prefix = "mo",
			label = "MODIFY ... FROM",
			insert_text = "MODIFY ${1:itab} FROM ${2:wa}.$0",
		},
		{
			prefix = "mo",
			label = "MODIFY ... FROM ... INDEX",
			insert_text = "MODIFY ${1:itab} FROM ${2:wa} INDEX ${3:lv_index}.$0",
		},
		{
			prefix = "mo",
			label = "MODIFY TABLE ... FROM",
			insert_text = "MODIFY TABLE ${1:itab} FROM ${2:wa}.$0",
		},
		{
			prefix = "mo",
			label = "MODIFY ... FROM ... TRANSPORTING ... WHERE",
			insert_text = "MODIFY ${1:itab} FROM ${2:wa} TRANSPORTING ${3:field} WHERE ${4:key_field} = ${5:lv_key}.$0",
		},
		{
			prefix = "mo",
			label = "MODIFY ... FROM TABLE",
			insert_text = "MODIFY ${1:dbtab} FROM TABLE ${2:itab}.$0",
		},
		{
			prefix = "mo",
			label = "MODIFY ... FROM VALUE #( ... )",
			insert_text = "MODIFY ${1:dbtab} FROM VALUE #( ${2} ).$0",
		},
		{
			prefix = "mo",
			label = "MODIFY SCREEN",
			insert_text = "MODIFY SCREEN.$0",
		},
		{
			prefix = "mo",
			label = "MODIFY CURRENT LINE",
			insert_text = "MODIFY CURRENT LINE.$0",
		},
		{
			prefix = "mo",
			label = "MODIFY CURRENT LINE FIELD VALUE ... INTO",
			insert_text = "MODIFY CURRENT LINE FIELD VALUE ${1:field_name} INTO ${2:lv_value}.$0",
		},
		{
			prefix = "mo",
			label = "MODIFY LINE ... INDEX",
			insert_text = "MODIFY LINE ${1:lv_line} INDEX ${2:lv_index}.$0",
		},
		{
			prefix = "u",
			label = "UPDATE ... SET ... WHERE",
			insert_text = "UPDATE ${1:dbtab} SET ${2:field} = @${3:lv_value} WHERE ${4:key_field} = @${5:lv_key}.$0",
		},
		{
			prefix = "u",
			label = "UPDATE ... FROM",
			insert_text = "UPDATE ${1:dbtab} FROM ${2:wa}.$0",
		},
		{
			prefix = "u",
			label = "UPDATE ... FROM TABLE",
			insert_text = "UPDATE ${1:dbtab} FROM TABLE ${2:itab}.$0",
		},
	}

	for test_case, i in cases {
		uri := strings.concatenate(
			{"file:///D:/repo/completion_dml_template_", fmt.tprintf("%d", i), ".abap"},
			context.temp_allocator,
		)
		source := strings.concatenate({"REPORT zmain.\nFORM run.\n  ", test_case.prefix}, context.temp_allocator)
		state := lsp_test_state_with_open_document(uri, source)
		defer lsp_test_state_destroy(&state)

		offset := len(source)
		params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
		snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
		testing.expect(t, snapshot_ok)
		if !snapshot_ok {
			continue
		}

		items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
		item, item_ok := lsp_test_find_completion_item(items, test_case.label)
		testing.expect(t, item_ok)
		if !item_ok {
			continue
		}

		testing.expect_value(t, item.kind, COMPLETION_SNIPPET)
		testing.expect_value(
			t,
			item.sort_text,
			completion_sort_text("2", test_case.label, context.temp_allocator),
		)
		testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_SNIPPET)
		testing.expect_value(t, item.insert_text, test_case.insert_text)
		edit, edit_ok := item.text_edit.?
		testing.expect(t, edit_ok)
		if edit_ok {
			testing.expect_value(t, edit.new_text, item.insert_text)
			testing.expect_value(t, edit.range.start.line, 2)
			testing.expect_value(t, edit.range.start.character, 2)
			testing.expect_value(t, edit.range.end.line, 2)
			testing.expect_value(t, edit.range.end.character, 2 + len(test_case.prefix))
		}
	}
}

@(test)
lsp_completion_hyphenated_statement_templates_expand_from_hyphen_prefixes :: proc(
	t: ^testing.T,
) {
	field_uri := "file:///D:/repo/completion_field_symbol_template.abap"
	field_source := "REPORT zmain.\nFORM run.\n  field-sy"
	field_state := lsp_test_state_with_open_document(field_uri, field_source)
	defer lsp_test_state_destroy(&field_state)

	field_offset := len(field_source)
	field_params := lsp_test_rename_position_params(
		field_uri,
		offset_to_position(field_source, field_offset),
		"",
	)
	field_snapshot, field_completion_offset, field_snapshot_ok := snapshot_for_position(
		&field_state,
		field_params,
	)
	testing.expect(t, field_snapshot_ok)
	if field_snapshot_ok {
		field_items := completion_items_for_snapshot(
			field_snapshot,
			field_completion_offset,
			true,
			context.allocator,
		)
		field_item, field_item_ok := lsp_test_find_completion_item(
			field_items,
			"FIELD-SYMBOLS ... TYPE",
		)
		testing.expect(t, field_item_ok)
		if field_item_ok {
			testing.expect_value(
				t,
				field_item.insert_text,
				"FIELD-SYMBOLS <${1:fs}> TYPE ${2:any}.$0",
			)
			field_edit, field_edit_ok := field_item.text_edit.?
			testing.expect(t, field_edit_ok)
			if field_edit_ok {
				testing.expect_value(t, field_edit.new_text, field_item.insert_text)
				testing.expect_value(t, field_edit.range.start.line, 2)
				testing.expect_value(t, field_edit.range.start.character, 2)
				testing.expect_value(t, field_edit.range.end.line, 2)
				testing.expect_value(t, field_edit.range.end.character, 10)
			}
		}
	}

	move_uri := "file:///D:/repo/completion_move_corresponding_template.abap"
	move_source := "REPORT zmain.\nFORM run.\n  move-c"
	move_state := lsp_test_state_with_open_document(move_uri, move_source)
	defer lsp_test_state_destroy(&move_state)

	move_offset := len(move_source)
	move_params := lsp_test_rename_position_params(
		move_uri,
		offset_to_position(move_source, move_offset),
		"",
	)
	move_snapshot, move_completion_offset, move_snapshot_ok := snapshot_for_position(
		&move_state,
		move_params,
	)
	testing.expect(t, move_snapshot_ok)
	if move_snapshot_ok {
		move_items := completion_items_for_snapshot(
			move_snapshot,
			move_completion_offset,
			true,
			context.allocator,
		)
		move_item, move_item_ok := lsp_test_find_completion_item(
			move_items,
			"MOVE-CORRESPONDING ... TO",
		)
		testing.expect(t, move_item_ok)
		if move_item_ok {
			testing.expect_value(
				t,
				move_item.insert_text,
				"MOVE-CORRESPONDING ${1:source} TO ${2:target}.$0",
			)
			move_edit, move_edit_ok := move_item.text_edit.?
			testing.expect(t, move_edit_ok)
			if move_edit_ok {
				testing.expect_value(t, move_edit.new_text, move_item.insert_text)
				testing.expect_value(t, move_edit.range.start.line, 2)
				testing.expect_value(t, move_edit.range.start.character, 2)
				testing.expect_value(t, move_edit.range.end.line, 2)
				testing.expect_value(t, move_edit.range.end.character, 8)
			}
		}
	}
}

@(test)
lsp_completion_common_statement_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_common_template_plain.abap"
	source := "REPORT zmain.\nap"
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
	item, item_ok := lsp_test_find_completion_item(items, "APPEND VALUE #( ... ) TO")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(t, item.insert_text, "APPEND VALUE #( ) TO itab.")
}

@(test)
lsp_completion_method_definition_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_method_template_plain.abap"
	source := "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\n    class-m\nENDCLASS."
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "class-m") + len("class-m")
	testing.expect(t, offset >= len("class-m"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, false, context.allocator)
	item, item_ok := lsp_test_find_completion_item(
		items,
		"CLASS-METHODS ... IMPORTING RETURNING RAISING",
	)
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(
		t,
		item.insert_text,
		"CLASS-METHODS method_name\n      IMPORTING\n        !iv_value TYPE string\n      RETURNING\n        VALUE(rv_result) TYPE string\n      RAISING\n        cx_static_check.",
	)
}

@(test)
lsp_completion_interfaces_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_interfaces_template_plain.abap"
	source := "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\n    interfaces\nENDCLASS."
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "interfaces") + len("interfaces")
	testing.expect(t, offset >= len("interfaces"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, false, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "INTERFACES ...")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(t, item.insert_text, "INTERFACES lif_interface.")
}

@(test)
lsp_completion_aliases_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_aliases_template_plain.abap"
	source := "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\n    aliases\nENDCLASS."
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "aliases") + len("aliases")
	testing.expect(t, offset >= len("aliases"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, false, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "ALIASES ... FOR ...")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(t, item.insert_text, "ALIASES alias_name FOR lif_interface~member_name.")
}

@(test)
lsp_completion_begin_end_statement_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_begin_end_template_plain.abap"
	source := "REPORT zmain.\n  ty"
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
	item, item_ok := lsp_test_find_completion_item(items, "TYPES: BEGIN OF ... END OF")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(
		t,
		item.insert_text,
		"TYPES:\n    BEGIN OF ty_line,\n      field TYPE string,\n    END OF ty_line.",
	)
}

@(test)
lsp_completion_type_addition_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_type_addition_template_plain.abap"
	source := "TYPES: ty_ref type."
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "type.") + len("type")
	testing.expect(t, offset >= len("type"))
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, false, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "TYPE REF TO ...")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(t, item.insert_text, "TYPE REF TO object")
}

@(test)
lsp_completion_case_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_case_template_plain.abap"
	source := "REPORT zmain.\nca"
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
	item, item_ok := lsp_test_find_completion_item(items, "CASE ... WHEN ... WHEN OTHERS")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(
		t,
		item.insert_text,
		"CASE lv_value.\n  WHEN value_1.\n    \n  WHEN value_2.\n    \n  WHEN OTHERS.\n    \nENDCASE.",
	)
}

@(test)
lsp_completion_expression_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	cases := [?]Completion_Template_Prefix_Test_Case {
		{
			prefix = "fo",
			label = "FOR ... THEN ... WHILE",
			insert_text = "FOR index = 1 THEN index + 1 WHILE index <= limit ( index )",
		},
		{
			prefix = "re",
			label = "REDUCE ... FOR ... IN ... WHERE",
			insert_text = "REDUCE i( INIT result = 0 FOR row IN itab WHERE ( field = lv_value ) NEXT result = result + row-amount )",
		},
	}

	for test_case, i in cases {
		uri := strings.concatenate(
			{"file:///D:/repo/completion_expression_template_plain_", fmt.tprintf("%d", i), ".abap"},
			context.temp_allocator,
		)
		source := strings.concatenate({"DATA dummy TYPE i.\nWRITE ", test_case.prefix}, context.temp_allocator)
		state := lsp_test_state_with_open_document(uri, source)
		defer lsp_test_state_destroy(&state)

		offset := len(source)
		params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
		snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
		testing.expect(t, snapshot_ok)
		if !snapshot_ok {
			continue
		}

		items := completion_items_for_snapshot(snapshot, completion_offset, false, context.allocator)
		item, item_ok := lsp_test_find_completion_item(items, test_case.label)
		testing.expect(t, item_ok)
		if !item_ok {
			continue
		}

		testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
		testing.expect_value(t, item.insert_text, test_case.insert_text)
	}
}

@(test)
lsp_completion_fetch_cursor_table_expr_fields :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_cursor_table_expr_field.abap"
	source := `TYPES: BEGIN OF e070,
         trstatus TYPE string,
       END OF e070.

OPEN CURSOR WITH HOLD @DATA(lv_cursor) FOR
  SELECT trstatus
    FROM e070
    WHERE trstatus = '1'.

DO.
  FETCH NEXT CURSOR @lv_cursor
    INTO TABLE @DATA(lt_package)
    PACKAGE SIZE 100.

  lt_package[ 1 ]-.
ENDDO.

CLOSE CURSOR @lv_cursor.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	offset := strings.index(source, "lt_package[ 1 ]-.") + len("lt_package[ 1 ]-")
	params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
	snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
	testing.expect(t, snapshot_ok)
	if !snapshot_ok {
		return
	}

	items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
	item, item_ok := lsp_test_find_completion_item(items, "trstatus")
	testing.expect(t, item_ok)
	if item_ok {
		testing.expect_value(t, item.kind, COMPLETION_FIELD)
	}
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
lsp_completion_new_statement_templates_fall_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	try_uri := "file:///D:/repo/completion_try_template_plain.abap"
	try_source := "REPORT zmain.\ntr"
	try_state := lsp_test_state_with_open_document(try_uri, try_source)
	defer lsp_test_state_destroy(&try_state)

	try_offset := len(try_source)
	try_params := lsp_test_rename_position_params(
		try_uri,
		offset_to_position(try_source, try_offset),
		"",
	)
	try_snapshot, try_completion_offset, try_snapshot_ok := snapshot_for_position(
		&try_state,
		try_params,
	)
	testing.expect(t, try_snapshot_ok)
	if !try_snapshot_ok {
		return
	}

	try_items := completion_items_for_snapshot(
		try_snapshot,
		try_completion_offset,
		false,
		context.allocator,
	)
	try_item, try_item_ok := lsp_test_find_completion_item(
		try_items,
		"TRY ... CATCH ... ENDTRY",
	)
	testing.expect(t, try_item_ok)
	if !try_item_ok {
		return
	}

	testing.expect_value(t, try_item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(
		t,
		try_item.insert_text,
		"TRY.\n  \nCATCH cx_root INTO DATA(lx_error).\n  \nENDTRY.",
	)

	read_uri := "file:///D:/repo/completion_read_table_template_plain.abap"
	read_source := "REPORT zmain.\nre"
	read_state := lsp_test_state_with_open_document(read_uri, read_source)
	defer lsp_test_state_destroy(&read_state)

	read_offset := len(read_source)
	read_params := lsp_test_rename_position_params(
		read_uri,
		offset_to_position(read_source, read_offset),
		"",
	)
	read_snapshot, read_completion_offset, read_snapshot_ok := snapshot_for_position(
		&read_state,
		read_params,
	)
	testing.expect(t, read_snapshot_ok)
	if !read_snapshot_ok {
		return
	}

	read_items := completion_items_for_snapshot(
		read_snapshot,
		read_completion_offset,
		false,
		context.allocator,
	)
	read_item, read_item_ok := lsp_test_find_completion_item(
		read_items,
		"READ TABLE ... INDEX ... INTO",
	)
	testing.expect(t, read_item_ok)
	if !read_item_ok {
		return
	}

	testing.expect_value(t, read_item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(
		t,
		read_item.insert_text,
		"READ TABLE itab INDEX lv_index INTO DATA(ls_row).",
	)
}

@(test)
lsp_completion_select_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_select_template_plain.abap"
	source := "REPORT zmain.\nse"
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
	item, item_ok := lsp_test_find_completion_item(items, "SELECT ... WHERE")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(
		t,
		item.insert_text,
		`SELECT fields
  FROM table
  INTO TABLE @DATA(lt_rows)
  WHERE field = @lv_value.`,
	)
}

@(test)
lsp_completion_class_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_class_template_plain.abap"
	source := "cl"
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
	item, item_ok := lsp_test_find_completion_item(
		items,
		"CLASS ... DEFINITION / IMPLEMENTATION",
	)
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(
		t,
		item.insert_text,
		"CLASS lcl_class DEFINITION.\n  PUBLIC SECTION.\n    \nENDCLASS.\n\nCLASS lcl_class IMPLEMENTATION.\nENDCLASS.",
	)
}

@(test)
lsp_completion_interface_template_falls_back_to_plain_text_without_snippet_support :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/completion_interface_template_plain.abap"
	source := "int"
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
	item, item_ok := lsp_test_find_completion_item(items, "INTERFACE ... ENDINTERFACE")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(t, item.insert_text, "INTERFACE lif_interface.\n  \nENDINTERFACE.")
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
	item, item_ok := lsp_test_find_completion_item(items, "IF sy-subrc = 0")
	testing.expect(t, item_ok)
	if !item_ok {
		return
	}

	testing.expect_value(t, item.insert_text_format, COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT)
	testing.expect_value(t, item.insert_text, "IF sy-subrc = 0.\n  \nENDIF.")
	not_initial, not_initial_ok := lsp_test_find_completion_item(items, "IF ... IS NOT INITIAL")
	testing.expect(t, not_initial_ok)
	if not_initial_ok {
		testing.expect_value(
			t,
			not_initial.insert_text,
			"IF lv_value IS NOT INITIAL.\n  \nENDIF.",
		)
	}
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
	template_index := lsp_test_completion_item_index(items, "IF sy-subrc = 0")
	testing.expect(t, symbol_index >= 0)
	testing.expect(t, template_index >= 0)
	if symbol_index < 0 || template_index < 0 {
		return
	}

	testing.expect(t, symbol_index < template_index)
	testing.expect_value(t, items[symbol_index].sort_text, "1:if_candidate")
	testing.expect_value(t, items[template_index].sort_text, "2:if sy-subrc = 0")
}

@(test)
lsp_completion_class_templates_sort_after_matching_symbols :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_class_template_priority.abap"
	source := `DATA class_candidate TYPE i.
cla`
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
	symbol_index := lsp_test_completion_item_index(items, "class_candidate")
	template_index := lsp_test_completion_item_index(
		items,
		"CLASS ... DEFINITION / IMPLEMENTATION",
	)
	testing.expect(t, symbol_index >= 0)
	testing.expect(t, template_index >= 0)
	if symbol_index < 0 || template_index < 0 {
		return
	}

	testing.expect(t, symbol_index < template_index)
	testing.expect_value(t, items[symbol_index].sort_text, "1:class_candidate")
	testing.expect_value(
		t,
		items[template_index].sort_text,
		"2:class ... definition / implementation",
	)
}

@(test)
lsp_completion_select_templates_sort_after_matching_symbols :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_select_template_priority.abap"
	source := `DATA select_candidate TYPE i.
se`
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
	symbol_index := lsp_test_completion_item_index(items, "select_candidate")
	template_index := lsp_test_completion_item_index(items, "SELECT ... WHERE")
	testing.expect(t, symbol_index >= 0)
	testing.expect(t, template_index >= 0)
	if symbol_index < 0 || template_index < 0 {
		return
	}

	testing.expect(t, symbol_index < template_index)
	testing.expect_value(t, items[symbol_index].sort_text, "1:select_candidate")
	testing.expect_value(t, items[template_index].sort_text, "2:select ... where")
}

@(test)
lsp_completion_new_statement_templates_sort_after_matching_symbols :: proc(t: ^testing.T) {
	read_uri := "file:///D:/repo/completion_read_table_template_priority.abap"
	read_source := `DATA read_candidate TYPE i.
re`
	read_state := lsp_test_state_with_open_document(read_uri, read_source)
	defer lsp_test_state_destroy(&read_state)

	read_offset := len(read_source)
	read_params := lsp_test_rename_position_params(
		read_uri,
		offset_to_position(read_source, read_offset),
		"",
	)
	read_snapshot, read_completion_offset, read_snapshot_ok := snapshot_for_position(
		&read_state,
		read_params,
	)
	testing.expect(t, read_snapshot_ok)
	if !read_snapshot_ok {
		return
	}

	read_items := completion_items_for_snapshot(
		read_snapshot,
		read_completion_offset,
		true,
		context.allocator,
	)
	read_symbol_index := lsp_test_completion_item_index(read_items, "read_candidate")
	read_template_index := lsp_test_completion_item_index(
		read_items,
		"READ TABLE ... INDEX ... INTO",
	)
	testing.expect(t, read_symbol_index >= 0)
	testing.expect(t, read_template_index >= 0)
	if read_symbol_index < 0 || read_template_index < 0 {
		return
	}

	testing.expect(t, read_symbol_index < read_template_index)
	testing.expect_value(t, read_items[read_symbol_index].sort_text, "1:read_candidate")
	testing.expect_value(t, read_items[read_template_index].sort_text, "2:read table ... index ... into")

	commit_uri := "file:///D:/repo/completion_commit_template_priority.abap"
	commit_source := `DATA co_candidate TYPE i.
co`
	commit_state := lsp_test_state_with_open_document(commit_uri, commit_source)
	defer lsp_test_state_destroy(&commit_state)

	commit_offset := len(commit_source)
	commit_params := lsp_test_rename_position_params(
		commit_uri,
		offset_to_position(commit_source, commit_offset),
		"",
	)
	commit_snapshot, commit_completion_offset, commit_snapshot_ok := snapshot_for_position(
		&commit_state,
		commit_params,
	)
	testing.expect(t, commit_snapshot_ok)
	if !commit_snapshot_ok {
		return
	}

	commit_items := completion_items_for_snapshot(
		commit_snapshot,
		commit_completion_offset,
		true,
		context.allocator,
	)
	commit_symbol_index := lsp_test_completion_item_index(commit_items, "co_candidate")
	commit_template_index := lsp_test_completion_item_index(commit_items, "COMMIT WORK")
	continue_template_index := lsp_test_completion_item_index(commit_items, "CONTINUE")
	testing.expect(t, commit_symbol_index >= 0)
	testing.expect(t, commit_template_index >= 0)
	testing.expect(t, continue_template_index >= 0)
	if commit_symbol_index < 0 || commit_template_index < 0 || continue_template_index < 0 {
		return
	}

	testing.expect(t, commit_symbol_index < commit_template_index)
	testing.expect(t, commit_symbol_index < continue_template_index)
	testing.expect_value(t, commit_items[commit_symbol_index].sort_text, "1:co_candidate")
	testing.expect_value(t, commit_items[commit_template_index].sort_text, "2:commit work")
	testing.expect_value(t, commit_items[continue_template_index].sort_text, "2:continue")
}

@(test)
lsp_completion_case_and_expression_templates_sort_after_matching_symbols :: proc(t: ^testing.T) {
	cases := [?]Completion_Template_Priority_Test_Case {
		{
			source = "DATA case_candidate TYPE i.\nca",
			symbol_label = "case_candidate",
			template_label = "CASE ... WHEN ... WHEN OTHERS",
			symbol_sort = "1:case_candidate",
			template_sort = "2:case ... when ... when others",
		},
		{
			source = "DATA cond_candidate TYPE i.\nWRITE con",
			symbol_label = "cond_candidate",
			template_label = "COND #( WHEN ... THEN ... ELSE ... )",
			symbol_sort = "1:cond_candidate",
			template_sort = "2:cond #( when ... then ... else ... )",
		},
		{
			source = "DATA condense_candidate TYPE i.\nWRITE conde",
			symbol_label = "condense_candidate",
			template_label = "condense( val = ... )",
			symbol_sort = "1:condense_candidate",
			template_sort = "2:condense( val = ... )",
		},
		{
			source = "DATA find_candidate TYPE i.\nWRITE fin",
			symbol_label = "find_candidate",
			template_label = "find( val = ... sub = ... )",
			symbol_sort = "1:find_candidate",
			template_sort = "2:find( val = ... sub = ... )",
		},
		{
			source = "DATA filter_candidate TYPE i.\nWRITE fi",
			symbol_label = "filter_candidate",
			template_label = "FILTER #( ... WHERE ... )",
			symbol_sort = "1:filter_candidate",
			template_sort = "2:filter #( ... where ... )",
		},
		{
			source = "DATA reduce_candidate TYPE i.\nWRITE re",
			symbol_label = "reduce_candidate",
			template_label = "REDUCE ... FOR ... IN",
			symbol_sort = "1:reduce_candidate",
			template_sort = "2:reduce ... for ... in",
		},
		{
			source = "DATA for_candidate TYPE i.\nWRITE fo",
			symbol_label = "for_candidate",
			template_label = "FOR ... IN",
			symbol_sort = "1:for_candidate",
			template_sort = "2:for ... in",
		},
		{
			source = "DATA new_candidate TYPE i.\nWRITE ne",
			symbol_label = "new_candidate",
			template_label = "NEW #( ... )",
			symbol_sort = "1:new_candidate",
			template_sort = "2:new #( ... )",
		},
		{
			source = "DATA new_candidate TYPE i.\nWRITE ne",
			symbol_label = "new_candidate",
			template_label = "NEW ...",
			symbol_sort = "1:new_candidate",
			template_sort = "2:new ...",
		},
	}

	for test_case, i in cases {
		uri := strings.concatenate(
			{"file:///D:/repo/completion_new_template_priority_", fmt.tprintf("%d", i), ".abap"},
			context.temp_allocator,
		)
		state := lsp_test_state_with_open_document(uri, test_case.source)
		defer lsp_test_state_destroy(&state)

		offset := len(test_case.source)
		params := lsp_test_rename_position_params(uri, offset_to_position(test_case.source, offset), "")
		snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
		testing.expect(t, snapshot_ok)
		if !snapshot_ok {
			continue
		}

		items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
		symbol_index := lsp_test_completion_item_index(items, test_case.symbol_label)
		template_index := lsp_test_completion_item_index(items, test_case.template_label)
		testing.expect(t, symbol_index >= 0)
		testing.expect(t, template_index >= 0)
		if symbol_index < 0 || template_index < 0 {
			continue
		}

		testing.expect(t, symbol_index < template_index)
		testing.expect_value(t, items[symbol_index].sort_text, test_case.symbol_sort)
		testing.expect_value(t, items[template_index].sort_text, test_case.template_sort)
	}
}

@(test)
lsp_completion_common_statement_templates_sort_after_matching_symbols :: proc(t: ^testing.T) {
	cases := [?]Completion_Template_Priority_Test_Case {
		{
			source = "DATA message_candidate TYPE i.\nme",
			symbol_label = "message_candidate",
			template_label = "MESSAGE ... TYPE",
			symbol_sort = "1:message_candidate",
			template_sort = "2:message ... type",
		},
		{
			source = "DATA delete_candidate TYPE i.\ndel",
			symbol_label = "delete_candidate",
			template_label = "DELETE ... INDEX",
			symbol_sort = "1:delete_candidate",
			template_sort = "2:delete ... index",
		},
		{
			source = "DATA update_candidate TYPE i.\nup",
			symbol_label = "update_candidate",
			template_label = "UPDATE ... SET ... WHERE",
			symbol_sort = "1:update_candidate",
			template_sort = "2:update ... set ... where",
		},
		{
			source = "DATA condense_candidate TYPE i.\ncond",
			symbol_label = "condense_candidate",
			template_label = "CONDENSE ...",
			symbol_sort = "1:condense_candidate",
			template_sort = "2:condense ...",
		},
		{
			source = "DATA find_candidate TYPE i.\nfind",
			symbol_label = "find_candidate",
			template_label = "FIND ... IN",
			symbol_sort = "1:find_candidate",
			template_sort = "2:find ... in",
		},
		{
			source = "DATA types_candidate TYPE i.\nty",
			symbol_label = "types_candidate",
			template_label = "TYPES: BEGIN OF ... END OF",
			symbol_sort = "1:types_candidate",
			template_sort = "2:types: begin of ... end of",
		},
		{
			source = "DATA type_candidate TYPE i.\ntype",
			symbol_label = "type_candidate",
			template_label = "TYPE-POOLS ...",
			symbol_sort = "1:type_candidate",
			template_sort = "2:type-pools ...",
		},
		{
			source = "DATA data_candidate TYPE i.\nda",
			symbol_label = "data_candidate",
			template_label = "DATA: BEGIN OF ... END OF",
			symbol_sort = "1:data_candidate",
			template_sort = "2:data: begin of ... end of",
		},
		{
			source = "DATA selection_candidate TYPE i.\nse",
			symbol_label = "selection_candidate",
			template_label = "SELECTION-SCREEN BEGIN OF BLOCK ... END OF BLOCK",
			symbol_sort = "1:selection_candidate",
			template_sort = "2:selection-screen begin of block ... end of block",
		},
		{
			source = "DATA methods_candidate TYPE i.\nmeth",
			symbol_label = "methods_candidate",
			template_label = "METHODS ...",
			symbol_sort = "1:methods_candidate",
			template_sort = "2:methods ...",
		},
		{
			source = "DATA class_methods_candidate TYPE i.\nclass",
			symbol_label = "class_methods_candidate",
			template_label = "CLASS-METHODS ...",
			symbol_sort = "1:class_methods_candidate",
			template_sort = "2:class-methods ...",
		},
	}

	for test_case, i in cases {
		uri := strings.concatenate(
			{"file:///D:/repo/completion_common_template_priority_", fmt.tprintf("%d", i), ".abap"},
			context.temp_allocator,
		)
		state := lsp_test_state_with_open_document(uri, test_case.source)
		defer lsp_test_state_destroy(&state)

		offset := len(test_case.source)
		params := lsp_test_rename_position_params(uri, offset_to_position(test_case.source, offset), "")
		snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
		testing.expect(t, snapshot_ok)
		if !snapshot_ok {
			continue
		}

		items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
		symbol_index := lsp_test_completion_item_index(items, test_case.symbol_label)
		template_index := lsp_test_completion_item_index(items, test_case.template_label)
		testing.expect(t, symbol_index >= 0)
		testing.expect(t, template_index >= 0)
		if symbol_index < 0 || template_index < 0 {
			continue
		}

		testing.expect(t, symbol_index < template_index)
		testing.expect_value(t, items[symbol_index].sort_text, test_case.symbol_sort)
		testing.expect_value(t, items[template_index].sort_text, test_case.template_sort)
	}
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
lsp_completion_select_templates_do_not_match_expression_prefixes :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_select_template_expression.abap"
	source := `DATA select_value TYPE i.
WRITE se`
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
	_, item_ok := lsp_test_find_completion_item(items, "SELECT ... WHERE")

	testing.expect(t, !item_ok)
}

@(test)
lsp_completion_new_statement_templates_do_not_match_expression_prefixes :: proc(t: ^testing.T) {
	try_uri := "file:///D:/repo/completion_try_template_expression.abap"
	try_source := `DATA try_value TYPE i.
WRITE tr`
	try_state := lsp_test_state_with_open_document(try_uri, try_source)
	defer lsp_test_state_destroy(&try_state)

	try_offset := len(try_source)
	try_params := lsp_test_rename_position_params(
		try_uri,
		offset_to_position(try_source, try_offset),
		"",
	)
	try_snapshot, try_completion_offset, try_snapshot_ok := snapshot_for_position(
		&try_state,
		try_params,
	)
	testing.expect(t, try_snapshot_ok)
	if !try_snapshot_ok {
		return
	}

	try_items := completion_items_for_snapshot(
		try_snapshot,
		try_completion_offset,
		true,
		context.allocator,
	)
	_, try_item_ok := lsp_test_find_completion_item(try_items, "TRY ... CATCH ... ENDTRY")
	testing.expect(t, !try_item_ok)

	commit_uri := "file:///D:/repo/completion_commit_template_expression.abap"
	commit_source := `DATA commit_value TYPE i.
WRITE co`
	commit_state := lsp_test_state_with_open_document(commit_uri, commit_source)
	defer lsp_test_state_destroy(&commit_state)

	commit_offset := len(commit_source)
	commit_params := lsp_test_rename_position_params(
		commit_uri,
		offset_to_position(commit_source, commit_offset),
		"",
	)
	commit_snapshot, commit_completion_offset, commit_snapshot_ok := snapshot_for_position(
		&commit_state,
		commit_params,
	)
	testing.expect(t, commit_snapshot_ok)
	if !commit_snapshot_ok {
		return
	}

	commit_items := completion_items_for_snapshot(
		commit_snapshot,
		commit_completion_offset,
		true,
		context.allocator,
	)
	_, commit_ok := lsp_test_find_completion_item(commit_items, "COMMIT WORK")
	_, continue_ok := lsp_test_find_completion_item(commit_items, "CONTINUE")
	testing.expect(t, !commit_ok)
	testing.expect(t, !continue_ok)

	read_uri := "file:///D:/repo/completion_read_table_template_expression.abap"
	read_source := `DATA read_value TYPE i.
WRITE re`
	read_state := lsp_test_state_with_open_document(read_uri, read_source)
	defer lsp_test_state_destroy(&read_state)

	read_offset := len(read_source)
	read_params := lsp_test_rename_position_params(
		read_uri,
		offset_to_position(read_source, read_offset),
		"",
	)
	read_snapshot, read_completion_offset, read_snapshot_ok := snapshot_for_position(
		&read_state,
		read_params,
	)
	testing.expect(t, read_snapshot_ok)
	if !read_snapshot_ok {
		return
	}

	read_items := completion_items_for_snapshot(
		read_snapshot,
		read_completion_offset,
		true,
		context.allocator,
	)
	_, read_ok := lsp_test_find_completion_item(
		read_items,
		"READ TABLE ... INDEX ... INTO",
	)
	testing.expect(t, !read_ok)
}

@(test)
lsp_completion_case_template_does_not_match_expression_prefixes :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_case_template_expression.abap"
	source := `DATA case_value TYPE i.
WRITE ca`
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
	_, item_ok := lsp_test_find_completion_item(items, "CASE ... WHEN ... WHEN OTHERS")

	testing.expect(t, !item_ok)
}

@(test)
lsp_completion_common_statement_templates_do_not_match_expression_prefixes :: proc(t: ^testing.T) {
	cases := [?]Completion_Template_Prefix_Test_Case {
		{prefix = "me", label = "MESSAGE ... TYPE"},
		{prefix = "me", label = "METHODS ..."},
		{prefix = "cond", label = "CONDENSE ..."},
		{prefix = "find", label = "FIND ... IN"},
		{prefix = "type", label = "TYPE-POOLS ..."},
		{prefix = "type", label = "TYPES ... TYPE"},
		{prefix = "type", label = "TYPE REF TO ..."},
	}

	for test_case, i in cases {
		uri := strings.concatenate(
			{"file:///D:/repo/completion_common_template_expression_", fmt.tprintf("%d", i), ".abap"},
			context.temp_allocator,
		)
		source := strings.concatenate(
			{"DATA message_value TYPE i.\nWRITE ", test_case.prefix},
			context.temp_allocator,
		)
		state := lsp_test_state_with_open_document(uri, source)
		defer lsp_test_state_destroy(&state)

		offset := len(source)
		params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
		snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
		testing.expect(t, snapshot_ok)
		if !snapshot_ok {
			continue
		}

		items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
		_, item_ok := lsp_test_find_completion_item(items, test_case.label)
		testing.expect(t, !item_ok)
	}
}

@(test)
lsp_completion_begin_end_statement_templates_do_not_match_expression_prefixes :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_begin_end_template_expression.abap"
	source := `DATA data_value TYPE i.
WRITE da`
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
	_, data_ok := lsp_test_find_completion_item(items, "DATA: BEGIN OF ... END OF")
	_, common_part_ok := lsp_test_find_completion_item(
		items,
		"DATA: BEGIN OF COMMON PART ... END OF COMMON PART",
	)

	testing.expect(t, !data_ok)
	testing.expect(t, !common_part_ok)
}

@(test)
lsp_completion_expression_templates_do_not_match_selector_prefixes :: proc(t: ^testing.T) {
	cases := [?]Completion_Template_Prefix_Test_Case {
		{
			prefix = "cond",
			label = "COND #( WHEN ... THEN ... ELSE ... )",
			insert_text = "",
		},
		{
			prefix = "conde",
			label = "condense( val = ... )",
			insert_text = "",
		},
		{
			prefix = "find",
			label = "find( val = ... sub = ... )",
			insert_text = "",
		},
		{
			prefix = "fi",
			label = "FILTER #( ... WHERE ... )",
			insert_text = "",
		},
		{
			prefix = "re",
			label = "REDUCE ... FOR ... IN",
			insert_text = "",
		},
		{
			prefix = "re",
			label = "REDUCE ... FOR ... IN ... WHERE",
			insert_text = "",
		},
		{
			prefix = "fo",
			label = "FOR ... IN",
			insert_text = "",
		},
		{
			prefix = "ne",
			label = "NEW #( ... )",
			insert_text = "",
		},
		{
			prefix = "ne",
			label = "NEW ...",
			insert_text = "",
		},
	}

	for test_case, i in cases {
		uri := strings.concatenate(
			{"file:///D:/repo/completion_expression_selector_", fmt.tprintf("%d", i), ".abap"},
			context.temp_allocator,
		)
		source := strings.concatenate({"DATA dummy TYPE i.\ndummy->", test_case.prefix}, context.temp_allocator)
		state := lsp_test_state_with_open_document(uri, source)
		defer lsp_test_state_destroy(&state)

		offset := len(source)
		params := lsp_test_rename_position_params(uri, offset_to_position(source, offset), "")
		snapshot, completion_offset, snapshot_ok := snapshot_for_position(&state, params)
		testing.expect(t, snapshot_ok)
		if !snapshot_ok {
			continue
		}

		items := completion_items_for_snapshot(snapshot, completion_offset, true, context.allocator)
		_, item_ok := lsp_test_find_completion_item(items, test_case.label)
		testing.expect(t, !item_ok)
	}
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
	labels := [?]string {
		"IF ... ENDIF",
		"IF sy-subrc = 0",
		"IF sy-subrc <> 0",
		"IF ... IS INITIAL",
		"IF ... IS NOT INITIAL",
	}
	for label in labels {
		_, item_ok := lsp_test_find_completion_item(items, label)
		testing.expect(t, !item_ok)
	}
}

@(test)
lsp_completion_class_templates_do_not_match_expression_prefixes :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/completion_class_template_expression.abap"
	source := `DATA class_value TYPE i.
WRITE cla`
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
	_, item_ok := lsp_test_find_completion_item(
		items,
		"CLASS ... DEFINITION / IMPLEMENTATION",
	)

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
		clear_parse_diagnostics(&state)
		if state.parse_diagnostics.allocator.procedure != nil {
			delete(state.parse_diagnostics)
		}
		for _, &doc in state.documents {
			document_destroy(&doc, state.allocator)
		}
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
		clear_parse_diagnostics(&state)
		if state.parse_diagnostics.allocator.procedure != nil {
			delete(state.parse_diagnostics)
		}
		for _, &doc in state.documents {
			document_destroy(&doc, state.allocator)
		}
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
lsp_code_action_adds_missing_method_implementation_to_existing_block :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/code_action_missing_method_impl.abap"
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS do_something.
    METHODS method_name.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD do_something.
  ENDMETHOD.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	diagnostics := diagnostics_for_uri(&state, uri, context.allocator)
	method_diagnostic := Diagnostic{}
	method_diagnostic_found := false
	for diagnostic in diagnostics {
		if diagnostic.code == "Missing_Method_Implementation" &&
		   strings.contains(diagnostic.message, "method_name") {
			method_diagnostic = diagnostic
			method_diagnostic_found = true
			break
		}
	}
	testing.expect(t, method_diagnostic_found)
	if !method_diagnostic_found {
		return
	}

	params := lsp_test_code_action_params(uri, method_diagnostic.range)
	actions := code_actions_for_params(&state, params, context.allocator)

	testing.expect_value(t, len(actions), 1)
	if len(actions) != 1 {
		return
	}
	testing.expect_value(t, actions[0].kind, "quickfix")
	testing.expect(t, strings.contains(actions[0].title, "method_name"))
	edits, edits_ok := actions[0].edit.changes[uri]
	testing.expect(t, edits_ok)
	if !edits_ok {
		return
	}
	testing.expect_value(t, len(edits), 1)
	applied := lsp_test_apply_text_edits(t, source, edits, context.allocator)
	testing.expect(
		t,
		strings.contains(
			applied,
			"  METHOD method_name.\n  ENDMETHOD.\nENDCLASS.",
		),
	)
}

@(test)
lsp_code_action_adds_missing_interface_method_implementation_from_alias :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/code_action_missing_interface_method_impl.abap"
	source := `INTERFACE lif_interface.
  METHODS method_name.
ENDINTERFACE.

CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_interface.
    ALIASES short_name FOR lif_interface~method_name.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
ENDCLASS.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	diagnostics := diagnostics_for_uri(&state, uri, context.allocator)
	alias_diagnostic := Diagnostic{}
	alias_diagnostic_found := false
	for diagnostic in diagnostics {
		if diagnostic.code != "Missing_Method_Implementation" ||
		   !strings.contains(diagnostic.message, "lif_interface~method_name") {
			continue
		}
		start := position_to_offset(source, diagnostic.range.start)
		end := position_to_offset(source, diagnostic.range.end)
		if source[start:end] == "short_name" {
			alias_diagnostic = diagnostic
			alias_diagnostic_found = true
			break
		}
	}
	testing.expect(t, alias_diagnostic_found)
	if !alias_diagnostic_found {
		return
	}

	params := lsp_test_code_action_params(uri, alias_diagnostic.range)
	actions := code_actions_for_params(&state, params, context.allocator)

	testing.expect_value(t, len(actions), 1)
	if len(actions) != 1 {
		return
	}
	testing.expect_value(t, actions[0].kind, "quickfix")
	testing.expect(t, strings.contains(actions[0].title, "lif_interface~method_name"))
	edits, edits_ok := actions[0].edit.changes[uri]
	testing.expect(t, edits_ok)
	if !edits_ok {
		return
	}
	testing.expect_value(t, len(edits), 1)
	applied := lsp_test_apply_text_edits(t, source, edits, context.allocator)
	testing.expect(
		t,
		strings.contains(
			applied,
			"  METHOD lif_interface~method_name.\n  ENDMETHOD.\nENDCLASS.",
		),
	)
}

@(test)
lsp_code_action_fills_empty_value_constructor_with_target_structure_fields :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/code_action_value_constructor_fields.abap"
	source := `TYPES:
  BEGIN OF ty_line,
    field TYPE string,
    count TYPE i,
  END OF ty_line.
TYPES ty_table TYPE STANDARD TABLE OF ty_line WITH EMPTY KEY.
DATA lt_table TYPE ty_table.
APPEND VALUE #(  ) TO lt_table.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	value_offset := strings.index(source, "VALUE #(")
	testing.expect(t, value_offset >= 0)
	if value_offset < 0 {
		return
	}
	params := lsp_test_code_action_params(
		uri,
		range_from_offsets(source, value_offset, value_offset),
	)
	actions := code_actions_for_params(&state, params, context.allocator)

	action_found := false
	for action in actions {
		if action.title != "Fill VALUE with structure fields" {
			continue
		}
		action_found = true
		testing.expect_value(t, action.kind, "quickfix")
		edits, edits_ok := action.edit.changes[uri]
		testing.expect(t, edits_ok)
		if !edits_ok {
			return
		}
		testing.expect_value(t, len(edits), 1)
		applied := lsp_test_apply_text_edits(t, source, edits, context.allocator)
		testing.expect(
			t,
			strings.contains(
				applied,
				`APPEND VALUE #(
  field = VALUE #( )
  count = VALUE #( )
) TO lt_table.`,
			),
		)
	}
	testing.expect(t, action_found)
}

@(test)
lsp_code_action_fills_empty_value_constructor_with_range_fields :: proc(t: ^testing.T) {
	uri := "file:///D:/repo/code_action_value_constructor_range_fields.abap"
	source := `DATA lr_str TYPE RANGE OF string.
APPEND VALUE #(  ) TO lr_str.`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	value_offset := strings.index(source, "VALUE #(")
	testing.expect(t, value_offset >= 0)
	if value_offset < 0 {
		return
	}
	params := lsp_test_code_action_params(
		uri,
		range_from_offsets(source, value_offset, value_offset),
	)
	actions := code_actions_for_params(&state, params, context.allocator)

	action_found := false
	for action in actions {
		if action.title != "Fill VALUE with structure fields" {
			continue
		}
		action_found = true
		edits, edits_ok := action.edit.changes[uri]
		testing.expect(t, edits_ok)
		if !edits_ok {
			return
		}
		testing.expect_value(t, len(edits), 1)
		applied := lsp_test_apply_text_edits(t, source, edits, context.allocator)
		testing.expect(
			t,
			strings.contains(
				applied,
				`APPEND VALUE #(
  sign = VALUE #( )
  option = VALUE #( )
  low = VALUE #( )
  high = VALUE #( )
) TO lr_str.`,
			),
		)
	}
	testing.expect(t, action_found)
}

@(test)
lsp_code_action_fills_value_constructor_for_in_row_with_target_structure_fields :: proc(
	t: ^testing.T,
) {
	uri := "file:///D:/repo/code_action_value_constructor_for_in_fields.abap"
	source := `TYPES:
  BEGIN OF ty_line,
    docnum TYPE string,
    count TYPE i,
  END OF ty_line,
  tt_lines TYPE STANDARD TABLE OF ty_line WITH EMPTY KEY.

DATA lt_other_lines TYPE tt_lines.

DATA(lt_lines) = VALUE tt_lines(
  FOR ls_line IN lt_other_lines
  (  )
).`
	state := lsp_test_state_with_open_document(uri, source)
	defer lsp_test_state_destroy(&state)

	row_offset := strings.index(source, "(  )")
	testing.expect(t, row_offset >= 0)
	if row_offset < 0 {
		return
	}
	params := lsp_test_code_action_params(
		uri,
		range_from_offsets(source, row_offset + 1, row_offset + 1),
	)
	actions := code_actions_for_params(&state, params, context.allocator)

	action_found := false
	for action in actions {
		if action.title != "Fill VALUE with structure fields" {
			continue
		}
		action_found = true
		testing.expect_value(t, action.kind, "quickfix")
		edits, edits_ok := action.edit.changes[uri]
		testing.expect(t, edits_ok)
		if !edits_ok {
			return
		}
		testing.expect_value(t, len(edits), 1)
		applied := lsp_test_apply_text_edits(t, source, edits, context.allocator)
		testing.expect(
			t,
			strings.contains(
				applied,
				`DATA(lt_lines) = VALUE tt_lines(
  FOR ls_line IN lt_other_lines
  (
    docnum = VALUE #( )
    count = VALUE #( )
  )
).`,
			),
		)
	}
	testing.expect(t, action_found)
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
lsp_hover_reports_range_table_value_type :: proc(t: ^testing.T) {
	source := `DATA lr_str TYPE RANGE OF string.
APPEND VALUE #( ) TO lr_str.`

	text := lsp_test_hover_text(t, source, "APPEND VALUE #( ) TO lr_str", "lr_str")

	testing.expect(t, strings.contains(text, "`lr_str` variable"))
	testing.expect(t, strings.contains(text, "type: `RANGE OF string`"))
	testing.expect(t, !strings.contains(text, "RANGE OF range"))
}

@(test)
lsp_hover_reports_modify_transporting_field_types :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_nested,
         part TYPE string,
       END OF ty_nested.
TYPES: BEGIN OF ty_row,
         id TYPE string,
         nested TYPE ty_nested,
       END OF ty_row.

SELECT *
  FROM zmissing_rows
  INTO TABLE @DATA(lt_rows).

MODIFY lt_rows
  FROM VALUE ty_row(
    id = '1'
    nested = VALUE #( )
  )
  TRANSPORTING id nested-part.`

	id_text := lsp_test_hover_text(t, source, "TRANSPORTING id", "id")
	part_text := lsp_test_hover_text(t, source, "nested-part", "part")

	testing.expect(t, strings.contains(id_text, "`id` field"))
	testing.expect(t, strings.contains(id_text, "type: `string`"))
	testing.expect(t, strings.contains(part_text, "`part` field"))
	testing.expect(t, strings.contains(part_text, "type: `string`"))
}

@(test)
lsp_hover_reports_constant_type_and_value_as_abap_syntax :: proc(t: ^testing.T) {
	source := `CONSTANTS:
  BEGIN OF c_shipping_status,
    found_ok          TYPE c VALUE '1',
    not_found         TYPE c VALUE '2',
    fetch_started_aif TYPE c VALUE '3',
  END OF c_shipping_status,
  c_other_constant TYPE c VALUE 'A'.

DATA(lv_val) = c_shipping_status-fetch_started_aif.
DATA(lv_val1) = c_other_constant.`

	member_text := lsp_test_hover_text(
		t,
		source,
		"c_shipping_status-fetch_started_aif",
		"fetch_started_aif",
	)
	plain_text := lsp_test_hover_text(
		t,
		source,
		"DATA(lv_val1) = c_other_constant",
		"c_other_constant",
	)

	testing.expect(t, strings.contains(member_text, "```abap"))
	testing.expect(
		t,
		strings.contains(
			member_text,
			`CONSTANTS:
  BEGIN OF c_shipping_status,
    fetch_started_aif TYPE c VALUE '3',
  END OF c_shipping_status.`,
		),
	)
	testing.expect(t, strings.contains(plain_text, "```abap"))
	testing.expect(t, strings.contains(plain_text, "CONSTANTS c_other_constant TYPE c VALUE 'A'."))
}

@(test)
lsp_hover_reports_constant_group_values_as_abap_syntax :: proc(t: ^testing.T) {
	source := `CONSTANTS:
  BEGIN OF c_shipping_status,
    found_ok          TYPE c VALUE '1',
    not_found         TYPE c VALUE '2',
    fetch_started_aif TYPE c VALUE '3',
  END OF c_shipping_status.

DATA(lv_val) = c_shipping_status-fetch_started_aif.`

	text := lsp_test_hover_text(
		t,
		source,
		"c_shipping_status-fetch_started_aif",
		"c_shipping_status",
	)

	testing.expect(
		t,
		strings.contains(
			text,
			`CONSTANTS:
  BEGIN OF c_shipping_status,
    found_ok TYPE c VALUE '1',
    not_found TYPE c VALUE '2',
    fetch_started_aif TYPE c VALUE '3',
  END OF c_shipping_status.`,
		),
	)
}

@(test)
lsp_hover_reports_cursor_inline_handle_and_fetch_table_types :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF e070,
         trstatus TYPE string,
       END OF e070.

OPEN CURSOR WITH HOLD @DATA(lv_cursor) FOR
  SELECT trstatus
    FROM e070
    WHERE trstatus = '1'.

DO.
  FETCH NEXT CURSOR @lv_cursor
    INTO TABLE @DATA(lt_package)
    PACKAGE SIZE 100.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
ENDDO.

CLOSE CURSOR @lv_cursor.`

	cursor_text := lsp_test_hover_text(t, source, "@DATA(lv_cursor)", "lv_cursor")
	package_text := lsp_test_hover_text(t, source, "@DATA(lt_package)", "lt_package")

	testing.expect(t, strings.contains(cursor_text, "`lv_cursor` variable"))
	testing.expect(t, strings.contains(cursor_text, "type: `cursor`"))
	testing.expect(t, !strings.contains(cursor_text, "type: `unknown`"))
	testing.expect(t, strings.contains(package_text, "`lt_package` variable"))
	testing.expect(t, strings.contains(package_text, "type: `STANDARD TABLE OF"))
	testing.expect(t, !strings.contains(package_text, "STANDARD TABLE OF unknown"))
}

@(test)
lsp_hover_reports_catch_inline_exception_ref_type :: proc(t: ^testing.T) {
	source := `TRY.
  
CATCH cx_root INTO DATA(lx_error).
  
ENDTRY.`

	text := lsp_test_hover_text(t, source, "DATA(lx_error)", "lx_error")

	testing.expect(t, strings.contains(text, "`lx_error` variable"))
	testing.expect(t, strings.contains(text, "type: `ref to cx_root`"))
	testing.expect(t, !strings.contains(text, "type: `unknown`"))
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
	clear_parse_diagnostics(state)
	if state.parse_diagnostics.allocator.procedure != nil {
		delete(state.parse_diagnostics)
	}
	for _, &doc in state.documents {
		document_destroy(&doc, state.allocator)
	}
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

lsp_test_reset_temp_allocator :: proc() {
	temp := virtual.arena_temp_begin(cast(^virtual.Arena)context.temp_allocator.data)
	virtual.arena_temp_end(temp)
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

lsp_test_code_action_params :: proc(uri: string, range: Range) -> json.Object {
	params := make(json.Object, 3, context.allocator)
	text_document := make(json.Object, 1, context.allocator)
	text_document["uri"] = json.String(uri)
	params["textDocument"] = text_document

	range_object := make(json.Object, 2, context.allocator)
	start := make(json.Object, 2, context.allocator)
	start["line"] = json.Integer(range.start.line)
	start["character"] = json.Integer(range.start.character)
	range_object["start"] = start
	end := make(json.Object, 2, context.allocator)
	end["line"] = json.Integer(range.end.line)
	end["character"] = json.Integer(range.end.character)
	range_object["end"] = end
	params["range"] = range_object

	context_object := make(json.Object, 1, context.allocator)
	only := make(json.Array, 0, 1, context.allocator)
	append(&only, json.String("quickfix"))
	context_object["only"] = only
	params["context"] = context_object
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

lsp_test_did_change_params :: proc(uri, source: string, version: int) -> json.Object {
	params := make(json.Object, 2, context.allocator)
	text_document := make(json.Object, 2, context.allocator)
	text_document["uri"] = json.String(uri)
	text_document["version"] = json.Integer(version)
	params["textDocument"] = text_document

	changes := make(json.Array, 0, 1, context.allocator)
	change := make(json.Object, 1, context.allocator)
	change["text"] = json.String(source)
	append(&changes, change)
	params["contentChanges"] = changes
	return params
}

lsp_test_did_incremental_change_params :: proc(
	uri: string,
	range: Range,
	text: string,
	version: int,
) -> json.Object {
	params := make(json.Object, 2, context.allocator)
	text_document := make(json.Object, 2, context.allocator)
	text_document["uri"] = json.String(uri)
	text_document["version"] = json.Integer(version)
	params["textDocument"] = text_document

	changes := make(json.Array, 0, 1, context.allocator)
	change := make(json.Object, 2, context.allocator)
	range_object := make(json.Object, 2, context.allocator)
	start := make(json.Object, 2, context.allocator)
	start["line"] = json.Integer(range.start.line)
	start["character"] = json.Integer(range.start.character)
	range_object["start"] = start
	end := make(json.Object, 2, context.allocator)
	end["line"] = json.Integer(range.end.line)
	end["character"] = json.Integer(range.end.character)
	range_object["end"] = end
	change["range"] = range_object
	change["text"] = json.String(text)
	append(&changes, change)
	params["contentChanges"] = changes
	return params
}

lsp_test_did_save_params :: proc(uri, source: string) -> json.Object {
	params := make(json.Object, 2, context.allocator)
	text_document := make(json.Object, 1, context.allocator)
	text_document["uri"] = json.String(uri)
	params["textDocument"] = text_document
	params["text"] = json.String(source)
	return params
}

lsp_test_unresolved_count :: proc(
	analysis: ^semantic.Workspace_Analysis,
	kind: semantic.External_Candidate_Kind,
	name: string,
) -> int {
	count := 0
	for candidate in analysis.unresolved {
		if candidate.kind == kind && candidate.name == name {
			count += 1
		}
	}
	return count
}
