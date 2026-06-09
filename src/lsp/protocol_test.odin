package abap_frontend_lsp

import "src:parser"
import "src:semantic"
import workspace "src:workspace"

import json "core:encoding/json"
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
lsp_uri_matches_or_under_accepts_file_uris_and_paths :: proc(t: ^testing.T) {
	testing.expect(t, lsp_uri_matches_or_under(`D:\repo\pkg\zmain.abap`, "file:///D:/repo/pkg"))
	testing.expect(t, lsp_uri_matches_or_under("file:///D:/repo/pkg/zmain.abap", `D:\repo\pkg`))
	testing.expect(t, !lsp_uri_matches_or_under(`D:\repo\pkg2\zmain.abap`, "file:///D:/repo/pkg"))
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
