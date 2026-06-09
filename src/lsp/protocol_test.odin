package abap_frontend_lsp

import "src:parser"
import "src:semantic"

import "core:strings"
import "core:testing"

@(test)
file_uri_to_path_decodes_windows_paths :: proc(t: ^testing.T) {
	path, ok := file_uri_to_path("file:///D:/dev/rust/abap%20lsp/demo.abap", context.allocator)

	testing.expect(t, ok)
	testing.expect_value(t, path, `D:\dev\rust\abap lsp\demo.abap`)
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
