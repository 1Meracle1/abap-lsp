package abap_frontend_lsp

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
