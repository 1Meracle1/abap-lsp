package lsp

import "../lang/lexer"

position_to_offset :: proc(text: string, pos: Position) -> int {
	line := 0
	offset := 0
	len_text := len(text)

	// fast forward to line
	for line < int(pos.line) && offset < len_text {
		if text[offset] == '\n' {
			line += 1
		}
		offset += 1
	}

	if line != int(pos.line) {
		return -1
	}

	// add character offset
	// TODO: handle utf-16 conversion properly if needed
	target_offset := offset + int(pos.character)

	// ensure we don't cross line boundary or EOF
	for i := offset; i < target_offset && i < len_text; i += 1 {
		if text[i] == '\n' {
			return i // stop at newline
		}
	}

	if target_offset > len_text {
		return len_text
	}

	return target_offset
}

offset_to_position :: proc(text: string, offset: int) -> Position {
	line := 0
	col := 0
	for i := 0; i < offset && i < len(text); i += 1 {
		if text[i] == '\n' {
			line += 1
			col = 0
		} else {
			col += 1
		}
	}
	return Position{line = line, character = col}
}

text_range_to_lsp_range :: proc(text: string, range: lexer.TextRange) -> Range {
	return Range{
		start = offset_to_position(text, range.start),
		end   = offset_to_position(text, range.end),
	}
}