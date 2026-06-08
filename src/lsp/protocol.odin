package abap_frontend_lsp

import json "core:encoding/json"
import "core:mem"
import "core:strings"
import "core:unicode/utf8"

JSON_RPC_VERSION :: "2.0"

METHOD_INITIALIZE :: "initialize"
METHOD_INITIALIZED :: "initialized"
METHOD_SHUTDOWN :: "shutdown"
METHOD_EXIT :: "exit"
METHOD_DID_OPEN :: "textDocument/didOpen"
METHOD_DID_CHANGE :: "textDocument/didChange"
METHOD_DID_CLOSE :: "textDocument/didClose"
METHOD_COMPLETION :: "textDocument/completion"
METHOD_HOVER :: "textDocument/hover"
METHOD_DEFINITION :: "textDocument/definition"
METHOD_REFERENCES :: "textDocument/references"
METHOD_SEMANTIC_TOKENS_FULL :: "textDocument/semanticTokens/full"
METHOD_FOLDING_RANGE :: "textDocument/foldingRange"
METHOD_PUBLISH_DIAGNOSTICS :: "textDocument/publishDiagnostics"

RPC_PARSE_ERROR :: -32700
RPC_INVALID_REQUEST :: -32600
RPC_METHOD_NOT_FOUND :: -32601
RPC_INVALID_PARAMS :: -32602
RPC_INTERNAL_ERROR :: -32603

TEXT_DOCUMENT_SYNC_FULL :: 1

DIAGNOSTIC_ERROR :: 1
DIAGNOSTIC_WARNING :: 2
DIAGNOSTIC_INFORMATION :: 3
DIAGNOSTIC_HINT :: 4

COMPLETION_METHOD :: 2
COMPLETION_FUNCTION :: 3
COMPLETION_FIELD :: 5
COMPLETION_VARIABLE :: 6
COMPLETION_CLASS :: 7
COMPLETION_INTERFACE :: 8
COMPLETION_MODULE :: 9
COMPLETION_PROPERTY :: 10
COMPLETION_ENUM_MEMBER :: 20
COMPLETION_CONSTANT :: 21
COMPLETION_STRUCT :: 22
COMPLETION_EVENT :: 23

Position :: struct {
	line:      int `json:"line"`,
	character: int `json:"character"`,
}

Range :: struct {
	start: Position `json:"start"`,
	end:   Position `json:"end"`,
}

Location :: struct {
	uri:   string `json:"uri"`,
	range: Range `json:"range"`,
}

Diagnostic :: struct {
	range:    Range `json:"range"`,
	severity: int `json:"severity"`,
	code:     string `json:"code"`,
	source:   string `json:"source"`,
	message:  string `json:"message"`,
}

Publish_Diagnostics_Params :: struct {
	uri:         string `json:"uri"`,
	diagnostics: []Diagnostic `json:"diagnostics"`,
}

Completion_Item :: struct {
	label: string `json:"label"`,
	kind:  int `json:"kind"`,
}

Completion_List :: struct {
	is_incomplete: bool `json:"isIncomplete"`,
	items:         []Completion_Item `json:"items"`,
}

Hover_Markup :: struct {
	kind:  string `json:"kind"`,
	value: string `json:"value"`,
}

Hover :: struct {
	contents: Hover_Markup `json:"contents"`,
	range:    Range `json:"range"`,
}

Semantic_Tokens :: struct {
	data: []u32 `json:"data"`,
}

Folding_Range :: struct {
	start_line:      int `json:"startLine"`,
	start_character: int `json:"startCharacter"`,
	end_line:        int `json:"endLine"`,
	end_character:   int `json:"endCharacter"`,
}

Text_Document_Position :: struct {
	uri:      string,
	position: Position,
	ok:       bool,
}

Text_Document_Identifier :: struct {
	uri: string,
}

Text_Document_Item :: struct {
	uri:         string,
	language_id: string,
	version:     int,
	text:        string,
}

Text_Document_Content_Change :: struct {
	text: string,
}

Rpc_Message :: struct {
	id:     json.Value,
	method: string,
	params: json.Value,
	has_id: bool,
	ok:     bool,
	error:  string,
}

position_to_offset :: proc(source: string, position: Position) -> int {
	if position.line <= 0 {
		return utf16_column_to_offset(source, 0, position.character)
	}
	line := 0
	start := 0
	for i in 0 ..< len(source) {
		if line == position.line {
			return utf16_column_to_offset(source, start, position.character)
		}
		if source[i] == '\n' {
			line += 1
			start = i + 1
		}
	}
	return utf16_column_to_offset(source, start, position.character)
}

offset_to_position :: proc(source: string, offset: int) -> Position {
	target := clamp(offset, 0, len(source))
	line := 0
	line_start := 0
	for i in 0 ..< target {
		if source[i] == '\n' {
			line += 1
			line_start = i + 1
		}
	}
	return Position{line = line, character = utf16_units(source[line_start:target])}
}

range_from_offsets :: proc(source: string, start, end: int) -> Range {
	s := clamp(start, 0, len(source))
	e := clamp(end, 0, len(source))
	if e < s {
		e = s
	}
	return Range{start = offset_to_position(source, s), end = offset_to_position(source, e)}
}

line_end_offset :: proc(source: string, line_start: int) -> int {
	for i := line_start; i < len(source); i += 1 {
		if source[i] == '\n' {
			if i > line_start && source[i - 1] == '\r' {
				return i - 1
			}
			return i
		}
	}
	return len(source)
}

utf16_column_to_offset :: proc(source: string, line_start, character: int) -> int {
	start := line_start
	if start < 0 {
		start = 0
	}
	if start > len(source) {
		start = len(source)
	}
	line_end := line_end_offset(source, start)
	if character <= 0 {
		return start
	}
	offset := start
	units := 0
	for offset < line_end && units < character {
		r, width := utf8.decode_rune_in_string(source[offset:])
		if width <= 0 {
			break
		}
		next_units := utf16_rune_units(r)
		if units + next_units > character {
			break
		}
		units += next_units
		offset += width
	}
	return offset
}

utf16_units :: proc(text: string) -> int {
	units := 0
	for offset := 0; offset < len(text); {
		r, width := utf8.decode_rune_in_string(text[offset:])
		if width <= 0 {
			break
		}
		units += utf16_rune_units(r)
		offset += width
	}
	return units
}

// LSP positions are UTF-16 code units, while parser positions are byte offsets.
// The core UTF-16 package handles encoding buffers; here we only need per-rune
// width while walking the original UTF-8 source.
utf16_rune_units :: #force_inline proc "contextless" (r: rune) -> int {
	return 2 if r >= 0x10000 else 1
}

json_object :: proc(value: json.Value) -> (json.Object, bool) {
	if object, ok := value.(json.Object); ok {
		return object, true
	}
	return nil, false
}

json_array :: proc(value: json.Value) -> (json.Array, bool) {
	if array, ok := value.(json.Array); ok {
		return array, true
	}
	return nil, false
}

object_string :: proc(object: json.Object, key: string) -> (string, bool) {
	if value, ok := object[key]; ok {
		return value.(json.String)
	}
	return "", false
}

object_integer :: proc(object: json.Object, key: string) -> (int, bool) {
	if value, ok := object[key]; ok {
		res, res_ok := value.(json.Integer)
		return cast(int)res, res_ok
	}
	return 0, false
}

object_object :: proc(object: json.Object, key: string) -> (json.Object, bool) {
	if value, ok := object[key]; ok {
		return json_object(value)
	}
	return nil, false
}

object_array :: proc(object: json.Object, key: string) -> (json.Array, bool) {
	if value, ok := object[key]; ok {
		return json_array(value)
	}
	return nil, false
}

text_document_position_from_params :: proc(params: json.Value) -> Text_Document_Position {
	object, ok := json_object(params)
	if !ok {
		return {}
	}
	text_document, text_ok := object_object(object, "textDocument")
	position_object, pos_ok := object_object(object, "position")
	if !text_ok || !pos_ok {
		return {}
	}
	uri, uri_ok := object_string(text_document, "uri")
	line, line_ok := object_integer(position_object, "line")
	character, char_ok := object_integer(position_object, "character")
	if !uri_ok || !line_ok || !char_ok {
		return {}
	}
	return Text_Document_Position {
		uri = normalize_lsp_uri(uri, context.allocator),
		position = Position{line = line, character = character},
		ok = true,
	}
}

normalize_lsp_uri :: proc(uri: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	for i := 0; i < len(uri); i += 1 {
		ch := uri[i]
		if ch == '\\' {
			ch = '/'
		}
		strings.write_byte(&out, ch)
	}
	return strings.to_string(out)
}
