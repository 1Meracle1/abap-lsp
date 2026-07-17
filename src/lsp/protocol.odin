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
METHOD_DID_SAVE :: "textDocument/didSave"
METHOD_DID_CLOSE :: "textDocument/didClose"
METHOD_DID_CHANGE_WATCHED_FILES :: "workspace/didChangeWatchedFiles"
METHOD_DID_CHANGE_WORKSPACE_FOLDERS :: "workspace/didChangeWorkspaceFolders"
METHOD_DID_CREATE_FILES :: "workspace/didCreateFiles"
METHOD_DID_RENAME_FILES :: "workspace/didRenameFiles"
METHOD_DID_DELETE_FILES :: "workspace/didDeleteFiles"
METHOD_COMPLETION :: "textDocument/completion"
METHOD_HOVER :: "textDocument/hover"
METHOD_DEFINITION :: "textDocument/definition"
METHOD_IMPLEMENTATION :: "textDocument/implementation"
METHOD_REFERENCES :: "textDocument/references"
METHOD_PREPARE_RENAME :: "textDocument/prepareRename"
METHOD_RENAME :: "textDocument/rename"
METHOD_CODE_ACTION :: "textDocument/codeAction"
METHOD_SEMANTIC_TOKENS_FULL :: "textDocument/semanticTokens/full"
METHOD_FOLDING_RANGE :: "textDocument/foldingRange"
METHOD_DOCUMENT_SYMBOL :: "textDocument/documentSymbol"
METHOD_PUBLISH_DIAGNOSTICS :: "textDocument/publishDiagnostics"
METHOD_READ_DEPENDENCY_DOCUMENT :: "abapls/readDependencyDocument"

RPC_PARSE_ERROR :: -32700
RPC_INVALID_REQUEST :: -32600
RPC_METHOD_NOT_FOUND :: -32601
RPC_INVALID_PARAMS :: -32602
RPC_INTERNAL_ERROR :: -32603

TEXT_DOCUMENT_SYNC_FULL :: 1

FILE_CHANGE_CREATED :: 1
FILE_CHANGE_CHANGED :: 2
FILE_CHANGE_DELETED :: 3

DIAGNOSTIC_ERROR :: 1
DIAGNOSTIC_WARNING :: 2
DIAGNOSTIC_INFORMATION :: 3
DIAGNOSTIC_HINT :: 4
DIAGNOSTIC_TAG_UNNECESSARY :: 2

COMPLETION_METHOD :: 2
COMPLETION_FUNCTION :: 3
COMPLETION_FIELD :: 5
COMPLETION_VARIABLE :: 6
COMPLETION_CLASS :: 7
COMPLETION_INTERFACE :: 8
COMPLETION_MODULE :: 9
COMPLETION_PROPERTY :: 10
COMPLETION_SNIPPET :: 15
COMPLETION_ENUM_MEMBER :: 20
COMPLETION_CONSTANT :: 21
COMPLETION_STRUCT :: 22
COMPLETION_EVENT :: 23

COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT :: 1
COMPLETION_INSERT_TEXT_FORMAT_SNIPPET :: 2

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
	range:            Range `json:"range"`,
	severity:         int `json:"severity"`,
	code:             string `json:"code"`,
	code_description: Maybe(Diagnostic_Code_Description) `json:"codeDescription,omitempty"`,
	source:           string `json:"source"`,
	message:          string `json:"message"`,
	tags:             []int `json:"tags,omitempty"`,
	data:             Maybe(Diagnostic_Lint_Data) `json:"data,omitempty"`,
}

Diagnostic_Code_Description :: struct {
	href: string `json:"href"`,
}

Diagnostic_Lint_Suppression_Data :: struct {
	kind:  string `json:"kind"`,
	token: string `json:"token"`,
	range: []int `json:"range"`,
}

Diagnostic_Lint_Data :: struct {
	kind:        string `json:"kind"`,
	lint_id:     string `json:"lintId"`,
	level:       string `json:"level"`,
	group:       string `json:"group"`,
	origin:      string `json:"origin"`,
	suppressed:  bool   `json:"suppressed"`,
	suppression: Maybe(Diagnostic_Lint_Suppression_Data) `json:"suppression,omitempty"`,
}

Publish_Diagnostics_Params :: struct {
	uri:         string `json:"uri"`,
	version:     int `json:"version,omitempty"`,
	diagnostics: []Diagnostic `json:"diagnostics"`,
}

Completion_Item :: struct {
	label:              string `json:"label"`,
	kind:               int    `json:"kind"`,
	sort_text:          string `json:"sortText"`,
	insert_text:        string `json:"insertText"`,
	insert_text_format: int    `json:"insertTextFormat"`,
	filter_text:        Maybe(string) `json:"filterText,omitempty"`,
	text_edit:          Maybe(Text_Edit) `json:"textEdit,omitempty"`,
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

Read_Dependency_Document_Result :: struct {
	source_text: string `json:"sourceText"`
}

Text_Edit :: struct {
	range:    Range `json:"range"`,
	new_text: string `json:"newText"`,
}

Workspace_Edit :: struct {
	changes: map[string][]Text_Edit `json:"changes"`,
}

Code_Action :: struct {
	title:        string `json:"title"`,
	kind:         string `json:"kind"`,
	edit:         Workspace_Edit `json:"edit"`,
	is_preferred: bool `json:"isPreferred"`,
}

Prepare_Rename_Response :: struct {
	range:       Range `json:"range"`,
	placeholder: string `json:"placeholder"`,
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

Document_Symbol :: struct {
	name:            string `json:"name"`,
	detail:          string `json:"detail,omitempty"`,
	kind:            int `json:"kind"`,
	range:           Range `json:"range"`,
	selection_range: Range `json:"selectionRange"`,
	children:        []Document_Symbol `json:"children,omitempty"`,
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

object_string :: #force_inline proc "contextless" (object: json.Object, key: string) -> (string, bool) {
	if value, ok := object[key]; ok {
		return value.(json.String)
	}
	return "", false
}

object_integer :: #force_inline proc "contextless" (object: json.Object, key: string) -> (int, bool) {
	if value, ok := object[key]; ok {
		res, res_ok := value.(json.Integer)
		return cast(int)res, res_ok
	}
	return 0, false
}

object_boolean :: #force_inline proc "contextless" (object: json.Object, key: string) -> (bool, bool) {
	if value, ok := object[key]; ok {
		res, res_ok := value.(json.Boolean)
		return bool(res), res_ok
	}
	return false, false
}

object_object :: #force_inline proc "contextless" (object: json.Object, key: string) -> (json.Object, bool) {
	if value, ok := object[key]; ok {
		return value.(json.Object)
	}
	return nil, false
}

object_array :: #force_inline proc "contextless" (object: json.Object, key: string) -> (json.Array, bool) {
	if value, ok := object[key]; ok {
		return value.(json.Array)
	}
	return nil, false
}

text_document_position_from_params :: proc(params: json.Value) -> Text_Document_Position {
	object, ok := params.(json.Object)
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
