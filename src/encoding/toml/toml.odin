package abap_frontend_encoding_toml

import "core:mem"
import "core:strconv"
import "core:strings"

String :: string
Integer :: i64
Boolean :: bool
Array :: [dynamic]Value
Table_Data :: struct {
	entries: map[string]Value,
}
Table :: ^Table_Data

Value :: union {
	String,
	Integer,
	Boolean,
	Array,
	Table,
}

Parse_Error :: struct {
	message: string,
	offset:  int,
}

Parse_Result :: struct {
	root:   Table,
	errors: []Parse_Error,
}

Parser :: struct {
	source:    string,
	pos:       int,
	allocator: mem.Allocator,
	root:      Table,
	current:   Table,
	errors:    [dynamic]Parse_Error,
}

parse_string :: proc(source: string, allocator: mem.Allocator) -> Parse_Result {
	root := make_table(allocator)
	p := Parser {
		source = source,
		allocator = allocator,
		root = root,
		current = root,
		errors = make([dynamic]Parse_Error, 0, 2, allocator),
	}

	for {
		skip_ws_comments(&p)
		if at_end(&p) {
			break
		}
		if peek(&p) == '[' {
			parse_table_header(&p)
		} else {
			parse_key_value(&p, p.current)
		}
	}

	return Parse_Result{root = root, errors = p.errors[:]}
}

from_str :: parse_string

destroy_parse_result :: proc(result: Parse_Result, allocator: mem.Allocator) {
	destroy_table(result.root, allocator)
	delete(result.errors, allocator)
}

destroy_value :: proc(value: Value, allocator: mem.Allocator) {
	#partial switch v in value {
	case String:
		delete(v, allocator)
	case Array:
		for elem in v {
			destroy_value(elem, allocator)
		}
		delete(v)
	case Table:
		destroy_table(v, allocator)
	}
}

destroy_table :: proc(table: Table, allocator: mem.Allocator) {
	if table == nil {
		return
	}
	for key, value in table.entries {
		delete(key, allocator)
		destroy_value(value, allocator)
	}
	delete(table.entries)
	free(table, allocator)
}

table_get :: proc(table: Table, key: string) -> (Value, bool) {
	if table == nil {
		return nil, false
	}
	value, ok := table.entries[key]
	return value, ok
}

table_get_table :: proc(table: Table, key: string) -> (Table, bool) {
	value, ok := table_get(table, key)
	if !ok {
		return {}, false
	}
	#partial switch v in value {
	case Table:
		return v, true
	}
	return {}, false
}

table_get_array :: proc(table: Table, key: string) -> (Array, bool) {
	value, ok := table_get(table, key)
	if !ok {
		return {}, false
	}
	#partial switch v in value {
	case Array:
		return v, true
	}
	return {}, false
}

table_get_string :: proc(table: Table, key: string) -> (string, bool) {
	value, ok := table_get(table, key)
	if !ok {
		return "", false
	}
	#partial switch v in value {
	case String:
		return v, true
	}
	return "", false
}

table_get_int :: proc(table: Table, key: string) -> (i64, bool) {
	value, ok := table_get(table, key)
	if !ok {
		return 0, false
	}
	#partial switch v in value {
	case Integer:
		return v, true
	}
	return 0, false
}

table_get_bool :: proc(table: Table, key: string) -> (bool, bool) {
	value, ok := table_get(table, key)
	if !ok {
		return false, false
	}
	#partial switch v in value {
	case Boolean:
		return v, true
	}
	return false, false
}

array_get_table :: proc(array: Array, index: int) -> (Table, bool) {
	if index < 0 || index >= len(array) {
		return {}, false
	}
	#partial switch v in array[index] {
	case Table:
		return v, true
	}
	return {}, false
}

array_get_string :: proc(array: Array, index: int) -> (string, bool) {
	if index < 0 || index >= len(array) {
		return "", false
	}
	#partial switch v in array[index] {
	case String:
		return v, true
	}
	return "", false
}

encode_string :: proc(table: Table, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	first := true
	if table == nil {
		return strings.to_string(out)
	}
	for key, value in table.entries {
		if !first {
			strings.write_byte(&out, '\n')
		}
		first = false
		write_key(&out, key)
		strings.write_string(&out, " = ")
		write_value(&out, value)
		strings.write_byte(&out, '\n')
	}
	return strings.to_string(out)
}

parse_table_header :: proc(p: ^Parser) {
	expect_byte(p, '[')
	array_table := false
	if allow_byte(p, '[') {
		array_table = true
	}
	skip_inline_ws(p)
	path, ok := parse_key_path(p)
	defer delete(path)
	skip_inline_ws(p)
	if !expect_byte(p, ']') {
		skip_line(p)
		return
	}
	if array_table && !expect_byte(p, ']') {
		skip_line(p)
		return
	}
	if !ok || len(path) == 0 {
		add_error(p, "expected table name")
		return
	}
	if array_table {
		if table, table_ok := push_array_table(p, p.root, path[:]); table_ok {
			p.current = table
		}
	} else if table, table_ok := table_for_path(p, p.root, path[:]); table_ok {
		p.current = table
	}
}

parse_key_value :: proc(p: ^Parser, table: Table) {
	path, ok := parse_key_path(p)
	defer delete(path)
	if !ok || len(path) == 0 {
		add_error(p, "expected key")
		skip_line(p)
		return
	}
	skip_inline_ws(p)
	if !expect_byte(p, '=') {
		skip_line(p)
		return
	}
	value, value_ok := parse_value(p)
	if value_ok {
		insert_value(p, table, path[:], value)
	}
}

parse_key_path :: proc(p: ^Parser) -> ([dynamic]string, bool) {
	path := make([dynamic]string, 0, 4, p.allocator)
	for {
		skip_inline_ws(p)
		part, ok := parse_key_part(p)
		if !ok {
			return path, false
		}
		append(&path, part)
		skip_inline_ws(p)
		if !allow_byte(p, '.') {
			break
		}
	}
	return path, true
}

parse_key_part :: proc(p: ^Parser) -> (string, bool) {
	if at_end(p) {
		return "", false
	}
	if peek(p) == '"' || peek(p) == '\'' {
		quote := peek(p)
		p.pos += 1
		start := p.pos
		for !at_end(p) {
			if peek(p) == quote {
				part := p.source[start:p.pos]
				p.pos += 1
				return part, true
			}
			if quote == '"' && peek(p) == '\\' && p.pos + 1 < len(p.source) {
				p.pos += 2
			} else {
				p.pos += 1
			}
		}
		add_error(p, "unterminated quoted key")
		return "", false
	}

	start := p.pos
	for !at_end(p) && bare_key_byte(peek(p)) {
		p.pos += 1
	}
	if p.pos == start {
		return "", false
	}
	return p.source[start:p.pos], true
}

parse_value :: proc(p: ^Parser) -> (Value, bool) {
	skip_ws_comments(p)
	if at_end(p) {
		add_error(p, "expected value")
		return nil, false
	}
	switch peek(p) {
	case '"', '\'':
		text, ok := parse_string_value(p)
		if !ok {
			return nil, false
		}
		return String(text), true
	case '[':
		return parse_array(p)
	case '{':
		return parse_inline_table(p)
	}
	if match_word(p, "true") {
		p.pos += 4
		return Boolean(true), true
	}
	if match_word(p, "false") {
		p.pos += 5
		return Boolean(false), true
	}
	return parse_integer_value(p)
}

parse_string_value :: proc(p: ^Parser) -> (string, bool) {
	quote := peek(p)
	start := p.pos
	p.pos += 1
	for !at_end(p) {
		c := peek(p)
		if c == quote {
			p.pos += 1
			text := p.source[start:p.pos]
			value, allocated, ok := strconv.unquote_string(text, p.allocator)
			if !ok {
				add_error(p, "invalid string")
				return "", false
			}
			if allocated {
				return value, true
			}
			return strings.clone(value, p.allocator), true
		}
		if c == '\n' || c == '\r' {
			break
		}
		if quote == '"' && c == '\\' && p.pos + 1 < len(p.source) {
			p.pos += 2
		} else {
			p.pos += 1
		}
	}
	add_error(p, "unterminated string")
	return "", false
}

parse_array :: proc(p: ^Parser) -> (Value, bool) {
	expect_byte(p, '[')
	array: Array
	array.allocator = p.allocator
	for {
		skip_ws_comments(p)
		if allow_byte(p, ']') {
			return array, true
		}
		value, ok := parse_value(p)
		if !ok {
			return nil, false
		}
		append(&array, value)
		skip_ws_comments(p)
		if allow_byte(p, ',') {
			continue
		}
		if expect_byte(p, ']') {
			return array, true
		}
		return nil, false
	}
}

parse_inline_table :: proc(p: ^Parser) -> (Value, bool) {
	expect_byte(p, '{')
	table := make_table(p.allocator)
	for {
		skip_ws_comments(p)
		if allow_byte(p, '}') {
			return table, true
		}
		parse_key_value(p, table)
		skip_ws_comments(p)
		if allow_byte(p, ',') {
			continue
		}
		if expect_byte(p, '}') {
			return table, true
		}
		return nil, false
	}
}

parse_integer_value :: proc(p: ^Parser) -> (Value, bool) {
	start := p.pos
	if peek(p) == '+' || peek(p) == '-' {
		p.pos += 1
	}
	for !at_end(p) && (digit_byte(peek(p)) || peek(p) == '_') {
		p.pos += 1
	}
	if p.pos == start || (p.pos == start + 1 && (p.source[start] == '+' || p.source[start] == '-')) {
		add_error(p, "unsupported value")
		skip_value(p)
		return nil, false
	}
	value, ok := parse_i64_underscored(p.source[start:p.pos])
	if !ok {
		add_error(p, "invalid integer")
		return nil, false
	}
	return Integer(value), true
}

parse_i64_underscored :: proc(text: string) -> (i64, bool) {
	sign: i64 = 1
	i := 0
	if len(text) > 0 && text[0] == '-' {
		sign = -1
		i = 1
	} else if len(text) > 0 && text[0] == '+' {
		i = 1
	}
	value: i64
	seen := false
	for i < len(text) {
		c := text[i]
		i += 1
		if c == '_' {
			continue
		}
		if !digit_byte(c) {
			return 0, false
		}
		seen = true
		value = value * 10 + i64(c - '0')
	}
	return value * sign, seen
}

insert_value :: proc(p: ^Parser, table: Table, path: []string, value: Value) -> bool {
	target := table
	for part in path[:len(path) - 1] {
		next, ok := table_child(p, target, part)
		if !ok {
			destroy_value(value, p.allocator)
			return false
		}
		target = next
	}
	key := path[len(path) - 1]
	if _, exists := target.entries[key]; exists {
		add_error(p, "duplicate key")
		destroy_value(value, p.allocator)
		return false
	}
	target.entries[strings.clone(key, p.allocator)] = value
	return true
}

table_for_path :: proc(p: ^Parser, root: Table, path: []string) -> (Table, bool) {
	table := root
	for part in path {
		next, ok := table_child(p, table, part)
		if !ok {
			return {}, false
		}
		table = next
	}
	return table, true
}

table_child :: proc(p: ^Parser, table: Table, key: string) -> (Table, bool) {
	if value, ok := table.entries[key]; ok {
		#partial switch v in value {
		case Table:
			return v, true
		}
		add_error(p, "expected table")
		return {}, false
	}
	child := make_table(p.allocator)
	table.entries[strings.clone(key, p.allocator)] = child
	return child, true
}

push_array_table :: proc(p: ^Parser, root: Table, path: []string) -> (Table, bool) {
	parent := root
	if len(path) > 1 {
		next, ok := table_for_path(p, root, path[:len(path) - 1])
		if !ok {
			return {}, false
		}
		parent = next
	}
	key := path[len(path) - 1]
	child := make_table(p.allocator)
	if value, ok := parent.entries[key]; ok {
		#partial switch array in value {
		case Array:
			a := array
			append(&a, child)
			parent.entries[key] = a
			return child, true
		}
		add_error(p, "expected array of tables")
		destroy_table(child, p.allocator)
		return {}, false
	}
	array: Array
	array.allocator = p.allocator
	append(&array, child)
	parent.entries[strings.clone(key, p.allocator)] = array
	return child, true
}

write_value :: proc(out: ^strings.Builder, value: Value) {
	#partial switch v in value {
	case String:
		write_string(out, v)
	case Integer:
		buf: [32]byte
		strings.write_string(out, strconv.write_int(buf[:], v, 10))
	case Boolean:
		strings.write_string(out, "true" if v else "false")
	case Array:
		strings.write_byte(out, '[')
		for elem, i in v {
			if i > 0 {
				strings.write_string(out, ", ")
			}
			write_value(out, elem)
		}
		strings.write_byte(out, ']')
	case Table:
		strings.write_byte(out, '{')
		i := 0
		for key, elem in v.entries {
			if i > 0 {
				strings.write_string(out, ", ")
			}
			write_key(out, key)
			strings.write_string(out, " = ")
			write_value(out, elem)
			i += 1
		}
		strings.write_byte(out, '}')
	}
}

write_key :: proc(out: ^strings.Builder, key: string) {
	if bare_key(key) {
		strings.write_string(out, key)
	} else {
		write_string(out, key)
	}
}

write_string :: proc(out: ^strings.Builder, value: string) {
	strings.write_byte(out, '"')
	for c in value {
		switch c {
		case '\b':
			strings.write_string(out, `\b`)
		case '\t':
			strings.write_string(out, `\t`)
		case '\n':
			strings.write_string(out, `\n`)
		case '\f':
			strings.write_string(out, `\f`)
		case '\r':
			strings.write_string(out, `\r`)
		case '"':
			strings.write_string(out, `\"`)
		case '\\':
			strings.write_string(out, `\\`)
		case:
			strings.write_rune(out, c)
		}
	}
	strings.write_byte(out, '"')
}

skip_ws_comments :: proc(p: ^Parser) {
	for !at_end(p) {
		switch peek(p) {
		case ' ', '\t', '\n', '\r':
			p.pos += 1
		case '#':
			skip_line(p)
		case:
			return
		}
	}
}

skip_inline_ws :: proc(p: ^Parser) {
	for !at_end(p) && (peek(p) == ' ' || peek(p) == '\t') {
		p.pos += 1
	}
}

skip_line :: proc(p: ^Parser) {
	for !at_end(p) && peek(p) != '\n' {
		p.pos += 1
	}
}

skip_value :: proc(p: ^Parser) {
	for !at_end(p) && !value_end_byte(peek(p)) {
		p.pos += 1
	}
}

expect_byte :: proc(p: ^Parser, b: byte) -> bool {
	if allow_byte(p, b) {
		return true
	}
	add_error(p, "unexpected token")
	return false
}

allow_byte :: proc(p: ^Parser, b: byte) -> bool {
	if !at_end(p) && peek(p) == b {
		p.pos += 1
		return true
	}
	return false
}

match_word :: proc(p: ^Parser, word: string) -> bool {
	if p.pos + len(word) > len(p.source) || p.source[p.pos:p.pos + len(word)] != word {
		return false
	}
	end := p.pos + len(word)
	return end == len(p.source) || value_end_byte(p.source[end])
}

peek :: proc(p: ^Parser) -> byte {
	return p.source[p.pos]
}

at_end :: proc(p: ^Parser) -> bool {
	return p.pos >= len(p.source)
}

add_error :: proc(p: ^Parser, message: string) {
	append(&p.errors, Parse_Error{message = message, offset = p.pos})
}

bare_key :: proc(value: string) -> bool {
	if len(value) == 0 {
		return false
	}
	for c in value {
		if !bare_key_rune(c) {
			return false
		}
	}
	return true
}

bare_key_byte :: proc(c: byte) -> bool {
	return(
		('a' <= c && c <= 'z') ||
		('A' <= c && c <= 'Z') ||
		('0' <= c && c <= '9') ||
		c == '_' ||
		c == '-' \
	)
}

bare_key_rune :: proc(c: rune) -> bool {
	return(
		('a' <= c && c <= 'z') ||
		('A' <= c && c <= 'Z') ||
		('0' <= c && c <= '9') ||
		c == '_' ||
		c == '-' \
	)
}

digit_byte :: proc(c: byte) -> bool {
	return '0' <= c && c <= '9'
}

value_end_byte :: proc(c: byte) -> bool {
	return c == ',' || c == ']' || c == '}' || c == '#' || c == '\n' || c == '\r' || c == ' ' || c == '\t'
}

make_table :: proc(allocator: mem.Allocator) -> Table {
	table := new(Table_Data, allocator)
	table.entries = make(map[string]Value, 0, allocator)
	return table
}
