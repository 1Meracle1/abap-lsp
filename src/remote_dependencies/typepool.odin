package abap_frontend_remote_dependencies

import "src:adt"
import "src:ast"
import "src:parser"

import "core:mem"
import "core:strings"

TYPEPOOL_OBJECT_KIND :: "type-pool"
TYPEPOOL_OBJECT_TYPE :: "TYPEPOOL"
TYPEPOOL_DECL_KEYWORD :: "TYPE-POOL"
TYPEPOOL_PARSE_URI :: "abapls-typepool-source"
TYPEPOOL_SYMBOL_PARSE_URI :: "abapls-typepool-symbols"
TYPEPOOL_DEPENDENCY_URI_PREFIX :: "abapls-typepool:/"
TYPEPOOL_OBJECT_URI_PREFIX :: "type-pool:"

typepool_dependency_source :: proc(source: string, allocator: mem.Allocator) -> string {
	trimmed := strings.trim_left_space(source)
	if !starts_with_ignore_case(trimmed, TYPEPOOL_DECL_KEYWORD) ||
	   len(trimmed) <= len(TYPEPOOL_DECL_KEYWORD) {
		return strings.clone(source, allocator)
	}
	next := trimmed[len(TYPEPOOL_DECL_KEYWORD)]
	if next != ' ' && next != '\t' && next != '\r' && next != '\n' {
		return strings.clone(source, allocator)
	}
	if dot := strings.index_byte(trimmed, '.'); dot >= 0 {
		return strings.clone(strings.trim_left_space(trimmed[dot + 1:]), allocator)
	}
	return strings.clone(source, allocator)
}

expanded_typepool_dependency_source :: proc(
	client: ^adt.Client,
	raw: string,
	allocator: mem.Allocator,
) -> string {
	source := typepool_dependency_source(raw, context.temp_allocator)
	parsed := parser.parse(source, TYPEPOOL_PARSE_URI, context.temp_allocator)
	if client != nil && typepool_parsed_source_has_includes(parsed.root) {
		out := strings.builder_make(allocator)
		seen := make(map[string]bool, 8, context.temp_allocator)
		wrote := false
		append_expanded_typepool_source(&out, client, source, parsed.root, &seen, &wrote)
		source = strings.to_string(out)
	}
	return expanded_typepool_macro_source(source, allocator)
}

append_expanded_typepool_source :: proc(
	out: ^strings.Builder,
	client: ^adt.Client,
	source: string,
	root: ^ast.File,
	seen: ^map[string]bool,
	wrote: ^bool,
) {
	if root == nil {
		write_typepool_source_part(out, source, wrote)
		return
	}

	last := 0
	for stmt in root.stmts {
		include, ok := stmt.derived_stmt.(^ast.Include_Stmt)
		if !ok {
			continue
		}
		write_typepool_source_part(out, source[last:stmt.range.start], wrote)
		failed := false
		for name in include.names {
			key := strings.to_lower(name.name, context.temp_allocator)
			if key in seen^ {
				continue
			}
			seen^[key] = true
			fetched, err := adt.fetch_source(client, .Include, name.name, "", context.temp_allocator)
			if err != .None {
				failed = true
				continue
			}
			include_source := typepool_dependency_source(fetched.body, context.temp_allocator)
			include_parsed := parser.parse(
				include_source,
				TYPEPOOL_PARSE_URI,
				context.temp_allocator,
			)
			append_expanded_typepool_source(out, client, include_source, include_parsed.root, seen, wrote)
		}
		if failed {
			write_typepool_source_part(out, source[stmt.range.start:stmt.range.end], wrote)
		}
		last = stmt.range.end
	}
	write_typepool_source_part(out, source[last:], wrote)
}

write_typepool_source_part :: proc(out: ^strings.Builder, source: string, wrote: ^bool) {
	part := strings.trim_space(source)
	if part == "" {
		return
	}
	if wrote^ {
		strings.write_byte(out, '\n')
	}
	strings.write_string(out, part)
	wrote^ = true
}

expanded_typepool_macro_source :: proc(source: string, allocator: mem.Allocator) -> string {
	parsed := parser.parse(source, TYPEPOOL_PARSE_URI, context.temp_allocator)
	if parsed.root == nil {
		return strings.clone(source, allocator)
	}
	macros := make(map[string]string, 8, context.temp_allocator)
	out := strings.builder_make(allocator)
	last := 0
	for stmt in parsed.root.stmts {
		#partial switch n in stmt.derived_stmt {
		case ^ast.Macro_Def_Stmt:
			if n.name != "" {
				macros[strings.to_lower(n.name, context.temp_allocator)] = n.body
			}
			strings.write_string(&out, source[last:stmt.range.start])
			last = stmt.range.end
		case ^ast.Macro_Call_Stmt:
			key := strings.to_lower(n.name, context.temp_allocator)
			body, ok := macros[key]
			if !ok {
				continue
			}
			strings.write_string(&out, source[last:stmt.range.start])
			args := typepool_macro_call_args(source[stmt.range.start:stmt.range.end], n.name, context.temp_allocator)
			write_expanded_typepool_macro_body(&out, body, args[:])
			last = stmt.range.end
		}
	}
	strings.write_string(&out, source[last:])
	return strings.to_string(out)
}

write_expanded_typepool_macro_body :: proc(
	out: ^strings.Builder,
	body: string,
	args: []string,
) {
	for i := 0; i < len(body); {
		if body[i] == '&' && i + 1 < len(body) && body[i + 1] >= '1' && body[i + 1] <= '9' {
			arg_index := int(body[i + 1] - '1')
			if arg_index < len(args) {
				strings.write_string(out, args[arg_index])
			}
			i += 2
			continue
		}
		strings.write_byte(out, body[i])
		i += 1
	}
}

typepool_macro_call_args :: proc(call_source, name: string, allocator: mem.Allocator) -> [dynamic]string {
	text := strings.trim_space(call_source)
	if strings.has_suffix(text, ".") {
		text = strings.trim_space(text[:len(text) - 1])
	}
	if starts_with_ignore_case(text, name) {
		text = strings.trim_space(text[len(name):])
	}
	args := make([dynamic]string, 0, 4, allocator)
	for len(text) > 0 {
		end := 0
		for end < len(text) && !strings.is_ascii_space(rune(text[end])) {
			end += 1
		}
		if end > 0 {
			append(&args, text[:end])
		}
		text = strings.trim_left_space(text[end:])
	}
	return args
}

typepool_source_symbols :: proc(
	source: string,
	allocator: mem.Allocator,
) -> [dynamic]string {
	symbols := make([dynamic]string, 0, 8, allocator)
	parsed := parser.parse(source, TYPEPOOL_SYMBOL_PARSE_URI, context.temp_allocator)
	if parsed.root == nil {
		return symbols
	}
	for stmt in parsed.root.stmts {
		#partial switch n in stmt.derived_stmt {
		case ^ast.Types_Decl:
			for clause in n.types {
				if clause.kind == .Begin_Group || clause.kind == .Normal {
					insert_unique_typepool_symbol(&symbols, clause.name, allocator)
				}
			}
		case ^ast.Constants_Decl:
			for clause in n.constants {
				if clause.kind == .Begin_Group || clause.kind == .Normal {
					insert_unique_typepool_symbol(&symbols, clause.name, allocator)
				}
			}
		}
	}
	return symbols
}

insert_unique_typepool_symbol :: proc(
	symbols: ^[dynamic]string,
	name: string,
	allocator: mem.Allocator,
) {
	if name == "" {
		return
	}
	key := strings.to_lower(name, allocator)
	for existing in symbols^ {
		if existing == key {
			return
		}
	}
	append(symbols, key)
}

typepool_source_has_pending_expansion :: proc(source: string, allocator: mem.Allocator) -> bool {
	upper := strings.to_upper(source, context.temp_allocator)
	if !strings.contains(upper, "INCLUDE") &&
	   !strings.contains(upper, "DEFINE") &&
	   !strings.contains(upper, "END-OF-DEFINITION") {
		return false
	}
	parsed := parser.parse(source, TYPEPOOL_PARSE_URI, allocator)
	return typepool_parsed_source_has_includes(parsed.root) ||
	       typepool_parsed_source_has_macros(parsed.root)
}

typepool_parsed_source_has_includes :: proc(root: ^ast.File) -> bool {
	if root == nil {
		return false
	}
	for stmt in root.stmts {
		if _, ok := stmt.derived_stmt.(^ast.Include_Stmt); ok {
			return true
		}
	}
	return false
}

typepool_parsed_source_has_macros :: proc(root: ^ast.File) -> bool {
	if root == nil {
		return false
	}
	for stmt in root.stmts {
		#partial switch _ in stmt.derived_stmt {
		case ^ast.Macro_Def_Stmt,
		     ^ast.Macro_Call_Stmt:
			return true
		}
	}
	return false
}

starts_with_ignore_case :: proc(source, prefix: string) -> bool {
	return len(source) >= len(prefix) && strings.equal_fold(source[:len(prefix)], prefix)
}

typepool_dependency_uri :: proc(pool: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, TYPEPOOL_DEPENDENCY_URI_PREFIX)
	strings.write_string(&out, strings.to_lower(pool, allocator))
	strings.write_string(&out, ".abap")
	return strings.to_string(out)
}

typepool_object_uri :: proc(pool: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, TYPEPOOL_OBJECT_URI_PREFIX)
	strings.write_string(&out, strings.to_upper(pool, allocator))
	return strings.to_string(out)
}
