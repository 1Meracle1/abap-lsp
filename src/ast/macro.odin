package abap_frontend_ast

import "core:mem"
import "core:strings"

expand_macros_to_source :: proc(
	file: ^File,
	allocator: mem.Allocator,
	options := DEFAULT_PRINT_OPTIONS,
) -> (
	string,
	bool,
) {
	out := strings.builder_make(allocator)
	expanded := write_macro_expanded_file(&out, file, options)
	return strings.to_string(out), expanded
}

write_macro_expanded_file :: proc(
	out: ^strings.Builder,
	file: ^File,
	options := DEFAULT_PRINT_OPTIONS,
) -> bool {
	if file == nil {
		return false
	}
	p := Printer {
		out     = out,
		options = options,
	}
	return emit_macro_expanded_file(&p, file)
}

emit_macro_expanded_file :: proc(p: ^Printer, file: ^File) -> bool {
	emit_leading_trivia(p, &file.node)

	macros := make(map[string]^Macro_Def_Stmt, 8, context.temp_allocator)
	detached_index := 0
	wrote_any := false
	expanded := false

	for stmt in file.stmts {
		for detached_index < len(file.detached_trivia) &&
		    file.detached_trivia[detached_index].trivia.range.start < stmt.range.start {
			if wrote_any {
				emit_newline(p)
			}
			emit(p, file.detached_trivia[detached_index].trivia.text)
			wrote_any = true
			detached_index += 1
		}

		if def, ok := stmt.derived_stmt.(^Macro_Def_Stmt); ok {
			if def.name.text != "" {
				macros[strings.to_lower(def.name.text, context.temp_allocator)] = def
			}
			expanded = true
			continue
		}

		if call, ok := stmt.derived_stmt.(^Macro_Call_Stmt); ok {
			if def, found := macros[strings.to_lower(call.name.text, context.temp_allocator)];
			   found {
				if len(def.body) > 0 {
					if wrote_any {
						emit_newline(p)
					}
					emit_macro_expanded_stmt_list(p, def.body, call.args[:])
					wrote_any = true
				}
				expanded = true
				continue
			}
		}

		if wrote_any {
			emit_newline(p)
		}
		emit_node(p, stmt)
		wrote_any = true
	}

	for detached_index < len(file.detached_trivia) {
		if wrote_any {
			emit_newline(p)
		}
		emit(p, file.detached_trivia[detached_index].trivia.text)
		wrote_any = true
		detached_index += 1
	}

	return expanded
}

emit_macro_expanded_stmt_list :: proc(p: ^Printer, body: [dynamic]^Stmt, args: []^Expr) {
	old_args := p.macro_args
	p.macro_args = args
	defer p.macro_args = old_args

	emit_stmt_list(p, body)
}

macro_arg_replacement :: proc(p: ^Printer, arg: ^Macro_Arg_Ref_Expr) -> ^Expr {
	index := arg.slot - 1
	if index < 0 || index >= len(p.macro_args) {
		return nil
	}
	return p.macro_args[index]
}

emit_macro_arg_replacement :: proc(p: ^Printer, arg: ^Expr) {
	old_args := p.macro_args
	p.macro_args = nil
	defer p.macro_args = old_args

	emit_node(p, arg)
}
