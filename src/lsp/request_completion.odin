package abap_frontend_lsp

import "src:semantic"
import string_interner "src:string_interner"

import json "core:encoding/json"
import "core:fmt"
import "core:mem"
import "core:strings"

handle_completion :: proc(ctx: ^Request_Context, params: json.Value) {
	snapshot, offset, ok := snapshot_for_position(ctx.state, params)
	if !ok {
		send_success(ctx.output, ctx.id, Completion_List{}, ctx.state.allocator)
		return
	}
	out := completion_items_for_snapshot(
		snapshot,
		offset,
		ctx.state.completion_snippets_supported,
		ctx.state.allocator,
	)
	send_success(
		ctx.output,
		ctx.id,
		Completion_List{is_incomplete = false, items = out},
		ctx.state.allocator,
	)
}

completion_items_for_snapshot :: proc(
	snapshot: Snapshot_Lookup,
	offset: int,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> []Completion_Item {
	prefix := completion_prefix(snapshot.source, offset, context.temp_allocator)
	query := semantic.semantic_query(
		snapshot.project,
		snapshot.checker,
		snapshot.file,
		snapshot.provider_index,
	)
	items := semantic.semantic_completion_items_at_offset(
		semantic.semantic_query_completion(query),
		offset,
		prefix,
		context.temp_allocator,
		snapshot.source,
	)
	indent := completion_line_indent(snapshot.source, offset, context.temp_allocator)
	out := make([]Completion_Item, len(items), allocator)
	for item, i in items {
		out[i] = completion_item_from_semantic_item(
			snapshot.project,
			item,
			indent,
			snippets_supported,
			allocator,
		)
	}
	return out
}

completion_item_from_semantic_item :: proc(
	project: ^semantic.Project,
	item: semantic.Semantic_Completion_Item,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	name := string_interner.load(project.interner, item.name)
	out := Completion_Item {
		label = name,
		kind = completion_kind(item.entity),
		insert_text = name,
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
	if snippets_supported &&
	   item.source == .Selector_Member &&
	   item.entity != nil &&
	   item.entity.kind == .Method {
		out.insert_text = completion_method_call_snippet(project, item.entity, name, indent, allocator)
		out.insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET
	}
	return out
}

completion_prefix :: proc(source: string, offset: int, allocator: mem.Allocator) -> string {
	end := clamp(offset, 0, len(source))
	start := end
	for start > 0 {
		ch := source[start - 1]
		if !(('a' <= ch && ch <= 'z') ||
			   ('A' <= ch && ch <= 'Z') ||
			   ('0' <= ch && ch <= '9') ||
			   ch == '_' ||
			   ch == '/') {
			break
		}
		start -= 1
	}
	if start == end {
		return ""
	}
	return strings.clone(source[start:end], allocator)
}

completion_line_indent :: proc(source: string, offset: int, allocator: mem.Allocator) -> string {
	line_start := clamp(offset, 0, len(source))
	for line_start > 0 && source[line_start - 1] != '\n' {
		line_start -= 1
	}
	indent_end := line_start
	for indent_end < len(source) && (source[indent_end] == ' ' || source[indent_end] == '\t') {
		indent_end += 1
	}
	return strings.clone(source[line_start:indent_end], allocator)
}

completion_method_call_snippet :: proc(
	project: ^semantic.Project,
	method: ^semantic.Entity,
	name: string,
	indent: string,
	allocator: mem.Allocator,
) -> string {
	payload, ok := method.payload.(^semantic.Entity_Routine_Payload)
	assert(ok && payload != nil)

	out := strings.builder_make(allocator)
	completion_snippet_write_text(&out, name)
	if !completion_method_has_call_parameters(payload) {
		strings.write_string(&out, "( )$0")
		return strings.to_string(out)
	}

	strings.write_string(&out, "(\n")
	tabstop := 1
	if completion_method_only_call_exporting(payload) {
		_ = completion_write_method_call_section(
			&out,
			project,
			payload.parameters[:],
			.Method_Importing,
			"",
			indent,
			&tabstop,
		)
	} else {
		_ = completion_write_method_call_section(
			&out,
			project,
			payload.parameters[:],
			.Method_Importing,
			"EXPORTING",
			indent,
			&tabstop,
		)
		_ = completion_write_method_call_section(
			&out,
			project,
			payload.parameters[:],
			.Method_Exporting,
			"IMPORTING",
			indent,
			&tabstop,
		)
		_ = completion_write_method_call_section(
			&out,
			project,
			payload.parameters[:],
			.Method_Changing,
			"CHANGING",
			indent,
			&tabstop,
		)
	}
	strings.write_string(&out, indent)
	strings.write_string(&out, ")$0")
	return strings.to_string(out)
}

completion_method_has_call_parameters :: proc(payload: ^semantic.Entity_Routine_Payload) -> bool {
	assert(payload != nil)
	for param in payload.parameters {
		#partial switch completion_parameter_section(param) {
		case .Method_Importing, .Method_Exporting, .Method_Changing:
			return true
		case:
		}
	}
	return false
}

completion_method_only_call_exporting :: proc(payload: ^semantic.Entity_Routine_Payload) -> bool {
	assert(payload != nil)
	has_exporting := false
	for param in payload.parameters {
		#partial switch completion_parameter_section(param) {
		case .Method_Importing:
			has_exporting = true
		case .Method_Exporting, .Method_Changing:
			return false
		case:
		}
	}
	return has_exporting
}

completion_write_method_call_section :: proc(
	out: ^strings.Builder,
	project: ^semantic.Project,
	parameters: []^semantic.Entity,
	section: semantic.Entity_Parameter_Section,
	heading: string,
	indent: string,
	tabstop: ^int,
) -> bool {
	wrote := false
	for param in parameters {
		if completion_parameter_section(param) != section {
			continue
		}
		if !wrote {
			if heading != "" {
				strings.write_string(out, indent)
				strings.write_string(out, "  ")
				strings.write_string(out, heading)
				strings.write_byte(out, '\n')
			}
			wrote = true
		}
		strings.write_string(out, indent)
		strings.write_string(out, "  " if heading == "" else "    ")
		completion_snippet_write_text(out, string_interner.load(project.interner, param.name))
		strings.write_string(out, " = ")
		strings.write_string(out, fmt.tprintf("$%d", tabstop^))
		strings.write_byte(out, '\n')
		tabstop^ += 1
	}
	return wrote
}

completion_parameter_section :: proc(param: ^semantic.Entity) -> semantic.Entity_Parameter_Section {
	assert(param != nil)
	payload, ok := param.payload.(^semantic.Entity_Variable_Payload)
	assert(ok && payload != nil)
	return payload.section
}

completion_snippet_write_text :: proc(out: ^strings.Builder, text: string) {
	for i in 0 ..< len(text) {
		ch := text[i]
		if ch == '$' || ch == '}' || ch == '\\' {
			strings.write_byte(out, '\\')
		}
		strings.write_byte(out, ch)
	}
}

completion_kind :: proc "contextless" (entity: ^semantic.Entity) -> int {
	if entity == nil {
		return COMPLETION_VARIABLE
	}
	#partial switch entity.kind {
	case .Class:
		return COMPLETION_CLASS
	case .Interface:
		return COMPLETION_INTERFACE
	case .Type_Def:
		return COMPLETION_STRUCT
	case .Field:
		return COMPLETION_FIELD
	case .Form:
		return COMPLETION_FUNCTION
	case .Module:
		return COMPLETION_MODULE
	case .Method:
		return COMPLETION_METHOD
	case .Event:
		return COMPLETION_EVENT
	case .Constant:
		return COMPLETION_CONSTANT
	case .Enum_Member:
		return COMPLETION_ENUM_MEMBER
	case:
	}
	return COMPLETION_VARIABLE
}
