package abap_frontend_lsp

import "src:semantic"
import string_interner "src:string_interner"

import json "core:encoding/json"
import "core:mem"
import "core:strings"

handle_completion :: proc(ctx: ^Request_Context, params: json.Value) {
	snapshot, offset, ok := snapshot_for_position(ctx.state, params)
	if !ok {
		send_success(ctx.output, ctx.id, Completion_List{}, ctx.state.allocator)
		return
	}
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
	)
	out := make([]Completion_Item, len(items), ctx.state.allocator)
	for item, i in items {
		name := string_interner.load(snapshot.project.interner, item.name)
		out[i] = Completion_Item {
			label = name,
			kind  = completion_kind(item.entity),
		}
	}
	send_success(
		ctx.output,
		ctx.id,
		Completion_List{is_incomplete = false, items = out},
		ctx.state.allocator,
	)
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
