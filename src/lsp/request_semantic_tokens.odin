package abap_frontend_lsp

import "src:semantic"

import json "core:encoding/json"
import "core:mem"
import "core:slice"

Pending_Token :: struct {
	start:      int,
	end:        int,
	token_type: u32,
	modifiers:  u32,
	priority:   int,
}

Semantic_Token_Type_Indices :: struct {
	type_:       u32,
	class:       u32,
	interface_:  u32,
	parameter:   u32,
	variable:    u32,
	property:    u32,
	function:    u32,
	method:      u32,
	event:       u32,
	namespace:   u32,
	enum_member: u32,
}

TOKEN_TYPE_INDICES :: Semantic_Token_Type_Indices {
	type_       = 0,
	class       = 1,
	interface_  = 2,
	parameter   = 3,
	variable    = 4,
	property    = 5,
	function    = 6,
	method      = 7,
	event       = 8,
	namespace   = 9,
	enum_member = 10,
}

handle_semantic_tokens :: proc(ctx: ^Request_Context, params: json.Value) {
	uri := uri_from_text_document_params(params)
	if uri == "" {
		send_success(ctx.output, ctx.id, Semantic_Tokens{}, ctx.state.allocator)
		return
	}
	snapshot := snapshot_for_uri(ctx.state, uri)
	if !snapshot.ok {
		send_success(ctx.output, ctx.id, Semantic_Tokens{}, ctx.state.allocator)
		return
	}
	tokens := semantic_tokens_for_snapshot(snapshot, ctx.state.allocator)
	send_success(ctx.output, ctx.id, Semantic_Tokens{data = tokens}, ctx.state.allocator)
}

semantic_tokens_for_snapshot :: proc(
	snapshot: Snapshot_Lookup,
	allocator: mem.Allocator,
) -> []u32 {
	pending := make([dynamic]Pending_Token, 0, 128, context.temp_allocator)
	for entity in snapshot.checker.info.definitions {
		if entity == nil || entity.source_file != snapshot.file {
			continue
		}
		push_pending_token(&pending, entity.name_range, entity, true)
	}
	for use in snapshot.checker.info.uses {
		if use.file != snapshot.file || use.entity == nil {
			continue
		}
		push_pending_token(&pending, semantic.semantic_entity_use_range(use), use.entity, false)
	}
	slice.sort_by(pending[:], pending_token_less)
	merged := make([dynamic]Pending_Token, 0, len(pending), context.temp_allocator)
	last_end := 0
	for token in pending {
		if token.start < last_end {
			continue
		}
		append(&merged, token)
		last_end = token.end
	}
	return encode_semantic_token_deltas(snapshot.source, merged[:], allocator)
}

push_pending_token :: proc(
	pending: ^[dynamic]Pending_Token,
	range: semantic.Range,
	entity: ^semantic.Entity,
	declaration: bool,
) {
	if range.start >= range.end {
		return
	}
	token_type := semantic_token_type(entity)

	TOKEN_MOD_DECLARATION :: u32(1 << 0)
	TOKEN_MOD_READONLY :: u32(1 << 1)
	modifiers: u32
	if declaration {
		modifiers |= TOKEN_MOD_DECLARATION
	}
	if entity != nil &&
	   (entity.kind == .Constant || entity.kind == .Enum_Member || .Read_Only in entity.flags) {
		modifiers |= TOKEN_MOD_READONLY
	}
	append(
		pending,
		Pending_Token {
			start = range.start,
			end = range.end,
			token_type = token_type,
			modifiers = modifiers,
			priority = 0 if declaration else 1,
		},
	)
}

pending_token_less :: proc(a, b: Pending_Token) -> bool {
	if a.start != b.start {
		return a.start < b.start
	}
	a_width := a.end - a.start
	b_width := b.end - b.start
	if a_width != b_width {
		return a_width < b_width
	}
	return a.priority < b.priority
}

encode_semantic_token_deltas :: proc(
	source: string,
	tokens: []Pending_Token,
	allocator: mem.Allocator,
) -> []u32 {
	out := make([dynamic]u32, 0, len(tokens) * 5, allocator)
	prev_line := 0
	prev_character := 0
	for token in tokens {
		start := offset_to_position(source, token.start)
		end := offset_to_position(source, token.end)
		if end.line != start.line || end.character <= start.character {
			continue
		}
		delta_line := start.line - prev_line
		delta_start := start.character - prev_character if delta_line == 0 else start.character
		append(&out, u32(delta_line))
		append(&out, u32(delta_start))
		append(&out, u32(end.character - start.character))
		append(&out, token.token_type)
		append(&out, token.modifiers)
		prev_line = start.line
		prev_character = start.character
	}
	return out[:]
}

semantic_token_type :: proc "contextless" (entity: ^semantic.Entity) -> u32 {
	if entity == nil {
		return TOKEN_TYPE_INDICES.variable
	}
	#partial switch entity.kind {
	case .Type_Def:
		return TOKEN_TYPE_INDICES.type_
	case .Class:
		return TOKEN_TYPE_INDICES.class
	case .Interface:
		return TOKEN_TYPE_INDICES.type_
	case .Parameter:
		return TOKEN_TYPE_INDICES.parameter
	case .Field:
		return TOKEN_TYPE_INDICES.property
	case .Form, .Module, .Builtin:
		return TOKEN_TYPE_INDICES.function
	case .Method:
		return TOKEN_TYPE_INDICES.method
	case .Event:
		return TOKEN_TYPE_INDICES.event
	case .Enum_Member:
		return TOKEN_TYPE_INDICES.enum_member
	case .Variable, .Constant, .Field_Symbol, .Exception, .Include, .Control, .Report:
		return TOKEN_TYPE_INDICES.variable
	case .Alias, .Invalid:
	}
	return TOKEN_TYPE_INDICES.variable
}
