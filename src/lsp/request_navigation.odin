package abap_frontend_lsp

import "src:ast"
import "src:semantic"
import string_interner "src:string_interner"

import json "core:encoding/json"
import "core:fmt"
import "core:mem"
import "core:strings"

handle_hover :: proc(ctx: ^Request_Context, params: json.Value) {
	found := entity_at_position(ctx.state, params)
	if !found.ok {
		send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
		return
	}
	text := entity_hover_text(found.snapshot.project, found.entity)
	hover := Hover {
		contents = Hover_Markup{kind = "markdown", value = text},
		range = range_from_offsets(found.snapshot.source, found.range.start, found.range.end),
	}
	send_success(ctx.output, ctx.id, hover, ctx.state.allocator)
}

handle_definition :: proc(ctx: ^Request_Context, params: json.Value) {
	found := entity_at_position(ctx.state, params)
	if !found.ok || found.entity.source_file == nil {
		send_success(ctx.output, ctx.id, json.Null(nil), ctx.state.allocator)
		return
	}
	source := source_for_project_file(ctx.state, found.entity.source_file)
	if source == "" {
		source = found.snapshot.source
	}
	location := Location {
		uri   = found.entity.source_file.path,
		range = range_from_offsets(
			source,
			found.entity.name_range.start,
			found.entity.name_range.end,
		),
	}
	send_success(ctx.output, ctx.id, location, ctx.state.allocator)
}

handle_references :: proc(ctx: ^Request_Context, params: json.Value) {
	found := entity_at_position(ctx.state, params)
	if !found.ok {
		send_success(ctx.output, ctx.id, []Location{}, ctx.state.allocator)
		return
	}
	query := semantic.semantic_query(found.snapshot.project, found.snapshot.checker)
	refs := semantic.semantic_ref_resolving_to_entity(
		semantic.semantic_query_refs(query),
		found.entity,
		context.temp_allocator,
	)
	locations := make([dynamic]Location, 0, len(refs) + 1, ctx.state.allocator)
	if found.entity.source_file != nil {
		source := source_for_project_file(ctx.state, found.entity.source_file)
		append(
			&locations,
			Location {
				uri = found.entity.source_file.path,
				range = range_from_offsets(
					source,
					found.entity.name_range.start,
					found.entity.name_range.end,
				),
			},
		)
	}
	for ref in refs {
		if ref == nil || ref.file == nil {
			continue
		}
		source := source_for_project_file(ctx.state, ref.file)
		range := semantic.semantic_entity_use_range(ref^)
		if range.start >= range.end {
			continue
		}
		location := Location {
			uri   = ref.file.path,
			range = range_from_offsets(source, range.start, range.end),
		}
		if !location_present(locations[:], location) {
			append(&locations, location)
		}
	}
	send_success(ctx.output, ctx.id, locations[:], ctx.state.allocator)
}

entity_label :: proc(project: ^semantic.Project, entity: ^semantic.Entity) -> string {
	if entity == nil {
		return ""
	}
	name := string_interner.load(project.interner, entity.name)
	return fmt.tprintf("`%s` %s", name, entity_kind_label(entity.kind))
}

entity_detail :: proc(project: ^semantic.Project, entity: ^semantic.Entity) -> string {
	if entity == nil {
		return ""
	}
	type_text := type_label(project, entity.type)
	if type_text == "" {
		type_text = declared_entity_type_label(entity)
	}
	if type_text == "" {
		return ""
	}
	return fmt.tprintf("type: `%s`", type_text)
}

entity_hover_text :: proc(project: ^semantic.Project, entity: ^semantic.Entity) -> string {
	label := entity_label(project, entity)
	detail := entity_detail(project, entity)
	documentation := entity_documentation(project, entity)
	out := strings.builder_make(context.temp_allocator)
	strings.write_string(&out, label)
	if detail != "" {
		strings.write_string(&out, "\n\n")
		strings.write_string(&out, detail)
	}
	if documentation != "" {
		strings.write_string(&out, "\n\n")
		strings.write_string(&out, documentation)
	}
	return strings.to_string(out)
}

entity_documentation :: proc(project: ^semantic.Project, entity: ^semantic.Entity) -> string {
	if entity == nil {
		return ""
	}
	if .Builtin in entity.flags {
		if payload, ok := entity.payload.(^semantic.Entity_Builtin_Payload); ok && payload != nil && payload.docs != "" {
			return payload.docs
		}
		name := string_interner.load(project.interner, entity.name)
		if entity.kind == .Field && entity.owner != nil {
			owner_name := string_interner.load(project.interner, entity.owner.name)
			if docs := semantic.checker_builtin_structure_field_description(owner_name, name); docs != "" {
				return docs
			}
		}
		if docs := semantic.checker_builtin_symbol_description(name, entity.kind); docs != "" {
			return docs
		}
	}
	if entity.decl_info == nil {
		return ""
	}
	return decl_info_documentation(entity.decl_info, context.temp_allocator)
}

decl_info_documentation :: proc(info: ^semantic.Decl_Info, allocator: mem.Allocator) -> string {
	if info == nil {
		return ""
	}
	out := strings.builder_make(allocator)
	write_comment_documentation(&out, info.docs)
	if len(info.docs) > 0 && len(info.comment) > 0 {
		strings.write_byte(&out, '\n')
	}
	write_comment_documentation(&out, info.comment)
	return strings.to_string(out)
}

write_comment_documentation :: proc(out: ^strings.Builder, trivia: []ast.Ast_Trivia) {
	wrote := false
	for item in trivia {
		if item.kind != .Comment {
			continue
		}
		text := clean_comment_documentation(item.text)
		if text == "" {
			continue
		}
		if wrote {
			strings.write_byte(out, '\n')
		}
		strings.write_string(out, text)
		wrote = true
	}
}

clean_comment_documentation :: proc(text: string) -> string {
	trimmed := strings.trim_space(text)
	if len(trimmed) > 0 && (trimmed[0] == '"' || trimmed[0] == '*') {
		trimmed = strings.trim_space(trimmed[1:])
	}
	return trimmed
}

entity_kind_label :: proc(kind: semantic.Entity_Kind) -> string {
	#partial switch kind {
	case .Type_Def:
		return "type"
	case .Field_Symbol:
		return "field-symbol"
	case .Invalid:
		return "symbol"
	}
	raw := fmt.tprintf("%v", kind)
	lower := strings.to_lower(raw, context.temp_allocator)
	label, _ := strings.replace_all(lower, "_", " ", context.temp_allocator)
	return label
}

declared_entity_type_label :: proc(entity: ^semantic.Entity) -> string {
	if entity == nil || entity.decl_info == nil || entity.decl_info.type_clause == nil {
		return ""
	}
	return declared_type_clause_label(entity.decl_info.type_clause)
}

declared_type_clause_label :: proc(clause: ^ast.Data_Type_Clause) -> string {
	if clause == nil {
		return ""
	}
	base := declared_type_ref_label(clause.type_ref)
	#partial switch clause.form {
	case .Ref_To:
		return fmt.tprintf("ref to %s", base) if base != "" else "ref"
	case .Any_Table,
	     .Table,
	     .Like_Table,
	     .Index_Table,
	     .Standard_Table,
	     .Sorted_Table,
	     .Hashed_Table,
	     .Like_Standard_Table,
	     .Like_Sorted_Table,
	     .Like_Hashed_Table:
		return fmt.tprintf("table of %s", base) if base != "" else "table"
	case .Like_Line_Of,
	     .Type_Line_Of:
		return fmt.tprintf("line of %s", base) if base != "" else ""
	case .Range_Of:
		return fmt.tprintf("range of %s", base) if base != "" else "range"
	}
	return base
}

declared_type_ref_label :: proc(expr: ^ast.Expr) -> string {
	if expr == nil {
		return ""
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Type_Ref_Expr:
		if n.name.text != "" {
			return n.name.text
		}
		return n.source.text
	case ^ast.Ident_Expr:
		return n.name
	case ^ast.Literal_Expr:
		return n.value
	}
	return ""
}

type_label :: proc(project: ^semantic.Project, typ: ^semantic.Type) -> string {
	if typ == nil {
		return ""
	}
	switch typ.kind {
	case .Builtin, .Named, .Class, .Interface:
		if string_interner.is_valid(typ.name) {
			return string_interner.load(project.interner, typ.name)
		}
	case .Structure:
		if typ.structure != nil && string_interner.is_valid(typ.structure.name) {
			return string_interner.load(project.interner, typ.structure.name)
		}
		return "structure"
	case .Table:
		base := type_label(project, typ.base)
		return fmt.tprintf("table of %s", base) if base != "" else "table"
	case .Ref:
		base := type_label(project, typ.base)
		return fmt.tprintf("ref to %s", base) if base != "" else "ref"
	case .Routine:
		return "routine"
	case .Unknown:
	}
	return ""
}

source_for_project_file :: proc(state: ^Server_State, file: ^semantic.Project_File) -> string {
	if file == nil {
		return ""
	}
	if doc, ok := state.documents[file.path]; ok {
		return doc.text
	}
	return ""
}

location_present :: proc(locations: []Location, location: Location) -> bool {
	for existing in locations {
		if existing.uri == location.uri && existing.range == location.range {
			return true
		}
	}
	return false
}
