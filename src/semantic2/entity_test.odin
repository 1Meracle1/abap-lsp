package abap_frontend_semantic

import analyze "src:semantic/analyze"

import "core:testing"

entity_kind_from_legacy_symbol_kind :: proc "contextless" (
	kind: analyze.Symbol_Kind,
) -> (Entity_Kind, bool) {
	switch kind {
	case .Builtin_Type:
		return .Builtin_Type, true
	case .Builtin_Routine:
		return .Builtin_Routine, true
	case .Builtin_Constant:
		return .Builtin_Constant, true
	case .Builtin_Variable:
		return .Builtin_Variable, true
	case .Variable:
		return .Variable, true
	case .Constant:
		return .Constant, true
	case .Enum_Member:
		return .Enum_Member, true
	case .Type_Def:
		return .Type_Def, true
	case .Field_Symbol:
		return .Field_Symbol, true
	case .Form:
		return .Form, true
	case .Parameter:
		return .Parameter, true
	case .Exception:
		return .Exception, true
	case .Class:
		return .Class, true
	case .Interface:
		return .Interface, true
	case .Method:
		return .Method, true
	case .Field:
		return .Field, true
	case .Include:
		return .Include, true
	case .Event:
		return .Event, true
	case .Alias:
		return .Alias, true
	case .Module:
		return .Module, true
	case .Control:
		return .Control, true
	case .Report:
		return .Report, true
	}
	return .Invalid, false
}

namespace_from_legacy_namespace :: proc "contextless" (
	namespace: analyze.Namespace,
) -> Namespace {
	switch namespace {
	case .Value:
		return .Value
	case .Type:
		return .Type
	case .Routine:
		return .Routine
	}
	return .Value
}

@(test)
root_semantic_entity_kind_covers_legacy_symbol_kinds :: proc(t: ^testing.T) {
	legacy_kinds := [?]analyze.Symbol_Kind {
		.Builtin_Type,
		.Builtin_Routine,
		.Builtin_Constant,
		.Builtin_Variable,
		.Variable,
		.Constant,
		.Enum_Member,
		.Type_Def,
		.Field_Symbol,
		.Form,
		.Parameter,
		.Exception,
		.Class,
		.Interface,
		.Method,
		.Field,
		.Include,
		.Event,
		.Alias,
		.Module,
		.Control,
		.Report,
	}
	for legacy_kind in legacy_kinds {
		entity_kind, ok := entity_kind_from_legacy_symbol_kind(legacy_kind)
		testing.expect(t, ok)
		testing.expect(t, entity_kind != .Invalid)
	}
}

@(test)
root_semantic_entity_namespace_occupancy_matches_legacy_symbols :: proc(t: ^testing.T) {
	legacy_kinds := [?]analyze.Symbol_Kind {
		.Builtin_Type,
		.Builtin_Routine,
		.Builtin_Constant,
		.Builtin_Variable,
		.Variable,
		.Constant,
		.Enum_Member,
		.Type_Def,
		.Field_Symbol,
		.Form,
		.Parameter,
		.Exception,
		.Class,
		.Interface,
		.Method,
		.Field,
		.Include,
		.Event,
		.Alias,
		.Module,
		.Control,
		.Report,
	}
	legacy_namespaces := [?]analyze.Namespace{.Value, .Type, .Routine}
	for legacy_kind in legacy_kinds {
		entity_kind, ok := entity_kind_from_legacy_symbol_kind(legacy_kind)
		testing.expect(t, ok)
		for legacy_namespace in legacy_namespaces {
			namespace := namespace_from_legacy_namespace(legacy_namespace)
			testing.expect_value(
				t,
				entity_kind_occupies(entity_kind, namespace),
				analyze.symbol_kind_occupies(legacy_kind, legacy_namespace),
			)
		}
	}
}

@(test)
root_semantic_entity_arena_allocates_representative_entities :: proc(t: ^testing.T) {
	arena := entity_arena_make(context.allocator, Source_File_Id(7))
	defer entity_arena_destroy(&arena)

	root := entity_arena_add_scope(&arena, .File, {})
	kinds := [?]Entity_Kind {
		.Variable,
		.Constant,
		.Type_Def,
		.Class,
		.Method,
		.Parameter,
		.Include,
		.Alias,
		.Builtin_Type,
	}
	names := [?]string {
		"GV_VALUE",
		"GC_LIMIT",
		"TY_ROW",
		"LCL_APP",
		"EXECUTE",
		"IV_VALUE",
		"ZINCLUDE",
		"IFACE_ALIAS",
		"I",
	}

	for kind, i in kinds {
		id := entity_arena_add_entity(&arena, kind, names[i], root)
		entity := entity_arena_entity(&arena, id)
		testing.expect(t, entity != nil)
		if entity != nil {
			testing.expect_value(t, entity.id, id)
			testing.expect_value(t, entity.kind, kind)
			testing.expect_value(t, entity.source_file, Source_File_Id(7))
			testing.expect(t, entity.name != names[i])
		}
	}

	builtin := entity_arena_find(&arena, "i", .Builtin_Type)
	testing.expect(t, builtin != nil)
	if builtin != nil {
		testing.expect(t, .Builtin in builtin.flags)
	}
}

@(test)
root_semantic_entity_arena_looks_up_scope_declarations :: proc(t: ^testing.T) {
	arena := entity_arena_make(context.allocator, Source_File_Id(1))
	defer entity_arena_destroy(&arena)

	root := entity_arena_add_scope(&arena, .File, {})
	value_id := entity_arena_add_entity(&arena, .Variable, "GV_VALUE", root)
	type_id := entity_arena_add_entity(&arena, .Type_Def, "TY_VALUE", root)
	routine_id := entity_arena_add_entity(&arena, .Form, "RUN", root)
	alias_id := entity_arena_add_entity(&arena, .Alias, "RENAMED", root)

	found_value, value_ok := entity_arena_lookup(&arena, root, .Value, "gv_value")
	found_type, type_ok := entity_arena_lookup(&arena, root, .Type, "ty_value")
	found_routine, routine_ok := entity_arena_lookup(&arena, root, .Routine, "run")
	_, alias_ok := entity_arena_lookup(&arena, root, .Value, "renamed")

	testing.expect(t, value_ok)
	testing.expect_value(t, found_value, value_id)
	testing.expect(t, type_ok)
	testing.expect_value(t, found_type, type_id)
	testing.expect(t, routine_ok)
	testing.expect_value(t, found_routine, routine_id)
	testing.expect(t, !alias_ok)
	testing.expect(t, !entity_arena_scope_has_declared(&arena, root, .Value, "renamed"))
	testing.expect(t, entity_arena_entity(&arena, alias_id) != nil)
}
