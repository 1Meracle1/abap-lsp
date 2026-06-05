package abap_frontend_semantic_analyze

import "src:ast"

semantic_node_from_expr :: #force_inline proc(expr: ^ast.Expr) -> ^ast.Node {
	if expr == nil {
		return nil
	}
	return &expr.expr_base
}

semantic_node_from_stmt :: #force_inline proc(stmt: ^ast.Stmt) -> ^ast.Node {
	if stmt == nil {
		return nil
	}
	return &stmt.stmt_base
}

semantic_node_from_decl :: #force_inline proc(decl: ^ast.Decl) -> ^ast.Node {
	if decl == nil {
		return nil
	}
	return semantic_node_from_stmt(&decl.decl_base)
}

semantic_provider_for_source_file :: proc(source_file_id: Source_File_Id) -> ast.Provider_Handle {
	return semantic_ast_provider_handle(provider_handle_for_source_file(source_file_id))
}

semantic_ast_provider_handle :: proc(provider: Provider_Handle) -> ast.Provider_Handle {
	#partial switch provider.kind {
	case .Builtin:
		return ast.Provider_Handle {
			kind     = .Builtin,
			id       = ast.Provider_Id(u32(provider.id)),
			revision = provider.revision,
		}
	case .File:
		return ast.Provider_Handle {
			kind     = .File,
			id       = ast.Provider_Id(u32(provider.id)),
			revision = provider.revision,
		}
	case .Summary_Provider:
		return ast.Provider_Handle {
			kind     = .Summary_Provider,
			id       = ast.Provider_Id(u32(provider.id)),
			revision = provider.revision,
		}
	}
	return ast.Provider_Handle{}
}

semantic_ast_scope_handle :: proc(handle: Scope_Handle) -> ast.Scope_Handle {
	if !provider_handle_is_valid(handle.provider) || handle.id == INVALID_SCOPE_ID {
		return ast.Scope_Handle{}
	}
	return ast.Scope_Handle {
		provider = semantic_ast_provider_handle(handle.provider),
		id       = ast.Scope_Id(u32(handle.id)),
	}
}

semantic_ast_entity_handle :: proc(handle: Entity_Handle) -> ast.Entity_Handle {
	if !provider_handle_is_valid(handle.provider) || handle.id == INVALID_SYMBOL_ID {
		return ast.Entity_Handle{}
	}
	return ast.Entity_Handle {
		provider = semantic_ast_provider_handle(handle.provider),
		id       = ast.Entity_Id(u32(handle.id)),
	}
}

semantic_ast_decl_handle :: proc(handle: Decl_Handle) -> ast.Decl_Handle {
	if !provider_handle_is_valid(handle.provider) || handle.id == INVALID_DECL_INFO_ID {
		return ast.Decl_Handle{}
	}
	return ast.Decl_Handle {
		provider = semantic_ast_provider_handle(handle.provider),
		id       = ast.Decl_Id(u32(handle.id)),
	}
}

semantic_ast_type_handle :: proc(handle: Type_Handle) -> ast.Type_Handle {
	if handle.id == UNKNOWN_TYPE_ID {
		return ast.Type_Handle{id = ast.UNKNOWN_TYPE_ID}
	}
	if !provider_handle_is_valid(handle.provider) || handle.id == INVALID_TYPE_ID {
		return ast.Type_Handle{}
	}
	return ast.Type_Handle {
		provider = semantic_ast_provider_handle(handle.provider),
		id       = ast.Type_Id(u32(handle.id)),
	}
}

semantic_scope_handle :: proc(source_file_id: Source_File_Id, scope_id: Scope_Id) -> ast.Scope_Handle {
	return semantic_ast_scope_handle(scope_handle_for_source_file(source_file_id, scope_id))
}

semantic_entity_handle :: proc(unit: ^Source_File_Provider, handle: Symbol_Link) -> ast.Entity_Handle {
	entity, ok := entity_handle_from_source_file_symbol_handle(unit, handle)
	if !ok {
		return ast.Entity_Handle{}
	}
	return semantic_ast_entity_handle(entity)
}

semantic_type_handle :: proc(unit: ^Source_File_Provider, fact: Type_Fact_Data) -> ast.Type_Handle {
	return semantic_ast_type_handle(type_handle_from_source_file_fact(unit, fact))
}

semantic_use_handle :: proc(source_file_id: Source_File_Id, reference_id: Reference_Id) -> ast.Use_Handle {
	if source_file_id == INVALID_SOURCE_FILE_ID || reference_id == INVALID_REFERENCE_ID {
		return ast.Use_Handle{}
	}
	return ast.Use_Handle {
		file = ast.File_Id(u32(source_file_id)),
		id   = ast.Use_Id(u32(reference_id)),
	}
}

add_declaration_semantics :: proc(
	unit: ^Source_File_Provider,
	node: ^ast.Node,
	scope: Scope_Id,
	symbol_id: Symbol_Id,
	decl_info_id: Decl_Info_Id,
) {
	if unit == nil || node == nil {
		return
	}
	if scope != INVALID_SCOPE_ID {
		node.sem.scope = semantic_scope_handle(unit.source_file_id, scope)
		node.sem.flags += {.Has_Scope}
	}
	if symbol_id != INVALID_SYMBOL_ID {
		if entity, ok := entity_handle_from_source_file_symbol(unit, symbol_id); ok {
			node.sem.entity = semantic_ast_entity_handle(entity)
			node.sem.flags += {.Has_Entity}
		}
	}
	if decl_info_id != INVALID_DECL_INFO_ID {
		node.sem.decl = semantic_ast_decl_handle(decl_handle_for_source_file(unit.source_file_id, decl_info_id))
		node.sem.flags += {.Has_Decl}
	}
}

addressing_mode_from_operand_mode :: proc(mode: Operand_Mode) -> ast.Addressing_Mode {
	#partial switch mode {
	case .Value:
		return .Value
	case .Variable:
		return .Variable
	case .Constant:
		return .Constant
	case .Type:
		return .Type
	case .Routine:
		return .Routine
	case .Method:
		return .Method
	case .Field:
		return .Field
	case .Unknown:
		return .No_Value
	case .Invalid:
		return .Invalid
	}
	return .Invalid
}

operand_mode_from_addressing_mode :: proc(mode: ast.Addressing_Mode) -> Operand_Mode {
	#partial switch mode {
	case .Value, .Table_Line, .Optional_Ok:
		return .Value
	case .Variable:
		return .Variable
	case .Constant:
		return .Constant
	case .Type:
		return .Type
	case .Routine:
		return .Routine
	case .Method:
		return .Method
	case .Field:
		return .Field
	case .No_Value:
		return .Unknown
	case .Invalid:
		return .Invalid
	}
	return .Unknown
}

add_type_and_value :: proc(
	unit: ^Source_File_Provider,
	node: ^ast.Node,
	scope: Scope_Id,
	mode: Operand_Mode,
	fact: Type_Fact_Data,
	assignable := false,
	lhs := false,
) {
	if unit == nil || node == nil {
		return
	}
	if scope != INVALID_SCOPE_ID {
		node.sem.scope = semantic_scope_handle(unit.source_file_id, scope)
		node.sem.flags += {.Has_Scope}
	}
	flags := ast.Type_And_Value_Flags{}
	if assignable {
		flags += {.Assignable}
		node.sem.flags += {.Assignable}
	}
	if lhs {
		flags += {.Is_LHS}
		node.sem.flags += {.Is_LHS}
	}
	if type_fact_is_high_confidence(fact) {
		flags += {.High_Confidence}
	}
	if !type_fact_is_known(fact) {
		flags += {.Untyped}
	}
	node.sem.tav = ast.Type_And_Value {
		type  = semantic_type_handle(unit, fact),
		mode  = addressing_mode_from_operand_mode(mode),
		value = ast.INVALID_EXACT_VALUE_ID,
		flags = flags,
	}
	node.sem.flags += {.Has_Type_And_Value}
}

add_entity_use :: proc(
	unit: ^Source_File_Provider,
	node: ^ast.Node,
	scope: Scope_Id,
	reference_id: Reference_Id,
	resolution: Resolution,
	has_resolution: bool,
) {
	if unit == nil || node == nil {
		return
	}
	if scope != INVALID_SCOPE_ID {
		node.sem.scope = semantic_scope_handle(unit.source_file_id, scope)
		node.sem.flags += {.Has_Scope}
	}
	node.sem.use = semantic_use_handle(unit.source_file_id, reference_id)
	node.sem.flags += {.Has_Use}
	if has_resolution && resolution.kind == .Symbol {
		node.sem.entity = semantic_entity_handle(unit, resolution.symbol)
		node.sem.flags += {.Has_Entity}
	} else if has_resolution && resolution.kind == .Provider_Entity {
		node.sem.entity = semantic_ast_entity_handle(resolution.entity)
		node.sem.flags += {.Has_Entity}
	}
}

type_fact_from_type_and_value :: proc(tav: ast.Type_And_Value) -> Type_Fact_Data {
	if tav.type.id == ast.UNKNOWN_TYPE_ID || tav.type.id == ast.INVALID_TYPE_ID {
		return unknown_type_fact()
	}
	source_file_id := INVALID_SOURCE_FILE_ID
	if tav.type.provider.kind == .File || tav.type.provider.kind == .Summary_Provider {
		source_file_id = Source_File_Id(u32(tav.type.provider.id))
	}
	return Type_Fact_Data {
		type_id        = Type_Id(u32(tav.type.id)),
		type_unit      = source_file_id,
		structure      = INVALID_STRUCTURE_ID,
		structure_unit = INVALID_SOURCE_FILE_ID,
		confidence     = .High if .High_Confidence in tav.flags else .Low,
	}
}
