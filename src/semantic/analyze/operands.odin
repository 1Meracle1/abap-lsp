package abap_frontend_semantic_analyze

import "src:tokenizer"

add_syntax_operand :: proc(
	unit: ^Unit_Analysis,
	scope: Scope_Id,
	range: tokenizer.Range,
	mode: Operand_Mode,
	type_fact: Type_Fact_Data,
	symbol := Symbol_Handle{},
	has_symbol := false,
	assignable := false,
) {
	flags := Operand_Flags{.Syntax}
	if assignable {
		flags += {.Assignable}
	}
	append(
		&unit.operands,
		Operand_Data {
			scope = scope,
			range = range,
			mode = mode,
			type_fact = type_fact,
			symbol = symbol,
			has_symbol = has_symbol,
			flags = flags,
		},
	)
}

reference_operand :: proc(project: ^Project_Analysis, unit_index: int, ref: Reference_Data) -> Operand_Data {
	unit := &project.units[unit_index]
	mode := Operand_Mode.Unknown
	type_fact := unknown_type_fact()
	symbol_handle := Symbol_Handle{}
	has_symbol := false
	assignable := false

	if ref.has_resolution {
		switch ref.resolution.kind {
		case .Symbol:
			symbol_handle = ref.resolution.symbol
			has_symbol = true
			mode, assignable = operand_mode_from_symbol(project, symbol_handle)
			type_fact = type_fact_from_symbol_handle(project, unit_index, symbol_handle)
		case .Builtin_Type:
			mode = .Type
			type_fact = Type_Fact_Data {
				type_id = type_builtin(unit, ref.name),
				type_unit = unit.unit_id,
				structure = INVALID_STRUCTURE_ID,
				structure_unit = INVALID_UNIT_ID,
				declared_type = builtin_type_ref(ref.name),
				has_declared_type = true,
				type_clause_display = ref.name,
				confidence = .High,
			}
		case .Builtin_Routine:
			mode = .Routine
		case .Internal_Table_Line, .External:
			mode = .Value
		}
	}
	flags := Operand_Flags{}
	if assignable {
		flags += {.Assignable}
	}
	return Operand_Data {
		scope = ref.scope,
		range = ref.range,
		mode = mode,
		type_fact = type_fact,
		symbol = symbol_handle,
		has_symbol = has_symbol,
		flags = flags,
	}
}

operand_mode_from_symbol :: proc(
	project: ^Project_Analysis,
	handle: Symbol_Handle,
) -> (Operand_Mode, bool) {
	unit_index := unit_id_index(handle.unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return .Unknown, false
	}
	s := symbol(&project.units[unit_index], handle.symbol)
	if s == nil {
		return .Unknown, false
	}
	switch s.kind {
	case .Builtin_Type, .Type_Def, .Class, .Interface:
		return .Type, false
	case .Builtin_Routine, .Form, .Module, .Event:
		return .Routine, false
	case .Method:
		return .Method, false
	case .Builtin_Constant, .Constant, .Enum_Member:
		return .Constant, false
	case .Field:
		return .Field, true
	case .Builtin_Variable, .Variable, .Field_Symbol, .Parameter, .Exception, .Include, .Control, .Report:
		return .Variable, true
	case .Alias:
		return .Unknown, false
	}
	return .Unknown, false
}
