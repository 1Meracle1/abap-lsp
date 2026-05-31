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

check_project_operands :: proc(project: ^Project_Analysis, lookup: ^Project_Index) {
	for i in 0 ..< len(project.units) {
		check_unit_operands(project, lookup, i)
	}
}

check_project_operands_for_units :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_ids: []Unit_Id,
) {
	for unit_id in unit_ids {
		unit_index := unit_id_index(unit_id)
		if unit_index >= 0 && unit_index < len(project.units) {
			check_unit_operands(project, lookup, unit_index)
		}
	}
}

check_unit_operands :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
) {
	unit := &project.units[unit_index]
	write := 0
	for i in 0 ..< len(unit.operands) {
		operand := unit.operands[i]
		if !(.Syntax in operand.flags) {
			continue
		}
		if operand.has_symbol && operand.symbol.unit == unit.unit_id {
			if symbol(unit, operand.symbol.symbol) != nil {
				operand.type_fact = type_fact_from_symbol_handle(project, unit_index, operand.symbol)
			}
		}
		unit.operands[write] = operand
		write += 1
	}
	resize(&unit.operands, write)

	for ref in unit.references {
		add_reference_operand(project, unit_index, ref)
	}
	for access in unit.field_accesses {
		if access.in_type_position {
			continue
		}
		if fact, ok := resolve_field_access_tail(project, lookup, unit_index, access); ok {
			append_checked_operand(
				unit,
				access.scope,
				field_access_range(access),
				.Field,
				fact,
				assignable = true,
			)
		}
	}
	for site in unit.call_sites {
		if fact := call_result_type_fact(project, lookup, unit_index, site);
		   type_fact_is_known(fact) {
			append_checked_operand(
				unit,
				site.scope,
				site.range,
				.Value,
				fact,
			)
		}
	}
	for site in unit.assignment_sites {
		if site.lhs_range.end > site.lhs_range.start {
			append_checked_operand(
				unit,
				site.scope,
				site.lhs_range,
				.Variable,
				site.lhs,
				assignable = true,
			)
		}
		if site.rhs_range.end > site.rhs_range.start {
			append_checked_operand(
				unit,
				site.scope,
				site.rhs_range,
				.Value,
				site.rhs,
			)
		}
	}
}

append_checked_operand :: proc(
	unit: ^Unit_Analysis,
	scope: Scope_Id,
	range: tokenizer.Range,
	mode: Operand_Mode,
	type_fact: Type_Fact_Data,
	symbol := Symbol_Handle{},
	has_symbol := false,
	assignable := false,
) {
	flags := Operand_Flags{}
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

add_reference_operand :: proc(project: ^Project_Analysis, unit_index: int, ref: Reference_Data) {
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
			}
		case .Builtin_Routine:
			mode = .Routine
		case .Internal_Table_Line, .External:
			mode = .Value
		}
	}
	append_checked_operand(
		unit,
		ref.scope,
		ref.range,
		mode,
		type_fact,
		symbol_handle,
		has_symbol,
		assignable,
	)
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
