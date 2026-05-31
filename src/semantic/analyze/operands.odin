package abap_frontend_semantic_analyze

import "src:tokenizer"

add_syntax_operand :: proc(
	unit: ^Unit_Analysis,
	scope: Scope_Id,
	range: tokenizer.Range,
	mode: Operand_Mode,
	type_id := UNKNOWN_TYPE_ID,
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
			type_id = type_id,
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
			if s := symbol(unit, operand.symbol.symbol); s != nil {
				operand.type_id = s.type_id
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
				type_id_from_type_fact(unit, access.scope, fact),
				assignable = true,
			)
		}
	}
	for site in unit.call_sites {
		if fact := call_result_type_fact(project, lookup, unit_index, site);
		   operand_type_fact_known(fact) {
			append_checked_operand(
				unit,
				site.scope,
				site.range,
				.Value,
				type_id_from_type_fact(unit, site.scope, fact),
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
				type_id_from_type_fact(unit, site.scope, site.lhs),
				assignable = true,
			)
		}
		if site.rhs_range.end > site.rhs_range.start {
			append_checked_operand(
				unit,
				site.scope,
				site.rhs_range,
				.Value,
				type_id_from_type_fact(unit, site.scope, site.rhs),
			)
		}
	}
}

operand_type_fact_known :: #force_inline proc(fact: Type_Fact_Data) -> bool {
	return type_id_is_known(fact.type_id) || type_fact_is_known(fact)
}

append_checked_operand :: proc(
	unit: ^Unit_Analysis,
	scope: Scope_Id,
	range: tokenizer.Range,
	mode: Operand_Mode,
	type_id := UNKNOWN_TYPE_ID,
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
			type_id = type_id,
			symbol = symbol,
			has_symbol = has_symbol,
			flags = flags,
		},
	)
}

add_reference_operand :: proc(project: ^Project_Analysis, unit_index: int, ref: Reference_Data) {
	unit := &project.units[unit_index]
	mode := Operand_Mode.Unknown
	type_id := UNKNOWN_TYPE_ID
	symbol_handle := Symbol_Handle{}
	has_symbol := false
	assignable := false

	if ref.has_resolution {
		switch ref.resolution.kind {
		case .Symbol:
			symbol_handle = ref.resolution.symbol
			has_symbol = true
			mode, assignable = operand_mode_from_symbol(project, symbol_handle)
			type_id = type_id_from_symbol_operand(project, unit_index, ref.scope, symbol_handle)
		case .Builtin_Type:
			mode = .Type
			type_id = type_builtin(unit, ref.name)
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
		type_id,
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

type_id_from_symbol_operand :: proc(
	project: ^Project_Analysis,
	site_unit_index: int,
	scope: Scope_Id,
	handle: Symbol_Handle,
) -> Type_Id {
	fact := type_fact_from_symbol_handle(project, site_unit_index, handle)
	return type_id_from_type_fact(&project.units[site_unit_index], scope, fact)
}

type_id_from_type_fact :: proc(unit: ^Unit_Analysis, scope: Scope_Id, fact: Type_Fact_Data) -> Type_Id {
	if type_id_is_known(fact.type_id) {
		return fact.type_id
	}
	if fact.structure != INVALID_STRUCTURE_ID {
		return type_structure(unit, fact.structure)
	}
	if fact.has_declared_type {
		return type_id_from_declared_type(unit, scope, fact.declared_type)
	}
	return UNKNOWN_TYPE_ID
}
