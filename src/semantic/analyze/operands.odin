package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

Ast_Expression_Info_Kind :: enum {
	Reference,
	Selector,
	Call_Result,
}

Ast_Expression_Info :: struct {
	scope:     Scope_Id,
	range:     tokenizer.Range,
	node:      ^ast.Node,
	kind:      Ast_Expression_Info_Kind,
	type_fact: Type_Fact_Data,
}

Operand_Mode :: enum {
	Invalid,
	Unknown,
	Value,
	Variable,
	Constant,
	Type,
	Routine,
	Method,
	Field,
}

Operand_Flag :: enum {
	Assignable,
	Syntax,
}
Operand_Flags :: bit_set[Operand_Flag]

Ast_Operand_Info :: struct {
	scope:      Scope_Id,
	range:      tokenizer.Range,
	node:       ^ast.Node,
	mode:       Operand_Mode,
	type_fact:  Type_Fact_Data,
	symbol:     Symbol_Link,
	has_symbol: bool,
	flags:      Operand_Flags,
}

reference_operand :: proc(project: ^Project_Analysis, source_file_index: int, ref: Reference_Data) -> Ast_Operand_Info {
	unit := &project.providers.source_files[source_file_index]
	mode := Operand_Mode.Unknown
	type_fact := unknown_type_fact()
	symbol_handle := Symbol_Link{}
	has_symbol := false
	assignable := false

	if ref.has_resolution {
		switch ref.resolution.kind {
		case .Symbol:
			symbol_handle = ref.resolution.symbol
			has_symbol = true
			mode, assignable = operand_mode_from_symbol(project, symbol_handle)
			type_fact = type_fact_from_symbol_handle(project, source_file_index, symbol_handle)
		case .Provider_Entity:
			#partial switch ref.namespace {
			case .Type:
				mode = .Type
			case .Routine:
				mode = .Routine
			case .Value:
				mode = .Value
			}
		case .Builtin_Type:
			mode = .Type
			type_fact = Type_Fact_Data {
				type_id = type_builtin(unit, ref.name),
				type_unit = unit.source_file_id,
				structure = INVALID_STRUCTURE_ID,
				structure_unit = INVALID_SOURCE_FILE_ID,
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
	if ref.node != nil {
		add_type_and_value(unit, ref.node, ref.scope, mode, type_fact, assignable = assignable)
	}
	return Ast_Operand_Info {
		scope = ref.scope,
		range = ref.range,
		node = ref.node,
		mode = mode,
		type_fact = type_fact,
		symbol = symbol_handle,
		has_symbol = has_symbol,
		flags = flags,
	}
}

operand_mode_from_symbol :: proc(
	project: ^Project_Analysis,
	handle: Symbol_Link,
) -> (Operand_Mode, bool) {
	source_file_index := source_file_id_index(handle.unit)
	if source_file_index < 0 || source_file_index >= len(project.providers.source_files) {
		return .Unknown, false
	}
	s := symbol(&project.providers.source_files[source_file_index], handle.symbol)
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
