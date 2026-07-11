package abap_frontend_ir

import "core:mem"
import "core:strings"

print_module :: proc(module: ^Module, allocator: mem.Allocator = context.allocator) -> string {
	out: strings.Builder
	strings.builder_init(&out, 0, 4096, allocator)
	for entry in module.entries {
		if entry == INVALID_FUNCTION_ID || int(entry) >= len(module.functions) {
			continue
		}
		strings.write_string(&out, "entry @")
		strings.write_string(&out, module.functions[int(entry)].name)
		strings.write_byte(&out, '\n')
	}
	if len(module.entries) > 0 && len(module.functions) > 0 {
		strings.write_byte(&out, '\n')
	}
	for &function, i in module.functions {
		if i > 0 {
			strings.write_byte(&out, '\n')
		}
		print_function(&out, module, &function, Function_Id(i))
	}
	return strings.to_string(out)
}

print_function :: proc(out: ^strings.Builder, module: ^Module, function: ^Function, id: Function_Id) {
	strings.write_string(out, "func @")
	if function.name != "" {
		strings.write_string(out, function.name)
	} else {
		strings.write_string(out, "function")
		strings.write_int(out, int(id))
	}
	if function.role != .Unknown {
		strings.write_string(out, " role=")
		strings.write_string(out, function_role_name(function.role))
	}
	strings.write_string(out, " -> (")
	for typ, i in function.return_types {
		if i > 0 {
			strings.write_string(out, ", ")
		}
		print_type(out, module, typ)
	}
	strings.write_string(out, ") {\n")

	for slot, i in function.slots {
		strings.write_string(out, "  slot %s")
		strings.write_int(out, i)
		strings.write_byte(out, ' ')
		strings.write_string(out, slot_kind_name(slot.kind))
		if slot.name != "" {
			strings.write_byte(out, ' ')
			strings.write_string(out, slot.name)
		}
		strings.write_string(out, " : ")
		print_type(out, module, slot.type)
		strings.write_byte(out, '\n')
	}

	for &block, i in function.blocks {
		print_block(out, module, function, &block, Block_Id(i))
	}
	strings.write_string(out, "}\n")
}

print_block :: proc(out: ^strings.Builder, module: ^Module, function: ^Function, block: ^Block, id: Block_Id) {
	print_block_ref(out, function, id)
	strings.write_byte(out, '(')
	for arg, i in block.args {
		if i > 0 {
			strings.write_string(out, ", ")
		}
		print_value(out, arg)
		value := value_ptr(function, arg)
		if value.name != "" {
			strings.write_string(out, " ")
			strings.write_string(out, value.name)
		}
		strings.write_string(out, " : ")
		print_type(out, module, value.type)
	}
	strings.write_string(out, "):\n")

	for instruction in block.instructions {
		op := op_ptr(function, Op_Id(instruction))
		strings.write_string(out, "  ")
		print_op(out, module, function, op^)
		strings.write_byte(out, '\n')
	}
	strings.write_string(out, "  ")
	print_terminator(out, function, block.terminator)
	strings.write_byte(out, '\n')
}

print_op :: proc(out: ^strings.Builder, module: ^Module, function: ^Function, op: Op) {
	if len(op.results) > 0 {
		for result, i in op.results {
			if i > 0 {
				strings.write_string(out, ", ")
			}
			print_typed_value(out, module, function, result)
		}
		strings.write_string(out, " = ")
	}
	print_instruction_head(out, module, function, op)
	if len(op.operands) > 0 {
		strings.write_string(out, " (")
		for operand, i in op.operands {
			if i > 0 {
				strings.write_string(out, ", ")
			}
			print_value(out, operand)
		}
		strings.write_byte(out, ')')
	}
	if .Unsupported in op.effects {
		strings.write_string(out, " [unsupported]")
	}
}

print_instruction_head :: proc(out: ^strings.Builder, module: ^Module, function: ^Function, op: Op) {
	#partial switch op.opcode {
	case .Const:
		strings.write_string(out, "const")
		if constant, ok := op.attrs.(Constant_Id); ok && constant != INVALID_CONSTANT_ID && int(constant) < len(module.constants) {
			strings.write_string(out, " ")
			print_literal(out, module, function, op, module.constants[int(constant)].literal)
		}
		return
	case .Addr_Of:
		strings.write_string(out, "addr_of")
		print_address_payload(out, function, op)
		return
	case .Field_Addr:
		strings.write_string(out, "field_addr")
		print_address_payload(out, function, op)
		return
	case .Load:
		strings.write_string(out, "load")
		return
	case .Store:
		strings.write_string(out, "store")
		return
	case .Cast:
		strings.write_string(out, "cast")
		return
	case .Call:
		strings.write_string(out, "call")
		if attrs, ok := op.attrs.(Call_Attrs); ok {
			print_core_call_target(out, module, attrs.target)
		}
		return
	case .Invoke:
		strings.write_string(out, "invoke")
		if op.intrinsic != INVALID_INTRINSIC_ID && int(op.intrinsic) < len(module.intrinsics) {
			intrinsic := module.intrinsics[int(op.intrinsic)]
			if intrinsic.name != "" {
				strings.write_string(out, " @")
				strings.write_string(out, intrinsic.name)
			}
				print_intrinsic_payload(out, module, intrinsic)
		} else if attrs, ok := op.attrs.(Call_Attrs); ok {
			print_core_call_target(out, module, attrs.target)
		}
		return
	case .Intrinsic:
		strings.write_string(out, "intrinsic")
		if op.intrinsic != INVALID_INTRINSIC_ID && int(op.intrinsic) < len(module.intrinsics) {
			intrinsic := module.intrinsics[int(op.intrinsic)]
			if intrinsic.name != "" {
				strings.write_string(out, " @")
				strings.write_string(out, intrinsic.name)
			}
				print_intrinsic_payload(out, module, intrinsic)
		}
		return
	case .Unsupported:
		strings.write_string(out, "unsupported")
		if attrs, ok := op.attrs.(Unsupported_Attrs); ok && attrs.message != "" {
			strings.write_string(out, " ")
			print_quoted(out, attrs.message)
		}
		return
	}
	strings.write_string(out, opcode_name(op.opcode))
}

print_address_payload :: proc(out: ^strings.Builder, function: ^Function, op: Op) {
	#partial switch attrs in op.attrs {
	case Slot_Address_Attrs:
		strings.write_string(out, " %s")
		strings.write_int(out, int(attrs.slot))
	case Projection_Id:
		print_projection_payload(out, function, attrs)
	}
}

print_intrinsic_payload :: proc(out: ^strings.Builder, module: ^Module, intrinsic: Intrinsic) {
	#partial switch payload in intrinsic.payload {
	case Intrinsic_System_Field_Payload:
		if payload.system_field != "" {
			strings.write_string(out, " .")
			strings.write_string(out, payload.system_field)
		}
	case Intrinsic_Call_Payload:
		if payload.callee_name != "" {
			strings.write_string(out, " @")
			strings.write_string(out, payload.callee_name)
		}
		if print_intrinsic_call_kind(intrinsic.op, payload.call_kind) {
			strings.write_string(out, " call=")
			strings.write_string(out, abap_call_kind_name(payload.call_kind))
		}
	case Intrinsic_Message_Payload:
		if payload.form != .Unknown {
			strings.write_string(out, " form=")
			strings.write_string(out, abap_message_form_name(payload.form))
		}
		if payload.id != "" {
			strings.write_string(out, " id=")
			strings.write_string(out, payload.id)
		}
		if payload.msg_type != "" {
			strings.write_string(out, " type=")
			strings.write_string(out, payload.msg_type)
		}
		if payload.number != "" {
			strings.write_string(out, " number=")
			strings.write_string(out, payload.number)
		}
		if payload.head_operands > 0 {
			strings.write_string(out, " head_operands=")
			strings.write_int(out, payload.head_operands)
		}
		if payload.arg_count > 0 {
			strings.write_string(out, " args=")
			strings.write_int(out, payload.arg_count)
		}
		if payload.has_into {
			strings.write_string(out, " into")
		}
		if payload.has_display_like {
			strings.write_string(out, " display_like")
			if payload.display_like != "" {
				strings.write_byte(out, '=')
				strings.write_string(out, payload.display_like)
			} else if payload.display_like_operand {
				strings.write_string(out, "=operand")
			}
		}
		if payload.has_raising {
			strings.write_string(out, " raising")
			if payload.raising != "" {
				strings.write_byte(out, '=')
				strings.write_string(out, payload.raising)
			} else if payload.raising_operand {
				strings.write_string(out, "=operand")
			}
		}
	case Intrinsic_Exception_Payload:
		if payload.exception_name != "" {
			strings.write_string(out, " exception=")
			strings.write_string(out, payload.exception_name)
		}
	case Intrinsic_String_Payload:
		if payload.has_separator {
			strings.write_string(out, " separator")
		}
		if payload.respecting_blanks {
			strings.write_string(out, " respecting_blanks")
		}
		if payload.no_gaps {
			strings.write_string(out, " no_gaps")
		}
		if payload.translate_mode != .Unknown {
			strings.write_string(out, " mode=")
			strings.write_string(out, abap_translate_mode_name(payload.translate_mode))
		}
		if payload.replace_occurrence != .Unknown {
			strings.write_string(out, " occurrence=")
			strings.write_string(out, abap_replace_occurrence_name(payload.replace_occurrence))
		}
		if payload.shift_direction != .Unknown {
			strings.write_string(out, " direction=")
			strings.write_string(out, abap_shift_direction_name(payload.shift_direction))
		}
		if payload.find_occurrence != .Unknown {
			strings.write_string(out, " occurrence=")
			strings.write_string(out, abap_find_occurrence_name(payload.find_occurrence))
		}
		if payload.find_ignoring_case {
			strings.write_string(out, " ignoring_case")
		}
	case Intrinsic_Table_Payload:
		if payload.access != .Unknown {
			strings.write_string(out, " access=")
			strings.write_string(out, table_access_kind_name(payload.access))
		}
		if payload.key_kind != .None {
			strings.write_string(out, " key=")
			strings.write_string(out, table_key_kind_name(payload.key_kind))
			if payload.key_name != "" {
				strings.write_byte(out, ':')
				strings.write_string(out, payload.key_name)
			}
		}
		if payload.result_kind != .None {
			strings.write_string(out, " result=")
			strings.write_string(out, table_result_kind_name(payload.result_kind))
		}
		if payload.source_kind != .Unknown {
			strings.write_string(out, " source=")
			strings.write_string(out, table_source_kind_name(payload.source_kind))
		}
		if payload.row_type != BUILTIN_TYPE_VOID {
			strings.write_string(out, " row=")
			print_type(out, module, payload.row_type)
		}
		if payload.component_count > 0 {
			strings.write_string(out, " components=")
			strings.write_int(out, payload.component_count)
		}
		if payload.binary_search {
			strings.write_string(out, " binary_search")
		}
		if payload.stable {
			strings.write_string(out, " stable")
		}
	case Intrinsic_SQL_Payload:
		if payload.source_kind != .Unknown {
			strings.write_string(out, " source=")
			strings.write_string(out, sql_source_kind_name(payload.source_kind))
			if payload.source_name != "" {
				strings.write_byte(out, ':')
				strings.write_string(out, payload.source_name)
			}
		}
		if payload.source_alias != "" {
			strings.write_string(out, " alias=")
			strings.write_string(out, payload.source_alias)
		}
		if payload.result_kind != .None {
			strings.write_string(out, " result=")
			strings.write_string(out, sql_result_kind_name(payload.result_kind))
		}
		if payload.row_type != BUILTIN_TYPE_VOID {
			strings.write_string(out, " row=")
			print_type(out, module, payload.row_type)
		}
		if payload.scalar_type != BUILTIN_TYPE_VOID {
			strings.write_string(out, " scalar=")
			print_type(out, module, payload.scalar_type)
		}
		if payload.source_count > 0 {
			strings.write_string(out, " sources=")
			strings.write_int(out, payload.source_count)
		}
		if payload.projection_count > 0 {
			strings.write_string(out, " projections=")
			strings.write_int(out, payload.projection_count)
		}
		if payload.assignment_count > 0 {
			strings.write_string(out, " assignments=")
			strings.write_int(out, payload.assignment_count)
		}
		if payload.single {
			strings.write_string(out, " single")
		}
		if payload.is_distinct {
			strings.write_string(out, " distinct")
		}
		if payload.for_all_entries {
			strings.write_string(out, " for_all_entries")
		}
		if payload.from_table {
			strings.write_string(out, " from_table")
		}
	case Intrinsic_Unsupported_Payload:
		if payload.message != "" {
			strings.write_string(out, " ")
			print_quoted(out, payload.message)
		}
	}
}

print_intrinsic_call_kind :: proc "contextless" (op: Intrinsic_Op, call_kind: Abap_Call_Kind) -> bool {
	if call_kind == .Unknown {
		return false
	}
	if op == .Call_Method && call_kind == .Method {
		return false
	}
	if op == .Call_Routine && call_kind == .Form {
		return false
	}
	return true
}

print_core_call_target :: proc(out: ^strings.Builder, module: ^Module, target: Function_Id) {
	if target == INVALID_FUNCTION_ID || int(target) >= len(module.functions) {
		return
	}
	strings.write_string(out, " @")
	strings.write_string(out, module.functions[int(target)].name)
}

print_terminator :: proc(out: ^strings.Builder, function: ^Function, terminator: Instruction_Id) {
	if terminator == INVALID_INSTRUCTION_ID || int(terminator) >= len(function.instructions) {
		strings.write_string(out, "<missing terminator>")
		return
	}
	term := function.instructions[int(terminator)]
	#partial switch term.opcode {
	case .Br:
		strings.write_string(out, "cf.br ")
		if len(term.successors) > 0 {
			print_block_ref(out, function, term.successors[0].target)
			print_value_list(out, term.successors[0].args[:])
		} else {
			print_block_ref(out, function, INVALID_BLOCK_ID)
			print_value_list(out, nil)
		}
	case .Cond_Br:
		strings.write_string(out, "cf.cond_br ")
		print_value(out, term.operands[0] if len(term.operands) > 0 else INVALID_VALUE_ID)
		strings.write_string(out, ", ")
		if len(term.successors) > 0 {
			print_block_ref(out, function, term.successors[0].target)
			print_value_list(out, term.successors[0].args[:])
		} else {
			print_block_ref(out, function, INVALID_BLOCK_ID)
			print_value_list(out, nil)
		}
		strings.write_string(out, ", ")
		if len(term.successors) > 1 {
			print_block_ref(out, function, term.successors[1].target)
			print_value_list(out, term.successors[1].args[:])
		} else {
			print_block_ref(out, function, INVALID_BLOCK_ID)
			print_value_list(out, nil)
		}
	case .Switch:
		strings.write_string(out, "cf.switch ")
		print_value(out, term.operands[0] if len(term.operands) > 0 else INVALID_VALUE_ID)
		strings.write_string(out, ", default ")
		if len(term.successors) > 0 {
			print_block_ref(out, function, term.successors[0].target)
			print_value_list(out, term.successors[0].args[:])
		} else {
			print_block_ref(out, function, INVALID_BLOCK_ID)
			print_value_list(out, nil)
		}
		strings.write_string(out, ", cases [")
		for edge, i in term.successors[1:] {
			if i > 0 {
				strings.write_string(out, ", ")
			}
			print_value(out, edge.case_value)
			strings.write_string(out, ": ")
			print_block_ref(out, function, edge.target)
			print_value_list(out, edge.args[:])
		}
		strings.write_byte(out, ']')
	case .Return:
		strings.write_string(out, "cf.return")
		print_value_list(out, term.operands[:])
	case .Unreachable:
		strings.write_string(out, "cf.unreachable")
	case:
		strings.write_string(out, opcode_name(term.opcode))
	}
}

print_value_list :: proc(out: ^strings.Builder, values: []Value_Id) {
	strings.write_byte(out, '(')
	for value, i in values {
		if i > 0 {
			strings.write_string(out, ", ")
		}
		print_value(out, value)
	}
	strings.write_byte(out, ')')
}

print_value :: proc(out: ^strings.Builder, value: Value_Id) {
	if value == INVALID_VALUE_ID {
		strings.write_string(out, "%invalid")
		return
	}
	strings.write_string(out, "%v")
	strings.write_int(out, int(value))
}

print_typed_value :: proc(out: ^strings.Builder, module: ^Module, function: ^Function, value: Value_Id) {
	print_value(out, value)
	strings.write_string(out, " : ")
	print_type(out, module, value_type(function, value))
}

print_block_ref :: proc(out: ^strings.Builder, function: ^Function, block: Block_Id) {
	if block == INVALID_BLOCK_ID || int(block) >= len(function.blocks) {
		strings.write_string(out, "^invalid")
		return
	}
	strings.write_string(out, "^b")
	strings.write_int(out, int(block))
	name := function.blocks[int(block)].name
	if name != "" {
		strings.write_byte(out, '.')
		strings.write_string(out, name)
	}
}

print_type :: proc(out: ^strings.Builder, module: ^Module, typ: Type_Id) {
	if typ == INVALID_TYPE_ID || int(typ) >= len(module.types) {
		strings.write_string(out, "invalid")
		return
	}
	record := type_ptr(module, typ)
	if record.name != "" {
		strings.write_string(out, record.name)
		return
	}
	strings.write_string(out, type_kind_name(record.kind))
}

print_quoted :: proc(out: ^strings.Builder, text: string) {
	strings.write_byte(out, '"')
	for ch in text {
		if ch == '"' || ch == '\\' {
			strings.write_byte(out, '\\')
		}
		strings.write_rune(out, ch)
	}
	strings.write_byte(out, '"')
}

print_projection_payload :: proc(out: ^strings.Builder, function: ^Function, projection: Projection_Id) {
	if int(projection) >= len(function.projections) {
		strings.write_string(out, " path=<invalid>")
		return
	}
	path := function.projections[int(projection)]
	if len(path.segments) == 0 {
		return
	}
	strings.write_string(out, " path=")
	for segment, i in path.segments {
		if i > 0 {
			strings.write_string(out, "/")
		}
		print_projection_segment(out, segment)
	}
}

print_projection_segment :: proc(out: ^strings.Builder, segment: Projection_Segment) {
	switch segment.selector {
	case .Dash:
		strings.write_byte(out, '-')
	case .Arrow:
		strings.write_string(out, "->")
	case .Fat_Arrow:
		strings.write_string(out, "=>")
	case .Tilde:
		strings.write_byte(out, '~')
	}
	strings.write_string(out, segment.name)
	if segment.field_index >= 0 {
		strings.write_byte(out, '#')
		strings.write_int(out, int(segment.field_index))
	}
}

print_literal :: proc(out: ^strings.Builder, module: ^Module, function: ^Function, op: Op, literal: string) {
	typ := INVALID_TYPE_ID
	if len(op.results) > 0 {
		typ = value_type(function, op.results[0])
	}
	if literal_type_prints_bare(module, typ) {
		strings.write_string(out, literal)
		return
	}
	if literal_type_prints_string(module, typ) {
		print_string_literal(out, literal)
		return
	}
	print_quoted(out, literal)
}

literal_type_prints_bare :: proc(module: ^Module, typ: Type_Id) -> bool {
	if typ == BUILTIN_TYPE_INTEGER || typ == BUILTIN_TYPE_PREDICATE {
		return true
	}
	if record, ok := module_type_record(module, typ); ok {
		return record.kind == .Integer || record.kind == .Predicate
	}
	return false
}

literal_type_prints_string :: proc(module: ^Module, typ: Type_Id) -> bool {
	if typ == BUILTIN_TYPE_STRING {
		return true
	}
	if record, ok := module_type_record(module, typ); ok {
		return record.kind == .String
	}
	return false
}

print_string_literal :: proc(out: ^strings.Builder, text: string) {
	if literal_has_delimiters(text, '\'') || literal_has_delimiters(text, '"') {
		strings.write_string(out, text)
		return
	}
	if literal_has_delimiters(text, '`') {
		print_quoted(out, text[1:len(text) - 1])
		return
	}
	print_quoted(out, text)
}

literal_has_delimiters :: proc "contextless" (text: string, delimiter: u8) -> bool {
	return len(text) >= 2 && text[0] == delimiter && text[len(text) - 1] == delimiter
}

slot_kind_name :: proc "contextless" (kind: Slot_Kind) -> string {
	switch kind {
	case .Local:
		return "local"
	case .Parameter:
		return "param"
	case .Global:
		return "global"
	case .Runtime:
		return "runtime"
	case .Instance:
		return "instance"
	case .Field:
		return "field"
	case .Table_Handle:
		return "table"
	case .Temporary:
		return "temp"
	}
	unreachable()
}

function_role_name :: proc "contextless" (role: Function_Role) -> string {
	#partial switch role {
	case .Unknown:
		return "unknown"
	case .Internal:
		return "internal"
	case .Report_Entry:
		return "report_entry"
	case .Report_Start:
		return "report_start"
	case .Load_Of_Program:
		return "load_of_program"
	case .Event:
		return "event"
	case .Report_Event:
		return "report_event"
	case .Form:
		return "form"
	case .Function_Module:
		return "function_module"
	case .Module:
		return "module"
	case .Method:
		return "method"
	case .Constructor:
		return "constructor"
	case .Class_Constructor:
		return "class_constructor"
	case .Test_Entry:
		return "test_entry"
	}
	unreachable()
}

type_kind_name :: proc "contextless" (kind: Type_Kind) -> string {
	#partial switch kind {
	case .Void:
		return "void"
	case .Token:
		return "token"
	case .World:
		return "world"
	case .Predicate:
		return "predicate"
	case .Integer:
		return "i"
	case .Decimal:
		return "decimal"
	case .Float:
		return "float"
	case .String:
		return "string"
	case .Char:
		return "char"
	case .Numc:
		return "numc"
	case .Bytes:
		return "bytes"
	case .Date:
		return "date"
	case .Time:
		return "time"
	case .Structure:
		return "structure"
	case .Struct:
		return "struct"
	case .Table:
		return "table"
	case .Table_Iterator:
		return "table_iter"
	case .Reference:
		return "ref"
	case .Pointer:
		return "ptr"
	case .Object:
		return "object"
	case .Interface:
		return "interface"
	case .Exception:
		return "exception"
	case .Routine:
		return "routine"
	case .Unknown:
		return "unknown"
	case .Semantic:
		return "semantic"
	}
	unreachable()
}

opcode_name :: proc "contextless" (opcode: Opcode) -> string {
	switch opcode {
	case .Const:
		return "const"
	case .Initial:
		return "initial"
	case .Null_Ref:
		return "null_ref"
	case .Global_Addr:
		return "global_addr"
	case .Function_Addr:
		return "function_addr"
	case .Add:
		return "add"
	case .Sub:
		return "sub"
	case .Mul:
		return "mul"
	case .Div:
		return "div"
	case .Mod:
		return "mod"
	case .Neg:
		return "neg"
	case .And:
		return "and"
	case .Or:
		return "or"
	case .Xor:
		return "xor"
	case .Not:
		return "not"
	case .Cmp:
		return "cmp"
	case .Select:
		return "select"
	case .Cast:
		return "cast"
	case .Int_Extend:
		return "int_extend"
	case .Int_Truncate:
		return "int_truncate"
	case .Ref_Cast:
		return "ref_cast"
	case .Addr_Cast:
		return "addr_cast"
	case .Alloca:
		return "alloca"
	case .Addr_Of:
		return "addr_of"
	case .Deref:
		return "deref"
	case .Field_Addr:
		return "field_addr"
	case .Index_Addr:
		return "index_addr"
	case .Table_Row_Addr:
		return "table_row_addr"
	case .Load:
		return "load"
	case .Store:
		return "store"
	case .Struct_Init:
		return "struct_init"
	case .Extract_Value:
		return "extract_value"
	case .Insert_Value:
		return "insert_value"
	case .Call:
		return "call"
	case .Invoke:
		return "invoke"
	case .Intrinsic:
		return "intrinsic"
	case .Br:
		return "br"
	case .Cond_Br:
		return "cond_br"
	case .Switch:
		return "switch"
	case .Return:
		return "return"
	case .Unreachable:
		return "unreachable"
	case .Trap:
		return "trap"
	case .Debug_Value:
		return "debug_value"
	case .Unsupported:
		return "unsupported"
	}
	unreachable()
}

abap_call_kind_name :: proc "contextless" (kind: Abap_Call_Kind) -> string {
	switch kind {
	case .Unknown:
		return "unknown"
	case .Builtin:
		return "builtin"
	case .Form:
		return "form"
	case .Function_Module:
		return "function"
	case .Method:
		return "method"
	case .Module:
		return "module"
	case .Routine:
		return "routine"
	}
	unreachable()
}

abap_message_form_name :: proc "contextless" (form: Abap_Message_Form) -> string {
	switch form {
	case .Unknown:
		return "unknown"
	case .Default:
		return "default"
	case .Compact:
		return "compact"
	case .Explicit:
		return "explicit"
	}
	unreachable()
}

abap_translate_mode_name :: proc "contextless" (mode: Abap_Translate_Mode) -> string {
	switch mode {
	case .Unknown:
		return "unknown"
	case .To_Upper:
		return "to_upper"
	case .To_Lower:
		return "to_lower"
	}
	unreachable()
}

abap_replace_occurrence_name :: proc "contextless" (occurrence: Abap_Replace_Occurrence) -> string {
	switch occurrence {
	case .Unknown:
		return "unknown"
	case .First:
		return "first"
	case .All:
		return "all"
	}
	unreachable()
}

abap_shift_direction_name :: proc "contextless" (direction: Abap_Shift_Direction) -> string {
	switch direction {
	case .Unknown:
		return "unknown"
	case .Left:
		return "left"
	case .Right:
		return "right"
	}
	unreachable()
}

abap_find_occurrence_name :: proc "contextless" (occurrence: Abap_Find_Occurrence) -> string {
	switch occurrence {
	case .Unknown:
		return "unknown"
	case .First:
		return "first"
	case .All:
		return "all"
	}
	unreachable()
}

table_access_kind_name :: proc "contextless" (kind: Table_Access_Kind) -> string {
	switch kind {
	case .Unknown:
		return "unknown"
	case .Sequential:
		return "sequential"
	case .Index:
		return "index"
	case .Key:
		return "key"
	case .Table_Key:
		return "table_key"
	case .Where:
		return "where"
	case .Full:
		return "full"
	case .Sort:
		return "sort"
	}
	unreachable()
}

table_key_kind_name :: proc "contextless" (kind: Table_Key_Kind) -> string {
	switch kind {
	case .None:
		return "none"
	case .Primary:
		return "primary"
	case .Named:
		return "named"
	case .Dynamic:
		return "dynamic"
	case .Free:
		return "free"
	case .Table:
		return "table"
	}
	unreachable()
}

table_result_kind_name :: proc "contextless" (kind: Table_Result_Kind) -> string {
	switch kind {
	case .None:
		return "none"
	case .Value:
		return "value"
	case .Into:
		return "into"
	case .Assigning:
		return "assigning"
	case .Reference_Into:
		return "reference_into"
	case .No_Fields:
		return "no_fields"
	}
	unreachable()
}

table_source_kind_name :: proc "contextless" (kind: Table_Source_Kind) -> string {
	switch kind {
	case .Unknown:
		return "unknown"
	case .Row:
		return "row"
	case .Lines_Of:
		return "lines_of"
	case .Initial_Line:
		return "initial_line"
	case .From_Table:
		return "from_table"
	}
	unreachable()
}

sql_source_kind_name :: proc "contextless" (kind: Sql_Source_Kind) -> string {
	switch kind {
	case .Unknown:
		return "unknown"
	case .Resolved:
		return "resolved"
	case .Internal:
		return "internal"
	case .Dynamic:
		return "dynamic"
	case .Unresolved:
		return "unresolved"
	}
	unreachable()
}

sql_result_kind_name :: proc "contextless" (kind: Sql_Result_Kind) -> string {
	switch kind {
	case .None:
		return "none"
	case .Into:
		return "into"
	case .Into_Table:
		return "into_table"
	case .Appending:
		return "appending"
	case .Appending_Table:
		return "appending_table"
	}
	unreachable()
}
