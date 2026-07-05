package abap_frontend_ir

import "core:mem"
import "core:strings"

print_module :: proc(module: ^Module, allocator: mem.Allocator = context.allocator) -> string {
	out: strings.Builder
	strings.builder_init(&out, 0, 4096, allocator)
	assert(module != nil)
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
	for param, i in block.params {
		if i > 0 {
			strings.write_string(out, ", ")
		}
		print_value(out, param.value)
		if param.name != "" {
			strings.write_string(out, " ")
			strings.write_string(out, param.name)
		}
		strings.write_string(out, " : ")
		print_type(out, module, value_type(function, param.value))
	}
	strings.write_string(out, "):\n")

	for op in block.ops {
		strings.write_string(out, "  ")
		print_op(out, module, function, op)
		strings.write_byte(out, '\n')
	}
	strings.write_string(out, "  ")
	print_terminator(out, function, block.term)
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
	strings.write_string(out, op_kind_name(op.kind))
	if op_kind_uses_slot(op.kind) {
		strings.write_string(out, " %s")
		strings.write_int(out, int(op.payload.slot))
	}
	if op.payload.field_name != "" {
		strings.write_string(out, " .")
		strings.write_string(out, op.payload.field_name)
	}
	if op.payload.has_projection {
		print_projection_payload(out, function, op.payload.projection)
	}
	if op.payload.system_field != "" {
		strings.write_string(out, " .")
		strings.write_string(out, op.payload.system_field)
	}
	if op.payload.literal != "" {
		strings.write_string(out, " ")
		print_literal(out, module, function, op)
	}
	if op.kind == .Core_Call && op.payload.has_call_function_target {
		print_core_call_target(out, module, op)
	} else if op.payload.callee_name != "" {
		strings.write_string(out, " @")
		strings.write_string(out, op.payload.callee_name)
	}
	if print_op_call_kind(op.kind, op.payload.call_kind) {
		strings.write_string(out, " call=")
		strings.write_string(out, abap_call_kind_name(op.payload.call_kind))
	}
	if op.payload.message_form != .Unknown {
		strings.write_string(out, " form=")
		strings.write_string(out, abap_message_form_name(op.payload.message_form))
	}
	if op.payload.message_id != "" {
		strings.write_string(out, " id=")
		strings.write_string(out, op.payload.message_id)
	}
	if op.payload.message_type != "" {
		strings.write_string(out, " type=")
		strings.write_string(out, op.payload.message_type)
	}
	if op.payload.message_number != "" {
		strings.write_string(out, " number=")
		strings.write_string(out, op.payload.message_number)
	}
	if op.payload.message_head_operands > 0 {
		strings.write_string(out, " head_operands=")
		strings.write_int(out, op.payload.message_head_operands)
	}
	if op.payload.message_arg_count > 0 {
		strings.write_string(out, " args=")
		strings.write_int(out, op.payload.message_arg_count)
	}
	if op.payload.message_has_into {
		strings.write_string(out, " into")
	}
	if op.payload.message_has_display_like {
		strings.write_string(out, " display_like")
		if op.payload.message_display_like != "" {
			strings.write_byte(out, '=')
			strings.write_string(out, op.payload.message_display_like)
		} else if op.payload.message_display_like_operand {
			strings.write_string(out, "=operand")
		}
	}
	if op.payload.message_has_raising {
		strings.write_string(out, " raising")
		if op.payload.message_raising != "" {
			strings.write_byte(out, '=')
			strings.write_string(out, op.payload.message_raising)
		} else if op.payload.message_raising_operand {
			strings.write_string(out, "=operand")
		}
	}
	if op.payload.table_access != .Unknown {
		strings.write_string(out, " access=")
		strings.write_string(out, table_access_kind_name(op.payload.table_access))
	}
	if op.payload.table_key_kind != .None {
		strings.write_string(out, " key=")
		strings.write_string(out, table_key_kind_name(op.payload.table_key_kind))
		if op.payload.table_key_name != "" {
			strings.write_byte(out, ':')
			strings.write_string(out, op.payload.table_key_name)
		}
	}
	if op.payload.table_result_kind != .None {
		strings.write_string(out, " result=")
		strings.write_string(out, table_result_kind_name(op.payload.table_result_kind))
	}
	if op.payload.table_source_kind != .Unknown {
		strings.write_string(out, " source=")
		strings.write_string(out, table_source_kind_name(op.payload.table_source_kind))
	}
	if op.payload.table_row_type != BUILTIN_TYPE_VOID {
		strings.write_string(out, " row=")
		print_type(out, module, op.payload.table_row_type)
	}
	if op.payload.table_component_count > 0 {
		strings.write_string(out, " components=")
		strings.write_int(out, op.payload.table_component_count)
	}
	if op.payload.table_binary_search {
		strings.write_string(out, " binary_search")
	}
	if op.payload.table_stable {
		strings.write_string(out, " stable")
	}
	if op.payload.sql_source_kind != .Unknown {
		strings.write_string(out, " source=")
		strings.write_string(out, sql_source_kind_name(op.payload.sql_source_kind))
		if op.payload.sql_source_name != "" {
			strings.write_byte(out, ':')
			strings.write_string(out, op.payload.sql_source_name)
		}
	}
	if op.payload.sql_source_alias != "" {
		strings.write_string(out, " alias=")
		strings.write_string(out, op.payload.sql_source_alias)
	}
	if op.payload.sql_result_kind != .None {
		strings.write_string(out, " result=")
		strings.write_string(out, sql_result_kind_name(op.payload.sql_result_kind))
	}
	if op.payload.sql_row_type != BUILTIN_TYPE_VOID {
		strings.write_string(out, " row=")
		print_type(out, module, op.payload.sql_row_type)
	}
	if op.payload.sql_scalar_type != BUILTIN_TYPE_VOID {
		strings.write_string(out, " scalar=")
		print_type(out, module, op.payload.sql_scalar_type)
	}
	if op.payload.sql_source_count > 0 {
		strings.write_string(out, " sources=")
		strings.write_int(out, op.payload.sql_source_count)
	}
	if op.payload.sql_projection_count > 0 {
		strings.write_string(out, " projections=")
		strings.write_int(out, op.payload.sql_projection_count)
	}
	if op.payload.sql_assignment_count > 0 {
		strings.write_string(out, " assignments=")
		strings.write_int(out, op.payload.sql_assignment_count)
	}
	if op.payload.sql_single {
		strings.write_string(out, " single")
	}
	if op.payload.sql_distinct {
		strings.write_string(out, " distinct")
	}
	if op.payload.sql_for_all_entries {
		strings.write_string(out, " for_all_entries")
	}
	if op.payload.sql_from_table {
		strings.write_string(out, " from_table")
	}
	if print_op_call_target(op.kind) && op.payload.call_target != nil && op.payload.call_target.name != "" {
		strings.write_string(out, " target=")
		strings.write_string(out, op.payload.call_target.name)
	}
	if op.payload.unsupported_message != "" {
		strings.write_string(out, " ")
		print_quoted(out, op.payload.unsupported_message)
	}
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
	if .Unsupported in op.flags {
		strings.write_string(out, " [unsupported]")
	}
}

print_op_call_kind :: proc "contextless" (op_kind: Op_Kind, call_kind: Abap_Call_Kind) -> bool {
	if call_kind == .Unknown {
		return false
	}
	if op_kind == .Abap_Method_Call && call_kind == .Method {
		return false
	}
	if op_kind == .Abap_Routine_Call && call_kind == .Form {
		return false
	}
	return true
}

print_op_call_target :: proc "contextless" (op_kind: Op_Kind) -> bool {
	return op_kind != .Abap_Method_Call && op_kind != .Abap_Routine_Call
}

print_core_call_target :: proc(out: ^strings.Builder, module: ^Module, op: Op) {
	target := op.payload.call_function_target
	if target == INVALID_FUNCTION_ID || int(target) >= len(module.functions) {
		return
	}
	strings.write_string(out, " @")
	strings.write_string(out, module.functions[int(target)].name)
}

print_terminator :: proc(out: ^strings.Builder, function: ^Function, term: Terminator) {
	switch term.kind {
	case .Invalid:
		strings.write_string(out, "<missing terminator>")
	case .Branch:
		strings.write_string(out, "cf.br ")
		print_block_ref(out, function, term.target)
		print_value_list(out, term.target_args[:])
	case .Cond_Branch:
		strings.write_string(out, "cf.cond_br ")
		print_value(out, term.condition)
		strings.write_string(out, ", ")
		print_block_ref(out, function, term.true_target)
		print_value_list(out, term.true_args[:])
		strings.write_string(out, ", ")
		print_block_ref(out, function, term.false_target)
		print_value_list(out, term.false_args[:])
	case .Return:
		strings.write_string(out, "cf.return")
		print_value_list(out, term.values[:])
	case .Unreachable:
		strings.write_string(out, "cf.unreachable")
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
	if block == INVALID_BLOCK_ID || function == nil || int(block) >= len(function.blocks) {
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
		strings.write_string(out, "!invalid")
		return
	}
	record := type_ptr(module, typ)
	if record.name != "" {
		strings.write_string(out, "!")
		strings.write_string(out, record.name)
		return
	}
	strings.write_string(out, "!")
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

print_literal :: proc(out: ^strings.Builder, module: ^Module, function: ^Function, op: Op) {
	typ := INVALID_TYPE_ID
	if len(op.results) > 0 {
		typ = value_type(function, op.results[0])
	}
	if literal_type_prints_bare(module, typ) {
		strings.write_string(out, op.payload.literal)
		return
	}
	if literal_type_prints_string(module, typ) {
		print_string_literal(out, op.payload.literal)
		return
	}
	print_quoted(out, op.payload.literal)
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
	switch role {
	case .Unknown:
		return "unknown"
	case .Report_Entry:
		return "report_entry"
	case .Load_Of_Program:
		return "load_of_program"
	case .Event:
		return "event"
	case .Form:
		return "form"
	case .Function_Module:
		return "function_module"
	case .Module:
		return "module"
	case .Method:
		return "method"
	}
	unreachable()
}

type_kind_name :: proc "contextless" (kind: Type_Kind) -> string {
	switch kind {
	case .Void:
		return "void"
	case .World:
		return "world"
	case .Predicate:
		return "predicate"
	case .Integer:
		return "i"
	case .String:
		return "string"
	case .Structure:
		return "structure"
	case .Table:
		return "table"
	case .Table_Iterator:
		return "table_iter"
	case .Reference:
		return "ref"
	case .Routine:
		return "routine"
	case .Unknown:
		return "unknown"
	case .Semantic:
		return "semantic"
	}
	unreachable()
}

op_kind_name :: proc "contextless" (kind: Op_Kind) -> string {
	switch kind {
	case .Core_Const:
		return "core.const"
	case .Core_Load:
		return "core.load"
	case .Core_Store:
		return "core.store"
	case .Core_Field_Load:
		return "core.field_load"
	case .Core_Field_Store:
		return "core.field_store"
	case .Core_Cast:
		return "core.cast"
	case .Core_Call:
		return "core.call"
	case .Core_Unsupported:
		return "core.unsupported"
	case .Abap_Move:
		return "abap.move"
	case .Abap_Add:
		return "abap.add"
	case .Abap_Subtract:
		return "abap.sub"
	case .Abap_Multiply:
		return "abap.mul"
	case .Abap_Divide:
		return "abap.div"
	case .Abap_Equal:
		return "abap.eq"
	case .Abap_Not_Equal:
		return "abap.ne"
	case .Abap_Less:
		return "abap.lt"
	case .Abap_Less_Equal:
		return "abap.le"
	case .Abap_Greater:
		return "abap.gt"
	case .Abap_Greater_Equal:
		return "abap.ge"
	case .Abap_And:
		return "abap.and"
	case .Abap_Or:
		return "abap.or"
	case .Abap_Not:
		return "abap.not"
	case .Abap_Is_Initial:
		return "abap.is_initial"
	case .Abap_String_Concat:
		return "abap.concat"
	case .Abap_String_Template:
		return "abap.string_template"
	case .Abap_Construct:
		return "abap.construct"
	case .Abap_Builtin_Call:
		return "abap.builtin_call"
	case .Abap_Routine_Call:
		return "abap.routine_call"
	case .Abap_Method_Call:
		return "abap.method_call"
	case .Abap_Message:
		return "abap.message"
	case .Abap_Write:
		return "abap.write"
	case .Abap_Clear:
		return "abap.clear"
	case .Abap_Refresh:
		return "abap.refresh"
	case .Abap_Free:
		return "abap.free"
	case .Abap_Assign_Field:
		return "abap.assign_field"
	case .Abap_Unassign:
		return "abap.unassign"
	case .Table_Iter:
		return "table.iter"
	case .Table_Next:
		return "table.next"
	case .Table_Read:
		return "table.read"
	case .Table_Append:
		return "table.append"
	case .Table_Insert:
		return "table.insert"
	case .Table_Modify:
		return "table.modify"
	case .Table_Delete:
		return "table.delete"
	case .Table_Sort:
		return "table.sort"
	case .Table_Length:
		return "table.length"
	case .Sql_Select:
		return "sql.select"
	case .Sql_Open_Cursor:
		return "sql.open_cursor"
	case .Sql_Fetch:
		return "sql.fetch"
	case .Sql_Close_Cursor:
		return "sql.close_cursor"
	case .Sql_Insert:
		return "sql.insert"
	case .Sql_Update:
		return "sql.update"
	case .Sql_Delete:
		return "sql.delete"
	case .Sql_Modify:
		return "sql.modify"
	case .System_Read:
		return "system.read"
	case .System_Write:
		return "system.write"
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

op_kind_uses_slot :: proc "contextless" (kind: Op_Kind) -> bool {
	#partial switch kind {
	case .Core_Load, .Core_Store:
		return true
	}
	return false
}
