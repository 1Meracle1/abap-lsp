package abap_frontend_runtime

import "core:mem"
import "core:slice"
import "core:strconv"
import "core:strings"

Run_Status :: enum {
	Completed,
	Trapped,
}

Trap_Kind :: enum {
	None,
	Invalid_Module,
	Invalid_Function,
	Invalid_Instruction,
	Unsupported,
	Type,
	Divide_By_Zero,
	Step_Limit,
}

Value_Kind :: enum {
	Initial,
	World,
	Integer,
	String,
	Structure,
	Object,
	Predicate,
	Table,
	Table_Iterator,
}

IO_Event_Kind :: enum {
	Write,
	Message,
}

Arithmetic_Kind :: enum {
	Add,
	Subtract,
	Multiply,
	Divide,
}

Comparison_Kind :: enum {
	Equal,
	Not_Equal,
	Less,
	Less_Equal,
	Greater,
	Greater_Equal,
}

Table_Operation :: enum {
	Iter,
	Next,
	Read,
	Modify,
	Delete,
	Append,
	Insert,
	Collect,
	Loop,
	Sort,
}

SQL_Operation :: enum {
	Select,
	Open_Cursor,
	Fetch,
	Close_Cursor,
	Modify,
	Delete,
	Insert,
	Update,
}

Source_Range :: struct {
	start: int,
	end:   int,
}

// Source_Loc carries borrowed provenance; callers own path storage.
Source_Loc :: struct {
	path:  string,
	range: Source_Range,
}

IO_Policy :: struct {
	capture_write:   bool,
	capture_message: bool,
}

Table_Data :: struct {
	allocator: mem.Allocator,
	refs:      int,
	rows:      [dynamic]Value,
}

Structure_Data :: struct {
	allocator: mem.Allocator,
	refs:      int,
	name:      string,
	fields:    map[string]Value,
}

Object_Data :: struct {
	allocator: mem.Allocator,
	refs:      int,
	type_name: string,
	fields:    map[string]Value,
}

Table_Iterator_Data :: struct {
	allocator: mem.Allocator,
	refs:      int,
	table:     ^Table_Data,
	index:     int,
}

Value_Initial :: struct {}

Value_World :: struct {}

Value_Integer :: struct {
	value: i64,
}

Value_String :: struct {
	text: string,
}

Value_Structure :: struct {
	name: string,
	data: ^Structure_Data,
}

Value_Object :: struct {
	data: ^Object_Data,
}

Value_Predicate :: struct {
	value: bool,
}

Value_Table :: struct {
	data: ^Table_Data,
}

Value_Table_Iterator :: struct {
	data: ^Table_Iterator_Data,
}

Value :: union {
	Value_Initial,
	Value_World,
	Value_Integer,
	Value_String,
	Value_Structure,
	Value_Object,
	Value_Predicate,
	Value_Table,
	Value_Table_Iterator,
}

Named_Value :: struct {
	scope: string,
	name:  string,
	value: Value,
}

IO_Event :: struct {
	kind:         IO_Event_Kind,
	text:         string,
	message_type: string,
	source:       Source_Loc,
}

Trap :: struct {
	kind:    Trap_Kind,
	message: string,
	source:  Source_Loc,
}

Context_Options :: struct {
	io_policy: IO_Policy,
}

Context :: struct {
	allocator:     mem.Allocator,
	io_policy:     IO_Policy,
	world:         Value,
	global_values: map[string]Value,
	system_values: map[string]Value,
	events:        [dynamic]IO_Event,
	trap:          Trap,
}

Message_Descriptor :: struct {
	message_id:    string,
	message_type:  string,
	message_number: string,
}

Call_Request :: struct {
	callee_name: string,
	values:      []Value,
	result_type: string,
}

Table_Request :: struct {
	operation:   Table_Operation,
	values:      []Value,
	result_type: string,
	index:       int,
}

Assign_Request :: struct {
	values: []Value,
}

SQL_Request :: struct {
	operation:   SQL_Operation,
	result_type: string,
}

Field_Request :: struct {
	base:        Value,
	name:        string,
	value:       Value,
	result_type: string,
}

io_policy_captured :: #force_inline proc "contextless" () -> IO_Policy {
	return IO_Policy {
		capture_write   = true,
		capture_message = true,
	}
}

context_make :: proc(
	options: Context_Options = {},
	allocator: mem.Allocator = context.allocator,
) -> Context {
	io_policy := options.io_policy
	if !io_policy.capture_write && !io_policy.capture_message {
		io_policy = io_policy_captured()
	}
	return Context {
		allocator = allocator,
		io_policy = io_policy,
		world = Value_World{},
		global_values = make(map[string]Value, 32, allocator),
		system_values = make(map[string]Value, 8, allocator),
		events = make([dynamic]IO_Event, 0, 8, allocator),
	}
}

context_destroy :: proc(ctx: ^Context) {
	for key, &value in ctx.global_values {
		delete(key, ctx.allocator)
		value_destroy(&value)
	}
	delete(ctx.global_values)
	for key, &value in ctx.system_values {
		delete(key, ctx.allocator)
		value_destroy(&value)
	}
	delete(ctx.system_values)
	for &event in ctx.events {
		io_event_destroy(&event)
	}
	delete(ctx.events)
	value_destroy(&ctx.world)
	delete(ctx.trap.message)
	ctx^ = {}
}

context_trapped :: #force_inline proc "contextless" (ctx: ^Context) -> bool {
	return ctx.trap.kind != .None
}

context_trap :: proc(
	ctx: ^Context,
	kind: Trap_Kind,
	message: string,
	source: Source_Loc = {},
) {
	delete(ctx.trap.message)
	ctx.trap = Trap {
		kind = kind,
		message = strings.clone(message, ctx.allocator),
		source = source,
	}
}

io_event_destroy :: proc(event: ^IO_Event) {
	delete(event.text)
	delete(event.message_type)
	event^ = {}
}

io_event_clone :: proc(event: IO_Event, allocator: mem.Allocator) -> IO_Event {
	return IO_Event {
		kind = event.kind,
		text = strings.clone(event.text, allocator),
		message_type = strings.clone(event.message_type, allocator),
		source = event.source,
	}
}

trap_clone :: proc(trap: Trap, allocator: mem.Allocator) -> Trap {
	return Trap {
		kind = trap.kind,
		message = strings.clone(trap.message, allocator),
		source = trap.source,
	}
}

named_value_clone :: proc(scope, name: string, value: Value, allocator: mem.Allocator) -> Named_Value {
	return Named_Value {
		scope = strings.clone(scope, allocator),
		name = strings.clone(name, allocator),
		value = value_clone(value, allocator),
	}
}

named_value_destroy :: proc(value: ^Named_Value) {
	delete(value.scope)
	delete(value.name)
	value_destroy(&value.value)
	value^ = {}
}

context_collect_final_values :: proc(
	ctx: ^Context,
	out: ^[dynamic]Named_Value,
	allocator: mem.Allocator,
) {
	values := make([dynamic]Named_Value, 0, len(ctx.global_values) + len(ctx.system_values), context.temp_allocator)
	for key, value in ctx.global_values {
		append(&values, named_value_clone("global", key, value, context.temp_allocator))
	}
	for key, value in ctx.system_values {
		append(&values, named_value_clone("system", key, value, context.temp_allocator))
	}
	slice.sort_by(values[:], named_value_less)
	for value in values {
		append(out, named_value_clone(value.scope, value.name, value.value, allocator))
	}
}

context_global_read :: #force_inline proc "contextless" (ctx: ^Context, name: string) -> Value {
	return map_value_or_initial(ctx.global_values, name)
}

context_global_write :: #force_inline proc(ctx: ^Context, name: string, value: Value) {
	map_set_value(&ctx.global_values, name, value, ctx.allocator)
}

context_runtime_read :: proc(ctx: ^Context, name: string) -> Value {
	switch name {
	case "sy", "syst":
		return Value_Structure{name = "sy"}
	}
	return map_value_or_initial(ctx.global_values, name)
}

context_runtime_write :: proc(
	ctx: ^Context,
	name: string,
	value: Value,
	source: Source_Loc = {},
) -> bool {
	switch name {
	case "sy", "syst":
		context_trap(ctx, .Unsupported, "runtime-provided structure assignment is not implemented", source)
		return false
	}
	context_global_write(ctx, name, value)
	return true
}

context_system_read :: #force_inline proc(ctx: ^Context, field_name: string) -> Value {
	value := map_value_or_initial(ctx.system_values, field_name)
	if value_kind(value) == .Initial {
		switch field_name {
		case "subrc", "tabix", "dbcnt":
			return value_integer_make(0)
		}
	}
	return value
}

context_system_write :: #force_inline proc(ctx: ^Context, field_name: string, value: Value) {
	map_set_value(&ctx.system_values, field_name, value, ctx.allocator)
}

context_write :: proc(
	ctx: ^Context,
	values: []Value,
	source: Source_Loc = {},
) -> bool {
	if !ctx.io_policy.capture_write {
		context_trap(ctx, .Unsupported, "WRITE output is denied by runtime I/O policy", source)
		return false
	}
	out := strings.builder_make(context.temp_allocator)
	for value, i in values {
		if i > 0 {
			strings.write_byte(&out, ' ')
		}
		if !value_write_scalar_text(&out, value) {
			context_trap(ctx, .Type, "WRITE requires scalar text operands", source)
			return false
		}
	}
	append_event(ctx, .Write, strings.to_string(out), "", source)
	return true
}

context_message :: proc(
	ctx: ^Context,
	descriptor: Message_Descriptor,
	values: []Value,
	source: Source_Loc = {},
) -> (string, bool) {
	if !ctx.io_policy.capture_message {
		context_trap(ctx, .Unsupported, "MESSAGE output is denied by runtime I/O policy", source)
		return "", false
	}
	text, text_ok := message_text(descriptor, values, context.temp_allocator)
	if !text_ok {
		context_trap(ctx, .Type, "MESSAGE requires scalar text operands", source)
		return "", false
	}
	append_event(ctx, .Message, text, descriptor.message_type, source)
	return text, true
}

abap_integer_arithmetic :: proc(
	ctx: ^Context,
	kind: Arithmetic_Kind,
	left, right: Value,
	source: Source_Loc = {},
) -> (Value, bool) {
	left_int, left_ok := value_integer(left)
	right_int, right_ok := value_integer(right)
	if !left_ok || !right_ok {
		context_trap(ctx, .Type, "integer arithmetic requires integer operands", source)
		return {}, false
	}
	switch kind {
	case .Add:
		return value_integer_make(left_int + right_int), true
	case .Subtract:
		return value_integer_make(left_int - right_int), true
	case .Multiply:
		return value_integer_make(left_int * right_int), true
	case .Divide:
		if right_int == 0 {
			context_trap(ctx, .Divide_By_Zero, "division by zero", source)
			return {}, false
		}
		return value_integer_make(left_int / right_int), true
	}
	return {}, false
}

abap_compare :: proc(
	ctx: ^Context,
	kind: Comparison_Kind,
	left, right: Value,
	source: Source_Loc = {},
) -> (Value, bool) {
	cmp: int
	if left_int, left_ok := value_integer(left); left_ok {
		if right_int, right_ok := value_integer(right); right_ok {
			cmp = -1 if left_int < right_int else (1 if left_int > right_int else 0)
		} else {
			context_trap(ctx, .Type, "comparison operands have incompatible runtime types", source)
			return {}, false
		}
	} else {
		left_text, left_text_ok := value_scalar_text(left, context.temp_allocator)
		right_text, right_text_ok := value_scalar_text(right, context.temp_allocator)
		if !left_text_ok || !right_text_ok {
			context_trap(ctx, .Type, "comparison requires scalar operands", source)
			return {}, false
		}
		cmp = strings.compare(left_text, right_text)
	}

	result := false
	switch kind {
	case .Equal:
		result = cmp == 0
	case .Not_Equal:
		result = cmp != 0
	case .Less:
		result = cmp < 0
	case .Less_Equal:
		result = cmp <= 0
	case .Greater:
		result = cmp > 0
	case .Greater_Equal:
		result = cmp >= 0
	}
	return value_predicate(result), true
}

abap_string_join :: proc(
	ctx: ^Context,
	values: []Value,
	allocator: mem.Allocator,
	source: Source_Loc = {},
) -> (Value, bool) {
	out := strings.builder_make(context.temp_allocator)
	for value in values {
		if !value_write_scalar_text(&out, value) {
			context_trap(ctx, .Type, "string operation requires scalar text operands", source)
			return {}, false
		}
	}
	return value_string(strings.to_string(out), allocator), true
}

abap_construct :: proc(
	ctx: ^Context,
	callee_name: string,
	values: []Value,
	result_type: string,
	source: Source_Loc = {},
) -> (Value, bool) {
	switch callee_name {
	case "", "value":
		if len(values) > 0 {
			return value_clone(values[0], ctx.allocator), true
		}
		return initial_for_type(result_type, ctx.allocator), true
	case "new", "ref":
		if type_is_reference(result_type) {
			return value_object(result_type, ctx.allocator), true
		}
		return value_structure(result_type, ctx.allocator), true
	case "conv", "exact", "cast":
		if len(values) == 1 {
			value, ok := value_cast(values[0], result_type, ctx.allocator)
			if !ok {
				context_trap(ctx, .Type, "constructor cast failed", source)
				return {}, false
			}
			return value, true
		}
		context_trap(ctx, .Unsupported, "constructor cast value runtime semantics are not implemented", source)
		return {}, false
	case:
		context_trap(ctx, .Unsupported, "constructor runtime semantics are not implemented", source)
		return {}, false
	}
}

context_call :: proc(
	ctx: ^Context,
	request: Call_Request,
	source: Source_Loc = {},
) -> (Value, bool) {
	name := request.callee_name
	switch name {
	case "boolc":
		return value_string("X" if len(request.values) > 0 && value_truthy(request.values[0]) else "", ctx.allocator), true
	case "abs":
		if len(request.values) == 0 {
			return value_integer_make(0), true
		}
		value, ok := value_integer(request.values[0])
		if !ok {
			context_trap(ctx, .Type, "ABS requires an integer-compatible operand", source)
			return {}, false
		}
		if value < 0 {
			value = -value
		}
		return value_integer_make(value), true
	case "sign":
		if len(request.values) == 0 {
			return value_integer_make(0), true
		}
		value, ok := value_integer(request.values[0])
		if !ok {
			context_trap(ctx, .Type, "SIGN requires an integer-compatible operand", source)
			return {}, false
		}
		return value_integer_make(-1 if value < 0 else (1 if value > 0 else 0)), true
	case "strlen", "charlen", "numofchar", "dbmaxlen", "xstrlen":
		if len(request.values) == 0 {
			return value_integer_make(0), true
		}
		text, text_ok := value_scalar_text(request.values[0], context.temp_allocator)
		if !text_ok {
			context_trap(ctx, .Type, "string length builtin requires a scalar text operand", source)
			return {}, false
		}
		return value_integer_make(i64(len(text))), true
	case "lines":
		if len(request.values) == 0 {
			return value_integer_make(0), true
		}
		return value_integer_make(i64(table_len(request.values[0]))), true
	case "nmax", "nmin":
		return builtin_extremum(ctx, name, request.values, source)
	case "to_lower":
		if len(request.values) == 0 {
			return value_string("", ctx.allocator), true
		}
		text, text_ok := value_scalar_text(request.values[0], context.temp_allocator)
		if !text_ok {
			context_trap(ctx, .Type, "TO_LOWER requires a scalar text operand", source)
			return {}, false
		}
		return value_string(strings.to_lower(text, context.temp_allocator), ctx.allocator), true
	case "to_upper":
		if len(request.values) == 0 {
			return value_string("", ctx.allocator), true
		}
		text, text_ok := value_scalar_text(request.values[0], context.temp_allocator)
		if !text_ok {
			context_trap(ctx, .Type, "TO_UPPER requires a scalar text operand", source)
			return {}, false
		}
		return value_string(strings.to_upper(text, context.temp_allocator), ctx.allocator), true
	}
	context_trap(ctx, .Unsupported, "ABAP call dispatch is not implemented", source)
	return {}, false
}

context_table_read :: proc(
	ctx: ^Context,
	request: Table_Request,
	source: Source_Loc = {},
) -> (Value, Value, bool) {
	if len(request.values) == 0 {
		context_trap(ctx, .Type, "table read requires a table operand", source)
		return {}, {}, false
	}
	table := request.values[0]
	index := request.index
	if index == 0 && len(request.values) > 1 {
		if parsed, ok := value_integer(request.values[1]); ok {
			index = int(parsed)
		}
	}
	row, found := table_read(table, index, ctx.allocator)
	if found {
		return row, value_integer_make(0), true
	}
	return initial_for_type(request.result_type, ctx.allocator), value_integer_make(4), true
}

context_table_iter :: proc(
	ctx: ^Context,
	request: Table_Request,
	source: Source_Loc = {},
) -> (Value, bool) {
	if len(request.values) == 0 {
		context_trap(ctx, .Type, "table iterator requires a table operand", source)
		return {}, false
	}
	return value_table_iterator(request.values[0], ctx.allocator), true
}

context_table_next :: proc(
	ctx: ^Context,
	request: Table_Request,
	source: Source_Loc = {},
) -> (Value, Value, Value, bool) {
	if len(request.values) == 0 || value_iterator_data(request.values[0]) == nil {
		context_trap(ctx, .Type, "table next requires a table iterator operand", source)
		return {}, {}, {}, false
	}
	iter := value_iterator_data(request.values[0])
	next := value_iterator_advanced(request.values[0], ctx.allocator)
	if iter.table == nil || iter.index < 0 || iter.index >= len(iter.table.rows) {
		return value_predicate(false), initial_for_type(request.result_type, ctx.allocator), next, true
	}
	return value_predicate(true), value_deep_clone(iter.table.rows[iter.index], ctx.allocator), next, true
}

context_table_mutate :: proc(
	ctx: ^Context,
	request: Table_Request,
	source: Source_Loc = {},
) -> bool {
	#partial switch request.operation {
	case .Append:
		if len(request.values) < 2 {
			context_trap(ctx, .Type, "APPEND requires row and table operands", source)
			return false
		}
		return table_append(ctx, request.values[1], request.values[0], source)
	case .Insert:
		if len(request.values) < 2 {
			context_trap(ctx, .Type, "INSERT requires row and table operands", source)
			return false
		}
		index := request.index
		if index == 0 && len(request.values) > 2 {
			if parsed, ok := value_integer(request.values[2]); ok {
				index = int(parsed)
			}
		}
		return table_insert(ctx, request.values[1], request.values[0], index, source)
	case .Modify:
		if len(request.values) < 2 {
			context_trap(ctx, .Type, "MODIFY requires row and table operands", source)
			return false
		}
		index := request.index
		if index == 0 && len(request.values) > 2 {
			if parsed, ok := value_integer(request.values[2]); ok {
				index = int(parsed)
			}
		}
		return table_modify(ctx, request.values[1], request.values[0], index, source)
	case .Delete:
		if len(request.values) < 1 {
			context_trap(ctx, .Type, "DELETE requires a table operand", source)
			return false
		}
		index := request.index
		if index == 0 && len(request.values) > 1 {
			if parsed, ok := value_integer(request.values[1]); ok {
				index = int(parsed)
			}
		}
		return table_delete(ctx, request.values[0], index, source)
	case .Sort:
		if len(request.values) < 1 {
			context_trap(ctx, .Type, "SORT requires a table operand", source)
			return false
		}
		return table_sort(ctx, request.values[0], source)
	case:
		context_trap(ctx, .Unsupported, "internal table operation is not implemented", source)
		return false
	}
}

context_sql_select :: proc(
	ctx: ^Context,
	request: SQL_Request,
	source: Source_Loc = {},
) -> (Value, Value, bool) {
	return initial_for_type(request.result_type, ctx.allocator), value_integer_make(4), true
}

context_sql_mutate :: proc(
	ctx: ^Context,
	request: SQL_Request,
	source: Source_Loc = {},
) -> bool {
	return true
}

context_field_load :: proc(
	ctx: ^Context,
	request: Field_Request,
	source: Source_Loc = {},
) -> (Value, bool) {
	if value_kind(request.base) == .Structure && structure_is_system(request.base) {
		return value_clone(context_system_read(ctx, request.name), ctx.allocator), true
	}
	if structure := value_structure_data(request.base); structure != nil {
		if value, ok := structure.fields[request.name]; ok {
			return value_clone(value, ctx.allocator), true
		}
		return initial_for_type(request.result_type, ctx.allocator), true
	}
	if value_kind(request.base) == .Object {
		object := value_object_data(request.base)
		if object == nil {
			context_trap(ctx, .Type, "object reference is initial", source)
			return {}, false
		}
		if value, ok := object.fields[request.name]; ok {
			return value_clone(value, ctx.allocator), true
		}
		return initial_for_type(request.result_type, ctx.allocator), true
	}
	context_trap(ctx, .Unsupported, "object and structure field runtime semantics are not implemented", source)
	return {}, false
}

context_field_store :: proc(
	ctx: ^Context,
	request: Field_Request,
	source: Source_Loc = {},
) -> bool {
	if value_kind(request.base) == .Structure && structure_is_system(request.base) {
		context_system_write(ctx, request.name, request.value)
		return true
	}
	if structure := value_structure_data(request.base); structure != nil {
		structure_set_field(structure, request.name, request.value)
		return true
	}
	if value_kind(request.base) == .Object {
		object := value_object_data(request.base)
		if object == nil {
			context_trap(ctx, .Type, "object reference is initial", source)
			return false
		}
		object_set_field(object, request.name, request.value)
		return true
	}
	context_trap(ctx, .Unsupported, "object and structure field runtime semantics are not implemented", source)
	return false
}

context_assign_field :: proc(
	ctx: ^Context,
	request: Assign_Request,
	source: Source_Loc = {},
) -> (Value, bool) {
	if len(request.values) == 0 {
		context_trap(ctx, .Type, "ASSIGN requires a source operand", source)
		return {}, false
	}
	return value_clone(request.values[0], ctx.allocator), true
}

builtin_extremum :: proc(
	ctx: ^Context,
	name: string,
	values: []Value,
	source: Source_Loc,
) -> (Value, bool) {
	if len(values) == 0 {
		return value_integer_make(0), true
	}
	best, best_ok := value_integer(values[0])
	if !best_ok {
		context_trap(ctx, .Type, "numeric builtin requires integer-compatible operands", source)
		return {}, false
	}
	for value in values[1:] {
		current, current_ok := value_integer(value)
		if !current_ok {
			context_trap(ctx, .Type, "numeric builtin requires integer-compatible operands", source)
			return {}, false
		}
		if (name == "nmax" && current > best) || (name == "nmin" && current < best) {
			best = current
		}
	}
	return value_integer_make(best), true
}

table_len :: proc "contextless" (value: Value) -> int {
	if table := value_table_data(value); table != nil {
		return len(table.rows)
	}
	return 0
}

table_read :: proc(value: Value, index: int, allocator: mem.Allocator) -> (Value, bool) {
	table := value_table_data(value)
	if value_kind(value) == .Initial {
		return {}, false
	}
	if table == nil {
		return {}, false
	}
	row_index := index - 1
	if index == 0 {
		row_index = 0
	}
	if row_index < 0 || row_index >= len(table.rows) {
		return {}, false
	}
	return value_deep_clone(table.rows[row_index], allocator), true
}

table_append :: proc(ctx: ^Context, table: Value, row: Value, source: Source_Loc) -> bool {
	data := value_table_data(table)
	if data == nil {
		context_trap(ctx, .Type, "table operand is not mutable", source)
		return false
	}
	append(&data.rows, value_deep_clone(row, data.allocator))
	return true
}

table_insert :: proc(ctx: ^Context, table: Value, row: Value, index: int, source: Source_Loc) -> bool {
	data := value_table_data(table)
	if data == nil {
		context_trap(ctx, .Type, "table operand is not mutable", source)
		return false
	}
	row_index := index - 1
	if index == 0 || row_index >= len(data.rows) {
		append(&data.rows, value_deep_clone(row, data.allocator))
		return true
	}
	if row_index < 0 {
		row_index = 0
	}
	append(&data.rows, Value{})
	for i := len(data.rows) - 1; i > row_index; i -= 1 {
		data.rows[i] = data.rows[i - 1]
	}
	data.rows[row_index] = value_deep_clone(row, data.allocator)
	return true
}

table_modify :: proc(ctx: ^Context, table: Value, row: Value, index: int, source: Source_Loc) -> bool {
	data := value_table_data(table)
	if data == nil {
		context_trap(ctx, .Type, "table operand is not mutable", source)
		return false
	}
	row_index := index - 1
	if index == 0 {
		row_index = 0
	}
	if row_index < 0 || row_index >= len(data.rows) {
		return true
	}
	value_destroy(&data.rows[row_index])
	data.rows[row_index] = value_deep_clone(row, data.allocator)
	return true
}

table_delete :: proc(ctx: ^Context, table: Value, index: int, source: Source_Loc) -> bool {
	data := value_table_data(table)
	if data == nil {
		context_trap(ctx, .Type, "table operand is not mutable", source)
		return false
	}
	row_index := index - 1
	if index == 0 {
		row_index = 0
	}
	if row_index < 0 || row_index >= len(data.rows) {
		return true
	}
	value_destroy(&data.rows[row_index])
	ordered_remove(&data.rows, row_index)
	return true
}

table_sort :: proc(ctx: ^Context, table: Value, source: Source_Loc) -> bool {
	data := value_table_data(table)
	if data == nil {
		context_trap(ctx, .Type, "table operand is not mutable", source)
		return false
	}
	for row in data.rows {
		if !value_has_scalar_text(row) {
			context_trap(ctx, .Type, "SORT requires scalar row values", source)
			return false
		}
	}
	slice.sort_by(data.rows[:], table_row_less)
	return true
}

table_row_less :: proc(left, right: Value) -> bool {
	left_text, left_ok := value_scalar_text(left, context.temp_allocator)
	right_text, right_ok := value_scalar_text(right, context.temp_allocator)
	assert(left_ok && right_ok)
	return strings.compare(left_text, right_text) < 0
}

table_retain :: #force_inline proc "contextless" (table: ^Table_Data) {
	if table != nil {
		table.refs += 1
	}
}

table_release :: proc(table: ^Table_Data) {
	if table == nil {
		return
	}
	assert(table.refs > 0)
	table.refs -= 1
	if table.refs > 0 {
		return
	}
	for &row in table.rows {
		value_destroy(&row)
	}
	delete(table.rows)
	mem.free(rawptr(table), table.allocator)
}

structure_retain :: #force_inline proc "contextless" (structure: ^Structure_Data) {
	if structure != nil {
		structure.refs += 1
	}
}

structure_release :: proc(structure: ^Structure_Data) {
	if structure == nil {
		return
	}
	assert(structure.refs > 0)
	structure.refs -= 1
	if structure.refs > 0 {
		return
	}
	delete(structure.name, structure.allocator)
	for key, &value in structure.fields {
		delete(key, structure.allocator)
		value_destroy(&value)
	}
	delete(structure.fields)
	mem.free(rawptr(structure), structure.allocator)
}

object_retain :: #force_inline proc "contextless" (object: ^Object_Data) {
	if object != nil {
		object.refs += 1
	}
}

object_release :: proc(object: ^Object_Data) {
	if object == nil {
		return
	}
	assert(object.refs > 0)
	object.refs -= 1
	if object.refs > 0 {
		return
	}
	delete(object.type_name, object.allocator)
	for key, &value in object.fields {
		delete(key, object.allocator)
		value_destroy(&value)
	}
	delete(object.fields)
	mem.free(rawptr(object), object.allocator)
}

structure_set_field :: proc(structure: ^Structure_Data, name: string, value: Value) {
	assert(structure != nil)
	new_value := value_clone(value, structure.allocator)
	if existing, ok := structure.fields[name]; ok {
		value_destroy(&existing)
		structure.fields[name] = new_value
		return
	}
	owned_key := strings.clone(name, structure.allocator)
	structure.fields[owned_key] = new_value
}

object_set_field :: proc(object: ^Object_Data, name: string, value: Value) {
	assert(object != nil)
	new_value := value_clone(value, object.allocator)
	if existing, ok := object.fields[name]; ok {
		value_destroy(&existing)
		object.fields[name] = new_value
		return
	}
	owned_key := strings.clone(name, object.allocator)
	object.fields[owned_key] = new_value
}

iterator_retain :: #force_inline proc "contextless" (iterator: ^Table_Iterator_Data) {
	if iterator != nil {
		iterator.refs += 1
	}
}

iterator_release :: proc(iterator: ^Table_Iterator_Data) {
	if iterator == nil {
		return
	}
	assert(iterator.refs > 0)
	iterator.refs -= 1
	if iterator.refs > 0 {
		return
	}
	table_release(iterator.table)
	mem.free(rawptr(iterator), iterator.allocator)
}

map_set_value :: proc(
	values: ^map[string]Value,
	name: string,
	value: Value,
	allocator: mem.Allocator,
) {
	new_value := value_clone(value, allocator)
	if existing, ok := values^[name]; ok {
		value_destroy(&existing)
		values^[name] = new_value
		return
	}
	owned_key := strings.clone(name, allocator)
	values^[owned_key] = new_value
}

map_value_or_initial :: #force_inline proc "contextless" (values: map[string]Value, name: string) -> Value {
	if value, ok := values[name]; ok {
		return value
	}
	return value_initial()
}

value_initial :: #force_inline proc "contextless" () -> Value {
	return Value_Initial{}
}

value_kind :: proc "contextless" (value: Value) -> Value_Kind {
	#partial switch _ in value {
	case Value_Initial:
		return .Initial
	case Value_World:
		return .World
	case Value_Integer:
		return .Integer
	case Value_String:
		return .String
	case Value_Structure:
		return .Structure
	case Value_Object:
		return .Object
	case Value_Predicate:
		return .Predicate
	case Value_Table:
		return .Table
	case Value_Table_Iterator:
		return .Table_Iterator
	}
	return .Initial
}

value_int :: proc "contextless" (value: Value) -> i64 {
	#partial switch v in value {
	case Value_Integer:
		return v.value
	case Value_Predicate:
		return 1 if v.value else 0
	}
	return 0
}

value_text :: proc "contextless" (value: Value) -> string {
	#partial switch v in value {
	case Value_String:
		return v.text
	case Value_Structure:
		if v.name != "" {
			return v.name
		}
		if v.data != nil {
			return v.data.name
		}
	}
	return ""
}

value_structure_data :: proc "contextless" (value: Value) -> ^Structure_Data {
	#partial switch v in value {
	case Value_Structure:
		return v.data
	}
	return nil
}

value_object_data :: proc "contextless" (value: Value) -> ^Object_Data {
	#partial switch v in value {
	case Value_Object:
		return v.data
	}
	return nil
}

value_table_data :: proc "contextless" (value: Value) -> ^Table_Data {
	#partial switch v in value {
	case Value_Table:
		return v.data
	}
	return nil
}

value_iterator_data :: proc "contextless" (value: Value) -> ^Table_Iterator_Data {
	#partial switch v in value {
	case Value_Table_Iterator:
		return v.data
	}
	return nil
}

value_from_literal :: proc(literal, typ: string, allocator: mem.Allocator) -> Value {
	if type_is_integer(typ) {
		parsed, ok := strconv.parse_int(strings.trim_space(literal), 10)
		if ok {
			return value_integer_make(i64(parsed))
		}
	}
	if type_is_predicate(typ) {
		return value_predicate(literal != "" && literal != "0" && literal != " ")
	}
	if type_is_string(typ) {
		text := literal
		if len(literal) >= 2 &&
		   ((literal[0] == '\'' && literal[len(literal) - 1] == '\'') ||
		    (literal[0] == '`' && literal[len(literal) - 1] == '`')) {
			text = literal[1:len(literal) - 1]
		}
		return value_string(text, allocator)
	}
	if type_is_table(typ) {
		return value_table(allocator)
	}
	if type_is_reference(typ) {
		return value_initial()
	}
	if typ != "" {
		return value_structure(typ, allocator)
	}
	text := literal
	if len(literal) >= 2 &&
	   ((literal[0] == '\'' && literal[len(literal) - 1] == '\'') ||
	    (literal[0] == '`' && literal[len(literal) - 1] == '`')) {
		text = literal[1:len(literal) - 1]
	}
	return value_string(text, allocator)
}

value_cast :: proc(value: Value, typ: string, allocator: mem.Allocator) -> (Value, bool) {
	if type_is_integer(typ) {
		int_value, ok := value_integer(value)
		return value_integer_make(int_value), ok
	}
	if type_is_predicate(typ) {
		return value_predicate(value_truthy(value)), true
	}
	if type_is_string(typ) || typ == "" {
		text, text_ok := value_scalar_text(value, context.temp_allocator)
		if !text_ok {
			return {}, false
		}
		return value_string(text, allocator), true
	}
	if type_is_reference(typ) {
		kind := value_kind(value)
		if kind == .Initial || kind == .Object {
			return value_clone(value, allocator), true
		}
		return {}, false
	}
	return value_clone(value, allocator), true
}

initial_for_type :: proc(typ: string, allocator: mem.Allocator) -> Value {
	if type_is_integer(typ) {
		return value_integer_make(0)
	}
	if type_is_predicate(typ) {
		return value_predicate(false)
	}
	if type_is_string(typ) {
		return value_string("", allocator)
	}
	if type_is_table(typ) {
		return value_table(allocator)
	}
	if type_is_reference(typ) {
		return value_initial()
	}
	if typ != "" {
		return value_structure(typ, allocator)
	}
	return value_initial()
}

type_is_integer :: #force_inline proc(typ: string) -> bool {
	return typ == "!i" || typ == "i" || strings.contains(typ, "int") || strings.contains(typ, "numeric")
}

type_is_string :: #force_inline proc(typ: string) -> bool {
	return typ == "!string" || typ == "string" || strings.contains(typ, "char") || strings.contains(typ, "string")
}

type_is_predicate :: #force_inline proc "contextless" (typ: string) -> bool {
	return typ == "!predicate" || typ == "predicate" || typ == "bool" || typ == "abap_bool"
}

type_is_table :: #force_inline proc(typ: string) -> bool {
	return typ == "!table" ||
	       typ == "table" ||
	       strings.contains(typ, " table") ||
	       strings.contains(typ, "table of") ||
	       strings.has_suffix(typ, "_tab")
}

type_is_reference :: #force_inline proc(typ: string) -> bool {
	return typ == "!ref" ||
	       typ == "ref" ||
	       strings.has_prefix(typ, "!ref:") ||
	       strings.has_prefix(typ, "ref:")
}

value_integer_make :: #force_inline proc "contextless" (value: i64) -> Value {
	return Value_Integer{value = value}
}

value_predicate :: #force_inline proc "contextless" (value: bool) -> Value {
	return Value_Predicate{value = value}
}

value_string :: #force_inline proc(value: string, allocator: mem.Allocator) -> Value {
	return Value_String{text = strings.clone(value, allocator)}
}

value_structure :: #force_inline proc(name: string, allocator: mem.Allocator) -> Value {
	structure, _ := mem.new(Structure_Data, allocator)
	structure^ = Structure_Data {
		allocator = allocator,
		refs = 1,
		name = strings.clone(name, allocator),
		fields = make(map[string]Value, 8, allocator),
	}
	return Value_Structure{data = structure}
}

value_object :: proc(type_name: string, allocator: mem.Allocator) -> Value {
	object, _ := mem.new(Object_Data, allocator)
	object^ = Object_Data {
		allocator = allocator,
		refs = 1,
		type_name = strings.clone(type_name, allocator),
		fields = make(map[string]Value, 8, allocator),
	}
	return Value_Object{data = object}
}

value_table :: proc(allocator: mem.Allocator) -> Value {
	table, _ := mem.new(Table_Data, allocator)
	table^ = Table_Data {
		allocator = allocator,
		refs = 1,
		rows = make([dynamic]Value, 0, 4, allocator),
	}
	return Value_Table{data = table}
}

value_table_iterator :: proc(table_value: Value, allocator: mem.Allocator) -> Value {
	table := value_table_data(table_value)
	table_retain(table)
	iterator, _ := mem.new(Table_Iterator_Data, allocator)
	iterator^ = Table_Iterator_Data {
		allocator = allocator,
		refs = 1,
		table = table,
	}
	return Value_Table_Iterator{data = iterator}
}

value_iterator_advanced :: proc(iterator_value: Value, allocator: mem.Allocator) -> Value {
	iterator := value_iterator_data(iterator_value)
	if iterator == nil {
		return value_initial()
	}
	table_retain(iterator.table)
	next, _ := mem.new(Table_Iterator_Data, allocator)
	next^ = Table_Iterator_Data {
		allocator = allocator,
		refs = 1,
		table = iterator.table,
		index = iterator.index + 1,
	}
	return Value_Table_Iterator{data = next}
}

value_clone :: proc(value: Value, allocator: mem.Allocator) -> Value {
	#partial switch v in value {
	case Value_String:
		return Value_String{text = strings.clone(v.text, allocator)}
	case Value_Structure:
		structure_retain(v.data)
		return value
	case Value_Object:
		object_retain(v.data)
		return value
	case Value_Table:
		table_retain(v.data)
		return value
	case Value_Table_Iterator:
		iterator_retain(v.data)
		return value
	case Value_Initial, Value_World, Value_Integer, Value_Predicate:
		return value
	}
	return value_initial()
}

value_deep_clone :: proc(value: Value, allocator: mem.Allocator) -> Value {
	#partial switch v in value {
	case Value_Structure:
		if v.data == nil {
			return value_initial()
		}
		out := value_structure(v.data.name, allocator)
		out_structure := value_structure_data(out)
		assert(out_structure != nil)
		for key, field in v.data.fields {
			field_clone := value_deep_clone(field, allocator)
			structure_set_field(out_structure, key, field_clone)
			value_destroy(&field_clone)
		}
		return out
	case Value_Table:
		out := value_table(allocator)
		out_table := value_table_data(out)
		assert(out_table != nil)
		if v.data == nil {
			return out
		}
		for row in v.data.rows {
			append(&out_table.rows, value_deep_clone(row, allocator))
		}
		return out
	case Value_Table_Iterator:
		return value_clone(value, allocator)
	case Value_Object:
		return value_clone(value, allocator)
	case Value_String, Value_Initial, Value_World, Value_Integer, Value_Predicate:
		return value_clone(value, allocator)
	}
	return value_initial()
}

value_destroy :: proc(value: ^Value) {
	#partial switch v in value^ {
	case Value_String:
		delete(v.text)
	case Value_Structure:
		structure_release(v.data)
	case Value_Object:
		object_release(v.data)
	case Value_Table:
		table_release(v.data)
	case Value_Table_Iterator:
		iterator_release(v.data)
	}
	value^ = {}
}

value_integer :: proc(value: Value) -> (i64, bool) {
	#partial switch v in value {
	case Value_Integer:
		return v.value, true
	case Value_Predicate:
		return 1 if v.value else 0, true
	case Value_Initial:
		return 0, true
	case Value_String:
		parsed, ok := strconv.parse_int(strings.trim_space(v.text), 10)
		return i64(parsed), ok
	case Value_World, Value_Structure, Value_Object, Value_Table, Value_Table_Iterator:
		return 0, false
	}
	return 0, true
}

value_truthy :: proc(value: Value) -> bool {
	#partial switch v in value {
	case Value_Initial:
		return false
	case Value_World:
		return true
	case Value_Structure:
		return true
	case Value_Object:
		return v.data != nil
	case Value_Table:
		return v.data != nil && len(v.data.rows) > 0
	case Value_Table_Iterator:
		return v.data != nil && v.data.table != nil && v.data.index < len(v.data.table.rows)
	case Value_Integer:
		return v.value != 0
	case Value_Predicate:
		return v.value
	case Value_String:
		return strings.trim_space(v.text) != ""
	}
	return false
}

value_is_initial :: #force_inline proc "contextless" (value: Value) -> bool {
	#partial switch v in value {
	case Value_Initial:
		return true
	case Value_World:
		return false
	case Value_Structure:
		return false
	case Value_Object:
		return v.data == nil
	case Value_Table:
		return v.data == nil || len(v.data.rows) == 0
	case Value_Table_Iterator:
		return v.data == nil ||
		       v.data.table == nil ||
		       v.data.index >= len(v.data.table.rows)
	case Value_Integer:
		return v.value == 0
	case Value_Predicate:
		return !v.value
	case Value_String:
		return v.text == ""
	}
	return true
}

value_has_scalar_text :: proc "contextless" (value: Value) -> bool {
	#partial switch _ in value {
	case Value_Initial, Value_Integer, Value_Predicate, Value_String:
		return true
	case Value_World, Value_Structure, Value_Object, Value_Table, Value_Table_Iterator:
		return false
	}
	return true
}

value_scalar_text :: proc(value: Value, allocator: mem.Allocator) -> (string, bool) {
	#partial switch v in value {
	case Value_Initial:
		return "", true
	case Value_Integer:
		buf: [32]byte
		return strings.clone(strconv.write_int(buf[:], v.value, 10), allocator), true
	case Value_Predicate:
		return "X" if v.value else "", true
	case Value_String:
		return v.text, true
	case Value_World, Value_Structure, Value_Object, Value_Table, Value_Table_Iterator:
		return "", false
	}
	return "", true
}

value_write_scalar_text :: proc(out: ^strings.Builder, value: Value) -> bool {
	#partial switch v in value {
	case Value_Initial:
		return true
	case Value_Integer:
		buf: [32]byte
		strings.write_string(out, strconv.write_int(buf[:], v.value, 10))
		return true
	case Value_Predicate:
		if v.value {
			strings.write_byte(out, 'X')
		}
		return true
	case Value_String:
		strings.write_string(out, v.text)
		return true
	case Value_World, Value_Structure, Value_Object, Value_Table, Value_Table_Iterator:
		return false
	}
	return true
}

structure_is_system :: #force_inline proc "contextless" (value: Value) -> bool {
	#partial switch v in value {
	case Value_Structure:
		return v.name == "sy" ||
		       v.name == "syst" ||
		       (v.data != nil && (v.data.name == "sy" || v.data.name == "syst"))
	}
	return false
}

append_event :: proc(
	ctx: ^Context,
	kind: IO_Event_Kind,
	text, message_type: string,
	source: Source_Loc,
) {
	append(
		&ctx.events,
		IO_Event {
			kind = kind,
			text = strings.clone(text, ctx.allocator),
			message_type = strings.clone(message_type, ctx.allocator),
			source = source,
		},
	)
}

message_text :: proc(
	descriptor: Message_Descriptor,
	values: []Value,
	allocator: mem.Allocator,
) -> (string, bool) {
	out := strings.builder_make(allocator)
	if descriptor.message_type != "" {
		strings.write_string(&out, descriptor.message_type)
	}
	if descriptor.message_number != "" {
		if strings.builder_len(out) > 0 {
			strings.write_byte(&out, ' ')
		}
		strings.write_string(&out, descriptor.message_number)
	}
	if descriptor.message_id != "" {
		if strings.builder_len(out) > 0 {
			strings.write_byte(&out, ' ')
		}
		strings.write_byte(&out, '(')
		strings.write_string(&out, descriptor.message_id)
		strings.write_byte(&out, ')')
	}
	for value in values {
		if strings.builder_len(out) > 0 {
			strings.write_byte(&out, ' ')
		}
		if !value_write_scalar_text(&out, value) {
			return "", false
		}
	}
	return strings.to_string(out), true
}

named_value_less :: proc(left, right: Named_Value) -> bool {
	if left.scope != right.scope {
		return strings.compare(left.scope, right.scope) < 0
	}
	return strings.compare(left.name, right.name) < 0
}
