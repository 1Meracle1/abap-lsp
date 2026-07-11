package abap_frontend_vm_runtime

import ir "src:ir"

import "core:mem"

Type_Descriptor :: ^ir.Runtime_Type_Descriptor

Trap_Kind :: enum {
	None,
	Invalid_Module,
	Invalid_Function,
	Invalid_Instruction,
	Unsupported,
	Exception,
	Type,
	Overflow,
	Divide_By_Zero,
	Step_Limit,
}

Value_Kind :: enum {
	Initial,
	World,
	Integer,
	Decimal,
	Float,
	String,
	Structure,
	Object,
	Predicate,
	Table,
	Table_Iterator,
	Reference,
}

IO_Event_Kind :: enum {
	Write,
	Message,
}

IO_Policy_Kind :: enum {
	Default,
	Capture_All,
	Deny_All,
	Custom,
}

Arithmetic_Kind :: enum {
	Add,
	Subtract,
	Multiply,
	Divide,
	Integer_Divide,
	Modulo,
}

Comparison_Kind :: enum {
	Equal,
	Not_Equal,
	Less,
	Less_Equal,
	Greater,
	Greater_Equal,
}

Translate_Kind :: enum {
	To_Upper,
	To_Lower,
}

Replace_Occurrence :: enum {
	First,
	All,
}

Shift_Direction :: enum {
	Left,
	Right,
}

Find_Occurrence :: enum {
	First,
	All,
}

Find_Result :: struct {
	subrc:  i64,
	offset: i64,
	length: i64,
	count:  i64,
}

Search_Result :: struct {
	subrc: i64,
	fdpos: i64,
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

// Source_Loc is borrowed unless returned by source_loc_clone.
Source_Loc :: struct {
	path:  string,
	range: Source_Range,
}

IO_Policy :: struct {
	kind:            IO_Policy_Kind,
	capture_write:   bool,
	capture_message: bool,
}

Table_Data :: struct {
	allocator: mem.Allocator,
	refs:      int,
	rows:      [dynamic]Value,
}

Table_Component :: struct {
	path:  []string,
	value: Value,
}

Table_Sort_Component :: struct {
	path:       []string,
	descending: bool,
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
	matched:   bool,
	filters:   [dynamic]Table_Component,
}

Reference_Mode :: enum {
	Alias,
	Data,
	Binding,
}

Reference_Target_Kind :: enum {
	None,
	Cell,
	Global,
	Field,
}

Cell_Data :: struct {
	allocator: mem.Allocator,
	refs:      int,
	value:     Value,
}

Reference_Data :: struct {
	allocator:   mem.Allocator,
	refs:        int,
	mode:        Reference_Mode,
	target_kind: Reference_Target_Kind,
	cell:        ^Cell_Data,
	ctx:         ^Context,
	name:        string,
	base:        Value,
	field_name:  string,
	result_type: Type_Descriptor,
}

Value_Initial :: struct {}

Value_World :: struct {}

Value_Integer :: distinct i64

// Decimal values are stored exactly as coefficient * 10^-scale. i128 covers
// ABAP packed numbers and decfloat34 coefficients without a native dependency.
Value_Decimal :: struct {
	coefficient: i128,
	scale:       i32,
}

Value_Float :: distinct f64

Value_String :: distinct string

Value_Structure :: struct {
	name: string,
	data: ^Structure_Data,
}

Value_Object         :: distinct ^Object_Data
Value_Predicate      :: distinct bool
Value_Table          :: distinct ^Table_Data
Value_Table_Iterator :: distinct ^Table_Iterator_Data
Value_Reference      :: distinct ^Reference_Data

Value :: union {
	Value_Initial,
	Value_World,
	Value_Integer,
	Value_Decimal,
	Value_Float,
	Value_String,
	Value_Structure,
	Value_Object,
	Value_Predicate,
	Value_Table,
	Value_Table_Iterator,
	Value_Reference,
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

Exception_State :: struct {
	raised:    bool,
	type_name: string,
	value:     Value,
	source:    Source_Loc,
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
	exception:     Exception_State,
}

Message_Descriptor :: struct {
	message_id:    string,
	message_type:  string,
	message_number: string,
}

Call_Request :: struct {
	callee_name: string,
	values:      []Value,
	result_type: Type_Descriptor,
}

Table_Request :: struct {
	operation:       Table_Operation,
	values:          []Value,
	result_type:     Type_Descriptor,
	index:           int,
	components:      []Table_Component,
	sort_components: []Table_Sort_Component,
	descending:      bool,
}

Table_Read_Result :: struct {
	row:   Value,
	subrc: Value,
	tabix: Value,
}

Table_Next_Result :: struct {
	has_row:   Value,
	row:       Value,
	next_iter: Value,
	subrc:     Value,
	tabix:     Value,
}

Table_Mutate_Result :: struct {
	subrc: Value,
	tabix: Value,
}

Assign_Request :: struct {
	values: []Value,
}

SQL_Request :: struct {
	operation:   SQL_Operation,
	result_type: Type_Descriptor,
}

Field_Request :: struct {
	base:        Value,
	name:        string,
	value:       Value,
	result_type: Type_Descriptor,
}
