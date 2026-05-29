package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

import "core:mem"
import "core:strings"

Symbol_Kind :: enum {
	Builtin_Type,
	Builtin_Routine,
	Builtin_Constant,
	Builtin_Variable,
	Variable,
	Constant,
	Enum_Member,
	Type_Def,
	Field_Symbol,
	Form,
	Parameter,
	Exception,
	Class,
	Interface,
	Method,
	Field,
	Include,
	Event,
	Module,
	Control,
	Report,
}

Visibility :: enum {
	Public,
	Protected,
	Private,
}

Class_Member_Kind :: enum {
	Attribute,
	Method,
	Event,
}

symbol_kind_is_builtin :: #force_inline proc(kind: Symbol_Kind) -> bool {
	return(
		kind == .Builtin_Type ||
		kind == .Builtin_Routine ||
		kind == .Builtin_Constant ||
		kind == .Builtin_Variable \
	)
}

symbol_kind_occupies :: proc(kind: Symbol_Kind, namespace: Namespace) -> bool {
	switch kind {
	case .Builtin_Type, .Type_Def, .Class, .Interface:
		return namespace == .Type
	case .Builtin_Routine, .Form, .Method, .Module, .Event:
		return namespace == .Routine
	case .Builtin_Constant,
	     .Builtin_Variable,
	     .Variable,
	     .Constant,
	     .Enum_Member,
	     .Field_Symbol,
	     .Parameter,
	     .Exception,
	     .Field,
	     .Include,
	     .Control,
	     .Report:
		return namespace == .Value
	}
	return false
}

Reference_Kind :: enum {
	Identifier,
	Type_Ref,
	Interface_Use,
	Structured_Decl_End,
	Message_Class,
	Routine_Call,
	Static_Target,
	Include,
}

Resolution_Kind :: enum {
	Symbol,
	Builtin_Type,
	Builtin_Routine,
	Internal_Table_Line,
	External,
}

Resolution :: struct {
	kind:   Resolution_Kind,
	symbol: Symbol_Handle,
}

Sql_Resolution :: enum {
	Unresolved,
	External,
	Local_Cte,
	Internal_Table,
	Hierarchy,
}

Sql_Source_Kind :: enum {
	From,
	Join,
}

Sql_Dynamic_Fragment_Kind :: enum {
	Source,
	Projection,
	Where,
}

Sql_Query_Flag :: enum {
	Has_Projection_Clause,
	Has_From_Clause,
	Has_Into_Clause,
	Has_Where_Clause,
	Has_Group_By_Clause,
	Has_Having_Clause,
	Has_Order_By_Clause,
	Order_By_Primary_Key,
	Has_For_All_Entries,
	Has_For_Update,
	Has_Up_To_Clause,
	Has_Package_Size_Clause,
	Has_Offset_Clause,
	Has_Abap_Options_Clause,
	Has_Set_Operator_Clause,
	Is_Single,
	Is_Distinct,
	Is_For_Update,
	Has_Package_Size,
	Has_Set_Operators,
	Has_Endselect,
	Has_Dynamic_Where,
}
Sql_Query_Flags :: bit_set[Sql_Query_Flag]

Sql_Query_Data :: struct {
	id:                     int,
	scope:                  Scope_Id,
	range:                  tokenizer.Range,
	projection_clause:      tokenizer.Range,
	from_clause:            tokenizer.Range,
	into_clause:            tokenizer.Range,
	where_clause:           tokenizer.Range,
	group_by_clause:        tokenizer.Range,
	having_clause:          tokenizer.Range,
	order_by_clause:        tokenizer.Range,
	order_by_fields:        [dynamic]string,
	for_all_entries_clause: tokenizer.Range,
	for_all_entries_name:   string,
	for_update_clause:      tokenizer.Range,
	up_to_clause:           tokenizer.Range,
	package_size_clause:    tokenizer.Range,
	offset_clause:          tokenizer.Range,
	abap_options_clause:    tokenizer.Range,
	set_operator_clause:    tokenizer.Range,
	flags:                  Sql_Query_Flags,
}

Sql_Source_Data :: struct {
	query_id:    int,
	range:       tokenizer.Range,
	source_kind: Sql_Source_Kind,
	name:        string,
	alias:       string,
	join_kind:   string,
	resolution:  Sql_Resolution,
}

Sql_Dynamic_Fragment_Data :: struct {
	query_id: int,
	scope:    Scope_Id,
	range:    tokenizer.Range,
	kind:     Sql_Dynamic_Fragment_Kind,
}

Sql_Projection_Kind :: enum {
	Star,
	Qualified_Star,
	Column,
	Aggregate,
	Expression,
}

Sql_Projection_Data :: struct {
	query_id:     int,
	range:        tokenizer.Range,
	kind:         Sql_Projection_Kind,
	source_alias: string,
	name:         string,
	alias:        string,
}

Sql_Name_Ref_Kind :: enum {
	Source,
	Alias,
	Column,
	Qualified_Column,
	Star,
	Qualified_Star,
	Aggregate,
	Function,
}

Sql_Name_Ref_Data :: struct {
	query_id:   int,
	scope:      Scope_Id,
	range:      tokenizer.Range,
	name:       string,
	qualifier:  string,
	kind:       Sql_Name_Ref_Kind,
	resolution: Sql_Resolution,
}

Sql_Predicate_Kind :: enum {
	Where,
	Join_On,
	Having,
	Dynamic_Where,
	For_All_Entries,
}

Sql_Predicate_Data :: struct {
	query_id: int,
	range:    tokenizer.Range,
	kind:     Sql_Predicate_Kind,
}

Sql_Target_Kind :: enum {
	Into,
	Appending,
}

Sql_Target_Flag :: enum {
	Has_Target_Range,
	Is_Table,
	Is_Corresponding,
	Is_Inline,
}
Sql_Target_Flags :: bit_set[Sql_Target_Flag]

Sql_Target_Data :: struct {
	query_id:     int,
	scope:        Scope_Id,
	range:        tokenizer.Range,
	target_range: tokenizer.Range,
	kind:         Sql_Target_Kind,
	target_name:  string,
	flags:        Sql_Target_Flags,
}

Create_Data_Type_Handle_Site_Data :: struct {
	scope:        Scope_Id,
	target_name:  string,
	target_range: tokenizer.Range,
	handle_name:  string,
	handle_range: tokenizer.Range,
}

Field_Type_Ref_Data :: struct {
	namespace:    Namespace,
	is_ref:       bool,
	base_name:    string,
	base_range:   tokenizer.Range,
	field_path:   [dynamic]string,
	field_ranges: [dynamic]tokenizer.Range,
	field_derefs: [dynamic]bool,
	field_selectors: [dynamic]ast.Selector_Op,
}

Type_Fact_Data :: struct {
	structure:           Structure_Id,
	declared_type:       Field_Type_Ref_Data,
	has_declared_type:   bool,
	type_clause_display: string,
	table_line:          ^Type_Fact_Data,
}

unknown_type_fact :: #force_inline proc() -> Type_Fact_Data {
	return Type_Fact_Data{structure = INVALID_STRUCTURE_ID}
}

type_fact_is_known :: #force_inline proc(fact: Type_Fact_Data) -> bool {
	return(
		fact.structure != INVALID_STRUCTURE_ID ||
		fact.has_declared_type ||
		fact.type_clause_display != "" ||
		fact.table_line != nil \
	)
}

Symbol_Data :: struct {
	id:                   Symbol_Id,
	name:                 string,
	kind:                 Symbol_Kind,
	scope:                Scope_Id,
	decl_range:           tokenizer.Range,
	structure:            Structure_Id,
	declared_type:        Field_Type_Ref_Data,
	has_declared_type:    bool,
	type_clause_display:  string,
	value_clause_display: string,
	type_clause_form:     ast.Data_Type_Form,
	has_type_clause_form: bool,
	type_clause_table_has_of: bool,
}

Reference_Data :: struct {
	id:                  Reference_Id,
	name:                string,
	namespace:           Namespace,
	kind:                Reference_Kind,
	scope:               Scope_Id,
	range:               tokenizer.Range,
	resolution:          Resolution,
	has_resolution:      bool,
	type_is_ref:         bool,
	type_has_path:       bool,
	type_first_selector: ast.Selector_Op,
}

Message_Class_Use_Data :: struct {
	name:  string,
	range: tokenizer.Range,
}

Message_Use_Flag :: enum {
	Has_Class_Range,
	Has_Id_Range,
}
Message_Use_Flags :: bit_set[Message_Use_Flag]

Message_Use_Data :: struct {
	range:           tokenizer.Range,
	class_name:      string,
	class_range:     tokenizer.Range,
	id:              string,
	id_range:        tokenizer.Range,
	with_arg_ranges: [dynamic]tokenizer.Range,
	flags:           Message_Use_Flags,
}

Message_Class_Entry_Data :: struct {
	class_name: string,
	id:         string,
	text:       string,
	range:      tokenizer.Range,
}

Diagnostic_Kind :: enum {
	Syntax_Error,
	Duplicate_Declaration,
	Shadowed_Symbol,
	Mismatched_Structured_Declaration,
	Unresolved_Reference,
	Unresolved_Include,
	Include_Cycle,
	Wrong_Namespace,
	Unknown_Field,
	Invalid_Builtin_Named_Argument,
	Invalid_Perform_Call,
	Abstract_Class_Instantiation,
	Missing_Method_Implementation,
	Missing_Super_Constructor_Call,
	Invalid_Object_Type_Reference,
	Invalid_Parameter_Type,
	Invalid_Generic_Table_Type,
	Invalid_Generic_Builtin_Type,
	Invalid_Create_Data_Target,
	Invalid_Create_Data_Type_Handle,
	Incompatible_Assignment_Type,
	Incompatible_Argument_Type,
	Invalid_Concatenate_Source,
	Unknown_Named_Parameter,
	Unknown_Function_Module_Exception,
	Duplicate_Named_Parameter,
	Missing_Required_Parameter,
	Unverified_Open_Sql_Source,
	Invalid_Open_Sql_Into_Target,
	Invalid_Open_Sql_Syntax,
	Invalid_Message,
	Invalid_Control_Break,
	Invalid_Constructor_For_Iterator_Reuse,
	Missing_Tables_Declaration,
	Unreachable_Code,
	Use_Before_Definite_Assignment,
	Possibly_Unbound_Field_Symbol,
	Dead_Store,
	Unsorted_Read_Table_Binary_Search,
}

Diagnostic :: struct {
	kind:    Diagnostic_Kind,
	range:   tokenizer.Range,
	message: string,
}

Include_Edge :: struct {
	name:       string,
	range:      tokenizer.Range,
	target:     Unit_Id,
	has_target: bool,
	if_found:   bool,
}

Table_Work_Area_Data :: struct {
	name:  string,
	scope: Scope_Id,
	range: tokenizer.Range,
}

Field_Access_Segment :: struct {
	name:                string,
	range:               tokenizer.Range,
	selector:            ast.Selector_Op,
	deref:               bool,
	interface_name:      string,
	interface_range:     tokenizer.Range,
	interface_qualified: bool,
}

Field_Access :: struct {
	scope:            Scope_Id,
	base_namespace:   Namespace,
	base_name:        string,
	base_range:       tokenizer.Range,
	field_path:       [dynamic]Field_Access_Segment,
	in_type_position: bool,
}

Loop_Where_Field_Context :: struct {
	scope:         Scope_Id,
	range:         tokenizer.Range,
	source_access: Field_Access,
	target_access: Field_Access,
	has_target:    bool,
}

Constructor_For_Binding_Data :: struct {
	scope:             Scope_Id,
	range:             tokenizer.Range,
	name:              string,
	source_access:     Field_Access,
	has_source_access: bool,
}

Loop_At_Field_Context :: struct {
	scope:         Scope_Id,
	range:         tokenizer.Range,
	source_access: Field_Access,
	target_access: Field_Access,
	has_target:    bool,
}

Structure_Field_Flag :: enum {
	Has_Decl_Range,
	Has_Type_Ref,
	Is_Key,
}
Structure_Field_Flags :: bit_set[Structure_Field_Flag]

Structure_Field_Data :: struct {
	name:                 string,
	decl_range:           tokenizer.Range,
	decl_unit:            Unit_Id,
	structure:            Structure_Id,
	type_ref:             Field_Type_Ref_Data,
	value_clause_display: string,
	description:          string,
	flags:                Structure_Field_Flags,
}

Structure_Field_Shape_Kind :: enum {
	Scalar,
	Structured,
}

Structure_Field_Info :: struct {
	owner:                Structure_Id,
	owner_unit:           Unit_Id,
	name:                 string,
	decl_range:           tokenizer.Range,
	decl_unit:            Unit_Id,
	shape:                Structure_Field_Shape_Kind,
	structure:            Structure_Id,
	type_ref:             Field_Type_Ref_Data,
	value_clause_display: string,
	description:          string,
	flags:                Structure_Field_Flags,
}

Structure_Data :: struct {
	id:               Structure_Id,
	origin_unit:      Unit_Id,
	origin_structure: Structure_Id,
	name:             string,
	fields:           [dynamic]Structure_Field_Data,
}

Method_Parameter_Section :: enum {
	Importing,
	Exporting,
	Changing,
	Receiving,
	Returning,
}

Class_Member_Parameter_Flag :: enum {
	Has_Declared_Type,
	Is_Optional,
}
Class_Member_Parameter_Flags :: bit_set[Class_Member_Parameter_Flag]

Parameter_Passing_Kind :: enum {
	Direct,
	Value,
	Reference,
}

Class_Member_Parameter_Data :: struct {
	section:             Method_Parameter_Section,
	name:                string,
	range:               tokenizer.Range,
	passing:             Parameter_Passing_Kind,
	declared_type:       Field_Type_Ref_Data,
	type_clause_display: string,
	type_clause_form:    ast.Data_Type_Form,
	has_type_clause_form: bool,
	type_clause_table_has_of: bool,
	flags:               Class_Member_Parameter_Flags,
}

Form_Parameter_Section :: enum {
	Tables,
	Using,
	Changing,
}

Form_Parameter_Passing_Kind :: enum {
	Direct,
	Value,
	Reference,
}

Form_Parameter_Data :: struct {
	symbol:  Symbol_Id,
	section: Form_Parameter_Section,
	passing: Form_Parameter_Passing_Kind,
}

Form_Routine_Data :: struct {
	symbol:     Symbol_Id,
	signature:  string,
	parameters: [dynamic]Form_Parameter_Data,
}

Function_Module_Parameter_Section :: enum {
	Importing,
	Exporting,
	Changing,
	Tables,
}

Function_Module_Parameter_Flag :: enum {
	Has_Declared_Type,
	Is_Untyped,
	Is_Optional,
	Has_Default_Value,
}
Function_Module_Parameter_Flags :: bit_set[Function_Module_Parameter_Flag]

Function_Module_Parameter_Data :: struct {
	section:             Function_Module_Parameter_Section,
	name:                string,
	range:               tokenizer.Range,
	passing:             Parameter_Passing_Kind,
	declared_type:       Field_Type_Ref_Data,
	type_clause_display: string,
	flags:               Function_Module_Parameter_Flags,
}

Function_Module_Exception_Data :: struct {
	name:  string,
	range: tokenizer.Range,
}

Function_Module_Data :: struct {
	symbol:     Symbol_Id,
	signature:  string,
	parameters: [dynamic]Function_Module_Parameter_Data,
	exceptions: [dynamic]Function_Module_Exception_Data,
}

Class_Member_Implementation_Data :: struct {
	unit:  Unit_Id,
	range: tokenizer.Range,
}

Class_Member_Flag :: enum {
	Is_Static,
	Is_Redefinition,
	For_Event,
	Has_Implementation_Range,
	Has_Implementation,
}
Class_Member_Flags :: bit_set[Class_Member_Flag]

Class_Member_Data :: struct {
	class_symbol:         Symbol_Id,
	name:                 string,
	kind:                 Class_Member_Kind,
	visibility:           Visibility,
	decl_range:           tokenizer.Range,
	implementation_range: tokenizer.Range,
	implementation:       Class_Member_Implementation_Data,
	signature:            string,
	parameters:           [dynamic]Class_Member_Parameter_Data,
	exceptions:           [dynamic]Function_Module_Exception_Data,
	event_name:           string,
	event_range:          tokenizer.Range,
	event_source_type:    Field_Type_Ref_Data,
	structure:            Structure_Id,
	flags:                Class_Member_Flags,
}

Class_Inheritance_Data :: struct {
	class_symbol:    Symbol_Id,
	superclass_name: string,
}

Class_Friend_Data :: struct {
	class_symbol: Symbol_Id,
	friend_name:  string,
	range:        tokenizer.Range,
}

Class_Definition_Data :: struct {
	class_symbol: Symbol_Id,
	is_abstract:  bool,
}

Implemented_Interface_Data :: struct {
	owner_symbol:   Symbol_Id,
	interface_name: string,
	range:          tokenizer.Range,
}

Member_Alias_Data :: struct {
	owner_symbol:          Symbol_Id,
	alias_name:            string,
	target_interface_name: string,
	target_member_name:    string,
	range:                 tokenizer.Range,
}

Named_Argument_Section :: enum {
	Exporting,
	Importing,
	Changing,
	Tables,
	Receiving,
	Exceptions,
}

Named_Argument_Target_Kind :: enum {
	Constructor,
	Function,
	Report,
	Routine,
	Implicit_Method,
	Method,
	Event,
}

Named_Argument_Target :: struct {
	kind:                Named_Argument_Target_Kind,
	type_name:           string,
	function_name:       string,
	report_name:         string,
	routine_name:        string,
	method_name:         string,
	method_range:        tokenizer.Range,
	receiver_path:       [dynamic]Field_Access_Segment,
	base_namespace:      Namespace,
	base_name:           string,
	interface_qualified: bool,
	event_qualifier:     string,
	event_name:          string,
}

Named_Argument_Access :: struct {
	scope:       Scope_Id,
	name:        string,
	range:       tokenizer.Range,
	section:     Named_Argument_Section,
	has_section: bool,
	target:      Named_Argument_Target,
}

Call_Argument_Data :: struct {
	range:       tokenizer.Range,
	name:        string,
	section:     Named_Argument_Section,
	has_section: bool,
	ordinal:     int,
	type_fact:   Type_Fact_Data,
}

Call_Site_Data :: struct {
	scope:     Scope_Id,
	range:     tokenizer.Range,
	target:    Named_Argument_Target,
	arguments: [dynamic]Call_Argument_Data,
}

Assignment_Site_Flag :: enum {
	Has_Lhs_Target_Access,
	Rhs_Is_Top_Level_Sum,
	Assigns_Table_Line,
	Is_Corresponding,
}
Assignment_Site_Flags :: bit_set[Assignment_Site_Flag]

Assignment_Site_Data :: struct {
	scope:             Scope_Id,
	range:             tokenizer.Range,
	lhs_range:         tokenizer.Range,
	rhs_range:         tokenizer.Range,
	lhs_target_access: Field_Access,
	lhs:               Type_Fact_Data,
	rhs:               Type_Fact_Data,
	flags:             Assignment_Site_Flags,
}

Concatenate_Lines_Of_Site_Data :: struct {
	scope:        Scope_Id,
	range:        tokenizer.Range,
	source_range: tokenizer.Range,
	source:       Type_Fact_Data,
	byte_mode:    bool,
}

Expression_Fact_Kind :: enum {
	Reference,
	Selector,
	Call_Result,
}

Expression_Fact_Data :: struct {
	scope:     Scope_Id,
	range:     tokenizer.Range,
	kind:      Expression_Fact_Kind,
	type_fact: Type_Fact_Data,
}

Value_Flow_Kind :: enum {
	Assignment,
	Call_Argument,
	Field_Symbol_Assignment,
	Conditional_Field_Symbol_Assignment,
}

Value_Flow_Target_Kind :: enum {
	Assignment,
	Call_Parameter,
	Field_Symbol,
}

Value_Flow_Target_Data :: struct {
	kind:                     Value_Flow_Target_Kind,
	range:                    tokenizer.Range,
	call_range:               tokenizer.Range,
	target:                   Named_Argument_Target,
	parameter_name:           string,
	parameter_decl_unit:      Unit_Id,
	parameter_decl_range:     tokenizer.Range,
	has_parameter_decl_range: bool,
	name:                     string,
}

Value_Flow_Edge_Data :: struct {
	scope:        Scope_Id,
	kind:         Value_Flow_Kind,
	source_range: tokenizer.Range,
	source_type:  Type_Fact_Data,
	target:       Value_Flow_Target_Data,
	target_type:  Type_Fact_Data,
}

Perform_Parameter_Section :: enum {
	Tables,
	Using,
	Changing,
}

Perform_Argument_Data :: struct {
	range:              tokenizer.Range,
	section:            Perform_Parameter_Section,
	ordinal_in_section: int,
}

Perform_Program_Data :: struct {
	name:       string,
	range:      tokenizer.Range,
	is_dynamic: bool,
}

Perform_Call_Flag :: enum {
	Is_Dynamic,
	Has_Program,
	Has_If_Found,
	Section_Order_Invalid,
}
Perform_Call_Flags :: bit_set[Perform_Call_Flag]

Perform_Call_Data :: struct {
	scope:         Scope_Id,
	range:         tokenizer.Range,
	routine_name:  string,
	routine_range: tokenizer.Range,
	program:       Perform_Program_Data,
	parameters:    [dynamic]Perform_Parameter_Section,
	arguments:     [dynamic]Perform_Argument_Data,
	flags:         Perform_Call_Flags,
}

Routine_Loop_Kind :: enum {
	While,
	Do,
	Loop,
}

Routine_Site_Kind :: enum {
	Unknown_Effect,
	Clear,
	Unassign,
	Delete,
	Read_Table,
	Return,
	Raise,
	Leave,
	Leave_List_Processing,
	Exit,
	Continue,
	Stop,
}

Routine_Site_Data :: struct {
	scope:        Scope_Id,
	range:        tokenizer.Range,
	kind:         Routine_Site_Kind,
	target_range: tokenizer.Range,
	has_target:   bool,
}

Internal_Table_Order_Data :: struct {
	scope:      Scope_Id,
	range:      tokenizer.Range,
	table_name: string,
	key_fields: [dynamic]string,
}

Read_Table_Binary_Search_Data :: struct {
	scope:      Scope_Id,
	range:      tokenizer.Range,
	table_name: string,
	key_fields: [dynamic]string,
}

Find_Write_Target_Data :: struct {
	range:               tokenizer.Range,
	definitely_assigned: bool,
}

Find_Site_Data :: struct {
	scope:         Scope_Id,
	range:         tokenizer.Range,
	read_ranges:   [dynamic]tokenizer.Range,
	write_targets: [dynamic]Find_Write_Target_Data,
}

System_Field_Statement_Kind :: enum {
	Append,
	Assign,
	Authority_Check,
	Call_Function,
	Convert,
	Delete_Report,
	Delete_Table,
	Delete_Db_Table,
	Describe_Table,
	Do,
	Find,
	Insert_Report,
	Insert_Table,
	Insert_Db_Table,
	Insert_Textpool,
	Loop_At,
	Message,
	Modify_Table,
	Modify_Db_Table,
	Read_Report,
	Read_Table,
	Search,
	Select,
	Syntax_Check,
	Update_Db_Table,
	While,
}

System_Field_Update_Data :: struct {
	scope:      Scope_Id,
	range:      tokenizer.Range,
	statement:  System_Field_Statement_Kind,
	field_name: string,
}

Field_Symbol_State_Check_Kind :: enum {
	Is_Assigned,
	Is_Not_Assigned,
}

Value_State_Check_Kind :: enum {
	Is_Initial,
	Is_Not_Initial,
	Equals_Zero,
	Not_Equals_Zero,
	Condition_Probe,
}

Field_Symbol_State_Check_Data :: struct {
	scope:        Scope_Id,
	range:        tokenizer.Range,
	symbol_name:  string,
	symbol_range: tokenizer.Range,
	kind:         Field_Symbol_State_Check_Kind,
}

Value_State_Check_Data :: struct {
	scope:        Scope_Id,
	range:        tokenizer.Range,
	symbol_name:  string,
	symbol_range: tokenizer.Range,
	field_name:   string,
	kind:         Value_State_Check_Kind,
}

If_Region_Data :: struct {
	scope:         Scope_Id,
	range:         tokenizer.Range,
	then_scope:    Scope_Id,
	elseif_scopes: [dynamic]Scope_Id,
	else_scope:    Scope_Id,
}

Case_Region_Data :: struct {
	scope:           Scope_Id,
	range:           tokenizer.Range,
	when_scopes:     [dynamic]Scope_Id,
	has_when_others: bool,
}

Loop_Region_Flag :: enum {
	Has_Source_Access,
	Has_Target_Access,
}
Loop_Region_Flags :: bit_set[Loop_Region_Flag]

Loop_Region_Data :: struct {
	scope:         Scope_Id,
	range:         tokenizer.Range,
	kind:          Routine_Loop_Kind,
	body_scope:    Scope_Id,
	source_access: Field_Access,
	target_access: Field_Access,
	flags:         Loop_Region_Flags,
}

At_Group_Kind :: enum {
	First,
	New,
	End_Of,
	Last,
}

At_Region_Data :: struct {
	scope:      Scope_Id,
	range:      tokenizer.Range,
	kind:       At_Group_Kind,
	body_scope: Scope_Id,
}

Try_Region_Data :: struct {
	scope:         Scope_Id,
	range:         tokenizer.Range,
	body_scope:    Scope_Id,
	catch_scopes:  [dynamic]Scope_Id,
	cleanup_scope: Scope_Id,
}

Routine_Control_Region_Kind :: enum {
	If,
	Case,
	Loop,
	At,
	Try,
}

Routine_Control_Region_Data :: struct {
	kind:  Routine_Control_Region_Kind,
	if_:   If_Region_Data,
	case_: Case_Region_Data,
	loop:  Loop_Region_Data,
	at:    At_Region_Data,
	try:   Try_Region_Data,
}

Unit_Analysis :: struct {
	unit_id:                                Unit_Id,
	uri:                                    string,
	source:                                 string,
	source_mode:                            Source_Mode,
	root_scope:                             Scope_Id,
	scopes:                                 [dynamic]Scope_Data,
	symbols:                                [dynamic]Symbol_Data,
	structures:                             [dynamic]Structure_Data,
	references:                             [dynamic]Reference_Data,
	message_default_class:                  Message_Class_Use_Data,
	has_message_default_class:              bool,
	message_uses:                           [dynamic]Message_Use_Data,
	message_class_entries:                  [dynamic]Message_Class_Entry_Data,
	diagnostics:                            [dynamic]Diagnostic,
	include_edges:                          [dynamic]Include_Edge,
	table_work_areas:                       [dynamic]Table_Work_Area_Data,
	selection_screen_report_type_positions: [dynamic]tokenizer.Range,
	field_accesses:                         [dynamic]Field_Access,
	loop_where_field_contexts:              [dynamic]Loop_Where_Field_Context,
	loop_at_field_contexts:                 [dynamic]Loop_At_Field_Context,
	constructor_for_bindings:               [dynamic]Constructor_For_Binding_Data,
	class_members:                          [dynamic]Class_Member_Data,
	class_definitions:                      [dynamic]Class_Definition_Data,
	class_inheritance:                      [dynamic]Class_Inheritance_Data,
	class_friends:                          [dynamic]Class_Friend_Data,
	implemented_interfaces:                 [dynamic]Implemented_Interface_Data,
	member_aliases:                         [dynamic]Member_Alias_Data,
	form_routines:                          [dynamic]Form_Routine_Data,
	function_modules:                       [dynamic]Function_Module_Data,
	named_arguments:                        [dynamic]Named_Argument_Access,
	call_sites:                             [dynamic]Call_Site_Data,
	assignment_sites:                       [dynamic]Assignment_Site_Data,
	concatenate_lines_of_sites:             [dynamic]Concatenate_Lines_Of_Site_Data,
	expression_facts:                       [dynamic]Expression_Fact_Data,
	value_flow_edges:                       [dynamic]Value_Flow_Edge_Data,
	perform_calls:                          [dynamic]Perform_Call_Data,
	find_sites:                             [dynamic]Find_Site_Data,
	system_field_updates:                   [dynamic]System_Field_Update_Data,
	routine_sites:                          [dynamic]Routine_Site_Data,
	internal_table_orders:                  [dynamic]Internal_Table_Order_Data,
	read_table_binary_searches:             [dynamic]Read_Table_Binary_Search_Data,
	field_symbol_state_checks:              [dynamic]Field_Symbol_State_Check_Data,
	value_state_checks:                     [dynamic]Value_State_Check_Data,
	routine_control_regions:                [dynamic]Routine_Control_Region_Data,
	sql_queries:                            [dynamic]Sql_Query_Data,
	sql_sources:                            [dynamic]Sql_Source_Data,
	sql_dynamic_fragments:                  [dynamic]Sql_Dynamic_Fragment_Data,
	sql_projections:                        [dynamic]Sql_Projection_Data,
	sql_name_refs:                          [dynamic]Sql_Name_Ref_Data,
	sql_predicates:                         [dynamic]Sql_Predicate_Data,
	sql_targets:                            [dynamic]Sql_Target_Data,
	create_data_type_handles:               [dynamic]Create_Data_Type_Handle_Site_Data,
	provided_names:                         [dynamic]string,
	scope_index:                            Scope_Index,
	semantic_index:                         Semantic_Index,
}

declare_symbol :: proc(
	unit: ^Unit_Analysis,
	scope: Scope_Id,
	name: string,
	kind: Symbol_Kind,
	decl_range: tokenizer.Range,
	structure := INVALID_STRUCTURE_ID,
	declared_type := Field_Type_Ref_Data{},
	has_declared_type := false,
	type_clause_display := "",
	value_clause_display := "",
	type_clause_form := ast.Data_Type_Form{},
	has_type_clause_form := false,
	type_clause_table_has_of := false,
) -> Symbol_Id {
	id := Symbol_Id(u32(len(unit.symbols)))
	append(
		&unit.symbols,
		Symbol_Data {
			id = id,
			name = name,
			kind = kind,
			scope = scope,
			decl_range = decl_range,
			structure = structure,
			declared_type = declared_type,
			has_declared_type = has_declared_type,
			type_clause_display = type_clause_display,
			value_clause_display = value_clause_display,
			type_clause_form = type_clause_form,
			has_type_clause_form = has_type_clause_form,
			type_clause_table_has_of = type_clause_table_has_of,
		},
	)
	append(&unit.scopes[scope_id_index(scope)].declarations, id)
	return id
}

push_structure :: proc(
	unit: ^Unit_Analysis,
	name: string,
	fields: [dynamic]Structure_Field_Data,
) -> Structure_Id {
	id := Structure_Id(u32(len(unit.structures)))
	append(
		&unit.structures,
		Structure_Data {
			id = id,
			origin_unit = unit.unit_id,
			origin_structure = id,
			name = name,
			fields = fields,
		},
	)
	return id
}

symbol :: proc(unit: ^Unit_Analysis, id: Symbol_Id) -> ^Symbol_Data {
	if id == INVALID_SYMBOL_ID || symbol_id_index(id) >= len(unit.symbols) {
		return nil
	}
	return &unit.symbols[symbol_id_index(id)]
}

structure :: proc(unit: ^Unit_Analysis, id: Structure_Id) -> ^Structure_Data {
	if id == INVALID_STRUCTURE_ID || structure_id_index(id) >= len(unit.structures) {
		return nil
	}
	return &unit.structures[structure_id_index(id)]
}

scope :: proc(unit: ^Unit_Analysis, id: Scope_Id) -> ^Scope_Data {
	if id == INVALID_SCOPE_ID || scope_id_index(id) >= len(unit.scopes) {
		return nil
	}
	return &unit.scopes[scope_id_index(id)]
}

find_symbol :: proc(unit: ^Unit_Analysis, name: string, kind: Symbol_Kind) -> ^Symbol_Data {
	for &s in unit.symbols {
		if s.kind == kind && strings.equal_fold(s.name, name) {
			return &s
		}
	}
	return nil
}

rebuild_semantic_index :: proc(unit: ^Unit_Analysis, allocator: mem.Allocator) {
	semantic_index_destroy(&unit.semantic_index)
	unit.semantic_index = build_semantic_index(unit, allocator)
}

find_structure :: proc(unit: ^Unit_Analysis, name: string) -> ^Structure_Data {
	for &s in unit.structures {
		if strings.equal_fold(s.name, name) {
			return &s
		}
	}
	return nil
}

structure_field :: proc(
	unit: ^Unit_Analysis,
	structure_id: Structure_Id,
	field_name: string,
) -> ^Structure_Field_Data {
	s := structure(unit, structure_id)
	if s == nil {
		return nil
	}
	for &field in s.fields {
		if strings.equal_fold(field.name, field_name) {
			return &field
		}
	}
	return nil
}

structure_field_info :: proc(
	unit: ^Unit_Analysis,
	structure_id: Structure_Id,
	field_name: string,
) -> (
	Structure_Field_Info,
	bool,
) {
	field := structure_field(unit, structure_id, field_name)
	if field == nil {
		return {}, false
	}
	owner := structure(unit, structure_id)
	info := Structure_Field_Info {
		owner                = structure_id,
		owner_unit           = owner.origin_unit,
		name                 = field.name,
		decl_range           = field.decl_range,
		decl_unit            = field.decl_unit,
		shape                = .Scalar,
		structure            = field.structure,
		type_ref             = field.type_ref,
		value_clause_display = field.value_clause_display,
		description          = field.description,
		flags                = field.flags,
	}
	if field.structure != INVALID_STRUCTURE_ID {
		info.shape = .Structured
	}
	return info, true
}

builtin_type_ref :: #force_inline proc(name: string) -> Field_Type_Ref_Data {
	return Field_Type_Ref_Data{namespace = .Type, base_name = name}
}
