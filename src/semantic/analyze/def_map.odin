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
	Alias,
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
	case .Alias:
		return false
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

Type_Kind :: enum {
	Unknown,
	Builtin,
	Named,
	Structure,
	Table,
	Ref,
	Class,
	Interface,
}

Type_Data :: struct {
	id:         Type_Id,
	kind:       Type_Kind,
	name:       string,
	symbol:     Symbol_Id,
	structure:  Structure_Id,
	base:       Type_Id,
	table_form: ast.Data_Type_Form,
}

Decl_Info_State :: enum {
	Unresolved,
	Resolving,
	Resolved,
	Failed,
}

Decl_Info_Flag :: enum {
	Is_Static,
	Is_Redefinition,
	For_Event,
	Has_Implementation,
	Is_Abstract,
	Has_Declared_Type,
	Has_Event_Derived_Type,
	Is_Optional,
	Is_Untyped,
	Has_Default_Value,
}
Decl_Info_Flags :: bit_set[Decl_Info_Flag]

Decl_Parameter_Section :: enum {
	None,
	Method_Importing,
	Method_Exporting,
	Method_Changing,
	Method_Receiving,
	Method_Returning,
	Form_Tables,
	Form_Using,
	Form_Changing,
	Function_Importing,
	Function_Exporting,
	Function_Changing,
	Function_Tables,
}

Decl_Parameter_Passing :: enum {
	None,
	Direct,
	Value,
	Reference,
}

Decl_Signature_Parameter_Data :: struct {
	symbol:                   Entity_Id,
	name:                     string,
	range:                    tokenizer.Range,
	section:                  Decl_Parameter_Section,
	passing:                  Decl_Parameter_Passing,
	type_id:                  Type_Id,
	declared_type:            Field_Type_Ref_Data,
	type_clause_display:      string,
	type_clause_form:         ast.Data_Type_Form,
	has_type_clause_form:     bool,
	type_clause_table_has_of: bool,
	flags:                    Decl_Info_Flags,
}

Decl_Signature_Exception_Data :: struct {
	name:  string,
	range: tokenizer.Range,
}

Decl_Info_Data :: struct {
	id:                          Decl_Info_Id,
	entity:                      Entity_Id,
	owner:                       Entity_Id,
	scope:                       Scope_Id,
	signature_scope:             Scope_Id,
	body_scope:                  Scope_Id,
	name:                        string,
	kind:                        Symbol_Kind,
	decl_range:                  tokenizer.Range,
	name_range:                  tokenizer.Range,
	signature:                   string,
	clause_kind:                 ast.Decl_Clause_Kind,
	clause_flags:                ast.Decl_Clause_Flags,
	type_clause:                 ^ast.Data_Type_Clause,
	value_clause:                ^ast.Value_Clause,
	default_clause:              ^ast.Default_Clause,
	visibility:                  Visibility,
	member_kind:                 Class_Member_Kind,
	implementation_unit:         Unit_Id,
	implementation_range:        tokenizer.Range,
	event_name:                  string,
	event_range:                 tokenizer.Range,
	event_source_type:           Field_Type_Ref_Data,
	event_source_type_id:        Type_Id,
	alias_target_interface_name: string,
	alias_target_member_name:    string,
	signature_parameters:        [dynamic]Decl_Signature_Parameter_Data,
	signature_exceptions:        [dynamic]Decl_Signature_Exception_Data,
	parameter_section:           Decl_Parameter_Section,
	parameter_passing:           Decl_Parameter_Passing,
	flags:                       Decl_Info_Flags,
	state:                       Decl_Info_State,
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
	type_id:             Type_Id,
	type_unit:           Unit_Id,
	structure:           Structure_Id,
	structure_unit:      Unit_Id,
	declared_type:       Field_Type_Ref_Data,
	has_declared_type:   bool,
	type_clause_display: string,
	table_line:          ^Type_Fact_Data,
}

unknown_type_fact :: #force_inline proc() -> Type_Fact_Data {
	return Type_Fact_Data {
		type_id = UNKNOWN_TYPE_ID,
		type_unit = INVALID_UNIT_ID,
		structure = INVALID_STRUCTURE_ID,
		structure_unit = INVALID_UNIT_ID,
	}
}

type_fact_is_known :: #force_inline proc(fact: Type_Fact_Data) -> bool {
	return(
		type_id_is_known(fact.type_id) ||
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
	owner:                Entity_Id,
	scope:                Scope_Id,
	decl_info:            Decl_Info_Id,
	type_id:              Type_Id,
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
	type_clause_form:    ast.Data_Type_Form,
	has_type_clause_form: bool,
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
	Unresolved_Open_Sql_Source,
	Invalid_Open_Sql_Into_Target,
	Invalid_Open_Sql_Syntax,
	Invalid_Message,
	Invalid_Control_Break,
	Invalid_Constructor_For_Iterator_Reuse,
	Missing_Tables_Declaration,
	Unreachable_Code,
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
	Is_Include,
}
Structure_Field_Flags :: bit_set[Structure_Field_Flag]

Structure_Field_Data :: struct {
	name:                 string,
	decl_range:           tokenizer.Range,
	decl_unit:            Unit_Id,
	type_id:              Type_Id,
	structure:            Structure_Id,
	type_ref:             Field_Type_Ref_Data,
	type_clause_form:     ast.Data_Type_Form,
	has_type_clause_form: bool,
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
	type_id:              Type_Id,
	structure:            Structure_Id,
	type_ref:             Field_Type_Ref_Data,
	type_clause_form:     ast.Data_Type_Form,
	has_type_clause_form: bool,
	value_clause_display: string,
	description:          string,
	flags:                Structure_Field_Flags,
}

Structure_Data :: struct {
	id:               Structure_Id,
	origin_unit:      Unit_Id,
	origin_structure: Structure_Id,
	name:             string,
	scope:            Scope_Id,
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
	symbol:              Symbol_Id,
	section:             Method_Parameter_Section,
	name:                string,
	range:               tokenizer.Range,
	passing:             Parameter_Passing_Kind,
	type_id:             Type_Id,
	declared_type:       Field_Type_Ref_Data,
	type_clause_display: string,
	type_clause_form:    ast.Data_Type_Form,
	has_type_clause_form: bool,
	type_clause_table_has_of: bool,
	flags:               Class_Member_Parameter_Flags,
}

Function_Module_Parameter_Section :: enum {
	Importing,
	Exporting,
	Changing,
	Tables,
}

Function_Module_Exception_Data :: struct {
	name:  string,
	range: tokenizer.Range,
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
	symbol:                Symbol_Id,
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

Operand_Data :: struct {
	scope:      Scope_Id,
	range:      tokenizer.Range,
	mode:       Operand_Mode,
	type_fact:  Type_Fact_Data,
	symbol:     Symbol_Handle,
	has_symbol: bool,
	flags:      Operand_Flags,
}

Unit_Analysis :: struct {
	unit_id:                                Unit_Id,
	uri:                                    string,
	source:                                 string,
	source_mode:                            Source_Mode,
	root_scope:                             Scope_Id,
	scopes:                                 [dynamic]Scope_Data,
	symbols:                                [dynamic]Symbol_Data,
	decl_infos:                             [dynamic]Decl_Info_Data,
	types:                                  [dynamic]Type_Data,
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
	class_definitions:                      [dynamic]Class_Definition_Data,
	class_inheritance:                      [dynamic]Class_Inheritance_Data,
	class_friends:                          [dynamic]Class_Friend_Data,
	implemented_interfaces:                 [dynamic]Implemented_Interface_Data,
	member_aliases:                         [dynamic]Member_Alias_Data,
	named_arguments:                        [dynamic]Named_Argument_Access,
	call_sites:                             [dynamic]Call_Site_Data,
	assignment_sites:                       [dynamic]Assignment_Site_Data,
	concatenate_lines_of_sites:             [dynamic]Concatenate_Lines_Of_Site_Data,
	expression_facts:                       [dynamic]Expression_Fact_Data,
	operands:                               [dynamic]Operand_Data,
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
}

type_arena_init :: proc(unit: ^Unit_Analysis, allocator: mem.Allocator) {
	unit.types = make([dynamic]Type_Data, 0, 32, allocator)
	append(
		&unit.types,
		Type_Data {
			id = UNKNOWN_TYPE_ID,
			kind = .Unknown,
			symbol = INVALID_SYMBOL_ID,
			structure = INVALID_STRUCTURE_ID,
			base = UNKNOWN_TYPE_ID,
		},
	)
}

type_id_is_known :: #force_inline proc(id: Type_Id) -> bool {
	return id != UNKNOWN_TYPE_ID && id != INVALID_TYPE_ID
}

type_data :: proc(unit: ^Unit_Analysis, id: Type_Id) -> ^Type_Data {
	if id == INVALID_TYPE_ID || type_id_index(id) >= len(unit.types) {
		return nil
	}
	return &unit.types[type_id_index(id)]
}

type_intern :: proc(unit: ^Unit_Analysis, item: Type_Data) -> Type_Id {
	assert(len(unit.types) > 0)
	for &existing in unit.types {
		if type_data_equal(existing, item) {
			if existing.kind == .Named && type_id_is_known(item.base) && existing.base != item.base {
				existing.base = item.base
			}
			return existing.id
		}
	}
	id := Type_Id(u32(len(unit.types)))
	next := item
	next.id = id
	append(&unit.types, next)
	return id
}

type_data_equal :: proc(a, b: Type_Data) -> bool {
	if a.kind != b.kind {
		return false
	}
	#partial switch a.kind {
	case .Unknown:
		return true
	case .Builtin:
		return strings.equal_fold(a.name, b.name)
	case .Named:
		if a.symbol != INVALID_SYMBOL_ID || b.symbol != INVALID_SYMBOL_ID {
			return a.symbol == b.symbol
		}
		return strings.equal_fold(a.name, b.name) && a.base == b.base
	case .Structure:
		return a.structure == b.structure
	case .Table:
		return a.base == b.base && a.table_form == b.table_form
	case .Ref:
		return a.base == b.base
	case .Class, .Interface:
		if a.symbol != INVALID_SYMBOL_ID || b.symbol != INVALID_SYMBOL_ID {
			return a.symbol == b.symbol
		}
		return strings.equal_fold(a.name, b.name)
	}
	return false
}

type_builtin :: proc(unit: ^Unit_Analysis, name: string) -> Type_Id {
	if name == "" {
		return UNKNOWN_TYPE_ID
	}
	return type_intern(
		unit,
		Type_Data {
			kind = .Builtin,
			name = name,
			symbol = INVALID_SYMBOL_ID,
			structure = INVALID_STRUCTURE_ID,
			base = UNKNOWN_TYPE_ID,
		},
	)
}

type_named :: proc(unit: ^Unit_Analysis, name: string, symbol_id: Symbol_Id, base := UNKNOWN_TYPE_ID) -> Type_Id {
	if name == "" {
		return UNKNOWN_TYPE_ID
	}
	return type_intern(
		unit,
		Type_Data {
			kind = .Named,
			name = name,
			symbol = symbol_id,
			structure = INVALID_STRUCTURE_ID,
			base = base,
		},
	)
}

type_structure :: proc(unit: ^Unit_Analysis, structure_id: Structure_Id) -> Type_Id {
	st := structure(unit, structure_id)
	if st == nil {
		return UNKNOWN_TYPE_ID
	}
	return type_intern(
		unit,
		Type_Data {
			kind = .Structure,
			name = st.name,
			symbol = INVALID_SYMBOL_ID,
			structure = structure_id,
			base = UNKNOWN_TYPE_ID,
		},
	)
}

type_table :: proc(unit: ^Unit_Analysis, row: Type_Id, form: ast.Data_Type_Form) -> Type_Id {
	return type_intern(
		unit,
		Type_Data {
			kind = .Table,
			symbol = INVALID_SYMBOL_ID,
			structure = INVALID_STRUCTURE_ID,
			base = row,
			table_form = form,
		},
	)
}

type_ref :: proc(unit: ^Unit_Analysis, target: Type_Id) -> Type_Id {
	return type_intern(
		unit,
		Type_Data {
			kind = .Ref,
			symbol = INVALID_SYMBOL_ID,
			structure = INVALID_STRUCTURE_ID,
			base = target,
		},
	)
}

type_class_or_interface :: proc(
	unit: ^Unit_Analysis,
	name: string,
	symbol_id: Symbol_Id,
	kind: Symbol_Kind,
) -> Type_Id {
	type_kind := Type_Kind.Class if kind == .Class else Type_Kind.Interface
	return type_intern(
		unit,
		Type_Data {
			kind = type_kind,
			name = name,
			symbol = symbol_id,
			structure = INVALID_STRUCTURE_ID,
			base = UNKNOWN_TYPE_ID,
		},
	)
}

type_id_from_symbol_data :: proc(unit: ^Unit_Analysis, s: ^Symbol_Data, depth := 0) -> Type_Id {
	if s == nil {
		return UNKNOWN_TYPE_ID
	}
	return type_id_from_symbol_fields(
		unit,
		s.id,
		s.scope,
		s.name,
		s.kind,
		s.structure,
		s.declared_type,
		s.has_declared_type,
		s.type_clause_form,
		s.has_type_clause_form,
		depth,
	)
}

type_id_from_symbol_fields :: proc(
	unit: ^Unit_Analysis,
	symbol_id: Symbol_Id,
	scope_id: Scope_Id,
	name: string,
	kind: Symbol_Kind,
	structure_id: Structure_Id,
	declared_type: Field_Type_Ref_Data,
	has_declared_type: bool,
	type_form := ast.Data_Type_Form{},
	has_type_form := false,
	depth := 0,
) -> Type_Id {
	base := UNKNOWN_TYPE_ID
	if has_declared_type {
		base = type_id_from_declared_type(unit, scope_id, declared_type, type_form, has_type_form, depth + 1)
	}
	if structure_id != INVALID_STRUCTURE_ID {
		structure_type := type_structure(unit, structure_id)
		if has_type_form && type_form == .Range_Of {
			base = type_table(unit, structure_type, type_form)
		} else if !type_id_is_known(base) {
			base = structure_type
		}
	}
	if has_type_form && type_form_is_table_category(type_form) && !type_id_is_known(base) {
		base = type_table(unit, UNKNOWN_TYPE_ID, type_form)
	}
	#partial switch kind {
	case .Builtin_Type:
		return type_builtin(unit, name)
	case .Type_Def:
		return type_named(unit, name, symbol_id, base)
	case .Class, .Interface:
		return type_class_or_interface(unit, name, symbol_id, kind)
	}
	return base
}

type_id_from_declared_type :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	type_ref_data: Field_Type_Ref_Data,
	type_form := ast.Data_Type_Form{},
	has_type_form := false,
	depth := 0,
) -> Type_Id {
	if depth > 16 {
		return UNKNOWN_TYPE_ID
	}
	base := type_id_from_type_ref_path(unit, scope_id, type_ref_data, depth + 1)
	if has_type_form {
		#partial switch type_form {
		case .Type_Line_Of, .Like_Line_Of:
			return type_row_type(unit, base, depth + 1)
		case .Range_Of:
			return type_table(unit, base, type_form)
		case .Any_Table,
		     .Table,
		     .Like_Table,
		     .Index_Table,
		     .Standard_Table,
		     .Sorted_Table,
		     .Hashed_Table,
		     .Like_Standard_Table,
		     .Like_Sorted_Table,
		     .Like_Hashed_Table:
			return type_table(unit, base, type_form)
		}
	}
	return base
}

type_id_from_type_ref_path :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	type_ref_data: Field_Type_Ref_Data,
	depth: int,
) -> Type_Id {
	if depth > 16 || type_ref_data.base_name == "" {
		return UNKNOWN_TYPE_ID
	}
	current := UNKNOWN_TYPE_ID
	symbol_id, has_symbol := type_symbol_for_ref(unit, scope_id, type_ref_data)
	path_start := 0
	if has_symbol {
		current = type_id_from_symbol(unit, symbol_id, depth + 1)
		if s := symbol(unit, symbol_id);
		   s != nil &&
		   (s.kind == .Class || s.kind == .Interface) &&
		   len(type_ref_data.field_path) > 0 &&
		   type_selector_at(type_ref_data.field_selectors[:], 0) != .Dash {
			if nested, ok := type_class_symbol(unit, symbol_id, type_ref_data.field_path[0]); ok {
				current = type_id_from_symbol(unit, nested, depth + 1)
				path_start = 1
			}
		}
	} else if is_builtin_type_name(type_ref_data.base_name) {
		current = type_builtin(unit, type_ref_data.base_name)
	} else if type_ref_data.namespace == .Type {
		current = type_named(unit, type_ref_data.base_name, INVALID_SYMBOL_ID)
	}
	for i := path_start; i < len(type_ref_data.field_path); i += 1 {
		if i < len(type_ref_data.field_derefs) && type_ref_data.field_derefs[i] {
			current = type_ref_target(unit, current, depth + 1)
			continue
		}
		selector := type_selector_at(type_ref_data.field_selectors[:], i)
		name := type_ref_data.field_path[i]
		if selector == .Arrow {
			target := type_ref_target(unit, current, depth + 1)
			if class_symbol, ok := type_class_symbol_from_type(unit, target, depth + 1); ok {
				if member := unit_class_member_symbol(unit, class_symbol, name); member != nil {
					current = member.type_id
					continue
				}
			}
			return UNKNOWN_TYPE_ID
		}
		if selector == .Fat_Arrow || selector == .Tilde {
			if class_symbol, ok := type_class_symbol_from_type(unit, current, depth + 1); ok {
				if nested, nested_ok := type_class_symbol(unit, class_symbol, name); nested_ok {
					current = type_id_from_symbol(unit, nested, depth + 1)
					continue
				}
			}
			return UNKNOWN_TYPE_ID
		}
		if structure_id, ok := type_structure_id(unit, current, depth + 1); ok {
			if field := structure_field(unit, structure_id, name); field != nil {
				current = field.type_id
				if !type_id_is_known(current) && field.structure != INVALID_STRUCTURE_ID {
					current = type_structure(unit, field.structure)
				}
				continue
			}
		}
		return UNKNOWN_TYPE_ID
	}
	if type_ref_data.is_ref {
		return type_ref(unit, current)
	}
	return current
}

type_id_from_symbol :: proc(unit: ^Unit_Analysis, symbol_id: Symbol_Id, depth := 0) -> Type_Id {
	if depth > 16 {
		return UNKNOWN_TYPE_ID
	}
	s := symbol(unit, symbol_id)
	if s == nil {
		return UNKNOWN_TYPE_ID
	}
	if type_id_is_known(s.type_id) {
		return s.type_id
	}
	return type_id_from_symbol_data(unit, s, depth + 1)
}

type_row_type :: proc(unit: ^Unit_Analysis, id: Type_Id, depth := 0) -> Type_Id {
	if depth > 16 {
		return UNKNOWN_TYPE_ID
	}
	if t := type_data(unit, id); t != nil {
		#partial switch t.kind {
		case .Table:
			return t.base
		case .Named:
			return type_row_type(unit, t.base, depth + 1)
		}
	}
	return UNKNOWN_TYPE_ID
}

type_ref_target :: proc(unit: ^Unit_Analysis, id: Type_Id, depth := 0) -> Type_Id {
	if depth > 16 {
		return UNKNOWN_TYPE_ID
	}
	if t := type_data(unit, id); t != nil {
		#partial switch t.kind {
		case .Ref:
			return t.base
		case .Named:
			return type_ref_target(unit, t.base, depth + 1)
		}
	}
	return UNKNOWN_TYPE_ID
}

type_structure_id :: proc(unit: ^Unit_Analysis, id: Type_Id, depth := 0) -> (Structure_Id, bool) {
	if depth > 16 {
		return INVALID_STRUCTURE_ID, false
	}
	if t := type_data(unit, id); t != nil {
		#partial switch t.kind {
		case .Structure:
			return t.structure, true
		case .Named:
			return type_structure_id(unit, t.base, depth + 1)
		}
	}
	return INVALID_STRUCTURE_ID, false
}

type_class_symbol_from_type :: proc(unit: ^Unit_Analysis, id: Type_Id, depth := 0) -> (Symbol_Id, bool) {
	if depth > 16 {
		return INVALID_SYMBOL_ID, false
	}
	if t := type_data(unit, id); t != nil {
		#partial switch t.kind {
		case .Class, .Interface:
			return t.symbol, t.symbol != INVALID_SYMBOL_ID
		case .Named, .Ref:
			return type_class_symbol_from_type(unit, t.base, depth + 1)
		}
	}
	return INVALID_SYMBOL_ID, false
}

type_symbol_for_ref :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	type_ref_data: Field_Type_Ref_Data,
) -> (Symbol_Id, bool) {
	if symbol_id, ok := type_lookup_scope_chain(unit, scope_id, type_ref_data.namespace, type_ref_data.base_name);
	   ok {
		return symbol_id, true
	}
	if type_ref_data.namespace == .Type {
		if symbol_id, ok := type_lookup_scope_chain(unit, scope_id, .Value, type_ref_data.base_name);
		   ok {
			return symbol_id, true
		}
		if class_symbol, ok := type_enclosing_owner(unit, scope_id, .Class); ok {
			return type_class_symbol(unit, class_symbol, type_ref_data.base_name)
		}
		if interface_symbol, ok := type_enclosing_owner(unit, scope_id, .Interface); ok {
			return type_class_symbol(unit, interface_symbol, type_ref_data.base_name)
		}
	}
	return INVALID_SYMBOL_ID, false
}

type_lookup_scope_chain :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (Symbol_Id, bool) {
	current := scope_id
	for current != INVALID_SCOPE_ID {
		if id, ok := scope_lookup_declaration(unit, current, namespace, name); ok {
			return id, true
		}
		s := scope(unit, current)
		if s == nil {
			break
		}
		current = s.parent
	}
	return INVALID_SYMBOL_ID, false
}

type_enclosing_owner :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	kind: Scope_Kind,
) -> (Symbol_Id, bool) {
	current := scope_id
	for current != INVALID_SCOPE_ID {
		s := scope(unit, current)
		if s == nil {
			break
		}
		if s.kind == kind && s.owner != INVALID_SYMBOL_ID {
			return s.owner, true
		}
		current = s.parent
	}
	return INVALID_SYMBOL_ID, false
}

type_class_symbol :: proc(unit: ^Unit_Analysis, owner: Symbol_Id, name: string) -> (Symbol_Id, bool) {
	assert(len(unit.scope_index.enclosing_classes) == len(unit.scopes))
	key := Class_Scope_Index_Key{class_symbol = owner, namespace = .Type, name = name}
	if id, ok := unit.scope_index.class_symbols[key]; ok {
		return id, true
	}
	return INVALID_SYMBOL_ID, false
}

type_selector_at :: #force_inline proc(selectors: []ast.Selector_Op, index: int) -> ast.Selector_Op {
	return selectors[index] if index < len(selectors) else .Dash
}

push_decl_info :: proc(
	decl_infos: ^[dynamic]Decl_Info_Data,
	entity: Entity_Id,
	scope: Scope_Id,
	name: string,
	kind: Symbol_Kind,
	decl_range: tokenizer.Range,
	clause_kind := ast.Decl_Clause_Kind.Normal,
	clause_flags := ast.Decl_Clause_Flags{},
	type_clause: ^ast.Data_Type_Clause = nil,
	value_clause: ^ast.Value_Clause = nil,
	default_clause: ^ast.Default_Clause = nil,
) -> Decl_Info_Id {
	id := Decl_Info_Id(u32(len(decl_infos^)))
	append(
		decl_infos,
		Decl_Info_Data {
			id = id,
			entity = entity,
			owner = INVALID_SYMBOL_ID,
			scope = scope,
			signature_scope = INVALID_SCOPE_ID,
			body_scope = INVALID_SCOPE_ID,
			name = name,
			kind = kind,
			decl_range = decl_range,
			name_range = decl_range,
			clause_kind = clause_kind,
			clause_flags = clause_flags,
			type_clause = type_clause,
			value_clause = value_clause,
			default_clause = default_clause,
			implementation_unit = INVALID_UNIT_ID,
		},
	)
	return id
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
	type_id := UNKNOWN_TYPE_ID,
	owner := INVALID_SYMBOL_ID,
) -> Symbol_Id {
	scope_index := scope_id_index(scope)
	assert(scope_index >= 0 && scope_index < len(unit.scopes))
	id := Symbol_Id(u32(len(unit.symbols)))
	decl_info := INVALID_DECL_INFO_ID
	if !symbol_kind_is_builtin(kind) {
		decl_info = push_decl_info(
			&unit.decl_infos,
			id,
			scope,
			name,
			kind,
			decl_range,
		)
		if owner != INVALID_SYMBOL_ID {
			unit.decl_infos[decl_info_id_index(decl_info)].owner = owner
		}
	}
	resolved_type_id := type_id
	if !type_id_is_known(resolved_type_id) {
		resolved_type_id = type_id_from_symbol_fields(
			unit,
			id,
			scope,
			name,
			kind,
			structure,
			declared_type,
			has_declared_type,
			type_clause_form,
			has_type_clause_form,
		)
	}
	append(
		&unit.symbols,
		Symbol_Data {
			id = id,
			name = name,
			kind = kind,
			owner = owner,
			scope = scope,
			decl_info = decl_info,
			type_id = resolved_type_id,
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
	scope_record_declaration(unit, scope, id)
	return id
}

push_structure :: proc(
	unit: ^Unit_Analysis,
	name: string,
	fields: [dynamic]Structure_Field_Data,
	scope := INVALID_SCOPE_ID,
) -> Structure_Id {
	id := Structure_Id(u32(len(unit.structures)))
	append(
		&unit.structures,
		Structure_Data {
			id = id,
			origin_unit = unit.unit_id,
			origin_structure = id,
			name = name,
			scope = scope,
			fields = fields,
		},
	)
	_ = type_structure(unit, id)
	return id
}

decl_info :: proc(unit: ^Unit_Analysis, id: Decl_Info_Id) -> ^Decl_Info_Data {
	if id == INVALID_DECL_INFO_ID || decl_info_id_index(id) >= len(unit.decl_infos) {
		return nil
	}
	return &unit.decl_infos[decl_info_id_index(id)]
}

entity_decl_info :: proc(unit: ^Unit_Analysis, id: Entity_Id) -> ^Decl_Info_Data {
	s := symbol(unit, id)
	if s == nil {
		return nil
	}
	return decl_info(unit, s.decl_info)
}

entity_signature_parameter :: proc(
	unit: ^Unit_Analysis,
	owner: Entity_Id,
	name: string,
) -> ^Decl_Signature_Parameter_Data {
	info := entity_decl_info(unit, owner)
	if info == nil {
		return nil
	}
	for &param in info.signature_parameters {
		if strings.equal_fold(param.name, name) {
			return &param
		}
	}
	return nil
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

unit_class_member_symbol :: proc(unit: ^Unit_Analysis, class_symbol: Symbol_Id, name: string) -> ^Symbol_Data {
	for &s in unit.symbols {
		if s.owner != class_symbol || !strings.equal_fold(s.name, name) {
			continue
		}
		scope_data := scope(unit, s.scope)
		if scope_data != nil &&
		   (scope_data.kind == .Class || scope_data.kind == .Interface) &&
		   scope_data.owner == class_symbol {
			return &s
		}
	}
	return nil
}

range_contains_offset :: #force_inline proc(range: tokenizer.Range, offset: int) -> bool {
	return range.start <= offset && offset < range.end
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
	assert(owner != nil)
	info := Structure_Field_Info {
		owner                = structure_id,
		owner_unit           = owner.origin_unit,
		name                 = field.name,
		decl_range           = field.decl_range,
		decl_unit            = field.decl_unit,
		shape                = .Scalar,
		type_id              = field.type_id,
		structure            = field.structure,
		type_ref             = field.type_ref,
		type_clause_form     = field.type_clause_form,
		has_type_clause_form = field.has_type_clause_form,
		value_clause_display = field.value_clause_display,
		description          = field.description,
		flags                = field.flags,
	}
	if field.structure != INVALID_STRUCTURE_ID {
		info.shape = .Structured
	}
	return info, true
}

refresh_unit_type_ids :: proc(unit: ^Unit_Analysis) {
	assert(len(unit.scope_index.enclosing_classes) == len(unit.scopes))
	for i in 0 ..< len(unit.symbols) {
		s := &unit.symbols[i]
		s.type_id = type_id_from_symbol_data(unit, s)
	}
	for i in 0 ..< len(unit.structures) {
		st := &unit.structures[i]
		scope_id := st.scope
		if scope_id == INVALID_SCOPE_ID {
			scope_id = unit.root_scope
		}
		for j in 0 ..< len(st.fields) {
			field := &st.fields[j]
			field.type_id = type_id_from_structure_field(unit, scope_id, field^)
		}
	}
	for i in 0 ..< len(unit.decl_infos) {
		info := &unit.decl_infos[i]
		scope_id := info.signature_scope if info.signature_scope != INVALID_SCOPE_ID else info.scope
		if info.event_source_type.base_name != "" {
			info.event_source_type_id = type_id_from_declared_type(unit, scope_id, info.event_source_type)
		}
		for j in 0 ..< len(info.signature_parameters) {
			param := &info.signature_parameters[j]
			param.type_id = type_id_from_parameter_symbol_or_ref(
				unit,
				param.symbol,
				scope_id,
				param.declared_type,
				param.type_clause_form,
				param.has_type_clause_form,
				.Has_Declared_Type in param.flags,
			)
		}
	}
}

type_id_from_structure_field :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	field: Structure_Field_Data,
) -> Type_Id {
	if field.structure != INVALID_STRUCTURE_ID {
		return type_structure(unit, field.structure)
	}
	if .Has_Type_Ref in field.flags {
		return type_id_from_declared_type(
			unit,
			scope_id,
			field.type_ref,
			field.type_clause_form,
			field.has_type_clause_form,
		)
	}
	return UNKNOWN_TYPE_ID
}

type_id_from_parameter_symbol_or_ref :: proc(
	unit: ^Unit_Analysis,
	symbol_id: Symbol_Id,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	type_form: ast.Data_Type_Form,
	has_type_form: bool,
	has_type: bool,
) -> Type_Id {
	if s := symbol(unit, symbol_id); s != nil {
		return s.type_id
	}
	if has_type {
		return type_id_from_declared_type(unit, scope_id, type_ref, type_form, has_type_form)
	}
	return UNKNOWN_TYPE_ID
}

builtin_type_ref :: #force_inline proc(name: string) -> Field_Type_Ref_Data {
	return Field_Type_Ref_Data{namespace = .Type, base_name = name}
}
