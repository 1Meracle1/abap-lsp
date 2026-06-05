package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

// Temporary AST-derived body-model caches. These should collapse into
// checker-local state and AST semantics as body checking is moved to the final
// Odin-style checker pipeline.

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
	scope:                     Scope_Id,
	base_namespace:            Namespace,
	base_name:                 string,
	base_range:                tokenizer.Range,
	node:                      ^ast.Node,
	field_path:                [dynamic]Field_Access_Segment,
	in_type_position:          bool,
	requires_known_base_shape: bool,
	where_candidate_name:      string,
}

Table_Expr_Data :: struct {
	scope:        Scope_Id,
	range:        tokenizer.Range,
	node:         ^ast.Node,
	table_access: Field_Access,
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
	value_range: tokenizer.Range,
	name:        string,
	section:     Named_Argument_Section,
	has_section: bool,
	ordinal:     int,
	type_fact:   Type_Fact_Data,
}

Call_Site_Data :: struct {
	scope:     Scope_Id,
	range:     tokenizer.Range,
	node:      ^ast.Node,
	target:    Named_Argument_Target,
	arguments: [dynamic]Call_Argument_Data,
}

Call_Function_Exception_Message_Site_Data :: struct {
	range:     tokenizer.Range,
	type_fact: Type_Fact_Data,
}

Assignment_Site_Flag :: enum {
	Has_Lhs_Target_Access,
	Rhs_Is_Top_Level_Sum,
	Assigns_Table_Line,
	Is_Corresponding,
	Is_Downcast,
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

Create_Data_Type_Handle_Site_Data :: struct {
	scope:        Scope_Id,
	target_name:  string,
	target_range: tokenizer.Range,
	handle_name:  string,
	handle_range: tokenizer.Range,
}

Source_File_Fact_Model :: struct {
	message_default_class:                 Message_Class_Use_Data,
	has_message_default_class:             bool,
	message_uses:                          [dynamic]Message_Use_Data,
	table_work_areas:                      [dynamic]Table_Work_Area_Data,
	field_accesses:                        [dynamic]Field_Access,
	table_exprs:                           [dynamic]Table_Expr_Data,
	loop_where_field_contexts:             [dynamic]Loop_Where_Field_Context,
	loop_at_field_contexts:                [dynamic]Loop_At_Field_Context,
	constructor_for_bindings:              [dynamic]Constructor_For_Binding_Data,
	named_arguments:                       [dynamic]Named_Argument_Access,
	call_sites:                            [dynamic]Call_Site_Data,
	call_function_exception_message_sites: [dynamic]Call_Function_Exception_Message_Site_Data,
	assignment_sites:                      [dynamic]Assignment_Site_Data,
	concatenate_lines_of_sites:            [dynamic]Concatenate_Lines_Of_Site_Data,
	sql_queries:                           [dynamic]Sql_Query_Data,
	sql_sources:                           [dynamic]Sql_Source_Data,
	sql_dynamic_fragments:                 [dynamic]Sql_Dynamic_Fragment_Data,
	sql_projections:                       [dynamic]Sql_Projection_Data,
	sql_name_refs:                         [dynamic]Sql_Name_Ref_Data,
	sql_predicates:                        [dynamic]Sql_Predicate_Data,
	sql_predicate_names:                   [dynamic]Sql_Predicate_Name_Data,
	sql_targets:                           [dynamic]Sql_Target_Data,
	create_data_type_handles:              [dynamic]Create_Data_Type_Handle_Site_Data,
}
