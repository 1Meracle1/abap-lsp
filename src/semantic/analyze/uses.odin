package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

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
	Provider_Entity,
	Builtin_Type,
	Builtin_Routine,
	Internal_Table_Line,
	External,
}

Resolution :: struct {
	kind:   Resolution_Kind,
	symbol: Symbol_Link,
	entity: Entity_Handle,
}

Reference_Data :: struct {
	id:                   Reference_Id,
	name:                 string,
	namespace:            Namespace,
	kind:                 Reference_Kind,
	scope:                Scope_Id,
	range:                tokenizer.Range,
	node:                 ^ast.Node,
	resolution:           Resolution,
	has_resolution:       bool,
	type_is_ref:          bool,
	type_has_path:        bool,
	type_first_selector:  ast.Selector_Op,
	type_clause_form:     ast.Data_Type_Form,
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

Include_Edge :: struct {
	name:       string,
	range:      tokenizer.Range,
	target:     Source_File_Id,
	has_target: bool,
	if_found:   bool,
}
