package abap_frontend_semantic_analyze

import "src:tokenizer"

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

diagnostic_is_warning :: proc(kind: Diagnostic_Kind) -> bool {
	#partial switch kind {
	case .Shadowed_Symbol,
	     .Unreachable_Code:
		return true
	}
	return false
}
