package abap_frontend_semantic_analyze

import "src:tokenizer"

import "core:mem"
import "core:strings"

unit_analysis_make :: proc(
	unit_id: Unit_Id,
	uri: string,
	range: tokenizer.Range,
	allocator: mem.Allocator,
) -> Unit_Analysis {
	unit := Unit_Analysis {
		unit_id    = unit_id,
		uri        = strings.clone(uri, allocator),
		root_scope = INVALID_SCOPE_ID,
	}
	unit.scopes = make([dynamic]Scope_Data, 0, 16, allocator)
	unit.symbols = make([dynamic]Symbol_Data, 0, 64, allocator)
	unit.decl_infos = make([dynamic]Decl_Info_Data, 0, 64, allocator)
	type_arena_init(&unit, allocator)
	unit.structures = make([dynamic]Structure_Data, 0, 8, allocator)
	unit.references = make([dynamic]Reference_Data, 0, 32, allocator)
	unit.message_uses = make([dynamic]Message_Use_Data, 0, 0, allocator)
	unit.message_class_entries = make([dynamic]Message_Class_Entry_Data, 0, 0, allocator)
	unit.diagnostics = make([dynamic]Diagnostic, 0, 8, allocator)
	unit.include_edges = make([dynamic]Include_Edge, 0, 4, allocator)
	unit.table_work_areas = make([dynamic]Table_Work_Area_Data, 0, 0, allocator)
	unit.selection_screen_report_type_positions = make([dynamic]tokenizer.Range, 0, 0, allocator)
	unit.field_accesses = make([dynamic]Field_Access, 0, 0, allocator)
	unit.loop_where_field_contexts = make([dynamic]Loop_Where_Field_Context, 0, 0, allocator)
	unit.loop_at_field_contexts = make([dynamic]Loop_At_Field_Context, 0, 0, allocator)
	unit.constructor_for_bindings = make([dynamic]Constructor_For_Binding_Data, 0, 0, allocator)
	unit.class_members = make([dynamic]Class_Member_Data, 0, 0, allocator)
	unit.class_definitions = make([dynamic]Class_Definition_Data, 0, 0, allocator)
	unit.class_inheritance = make([dynamic]Class_Inheritance_Data, 0, 0, allocator)
	unit.class_friends = make([dynamic]Class_Friend_Data, 0, 0, allocator)
	unit.implemented_interfaces = make([dynamic]Implemented_Interface_Data, 0, 0, allocator)
	unit.member_aliases = make([dynamic]Member_Alias_Data, 0, 0, allocator)
	unit.form_routines = make([dynamic]Form_Routine_Data, 0, 0, allocator)
	unit.function_modules = make([dynamic]Function_Module_Data, 0, 0, allocator)
	unit.named_arguments = make([dynamic]Named_Argument_Access, 0, 0, allocator)
	unit.call_sites = make([dynamic]Call_Site_Data, 0, 0, allocator)
	unit.assignment_sites = make([dynamic]Assignment_Site_Data, 0, 0, allocator)
	unit.concatenate_lines_of_sites = make(
		[dynamic]Concatenate_Lines_Of_Site_Data,
		0,
		0,
		allocator,
	)
	unit.expression_facts = make([dynamic]Expression_Fact_Data, 0, 0, allocator)
	unit.operands = make([dynamic]Operand_Data, 0, 0, allocator)
	unit.sql_queries = make([dynamic]Sql_Query_Data, 0, 0, allocator)
	unit.sql_sources = make([dynamic]Sql_Source_Data, 0, 0, allocator)
	unit.sql_dynamic_fragments = make([dynamic]Sql_Dynamic_Fragment_Data, 0, 0, allocator)
	unit.sql_projections = make([dynamic]Sql_Projection_Data, 0, 0, allocator)
	unit.sql_name_refs = make([dynamic]Sql_Name_Ref_Data, 0, 0, allocator)
	unit.sql_predicates = make([dynamic]Sql_Predicate_Data, 0, 0, allocator)
	unit.sql_targets = make([dynamic]Sql_Target_Data, 0, 0, allocator)
	unit.create_data_type_handles = make([dynamic]Create_Data_Type_Handle_Site_Data, 0, 0, allocator)
	unit.provided_names = make([dynamic]string, 0, 4, allocator)
	unit.scope_index = scope_index_make(allocator)
	unit.root_scope = add_scope(&unit, .File, range, allocator = allocator)
	install_builtins(&unit, unit.root_scope, allocator)
	return unit
}
