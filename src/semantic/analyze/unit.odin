package abap_frontend_semantic_analyze

import "src:tokenizer"

import "core:mem"
import "core:strings"

source_file_provider_make :: proc(
	source_file_id: Source_File_Id,
	role: Source_File_Role,
	uri: string,
	range: tokenizer.Range,
	allocator: mem.Allocator,
) -> Source_File_Provider {
	unit := Source_File_Provider {
		source_file_id    = source_file_id,
		role       = role,
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
	unit.table_exprs = make([dynamic]Table_Expr_Data, 0, 0, allocator)
	unit.loop_where_field_contexts = make([dynamic]Loop_Where_Field_Context, 0, 0, allocator)
	unit.loop_at_field_contexts = make([dynamic]Loop_At_Field_Context, 0, 0, allocator)
	unit.constructor_for_bindings = make([dynamic]Constructor_For_Binding_Data, 0, 0, allocator)
	unit.class_definitions = make([dynamic]Class_Definition_Data, 0, 0, allocator)
	unit.class_inheritance = make([dynamic]Class_Inheritance_Data, 0, 0, allocator)
	unit.class_friends = make([dynamic]Class_Friend_Data, 0, 0, allocator)
	unit.implemented_interfaces = make([dynamic]Implemented_Interface_Data, 0, 0, allocator)
	unit.member_aliases = make([dynamic]Member_Alias_Data, 0, 0, allocator)
	unit.named_arguments = make([dynamic]Named_Argument_Access, 0, 0, allocator)
	unit.call_sites = make([dynamic]Call_Site_Data, 0, 0, allocator)
	unit.call_function_exception_message_sites = make(
		[dynamic]Call_Function_Exception_Message_Site_Data,
		0,
		0,
		allocator,
	)
	unit.assignment_sites = make([dynamic]Assignment_Site_Data, 0, 0, allocator)
	unit.concatenate_lines_of_sites = make(
		[dynamic]Concatenate_Lines_Of_Site_Data,
		0,
		0,
		allocator,
	)
	unit.sql_queries = make([dynamic]Sql_Query_Data, 0, 0, allocator)
	unit.sql_sources = make([dynamic]Sql_Source_Data, 0, 0, allocator)
	unit.sql_dynamic_fragments = make([dynamic]Sql_Dynamic_Fragment_Data, 0, 0, allocator)
	unit.sql_projections = make([dynamic]Sql_Projection_Data, 0, 0, allocator)
	unit.sql_name_refs = make([dynamic]Sql_Name_Ref_Data, 0, 0, allocator)
	unit.sql_predicates = make([dynamic]Sql_Predicate_Data, 0, 0, allocator)
	unit.sql_predicate_names = make([dynamic]Sql_Predicate_Name_Data, 0, 0, allocator)
	unit.sql_targets = make([dynamic]Sql_Target_Data, 0, 0, allocator)
	unit.create_data_type_handles = make([dynamic]Create_Data_Type_Handle_Site_Data, 0, 0, allocator)
	unit.provided_names = make([dynamic]string, 0, 4, allocator)
	unit.scope_index = scope_index_make(allocator)
	unit.root_scope = add_scope(&unit, .File, range, allocator = allocator)
	return unit
}

source_file_fact_model_make :: proc(allocator: mem.Allocator) -> Source_File_Fact_Model {
	model: Source_File_Fact_Model
	model.message_uses = make([dynamic]Message_Use_Data, 0, 0, allocator)
	model.table_work_areas = make([dynamic]Table_Work_Area_Data, 0, 0, allocator)
	model.field_accesses = make([dynamic]Field_Access, 0, 0, allocator)
	model.table_exprs = make([dynamic]Table_Expr_Data, 0, 0, allocator)
	model.loop_where_field_contexts = make([dynamic]Loop_Where_Field_Context, 0, 0, allocator)
	model.loop_at_field_contexts = make([dynamic]Loop_At_Field_Context, 0, 0, allocator)
	model.constructor_for_bindings = make([dynamic]Constructor_For_Binding_Data, 0, 0, allocator)
	model.named_arguments = make([dynamic]Named_Argument_Access, 0, 0, allocator)
	model.call_sites = make([dynamic]Call_Site_Data, 0, 0, allocator)
	model.call_function_exception_message_sites = make(
		[dynamic]Call_Function_Exception_Message_Site_Data,
		0,
		0,
		allocator,
	)
	model.assignment_sites = make([dynamic]Assignment_Site_Data, 0, 0, allocator)
	model.concatenate_lines_of_sites = make(
		[dynamic]Concatenate_Lines_Of_Site_Data,
		0,
		0,
		allocator,
	)
	model.sql_queries = make([dynamic]Sql_Query_Data, 0, 0, allocator)
	model.sql_sources = make([dynamic]Sql_Source_Data, 0, 0, allocator)
	model.sql_dynamic_fragments = make([dynamic]Sql_Dynamic_Fragment_Data, 0, 0, allocator)
	model.sql_projections = make([dynamic]Sql_Projection_Data, 0, 0, allocator)
	model.sql_name_refs = make([dynamic]Sql_Name_Ref_Data, 0, 0, allocator)
	model.sql_predicates = make([dynamic]Sql_Predicate_Data, 0, 0, allocator)
	model.sql_predicate_names = make([dynamic]Sql_Predicate_Name_Data, 0, 0, allocator)
	model.sql_targets = make([dynamic]Sql_Target_Data, 0, 0, allocator)
	model.create_data_type_handles = make([dynamic]Create_Data_Type_Handle_Site_Data, 0, 0, allocator)
	return model
}

source_file_apply_fact_model :: proc(unit: ^Source_File_Provider, model: Source_File_Fact_Model) {
	if unit == nil {
		return
	}
	unit.message_default_class = model.message_default_class
	unit.has_message_default_class = model.has_message_default_class
	unit.message_uses = model.message_uses
	unit.table_work_areas = model.table_work_areas
	unit.field_accesses = model.field_accesses
	unit.table_exprs = model.table_exprs
	unit.loop_where_field_contexts = model.loop_where_field_contexts
	unit.loop_at_field_contexts = model.loop_at_field_contexts
	unit.constructor_for_bindings = model.constructor_for_bindings
	unit.named_arguments = model.named_arguments
	unit.call_sites = model.call_sites
	unit.call_function_exception_message_sites = model.call_function_exception_message_sites
	unit.assignment_sites = model.assignment_sites
	unit.concatenate_lines_of_sites = model.concatenate_lines_of_sites
	unit.sql_queries = model.sql_queries
	unit.sql_sources = model.sql_sources
	unit.sql_dynamic_fragments = model.sql_dynamic_fragments
	unit.sql_projections = model.sql_projections
	unit.sql_name_refs = model.sql_name_refs
	unit.sql_predicates = model.sql_predicates
	unit.sql_predicate_names = model.sql_predicate_names
	unit.sql_targets = model.sql_targets
	unit.create_data_type_handles = model.create_data_type_handles
}

source_file_refresh_fact_model :: proc(
	unit: ^Source_File_Provider,
	allocator: mem.Allocator,
) {
	if unit == nil {
		return
	}
	existing_include_edges := make([]Include_Edge, len(unit.include_edges), allocator)
	copy(existing_include_edges, unit.include_edges[:])
	clear(&unit.references)
	clear(&unit.include_edges)
	model := source_file_fact_model_build(unit, allocator)
	source_file_apply_fact_model(unit, model)
	source_file_restore_include_targets(unit, existing_include_edges)
	resolve_unit_with_index(unit, &unit.scope_index)
}

source_file_restore_include_targets :: proc(unit: ^Source_File_Provider, old_edges: []Include_Edge) {
	if unit == nil || len(old_edges) == 0 {
		return
	}
	for &edge in unit.include_edges {
		for old in old_edges {
			if old.name == edge.name && old.range == edge.range {
				edge.target = old.target
				edge.has_target = old.has_target
				break
			}
		}
	}
}
