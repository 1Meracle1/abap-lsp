package abap_frontend_semantic_analyze

import "src:tokenizer"

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

Sql_Predicate_Name_Data :: struct {
	query_id: int,
	scope:    Scope_Id,
	range:    tokenizer.Range,
	name:     string,
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
