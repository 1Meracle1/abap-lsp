package abap_frontend_lsp

import "src:ast"
import "src:semantic"
import "src:utils"

import json "core:encoding/json"
import "core:fmt"
import "core:mem"
import "core:strings"

handle_completion :: proc(ctx: ^Request_Context, params: json.Value) {
	snapshot, offset, ok := snapshot_for_position(ctx.state, params)
	if !ok {
		send_success(ctx.output, ctx.id, Completion_List{}, ctx.state.allocator)
		return
	}
	out := completion_items_for_snapshot(
		snapshot,
		offset,
		ctx.state.completion_snippets_supported,
		context.temp_allocator,
	)
	send_success(
		ctx.output,
		ctx.id,
		Completion_List{is_incomplete = false, items = out},
		context.temp_allocator,
	)
}

completion_items_for_snapshot :: proc(
	snapshot: Snapshot_Lookup,
	offset: int,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> []Completion_Item {
	prefix := completion_prefix(snapshot.source, offset, context.temp_allocator)
	template_prefix := completion_template_prefix(snapshot.source, offset, context.temp_allocator)
	query := semantic.semantic_query(
		snapshot.project,
		snapshot.checker,
		snapshot.file,
		snapshot.provider_index,
	)
	items := semantic.semantic_completion_items_at_offset(
		semantic.semantic_query_completion(query),
		offset,
		prefix,
		context.temp_allocator,
		snapshot.source,
	)
	scope := semantic.semantic_query_scope_at_offset(snapshot.file, offset, snapshot.checker)
	method_body_call_completion := completion_in_method_implementation_body(
		scope,
		snapshot.file,
		offset,
	)
	indent := completion_line_indent(snapshot.source, offset, context.temp_allocator)
	template_indent := completion_template_base_indent(indent, snippets_supported)
	member_prefix_start := completion_prefix_start(snapshot.source, offset)
	selector_filter_prefix_start := completion_selector_filter_prefix_start(snapshot.source, offset)
	selector_filter_prefix := completion_selector_filter_prefix(
		snapshot.source,
		selector_filter_prefix_start,
		member_prefix_start,
		context.temp_allocator,
	)
	selector_replace_range := range_from_offsets(
		snapshot.source,
		selector_filter_prefix_start,
		offset,
	)
	template_replace_range := completion_template_replace_range(snapshot.source, offset)
	if_template_count := completion_if_template_count(snapshot.source, offset, template_prefix)
	case_template_count := completion_case_template_count(snapshot.source, offset, template_prefix)
	class_template_count := completion_class_template_count(
		snapshot.source,
		offset,
		template_prefix,
	)
	interface_template_count := completion_interface_template_count(
		snapshot.source,
		offset,
		template_prefix,
	)
	try_template_count := completion_try_template_count(snapshot.source, offset, template_prefix)
	loop_template_count := completion_loop_template_count(snapshot.source, offset, template_prefix)
	select_template_count := completion_select_template_count(
		snapshot.source,
		offset,
		template_prefix,
	)
	commit_template_count := completion_commit_template_count(
		snapshot.source,
		offset,
		template_prefix,
	)
	continue_template_count := completion_continue_template_count(
		snapshot.source,
		offset,
		template_prefix,
	)
	read_table_template_count := completion_read_table_template_count(
		snapshot.source,
		offset,
		template_prefix,
	)
	get_time_stamp_template_count := completion_get_time_stamp_field_template_count(
		snapshot.source,
		offset,
		template_prefix,
	)
	expression_template_count := completion_expression_template_count(
		snapshot.source,
		offset,
		template_prefix,
	)
	common_statement_template_count := completion_common_statement_template_count(
		snapshot.source,
		offset,
		template_prefix,
	)
	template_count :=
		if_template_count +
		case_template_count +
		class_template_count +
		interface_template_count +
		loop_template_count +
		select_template_count +
		get_time_stamp_template_count +
		try_template_count +
		commit_template_count +
		continue_template_count +
		read_table_template_count +
		expression_template_count +
		common_statement_template_count
	out := make([]Completion_Item, len(items) + template_count, allocator)
	for item, i in items {
		out[i] = completion_item_from_semantic_item(
			snapshot.project,
			item,
			selector_replace_range,
			selector_filter_prefix,
			indent,
			snippets_supported,
			method_body_call_completion,
			allocator,
		)
	}
	template_index := len(items)
	if if_template_count > 0 {
		completion_append_if_templates(
			out[template_index:template_index + if_template_count],
			template_indent,
			snippets_supported,
			allocator,
		)
		template_index += if_template_count
	}
	if case_template_count > 0 {
		out[template_index] = completion_case_template_item(
			template_indent,
			template_replace_range,
			snippets_supported,
			allocator,
		)
		template_index += case_template_count
	}
	if class_template_count > 0 {
		completion_append_class_templates(
			out[template_index:template_index + class_template_count],
			template_indent,
			snippets_supported,
			allocator,
		)
		template_index += class_template_count
	}
	if interface_template_count > 0 {
		out[template_index] = completion_interface_template_item(
			template_indent,
			snippets_supported,
			allocator,
		)
		template_index += interface_template_count
	}
	if try_template_count > 0 {
		out[template_index] = completion_try_template_item(
			template_indent,
			snippets_supported,
			allocator,
		)
		template_index += try_template_count
	}
	if loop_template_count > 0 {
		completion_append_loop_templates(
			out[template_index:template_index + loop_template_count],
			template_indent,
			snippets_supported,
			allocator,
		)
		template_index += loop_template_count
	}
	if select_template_count > 0 {
		completion_append_select_templates(
			out[template_index:template_index + select_template_count],
			template_indent,
			snippets_supported,
			allocator,
		)
		template_index += select_template_count
	}
	if commit_template_count > 0 {
		completion_append_commit_templates(
			out[template_index:template_index + commit_template_count],
			snippets_supported,
			allocator,
		)
		template_index += commit_template_count
	}
	if continue_template_count > 0 {
		out[template_index] = completion_continue_template_item(snippets_supported, allocator)
		template_index += continue_template_count
	}
	if read_table_template_count > 0 {
		completion_append_read_table_templates(
			out[template_index:template_index + read_table_template_count],
			snippets_supported,
			allocator,
		)
		template_index += read_table_template_count
	}
	if get_time_stamp_template_count > 0 {
		out[template_index] = completion_get_time_stamp_field_template_item(
			snippets_supported,
			allocator,
		)
		template_index += get_time_stamp_template_count
	}
	if expression_template_count > 0 {
		completion_append_expression_templates(
			out[template_index:template_index + expression_template_count],
			template_prefix,
			template_replace_range,
			snippets_supported,
			allocator,
		)
		template_index += expression_template_count
	}
	if common_statement_template_count > 0 {
		completion_append_common_statement_templates(
			out[template_index:template_index + common_statement_template_count],
			snapshot.source,
			offset,
			template_prefix,
			template_replace_range,
			template_indent,
			snippets_supported,
			allocator,
		)
		template_index += common_statement_template_count
	}
	assert(template_index == len(out))
	return out
}

completion_item_from_semantic_item :: proc(
	project: ^semantic.Project,
	item: semantic.Semantic_Completion_Item,
	selector_replace_range: Range,
	selector_filter_prefix: string,
	indent: string,
	snippets_supported: bool,
	method_body_call_completion: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	out := Completion_Item {
		label              = item.name,
		kind               = completion_kind(item.entity),
		sort_text          = completion_sort_text("1", item.name, allocator),
		insert_text        = item.name,
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
	if snippets_supported &&
	   completion_semantic_item_uses_method_call_snippet(item, method_body_call_completion) {
		out.insert_text = completion_method_call_snippet(
			project,
			item.entity,
			item.name,
			"",
			allocator,
		)
		out.insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET
	}
	if item.source == .Selector_Member {
		new_text := out.insert_text
		insert_prefix := completion_selector_insert_prefix(selector_filter_prefix, item, allocator)
		if insert_prefix != "" {
			out.filter_text = strings.concatenate({insert_prefix, item.name}, allocator)
			new_text = strings.concatenate({insert_prefix, out.insert_text}, allocator)
		}
		out.text_edit = Text_Edit{range = selector_replace_range, new_text = new_text}
	}
	return out
}

completion_selector_insert_prefix :: proc(
	prefix: string,
	item: semantic.Semantic_Completion_Item,
	allocator: mem.Allocator,
) -> string {
	if prefix == "" {
		return ""
	}
	if item.source == .Selector_Member &&
	   item.selector_op == .Arrow &&
	   strings.has_suffix(prefix, "-") &&
	   !strings.has_suffix(prefix, "->") {
		return strings.concatenate({prefix, ">"}, allocator)
	}
	return prefix
}

completion_semantic_item_uses_method_call_snippet :: proc(
	item: semantic.Semantic_Completion_Item,
	method_body_call_completion: bool,
) -> bool {
	if item.entity == nil || item.entity.kind != .Method {
		return false
	}
	if item.source == .Selector_Member {
		return true
	}
	if !method_body_call_completion || item.source != .Lexical_Scope {
		return false
	}
	owner := item.entity.owner
	return owner != nil && (owner.kind == .Class || owner.kind == .Interface)
}

completion_in_method_implementation_body :: proc(
	scope: ^semantic.Scope,
	file: ^semantic.Project_File,
	offset: int,
) -> bool {
	if scope == nil || file == nil {
		return false
	}
	for current := scope; current != nil; current = current.parent {
		if current.kind != .Method || current.owner == nil || current.owner.kind != .Method {
			continue
		}
		payload, ok := current.owner.payload.(^semantic.Entity_Routine_Payload)
		if !ok ||
		   payload == nil ||
		   payload.implementation_unit != file ||
		   !completion_range_contains_offset(payload.implementation_range, offset) {
			continue
		}
		if current.owner.decl_info == nil || current.owner.decl_info.decl_node == nil {
			return false
		}
		method, method_ok := current.owner.decl_info.decl_node.derived.(^ast.Method_Decl)
		return method_ok && offset >= method.header_range.end
	}
	return false
}

completion_range_contains_offset :: proc "contextless" (range: semantic.Range, offset: int) -> bool {
	return range.start <= offset && offset < range.end
}

completion_sort_text :: proc(priority, label: string, allocator: mem.Allocator) -> string {
	lower := utils.to_lower_ascii(label, context.temp_allocator)
	return strings.concatenate({priority, ":", lower}, allocator)
}

IF_TEMPLATE_COUNT :: 5

Completion_If_Template :: enum {
	Generic,
	Sy_Subrc_Zero,
	Sy_Subrc_Not_Zero,
	Is_Initial,
	Is_Not_Initial,
}

completion_if_template_count :: proc(source: string, offset: int, prefix: string) -> int {
	if !completion_keyword_prefix_matches(prefix, "IF") ||
	   !completion_template_at_statement_start(source, offset) {
		return 0
	}
	return IF_TEMPLATE_COUNT
}

completion_case_template_count :: proc(source: string, offset: int, prefix: string) -> int {
	if !completion_keyword_prefix_matches(prefix, "CASE") ||
	   !completion_template_at_statement_start(source, offset) {
		return 0
	}
	return 1
}

CLASS_TEMPLATE_COUNT :: 6

Completion_Class_Template :: enum {
	Basic,
	Public_Final_Create_Public,
	Inheriting_From,
	Final_Create_Public,
	Abstract,
	For_Testing,
}

completion_class_template_count :: proc(source: string, offset: int, prefix: string) -> int {
	if !completion_keyword_prefix_matches(prefix, "CLASS") ||
	   !completion_template_at_statement_start(source, offset) {
		return 0
	}
	return CLASS_TEMPLATE_COUNT
}

completion_interface_template_count :: proc(source: string, offset: int, prefix: string) -> int {
	if !completion_keyword_prefix_matches(prefix, "INTERFACE") ||
	   !completion_template_at_statement_start(source, offset) {
		return 0
	}
	return 1
}

completion_try_template_count :: proc(source: string, offset: int, prefix: string) -> int {
	if !completion_keyword_prefix_matches(prefix, "TRY") ||
	   !completion_template_at_statement_start(source, offset) {
		return 0
	}
	return 1
}

completion_loop_template_count :: proc(source: string, offset: int, prefix: string) -> int {
	if !completion_keyword_prefix_matches(prefix, "LOOP") ||
	   !completion_template_at_statement_start(source, offset) {
		return 0
	}
	return 2
}

SELECT_TEMPLATE_COUNT :: 7

Completion_Select_Template :: enum {
	Basic_Where,
	Single,
	Limit_Offset,
	For_All_Entries,
	Join,
	Package_Size,
	Cursor,
}

completion_select_template_count :: proc(source: string, offset: int, prefix: string) -> int {
	if !completion_keyword_prefix_matches(prefix, "SELECT") ||
	   !completion_template_at_statement_start(source, offset) {
		return 0
	}
	return SELECT_TEMPLATE_COUNT
}

COMMIT_TEMPLATE_COUNT :: 2

Completion_Commit_Template :: enum {
	Work,
	Work_And_Wait,
}

completion_commit_template_count :: proc(source: string, offset: int, prefix: string) -> int {
	if !completion_keyword_prefix_matches(prefix, "COMMIT") ||
	   !completion_template_at_statement_start(source, offset) {
		return 0
	}
	return COMMIT_TEMPLATE_COUNT
}

completion_continue_template_count :: proc(source: string, offset: int, prefix: string) -> int {
	if !completion_keyword_prefix_matches(prefix, "CONTINUE") ||
	   !completion_template_at_statement_start(source, offset) {
		return 0
	}
	return 1
}

READ_TABLE_TEMPLATE_COUNT :: 11

Completion_Read_Table_Template :: enum {
	Index_Into,
	Index_Assigning,
	Index_Using_Key_Into,
	Key_Into,
	Key_Assigning,
	Key_Reference_Into,
	Key_Transporting_No_Fields,
	Key_Binary_Search_Into,
	Table_Key_Components_Into,
	Table_Key_Components_Assigning,
	Table_Key_Components_Transporting_No_Fields,
}

completion_read_table_template_count :: proc(source: string, offset: int, prefix: string) -> int {
	if !completion_keyword_prefix_matches(prefix, "READ") ||
	   !completion_template_at_statement_start(source, offset) {
		return 0
	}
	return READ_TABLE_TEMPLATE_COUNT
}

completion_get_time_stamp_field_template_count :: proc(
	source: string,
	offset: int,
	prefix: string,
) -> int {
	if !completion_keyword_prefix_matches(prefix, "GET") ||
	   !completion_template_at_statement_start(source, offset) {
		return 0
	}
	return 1
}

Completion_Statement_Template :: struct {
	keyword:                          string,
	label:                            string,
	snippet:                          string,
	plain:                            string,
	types_chain_clause:               bool,
	type_addition_clause:             bool,
	type_addition_complex_definition: bool,
}

EXPRESSION_TEMPLATES :: [?]Completion_Statement_Template {
	{
		keyword = "NEW",
		label = "NEW #( ... )",
		snippet = "NEW #( ${1} )$0",
		plain = "NEW #( )",
	},
	{
		keyword = "NEW",
		label = "NEW ...",
		snippet = "NEW ${1:lcl_class}( ${2} )$0",
		plain = "NEW lcl_class( )",
	},
	{
		keyword = "COND",
		label = "COND #( WHEN ... THEN ... ELSE ... )",
		snippet = "COND #( WHEN ${1:condition} THEN ${2:value} ELSE ${3:default} )$0",
		plain = "COND #( WHEN condition THEN value ELSE default )",
	},
	{
		keyword = "COND",
		label = "COND ...",
		snippet = "COND ${1:string}( WHEN ${2:condition} THEN ${3:value} ELSE ${4:default} )$0",
		plain = "COND string( WHEN condition THEN value ELSE default )",
	},
	{
		keyword = "COND",
		label = "COND ... LET ... IN",
		snippet = "COND ${1:string}( LET ${2:lv_value} = ${3:value} IN WHEN ${4:condition} THEN ${2:lv_value} ELSE ${5:default} )$0",
		plain = "COND string( LET lv_value = value IN WHEN condition THEN lv_value ELSE default )",
	},
	{
		keyword = "CONDENSE",
		label = "condense( val = ... )",
		snippet = "condense( val = ${1:lv_text} )$0",
		plain = "condense( val = lv_text )",
	},
	{
		keyword = "CONDENSE",
		label = "condense( val = ... del = ... )",
		snippet = "condense( val = ${1:lv_text} del = ${2:space} )$0",
		plain = "condense( val = lv_text del = space )",
	},
	{
		keyword = "CONDENSE",
		label = "condense( val = ... from = ... to = ... )",
		snippet = "condense( val = ${1:lv_text} from = ${2:'_'} to = ${3:space} )$0",
		plain = "condense( val = lv_text from = '_' to = space )",
	},
	{
		keyword = "FIND",
		label = "find( val = ... sub = ... )",
		snippet = "find( val = ${1:lv_text} sub = ${2:'text'} )$0",
		plain = "find( val = lv_text sub = 'text' )",
	},
	{
		keyword = "FIND",
		label = "find( val = ... regex = ... )",
		snippet = "find( val = ${1:lv_text} regex = ${2:'pattern'} )$0",
		plain = "find( val = lv_text regex = 'pattern' )",
	},
	{
		keyword = "FIND",
		label = "find( val = ... sub = ... occ = ... )",
		snippet = "find( val = ${1:lv_text} sub = ${2:'text'} occ = ${3:1} )$0",
		plain = "find( val = lv_text sub = 'text' occ = 1 )",
	},
	{
		keyword = "FILTER",
		label = "FILTER #( ... WHERE ... )",
		snippet = "FILTER #( ${1:itab} WHERE ${2:field} = ${3:lv_value} )$0",
		plain = "FILTER #( itab WHERE field = lv_value )",
	},
	{
		keyword = "FILTER",
		label = "FILTER #( ... USING KEY ... WHERE ... )",
		snippet = "FILTER #( ${1:itab} USING KEY ${2:key_name} WHERE ${3:field} = ${4:lv_value} )$0",
		plain = "FILTER #( itab USING KEY key_name WHERE field = lv_value )",
	},
	{
		keyword = "FILTER",
		label = "FILTER #( ... EXCEPT WHERE ... )",
		snippet = "FILTER #( ${1:itab} EXCEPT WHERE ${2:field} = ${3:lv_value} )$0",
		plain = "FILTER #( itab EXCEPT WHERE field = lv_value )",
	},
	{
		keyword = "REDUCE",
		label = "REDUCE ... FOR ... IN",
		snippet = "REDUCE ${1:i}( INIT ${2:result} = ${3:0} FOR ${4:row} IN ${5:itab} NEXT ${2:result} = ${2:result} + ${4:row}-${6:amount} )$0",
		plain = "REDUCE i( INIT result = 0 FOR row IN itab NEXT result = result + row-amount )",
	},
	{
		keyword = "REDUCE",
		label = "REDUCE ... FOR ... IN ... WHERE",
		snippet = "REDUCE ${1:i}( INIT ${2:result} = ${3:0} FOR ${4:row} IN ${5:itab} WHERE ( ${6:field} = ${7:lv_value} ) NEXT ${2:result} = ${2:result} + ${4:row}-${8:amount} )$0",
		plain = "REDUCE i( INIT result = 0 FOR row IN itab WHERE ( field = lv_value ) NEXT result = result + row-amount )",
	},
	{
		keyword = "REDUCE",
		label = "REDUCE ... FOR ... THEN ... UNTIL",
		snippet = "REDUCE ${1:i}( INIT ${2:result} = ${3:0} FOR ${4:index} = ${5:1} THEN ${4:index} + ${6:1} UNTIL ${4:index} > ${7:limit} NEXT ${2:result} = ${2:result} + ${4:index} )$0",
		plain = "REDUCE i( INIT result = 0 FOR index = 1 THEN index + 1 UNTIL index > limit NEXT result = result + index )",
	},
	{
		keyword = "REDUCE",
		label = "REDUCE ... FOR ... THEN ... WHILE",
		snippet = "REDUCE ${1:i}( INIT ${2:result} = ${3:0} FOR ${4:index} = ${5:1} THEN ${4:index} + ${6:1} WHILE ${4:index} <= ${7:limit} NEXT ${2:result} = ${2:result} + ${4:index} )$0",
		plain = "REDUCE i( INIT result = 0 FOR index = 1 THEN index + 1 WHILE index <= limit NEXT result = result + index )",
	},
	{
		keyword = "FOR",
		label = "FOR ... IN",
		snippet = "FOR ${1:row} IN ${2:itab} ( ${1:row} )$0",
		plain = "FOR row IN itab ( row )",
	},
	{
		keyword = "FOR",
		label = "FOR ... IN ... WHERE",
		snippet = "FOR ${1:row} IN ${2:itab} WHERE ( ${3:field} = ${4:lv_value} ) ( ${1:row} )$0",
		plain = "FOR row IN itab WHERE ( field = lv_value ) ( row )",
	},
	{
		keyword = "FOR",
		label = "FOR GROUPS ... GROUP BY",
		snippet = "FOR GROUPS ${1:group} OF ${2:row} IN ${3:itab} GROUP BY ${2:row}-${4:field} ( ${1:group} )$0",
		plain = "FOR GROUPS group OF row IN itab GROUP BY row-field ( group )",
	},
	{
		keyword = "FOR",
		label = "FOR GROUPS ... GROUP BY ( ... )",
		snippet = "FOR GROUPS ${1:group} OF ${2:row} IN ${3:itab} GROUP BY ( ${4:key} = ${2:row}-${5:field} ) ( ${1:group}-${4:key} )$0",
		plain = "FOR GROUPS group OF row IN itab GROUP BY ( key = row-field ) ( group-key )",
	},
	{
		keyword = "FOR",
		label = "FOR ... THEN ... UNTIL",
		snippet = "FOR ${1:index} = ${2:1} THEN ${1} + ${3:1} UNTIL ${1:index} > ${4:limit} ( ${1:index} )$0",
		plain = "FOR index = 1 THEN index + 1 UNTIL index > limit ( index )",
	},
	{
		keyword = "FOR",
		label = "FOR ... THEN ... WHILE",
		snippet = "FOR ${1:index} = ${2:1} THEN ${1:index} + ${3:1} WHILE ${1:index} <= ${4:limit} ( ${1:index} )$0",
		plain = "FOR index = 1 THEN index + 1 WHILE index <= limit ( index )",
	},
}

COMMON_STATEMENT_TEMPLATES :: [?]Completion_Statement_Template {
	{
		keyword = "TYPES",
		label = "TYPES: BEGIN OF ... END OF",
		snippet = "TYPES:\n  BEGIN OF ${1:ty_line},\n    ${2:field} TYPE ${3:string},\n  END OF ${1:ty_line}.$0",
		plain = "TYPES:\n  BEGIN OF ty_line,\n    field TYPE string,\n  END OF ty_line.",
	},
	{
		keyword = "TYPES",
		label = "TYPES ... TYPE",
		snippet = "TYPES ${1:ty_value} TYPE ${2:string}.$0",
		plain = "TYPES ty_value TYPE string.",
	},
	{
		keyword = "TYPES",
		label = "TYPES ... LIKE",
		snippet = "TYPES ${1:ty_value} LIKE ${2:sy-datum}.$0",
		plain = "TYPES ty_value LIKE sy-datum.",
	},
	{
		keyword = "TYPES",
		label = "TYPES ... TYPE c LENGTH",
		snippet = "TYPES ${1:ty_text} TYPE c LENGTH ${2:10}.$0",
		plain = "TYPES ty_text TYPE c LENGTH 10.",
	},
	{
		keyword = "TYPES",
		label = "TYPES ... TYPE p LENGTH DECIMALS",
		snippet = "TYPES ${1:ty_amount} TYPE p LENGTH ${2:8} DECIMALS ${3:2}.$0",
		plain = "TYPES ty_amount TYPE p LENGTH 8 DECIMALS 2.",
	},
	{
		keyword = "TYPES",
		label = "TYPES ... TYPE REF TO",
		snippet = "TYPES ${1:ty_ref} TYPE REF TO ${2:object}.$0",
		plain = "TYPES ty_ref TYPE REF TO object.",
	},
	{
		keyword = "TYPES",
		label = "TYPES ... TYPE STANDARD TABLE OF",
		snippet = "TYPES ${1:ty_table} TYPE STANDARD TABLE OF ${2:string} WITH EMPTY KEY.$0",
		plain = "TYPES ty_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.",
	},
	{
		keyword = "TYPES",
		label = "TYPES ... TYPE SORTED TABLE OF",
		snippet = "TYPES ${1:ty_table} TYPE SORTED TABLE OF ${2:string} WITH UNIQUE KEY ${3:table_line}.$0",
		plain = "TYPES ty_table TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.",
	},
	{
		keyword = "TYPES",
		label = "TYPES ... TYPE HASHED TABLE OF",
		snippet = "TYPES ${1:ty_table} TYPE HASHED TABLE OF ${2:string} WITH UNIQUE KEY ${3:table_line}.$0",
		plain = "TYPES ty_table TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.",
	},
	{
		keyword = "TYPES",
		label = "TYPES ... TYPE RANGE OF",
		snippet = "TYPES ${1:ty_range} TYPE RANGE OF ${2:sy-datum}.$0",
		plain = "TYPES ty_range TYPE RANGE OF sy-datum.",
	},
	{
		keyword = "TYPE-POOLS",
		label = "TYPE-POOLS ...",
		snippet = "TYPE-POOLS ${1:abap}.$0",
		plain = "TYPE-POOLS abap.",
	},
	{
		keyword = "TYPE-POOLS",
		label = "TYPE-POOLS: ...",
		snippet = "TYPE-POOLS:\n  ${1:abap},\n  ${2:icon}.$0",
		plain = "TYPE-POOLS:\n  abap,\n  icon.",
	},
	{
		keyword = "BEGIN",
		label = "BEGIN OF ... END OF",
		snippet = "BEGIN OF ${1:ty_line},\n  ${2:field} TYPE ${3:string},\nEND OF ${1:ty_line}$0",
		plain = "BEGIN OF ty_line,\n  field TYPE string,\nEND OF ty_line",
		types_chain_clause = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE ...",
		snippet = "TYPE ${1:string}$0",
		plain = "TYPE string",
		type_addition_clause = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE c LENGTH",
		snippet = "TYPE c LENGTH ${1:10}$0",
		plain = "TYPE c LENGTH 10",
		type_addition_clause = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE p LENGTH DECIMALS",
		snippet = "TYPE p LENGTH ${1:8} DECIMALS ${2:2}$0",
		plain = "TYPE p LENGTH 8 DECIMALS 2",
		type_addition_clause = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE REF TO ...",
		snippet = "TYPE REF TO ${1:object}$0",
		plain = "TYPE REF TO object",
		type_addition_clause = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE LINE OF ...",
		snippet = "TYPE LINE OF ${1:itab}$0",
		plain = "TYPE LINE OF itab",
		type_addition_clause = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE TABLE OF ...",
		snippet = "TYPE TABLE OF ${1:string}$0",
		plain = "TYPE TABLE OF string",
		type_addition_clause = true,
		type_addition_complex_definition = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE ANY TABLE",
		snippet = "TYPE ANY TABLE$0",
		plain = "TYPE ANY TABLE",
		type_addition_clause = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE INDEX TABLE",
		snippet = "TYPE INDEX TABLE$0",
		plain = "TYPE INDEX TABLE",
		type_addition_clause = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE STANDARD TABLE",
		snippet = "TYPE STANDARD TABLE$0",
		plain = "TYPE STANDARD TABLE",
		type_addition_clause = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE STANDARD TABLE OF ... WITH EMPTY KEY",
		snippet = "TYPE STANDARD TABLE OF ${1:string} WITH EMPTY KEY$0",
		plain = "TYPE STANDARD TABLE OF string WITH EMPTY KEY",
		type_addition_clause = true,
		type_addition_complex_definition = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE STANDARD TABLE OF ... WITH DEFAULT KEY",
		snippet = "TYPE STANDARD TABLE OF ${1:string} WITH DEFAULT KEY$0",
		plain = "TYPE STANDARD TABLE OF string WITH DEFAULT KEY",
		type_addition_clause = true,
		type_addition_complex_definition = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE SORTED TABLE OF ... WITH UNIQUE KEY",
		snippet = "TYPE SORTED TABLE OF ${1:string} WITH UNIQUE KEY ${2:table_line}$0",
		plain = "TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line",
		type_addition_clause = true,
		type_addition_complex_definition = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE SORTED TABLE OF ... WITH NON-UNIQUE KEY",
		snippet = "TYPE SORTED TABLE OF ${1:string} WITH NON-UNIQUE KEY ${2:table_line}$0",
		plain = "TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line",
		type_addition_clause = true,
		type_addition_complex_definition = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE HASHED TABLE OF ... WITH UNIQUE KEY",
		snippet = "TYPE HASHED TABLE OF ${1:string} WITH UNIQUE KEY ${2:table_line}$0",
		plain = "TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line",
		type_addition_clause = true,
		type_addition_complex_definition = true,
	},
	{
		keyword = "TYPE",
		label = "TYPE RANGE OF ...",
		snippet = "TYPE RANGE OF ${1:sy-datum}$0",
		plain = "TYPE RANGE OF sy-datum",
		type_addition_clause = true,
		type_addition_complex_definition = true,
	},
	{
		keyword = "DATA",
		label = "DATA: BEGIN OF ... END OF",
		snippet = "DATA:\n  BEGIN OF ${1:ls_row},\n    ${2:field} TYPE ${3:string},\n  END OF ${1:ls_row}.$0",
		plain = "DATA:\n  BEGIN OF ls_row,\n    field TYPE string,\n  END OF ls_row.",
	},
	{
		keyword = "DATA",
		label = "DATA: BEGIN OF COMMON PART ... END OF COMMON PART",
		snippet = "DATA:\n  BEGIN OF COMMON PART ${1:common_part}.\nDATA:\n  END OF COMMON PART.$0",
		plain = "DATA:\n  BEGIN OF COMMON PART common_part.\nDATA:\n  END OF COMMON PART.",
	},
	{
		keyword = "CONSTANTS",
		label = "CONSTANTS: BEGIN OF ... END OF",
		snippet = "CONSTANTS:\n  BEGIN OF ${1:c_values},\n    ${2:name} TYPE ${3:string} VALUE ${4:''},\n  END OF ${1:c_values}.$0",
		plain = "CONSTANTS:\n  BEGIN OF c_values,\n    name TYPE string VALUE '',\n  END OF c_values.",
	},
	{
		keyword = "STATICS",
		label = "STATICS: BEGIN OF ... END OF",
		snippet = "STATICS:\n  BEGIN OF ${1:s_state},\n    ${2:field} TYPE ${3:string},\n  END OF ${1:s_state}.$0",
		plain = "STATICS:\n  BEGIN OF s_state,\n    field TYPE string,\n  END OF s_state.",
	},
	{
		keyword = "CLASS-DATA",
		label = "CLASS-DATA: BEGIN OF ... END OF",
		snippet = "CLASS-DATA:\n  BEGIN OF ${1:gs_row},\n    ${2:field} TYPE ${3:string},\n  END OF ${1:gs_row}.$0",
		plain = "CLASS-DATA:\n  BEGIN OF gs_row,\n    field TYPE string,\n  END OF gs_row.",
	},
	{
		keyword = "INTERFACES",
		label = "INTERFACES ...",
		snippet = "INTERFACES ${1:lif_interface}.$0",
		plain = "INTERFACES lif_interface.",
	},
	{
		keyword = "ALIASES",
		label = "ALIASES ... FOR ...",
		snippet = "ALIASES ${1:alias_name} FOR ${2:lif_interface}~${3:member_name}.$0",
		plain = "ALIASES alias_name FOR lif_interface~member_name.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ...",
		snippet = "METHODS ${1:method_name}.$0",
		plain = "METHODS method_name.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... IMPORTING",
		snippet = "METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}.$0",
		plain = "METHODS method_name\n  IMPORTING\n    !iv_value TYPE string.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... EXPORTING",
		snippet = "METHODS ${1:method_name}\n  EXPORTING\n    !${2:ev_value} TYPE ${3:string}.$0",
		plain = "METHODS method_name\n  EXPORTING\n    !ev_value TYPE string.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... CHANGING",
		snippet = "METHODS ${1:method_name}\n  CHANGING\n    !${2:cv_value} TYPE ${3:string}.$0",
		plain = "METHODS method_name\n  CHANGING\n    !cv_value TYPE string.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... RECEIVING",
		snippet = "METHODS ${1:method_name}\n  RECEIVING\n    VALUE(${2:rv_result}) TYPE ${3:string}.$0",
		plain = "METHODS method_name\n  RECEIVING\n    VALUE(rv_result) TYPE string.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... RETURNING",
		snippet = "METHODS ${1:method_name}\n  RETURNING\n    VALUE(${2:rv_result}) TYPE ${3:string}.$0",
		plain = "METHODS method_name\n  RETURNING\n    VALUE(rv_result) TYPE string.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... IMPORTING RETURNING",
		snippet = "METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  RETURNING\n    VALUE(${4:rv_result}) TYPE ${5:string}.$0",
		plain = "METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  RETURNING\n    VALUE(rv_result) TYPE string.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... IMPORTING EXPORTING",
		snippet = "METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  EXPORTING\n    !${4:ev_value} TYPE ${5:string}.$0",
		plain = "METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  EXPORTING\n    !ev_value TYPE string.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... IMPORTING CHANGING",
		snippet = "METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  CHANGING\n    !${4:cv_value} TYPE ${5:string}.$0",
		plain = "METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  CHANGING\n    !cv_value TYPE string.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... IMPORTING EXPORTING CHANGING",
		snippet = "METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  EXPORTING\n    !${4:ev_value} TYPE ${5:string}\n  CHANGING\n    !${6:cv_value} TYPE ${7:string}.$0",
		plain = "METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  EXPORTING\n    !ev_value TYPE string\n  CHANGING\n    !cv_value TYPE string.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... RAISING",
		snippet = "METHODS ${1:method_name}\n  RAISING\n    ${2:cx_static_check}.$0",
		plain = "METHODS method_name\n  RAISING\n    cx_static_check.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... IMPORTING RAISING",
		snippet = "METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  RAISING\n    ${4:cx_static_check}.$0",
		plain = "METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  RAISING\n    cx_static_check.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... IMPORTING RETURNING RAISING",
		snippet = "METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  RETURNING\n    VALUE(${4:rv_result}) TYPE ${5:string}\n  RAISING\n    ${6:cx_static_check}.$0",
		plain = "METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  RETURNING\n    VALUE(rv_result) TYPE string\n  RAISING\n    cx_static_check.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... EXCEPTIONS",
		snippet = "METHODS ${1:method_name}\n  EXCEPTIONS\n    ${2:failed} = ${3:1}.$0",
		plain = "METHODS method_name\n  EXCEPTIONS\n    failed = 1.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... FOR EVENT",
		snippet = "METHODS ${1:on_event}\n  FOR EVENT ${2:event_name} OF ${3:lcl_source}\n  IMPORTING\n    !${4:sender}.$0",
		plain = "METHODS on_event\n  FOR EVENT event_name OF lcl_source\n  IMPORTING\n    !sender.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... FOR TESTING",
		snippet = "METHODS ${1:test_method} FOR TESTING.$0",
		plain = "METHODS test_method FOR TESTING.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... REDEFINITION",
		snippet = "METHODS ${1:method_name} REDEFINITION.$0",
		plain = "METHODS method_name REDEFINITION.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... ABSTRACT",
		snippet = "METHODS ${1:method_name} ABSTRACT.$0",
		plain = "METHODS method_name ABSTRACT.",
	},
	{
		keyword = "METHODS",
		label = "METHODS ... FINAL",
		snippet = "METHODS ${1:method_name} FINAL.$0",
		plain = "METHODS method_name FINAL.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ...",
		snippet = "CLASS-METHODS ${1:method_name}.$0",
		plain = "CLASS-METHODS method_name.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... IMPORTING",
		snippet = "CLASS-METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}.$0",
		plain = "CLASS-METHODS method_name\n  IMPORTING\n    !iv_value TYPE string.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... EXPORTING",
		snippet = "CLASS-METHODS ${1:method_name}\n  EXPORTING\n    !${2:ev_value} TYPE ${3:string}.$0",
		plain = "CLASS-METHODS method_name\n  EXPORTING\n    !ev_value TYPE string.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... CHANGING",
		snippet = "CLASS-METHODS ${1:method_name}\n  CHANGING\n    !${2:cv_value} TYPE ${3:string}.$0",
		plain = "CLASS-METHODS method_name\n  CHANGING\n    !cv_value TYPE string.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... RECEIVING",
		snippet = "CLASS-METHODS ${1:method_name}\n  RECEIVING\n    VALUE(${2:rv_result}) TYPE ${3:string}.$0",
		plain = "CLASS-METHODS method_name\n  RECEIVING\n    VALUE(rv_result) TYPE string.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... RETURNING",
		snippet = "CLASS-METHODS ${1:method_name}\n  RETURNING\n    VALUE(${2:rv_result}) TYPE ${3:string}.$0",
		plain = "CLASS-METHODS method_name\n  RETURNING\n    VALUE(rv_result) TYPE string.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... IMPORTING RETURNING",
		snippet = "CLASS-METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  RETURNING\n    VALUE(${4:rv_result}) TYPE ${5:string}.$0",
		plain = "CLASS-METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  RETURNING\n    VALUE(rv_result) TYPE string.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... IMPORTING EXPORTING",
		snippet = "CLASS-METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  EXPORTING\n    !${4:ev_value} TYPE ${5:string}.$0",
		plain = "CLASS-METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  EXPORTING\n    !ev_value TYPE string.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... IMPORTING CHANGING",
		snippet = "CLASS-METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  CHANGING\n    !${4:cv_value} TYPE ${5:string}.$0",
		plain = "CLASS-METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  CHANGING\n    !cv_value TYPE string.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... IMPORTING EXPORTING CHANGING",
		snippet = "CLASS-METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  EXPORTING\n    !${4:ev_value} TYPE ${5:string}\n  CHANGING\n    !${6:cv_value} TYPE ${7:string}.$0",
		plain = "CLASS-METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  EXPORTING\n    !ev_value TYPE string\n  CHANGING\n    !cv_value TYPE string.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... RAISING",
		snippet = "CLASS-METHODS ${1:method_name}\n  RAISING\n    ${2:cx_static_check}.$0",
		plain = "CLASS-METHODS method_name\n  RAISING\n    cx_static_check.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... IMPORTING RAISING",
		snippet = "CLASS-METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  RAISING\n    ${4:cx_static_check}.$0",
		plain = "CLASS-METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  RAISING\n    cx_static_check.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... IMPORTING RETURNING RAISING",
		snippet = "CLASS-METHODS ${1:method_name}\n  IMPORTING\n    !${2:iv_value} TYPE ${3:string}\n  RETURNING\n    VALUE(${4:rv_result}) TYPE ${5:string}\n  RAISING\n    ${6:cx_static_check}.$0",
		plain = "CLASS-METHODS method_name\n  IMPORTING\n    !iv_value TYPE string\n  RETURNING\n    VALUE(rv_result) TYPE string\n  RAISING\n    cx_static_check.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... EXCEPTIONS",
		snippet = "CLASS-METHODS ${1:method_name}\n  EXCEPTIONS\n    ${2:failed} = ${3:1}.$0",
		plain = "CLASS-METHODS method_name\n  EXCEPTIONS\n    failed = 1.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... FOR EVENT",
		snippet = "CLASS-METHODS ${1:on_event}\n  FOR EVENT ${2:event_name} OF ${3:lcl_source}\n  IMPORTING\n    !${4:sender}.$0",
		plain = "CLASS-METHODS on_event\n  FOR EVENT event_name OF lcl_source\n  IMPORTING\n    !sender.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... ABSTRACT",
		snippet = "CLASS-METHODS ${1:method_name} ABSTRACT.$0",
		plain = "CLASS-METHODS method_name ABSTRACT.",
	},
	{
		keyword = "CLASS-METHODS",
		label = "CLASS-METHODS ... FINAL",
		snippet = "CLASS-METHODS ${1:method_name} FINAL.$0",
		plain = "CLASS-METHODS method_name FINAL.",
	},
	{
		keyword = "SELECTION-SCREEN",
		label = "SELECTION-SCREEN BEGIN OF SCREEN ... END OF SCREEN",
		snippet = "SELECTION-SCREEN BEGIN OF SCREEN ${1:1000} TITLE ${2:sy-title}.\n  $0\nSELECTION-SCREEN END OF SCREEN ${1:1000}.",
		plain = "SELECTION-SCREEN BEGIN OF SCREEN 1000 TITLE sy-title.\n  \nSELECTION-SCREEN END OF SCREEN 1000.",
	},
	{
		keyword = "SELECTION-SCREEN",
		label = "SELECTION-SCREEN BEGIN OF BLOCK ... END OF BLOCK",
		snippet = "SELECTION-SCREEN BEGIN OF BLOCK ${1:b1} WITH FRAME TITLE ${2:text-001}.\n  $0\nSELECTION-SCREEN END OF BLOCK ${1:b1}.",
		plain = "SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.\n  \nSELECTION-SCREEN END OF BLOCK b1.",
	},
	{
		keyword = "SELECTION-SCREEN",
		label = "SELECTION-SCREEN BEGIN OF LINE ... END OF LINE",
		snippet = "SELECTION-SCREEN BEGIN OF LINE.\n  $0\nSELECTION-SCREEN END OF LINE.",
		plain = "SELECTION-SCREEN BEGIN OF LINE.\n  \nSELECTION-SCREEN END OF LINE.",
	},
	{
		keyword = "MESSAGE",
		label = "MESSAGE ... TYPE",
		snippet = "MESSAGE ${1:'Text'} TYPE ${2:'S'}.$0",
		plain = "MESSAGE 'Text' TYPE 'S'.",
	},
	{
		keyword = "MESSAGE",
		label = "MESSAGE ID ... TYPE ... NUMBER",
		snippet = "MESSAGE ID ${1:sy-msgid} TYPE ${2:sy-msgty} NUMBER ${3:sy-msgno} WITH ${4:sy-msgv1} ${5:sy-msgv2} ${6:sy-msgv3} ${7:sy-msgv4}.$0",
		plain = "MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.",
	},
	{
		keyword = "MESSAGE",
		label = "MESSAGE ... TYPE ... INTO",
		snippet = "MESSAGE ${1:'Text'} TYPE ${2:'S'} INTO ${3:lv_message}.$0",
		plain = "MESSAGE 'Text' TYPE 'S' INTO lv_message.",
	},
	{
		keyword = "OPEN",
		label = "OPEN DATASET ... FOR INPUT",
		snippet = "OPEN DATASET ${1:lv_filename} FOR INPUT IN TEXT MODE ENCODING DEFAULT\n             MESSAGE ${2:lv_message} IGNORING CONVERSION ERRORS.$0",
		plain = "OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT\n             MESSAGE lv_message IGNORING CONVERSION ERRORS.",
	},
	{
		keyword = "OPEN",
		label = "OPEN DATASET ... FOR OUTPUT",
		snippet = "OPEN DATASET ${1:lv_filename} FOR OUTPUT IN TEXT MODE ENCODING DEFAULT\n             MESSAGE ${2:lv_message}.$0",
		plain = "OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT\n             MESSAGE lv_message.",
	},
	{
		keyword = "OPEN",
		label = "OPEN DATASET ... READ DATASET ... CLOSE DATASET",
		snippet = "OPEN DATASET ${1:lv_filename} FOR INPUT IN TEXT MODE ENCODING DEFAULT\n             MESSAGE ${2:lv_message} IGNORING CONVERSION ERRORS.\n\nDO.\n  READ DATASET ${1:lv_filename} INTO ${3:lv_line}.\n  IF sy-subrc <> 0.\n    EXIT.\n  ENDIF.\n\n  $0\nENDDO.\n\nCLOSE DATASET ${1:lv_filename}.",
		plain = "OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT\n             MESSAGE lv_message IGNORING CONVERSION ERRORS.\n\nDO.\n  READ DATASET lv_filename INTO lv_line.\n  IF sy-subrc <> 0.\n    EXIT.\n  ENDIF.\n\n  \nENDDO.\n\nCLOSE DATASET lv_filename.",
	},
	{
		keyword = "READ",
		label = "READ DATASET ... INTO",
		snippet = "READ DATASET ${1:lv_filename} INTO ${2:lv_line}.$0",
		plain = "READ DATASET lv_filename INTO lv_line.",
	},
	{
		keyword = "READ",
		label = "READ DATASET ... INTO ... LENGTH",
		snippet = "READ DATASET ${1:lv_filename} INTO ${2:lv_line} MAXIMUM LENGTH ${3:lv_max_length} ACTUAL LENGTH ${4:lv_length}.$0",
		plain = "READ DATASET lv_filename INTO lv_line MAXIMUM LENGTH lv_max_length ACTUAL LENGTH lv_length.",
	},
	{
		keyword = "CLOSE",
		label = "CLOSE DATASET ...",
		snippet = "CLOSE DATASET ${1:lv_filename}.$0",
		plain = "CLOSE DATASET lv_filename.",
	},
	{
		keyword = "SUBMIT",
		label = "SUBMIT ... AND RETURN",
		snippet = "SUBMIT ${1:report} AND RETURN.$0",
		plain = "SUBMIT report AND RETURN.",
	},
	{
		keyword = "SUBMIT",
		label = "SUBMIT ... WITH ... EQ",
		snippet = "SUBMIT ${1:report}\n  WITH ${2:p_param} EQ ${3:lv_value}\n  AND RETURN.$0",
		plain = "SUBMIT report\n  WITH p_param EQ lv_value\n  AND RETURN.",
	},
	{
		keyword = "SUBMIT",
		label = "SUBMIT ... WITH ... IN",
		snippet = "SUBMIT ${1:report}\n  WITH ${2:s_range} IN ${3:lt_range}\n  AND RETURN.$0",
		plain = "SUBMIT report\n  WITH s_range IN lt_range\n  AND RETURN.",
	},
	{
		keyword = "SUBMIT",
		label = "SUBMIT ... VIA JOB ... NUMBER ... WITH ... USER ... AND RETURN",
		snippet = "SUBMIT ${1:report}\n  VIA JOB ${2:lv_jobname}\n  NUMBER ${3:lv_jobcount}\n  WITH ${4:s_range} IN ${5:lt_range}\n  WITH ${6:p_flag} EQ ${7:lv_flag}\n  USER ${8:sy-uname}\n  AND RETURN.$0",
		plain = "SUBMIT report\n  VIA JOB lv_jobname\n  NUMBER lv_jobcount\n  WITH s_range IN lt_range\n  WITH p_flag EQ lv_flag\n  USER sy-uname\n  AND RETURN.",
	},
	{
		keyword = "DESCRIBE",
		label = "DESCRIBE TABLE ... LINES",
		snippet = "DESCRIBE TABLE ${1:itab} LINES ${2:lv_lines}.$0",
		plain = "DESCRIBE TABLE itab LINES lv_lines.",
	},
	{
		keyword = "DESCRIBE",
		label = "DESCRIBE FIELD ... TYPE",
		snippet = "DESCRIBE FIELD ${1:lv_value} TYPE ${2:lv_type}.$0",
		plain = "DESCRIBE FIELD lv_value TYPE lv_type.",
	},
	{
		keyword = "DESCRIBE",
		label = "DESCRIBE FIELD ... LENGTH",
		snippet = "DESCRIBE FIELD ${1:lv_value} LENGTH ${2:lv_length} IN CHARACTER MODE.$0",
		plain = "DESCRIBE FIELD lv_value LENGTH lv_length IN CHARACTER MODE.",
	},
	{
		keyword = "EXPORT",
		label = "EXPORT ... TO MEMORY ID",
		snippet = "EXPORT ${1:name} = ${2:value} TO MEMORY ID ${3:'id'}.$0",
		plain = "EXPORT name = value TO MEMORY ID 'id'.",
	},
	{
		keyword = "EXPORT",
		label = "EXPORT ... TO DATABASE",
		snippet = "EXPORT ${1:name} = ${2:value} TO DATABASE ${3:indx(st)} ID ${4:lv_id}.$0",
		plain = "EXPORT name = value TO DATABASE indx(st) ID lv_id.",
	},
	{
		keyword = "EXPORT",
		label = "EXPORT ... TO DATA BUFFER",
		snippet = "EXPORT ${1:data} TO DATA BUFFER ${2:lv_buffer}.$0",
		plain = "EXPORT data TO DATA BUFFER lv_buffer.",
	},
	{
		keyword = "IMPORT",
		label = "IMPORT ... FROM MEMORY ID",
		snippet = "IMPORT ${1:name} = ${2:value} FROM MEMORY ID ${3:'id'}.$0",
		plain = "IMPORT name = value FROM MEMORY ID 'id'.",
	},
	{
		keyword = "IMPORT",
		label = "IMPORT ... FROM DATABASE",
		snippet = "IMPORT ${1:name} = ${2:value} FROM DATABASE ${3:indx(st)} ID ${4:lv_id}.$0",
		plain = "IMPORT name = value FROM DATABASE indx(st) ID lv_id.",
	},
	{
		keyword = "IMPORT",
		label = "IMPORT ... FROM DATA BUFFER",
		snippet = "IMPORT ${1:data} FROM DATA BUFFER ${2:lv_buffer}.$0",
		plain = "IMPORT data FROM DATA BUFFER lv_buffer.",
	},
	{
		keyword = "RAISE",
		label = "RAISE EXCEPTION TYPE",
		snippet = "RAISE EXCEPTION TYPE ${1:cx_static_check}.$0",
		plain = "RAISE EXCEPTION TYPE cx_static_check.",
	},
	{
		keyword = "RAISE",
		label = "RAISE EXCEPTION NEW",
		snippet = "RAISE EXCEPTION NEW ${1:cx_static_check}( ${2} ).$0",
		plain = "RAISE EXCEPTION NEW cx_static_check( ).",
	},
	{
		keyword = "RAISE",
		label = "RAISE ...",
		snippet = "RAISE ${1:exception}.$0",
		plain = "RAISE exception.",
	},
	{
		keyword = "INSERT",
		label = "INSERT ... INTO TABLE",
		snippet = "INSERT ${1:wa} INTO TABLE ${2:itab}.$0",
		plain = "INSERT wa INTO TABLE itab.",
	},
	{
		keyword = "INSERT",
		label = "INSERT ... INTO ... INDEX",
		snippet = "INSERT ${1:wa} INTO ${2:itab} INDEX ${3:lv_index}.$0",
		plain = "INSERT wa INTO itab INDEX lv_index.",
	},
	{
		keyword = "INSERT",
		label = "INSERT VALUE #( ... ) INTO TABLE",
		snippet = "INSERT VALUE #( ${1} ) INTO TABLE ${2:itab}.$0",
		plain = "INSERT VALUE #( ) INTO TABLE itab.",
	},
	{
		keyword = "INSERT",
		label = "INSERT ... FROM TABLE",
		snippet = "INSERT ${1:dbtab} FROM TABLE ${2:itab}.$0",
		plain = "INSERT dbtab FROM TABLE itab.",
	},
	{
		keyword = "INSERT",
		label = "INSERT ... FROM VALUE #( ... )",
		snippet = "INSERT ${1:dbtab} FROM VALUE #( ${2} ).$0",
		plain = "INSERT dbtab FROM VALUE #( ).",
	},
	{
		keyword = "MODIFY",
		label = "MODIFY ... FROM",
		snippet = "MODIFY ${1:itab} FROM ${2:wa}.$0",
		plain = "MODIFY itab FROM wa.",
	},
	{
		keyword = "MODIFY",
		label = "MODIFY ... FROM ... INDEX",
		snippet = "MODIFY ${1:itab} FROM ${2:wa} INDEX ${3:lv_index}.$0",
		plain = "MODIFY itab FROM wa INDEX lv_index.",
	},
	{
		keyword = "MODIFY",
		label = "MODIFY TABLE ... FROM",
		snippet = "MODIFY TABLE ${1:itab} FROM ${2:wa}.$0",
		plain = "MODIFY TABLE itab FROM wa.",
	},
	{
		keyword = "MODIFY",
		label = "MODIFY ... FROM ... TRANSPORTING ... WHERE",
		snippet = "MODIFY ${1:itab} FROM ${2:wa} TRANSPORTING ${3:field} WHERE ${4:key_field} = ${5:lv_key}.$0",
		plain = "MODIFY itab FROM wa TRANSPORTING field WHERE key_field = lv_key.",
	},
	{
		keyword = "MODIFY",
		label = "MODIFY ... FROM TABLE",
		snippet = "MODIFY ${1:dbtab} FROM TABLE ${2:itab}.$0",
		plain = "MODIFY dbtab FROM TABLE itab.",
	},
	{
		keyword = "MODIFY",
		label = "MODIFY ... FROM VALUE #( ... )",
		snippet = "MODIFY ${1:dbtab} FROM VALUE #( ${2} ).$0",
		plain = "MODIFY dbtab FROM VALUE #( ).",
	},
	{
		keyword = "MODIFY",
		label = "MODIFY ... FROM VALUE #( ... ) TRANSPORTING",
		snippet = "MODIFY ${1:itab} FROM VALUE #( ${2} ) TRANSPORTING ${3:field}.$0",
		plain = "MODIFY itab FROM VALUE #( ) TRANSPORTING field.",
	},
	{
		keyword = "MODIFY",
		label = "MODIFY SCREEN",
		snippet = "MODIFY SCREEN.$0",
		plain = "MODIFY SCREEN.",
	},
	{
		keyword = "MODIFY",
		label = "MODIFY CURRENT LINE",
		snippet = "MODIFY CURRENT LINE.$0",
		plain = "MODIFY CURRENT LINE.",
	},
	{
		keyword = "MODIFY",
		label = "MODIFY CURRENT LINE FIELD VALUE ... INTO",
		snippet = "MODIFY CURRENT LINE FIELD VALUE ${1:field_name} INTO ${2:lv_value}.$0",
		plain = "MODIFY CURRENT LINE FIELD VALUE field_name INTO lv_value.",
	},
	{
		keyword = "MODIFY",
		label = "MODIFY LINE ... INDEX",
		snippet = "MODIFY LINE ${1:lv_line} INDEX ${2:lv_index}.$0",
		plain = "MODIFY LINE lv_line INDEX lv_index.",
	},
	{
		keyword = "DELETE",
		label = "DELETE ... INDEX",
		snippet = "DELETE ${1:itab} INDEX ${2:lv_index}.$0",
		plain = "DELETE itab INDEX lv_index.",
	},
	{
		keyword = "DELETE",
		label = "DELETE ... WHERE",
		snippet = "DELETE ${1:itab} WHERE ${2:field} = ${3:lv_value}.$0",
		plain = "DELETE itab WHERE field = lv_value.",
	},
	{
		keyword = "DELETE",
		label = "DELETE TABLE ... WITH TABLE KEY",
		snippet = "DELETE TABLE ${1:itab} WITH TABLE KEY ${2:field} = ${3:lv_value}.$0",
		plain = "DELETE TABLE itab WITH TABLE KEY field = lv_value.",
	},
	{
		keyword = "DELETE",
		label = "DELETE ADJACENT DUPLICATES ... COMPARING",
		snippet = "DELETE ADJACENT DUPLICATES FROM ${1:itab} COMPARING ${2:field}.$0",
		plain = "DELETE ADJACENT DUPLICATES FROM itab COMPARING field.",
	},
	{
		keyword = "DELETE",
		label = "DELETE FROM ... WHERE",
		snippet = "DELETE FROM ${1:dbtab} WHERE ${2:field} = @${3:lv_value}.$0",
		plain = "DELETE FROM dbtab WHERE field = @lv_value.",
	},
	{
		keyword = "DELETE",
		label = "DELETE ... FROM TABLE",
		snippet = "DELETE ${1:dbtab} FROM TABLE ${2:itab}.$0",
		plain = "DELETE dbtab FROM TABLE itab.",
	},
	{
		keyword = "UPDATE",
		label = "UPDATE ... SET ... WHERE",
		snippet = "UPDATE ${1:dbtab} SET ${2:field} = @${3:lv_value} WHERE ${4:key_field} = @${5:lv_key}.$0",
		plain = "UPDATE dbtab SET field = @lv_value WHERE key_field = @lv_key.",
	},
	{
		keyword = "UPDATE",
		label = "UPDATE ... FROM",
		snippet = "UPDATE ${1:dbtab} FROM ${2:wa}.$0",
		plain = "UPDATE dbtab FROM wa.",
	},
	{
		keyword = "UPDATE",
		label = "UPDATE ... FROM TABLE",
		snippet = "UPDATE ${1:dbtab} FROM TABLE ${2:itab}.$0",
		plain = "UPDATE dbtab FROM TABLE itab.",
	},
	{
		keyword = "FIELD-SYMBOLS",
		label = "FIELD-SYMBOLS ... TYPE",
		snippet = "FIELD-SYMBOLS <${1:fs}> TYPE ${2:any}.$0",
		plain = "FIELD-SYMBOLS <fs> TYPE any.",
	},
	{
		keyword = "FIELD-SYMBOLS",
		label = "FIELD-SYMBOLS ... LIKE LINE OF",
		snippet = "FIELD-SYMBOLS <${1:fs}> LIKE LINE OF ${2:itab}.$0",
		plain = "FIELD-SYMBOLS <fs> LIKE LINE OF itab.",
	},
	{
		keyword = "FIELD-SYMBOLS",
		label = "FIELD-SYMBOLS ... TYPE ANY TABLE",
		snippet = "FIELD-SYMBOLS <${1:table}> TYPE ANY TABLE.$0",
		plain = "FIELD-SYMBOLS <table> TYPE ANY TABLE.",
	},
	{
		keyword = "MOVE-CORRESPONDING",
		label = "MOVE-CORRESPONDING ... TO",
		snippet = "MOVE-CORRESPONDING ${1:source} TO ${2:target}.$0",
		plain = "MOVE-CORRESPONDING source TO target.",
	},
	{
		keyword = "MOVE-CORRESPONDING",
		label = "MOVE-CORRESPONDING EXACT ... TO",
		snippet = "MOVE-CORRESPONDING EXACT ${1:source} TO ${2:target}.$0",
		plain = "MOVE-CORRESPONDING EXACT source TO target.",
	},
	{
		keyword = "CONDENSE",
		label = "CONDENSE ...",
		snippet = "CONDENSE ${1:lv_text}.$0",
		plain = "CONDENSE lv_text.",
	},
	{
		keyword = "CONDENSE",
		label = "CONDENSE ... NO-GAPS",
		snippet = "CONDENSE ${1:lv_text} NO-GAPS.$0",
		plain = "CONDENSE lv_text NO-GAPS.",
	},
	{
		keyword = "CONVERT",
		label = "CONVERT DATE ... TIME ... INTO TIME STAMP",
		snippet = "CONVERT DATE ${1:lv_date}\n        TIME ${2:lv_time}\n        INTO TIME STAMP DATA(${3:lv_timestamp})\n        TIME ZONE ${4:lv_time_zone}.$0",
		plain = "CONVERT DATE lv_date\n        TIME lv_time\n        INTO TIME STAMP DATA(lv_timestamp)\n        TIME ZONE lv_time_zone.",
	},
	{
		keyword = "CONVERT",
		label = "CONVERT DATE ... TIME ... DAYLIGHT SAVING TIME ... INTO TIME STAMP",
		snippet = "CONVERT DATE ${1:lv_date}\n        TIME ${2:lv_time}\n        DAYLIGHT SAVING TIME ${3:lv_dst}\n        INTO TIME STAMP DATA(${4:lv_timestamp})\n        TIME ZONE ${5:lv_time_zone}.$0",
		plain = "CONVERT DATE lv_date\n        TIME lv_time\n        DAYLIGHT SAVING TIME lv_dst\n        INTO TIME STAMP DATA(lv_timestamp)\n        TIME ZONE lv_time_zone.",
	},
	{
		keyword = "CONVERT",
		label = "CONVERT TIME STAMP ... INTO DATE ... TIME",
		snippet = "CONVERT TIME STAMP ${1:lv_timestamp}\n        TIME ZONE ${2:lv_time_zone}\n        INTO DATE DATA(${3:lv_date})\n             TIME DATA(${4:lv_time}).$0",
		plain = "CONVERT TIME STAMP lv_timestamp\n        TIME ZONE lv_time_zone\n        INTO DATE DATA(lv_date)\n             TIME DATA(lv_time).",
	},
	{
		keyword = "CONCATENATE",
		label = "CONCATENATE ... INTO",
		snippet = "CONCATENATE ${1:lv_a} ${2:lv_b} INTO ${3:lv_text}.$0",
		plain = "CONCATENATE lv_a lv_b INTO lv_text.",
	},
	{
		keyword = "CONCATENATE",
		label = "CONCATENATE ... INTO ... SEPARATED BY",
		snippet = "CONCATENATE ${1:lv_a} ${2:lv_b} INTO ${3:lv_text} SEPARATED BY ${4:space}.$0",
		plain = "CONCATENATE lv_a lv_b INTO lv_text SEPARATED BY space.",
	},
	{
		keyword = "CONCATENATE",
		label = "CONCATENATE LINES OF ... INTO",
		snippet = "CONCATENATE LINES OF ${1:lt_lines} INTO ${2:lv_text} SEPARATED BY ${3:cl_abap_char_utilities=>newline}.$0",
		plain = "CONCATENATE LINES OF lt_lines INTO lv_text SEPARATED BY cl_abap_char_utilities=>newline.",
	},
	{
		keyword = "SPLIT",
		label = "SPLIT ... AT ... INTO TABLE",
		snippet = "SPLIT ${1:lv_text} AT ${2:','} INTO TABLE ${3:lt_parts}.$0",
		plain = "SPLIT lv_text AT ',' INTO TABLE lt_parts.",
	},
	{
		keyword = "SPLIT",
		label = "SPLIT ... AT ... INTO",
		snippet = "SPLIT ${1:lv_text} AT ${2:','} INTO ${3:lv_part1} ${4:lv_part2}.$0",
		plain = "SPLIT lv_text AT ',' INTO lv_part1 lv_part2.",
	},
	{
		keyword = "APPEND",
		label = "APPEND ... TO",
		snippet = "APPEND ${1:wa} TO ${2:itab}.$0",
		plain = "APPEND wa TO itab.",
	},
	{
		keyword = "APPEND",
		label = "APPEND VALUE #( ... ) TO",
		snippet = "APPEND VALUE #( ${1} ) TO ${2:itab}.$0",
		plain = "APPEND VALUE #( ) TO itab.",
	},
	{
		keyword = "APPEND",
		label = "APPEND INITIAL LINE ... ASSIGNING",
		snippet = "APPEND INITIAL LINE TO ${1:itab} ASSIGNING FIELD-SYMBOL(<${2:row}>).$0",
		plain = "APPEND INITIAL LINE TO itab ASSIGNING FIELD-SYMBOL(<row>).",
	},
	{
		keyword = "APPEND",
		label = "APPEND LINES OF ... TO",
		snippet = "APPEND LINES OF ${1:source} TO ${2:target}.$0",
		plain = "APPEND LINES OF source TO target.",
	},
	{
		keyword = "SORT",
		label = "SORT ... BY",
		snippet = "SORT ${1:itab} BY ${2:field}.$0",
		plain = "SORT itab BY field.",
	},
	{
		keyword = "SORT",
		label = "SORT ... STABLE BY",
		snippet = "SORT ${1:itab} STABLE BY ${2:field}.$0",
		plain = "SORT itab STABLE BY field.",
	},
	{
		keyword = "SORT",
		label = "SORT ... BY ... DESCENDING",
		snippet = "SORT ${1:itab} BY ${2:field} DESCENDING.$0",
		plain = "SORT itab BY field DESCENDING.",
	},
	{
		keyword = "FIND",
		label = "FIND ... IN",
		snippet = "FIND ${1:'text'} IN ${2:lv_text}.$0",
		plain = "FIND 'text' IN lv_text.",
	},
	{
		keyword = "FIND",
		label = "FIND FIRST OCCURRENCE OF ... IN",
		snippet = "FIND FIRST OCCURRENCE OF ${1:'text'} IN ${2:lv_text} MATCH OFFSET ${3:lv_offset} MATCH LENGTH ${4:lv_length}.$0",
		plain = "FIND FIRST OCCURRENCE OF 'text' IN lv_text MATCH OFFSET lv_offset MATCH LENGTH lv_length.",
	},
	{
		keyword = "FIND",
		label = "FIND ALL OCCURRENCES OF ... IN",
		snippet = "FIND ALL OCCURRENCES OF ${1:'text'} IN ${2:lv_text} MATCH COUNT ${3:lv_count}.$0",
		plain = "FIND ALL OCCURRENCES OF 'text' IN lv_text MATCH COUNT lv_count.",
	},
	{
		keyword = "FIND",
		label = "FIND REGEX ... IN",
		snippet = "FIND REGEX ${1:'pattern'} IN ${2:lv_text} MATCH OFFSET ${3:lv_offset} MATCH LENGTH ${4:lv_length}.$0",
		plain = "FIND REGEX 'pattern' IN lv_text MATCH OFFSET lv_offset MATCH LENGTH lv_length.",
	},
	{
		keyword = "FIND",
		label = "FIND REGEX ... IN TABLE",
		snippet = "FIND REGEX ${1:'pattern'} IN TABLE ${2:lt_text} MATCH LINE ${3:lv_line} MATCH OFFSET ${4:lv_offset} SUBMATCHES ${5:lv_match}.$0",
		plain = "FIND REGEX 'pattern' IN TABLE lt_text MATCH LINE lv_line MATCH OFFSET lv_offset SUBMATCHES lv_match.",
	},
	{
		keyword = "FIND",
		label = "FIND ALL OCCURRENCES OF REGEX ... IN TABLE ... RESULTS",
		snippet = "FIND ALL OCCURRENCES OF REGEX ${1:'pattern'} IN TABLE ${2:lt_text} RESULTS ${3:lt_results}.$0",
		plain = "FIND ALL OCCURRENCES OF REGEX 'pattern' IN TABLE lt_text RESULTS lt_results.",
	},
	{
		keyword = "FIND",
		label = "FIND ... IN SECTION OFFSET ... LENGTH ... OF",
		snippet = "FIND ${1:'text'} IN SECTION OFFSET ${2:lv_offset} LENGTH ${3:lv_length} OF ${4:lv_text} MATCH OFFSET ${5:lv_match_offset}.$0",
		plain = "FIND 'text' IN SECTION OFFSET lv_offset LENGTH lv_length OF lv_text MATCH OFFSET lv_match_offset.",
	},
}

completion_expression_template_count :: proc(source: string, offset: int, prefix: string) -> int {
	if !completion_template_at_expression_start(source, offset) {
		return 0
	}
	count := 0
	for template in EXPRESSION_TEMPLATES {
		if completion_keyword_prefix_matches(prefix, template.keyword) {
			count += 1
		}
	}
	return count
}

completion_append_expression_templates :: proc(
	out: []Completion_Item,
	prefix: string,
	replace_range: Range,
	snippets_supported: bool,
	allocator: mem.Allocator,
) {
	index := 0
	for template in EXPRESSION_TEMPLATES {
		if !completion_keyword_prefix_matches(prefix, template.keyword) {
			continue
		}
		assert(index < len(out))
		out[index] = completion_statement_template_item(
			template,
			replace_range,
			"",
			snippets_supported,
			allocator,
		)
		index += 1
	}
	assert(index == len(out))
}

completion_common_statement_template_count :: proc(
	source: string,
	offset: int,
	prefix: string,
) -> int {
	count := 0
	for template in COMMON_STATEMENT_TEMPLATES {
		if completion_common_statement_template_matches(source, offset, prefix, template) {
			count += 1
		}
	}
	return count
}

completion_append_common_statement_templates :: proc(
	out: []Completion_Item,
	source: string,
	offset: int,
	prefix: string,
	replace_range: Range,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) {
	index := 0
	for template in COMMON_STATEMENT_TEMPLATES {
		if !completion_common_statement_template_matches(source, offset, prefix, template) {
			continue
		}
		assert(index < len(out))
		out[index] = completion_statement_template_item(
			template,
			replace_range,
			indent,
			snippets_supported,
			allocator,
		)
		index += 1
	}
	assert(index == len(out))
}

completion_common_statement_template_matches :: proc(
	source: string,
	offset: int,
	prefix: string,
	template: Completion_Statement_Template,
) -> bool {
	if !completion_keyword_prefix_matches(prefix, template.keyword) {
		return false
	}
	if template.types_chain_clause {
		return completion_template_in_types_chain_clause(source, offset)
	}
	if template.type_addition_clause {
		if !completion_template_in_type_addition_clause(source, offset) {
			return false
		}
		return !template.type_addition_complex_definition ||
		       !completion_template_in_oop_signature_type_addition_clause(source, offset)
	}
	return completion_template_at_statement_start(source, offset)
}

completion_statement_template_item :: proc(
	template: Completion_Statement_Template,
	replace_range: Range,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	insert_text := completion_statement_template_insert_text(
		template,
		indent,
		snippets_supported,
		allocator,
	)
	return Completion_Item {
		label = template.label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", template.label, allocator),
		insert_text = insert_text,
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
		text_edit = Text_Edit{range = replace_range, new_text = insert_text},
	}
}

completion_statement_template_insert_text :: proc(
	template: Completion_Statement_Template,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> string {
	text := template.snippet if snippets_supported else template.plain
	if snippets_supported || indent == "" || !strings.contains(text, "\n") {
		return strings.clone(text, allocator)
	}
	out := strings.builder_make(allocator)
	for i in 0 ..< len(text) {
		strings.write_byte(&out, text[i])
		if text[i] == '\n' {
			strings.write_string(&out, indent)
		}
	}
	return strings.to_string(out)
}

completion_keyword_prefix_matches :: proc(prefix, keyword: string) -> bool {
	if prefix == "" || len(prefix) > len(keyword) {
		return false
	}
	lower := utils.to_lower_ascii(prefix, context.temp_allocator)
	keyword_lower := utils.to_lower_ascii(keyword, context.temp_allocator)
	return strings.has_prefix(keyword_lower, lower)
}

completion_template_base_indent :: proc "contextless" (
	indent: string,
	snippets_supported: bool,
) -> string {
	return "" if snippets_supported else indent
}

completion_template_at_statement_start :: proc(source: string, offset: int) -> bool {
	prefix_start := completion_template_prefix_start(source, offset)
	i := prefix_start
	for i > 0 {
		switch source[i - 1] {
		case ' ', '\t', '\r':
			i -= 1
			continue
		}
		break
	}
	if i == 0 {
		return true
	}
	prev := source[i - 1]
	return prev == '\n' || prev == '.'
}

completion_template_in_types_chain_clause :: proc(source: string, offset: int) -> bool {
	prefix_start := completion_template_prefix_start(source, offset)
	i := completion_template_skip_space_backward(source, prefix_start)
	if i == 0 || source[i - 1] != ',' {
		return false
	}

	colon := completion_template_chain_colon_before(source, i - 1)
	if colon < 0 {
		return false
	}

	keyword_end := completion_template_skip_space_backward(source, colon)
	keyword_start := keyword_end
	for keyword_start > 0 && completion_template_prefix_char(source[keyword_start - 1]) {
		keyword_start -= 1
	}
	if keyword_start == keyword_end {
		return false
	}

	keyword := utils.to_lower_ascii(source[keyword_start:keyword_end], context.temp_allocator)
	return keyword == "types"
}

completion_template_in_type_addition_clause :: proc(source: string, offset: int) -> bool {
	prefix_start := completion_template_prefix_start(source, offset)
	i := completion_template_skip_space_backward(source, prefix_start)
	if i == 0 {
		return false
	}

	switch source[i - 1] {
	case ':', ',', '.':
		return false
	}

	keyword_start, keyword_end, keyword_ok := completion_template_statement_keyword_before(
		source,
		prefix_start,
	)
	if !keyword_ok || keyword_end >= i {
		return false
	}
	keyword := utils.to_lower_ascii(source[keyword_start:keyword_end], context.temp_allocator)
	if !completion_template_type_addition_after_decl_name(source, prefix_start) {
		return false
	}
	if completion_template_type_addition_decl_statement_keyword(keyword) {
		return completion_template_type_addition_decl_statement_context(
			source,
			keyword_end,
			prefix_start,
		)
	}
	if completion_template_type_addition_signature_keyword(keyword) {
		return completion_template_has_word_between(
			source,
			keyword_end,
			prefix_start,
			"importing",
		) ||
		       completion_template_has_word_between(source, keyword_end, prefix_start, "exporting") ||
		       completion_template_has_word_between(source, keyword_end, prefix_start, "changing") ||
		       completion_template_has_word_between(source, keyword_end, prefix_start, "returning") ||
		       completion_template_has_word_between(source, keyword_end, prefix_start, "receiving")
	}
	if keyword == "form" {
		return completion_template_has_word_between(source, keyword_end, prefix_start, "using") ||
		       completion_template_has_word_between(source, keyword_end, prefix_start, "changing") ||
		       completion_template_has_word_between(source, keyword_end, prefix_start, "tables")
	}
	if keyword == "function" {
		return completion_template_has_word_between(
			source,
			keyword_end,
			prefix_start,
			"importing",
		) ||
		       completion_template_has_word_between(source, keyword_end, prefix_start, "exporting") ||
		       completion_template_has_word_between(source, keyword_end, prefix_start, "changing") ||
		       completion_template_has_word_between(source, keyword_end, prefix_start, "tables")
	}
	return false
}

completion_template_in_oop_signature_type_addition_clause :: proc(source: string, offset: int) -> bool {
	prefix_start := completion_template_prefix_start(source, offset)
	keyword_start, keyword_end, keyword_ok := completion_template_statement_keyword_before(
		source,
		prefix_start,
	)
	if !keyword_ok || keyword_end <= keyword_start {
		return false
	}
	keyword := utils.to_lower_ascii(source[keyword_start:keyword_end], context.temp_allocator)
	if !completion_template_type_addition_signature_keyword(keyword) {
		return false
	}
	return completion_template_has_word_between(source, keyword_end, prefix_start, "importing") ||
	       completion_template_has_word_between(source, keyword_end, prefix_start, "exporting") ||
	       completion_template_has_word_between(source, keyword_end, prefix_start, "changing") ||
	       completion_template_has_word_between(source, keyword_end, prefix_start, "returning") ||
	       completion_template_has_word_between(source, keyword_end, prefix_start, "receiving")
}

completion_template_type_addition_decl_statement_keyword :: proc "contextless" (
	keyword: string,
) -> bool {
	return keyword == "types" ||
	       keyword == "data" ||
	       keyword == "class-data" ||
	       keyword == "constants" ||
	       keyword == "statics" ||
	       keyword == "field-symbols" ||
	       keyword == "parameters" ||
	       keyword == "parameter"
}

completion_template_type_addition_signature_keyword :: proc "contextless" (
	keyword: string,
) -> bool {
	return keyword == "methods" ||
	       keyword == "class-methods" ||
	       keyword == "events" ||
	       keyword == "class-events"
}

completion_template_type_addition_decl_statement_context :: proc(
	source: string,
	keyword_end: int,
	prefix_start: int,
) -> bool {
	clause_start := completion_template_decl_clause_start(source, keyword_end, prefix_start)
	return !completion_template_has_word_between(source, clause_start, prefix_start, "type") &&
	       !completion_template_has_word_between(source, clause_start, prefix_start, "like") &&
	       !completion_template_has_word_between(source, clause_start, prefix_start, "structure")
}

completion_template_decl_clause_start :: proc(source: string, start, offset: int) -> int {
	out := clamp(start, 0, len(source))
	end := clamp(offset, out, len(source))
	for i := out; i < end; i += 1 {
		switch source[i] {
		case ':', ',':
			out = i + 1
		}
	}
	return out
}

completion_template_type_addition_after_decl_name :: proc(source: string, offset: int) -> bool {
	i := completion_template_skip_space_backward(source, offset)
	if i == 0 {
		return false
	}
	ch := source[i - 1]
	if ch == ')' {
		return completion_template_wrapped_decl_name_before(source, i)
	}
	if ch == '>' {
		return completion_template_field_symbol_name_before(source, i)
	}
	if !completion_template_decl_name_char(ch) {
		return false
	}
	start := i
	for start > 0 && completion_template_decl_name_char(source[start - 1]) {
		start -= 1
	}
	word := utils.to_lower_ascii(source[start:i], context.temp_allocator)
	return !completion_template_type_addition_forbidden_previous_word(word)
}

completion_template_wrapped_decl_name_before :: proc(source: string, offset: int) -> bool {
	i := clamp(offset, 0, len(source))
	depth := 0
	open := -1
	for i > 0 {
		i -= 1
		switch source[i] {
		case ')':
			depth += 1
		case '(':
			depth -= 1
			if depth == 0 {
				open = i
				break
			}
		}
	}
	if open < 0 || open + 1 >= offset - 1 {
		return false
	}
	wrapper_end := completion_template_skip_space_backward(source, open)
	wrapper_start := wrapper_end
	for wrapper_start > 0 && completion_template_prefix_char(source[wrapper_start - 1]) {
		wrapper_start -= 1
	}
	if wrapper_start == wrapper_end {
		return false
	}
	wrapper := utils.to_lower_ascii(source[wrapper_start:wrapper_end], context.temp_allocator)
	return wrapper == "value" || wrapper == "reference"
}

completion_template_field_symbol_name_before :: proc(source: string, offset: int) -> bool {
	i := clamp(offset, 0, len(source))
	for i > 0 {
		i -= 1
		switch source[i] {
		case '<':
			return i + 1 < offset - 1
		case ' ', '\t', '\r', '\n', ',', '.', ':':
			return false
		}
	}
	return false
}

completion_template_type_addition_forbidden_previous_word :: proc "contextless" (
	word: string,
) -> bool {
	return word == "type" ||
	       word == "like" ||
	       word == "structure" ||
	       word == "importing" ||
	       word == "exporting" ||
	       word == "changing" ||
	       word == "returning" ||
	       word == "receiving" ||
	       word == "using" ||
	       word == "tables" ||
	       word == "exceptions" ||
	       word == "raising" ||
	       word == "for" ||
	       word == "event" ||
	       word == "of" ||
	       word == "default" ||
	       word == "optional" ||
	       word == "preferred" ||
	       word == "parameter" ||
	       word == "as" ||
	       word == "with" ||
	       word == "key"
}

completion_template_has_word_between :: proc(
	source: string,
	start, offset: int,
	word: string,
) -> bool {
	i := clamp(start, 0, len(source))
	end := clamp(offset, i, len(source))
	for i < end {
		if !completion_template_decl_name_char(source[i]) {
			i += 1
			continue
		}
		word_start := i
		for i < end && completion_template_decl_name_char(source[i]) {
			i += 1
		}
		candidate := utils.to_lower_ascii(source[word_start:i], context.temp_allocator)
		if candidate == word {
			return true
		}
	}
	return false
}

completion_template_decl_name_char :: proc "contextless" (ch: u8) -> bool {
	return completion_template_prefix_char(ch) || ch == '!'
}

completion_template_statement_keyword_before :: proc(
	source: string,
	offset: int,
) -> (int, int, bool) {
	start := clamp(offset, 0, len(source))
	for start > 0 && source[start - 1] != '.' {
		start -= 1
	}
	for start < len(source) && completion_template_space_char(source[start]) {
		start += 1
	}

	end := start
	for end < len(source) && completion_template_prefix_char(source[end]) {
		end += 1
	}
	return start, end, start < end
}

completion_template_skip_space_backward :: proc(source: string, offset: int) -> int {
	i := clamp(offset, 0, len(source))
	for i > 0 {
		if completion_template_space_char(source[i - 1]) {
			i -= 1
			continue
		}
		break
	}
	return i
}

completion_template_space_char :: proc "contextless" (ch: u8) -> bool {
	return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'
}

completion_template_chain_colon_before :: proc(source: string, offset: int) -> int {
	i := clamp(offset, 0, len(source))
	for i > 0 {
		i -= 1
		switch source[i] {
		case ':':
			return i
		case '.':
			return -1
		}
	}
	return -1
}

completion_template_at_expression_start :: proc(source: string, offset: int) -> bool {
	prefix_start := completion_template_prefix_start(source, offset)
	if prefix_start == 0 {
		return true
	}
	switch source[prefix_start - 1] {
	case ' ', '\t', '\r', '\n', '.', '(', '[', '{', ',', '=', '+', '*', '/', '<':
		return true
	}
	return false
}

completion_case_template_item :: proc(
	indent: string,
	replace_range: Range,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	label := "CASE ... WHEN ... WHEN OTHERS"
	insert_text := completion_case_template_insert_text(indent, snippets_supported, allocator)
	return Completion_Item {
		label = label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", label, allocator),
		insert_text = insert_text,
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
		text_edit = Text_Edit{range = replace_range, new_text = insert_text},
	}
}

completion_case_template_insert_text :: proc(
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> string {
	base_indent := completion_template_base_indent(indent, snippets_supported)
	out := strings.builder_make(allocator)
	strings.write_string(&out, "CASE ${1:lv_value}." if snippets_supported else "CASE lv_value.")
	completion_template_write_newline_indent(
		&out,
		base_indent,
		1,
		"WHEN ${2:value_1}." if snippets_supported else "WHEN value_1.",
	)
	completion_template_write_newline_indent(&out, base_indent, 2, "${3}" if snippets_supported else "")
	completion_template_write_newline_indent(
		&out,
		base_indent,
		1,
		"WHEN ${4:value_2}." if snippets_supported else "WHEN value_2.",
	)
	completion_template_write_newline_indent(&out, base_indent, 2, "${5}" if snippets_supported else "")
	completion_template_write_newline_indent(&out, base_indent, 1, "WHEN OTHERS.")
	completion_template_write_newline_indent(&out, base_indent, 2, "$0" if snippets_supported else "")
	completion_template_write_newline_indent(&out, base_indent, 0, "ENDCASE.")
	return strings.to_string(out)
}

completion_append_if_templates :: proc(
	out: []Completion_Item,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) {
	assert(len(out) == IF_TEMPLATE_COUNT)
	out[0] = completion_if_template_item(
		"IF ... ENDIF",
		.Generic,
		indent,
		snippets_supported,
		allocator,
	)
	out[1] = completion_if_template_item(
		"IF sy-subrc = 0",
		.Sy_Subrc_Zero,
		indent,
		snippets_supported,
		allocator,
	)
	out[2] = completion_if_template_item(
		"IF sy-subrc <> 0",
		.Sy_Subrc_Not_Zero,
		indent,
		snippets_supported,
		allocator,
	)
	out[3] = completion_if_template_item(
		"IF ... IS INITIAL",
		.Is_Initial,
		indent,
		snippets_supported,
		allocator,
	)
	out[4] = completion_if_template_item(
		"IF ... IS NOT INITIAL",
		.Is_Not_Initial,
		indent,
		snippets_supported,
		allocator,
	)
}

completion_if_template_item :: proc(
	label: string,
	variant: Completion_If_Template,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	return Completion_Item {
		label = label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", label, allocator),
		insert_text = completion_if_template_insert_text(
			variant,
			indent,
			snippets_supported,
			allocator,
		),
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
}

completion_if_template_insert_text :: proc(
	variant: Completion_If_Template,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> string {
	base_indent := completion_template_base_indent(indent, snippets_supported)
	out := strings.builder_make(allocator)
	strings.write_string(&out, completion_if_template_header(variant, snippets_supported))
	strings.write_byte(&out, '\n')
	strings.write_string(&out, base_indent)
	strings.write_string(&out, "  ")
	if snippets_supported {
		strings.write_string(&out, "$0")
	}
	strings.write_byte(&out, '\n')
	strings.write_string(&out, base_indent)
	strings.write_string(&out, "ENDIF.")
	return strings.to_string(out)
}

completion_if_template_header :: proc "contextless" (
	variant: Completion_If_Template,
	snippets_supported: bool,
) -> string {
	switch variant {
	case .Generic:
		return "IF ${1:condition}." if snippets_supported else "IF condition."
	case .Sy_Subrc_Zero:
		return "IF sy-subrc = 0."
	case .Sy_Subrc_Not_Zero:
		return "IF sy-subrc <> 0."
	case .Is_Initial:
		return "IF ${1:lv_value} IS INITIAL." if snippets_supported else "IF lv_value IS INITIAL."
	case .Is_Not_Initial:
		return(
			"IF ${1:lv_value} IS NOT INITIAL." if snippets_supported else "IF lv_value IS NOT INITIAL." \
		)
	}
	return ""
}

completion_append_class_templates :: proc(
	out: []Completion_Item,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) {
	assert(len(out) == CLASS_TEMPLATE_COUNT)
	out[0] = completion_class_template_item(
		"CLASS ... DEFINITION / IMPLEMENTATION",
		.Basic,
		indent,
		snippets_supported,
		allocator,
	)
	out[1] = completion_class_template_item(
		"CLASS ... DEFINITION PUBLIC FINAL CREATE PUBLIC",
		.Public_Final_Create_Public,
		indent,
		snippets_supported,
		allocator,
	)
	out[2] = completion_class_template_item(
		"CLASS ... DEFINITION INHERITING FROM",
		.Inheriting_From,
		indent,
		snippets_supported,
		allocator,
	)
	out[3] = completion_class_template_item(
		"CLASS ... DEFINITION FINAL CREATE PUBLIC",
		.Final_Create_Public,
		indent,
		snippets_supported,
		allocator,
	)
	out[4] = completion_class_template_item(
		"CLASS ... DEFINITION ABSTRACT",
		.Abstract,
		indent,
		snippets_supported,
		allocator,
	)
	out[5] = completion_class_template_item(
		"CLASS ... DEFINITION FOR TESTING",
		.For_Testing,
		indent,
		snippets_supported,
		allocator,
	)
}

completion_class_template_item :: proc(
	label: string,
	variant: Completion_Class_Template,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	return Completion_Item {
		label = label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", label, allocator),
		insert_text = completion_class_template_insert_text(
			variant,
			indent,
			snippets_supported,
			allocator,
		),
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
}

completion_class_template_insert_text :: proc(
	variant: Completion_Class_Template,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> string {
	base_indent := completion_template_base_indent(indent, snippets_supported)
	out := strings.builder_make(allocator)
	switch variant {
	case .Basic:
		class_name := "${1:lcl_class}" if snippets_supported else "lcl_class"
		completion_write_class_definition_header(&out, class_name, "DEFINITION.")
		completion_write_class_public_section(&out, base_indent, "$0" if snippets_supported else "")
		completion_write_class_implementation(&out, class_name, base_indent)
	case .Public_Final_Create_Public:
		class_name := "${1:zcl_class}" if snippets_supported else "zcl_class"
		completion_write_class_definition_header(
			&out,
			class_name,
			"DEFINITION PUBLIC FINAL CREATE PUBLIC.",
		)
		completion_write_class_public_section(&out, base_indent, "$0" if snippets_supported else "")
		completion_write_class_implementation(&out, class_name, base_indent)
	case .Inheriting_From:
		class_name := "${1:lcl_child}" if snippets_supported else "lcl_child"
		superclass_name := "${2:lcl_parent}" if snippets_supported else "lcl_parent"
		completion_write_class_definition_header(
			&out,
			class_name,
			strings.concatenate(
				{"DEFINITION INHERITING FROM ", superclass_name, "."},
				context.temp_allocator,
			),
		)
		completion_write_class_public_section(&out, base_indent, "$0" if snippets_supported else "")
		completion_write_class_implementation(&out, class_name, base_indent)
	case .Final_Create_Public:
		class_name := "${1:lcl_class}" if snippets_supported else "lcl_class"
		completion_write_class_definition_header(
			&out,
			class_name,
			"DEFINITION FINAL CREATE PUBLIC.",
		)
		completion_write_class_public_section(&out, base_indent, "$0" if snippets_supported else "")
		completion_write_class_implementation(&out, class_name, base_indent)
	case .Abstract:
		class_name := "${1:lcl_class}" if snippets_supported else "lcl_class"
		completion_write_class_definition_header(&out, class_name, "DEFINITION ABSTRACT.")
		completion_write_class_public_section(&out, base_indent, "$0" if snippets_supported else "")
		completion_write_class_implementation(&out, class_name, base_indent)
	case .For_Testing:
		class_name := "${1:ltc_class}" if snippets_supported else "ltc_class"
		method_name := "${2:test_method}" if snippets_supported else "test_method"
		completion_write_class_definition_header(
			&out,
			class_name,
			"DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.",
		)
		completion_template_write_newline_indent(&out, base_indent, 1, "PRIVATE SECTION.")
		completion_template_write_newline_indent(
			&out,
			base_indent,
			2,
			strings.concatenate(
				{"METHODS ", method_name, " FOR TESTING."},
				context.temp_allocator,
			),
		)
		completion_template_write_newline_indent(&out, base_indent, 0, "ENDCLASS.")
		completion_template_write_newline_indent(&out, base_indent, 0, "")
		completion_template_write_newline_indent(
			&out,
			base_indent,
			0,
			strings.concatenate(
				{"CLASS ", class_name, " IMPLEMENTATION."},
				context.temp_allocator,
			),
		)
		completion_template_write_newline_indent(
			&out,
			base_indent,
			1,
			strings.concatenate({"METHOD ", method_name, "."}, context.temp_allocator),
		)
		completion_template_write_newline_indent(
			&out,
			base_indent,
			2,
			"$0" if snippets_supported else "",
		)
		completion_template_write_newline_indent(&out, base_indent, 1, "ENDMETHOD.")
		completion_template_write_newline_indent(&out, base_indent, 0, "ENDCLASS.")
	}
	return strings.to_string(out)
}

completion_write_class_definition_header :: proc(
	out: ^strings.Builder,
	class_name: string,
	addition: string,
) {
	strings.write_string(out, "CLASS ")
	strings.write_string(out, class_name)
	strings.write_byte(out, ' ')
	strings.write_string(out, addition)
}

completion_write_class_public_section :: proc(
	out: ^strings.Builder,
	indent: string,
	body_text: string,
) {
	completion_template_write_newline_indent(out, indent, 1, "PUBLIC SECTION.")
	completion_template_write_newline_indent(out, indent, 2, body_text)
	completion_template_write_newline_indent(out, indent, 0, "ENDCLASS.")
}

completion_write_class_implementation :: proc(
	out: ^strings.Builder,
	class_name: string,
	indent: string,
) {
	completion_template_write_newline_indent(out, indent, 0, "")
	completion_template_write_newline_indent(
		out,
		indent,
		0,
		strings.concatenate({"CLASS ", class_name, " IMPLEMENTATION."}, context.temp_allocator),
	)
	completion_template_write_newline_indent(out, indent, 0, "ENDCLASS.")
}

completion_interface_template_item :: proc(
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	label := "INTERFACE ... ENDINTERFACE"
	return Completion_Item {
		label = label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", label, allocator),
		insert_text = completion_interface_template_insert_text(
			indent,
			snippets_supported,
			allocator,
		),
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
}

completion_interface_template_insert_text :: proc(
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> string {
	interface_name := "${1:lif_interface}" if snippets_supported else "lif_interface"
	base_indent := completion_template_base_indent(indent, snippets_supported)
	out := strings.builder_make(allocator)
	strings.write_string(&out, "INTERFACE ")
	strings.write_string(&out, interface_name)
	strings.write_string(&out, ".")
	completion_template_write_newline_indent(
		&out,
		base_indent,
		1,
		"$0" if snippets_supported else "",
	)
	completion_template_write_newline_indent(&out, base_indent, 0, "ENDINTERFACE.")
	return strings.to_string(out)
}

completion_append_select_templates :: proc(
	out: []Completion_Item,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) {
	assert(len(out) == SELECT_TEMPLATE_COUNT)
	out[0] = completion_select_template_item(
		"SELECT ... WHERE",
		.Basic_Where,
		indent,
		snippets_supported,
		allocator,
	)
	out[1] = completion_select_template_item(
		"SELECT SINGLE ... WHERE",
		.Single,
		indent,
		snippets_supported,
		allocator,
	)
	out[2] = completion_select_template_item(
		"SELECT ... UP TO ... OFFSET",
		.Limit_Offset,
		indent,
		snippets_supported,
		allocator,
	)
	out[3] = completion_select_template_item(
		"SELECT ... FOR ALL ENTRIES",
		.For_All_Entries,
		indent,
		snippets_supported,
		allocator,
	)
	out[4] = completion_select_template_item(
		"SELECT ... JOIN",
		.Join,
		indent,
		snippets_supported,
		allocator,
	)
	out[5] = completion_select_template_item(
		"SELECT ... PACKAGE SIZE",
		.Package_Size,
		indent,
		snippets_supported,
		allocator,
	)
	out[6] = completion_select_template_item(
		"SELECT ... CURSOR PACKAGE",
		.Cursor,
		indent,
		snippets_supported,
		allocator,
	)
}

completion_select_template_item :: proc(
	label: string,
	variant: Completion_Select_Template,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	return Completion_Item {
		label = label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", label, allocator),
		insert_text = completion_select_template_insert_text(
			variant,
			indent,
			snippets_supported,
			allocator,
		),
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
}

completion_select_template_insert_text :: proc(
	variant: Completion_Select_Template,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	switch variant {
	case .Basic_Where:
		if snippets_supported {
			strings.write_string(&out, "SELECT ${1:fields}")
			completion_template_write_newline_indent(&out, indent, 1, "FROM ${2:table}")
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"INTO TABLE @DATA(${3:lt_rows})",
			)
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"WHERE ${4:field} = @${5:lv_value}.$0",
			)
		} else {
			strings.write_string(&out, "SELECT fields")
			completion_template_write_newline_indent(&out, indent, 1, "FROM table")
			completion_template_write_newline_indent(&out, indent, 1, "INTO TABLE @DATA(lt_rows)")
			completion_template_write_newline_indent(&out, indent, 1, "WHERE field = @lv_value.")
		}
	case .Single:
		if snippets_supported {
			strings.write_string(&out, "SELECT SINGLE ${1:field}")
			completion_template_write_newline_indent(&out, indent, 1, "FROM ${2:table}")
			completion_template_write_newline_indent(&out, indent, 1, "INTO @DATA(${3:lv_value})")
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"WHERE ${4:key_field} = @${5:lv_key}.$0",
			)
		} else {
			strings.write_string(&out, "SELECT SINGLE field")
			completion_template_write_newline_indent(&out, indent, 1, "FROM table")
			completion_template_write_newline_indent(&out, indent, 1, "INTO @DATA(lv_value)")
			completion_template_write_newline_indent(&out, indent, 1, "WHERE key_field = @lv_key.")
		}
	case .Limit_Offset:
		if snippets_supported {
			strings.write_string(&out, "SELECT ${1:fields}")
			completion_template_write_newline_indent(&out, indent, 1, "FROM ${2:table}")
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"INTO TABLE @DATA(${3:lt_rows})",
			)
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"WHERE ${4:field} = @${5:lv_value}",
			)
			completion_template_write_newline_indent(&out, indent, 1, "ORDER BY ${6:field}")
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"UP TO @${7:lv_page_size} ROWS",
			)
			completion_template_write_newline_indent(&out, indent, 1, "OFFSET @${8:lv_offset}.$0")
		} else {
			strings.write_string(&out, "SELECT fields")
			completion_template_write_newline_indent(&out, indent, 1, "FROM table")
			completion_template_write_newline_indent(&out, indent, 1, "INTO TABLE @DATA(lt_rows)")
			completion_template_write_newline_indent(&out, indent, 1, "WHERE field = @lv_value")
			completion_template_write_newline_indent(&out, indent, 1, "ORDER BY field")
			completion_template_write_newline_indent(&out, indent, 1, "UP TO @lv_page_size ROWS")
			completion_template_write_newline_indent(&out, indent, 1, "OFFSET @lv_offset.")
		}
	case .For_All_Entries:
		if snippets_supported {
			strings.write_string(&out, "IF ${1:lt_keys} IS NOT INITIAL.")
			completion_template_write_newline_indent(&out, indent, 1, "SELECT ${2:fields}")
			completion_template_write_newline_indent(&out, indent, 2, "FROM ${3:table}")
			completion_template_write_newline_indent(
				&out,
				indent,
				2,
				"INTO TABLE @DATA(${4:lt_rows})",
			)
			completion_template_write_newline_indent(
				&out,
				indent,
				2,
				"FOR ALL ENTRIES IN @${1:lt_keys}",
			)
			completion_template_write_newline_indent(
				&out,
				indent,
				2,
				"WHERE ${5:key_field} = @${1:lt_keys}-${6:key_field}.",
			)
			completion_template_write_newline_indent(&out, indent, 1, "$0")
			completion_template_write_newline_indent(&out, indent, 0, "ENDIF.")
		} else {
			strings.write_string(&out, "IF lt_keys IS NOT INITIAL.")
			completion_template_write_newline_indent(&out, indent, 1, "SELECT fields")
			completion_template_write_newline_indent(&out, indent, 2, "FROM table")
			completion_template_write_newline_indent(&out, indent, 2, "INTO TABLE @DATA(lt_rows)")
			completion_template_write_newline_indent(
				&out,
				indent,
				2,
				"FOR ALL ENTRIES IN @lt_keys",
			)
			completion_template_write_newline_indent(
				&out,
				indent,
				2,
				"WHERE key_field = @lt_keys-key_field.",
			)
			completion_template_write_newline_indent(&out, indent, 1, "")
			completion_template_write_newline_indent(&out, indent, 0, "ENDIF.")
		}
	case .Join:
		if snippets_supported {
			strings.write_string(&out, "SELECT ${1:a~field}, ${2:b~field}")
			completion_template_write_newline_indent(&out, indent, 1, "FROM ${3:table_a} AS a")
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"INNER JOIN ${4:table_b} AS b ON b~${5:key} = a~${6:key}",
			)
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"INTO TABLE @DATA(${7:lt_rows})",
			)
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"WHERE a~${8:field} = @${9:lv_value}.$0",
			)
		} else {
			strings.write_string(&out, "SELECT a~field, b~field")
			completion_template_write_newline_indent(&out, indent, 1, "FROM table_a AS a")
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"INNER JOIN table_b AS b ON b~key = a~key",
			)
			completion_template_write_newline_indent(&out, indent, 1, "INTO TABLE @DATA(lt_rows)")
			completion_template_write_newline_indent(&out, indent, 1, "WHERE a~field = @lv_value.")
		}
	case .Package_Size:
		if snippets_supported {
			strings.write_string(&out, "SELECT ${1:fields}")
			completion_template_write_newline_indent(&out, indent, 1, "FROM ${2:table}")
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"INTO TABLE @DATA(${3:lt_package})",
			)
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"WHERE ${4:field} = @${5:lv_value}",
			)
			completion_template_write_newline_indent(&out, indent, 1, "PACKAGE SIZE ${6:100}.$0")
		} else {
			strings.write_string(&out, "SELECT fields")
			completion_template_write_newline_indent(&out, indent, 1, "FROM table")
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"INTO TABLE @DATA(lt_package)",
			)
			completion_template_write_newline_indent(&out, indent, 1, "WHERE field = @lv_value")
			completion_template_write_newline_indent(&out, indent, 1, "PACKAGE SIZE 100.")
		}
	case .Cursor:
		if snippets_supported {
			strings.write_string(&out, "OPEN CURSOR WITH HOLD @${1:lv_cursor} FOR")
			completion_template_write_newline_indent(&out, indent, 1, "SELECT ${2:fields}")
			completion_template_write_newline_indent(&out, indent, 2, "FROM ${3:table}")
			completion_template_write_newline_indent(
				&out,
				indent,
				2,
				"WHERE ${4:field} = @${5:lv_value}.",
			)
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 0, "DO.")
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"FETCH NEXT CURSOR @${1:lv_cursor}",
			)
			completion_template_write_newline_indent(
				&out,
				indent,
				2,
				"INTO TABLE @DATA(${6:lt_package})",
			)
			completion_template_write_newline_indent(&out, indent, 2, "PACKAGE SIZE ${7:100}.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 1, "IF sy-subrc <> 0.")
			completion_template_write_newline_indent(&out, indent, 2, "EXIT.")
			completion_template_write_newline_indent(&out, indent, 1, "ENDIF.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 1, "$0")
			completion_template_write_newline_indent(&out, indent, 0, "ENDDO.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(
				&out,
				indent,
				0,
				"CLOSE CURSOR @${1:lv_cursor}.",
			)
		} else {
			strings.write_string(&out, "OPEN CURSOR WITH HOLD @lv_cursor FOR")
			completion_template_write_newline_indent(&out, indent, 1, "SELECT fields")
			completion_template_write_newline_indent(&out, indent, 2, "FROM table")
			completion_template_write_newline_indent(&out, indent, 2, "WHERE field = @lv_value.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 0, "DO.")
			completion_template_write_newline_indent(
				&out,
				indent,
				1,
				"FETCH NEXT CURSOR @lv_cursor",
			)
			completion_template_write_newline_indent(
				&out,
				indent,
				2,
				"INTO TABLE @DATA(lt_package)",
			)
			completion_template_write_newline_indent(&out, indent, 2, "PACKAGE SIZE 100.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 1, "IF sy-subrc <> 0.")
			completion_template_write_newline_indent(&out, indent, 2, "EXIT.")
			completion_template_write_newline_indent(&out, indent, 1, "ENDIF.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 1, "")
			completion_template_write_newline_indent(&out, indent, 0, "ENDDO.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 0, "CLOSE CURSOR @lv_cursor.")
		}
	}
	return strings.to_string(out)
}

completion_template_write_newline_indent :: proc(
	out: ^strings.Builder,
	indent: string,
	extra_indent: int,
	text: string,
) {
	strings.write_byte(out, '\n')
	strings.write_string(out, indent)
	for _ in 0 ..< extra_indent {
		strings.write_string(out, "  ")
	}
	strings.write_string(out, text)
}

completion_try_template_item :: proc(
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	label := "TRY ... CATCH ... ENDTRY"
	return Completion_Item {
		label = label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", label, allocator),
		insert_text = completion_try_template_insert_text(indent, snippets_supported, allocator),
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
}

completion_try_template_insert_text :: proc(
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "TRY.")
	completion_template_write_newline_indent(&out, indent, 1, "${1}" if snippets_supported else "")
	completion_template_write_newline_indent(
		&out,
		indent,
		0,
		"CATCH ${2:cx_root} INTO DATA(${3:lx_error})." if snippets_supported else "CATCH cx_root INTO DATA(lx_error).",
	)
	completion_template_write_newline_indent(&out, indent, 1, "$0" if snippets_supported else "")
	completion_template_write_newline_indent(&out, indent, 0, "ENDTRY.")
	return strings.to_string(out)
}

completion_append_commit_templates :: proc(
	out: []Completion_Item,
	snippets_supported: bool,
	allocator: mem.Allocator,
) {
	assert(len(out) == COMMIT_TEMPLATE_COUNT)
	out[0] = completion_commit_template_item("COMMIT WORK", .Work, snippets_supported, allocator)
	out[1] = completion_commit_template_item(
		"COMMIT WORK AND WAIT",
		.Work_And_Wait,
		snippets_supported,
		allocator,
	)
}

completion_commit_template_item :: proc(
	label: string,
	variant: Completion_Commit_Template,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	return Completion_Item {
		label = label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", label, allocator),
		insert_text = completion_commit_template_insert_text(
			variant,
			snippets_supported,
			allocator,
		),
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
}

completion_commit_template_insert_text :: proc(
	variant: Completion_Commit_Template,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> string {
	text: string
	switch variant {
	case .Work:
		text = "COMMIT WORK.$0" if snippets_supported else "COMMIT WORK."
	case .Work_And_Wait:
		text = "COMMIT WORK AND WAIT.$0" if snippets_supported else "COMMIT WORK AND WAIT."
	}
	return strings.clone(text, allocator)
}

completion_continue_template_item :: proc(
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	label := "CONTINUE"
	insert_text := "CONTINUE.$0" if snippets_supported else "CONTINUE."
	return Completion_Item {
		label = label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", label, allocator),
		insert_text = strings.clone(insert_text, allocator),
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
}

completion_append_read_table_templates :: proc(
	out: []Completion_Item,
	snippets_supported: bool,
	allocator: mem.Allocator,
) {
	assert(len(out) == READ_TABLE_TEMPLATE_COUNT)
	out[0] = completion_read_table_template_item(
		"READ TABLE ... INDEX ... INTO",
		.Index_Into,
		snippets_supported,
		allocator,
	)
	out[1] = completion_read_table_template_item(
		"READ TABLE ... INDEX ... ASSIGNING",
		.Index_Assigning,
		snippets_supported,
		allocator,
	)
	out[2] = completion_read_table_template_item(
		"READ TABLE ... INDEX ... USING KEY ... INTO",
		.Index_Using_Key_Into,
		snippets_supported,
		allocator,
	)
	out[3] = completion_read_table_template_item(
		"READ TABLE ... WITH KEY ... INTO",
		.Key_Into,
		snippets_supported,
		allocator,
	)
	out[4] = completion_read_table_template_item(
		"READ TABLE ... WITH KEY ... ASSIGNING",
		.Key_Assigning,
		snippets_supported,
		allocator,
	)
	out[5] = completion_read_table_template_item(
		"READ TABLE ... WITH KEY ... REFERENCE INTO",
		.Key_Reference_Into,
		snippets_supported,
		allocator,
	)
	out[6] = completion_read_table_template_item(
		"READ TABLE ... WITH KEY ... TRANSPORTING NO FIELDS",
		.Key_Transporting_No_Fields,
		snippets_supported,
		allocator,
	)
	out[7] = completion_read_table_template_item(
		"READ TABLE ... WITH KEY ... BINARY SEARCH",
		.Key_Binary_Search_Into,
		snippets_supported,
		allocator,
	)
	out[8] = completion_read_table_template_item(
		"READ TABLE ... WITH TABLE KEY ... COMPONENTS ... INTO",
		.Table_Key_Components_Into,
		snippets_supported,
		allocator,
	)
	out[9] = completion_read_table_template_item(
		"READ TABLE ... WITH TABLE KEY ... COMPONENTS ... ASSIGNING",
		.Table_Key_Components_Assigning,
		snippets_supported,
		allocator,
	)
	out[10] = completion_read_table_template_item(
		"READ TABLE ... WITH TABLE KEY ... COMPONENTS ... TRANSPORTING NO FIELDS",
		.Table_Key_Components_Transporting_No_Fields,
		snippets_supported,
		allocator,
	)
}

completion_read_table_template_item :: proc(
	label: string,
	variant: Completion_Read_Table_Template,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	return Completion_Item {
		label = label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", label, allocator),
		insert_text = completion_read_table_template_insert_text(
			variant,
			snippets_supported,
			allocator,
		),
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
}

completion_read_table_template_insert_text :: proc(
	variant: Completion_Read_Table_Template,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> string {
	text: string
	switch variant {
	case .Index_Into:
		text =
			"READ TABLE ${1:itab} INDEX ${2:lv_index} INTO DATA(${3:ls_row}).$0" if snippets_supported else "READ TABLE itab INDEX lv_index INTO DATA(ls_row)."
	case .Index_Assigning:
		text =
			"READ TABLE ${1:itab} INDEX ${2:lv_index} ASSIGNING FIELD-SYMBOL(<${3:ls_row}>).$0" if snippets_supported else "READ TABLE itab INDEX lv_index ASSIGNING FIELD-SYMBOL(<ls_row>)."
	case .Index_Using_Key_Into:
		text =
			"READ TABLE ${1:itab} INDEX ${2:lv_index} USING KEY ${3:key_name} INTO DATA(${4:ls_row}).$0" if snippets_supported else "READ TABLE itab INDEX lv_index USING KEY key_name INTO DATA(ls_row)."
	case .Key_Into:
		text =
			"READ TABLE ${1:itab} WITH KEY ${2:id} = ${3:lv_id} INTO DATA(${4:ls_row}).$0" if snippets_supported else "READ TABLE itab WITH KEY id = lv_id INTO DATA(ls_row)."
	case .Key_Assigning:
		text =
			"READ TABLE ${1:itab} WITH KEY ${2:id} = ${3:lv_id} ASSIGNING FIELD-SYMBOL(<${4:ls_row}>).$0" if snippets_supported else "READ TABLE itab WITH KEY id = lv_id ASSIGNING FIELD-SYMBOL(<ls_row>)."
	case .Key_Reference_Into:
		text =
			"READ TABLE ${1:itab} WITH KEY ${2:id} = ${3:lv_id} REFERENCE INTO DATA(${4:lr_row}).$0" if snippets_supported else "READ TABLE itab WITH KEY id = lv_id REFERENCE INTO DATA(lr_row)."
	case .Key_Transporting_No_Fields:
		text =
			"READ TABLE ${1:itab} WITH KEY ${2:id} = ${3:lv_id} TRANSPORTING NO FIELDS.$0" if snippets_supported else "READ TABLE itab WITH KEY id = lv_id TRANSPORTING NO FIELDS."
	case .Key_Binary_Search_Into:
		text =
			"READ TABLE ${1:itab} WITH KEY ${2:id} = ${3:lv_id} BINARY SEARCH INTO DATA(${4:ls_row}).$0" if snippets_supported else "READ TABLE itab WITH KEY id = lv_id BINARY SEARCH INTO DATA(ls_row)."
	case .Table_Key_Components_Into:
		text =
			"READ TABLE ${1:itab} WITH TABLE KEY ${2:key_name} COMPONENTS ${3:id} = ${4:lv_id} INTO DATA(${5:ls_row}).$0" if snippets_supported else "READ TABLE itab WITH TABLE KEY key_name COMPONENTS id = lv_id INTO DATA(ls_row)."
	case .Table_Key_Components_Assigning:
		text =
			"READ TABLE ${1:itab} WITH TABLE KEY ${2:key_name} COMPONENTS ${3:id} = ${4:lv_id} ASSIGNING FIELD-SYMBOL(<${5:ls_row}>).$0" if snippets_supported else "READ TABLE itab WITH TABLE KEY key_name COMPONENTS id = lv_id ASSIGNING FIELD-SYMBOL(<ls_row>)."
	case .Table_Key_Components_Transporting_No_Fields:
		text =
			"READ TABLE ${1:itab} WITH TABLE KEY ${2:key_name} COMPONENTS ${3:id} = ${4:lv_id} TRANSPORTING NO FIELDS.$0" if snippets_supported else "READ TABLE itab WITH TABLE KEY key_name COMPONENTS id = lv_id TRANSPORTING NO FIELDS."
	}
	return strings.clone(text, allocator)
}

completion_get_time_stamp_field_template_item :: proc(
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	label := "GET TIME STAMP FIELD"
	insert_text: string
	if snippets_supported {
		insert_text = strings.clone("GET TIME STAMP FIELD ${1:lv_timestamp}.$0", allocator)
	} else {
		insert_text = strings.clone("GET TIME STAMP FIELD lv_timestamp.", allocator)
	}
	return Completion_Item {
		label = label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", label, allocator),
		insert_text = insert_text,
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
}

completion_append_loop_templates :: proc(
	out: []Completion_Item,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) {
	assert(len(out) == 2)
	out[0] = completion_loop_template_item(
		"LOOP AT ... ASSIGNING",
		"assigning",
		indent,
		snippets_supported,
		allocator,
	)
	out[1] = completion_loop_template_item(
		"LOOP AT ... INTO",
		"into",
		indent,
		snippets_supported,
		allocator,
	)
}

completion_loop_template_item :: proc(
	label: string,
	variant: string,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	return Completion_Item {
		label = label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", label, allocator),
		insert_text = completion_loop_template_insert_text(
			variant,
			indent,
			snippets_supported,
			allocator,
		),
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
}

completion_loop_template_insert_text :: proc(
	variant: string,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	if variant == "assigning" {
		if snippets_supported {
			strings.write_string(&out, "LOOP AT ${1:itab} ASSIGNING FIELD-SYMBOL(<${2:row}>).")
		} else {
			strings.write_string(&out, "LOOP AT itab ASSIGNING FIELD-SYMBOL(<row>).")
		}
	} else {
		assert(variant == "into")
		if snippets_supported {
			strings.write_string(&out, "LOOP AT ${1:itab} INTO DATA(${2:row}).")
		} else {
			strings.write_string(&out, "LOOP AT itab INTO DATA(row).")
		}
	}
	strings.write_byte(&out, '\n')
	strings.write_string(&out, indent)
	strings.write_string(&out, "  ")
	if snippets_supported {
		strings.write_string(&out, "$0")
	}
	strings.write_byte(&out, '\n')
	strings.write_string(&out, indent)
	strings.write_string(&out, "ENDLOOP.")
	return strings.to_string(out)
}

completion_prefix :: proc(source: string, offset: int, allocator: mem.Allocator) -> string {
	end := clamp(offset, 0, len(source))
	start := completion_prefix_start(source, offset)
	if start == end {
		return ""
	}
	return strings.clone(source[start:end], allocator)
}

completion_template_prefix :: proc(
	source: string,
	offset: int,
	allocator: mem.Allocator,
) -> string {
	end := clamp(offset, 0, len(source))
	start := completion_template_prefix_start(source, offset)
	if start == end {
		return ""
	}
	return strings.clone(source[start:end], allocator)
}

completion_template_replace_range :: proc(source: string, offset: int) -> Range {
	end := clamp(offset, 0, len(source))
	start := completion_template_prefix_start(source, offset)
	return range_from_offsets(source, start, end)
}

completion_prefix_start :: proc(source: string, offset: int) -> int {
	start := clamp(offset, 0, len(source))
	for start > 0 && completion_prefix_char(source[start - 1]) {
		start -= 1
	}
	return start
}

completion_selector_filter_prefix :: proc(
	source: string,
	start: int,
	offset: int,
	allocator: mem.Allocator,
) -> string {
	end := clamp(offset, 0, len(source))
	prefix_start := clamp(start, 0, end)
	if prefix_start == end {
		return ""
	}
	return strings.clone(source[prefix_start:end], allocator)
}

completion_selector_filter_prefix_start :: proc(source: string, offset: int) -> int {
	start := clamp(offset, 0, len(source))
	for start > 0 && completion_selector_filter_prefix_char(source[start - 1]) {
		start -= 1
	}
	return start
}

completion_template_prefix_start :: proc(source: string, offset: int) -> int {
	start := clamp(offset, 0, len(source))
	for start > 0 && completion_template_prefix_char(source[start - 1]) {
		start -= 1
	}
	return start
}

completion_prefix_char :: proc "contextless" (ch: u8) -> bool {
	return(
		('a' <= ch && ch <= 'z') ||
		('A' <= ch && ch <= 'Z') ||
		('0' <= ch && ch <= '9') ||
		ch == '_' ||
		ch == '/' \
	)
}

completion_template_prefix_char :: proc "contextless" (ch: u8) -> bool {
	return completion_prefix_char(ch) || ch == '-'
}

completion_selector_filter_prefix_char :: proc "contextless" (ch: u8) -> bool {
	return completion_prefix_char(ch) || ch == '-' || ch == '>' || ch == '=' || ch == '~'
}

completion_line_indent :: proc(source: string, offset: int, allocator: mem.Allocator) -> string {
	line_start := clamp(offset, 0, len(source))
	for line_start > 0 && source[line_start - 1] != '\n' {
		line_start -= 1
	}
	indent_end := line_start
	for indent_end < len(source) && (source[indent_end] == ' ' || source[indent_end] == '\t') {
		indent_end += 1
	}
	return strings.clone(source[line_start:indent_end], allocator)
}

completion_method_call_snippet :: proc(
	project: ^semantic.Project,
	method: ^semantic.Entity,
	name: string,
	indent: string,
	allocator: mem.Allocator,
) -> string {
	payload, ok := method.payload.(^semantic.Entity_Routine_Payload)
	assert(ok && payload != nil)

	out := strings.builder_make(allocator)
	completion_snippet_write_text(&out, name)
	if !completion_method_has_call_parameters(payload) {
		strings.write_string(&out, "( )$0")
		return strings.to_string(out)
	}

	strings.write_string(&out, "(\n")
	tabstop := 1
	if completion_method_only_call_exporting(payload) {
		completion_write_method_call_section(
			&out,
			project,
			payload.parameters[:],
			.Method_Importing,
			"",
			indent,
			&tabstop,
		)
	} else {
		completion_write_method_call_section(
			&out,
			project,
			payload.parameters[:],
			.Method_Importing,
			"EXPORTING",
			indent,
			&tabstop,
		)
		completion_write_method_call_section(
			&out,
			project,
			payload.parameters[:],
			.Method_Exporting,
			"IMPORTING",
			indent,
			&tabstop,
		)
		completion_write_method_call_section(
			&out,
			project,
			payload.parameters[:],
			.Method_Changing,
			"CHANGING",
			indent,
			&tabstop,
		)
	}
	strings.write_string(&out, indent)
	strings.write_string(&out, ")$0")
	return strings.to_string(out)
}

completion_method_has_call_parameters :: proc(payload: ^semantic.Entity_Routine_Payload) -> bool {
	assert(payload != nil)
	for param in payload.parameters {
		#partial switch completion_parameter_section(param) {
		case .Method_Importing, .Method_Exporting, .Method_Changing:
			return true
		case:
		}
	}
	return false
}

completion_method_only_call_exporting :: proc(payload: ^semantic.Entity_Routine_Payload) -> bool {
	assert(payload != nil)
	has_exporting := false
	for param in payload.parameters {
		#partial switch completion_parameter_section(param) {
		case .Method_Importing:
			has_exporting = true
		case .Method_Exporting, .Method_Changing:
			return false
		case:
		}
	}
	return has_exporting
}

completion_write_method_call_section :: proc(
	out: ^strings.Builder,
	project: ^semantic.Project,
	parameters: []^semantic.Entity,
	section: semantic.Entity_Parameter_Section,
	heading: string,
	indent: string,
	tabstop: ^int,
) -> bool {
	wrote := false
	for param in parameters {
		if completion_parameter_section(param) != section {
			continue
		}
		if !wrote {
			if heading != "" {
				strings.write_string(out, indent)
				strings.write_string(out, "  ")
				strings.write_string(out, heading)
				strings.write_byte(out, '\n')
			}
			wrote = true
		}
		strings.write_string(out, indent)
		strings.write_string(out, "  " if heading == "" else "    ")
		completion_snippet_write_text(out, param.name)
		strings.write_string(out, " = ")
		strings.write_string(out, fmt.tprintf("$%d", tabstop^))
		strings.write_byte(out, '\n')
		tabstop^ += 1
	}
	return wrote
}

completion_parameter_section :: proc(
	param: ^semantic.Entity,
) -> semantic.Entity_Parameter_Section {
	assert(param != nil)
	payload, ok := param.payload.(^semantic.Entity_Variable_Payload)
	assert(ok && payload != nil)
	return payload.section
}

completion_snippet_write_text :: proc(out: ^strings.Builder, text: string) {
	for i in 0 ..< len(text) {
		ch := text[i]
		if ch == '$' || ch == '}' || ch == '\\' {
			strings.write_byte(out, '\\')
		}
		strings.write_byte(out, ch)
	}
}

completion_kind :: proc "contextless" (entity: ^semantic.Entity) -> int {
	if entity == nil {
		return COMPLETION_VARIABLE
	}
	#partial switch entity.kind {
	case .Class:
		return COMPLETION_CLASS
	case .Interface:
		return COMPLETION_INTERFACE
	case .Type_Def:
		return COMPLETION_STRUCT
	case .Field:
		return COMPLETION_FIELD
	case .Form:
		return COMPLETION_FUNCTION
	case .Module:
		return COMPLETION_MODULE
	case .Method:
		return COMPLETION_METHOD
	case .Event:
		return COMPLETION_EVENT
	case .Constant:
		return COMPLETION_CONSTANT
	case .Enum_Member:
		return COMPLETION_ENUM_MEMBER
	case:
	}
	return COMPLETION_VARIABLE
}
