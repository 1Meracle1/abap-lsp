package abap_frontend_lsp

import "src:semantic"
import string_interner "src:string_interner"

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
		ctx.state.allocator,
	)
	send_success(
		ctx.output,
		ctx.id,
		Completion_List{is_incomplete = false, items = out},
		ctx.state.allocator,
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
	indent := completion_line_indent(snapshot.source, offset, context.temp_allocator)
	template_replace_range := completion_template_replace_range(snapshot.source, offset)
	if_template_count := completion_if_template_count(snapshot.source, offset, template_prefix)
	class_template_count := completion_class_template_count(snapshot.source, offset, template_prefix)
	try_template_count := completion_try_template_count(snapshot.source, offset, template_prefix)
	loop_template_count := completion_loop_template_count(snapshot.source, offset, template_prefix)
	select_template_count := completion_select_template_count(snapshot.source, offset, template_prefix)
	commit_template_count := completion_commit_template_count(snapshot.source, offset, template_prefix)
	continue_template_count := completion_continue_template_count(snapshot.source, offset, template_prefix)
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
	common_statement_template_count := completion_common_statement_template_count(
		snapshot.source,
		offset,
		template_prefix,
	)
	template_count := if_template_count + class_template_count + loop_template_count +
	                  select_template_count + get_time_stamp_template_count +
	                  try_template_count + commit_template_count + continue_template_count +
	                  read_table_template_count + common_statement_template_count
	out := make([]Completion_Item, len(items) + template_count, allocator)
	for item, i in items {
		out[i] = completion_item_from_semantic_item(
			snapshot.project,
			item,
			indent,
			snippets_supported,
			allocator,
		)
	}
	template_index := len(items)
	if if_template_count > 0 {
		completion_append_if_templates(
			out[template_index:template_index + if_template_count],
			indent,
			snippets_supported,
			allocator,
		)
		template_index += if_template_count
	}
	if class_template_count > 0 {
		completion_append_class_templates(
			out[template_index:template_index + class_template_count],
			indent,
			snippets_supported,
			allocator,
		)
		template_index += class_template_count
	}
	if try_template_count > 0 {
		out[template_index] = completion_try_template_item(
			indent,
			snippets_supported,
			allocator,
		)
		template_index += try_template_count
	}
	if loop_template_count > 0 {
		completion_append_loop_templates(
			out[template_index:template_index + loop_template_count],
			indent,
			snippets_supported,
			allocator,
		)
		template_index += loop_template_count
	}
	if select_template_count > 0 {
		completion_append_select_templates(
			out[template_index:template_index + select_template_count],
			indent,
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
	if common_statement_template_count > 0 {
		completion_append_common_statement_templates(
			out[template_index:template_index + common_statement_template_count],
			template_prefix,
			template_replace_range,
			snippets_supported,
			allocator,
		)
	}
	return out
}

completion_item_from_semantic_item :: proc(
	project: ^semantic.Project,
	item: semantic.Semantic_Completion_Item,
	indent: string,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	name := string_interner.load(project.interner, item.name)
	out := Completion_Item {
		label              = name,
		kind               = completion_kind(item.entity),
		sort_text          = completion_sort_text("1", name, allocator),
		insert_text        = name,
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
	}
	if snippets_supported &&
	   item.source == .Selector_Member &&
	   item.entity != nil &&
	   item.entity.kind == .Method {
		out.insert_text = completion_method_call_snippet(
			project,
			item.entity,
			name,
			indent,
			allocator,
		)
		out.insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET
	}
	return out
}

completion_sort_text :: proc(priority, label: string, allocator: mem.Allocator) -> string {
	lower := strings.to_lower(label, context.temp_allocator)
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
	keyword: string,
	label:   string,
	snippet: string,
	plain:   string,
}

COMMON_STATEMENT_TEMPLATES :: [?]Completion_Statement_Template {
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
}

completion_common_statement_template_count :: proc(
	source: string,
	offset: int,
	prefix: string,
) -> int {
	if !completion_template_at_statement_start(source, offset) {
		return 0
	}
	count := 0
	for template in COMMON_STATEMENT_TEMPLATES {
		if completion_keyword_prefix_matches(prefix, template.keyword) {
			count += 1
		}
	}
	return count
}

completion_append_common_statement_templates :: proc(
	out: []Completion_Item,
	prefix: string,
	replace_range: Range,
	snippets_supported: bool,
	allocator: mem.Allocator,
) {
	index := 0
	for template in COMMON_STATEMENT_TEMPLATES {
		if !completion_keyword_prefix_matches(prefix, template.keyword) {
			continue
		}
		assert(index < len(out))
		out[index] = completion_statement_template_item(
			template,
			replace_range,
			snippets_supported,
			allocator,
		)
		index += 1
	}
	assert(index == len(out))
}

completion_statement_template_item :: proc(
	template: Completion_Statement_Template,
	replace_range: Range,
	snippets_supported: bool,
	allocator: mem.Allocator,
) -> Completion_Item {
	insert_text := strings.clone(template.snippet if snippets_supported else template.plain, allocator)
	return Completion_Item {
		label = template.label,
		kind = COMPLETION_SNIPPET,
		sort_text = completion_sort_text("2", template.label, allocator),
		insert_text = insert_text,
		insert_text_format = COMPLETION_INSERT_TEXT_FORMAT_SNIPPET if snippets_supported else COMPLETION_INSERT_TEXT_FORMAT_PLAIN_TEXT,
		text_edit = Text_Edit{range = replace_range, new_text = insert_text},
	}
}

completion_keyword_prefix_matches :: proc(prefix, keyword: string) -> bool {
	if prefix == "" || len(prefix) > len(keyword) {
		return false
	}
	lower := strings.to_lower(prefix, context.temp_allocator)
	keyword_lower := strings.to_lower(keyword, context.temp_allocator)
	return strings.has_prefix(keyword_lower, lower)
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
	out := strings.builder_make(allocator)
	strings.write_string(&out, completion_if_template_header(variant, snippets_supported))
	strings.write_byte(&out, '\n')
	strings.write_string(&out, indent)
	strings.write_string(&out, "  ")
	if snippets_supported {
		strings.write_string(&out, "$0")
	}
	strings.write_byte(&out, '\n')
	strings.write_string(&out, indent)
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
		return "IF ${1:lv_value} IS NOT INITIAL." if snippets_supported else "IF lv_value IS NOT INITIAL."
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
	out := strings.builder_make(allocator)
	switch variant {
	case .Basic:
		class_name := "${1:lcl_class}" if snippets_supported else "lcl_class"
		completion_write_class_definition_header(&out, class_name, "DEFINITION.")
		completion_write_class_public_section(&out, indent, "$0" if snippets_supported else "")
		completion_write_class_implementation(&out, class_name, indent)
	case .Public_Final_Create_Public:
		class_name := "${1:zcl_class}" if snippets_supported else "zcl_class"
		completion_write_class_definition_header(
			&out,
			class_name,
			"DEFINITION PUBLIC FINAL CREATE PUBLIC.",
		)
		completion_write_class_public_section(&out, indent, "$0" if snippets_supported else "")
		completion_write_class_implementation(&out, class_name, indent)
	case .Inheriting_From:
		class_name := "${1:lcl_child}" if snippets_supported else "lcl_child"
		superclass_name := "${2:lcl_parent}" if snippets_supported else "lcl_parent"
		completion_write_class_definition_header(
			&out,
			class_name,
			strings.concatenate({"DEFINITION INHERITING FROM ", superclass_name, "."}, context.temp_allocator),
		)
		completion_write_class_public_section(&out, indent, "$0" if snippets_supported else "")
		completion_write_class_implementation(&out, class_name, indent)
	case .Final_Create_Public:
		class_name := "${1:lcl_class}" if snippets_supported else "lcl_class"
		completion_write_class_definition_header(
			&out,
			class_name,
			"DEFINITION FINAL CREATE PUBLIC.",
		)
		completion_write_class_public_section(&out, indent, "$0" if snippets_supported else "")
		completion_write_class_implementation(&out, class_name, indent)
	case .Abstract:
		class_name := "${1:lcl_class}" if snippets_supported else "lcl_class"
		completion_write_class_definition_header(
			&out,
			class_name,
			"DEFINITION ABSTRACT.",
		)
		completion_write_class_public_section(&out, indent, "$0" if snippets_supported else "")
		completion_write_class_implementation(&out, class_name, indent)
	case .For_Testing:
		class_name := "${1:ltc_class}" if snippets_supported else "ltc_class"
		method_name := "${2:test_method}" if snippets_supported else "test_method"
		completion_write_class_definition_header(
			&out,
			class_name,
			"DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.",
		)
		completion_template_write_newline_indent(&out, indent, 1, "PRIVATE SECTION.")
		completion_template_write_newline_indent(
			&out,
			indent,
			2,
			strings.concatenate({"METHODS ", method_name, " FOR TESTING."}, context.temp_allocator),
		)
		completion_template_write_newline_indent(&out, indent, 0, "ENDCLASS.")
		completion_template_write_newline_indent(&out, indent, 0, "")
		completion_template_write_newline_indent(
			&out,
			indent,
			0,
			strings.concatenate({"CLASS ", class_name, " IMPLEMENTATION."}, context.temp_allocator),
		)
		completion_template_write_newline_indent(
			&out,
			indent,
			1,
			strings.concatenate({"METHOD ", method_name, "."}, context.temp_allocator),
		)
		completion_template_write_newline_indent(&out, indent, 2, "$0" if snippets_supported else "")
		completion_template_write_newline_indent(&out, indent, 1, "ENDMETHOD.")
		completion_template_write_newline_indent(&out, indent, 0, "ENDCLASS.")
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
			completion_template_write_newline_indent(&out, indent, 1, "INTO TABLE @DATA(${3:lt_rows})")
			completion_template_write_newline_indent(&out, indent, 1, "WHERE ${4:field} = @${5:lv_value}.$0")
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
			completion_template_write_newline_indent(&out, indent, 1, "WHERE ${4:key_field} = @${5:lv_key}.$0")
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
			completion_template_write_newline_indent(&out, indent, 1, "INTO TABLE @DATA(${3:lt_rows})")
			completion_template_write_newline_indent(&out, indent, 1, "WHERE ${4:field} = @${5:lv_value}")
			completion_template_write_newline_indent(&out, indent, 1, "ORDER BY ${6:field}")
			completion_template_write_newline_indent(&out, indent, 1, "UP TO @${7:lv_page_size} ROWS")
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
			completion_template_write_newline_indent(&out, indent, 2, "INTO TABLE @DATA(${4:lt_rows})")
			completion_template_write_newline_indent(&out, indent, 2, "FOR ALL ENTRIES IN @${1:lt_keys}")
			completion_template_write_newline_indent(&out, indent, 2, "WHERE ${5:key_field} = @${1:lt_keys}-${6:key_field}.")
			completion_template_write_newline_indent(&out, indent, 1, "$0")
			completion_template_write_newline_indent(&out, indent, 0, "ENDIF.")
		} else {
			strings.write_string(&out, "IF lt_keys IS NOT INITIAL.")
			completion_template_write_newline_indent(&out, indent, 1, "SELECT fields")
			completion_template_write_newline_indent(&out, indent, 2, "FROM table")
			completion_template_write_newline_indent(&out, indent, 2, "INTO TABLE @DATA(lt_rows)")
			completion_template_write_newline_indent(&out, indent, 2, "FOR ALL ENTRIES IN @lt_keys")
			completion_template_write_newline_indent(&out, indent, 2, "WHERE key_field = @lt_keys-key_field.")
			completion_template_write_newline_indent(&out, indent, 1, "")
			completion_template_write_newline_indent(&out, indent, 0, "ENDIF.")
		}
	case .Join:
		if snippets_supported {
			strings.write_string(&out, "SELECT ${1:a~field}, ${2:b~field}")
			completion_template_write_newline_indent(&out, indent, 1, "FROM ${3:table_a} AS a")
			completion_template_write_newline_indent(&out, indent, 1, "INNER JOIN ${4:table_b} AS b ON b~${5:key} = a~${6:key}")
			completion_template_write_newline_indent(&out, indent, 1, "INTO TABLE @DATA(${7:lt_rows})")
			completion_template_write_newline_indent(&out, indent, 1, "WHERE a~${8:field} = @${9:lv_value}.$0")
		} else {
			strings.write_string(&out, "SELECT a~field, b~field")
			completion_template_write_newline_indent(&out, indent, 1, "FROM table_a AS a")
			completion_template_write_newline_indent(&out, indent, 1, "INNER JOIN table_b AS b ON b~key = a~key")
			completion_template_write_newline_indent(&out, indent, 1, "INTO TABLE @DATA(lt_rows)")
			completion_template_write_newline_indent(&out, indent, 1, "WHERE a~field = @lv_value.")
		}
	case .Package_Size:
		if snippets_supported {
			strings.write_string(&out, "SELECT ${1:fields}")
			completion_template_write_newline_indent(&out, indent, 1, "FROM ${2:table}")
			completion_template_write_newline_indent(&out, indent, 1, "INTO TABLE @DATA(${3:lt_package})")
			completion_template_write_newline_indent(&out, indent, 1, "WHERE ${4:field} = @${5:lv_value}")
			completion_template_write_newline_indent(&out, indent, 1, "PACKAGE SIZE ${6:100}.$0")
		} else {
			strings.write_string(&out, "SELECT fields")
			completion_template_write_newline_indent(&out, indent, 1, "FROM table")
			completion_template_write_newline_indent(&out, indent, 1, "INTO TABLE @DATA(lt_package)")
			completion_template_write_newline_indent(&out, indent, 1, "WHERE field = @lv_value")
			completion_template_write_newline_indent(&out, indent, 1, "PACKAGE SIZE 100.")
		}
	case .Cursor:
		if snippets_supported {
			strings.write_string(&out, "OPEN CURSOR WITH HOLD @${1:lv_cursor} FOR")
			completion_template_write_newline_indent(&out, indent, 1, "SELECT ${2:fields}")
			completion_template_write_newline_indent(&out, indent, 2, "FROM ${3:table}")
			completion_template_write_newline_indent(&out, indent, 2, "WHERE ${4:field} = @${5:lv_value}.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 0, "DO.")
			completion_template_write_newline_indent(&out, indent, 1, "FETCH NEXT CURSOR @${1:lv_cursor}")
			completion_template_write_newline_indent(&out, indent, 2, "INTO TABLE @DATA(${6:lt_package})")
			completion_template_write_newline_indent(&out, indent, 2, "PACKAGE SIZE ${7:100}.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 1, "IF sy-subrc <> 0.")
			completion_template_write_newline_indent(&out, indent, 2, "EXIT.")
			completion_template_write_newline_indent(&out, indent, 1, "ENDIF.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 1, "$0")
			completion_template_write_newline_indent(&out, indent, 0, "ENDDO.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 0, "CLOSE CURSOR @${1:lv_cursor}.")
		} else {
			strings.write_string(&out, "OPEN CURSOR WITH HOLD @lv_cursor FOR")
			completion_template_write_newline_indent(&out, indent, 1, "SELECT fields")
			completion_template_write_newline_indent(&out, indent, 2, "FROM table")
			completion_template_write_newline_indent(&out, indent, 2, "WHERE field = @lv_value.")
			completion_template_write_newline_indent(&out, indent, 0, "")
			completion_template_write_newline_indent(&out, indent, 0, "DO.")
			completion_template_write_newline_indent(&out, indent, 1, "FETCH NEXT CURSOR @lv_cursor")
			completion_template_write_newline_indent(&out, indent, 2, "INTO TABLE @DATA(lt_package)")
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
	out[0] = completion_commit_template_item(
		"COMMIT WORK",
		.Work,
		snippets_supported,
		allocator,
	)
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
		insert_text = completion_commit_template_insert_text(variant, snippets_supported, allocator),
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

completion_template_prefix :: proc(source: string, offset: int, allocator: mem.Allocator) -> string {
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
		_ = completion_write_method_call_section(
			&out,
			project,
			payload.parameters[:],
			.Method_Importing,
			"",
			indent,
			&tabstop,
		)
	} else {
		_ = completion_write_method_call_section(
			&out,
			project,
			payload.parameters[:],
			.Method_Importing,
			"EXPORTING",
			indent,
			&tabstop,
		)
		_ = completion_write_method_call_section(
			&out,
			project,
			payload.parameters[:],
			.Method_Exporting,
			"IMPORTING",
			indent,
			&tabstop,
		)
		_ = completion_write_method_call_section(
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
		completion_snippet_write_text(out, string_interner.load(project.interner, param.name))
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
