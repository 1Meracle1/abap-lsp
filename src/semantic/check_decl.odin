package abap_frontend_semantic2

import "src:ast"

import "core:mem"
import "core:strconv"
import "core:strings"

Decl_Structure_Frame :: struct {
	entity:    ^Entity,
	structure: ^Structure,
	scope:     ^Scope,
}

decl_info_attach_trivia :: proc(info: ^Decl_Info, name_range: Range, allocator: mem.Allocator) {
	if info == nil || info.decl_node == nil {
		return
	}
	if name_range.start >= name_range.end {
		return
	}
	if len(info.docs) == 0 && decl_node_symbol_count(info.decl_node) == 1 {
		info.docs = ast_comment_trivia(info.decl_node.leading_trivia[:], allocator)
	}
	if len(info.comment) == 0 {
		info.comment = decl_trailing_comment_trivia(info.decl_node, name_range, allocator)
	}
}

ast_comment_trivia :: proc(trivia: []ast.Ast_Trivia, allocator: mem.Allocator) -> []ast.Ast_Trivia {
	out: [dynamic]ast.Ast_Trivia
	for item in trivia {
		if item.kind == .Comment {
			if cap(out) == 0 {
				out = make([dynamic]ast.Ast_Trivia, 0, len(trivia), allocator)
			}
			append(&out, item)
		}
	}
	if cap(out) == 0 {
		return nil
	}
	return out[:]
}

decl_trailing_comment_trivia :: proc(
	node: ^ast.Node,
	name_range: Range,
	allocator: mem.Allocator,
) -> []ast.Ast_Trivia {
	if node == nil || name_range.start >= name_range.end {
		return nil
	}
	out: [dynamic]ast.Ast_Trivia
	for item in node.trailing_trivia {
		if item.kind != .Comment {
			continue
		}
		if decl_node_range_is_nearest_before(node, name_range, item.range.start) {
			if cap(out) == 0 {
				out = make([dynamic]ast.Ast_Trivia, 0, len(node.trailing_trivia), allocator)
			}
			append(&out, item)
		}
	}
	if cap(out) == 0 {
		return nil
	}
	return out[:]
}

decl_node_range_is_nearest_before :: proc(node: ^ast.Node, candidate: Range, limit: int) -> bool {
	if node == nil || candidate.start >= candidate.end || candidate.end > limit {
		return false
	}
	return !decl_node_has_symbol_range_between(node, candidate.end, limit)
}

decl_node_has_symbol_range_between :: proc(node: ^ast.Node, after, before: int) -> bool {
	if node == nil {
		return false
	}
	#partial switch n in node.derived {
	case ^ast.Data_Chained_Decl:
		for clause in n.decls {
			if decl_range_between(decl_data_clause_name_range(clause), after, before) {
				return true
			}
		}
	case ^ast.Class_Data_Decl:
		for clause in n.decls {
			if decl_range_between(decl_data_clause_name_range(clause), after, before) {
				return true
			}
		}
	case ^ast.Types_Decl:
		for clause in n.types {
			if decl_range_between(decl_types_clause_name_range(clause), after, before) {
				return true
			}
		}
	case ^ast.Constants_Decl:
		for clause in n.constants {
			if decl_range_between(decl_constants_clause_name_range(clause), after, before) {
				return true
			}
		}
	case ^ast.Statics_Decl:
		for clause in n.statics {
			if decl_range_between(decl_statics_clause_name_range(clause), after, before) {
				return true
			}
		}
	case ^ast.Field_Symbols_Decl:
		for clause in n.field_symbols {
			if decl_range_between(clause.name.range, after, before) {
				return true
			}
		}
	case ^ast.Parameters_Decl:
		for clause in n.parameters {
			if decl_range_between(clause.name.range, after, before) {
				return true
			}
		}
	case ^ast.Ranges_Decl:
		for clause in n.ranges {
			if decl_range_between(clause.name.range, after, before) {
				return true
			}
		}
	case ^ast.Select_Options_Decl:
		for clause in n.options {
			if decl_range_between(clause.name.range, after, before) {
				return true
			}
		}
	case ^ast.Controls_Decl:
		for clause in n.controls {
			if decl_range_between(clause.name.range, after, before) {
				return true
			}
		}
	case ^ast.Include_Stmt:
		for name in n.names {
			if decl_range_between(name.name.range, after, before) {
				return true
			}
		}
	case ^ast.Oop_Simple_Stmt:
		for member in n.members {
			if decl_range_between(member.name.range, after, before) {
				return true
			}
		}
		for alias in n.aliases {
			if decl_range_between(alias.name.range, after, before) {
				return true
			}
		}
	}
	return false
}

decl_range_between :: proc(range: Range, after, before: int) -> bool {
	return range.start < range.end && range.end > after && range.end <= before
}

decl_node_symbol_count :: proc(node: ^ast.Node) -> int {
	if node == nil {
		return 0
	}
	#partial switch n in node.derived {
	case ^ast.Data_Chained_Decl:
		count := 0
		for clause in n.decls {
			if decl_range_valid(decl_data_clause_name_range(clause)) {
				count += 1
			}
		}
		return count
	case ^ast.Class_Data_Decl:
		count := 0
		for clause in n.decls {
			if decl_range_valid(decl_data_clause_name_range(clause)) {
				count += 1
			}
		}
		return count
	case ^ast.Types_Decl:
		count := 0
		for clause in n.types {
			if decl_range_valid(decl_types_clause_name_range(clause)) {
				count += 1
			}
		}
		return count
	case ^ast.Constants_Decl:
		count := 0
		for clause in n.constants {
			if decl_range_valid(decl_constants_clause_name_range(clause)) {
				count += 1
			}
		}
		return count
	case ^ast.Statics_Decl:
		count := 0
		for clause in n.statics {
			if decl_range_valid(decl_statics_clause_name_range(clause)) {
				count += 1
			}
		}
		return count
	case ^ast.Field_Symbols_Decl:
		count := 0
		for clause in n.field_symbols {
			if decl_range_valid(clause.name.range) {
				count += 1
			}
		}
		return count
	case ^ast.Parameters_Decl:
		count := 0
		for clause in n.parameters {
			if decl_range_valid(clause.name.range) {
				count += 1
			}
		}
		return count
	case ^ast.Ranges_Decl:
		count := 0
		for clause in n.ranges {
			if decl_range_valid(clause.name.range) {
				count += 1
			}
		}
		return count
	case ^ast.Select_Options_Decl:
		count := 0
		for clause in n.options {
			if decl_range_valid(clause.name.range) {
				count += 1
			}
		}
		return count
	case ^ast.Controls_Decl:
		count := 0
		for clause in n.controls {
			if decl_range_valid(clause.name.range) {
				count += 1
			}
		}
		return count
	case ^ast.Include_Stmt:
		count := 0
		for name in n.names {
			if decl_range_valid(name.name.range) {
				count += 1
			}
		}
		return count
	case ^ast.Oop_Simple_Stmt:
		count := 0
		for member in n.members {
			if decl_range_valid(member.name.range) {
				count += 1
			}
		}
		for alias in n.aliases {
			if decl_range_valid(alias.name.range) {
				count += 1
			}
		}
		return count
	}
	return 1
}

decl_range_valid :: proc(range: Range) -> bool {
	return range.start < range.end
}

decl_data_clause_name_range :: proc(clause: ast.Data_Decl_Clause) -> Range {
	if clause.as_name.text != "" {
		return clause.as_name.range
	}
	return clause.name.range
}

decl_types_clause_name_range :: proc(clause: ast.Types_Clause) -> Range {
	if clause.as_name.text != "" {
		return clause.as_name.range
	}
	return clause.name.range
}

decl_constants_clause_name_range :: proc(clause: ast.Constants_Clause) -> Range {
	if clause.as_name.text != "" {
		return clause.as_name.range
	}
	return clause.name.range
}

decl_statics_clause_name_range :: proc(clause: ast.Statics_Clause) -> Range {
	if clause.as_name.text != "" {
		return clause.as_name.range
	}
	return clause.name.range
}

checker_collect_file_entities :: proc(ctx: ^Checker_Context, file: ^Project_File) {
	checker_context_set_file(ctx, file)
	if file.root == nil {
		return
	}
	for stmt in file.root.stmts {
		checker_collect_stmt_entities(ctx, stmt)
	}
}

checker_collect_stmt_entities :: proc(ctx: ^Checker_Context, stmt: ^ast.Stmt) {
	if stmt == nil {
		return
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Data_Chained_Decl:
		checker_collect_data_chained_decl(ctx, n)
	case ^ast.Data_Inline_Decl:
		_ = checker_collect_variable_decl(ctx, ctx.scope, n.name.text, .Variable, n.name.range, &n.node.decl_base.stmt_base, nil, nil)
	case ^ast.Types_Decl:
		checker_collect_types_decl(ctx, n)
	case ^ast.Constants_Decl:
		checker_collect_constants_decl(ctx, n)
	case ^ast.Field_Symbols_Decl:
		checker_collect_field_symbols_decl(ctx, n)
	case ^ast.Statics_Decl:
		checker_collect_statics_decl(ctx, n)
	case ^ast.Tables_Decl:
		checker_collect_tables_decl(ctx, n)
	case ^ast.Ranges_Decl:
		checker_collect_ranges_decl(ctx, n)
	case ^ast.Parameters_Decl:
		checker_collect_parameters_decl(ctx, n)
	case ^ast.Select_Options_Decl:
		checker_collect_select_options_decl(ctx, n)
	case ^ast.Controls_Decl:
		checker_collect_controls_decl(ctx, n)
	case ^ast.Class_Data_Decl:
		checker_collect_class_data_decl(ctx, n)
	case ^ast.Function_Pool_Decl:
		checker_collect_report_decl(ctx, n.name.text, n.name.range, &n.node.decl_base.stmt_base)
	case ^ast.Include_Stmt:
		checker_collect_include_stmt(ctx, n)
	case ^ast.Report_Stmt:
		checker_collect_report_stmt(ctx, n)
	case ^ast.Class_Decl:
		checker_collect_class_decl(ctx, n)
	case ^ast.Interface_Decl:
		checker_collect_interface_decl(ctx, n)
	case ^ast.Form_Decl:
		name_range := n.name.range if n.name.text != "" else n.header_range
		checker_collect_routine_decl(ctx, n.name.text, .Form, n.range, name_range, "", &n.node.stmt_base)
	case ^ast.Method_Decl:
		checker_collect_method_decl(ctx, n)
	case ^ast.Function_Decl:
		name_range := n.name.range if n.name.text != "" else n.header_range
		checker_collect_routine_decl(ctx, n.name.text, .Module, n.range, name_range, "", &n.node.stmt_base)
	case ^ast.Module_Decl:
		name_range := n.name.range if n.name.text != "" else n.header_range
		checker_collect_routine_decl(ctx, n.name.text, .Module, n.range, name_range, "", &n.node.stmt_base)
	case ^ast.Event_Block_Stmt:
		signature := checker_event_block_decl_signature(n)
		checker_collect_routine_decl(
			ctx,
			signature,
			.Event,
			n.range,
			n.header_range,
			signature,
			&n.node.stmt_base,
		)
	case ^ast.Oop_Simple_Stmt:
		checker_collect_oop_simple_stmt(ctx, n, nil, .Public)
	}
}

checker_event_block_decl_signature :: proc(stmt: ^ast.Event_Block_Stmt) -> string {
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, checker_event_block_kind_text(stmt.kind))
	#partial switch stmt.addition {
	case .Selection_Screen_Output:
		strings.write_string(&builder, " OUTPUT")
	case .Selection_Screen_On:
		strings.write_string(&builder, " ON")
		checker_event_block_write_target(&builder, stmt.target)
	case .Selection_Screen_On_End_Of:
		strings.write_string(&builder, " ON END OF")
		checker_event_block_write_target(&builder, stmt.target)
	case .Selection_Screen_On_Help_Request_For:
		strings.write_string(&builder, " ON HELP-REQUEST FOR")
		checker_event_block_write_target(&builder, stmt.target)
	case .Selection_Screen_On_Value_Request_For:
		strings.write_string(&builder, " ON VALUE-REQUEST FOR")
		checker_event_block_write_target(&builder, stmt.target)
	case .Selection_Screen_On_Radiobutton_Group:
		strings.write_string(&builder, " ON RADIOBUTTON GROUP")
		checker_event_block_write_target(&builder, stmt.target)
	case .Selection_Screen_On_Block:
		strings.write_string(&builder, " ON BLOCK")
		checker_event_block_write_target(&builder, stmt.target)
	case .Selection_Screen_On_Exit_Command:
		strings.write_string(&builder, " ON EXIT-COMMAND")
	case .Top_Of_Page_During_Line_Selection:
		strings.write_string(&builder, " DURING LINE-SELECTION")
	}
	return strings.to_string(builder)
}

checker_event_block_write_target :: proc(builder: ^strings.Builder, target: ast.Token_Text) {
	if target.text != "" {
		strings.write_byte(builder, ' ')
		strings.write_string(builder, target.text)
	}
}

checker_event_block_kind_text :: proc(kind: ast.Event_Block_Kind) -> string {
	#partial switch kind {
	case .Initialization:
		return "INITIALIZATION"
	case .Load_Of_Program:
		return "LOAD-OF-PROGRAM"
	case .Start_Of_Selection:
		return "START-OF-SELECTION"
	case .End_Of_Selection:
		return "END-OF-SELECTION"
	case .Top_Of_Page:
		return "TOP-OF-PAGE"
	case .End_Of_Page:
		return "END-OF-PAGE"
	case .At_Selection_Screen:
		return "AT SELECTION-SCREEN"
	}
	return ""
}

checker_collect_data_chained_decl :: proc(
	ctx: ^Checker_Context,
	decl: ^ast.Data_Chained_Decl,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) {
	frames := make([dynamic]Decl_Structure_Frame, 0, 4, context.temp_allocator)
	for clause in decl.decls {
		checker_collect_data_decl_clause(
			ctx,
			&frames,
			clause,
			decl.range,
			&decl.node.decl_base.stmt_base,
			.Variable,
			owner,
			visibility,
		)
	}
}

checker_collect_constants_decl :: proc(
	ctx: ^Checker_Context,
	decl: ^ast.Constants_Decl,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) {
	frames := make([dynamic]Decl_Structure_Frame, 0, 4, context.temp_allocator)
	for clause in decl.constants {
		checker_collect_data_branch(
			ctx,
			&frames,
			clause.kind,
			clause.flags,
			clause.name,
			decl.range,
			&decl.node.decl_base.stmt_base,
			clause.type_clause,
			clause.value_clause,
			clause.occurs,
			clause.include_ref,
			clause.as_name,
			clause.renaming_suffix,
			.Constant,
			owner,
			visibility = visibility,
		)
	}
}

checker_collect_statics_decl :: proc(
	ctx: ^Checker_Context,
	decl: ^ast.Statics_Decl,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) {
	frames := make([dynamic]Decl_Structure_Frame, 0, 4, context.temp_allocator)
	for clause in decl.statics {
		entity := checker_collect_data_branch(
			ctx,
			&frames,
			clause.kind,
			clause.flags,
			clause.name,
			decl.range,
			&decl.node.decl_base.stmt_base,
			clause.type_clause,
			clause.value_clause,
			clause.occurs,
			clause.include_ref,
			clause.as_name,
			clause.renaming_suffix,
			.Variable,
			owner,
			visibility = visibility,
		)
		checker_note_variable_decl_flags(entity, is_static = true)
	}
}

checker_collect_class_data_decl :: proc(
	ctx: ^Checker_Context,
	decl: ^ast.Class_Data_Decl,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) {
	frames := make([dynamic]Decl_Structure_Frame, 0, 4, context.temp_allocator)
	for clause in decl.decls {
		entity := checker_collect_data_decl_clause(
			ctx,
			&frames,
			clause,
			decl.range,
			&decl.node.decl_base.stmt_base,
			.Variable,
			owner,
			visibility,
		)
		checker_note_variable_decl_flags(entity, is_static = true, read_only = .Read_Only in clause.flags)
	}
}

checker_collect_data_decl_clause :: proc(
	ctx: ^Checker_Context,
	frames: ^[dynamic]Decl_Structure_Frame,
	clause: ast.Data_Decl_Clause,
	range: Range,
	node: ^ast.Node,
	entity_kind: Entity_Kind,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) -> ^Entity {
	return checker_collect_data_branch(
		ctx,
		frames,
		clause.kind,
		clause.flags,
		clause.name,
		range,
		node,
		clause.type_clause,
		clause.value_clause,
		clause.occurs,
		clause.include_ref,
		clause.as_name,
		clause.renaming_suffix,
		entity_kind,
		owner,
		visibility,
	)
}

checker_collect_data_branch :: proc(
	ctx: ^Checker_Context,
	frames: ^[dynamic]Decl_Structure_Frame,
	kind: ast.Decl_Clause_Kind,
	flags: ast.Decl_Clause_Flags,
	name: ast.Token_Text,
	range: Range,
	node: ^ast.Node,
	type_clause: ^ast.Data_Type_Clause,
	value_clause: ^ast.Value_Clause,
	occurs: ^ast.Expr,
	include_ref: ^ast.Expr,
	as_name: ast.Token_Text,
	renaming_suffix: ast.Token_Text,
	entity_kind: Entity_Kind,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) -> ^Entity {
	if .Common_Part_Delimiter in flags {
		return nil
	}
	read_only := .Read_Only in flags

	switch kind {
	case .Begin_Group:
		entity: ^Entity
		if len(frames^) > 0 {
			parent := &frames^[len(frames^) - 1]
			entity = checker_collect_structure_field(ctx, parent.structure, parent.scope, parent.entity, name.text, name.range, node, type_clause, value_clause, occurs)
		} else {
			entity = checker_collect_variable_decl(ctx, ctx.scope, name.text, entity_kind, name.range, node, type_clause, value_clause, occurs = occurs)
			checker_note_variable_decl_flags(entity, read_only = read_only)
			checker_note_member_owner(entity, owner, .Attribute, visibility)
		}
		if entity == nil {
			return nil
		}
		structure, scope := checker_attach_structure_to_entity(ctx, entity, range)
		append(frames, Decl_Structure_Frame{entity = entity, structure = structure, scope = scope})
		return entity
	case .End_Group:
		if len(frames^) > 0 {
			frame := pop(frames)
			checker_record_structure_end_name_use(ctx, frame.entity, name)
		}
	case .Normal:
		if len(frames^) > 0 {
			parent := &frames^[len(frames^) - 1]
			return checker_collect_structure_field(ctx, parent.structure, parent.scope, parent.entity, name.text, name.range, node, type_clause, value_clause, occurs)
		}
		entity := checker_collect_variable_decl(ctx, ctx.scope, name.text, entity_kind, name.range, node, type_clause, value_clause, occurs = occurs)
		checker_note_variable_decl_flags(entity, read_only = read_only)
		checker_note_member_owner(entity, owner, .Attribute, visibility)
		return entity
	case .Include_Type, .Include_Structure:
		if len(frames^) > 0 {
			parent := &frames^[len(frames^) - 1]
			return checker_collect_structure_include(
				ctx,
				parent.structure,
				parent.scope,
				parent.entity,
				kind,
				range,
				node,
				include_ref,
				as_name,
				renaming_suffix,
				value_clause,
			)
		}
		return nil
	}
	return nil
}

checker_collect_types_decl :: proc(
	ctx: ^Checker_Context,
	decl: ^ast.Types_Decl,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) {
	frames := make([dynamic]Decl_Structure_Frame, 0, 4, context.temp_allocator)
	for clause in decl.types {
		checker_collect_type_clause(ctx, &frames, clause, decl.range, &decl.node.decl_base.stmt_base, owner, visibility)
	}
}

checker_collect_type_clause :: proc(
	ctx: ^Checker_Context,
	frames: ^[dynamic]Decl_Structure_Frame,
	clause: ast.Types_Clause,
	range: Range,
	node: ^ast.Node,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) -> ^Entity {
	if .Common_Part_Delimiter in clause.flags {
		return nil
	}

	switch clause.kind {
	case .Begin_Group:
		entity: ^Entity
		if len(frames^) > 0 {
			parent := &frames^[len(frames^) - 1]
			entity = checker_collect_structure_field(ctx, parent.structure, parent.scope, parent.entity, clause.name.text, clause.name.range, node, clause.type_clause, nil, clause.occurs)
		} else {
			entity = checker_collect_type_decl(ctx, ctx.scope, clause.name.text, clause.name.range, node, clause.type_clause, clause.occurs)
			checker_note_member_owner(entity, owner, .None, visibility)
		}
		if entity == nil {
			return nil
		}
		structure, scope := checker_attach_structure_to_entity(ctx, entity, range)
		append(frames, Decl_Structure_Frame{entity = entity, structure = structure, scope = scope})
		return entity
	case .End_Group:
		if len(frames^) > 0 {
			frame := pop(frames)
			checker_record_structure_end_name_use(ctx, frame.entity, clause.name)
		}
	case .Normal:
		if len(frames^) > 0 {
			parent := &frames^[len(frames^) - 1]
			return checker_collect_structure_field(ctx, parent.structure, parent.scope, parent.entity, clause.name.text, clause.name.range, node, clause.type_clause, nil, clause.occurs)
		}
		entity := checker_collect_type_decl(ctx, ctx.scope, clause.name.text, clause.name.range, node, clause.type_clause, clause.occurs)
		checker_note_member_owner(entity, owner, .None, visibility)
		return entity
	case .Include_Type, .Include_Structure:
		if len(frames^) > 0 {
			parent := &frames^[len(frames^) - 1]
			return checker_collect_structure_include(
				ctx,
				parent.structure,
				parent.scope,
				parent.entity,
				clause.kind,
				range,
				node,
				clause.include_ref,
				clause.as_name,
				clause.renaming_suffix,
			)
		}
		return nil
	}
	return nil
}

checker_record_structure_end_name_use :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	name: ast.Token_Text,
) {
	if entity == nil || name.text == "" || name.range.start >= name.range.end {
		return
	}
	if project_intern_lower_ascii(ctx.project, name.text) != entity.name {
		checker_add_diagnostic(
			ctx,
			.Mismatched_Structure_End,
			name.range,
			checker_structure_end_name_mismatch_message(ctx, name.text, entity.name),
			entity,
			entity.decl_info,
		)
		return
	}
	checker_add_entity_use_at_range(ctx, nil, entity, name.range)
}

checker_structure_end_name_mismatch_message :: proc(
	ctx: ^Checker_Context,
	end_name: string,
	begin_name: string,
) -> string {
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "END OF ")
	strings.write_string(&builder, end_name)
	strings.write_string(&builder, " does not match BEGIN OF ")
	strings.write_string(&builder, begin_name)
	return strings.to_string(builder)
}

checker_attach_structure_to_entity :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	range: Range,
) -> (^Structure, ^Scope) {
	assert(entity != nil)
	scope := checker_create_scope(ctx.checker, entity.scope, .Structure, range, entity, entity.decl_info)
	structure := project_new_structure(ctx.project, entity.name, ctx.file, scope, range)
	structure_type := project_type_structure(ctx.project, structure)
	if entity.kind == .Type_Def {
		entity.type = project_type_named(ctx.project, entity.name, entity, structure_type)
		if payload, ok := entity.payload.(^Entity_Type_Name_Payload); ok && payload != nil {
			payload.structure = structure
			payload.underlying = structure_type
			payload.original_type = entity.type
		}
	} else {
		entity.type = structure_type
	}
	return structure, scope
}

checker_collect_type_decl :: proc(
	ctx: ^Checker_Context,
	scope: ^Scope,
	name: string,
	range: Range,
	node: ^ast.Node,
	type_clause: ^ast.Data_Type_Clause,
	occurs: ^ast.Expr = nil,
) -> ^Entity {
	if name == "" {
		return nil
	}
	entity := project_new_entity(ctx.project, .Type_Def)
	entity.node = node
	interned := project_intern_lower_ascii(ctx.project, name)
	decl := project_new_decl_info(ctx.project, entity, scope, interned, .Type_Def, range, node, type_clause, occurs)
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	return entity
}

checker_collect_structure_field :: proc(
	ctx: ^Checker_Context,
	structure: ^Structure,
	scope: ^Scope,
	owner: ^Entity,
	name: string,
	range: Range,
	node: ^ast.Node,
	type_clause: ^ast.Data_Type_Clause,
	value_clause: ^ast.Value_Clause = nil,
	occurs: ^ast.Expr = nil,
) -> ^Entity {
	if name == "" {
		return nil
	}
	entity := project_new_entity(ctx.project, .Field)
	entity.node = node
	entity.owner = owner
	entity.source_file = ctx.file
	interned := project_intern_lower_ascii(ctx.project, name)
	decl := project_new_decl_info(ctx.project, entity, scope, interned, .Field, range, node, type_clause, occurs, value_clause)
	payload, ok := entity.payload.(^Entity_Field_Payload)
	if ok && payload != nil {
		payload.owner_structure = structure
		payload.decl_unit = ctx.file
		payload.decl_range = range
		payload.field_index = len(structure.fields)
		payload.value_clause = value_clause
		payload.type_clause_form = checker_type_form_with_occurs(type_clause.form, occurs) if type_clause != nil else ast.Data_Type_Form.Type
		payload.has_type_clause_form = type_clause != nil
		if type_clause != nil {
			payload.flags += {.Has_Type_Ref}
		}
	}
	append(&structure.fields, entity)
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	return entity
}

checker_collect_structure_include :: proc(
	ctx: ^Checker_Context,
	structure: ^Structure,
	scope: ^Scope,
	owner: ^Entity,
	kind: ast.Decl_Clause_Kind,
	range: Range,
	node: ^ast.Node,
	include_ref: ^ast.Expr,
	as_name: ast.Token_Text = {},
	renaming_suffix: ast.Token_Text = {},
	value_clause: ^ast.Value_Clause = nil,
) -> ^Entity {
	assert(kind == .Include_Type || kind == .Include_Structure)
	name := as_name.text
	name_range := as_name.range if name != "" else Range{}
	entity := project_new_entity(ctx.project, .Field)
	entity.node = node
	entity.owner = owner
	entity.source_file = ctx.file
	interned := project_intern_lower_ascii(ctx.project, name)
	decl := project_new_decl_info(
		ctx.project,
		entity,
		scope,
		interned,
		.Field,
		name_range,
		node,
		value_clause = value_clause,
	)
	payload, ok := entity.payload.(^Entity_Field_Payload)
	assert(ok && payload != nil)
	payload.owner_structure = structure
	payload.decl_unit = ctx.file
	payload.decl_range = name_range
	payload.field_index = len(structure.fields)
	payload.value_clause = value_clause
	payload.type_ref = checker_type_ref_data_from_expr(
		ctx,
		include_ref,
		.Type if kind == .Include_Type else .Value,
	)
	payload.type_clause_form = .Structure
	payload.has_type_clause_form = true
	payload.flags += {.Has_Type_Ref}
	if name == "" {
		payload.flags += {.Is_Include}
		if renaming_suffix.text != "" {
			payload.include_renaming_suffix = strings.clone(renaming_suffix.text, ctx.project.allocator)
		}
	}
	append(&structure.fields, entity)
	_ = checker_add_entity_and_decl_info(ctx, entity, decl, insert_in_scope = name != "")
	return entity
}

checker_collect_variable_decl :: proc(
	ctx: ^Checker_Context,
	scope: ^Scope,
	name: string,
	kind: Entity_Kind,
	range: Range,
	node: ^ast.Node,
	type_clause: ^ast.Data_Type_Clause,
	value_clause: ^ast.Value_Clause,
	default_expr: ^ast.Expr = nil,
	occurs: ^ast.Expr = nil,
) -> ^Entity {
	if name == "" {
		return nil
	}
	entity := project_new_entity(ctx.project, kind)
	entity.node = node
	interned := project_intern_lower_ascii(ctx.project, name)
	decl := project_new_decl_info(ctx.project, entity, scope, interned, kind, range, node, type_clause, occurs, value_clause, default_expr)
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	checker_note_variable_decl_flags(entity, has_type = type_clause != nil)
	return entity
}

checker_note_variable_decl_flags :: proc(
	entity: ^Entity,
	has_type := false,
	is_static := false,
	read_only := false,
) {
	if entity == nil {
		return
	}
	if has_type {
		entity.flags += {.Has_Declared_Type}
		entity.flags -= {.Untyped}
	} else {
		if !(.Has_Declared_Type in entity.flags) {
			entity.flags += {.Untyped}
		}
	}
	if is_static {
		entity.flags += {.Static}
	}
	if read_only {
		entity.flags += {.Read_Only}
	}
}

checker_note_member_owner :: proc(
	entity: ^Entity,
	owner: ^Entity,
	member_kind: Class_Member_Kind,
	visibility: Visibility = .Public,
) {
	if entity == nil || owner == nil {
		return
	}
	entity.owner = owner
	entity.member_kind = member_kind
	entity.visibility = visibility
	if payload, ok := entity.payload.(^Entity_Routine_Payload); ok && payload != nil {
		payload.member_kind = member_kind
		payload.visibility = visibility
	}
}

checker_collect_field_symbols_decl :: proc(
	ctx: ^Checker_Context,
	decl: ^ast.Field_Symbols_Decl,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) {
	for clause in decl.field_symbols {
		entity := checker_collect_variable_decl(
			ctx,
			ctx.scope,
			clause.name.text,
			.Field_Symbol,
			clause.name.range,
			&decl.node.decl_base.stmt_base,
			clause.type_clause,
			nil,
		)
		checker_note_member_owner(entity, owner, .Attribute, visibility)
	}
}

checker_collect_tables_decl :: proc(
	ctx: ^Checker_Context,
	decl: ^ast.Tables_Decl,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) {
	for clause in decl.tables {
		entity := checker_collect_variable_decl(ctx, ctx.scope, clause.name.text, .Variable, clause.name.range, &decl.node.decl_base.stmt_base, nil, nil)
		checker_note_variable_decl_flags(entity, has_type = clause.name.text != "")
		checker_note_member_owner(entity, owner, .Attribute, visibility)
	}
}

checker_collect_ranges_decl :: proc(
	ctx: ^Checker_Context,
	decl: ^ast.Ranges_Decl,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) {
	for clause in decl.ranges {
		entity := checker_collect_variable_decl(ctx, ctx.scope, clause.name.text, .Variable, clause.name.range, &decl.node.decl_base.stmt_base, nil, nil)
		if entity != nil {
			structure, scope := checker_attach_structure_to_entity(ctx, entity, decl.range)
			checker_collect_range_component(ctx, structure, scope, entity, "sign", Range{}, &decl.node.decl_base.stmt_base)
			checker_collect_range_component(ctx, structure, scope, entity, "option", Range{}, &decl.node.decl_base.stmt_base)
			checker_collect_range_component(ctx, structure, scope, entity, "low", Range{}, &decl.node.decl_base.stmt_base)
			checker_collect_range_component(ctx, structure, scope, entity, "high", Range{}, &decl.node.decl_base.stmt_base)
		}
		checker_note_member_owner(entity, owner, .Attribute, visibility)
	}
}

checker_collect_select_options_decl :: proc(
	ctx: ^Checker_Context,
	decl: ^ast.Select_Options_Decl,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) {
	for clause in decl.options {
		entity := checker_collect_variable_decl(ctx, ctx.scope, clause.name.text, .Variable, clause.name.range, &decl.node.decl_base.stmt_base, nil, nil, clause.default_expr)
		if entity != nil {
			structure, scope := checker_attach_structure_to_entity(ctx, entity, decl.range)
			checker_collect_range_component(ctx, structure, scope, entity, "sign", Range{}, &decl.node.decl_base.stmt_base)
			checker_collect_range_component(ctx, structure, scope, entity, "option", Range{}, &decl.node.decl_base.stmt_base)
			checker_collect_range_component(ctx, structure, scope, entity, "low", Range{}, &decl.node.decl_base.stmt_base)
			checker_collect_range_component(ctx, structure, scope, entity, "high", Range{}, &decl.node.decl_base.stmt_base)
		}
		checker_note_member_owner(entity, owner, .Attribute, visibility)
	}
}

checker_collect_range_component :: #force_inline proc(
	ctx: ^Checker_Context,
	structure: ^Structure,
	scope: ^Scope,
	owner: ^Entity,
	name: string,
	range: Range,
	node: ^ast.Node,
) {
	_ = checker_collect_structure_field(ctx, structure, scope, owner, name, range, node, nil, nil)
}

checker_collect_parameters_decl :: proc(
	ctx: ^Checker_Context,
	decl: ^ast.Parameters_Decl,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) {
	for clause in decl.parameters {
		entity := checker_collect_variable_decl(
			ctx,
			ctx.scope,
			clause.name.text,
			.Variable,
			clause.name.range,
			&decl.node.decl_base.stmt_base,
			clause.type_clause,
			nil,
			clause.default_expr,
		)
		if entity != nil && .As_Checkbox in clause.flags {
			entity.flags += {.Has_Declared_Type}
			entity.flags -= {.Untyped}
		}
		checker_note_member_owner(entity, owner, .Attribute, visibility)
	}
}

checker_collect_controls_decl :: proc(
	ctx: ^Checker_Context,
	decl: ^ast.Controls_Decl,
	owner: ^Entity = nil,
	visibility: Visibility = .Public,
) {
	for clause in decl.controls {
		entity := checker_collect_variable_decl(
			ctx,
			ctx.scope,
			clause.name.text,
			.Control,
			clause.name.range,
			&decl.node.decl_base.stmt_base,
			clause.type_clause,
			nil,
		)
		checker_note_member_owner(entity, owner, .Attribute, visibility)
	}
}

checker_collect_report_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Report_Stmt) {
	if stmt.kind != .Report && stmt.kind != .Program {
		return
	}
	if name, range, ok := checker_expr_name(stmt.name); ok {
		checker_collect_report_decl(ctx, name, range, &stmt.node.stmt_base)
	}
}

checker_collect_report_decl :: proc(ctx: ^Checker_Context, name: string, range: Range, node: ^ast.Node) -> ^Entity {
	if name == "" {
		return nil
	}
	entity := project_new_entity(ctx.project, .Report)
	entity.node = node
	interned := project_intern_lower_ascii(ctx.project, name)
	decl := project_new_decl_info(ctx.project, entity, ctx.scope, interned, .Report, range, node)
	if payload, ok := entity.payload.(^Entity_Report_Payload); ok && payload != nil {
		append(&payload.provided_names, interned)
	}
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	return entity
}

checker_collect_include_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Include_Stmt) {
	for include_name in stmt.names {
		if include_name.name.text == "" {
			continue
		}
		entity := project_new_entity(ctx.project, .Include)
		entity.node = &stmt.node.stmt_base
		interned := project_intern_lower_ascii(ctx.project, include_name.name.text)
		decl := project_new_decl_info(ctx.project, entity, ctx.scope, interned, .Include, include_name.name.range, &stmt.node.stmt_base)
		if payload, ok := entity.payload.(^Entity_Include_Payload); ok && payload != nil {
			payload.if_found = stmt.if_found
		}
		_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	}
}

checker_collect_class_decl :: proc(ctx: ^Checker_Context, decl: ^ast.Class_Decl) -> ^Entity {
	entity := checker_find_object_entity(ctx, decl.name.text, .Class)
	if entity == nil && decl.name.text != "" {
		entity = project_new_entity(ctx.project, .Class)
		entity.node = &decl.node.stmt_base
		interned := project_intern_lower_ascii(ctx.project, decl.name.text)
		info := project_new_decl_info(ctx.project, entity, ctx.scope, interned, .Class, decl.name.range, &decl.node.stmt_base)
		_ = checker_add_entity_and_decl_info(ctx, entity, info)
	}
	if entity == nil {
		return nil
	}

	payload, ok := entity.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	if .Bodyless in decl.flags {
		entity.flags += {.Forward}
		return entity
	}
	if .Implementation in decl.flags {
		entity.flags += {.Has_Implementation}
		checker_collect_class_implementation(ctx, entity, decl.body)
		return entity
	}

	entity.flags -= {.Forward}
	payload.is_public = .Public in decl.flags
	if .Abstract in decl.flags {
		entity.flags += {.Abstract}
		payload.is_abstract = true
	}
	payload.is_final = .Final in decl.flags
	payload.create_visibility = decl.create_visibility
	payload.is_shared_memory_enabled = .Shared_Memory_Enabled in decl.flags
	payload.is_for_testing = .For_Testing in decl.flags
	payload.test_risk_level = decl.risk_level
	payload.test_duration = decl.duration
	if decl.superclass_name.text != "" {
		payload.superclass_name = project_intern_lower_ascii(ctx.project, decl.superclass_name.text)
		payload.superclass_range = decl.superclass_name.range
	}
	for friend in decl.friends {
		if friend.name.text != "" {
			append(&payload.friends, project_intern_lower_ascii(ctx.project, friend.name.text))
		}
	}
	scope := checker_ensure_object_definition_scope(ctx, entity, .Class, decl.range)
	body_ctx := ctx^
	body_ctx.scope = scope
	checker_collect_class_body(&body_ctx, decl.body, entity, .Private)
	return entity
}

checker_collect_interface_decl :: proc(ctx: ^Checker_Context, decl: ^ast.Interface_Decl) -> ^Entity {
	entity := checker_find_object_entity(ctx, decl.name.text, .Interface)
	if entity == nil && decl.name.text != "" {
		entity = project_new_entity(ctx.project, .Interface)
		entity.node = &decl.node.stmt_base
		interned := project_intern_lower_ascii(ctx.project, decl.name.text)
		info := project_new_decl_info(ctx.project, entity, ctx.scope, interned, .Interface, decl.name.range, &decl.node.stmt_base)
		_ = checker_add_entity_and_decl_info(ctx, entity, info)
	}
	if entity == nil {
		return nil
	}
	payload, ok := entity.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	if .Bodyless in decl.flags {
		entity.flags += {.Forward}
		return entity
	}
	entity.flags -= {.Forward}
	payload.is_public = .Public in decl.flags
	scope := checker_ensure_object_definition_scope(ctx, entity, .Interface, decl.range)
	body_ctx := ctx^
	body_ctx.scope = scope
	checker_collect_class_body(&body_ctx, decl.body, entity, .Public)
	return entity
}

checker_find_object_entity :: proc(ctx: ^Checker_Context, name: string, kind: Entity_Kind) -> ^Entity {
	if name == "" {
		return nil
	}
	interned := project_intern_lower_ascii(ctx.project, name)
	_, entity, ok := checker_lookup_declaration(ctx, .Type, interned)
	if ok && entity.kind == kind {
		return entity
	}
	return nil
}

checker_ensure_object_definition_scope :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	scope_kind: Scope_Kind,
	range: Range,
) -> ^Scope {
	payload, ok := entity.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	if payload.definition_scope == nil {
		payload.definition_scope = checker_create_scope(ctx.checker, entity.scope, scope_kind, range, entity, entity.decl_info)
	}
	return payload.definition_scope
}

checker_collect_class_implementation :: proc(ctx: ^Checker_Context, owner: ^Entity, body: [dynamic]^ast.Stmt) {
	scope := checker_ensure_object_definition_scope(ctx, owner, .Class if owner.kind == .Class else .Interface, owner.name_range)
	body_ctx := ctx^
	body_ctx.scope = scope
	for stmt in body {
		if method, ok := stmt.derived_stmt.(^ast.Method_Decl); ok {
			checker_collect_method_decl(&body_ctx, method)
			continue
		}
		checker_collect_stmt_entities(&body_ctx, stmt)
	}
}

checker_collect_class_body :: proc(
	ctx: ^Checker_Context,
	body: [dynamic]^ast.Stmt,
	owner: ^Entity,
	default_visibility: Visibility,
) {
	visibility := default_visibility
	for stmt in body {
		if stmt == nil {
			continue
		}
		if oop, ok := stmt.derived_stmt.(^ast.Oop_Simple_Stmt); ok {
			if oop.kind == .Class_Section {
				visibility = checker_visibility_from_ast(oop.visibility, visibility)
			} else {
				checker_collect_oop_simple_stmt(ctx, oop, owner, visibility)
			}
			continue
		}
		#partial switch n in stmt.derived_stmt {
		case ^ast.Data_Chained_Decl:
			checker_collect_data_chained_decl(ctx, n, owner, visibility)
		case ^ast.Class_Data_Decl:
			checker_collect_class_data_decl(ctx, n, owner, visibility)
		case ^ast.Statics_Decl:
			checker_collect_statics_decl(ctx, n, owner, visibility)
		case ^ast.Constants_Decl:
			checker_collect_constants_decl(ctx, n, owner, visibility)
		case ^ast.Field_Symbols_Decl:
			checker_collect_field_symbols_decl(ctx, n, owner, visibility)
		case ^ast.Types_Decl:
			checker_collect_types_decl(ctx, n, owner, visibility)
		case:
			checker_collect_stmt_entities(ctx, stmt)
		}
	}
}

checker_collect_oop_simple_stmt :: proc(
	ctx: ^Checker_Context,
	stmt: ^ast.Oop_Simple_Stmt,
	owner: ^Entity,
	visibility: Visibility,
) {
	#partial switch stmt.kind {
	case .Methods, .Class_Methods:
		for member in stmt.members {
			entity := checker_collect_oop_routine_member(ctx, stmt, member, owner, .Method, visibility)
			if entity != nil {
				payload := entity.payload.(^Entity_Routine_Payload)
				payload.is_static = stmt.kind == .Class_Methods
				payload.member_kind = .Method
				if payload.is_static {
					entity.flags += {.Static}
				}
				if .Redefinition in member.flags {
					entity.flags += {.Redefinition}
					payload.is_redefinition = true
				}
				if member.event_handler.event_name.text != "" {
					entity.flags += {.For_Event}
					payload.for_event = true
					payload.event_name = project_intern_lower_ascii(ctx.project, member.event_handler.event_name.text)
					payload.event_range = member.event_handler.event_name.range
					payload.event_source_type = checker_type_ref_data_from_expr(ctx, member.event_handler.source_type, .Type)
				}
				checker_check_oop_constructor_definition_form(ctx, stmt, member, entity)
			}
		}
	case .Events, .Class_Events:
		for member in stmt.members {
			entity := checker_collect_oop_routine_member(ctx, stmt, member, owner, .Event, visibility)
			if entity != nil {
				payload := entity.payload.(^Entity_Routine_Payload)
				payload.is_static = stmt.kind == .Class_Events
				payload.member_kind = .Event
				if payload.is_static {
					entity.flags += {.Static}
				}
			}
		}
	case .Interfaces:
		if owner == nil {
			return
		}
		payload, ok := owner.payload.(^Entity_Object_Payload)
		assert(ok && payload != nil)
		for member in stmt.members {
			if member.name.text != "" {
				append(&payload.implemented_interfaces, project_intern_lower_ascii(ctx.project, member.name.text))
			}
		}
	case .Aliases:
		checker_collect_oop_aliases(ctx, stmt, owner, visibility)
	case:
	}
}

checker_collect_oop_routine_member :: proc(
	ctx: ^Checker_Context,
	stmt: ^ast.Oop_Simple_Stmt,
	member: ast.Oop_Member_Clause,
	owner: ^Entity,
	kind: Entity_Kind,
	visibility: Visibility,
) -> ^Entity {
	name := member.name.text
	header_range := member.name.range
	if kind == .Method {
		name = checker_oop_method_entity_name(member)
	}
	range := member.range if member.range.end > member.range.start else stmt.range
	entity := checker_collect_routine_decl(ctx, name, kind, range, header_range, "", &stmt.node.stmt_base)
	if entity == nil {
		return nil
	}
	checker_note_member_owner(entity, owner, .Method if kind == .Method else .Event, visibility)
	payload, ok := entity.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil)
	payload.visibility = visibility
	if .Abstract in member.flags {
		entity.flags += {.Abstract}
	}
	if .Final in member.flags {
		entity.flags += {.Final}
	}
	checker_collect_oop_signature(ctx, entity, member.signatures[:], kind)
	return entity
}

checker_collect_oop_aliases :: proc(
	ctx: ^Checker_Context,
	stmt: ^ast.Oop_Simple_Stmt,
	owner: ^Entity,
	visibility: Visibility,
) {
	if len(stmt.aliases) > 0 {
		for alias in stmt.aliases {
			checker_collect_oop_alias(ctx, alias.name, alias.target, stmt.range, &stmt.node.stmt_base, owner, visibility)
		}
		return
	}
	for member in stmt.members {
		for sig in member.signatures {
			if sig.kind == .For && len(sig.values) > 0 {
				checker_collect_oop_alias(ctx, member.name, sig.values[0], stmt.range, &stmt.node.stmt_base, owner, visibility)
				break
			}
		}
	}
}

checker_collect_oop_alias :: proc(
	ctx: ^Checker_Context,
	name: ast.Token_Text,
	target: ^ast.Expr,
	range: Range,
	node: ^ast.Node,
	owner: ^Entity,
	visibility: Visibility,
) -> ^Entity {
	if name.text == "" {
		return nil
	}
	entity := project_new_entity(ctx.project, .Alias)
	entity.node = node
	entity.owner = owner
	entity.member_kind = .None
	entity.visibility = visibility
	interned := project_intern_lower_ascii(ctx.project, name.text)
	decl := project_new_decl_info(ctx.project, entity, ctx.scope, interned, .Alias, name.range, node)
	if payload, ok := entity.payload.(^Entity_Alias_Payload); ok && payload != nil {
		target_ref := checker_type_ref_data_from_expr(ctx, target, .Type)
		payload.target_interface_name = target_ref.base_name
		payload.target_interface_range = target_ref.base_range
		if len(target_ref.field_path) > 0 {
			payload.target_member_name = target_ref.field_path[0]
			if len(target_ref.field_ranges) > 0 {
				payload.target_member_range = target_ref.field_ranges[0]
			}
		}
		payload.visibility = visibility
	}
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	return entity
}

checker_collect_method_decl :: proc(ctx: ^Checker_Context, decl: ^ast.Method_Decl) -> ^Entity {
	name := decl.name.text
	if decl.qualifier.text == "" {
		name = checker_method_entity_name(decl.name.text)
	}
	entity := checker_find_routine_entity_in_scope(ctx, name, .Method)
	if entity == nil {
		entity = checker_collect_routine_decl(ctx, name, .Method, decl.range, decl.header_range, "", &decl.node.stmt_base)
		if owner := checker_enclosing_object_owner(ctx.scope); owner != nil {
			checker_note_member_owner(entity, owner, .Method)
		}
	} else {
		checker_attach_routine_body_decl(ctx, entity, decl.range, decl.header_range, "", &decl.node.stmt_base)
	}
	if entity == nil {
		return nil
	}
	payload, ok := entity.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil)
	payload.has_implementation = true
	payload.implementation_unit = ctx.file
	payload.implementation_range = decl.range
	payload.implementation_name_range = decl.name.range
	entity.flags += {.Has_Implementation}
	return entity
}

checker_check_oop_constructor_definition_form :: proc(
	ctx: ^Checker_Context,
	stmt: ^ast.Oop_Simple_Stmt,
	member: ast.Oop_Member_Clause,
	entity: ^Entity,
) {
	if entity == nil || entity.kind != .Method {
		return
	}
	constructor_name := project_intern_lower_ascii(ctx.project, "constructor")
	class_constructor_name := project_intern_lower_ascii(ctx.project, "class_constructor")
	if entity.name == constructor_name {
		checker_check_instance_constructor_definition_form(ctx, stmt, member, entity)
	} else if entity.name == class_constructor_name {
		checker_check_class_constructor_definition_form(ctx, stmt, member, entity)
	}
}

checker_check_instance_constructor_definition_form :: proc(
	ctx: ^Checker_Context,
	stmt: ^ast.Oop_Simple_Stmt,
	member: ast.Oop_Member_Clause,
	entity: ^Entity,
) {
	if entity.owner == nil || entity.owner.kind != .Class {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, member.name.range, "constructor can only be declared in a class")
	}
	if stmt.kind == .Class_Methods {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, member.name.range, "constructor cannot be declared with CLASS-METHODS")
	}
	checker_check_constructor_member_flags(ctx, member, "constructor")
	if member.event_handler.event_name.text != "" {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, member.event_handler.event_name.range, "constructor cannot be an event handler")
	}
	for sig in member.signatures {
		#partial switch sig.kind {
		case .Importing, .Raising, .Exceptions:
		case:
			checker_add_diagnostic(ctx, .Invalid_Syntax_Form, sig.range, "constructor allows only IMPORTING parameters and exceptions")
		}
	}
}

checker_check_class_constructor_definition_form :: proc(
	ctx: ^Checker_Context,
	stmt: ^ast.Oop_Simple_Stmt,
	member: ast.Oop_Member_Clause,
	entity: ^Entity,
) {
	if entity.owner == nil || entity.owner.kind != .Class {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, member.name.range, "class constructor can only be declared in a class")
	}
	if stmt.kind != .Class_Methods {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, member.name.range, "class constructor must be declared with CLASS-METHODS")
	}
	checker_check_constructor_member_flags(ctx, member, "class constructor")
	if member.event_handler.event_name.text != "" {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, member.event_handler.event_name.range, "class constructor cannot be an event handler")
	}
	for sig in member.signatures {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, sig.range, "class constructor cannot declare a signature")
	}
}

checker_check_constructor_member_flags :: proc(
	ctx: ^Checker_Context,
	member: ast.Oop_Member_Clause,
	subject: string,
) {
	if .Abstract in member.flags {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			member.name.range,
			checker_constructor_form_message(subject, " cannot be ABSTRACT"),
		)
	}
	if .Final in member.flags {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			member.name.range,
			checker_constructor_form_message(subject, " cannot be FINAL"),
		)
	}
	if .Redefinition in member.flags {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			member.name.range,
			checker_constructor_form_message(subject, " cannot be REDEFINITION"),
		)
	}
}

checker_constructor_form_message :: proc(subject, suffix: string) -> string {
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, subject)
	strings.write_string(&builder, suffix)
	return strings.to_string(builder)
}

checker_find_routine_entity_in_scope :: proc(ctx: ^Checker_Context, name: string, kind: Entity_Kind) -> ^Entity {
	if name == "" {
		return nil
	}
	interned := project_intern_lower_ascii(ctx.project, name)
	if entity, ok := scope_lookup_declaration(ctx.scope, .Routine, interned); ok && entity.kind == kind {
		return entity
	}
	return nil
}

checker_collect_routine_decl :: proc(
	ctx: ^Checker_Context,
	name: string,
	kind: Entity_Kind,
	range: Range,
	header_range: Range,
	signature: string,
	node: ^ast.Node,
	scope: ^Scope = nil,
) -> ^Entity {
	if name == "" {
		return nil
	}
	decl_scope := scope if scope != nil else ctx.scope
	entity := project_new_entity(ctx.project, kind)
	entity.node = node
	interned := project_intern_lower_ascii(ctx.project, name)
	decl := project_new_decl_info(ctx.project, entity, decl_scope, interned, kind, header_range, node)
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	checker_initialize_routine_payload(ctx, entity, range, signature)
	checker_collect_routine_parameters(ctx, entity, node)
	return entity
}

checker_attach_routine_body_decl :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	range: Range,
	header_range: Range,
	signature: string,
	node: ^ast.Node,
) {
	assert(entity != nil)
	decl_scope := entity.scope
	previous_docs: []ast.Ast_Trivia
	previous_comment: []ast.Ast_Trivia
	if entity.decl_info != nil {
		previous_docs = entity.decl_info.docs
		previous_comment = entity.decl_info.comment
	}
	payload, ok := entity.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil)
	if payload.signature_scope != nil {
		decl_scope = payload.signature_scope
	}
	decl := project_new_decl_info(ctx.project, nil, decl_scope, entity.name, entity.kind, header_range, node)
	if len(decl.docs) == 0 {
		decl.docs = previous_docs
	}
	if len(decl.comment) == 0 {
		decl.comment = previous_comment
	}
	decl.entity = entity
	entity.decl_info = decl
	entity.node = node
	checker_initialize_routine_payload(ctx, entity, range, signature)
	checker_add_definition(ctx.info, entity)
	if entity.state == .Unresolved {
		checker_enqueue_entity(ctx.info, entity)
	}
}

checker_initialize_routine_payload :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	range: Range,
	signature: string,
) {
	payload, ok := entity.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil)
	if signature != "" {
		payload.signature = strings.clone(signature, ctx.project.allocator)
	}
	if payload.signature_scope == nil {
		payload.signature_scope = checker_create_scope(ctx.checker, entity.scope, checker_scope_kind_for_routine(entity.kind), range, entity, entity.decl_info)
	}
	payload.body_scope = payload.signature_scope
}

checker_collect_routine_parameters :: proc(ctx: ^Checker_Context, owner: ^Entity, node: ^ast.Node) {
	assert(owner != nil)
	payload, ok := owner.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil && payload.signature_scope != nil && node != nil)
	#partial switch n in node.derived {
	case ^ast.Form_Decl:
		for param in n.form_parameters {
			checker_collect_parameter_decl(
				ctx,
				payload.signature_scope,
				owner,
				param.name.text,
				param.name.range,
				param.type_clause,
				checker_form_parameter_section_from_ast(param.section),
				checker_parameter_passing_from_ast(param.passing),
			)
		}
	case ^ast.Function_Decl:
		for param in n.function_parameters {
			checker_collect_parameter_decl(
				ctx,
				payload.signature_scope,
				owner,
				param.name.text,
				param.name.range,
				param.type_clause,
				checker_function_parameter_section_from_ast(param.section),
				checker_parameter_passing_from_ast(param.passing),
				optional = .Is_Optional in param.flags,
				has_default = .Has_Default_Value in param.flags,
				default_expr = param.default_expr,
			)
		}
		for exception in n.exceptions {
			exc := checker_collect_variable_decl(ctx, payload.signature_scope, exception.name.text, .Exception, exception.name.range, node, nil, nil)
			if exc == nil {
				continue
			}
			exc.owner = owner
			append(&payload.exceptions, exc.name)
		}
	}
}

checker_collect_oop_signature :: proc(
	ctx: ^Checker_Context,
	owner: ^Entity,
	signatures: []ast.Oop_Signature_Clause,
	routine_kind: Entity_Kind,
) {
	payload, ok := owner.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil)
	for sig in signatures {
		section, section_ok := checker_oop_parameter_section_from_ast(sig.kind, routine_kind)
		if section_ok {
			for param in sig.parameters {
				checker_collect_parameter_decl(
					ctx,
					payload.signature_scope,
					owner,
					param.name.text,
					param.name.range,
					param.type_clause,
					section,
					checker_parameter_passing_from_ast(param.passing),
					optional = param.optional,
					has_default = param.has_default,
					default_expr = param.default_expr,
				)
			}
			continue
		}
		if sig.kind == .Exceptions {
			for value in sig.values {
				if name, _, name_ok := checker_expr_name(value); name_ok {
					append(&payload.exceptions, project_intern_lower_ascii(ctx.project, name))
				}
			}
			continue
		}
		if sig.kind == .Raising {
			for value in sig.values {
				checker_collect_oop_raising_exception(ctx, payload, value)
			}
		}
	}
}

checker_collect_oop_raising_exception :: proc(
	ctx: ^Checker_Context,
	payload: ^Entity_Routine_Payload,
	value: ^ast.Expr,
) {
	type_ref := checker_type_ref_data_from_expr(ctx, value, .Type)
	if type_ref.base_name == "" {
		return
	}
	append(&payload.exceptions, type_ref.base_name)
	append(&payload.exception_type_refs, type_ref)
}

checker_collect_parameter_decl :: proc(
	ctx: ^Checker_Context,
	scope: ^Scope,
	owner: ^Entity,
	name: string,
	range: Range,
	type_clause: ^ast.Data_Type_Clause,
	section: Entity_Parameter_Section,
	passing: Entity_Parameter_Passing,
	optional := false,
	has_default := false,
	default_expr: ^ast.Expr = nil,
) -> ^Entity {
	entity := checker_collect_variable_decl(ctx, scope, name, .Parameter, range, owner.node, type_clause, nil, default_expr)
	if entity == nil {
		return nil
	}
	entity.owner = owner
	entity.flags += {.Parameter}
	if optional {
		entity.flags += {.Optional}
	}
	if has_default {
		entity.flags += {.Has_Default_Value}
	}
	if payload, ok := entity.payload.(^Entity_Variable_Payload); ok && payload != nil {
		payload.section = section
		payload.passing = passing
	}
	if routine, ok := owner.payload.(^Entity_Routine_Payload); ok && routine != nil {
		append(&routine.parameters, entity)
	}
	return entity
}

checker_check_queued_entities :: proc(ctx: ^Checker_Context) {
	for index := 0; index < len(ctx.info.entity_queue); index += 1 {
		entity := ctx.info.entity_queue[index]
		checker_check_entity_decl(ctx, entity)
	}
	clear(&ctx.info.entity_queue)
}

checker_check_entity_decl :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	decl: ^Decl_Info = nil,
	named_type: ^Type = nil,
) {
	if entity == nil || entity.state == .Resolved || entity.state == .Failed {
		return
	}
	current_decl := decl
	if entity.state == .Resolving || checker_type_path_contains(ctx, entity) {
		entity.state = .Failed
		checker_add_diagnostic(ctx, .Declaration_Cycle, entity.name_range, "declaration cycle", entity, current_decl)
		return
	}
	if current_decl == nil {
		current_decl = entity.decl_info
	}
	assert(current_decl != nil)
	assert(current_decl.scope != nil)

	local := ctx^
	if entity.source_file != nil {
		local.file = entity.source_file
	}
	local.scope = current_decl.scope
	local.decl = current_decl
	local.current_decl = current_decl
	entity.state = .Resolving
	current_decl.state = .Resolving

	track_path := checker_entity_tracks_type_path(entity)
	if track_path {
		append(&local.type_path, entity)
	}
	defer if track_path {
		_ = pop(&local.type_path)
	}

	switch entity.kind {
	case .Builtin:
		checker_check_builtin_decl(&local, entity)
	case .Variable, .Field_Symbol, .Parameter, .Exception, .Control:
		checker_check_variable_decl(&local, entity, current_decl)
	case .Constant, .Enum_Member:
		checker_check_constant_decl(&local, entity, current_decl)
	case .Type_Def:
		checker_check_type_decl(&local, entity, current_decl, named_type)
	case .Form, .Method, .Module, .Event:
		checker_check_routine_decl(&local, entity, current_decl)
	case .Class, .Interface:
		checker_check_object_decl(&local, entity, current_decl)
	case .Field, .Include, .Alias, .Report:
		checker_check_metadata_decl(&local, entity, current_decl)
	case .Invalid:
		entity.state = .Failed
		current_decl.state = .Failed
		return
	}

	if entity.state == .Resolving {
		entity.state = .Resolved
	}
	if current_decl.state == .Resolving {
		current_decl.state = .Resolved if entity.state == .Resolved else .Failed
	}
	if entity.state == .Resolved {
		append(&ctx.info.checked_entities, entity)
	}
}

checker_check_builtin_decl :: proc(ctx: ^Checker_Context, entity: ^Entity) {
	if entity.type == nil {
		entity.type = project_type_unknown(ctx.project)
	}
}

checker_check_variable_decl :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) {
	if typ := checker_check_decl_type_clause(ctx, entity, decl.type_clause, decl.occurs); typ != nil {
		entity.type = typ
	}
	checker_check_value_clause(ctx, decl.value_clause)
	checker_check_default_expr(ctx, decl.default_expr)
	if entity.type == nil {
		entity.type = project_type_unknown(ctx.project)
	}
}

checker_check_constant_decl :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) {
	if typ := checker_check_decl_type_clause(ctx, entity, decl.type_clause, decl.occurs); typ != nil {
		entity.type = typ
	}
	checker_check_value_clause(ctx, decl.value_clause)
	checker_record_constant_value(ctx, entity, decl.value_clause)
	if entity.type == nil {
		entity.type = project_type_unknown(ctx.project)
	}
}

checker_record_constant_value :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	clause: ^ast.Value_Clause,
) {
	if entity == nil || clause == nil || clause.expr == nil {
		return
	}
	payload, payload_ok := entity.payload.(^Entity_Constant_Payload)
	if !payload_ok || payload == nil {
		return
	}
	if lit, lit_ok := clause.expr.derived_expr.(^ast.Literal_Expr); lit_ok {
		if text, text_ok := checker_literal_text_value(lit.value); text_ok {
			value := new(Constant_Text_Value, ctx.project.allocator)
			value.value = strings.clone(text, ctx.project.allocator)
			payload.constant_value = value
			return
		}
		if checker_literal_is_integer(lit.value) {
			if parsed, parse_ok := strconv.parse_int(lit.value, 10); parse_ok {
				value := new(Constant_Integer_Value, ctx.project.allocator)
				value.value = i64(parsed)
				payload.constant_value = value
				return
			}
		}
	}
}

checker_literal_text_value :: proc(value: string) -> (string, bool) {
	if len(value) < 2 {
		return "", false
	}
	if (value[0] == '\'' && value[len(value) - 1] == '\'') ||
	   (value[0] == '`' && value[len(value) - 1] == '`') {
		return value[1:len(value) - 1], true
	}
	return "", false
}

checker_check_type_decl :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	decl: ^Decl_Info,
	named_type: ^Type,
) {
	if named_type != nil {
		entity.type = named_type
		return
	}
	if decl.type_clause != nil {
		base := checker_check_decl_type_clause(ctx, entity, decl.type_clause, decl.occurs)
		if base == nil {
			base = project_type_unknown(ctx.project)
		}
		entity.type = project_type_named(ctx.project, entity.name, entity, base)
		if payload, ok := entity.payload.(^Entity_Type_Name_Payload); ok && payload != nil {
			payload.is_alias = true
			payload.underlying = base
			payload.original_type = entity.type
		}
	} else if entity.type == nil {
		entity.type = project_type_named(ctx.project, entity.name, entity, project_type_unknown(ctx.project))
	}
}

checker_check_routine_decl :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) {
	payload, ok := entity.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil)
	assert(payload.signature_scope != nil && payload.body_scope != nil)
	checker_prepare_oop_routine_signature(ctx, entity)
	checker_check_routine_exception_type_refs(ctx, entity, payload)
	if entity.type == nil || entity.type.kind != .Routine {
		entity.type = project_type_routine(ctx.project, payload.signature_scope)
	}
	entity.type.routine.parameters = payload.parameters
	entity.type.routine.exceptions = payload.exceptions
	entity.type.base = checker_routine_result_type(ctx, payload)
	body := checker_routine_body_from_decl(decl)
	if event := checker_event_block_from_decl(decl); event != nil {
		header_ctx := ctx^
		header_ctx.scope = payload.body_scope
		header_ctx.current_routine = entity
		header_ctx.current_signature = entity.type
		checker_check_event_block_header(&header_ctx, event)
	}
	if len(body) > 0 {
		body_ctx := ctx^
		if payload.implementation_unit != nil {
			body_ctx.file = payload.implementation_unit
		}
		body_ctx.scope = payload.body_scope
		body_ctx.current_routine = entity
		body_ctx.current_signature = entity.type
		checker_check_stmt_list(&body_ctx, body)
	}
}

checker_event_block_from_decl :: proc(decl: ^Decl_Info) -> ^ast.Event_Block_Stmt {
	if decl == nil || decl.decl_node == nil {
		return nil
	}
	if event, ok := decl.decl_node.derived.(^ast.Event_Block_Stmt); ok {
		return event
	}
	return nil
}

checker_check_event_block_header :: proc(ctx: ^Checker_Context, stmt: ^ast.Event_Block_Stmt) {
	target, ok := checker_event_block_semantic_target(stmt)
	if !ok || target.text == "" {
		return
	}
	interned := project_intern_lower_ascii(ctx.project, target.text)
	if interned == "" {
		return
	}
	if _, entity, found := checker_lookup_reference(ctx, .Value, interned); found {
		checker_add_entity_use_at_range(ctx, &stmt.node.stmt_base, entity, target.range)
		checker_check_entity_for_operand(ctx, entity)
		return
	}
	checker_add_unresolved_candidate(
		ctx,
		interned,
		.Value,
		.Global_Symbol,
		.Identifier,
		.Unresolved_Reference,
		target.range,
		&stmt.node.stmt_base,
	)
}

checker_event_block_semantic_target :: proc(stmt: ^ast.Event_Block_Stmt) -> (ast.Token_Text, bool) {
	#partial switch stmt.addition {
	case .Selection_Screen_On,
	     .Selection_Screen_On_End_Of,
	     .Selection_Screen_On_Help_Request_For,
	     .Selection_Screen_On_Value_Request_For,
	     .Selection_Screen_On_Radiobutton_Group,
	     .Selection_Screen_On_Block:
		return stmt.target, stmt.target.text != ""
	}
	return {}, false
}

checker_check_routine_exception_type_refs :: proc(
	ctx: ^Checker_Context,
	routine: ^Entity,
	payload: ^Entity_Routine_Payload,
) {
	assert(routine != nil && payload != nil)
	for type_ref in payload.exception_type_refs {
		_, _ = checker_type_from_ref_data(
			ctx,
			type_ref,
			routine.node,
			preferred_external_kind = .Class,
		)
	}
}

checker_routine_result_type :: proc(ctx: ^Checker_Context, payload: ^Entity_Routine_Payload) -> ^Type {
	assert(payload != nil)
	for param in payload.parameters {
		param_payload, ok := param.payload.(^Entity_Variable_Payload)
		assert(ok && param_payload != nil)
		if param_payload.section != .Method_Returning && param_payload.section != .Method_Receiving {
			continue
		}
		checker_check_entity_decl(ctx, param)
		return param.type if param.type != nil else project_type_unknown(ctx.project)
	}
	return nil
}

checker_scope_kind_for_routine :: proc(kind: Entity_Kind) -> Scope_Kind {
	#partial switch kind {
	case .Form:
		return .Form
	case .Module:
		return .Module
	case .Event:
		return .Event
	case .Method:
		return .Method
	case:
		unreachable()
	}
	return .Method
}

checker_check_object_decl :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) {
	_ = decl
	payload, ok := entity.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	if payload.definition_scope == nil {
		scope_kind := Scope_Kind.Class if entity.kind == .Class else Scope_Kind.Interface
		payload.definition_scope = checker_create_scope(ctx.checker, entity.scope, scope_kind, owner = entity)
	}
	if entity.type == nil {
		entity.type = project_type_class_or_interface(ctx.project, entity.name, entity, entity.kind)
	}
	checker_check_object_semantics(ctx, entity)
	checker_check_object_body_oop_load_stmts(ctx, entity, payload)
}

checker_check_metadata_decl :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) {
	if entity.kind == .Alias {
		checker_check_oop_alias_decl(ctx, entity, decl)
		return
	}
	if entity.kind == .Field {
		if typ := checker_check_field_type(ctx, entity, decl); typ != nil {
			entity.type = typ
		}
	} else if typ := checker_check_decl_type_clause(ctx, entity, decl.type_clause, decl.occurs); typ != nil {
		entity.type = typ
	}
	checker_check_value_clause(ctx, decl.value_clause)
	checker_check_default_expr(ctx, decl.default_expr)
	if entity.type == nil {
		entity.type = project_type_unknown(ctx.project)
	}
}

checker_check_value_clause :: proc(ctx: ^Checker_Context, clause: ^ast.Value_Clause) {
	if clause == nil {
		return
	}
	checker_check_expr(ctx, clause.expr)
}

checker_check_default_expr :: proc(ctx: ^Checker_Context, expr: ^ast.Expr) {
	if expr == nil {
		return
	}
	checker_check_expr(ctx, expr)
}

checker_entity_tracks_type_path :: proc(entity: ^Entity) -> bool {
	#partial switch entity.kind {
	case .Variable, .Constant, .Enum_Member, .Type_Def, .Field:
		return true
	case:
		return false
	}
}

checker_type_path_contains :: proc(ctx: ^Checker_Context, entity: ^Entity) -> bool {
	for item in ctx.type_path {
		if item == entity {
			return true
		}
	}
	return false
}

checker_routine_body_from_decl :: proc(decl: ^Decl_Info) -> [dynamic]^ast.Stmt {
	if decl == nil || decl.decl_node == nil {
		return nil
	}
	#partial switch n in decl.decl_node.derived {
	case ^ast.Form_Decl:
		return n.body
	case ^ast.Method_Decl:
		return n.body
	case ^ast.Function_Decl:
		return n.body
	case ^ast.Module_Decl:
		return n.body
	case ^ast.Event_Block_Stmt:
		return n.body
	}
	return nil
}

checker_form_parameter_section_from_ast :: proc(section: ast.Form_Parameter_Section) -> Entity_Parameter_Section {
	#partial switch section {
	case .Tables:
		return .Form_Tables
	case .Using:
		return .Form_Using
	case .Changing:
		return .Form_Changing
	}
	return .None
}

checker_function_parameter_section_from_ast :: proc(
	section: ast.Function_Parameter_Section,
) -> Entity_Parameter_Section {
	#partial switch section {
	case .Importing:
		return .Function_Importing
	case .Exporting:
		return .Function_Exporting
	case .Changing:
		return .Function_Changing
	case .Tables:
		return .Function_Tables
	}
	return .None
}

checker_oop_parameter_section_from_ast :: proc(
	section: ast.Oop_Signature_Kind,
	routine_kind: Entity_Kind,
) -> (Entity_Parameter_Section, bool) {
	if routine_kind == .Event {
		if section == .Exporting {
			return .Method_Exporting, true
		}
		return .None, false
	}
	#partial switch section {
	case .Importing:
		return .Method_Importing, true
	case .Exporting:
		return .Method_Exporting, true
	case .Changing:
		return .Method_Changing, true
	case .Receiving:
		return .Method_Receiving, true
	case .Returning:
		return .Method_Returning, true
	case:
	}
	return .None, false
}

checker_parameter_passing_from_ast :: proc(passing: ast.Parameter_Passing_Kind) -> Entity_Parameter_Passing {
	switch passing {
	case .Direct:
		return .Direct
	case .Value:
		return .Value
	case .Reference:
		return .Reference
	}
	return .None
}

checker_visibility_from_ast :: proc(visibility: ast.Oop_Visibility, fallback: Visibility) -> Visibility {
	switch visibility {
	case .Public:
		return .Public
	case .Protected:
		return .Protected
	case .Private:
		return .Private
	case .Unspecified:
	}
	return fallback
}

checker_oop_method_entity_name :: proc(member: ast.Oop_Member_Clause) -> string {
	if member.qualifier.text != "" {
		return member.name.text
	}
	if member.member_name.text != "" {
		return member.member_name.text
	}
	return checker_method_entity_name(member.name.text)
}

checker_method_entity_name :: proc(name: string) -> string {
	for i := len(name) - 1; i >= 0; i -= 1 {
		if name[i] == '~' {
			if i > 0 && i + 1 < len(name) {
				return name[i + 1:]
			}
			return name
		}
	}
	return name
}

checker_expr_name :: proc(expr: ^ast.Expr) -> (string, Range, bool) {
	if expr == nil {
		return "", Range{}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name, n.range, n.name != ""
	case ^ast.Type_Ref_Expr:
		if n.raw_operand {
			return "", Range{}, false
		}
		return n.name.text, n.name.range, n.name.text != ""
	case ^ast.Literal_Expr:
		return n.value, n.range, n.value != ""
	}
	return "", Range{}, false
}
