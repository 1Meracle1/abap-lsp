package lang_ast

// find_node_at_offset traverses the AST to find the most specific node
// that contains the given offset.
find_node_at_offset :: proc(node: ^Node, offset: int) -> ^Node {
	if node == nil {
		return nil
	}

	// Check if the offset is within the node's range
	if offset < node.range.start || offset > node.range.end {
		return nil
	}

	#partial switch n in node.derived {
	case ^Program:
		for _, file in n.files {
			if res := find_node_at_offset(&file.node, offset); res != nil {
				return res
			}
		}

	case ^File:
		for decl in n.decls {
			if res := find_node_at_offset(&decl.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Data_Inline_Decl:
		if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
			return res
		}
		if res := find_node_at_offset(&n.value.expr_base, offset); res != nil {
			return res
		}

	case ^Data_Typed_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		if res := find_node_at_offset(&n.typed.expr_base, offset); res != nil {
			return res
		}
		if n.value != nil {
			if res := find_node_at_offset(&n.value.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Data_Typed_Chain_Decl:
		for child in n.decls {
			if child.ident != nil {
				if res := find_node_at_offset(&child.ident.expr_base, offset); res != nil {
					return res
				}
			}
			if res := find_node_at_offset(&child.typed.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Types_Decl:
		if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
			return res
		}
		if res := find_node_at_offset(&n.typed.expr_base, offset); res != nil {
			return res
		}
		if n.length != nil {
			if res := find_node_at_offset(&n.length.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Types_Chain_Decl:
		for child in n.decls {
			if res := find_node_at_offset(&child.ident.expr_base, offset); res != nil {
				return res
			}
			if res := find_node_at_offset(&child.typed.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Types_Struct_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		for comp in n.components {
			if res := find_node_at_offset(&comp.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Const_Decl:
		if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
			return res
		}
		if res := find_node_at_offset(&n.typed.expr_base, offset); res != nil {
			return res
		}
		if n.value != nil {
			if res := find_node_at_offset(&n.value.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Const_Chain_Decl:
		for child in n.decls {
			if res := find_node_at_offset(&child.ident.expr_base, offset); res != nil {
				return res
			}
			if res := find_node_at_offset(&child.typed.expr_base, offset); res != nil {
				return res
			}
			if child.value != nil {
				if res := find_node_at_offset(&child.value.expr_base, offset); res != nil {
					return res
				}
			}
		}

	case ^Const_Struct_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		for comp in n.components {
			if res := find_node_at_offset(&comp.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Data_Struct_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		for comp in n.components {
			if res := find_node_at_offset(&comp.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Ident:
		return node

	case ^Basic_Lit:
		return node

	case ^Unary_Expr:
		if res := find_node_at_offset(&n.expr.expr_base, offset); res != nil {
			return res
		}

	case ^Binary_Expr:
		if res := find_node_at_offset(&n.left.expr_base, offset); res != nil {
			return res
		}
		if res := find_node_at_offset(&n.right.expr_base, offset); res != nil {
			return res
		}

	case ^Paren_Expr:
		if res := find_node_at_offset(&n.expr.expr_base, offset); res != nil {
			return res
		}

	case ^Selector_Expr:
		if res := find_node_at_offset(&n.expr.expr_base, offset); res != nil {
			return res
		}
		if res := find_node_at_offset(&n.field.expr_base, offset); res != nil {
			return res
		}

	case ^Index_Expr:
		if res := find_node_at_offset(&n.expr.expr_base, offset); res != nil {
			return res
		}
		if res := find_node_at_offset(&n.index.expr_base, offset); res != nil {
			return res
		}

	case ^Call_Expr:
		if res := find_node_at_offset(&n.expr.expr_base, offset); res != nil {
			return res
		}
		for arg in n.args {
			if res := find_node_at_offset(&arg.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Constructor_Expr:
		if n.type_expr != nil {
			if res := find_node_at_offset(&n.type_expr.expr_base, offset); res != nil {
				return res
			}
		}
		for arg in n.args {
			if res := find_node_at_offset(&arg.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Table_Type:
		if n.elem != nil {
			if res := find_node_at_offset(&n.elem.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Ref_Type:
		if n.target != nil {
			if res := find_node_at_offset(&n.target.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Line_Type:
		if n.table != nil {
			if res := find_node_at_offset(&n.table.expr_base, offset); res != nil {
				return res
			}
		}

	case ^New_Expr:
		if n.type_expr != nil {
			if res := find_node_at_offset(&n.type_expr.expr_base, offset); res != nil {
				return res
			}
		}
		for arg in n.args {
			if res := find_node_at_offset(&arg.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Named_Arg:
		if n.name != nil {
			if res := find_node_at_offset(&n.name.expr_base, offset); res != nil {
				return res
			}
		}
		if n.value != nil {
			if res := find_node_at_offset(&n.value.expr_base, offset); res != nil {
				return res
			}
		}

	case ^For_Expr:
		if n.var_name != nil {
			if res := find_node_at_offset(&n.var_name.expr_base, offset); res != nil {
				return res
			}
		}
		if n.itab != nil {
			if res := find_node_at_offset(&n.itab.expr_base, offset); res != nil {
				return res
			}
		}
		if n.where_cond != nil {
			if res := find_node_at_offset(&n.where_cond.expr_base, offset); res != nil {
				return res
			}
		}
		// Check result_args (new field)
		for arg in n.result_args {
			if res := find_node_at_offset(&arg.expr_base, offset); res != nil {
				return res
			}
		}
		// Also check legacy result_expr for backward compatibility
		if n.result_expr != nil && len(n.result_args) == 0 {
			if res := find_node_at_offset(&n.result_expr.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Value_Row_Expr:
		for arg in n.args {
			if res := find_node_at_offset(&arg.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Expr_Stmt:
		if res := find_node_at_offset(&n.expr.expr_base, offset); res != nil {
			return res
		}

	case ^Assign_Stmt:
		for expr in n.lhs {
			if res := find_node_at_offset(&expr.expr_base, offset); res != nil {
				return res
			}
		}
		for expr in n.rhs {
			if res := find_node_at_offset(&expr.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Block_Stmt:
		if n.label != nil {
			if res := find_node_at_offset(&n.label.expr_base, offset); res != nil {
				return res
			}
		}
		for stmt in n.stmts {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^If_Stmt:
		if n.cond != nil {
			if res := find_node_at_offset(&n.cond.expr_base, offset); res != nil {
				return res
			}
		}
		for stmt in n.body {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}
		for branch in n.elseif_branches {
			if res := find_node_at_offset(&branch.node, offset); res != nil {
				return res
			}
		}
		for stmt in n.else_body {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Elseif_Branch:
		if n.cond != nil {
			if res := find_node_at_offset(&n.cond.expr_base, offset); res != nil {
				return res
			}
		}
		for stmt in n.body {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Predicate_Expr:
		if n.expr != nil {
			if res := find_node_at_offset(&n.expr.expr_base, offset); res != nil {
				return res
			}
		}

	case ^String_Template_Expr:
		for part in n.parts {
			if part.is_expr && part.expr != nil {
				if res := find_node_at_offset(&part.expr.expr_base, offset); res != nil {
					return res
				}
			}
		}

	case ^Return_Stmt:
		for expr in n.results {
			if res := find_node_at_offset(&expr.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Modify_Screen_Stmt:
		return node

	case ^Leave_Program_Stmt:
		return node

	case ^Set_Stmt:
		if n.expr != nil {
			if res := find_node_at_offset(&n.expr.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Case_Stmt:
		if n.expr != nil {
			if res := find_node_at_offset(&n.expr.expr_base, offset); res != nil {
				return res
			}
		}
		for branch in n.branches {
			if res := find_node_at_offset(&branch.expr.expr_base, offset); res != nil {
				return res
			}
			for stmt in branch.body {
				if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
					return res
				}
			}
		}

	case ^While_Stmt:
		if n.cond != nil {
			if res := find_node_at_offset(&n.cond.expr_base, offset); res != nil {
				return res
			}
		}
		for stmt in n.body {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Loop_Stmt:
		if n.itab != nil {
			if res := find_node_at_offset(&n.itab.expr_base, offset); res != nil {
				return res
			}
		}
		if n.into_target != nil {
			if res := find_node_at_offset(&n.into_target.expr_base, offset); res != nil {
				return res
			}
		}
		if n.assigning_target != nil {
			if res := find_node_at_offset(&n.assigning_target.expr_base, offset); res != nil {
				return res
			}
		}
		if n.from_expr != nil {
			if res := find_node_at_offset(&n.from_expr.expr_base, offset); res != nil {
				return res
			}
		}
		if n.to_expr != nil {
			if res := find_node_at_offset(&n.to_expr.expr_base, offset); res != nil {
				return res
			}
		}
		if n.where_cond != nil {
			if res := find_node_at_offset(&n.where_cond.expr_base, offset); res != nil {
				return res
			}
		}
		if n.group_var != nil {
			if res := find_node_at_offset(&n.group_var.expr_base, offset); res != nil {
				return res
			}
		}
		for stmt in n.body {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Clear_Stmt:
		for expr in n.exprs {
			if res := find_node_at_offset(&expr.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Message_Stmt:
		if n.msg_expr != nil {
			if res := find_node_at_offset(&n.msg_expr.expr_base, offset); res != nil {
				return res
			}
		}
		if n.msg_type != nil {
			if res := find_node_at_offset(&n.msg_type.expr_base, offset); res != nil {
				return res
			}
		}
		if n.display_like != nil {
			if res := find_node_at_offset(&n.display_like.expr_base, offset); res != nil {
				return res
			}
		}
		for arg in n.with_args {
			if res := find_node_at_offset(&arg.expr_base, offset); res != nil {
				return res
			}
		}
		if n.into_target != nil {
			if res := find_node_at_offset(&n.into_target.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Insert_Stmt:
		if n.value_expr != nil {
			if res := find_node_at_offset(&n.value_expr.expr_base, offset); res != nil {
				return res
			}
		}
		if n.target != nil {
			if res := find_node_at_offset(&n.target.expr_base, offset); res != nil {
				return res
			}
		}
		if n.source != nil {
			if res := find_node_at_offset(&n.source.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Sort_Stmt:
		if n.itab != nil {
			if res := find_node_at_offset(&n.itab.expr_base, offset); res != nil {
				return res
			}
		}
		for col in n.cols_by {
			if col.col != nil {
				if res := find_node_at_offset(&n.itab.expr_base, offset); res != nil {
					return res
				}
			}
		}

	case ^Append_Stmt:
		if n.source != nil {
			if res := find_node_at_offset(&n.source.expr_base, offset); res != nil {
				return res
			}
		}
		if n.target != nil {
			if res := find_node_at_offset(&n.target.expr_base, offset); res != nil {
				return res
			}
		}
		if n.assigning_target != nil {
			if res := find_node_at_offset(&n.assigning_target.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Read_Table_Stmt:
		if n.itab != nil {
			if res := find_node_at_offset(&n.itab.expr_base, offset); res != nil {
				return res
			}
		}
		if n.index_expr != nil {
			if res := find_node_at_offset(&n.index_expr.expr_base, offset); res != nil {
				return res
			}
		}
		if n.using_key != nil {
			if res := find_node_at_offset(&n.using_key.expr_base, offset); res != nil {
				return res
			}
		}
		if n.into_target != nil {
			if res := find_node_at_offset(&n.into_target.expr_base, offset); res != nil {
				return res
			}
		}
		if n.assigning_target != nil {
			if res := find_node_at_offset(&n.assigning_target.expr_base, offset); res != nil {
				return res
			}
		}
		if n.key != nil {
			for comp in n.key.components {
				if comp.name != nil {
					if res := find_node_at_offset(&comp.name.expr_base, offset); res != nil {
						return res
					}
				}
				if comp.value != nil {
					if res := find_node_at_offset(&comp.value.expr_base, offset); res != nil {
						return res
					}
				}
			}
		}

	case ^Authority_Check_Stmt:
		if n.object != nil {
			if res := find_node_at_offset(&n.object.expr_base, offset); res != nil {
				return res
			}
		}
		if n.user != nil {
			if res := find_node_at_offset(&n.user.expr_base, offset); res != nil {
				return res
			}
		}
		for id in n.ids {
			if id.id != nil {
				if res := find_node_at_offset(&id.id.expr_base, offset); res != nil {
					return res
				}
			}
			if id.field != nil {
				if res := find_node_at_offset(&id.field.expr_base, offset); res != nil {
					return res
				}
			}
		}

	case ^Delete_Stmt:
		if n.target != nil {
			if res := find_node_at_offset(&n.target.expr_base, offset); res != nil {
				return res
			}
		}
		if n.where_cond != nil {
			if res := find_node_at_offset(&n.where_cond.expr_base, offset); res != nil {
				return res
			}
		}
		if n.index_expr != nil {
			if res := find_node_at_offset(&n.index_expr.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Condense_Stmt:
		if n.text != nil {
			if res := find_node_at_offset(&n.text.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Call_Function_Stmt:
		if n.func_name != nil {
			if res := find_node_at_offset(&n.func_name.expr_base, offset); res != nil {
				return res
			}
		}
		if n.destination != nil {
			if res := find_node_at_offset(&n.destination.expr_base, offset); res != nil {
				return res
			}
		}
		for param in n.exporting {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}
		for param in n.importing {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}
		for param in n.tables {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}
		for param in n.changing {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}
		for param in n.exceptions {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}

	case ^Call_Function_Param:
		if n.name != nil {
			if res := find_node_at_offset(&n.name.expr_base, offset); res != nil {
				return res
			}
		}
		if n.value != nil {
			if res := find_node_at_offset(&n.value.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Field_Symbol_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		if n.typed != nil {
			if res := find_node_at_offset(&n.typed.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Controls_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		if n.screen_dynnr != nil {
			if res := find_node_at_offset(&n.screen_dynnr.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Controls_Chain_Decl:
		for decl in n.decls {
			if res := find_node_at_offset(&decl.decl_base.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Form_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		for param in n.tables_params {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}
		for param in n.using_params {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}
		for param in n.changing_params {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}
		for stmt in n.body {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Form_Param:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		if n.typed != nil {
			if res := find_node_at_offset(&n.typed.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Class_Def_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		if n.inheriting_from != nil {
			if res := find_node_at_offset(&n.inheriting_from.expr_base, offset); res != nil {
				return res
			}
		}
		for section in n.sections {
			if res := find_node_at_offset(&section.node, offset); res != nil {
				return res
			}
		}

	case ^Class_Impl_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		for method in n.methods {
			if res := find_node_at_offset(&method.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Class_Section:
		for t in n.types {
			if res := find_node_at_offset(&t.stmt_base, offset); res != nil {
				return res
			}
		}
		for d in n.data {
			if res := find_node_at_offset(&d.stmt_base, offset); res != nil {
				return res
			}
		}
		for m in n.methods {
			if res := find_node_at_offset(&m.stmt_base, offset); res != nil {
				return res
			}
		}
		for i in n.interfaces {
			if res := find_node_at_offset(&i.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Method_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		for param in n.params {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}
		for exc in n.raising {
			if res := find_node_at_offset(&exc.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Method_Param:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		if n.typed != nil {
			if res := find_node_at_offset(&n.typed.expr_base, offset); res != nil {
				return res
			}
		}
		if n.default != nil {
			if res := find_node_at_offset(&n.default.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Method_Impl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		for stmt in n.body {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Attr_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		if n.typed != nil {
			if res := find_node_at_offset(&n.typed.expr_base, offset); res != nil {
				return res
			}
		}
		if n.value != nil {
			if res := find_node_at_offset(&n.value.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Interfaces_Decl:
		for name in n.names {
			if res := find_node_at_offset(&name.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Interface_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		for method in n.methods {
			if res := find_node_at_offset(&method.stmt_base, offset); res != nil {
				return res
			}
		}
		for t in n.types {
			if res := find_node_at_offset(&t.stmt_base, offset); res != nil {
				return res
			}
		}
		for d in n.data {
			if res := find_node_at_offset(&d.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Report_Decl:
		if n.name != nil {
			if res := find_node_at_offset(&n.name.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Include_Decl:
		if n.name != nil {
			if res := find_node_at_offset(&n.name.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Event_Block:
		for stmt in n.body {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Call_Screen_Stmt:
		if n.screen_no != nil {
			if res := find_node_at_offset(&n.screen_no.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Module_Decl:
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		for stmt in n.body {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Select_Stmt:
		// Check fields
		for field in n.fields {
			if res := find_node_at_offset(&field.expr_base, offset); res != nil {
				return res
			}
		}
		// Check FROM table
		if n.from_table != nil {
			if res := find_node_at_offset(&n.from_table.expr_base, offset); res != nil {
				return res
			}
		}
		// Check FROM alias
		if n.from_alias != nil {
			if res := find_node_at_offset(&n.from_alias.expr_base, offset); res != nil {
				return res
			}
		}
		// Check joins
		for join in n.joins {
			if res := find_node_at_offset(&join.node, offset); res != nil {
				return res
			}
		}
		// Check INTO target
		if n.into_target != nil {
			if res := find_node_at_offset(&n.into_target.expr_base, offset); res != nil {
				return res
			}
		}
		// Check WHERE condition
		if n.where_cond != nil {
			if res := find_node_at_offset(&n.where_cond.expr_base, offset); res != nil {
				return res
			}
		}
		// Check ORDER BY
		for col in n.order_by {
			if res := find_node_at_offset(&col.expr_base, offset); res != nil {
				return res
			}
		}
		// Check GROUP BY
		for col in n.group_by {
			if res := find_node_at_offset(&col.expr_base, offset); res != nil {
				return res
			}
		}
		// Check HAVING condition
		if n.having_cond != nil {
			if res := find_node_at_offset(&n.having_cond.expr_base, offset); res != nil {
				return res
			}
		}
		// Check FOR ALL ENTRIES
		if n.for_all_entries != nil {
			if res := find_node_at_offset(&n.for_all_entries.expr_base, offset); res != nil {
				return res
			}
		}
		// Check UP TO ROWS
		if n.up_to_rows != nil {
			if res := find_node_at_offset(&n.up_to_rows.expr_base, offset); res != nil {
				return res
			}
		}
		// Check body statements
		for stmt in n.body {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Select_Join:
		if n.table != nil {
			if res := find_node_at_offset(&n.table.expr_base, offset); res != nil {
				return res
			}
		}
		if n.alias != nil {
			if res := find_node_at_offset(&n.alias.expr_base, offset); res != nil {
				return res
			}
		}
		if n.on_cond != nil {
			if res := find_node_at_offset(&n.on_cond.expr_base, offset); res != nil {
				return res
			}
		}
	}

	return node
}

// find_enclosing_form finds the Form_Decl that contains the given offset, if any.
// Returns nil if the offset is not inside any form.
find_enclosing_form :: proc(file: ^File, offset: int) -> ^Form_Decl {
	if file == nil {
		return nil
	}

	for decl in file.decls {
		if form, ok := decl.derived_stmt.(^Form_Decl); ok {
			// Check if offset is within this form's range
			if offset >= form.range.start && offset <= form.range.end {
				return form
			}
		}
	}

	return nil
}

// find_enclosing_class_def finds the Class_Def_Decl that contains the given offset, if any.
find_enclosing_class_def :: proc(file: ^File, offset: int) -> ^Class_Def_Decl {
	if file == nil {
		return nil
	}

	for decl in file.decls {
		if class, ok := decl.derived_stmt.(^Class_Def_Decl); ok {
			if offset >= class.range.start && offset <= class.range.end {
				return class
			}
		}
	}

	return nil
}

// find_enclosing_class_impl finds the Class_Impl_Decl that contains the given offset, if any.
find_enclosing_class_impl :: proc(file: ^File, offset: int) -> ^Class_Impl_Decl {
	if file == nil {
		return nil
	}

	for decl in file.decls {
		if class, ok := decl.derived_stmt.(^Class_Impl_Decl); ok {
			if offset >= class.range.start && offset <= class.range.end {
				return class
			}
		}
	}

	return nil
}

// find_enclosing_interface finds the Interface_Decl that contains the given offset, if any.
find_enclosing_interface :: proc(file: ^File, offset: int) -> ^Interface_Decl {
	if file == nil {
		return nil
	}

	for decl in file.decls {
		if iface, ok := decl.derived_stmt.(^Interface_Decl); ok {
			if offset >= iface.range.start && offset <= iface.range.end {
				return iface
			}
		}
	}

	return nil
}

// find_enclosing_method_impl finds the Method_Impl that contains the given offset, if any.
find_enclosing_method_impl :: proc(file: ^File, offset: int) -> ^Method_Impl {
	if file == nil {
		return nil
	}

	for decl in file.decls {
		if class_impl, ok := decl.derived_stmt.(^Class_Impl_Decl); ok {
			for method in class_impl.methods {
				if method_impl, mok := method.derived_stmt.(^Method_Impl); mok {
					if offset >= method_impl.range.start && offset <= method_impl.range.end {
						return method_impl
					}
				}
			}
		}
	}

	return nil
}

// find_enclosing_event_block finds the Event_Block that contains the given offset, if any.
find_enclosing_event_block :: proc(file: ^File, offset: int) -> ^Event_Block {
	if file == nil {
		return nil
	}

	for decl in file.decls {
		if event, ok := decl.derived_stmt.(^Event_Block); ok {
			if offset >= event.range.start && offset <= event.range.end {
				return event
			}
		}
	}

	return nil
}

// find_enclosing_module finds the Module_Decl that contains the given offset, if any.
find_enclosing_module :: proc(file: ^File, offset: int) -> ^Module_Decl {
	if file == nil {
		return nil
	}

	for decl in file.decls {
		if module, ok := decl.derived_stmt.(^Module_Decl); ok {
			if offset >= module.range.start && offset <= module.range.end {
				return module
			}
		}
	}

	return nil
}
