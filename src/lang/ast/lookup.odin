package lang_ast

import "core:fmt"

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
		if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
			return res
		}
		if res := find_node_at_offset(&n.typed.expr_base, offset); res != nil {
			return res
		}

	case ^Data_Typed_Chain_Decl:
		for child in n.decls {
			if res := find_node_at_offset(&child.ident.expr_base, offset); res != nil {
				return res
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

	case ^Return_Stmt:
		for expr in n.results {
			if res := find_node_at_offset(&expr.expr_base, offset); res != nil {
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
