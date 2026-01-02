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

	// Try to find a more specific child node
	// This depends on the exact node type
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

	case ^Types_Chain_Decl:
		for child in n.decls {
			if res := find_node_at_offset(&child.ident.expr_base, offset); res != nil {
				return res
			}
			if res := find_node_at_offset(&child.typed.expr_base, offset); res != nil {
				return res
			}
		}

	case ^Ident:
		// Leaf node, return self
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
		if n.body != nil {
			if res := find_node_at_offset(&n.body.stmt_base, offset); res != nil {
				return res
			}
		}
		if n.else_stmt != nil {
			if res := find_node_at_offset(&n.else_stmt.stmt_base, offset); res != nil {
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
		// Check form name
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		// Check TABLES parameters
		for param in n.tables_params {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}
		// Check USING parameters
		for param in n.using_params {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}
		// Check CHANGING parameters
		for param in n.changing_params {
			if res := find_node_at_offset(&param.node, offset); res != nil {
				return res
			}
		}
		// Check body statements
		for stmt in n.body {
			if res := find_node_at_offset(&stmt.stmt_base, offset); res != nil {
				return res
			}
		}

	case ^Form_Param:
		// Check parameter name
		if n.ident != nil {
			if res := find_node_at_offset(&n.ident.expr_base, offset); res != nil {
				return res
			}
		}
		// Check parameter type
		if n.typed != nil {
			if res := find_node_at_offset(&n.typed.expr_base, offset); res != nil {
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