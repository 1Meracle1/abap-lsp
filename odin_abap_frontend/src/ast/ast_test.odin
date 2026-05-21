package abap_frontend_ast

import "../tokenizer"

import "core:testing"

test_count_visit :: proc(v: ^Visitor, node: ^Node) -> ^Visitor {
	count := cast(^int)v.data
	count^ += 1
	return v
}

@(test)
new_clone_walk_and_print_cover_surface_nodes :: proc(t: ^testing.T) {
	r := tokenizer.text_range(0, 1)
	file := new(File, r, context.allocator)
	file.stmts = make([dynamic]^Stmt, 0, 1, context.allocator)

	object_name := new(Ident_Expr, r, context.allocator)
	object_name.name = "lv_value"
	object := new(Host_Expr, r, context.allocator)
	object.value = object_name
	stmt := new(Expr_Stmt, r, context.allocator)
	stmt.expr = object
	append(&file.stmts, stmt)

	testing.expect(t, object.derived.(^Host_Expr) == object)
	testing.expect(t, object.derived_expr.(^Host_Expr) == object)
	testing.expect_value(t, print_node(file, context.allocator), "@lv_value.")

	cloned := clone_node(file, context.allocator)
	cloned_file := cloned.derived.(^File)
	testing.expect(t, cloned_file != file)
	testing.expect_value(t, print_node(cloned, context.allocator), print_node(file, context.allocator))

	count := 0
	visitor := Visitor{visit = test_count_visit, data = &count}
	walk(&visitor, file)
	testing.expect(t, count >= 4)
}
