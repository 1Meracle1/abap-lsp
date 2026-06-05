package abap_frontend_ast

import "src:tokenizer"

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

@(test)
semantic_fields_clone_without_affecting_print :: proc(t: ^testing.T) {
	r := tokenizer.text_range(0, 1)
	provider := Provider_Handle{kind = .File, id = Provider_Id(1), revision = 2}

	file := new(File, r, context.allocator)
	file.stmts = make([dynamic]^Stmt, 0, 1, context.allocator)
	file.sem = Node_Semantic {
		scope = Scope_Handle{provider = provider, id = Scope_Id(3)},
		flags = {.Has_Scope},
	}

	name := new(Ident_Expr, r, context.allocator)
	name.name = "lv_value"
	name.sem = Node_Semantic {
		scope = Scope_Handle{provider = provider, id = Scope_Id(3)},
		entity = Entity_Handle{provider = provider, id = Entity_Id(4)},
		decl = Decl_Handle{provider = provider, id = Decl_Id(10)},
		use = Use_Handle{file = File_Id(5), id = Use_Id(6), revision = 7},
		tav = Type_And_Value {
			type = Type_Handle{provider = provider, id = Type_Id(8)},
			mode = .Variable,
			value = Exact_Value_Id(9),
			flags = {.Assignable, .High_Confidence},
		},
		flags = {.Has_Scope, .Has_Entity, .Has_Decl, .Has_Use, .Has_Type_And_Value, .Assignable},
	}

	stmt := new(Expr_Stmt, r, context.allocator)
	stmt.expr = name
	append(&file.stmts, stmt)

	testing.expect_value(t, print_node(file, context.allocator), "lv_value.")

	cloned := clone_node(file, context.allocator)
	cloned_file := cloned.derived.(^File)
	cloned_stmt := cloned_file.stmts[0].derived_stmt.(^Expr_Stmt)
	cloned_name := cloned_stmt.expr.derived_expr.(^Ident_Expr)

	testing.expect_value(t, print_node(cloned, context.allocator), "lv_value.")
	testing.expect(t, .Has_Scope in cloned_file.sem.flags)
	testing.expect(t, .Has_Type_And_Value in cloned_name.sem.flags)
	testing.expect(t, .Assignable in cloned_name.sem.tav.flags)
	testing.expect_value(t, cloned_name.sem.entity.id, Entity_Id(4))
	testing.expect_value(t, cloned_name.sem.decl.id, Decl_Id(10))
	testing.expect_value(t, cloned_name.sem.use.id, Use_Id(6))
	testing.expect_value(t, cloned_name.sem.tav.type.id, Type_Id(8))
	testing.expect_value(t, cloned_name.sem.tav.mode, Addressing_Mode.Variable)
}
