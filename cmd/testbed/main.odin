package main

import "../../pkg/lang/ast"
import "../../pkg/lang/parser"
import "core:fmt"

main :: proc() {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(lv_value) = 1.`
	p: parser.Parser
	parser.parse_file(&p, file)

	decl, decl_ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
	assert(decl_ok)
	assert(decl.ident.name == "lv_value")

	expr, expr_ok := decl.value.derived_expr.(^ast.Basic_Lit)
	assert(expr_ok)
	assert(expr.tok.lit == "1")
}
