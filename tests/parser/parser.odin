package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:testing"

@(test)
basic_inline_data_decl_test :: proc(t: ^testing.T) {
    file := ast.new(ast.File, {})
    file.fullpath = "test.abap"
    file.src = `DATA(lv_value) = 1.`
    p: parser.Parser
    parser.parse_file(&p, file)
    testing.expect(t, len(file.syntax_errors) == 0)
    testing.expect(t, len(file.decls) == 1)
    
    testing.expect(t, file.decls[0].derived_stmt != nil)
    decl, decl_ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
    testing.expect(t, decl_ok)
    testing.expect(t, decl.ident != nil)
    testing.expect(t, decl.ident.name == "lv_value")

    testing.expect(t, decl.value != nil)
    expr, expr_ok := decl.value.derived_expr.(^ast.Basic_Lit)
    testing.expect(t, expr_ok)
    testing.expect(t, expr.tok.lit == "1")
}