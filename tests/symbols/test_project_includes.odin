package tests_symbols

import "../../src/lang/ast"
import "../../src/lang/parser"
import "../../src/lang/symbols"
import "core:fmt"
import "core:strings"
import "core:testing"

// Test that symbols defined in main are visible in include file
@(test)
test_main_symbol_visible_in_include :: proc(t: ^testing.T) {
	// main.abap: Defines DATA: gv_main TYPE i. then INCLUDE inc.
	main_src := `REPORT my_report.
DATA gv_main TYPE i.
INCLUDE inc.
gv_main = gv_inc.`

	// inc.abap: Uses gv_main. Defines DATA: gv_inc TYPE i.
	inc_src := `DATA gv_inc TYPE i.
gv_inc = gv_main.`

	// Parse both files
	main_ast := ast.new(ast.File, {})
	main_ast.src = main_src
	main_ast.fullpath = "file:///test/main.abap"
	
	p1: parser.Parser
	parser.parse_file(&p1, main_ast)
	
	inc_ast := ast.new(ast.File, {})
	inc_ast.src = inc_src
	inc_ast.fullpath = "file:///test/inc.abap"
	
	p2: parser.Parser
	parser.parse_file(&p2, inc_ast)
	
	// Create include URI map
	include_uris := make(map[string]string)
	include_uris["inc"] = "file:///test/inc.abap"
	
	// Create include ASTs map for resolver
	include_asts := make(map[string]^ast.File)
	include_asts["inc"] = inc_ast
	
	// Resolve project - manually simulate the resolution
	result := symbols.resolve_project_files(
		main_ast,
		"file:///test/main.abap",
		proc(name: string) -> ^ast.File {
			// This is a simplified resolver for testing
			return nil // Will be handled by direct resolution below
		},
		include_uris,
	)
	
	if result == nil {
		testing.expect(t, false, "resolve_project_files returned nil")
		return
	}
	defer symbols.destroy_project_resolution_result(result)
	
	// For this test, we need to manually process since we can't use closures
	// Let's use the simpler approach: resolve manually with state cloning
	
	// Start fresh with manual resolution
	main_table := symbols.create_empty_symbol_table()
	
	// Process main file declarations until include
	// First: REPORT
	for decl in main_ast.decls {
		#partial switch d in decl.derived_stmt {
		case ^ast.Report_Decl:
			symbols.resolve_report_decl(main_table, d)
		case ^ast.Data_Typed_Decl:
			symbols.resolve_typed_decl(main_table, d, false, true)
		case ^ast.Include_Decl:
			// Clone state for include
			inc_table := symbols.clone_symbol_table(main_table)
			
			// Resolve include file
			for inc_decl in inc_ast.decls {
				#partial switch id in inc_decl.derived_stmt {
				case ^ast.Data_Typed_Decl:
					symbols.resolve_typed_decl(inc_table, id, false, true)
				}
			}
			
			// Check: gv_main should be visible in include (was cloned from main)
			_, gv_main_in_inc := inc_table.symbols["gv_main"]
			testing.expect(
				t,
				gv_main_in_inc,
				"gv_main should be visible in include file (inherited from main before INCLUDE)",
			)
			
			// Check: gv_inc was added in include
			_, gv_inc_in_inc := inc_table.symbols["gv_inc"]
			testing.expect(
				t,
				gv_inc_in_inc,
				"gv_inc should be defined in include file",
			)
			
			// Merge include symbols back into main
			symbols.merge_symbols_into(main_table, inc_table)
		}
	}
	
	// Check: gv_main should be in main table
	_, gv_main_in_main := main_table.symbols["gv_main"]
	testing.expect(
		t,
		gv_main_in_main,
		"gv_main should be in main symbol table",
	)
	
	// Check: gv_inc should now be visible in main (after include processing)
	_, gv_inc_in_main := main_table.symbols["gv_inc"]
	testing.expect(
		t,
		gv_inc_in_main,
		"gv_inc from include should be visible in main after INCLUDE statement",
	)
}

// Test that symbols defined in include are visible in main after INCLUDE
@(test)
test_include_symbol_visible_in_main_after_include :: proc(t: ^testing.T) {
	// This tests the "accumulation" aspect:
	// After processing INCLUDE inc, symbols from inc should be available
	
	main_src := `REPORT my_report.
DATA gv_before TYPE i.
INCLUDE inc.
DATA gv_after TYPE i VALUE gv_inc.`

	inc_src := `DATA gv_inc TYPE i VALUE 42.`

	// Parse both files
	main_ast := ast.new(ast.File, {})
	main_ast.src = main_src
	
	p1: parser.Parser
	parser.parse_file(&p1, main_ast)
	
	inc_ast := ast.new(ast.File, {})
	inc_ast.src = inc_src
	
	p2: parser.Parser
	parser.parse_file(&p2, inc_ast)
	
	// Manual resolution with state cloning
	main_table := symbols.create_empty_symbol_table()
	seen_include := false
	
	for decl in main_ast.decls {
		#partial switch d in decl.derived_stmt {
		case ^ast.Report_Decl:
			symbols.resolve_report_decl(main_table, d)
		case ^ast.Data_Typed_Decl:
			symbols.resolve_typed_decl(main_table, d, false, true)
			
			// After include, gv_inc should be available
			if seen_include {
				_, has_gv_inc := main_table.symbols["gv_inc"]
				testing.expect(
					t,
					has_gv_inc,
					"gv_inc should be available in main after INCLUDE",
				)
			}
		case ^ast.Include_Decl:
			// Clone and process include
			inc_table := symbols.clone_symbol_table(main_table)
			
			for inc_decl in inc_ast.decls {
				#partial switch id in inc_decl.derived_stmt {
				case ^ast.Data_Typed_Decl:
					symbols.resolve_typed_decl(inc_table, id, false, true)
				}
			}
			
			// Before merge, gv_before should be in inc_table (inherited)
			_, has_gv_before := inc_table.symbols["gv_before"]
			testing.expect(
				t,
				has_gv_before,
				"gv_before should be inherited by include",
			)
			
			// Merge back
			symbols.merge_symbols_into(main_table, inc_table)
			seen_include = true
		}
	}
	
	// Final checks
	testing.expect(t, seen_include, "Should have processed an include")
	
	_, has_gv_before := main_table.symbols["gv_before"]
	_, has_gv_inc := main_table.symbols["gv_inc"]
	_, has_gv_after := main_table.symbols["gv_after"]
	
	testing.expect(t, has_gv_before, "main table should have gv_before")
	testing.expect(t, has_gv_inc, "main table should have gv_inc after include")
	testing.expect(t, has_gv_after, "main table should have gv_after")
}

// Test clone_symbol_table creates independent copy
@(test)
test_clone_symbol_table_independence :: proc(t: ^testing.T) {
	// Create original table with some symbols
	original := symbols.create_empty_symbol_table()
	
	sym1 := symbols.Symbol{
		name = "var1",
		kind = .Variable,
	}
	symbols.add_symbol(original, sym1)
	
	// Clone
	cloned := symbols.clone_symbol_table(original)
	
	// Verify clone has the symbol
	_, has_var1 := cloned.symbols["var1"]
	testing.expect(t, has_var1, "cloned table should have var1")
	
	// Add new symbol to clone
	sym2 := symbols.Symbol{
		name = "var2",
		kind = .Variable,
	}
	symbols.add_symbol(cloned, sym2)
	
	// Verify clone has new symbol
	_, clone_has_var2 := cloned.symbols["var2"]
	testing.expect(t, clone_has_var2, "cloned table should have var2")
	
	// Verify original does NOT have the new symbol (independence)
	_, original_has_var2 := original.symbols["var2"]
	testing.expect(t, !original_has_var2, "original should NOT have var2 - tables should be independent")
	
	// Cleanup
	symbols.destroy_symbol_table(original)
	symbols.destroy_symbol_table(cloned)
}

// Test merge_symbols_into merges correctly
@(test)
test_merge_symbols_into :: proc(t: ^testing.T) {
	target := symbols.create_empty_symbol_table()
	source := symbols.create_empty_symbol_table()
	
	// Add symbol to target
	sym_target := symbols.Symbol{
		name = "target_var",
		kind = .Variable,
	}
	symbols.add_symbol(target, sym_target)
	
	// Add symbol to source
	sym_source := symbols.Symbol{
		name = "source_var",
		kind = .Variable,
	}
	symbols.add_symbol(source, sym_source)
	
	// Merge
	symbols.merge_symbols_into(target, source)
	
	// Verify target has both symbols
	_, has_target_var := target.symbols["target_var"]
	_, has_source_var := target.symbols["source_var"]
	
	testing.expect(t, has_target_var, "target should still have target_var")
	testing.expect(t, has_source_var, "target should now have source_var from merge")
	
	// Cleanup
	symbols.destroy_symbol_table(target)
	symbols.destroy_symbol_table(source)
}

