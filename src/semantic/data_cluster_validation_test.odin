package abap_frontend_semantic2

import "src:ast"

import "core:testing"

@(test)
data_cluster_operands_follow_import_export_direction :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA import_parameter TYPE string.
DATA export_parameter TYPE string.
DATA import_buffer TYPE xstring.
DATA export_buffer TYPE xstring.
DATA import_area TYPE string.
DATA export_area TYPE string.
DATA client TYPE string.
DATA id TYPE string.
IMPORT row = import_parameter FROM DATA BUFFER import_buffer.
EXPORT row = export_parameter TO DATA BUFFER export_buffer.
IMPORT row = DATA(inline_parameter) FROM DATABASE demo_indx_blob(sc) TO DATA(inline_area) CLIENT client ID id.
EXPORT row = export_parameter TO SHARED MEMORY demo_indx_blob(sc) FROM export_area CLIENT client ID id.`
	checker, file := checker_test_check_source(t, &project, source, "mem://data_cluster_direction.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	import_buffer := file.root.stmts[8].derived_stmt.(^ast.Import_Stmt)
	export_buffer := file.root.stmts[9].derived_stmt.(^ast.Export_Stmt)
	import_database := file.root.stmts[10].derived_stmt.(^ast.Import_Stmt)
	export_shared := file.root.stmts[11].derived_stmt.(^ast.Export_Stmt)
	readable := [?]^ast.Expr {
		import_buffer.medium.object,
		export_buffer.parameters[0].value,
		export_shared.parameters[0].value,
		export_shared.medium.work_area,
		import_database.medium.client,
		import_database.medium.id,
	}
	for expr in readable {
		info, ok := checker_test_expr_info_for_node(t, &checker, &expr.expr_base)
		testing.expect(t, ok)
		if ok {
			testing.expect(t, !info.is_lhs)
		}
	}
	writable := [?]^ast.Expr {
		import_buffer.parameters[0].value,
		export_buffer.medium.object,
		import_database.parameters[0].value,
		import_database.medium.work_area,
	}
	for expr in writable {
		info, ok := checker_test_expr_info_for_node(t, &checker, &expr.expr_base)
		testing.expect(t, ok)
		if ok {
			testing.expect(t, info.is_lhs)
		}
	}
}

@(test)
data_cluster_operands_report_unresolved_values_and_reject_constant_outputs :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `CONSTANTS constant TYPE string VALUE ''.
IMPORT row = constant FROM DATA BUFFER missing_import_buffer.
EXPORT row = missing_export_parameter TO DATA BUFFER constant.
IMPORT row = missing_import_parameter FROM MEMORY ID missing_import_id.
EXPORT row = missing_export_parameter_2 TO DATABASE demo_indx_blob(sc) FROM missing_export_area CLIENT missing_client ID missing_export_id.
IMPORT row = constant FROM DATABASE demo_indx_blob(sc) TO constant CLIENT '100' ID 'id'.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://data_cluster_validation.abap")

	names := [?]string {
		"missing_import_buffer",
		"missing_export_parameter",
		"missing_import_parameter",
		"missing_import_id",
		"missing_export_parameter_2",
		"missing_export_area",
		"missing_client",
		"missing_export_id",
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), len(names))
	for name in names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "IMPORT parameter target is not writable"),
		2,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "EXPORT medium target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "IMPORT work area target is not writable"),
		1,
	)
}
