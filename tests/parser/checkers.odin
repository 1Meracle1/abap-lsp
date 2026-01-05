package tests_parser

import "../../src/lang/ast"
import lexer "../../src/lang/lexer"
import "core:fmt"
import "core:testing"


check_expr :: proc(
	t: ^testing.T,
	expected: ast.Any_Expr,
	actual: ^ast.Expr,
	loc := #caller_location,
) {
	if actual == nil {
		testing.expect(t, expected == nil, "Expected non-nil expr, got nil", loc = loc)
		return
	}

	if expected == nil {
		testing.expect(t, actual == nil, "Expected nil expr, got non-nil", loc = loc)
		return
	}

	actual_derived := actual.derived_expr

	#partial switch ex in expected {
	case ^ast.Basic_Lit:
		ac, ok := actual_derived.(^ast.Basic_Lit)
		if !testing.expect(t, ok, fmt.tprintf("Expected Basic_Lit, got %T", actual_derived), loc = loc) do return
		testing.expect(
			t,
			ex.tok.lit == ac.tok.lit,
			fmt.tprintf("Expected lit '%s', got '%s'", ex.tok.lit, ac.tok.lit),
			loc = loc,
		)

	case ^ast.Ident:
		ac, ok := actual_derived.(^ast.Ident)
		if !testing.expect(t, ok, fmt.tprintf("Expected Ident, got %T", actual_derived), loc = loc) do return
		testing.expect(
			t,
			ex.name == ac.name,
			fmt.tprintf("Expected ident '%s', got '%s'", ex.name, ac.name),
			loc = loc,
		)

	case ^ast.Selector_Expr:
		ac, ok := actual_derived.(^ast.Selector_Expr)
		if !testing.expect(t, ok, fmt.tprintf("Expected Selector_Expr, got %T", actual_derived), loc = loc) do return
		testing.expect(
			t,
			ex.op.kind == ac.op.kind,
			fmt.tprintf("Expected selector op '%v', got '%v'", ex.op.kind, ac.op.kind),
			loc = loc,
		)
		if testing.expect(t, ac.field != nil, "Actual selector field is nil", loc = loc) {
			testing.expect(
				t,
				ex.field.name == ac.field.name,
				fmt.tprintf(
					"Expected selector field '%s', got '%s'",
					ex.field.name,
					ac.field.name,
				),
				loc = loc,
			)
		}
		check_expr(t, ex.expr.derived_expr, ac.expr, loc = loc)

	case ^ast.New_Expr:
		ac, ok := actual_derived.(^ast.New_Expr)
		if !testing.expect(t, ok, fmt.tprintf("Expected New_Expr, got %T", actual_derived), loc = loc) do return
		testing.expect(
			t,
			ex.is_inferred == ac.is_inferred,
			fmt.tprintf("Expected is_inferred '%v', got '%v'", ex.is_inferred, ac.is_inferred),
			loc = loc,
		)
		if ex.type_expr != nil {
			if !testing.expect(t, ac.type_expr != nil, "Actual type_expr is nil", loc = loc) do return
			check_expr(t, ex.type_expr.derived_expr, ac.type_expr, loc = loc)
		} else {
			testing.expect(
				t,
				ac.type_expr == nil,
				"Expected nil type_expr, got non-nil",
				loc = loc,
			)
		}
		if !testing.expect(t, len(ex.args) == len(ac.args), fmt.tprintf("Expected %d args, got %d", len(ex.args), len(ac.args)), loc = loc) do return
		for i := 0; i < len(ex.args); i += 1 {
			check_expr(t, ex.args[i].derived_expr, ac.args[i], loc = loc)
		}

	case ^ast.Call_Expr:
		ac, ok := actual_derived.(^ast.Call_Expr)
		if !testing.expect(t, ok, fmt.tprintf("Expected Call_Expr, got %T", actual_derived), loc = loc) do return
		check_expr(t, ex.expr.derived_expr, ac.expr, loc = loc)
		if !testing.expect(t, len(ex.args) == len(ac.args), fmt.tprintf("Expected %d call args, got %d", len(ex.args), len(ac.args)), loc = loc) do return
		for i := 0; i < len(ex.args); i += 1 {
			check_expr(t, ex.args[i].derived_expr, ac.args[i], loc = loc)
		}

	case ^ast.Binary_Expr:
		ac, ok := actual_derived.(^ast.Binary_Expr)
		if !testing.expect(t, ok, fmt.tprintf("Expected Binary_Expr, got %T", actual_derived), loc = loc) do return
		check_expr(t, ex.left.derived_expr, ac.left, loc = loc)
		check_expr(t, ex.right.derived_expr, ac.right, loc = loc)

	case ^ast.Predicate_Expr:
		ac, ok := actual_derived.(^ast.Predicate_Expr)
		if !testing.expect(t, ok, fmt.tprintf("Expected Predicate_Expr, got %T", actual_derived), loc = loc) do return
		check_expr(t, ex.expr.derived_expr, ac.expr, loc = loc)
		testing.expect(
			t,
			ex.predicate == ac.predicate,
			fmt.tprintf("Expected predicate '%v', got '%v'", ex.predicate, ac.predicate),
			loc = loc,
		)
		testing.expect(
			t,
			ex.is_negated == ac.is_negated,
			fmt.tprintf("Expected is_negated '%v', got '%v'", ex.is_negated, ac.is_negated),
			loc = loc,
		)

	case ^ast.Unary_Expr:
		ac, ok := actual_derived.(^ast.Unary_Expr)
		if !testing.expect(t, ok, fmt.tprintf("Expected Unary_Expr, got %T", actual_derived), loc = loc) do return
		check_expr(t, ex.expr.derived_expr, ac.expr, loc = loc)

	case ^ast.Paren_Expr:
		ac, ok := actual_derived.(^ast.Paren_Expr)
		if !testing.expect(t, ok, fmt.tprintf("Expected Paren_Expr, got %T", actual_derived), loc = loc) do return
		check_expr(t, ex.expr.derived_expr, ac.expr, loc = loc)

	case:
		testing.expect(
			t,
			false,
			fmt.tprintf("Unsupported expected type in check_expr: %T", expected),
			loc = loc,
		)
	}
}

check_stmt :: proc(
	t: ^testing.T,
	expected: ast.Any_Stmt,
	actual: ^ast.Stmt,
	loc := #caller_location,
) {
	if actual == nil {
		testing.expect(t, expected == nil, "Expected non-nil stmt, got nil", loc = loc)
		return
	}

	if expected == nil {
		testing.expect(t, actual == nil, "Expected nil stmt, got non-nil", loc = loc)
		return
	}

	actual_derived := actual.derived_stmt

	#partial switch ex in expected {
	case ^ast.Authority_Check_Stmt:
		ac, ok := actual_derived.(^ast.Authority_Check_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected Authority_Check_Stmt, got %T", actual_derived), loc = loc) do return

		check_expr(t, ex.object.derived_expr, ac.object, loc = loc)

		if ex.user != nil {
			if !testing.expect(t, ac.user != nil, "Expected user, got nil", loc = loc) do return
			check_expr(t, ex.user.derived_expr, ac.user, loc = loc)
		} else {
			testing.expect(t, ac.user == nil, "Expected nil user", loc = loc)
		}

		if !testing.expect(t, len(ex.ids) == len(ac.ids), fmt.tprintf("Expected %d ids, got %d", len(ex.ids), len(ac.ids)), loc = loc) do return

		for i := 0; i < len(ex.ids); i += 1 {
			ex_id := ex.ids[i]
			ac_id := ac.ids[i]

			check_expr(t, ex_id.id.derived_expr, ac_id.id, loc = loc)
			testing.expect(t, ex_id.is_dummy == ac_id.is_dummy, "is_dummy mismatch", loc = loc)

			if ex_id.field != nil {
				if !testing.expect(t, ac_id.field != nil, "Expected field, got nil", loc = loc) do return
				check_expr(t, ex_id.field.derived_expr, ac_id.field, loc = loc)
			} else {
				if !ex_id.is_dummy {
					testing.expect(t, ac_id.field == nil, "Expected nil field", loc = loc)
				}
			}
		}

	case ^ast.Delete_Stmt:
		ac, ok := actual_derived.(^ast.Delete_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected Delete_Stmt, got %T", actual_derived), loc = loc) do return

		testing.expect(
			t,
			ex.kind == ac.kind,
			fmt.tprintf("Expected kind %v, got %v", ex.kind, ac.kind),
			loc = loc,
		)
		check_expr(t, ex.target.derived_expr, ac.target, loc = loc)
		if ex.where_cond != nil {
			if !testing.expect(t, ac.where_cond != nil, "Expected where_cond, got nil", loc = loc) do return
			check_expr(t, ex.where_cond.derived_expr, ac.where_cond, loc = loc)
		}
		if ex.index_expr != nil {
			if !testing.expect(t, ac.index_expr != nil, "Expected index_expr, got nil", loc = loc) do return
			check_expr(t, ex.index_expr.derived_expr, ac.index_expr, loc = loc)
		}

	case ^ast.Data_Inline_Decl:
		ac, ok := actual_derived.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Data_Inline_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual ident is nil", loc = loc) {
			testing.expect(t, ex.ident != nil, "Expected ident is nil") // Should be enforced by builder
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf("Expected Decl ident '%s', got '%s'", ex.ident.name, ac.ident.name),
				loc = loc,
			)
		}

		check_expr(t, ex.value.derived_expr, ac.value, loc = loc)

	case ^ast.Data_Typed_Decl:
		ac, ok := actual_derived.(^ast.Data_Typed_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual ident is nil", loc = loc) {
			testing.expect(t, ex.ident != nil, "Expected ident is nil") // Should be enforced by builder
			check_expr(t, ex.ident.derived_expr, ac.ident, loc = loc)
		}

		check_expr(t, ex.typed.derived_expr, ac.typed, loc = loc)

	case ^ast.Data_Typed_Chain_Decl:
		ac, ok := actual_derived.(^ast.Data_Typed_Chain_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Chain_Decl, got %T", actual_derived), loc = loc) do return

		if !testing.expect(t, len(ex.decls) == len(ac.decls), fmt.tprintf("Expected %d decls in chain, got %d", len(ex.decls), len(ac.decls)), loc = loc) do return

		for i := 0; i < len(ex.decls); i += 1 {
			ex_decl := ex.decls[i]
			ac_decl := ac.decls[i]

			if testing.expect(
				t,
				ac_decl.ident != nil,
				fmt.tprintf("Actual ident[%d] is nil", i),
				loc = loc,
			) {
				check_expr(t, ex_decl.ident.derived_expr, ac_decl.ident, loc = loc)
			}

			check_expr(t, ex_decl.typed.derived_expr, ac_decl.typed, loc = loc)
		}

	case ^ast.Types_Decl:
		ac, ok := actual_derived.(^ast.Types_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Types_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual ident is nil", loc = loc) {
			testing.expect(t, ex.ident != nil, "Expected ident is nil")
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf(
					"Expected Types_Decl ident '%s', got '%s'",
					ex.ident.name,
					ac.ident.name,
				),
				loc = loc,
			)
		}

		check_expr(t, ex.typed.derived_expr, ac.typed, loc = loc)

	case ^ast.Types_Chain_Decl:
		ac, ok := actual_derived.(^ast.Types_Chain_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Types_Chain_Decl, got %T", actual_derived), loc = loc) do return

		if !testing.expect(t, len(ex.decls) == len(ac.decls), fmt.tprintf("Expected %d decls in types chain, got %d", len(ex.decls), len(ac.decls)), loc = loc) do return

		for i := 0; i < len(ex.decls); i += 1 {
			ex_decl := ex.decls[i]
			ac_decl := ac.decls[i]

			if testing.expect(
				t,
				ac_decl.ident != nil,
				fmt.tprintf("Actual types ident[%d] is nil", i),
				loc = loc,
			) {
				testing.expect(
					t,
					ex_decl.ident.name == ac_decl.ident.name,
					fmt.tprintf(
						"Expected types chain decl[%d] ident '%s', got '%s'",
						i,
						ex_decl.ident.name,
						ac_decl.ident.name,
					),
					loc = loc,
				)
			}

			check_expr(t, ex_decl.typed.derived_expr, ac_decl.typed, loc = loc)
		}

	case ^ast.Types_Struct_Decl:
		ac, ok := actual_derived.(^ast.Types_Struct_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Types_Struct_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual struct ident is nil", loc = loc) {
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf("Expected struct name '%s', got '%s'", ex.ident.name, ac.ident.name),
				loc = loc,
			)
		}

		if !testing.expect(t, len(ex.components) == len(ac.components), fmt.tprintf("Expected %d components, got %d", len(ex.components), len(ac.components)), loc = loc) do return

		for i := 0; i < len(ex.components); i += 1 {
			check_stmt(t, ex.components[i].derived_stmt, ac.components[i], loc = loc)
		}

	case ^ast.Const_Decl:
		ac, ok := actual_derived.(^ast.Const_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Const_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual const ident is nil", loc = loc) {
			testing.expect(t, ex.ident != nil, "Expected ident is nil")
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf(
					"Expected Const_Decl ident '%s', got '%s'",
					ex.ident.name,
					ac.ident.name,
				),
				loc = loc,
			)
		}

		check_expr(t, ex.typed.derived_expr, ac.typed, loc = loc)

		if ex.value != nil {
			if !testing.expect(t, ac.value != nil, "Expected value, got nil", loc = loc) do return
			check_expr(t, ex.value.derived_expr, ac.value, loc = loc)
		}

	case ^ast.Const_Chain_Decl:
		ac, ok := actual_derived.(^ast.Const_Chain_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Const_Chain_Decl, got %T", actual_derived), loc = loc) do return

		if !testing.expect(t, len(ex.decls) == len(ac.decls), fmt.tprintf("Expected %d decls in chain, got %d", len(ex.decls), len(ac.decls)), loc = loc) do return

		for i := 0; i < len(ex.decls); i += 1 {
			ex_decl := ex.decls[i]
			ac_decl := ac.decls[i]

			if testing.expect(
				t,
				ac_decl.ident != nil,
				fmt.tprintf("Actual ident[%d] is nil", i),
				loc = loc,
			) {
				testing.expect(
					t,
					ex_decl.ident.name == ac_decl.ident.name,
					fmt.tprintf(
						"Expected chain decl[%d] ident '%s', got '%s'",
						i,
						ex_decl.ident.name,
						ac_decl.ident.name,
					),
					loc = loc,
				)
			}

			check_expr(t, ex_decl.typed.derived_expr, ac_decl.typed, loc = loc)

			if ex_decl.value != nil {
				if !testing.expect(t, ac_decl.value != nil, fmt.tprintf("Expected value[%d], got nil", i), loc = loc) do return
				check_expr(t, ex_decl.value.derived_expr, ac_decl.value, loc = loc)
			}
		}

	case ^ast.Const_Struct_Decl:
		ac, ok := actual_derived.(^ast.Const_Struct_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Const_Struct_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual const struct ident is nil", loc = loc) {
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf(
					"Expected const struct name '%s', got '%s'",
					ex.ident.name,
					ac.ident.name,
				),
				loc = loc,
			)
		}

		if !testing.expect(t, len(ex.components) == len(ac.components), fmt.tprintf("Expected %d components, got %d", len(ex.components), len(ac.components)), loc = loc) do return

		for i := 0; i < len(ex.components); i += 1 {
			check_stmt(t, ex.components[i].derived_stmt, ac.components[i], loc = loc)
		}

	case ^ast.Assign_Stmt:
		ac, ok := actual_derived.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", actual_derived), loc = loc) do return

		if !testing.expect(t, len(ex.lhs) == len(ac.lhs), fmt.tprintf("Expected %d lhs, got %d", len(ex.lhs), len(ac.lhs)), loc = loc) do return
		if !testing.expect(t, len(ex.rhs) == len(ac.rhs), fmt.tprintf("Expected %d rhs, got %d", len(ex.rhs), len(ac.rhs)), loc = loc) do return

		for i := 0; i < len(ex.lhs); i += 1 {
			check_expr(t, ex.lhs[i].derived_expr, ac.lhs[i], loc = loc)
		}

		testing.expect(
			t,
			ex.op.kind == ac.op.kind,
			fmt.tprintf("Expected assign op '%v', got '%v'", ex.op.kind, ac.op.kind),
			loc = loc,
		)

		for i := 0; i < len(ex.rhs); i += 1 {
			check_expr(t, ex.rhs[i].derived_expr, ac.rhs[i], loc = loc)
		}

	case ^ast.Form_Decl:
		ac, ok := actual_derived.(^ast.Form_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Form_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual form ident is nil", loc = loc) {
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf("Expected form name '%s', got '%s'", ex.ident.name, ac.ident.name),
				loc = loc,
			)
		}

		// Check TABLES params
		if !testing.expect(t, len(ex.tables_params) == len(ac.tables_params), fmt.tprintf("Expected %d TABLES params, got %d", len(ex.tables_params), len(ac.tables_params)), loc = loc) do return
		for i := 0; i < len(ex.tables_params); i += 1 {
			check_form_param(t, ex.tables_params[i], ac.tables_params[i], loc = loc)
		}

		// Check USING params
		if !testing.expect(t, len(ex.using_params) == len(ac.using_params), fmt.tprintf("Expected %d USING params, got %d", len(ex.using_params), len(ac.using_params)), loc = loc) do return
		for i := 0; i < len(ex.using_params); i += 1 {
			check_form_param(t, ex.using_params[i], ac.using_params[i], loc = loc)
		}

		// Check CHANGING params
		if !testing.expect(t, len(ex.changing_params) == len(ac.changing_params), fmt.tprintf("Expected %d CHANGING params, got %d", len(ex.changing_params), len(ac.changing_params)), loc = loc) do return
		for i := 0; i < len(ex.changing_params); i += 1 {
			check_form_param(t, ex.changing_params[i], ac.changing_params[i], loc = loc)
		}

		// Check body statements
		if !testing.expect(t, len(ex.body) == len(ac.body), fmt.tprintf("Expected %d body statements, got %d", len(ex.body), len(ac.body)), loc = loc) do return
		for i := 0; i < len(ex.body); i += 1 {
			check_stmt(t, ex.body[i].derived_stmt, ac.body[i], loc = loc)
		}

	case ^ast.If_Stmt:
		ac, ok := actual_derived.(^ast.If_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected If_Stmt, got %T", actual_derived), loc = loc) do return

		// Check condition
		if ex.cond != nil {
			check_expr(t, ex.cond.derived_expr, ac.cond, loc = loc)
		}

		// Check body statements
		if !testing.expect(t, len(ex.body) == len(ac.body), fmt.tprintf("Expected %d body statements, got %d", len(ex.body), len(ac.body)), loc = loc) do return
		for i := 0; i < len(ex.body); i += 1 {
			check_stmt(t, ex.body[i].derived_stmt, ac.body[i], loc = loc)
		}

		// Check ELSEIF branches
		if !testing.expect(t, len(ex.elseif_branches) == len(ac.elseif_branches), fmt.tprintf("Expected %d ELSEIF branches, got %d", len(ex.elseif_branches), len(ac.elseif_branches)), loc = loc) do return
		for i := 0; i < len(ex.elseif_branches); i += 1 {
			ex_branch := ex.elseif_branches[i]
			ac_branch := ac.elseif_branches[i]
			if ex_branch.cond != nil {
				check_expr(t, ex_branch.cond.derived_expr, ac_branch.cond, loc = loc)
			}
			if !testing.expect(t, len(ex_branch.body) == len(ac_branch.body), fmt.tprintf("Expected %d body stmts in ELSEIF[%d], got %d", len(ex_branch.body), i, len(ac_branch.body)), loc = loc) do return
			for j := 0; j < len(ex_branch.body); j += 1 {
				check_stmt(t, ex_branch.body[j].derived_stmt, ac_branch.body[j], loc = loc)
			}
		}

		// Check ELSE body statements
		if !testing.expect(t, len(ex.else_body) == len(ac.else_body), fmt.tprintf("Expected %d ELSE body statements, got %d", len(ex.else_body), len(ac.else_body)), loc = loc) do return
		for i := 0; i < len(ex.else_body); i += 1 {
			check_stmt(t, ex.else_body[i].derived_stmt, ac.else_body[i], loc = loc)
		}

	case:
		testing.expect(
			t,
			false,
			fmt.tprintf("Unsupported expected type in check_stmt: %T", expected),
			loc = loc,
		)
	}
}

check_form_param :: proc(
	t: ^testing.T,
	expected: ^ast.Form_Param,
	actual: ^ast.Form_Param,
	loc := #caller_location,
) {
	if actual == nil {
		testing.expect(t, expected == nil, "Expected non-nil form param, got nil", loc = loc)
		return
	}

	testing.expect(
		t,
		expected.kind == actual.kind,
		fmt.tprintf("Expected param kind '%v', got '%v'", expected.kind, actual.kind),
		loc = loc,
	)

	if testing.expect(t, actual.ident != nil, "Actual param ident is nil", loc = loc) {
		testing.expect(
			t,
			expected.ident.name == actual.ident.name,
			fmt.tprintf(
				"Expected param name '%s', got '%s'",
				expected.ident.name,
				actual.ident.name,
			),
			loc = loc,
		)
	}

	if expected.typed != nil {
		check_expr(t, expected.typed.derived_expr, actual.typed, loc = loc)
	} else {
		testing.expect(t, actual.typed == nil, "Expected nil typed, got non-nil", loc = loc)
	}
}