package main

import "../../src/ast"
import "../../src/parser"
import "../../src/tokenizer"

import "base:runtime"
import "core:fmt"
import "core:os"

Node_Count :: struct {
	name:  string,
	count: int,
}

Node_Counts :: struct {
	items: [dynamic]Node_Count,
}

Tree_State :: struct {
	index: int,
}

main :: proc() {
	args := os.args
	if len(args) == 2 && args[1] == "--help" {
		print_usage()
		return
	}
	if len(args) != 3 {
		print_usage()
		os.exit(1)
	}

	command := args[1]
	path := args[2]
	if command == "tokens" || command == "token" {
		run_tokens(path)
		return
	}
	if command == "parse" {
		run_parse(path, false)
		return
	}
	if command == "tree" {
		run_parse(path, true)
		return
	}

	print_usage()
	os.exit(1)
}

print_usage :: proc() {
	fmt.println("abap_frontend")
	fmt.println("usage: abap_frontend --help")
	fmt.println("       abap_frontend tokens <file>")
	fmt.println("       abap_frontend parse <file>")
	fmt.println("       abap_frontend tree <file>")
}

read_source :: proc(path: string, allocator: runtime.Allocator) -> (string, bool) {
	data, err := os.read_entire_file(path, allocator)
	if err != nil {
		fmt.printf("error\tread\t%s\t%v\n", path, err)
		return "", false
	}
	return string(data), true
}

run_tokens :: proc(path: string) {
	allocator := runtime.heap_allocator()
	source, ok := read_source(path, allocator)
	if !ok {
		os.exit(1)
	}

	lexed := tokenizer.tokenize(source, allocator)
	fmt.printf("file\t%s\n", path)
	fmt.printf("tokens\t%d\n", len(lexed.tokens))
	for tok in lexed.tokens {
		fmt.printf("token\t%d\t%v\t%d\t%d\n", tok.index, tok.kind, tok.range.start, tok.range.end)
	}
	print_lex_errors(lexed.errors)
}

run_parse :: proc(path: string, dump_tree: bool) {
	allocator := runtime.heap_allocator()
	source, ok := read_source(path, allocator)
	if !ok {
		os.exit(1)
	}

	{
		context.temp_allocator = allocator
		parsed := parser.parse(source, path, allocator)
		fmt.printf("file\t%s\n", path)
		fmt.printf(
			"root\t%s\t%d\t%d\n",
			node_type_name(parsed.root),
			parsed.root.range.start,
			parsed.root.range.end,
		)
		fmt.printf("top_level_stmts\t%d\n", len(parsed.root.stmts))
		print_parse_errors(parsed.errors)
		if dump_tree {
			print_node_counts(parsed.root, allocator)
			print_tree(parsed.root)
		}
	}
}

print_lex_errors :: proc(errors: []tokenizer.Lex_Error) {
	fmt.printf("lex_errors\t%d\n", len(errors))
	for err, i in errors {
		fmt.printf("lex_error\t%d\t%d\t%d\t%s\n", i, err.range.start, err.range.end, err.message)
	}
}

print_parse_errors :: proc(errors: []parser.Parse_Error) {
	fmt.printf("parse_errors\t%d\n", len(errors))
	for err, i in errors {
		fmt.printf("parse_error\t%d\t%d\t%d\t%s\n", i, err.range.start, err.range.end, err.message)
	}
}

print_node_counts :: proc(root: ^ast.Node, allocator: runtime.Allocator) {
	counts := Node_Counts {
		items = make([dynamic]Node_Count, 0, 64, allocator),
	}
	visitor := ast.Visitor {
		visit = count_visit,
		data  = rawptr(&counts),
	}
	ast.walk(&visitor, root)
	fmt.printf("node_counts\t%d\n", len(counts.items))
	for item in counts.items {
		fmt.printf("node_count\t%s\t%d\n", item.name, item.count)
	}
}

count_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	counts := cast(^Node_Counts)v.data
	count_node(counts, node_type_name(node))
	return v
}

count_node :: proc(counts: ^Node_Counts, name: string) {
	for &item in counts.items {
		if item.name == name {
			item.count += 1
			return
		}
	}
	append(&counts.items, Node_Count{name, 1})
}

print_tree :: proc(root: ^ast.Node) {
	state := Tree_State{}
	visitor := ast.Visitor {
		visit = tree_visit,
		data  = rawptr(&state),
	}
	ast.walk(&visitor, root)
}

tree_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	state := cast(^Tree_State)v.data
	fmt.printf(
		"tree_node\t%d\t%s\t%d\t%d\n",
		state.index,
		node_type_name(node),
		node.range.start,
		node.range.end,
	)
	state.index += 1
	return v
}

node_type_name :: proc(node: ^ast.Node) -> string {
	if node == nil {
		return "nil"
	}
	#partial switch _ in node.derived {
	case ^ast.File:
		return "File"
	case ^ast.Bad_Expr:
		return "Bad_Expr"
	case ^ast.Char_String_Template_Expr:
		return "Char_String_Template_Expr"
	case ^ast.Template_Literal_Expr:
		return "Template_Literal_Expr"
	case ^ast.Template_Interpolation_Expr:
		return "Template_Interpolation_Expr"
	case ^ast.Template_Expr:
		return "Template_Expr"
	case ^ast.Template_Format_Spec_Expr:
		return "Template_Format_Spec_Expr"
	case ^ast.Binary_Expr:
		return "Binary_Expr"
	case ^ast.Unary_Expr:
		return "Unary_Expr"
	case ^ast.Paren_Expr:
		return "Paren_Expr"
	case ^ast.Ident_Expr:
		return "Ident_Expr"
	case ^ast.Literal_Expr:
		return "Literal_Expr"
	case ^ast.Type_Ref_Expr:
		return "Type_Ref_Expr"
	case ^ast.Host_Expr:
		return "Host_Expr"
	case ^ast.Table_Expr:
		return "Table_Expr"
	case ^ast.Selector_Expr:
		return "Selector_Expr"
	case ^ast.Substring_Expr:
		return "Substring_Expr"
	case ^ast.Call_Expr:
		return "Call_Expr"
	case ^ast.Call_Arg_List_Expr:
		return "Call_Arg_List_Expr"
	case ^ast.Call_Arg_Section_Expr:
		return "Call_Arg_Section_Expr"
	case ^ast.Call_Named_Arg_Expr:
		return "Call_Named_Arg_Expr"
	case ^ast.Call_Positional_Arg_Expr:
		return "Call_Positional_Arg_Expr"
	case ^ast.Constructor_Expr:
		return "Constructor_Expr"
	case ^ast.Is_Predicate_Expr:
		return "Is_Predicate_Expr"
	case ^ast.Instance_Of_Predicate_Expr:
		return "Instance_Of_Predicate_Expr"
	case ^ast.Between_Expr:
		return "Between_Expr"
	case ^ast.Let_Expr:
		return "Let_Expr"
	case ^ast.Constructor_Let_Binding_Expr:
		return "Constructor_Let_Binding_Expr"
	case ^ast.Constructor_When_Clause_Expr:
		return "Constructor_When_Clause_Expr"
	case ^ast.Constructor_Else_Clause_Expr:
		return "Constructor_Else_Clause_Expr"
	case ^ast.Constructor_For_Clause_Expr:
		return "Constructor_For_Clause_Expr"
	case ^ast.Constructor_Where_Clause_Expr:
		return "Constructor_Where_Clause_Expr"
	case ^ast.Constructor_Init_Clause_Expr:
		return "Constructor_Init_Clause_Expr"
	case ^ast.Constructor_Next_Clause_Expr:
		return "Constructor_Next_Clause_Expr"
	case ^ast.Constructor_Named_Assignment_Expr:
		return "Constructor_Named_Assignment_Expr"
	case ^ast.Constructor_Base_Clause_Expr:
		return "Constructor_Base_Clause_Expr"
	case ^ast.Constructor_Lines_Of_Clause_Expr:
		return "Constructor_Lines_Of_Clause_Expr"
	case ^ast.Constructor_Optional_Expr:
		return "Constructor_Optional_Expr"
	case ^ast.Constructor_Corresponding_Mapping_Clause_Expr:
		return "Constructor_Corresponding_Mapping_Clause_Expr"
	case ^ast.Constructor_Corresponding_Mapping_Assignment_Expr:
		return "Constructor_Corresponding_Mapping_Assignment_Expr"
	case ^ast.Constructor_Corresponding_Except_Clause_Expr:
		return "Constructor_Corresponding_Except_Clause_Expr"
	case ^ast.Data_Inline_Name_Expr:
		return "Data_Inline_Name_Expr"
	case ^ast.Field_Symbol_Inline_Name_Expr:
		return "Field_Symbol_Inline_Name_Expr"
	case ^ast.Data_Decl:
		return "Data_Decl"
	case ^ast.Data_Chained_Decl:
		return "Data_Chained_Decl"
	case ^ast.Data_Inline_Decl:
		return "Data_Inline_Decl"
	case ^ast.Types_Decl:
		return "Types_Decl"
	case ^ast.Constants_Decl:
		return "Constants_Decl"
	case ^ast.Field_Symbols_Decl:
		return "Field_Symbols_Decl"
	case ^ast.Statics_Decl:
		return "Statics_Decl"
	case ^ast.Tables_Decl:
		return "Tables_Decl"
	case ^ast.Ranges_Decl:
		return "Ranges_Decl"
	case ^ast.Parameters_Decl:
		return "Parameters_Decl"
	case ^ast.Select_Options_Decl:
		return "Select_Options_Decl"
	case ^ast.Controls_Decl:
		return "Controls_Decl"
	case ^ast.Class_Data_Decl:
		return "Class_Data_Decl"
	case ^ast.Type_Pools_Decl:
		return "Type_Pools_Decl"
	case ^ast.Function_Pool_Decl:
		return "Function_Pool_Decl"
	case ^ast.Assign_Stmt:
		return "Assign_Stmt"
	case ^ast.Downcast_Assign_Stmt:
		return "Downcast_Assign_Stmt"
	case ^ast.Expr_Stmt:
		return "Expr_Stmt"
	case ^ast.Clear_Stmt:
		return "Clear_Stmt"
	case ^ast.Refresh_Stmt:
		return "Refresh_Stmt"
	case ^ast.Free_Stmt:
		return "Free_Stmt"
	case ^ast.Unassign_Stmt:
		return "Unassign_Stmt"
	case ^ast.Move_Stmt:
		return "Move_Stmt"
	case ^ast.Add_Stmt:
		return "Add_Stmt"
	case ^ast.Subtract_Stmt:
		return "Subtract_Stmt"
	case ^ast.Multiply_Stmt:
		return "Multiply_Stmt"
	case ^ast.Divide_Stmt:
		return "Divide_Stmt"
	case ^ast.Compute_Stmt:
		return "Compute_Stmt"
	case ^ast.Concatenate_Stmt:
		return "Concatenate_Stmt"
	case ^ast.Split_Stmt:
		return "Split_Stmt"
	case ^ast.Condense_Stmt:
		return "Condense_Stmt"
	case ^ast.Replace_Stmt:
		return "Replace_Stmt"
	case ^ast.Translate_Stmt:
		return "Translate_Stmt"
	case ^ast.Shift_Stmt:
		return "Shift_Stmt"
	case ^ast.Find_Stmt:
		return "Find_Stmt"
	case ^ast.Search_Stmt:
		return "Search_Stmt"
	case ^ast.Perform_Stmt:
		return "Perform_Stmt"
	case ^ast.Call_Stmt:
		return "Call_Stmt"
	case ^ast.Submit_Stmt:
		return "Submit_Stmt"
	case ^ast.Message_Stmt:
		return "Message_Stmt"
	case ^ast.Write_Stmt:
		return "Write_Stmt"
	case ^ast.Assert_Stmt:
		return "Assert_Stmt"
	case ^ast.Check_Stmt:
		return "Check_Stmt"
	case ^ast.Flow_Stmt:
		return "Flow_Stmt"
	case ^ast.Transaction_Stmt:
		return "Transaction_Stmt"
	case ^ast.Describe_Stmt:
		return "Describe_Stmt"
	case ^ast.Runtime_Stmt:
		return "Runtime_Stmt"
	case ^ast.Raise_Stmt:
		return "Raise_Stmt"
	case ^ast.Authority_Check_Stmt:
		return "Authority_Check_Stmt"
	case ^ast.Field_Groups_Stmt:
		return "Field_Groups_Stmt"
	case ^ast.Insert_Dummy_Stmt:
		return "Insert_Dummy_Stmt"
	case ^ast.Field_Stmt:
		return "Field_Stmt"
	case ^ast.Assign_Field_Stmt:
		return "Assign_Field_Stmt"
	case ^ast.Create_Object_Stmt:
		return "Create_Object_Stmt"
	case ^ast.Text_Transform_Stmt:
		return "Text_Transform_Stmt"
	case ^ast.List_Control_Stmt:
		return "List_Control_Stmt"
	case ^ast.Line_Stmt:
		return "Line_Stmt"
	case ^ast.Macro_Def_Stmt:
		return "Macro_Def_Stmt"
	case ^ast.Macro_Call_Stmt:
		return "Macro_Call_Stmt"
	case ^ast.Oop_Simple_Stmt:
		return "Oop_Simple_Stmt"
	case ^ast.If_Stmt:
		return "If_Stmt"
	case ^ast.Case_Stmt:
		return "Case_Stmt"
	case ^ast.While_Stmt:
		return "While_Stmt"
	case ^ast.Do_Stmt:
		return "Do_Stmt"
	case ^ast.Loop_Stmt:
		return "Loop_Stmt"
	case ^ast.At_Stmt:
		return "At_Stmt"
	case ^ast.Try_Stmt:
		return "Try_Stmt"
	case ^ast.Class_Decl:
		return "Class_Decl"
	case ^ast.Interface_Decl:
		return "Interface_Decl"
	case ^ast.Method_Decl:
		return "Method_Decl"
	case ^ast.Form_Decl:
		return "Form_Decl"
	case ^ast.Function_Decl:
		return "Function_Decl"
	case ^ast.Module_Decl:
		return "Module_Decl"
	case ^ast.Event_Block_Stmt:
		return "Event_Block_Stmt"
	case ^ast.Enhancement_Stmt:
		return "Enhancement_Stmt"
	case ^ast.Enhancement_Section_Stmt:
		return "Enhancement_Section_Stmt"
	case ^ast.Test_Seam_Stmt:
		return "Test_Seam_Stmt"
	case ^ast.Test_Injection_Stmt:
		return "Test_Injection_Stmt"
	case ^ast.Select_Stmt:
		return "Select_Stmt"
	case ^ast.Open_Cursor_Stmt:
		return "Open_Cursor_Stmt"
	case ^ast.Fetch_Stmt:
		return "Fetch_Stmt"
	case ^ast.Close_Cursor_Stmt:
		return "Close_Cursor_Stmt"
	case ^ast.Insert_Stmt:
		return "Insert_Stmt"
	case ^ast.Append_Stmt:
		return "Append_Stmt"
	case ^ast.Modify_Stmt:
		return "Modify_Stmt"
	case ^ast.Sort_Stmt:
		return "Sort_Stmt"
	case ^ast.Update_Stmt:
		return "Update_Stmt"
	case ^ast.Delete_Stmt:
		return "Delete_Stmt"
	case ^ast.Read_Table_Stmt:
		return "Read_Table_Stmt"
	case ^ast.Dataset_Stmt:
		return "Dataset_Stmt"
	case ^ast.Report_Stmt:
		return "Report_Stmt"
	case ^ast.Textpool_Stmt:
		return "Textpool_Stmt"
	case ^ast.Exec_Sql_Stmt:
		return "Exec_Sql_Stmt"
	case ^ast.Generate_Stmt:
		return "Generate_Stmt"
	case ^ast.Invalid_Stmt:
		return "Invalid_Stmt"
	}
	return "Unknown"
}
