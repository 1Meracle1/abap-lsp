package lang_ast

import "../lexer"

Node :: struct {
	range:   lexer.TextRange,
	derived: Any_Node,
}

Program :: struct {
	using node: Node,
	name:       string,
	fullpath:   string,
	files:      map[string]^File,
}

Diagnostic :: struct {
	range:   lexer.TextRange,
	message: string,
}

File :: struct {
	using node:    Node,
	fullpath:      string,
	src:           string,
	decls:         [dynamic]^Stmt,
	comments:      [dynamic]lexer.Token,
	syntax_errors: [dynamic]Diagnostic,
}

// Base Types

Expr :: struct {
	using expr_base: Node,
	derived_expr:    Any_Expr,
}

Stmt :: struct {
	using stmt_base: Node,
	derived_stmt:    Any_Stmt,
}

Decl :: struct {
	using decl_base: Stmt,
}

// Expressions

Bad_Expr :: struct {
	using node: Expr,
}

Ident :: struct {
	using node: Expr,
	name:       string,
}

Basic_Lit :: struct {
	using node: Expr,
	tok:        lexer.Token,
}

Unary_Expr :: struct {
	using node: Expr,
	op:         lexer.Token,
	expr:       ^Expr,
}

Binary_Expr :: struct {
	using node: Expr,
	left:       ^Expr,
	op:         lexer.Token,
	right:      ^Expr,
}

Paren_Expr :: struct {
	using node: Expr,
	expr:       ^Expr,
}

Selector_Expr :: struct {
	using node: Expr,
	expr:       ^Expr,
	op:         lexer.Token,
	field:      ^Ident,
}

Index_Expr :: struct {
	using node: Expr,
	expr:       ^Expr,
	index:      ^Expr,
}

Call_Expr :: struct {
	using node: Expr,
	expr:       ^Expr,
	args:       []^Expr,
}

New_Expr :: struct {
	using node:  Expr,
	type_expr:   ^Expr,
	is_inferred: bool,
	args:        [dynamic]^Expr,
}

Constructor_Expr :: struct {
	using node:  Expr,
	keyword:     lexer.Token, // The constructor keyword (CONV, COND, etc.)
	type_expr:   ^Expr, // The type expression (nil if inferred)
	is_inferred: bool, // True if using # for type inference
	args:        [dynamic]^Expr,
}

// Named argument in a call expression (e.g., iv_object = 'ZATTP')
Named_Arg :: struct {
	using node: Expr,
	name:       ^Ident,
	value:      ^Expr,
}

// String template part - either a literal string or an embedded expression
String_Template_Part :: struct {
	is_expr: bool, // true if this is an embedded expression, false if literal
	literal: string, // literal text (if !is_expr)
	expr:    ^Expr, // embedded expression (if is_expr)
	range:   lexer.TextRange,
}

// String template expression (e.g., |Hello { name }!|)
String_Template_Expr :: struct {
	using node: Expr,
	parts:      [dynamic]String_Template_Part,
}

// Statements

Bad_Stmt :: struct {
	using node: Stmt,
}

Expr_Stmt :: struct {
	using node: Stmt,
	expr:       ^Expr,
}

Assign_Stmt :: struct {
	using node: Stmt,
	lhs:        []^Expr,
	op:         lexer.Token,
	rhs:        []^Expr,
}

Block_Stmt :: struct {
	using node: Stmt,
	label:      ^Expr,
	stmts:      []^Stmt,
}

If_Stmt :: struct {
	using node:      Stmt,
	cond:            ^Expr,
	body:            [dynamic]^Stmt,
	elseif_branches: [dynamic]^Elseif_Branch,
	else_body:       [dynamic]^Stmt,
}

Elseif_Branch :: struct {
	using node: Node,
	cond:       ^Expr,
	body:       [dynamic]^Stmt,
}

// Predicate expressions for IS INITIAL, IS SUPPLIED, IS BOUND, etc.
Predicate_Expr :: struct {
	using node: Expr,
	expr:       ^Expr,
	predicate:  Predicate_Kind,
	is_negated: bool, // for IS NOT
}

Predicate_Kind :: enum {
	Initial,
	Supplied,
	Bound,
	Assigned,
	Requested,
	Instance_Of,
}

Return_Stmt :: struct {
	using node: Stmt,
	results:    []^Expr,
}

Modify_Screen_Stmt :: struct {
	using node: Stmt,
}

Leave_Program_Stmt :: struct {
	using node: Stmt,
}

Set_Kind :: enum {
	Pf_Status,
	Titlebar,
	Cursor_Field,
	Screen,
}

Set_Stmt :: struct {
	using node: Stmt,
	kind:       Set_Kind,
	expr:       ^Expr,
}

Case_When_Branch :: struct {
	is_others: bool, // WHEN OTHERS is used
	expr:      ^Expr,
	body:      [dynamic]^Stmt,
}

Case_Stmt :: struct {
	using node: Stmt,
	expr:       ^Expr,
	branches:   [dynamic]Case_When_Branch,
}

While_Stmt :: struct {
	using node: Stmt,
	cond:       ^Expr,
	body:       [dynamic]^Stmt,
}

Clear_Stmt :: struct {
	using node: Stmt,
	exprs:      [dynamic]^Expr,
}

// MESSAGE statement
// Syntax: MESSAGE { msg | text } [TYPE type] [DISPLAY LIKE display_type] [WITH v1 [v2 [v3 [v4]]]] [INTO data]
Message_Stmt :: struct {
	using node:   Stmt,
	msg_expr:     ^Expr, // The message expression (string literal, identifier, or message ID like e899(class))
	msg_type:     ^Expr, // TYPE 'I' etc (optional)
	display_like: ^Expr, // DISPLAY LIKE 'E' (optional)
	with_args:    [dynamic]^Expr, // WITH v1 v2 v3 v4 (up to 4 args)
	into_target:  ^Expr, // INTO data (optional)
}

// Declarations

Bad_Decl :: struct {
	using node: Decl,
}

Data_Inline_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	value:      ^Expr,
}

Data_Typed_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	typed:      ^Expr,
	value:      ^Expr,
}

Data_Typed_Chain_Decl :: struct {
	using node: Decl,
	decls:      [dynamic]^Data_Typed_Decl,
}

// TYPES declarations

Types_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	typed:      ^Expr,
	length:     ^Expr,
}

Types_Chain_Decl :: struct {
	using node: Decl,
	decls:      [dynamic]^Types_Decl,
}

Types_Struct_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	components: [dynamic]^Stmt,
}

Form_Param_Kind :: enum {
	Tables,
	Using,
	Changing,
}

Form_Param :: struct {
	using node: Node,
	kind:       Form_Param_Kind,
	ident:      ^Ident,
	typed:      ^Expr,
}

Form_Decl :: struct {
	using node:      Decl,
	ident:           ^Ident,
	tables_params:   [dynamic]^Form_Param,
	using_params:    [dynamic]^Form_Param,
	changing_params: [dynamic]^Form_Param,
	body:            [dynamic]^Stmt,
}

Access_Modifier :: enum {
	Public,
	Protected,
	Private,
}

Method_Param_Kind :: enum {
	Importing,
	Exporting,
	Changing,
	Returning,
}

Method_Param :: struct {
	using node: Node,
	kind:       Method_Param_Kind,
	ident:      ^Ident,
	typed:      ^Expr,
	optional:   bool,
	default:    ^Expr,
}

Method_Decl :: struct {
	using node:      Decl,
	ident:           ^Ident,
	is_class:        bool,
	is_abstract:     bool,
	is_final:        bool,
	is_redefinition: bool,
	params:          [dynamic]^Method_Param,
	raising:         [dynamic]^Expr,
}

Attr_Decl :: struct {
	using node:   Decl,
	ident:        ^Ident,
	typed:        ^Expr,
	is_class:     bool,
	is_read_only: bool,
	value:        ^Expr,
}

Interfaces_Decl :: struct {
	using node: Decl,
	names:      [dynamic]^Ident,
}

Class_Section :: struct {
	using node: Node,
	access:     Access_Modifier,
	types:      [dynamic]^Stmt,
	data:       [dynamic]^Stmt,
	methods:    [dynamic]^Stmt,
	interfaces: [dynamic]^Stmt,
}

Class_Def_Decl :: struct {
	using node:      Decl,
	ident:           ^Ident,
	is_abstract:     bool,
	is_final:        bool,
	inheriting_from: ^Expr,
	sections:        [dynamic]^Class_Section,
}

Class_Impl_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	methods:    [dynamic]^Stmt,
}

Method_Impl :: struct {
	using node: Decl,
	ident:      ^Expr,
	body:       [dynamic]^Stmt,
}

Interface_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	methods:    [dynamic]^Stmt,
	types:      [dynamic]^Stmt,
	data:       [dynamic]^Stmt,
}

Report_Decl :: struct {
	using node: Decl,
	name:       ^Ident,
}

Include_Decl :: struct {
	using node: Decl,
	name:       ^Ident,
}

Event_Kind :: enum {
	StartOfSelection,
	EndOfSelection,
	Initialization,
	AtSelectionScreen,
	TopOfPage,
	EndOfPage,
}

Event_Block :: struct {
	using node: Decl,
	kind:       Event_Kind,
	body:       [dynamic]^Stmt,
}

Call_Screen_Stmt :: struct {
	using node: Stmt,
	screen_no:  ^Expr,
}

Module_Type :: enum {
	Output,
	Input,
}

Module_Decl :: struct {
	using node:  Decl,
	ident:       ^Ident,
	module_type: Module_Type,
	body:        [dynamic]^Stmt,
}

// Types

Table_Kind :: enum {
	Standard,
	Sorted,
	Hashed,
	Any,
}

Table_Key :: struct {
	is_unique:    bool,
	is_default:   bool,
	name:         ^Ident, // For named secondary keys
	components:   [dynamic]^Ident, // Key components
}

Table_Type :: struct {
	using node:   Expr,
	kind:         Table_Kind,
	elem:         ^Expr,
	primary_key:  ^Table_Key,
	secondary_keys: [dynamic]^Table_Key,
}

Ref_Type :: struct {
	using node: Expr,
	target:     ^Expr,
}

Line_Type :: struct {
	using node: Expr,
	table:      ^Expr,
}

Any_Node :: union {
	^Program,
	^File,
	// Expressions
	^Bad_Expr,
	^Ident,
	^Basic_Lit,
	^Unary_Expr,
	^Binary_Expr,
	^Paren_Expr,
	^Selector_Expr,
	^Index_Expr,
	^Call_Expr,
	^New_Expr,
	^Constructor_Expr,
	^Named_Arg,
	^Predicate_Expr,
	^String_Template_Expr,
	// Types
	^Table_Type,
	^Ref_Type,
	^Line_Type,
	// Statements
	^Bad_Stmt,
	^Expr_Stmt,
	^Assign_Stmt,
	^Block_Stmt,
	^If_Stmt,
	^Elseif_Branch,
	^Return_Stmt,
	^Modify_Screen_Stmt,
	^Leave_Program_Stmt,
	^Set_Stmt,
	^Case_Stmt,
	^While_Stmt,
	^Clear_Stmt,
	^Message_Stmt,
	// Declarations
	^Bad_Decl,
	^Data_Inline_Decl,
	^Data_Typed_Decl,
	^Data_Typed_Chain_Decl,
	^Types_Decl,
	^Types_Chain_Decl,
	^Types_Struct_Decl,
	^Form_Param,
	^Form_Decl,
	// Class/Interface declarations
	^Method_Param,
	^Method_Decl,
	^Attr_Decl,
	^Interfaces_Decl,
	^Class_Section,
	^Class_Def_Decl,
	^Class_Impl_Decl,
	^Method_Impl,
	^Interface_Decl,
	// Report/Include/Events
	^Report_Decl,
	^Include_Decl,
	^Event_Block,
	^Call_Screen_Stmt,
	^Module_Decl,
}

Any_Expr :: union {
	^Bad_Expr,
	^Ident,
	^Basic_Lit,
	^Unary_Expr,
	^Binary_Expr,
	^Paren_Expr,
	^Selector_Expr,
	^Index_Expr,
	^Call_Expr,
	^New_Expr,
	^Constructor_Expr,
	^Named_Arg,
	^Predicate_Expr,
	^String_Template_Expr,
	// Types
	^Table_Type,
	^Ref_Type,
	^Line_Type,
}

Any_Stmt :: union {
	^Bad_Stmt,
	^Expr_Stmt,
	^Assign_Stmt,
	^Block_Stmt,
	^If_Stmt,
	^Return_Stmt,
	^Modify_Screen_Stmt,
	^Leave_Program_Stmt,
	^Set_Stmt,
	^Case_Stmt,
	^While_Stmt,
	^Clear_Stmt,
	^Message_Stmt,
	//
	^Bad_Decl,
	^Data_Inline_Decl,
	^Data_Typed_Decl,
	^Data_Typed_Chain_Decl,
	^Types_Decl,
	^Types_Chain_Decl,
	^Types_Struct_Decl,
	^Form_Decl,
	// Class/Interface declarations
	^Method_Decl,
	^Attr_Decl,
	^Interfaces_Decl,
	^Class_Def_Decl,
	^Class_Impl_Decl,
	^Method_Impl,
	^Interface_Decl,
	// Report/Include/Events
	^Report_Decl,
	^Include_Decl,
	^Event_Block,
	^Call_Screen_Stmt,
	^Module_Decl,
}
