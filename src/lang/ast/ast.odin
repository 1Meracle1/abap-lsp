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
	using node: Stmt,
	cond:       ^Expr,
	body:       ^Stmt,
	else_stmt:  ^Stmt,
}

Return_Stmt :: struct {
	using node: Stmt,
	results:    []^Expr,
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
}

Data_Typed_Chain_Decl :: struct {
	using node: Decl,
	decls:      [dynamic]^Data_Typed_Decl,
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
	using node:       Decl,
	ident:            ^Ident,
	tables_params:    [dynamic]^Form_Param,
	using_params:     [dynamic]^Form_Param,
	changing_params:  [dynamic]^Form_Param,
	body:             [dynamic]^Stmt,
}

// Types

Table_Type :: struct {
	using node: Expr,
	elem:       ^Expr,
}

// Struct_Type :: struct {
// 	using node: Expr,
// 	pointer: tokenizer.Pos,
// 	elem:    ^Expr,
// }

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
	// Types
	^Table_Type,
	// Statements
	^Bad_Stmt,
	^Expr_Stmt,
	^Assign_Stmt,
	^Block_Stmt,
	^If_Stmt,
	^Return_Stmt,
	// Declarations
	^Bad_Decl,
	^Data_Inline_Decl,
	^Data_Typed_Decl,
	^Data_Typed_Chain_Decl,
	^Form_Param,
	^Form_Decl,
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
	// Types
	^Table_Type,
}

Any_Stmt :: union {
	^Bad_Stmt,
	^Expr_Stmt,
	^Assign_Stmt,
	^Block_Stmt,
	^If_Stmt,
	^Return_Stmt,
	//
	^Bad_Decl,
	^Data_Inline_Decl,
	^Data_Typed_Decl,
	^Data_Typed_Chain_Decl,
	^Form_Decl,
}
