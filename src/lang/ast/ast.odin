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
	using node:  Decl,
	ident:       ^Ident,
	components:  [dynamic]^Stmt,
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
	using node:   Decl,
	ident:        ^Ident,
	is_class:     bool,       
	is_abstract:  bool,       
	is_final:     bool,       
	is_redefinition: bool,    
	params:       [dynamic]^Method_Param,
	raising:      [dynamic]^Expr,  
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
	using node:       Decl,
	ident:            ^Ident,
	is_abstract:      bool,
	is_final:         bool,
	inheriting_from:  ^Expr,      
	sections:         [dynamic]^Class_Section,
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

// Types

Table_Type :: struct {
	using node: Expr,
	elem:       ^Expr,
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
}
