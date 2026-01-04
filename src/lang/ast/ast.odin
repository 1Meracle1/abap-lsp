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

// Embedded expression formatting option kind
Embedded_Format_Kind :: enum {
	Alpha,   // ALPHA = IN/OUT
	Date,    // DATE = ISO/USER/RAW/ENVIRONMENT
	Time,    // TIME = ISO/USER/RAW/ENVIRONMENT
	Width,   // WIDTH = n
	Align,   // ALIGN = LEFT/RIGHT/CENTER
	Pad,     // PAD = 'char'
	Case,    // CASE = UPPER/LOWER/RAW
	Sign,    // SIGN = LEFT/LEFTPLUS/LEFTSPACE/RIGHT/RIGHTPLUS/RIGHTSPACE
	Decimals, // DECIMALS = n
	Exponent, // EXPONENT = n
	Zero,    // ZERO = YES/NO
	Number,  // NUMBER = USER/RAW/ENVIRONMENT
	Style,   // STYLE = SIMPLE/SIGN_AS_POSTFIX/SCALE_PRESERVING
	Currency, // CURRENCY = currency_code
	Country, // COUNTRY = country_code
	Timestamp, // TIMESTAMP = SPACE/ISO/USER/ENVIRONMENT
	Timezone, // TIMEZONE = tz
}

// Embedded expression formatting option value
Embedded_Format_Value :: enum {
	In,       // ALPHA = IN
	Out,      // ALPHA = OUT
	Iso,      // DATE/TIME/TIMESTAMP = ISO
	User,     // DATE/TIME/NUMBER/TIMESTAMP = USER
	Raw,      // DATE/TIME/NUMBER/CASE = RAW
	Environment, // DATE/TIME/NUMBER/TIMESTAMP = ENVIRONMENT
	Left,     // ALIGN/SIGN = LEFT
	Right,    // ALIGN/SIGN = RIGHT
	Center,   // ALIGN = CENTER
	Upper,    // CASE = UPPER
	Lower,    // CASE = LOWER
	Yes,      // ZERO = YES
	No,       // ZERO = NO
	Simple,   // STYLE = SIMPLE
	Scale_Preserving, // STYLE = SCALE_PRESERVING
	Sign_As_Postfix, // STYLE = SIGN_AS_POSTFIX
	Leftplus, // SIGN = LEFTPLUS
	Leftspace, // SIGN = LEFTSPACE
	Rightplus, // SIGN = RIGHTPLUS
	Rightspace, // SIGN = RIGHTSPACE
	Space,    // TIMESTAMP = SPACE
	Custom,   // For numeric values (WIDTH, DECIMALS) or string values (PAD, CURRENCY)
}

// Embedded expression formatting option
Embedded_Format_Option :: struct {
	kind:       Embedded_Format_Kind,
	value:      Embedded_Format_Value,
	num_value:  int,    // For WIDTH, DECIMALS, EXPONENT
	str_value:  string, // For PAD, CURRENCY, COUNTRY, TIMEZONE
	range:      lexer.TextRange,
}

// String template part - either a literal string or an embedded expression
String_Template_Part :: struct {
	is_expr:        bool, // true if this is an embedded expression, false if literal
	literal:        string, // literal text (if !is_expr)
	expr:           ^Expr, // embedded expression (if is_expr)
	format_options: [dynamic]Embedded_Format_Option, // formatting options (if is_expr)
	range:          lexer.TextRange,
}

// String template expression (e.g., |Hello { name }!|)
String_Template_Expr :: struct {
	using node: Expr,
	parts:      [dynamic]String_Template_Part,
}

// FOR expression in constructor expressions (VALUE, REDUCE, etc.)
// Syntax: FOR var IN itab [WHERE ( condition )] [( result_expr | named_args... )]
For_Expr :: struct {
	using node:   Expr,
	var_name:     ^Ident, // Loop variable name
	itab:         ^Expr, // Internal table to iterate over
	where_cond:   ^Expr, // Optional WHERE condition
	result_expr:  ^Expr, // Result expression (what to produce for each iteration) - deprecated, use result_args
	result_args:  [dynamic]^Expr, // Result arguments (named args like field1 = val1, or single expression)
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

// LOOP statement kinds
Loop_Kind :: enum {
	At, // LOOP AT itab
	At_Screen, // LOOP AT SCREEN
	At_Group, // LOOP AT GROUP group_var
}

// GROUP BY key specification
Loop_Group_By :: struct {
	name:       ^Ident, // Optional group name
	components: [dynamic]^Named_Arg, // Key components like (key1 = expr1 key2 = expr2)
}

// LOOP statement
// Syntax variations:
// - LOOP AT itab [INTO wa | ASSIGNING <fs> | TRANSPORTING NO FIELDS] [WHERE condition].
// - LOOP AT itab ... GROUP BY key_spec [ASSIGNING <fs>].
// - LOOP AT GROUP group_var [INTO wa | ASSIGNING <fs>] [WHERE condition].
// - LOOP AT SCREEN.
Loop_Stmt :: struct {
	using node:             Stmt,
	kind:                   Loop_Kind,
	itab:                   ^Expr, // The internal table expression
	into_target:            ^Expr, // INTO target (work area or inline DATA)
	assigning_target:       ^Expr, // ASSIGNING <fs> target (field symbol)
	from_expr:              ^Expr, // FROM index expression
	to_expr:                ^Expr, // TO index expression
	where_cond:             ^Expr, // WHERE condition expression
	transporting_no_fields: bool, // TRANSPORTING NO FIELDS flag
	group_by:               ^Loop_Group_By, // GROUP BY specification
	group_var:              ^Expr, // For LOOP AT GROUP: the group variable
	body:                   [dynamic]^Stmt,
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

// INSERT statement kinds
Insert_Kind :: enum {
	Into_Table, // INSERT expr INTO TABLE itab
	Into_Db, // INSERT INTO target VALUES wa
	From_Wa, // INSERT target FROM wa
	From_Table, // INSERT target FROM TABLE itab
}

// INSERT statement
// Syntax variations:
// - INSERT VALUE #( ... ) INTO TABLE itab.
// - INSERT INTO target VALUES wa.
// - INSERT target FROM wa.
// - INSERT target FROM TABLE itab.
Insert_Stmt :: struct {
	using node: Stmt,
	kind:       Insert_Kind,
	value_expr: ^Expr, // The value/expression to insert (for Into_Table, Into_Db)
	target:     ^Expr, // The target table (internal or database table)
	source:     ^Expr, // The source work area or table (for From_Wa, From_Table)
}

Authority_Check_Id :: struct {
	id:       ^Expr,
	field:    ^Expr, // nil if DUMMY
	is_dummy: bool,
}

Authority_Check_Stmt :: struct {
	using node: Stmt,
	object:     ^Expr,
	user:       ^Expr,
	ids:        [dynamic]Authority_Check_Id,
}

Sort_Order_Kind :: enum {
	None, // ascending by default
	Ascending,
	Descending,
}

Sort_Cols_By :: struct {
	col:   ^Expr,
	order: Sort_Order_Kind,
}

Sort_Stmt :: struct {
	using node: Stmt,
	itab:       ^Expr,
	order:      Sort_Order_Kind,
	cols_by:    [dynamic]Sort_Cols_By,
}

// READ TABLE statement kinds
Read_Table_Kind :: enum {
	With_Key, // READ TABLE itab WITH KEY ...
	With_Table_Key,
	Index, // READ TABLE itab INDEX idx
}

// READ TABLE key specification
Read_Table_Key :: struct {
	key_name:   ^Ident, // For USING KEY key_name (optional named key)
	components: [dynamic]^Named_Arg, // Key components like field1 = val1 field2 = val2
	table_line: ^Expr, // For WITH KEY table_line = value
}

// READ TABLE statement
// Syntax variations:
// - READ TABLE itab WITH KEY field1 = val1 ... [USING KEY key_name] INTO wa.
// - READ TABLE itab WITH KEY field1 = val1 ... ASSIGNING <fs>.
// - READ TABLE itab WITH KEY field1 = val1 ... TRANSPORTING NO FIELDS.
// - READ TABLE itab INDEX idx [USING KEY key_name] INTO wa.
// - READ TABLE itab INDEX idx ASSIGNING FIELD-SYMBOL(<fs>).
Read_Table_Stmt :: struct {
	using node:             Stmt,
	kind:                   Read_Table_Kind,
	itab:                   ^Expr, // The internal table expression
	key:                    ^Read_Table_Key, // WITH KEY specification
	index_expr:             ^Expr, // INDEX expression
	using_key:              ^Ident, // USING KEY key_name (optional)
	into_target:            ^Expr, // INTO target (work area or inline DATA)
	assigning_target:       ^Expr, // ASSIGNING <fs> target (field symbol)
	transporting_no_fields: bool, // TRANSPORTING NO FIELDS flag
}

// APPEND statement kinds
Append_Kind :: enum {
	Simple, // APPEND expr TO itab
	Initial_Line, // APPEND INITIAL LINE TO itab [ASSIGNING <fs>]
	Lines_Of, // APPEND LINES OF itab2 TO itab1
}

// APPEND statement
// Syntax variations:
// - APPEND expr TO itab.
// - APPEND INITIAL LINE TO itab [ASSIGNING <fs>].
// - APPEND LINES OF itab2 TO itab1.
Append_Stmt :: struct {
	using node:       Stmt,
	kind:             Append_Kind,
	source:           ^Expr, // The value/expression to append (for Simple, Lines_Of)
	target:           ^Expr, // The target internal table
	assigning_target: ^Expr, // Field symbol for ASSIGNING clause (optional)
}

// DELETE statement kinds
Delete_Kind :: enum {
	Where, // DELETE itab WHERE ...
	Index, // DELETE itab INDEX idx
	Adjacent_Duplicates, // DELETE ADJACENT DUPLICATES FROM itab ...
}

// DELETE statement
// Syntax variations:
// - DELETE itab WHERE ...
// - DELETE itab INDEX idx.
// - DELETE ADJACENT DUPLICATES FROM itab ...
Delete_Stmt :: struct {
	using node: Stmt,
	kind:       Delete_Kind,
	target:     ^Expr, // The internal table
	where_cond: ^Expr, // WHERE condition
	index_expr: ^Expr, // INDEX expression
}

Condense_Stmt :: struct {
	using node: Stmt,
	text:       ^Expr,
}

// CALL FUNCTION parameter kinds
Call_Function_Param_Kind :: enum {
	Exporting,
	Importing,
	Tables,
	Changing,
	Exceptions,
}

// CALL FUNCTION parameter (name = value pairs)
Call_Function_Param :: struct {
	using node: Node,
	kind:       Call_Function_Param_Kind,
	name:       ^Ident, // Parameter name
	value:      ^Expr, // Parameter value
}

// CALL FUNCTION statement
// Syntax: CALL FUNCTION 'func_name' [DESTINATION dest]
//         [EXPORTING param = value ...]
//         [IMPORTING param = value ...]
//         [TABLES param = value ...]
//         [CHANGING param = value ...]
//         [EXCEPTIONS name = value ...].
Call_Function_Stmt :: struct {
	using node:   Stmt,
	func_name:    ^Expr, // Function name (typically a string literal)
	destination:  ^Expr, // Optional DESTINATION expression
	exporting:    [dynamic]^Call_Function_Param,
	importing:    [dynamic]^Call_Function_Param,
	tables:       [dynamic]^Call_Function_Param,
	changing:     [dynamic]^Call_Function_Param,
	exceptions:   [dynamic]^Call_Function_Param,
}

// SELECT statement join kind
Select_Join_Kind :: enum {
	Inner,       // INNER JOIN
	Left_Outer,  // LEFT OUTER JOIN
	Right_Outer, // RIGHT OUTER JOIN
}

// SELECT statement join specification
Select_Join :: struct {
	using node:  Node,
	kind:        Select_Join_Kind,
	table:       ^Expr,             // Table name
	alias:       ^Ident,            // AS alias (optional)
	on_cond:     ^Expr,             // ON condition
}

// SELECT statement target kind
Select_Into_Kind :: enum {
	Single,       // INTO @wa or INTO @DATA(wa)
	Table,        // INTO TABLE @itab or INTO TABLE @DATA(itab)
	Corresponding, // INTO CORRESPONDING FIELDS OF TABLE @itab
}

// SELECT statement
// Syntax variations:
// - SELECT [SINGLE] fields FROM table [INTO target] [WHERE cond] [ORDER BY cols] [UP TO n ROWS].
// - SELECT [SINGLE] * FROM table [AS alias] [INTO target] [WHERE cond].
// - SELECT FROM table [AS alias] FIELDS field_list [WHERE cond] [INTO target].
// - SELECT ... INNER JOIN ... ON ... [WHERE cond] [INTO target].
// - SELECT ... FOR ALL ENTRIES IN itab WHERE ... [INTO target].
// - SELECT ... GROUP BY cols HAVING cond [INTO target].
Select_Stmt :: struct {
	using node:          Stmt,
	is_single:           bool,                    // SINGLE modifier
	fields:              [dynamic]^Expr,          // Field list (* or specific fields)
	from_table:          ^Expr,                   // FROM table expression
	from_alias:          ^Ident,                  // AS alias for FROM table (optional)
	joins:               [dynamic]^Select_Join,   // JOIN clauses
	into_kind:           Select_Into_Kind,        // INTO target kind
	into_target:         ^Expr,                   // INTO target (work area or inline DATA)
	where_cond:          ^Expr,                   // WHERE condition
	order_by:            [dynamic]^Expr,          // ORDER BY columns
	group_by:            [dynamic]^Expr,          // GROUP BY columns
	having_cond:         ^Expr,                   // HAVING condition
	for_all_entries:     ^Expr,                   // FOR ALL ENTRIES IN itab
	up_to_rows:          ^Expr,                   // UP TO n ROWS
	body:                [dynamic]^Stmt,          // Body for SELECT loop (non-single)
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

// CONSTANTS declarations

Const_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	typed:      ^Expr,
	value:      ^Expr,
}

Const_Chain_Decl :: struct {
	using node: Decl,
	decls:      [dynamic]^Const_Decl,
}

Const_Struct_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	components: [dynamic]^Stmt,
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

// FIELD-SYMBOLS declaration
// Syntax: FIELD-SYMBOLS <fs> TYPE type.
// Syntax: FIELD-SYMBOLS <fs> LIKE expr.
// Syntax: FIELD-SYMBOLS <fs> LIKE LINE OF itab.
Field_Symbol_Decl :: struct {
	using node: Decl,
	ident:      ^Ident, // The field symbol name (including angle brackets)
	typed:      ^Expr, // The type expression
}

// CONTROLS declaration control types
Control_Kind :: enum {
	Tableview, // TYPE TABLEVIEW USING SCREEN dynnr
	Tabstrip,  // TYPE TABSTRIP
}

// CONTROLS declaration
// Syntax: CONTROLS contrl TYPE TABLEVIEW USING SCREEN dynnr.
// Syntax: CONTROLS contrl TYPE TABSTRIP.
Controls_Decl :: struct {
	using node:  Decl,
	ident:       ^Ident,      // Control name
	kind:        Control_Kind, // TABLEVIEW or TABSTRIP
	screen_dynnr: ^Expr,       // Screen number for TABLEVIEW (nil for TABSTRIP)
}

// CONTROLS chain declaration
// Syntax: CONTROLS: name1 TYPE TABSTRIP, name2 TYPE TABLEVIEW USING SCREEN 100.
Controls_Chain_Decl :: struct {
	using node: Decl,
	decls:      [dynamic]^Controls_Decl,
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
	is_unique:  bool,
	is_default: bool,
	name:       ^Ident, // For named secondary keys
	components: [dynamic]^Ident, // Key components
}

Table_Type :: struct {
	using node:     Expr,
	kind:           Table_Kind,
	elem:           ^Expr,
	primary_key:    ^Table_Key,
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
	^For_Expr,
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
	^Loop_Stmt,
	^Clear_Stmt,
	^Message_Stmt,
	^Insert_Stmt,
	^Sort_Stmt,
	^Append_Stmt,
	^Read_Table_Stmt,
	^Authority_Check_Stmt,
	^Delete_Stmt,
	^Condense_Stmt,
	^Call_Function_Stmt,
	^Call_Function_Param,
	^Select_Stmt,
	^Select_Join,
	// Declarations
	^Bad_Decl,
	^Data_Inline_Decl,
	^Data_Typed_Decl,
	^Data_Typed_Chain_Decl,
	^Types_Decl,
	^Types_Chain_Decl,
	^Types_Struct_Decl,
	^Const_Decl,
	^Const_Chain_Decl,
	^Const_Struct_Decl,
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
	// Field symbols
	^Field_Symbol_Decl,
	// Controls
	^Controls_Decl,
	^Controls_Chain_Decl,
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
	^For_Expr,
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
	^Loop_Stmt,
	^Clear_Stmt,
	^Message_Stmt,
	^Insert_Stmt,
	^Sort_Stmt,
	^Append_Stmt,
	^Read_Table_Stmt,
	^Authority_Check_Stmt,
	^Delete_Stmt,
	^Condense_Stmt,
	^Call_Function_Stmt,
	^Select_Stmt,
	// Declarations
	^Bad_Decl,
	^Data_Inline_Decl,
	^Data_Typed_Decl,
	^Data_Typed_Chain_Decl,
	^Types_Decl,
	^Types_Chain_Decl,
	^Types_Struct_Decl,
	^Const_Decl,
	^Const_Chain_Decl,
	^Const_Struct_Decl,
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
	// Field symbols
	^Field_Symbol_Decl,
	// Controls
	^Controls_Decl,
	^Controls_Chain_Decl,
}
