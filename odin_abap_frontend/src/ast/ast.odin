package abap_frontend_ast

import "../tokenizer"

import "core:mem"

Node :: struct {
	range:            tokenizer.Range,
	derived:          Any_Node,
	leading_comments: [dynamic]string,
	trailing_comment: string,
}

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

File :: struct {
	using node: Node,
	stmts:      [dynamic]^Stmt,
	allocator:  mem.Allocator,
}

Binary_Op :: enum {
	Add,
	Subtract,
	Multiply,
	Divide,
	Integer_Divide,
	Modulo,
	Concatenate,
	Equal,
	Not_Equal,
	Less,
	Less_Equal,
	Greater,
	Greater_Equal,
	Contains_Only,
	Contains_Not_Only,
	Contains_Any,
	Contains_Not_Any,
	Contains_String,
	Contains_No_String,
	Covers_Pattern,
	Covers_No_Pattern,
	In,
	Not_In,
	Bit_And,
	Bit_Or,
	Bit_Xor,
	Bit_O,
	Bit_Z,
	Bit_M,
	And,
	Or,
	Is,
	Between,
	Like,
	Not_Like,
}

Unary_Op :: enum {
	Minus,
	Plus,
	Not,
}

Selector_Op :: enum {
	Dash, // -
	Arrow, // ->
	Fat_Arrow, // =>
	Tilde, // ~
}

// ABAP syntax: no valid source form; recovery placeholder for a missing or malformed expression.
Bad_Expr :: struct {
	using node: Expr,
}

// ABAP syntax: character string template delimited by pipes, for example `|Hello { name }|`.
Char_String_Template_Expr :: struct {
	using node: Expr,
	parts:      [dynamic]^Expr,
}

// ABAP syntax: literal text segment inside a `|...|` string template.
Template_Literal_Expr :: struct {
	using node: Expr,
	literal:    string,
}

// ABAP syntax: `{ expr [format_option = value ...] }` interpolation inside a string template.
Template_Interpolation_Expr :: struct {
	using node:   Expr,
	expr:         ^Expr,
	format_specs: [dynamic]^Expr,
}

// ABAP syntax: expression part of a string-template interpolation, before output options.
Template_Expr :: struct {
	using node: Expr,
	expr:       ^Expr,
}

// ABAP syntax: string-template output option such as `WIDTH = n` or `ALIGN = LEFT`.
Template_Format_Spec_Expr :: struct {
	using node: Expr,
	name:       string,
	value:      ^Expr,
}

// ABAP syntax: infix expression `left op right`, for example `a + b`, `a = b`, or `a AND b`.
Binary_Expr :: struct {
	using node: Expr,
	left:       ^Expr,
	op:         Binary_Op,
	right:      ^Expr,
}

// ABAP syntax: prefix unary expression such as `-amount` or `+count`.
Unary_Expr :: struct {
	using node: Expr,
	op:         Unary_Op,
	expr:       ^Expr,
}

// ABAP syntax: parenthesized expression `( expr )`.
Paren_Expr :: struct {
	using node: Expr,
	expr:       ^Expr,
}

// ABAP syntax: identifier reference such as `foo`, `<fs>`, or `/NS/name`.
Ident_Expr :: struct {
	using node: Expr,
	name:       string,
}

// ABAP syntax: literal token such as `123`, `'text'`, or a backtick string.
Literal_Expr :: struct {
	using node: Expr,
	value:      string,
}

Type_Ref_Key_Kind :: enum {
	Default,
	Empty,
	Unique,
	Non_Unique,
	Generic,
}

// ABAP syntax: type-reference key addition such as `WITH DEFAULT KEY` or `WITH UNIQUE KEY id`.
Type_Ref_Key_Clause :: struct {
	kind:       Type_Ref_Key_Kind,
	default_key: bool,
	sorted:     bool,
	hashed:     bool,
	name:       string,
	components: [dynamic]string,
}

Type_Ref_Path_Segment :: struct {
	name:  string,
	range: tokenizer.Range,
}

Raw_Operand_Inline_Decl_Kind :: enum {
	Data,
	Field_Symbol,
}

Raw_Operand_Inline_Decl :: struct {
	kind:  Raw_Operand_Inline_Decl_Kind,
	name:  string,
	range: tokenizer.Range,
}

Raw_Operand_Path_Segment :: struct {
	name:  string,
	range: tokenizer.Range,
}

Raw_Operand_Ref :: struct {
	name:      string,
	range:     tokenizer.Range,
	type_base: bool,
	call_like: bool,
	path:      [dynamic]Raw_Operand_Path_Segment,
}

// ABAP syntax: declaration type reference such as `ty_line`, `REF TO object`, or `ty_line WITH DEFAULT KEY`.
Type_Ref_Expr :: struct {
	using node: Expr,
	text:       string,
	name:       string,
	is_ref:     bool,
	base_name:  string,
	base_range: tokenizer.Range,
	path:       [dynamic]Type_Ref_Path_Segment,
	key:        ^Type_Ref_Key_Clause,
	keys:       [dynamic]^Type_Ref_Key_Clause,
	raw_operand: bool,
	raw_decls: [dynamic]Raw_Operand_Inline_Decl,
	raw_refs:  [dynamic]Raw_Operand_Ref,
}

// ABAP syntax: Open SQL host expression such as `@lv_value`.
Host_Expr :: struct {
	using node: Expr,
	value:      ^Expr,
}

// ABAP syntax: table expression such as `itab[ 1 ]` or `itab[ key = value ]`.
Table_Expr :: struct {
	using node: Expr,
	table:      ^Expr,
	selectors:  [dynamic]^Expr,
}

// ABAP syntax: component selector such as `base-field`, `base->field`, `class=>member`, or `iface~member`.
Selector_Expr :: struct {
	using node: Expr,
	base:       ^Expr,
	op:         Selector_Op,
	field:      ^Expr,
}

// ABAP syntax: offset/length access such as `text+off` or `text+off(len)`.
Substring_Expr :: struct {
	using node: Expr,
	base:       ^Expr,
	offset:     ^Expr,
	length:     ^Expr,
}

// ABAP syntax: functional or method call expression `callee( ... )`.
Call_Expr :: struct {
	using node: Expr,
	callee:     ^Expr,
	args:       ^Expr,
}

// ABAP syntax: parenthesized actual-parameter list in `callee( ... )`.
Call_Arg_List_Expr :: struct {
	using node: Expr,
	args:       [dynamic]^Expr,
}

Call_Arg_Section_Kind :: enum {
	Unknown,
	Exporting,
	Importing,
	Changing,
	Tables,
	Receiving,
	Exceptions,
}

// ABAP syntax: call parameter section such as `EXPORTING ...`, `IMPORTING ...`, or `CHANGING ...`.
Call_Arg_Section_Expr :: struct {
	using node: Expr,
	kind:       Call_Arg_Section_Kind,
	name:       string,
	args:       [dynamic]^Expr,
}

// ABAP syntax: named actual parameter `name = value`.
Call_Named_Arg_Expr :: struct {
	using node: Expr,
	name:       string,
	value:      ^Expr,
}

// ABAP syntax: positional actual parameter `value`.
Call_Positional_Arg_Expr :: struct {
	using node: Expr,
	value:      ^Expr,
}

Constructor_Kind :: enum {
	New,
	Value,
	Conv,
	Ref,
	Cast,
	Exact,
	Corresponding,
	Filter,
	Reduce,
	Switch,
	Cond,
	Throw,
}

Is_Predicate_Kind :: enum {
	Initial,
	Bound,
	Assigned,
	Requested,
	Supplied,
	Null,
}

Constructor_For_Kind :: enum {
	For_In,
	For_Then_Until,
	For_Then_While,
}

// ABAP syntax: constructor expression such as `VALUE type( ... )`, `NEW class( ... )`, or `CONV type( expr )`.
Constructor_Expr :: struct {
	using node: Expr,
	kind:       Constructor_Kind,
	type_ref:   ^Expr,
	args:       [dynamic]^Expr,
}

// ABAP syntax: `operand IS [NOT] INITIAL|BOUND|ASSIGNED|REQUESTED|SUPPLIED`.
Is_Predicate_Expr :: struct {
	using node: Expr,
	subject:    ^Expr,
	negated:    bool,
	kind:       Is_Predicate_Kind,
}

// ABAP syntax: `operand IS [NOT] INSTANCE OF type`.
Instance_Of_Predicate_Expr :: struct {
	using node: Expr,
	subject:    ^Expr,
	negated:    bool,
	type_ref:   ^Expr,
}

// ABAP syntax: `operand BETWEEN low AND high`.
Between_Expr :: struct {
	using node: Expr,
	subject:    ^Expr,
	low:        ^Expr,
	high:       ^Expr,
}

// ABAP SQL syntax: one `WHEN condition THEN result` arm inside a SQL `CASE` expression.
Sql_Case_When_Expr :: struct {
	using node: Expr,
	condition:  ^Expr,
	result:     ^Expr,
}

// ABAP SQL syntax: `CASE [operand] WHEN condition THEN result [ELSE result] END`.
Sql_Case_Expr :: struct {
	using node: Expr,
	operand:    ^Expr,
	whens:      [dynamic]^Expr,
	else_expr:  ^Expr,
}

// ABAP syntax: `LET name = value ... IN expr`.
Let_Expr :: struct {
	using node: Expr,
	bindings:   [dynamic]^Expr,
	body:       [dynamic]^Expr,
}

// ABAP syntax: one `name = value` binding inside a constructor `LET`.
Constructor_Let_Binding_Expr :: struct {
	using node: Expr,
	name:       string,
	value:      ^Expr,
}

// ABAP syntax: `WHEN condition THEN result` inside `COND` or `SWITCH`.
Constructor_When_Clause_Expr :: struct {
	using node: Expr,
	condition:  ^Expr,
	result:     ^Expr,
}

// ABAP syntax: `ELSE result` inside `COND` or `SWITCH`.
Constructor_Else_Clause_Expr :: struct {
	using node: Expr,
	result:     ^Expr,
}

// ABAP syntax: `FOR name IN source [WHERE (...)] ...` or `FOR name = init THEN next UNTIL|WHILE condition ...`.
Constructor_For_Clause_Expr :: struct {
	using node: Expr,
	kind:       Constructor_For_Kind,
	variable:   string,
	init:       ^Expr,
	then_expr:  ^Expr,
	condition:  ^Expr,
	source:     ^Expr,
	where_clause: ^Expr,
	body:       [dynamic]^Expr,
}

// ABAP syntax: `WHERE condition` inside a constructor `FOR ... IN` clause.
Constructor_Where_Clause_Expr :: struct {
	using node: Expr,
	condition:  ^Expr,
}

// ABAP syntax: `INIT name = value ...` inside `REDUCE`.
Constructor_Init_Clause_Expr :: struct {
	using node:  Expr,
	assignments: [dynamic]^Expr,
}

// ABAP syntax: `NEXT name = value ...` inside `REDUCE`.
Constructor_Next_Clause_Expr :: struct {
	using node:  Expr,
	assignments: [dynamic]^Expr,
}

// ABAP syntax: constructor component assignment `name = value`.
Constructor_Named_Assignment_Expr :: struct {
	using node: Expr,
	name:       string,
	value:      ^Expr,
}

// ABAP syntax: `BASE value` inside constructor expressions.
Constructor_Base_Clause_Expr :: struct {
	using node: Expr,
	value:      ^Expr,
}

// ABAP syntax: `LINES OF itab [FROM a] [TO b] [USING KEY key]`.
Constructor_Lines_Of_Clause_Expr :: struct {
	using node: Expr,
	source:     ^Expr,
	from:       ^Expr,
	to:         ^Expr,
	using_key:  string,
}

// ABAP syntax: `expr OPTIONAL` inside constructor expressions.
Constructor_Optional_Expr :: struct {
	using node: Expr,
	value:      ^Expr,
}

// ABAP syntax: `MAPPING ...` inside `CORRESPONDING`.
Constructor_Corresponding_Mapping_Clause_Expr :: struct {
	using node:  Expr,
	assignments: [dynamic]^Expr,
}

// ABAP syntax: one `dst = src [DEFAULT value] [MAPPING ...] [EXCEPT ...]` mapping entry.
Constructor_Corresponding_Mapping_Assignment_Expr :: struct {
	using node:            Expr,
	target:                string,
	source:                ^Expr,
	default_value:         ^Expr,
	discarding_duplicates: bool,
	mapping:               ^Expr,
	except:                ^Expr,
}

// ABAP syntax: `EXCEPT name ...` inside `CORRESPONDING`.
Constructor_Corresponding_Except_Clause_Expr :: struct {
	using node: Expr,
	names:      [dynamic]^Expr,
}

// ABAP syntax: inline declaration expression `DATA(name)`, mainly used at call sites for imported parameters.
Data_Inline_Name_Expr :: struct {
	using node: Expr,
	name:       string,
}

// ABAP syntax: inline declaration expression `FIELD-SYMBOL(<name>)`.
Field_Symbol_Inline_Name_Expr :: struct {
	using node: Expr,
	name:       string,
}

// ABAP syntax: DATA statement, for example `DATA name TYPE i.`.
Data_Decl :: struct {
	using node:  Decl,
	kind:        Decl_Clause_Kind,
	flags:       Decl_Clause_Flags,
	name:        string,
	paren_length: ^Paren_Length_Clause,
	length_clauses: [dynamic]Length_Clause,
	type_clause: ^Data_Type_Clause, // nil if untyped
	value_clause: ^Value_Clause,
	occurs:      ^Expr,
	include_ref: ^Expr,
	as_name:     string,
	renaming_suffix: string,
	read_only:   bool,
}

// ABAP syntax: inline DATA statement, for example `DATA(name) = 3.`.
Data_Inline_Decl :: struct {
	using node: Decl,
	name:       string,
	expr:       ^Expr,
}

// ABAP syntax: chained DATA declarations with or without type such as `DATA: a TYPE i, b TYPE string, c.`
Data_Chained_Decl :: struct {
	using node: Decl,
	decls:      [dynamic]Data_Chained_Branch,
}

Decl_Clause_Kind :: enum {
	Normal,
	Begin_Group,
	End_Group,
	Include_Type,
	Include_Structure,
}

Decl_Clause_Flag :: enum {
	Common_Part_Delimiter,
	With_Header_Line,
}
Decl_Clause_Flags :: bit_set[Decl_Clause_Flag]

// ABAP syntax: one entry inside a chained DATA statement, for example `a TYPE i` in `DATA: a TYPE i, b.`
Data_Chained_Branch :: struct {
	kind:            Decl_Clause_Kind,
	flags:           Decl_Clause_Flags,
	depth:           int,
	name:            string,
	paren_length:    ^Paren_Length_Clause,
	length_clauses:  [dynamic]Length_Clause,
	type_clause:     ^Data_Type_Clause, // nil if untyped
	value_clause:    ^Value_Clause,
	occurs:          ^Expr,
	include_ref:     ^Expr,
	as_name:         string,
	renaming_suffix: string,
	read_only:       bool,
}

// ABAP syntax: type-clause form keyword sequence, for example `TYPE REF TO`, `LIKE LINE OF`, or `TYPE STANDARD TABLE`.
Data_Type_Form :: enum {
	Type,
	Like,
	Structure,
	Ref_To,
	Like_Line_Of,
	Type_Line_Of,
	Table,
	Like_Table,
	Standard_Table,
	Sorted_Table,
	Hashed_Table,
	Like_Standard_Table,
	Like_Sorted_Table,
	Like_Hashed_Table,
	Range_Of,
}

// ABAP syntax: typed declaration addition such as `TYPE ty_line`, `LIKE other`, or `LIKE LINE OF itab`.
Data_Type_Clause :: struct {
	form:     Data_Type_Form,
	type_ref: ^Expr,
}

// ABAP syntax: TYPES statement, for example `TYPES ty TYPE i.`
Types_Decl :: struct {
	using node: Decl,
	types:      [dynamic]Types_Clause,
}

// ABAP syntax: one TYPES entry, for example `ty_i TYPE i` in `TYPES: ty_i TYPE i.`
Types_Clause :: struct {
	kind:            Decl_Clause_Kind,
	flags:           Decl_Clause_Flags,
	depth:           int,
	name:           string,
	paren_length:   ^Paren_Length_Clause,
	length_clauses: [dynamic]Length_Clause,
	type_clause:    ^Data_Type_Clause,
	occurs:          ^Expr,
	include_ref:     ^Expr,
	as_name:         string,
	renaming_suffix: string,
}

// ABAP syntax: CONSTANTS statement, for example `CONSTANTS c TYPE i VALUE 1.`
Constants_Decl :: struct {
	using node: Decl,
	constants:  [dynamic]Constants_Clause,
}

// ABAP syntax: one CONSTANTS entry, for example `c TYPE i VALUE 1`.
Constants_Clause :: struct {
	kind:           Decl_Clause_Kind,
	flags:          Decl_Clause_Flags,
	depth:          int,
	name:           string,
	paren_length:   ^Paren_Length_Clause,
	length_clauses: [dynamic]Length_Clause,
	type_clause:    ^Data_Type_Clause,
	value_clause:   ^Value_Clause,
	occurs:         ^Expr,
	include_ref:    ^Expr,
	as_name:        string,
	renaming_suffix: string,
}

// ABAP syntax: FIELD-SYMBOLS statement, for example `FIELD-SYMBOLS <fs> TYPE any.`
Field_Symbols_Decl :: struct {
	using node:    Decl,
	field_symbols: [dynamic]Field_Symbols_Clause,
}

// ABAP syntax: one FIELD-SYMBOLS entry, for example `<fs> TYPE any` or `<line> LIKE LINE OF itab`.
Field_Symbols_Clause :: struct {
	name:        string,
	type_clause: ^Data_Type_Clause,
}

// ABAP syntax: STATICS statement, for example `STATICS counter TYPE i.`
Statics_Decl :: struct {
	using node: Decl,
	statics:    [dynamic]Statics_Clause,
}

// ABAP syntax: one STATICS entry, for example `counter TYPE i VALUE 0`.
Statics_Clause :: struct {
	kind:           Decl_Clause_Kind,
	flags:          Decl_Clause_Flags,
	depth:          int,
	name:           string,
	paren_length:   ^Paren_Length_Clause,
	length_clauses: [dynamic]Length_Clause,
	type_clause:    ^Data_Type_Clause,
	value_clause:   ^Value_Clause,
	occurs:         ^Expr,
	include_ref:    ^Expr,
	as_name:        string,
	renaming_suffix: string,
}

// ABAP syntax: TABLES statement, for example `TABLES mara.`
Tables_Decl :: struct {
	using node: Decl,
	tables:     [dynamic]Tables_Clause,
}

// ABAP syntax: one TABLES work-area entry, for example `mara`.
Tables_Clause :: struct {
	name: string,
}

// ABAP syntax: RANGES statement, for example `RANGES r FOR mara-matnr.`
Ranges_Decl :: struct {
	using node: Decl,
	ranges:     [dynamic]Ranges_Clause,
}

// ABAP syntax: one RANGES entry, for example `r_matnr FOR mara-matnr`.
Ranges_Clause :: struct {
	name:       string,
	for_clause: ^For_Clause,
}

// ABAP syntax: PARAMETERS statement, for example `PARAMETERS p TYPE i.`
Parameters_Decl :: struct {
	using node: Decl,
	text:       string,
	parameters: [dynamic]Parameters_Clause,
}

// ABAP syntax: flag-style PARAMETERS additions such as `AS CHECKBOX`, `LOWER CASE`, or `OBLIGATORY`.
Parameter_Flag :: enum {
	As_Checkbox,
	Lower_Case,
	Obligatory,
	No_Display,
	Value_Check,
	Help_Request,
	Value_Request,
}
Parameter_Flags :: bit_set[Parameter_Flag]

// ABAP syntax: one PARAMETERS entry, for example `p_count TYPE i DEFAULT 1`.
Parameters_Clause :: struct {
	name:              string,
	paren_length:      ^Paren_Length_Clause,
	length_clauses:    [dynamic]Length_Clause,
	type_clause:       ^Data_Type_Clause,
	default_clause:    ^Default_Clause,
	flags:             Parameter_Flags,
	radiobutton_group: ^Radiobutton_Group_Clause,
	user_command:      ^User_Command_Clause,
	modif_id:          ^Modif_Id_Clause,
	memory_id:         ^Memory_Id_Clause,
	matchcode_object:  ^Matchcode_Object_Clause,
	visible_length:    ^Visible_Length_Clause,
}

// ABAP syntax: SELECT-OPTIONS statement, for example `SELECT-OPTIONS s FOR mara-matnr.`
Select_Options_Decl :: struct {
	using node: Decl,
	options:    [dynamic]Select_Options_Clause,
}

// ABAP syntax: flag-style SELECT-OPTIONS additions such as `NO-DISPLAY`, `NO-EXTENSION`, or `NO INTERVALS`.
Select_Option_Flag :: enum {
	Lower_Case,
	Obligatory,
	No_Display,
	No_Extension,
	No_Intervals,
	No_Database_Selection,
}
Select_Option_Flags :: bit_set[Select_Option_Flag]

// ABAP syntax: selection-screen request addition kind, either `HELP-REQUEST` or `VALUE-REQUEST`.
Selection_Request_Kind :: enum {
	Help_Request,
	Value_Request,
}

// ABAP syntax: one SELECT-OPTIONS entry, for example `s_matnr FOR mara-matnr DEFAULT 'A' TO 'Z'`.
Select_Options_Clause :: struct {
	name:             string,
	for_clause:       ^For_Clause,
	default_clause:   ^Default_Clause,
	to_clause:        ^To_Clause,
	option_clause:    ^Option_Clause,
	sign_clause:      ^Sign_Clause,
	flags:            Select_Option_Flags,
	modif_id:         ^Modif_Id_Clause,
	memory_id:        ^Memory_Id_Clause,
	matchcode_object: ^Matchcode_Object_Clause,
	visible_length:   ^Visible_Length_Clause,
	help_request:     ^Selection_Request_Clause,
	value_request:    ^Selection_Request_Clause,
}

// ABAP syntax: CONTROLS statement, for example `CONTROLS tc TYPE TABLEVIEW USING SCREEN 100.`
Controls_Decl :: struct {
	using node: Decl,
	controls:   [dynamic]Controls_Clause,
}

// ABAP syntax: one CONTROLS entry, for example `tc TYPE TABLEVIEW USING SCREEN 100`.
Controls_Clause :: struct {
	name:         string,
	type_clause:  ^Data_Type_Clause,
	using_screen: ^Using_Screen_Clause,
}

// ABAP syntax: CLASS-DATA statement, for example `CLASS-DATA gv TYPE i.`
Class_Data_Decl :: struct {
	using node: Decl,
	decls:      [dynamic]Class_Data_Clause,
}

// ABAP syntax: one CLASS-DATA entry, for example `gv TYPE i VALUE 0`.
Class_Data_Clause :: struct {
	kind:           Decl_Clause_Kind,
	flags:          Decl_Clause_Flags,
	depth:          int,
	name:           string,
	paren_length:   ^Paren_Length_Clause,
	length_clauses: [dynamic]Length_Clause,
	type_clause:    ^Data_Type_Clause,
	value_clause:   ^Value_Clause,
	occurs:         ^Expr,
	include_ref:    ^Expr,
	as_name:        string,
	renaming_suffix: string,
	read_only:      bool,
}

// ABAP syntax: TYPE-POOLS statement, for example `TYPE-POOLS abap.`
Type_Pools_Decl :: struct {
	using node: Decl,
	pools:      [dynamic]string,
}

// ABAP syntax: FUNCTION-POOL statement, for example `FUNCTION-POOL zfg MESSAGE-ID sv.`
Function_Pool_Decl :: struct {
	using node:  Decl,
	name:        string,
	message_id:  string,
}

Include_Name :: struct {
	name:  string,
	range: tokenizer.Range,
}

// ABAP syntax: program include statement, for example `INCLUDE zinc.` or `INCLUDE: ztop, zf01.`
Include_Stmt :: struct {
	using node: Stmt,
	names:      [dynamic]Include_Name,
	if_found:   bool,
}

// ABAP syntax: legacy parenthesized length after a declaration name, for example `c(14)` or `p_pass(30)`.
Paren_Length_Clause :: struct {
	expr: ^Expr,
}

Length_Clause_Kind :: enum {
	Length,
	Decimals,
}

// ABAP syntax: declaration length or decimal addition, for example `LENGTH 3` or `DECIMALS 7`.
Length_Clause :: struct {
	kind: Length_Clause_Kind,
	expr: ^Expr,
}

// ABAP syntax: declaration value addition, for example `VALUE 1`.
Value_Clause :: struct {
	expr:       ^Expr,
	is_initial: bool,
}

// ABAP syntax: selection parameter default addition, for example `DEFAULT 'X'`.
Default_Clause :: struct {
	expr: ^Expr,
}

// ABAP syntax: declaration reference addition, for example `FOR mara-matnr` in RANGES or SELECT-OPTIONS.
For_Clause :: struct {
	expr: ^Expr,
}

// ABAP syntax: CONTROLS screen binding addition, for example `USING SCREEN 100`.
Using_Screen_Clause :: struct {
	screen: ^Expr,
}

// ABAP syntax: SELECT-OPTIONS upper-bound default addition, for example `TO 'Z'`.
To_Clause :: struct {
	expr: ^Expr,
}

// ABAP syntax: SELECT-OPTIONS option addition, for example `OPTION BT`.
Option_Clause :: struct {
	option: string,
}

// ABAP syntax: SELECT-OPTIONS sign addition, for example `SIGN I`.
Sign_Clause :: struct {
	sign: string,
}

// ABAP syntax: PARAMETERS radio-button group addition, for example `RADIOBUTTON GROUP g01`.
Radiobutton_Group_Clause :: struct {
	group: string,
}

// ABAP syntax: PARAMETERS user-command addition, for example `USER-COMMAND upd`.
User_Command_Clause :: struct {
	command: string,
}

// ABAP syntax: selection-screen modification group addition, for example `MODIF ID grp`.
Modif_Id_Clause :: struct {
	id: string,
}

// ABAP syntax: selection-screen memory id addition, for example `MEMORY ID gln`.
Memory_Id_Clause :: struct {
	id: ^Expr,
}

// ABAP syntax: matchcode object addition, for example `MATCHCODE OBJECT /sttp/h_loc_gln`.
Matchcode_Object_Clause :: struct {
	object: ^Expr,
}

// ABAP syntax: selection-screen visible length addition, for example `VISIBLE LENGTH 20`.
Visible_Length_Clause :: struct {
	length: ^Expr,
}

// ABAP syntax: selection-screen request addition, for example `HELP-REQUEST FOR LOW` or `VALUE-REQUEST FOR HIGH`.
Selection_Request_Clause :: struct {
	kind:   Selection_Request_Kind,
	target: string,
}

// ABAP syntax: assignment statement `lhs = rhs.`
Assign_Stmt :: struct {
	using node: Stmt,
	lhs:        ^Expr,
	rhs:        ^Expr,
}

// ABAP syntax: cast assignment statement `lhs ?= rhs.`
Downcast_Assign_Stmt :: struct {
	using node: Stmt,
	lhs:        ^Expr,
	rhs:        ^Expr,
}

// ABAP syntax: expression statement retained for parser-surface template chunks.
Expr_Stmt :: struct {
	using node: Stmt,
	expr:       ^Expr,
}

Clear_Mode :: enum {
	Default,
	With_Value,
	Initial,
}

// ABAP syntax: one CLEAR operand, for example `lv` or `lv WITH 'X'`.
Clear_Operand_Clause :: struct {
	target: ^Expr,
	mode:   Clear_Mode,
	value:  ^Expr,
}

// ABAP syntax: `CLEAR dobj [WITH value].`
Clear_Stmt :: struct {
	using node: Stmt,
	operands:   [dynamic]Clear_Operand_Clause,
}

// ABAP syntax: one REFRESH operand, for example `itab` or `TABLE itab`.
Refresh_Operand_Clause :: struct {
	target: ^Expr,
	table:  bool,
}

// ABAP syntax: `REFRESH [TABLE] itab.`
Refresh_Stmt :: struct {
	using node: Stmt,
	operands:   [dynamic]Refresh_Operand_Clause,
}

// ABAP syntax: one FREE operand, for example `itab` or `OBJECT obj`.
Free_Operand_Clause :: struct {
	target: ^Expr,
	object: bool,
}

// ABAP syntax: `FREE dobj.`, `FREE OBJECT obj.`, `FREE MEMORY.`, or `FREE MEMORY ID id.`
Free_Stmt :: struct {
	using node: Stmt,
	operands:   [dynamic]Free_Operand_Clause,
	memory:     bool,
	memory_id:  ^Expr,
}

// ABAP syntax: one UNASSIGN operand, for example `<fs>`.
Unassign_Operand_Clause :: struct {
	target: ^Expr,
}

// ABAP syntax: `UNASSIGN <fs>.`
Unassign_Stmt :: struct {
	using node: Stmt,
	operands:   [dynamic]Unassign_Operand_Clause,
}

// ABAP syntax: one MOVE entry, for example `source TO target`.
Move_Entry_Clause :: struct {
	source: ^Expr,
	target: ^Expr,
}

// ABAP syntax: `MOVE source TO target.`
Move_Stmt :: struct {
	using node: Stmt,
	entries:    [dynamic]Move_Entry_Clause,
}

// ABAP syntax: one ADD entry, for example `source TO target [GIVING result]`.
Add_Entry_Clause :: struct {
	source: ^Expr,
	target: ^Expr,
	result: ^Expr,
}

// ABAP syntax: `ADD source TO target [GIVING result].`
Add_Stmt :: struct {
	using node: Stmt,
	entries:    [dynamic]Add_Entry_Clause,
}

// ABAP syntax: one SUBTRACT entry, for example `source FROM target [GIVING result]`.
Subtract_Entry_Clause :: struct {
	source: ^Expr,
	target: ^Expr,
	result: ^Expr,
}

// ABAP syntax: `SUBTRACT source FROM target [GIVING result].`
Subtract_Stmt :: struct {
	using node: Stmt,
	entries:    [dynamic]Subtract_Entry_Clause,
}

// ABAP syntax: one MULTIPLY entry, for example `target BY source [GIVING result]`.
Multiply_Entry_Clause :: struct {
	target: ^Expr,
	source: ^Expr,
	result: ^Expr,
}

// ABAP syntax: `MULTIPLY target BY source [GIVING result].`
Multiply_Stmt :: struct {
	using node: Stmt,
	entries:    [dynamic]Multiply_Entry_Clause,
}

Divide_Form :: enum {
	By,
	Into,
}

// ABAP syntax: one DIVIDE entry, for example `target BY source GIVING result` or `source INTO target`.
Divide_Entry_Clause :: struct {
	form:   Divide_Form,
	source: ^Expr,
	target: ^Expr,
	result: ^Expr,
}

// ABAP syntax: `DIVIDE a BY b [GIVING c]` or `DIVIDE a INTO b [GIVING c].`
Divide_Stmt :: struct {
	using node: Stmt,
	entries:    [dynamic]Divide_Entry_Clause,
}

// ABAP syntax: one COMPUTE entry, for example `[EXACT] target = source`.
Compute_Entry_Clause :: struct {
	exact:  bool,
	target: ^Expr,
	source: ^Expr,
}

// ABAP syntax: `COMPUTE [EXACT] target = source.`
Compute_Stmt :: struct {
	using node: Stmt,
	entries:    [dynamic]Compute_Entry_Clause,
}

// ABAP syntax: one CONCATENATE entry, for example `a b INTO c SEPARATED BY sep`.
Concatenate_Entry_Clause :: struct {
	sources:           [dynamic]^Expr,
	lines_of:          bool,
	target:            ^Expr,
	separator:         ^Expr,
	respecting_blanks: bool,
}

// ABAP syntax: `CONCATENATE source... INTO target [SEPARATED BY sep] [RESPECTING BLANKS].`
Concatenate_Stmt :: struct {
	using node: Stmt,
	entries:    [dynamic]Concatenate_Entry_Clause,
	byte_mode:  bool,
}

// ABAP syntax: one SPLIT entry, for example `source AT sep INTO a b`.
Split_Entry_Clause :: struct {
	source:     ^Expr,
	separator:  ^Expr,
	targets:    [dynamic]^Expr,
	into_table: bool,
}

// ABAP syntax: `SPLIT source AT sep INTO [TABLE] target... .`
Split_Stmt :: struct {
	using node: Stmt,
	entries:    [dynamic]Split_Entry_Clause,
}

// ABAP syntax: `CONDENSE text [NO-GAPS].`
Condense_Stmt :: struct {
	using node: Stmt,
	target:     ^Expr,
	no_gaps:    bool,
}

Replace_Occurrence :: enum {
	Default,
	First,
	All,
}

// ABAP syntax: `REPLACE [FIRST|ALL OCCURRENCES OF] [REGEX] pattern IN [TABLE] target WITH replacement.`
Replace_Stmt :: struct {
	using node:  Stmt,
	occurrence:  Replace_Occurrence,
	regex:       bool,
	pattern:     ^Expr,
	target:      ^Expr,
	replacement: ^Expr,
	in_table:    bool,
}

Translate_Form :: enum {
	Default,
	To_Upper,
	To_Lower,
	Using,
	To_Code_Page,
	From_Code_Page,
	To_Number_Format,
	From_Number_Format,
}

// ABAP syntax: `TRANSLATE text TO UPPER CASE`, `TRANSLATE text USING mask`, or code-page/number-format variants.
Translate_Stmt :: struct {
	using node: Stmt,
	target:     ^Expr,
	form:       Translate_Form,
	operand:    ^Expr,
}

Shift_Direction :: enum {
	Default,
	Left,
	Right,
}
Shift_Delete_Direction :: enum {
	None,
	Leading,
	Trailing,
}

// ABAP syntax: `SHIFT text [LEFT|RIGHT|CIRCULAR] [BY n PLACES] [DELETING LEADING|TRAILING pattern].`
Shift_Stmt :: struct {
	using node:       Stmt,
	target:           ^Expr,
	direction:        Shift_Direction,
	places:           ^Expr,
	circular:         bool,
	delete_direction: Shift_Delete_Direction,
	delete_pattern:   ^Expr,
}

Find_Occurrence :: enum {
	Default,
	First,
	All,
}

// ABAP syntax: `FIND [FIRST|ALL OCCURRENCES OF] [REGEX] pattern IN target ...`.
Find_Stmt :: struct {
	using node:   Stmt,
	occurrence:   Find_Occurrence,
	regex:        bool,
	pattern:      ^Expr,
	target:       ^Expr,
	match_offset: ^Expr,
	match_length: ^Expr,
	match_count:  ^Expr,
	results:      ^Expr,
	submatches:   [dynamic]^Expr,
}

// ABAP syntax: `SEARCH text FOR pattern [STARTING AT pos] [ENDING AT pos] [ABBREVIATED].`
Search_Stmt :: struct {
	using node:  Stmt,
	target:      ^Expr,
	pattern:     ^Expr,
	starting_at: ^Expr,
	ending_at:   ^Expr,
	abbreviated: bool,
}

// ABAP syntax: `PERFORM form [IN PROGRAM prog] [TABLES ...] [USING ...] [CHANGING ...] [IF FOUND].`
Perform_Stmt :: struct {
	using node: Stmt,
	form:       ^Expr,
	program:    ^Expr,
	tables:     [dynamic]^Expr,
	using_args: [dynamic]^Expr,
	changing:   [dynamic]^Expr,
	if_found:   bool,
}

Call_Kind :: enum {
	Direct,
	Method,
	Function,
	Customer_Function,
	Database_Procedure,
	Transformation,
	Badi,
	Screen,
	Selection_Screen,
	Transaction,
	Dialog,
	Subscreen,
}

Call_Stmt_Arg_Section :: struct {
	kind:  Call_Arg_Section_Kind,
	range: tokenizer.Range,
}

Call_Stmt_Named_Arg :: struct {
	section:     Call_Arg_Section_Kind,
	has_section: bool,
	name:        string,
	name_range:  tokenizer.Range,
	value_range: tokenizer.Range,
	raw_decls:   [dynamic]Raw_Operand_Inline_Decl,
	raw_refs:    [dynamic]Raw_Operand_Ref,
}

Call_Transformation_Arg_Kind :: enum {
	Options,
	Parameters,
	Source,
	Result,
}

Call_Transformation_Arg :: struct {
	kind:       Call_Transformation_Arg_Kind,
	name:       string,
	name_range: tokenizer.Range,
	has_eq:     bool,
	value:      ^Expr,
}

// ABAP syntax: `CALL METHOD target`, `CALL FUNCTION fm`, and related CALL variants; direct call statements use `call`.
Call_Stmt :: struct {
	using node:    Stmt,
	kind:          Call_Kind,
	call:          ^Expr,
	target:        ^Expr,
	arg_sections:  [dynamic]Call_Stmt_Arg_Section,
	named_args:    [dynamic]Call_Stmt_Named_Arg,
	transaction_operands: [dynamic]^Expr,
	transformation_args:  [dynamic]Call_Transformation_Arg,
}

Submit_Option_Kind :: enum {
	Using_Selection_Screen,
	Using_Selection_Set,
	Using_Selection_Sets_Of_Program,
	With_Selection_Table,
	With_Free_Selections,
	With_Parameter,
	Line_Size,
	Line_Count,
	User,
	Via_Job,
	Number,
	Language,
}

Submit_Option_Operator :: enum {
	None,
	Assign,
	Eq,
	Ne,
	Bt,
	Nb,
	Cp,
	Np,
	Ge,
	Gt,
	Le,
	Lt,
	Other,
}

// ABAP syntax: one SUBMIT option with a compact operand, for example `WITH p = v` or `LINE-SIZE n`.
Submit_Option_Clause :: struct {
	kind:       Submit_Option_Kind,
	name:       string,
	operator:   Submit_Option_Operator,
	value:      ^Expr,
	high_value: ^Expr,
}

// ABAP syntax: `SUBMIT report ... [AND RETURN].`
Submit_Stmt :: struct {
	using node:               Stmt,
	target:                   ^Expr,
	options:                  [dynamic]Submit_Option_Clause,
	via_selection_screen:     bool,
	exporting_list_to_memory: bool,
	to_sap_spool:             bool,
	without_spool_dynpro:     bool,
	and_return:               bool,
}

// ABAP syntax: MESSAGE head, for example `e001(id)` or `ID id TYPE type NUMBER number`.
Message_Head_Clause :: struct {
	code:     ^Expr,
	id:       ^Expr,
	msg_type: ^Expr,
	number:   ^Expr,
	compact_class_name:  string,
	compact_class_range: tokenizer.Range,
	has_compact_class:   bool,
}

// ABAP syntax: `MESSAGE ... [WITH ...] [INTO target] [DISPLAY LIKE type] [RAISING cx].`
Message_Stmt :: struct {
	using node:   Stmt,
	head:         ^Message_Head_Clause,
	with_args:    [dynamic]^Expr,
	into:         ^Expr,
	display_like: ^Expr,
	raising:      ^Expr,
}

// ABAP syntax: one WRITE operand, for example `/ value`, `AT pos(len)`, or `value`.
Write_Operand_Clause :: struct {
	value:      ^Expr,
	line_break: bool,
	position:   ^Expr,
	length:     ^Expr,
}

// ABAP syntax: `WRITE [/] value ... .`
Write_Stmt :: struct {
	using node: Stmt,
	operands:   [dynamic]Write_Operand_Clause,
}

// ABAP syntax: `ASSERT condition.`
Assert_Stmt :: struct {
	using node: Stmt,
	condition:  ^Expr,
}

// ABAP syntax: `CHECK condition.`
Check_Stmt :: struct {
	using node: Stmt,
	condition:  ^Expr,
}

Flow_Kind :: enum {
	Return,
	Continue,
	Exit,
	Stop,
}

// ABAP syntax: simple control-flow statements such as `RETURN.`, `CONTINUE.`, `EXIT.`, or `STOP.`
Flow_Stmt :: struct {
	using node: Stmt,
	kind:       Flow_Kind,
}

Transaction_Kind :: enum {
	Commit,
	Rollback,
}

// ABAP syntax: `COMMIT WORK [AND WAIT].` or `ROLLBACK WORK.`
Transaction_Stmt :: struct {
	using node: Stmt,
	kind:       Transaction_Kind,
	wait:       bool,
}

// ABAP syntax: one `DESCRIBE TABLE itab LINES target` entry.
Describe_Entry_Clause :: struct {
	source: ^Expr,
	target: ^Expr,
	table:  bool,
}

// ABAP syntax: `DESCRIBE ... .`
Describe_Stmt :: struct {
	using node: Stmt,
	entries:    [dynamic]Describe_Entry_Clause,
}

Runtime_Kind :: enum {
	Get,
	Set,
	Log_Point,
	Set_Handler,
	Get_Badi,
	Export,
	Import,
	Receive,
}

Runtime_Subject :: enum {
	None,
	Run_Time_Field,
	Time_Stamp_Field,
	Parameter_ID_Field,
	Cursor,
	Reference,
	PF_Status,
	Titlebar,
	Screen,
	User_Command,
	Badi,
	Handler,
	Update_Task_Local,
}

// ABAP syntax: compact runtime/environment statements such as `GET`, `SET`, `LOG-POINT`, `SET HANDLER`, and `GET BADI`.
Runtime_Stmt :: struct {
	using node: Stmt,
	kind:       Runtime_Kind,
	subject:    Runtime_Subject,
	id:         ^Expr,
	field:      ^Expr,
	target:     ^Expr,
	value:      ^Expr,
	line:       ^Expr,
	offset:     ^Expr,
	excluding:  [dynamic]^Expr,
	operands:   [dynamic]^Expr,
}

// ABAP syntax: one `ID auth_field FIELD value` clause inside `AUTHORITY-CHECK OBJECT`.
Authority_Check_ID_Clause :: struct {
	id:    ^Expr,
	field: ^Expr,
}

Raise_Kind :: enum {
	Exception,
	Event,
}

// ABAP syntax: `RAISE EXCEPTION ...` or `RAISE EVENT ...`.
Raise_Stmt :: struct {
	using node: Stmt,
	kind:       Raise_Kind,
	target_type: bool,
	target:     ^Expr,
	operands:   [dynamic]^Expr,
}

// ABAP syntax: `AUTHORITY-CHECK ...`.
Authority_Check_Stmt :: struct {
	using node: Stmt,
	operands:   [dynamic]^Expr,
	object:     ^Expr,
	ids:        [dynamic]Authority_Check_ID_Clause,
}

// ABAP syntax: `FIELD-GROUPS ...`.
Field_Groups_Stmt :: struct {
	using node: Stmt,
	groups:     [dynamic]^Expr,
}

// ABAP syntax: `INSERT DUMMY INTO fg.`
Insert_Dummy_Stmt :: struct {
	using node: Stmt,
	target:     ^Expr,
}

// ABAP syntax: dynpro `FIELD ...` statements.
Field_Stmt :: struct {
	using node: Stmt,
	operands:   [dynamic]^Expr,
}

// ABAP syntax: field-symbol assignment `ASSIGN ... TO <fs>.`
Assign_Field_Stmt :: struct {
	using node: Stmt,
	operands:   [dynamic]^Expr,
}

// ABAP syntax: object creation `CREATE OBJECT ref ...`.
Create_Object_Stmt :: struct {
	using node: Stmt,
	operands:   [dynamic]^Expr,
}

Text_Transform_Kind :: enum {
	Overlay,
	Pack,
	Unpack,
	Convert,
	Wait,
}

// ABAP syntax: compact classic string/runtime statements such as `OVERLAY`, `PACK`, `UNPACK`, `CONVERT`, and `WAIT`.
Text_Transform_Stmt :: struct {
	using node: Stmt,
	kind:       Text_Transform_Kind,
	operands:   [dynamic]^Expr,
}

List_Control_Kind :: enum {
	Skip,
	Uline,
	New_Line,
	New_Page,
	Reserve,
	Back,
	Format,
	Position,
	Hide,
}

// ABAP syntax: list-control statements such as `SKIP`, `ULINE`, `NEW-LINE`, `NEW-PAGE`, and `RESERVE`.
List_Control_Stmt :: struct {
	using node: Stmt,
	kind:       List_Control_Kind,
	operands:   [dynamic]^Expr,
}

Line_Kind :: enum {
	Read,
	Modify,
}

Line_Field_Value_Clause :: struct {
	field:  ^Expr,
	target: ^Expr,
}

// ABAP syntax: list-buffer line access such as `READ LINE n ...` or `MODIFY CURRENT LINE ...`.
Line_Stmt :: struct {
	using node: Stmt,
	kind:       Line_Kind,
	current:    bool,
	line:       ^Expr,
	index:      ^Expr,
	into:       ^Expr,
	fields:     [dynamic]Line_Field_Value_Clause,
}

// ABAP syntax: `DEFINE name. ... END-OF-DEFINITION.`
Macro_Def_Stmt :: struct {
	using node: Stmt,
	name:       string,
	body:       string,
}

// ABAP syntax: macro invocation such as `macro arg1 arg2.`
Macro_Call_Stmt :: struct {
	using node: Stmt,
	name:       string,
	args:       [dynamic]^Expr,
}

// ABAP syntax: `SELECTION-SCREEN ...`, including COMMENT-generated text fields.
Selection_Screen_Stmt :: struct {
	using node:   Stmt,
	text:         string,
	title_name:   string,
	title_range:  tokenizer.Range,
	comment_name: string,
	comment_range: tokenizer.Range,
	field_name:   string,
	field_range:  tokenizer.Range,
}

Oop_Simple_Kind :: enum {
	Class_Section,
	Methods,
	Class_Methods,
	Interfaces,
	Events,
	Class_Events,
	Aliases,
	Class_Deferred,
	Interface_Deferred,
	Class_Load,
	Interface_Load,
}

Oop_Visibility :: enum {
	Unspecified,
	Public,
	Protected,
	Private,
}

Oop_Signature_Kind :: enum {
	Importing,
	Exporting,
	Changing,
	Receiving,
	Returning,
	Raising,
	Exceptions,
	For,
}

Oop_Parameter_Clause :: struct {
	name:        string,
	range:       tokenizer.Range,
	type_clause: ^Data_Type_Clause,
	optional:    bool,
}

Oop_Signature_Clause :: struct {
	kind:       Oop_Signature_Kind,
	values:     [dynamic]^Expr,
	parameters: [dynamic]Oop_Parameter_Clause,
}

Oop_Member_Flag :: enum {
	Redefinition,
}
Oop_Member_Flags :: bit_set[Oop_Member_Flag]

Oop_Member_Clause :: struct {
	name:       string,
	flags:      Oop_Member_Flags,
	signatures: [dynamic]Oop_Signature_Clause,
}

// ABAP syntax: class/interface member declarations handled as simple OOP statements.
Oop_Simple_Stmt :: struct {
	using node: Stmt,
	kind:       Oop_Simple_Kind,
	visibility: Oop_Visibility,
	text:       string,
	members:    [dynamic]Oop_Member_Clause,
}

// ABAP syntax: `ELSEIF condition.` arm inside an IF block.
Elseif_Clause :: struct {
	range:     tokenizer.Range,
	condition: ^Expr,
	body:      [dynamic]^Stmt,
}

// ABAP syntax: `ELSE.` arm inside an IF block.
Else_Clause :: struct {
	range: tokenizer.Range,
	body:  [dynamic]^Stmt,
}

// ABAP syntax: `IF cond. ... [ELSEIF cond. ...] [ELSE. ...] ENDIF.`
If_Stmt :: struct {
	using node:     Stmt,
	condition:      ^Expr,
	body:           [dynamic]^Stmt,
	elseif_clauses: [dynamic]^Elseif_Clause,
	else_clause:    ^Else_Clause,
}

// ABAP syntax: `WHEN ... .` arm inside a CASE block.
When_Clause :: struct {
	range:     tokenizer.Range,
	operands:  [dynamic]^Expr,
	is_others: bool,
	body:      [dynamic]^Stmt,
}

// ABAP syntax: `CASE expr. WHEN ... ENDCASE.`
Case_Stmt :: struct {
	using node: Stmt,
	expr:       ^Expr,
	whens:      [dynamic]^When_Clause,
	recovery:   [dynamic]^Stmt,
	is_type_of: bool,
}

// ABAP syntax: `WHILE cond. ... ENDWHILE.`
While_Stmt :: struct {
	using node: Stmt,
	condition:  ^Expr,
	body:       [dynamic]^Stmt,
}

// ABAP syntax: `DO [count TIMES]. ... ENDDO.`
Do_Stmt :: struct {
	using node: Stmt,
	count:      ^Expr,
	body:       [dynamic]^Stmt,
}

// ABAP syntax: `LOOP AT source ... ENDLOOP.`
Loop_Stmt :: struct {
	using node:   Stmt,
	source:       ^Expr,
	body:         [dynamic]^Stmt,
	header_range: tokenizer.Range,
	header_text:  string,
}

At_Stmt_Kind :: enum {
	First,
	Last,
	New,
	End_Of,
}

// ABAP syntax: group-processing block `AT FIRST. ... ENDAT.` or `AT NEW field. ... ENDAT.`
At_Stmt :: struct {
	using node: Stmt,
	kind:       At_Stmt_Kind,
	expr:       ^Expr,
	body:       [dynamic]^Stmt,
}

// ABAP syntax: `CATCH cx_root [INTO target].` arm inside a TRY block.
Catch_Clause :: struct {
	range:      tokenizer.Range,
	exceptions: [dynamic]^Expr,
	into:       ^Expr,
	body:       [dynamic]^Stmt,
}

// ABAP syntax: `CLEANUP.` arm inside a TRY block.
Cleanup_Clause :: struct {
	range: tokenizer.Range,
	body:  [dynamic]^Stmt,
}

// ABAP syntax: `TRY. ... CATCH ... CLEANUP. ... ENDTRY.`
Try_Stmt :: struct {
	using node: Stmt,
	body:       [dynamic]^Stmt,
	catches:    [dynamic]^Catch_Clause,
	cleanup:    ^Cleanup_Clause,
}

Class_Decl_Flag :: enum {
	Implementation,
	Bodyless,
	Abstract,
}

Class_Decl_Flags :: bit_set[Class_Decl_Flag]

Class_Decl :: struct {
	using node:        Stmt,
	name:              string,
	body:              [dynamic]^Stmt,
	header_range:      tokenizer.Range,
	header_text:       string,
	flags:             Class_Decl_Flags,
	superclass_name:   string,
	superclass_range:  tokenizer.Range,
}

Interface_Decl :: struct {
	using node:   Stmt,
	name:         string,
	body:         [dynamic]^Stmt,
	header_range: tokenizer.Range,
	header_text:  string,
	is_bodyless:  bool,
}

Method_Decl :: struct {
	using node:   Stmt,
	name:         string,
	body:         [dynamic]^Stmt,
	header_range: tokenizer.Range,
	header_text:  string,
	is_amdp:      bool,
	amdp_body:    string,
}

Parameter_Passing_Kind :: enum {
	Direct,
	Value,
	Reference,
}

Form_Parameter_Section :: enum {
	Tables,
	Using,
	Changing,
}

Form_Parameter_Clause :: struct {
	section:     Form_Parameter_Section,
	name:        string,
	range:       tokenizer.Range,
	passing:     Parameter_Passing_Kind,
	type_clause: ^Data_Type_Clause,
}

Form_Decl :: struct {
	using node:      Stmt,
	name:            string,
	body:            [dynamic]^Stmt,
	header_range:    tokenizer.Range,
	header_text:     string,
	form_parameters: [dynamic]Form_Parameter_Clause,
}

Function_Parameter_Section :: enum {
	Importing,
	Exporting,
	Changing,
	Tables,
}

Function_Parameter_Flag :: enum {
	Is_Optional,
	Has_Default_Value,
}
Function_Parameter_Flags :: bit_set[Function_Parameter_Flag]

Function_Parameter_Clause :: struct {
	section:     Function_Parameter_Section,
	name:        string,
	range:       tokenizer.Range,
	passing:     Parameter_Passing_Kind,
	type_clause: ^Data_Type_Clause,
	flags:       Function_Parameter_Flags,
}

Function_Exception_Clause :: struct {
	name:  string,
	range: tokenizer.Range,
}

Function_Decl :: struct {
	using node:          Stmt,
	name:                string,
	body:                [dynamic]^Stmt,
	header_range:        tokenizer.Range,
	header_text:         string,
	function_parameters: [dynamic]Function_Parameter_Clause,
	exceptions:          [dynamic]Function_Exception_Clause,
}

Module_Decl :: struct {
	using node:   Stmt,
	name:         string,
	body:         [dynamic]^Stmt,
	header_range: tokenizer.Range,
	header_text:  string,
}

Event_Block_Stmt :: struct {
	using node:   Stmt,
	kind:         string,
	body:         [dynamic]^Stmt,
	header_range: tokenizer.Range,
	header_text:  string,
}

Enhancement_Stmt :: struct {
	using node:   Stmt,
	name:         string,
	body:         [dynamic]^Stmt,
	header_range: tokenizer.Range,
	header_text:  string,
}

Enhancement_Section_Stmt :: struct {
	using node:   Stmt,
	name:         string,
	body:         [dynamic]^Stmt,
	header_range: tokenizer.Range,
	header_text:  string,
}

Test_Seam_Stmt :: struct {
	using node:   Stmt,
	name:         string,
	body:         [dynamic]^Stmt,
	header_range: tokenizer.Range,
	header_text:  string,
}

Test_Injection_Stmt :: struct {
	using node:   Stmt,
	name:         string,
	body:         [dynamic]^Stmt,
	header_range: tokenizer.Range,
	header_text:  string,
}

// ABAP syntax: one SELECT projection, optionally with `AS alias`.
Select_Projection_Clause :: struct {
	value: ^Expr,
	alias: string,
	range: tokenizer.Range,
}

Select_Join_Kind :: enum {
	Inner,
	Left_Outer,
	Right_Outer,
	Full_Outer,
	Cross,
}

// ABAP syntax: one joined Open SQL data source and optional `ON` condition.
Select_Join_Clause :: struct {
	kind:   Select_Join_Kind,
	source: ^Expr,
	alias:  string,
	on:     ^Expr,
}

// ABAP syntax: Open SQL `FROM` source with optional alias and joins.
Select_Source_Clause :: struct {
	range:   tokenizer.Range,
	source:  ^Expr,
	alias:   string,
	dynamic_source: bool,
	joins:   [dynamic]Select_Join_Clause,
}

Select_Result_Kind :: enum {
	None,
	Into,
	Appending,
}

// ABAP syntax: SELECT result target, for example `INTO TABLE lt_rows` or `APPENDING CORRESPONDING FIELDS OF TABLE lt_rows`.
Select_Result_Clause :: struct {
	range:                tokenizer.Range,
	kind:                 Select_Result_Kind,
	target:               ^Expr,
	table:                bool,
	corresponding_fields: bool,
}

// ABAP syntax: SELECT query header from `SELECT ...` through the statement period.
Select_Query_Clause :: struct {
	single:          bool,
	is_distinct:     bool,
	projections:     [dynamic]^Expr,
	projection_clauses: [dynamic]Select_Projection_Clause,
	source:          ^Expr,
	source_clause:   ^Select_Source_Clause,
	result:          ^Select_Result_Clause,
	where_cond:      ^Expr,
	dynamic_where:   bool,
	for_all_entries: ^Expr,
	package_size:    ^Expr,
	up_to_rows:      ^Expr,
	set_ops:         [dynamic]Select_Set_Clause,
	projection_clause:      tokenizer.Range,
	from_clause:            tokenizer.Range,
	into_clause:            tokenizer.Range,
	where_clause:           tokenizer.Range,
	group_by_clause:        tokenizer.Range,
	having_clause:          tokenizer.Range,
	order_by_clause:        tokenizer.Range,
	order_by_primary_key:   bool,
	order_by_fields:        [dynamic]string,
	for_all_entries_clause: tokenizer.Range,
	for_update_clause:      tokenizer.Range,
	up_to_clause:           tokenizer.Range,
	package_size_clause:    tokenizer.Range,
	offset_clause:          tokenizer.Range,
	abap_options_clause:    tokenizer.Range,
	set_operator_clause:    tokenizer.Range,
}

Select_Set_Kind :: enum {
	Union,
	Intersect,
	Except,
}

// ABAP syntax: `UNION`, `INTERSECT`, or `EXCEPT` followed by another SELECT query.
Select_Set_Clause :: struct {
	kind:  Select_Set_Kind,
	all:   bool,
	query: Select_Query_Clause,
}

// ABAP syntax: one `WITH name AS ( SELECT ... )` common table expression.
Select_Cte_Clause :: struct {
	name:  string,
	query: Select_Query_Clause,
}

// ABAP syntax: WITH common table expression prefix before the main SELECT query.
Select_With_Clause :: struct {
	range:      tokenizer.Range,
	query_count: int,
	entries:    [dynamic]Select_Cte_Clause,
}

// ABAP syntax: `SELECT ... .` optionally followed by a loop body and `ENDSELECT.`.
Select_Stmt :: struct {
	using node: Stmt,
	with:       ^Select_With_Clause,
	query:      Select_Query_Clause,
	body:       [dynamic]^Stmt,
}

// ABAP syntax: `OPEN CURSOR [WITH HOLD] handle FOR SELECT ... .`
Open_Cursor_Stmt :: struct {
	using node: Stmt,
	with_hold:  bool,
	handle:     ^Expr,
	query:      Select_Query_Clause,
}

// ABAP syntax: `FETCH NEXT CURSOR handle INTO|APPENDING ... [PACKAGE SIZE n].`
Fetch_Stmt :: struct {
	using node:  Stmt,
	handle:      ^Expr,
	result:      ^Select_Result_Clause,
	package_size: ^Expr,
}

// ABAP syntax: `CLOSE CURSOR handle.`
Close_Cursor_Stmt :: struct {
	using node: Stmt,
	handle:     ^Expr,
}

Read_Table_Key_Kind :: enum {
	None,
	Key,
	Table_Key,
}

// ABAP syntax: one `name = value` component inside a READ TABLE key clause.
Read_Table_Key_Value_Clause :: struct {
	name:  string,
	value: ^Expr,
}

// ABAP syntax: one READ TABLE entry, including chained entries after `READ TABLE:`.
Read_Table_Entry_Clause :: struct {
	table:                  ^Expr,
	into:                   ^Expr,
	assigning:              ^Expr,
	reference_into:         ^Expr,
	key_kind:               Read_Table_Key_Kind,
	key_name:               string,
	key_values:             [dynamic]Read_Table_Key_Value_Clause,
	index:                  ^Expr,
	using_key:              ^Expr,
	transporting_no_fields: bool,
	binary_search:          bool,
	binary_search_clause:   tokenizer.Range,
	comparing:              [dynamic]^Expr,
}

// ABAP syntax: `READ TABLE itab ... .`
Read_Table_Stmt :: struct {
	using node: Stmt,
	entries:    [dynamic]Read_Table_Entry_Clause,
}

Insert_Form :: enum {
	Unknown,
	Internal_Table,
	Db_Table,
	Lines_Of,
}

// ABAP syntax: SQL-style `name = value` assignment used by INSERT/UPDATE `SET`.
Sql_Assignment_Clause :: struct {
	name:         ^Expr,
	value:        ^Expr,
	column_name:  string,
	column_range: tokenizer.Range,
}

// ABAP syntax: `INSERT ...` for internal tables or database tables.
Insert_Stmt :: struct {
	using node:              Stmt,
	form:                    Insert_Form,
	source:                  ^Expr,
	target:                  ^Expr,
	index:                   ^Expr,
	assigning:               ^Expr,
	reference_into:          ^Expr,
	assignments:             [dynamic]Sql_Assignment_Clause,
	db_table_name:           string,
	db_table_name_range:     tokenizer.Range,
	db_source_range:         tokenizer.Range,
	has_db_table_name:       bool,
	dynamic_source:          bool,
	initial_line:            bool,
	into_db_table:           bool,
	from_table:              bool,
	values_clause:           bool,
	from_clause:             tokenizer.Range,
	set_clause:              tokenizer.Range,
	accepting_duplicate_keys: bool,
	accepting_clause:        tokenizer.Range,
	client_clause:           tokenizer.Range,
	connection_clause:       tokenizer.Range,
}

// ABAP syntax: `APPEND wa TO itab` or `APPEND LINES OF src TO dst`.
Append_Stmt :: struct {
	using node:     Stmt,
	source:         ^Expr,
	target:         ^Expr,
	assigning:      ^Expr,
	reference_into: ^Expr,
	initial_line:   bool,
	lines_of:       bool,
	sorted:         bool,
}

// ABAP syntax: `MODIFY itab FROM wa ...` or database `MODIFY dbtab FROM wa`.
Modify_Stmt :: struct {
	using node:  Stmt,
	target:      ^Expr,
	source:      ^Expr,
	index:       ^Expr,
	where_cond:  ^Expr,
	where_clause: tokenizer.Range,
	transporting: [dynamic]^Expr,
	from_table:  bool,
	table_keyword: bool,
	dynamic_source: bool,
	dynamic_where: bool,
	db_source_range: tokenizer.Range,
	client_clause: tokenizer.Range,
	connection_clause: tokenizer.Range,
}

// ABAP syntax: `SORT itab [BY fields ...]`.
Sort_Stmt :: struct {
	using node: Stmt,
	target:     ^Expr,
	fields:     [dynamic]^Expr,
	stable:     bool,
	as_text:    bool,
	descending: bool,
}

// ABAP syntax: `UPDATE dbtab FROM wa` or `UPDATE dbtab SET col = value WHERE cond`.
Update_Stmt :: struct {
	using node:  Stmt,
	target:      ^Expr,
	source:      ^Expr,
	from_table:  bool,
	assignments: [dynamic]Sql_Assignment_Clause,
	where_cond:  ^Expr,
	set_clause: tokenizer.Range,
	where_clause: tokenizer.Range,
	dynamic_where: bool,
	dynamic_source: bool,
	db_source_range: tokenizer.Range,
	client_clause: tokenizer.Range,
	connection_clause: tokenizer.Range,
}

Delete_Form :: enum {
	Internal_Table,
	Db_Table,
	Adjacent_Duplicates,
}

// ABAP syntax: `DELETE itab ...`, `DELETE FROM dbtab ...`, or `DELETE ADJACENT DUPLICATES FROM itab`.
Delete_Stmt :: struct {
	using node: Stmt,
	form:       Delete_Form,
	target:     ^Expr,
	source:     ^Expr,
	index:      ^Expr,
	where_cond: ^Expr,
	where_clause: tokenizer.Range,
	using_key:  ^Expr,
	comparing:  [dynamic]^Expr,
	from_table: bool,
	explicit_from: bool,
	dynamic_source: bool,
	dynamic_where: bool,
	db_source_range: tokenizer.Range,
	client_clause: tokenizer.Range,
	connection_clause: tokenizer.Range,
}

Dataset_Kind :: enum {
	Open,
	Read,
	Transfer,
	Close,
	Delete,
	Get,
	Set,
	Truncate,
}

Dataset_Open_Access :: enum {
	Default,
	Input,
	Output,
	Append,
	Update,
}

// ABAP syntax: `OPEN DATASET`, `READ DATASET`, `TRANSFER`, `GET/SET/TRUNCATE DATASET`, and close/delete forms.
Dataset_Stmt :: struct {
	using node:         Stmt,
	kind:               Dataset_Kind,
	dataset:            ^Expr,
	source:             ^Expr,
	target:             ^Expr,
	access:             Dataset_Open_Access,
	text_mode:          bool,
	binary_mode:        bool,
	encoding:           string,
	position:           ^Expr,
	message:            ^Expr,
	maximum_length:     ^Expr,
	actual_length:      ^Expr,
	length:             ^Expr,
	attributes:         ^Expr,
	at_current_position: bool,
}

Report_Kind :: enum {
	Report,
	Program,
	Read_Report,
	Insert_Report,
	Delete_Report,
}

// ABAP syntax: `REPORT`, `PROGRAM`, `READ REPORT`, `INSERT REPORT`, or `DELETE REPORT`.
Report_Stmt :: struct {
	using node:      Stmt,
	kind:            Report_Kind,
	name:            ^Expr,
	source:          ^Expr,
	line_size:       ^Expr,
	line_count:      ^Expr,
	has_message_id:  bool,
	message_id:      string,
	message_id_range: tokenizer.Range,
}

Textpool_Kind :: enum {
	Read,
	Insert,
	Delete,
}

// ABAP syntax: `READ|INSERT|DELETE TEXTPOOL prog ... [LANGUAGE lang].`
Textpool_Stmt :: struct {
	using node: Stmt,
	kind:       Textpool_Kind,
	program:    ^Expr,
	table:      ^Expr,
	language:   ^Expr,
}

// ABAP syntax: native SQL island `EXEC SQL. ... ENDEXEC.`
Exec_Sql_Stmt :: struct {
	using node:  Stmt,
	body:        string,
	header_range: tokenizer.Range,
}

Generate_Kind :: enum {
	Subroutine_Pool,
	Dynpro,
}

// ABAP syntax: generated-source statements such as `GENERATE SUBROUTINE POOL ...` or `GENERATE DYNPRO ...`.
Generate_Stmt :: struct {
	using node: Stmt,
	kind:       Generate_Kind,
	source:     ^Expr,
	name:       ^Expr,
	program:    ^Expr,
	dynpro:     ^Expr,
	message:    ^Expr,
	line:       ^Expr,
	word:       ^Expr,
	offset:     ^Expr,
}

// ABAP syntax: no valid source form; recovered malformed statement text.
Invalid_Stmt :: struct {
	using node: Stmt,
}

Any_Node :: union {
	^File,
	^Bad_Expr,
	^Char_String_Template_Expr,
	^Template_Literal_Expr,
	^Template_Interpolation_Expr,
	^Template_Expr,
	^Template_Format_Spec_Expr,
	^Binary_Expr,
	^Unary_Expr,
	^Paren_Expr,
	^Ident_Expr,
	^Literal_Expr,
	^Type_Ref_Expr,
	^Host_Expr,
	^Table_Expr,
	^Selector_Expr,
	^Substring_Expr,
	^Call_Expr,
	^Call_Arg_List_Expr,
	^Call_Arg_Section_Expr,
	^Call_Named_Arg_Expr,
	^Call_Positional_Arg_Expr,
	^Constructor_Expr,
	^Is_Predicate_Expr,
	^Instance_Of_Predicate_Expr,
	^Between_Expr,
	^Sql_Case_When_Expr,
	^Sql_Case_Expr,
	^Let_Expr,
	^Constructor_Let_Binding_Expr,
	^Constructor_When_Clause_Expr,
	^Constructor_Else_Clause_Expr,
	^Constructor_For_Clause_Expr,
	^Constructor_Where_Clause_Expr,
	^Constructor_Init_Clause_Expr,
	^Constructor_Next_Clause_Expr,
	^Constructor_Named_Assignment_Expr,
	^Constructor_Base_Clause_Expr,
	^Constructor_Lines_Of_Clause_Expr,
	^Constructor_Optional_Expr,
	^Constructor_Corresponding_Mapping_Clause_Expr,
	^Constructor_Corresponding_Mapping_Assignment_Expr,
	^Constructor_Corresponding_Except_Clause_Expr,
	^Data_Inline_Name_Expr,
	^Field_Symbol_Inline_Name_Expr,
	^Data_Decl,
	^Data_Chained_Decl,
	^Data_Inline_Decl,
	^Types_Decl,
	^Constants_Decl,
	^Field_Symbols_Decl,
	^Statics_Decl,
	^Tables_Decl,
	^Ranges_Decl,
	^Parameters_Decl,
	^Select_Options_Decl,
	^Controls_Decl,
	^Class_Data_Decl,
	^Type_Pools_Decl,
	^Function_Pool_Decl,
	^Include_Stmt,
	^Assign_Stmt,
	^Downcast_Assign_Stmt,
	^Expr_Stmt,
	^Clear_Stmt,
	^Refresh_Stmt,
	^Free_Stmt,
	^Unassign_Stmt,
	^Move_Stmt,
	^Add_Stmt,
	^Subtract_Stmt,
	^Multiply_Stmt,
	^Divide_Stmt,
	^Compute_Stmt,
	^Concatenate_Stmt,
	^Split_Stmt,
	^Condense_Stmt,
	^Replace_Stmt,
	^Translate_Stmt,
	^Shift_Stmt,
	^Find_Stmt,
	^Search_Stmt,
	^Perform_Stmt,
	^Call_Stmt,
	^Submit_Stmt,
	^Message_Stmt,
	^Write_Stmt,
	^Assert_Stmt,
	^Check_Stmt,
	^Flow_Stmt,
	^Transaction_Stmt,
	^Describe_Stmt,
	^Runtime_Stmt,
	^Raise_Stmt,
	^Authority_Check_Stmt,
	^Field_Groups_Stmt,
	^Insert_Dummy_Stmt,
	^Field_Stmt,
	^Assign_Field_Stmt,
	^Create_Object_Stmt,
	^Text_Transform_Stmt,
	^List_Control_Stmt,
	^Line_Stmt,
	^Macro_Def_Stmt,
	^Macro_Call_Stmt,
	^Selection_Screen_Stmt,
	^Oop_Simple_Stmt,
	^If_Stmt,
	^Case_Stmt,
	^While_Stmt,
	^Do_Stmt,
	^Loop_Stmt,
	^At_Stmt,
	^Try_Stmt,
	^Class_Decl,
	^Interface_Decl,
	^Method_Decl,
	^Form_Decl,
	^Function_Decl,
	^Module_Decl,
	^Event_Block_Stmt,
	^Enhancement_Stmt,
	^Enhancement_Section_Stmt,
	^Test_Seam_Stmt,
	^Test_Injection_Stmt,
	^Select_Stmt,
	^Open_Cursor_Stmt,
	^Fetch_Stmt,
	^Close_Cursor_Stmt,
	^Insert_Stmt,
	^Append_Stmt,
	^Modify_Stmt,
	^Sort_Stmt,
	^Update_Stmt,
	^Delete_Stmt,
	^Read_Table_Stmt,
	^Dataset_Stmt,
	^Report_Stmt,
	^Textpool_Stmt,
	^Exec_Sql_Stmt,
	^Generate_Stmt,
	^Invalid_Stmt,
}

Any_Expr :: union {
	^Bad_Expr,
	^Char_String_Template_Expr,
	^Template_Literal_Expr,
	^Template_Interpolation_Expr,
	^Template_Expr,
	^Template_Format_Spec_Expr,
	^Binary_Expr,
	^Unary_Expr,
	^Paren_Expr,
	^Ident_Expr,
	^Literal_Expr,
	^Type_Ref_Expr,
	^Host_Expr,
	^Table_Expr,
	^Selector_Expr,
	^Substring_Expr,
	^Call_Expr,
	^Call_Arg_List_Expr,
	^Call_Arg_Section_Expr,
	^Call_Named_Arg_Expr,
	^Call_Positional_Arg_Expr,
	^Constructor_Expr,
	^Is_Predicate_Expr,
	^Instance_Of_Predicate_Expr,
	^Between_Expr,
	^Sql_Case_When_Expr,
	^Sql_Case_Expr,
	^Let_Expr,
	^Constructor_Let_Binding_Expr,
	^Constructor_When_Clause_Expr,
	^Constructor_Else_Clause_Expr,
	^Constructor_For_Clause_Expr,
	^Constructor_Where_Clause_Expr,
	^Constructor_Init_Clause_Expr,
	^Constructor_Next_Clause_Expr,
	^Constructor_Named_Assignment_Expr,
	^Constructor_Base_Clause_Expr,
	^Constructor_Lines_Of_Clause_Expr,
	^Constructor_Optional_Expr,
	^Constructor_Corresponding_Mapping_Clause_Expr,
	^Constructor_Corresponding_Mapping_Assignment_Expr,
	^Constructor_Corresponding_Except_Clause_Expr,
	^Data_Inline_Name_Expr,
	^Field_Symbol_Inline_Name_Expr,
}

Any_Stmt :: union {
	^Invalid_Stmt,
	^Assign_Stmt,
	^Downcast_Assign_Stmt,
	^Expr_Stmt,
	^Data_Decl,
	^Data_Chained_Decl,
	^Data_Inline_Decl,
	^Types_Decl,
	^Constants_Decl,
	^Field_Symbols_Decl,
	^Statics_Decl,
	^Tables_Decl,
	^Ranges_Decl,
	^Parameters_Decl,
	^Select_Options_Decl,
	^Controls_Decl,
	^Class_Data_Decl,
	^Type_Pools_Decl,
	^Function_Pool_Decl,
	^Include_Stmt,
	^Clear_Stmt,
	^Refresh_Stmt,
	^Free_Stmt,
	^Unassign_Stmt,
	^Move_Stmt,
	^Add_Stmt,
	^Subtract_Stmt,
	^Multiply_Stmt,
	^Divide_Stmt,
	^Compute_Stmt,
	^Concatenate_Stmt,
	^Split_Stmt,
	^Condense_Stmt,
	^Replace_Stmt,
	^Translate_Stmt,
	^Shift_Stmt,
	^Find_Stmt,
	^Search_Stmt,
	^Perform_Stmt,
	^Call_Stmt,
	^Submit_Stmt,
	^Message_Stmt,
	^Write_Stmt,
	^Assert_Stmt,
	^Check_Stmt,
	^Flow_Stmt,
	^Transaction_Stmt,
	^Describe_Stmt,
	^Runtime_Stmt,
	^Raise_Stmt,
	^Authority_Check_Stmt,
	^Field_Groups_Stmt,
	^Insert_Dummy_Stmt,
	^Field_Stmt,
	^Assign_Field_Stmt,
	^Create_Object_Stmt,
	^Text_Transform_Stmt,
	^List_Control_Stmt,
	^Line_Stmt,
	^Macro_Def_Stmt,
	^Macro_Call_Stmt,
	^Selection_Screen_Stmt,
	^Oop_Simple_Stmt,
	^If_Stmt,
	^Case_Stmt,
	^While_Stmt,
	^Do_Stmt,
	^Loop_Stmt,
	^At_Stmt,
	^Try_Stmt,
	^Class_Decl,
	^Interface_Decl,
	^Method_Decl,
	^Form_Decl,
	^Function_Decl,
	^Module_Decl,
	^Event_Block_Stmt,
	^Enhancement_Stmt,
	^Enhancement_Section_Stmt,
	^Test_Seam_Stmt,
	^Test_Injection_Stmt,
	^Select_Stmt,
	^Open_Cursor_Stmt,
	^Fetch_Stmt,
	^Close_Cursor_Stmt,
	^Insert_Stmt,
	^Append_Stmt,
	^Modify_Stmt,
	^Sort_Stmt,
	^Update_Stmt,
	^Delete_Stmt,
	^Read_Table_Stmt,
	^Dataset_Stmt,
	^Report_Stmt,
	^Textpool_Stmt,
	^Exec_Sql_Stmt,
	^Generate_Stmt,
}
