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
	// Set when this identifier is the name from inline DATA(name) parsed as an expression.
	inline_data_decl: ^Data_Inline_Decl,
	// True when parsed as ASSIGNING FIELD-SYMBOL(<fs>) / READ TABLE ... ASSIGNING FIELD-SYMBOL(<fs>), etc.
	is_inline_field_symbol_decl: bool,
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
	// Usually ^Ident; dynamic names use ^Paren_Expr, e.g. class=>(lv_attr), (lv_cls)=>(lv_attr).
	field:      ^Expr,
}

Index_Expr :: struct {
	using node: Expr,
	expr:       ^Expr,
	index:      ^Expr,
	// Table expression with explicit table key: itab[ KEY [key_name] COMPONENTS comp = val ... ]
	table_key_name: ^Ident, // Named secondary key; nil when KEY COMPONENTS ... (primary key) or no KEY clause
	has_key_clause: bool, // True when leading KEY keyword was parsed as table-key access (not as identifier)
}

// ABAP substring expression
// Syntax:
// - dobj(len)
// - dobj+off(len)
// - dobj+off(*)
Substring_Expr :: struct {
	using node:     Expr,
	expr:           ^Expr,
	offset:         ^Expr,
	length:         ^Expr,
	length_is_star: bool,
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
	Alpha, // ALPHA = IN/OUT
	Date, // DATE = ISO/USER/RAW/ENVIRONMENT
	Time, // TIME = ISO/USER/RAW/ENVIRONMENT
	Width, // WIDTH = n
	Align, // ALIGN = LEFT/RIGHT/CENTER
	Pad, // PAD = 'char'
	Case, // CASE = UPPER/LOWER/RAW
	Sign, // SIGN = LEFT/LEFTPLUS/LEFTSPACE/RIGHT/RIGHTPLUS/RIGHTSPACE
	Decimals, // DECIMALS = n
	Exponent, // EXPONENT = n
	Zero, // ZERO = YES/NO
	Number, // NUMBER = USER/RAW/ENVIRONMENT
	Style, // STYLE = SIMPLE/SIGN_AS_POSTFIX/SCALE_PRESERVING
	Currency, // CURRENCY = currency_code
	Country, // COUNTRY = country_code
	Timestamp, // TIMESTAMP = SPACE/ISO/USER/ENVIRONMENT
	Timezone, // TIMEZONE = tz
}

// Embedded expression formatting option value
Embedded_Format_Value :: enum {
	In, // ALPHA = IN
	Out, // ALPHA = OUT
	Iso, // DATE/TIME/TIMESTAMP = ISO
	User, // DATE/TIME/NUMBER/TIMESTAMP = USER
	Raw, // DATE/TIME/NUMBER/CASE = RAW
	Environment, // DATE/TIME/NUMBER/TIMESTAMP = ENVIRONMENT
	Left, // ALIGN/SIGN = LEFT
	Right, // ALIGN/SIGN = RIGHT
	Center, // ALIGN = CENTER
	Upper, // CASE = UPPER
	Lower, // CASE = LOWER
	Yes, // ZERO = YES
	No, // ZERO = NO
	Simple, // STYLE = SIMPLE
	Scale_Preserving, // STYLE = SCALE_PRESERVING
	Sign_As_Postfix, // STYLE = SIGN_AS_POSTFIX
	Leftplus, // SIGN = LEFTPLUS
	Leftspace, // SIGN = LEFTSPACE
	Rightplus, // SIGN = RIGHTPLUS
	Rightspace, // SIGN = RIGHTSPACE
	Space, // TIMESTAMP = SPACE
	Custom, // For numeric values (WIDTH, DECIMALS) or string values (PAD, CURRENCY)
}

// Embedded expression formatting option
Embedded_Format_Option :: struct {
	kind:      Embedded_Format_Kind,
	value:     Embedded_Format_Value,
	num_value: int, // For WIDTH, DECIMALS, EXPONENT
	str_value: string, // For PAD, CURRENCY, COUNTRY, TIMEZONE
	range:     lexer.TextRange,
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
	using node:  Expr,
	var_name:    ^Ident, // Loop variable name
	itab:        ^Expr, // Internal table to iterate over
	where_cond:  ^Expr, // Optional WHERE condition
	result_expr: ^Expr, // Result expression (what to produce for each iteration) - deprecated, use result_args
	result_args: [dynamic]^Expr, // Result arguments (named args like field1 = val1, or single expression)
}

// Value row expression - represents a parenthesized group of arguments in VALUE constructor
// Used for table rows like: VALUE #( ( field1 = val1 field2 = val2 ) ( field1 = val3 ) )
Value_Row_Expr :: struct {
	using node: Expr,
	args:       [dynamic]^Expr, // Row arguments (named args or single expressions)
}

// Statements

Bad_Stmt :: struct {
	using node: Stmt,
}

// Empty statement: a single period (.), allowed in classic ABAP source.
Empty_Stmt :: struct {
	using node: Stmt,
}

Expr_Stmt :: struct {
	using node: Stmt,
	expr:       ^Expr,
}

// Classic ABAP macro invocation: macro_name [actual_param ...].
// (Parameters are expanded textually; not a procedure call.)
Macro_Call_Stmt :: struct {
	using node: Stmt,
	name:       ^Expr,
	args:       []^Expr,
}

Assign_Stmt :: struct {
	using node: Stmt,
	lhs:        []^Expr,
	op:         lexer.Token,
	rhs:        []^Expr,
}

// MOVE-CORRESPONDING source TO target [KEEPING TARGET LINES].
Move_Corresponding_Stmt :: struct {
	using node: Stmt,
	source:               ^Expr,
	target:               ^Expr,
	keeping_target_lines: bool,
}

Assign_Field_Symbol_Stmt :: struct {
	using node:      Stmt,
	component:       ^Expr,
	structure:       ^Expr,
	source:          ^Expr,
	offset:          ^Expr,
	length:          ^Expr,
	length_is_star:  bool,
	is_component:    bool,
	is_dynamic:      bool,
	is_table_field:  bool,
	target:          ^Expr,
}

Block_Stmt :: struct {
	using node: Stmt,
	label:      ^Expr,
	stmts:      []^Stmt,
}

Try_Catch_Branch :: struct {
	using node:      Node,
	before_unwind:   bool,
	class_refs:      [dynamic]^Expr,
	into_target:     ^Expr,
	body:            [dynamic]^Stmt,
}

Try_Cleanup_Branch :: struct {
	using node:  Node,
	into_target: ^Expr,
	body:        [dynamic]^Stmt,
}

Try_Stmt :: struct {
	using node:       Stmt,
	body:             [dynamic]^Stmt,
	catch_branches:   [dynamic]^Try_Catch_Branch,
	cleanup_branch:   ^Try_Cleanup_Branch,
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
	type_ref:   ^Expr, // for IS INSTANCE OF - the type/class reference
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

// MODIFY dbtab FROM wa.
// MODIFY dbtab FROM TABLE itab.
Modify_From_Stmt :: struct {
	using node: Stmt,
	target: ^Expr, // database table
	source: ^Expr, // work area or internal table (after optional TABLE)
}

Leave_Program_Stmt :: struct {
	using node: Stmt,
}

// COMMIT WORK [AND WAIT].
Commit_Work_Stmt :: struct {
	using node: Stmt,
	and_wait: bool,
}

// ROLLBACK WORK.
Rollback_Work_Stmt :: struct {
	using node: Stmt,
}

Get_Time_Stamp_Stmt :: struct {
	using node: Stmt,
	target:     ^Expr,
}

// CONVERT DATE dat TIME tim INTO TIME STAMP tstamp TIME ZONE tz.
Convert_Date_Time_To_Time_Stamp_Stmt :: struct {
	using node: Stmt,
	date:       ^Expr,
	time:       ^Expr,
	stamp:      ^Expr,
	time_zone:  ^Expr,
}

// CONVERT TIME STAMP stamp [TIME ZONE tz] INTO DATE date TIME time.
Convert_Time_Stamp_To_Date_Time_Stmt :: struct {
	using node: Stmt,
	stamp:      ^Expr,
	time_zone:  ^Expr,
	date:       ^Expr,
	time:       ^Expr,
}

// GET BADI badi_ref [FILTERS name = value ...].
Get_Badi_Stmt :: struct {
	using node: Stmt,
	badi_ref:   ^Expr,
	filters:    [dynamic]^Named_Arg,
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

// SET HANDLER handler_ref ... FOR event_ref.
Set_Handler_Stmt :: struct {
	using node: Stmt,
	handlers:   [dynamic]^Expr,
	for_ref:    ^Expr,
}

// SET BIT position OF byte_string TO bit_value.
Set_Bit_Stmt :: struct {
	using node:    Stmt,
	bit_position:  ^Expr,
	of_target:     ^Expr,
	to_value:      ^Expr,
}

// GET BIT position OF byte_string INTO target.
Get_Bit_Stmt :: struct {
	using node:    Stmt,
	bit_position:  ^Expr,
	of_target:     ^Expr,
	into_target:   ^Expr,
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

// DO [. | n TIMES.] ... ENDDO.
Do_Stmt :: struct {
	using node: Stmt,
	times:      ^Expr, // nil for unconditional DO.
	body:       [dynamic]^Stmt,
}

// CONTINUE. — next loop iteration
Continue_Stmt :: struct {
	using node: Stmt,
}

// EXIT. — leave current loop (or program context)
Exit_Stmt :: struct {
	using node: Stmt,
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
// - LOOP AT itab [INTO wa | ASSIGNING <fs> | TRANSPORTING NO FIELDS] [USING KEY key_name]
//   [WHERE condition].
// - LOOP AT itab ... GROUP BY key_spec [ASSIGNING <fs>].
// - LOOP AT GROUP group_var [INTO wa | ASSIGNING <fs>] [WHERE condition].
// - LOOP AT SCREEN.
Loop_Stmt :: struct {
	using node:             Stmt,
	kind:                   Loop_Kind,
	itab:                   ^Expr, // The internal table expression
	into_target:            ^Expr, // INTO target (work area or inline DATA)
	assigning_target:       ^Expr, // ASSIGNING <fs> target (field symbol)
	using_key:              ^Ident, // USING KEY key_name (optional)
	from_expr:              ^Expr, // FROM index expression
	to_expr:                ^Expr, // TO index expression
	where_cond:             ^Expr, // WHERE condition expression
	transporting_no_fields: bool, // TRANSPORTING NO FIELDS flag
	group_by:               ^Loop_Group_By, // GROUP BY specification
	group_var:              ^Expr, // For LOOP AT GROUP: the group variable
	body:                   [dynamic]^Stmt,
}

// AT ... ENDAT blocks inside LOOP AT (control levels / grouping)
Loop_At_Control_Kind :: enum {
	First, // AT FIRST. ... ENDAT.
	Last, // AT LAST. ... ENDAT.
	New, // AT NEW f. ... ENDAT.
	End_Of, // AT END OF f. ... ENDAT.
}

Loop_At_Control_Stmt :: struct {
	using node: Stmt,
	kind:       Loop_At_Control_Kind,
	field:      ^Expr, // set for .New and .End_Of (grouping field)
	body:       [dynamic]^Stmt,
}

Clear_Stmt :: struct {
	using node: Stmt,
	exprs:      [dynamic]^Expr,
	with_expr:  ^Expr, // CLEAR dobj WITH dobj2 (reference targets)
}

// FREE dobj. or FREE: dobj1, dobj2, ... (chained; releases internal table / string memory)
Free_Stmt :: struct {
	using node: Stmt,
	exprs:      [dynamic]^Expr,
}

// REFRESH itab. or REFRESH: itab1, itab2, ... — reset internal table body / header state
Refresh_Stmt :: struct {
	using node: Stmt,
	exprs:      [dynamic]^Expr,
}

// UNASSIGN <fs>. or UNASSIGN: <fs1>, <fs2>.
Unassign_Stmt :: struct {
	using node: Stmt,
	targets:    [dynamic]^Expr, // field symbol references
}

// WRITE operand (one target of WRITE / WRITE: ... , ... ).
Write_Operand :: struct {
	range:           lexer.TextRange,
	line_feed:       bool, // leading /
	format_len:      ^Expr, // optional (len) before data (outputs as /(len) in list form)
	data:            ^Expr,
	to_target:       ^Expr,
	decimals:        ^Expr,
	time_zone:       ^Expr,
	left_justified:  bool,
	right_justified: bool,
	no_grouping:     bool,
	no_sign:         bool,
}

// WRITE statement — list output, optional TO for string templates, formatting clauses.
Write_Stmt :: struct {
	using node: Stmt,
	operands:   [dynamic]Write_Operand,
}

// MESSAGE statement
// Syntax:
// - MESSAGE { msg | text } [TYPE type] [DISPLAY LIKE display_type] [WITH v1 [v2 [v3 [v4]]]] [INTO data]
// - MESSAGE ID class TYPE type NUMBER num [WITH v1 [v2 [v3 [v4]]]] [INTO data] [DISPLAY LIKE ...]
Message_Stmt :: struct {
	using node:   Stmt,
	msg_expr:     ^Expr, // Static / text form: string, variable, or e899(class). Nil when ID form.
	id_class:     ^Expr, // ID class (MESSAGE ID class ...). Nil except for ID form.
	msg_type:     ^Expr, // TYPE 'I' etc (optional)
	msg_number:   ^Expr, // NUMBER 898 (optional; typical with ID form)
	display_like: ^Expr, // DISPLAY LIKE 'E' (optional)
	with_args:    [dynamic]^Expr, // WITH v1 v2 v3 v4 (up to 4 args)
	into_target:  ^Expr, // INTO data (optional)
}

// INSERT statement kinds
Insert_Kind :: enum {
	Into_Table, // INSERT expr INTO TABLE itab
	Into_Itab, // INSERT expr INTO itab [INDEX idx]
	Initial_Line_Into_Itab, // INSERT INITIAL LINE INTO [TABLE] itab [INDEX idx] [ASSIGNING <fs>]
	Into_Db, // INSERT INTO target VALUES wa
	From_Wa, // INSERT target FROM wa
	From_Table, // INSERT target FROM TABLE itab
	Lines_Of_Into_Table, // INSERT LINES OF itab_src INTO TABLE itab_tgt
	Lines_Of_Into_Itab, // INSERT LINES OF itab_src INTO itab_tgt [INDEX idx]
}

// INSERT statement
// Syntax variations:
// - INSERT VALUE #( ... ) INTO TABLE itab.
// - INSERT wa INTO itab [INDEX idx].
// - INSERT INITIAL LINE INTO [TABLE] itab [INDEX idx] [ASSIGNING <fs>].
// - INSERT LINES OF itab_src INTO TABLE itab_tgt.
// - INSERT LINES OF itab_src INTO itab_tgt [INDEX idx].
// - INSERT INTO target VALUES wa.
// - INSERT target FROM wa.
// - INSERT target FROM TABLE itab [ACCEPTING DUPLICATE KEYS].
Insert_Stmt :: struct {
	using node:   Stmt,
	kind:         Insert_Kind,
	value_expr:   ^Expr, // The value/expression to insert (for Into_Table, Into_Itab, Into_Db)
	target:       ^Expr, // The target table (internal or database table)
	source:       ^Expr, // Source work area or table (From_Wa, From_Table, Lines_Of_*); Into_Db VALUES expr
	index_expr:   ^Expr, // INDEX clause (Into_Itab, Lines_Of_Into_Itab, Initial_Line_Into_Itab); nil if omitted
	assigning_target: ^Expr, // ASSIGNING clause (Initial_Line_Into_Itab); nil if omitted
	accepting_duplicate_keys: bool, // INSERT ... FROM [TABLE] ... ACCEPTING DUPLICATE KEYS
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
	stable:     bool, // SORT ... STABLE
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
	key_name:   ^Ident, // WITH KEY / WITH TABLE KEY key_name COMPONENTS ... (optional; free key uses components only)
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
	binary_search:          bool, // BINARY SEARCH (optional, for sorted tables)
}

Read_Report_Stmt :: struct {
	using node: Stmt,
	prog:       ^Expr,
	itab:       ^Ident,
}

// DESCRIBE TABLE statement
// Syntax:
// - DESCRIBE TABLE itab LINES lines.
Describe_Table_Stmt :: struct {
	using node:    Stmt,
	table:         ^Expr,
	lines_target:  ^Expr,
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
// - APPEND LINES OF itab2 FROM idx_from TO idx_to TO itab_tgt. (line range, then target)
Append_Stmt :: struct {
	using node:       Stmt,
	kind:             Append_Kind,
	source:           ^Expr, // The value/expression to append (for Simple, Lines_Of)
	lines_from:       ^Expr, // APPEND LINES OF ... FROM (start line index), optional
	lines_to:         ^Expr, // ... TO ... (end line index) before final TO target, optional
	target:           ^Expr, // The target internal table
	assigning_target: ^Expr, // Field symbol for ASSIGNING clause (optional)
}

// DELETE statement kinds
Delete_Kind :: enum {
	Where, // DELETE itab WHERE ...
	Index, // DELETE itab INDEX idx
	Adjacent_Duplicates, // DELETE ADJACENT DUPLICATES FROM itab ...
	Table_From, // DELETE TABLE itab FROM wa.
	Db_From_Table, // DELETE dbtab FROM TABLE itab. (database rows from internal table)
}

// DELETE statement
// Syntax variations:
// - DELETE itab WHERE ...
// - DELETE itab INDEX idx.
// - DELETE ADJACENT DUPLICATES FROM itab ...
// - DELETE TABLE itab FROM wa.
// - DELETE dbtab FROM TABLE itab.  (e.g. namespace table /NS/obj)
// - DELETE FROM dbtab WHERE ... (Open SQL style before table name)
Delete_Stmt :: struct {
	using node:   Stmt,
	kind:         Delete_Kind,
	target:       ^Expr, // Internal table (Where, Index, Adjacent, Table_From) or dbtab (Db_From_Table)
	where_cond:   ^Expr, // WHERE condition
	index_expr:   ^Expr, // INDEX expression
	from_source:  ^Expr // DELETE TABLE itab FROM wa (work area); DELETE dbtab FROM TABLE itab (internal table)
}

Split_Mode :: enum {
	None,
	Character,
	Byte,
}

// SPLIT statement
// Syntax:
// - SPLIT dobj AT sep INTO result1 result2 ...
// - SPLIT dobj AT sep INTO TABLE result_tab
// - ... [IN CHARACTER MODE | IN BYTE MODE]
Split_Stmt :: struct {
	using node:    Stmt,
	source:        ^Expr,
	separator:     ^Expr,
	targets:       [dynamic]^Expr,
	table_target:  ^Expr,
	mode:          Split_Mode,
}

// CONCATENATE statement
// Syntax:
// - CONCATENATE dobj1 dobj2 ... INTO result
// - CONCATENATE dobj1 dobj2 ... INTO result SEPARATED BY sep
// - ... INTO result [RESPECTING BLANKS] [SEPARATED BY sep] (clause order flexible)
Concatenate_Stmt :: struct {
	using node:        Stmt,
	sources:           [dynamic]^Expr,
	target:            ^Expr,
	separator:         ^Expr,
	respecting_blanks: bool,
}

// CONDENSE dobj [NO-GAPS].
Condense_Stmt :: struct {
	using node: Stmt,
	text:       ^Expr,
	no_gaps:    bool,
}

// TRANSLATE statement (classic character/string transforms)
// - TRANSLATE dobj TO UPPER | LOWER CASE.
// - TRANSLATE dobj USING pattern.
Translate_Kind :: enum {
	Upper_Case,
	Lower_Case,
	Using,
}

Translate_Stmt :: struct {
	using node:       Stmt,
	target:           ^Expr,
	kind:             Translate_Kind,
	using_pattern:    ^Expr, // USING variant only
}

// SHIFT statement (classic character string shifting)
// - SHIFT dobj LEFT | RIGHT [DELETING LEADING | TRAILING mask].
// - SHIFT dobj BY num PLACES [LEFT | RIGHT] [CIRCULAR].
Shift_Direction :: enum {
	None,
	Left,
	Right,
}

Shift_Deleting_Kind :: enum {
	None,
	Leading,
	Trailing,
}

Shift_Stmt :: struct {
	using node:      Stmt,
	target:          ^Expr,
	by_places:       ^Expr, // BY n PLACES …; nil if that form is not used
	direction:       Shift_Direction, // simple LEFT/RIGHT or direction after PLACES
	circular:        bool, // only with BY … PLACES
	deleting:        Shift_Deleting_Kind,
	deleting_mask:   ^Expr,
}

// REPLACE statement (classic, IN/INTO, ALL/FIRST OCCURRENCES, optional REGEX)
Replace_Scope :: enum {
	Simple,
	All_Occurrences,
	First_Occurrence,
}

Replace_Stmt :: struct {
	using node:     Stmt,
	scope:          Replace_Scope,
	is_regex:       bool,
	pattern:        ^Expr,
	subject:        ^Expr, // IN subject … WITH … or … INTO subject
	replacement:    ^Expr,
	into_form:      bool, // Simple scope: WITH repl INTO subject (else IN subject WITH repl)
}

// RAISE / RAISE EXCEPTION statement
// Syntax:
// - RAISE exc. (non-class-based exception)
// - RAISE [RESUMABLE] EXCEPTION TYPE cx_class [EXPORTING p1 = a1 ...].
// - RAISE [RESUMABLE] EXCEPTION oref.
Raise_Exception_Stmt :: struct {
	using node:       Stmt,
	is_resumable:     bool,
	type_ref:         ^Expr, // Exception class after TYPE
	oref:             ^Expr, // Exception object reference variant
	legacy_exception: ^Expr, // Non-class-based exception name (RAISE exc.)
	exporting:        [dynamic]^Named_Arg,
}

// CHECK statement
// Syntax: CHECK logical_expression.
// Used to check a condition and exit the current processing block if false
Check_Stmt :: struct {
	using node: Stmt,
	cond:       ^Expr, // The logical expression to check
}

// ASSERT statement
// Syntax: ASSERT logical_expression.
// Used to assert that a condition holds at runtime
Assert_Stmt :: struct {
	using node: Stmt,
	cond:       ^Expr, // The logical expression to assert
}

// CALL FUNCTION / CALL BADI parameter section kinds (shared param node shape)
Call_Function_Param_Kind :: enum {
	Exporting,
	Importing,
	Tables,
	Changing,
	Receiving,
	Exceptions,
}

// CALL FUNCTION parameter (name = value pairs)
Call_Function_Param :: struct {
	using node: Node,
	kind:       Call_Function_Param_Kind,
	name:       ^Ident, // Parameter name
	value:      ^Expr, // Parameter value
	// EXCEPTIONS exc = n MESSAGE msg — optional message variable for RFC / classic exceptions
	message_value: ^Expr,
	// EXCEPTIONS OTHERS = n — ABAP catch-all; runtime sets sy-subrc from n when no named exception matches
	is_others:  bool,
}

// CALL FUNCTION statement
// Syntax: CALL FUNCTION 'func_name' [DESTINATION dest]
//         [STARTING NEW TASK task_id]
//         [IN BACKGROUND TASK | IN UPDATE TASK] [DESTINATION dest]
//         [EXPORTING param = value ...]
//         [IMPORTING param = value ...]
//         [TABLES param = value ...]
//         [CHANGING param = value ...]
//         [EXCEPTIONS name = value ...].
Call_Function_Stmt :: struct {
	using node:        Stmt,
	func_name:         ^Expr, // Function name (typically a string literal)
	destination:       ^Expr, // Optional DESTINATION expression
	starting_new_task: ^Expr, // Optional asynchronous RFC task id (STARTING NEW TASK)
	exporting:         [dynamic]^Call_Function_Param,
	importing:         [dynamic]^Call_Function_Param,
	tables:            [dynamic]^Call_Function_Param,
	changing:          [dynamic]^Call_Function_Param,
	exceptions:        [dynamic]^Call_Function_Param,
}

// CALL BADI statement
// Syntax: CALL BADI badi_ref->method
//         [EXPORTING ...] [IMPORTING ...] [CHANGING ...] [RECEIVING ...] [EXCEPTIONS ...].
Call_Badi_Stmt :: struct {
	using node:  Stmt,
	badi_target: ^Expr, // e.g. lo_badi->preprocess
	exporting:   [dynamic]^Call_Function_Param,
	importing:   [dynamic]^Call_Function_Param,
	changing:    [dynamic]^Call_Function_Param,
	receiving:   [dynamic]^Call_Function_Param,
	exceptions:  [dynamic]^Call_Function_Param,
}

// CALL system/kernel module (CALL 'C_DIR_READ_START' ID 'DIR' FIELD lv_path ...).
Call_System_Param :: struct {
	using node: Node,
	id_name:    ^Expr, // Parameter id, usually a text field literal
	field:      ^Expr, // Bound data object after FIELD
}

Call_System_Stmt :: struct {
	using node: Stmt,
	module:     ^Expr, // Module name, usually a text field literal
	params:     [dynamic]^Call_System_Param,
}

// CREATE OBJECT statement
// Syntax: CREATE OBJECT oref [TYPE class] [AREA HANDLE area] [EXPORTING p1 = a1 ...] [EXCEPTIONS exc = rc ...].
Create_Object_Stmt :: struct {
	using node:   Stmt,
	target:       ^Expr,
	type_ref:     ^Expr,
	area_handle:  ^Expr,
	exporting:    [dynamic]^Named_Arg,
	exceptions:   [dynamic]^Named_Arg,
}

// CREATE DATA statement — allocates a data object addressed by a data reference variable.
// Syntax (partial):
// - CREATE DATA dref TYPE type.
// - CREATE DATA dref LIKE dobj.
// - CREATE DATA dref TYPE HANDLE handle. (runtime type descriptor)
Create_Data_Stmt :: struct {
	using node:    Stmt,
	target:        ^Expr,
	type_ref:      ^Expr, // TYPE <type_expr> (after TYPE, when not TYPE HANDLE)
	like_ref:      ^Expr, // LIKE <data object>
	type_handle:   ^Expr, // TYPE HANDLE <handle expression>
}

// SELECT statement join kind
Select_Join_Kind :: enum {
	Inner, // INNER JOIN
	Left_Outer, // LEFT OUTER JOIN
	Right_Outer, // RIGHT OUTER JOIN
}

// SELECT statement join specification
Select_Join :: struct {
	using node: Node,
	kind:       Select_Join_Kind,
	table:      ^Expr, // Table name
	alias:      ^Ident, // AS alias (optional)
	on_cond:    ^Expr, // ON condition
}

// SELECT statement target kind
Select_Into_Kind :: enum {
	Single, // INTO @wa or INTO @DATA(wa)
	Table, // INTO TABLE @itab or INTO TABLE @DATA(itab)
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
	using node:      Stmt,
	is_single:       bool, // SINGLE modifier
	fields:          [dynamic]^Expr, // Field list (* or specific fields)
	from_table:      ^Expr, // FROM table expression
	from_alias:      ^Ident, // AS alias for FROM table (optional)
	joins:           [dynamic]^Select_Join, // JOIN clauses
	into_kind:       Select_Into_Kind, // INTO target kind
	into_target:     ^Expr, // INTO target (work area or inline DATA)
	where_cond:      ^Expr, // WHERE condition
	order_by:        [dynamic]^Expr, // ORDER BY columns
	group_by:        [dynamic]^Expr, // GROUP BY columns
	having_cond:     ^Expr, // HAVING condition
	for_all_entries: ^Expr, // FOR ALL ENTRIES IN itab
	up_to_rows:      ^Expr, // UP TO n ROWS
	appending:       bool, // APPENDING CORRESPONDING FIELDS ... (SELECT append into table)
	// True when INTO was CORRESPONDING FIELDS OF TABLE (...); false for OF wa (SELECT loop header).
	into_corresponding_of_table: bool,
	body:            [dynamic]^Stmt, // Body for SELECT loop (non-single)
}

// OPEN CURSOR ... FOR SELECT ... — database cursor over an Open SQL select (no INTO on inner SELECT).
Open_Cursor_Stmt :: struct {
	using node:    Stmt,
	cursor:        ^Ident,
	select_stmt:   ^Stmt, // ^Select_Stmt
}

// FETCH NEXT CURSOR ... INTO ... [PACKAGE SIZE n]. — reads next rows from an opened database cursor.
Fetch_Cursor_Stmt :: struct {
	using node:                  Stmt,
	cursor:                      ^Ident,
	into_kind:                   Select_Into_Kind,
	// True when INTO was CORRESPONDING FIELDS OF TABLE itab; false for OF wa.
	into_corresponding_of_table: bool,
	into_target:                 ^Expr,
	package_size:                ^Expr, // optional (nil if absent)
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
	ident:      ^Expr, // Can be ^Ident or ^Selector_Expr (e.g., screen0100-serial)
	// Legacy length in parentheses before TYPE/LIKE (e.g. DATA: lv_x(10) TYPE c)
	length:     ^Expr,
	typed:      ^Expr,
	value:      ^Expr,
	// True for STATICS name TYPE ... — procedure-persistent storage (not CLASS-DATA)
	is_static:  bool,
}

Data_Typed_Chain_Decl :: struct {
	using node: Decl,
	// Ordered chain members: each stmt is ^Data_Typed_Decl or ^Data_Struct_Decl
	parts:      [dynamic]^Stmt,
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
	// Ordered chain members: each stmt is ^Types_Decl or ^Types_Struct_Decl
	parts:      [dynamic]^Stmt,
}

// CONSTANTS declarations

Const_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	// Legacy length in parentheses before TYPE, e.g. CONSTANTS lcv(14) TYPE p ...
	length:     ^Expr,
	typed:      ^Expr,
	value:      ^Expr,
}

Const_Chain_Decl :: struct {
	using node: Decl,
	// Ordered chain members: each stmt is ^Const_Decl or ^Const_Struct_Decl
	parts:      [dynamic]^Stmt,
}

Const_Struct_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	components: [dynamic]^Stmt,
}

Types_Struct_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	// Members: ^Types_Decl, ^Types_Struct_Decl, ^Types_Include_Type_Decl
	components: [dynamic]^Stmt,
}

// INCLUDE TYPE type [ AS name ] in a TYPES ... BEGIN OF ... block
Types_Include_Type_Decl :: struct {
	using node: Decl,
	included:   ^Expr,
	as_name:    ^Ident, // Optional; nil if AS clause omitted
}

// DATA structure declaration (DATA: BEGIN OF name, ... END OF name.)
Data_Struct_Decl :: struct {
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
	likes:      ^Expr,
	optional:   bool,
	default:    ^Expr,
}

Method_Flag :: enum {
	Class,
	Abstract,
	Final,
	Redefinition,
	Testing,
}

Method_Flags :: bit_set[Method_Flag]

Method_Decl :: struct {
	using node: Decl,
	ident:      ^Ident,
	flags:      Method_Flags,
	params:     [dynamic]^Method_Param,
	raising:    [dynamic]^Expr,
}

Method_Chain_Decl :: struct {
	using node: Decl,
	decls:      [dynamic]^Method_Decl,
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

Class_Create_Kind :: enum {
	Public,
	Protected,
	Private,
}

Class_Def_Flag :: enum {
	Abstract,
	Final,
	Testing,
	Shared_Memory,
}
Class_Def_Flags :: bit_set[Class_Def_Flag]

Class_Def_Risk_Level :: enum {
	Critical,
	Dangerous,
	Harmless,
}

Class_Def_Duration :: enum {
	Short,
	Medium,
	Long,
}

Class_Def_Decl :: struct {
	using node:      Decl,
	ident:           ^Ident,
	visibility:      Access_Modifier,
	flags:           Class_Def_Flags,
	create_kind:     Class_Create_Kind,
	inheriting_from: ^Expr,
	behavior_of:     ^Expr,
	friends:         [dynamic]^Expr,
	global_friends:  bool,
	sections:        [dynamic]^Class_Section,
	risk_level:      Class_Def_Risk_Level,
	duration:        Class_Def_Duration,
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

// FIELD-SYMBOLS chain declaration
// Syntax: FIELD-SYMBOLS: <fs1> TYPE type1, <fs2> TYPE type2.
Field_Symbol_Chain_Decl :: struct {
	using node: Decl,
	decls:      [dynamic]^Field_Symbol_Decl,
}

// CONTROLS declaration control types
Control_Kind :: enum {
	Tableview, // TYPE TABLEVIEW USING SCREEN dynnr
	Tabstrip, // TYPE TABSTRIP
}

// CONTROLS declaration
// Syntax: CONTROLS contrl TYPE TABLEVIEW USING SCREEN dynnr.
// Syntax: CONTROLS contrl TYPE TABSTRIP.
Controls_Decl :: struct {
	using node:   Decl,
	ident:        ^Ident, // Control name
	kind:         Control_Kind, // TABLEVIEW or TABSTRIP
	screen_dynnr: ^Expr, // Screen number for TABLEVIEW (nil for TABSTRIP)
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

Call_Transaction_Authority :: enum {
	Unspecified,
	With,
	Without,
}

// CALL TRANSACTION tcod ... [WITH|WITHOUT AUTHORITY-CHECK] [USING bdc] [MODE m].
Call_Transaction_Stmt :: struct {
	using node:    Stmt,
	transaction:   ^Expr,
	authority:     Call_Transaction_Authority,
	bdc_tab:       ^Expr,
	mode:          ^Expr,
}

// CALL TRANSFORMATION — XSLT / simple transformation
// Syntax: CALL TRANSFORMATION id [OPTIONS opt] [PARAMETERS (...)]
//         SOURCE { XML | ASXML | BINARY } operand
//         [RESULT { XML | ASXML | BINARY } operand | root = dobj ...].
Call_Transformation_Stmt :: struct {
	using node:       Stmt,
	transformation:   ^Expr,
	options:          ^Expr,
	source:           ^Expr,
	result_stream:    ^Expr,
	result_roots:     [dynamic]^Named_Arg,
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

// RANGE OF base_type — selection table / ranges type (SIGN, OPTION, LOW, HIGH)
Range_Type :: struct {
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
	^Substring_Expr,
	^Call_Expr,
	^New_Expr,
	^Constructor_Expr,
	^Named_Arg,
	^Predicate_Expr,
	^String_Template_Expr,
	^For_Expr,
	^Value_Row_Expr,
	// Types
	^Table_Type,
	^Ref_Type,
	^Line_Type,
	^Range_Type,
	// Statements
	^Bad_Stmt,
	^Empty_Stmt,
	^Expr_Stmt,
	^Macro_Call_Stmt,
	^Assign_Stmt,
	^Move_Corresponding_Stmt,
	^Assign_Field_Symbol_Stmt,
	^Block_Stmt,
	^Try_Catch_Branch,
	^Try_Cleanup_Branch,
	^Try_Stmt,
	^If_Stmt,
	^Elseif_Branch,
	^Return_Stmt,
	^Modify_Screen_Stmt,
	^Modify_From_Stmt,
	^Leave_Program_Stmt,
	^Commit_Work_Stmt,
	^Rollback_Work_Stmt,
	^Get_Time_Stamp_Stmt,
	^Convert_Date_Time_To_Time_Stamp_Stmt,
	^Convert_Time_Stamp_To_Date_Time_Stmt,
	^Get_Badi_Stmt,
	^Get_Bit_Stmt,
	^Set_Stmt,
	^Set_Handler_Stmt,
	^Set_Bit_Stmt,
	^Case_Stmt,
	^While_Stmt,
	^Do_Stmt,
	^Continue_Stmt,
	^Exit_Stmt,
	^Loop_Stmt,
	^Loop_At_Control_Stmt,
	^Clear_Stmt,
	^Free_Stmt,
	^Refresh_Stmt,
	^Unassign_Stmt,
	^Write_Stmt,
	^Message_Stmt,
	^Insert_Stmt,
	^Sort_Stmt,
	^Append_Stmt,
	^Read_Table_Stmt,
	^Read_Report_Stmt,
	^Describe_Table_Stmt,
	^Authority_Check_Stmt,
	^Delete_Stmt,
	^Split_Stmt,
	^Concatenate_Stmt,
	^Condense_Stmt,
	^Translate_Stmt,
	^Shift_Stmt,
	^Replace_Stmt,
	^Raise_Exception_Stmt,
	^Check_Stmt,
	^Assert_Stmt,
	^Call_Function_Stmt,
	^Call_Badi_Stmt,
	^Call_System_Stmt,
	^Call_Function_Param,
	^Call_System_Param,
	^Create_Object_Stmt,
	^Create_Data_Stmt,
	^Select_Stmt,
	^Open_Cursor_Stmt,
	^Fetch_Cursor_Stmt,
	^Select_Join,
	// Declarations
	^Bad_Decl,
	^Data_Inline_Decl,
	^Data_Typed_Decl,
	^Data_Typed_Chain_Decl,
	^Data_Struct_Decl,
	^Types_Decl,
	^Types_Chain_Decl,
	^Types_Struct_Decl,
	^Types_Include_Type_Decl,
	^Const_Decl,
	^Const_Chain_Decl,
	^Const_Struct_Decl,
	^Form_Param,
	^Form_Decl,
	// Class/Interface declarations
	^Method_Param,
	^Method_Decl,
	^Method_Chain_Decl,
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
	^Call_Transaction_Stmt,
	^Call_Transformation_Stmt,
	^Module_Decl,
	// Field symbols
	^Field_Symbol_Decl,
	^Field_Symbol_Chain_Decl,
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
	^Substring_Expr,
	^Call_Expr,
	^New_Expr,
	^Constructor_Expr,
	^Named_Arg,
	^Predicate_Expr,
	^String_Template_Expr,
	^For_Expr,
	^Value_Row_Expr,
	// Types
	^Table_Type,
	^Ref_Type,
	^Line_Type,
	^Range_Type,
}

Any_Stmt :: union {
	^Bad_Stmt,
	^Empty_Stmt,
	^Expr_Stmt,
	^Macro_Call_Stmt,
	^Assign_Stmt,
	^Move_Corresponding_Stmt,
	^Assign_Field_Symbol_Stmt,
	^Block_Stmt,
	^Try_Stmt,
	^If_Stmt,
	^Return_Stmt,
	^Modify_Screen_Stmt,
	^Modify_From_Stmt,
	^Leave_Program_Stmt,
	^Commit_Work_Stmt,
	^Rollback_Work_Stmt,
	^Get_Time_Stamp_Stmt,
	^Convert_Date_Time_To_Time_Stamp_Stmt,
	^Convert_Time_Stamp_To_Date_Time_Stmt,
	^Get_Badi_Stmt,
	^Get_Bit_Stmt,
	^Set_Stmt,
	^Set_Handler_Stmt,
	^Set_Bit_Stmt,
	^Case_Stmt,
	^While_Stmt,
	^Do_Stmt,
	^Continue_Stmt,
	^Exit_Stmt,
	^Loop_Stmt,
	^Loop_At_Control_Stmt,
	^Clear_Stmt,
	^Free_Stmt,
	^Refresh_Stmt,
	^Unassign_Stmt,
	^Write_Stmt,
	^Message_Stmt,
	^Insert_Stmt,
	^Sort_Stmt,
	^Append_Stmt,
	^Read_Table_Stmt,
	^Read_Report_Stmt,
	^Describe_Table_Stmt,
	^Authority_Check_Stmt,
	^Delete_Stmt,
	^Split_Stmt,
	^Concatenate_Stmt,
	^Condense_Stmt,
	^Translate_Stmt,
	^Shift_Stmt,
	^Replace_Stmt,
	^Raise_Exception_Stmt,
	^Check_Stmt,
	^Assert_Stmt,
	^Call_Function_Stmt,
	^Call_Badi_Stmt,
	^Call_System_Stmt,
	^Create_Object_Stmt,
	^Create_Data_Stmt,
	^Select_Stmt,
	^Open_Cursor_Stmt,
	^Fetch_Cursor_Stmt,
	// Declarations
	^Bad_Decl,
	^Data_Inline_Decl,
	^Data_Typed_Decl,
	^Data_Typed_Chain_Decl,
	^Data_Struct_Decl,
	^Types_Decl,
	^Types_Chain_Decl,
	^Types_Struct_Decl,
	^Types_Include_Type_Decl,
	^Const_Decl,
	^Const_Chain_Decl,
	^Const_Struct_Decl,
	^Form_Decl,
	// Class/Interface declarations
	^Method_Decl,
	^Method_Chain_Decl,
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
	^Call_Transaction_Stmt,
	^Call_Transformation_Stmt,
	^Module_Decl,
	// Field symbols
	^Field_Symbol_Decl,
	^Field_Symbol_Chain_Decl,
	// Controls
	^Controls_Decl,
	^Controls_Chain_Decl,
}
