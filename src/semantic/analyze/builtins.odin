package abap_frontend_semantic_analyze

import "src:tokenizer"

import "core:mem"
import "core:strings"

Builtin_Type_Kind :: enum {
	Type,
	Constant,
	Variable,
}

Builtin_Field_Spec :: struct {
	name:           string,
	type_name:      string,
	structure_name: string,
	description:    string,
}

Builtin_Structure_Spec :: struct {
	name:   string,
	fields: []Builtin_Field_Spec,
}

Builtin_Symbol_Spec :: struct {
	name:           string,
	kind:           Builtin_Type_Kind,
	structure_name: string,
}

Builtin_Routine_Param_Spec :: struct {
	name:      string,
	type_name: string,
}

Builtin_Routine_Spec :: struct {
	name:                     string,
	params:                   []Builtin_Routine_Param_Spec,
	return_type:              string,
	description:              string,
	supports_named_arguments: bool,
}

Builtin_Class_Attribute_Spec :: struct {
	class_name: string,
	name:       string,
	type_name:  string,
}

BUILTIN_SCALAR_TYPES :: []string {
	"i",
	"int1",
	"int2",
	"int4",
	"int8",
	"f",
	"p",
	"decfloat16",
	"decfloat34",
	"string",
	"c",
	"n",
	"d",
	"t",
	"x",
	"xstring",
	"xsequence",
	"data",
	"any",
	"clike",
	"csequence",
	"object",
}

BUILTIN_STRUCTURES :: []Builtin_Structure_Spec {
	{
		name = "syst",
		fields = []Builtin_Field_Spec {
			{
				name = "abcde",
				type_name = "c",
				structure_name = "",
				description = "Latin alphabet helper text that can be indexed directly by offset and length.",
			},
			{
				name = "batch",
				type_name = "c",
				structure_name = "",
				description = "Set to 'X' in background processing and initial in dialog processing.",
			},
			{
				name = "binpt",
				type_name = "c",
				structure_name = "",
				description = "Set to 'X' while batch input is being processed.",
			},
			{
				name = "cprog",
				type_name = "c",
				structure_name = "",
				description = "Calling program for external procedures, otherwise the current program.",
			},
			{
				name = "datum",
				type_name = "d",
				structure_name = "",
				description = "Current system date.",
			},
			{
				name = "dbcnt",
				type_name = "i",
				structure_name = "",
				description = "Number of database rows processed by the last SQL statement that documents it.",
			},
			{
				name = "fdpos",
				type_name = "i",
				structure_name = "",
				description = "Found offset after supported search and comparison operations such as FIND.",
			},
			{
				name = "index",
				type_name = "i",
				structure_name = "",
				description = "Loop counter inside DO and WHILE loops; nested loops use the innermost counter.",
			},
			{
				name = "langu",
				type_name = "c",
				structure_name = "",
				description = "Single-character locale language key for the current internal session.",
			},
			{
				name = "msgid",
				type_name = "c",
				structure_name = "",
				description = "Message class captured by the last MESSAGE statement.",
			},
			{
				name = "msgno",
				type_name = "n",
				structure_name = "",
				description = "Message number captured by the last MESSAGE statement.",
			},
			{
				name = "msgty",
				type_name = "c",
				structure_name = "",
				description = "Message type captured by the last MESSAGE statement.",
			},
			{
				name = "msgv1",
				type_name = "c",
				structure_name = "",
				description = "First MESSAGE placeholder value captured by the last MESSAGE statement.",
			},
			{
				name = "msgv2",
				type_name = "c",
				structure_name = "",
				description = "Second MESSAGE placeholder value captured by the last MESSAGE statement.",
			},
			{
				name = "msgv3",
				type_name = "c",
				structure_name = "",
				description = "Third MESSAGE placeholder value captured by the last MESSAGE statement.",
			},
			{
				name = "msgv4",
				type_name = "c",
				structure_name = "",
				description = "Fourth MESSAGE placeholder value captured by the last MESSAGE statement.",
			},
			{
				name = "repid",
				type_name = "c",
				structure_name = "",
				description = "Program name exposed through sy-repid and syst-repid; SAP documents this as a predefined constant and type, not a real SYST component.",
			},
			{
				name = "subrc",
				type_name = "i",
				structure_name = "",
				description = "Return code set by many ABAP statements; 0 usually indicates success for the documented statement.",
			},
			{
				name = "tabix",
				type_name = "i",
				structure_name = "",
				description = "Current internal-table index from READ TABLE or LOOP AT on indexed access paths.",
			},
			{
				name = "tfill",
				type_name = "i",
				structure_name = "",
				description = "Row count of the internal table accessed by DESCRIBE TABLE, LOOP AT, or READ TABLE.",
			},
			{
				name = "timlo",
				type_name = "t",
				structure_name = "",
				description = "Current user time in the user's time zone.",
			},
			{
				name = "tzone",
				type_name = "i",
				structure_name = "",
				description = "System time-zone offset from UTC in seconds.",
			},
			{
				name = "zonlo",
				type_name = "c",
				structure_name = "",
				description = "Current user's time zone.",
			},
			{
				name = "ucomm",
				type_name = "c",
				structure_name = "",
				description = "Function code that triggered the current PAI processing.",
			},
			{
				name = "uname",
				type_name = "c",
				structure_name = "",
				description = "User name of the current session.",
			},
			{
				name = "uzeit",
				type_name = "t",
				structure_name = "",
				description = "Current system time.",
			},
		},
	},
	{
		name = "screen",
		fields = []Builtin_Field_Spec {
			{
				name = "name",
				type_name = "c",
				structure_name = "",
				description = "Name of the current dynpro field or screen element.",
			},
			{
				name = "group1",
				type_name = "c",
				structure_name = "",
				description = "Modification group 1 of the current screen element.",
			},
			{
				name = "group2",
				type_name = "c",
				structure_name = "",
				description = "Modification group 2 of the current screen element.",
			},
			{
				name = "group3",
				type_name = "c",
				structure_name = "",
				description = "Modification group 3 of the current screen element.",
			},
			{
				name = "group4",
				type_name = "c",
				structure_name = "",
				description = "Modification group 4 of the current screen element.",
			},
			{
				name = "required",
				type_name = "c",
				structure_name = "",
				description = "Whether the field is mandatory on the current dynpro.",
			},
			{
				name = "input",
				type_name = "c",
				structure_name = "",
				description = "Whether the field is ready for input on the current dynpro.",
			},
			{
				name = "output",
				type_name = "c",
				structure_name = "",
				description = "Whether the field is output-only on the current dynpro.",
			},
			{
				name = "intensified",
				type_name = "c",
				structure_name = "",
				description = "Whether the field is highlighted on the current dynpro.",
			},
			{
				name = "invisible",
				type_name = "c",
				structure_name = "",
				description = "Whether the field is hidden on the current dynpro.",
			},
			{
				name = "length",
				type_name = "x",
				structure_name = "",
				description = "Visible field length of the current dynpro element.",
			},
			{
				name = "active",
				type_name = "c",
				structure_name = "",
				description = "Combined active flag for the current dynpro element.",
			},
			{
				name = "display_3d",
				type_name = "c",
				structure_name = "",
				description = "Whether the current dynpro box is shown three-dimensionally.",
			},
			{
				name = "value_help",
				type_name = "c",
				structure_name = "",
				description = "Whether input help is shown for the current dynpro field.",
			},
			{
				name = "request",
				type_name = "c",
				structure_name = "",
				description = "Whether input exists, or is simulated, for the current dynpro field.",
			},
			{
				name = "values_in_combo",
				type_name = "c",
				structure_name = "",
				description = "Whether values exist in the current dynpro dropdown list box.",
			},
		},
	},
	{
		name = "match_result",
		fields = []Builtin_Field_Spec {
			{
				name = "offset",
				type_name = "i",
				structure_name = "",
				description = "Zero-based offset of the match in the searched data object.",
			},
			{
				name = "length",
				type_name = "i",
				structure_name = "",
				description = "Length of the matched segment.",
			},
			{
				name = "submatches",
				type_name = "match_result_tab",
				structure_name = "match_result",
				description = "Nested table containing captured submatches for a regex result.",
			},
			{
				name = "line",
				type_name = "i",
				structure_name = "",
				description = "Line number of the match for searches in internal tables.",
			},
		},
	},
	{
		name = "cntl_simple_event",
		fields = []Builtin_Field_Spec {
			{
				name = "eventid",
				type_name = "i",
				structure_name = "",
				description = "Control Framework event identifier.",
			},
			{
				name = "appl_event",
				type_name = "abap_bool",
				structure_name = "",
				description = "Whether the event is raised as an application event.",
			},
		},
	},
}

BUILTIN_SYMBOLS :: []Builtin_Symbol_Spec {
	{name = "abap_bool", kind = .Type, structure_name = ""},
	{name = "flag", kind = .Type, structure_name = ""},
	{name = "xfeld", kind = .Type, structure_name = ""},
	{name = "sy", kind = .Type, structure_name = "syst"},
	{name = "syst", kind = .Type, structure_name = "syst"},
	{name = "screen", kind = .Type, structure_name = "screen"},
	{name = "syst", kind = .Variable, structure_name = "syst"},
	{name = "sy", kind = .Variable, structure_name = "syst"},
	{name = "screen", kind = .Variable, structure_name = "screen"},
	{name = "guid", kind = .Type, structure_name = ""},
	{name = "symsgv", kind = .Type, structure_name = ""},
	{name = "sydatum", kind = .Type, structure_name = ""},
	{name = "timestamp", kind = .Type, structure_name = ""},
	{name = "cursor", kind = .Type, structure_name = ""},
	{name = "match_result", kind = .Type, structure_name = "match_result"},
	{name = "match_result_tab", kind = .Type, structure_name = "match_result"},
	{name = "syst_short", kind = .Type, structure_name = ""},
	{name = "syst_byte", kind = .Type, structure_name = ""},
	{name = "syst_long", kind = .Type, structure_name = ""},
	{name = "tabname", kind = .Type, structure_name = ""},
	{name = "cdobjectcl", kind = .Type, structure_name = ""},
	{name = "rs38l_fnam", kind = .Type, structure_name = ""},
	{name = "memoryid", kind = .Type, structure_name = ""},
	{name = "synt_errors", kind = .Type, structure_name = ""},
	{name = "synt_comment", kind = .Type, structure_name = ""},
	{name = "synt_map", kind = .Type, structure_name = ""},
	{name = "synt_it_trmsg_raw", kind = .Type, structure_name = ""},
	{name = "synt_includes", kind = .Type, structure_name = ""},
	{name = "synt_ext_check", kind = .Type, structure_name = ""},
	{name = "synt_interval", kind = .Type, structure_name = ""},
	{name = "synt_crossref", kind = .Type, structure_name = ""},
	{name = "synt_type_obj", kind = .Type, structure_name = ""},
	{name = "synt_type_childs", kind = .Type, structure_name = ""},
	{name = "synt_data_obj", kind = .Type, structure_name = ""},
	{name = "synt_dpar", kind = .Type, structure_name = ""},
	{name = "synt_env", kind = .Type, structure_name = ""},
	{name = "synt_comp_obj", kind = .Type, structure_name = ""},
	{name = "synt_xcross", kind = .Type, structure_name = ""},
	{name = "synt_xcross_level", kind = .Type, structure_name = ""},
	{name = "synt_xcross_stmnt", kind = .Type, structure_name = ""},
	{name = "synt_ext_obj_use", kind = .Type, structure_name = ""},
	{name = "synum01", kind = .Type, structure_name = ""},
	{name = "sychar68k", kind = .Type, structure_name = ""},
	{name = "abap_classname", kind = .Type, structure_name = ""},
	{name = "abap_compname", kind = .Type, structure_name = ""},
	{name = "abap_typename", kind = .Type, structure_name = ""},
	{name = "abap_keyname", kind = .Type, structure_name = ""},
	{name = "abap_keycompname", kind = .Type, structure_name = ""},
	{name = "abap_intfname", kind = .Type, structure_name = ""},
	{name = "abap_attrname", kind = .Type, structure_name = ""},
	{name = "abap_evntname", kind = .Type, structure_name = ""},
	{name = "abap_parmname", kind = .Type, structure_name = ""},
	{name = "abap_excpname", kind = .Type, structure_name = ""},
	{name = "abap_component_tab", kind = .Type, structure_name = ""},
	{name = "abap_func_parmbind_tab", kind = .Type, structure_name = ""},
	{name = "abap_func_excpbind_tab", kind = .Type, structure_name = ""},
	{name = "abap_func_parmbind", kind = .Type, structure_name = ""},
	{name = "abap_func_excpbind", kind = .Type, structure_name = ""},
	{name = "abap_trans_srcbind_tab", kind = .Type, structure_name = ""},
	{name = "abap_trans_resbind_tab", kind = .Type, structure_name = ""},
	{name = "abap_componentdescr", kind = .Type, structure_name = ""},
	{name = "abap_simple_componentdescr", kind = .Type, structure_name = ""},
	{name = "abap_abstypename", kind = .Type, structure_name = ""},
	{name = "abap_compdescr", kind = .Type, structure_name = ""},
	{name = "abap_keydescr", kind = .Type, structure_name = ""},
	{name = "abap_table_keydescr_tab", kind = .Type, structure_name = ""},
	{name = "abap_table_keycompdescr", kind = .Type, structure_name = ""},
	{name = "abap_table_keydescr", kind = .Type, structure_name = ""},
	{name = "abap_intfdescr_tab", kind = .Type, structure_name = ""},
	{name = "abap_typecategory", kind = .Type, structure_name = ""},
	{name = "abap_typekind", kind = .Type, structure_name = ""},
	{name = "abap_typepropkind", kind = .Type, structure_name = ""},
	{name = "abap_component_symbol_tab", kind = .Type, structure_name = ""},
	{name = "abap_component_view_tab", kind = .Type, structure_name = ""},
	{name = "abap_structkind", kind = .Type, structure_name = ""},
	{name = "abap_compdescr_tab", kind = .Type, structure_name = ""},
	{name = "abapsource", kind = .Type, structure_name = ""},
	{name = "abap_encoding", kind = .Type, structure_name = ""},
	{name = "abap_editmask", kind = .Type, structure_name = ""},
	{name = "abap_helpid", kind = .Type, structure_name = ""},
	{name = "abap_classkind", kind = .Type, structure_name = ""},
	{name = "abap_visibility", kind = .Type, structure_name = ""},
	{name = "abap_frndtypes_tab", kind = .Type, structure_name = ""},
	{name = "abap_tablekind", kind = .Type, structure_name = ""},
	{name = "abap_keydefkind", kind = .Type, structure_name = ""},
	{name = "abap_keydescr_tab", kind = .Type, structure_name = ""},
	{name = "abap_methname", kind = .Type, structure_name = ""},
	{name = "abap_methdescr", kind = .Type, structure_name = ""},
	{name = "abap_parmdescr", kind = .Type, structure_name = ""},
	{name = "abap_parmdescr_tab", kind = .Type, structure_name = ""},
	{name = "abap_excpdescr", kind = .Type, structure_name = ""},
	{name = "abap_excpdescr_tab", kind = .Type, structure_name = ""},
	{name = "abap_frnddescr", kind = .Type, structure_name = ""},
	{name = "abap_frnddescr_tab", kind = .Type, structure_name = ""},
	{name = "abap_intfdescr", kind = .Type, structure_name = ""},
	{name = "abap_typedef", kind = .Type, structure_name = ""},
	{name = "abap_attrdescr", kind = .Type, structure_name = ""},
	{name = "abap_evntdescr", kind = .Type, structure_name = ""},
	{name = "abap_endian", kind = .Type, structure_name = ""},
	{name = "abap_parmkind", kind = .Type, structure_name = ""},
	{name = "abap_typedef_tab", kind = .Type, structure_name = ""},
	{name = "abap_attrdescr_tab", kind = .Type, structure_name = ""},
	{name = "abap_methdescr_tab", kind = .Type, structure_name = ""},
	{name = "abap_evntdescr_tab", kind = .Type, structure_name = ""},
	{name = "abap_parmbind", kind = .Type, structure_name = ""},
	{name = "abap_parmbind_tab", kind = .Type, structure_name = ""},
	{name = "abap_excpbind", kind = .Type, structure_name = ""},
	{name = "abap_excpbind_tab", kind = .Type, structure_name = ""},
	{name = "abap_intfkind", kind = .Type, structure_name = ""},
	{name = "abap_char1", kind = .Type, structure_name = ""},
	{name = "abap_cr_lf", kind = .Type, structure_name = ""},
	{name = "abap_byte_order_mark", kind = .Type, structure_name = ""},
	{name = "abap_byte_order_utf8", kind = .Type, structure_name = ""},
	{name = "abap_trans_parmname", kind = .Type, structure_name = ""},
	{name = "abap_trans_parmvalue", kind = .Type, structure_name = ""},
	{name = "abap_trans_parmref", kind = .Type, structure_name = ""},
	{name = "abap_trans_parmbind", kind = .Type, structure_name = ""},
	{name = "abap_trans_parm_obj_bind", kind = .Type, structure_name = ""},
	{name = "abap_trans_parmbind_tab", kind = .Type, structure_name = ""},
	{name = "abap_trans_parm_obj_bind_tab", kind = .Type, structure_name = ""},
	{name = "abap_trans_objname", kind = .Type, structure_name = ""},
	{name = "abap_trans_objbind", kind = .Type, structure_name = ""},
	{name = "abap_trans_objbind_tab", kind = .Type, structure_name = ""},
	{name = "abap_trans_srcname", kind = .Type, structure_name = ""},
	{name = "abap_trans_srcbind", kind = .Type, structure_name = ""},
	{name = "abap_trans_srcbind_tab_sorted", kind = .Type, structure_name = ""},
	{name = "abap_trans_resname", kind = .Type, structure_name = ""},
	{name = "abap_trans_resbind", kind = .Type, structure_name = ""},
	{name = "abap_trans_resbind_tab_sorted", kind = .Type, structure_name = ""},
	{name = "abap_true", kind = .Constant, structure_name = ""},
	{name = "abap_false", kind = .Constant, structure_name = ""},
	{name = "abap_undefined", kind = .Constant, structure_name = ""},
	{name = "abap_on", kind = .Constant, structure_name = ""},
	{name = "abap_off", kind = .Constant, structure_name = ""},
	{name = "abap_max_abs_type_name_ln", kind = .Constant, structure_name = ""},
	{name = "abap_max_class_name_ln", kind = .Constant, structure_name = ""},
	{name = "abap_max_intf_name_ln", kind = .Constant, structure_name = ""},
	{name = "abap_max_comp_name_ln", kind = .Constant, structure_name = ""},
	{name = "abap_max_key_name_ln", kind = .Constant, structure_name = ""},
	{name = "abap_max_class_comp_name_ln", kind = .Constant, structure_name = ""},
	{name = "abap_max_edit_mask_ln", kind = .Constant, structure_name = ""},
	{name = "abap_max_help_id_ln", kind = .Constant, structure_name = ""},
	{name = "abap_max_db_string_ln", kind = .Constant, structure_name = ""},
	{name = "abap_max_db_rawstring_ln", kind = .Constant, structure_name = ""},
	{name = "abap_func_exporting", kind = .Constant, structure_name = ""},
	{name = "abap_func_importing", kind = .Constant, structure_name = ""},
	{name = "abap_func_tables", kind = .Constant, structure_name = ""},
	{name = "abap_func_changing", kind = .Constant, structure_name = ""},
	{name = "space", kind = .Constant, structure_name = ""},
	{name = "text", kind = .Variable, structure_name = ""},
	{name = "cntl_simple_event", kind = .Type, structure_name = "cntl_simple_event"},
	{name = "cntl_simple_events", kind = .Type, structure_name = "cntl_simple_event"},
}

BUILTIN_CLASS_ATTRIBUTES :: []Builtin_Class_Attribute_Spec {
	{class_name = "cl_abap_char_utilities", name = "charsize", type_name = "i"},
	{class_name = "cl_abap_char_utilities", name = "cr_lf", type_name = "abap_cr_lf"},
	{class_name = "cl_abap_char_utilities", name = "endian", type_name = "abap_endian"},
	{class_name = "cl_abap_char_utilities", name = "form_feed", type_name = "abap_char1"},
	{class_name = "cl_abap_char_utilities", name = "horizontal_tab", type_name = "abap_char1"},
	{class_name = "cl_abap_char_utilities", name = "minchar", type_name = "abap_char1"},
	{class_name = "cl_abap_char_utilities", name = "newline", type_name = "abap_char1"},
}

NUMERIC_ARG_PARAMS :: []Builtin_Routine_Param_Spec{{"arg", "data"}}
FLOAT_ARG_PARAMS :: []Builtin_Routine_Param_Spec{{"arg", "f"}}
IPOW_PARAMS :: []Builtin_Routine_Param_Spec{{"base", "data"}, {"exp", "i"}}
EXTREMUM_PARAMS :: []Builtin_Routine_Param_Spec {
	{"val1", "data"},
	{"val2", "data"},
	{"val3", "data"},
	{"val4", "data"},
	{"val5", "data"},
	{"val6", "data"},
	{"val7", "data"},
	{"val8", "data"},
	{"val9", "data"},
}
DEC_FLOAT_ROUNDING_PARAMS :: []Builtin_Routine_Param_Spec {
	{"val", "decfloat34"},
	{"dec", "i"},
	{"prec", "i"},
	{"mode", "data"},
}

BUILTIN_ROUTINES :: []Builtin_Routine_Spec {
	{
		name = "boolc",
		params = []Builtin_Routine_Param_Spec{{"log_exp", "abap_bool"}},
		return_type = "string",
		description = "Returns 'X' as a string when the logical expression is true, otherwise a blank string.",
		supports_named_arguments = false,
	},
	{
		name = "line_exists",
		params = []Builtin_Routine_Param_Spec{{"table_line", "data"}},
		return_type = "abap_bool",
		description = "Predicate function: returns whether a row exists for the given internal table expression.",
		supports_named_arguments = false,
	},
	{name = "abs", params = NUMERIC_ARG_PARAMS, return_type = "data", description = "Absolute value of `arg`.", supports_named_arguments = false},
	{name = "sign", params = NUMERIC_ARG_PARAMS, return_type = "data", description = "Sign of `arg`: -1, 0, or 1.", supports_named_arguments = false},
	{name = "ceil", params = NUMERIC_ARG_PARAMS, return_type = "data", description = "Smallest integer not less than `arg`.", supports_named_arguments = false},
	{name = "floor", params = NUMERIC_ARG_PARAMS, return_type = "data", description = "Largest integer not greater than `arg`.", supports_named_arguments = false},
	{name = "trunc", params = NUMERIC_ARG_PARAMS, return_type = "data", description = "Integer part of `arg`.", supports_named_arguments = false},
	{name = "frac", params = NUMERIC_ARG_PARAMS, return_type = "data", description = "Decimal part of `arg`.", supports_named_arguments = false},
	{name = "ipow", params = IPOW_PARAMS, return_type = "data", description = "Integer power: `base` raised to `exp`.", supports_named_arguments = true},
	{name = "nmax", params = EXTREMUM_PARAMS, return_type = "data", description = "Largest numeric argument.", supports_named_arguments = true},
	{name = "nmin", params = EXTREMUM_PARAMS, return_type = "data", description = "Smallest numeric argument.", supports_named_arguments = true},
	{name = "acos", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Arccosine of `arg`.", supports_named_arguments = false},
	{name = "asin", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Arcsine of `arg`.", supports_named_arguments = false},
	{name = "atan", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Arctangent of `arg`.", supports_named_arguments = false},
	{name = "cos", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Cosine of `arg`.", supports_named_arguments = false},
	{name = "sin", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Sine of `arg`.", supports_named_arguments = false},
	{name = "tan", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Tangent of `arg`.", supports_named_arguments = false},
	{name = "cosh", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Hyperbolic cosine of `arg`.", supports_named_arguments = false},
	{name = "sinh", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Hyperbolic sine of `arg`.", supports_named_arguments = false},
	{name = "tanh", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Hyperbolic tangent of `arg`.", supports_named_arguments = false},
	{name = "exp", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Exponential function for base e.", supports_named_arguments = false},
	{name = "log", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Natural logarithm of `arg`.", supports_named_arguments = false},
	{name = "log10", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Logarithm of `arg` to base 10.", supports_named_arguments = false},
	{name = "sqrt", params = FLOAT_ARG_PARAMS, return_type = "f", description = "Square root of `arg`.", supports_named_arguments = false},
	{
		name = "charlen",
		params = []Builtin_Routine_Param_Spec{{"arg", "string"}, {"text", "string"}},
		return_type = "i",
		description = "Length of the first character in the current code page.",
		supports_named_arguments = false,
	},
	{
		name = "dbmaxlen",
		params = []Builtin_Routine_Param_Spec{{"arg", "string"}, {"val", "string"}},
		return_type = "i",
		description = "Maximum ABAP Dictionary length for a string-like value.",
		supports_named_arguments = false,
	},
	{
		name = "numofchar",
		params = []Builtin_Routine_Param_Spec{{"arg", "string"}, {"str", "string"}},
		return_type = "i",
		description = "Number of characters in a text value.",
		supports_named_arguments = false,
	},
	{
		name = "strlen",
		params = []Builtin_Routine_Param_Spec{{"arg", "string"}, {"val", "string"}},
		return_type = "i",
		description = "Number of characters in a text value.",
		supports_named_arguments = false,
	},
	{
		name = "substring",
		params = []Builtin_Routine_Param_Spec{{"val", "string"}, {"off", "i"}, {"len", "i"}},
		return_type = "string",
		description = "Returns a substring of a text-like value; optional `off` selects the start position and optional `len` limits the length (if `len` is omitted, the remainder is returned).",
		supports_named_arguments = true,
	},
	{
		name = "condense",
		params = []Builtin_Routine_Param_Spec {
			{"val", "string"},
			{"del", "string"},
			{"from", "string"},
			{"to", "string"},
		},
		return_type = "string",
		description = "Returns a condensed character string: strips leading/trailing characters in `del`, replaces runs in `from` using `to` (all default to a single blank when omitted).",
		supports_named_arguments = true,
	},
	{
		name = "replace",
		params = []Builtin_Routine_Param_Spec {
			{"val", "string"},
			{"sub", "string"},
			{"regex", "string"},
			{"with", "string"},
			{"occ", "i"},
			{"case", "abap_bool"},
		},
		return_type = "string",
		description = "Returns a character string where occurrences of `sub` or `regex` in `val` are replaced by `with`; `occ` selects one occurrence or all occurrences and `case` controls case-sensitive matching.",
		supports_named_arguments = true,
	},
	{
		name = "escape",
		params = []Builtin_Routine_Param_Spec{{"val", "string"}, {"format", "data"}},
		return_type = "string",
		description = "Returns a character string with special characters escaped for the requested target format.",
		supports_named_arguments = true,
	},
	{
		name = "reverse",
		params = []Builtin_Routine_Param_Spec{{"val", "string"}},
		return_type = "string",
		description = "Returns a character string with its characters in reverse order.",
		supports_named_arguments = true,
	},
	{
		name = "round",
		params = DEC_FLOAT_ROUNDING_PARAMS,
		return_type = "decfloat34",
		description = "Rounds a decimal floating-point value to a given number of decimal places (`dec`) or significant digits (`prec`), optionally using a rounding mode from `CL_ABAP_MATH`.",
		supports_named_arguments = true,
	},
	{
		name = "rescale",
		params = DEC_FLOAT_ROUNDING_PARAMS,
		return_type = "decfloat34",
		description = "Rescales a decimal floating-point value by decimal places (`dec`) or precision (`prec`), optionally using a rounding mode from `CL_ABAP_MATH`.",
		supports_named_arguments = true,
	},
	{
		name = "to_lower",
		params = []Builtin_Routine_Param_Spec{{"val", "string"}},
		return_type = "string",
		description = "Returns a text value converted to lowercase.",
		supports_named_arguments = true,
	},
	{
		name = "to_upper",
		params = []Builtin_Routine_Param_Spec{{"val", "string"}},
		return_type = "string",
		description = "Returns a text value converted to uppercase.",
		supports_named_arguments = true,
	},
	{
		name = "to_mixed",
		params = []Builtin_Routine_Param_Spec {
			{"val", "string"},
			{"sep", "string"},
			{"case", "string"},
			{"min", "i"},
		},
		return_type = "string",
		description = "Converts separator-delimited text to mixed case.",
		supports_named_arguments = true,
	},
	{
		name = "from_mixed",
		params = []Builtin_Routine_Param_Spec {
			{"val", "string"},
			{"sep", "string"},
			{"case", "string"},
			{"min", "i"},
		},
		return_type = "string",
		description = "Converts mixed-case text to separator-delimited text.",
		supports_named_arguments = true,
	},
	{
		name = "xstrlen",
		params = []Builtin_Routine_Param_Spec{{"arg", "xstring"}, {"val", "xstring"}},
		return_type = "i",
		description = "Number of bytes in a byte string value.",
		supports_named_arguments = false,
	},
	{
		name = "lines",
		params = []Builtin_Routine_Param_Spec{{"arg", "data"}, {"val", "data"}},
		return_type = "i",
		description = "Number of rows in an internal table value.",
		supports_named_arguments = false,
	},
	{
		name = "concat_lines_of",
		params = []Builtin_Routine_Param_Spec{{"table", "data"}, {"sep", "string"}},
		return_type = "string",
		description = "Concatenates the rows of an internal table into one character string, optionally inserting `sep` between rows.",
		supports_named_arguments = true,
	},
}

install_builtins :: proc(unit: ^Unit_Analysis, root_scope: Scope_Id, allocator: mem.Allocator) {
	for spec in BUILTIN_STRUCTURES {
		fields := make([dynamic]Structure_Field_Data, 0, len(spec.fields), allocator)
		for field in spec.fields {
			nested := INVALID_STRUCTURE_ID
			if field.structure_name != "" {
				if s := find_structure(unit, field.structure_name); s != nil {
					nested = s.id
				}
			}
			flags: Structure_Field_Flags
			if field.type_name != "" {
				flags += {.Has_Type_Ref}
			}
			append(
				&fields,
				Structure_Field_Data {
					name = field.name,
					decl_unit = unit.unit_id,
					structure = nested,
					type_ref = builtin_type_ref(field.type_name),
					description = field.description,
					flags = flags,
				},
			)
		}
		_ = push_structure(unit, spec.name, fields)
	}

	zero := tokenizer.text_range(0, 0)
	for name in BUILTIN_SCALAR_TYPES {
		_ = declare_symbol(unit, root_scope, name, .Builtin_Type, zero)
	}
	for spec in BUILTIN_SYMBOLS {
		kind := Symbol_Kind.Builtin_Type
		switch spec.kind {
		case .Type:
			kind = .Builtin_Type
		case .Constant:
			kind = .Builtin_Constant
		case .Variable:
			kind = .Builtin_Variable
		}
		structure := INVALID_STRUCTURE_ID
		if spec.structure_name != "" {
			if s := find_structure(unit, spec.structure_name); s != nil {
				structure = s.id
			}
		}
		_ = declare_symbol(unit, root_scope, spec.name, kind, zero, structure)
	}
	for routine in BUILTIN_ROUTINES {
		return_type := builtin_type_ref(routine.return_type)
		_ = declare_symbol(
			unit,
			root_scope,
			routine.name,
			.Builtin_Routine,
			zero,
			declared_type = return_type,
			has_declared_type = true,
			type_clause_display = routine.return_type,
		)
	}
}

builtin_routine_spec :: proc(name: string) -> ^Builtin_Routine_Spec {
	for &routine in BUILTIN_ROUTINES {
		if strings.equal_fold(routine.name, name) {
			return &routine
		}
	}
	return nil
}

builtin_structure_field_description :: proc(structure_name, field_name: string) -> string {
	for structure in BUILTIN_STRUCTURES {
		if !strings.equal_fold(structure.name, structure_name) {
			continue
		}
		for field in structure.fields {
			if strings.equal_fold(field.name, field_name) {
				return field.description
			}
		}
	}
	return ""
}

builtin_class_attribute_type_fact :: proc(class_name, attribute_name: string) -> (Type_Fact_Data, bool) {
	for attribute in BUILTIN_CLASS_ATTRIBUTES {
		if strings.equal_fold(attribute.class_name, class_name) &&
		   strings.equal_fold(attribute.name, attribute_name) {
			return Type_Fact_Data {
					structure = INVALID_STRUCTURE_ID,
					declared_type = builtin_type_ref(attribute.type_name),
					has_declared_type = true,
					type_clause_display = attribute.type_name,
				},
				true
		}
	}
	return {}, false
}
