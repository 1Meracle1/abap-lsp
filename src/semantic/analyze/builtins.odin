package abap_frontend_semantic_analyze

import base_runtime "base:runtime"

import "src:tokenizer"

import "core:mem"
import "core:strings"
import "core:sync"

Builtin_Type_Kind :: enum {
	Type,
	Constant,
	Variable,
}

Builtin_Field_Spec :: struct {
	name:           string,
	type_name:      string,
	structure_name: string,
	is_ref:         bool,
	description:    string,
}

Builtin_Structure_Spec :: struct {
	name:   string,
	fields: []Builtin_Field_Spec,
}

Builtin_Symbol_Spec :: struct {
	name:                 string,
	kind:                 Builtin_Type_Kind,
	structure_name:       string,
	type_name:            string,
	type_field_name:      string,
	type_clause_display:  string,
	value_clause_display: string,
}

Builtin_Type_Metadata :: struct {
	type_name:           string,
	type_clause_display: string,
	is_ref:              bool,
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

BUILTIN_PRIMITIVE_TYPES :: []string {
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
	"%_c_pointer",
}

BUILTIN_GENERIC_PRIMITIVE_TYPES :: []string {
	"xsequence",
	"data",
	"any",
	"simple",
	"decfloat",
	"numeric",
	"clike",
	"csequence",
	"object",
}

is_builtin_primitive_type_name :: #force_inline proc "contextless" (name: string) -> bool {
	return builtin_type_name_in(BUILTIN_PRIMITIVE_TYPES, name)
}

is_generic_builtin_type_name :: #force_inline proc "contextless" (name: string) -> bool {
	return builtin_type_name_in(BUILTIN_GENERIC_PRIMITIVE_TYPES, name)
}

is_generic_builtin_ref_type_name :: #force_inline proc "contextless" (name: string) -> bool {
	return name == "data" || name == "object"
}

is_builtin_type_name :: #force_inline proc "contextless" (name: string) -> bool {
	return(
		is_builtin_primitive_type_name(name) ||
		is_generic_builtin_type_name(name) ||
		name == "any table" \
	)
}

builtin_type_name_in :: proc "contextless" (names: []string, name: string) -> bool {
	for builtin in names {
		if builtin == name {
			return true
		}
	}
	return false
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
				name = "datlo",
				type_name = "d",
				structure_name = "",
				description = "Local date of the current user.",
			},
			{
				name = "dbcnt",
				type_name = "i",
				structure_name = "",
				description = "Number of database rows processed by the last SQL statement that documents it.",
			},
			{
				name = "dynnr",
				type_name = "c",
				structure_name = "",
				description = "Current dynpro number.",
			},
			{
				name = "fdpos",
				type_name = "i",
				structure_name = "",
				description = "Found offset after supported search and comparison operations such as FIND.",
			},
			{
				name = "host",
				type_name = "c",
				structure_name = "",
				description = "Host name of the current application server instance.",
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
				name = "mandt",
				type_name = "c",
				structure_name = "",
				description = "Client ID of the current user.",
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
				name = "pfkey",
				type_name = "c",
				structure_name = "",
				description = "Current GUI status.",
			},
			{
				name = "repid",
				type_name = "c",
				structure_name = "",
				description = "Program name exposed through sy-repid and syst-repid; SAP documents this as a predefined constant and type, not a real SYST component.",
			},
			{
				name = "saprl",
				type_name = "c",
				structure_name = "",
				description = "ABAP release identifier of the current system.",
			},
			{
				name = "scols",
				type_name = "i",
				structure_name = "",
				description = "Number of columns on the screen.",
			},
			{
				name = "srows",
				type_name = "i",
				structure_name = "",
				description = "Number of screen rows.",
			},
			{
				name = "subrc",
				type_name = "i",
				structure_name = "",
				description = "Return code set by many ABAP statements; 0 usually indicates success for the documented statement.",
			},
			{name = "sysid", type_name = "c", structure_name = "", description = "SAP system ID."},
			{
				name = "tabix",
				type_name = "i",
				structure_name = "",
				description = "Current internal-table index from READ TABLE or LOOP AT on indexed access paths.",
			},
			{
				name = "tcode",
				type_name = "c",
				structure_name = "",
				description = "Current transaction code.",
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
		name = "textpool",
		fields = []Builtin_Field_Spec {
			{name = "id", type_name = "c"},
			{name = "key", type_name = "c"},
			{name = "entry", type_name = "c"},
			{name = "length", type_name = "i"},
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
	{
		name = "abap_trans_srcbind",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "abap_trans_srcname"},
			{name = "value", type_name = "data", is_ref = true},
		},
	},
	{
		name = "abap_trans_resbind",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "abap_trans_resname"},
			{name = "value", type_name = "data", is_ref = true},
		},
	},
	{
		name = "abap_componentdescr",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "string"},
			{name = "type", type_name = "cl_abap_datadescr", is_ref = true},
			{name = "as_include", type_name = "abap_bool"},
			{name = "suffix", type_name = "string"},
		},
	},
	{
		name = "abap_simple_componentdescr",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "string"},
			{name = "type", type_name = "cl_abap_datadescr", is_ref = true},
		},
	},
	{
		name = "abap_compdescr",
		fields = []Builtin_Field_Spec {
			{name = "length", type_name = "i"},
			{name = "decimals", type_name = "i"},
			{name = "type_kind", type_name = "abap_typekind"},
			{name = "name", type_name = "abap_compname"},
		},
	},
	{
		name = "abap_keydescr",
		fields = []Builtin_Field_Spec{{name = "name", type_name = "abap_keyname"}},
	},
	{
		name = "abap_table_keycompdescr",
		fields = []Builtin_Field_Spec{{name = "name", type_name = "string"}},
	},
	{
		name = "abap_table_keydescr",
		fields = []Builtin_Field_Spec {
			{
				name = "components",
				type_name = "abap_table_keycompdescr",
				structure_name = "abap_table_keycompdescr",
			},
			{name = "name", type_name = "string"},
			{name = "is_primary", type_name = "abap_bool"},
			{name = "access_kind", type_name = "abap_tablekind"},
			{name = "is_unique", type_name = "abap_bool"},
			{name = "key_kind", type_name = "abap_keydefkind"},
		},
	},
	{
		name = "abap_parmdescr",
		fields = []Builtin_Field_Spec {
			{name = "length", type_name = "i"},
			{name = "decimals", type_name = "i"},
			{name = "type_kind", type_name = "abap_typekind"},
			{name = "name", type_name = "abap_parmname"},
			{name = "parm_kind", type_name = "abap_parmkind"},
			{name = "by_value", type_name = "abap_bool"},
			{name = "is_optional", type_name = "abap_bool"},
		},
	},
	{
		name = "abap_excpdescr",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "abap_excpname"},
			{name = "is_resumable", type_name = "abap_bool"},
		},
	},
	{
		name = "abap_frnddescr",
		fields = []Builtin_Field_Spec{{name = "name", type_name = "abap_classname"}},
	},
	{
		name = "abap_intfdescr",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "abap_intfname"},
			{name = "is_inherited", type_name = "abap_bool"},
		},
	},
	{
		name = "abap_typedef",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "abap_typename"},
			{name = "alias_for", type_name = "abap_typename"},
			{name = "visibility", type_name = "abap_visibility"},
			{name = "is_interface", type_name = "abap_bool"},
			{name = "is_inherited", type_name = "abap_bool"},
		},
	},
	{
		name = "abap_attrdescr",
		fields = []Builtin_Field_Spec {
			{name = "length", type_name = "i"},
			{name = "decimals", type_name = "i"},
			{name = "name", type_name = "abap_attrname"},
			{name = "type_kind", type_name = "abap_typekind"},
			{name = "visibility", type_name = "abap_visibility"},
			{name = "is_interface", type_name = "abap_bool"},
			{name = "is_inherited", type_name = "abap_bool"},
			{name = "is_class", type_name = "abap_bool"},
			{name = "is_constant", type_name = "abap_bool"},
			{name = "is_virtual", type_name = "abap_bool"},
			{name = "is_read_only", type_name = "abap_bool"},
			{name = "alias_for", type_name = "abap_attrname"},
		},
	},
	{
		name = "abap_methdescr",
		fields = []Builtin_Field_Spec {
			{
				name = "parameters",
				type_name = "abap_parmdescr_tab",
				structure_name = "abap_parmdescr",
			},
			{
				name = "exceptions",
				type_name = "abap_excpdescr_tab",
				structure_name = "abap_excpdescr",
			},
			{name = "name", type_name = "abap_methname"},
			{name = "for_event", type_name = "abap_evntname"},
			{name = "of_class", type_name = "abap_classname"},
			{name = "visibility", type_name = "abap_visibility"},
			{name = "is_interface", type_name = "abap_bool"},
			{name = "is_inherited", type_name = "abap_bool"},
			{name = "is_redefined", type_name = "abap_bool"},
			{name = "is_abstract", type_name = "abap_bool"},
			{name = "is_final", type_name = "abap_bool"},
			{name = "is_class", type_name = "abap_bool"},
			{name = "alias_for", type_name = "abap_methname"},
			{name = "is_raising_excps", type_name = "abap_bool"},
		},
	},
	{
		name = "abap_evntdescr",
		fields = []Builtin_Field_Spec {
			{
				name = "parameters",
				type_name = "abap_parmdescr_tab",
				structure_name = "abap_parmdescr",
			},
			{name = "name", type_name = "abap_evntname"},
			{name = "visibility", type_name = "abap_visibility"},
			{name = "is_interface", type_name = "abap_bool"},
			{name = "is_inherited", type_name = "abap_bool"},
			{name = "is_class", type_name = "abap_bool"},
			{name = "alias_for", type_name = "abap_evntname"},
		},
	},
	{
		name = "abap_func_parmbind",
		fields = []Builtin_Field_Spec {
			{name = "value", type_name = "data", is_ref = true},
			{name = "tables_wa", type_name = "data", is_ref = true},
			{name = "kind", type_name = "i"},
			{name = "name", type_name = "abap_parmname"},
		},
	},
	{
		name = "abap_func_excpbind",
		fields = []Builtin_Field_Spec {
			{name = "message", type_name = "data", is_ref = true},
			{name = "value", type_name = "i"},
			{name = "name", type_name = "abap_excpname"},
		},
	},
	{
		name = "abap_parmbind",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "abap_parmname"},
			{name = "kind", type_name = "abap_parmkind"},
			{name = "value", type_name = "data", is_ref = true},
		},
	},
	{
		name = "abap_excpbind",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "abap_excpname"},
			{name = "value", type_name = "i"},
		},
	},
	{
		name = "abap_trans_parmbind",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "abap_trans_parmname"},
			{name = "value", type_name = "abap_trans_parmvalue"},
		},
	},
	{
		name = "abap_trans_parm_obj_bind",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "abap_trans_parmname"},
			{name = "value", type_name = "abap_trans_parmref", is_ref = true},
		},
	},
	{
		name = "abap_trans_objbind",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "abap_trans_objname"},
			{name = "value", type_name = "object", is_ref = true},
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
	{name = "textpool", kind = .Type, structure_name = "textpool"},
	{name = "textpool_table", kind = .Type, structure_name = "textpool"},
	{name = "syst_short", kind = .Type, structure_name = ""},
	{name = "syst_byte", kind = .Type, structure_name = ""},
	{name = "syst_long", kind = .Type, structure_name = ""},
	{name = "tabname", kind = .Type, structure_name = ""},
	{name = "progname", kind = .Type, structure_name = ""},
	{name = "include", kind = .Type, structure_name = ""},
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
	{name = "abap_component_tab", kind = .Type, structure_name = "abap_componentdescr"},
	{name = "abap_func_parmbind_tab", kind = .Type, structure_name = "abap_func_parmbind"},
	{name = "abap_func_excpbind_tab", kind = .Type, structure_name = "abap_func_excpbind"},
	{name = "abap_func_parmbind", kind = .Type, structure_name = "abap_func_parmbind"},
	{name = "abap_func_excpbind", kind = .Type, structure_name = "abap_func_excpbind"},
	{name = "abap_trans_srcbind_tab", kind = .Type, structure_name = "abap_trans_srcbind"},
	{name = "abap_trans_resbind_tab", kind = .Type, structure_name = "abap_trans_resbind"},
	{name = "abap_componentdescr", kind = .Type, structure_name = "abap_componentdescr"},
	{
		name = "abap_simple_componentdescr",
		kind = .Type,
		structure_name = "abap_simple_componentdescr",
	},
	{name = "abap_abstypename", kind = .Type, structure_name = ""},
	{name = "abap_compdescr", kind = .Type, structure_name = "abap_compdescr"},
	{name = "abap_keydescr", kind = .Type, structure_name = "abap_keydescr"},
	{name = "abap_table_keydescr_tab", kind = .Type, structure_name = "abap_table_keydescr"},
	{name = "abap_table_keycompdescr", kind = .Type, structure_name = "abap_table_keycompdescr"},
	{name = "abap_table_keydescr", kind = .Type, structure_name = "abap_table_keydescr"},
	{name = "abap_intfdescr_tab", kind = .Type, structure_name = "abap_intfdescr"},
	{name = "abap_typecategory", kind = .Type, structure_name = ""},
	{name = "abap_typekind", kind = .Type, structure_name = ""},
	{name = "abap_typepropkind", kind = .Type, structure_name = ""},
	{
		name = "abap_component_symbol_tab",
		kind = .Type,
		structure_name = "abap_simple_componentdescr",
	},
	{
		name = "abap_component_view_tab",
		kind = .Type,
		structure_name = "abap_simple_componentdescr",
	},
	{name = "abap_structkind", kind = .Type, structure_name = ""},
	{name = "abap_compdescr_tab", kind = .Type, structure_name = "abap_compdescr"},
	{name = "abapsource", kind = .Type, structure_name = ""},
	{name = "abap_encoding", kind = .Type, structure_name = ""},
	{name = "abap_editmask", kind = .Type, structure_name = ""},
	{name = "abap_helpid", kind = .Type, structure_name = ""},
	{name = "abap_classkind", kind = .Type, structure_name = ""},
	{name = "abap_visibility", kind = .Type, structure_name = ""},
	{name = "abap_frndtypes_tab", kind = .Type, structure_name = ""},
	{name = "abap_tablekind", kind = .Type, structure_name = ""},
	{name = "abap_keydefkind", kind = .Type, structure_name = ""},
	{name = "abap_keydescr_tab", kind = .Type, structure_name = "abap_keydescr"},
	{name = "abap_methname", kind = .Type, structure_name = ""},
	{name = "abap_methdescr", kind = .Type, structure_name = "abap_methdescr"},
	{name = "abap_parmdescr", kind = .Type, structure_name = "abap_parmdescr"},
	{name = "abap_parmdescr_tab", kind = .Type, structure_name = "abap_parmdescr"},
	{name = "abap_excpdescr", kind = .Type, structure_name = "abap_excpdescr"},
	{name = "abap_excpdescr_tab", kind = .Type, structure_name = "abap_excpdescr"},
	{name = "abap_frnddescr", kind = .Type, structure_name = "abap_frnddescr"},
	{name = "abap_frnddescr_tab", kind = .Type, structure_name = "abap_frnddescr"},
	{name = "abap_intfdescr", kind = .Type, structure_name = "abap_intfdescr"},
	{name = "abap_typedef", kind = .Type, structure_name = "abap_typedef"},
	{name = "abap_attrdescr", kind = .Type, structure_name = "abap_attrdescr"},
	{name = "abap_evntdescr", kind = .Type, structure_name = "abap_evntdescr"},
	{name = "abap_endian", kind = .Type, structure_name = ""},
	{name = "abap_parmkind", kind = .Type, structure_name = ""},
	{name = "abap_typedef_tab", kind = .Type, structure_name = "abap_typedef"},
	{name = "abap_attrdescr_tab", kind = .Type, structure_name = "abap_attrdescr"},
	{name = "abap_methdescr_tab", kind = .Type, structure_name = "abap_methdescr"},
	{name = "abap_evntdescr_tab", kind = .Type, structure_name = "abap_evntdescr"},
	{name = "abap_parmbind", kind = .Type, structure_name = "abap_parmbind"},
	{name = "abap_parmbind_tab", kind = .Type, structure_name = "abap_parmbind"},
	{name = "abap_excpbind", kind = .Type, structure_name = "abap_excpbind"},
	{name = "abap_excpbind_tab", kind = .Type, structure_name = "abap_excpbind"},
	{name = "abap_intfkind", kind = .Type, structure_name = ""},
	{name = "abap_char1", kind = .Type, structure_name = ""},
	{name = "abap_cr_lf", kind = .Type, structure_name = ""},
	{name = "abap_byte_order_mark", kind = .Type, structure_name = ""},
	{name = "abap_byte_order_utf8", kind = .Type, structure_name = ""},
	{name = "abap_trans_parmname", kind = .Type, structure_name = ""},
	{name = "abap_trans_parmvalue", kind = .Type, structure_name = ""},
	{name = "abap_trans_parmref", kind = .Type, structure_name = ""},
	{name = "abap_trans_parmbind", kind = .Type, structure_name = "abap_trans_parmbind"},
	{name = "abap_trans_parm_obj_bind", kind = .Type, structure_name = "abap_trans_parm_obj_bind"},
	{name = "abap_trans_parmbind_tab", kind = .Type, structure_name = "abap_trans_parmbind"},
	{
		name = "abap_trans_parm_obj_bind_tab",
		kind = .Type,
		structure_name = "abap_trans_parm_obj_bind",
	},
	{name = "abap_trans_objname", kind = .Type, structure_name = ""},
	{name = "abap_trans_objbind", kind = .Type, structure_name = "abap_trans_objbind"},
	{name = "abap_trans_objbind_tab", kind = .Type, structure_name = "abap_trans_objbind"},
	{name = "abap_trans_srcname", kind = .Type, structure_name = ""},
	{name = "abap_trans_srcbind", kind = .Type, structure_name = "abap_trans_srcbind"},
	{name = "abap_trans_srcbind_tab_sorted", kind = .Type, structure_name = "abap_trans_srcbind"},
	{name = "abap_trans_resname", kind = .Type, structure_name = ""},
	{name = "abap_trans_resbind", kind = .Type, structure_name = "abap_trans_resbind"},
	{name = "abap_trans_resbind_tab_sorted", kind = .Type, structure_name = "abap_trans_resbind"},
	{name = "%_charsize", kind = .Constant, type_name = "i", value_clause_display = "%_CHARSIZE"},
	{
		name = "%_endian",
		kind = .Constant,
		type_name = "abap_endian",
		value_clause_display = "%_ENDIAN",
	},
	{
		name = "%_minchar",
		kind = .Constant,
		type_name = "abap_char1",
		value_clause_display = "%_MINCHAR",
	},
	{
		name = "%_maxchar",
		kind = .Constant,
		type_name = "abap_char1",
		value_clause_display = "%_MAXCHAR",
	},
	{
		name = "%_horizontal_tab",
		kind = .Constant,
		type_name = "abap_char1",
		value_clause_display = "%_HORIZONTAL_TAB",
	},
	{
		name = "%_vertical_tab",
		kind = .Constant,
		type_name = "abap_char1",
		value_clause_display = "%_VERTICAL_TAB",
	},
	{
		name = "%_newline",
		kind = .Constant,
		type_name = "abap_char1",
		value_clause_display = "%_NEWLINE",
	},
	{
		name = "%_cr_lf",
		kind = .Constant,
		type_name = "abap_cr_lf",
		value_clause_display = "%_CR_LF",
	},
	{
		name = "%_formfeed",
		kind = .Constant,
		type_name = "abap_char1",
		value_clause_display = "%_FORMFEED",
	},
	{
		name = "%_backspace",
		kind = .Constant,
		type_name = "abap_char1",
		value_clause_display = "%_BACKSPACE",
	},
	{name = "abap_true", kind = .Constant, type_name = "abap_bool", value_clause_display = "'X'"},
	{name = "abap_false", kind = .Constant, type_name = "abap_bool", value_clause_display = "' '"},
	{
		name = "abap_undefined",
		kind = .Constant,
		type_name = "abap_bool",
		value_clause_display = "'-'",
	},
	{name = "abap_on", kind = .Constant, type_name = "abap_bool", value_clause_display = "'X'"},
	{name = "abap_off", kind = .Constant, type_name = "abap_bool", value_clause_display = "' '"},
	{
		name = "abap_max_abs_type_name_ln",
		kind = .Constant,
		type_name = "i",
		value_clause_display = "200",
	},
	{
		name = "abap_max_class_name_ln",
		kind = .Constant,
		type_name = "i",
		value_clause_display = "30",
	},
	{
		name = "abap_max_intf_name_ln",
		kind = .Constant,
		type_name = "i",
		value_clause_display = "30",
	},
	{
		name = "abap_max_comp_name_ln",
		kind = .Constant,
		type_name = "i",
		value_clause_display = "30",
	},
	{
		name = "abap_max_key_name_ln",
		kind = .Constant,
		type_name = "i",
		value_clause_display = "255",
	},
	{
		name = "abap_max_class_comp_name_ln",
		kind = .Constant,
		type_name = "i",
		value_clause_display = "61",
	},
	{
		name = "abap_max_edit_mask_ln",
		kind = .Constant,
		type_name = "i",
		value_clause_display = "7",
	},
	{name = "abap_max_help_id_ln", kind = .Constant, type_name = "i", value_clause_display = "62"},
	{
		name = "abap_max_db_string_ln",
		kind = .Constant,
		type_name = "i",
		value_clause_display = "536870912",
	},
	{
		name = "abap_max_db_rawstring_ln",
		kind = .Constant,
		type_name = "i",
		value_clause_display = "1073741824",
	},
	{
		name = "abap_func_exporting",
		kind = .Constant,
		type_name = "abap_func_parmbind",
		type_field_name = "kind",
		type_clause_display = "abap_func_parmbind-kind",
		value_clause_display = "10",
	},
	{
		name = "abap_func_importing",
		kind = .Constant,
		type_name = "abap_func_parmbind",
		type_field_name = "kind",
		type_clause_display = "abap_func_parmbind-kind",
		value_clause_display = "20",
	},
	{
		name = "abap_func_tables",
		kind = .Constant,
		type_name = "abap_func_parmbind",
		type_field_name = "kind",
		type_clause_display = "abap_func_parmbind-kind",
		value_clause_display = "30",
	},
	{
		name = "abap_func_changing",
		kind = .Constant,
		type_name = "abap_func_parmbind",
		type_field_name = "kind",
		type_clause_display = "abap_func_parmbind-kind",
		value_clause_display = "40",
	},
	{
		name = "icon_led_red",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@5C@'",
	},
	{
		name = "icon_led_yellow",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@5D@'",
	},
	{
		name = "icon_led_green",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@5B@'",
	},
	{
		name = "icon_led_inactive",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@BZ@'",
	},
	{
		name = "icon_message_information",
		kind = .Constant,
		type_name = "icon_l4",
		value_clause_display = "'@19@'",
	},
	{
		name = "icon_system_help",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@35@'",
	},
	{
		name = "icon_stack",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@3B@'",
	},
	{name = "icon_abap", kind = .Constant, type_name = "icon_l2", value_clause_display = "'@9U@'"},
	{
		name = "icon_warning",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@AH@'",
	},
	{
		name = "icon_package_standard",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@QC@'",
	},
	{
		name = "icon_no_status",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@MG@'",
	},
	{
		name = "icon_create",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@0Y@'",
	},
	{
		name = "icon_delete",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@11@'",
	},
	{
		name = "icon_change",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@0Z@'",
	},
	{
		name = "icon_adopt",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@IL@'",
	},
	{name = "icon_okay", kind = .Constant, type_name = "icon_l2", value_clause_display = "'@0V@'"},
	{
		name = "icon_set_state",
		kind = .Constant,
		type_name = "icon_l2",
		value_clause_display = "'@3J@'",
	},
	{name = "col_background", kind = .Constant, type_name = "c", value_clause_display = "'0'"},
	{name = "col_heading", kind = .Constant, type_name = "c", value_clause_display = "'1'"},
	{name = "col_normal", kind = .Constant, type_name = "c", value_clause_display = "'2'"},
	{name = "col_total", kind = .Constant, type_name = "c", value_clause_display = "'3'"},
	{name = "col_key", kind = .Constant, type_name = "c", value_clause_display = "'4'"},
	{name = "col_positive", kind = .Constant, type_name = "c", value_clause_display = "'5'"},
	{name = "col_negative", kind = .Constant, type_name = "c", value_clause_display = "'6'"},
	{name = "col_group", kind = .Constant, type_name = "c", value_clause_display = "'7'"},
	{name = "space", kind = .Constant, type_name = "c", value_clause_display = "' '"},
	{name = "text", kind = .Variable, structure_name = ""},
	{name = "cntl_simple_event", kind = .Type, structure_name = "cntl_simple_event"},
	{name = "cntl_simple_events", kind = .Type, structure_name = "cntl_simple_event"},
}

builtin_type_metadata :: proc(name: string) -> (Builtin_Type_Metadata, bool) {
	switch name {
	case "abap_bool",
	     "abap_typekind",
	     "abap_typecategory",
	     "abap_typepropkind",
	     "abap_structkind",
	     "abap_tablekind",
	     "abap_keydefkind",
	     "abap_classkind",
	     "abap_intfkind",
	     "abap_parmkind",
	     "abap_visibility",
	     "abap_char1":
		return {type_name = "c", type_clause_display = "c LENGTH 1"}, true
	case "abap_cr_lf":
		return {type_name = "c", type_clause_display = "c LENGTH 2"}, true
	case "abap_byte_order_mark":
		return {type_name = "x", type_clause_display = "x LENGTH 2"}, true
	case "abap_byte_order_utf8":
		return {type_name = "x", type_clause_display = "x LENGTH 3"}, true
	case "progname", "include":
		return {type_name = "c", type_clause_display = "c LENGTH 40"}, true
	case "abap_editmask":
		return {type_name = "c", type_clause_display = "c LENGTH abap_max_edit_mask_ln"}, true
	case "abap_helpid":
		return {type_name = "c", type_clause_display = "c LENGTH abap_max_help_id_ln"}, true
	case "abap_typename", "abap_attrname", "abap_methname", "abap_evntname":
		return {type_name = "c", type_clause_display = "c LENGTH abap_max_class_comp_name_ln"},
			true
	case "abap_abstypename":
		return {type_name = "c", type_clause_display = "c LENGTH abap_max_abs_type_name_ln"}, true
	case "abap_compname", "abap_parmname", "abap_excpname":
		return {type_name = "c", type_clause_display = "c LENGTH abap_max_comp_name_ln"}, true
	case "abap_keyname":
		return {type_name = "c", type_clause_display = "c LENGTH abap_max_key_name_ln"}, true
	case "abap_keycompname":
		return {type_name = "abap_keyname", type_clause_display = "abap_keyname"}, true
	case "abap_classname":
		return {type_name = "c", type_clause_display = "c LENGTH abap_max_class_name_ln"}, true
	case "abap_intfname":
		return {type_name = "c", type_clause_display = "c LENGTH abap_max_intf_name_ln"}, true
	case "textpool_table":
		return {
				type_name = "textpool",
				type_clause_display = "STANDARD TABLE OF textpool WITH DEFAULT KEY",
			},
			true
	case "abap_compdescr_tab":
		return {
				type_name = "abap_compdescr",
				type_clause_display = "STANDARD TABLE OF abap_compdescr WITH KEY name",
			},
			true
	case "abap_component_tab":
		return {
				type_name = "abap_componentdescr",
				type_clause_display = "STANDARD TABLE OF abap_componentdescr WITH KEY name",
			},
			true
	case "abap_component_symbol_tab":
		return {
				type_name = "abap_simple_componentdescr",
				type_clause_display = "HASHED TABLE OF abap_simple_componentdescr WITH UNIQUE KEY name",
			},
			true
	case "abap_component_view_tab":
		return {
				type_name = "abap_simple_componentdescr",
				type_clause_display = "STANDARD TABLE OF abap_simple_componentdescr WITH KEY name",
			},
			true
	case "abap_keydescr_tab":
		return {
				type_name = "abap_keydescr",
				type_clause_display = "STANDARD TABLE OF abap_keydescr WITH KEY name",
			},
			true
	case "abap_table_keydescr_tab":
		return {
				type_name = "abap_table_keydescr",
				type_clause_display = "STANDARD TABLE OF abap_table_keydescr WITH NON-UNIQUE KEY name",
			},
			true
	case "abap_parmdescr_tab":
		return {
				type_name = "abap_parmdescr",
				type_clause_display = "STANDARD TABLE OF abap_parmdescr WITH KEY name",
			},
			true
	case "abap_excpdescr_tab":
		return {
				type_name = "abap_excpdescr",
				type_clause_display = "STANDARD TABLE OF abap_excpdescr WITH KEY name",
			},
			true
	case "abap_frnddescr_tab":
		return {
				type_name = "abap_frnddescr",
				type_clause_display = "STANDARD TABLE OF abap_frnddescr WITH KEY name",
			},
			true
	case "abap_intfdescr_tab":
		return {
				type_name = "abap_intfdescr",
				type_clause_display = "STANDARD TABLE OF abap_intfdescr WITH KEY name",
			},
			true
	case "abap_typedef_tab":
		return {
				type_name = "abap_typedef",
				type_clause_display = "STANDARD TABLE OF abap_typedef WITH KEY name",
			},
			true
	case "abap_attrdescr_tab":
		return {
				type_name = "abap_attrdescr",
				type_clause_display = "STANDARD TABLE OF abap_attrdescr WITH KEY name",
			},
			true
	case "abap_methdescr_tab":
		return {
				type_name = "abap_methdescr",
				type_clause_display = "STANDARD TABLE OF abap_methdescr WITH KEY name",
			},
			true
	case "abap_evntdescr_tab":
		return {
				type_name = "abap_evntdescr",
				type_clause_display = "STANDARD TABLE OF abap_evntdescr WITH KEY name",
			},
			true
	case "abap_frndtypes_tab":
		return {
				type_name = "cl_abap_typedescr",
				type_clause_display = "STANDARD TABLE OF REF TO cl_abap_typedescr WITH KEY table_line",
				is_ref = true,
			},
			true
	case "abap_func_parmbind_tab":
		return {
				type_name = "abap_func_parmbind",
				type_clause_display = "SORTED TABLE OF abap_func_parmbind WITH UNIQUE KEY kind name",
			},
			true
	case "abap_func_excpbind_tab":
		return {
				type_name = "abap_func_excpbind",
				type_clause_display = "HASHED TABLE OF abap_func_excpbind WITH UNIQUE KEY name",
			},
			true
	case "abap_parmbind_tab":
		return {
				type_name = "abap_parmbind",
				type_clause_display = "HASHED TABLE OF abap_parmbind WITH UNIQUE KEY name",
			},
			true
	case "abap_excpbind_tab":
		return {
				type_name = "abap_excpbind",
				type_clause_display = "HASHED TABLE OF abap_excpbind WITH UNIQUE KEY name",
			},
			true
	case "abap_encoding":
		return {type_name = "abap_encod", type_clause_display = "abap_encod"}, true
	case "abap_endian":
		return {type_name = "abap_endia", type_clause_display = "abap_endia"}, true
	case "abap_trans_parmname",
	     "abap_trans_parmvalue",
	     "abap_trans_objname",
	     "abap_trans_srcname",
	     "abap_trans_resname":
		return {type_name = "string", type_clause_display = "string"}, true
	case "abap_trans_parmref":
		return {type_name = "data", type_clause_display = "REF TO data", is_ref = true}, true
	case "abap_trans_parmbind_tab":
		return {
				type_name = "abap_trans_parmbind",
				type_clause_display = "STANDARD TABLE OF abap_trans_parmbind WITH KEY name",
			},
			true
	case "abap_trans_parm_obj_bind_tab":
		return {
				type_name = "abap_trans_parm_obj_bind",
				type_clause_display = "SORTED TABLE OF abap_trans_parm_obj_bind WITH UNIQUE KEY name",
			},
			true
	case "abap_trans_objbind_tab":
		return {
				type_name = "abap_trans_objbind",
				type_clause_display = "STANDARD TABLE OF abap_trans_objbind WITH KEY name",
			},
			true
	case "abap_trans_srcbind_tab":
		return {
				type_name = "abap_trans_srcbind",
				type_clause_display = "STANDARD TABLE OF abap_trans_srcbind WITH KEY name",
			},
			true
	case "abap_trans_srcbind_tab_sorted":
		return {
				type_name = "abap_trans_srcbind",
				type_clause_display = "SORTED TABLE OF abap_trans_srcbind WITH UNIQUE KEY name",
			},
			true
	case "abap_trans_resbind_tab":
		return {
				type_name = "abap_trans_resbind",
				type_clause_display = "STANDARD TABLE OF abap_trans_resbind WITH KEY name",
			},
			true
	case "abap_trans_resbind_tab_sorted":
		return {
				type_name = "abap_trans_resbind",
				type_clause_display = "SORTED TABLE OF abap_trans_resbind WITH UNIQUE KEY name",
			},
			true
	}
	return {}, false
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
SUBSTRING_MATCH_PARAMS :: []Builtin_Routine_Param_Spec {
	{"val", "string"},
	{"sub", "string"},
	{"regex", "string"},
	{"occ", "i"},
	{"case", "abap_bool"},
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
	{
		name = "abs",
		params = NUMERIC_ARG_PARAMS,
		return_type = "data",
		description = "Absolute value of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "sign",
		params = NUMERIC_ARG_PARAMS,
		return_type = "data",
		description = "Sign of `arg`: -1, 0, or 1.",
		supports_named_arguments = false,
	},
	{
		name = "ceil",
		params = NUMERIC_ARG_PARAMS,
		return_type = "data",
		description = "Smallest integer not less than `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "floor",
		params = NUMERIC_ARG_PARAMS,
		return_type = "data",
		description = "Largest integer not greater than `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "trunc",
		params = NUMERIC_ARG_PARAMS,
		return_type = "data",
		description = "Integer part of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "frac",
		params = NUMERIC_ARG_PARAMS,
		return_type = "data",
		description = "Decimal part of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "ipow",
		params = IPOW_PARAMS,
		return_type = "data",
		description = "Integer power: `base` raised to `exp`.",
		supports_named_arguments = true,
	},
	{
		name = "nmax",
		params = EXTREMUM_PARAMS,
		return_type = "data",
		description = "Largest numeric argument.",
		supports_named_arguments = true,
	},
	{
		name = "nmin",
		params = EXTREMUM_PARAMS,
		return_type = "data",
		description = "Smallest numeric argument.",
		supports_named_arguments = true,
	},
	{
		name = "acos",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Arccosine of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "asin",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Arcsine of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "atan",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Arctangent of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "cos",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Cosine of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "sin",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Sine of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "tan",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Tangent of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "cosh",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Hyperbolic cosine of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "sinh",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Hyperbolic sine of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "tanh",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Hyperbolic tangent of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "exp",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Exponential function for base e.",
		supports_named_arguments = false,
	},
	{
		name = "log",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Natural logarithm of `arg`.",
		supports_named_arguments = false,
	},
	{
		name = "log10",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Logarithm of `arg` to base 10.",
		supports_named_arguments = false,
	},
	{
		name = "sqrt",
		params = FLOAT_ARG_PARAMS,
		return_type = "f",
		description = "Square root of `arg`.",
		supports_named_arguments = false,
	},
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
		name = "substring_before",
		params = SUBSTRING_MATCH_PARAMS,
		return_type = "string",
		description = "Returns the text before a substring or regular-expression match.",
		supports_named_arguments = true,
	},
	{
		name = "substring_after",
		params = SUBSTRING_MATCH_PARAMS,
		return_type = "string",
		description = "Returns the text after a substring or regular-expression match.",
		supports_named_arguments = true,
	},
	{
		name = "shift_left",
		params = []Builtin_Routine_Param_Spec {
			{"val", "string"},
			{"places", "i"},
			{"circular", "abap_bool"},
			{"sub", "string"},
		},
		return_type = "string",
		description = "Returns a text value shifted left.",
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
		name = "matches",
		params = []Builtin_Routine_Param_Spec {
			{"val", "string"},
			{"regex", "string"},
			{"case", "abap_bool"},
		},
		return_type = "abap_bool",
		description = "Predicate function: returns whether a text value matches a regular expression.",
		supports_named_arguments = true,
	},
	{
		name = "find",
		params = []Builtin_Routine_Param_Spec {
			{"val", "string"},
			{"sub", "string"},
			{"regex", "string"},
			{"occ", "i"},
			{"case", "abap_bool"},
		},
		return_type = "i",
		description = "Returns the offset of a substring or regular-expression match in a text value.",
		supports_named_arguments = true,
	},
	{
		name = "repeat",
		params = []Builtin_Routine_Param_Spec{{"val", "string"}, {"occ", "i"}},
		return_type = "string",
		description = "Returns a string containing `val` repeated `occ` times.",
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

builtin_entity_handle :: proc(namespace: Namespace, name: string) -> (Entity_Handle, bool) {
	if symbol_id, ok := builtin_symbol_id(namespace, name); ok {
		return Entity_Handle{provider = builtin_provider_handle(), id = Entity_Id(symbol_id)}, true
	}
	return {}, false
}

builtin_root_structure_name :: proc(namespace: Namespace, name: string) -> (string, bool) {
	for spec in BUILTIN_SYMBOLS {
		kind := builtin_symbol_kind_from_spec(spec.kind)
		if spec.structure_name != "" && builtin_symbol_matches(namespace, name, spec.name, kind) {
			return spec.structure_name, true
		}
	}
	return "", false
}

builtin_symbol_id :: proc(namespace: Namespace, name: string) -> (Symbol_Id, bool) {
	index := 0
	for builtin in BUILTIN_PRIMITIVE_TYPES {
		if builtin_symbol_matches(namespace, name, builtin, .Builtin_Type) {
			return builtin_symbol_id_from_index(index), true
		}
		index += 1
	}
	for builtin in BUILTIN_GENERIC_PRIMITIVE_TYPES {
		if builtin_symbol_matches(namespace, name, builtin, .Builtin_Type) {
			return builtin_symbol_id_from_index(index), true
		}
		index += 1
	}
	for spec in BUILTIN_SYMBOLS {
		kind := builtin_symbol_kind_from_spec(spec.kind)
		if builtin_symbol_matches(namespace, name, spec.name, kind) {
			return builtin_symbol_id_from_index(index), true
		}
		index += 1
	}
	for routine in BUILTIN_ROUTINES {
		if builtin_symbol_matches(namespace, name, routine.name, .Builtin_Routine) {
			return builtin_symbol_id_from_index(index), true
		}
		index += 1
	}
	return INVALID_SYMBOL_ID, false
}

BUILTIN_SYMBOL_ID_BASE :: u32(0x80000000)
BUILTIN_STRUCTURE_ID_BASE :: u32(0x80000000)

builtin_symbol_id_from_index :: proc "contextless" (index: int) -> Symbol_Id {
	return Symbol_Id(BUILTIN_SYMBOL_ID_BASE + u32(index))
}

builtin_symbol_index :: proc "contextless" (id: Symbol_Id) -> (int, bool) {
	raw := u32(id)
	if raw < BUILTIN_SYMBOL_ID_BASE {
		return -1, false
	}
	return int(raw - BUILTIN_SYMBOL_ID_BASE), true
}

builtin_structure_id :: proc "contextless" (index: int) -> Structure_Id {
	return Structure_Id(BUILTIN_STRUCTURE_ID_BASE + u32(index))
}

builtin_structure_index :: proc "contextless" (id: Structure_Id) -> (int, bool) {
	raw := u32(id)
	if raw < BUILTIN_STRUCTURE_ID_BASE {
		return -1, false
	}
	return int(raw - BUILTIN_STRUCTURE_ID_BASE), true
}

shared_builtin_provider_storage: Source_File_Provider
shared_builtin_provider_ready: bool
shared_builtin_provider_lock: sync.Mutex

shared_builtin_provider :: proc() -> ^Source_File_Provider {
	if sync.atomic_load_explicit(&shared_builtin_provider_ready, .Acquire) {
		return &shared_builtin_provider_storage
	}
	sync.mutex_lock(&shared_builtin_provider_lock)
	defer sync.mutex_unlock(&shared_builtin_provider_lock)
	if sync.atomic_load_explicit(&shared_builtin_provider_ready, .Acquire) {
		return &shared_builtin_provider_storage
	}
	allocator := base_runtime.heap_allocator()
	shared_builtin_provider_storage = source_file_provider_make(
		INVALID_SOURCE_FILE_ID,
		.Full_Source,
		"builtin://abap",
		tokenizer.Range{},
		allocator,
	)
	install_builtins(
		&shared_builtin_provider_storage,
		shared_builtin_provider_storage.root_scope,
		allocator,
	)
	for &st, i in shared_builtin_provider_storage.structures {
		st.id = builtin_structure_id(i)
		st.origin_unit = INVALID_SOURCE_FILE_ID
		st.origin_structure = st.id
		for &field in st.fields {
			field.decl_unit = INVALID_SOURCE_FILE_ID
			if field.structure != INVALID_STRUCTURE_ID {
				field.structure = builtin_structure_id(structure_id_index(field.structure))
			}
		}
	}
	for &s in shared_builtin_provider_storage.symbols {
		s.id = builtin_symbol_id_from_index(symbol_id_index(s.id))
		if s.structure != INVALID_STRUCTURE_ID {
			s.structure = builtin_structure_id(structure_id_index(s.structure))
		}
	}
	for &t in shared_builtin_provider_storage.types {
		if t.symbol != INVALID_SYMBOL_ID {
			t.symbol = builtin_symbol_id_from_index(symbol_id_index(t.symbol))
		}
		if t.structure != INVALID_STRUCTURE_ID {
			t.structure = builtin_structure_id(structure_id_index(t.structure))
		}
	}
	sync.atomic_store_explicit(&shared_builtin_provider_ready, true, .Release)
	return &shared_builtin_provider_storage
}

builtin_provider_is_shared :: proc "contextless" (unit: ^Source_File_Provider) -> bool {
	return unit == &shared_builtin_provider_storage
}

@(private)
builtin_symbol_matches :: proc(
	namespace: Namespace,
	query_name, builtin_name: string,
	kind: Symbol_Kind,
) -> bool {
	return symbol_kind_occupies(kind, namespace) && strings.equal_fold(query_name, builtin_name)
}

@(private)
builtin_symbol_kind_from_spec :: proc(kind: Builtin_Type_Kind) -> Symbol_Kind {
	switch kind {
	case .Type:
		return .Builtin_Type
	case .Constant:
		return .Builtin_Constant
	case .Variable:
		return .Builtin_Variable
	}
	return .Builtin_Type
}

install_builtins :: proc(
	unit: ^Source_File_Provider,
	root_scope: Scope_Id,
	allocator: mem.Allocator,
) {
	for &spec in BUILTIN_STRUCTURES {
		fields := make([dynamic]Structure_Field_Data, 0, len(spec.fields), allocator)
		for &field in spec.fields {
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
			type_ref := builtin_type_ref(field.type_name)
			type_ref.is_ref = field.is_ref
			type_id :=
				type_id_from_declared_type(unit, root_scope, type_ref) if field.type_name != "" else UNKNOWN_TYPE_ID
			if !type_id_is_known(type_id) && nested != INVALID_STRUCTURE_ID {
				type_id = type_structure(unit, nested)
			}
			append(
				&fields,
				Structure_Field_Data {
					name = field.name,
					decl_unit = unit.source_file_id,
					type_id = type_id,
					structure = nested,
					type_ref = type_ref,
					description = field.description,
					flags = flags,
				},
			)
		}
		_ = push_structure(unit, spec.name, fields)
	}

	zero := tokenizer.text_range(0, 0)
	for name in BUILTIN_PRIMITIVE_TYPES {
		_ = declare_symbol(unit, root_scope, name, .Builtin_Type, zero)
	}
	for name in BUILTIN_GENERIC_PRIMITIVE_TYPES {
		_ = declare_symbol(unit, root_scope, name, .Builtin_Type, zero)
	}
	for &spec in BUILTIN_SYMBOLS {
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
		declared_type := Field_Type_Ref_Data{}
		has_declared_type := false
		type_name := spec.type_name
		type_display := spec.type_clause_display
		type_is_ref := false
		if spec.kind == .Type && type_name == "" {
			if metadata, ok := builtin_type_metadata(spec.name); ok {
				type_name = metadata.type_name
				type_display = metadata.type_clause_display
				type_is_ref = metadata.is_ref
			}
		}
		if type_name != "" {
			declared_type = builtin_type_ref(type_name)
			declared_type.is_ref = type_is_ref
			has_declared_type = true
			if spec.type_field_name != "" {
				declared_type.field_path = make([dynamic]string, 0, 1, allocator)
				declared_type.field_ranges = make([dynamic]tokenizer.Range, 0, 1, allocator)
				append(&declared_type.field_path, spec.type_field_name)
				append(&declared_type.field_ranges, zero)
			}
		}
		if type_display == "" {
			type_display = type_name
		}
		_ = declare_symbol(
			unit,
			root_scope,
			spec.name,
			kind,
			zero,
			structure,
			declared_type,
			has_declared_type,
			type_display,
			spec.value_clause_display,
		)
	}
	for &routine in BUILTIN_ROUTINES {
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
	for &structure in BUILTIN_STRUCTURES {
		if !strings.equal_fold(structure.name, structure_name) {
			continue
		}
		for &field in structure.fields {
			if strings.equal_fold(field.name, field_name) {
				return field.description
			}
		}
	}
	return ""
}

builtin_class_attribute_type_fact :: proc(
	class_name, attribute_name: string,
) -> (
	Type_Fact_Data,
	bool,
) {
	for &attribute in BUILTIN_CLASS_ATTRIBUTES {
		if strings.equal_fold(attribute.class_name, class_name) &&
		   strings.equal_fold(attribute.name, attribute_name) {
			return Type_Fact_Data {
					structure = INVALID_STRUCTURE_ID,
					structure_unit = INVALID_SOURCE_FILE_ID,
					declared_type = builtin_type_ref(attribute.type_name),
					has_declared_type = true,
					type_clause_display = attribute.type_name,
					confidence = .High,
				},
				true
		}
	}
	return {}, false
}
