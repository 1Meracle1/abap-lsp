package abap_frontend_semantic

import "../tokenizer"

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
	hover_params:             []string,
	return_type:              string,
	supports_named_arguments: bool,
}

BUILTIN_SCALAR_TYPES :: []string {
	"i", "int1", "int2", "int4", "int8",
	"f", "p", "decfloat16", "decfloat34",
	"string", "c", "n", "d", "t", "x", "xstring",
	"data", "any", "clike", "csequence",
}

SYST_FIELDS :: []Builtin_Field_Spec {
	{"abcde", "c", ""},
	{"batch", "c", ""},
	{"binpt", "c", ""},
	{"cprog", "c", ""},
	{"datum", "d", ""},
	{"dbcnt", "i", ""},
	{"fdpos", "i", ""},
	{"index", "i", ""},
	{"langu", "c", ""},
	{"msgid", "c", ""},
	{"msgno", "n", ""},
	{"msgty", "c", ""},
	{"msgv1", "c", ""},
	{"msgv2", "c", ""},
	{"msgv3", "c", ""},
	{"msgv4", "c", ""},
	{"repid", "c", ""},
	{"subrc", "i", ""},
	{"tabix", "i", ""},
	{"tfill", "i", ""},
	{"timlo", "t", ""},
	{"tzone", "i", ""},
	{"ucomm", "c", ""},
	{"uname", "c", ""},
	{"uzeit", "t", ""},
}

SCREEN_FIELDS :: []Builtin_Field_Spec {
	{"name", "c", ""},
	{"group1", "c", ""},
	{"group2", "c", ""},
	{"group3", "c", ""},
	{"group4", "c", ""},
	{"required", "c", ""},
	{"input", "c", ""},
	{"output", "c", ""},
	{"intensified", "c", ""},
	{"invisible", "c", ""},
	{"length", "x", ""},
	{"active", "c", ""},
	{"display_3d", "c", ""},
	{"value_help", "c", ""},
	{"request", "c", ""},
	{"values_in_combo", "c", ""},
}

MATCH_RESULT_FIELDS :: []Builtin_Field_Spec {
	{"offset", "i", ""},
	{"length", "i", ""},
	{"submatches", "match_result_tab", "match_result"},
	{"line", "i", ""},
}

BUILTIN_STRUCTURES :: []Builtin_Structure_Spec {
	{"syst", SYST_FIELDS},
	{"screen", SCREEN_FIELDS},
	{"match_result", MATCH_RESULT_FIELDS},
}

BUILTIN_SYMBOLS :: []Builtin_Symbol_Spec {
	{"abap_bool", .Type, ""},
	{"flag", .Type, ""},
	{"xfeld", .Type, ""},
	{"sy", .Type, "syst"},
	{"syst", .Type, "syst"},
	{"screen", .Type, "screen"},
	{"syst", .Variable, "syst"},
	{"sy", .Variable, "syst"},
	{"screen", .Variable, "screen"},
	{"guid", .Type, ""},
	{"symsgv", .Type, ""},
	{"sydatum", .Type, ""},
	{"timestamp", .Type, ""},
	{"cursor", .Type, ""},
	{"match_result", .Type, "match_result"},
	{"match_result_tab", .Type, "match_result"},
	{"tabname", .Type, ""},
	{"cdobjectcl", .Type, ""},
	{"rs38l_fnam", .Type, ""},
	{"memoryid", .Type, ""},
	{"abap_true", .Constant, ""},
	{"abap_false", .Constant, ""},
	{"abap_undefined", .Constant, ""},
	{"space", .Constant, ""},
	{"text", .Variable, ""},
}

ARG_STRING_PARAMS :: []Builtin_Routine_Param_Spec {
	{"arg", "string"},
	{"val", "string"},
}

VAL_STRING_PARAMS :: []Builtin_Routine_Param_Spec {
	{"val", "string"},
}

MIXED_CASE_STRING_PARAMS :: []Builtin_Routine_Param_Spec {
	{"val", "string"},
	{"sep", "string"},
	{"case", "string"},
	{"min", "i"},
}

ARG_XSTRING_PARAMS :: []Builtin_Routine_Param_Spec {
	{"arg", "xstring"},
	{"val", "xstring"},
}

ARG_DATA_PARAMS :: []Builtin_Routine_Param_Spec {
	{"arg", "data"},
	{"val", "data"},
}

BUILTIN_ROUTINES :: []Builtin_Routine_Spec {
	{"line_exists", []Builtin_Routine_Param_Spec{{"table_line", "data"}}, []string{"table_line"}, "abap_bool", false},
	{"charlen", []Builtin_Routine_Param_Spec{{"arg", "string"}, {"text", "string"}}, []string{"arg"}, "i", false},
	{"dbmaxlen", ARG_STRING_PARAMS, []string{"arg"}, "i", false},
	{"numofchar", []Builtin_Routine_Param_Spec{{"arg", "string"}, {"str", "string"}}, []string{"arg"}, "i", false},
	{"strlen", ARG_STRING_PARAMS, []string{"arg"}, "i", false},
	{"substring", []Builtin_Routine_Param_Spec{{"val", "string"}, {"off", "i"}, {"len", "i"}}, []string{"val", "off", "len"}, "string", true},
	{"condense", []Builtin_Routine_Param_Spec{{"val", "string"}, {"del", "string"}, {"from", "string"}, {"to", "string"}}, []string{"val", "del", "from", "to"}, "string", true},
	{"replace", []Builtin_Routine_Param_Spec{{"val", "string"}, {"sub", "string"}, {"regex", "string"}, {"with", "string"}, {"occ", "i"}, {"case", "abap_bool"}}, []string{"val", "sub", "regex", "with", "occ", "case"}, "string", true},
	{"round", []Builtin_Routine_Param_Spec{{"val", "decfloat34"}, {"dec", "i"}, {"prec", "i"}, {"mode", "data"}}, []string{"val", "dec", "prec", "mode"}, "decfloat34", true},
	{"to_lower", VAL_STRING_PARAMS, []string{"val"}, "string", true},
	{"to_upper", VAL_STRING_PARAMS, []string{"val"}, "string", true},
	{"to_mixed", MIXED_CASE_STRING_PARAMS, []string{"val", "sep", "case", "min"}, "string", true},
	{"from_mixed", MIXED_CASE_STRING_PARAMS, []string{"val", "sep", "case", "min"}, "string", true},
	{"xstrlen", ARG_XSTRING_PARAMS, []string{"arg"}, "i", false},
	{"lines", ARG_DATA_PARAMS, []string{"arg"}, "i", false},
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
			append(&fields, Structure_Field_Data {
				name = field.name,
				decl_unit = unit.unit_id,
				structure = nested,
				type_ref = builtin_type_ref(field.type_name),
				flags = flags,
			})
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
