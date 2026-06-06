package abap_frontend_semantic

import "src:ast"
import string_interner "src:string_interner"

import "core:strings"

Builtin_Symbol_Kind :: enum {
	Type,
	Constant,
	Variable,
}

Builtin_Field_Spec :: struct {
	name:           string,
	type_name:      string,
	structure_name: string,
	is_ref:         bool,
	docs:           string,
}

Builtin_Structure_Spec :: struct {
	name:   string,
	fields: []Builtin_Field_Spec,
}

Builtin_Symbol_Spec :: struct {
	name:            string,
	kind:            Builtin_Symbol_Kind,
	structure_name:  string,
	type_name:       string,
	type_field_name: string,
	value:           Builtin_Value_Spec,
}

Builtin_Type_Metadata :: struct {
	type_name: string,
	is_ref:    bool,
	is_table:  bool,
}

Builtin_Value_Spec_Kind :: enum {
	None,
	Integer,
	Text,
}

Builtin_Value_Spec :: struct {
	kind:    Builtin_Value_Spec_Kind,
	integer: i64,
	text:    string,
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

BUILTIN_GENERIC_TYPES :: []string {
	"xsequence",
	"data",
	"any",
	"any table",
	"simple",
	"decfloat",
	"numeric",
	"clike",
	"csequence",
	"object",
}

BUILTIN_STRUCTURE_SPECS :: []Builtin_Structure_Spec {
	{
		name = "syst",
		fields = []Builtin_Field_Spec {
			{name = "abcde", type_name = "c", docs = "Latin alphabet helper text that can be indexed directly by offset and length."},
			{name = "batch", type_name = "c", docs = "Set to 'X' in background processing and initial in dialog processing."},
			{name = "binpt", type_name = "c", docs = "Set to 'X' while batch input is being processed."},
			{name = "cprog", type_name = "c", docs = "Calling program for external procedures, otherwise the current program."},
			{name = "datum", type_name = "d", docs = "Current system date."},
			{name = "datlo", type_name = "d", docs = "Local date of the current user."},
			{name = "dbcnt", type_name = "i", docs = "Number of database rows processed by the last SQL statement that documents it."},
			{name = "dynnr", type_name = "c", docs = "Current dynpro number."},
			{name = "fdpos", type_name = "i", docs = "Found offset after supported search and comparison operations such as FIND."},
			{name = "host", type_name = "c", docs = "Host name of the current application server instance."},
			{name = "index", type_name = "i", docs = "Loop counter inside DO and WHILE loops; nested loops use the innermost counter."},
			{name = "langu", type_name = "c", docs = "Single-character locale language key for the current internal session."},
			{name = "mandt", type_name = "c", docs = "Client ID of the current user."},
			{name = "msgid", type_name = "c", docs = "Message class captured by the last MESSAGE statement."},
			{name = "msgno", type_name = "n", docs = "Message number captured by the last MESSAGE statement."},
			{name = "msgty", type_name = "c", docs = "Message type captured by the last MESSAGE statement."},
			{name = "msgv1", type_name = "c", docs = "First MESSAGE placeholder value captured by the last MESSAGE statement."},
			{name = "msgv2", type_name = "c", docs = "Second MESSAGE placeholder value captured by the last MESSAGE statement."},
			{name = "msgv3", type_name = "c", docs = "Third MESSAGE placeholder value captured by the last MESSAGE statement."},
			{name = "msgv4", type_name = "c", docs = "Fourth MESSAGE placeholder value captured by the last MESSAGE statement."},
			{name = "pfkey", type_name = "c", docs = "Current GUI status."},
			{name = "repid", type_name = "c", docs = "Program name exposed through sy-repid and syst-repid."},
			{name = "saprl", type_name = "c", docs = "ABAP release identifier of the current system."},
			{name = "scols", type_name = "i", docs = "Number of columns on the screen."},
			{name = "srows", type_name = "i", docs = "Number of screen rows."},
			{name = "subrc", type_name = "i", docs = "Return code set by many ABAP statements; 0 usually indicates success for the documented statement."},
			{name = "sysid", type_name = "c", docs = "SAP system ID."},
			{name = "tabix", type_name = "i", docs = "Current internal-table index from READ TABLE or LOOP AT on indexed access paths."},
			{name = "tcode", type_name = "c", docs = "Current transaction code."},
			{name = "tfill", type_name = "i", docs = "Row count of the internal table accessed by DESCRIBE TABLE, LOOP AT, or READ TABLE."},
			{name = "timlo", type_name = "t", docs = "Current user time in the user's time zone."},
			{name = "tzone", type_name = "i", docs = "System time-zone offset from UTC in seconds."},
			{name = "zonlo", type_name = "c", docs = "Current user's time zone."},
			{name = "ucomm", type_name = "c", docs = "Function code that triggered the current PAI processing."},
			{name = "uname", type_name = "c", docs = "User name of the current session."},
			{name = "uzeit", type_name = "t", docs = "Current system time."},
		},
	},
	{
		name = "screen",
		fields = []Builtin_Field_Spec {
			{name = "name", type_name = "c", docs = "Name of the current dynpro field or screen element."},
			{name = "group1", type_name = "c", docs = "Modification group 1 of the current screen element."},
			{name = "group2", type_name = "c", docs = "Modification group 2 of the current screen element."},
			{name = "group3", type_name = "c", docs = "Modification group 3 of the current screen element."},
			{name = "group4", type_name = "c", docs = "Modification group 4 of the current screen element."},
			{name = "required", type_name = "c", docs = "Whether the field is mandatory on the current dynpro."},
			{name = "input", type_name = "c", docs = "Whether the field is ready for input on the current dynpro."},
			{name = "output", type_name = "c", docs = "Whether the field is output-only on the current dynpro."},
			{name = "intensified", type_name = "c", docs = "Whether the field is highlighted on the current dynpro."},
			{name = "invisible", type_name = "c", docs = "Whether the field is hidden on the current dynpro."},
			{name = "length", type_name = "x", docs = "Visible field length of the current dynpro element."},
			{name = "active", type_name = "c", docs = "Combined active flag for the current dynpro element."},
			{name = "display_3d", type_name = "c", docs = "Whether the current dynpro box is shown three-dimensionally."},
			{name = "value_help", type_name = "c", docs = "Whether input help is shown for the current dynpro field."},
			{name = "request", type_name = "c", docs = "Whether input exists, or is simulated, for the current dynpro field."},
			{name = "values_in_combo", type_name = "c", docs = "Whether values exist in the current dynpro dropdown list box."},
		},
	},
	{
		name = "match_result",
		fields = []Builtin_Field_Spec {
			{name = "offset", type_name = "i", docs = "Zero-based offset of the match in the searched data object."},
			{name = "length", type_name = "i", docs = "Length of the matched segment."},
			{name = "submatches", type_name = "match_result_tab", structure_name = "match_result", docs = "Nested table containing captured submatches for a regex result."},
			{name = "line", type_name = "i", docs = "Line number of the match for searches in internal tables."},
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
			{name = "eventid", type_name = "i", docs = "Control Framework event identifier."},
			{name = "appl_event", type_name = "abap_bool", docs = "Whether the event is raised as an application event."},
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

BUILTIN_SYMBOL_SPECS :: []Builtin_Symbol_Spec {
	{name = "abap_bool", kind = .Type},
	{name = "flag", kind = .Type},
	{name = "xfeld", kind = .Type},
	{name = "sy", kind = .Type, structure_name = "syst"},
	{name = "syst", kind = .Type, structure_name = "syst"},
	{name = "screen", kind = .Type, structure_name = "screen"},
	{name = "syst", kind = .Variable, structure_name = "syst"},
	{name = "sy", kind = .Variable, structure_name = "syst"},
	{name = "screen", kind = .Variable, structure_name = "screen"},
	{name = "guid", kind = .Type},
	{name = "symsgv", kind = .Type},
	{name = "sydatum", kind = .Type},
	{name = "timestamp", kind = .Type},
	{name = "cursor", kind = .Type},
	{name = "match_result", kind = .Type, structure_name = "match_result"},
	{name = "match_result_tab", kind = .Type, structure_name = "match_result"},
	{name = "textpool", kind = .Type, structure_name = "textpool"},
	{name = "textpool_table", kind = .Type, structure_name = "textpool"},
	{name = "syst_short", kind = .Type},
	{name = "syst_byte", kind = .Type},
	{name = "syst_long", kind = .Type},
	{name = "tabname", kind = .Type},
	{name = "progname", kind = .Type},
	{name = "include", kind = .Type},
	{name = "synt_errors", kind = .Type},
	{name = "synt_comment", kind = .Type},
	{name = "synt_map", kind = .Type},
	{name = "synt_it_trmsg_raw", kind = .Type},
	{name = "synt_includes", kind = .Type},
	{name = "synt_ext_check", kind = .Type},
	{name = "synt_interval", kind = .Type},
	{name = "synt_crossref", kind = .Type},
	{name = "synt_type_obj", kind = .Type},
	{name = "synt_type_childs", kind = .Type},
	{name = "synt_data_obj", kind = .Type},
	{name = "synt_dpar", kind = .Type},
	{name = "synt_env", kind = .Type},
	{name = "synt_comp_obj", kind = .Type},
	{name = "synt_xcross", kind = .Type},
	{name = "synt_xcross_level", kind = .Type},
	{name = "synt_xcross_stmnt", kind = .Type},
	{name = "synt_ext_obj_use", kind = .Type},
	{name = "synum01", kind = .Type},
	{name = "sychar68k", kind = .Type},
	{name = "abap_classname", kind = .Type},
	{name = "abap_compname", kind = .Type},
	{name = "abap_typename", kind = .Type},
	{name = "abap_keyname", kind = .Type},
	{name = "abap_keycompname", kind = .Type},
	{name = "abap_intfname", kind = .Type},
	{name = "abap_attrname", kind = .Type},
	{name = "abap_evntname", kind = .Type},
	{name = "abap_parmname", kind = .Type},
	{name = "abap_excpname", kind = .Type},
	{name = "abap_func_parmbind_tab", kind = .Type, structure_name = "abap_func_parmbind"},
	{name = "abap_func_excpbind_tab", kind = .Type, structure_name = "abap_func_excpbind"},
	{name = "abap_func_parmbind", kind = .Type, structure_name = "abap_func_parmbind"},
	{name = "abap_func_excpbind", kind = .Type, structure_name = "abap_func_excpbind"},
	{name = "abap_abstypename", kind = .Type},
	{name = "abap_typecategory", kind = .Type},
	{name = "abap_typekind", kind = .Type},
	{name = "abap_typepropkind", kind = .Type},
	{name = "abap_structkind", kind = .Type},
	{name = "abapsource", kind = .Type},
	{name = "abap_encoding", kind = .Type},
	{name = "abap_editmask", kind = .Type},
	{name = "abap_helpid", kind = .Type},
	{name = "abap_classkind", kind = .Type},
	{name = "abap_visibility", kind = .Type},
	{name = "abap_frndtypes_tab", kind = .Type},
	{name = "abap_tablekind", kind = .Type},
	{name = "abap_keydefkind", kind = .Type},
	{name = "abap_methname", kind = .Type},
	{name = "abap_endian", kind = .Type},
	{name = "abap_parmkind", kind = .Type},
	{name = "abap_intfkind", kind = .Type},
	{name = "abap_char1", kind = .Type},
	{name = "abap_cr_lf", kind = .Type},
	{name = "abap_byte_order_mark", kind = .Type},
	{name = "abap_byte_order_utf8", kind = .Type},
	{name = "abap_trans_parmname", kind = .Type},
	{name = "abap_trans_parmvalue", kind = .Type},
	{name = "abap_trans_parmref", kind = .Type},
	{name = "abap_trans_parmbind", kind = .Type, structure_name = "abap_trans_parmbind"},
	{name = "abap_trans_parm_obj_bind", kind = .Type, structure_name = "abap_trans_parm_obj_bind"},
	{name = "abap_trans_parmbind_tab", kind = .Type, structure_name = "abap_trans_parmbind"},
	{name = "abap_trans_parm_obj_bind_tab", kind = .Type, structure_name = "abap_trans_parm_obj_bind"},
	{name = "abap_trans_objname", kind = .Type},
	{name = "abap_trans_objbind", kind = .Type, structure_name = "abap_trans_objbind"},
	{name = "abap_trans_objbind_tab", kind = .Type, structure_name = "abap_trans_objbind"},
	{name = "abap_trans_srcname", kind = .Type},
	{name = "abap_trans_srcbind", kind = .Type, structure_name = "abap_trans_srcbind"},
	{name = "abap_trans_srcbind_tab_sorted", kind = .Type, structure_name = "abap_trans_srcbind"},
	{name = "abap_trans_resname", kind = .Type},
	{name = "abap_trans_resbind", kind = .Type, structure_name = "abap_trans_resbind"},
	{name = "abap_trans_resbind_tab_sorted", kind = .Type, structure_name = "abap_trans_resbind"},
	{name = "%_charsize", kind = .Constant, type_name = "i"},
	{name = "%_endian", kind = .Constant, type_name = "abap_endian"},
	{name = "%_minchar", kind = .Constant, type_name = "abap_char1"},
	{name = "%_maxchar", kind = .Constant, type_name = "abap_char1"},
	{name = "%_horizontal_tab", kind = .Constant, type_name = "abap_char1"},
	{name = "%_vertical_tab", kind = .Constant, type_name = "abap_char1"},
	{name = "%_newline", kind = .Constant, type_name = "abap_char1"},
	{name = "%_cr_lf", kind = .Constant, type_name = "abap_cr_lf"},
	{name = "%_formfeed", kind = .Constant, type_name = "abap_char1"},
	{name = "%_backspace", kind = .Constant, type_name = "abap_char1"},
	{name = "abap_true", kind = .Constant, type_name = "abap_bool", value = {kind = .Text, text = "X"}},
	{name = "abap_false", kind = .Constant, type_name = "abap_bool", value = {kind = .Text, text = " "}},
	{name = "abap_undefined", kind = .Constant, type_name = "abap_bool", value = {kind = .Text, text = "-"}},
	{name = "abap_on", kind = .Constant, type_name = "abap_bool", value = {kind = .Text, text = "X"}},
	{name = "abap_off", kind = .Constant, type_name = "abap_bool", value = {kind = .Text, text = " "}},
	{name = "abap_max_abs_type_name_ln", kind = .Constant, type_name = "i", value = {kind = .Integer, integer = 200}},
	{name = "abap_max_class_name_ln", kind = .Constant, type_name = "i", value = {kind = .Integer, integer = 30}},
	{name = "abap_max_intf_name_ln", kind = .Constant, type_name = "i", value = {kind = .Integer, integer = 30}},
	{name = "abap_max_comp_name_ln", kind = .Constant, type_name = "i", value = {kind = .Integer, integer = 30}},
	{name = "abap_max_key_name_ln", kind = .Constant, type_name = "i", value = {kind = .Integer, integer = 255}},
	{name = "abap_max_class_comp_name_ln", kind = .Constant, type_name = "i", value = {kind = .Integer, integer = 61}},
	{name = "abap_max_edit_mask_ln", kind = .Constant, type_name = "i", value = {kind = .Integer, integer = 7}},
	{name = "abap_max_help_id_ln", kind = .Constant, type_name = "i", value = {kind = .Integer, integer = 62}},
	{name = "abap_max_db_string_ln", kind = .Constant, type_name = "i", value = {kind = .Integer, integer = 536870912}},
	{name = "abap_max_db_rawstring_ln", kind = .Constant, type_name = "i", value = {kind = .Integer, integer = 1073741824}},
	{name = "abap_func_exporting", kind = .Constant, type_name = "abap_func_parmbind", type_field_name = "kind", value = {kind = .Integer, integer = 10}},
	{name = "abap_func_importing", kind = .Constant, type_name = "abap_func_parmbind", type_field_name = "kind", value = {kind = .Integer, integer = 20}},
	{name = "abap_func_tables", kind = .Constant, type_name = "abap_func_parmbind", type_field_name = "kind", value = {kind = .Integer, integer = 30}},
	{name = "abap_func_changing", kind = .Constant, type_name = "abap_func_parmbind", type_field_name = "kind", value = {kind = .Integer, integer = 40}},
	{name = "space", kind = .Constant, type_name = "c", value = {kind = .Text, text = " "}},
	{name = "text", kind = .Variable},
	{name = "cntl_simple_event", kind = .Type, structure_name = "cntl_simple_event"},
	{name = "cntl_simple_events", kind = .Type, structure_name = "cntl_simple_event"},
}

checker_register_builtins :: proc(checker: ^Checker) {
	assert(checker != nil && checker.info.builtin_scope != nil)
	ctx := checker.builtin_context
	ctx.scope = checker.info.builtin_scope

	for name in BUILTIN_PRIMITIVE_TYPES {
		entity := checker_register_builtin_entity(&ctx, name, .Type_Def)
		entity.type = project_type_builtin(ctx.project, entity.name, entity)
	}
	for name in BUILTIN_GENERIC_TYPES {
		entity := checker_register_builtin_entity(&ctx, name, .Type_Def)
		entity.type = project_type_builtin(ctx.project, entity.name, entity)
	}
	for &spec in BUILTIN_SYMBOL_SPECS {
		if spec.kind == .Type {
			checker_register_builtin_symbol(&ctx, spec)
		}
	}
	for &structure in BUILTIN_STRUCTURE_SPECS {
		checker_register_builtin_structure(&ctx, structure)
	}
	for &spec in BUILTIN_SYMBOL_SPECS {
		if spec.kind == .Type {
			checker_register_builtin_symbol(&ctx, spec)
		}
	}
	for &spec in BUILTIN_SYMBOL_SPECS {
		if spec.kind != .Type {
			checker_register_builtin_symbol(&ctx, spec)
		}
	}
	for &metadata in BUILTIN_PROCS {
		checker_register_builtin_proc(&ctx, metadata)
	}
}

checker_register_builtin_symbol :: proc(ctx: ^Checker_Context, spec: Builtin_Symbol_Spec) -> ^Entity {
	kind := Entity_Kind.Type_Def
	switch spec.kind {
	case .Type:
		kind = .Type_Def
	case .Constant:
		kind = .Constant
	case .Variable:
		kind = .Variable
	}
	entity := checker_register_builtin_entity(ctx, spec.name, kind)
	type_name := spec.type_name
	type_is_ref := false
	type_is_table := false
	if spec.kind == .Type && type_name == "" {
		if metadata, ok := checker_builtin_type_metadata(spec.name); ok {
			type_name = metadata.type_name
			type_is_ref = metadata.is_ref
			type_is_table = metadata.is_table
		}
	}
	if payload, ok := entity.payload.(^Entity_Constant_Payload); ok && payload != nil {
		payload.constant_value = checker_constant_value_from_builtin_spec(ctx, spec.value)
	}
	structure := checker_builtin_structure_by_name(ctx.checker, spec.structure_name)
	checker_set_builtin_entity_type(ctx, entity, type_name, spec.type_field_name, type_is_ref, type_is_table, structure)
	return entity
}

checker_register_builtin_proc :: proc(ctx: ^Checker_Context, metadata: Builtin_Proc_Metadata) -> ^Entity {
	entity := checker_register_builtin_entity(ctx, metadata.name, .Builtin)
	if payload, ok := entity.payload.(^Entity_Builtin_Payload); ok && payload != nil {
		payload.id = i32(metadata.id)
		payload.docs = strings.clone(metadata.docs, ctx.project.allocator) if metadata.docs != "" else ""
		payload.supports_named_args = metadata.supports_named_args
	}
	entity.type = project_type_routine(ctx.project)
	entity.type.routine.results = make([dynamic]^Entity, 0, 1, ctx.project.allocator)
	if metadata.return_type != "" {
		entity.type.base = checker_builtin_type_from_name(ctx.checker, metadata.return_type)
	}
	return entity
}

checker_constant_value_from_builtin_spec :: proc(
	ctx: ^Checker_Context,
	spec: Builtin_Value_Spec,
) -> Constant_Value {
	switch spec.kind {
	case .Integer:
		value := new(Constant_Integer_Value, ctx.project.allocator)
		value.value = spec.integer
		return value
	case .Text:
		value := new(Constant_Text_Value, ctx.project.allocator)
		value.value = strings.clone(spec.text, ctx.project.allocator) if spec.text != "" else ""
		return value
	case .None:
		return nil
	}
	return nil
}

checker_register_builtin_entity :: proc(
	ctx: ^Checker_Context,
	name: string,
	kind: Entity_Kind,
) -> ^Entity {
	assert(ctx != nil && ctx.scope != nil && kind != .Invalid)
	namespace := checker_builtin_namespace_for_kind(kind)
	interned := checker_intern_name(ctx.project, name)
	if existing, ok := scope_lookup_declaration(ctx.scope, namespace, interned); ok {
		existing.flags += {.Builtin}
		return existing
	}
	entity := project_new_entity(ctx.project, kind)
	entity.flags += {.Builtin}
	decl := project_new_decl_info(ctx.project, entity, ctx.scope, interned, kind)
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	checker_mark_builtin_entity_resolved(ctx, entity)
	return entity
}

checker_mark_builtin_entity_resolved :: proc(ctx: ^Checker_Context, entity: ^Entity) {
	assert(ctx != nil && entity != nil && entity.decl_info != nil)
	entity.state = .Resolved
	entity.decl_info.state = .Resolved
	for existing in ctx.info.checked_entities {
		if existing == entity {
			return
		}
	}
	append(&ctx.info.checked_entities, entity)
}

checker_register_builtin_structure :: proc(ctx: ^Checker_Context, spec: Builtin_Structure_Spec) -> ^Structure {
	owner := checker_register_builtin_entity(ctx, spec.name, .Type_Def)
	if payload, ok := owner.payload.(^Entity_Type_Name_Payload); ok && payload != nil && payload.structure != nil {
		return payload.structure
	}

	structure_scope := checker_create_scope(ctx.checker, ctx.info.builtin_scope, .Structure, owner = owner, decl_info = owner.decl_info)
	structure := project_new_structure(ctx.project, owner.name, nil, structure_scope)
	structure_type := project_type_structure(ctx.project, structure)
	if owner.type == nil || owner.type.kind == .Builtin {
		owner.type = project_type_named(ctx.project, owner.name, owner, structure_type)
	}
	if payload, ok := owner.payload.(^Entity_Type_Name_Payload); ok && payload != nil {
		payload.structure = structure
	}

	for &field in spec.fields {
		checker_register_builtin_structure_field(ctx, structure, owner, field)
	}
	return structure
}

checker_register_builtin_structure_field :: proc(
	ctx: ^Checker_Context,
	structure: ^Structure,
	owner: ^Entity,
	spec: Builtin_Field_Spec,
) -> ^Entity {
	assert(structure != nil && structure.scope != nil && owner != nil)
	interned := checker_intern_name(ctx.project, spec.name)
	if existing, ok := scope_lookup_declaration(structure.scope, .Value, interned); ok {
		return existing
	}

	entity := project_new_entity(ctx.project, .Field)
	entity.flags += {.Builtin}
	entity.owner = owner
	decl := project_new_decl_info(ctx.project, entity, structure.scope, interned, .Field)
	payload, payload_ok := entity.payload.(^Entity_Field_Payload)
	assert(payload_ok && payload != nil)
	payload.owner_structure = structure
	payload.field_index = i32(len(structure.fields))
	payload.type_clause_form = .Type
	payload.has_type_clause_form = spec.type_name != ""
	if spec.type_name != "" {
		payload.flags += {.Has_Type_Ref}
		payload.type_ref.namespace = .Type
		payload.type_ref.base_name = checker_intern_name(ctx.project, spec.type_name)
		payload.type_ref.is_ref = spec.is_ref
	}
	entity.type = checker_builtin_type_from_name(ctx.checker, spec.type_name, spec.is_ref)
	if previous := scope_insert_declaration(structure.scope, entity); previous != nil {
		return previous
	}
	entity.decl_info = decl
	entity.state = .Resolved
	decl.state = .Resolved
	append(&structure.fields, entity)
	checker_add_definition(ctx.info, entity)
	return entity
}

checker_set_builtin_entity_type :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	type_name: string,
	type_field_name: string,
	is_ref: bool,
	is_table: bool,
	structure: ^Structure,
) {
	assert(entity != nil)
	if payload, ok := entity.payload.(^Entity_Type_Name_Payload); ok && payload != nil && structure != nil {
		payload.structure = structure
	}
	#partial switch entity.kind {
	case .Type_Def:
		if type_name != "" {
			base := checker_builtin_type_from_symbol(ctx.checker, type_name, type_field_name, is_ref, is_table)
			entity.type = project_type_named(ctx.project, entity.name, entity, base)
		} else if structure != nil {
			entity.type = project_type_named(ctx.project, entity.name, entity, project_type_structure(ctx.project, structure))
		} else if entity.type == nil {
			entity.type = project_type_builtin(ctx.project, entity.name, entity)
		}
	case .Constant, .Variable:
		if type_name != "" {
			entity.type = checker_builtin_type_from_symbol(ctx.checker, type_name, type_field_name, is_ref, is_table)
		} else if structure != nil {
			entity.type = project_type_named(ctx.project, entity.name, entity, project_type_structure(ctx.project, structure))
		} else if entity.type == nil {
			entity.type = project_type_unknown(ctx.project)
		}
	}
}

checker_check_builtin_call :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	entity: ^Entity,
	call: ^ast.Call_Expr,
	lhs: bool,
) -> Operand {
	_ = call
	payload, ok := entity.payload.(^Entity_Builtin_Payload)
	assert(ok && payload != nil)
	id := Builtin_Proc_Id(payload.id)
	switch id {
	case .Boolc,
	     .Line_Exists,
	     .Abs,
	     .Sign,
	     .Ceil,
	     .Floor,
	     .Trunc,
	     .Frac,
	     .Ipow,
	     .Nmax,
	     .Nmin,
	     .Acos,
	     .Asin,
	     .Atan,
	     .Cos,
	     .Sin,
	     .Tan,
	     .Cosh,
	     .Sinh,
	     .Tanh,
	     .Exp,
	     .Log,
	     .Log10,
	     .Sqrt,
	     .Charlen,
	     .Dbmaxlen,
	     .Numofchar,
	     .Strlen,
	     .Substring,
	     .Substring_Before,
	     .Substring_After,
	     .Shift_Left,
	     .Condense,
	     .Replace,
	     .Matches,
	     .Find,
	     .Repeat,
	     .Escape,
	     .Reverse,
	     .Round,
	     .Rescale,
	     .To_Lower,
	     .To_Upper,
	     .To_Mixed,
	     .From_Mixed,
	     .Xstrlen,
	     .Lines,
	     .Concat_Lines_Of:
		if metadata, metadata_ok := checker_builtin_proc_metadata(id); metadata_ok {
			return checker_record_operand(ctx, node, .Value, checker_builtin_type_from_name(ctx.checker, metadata.return_type), lhs = lhs)
		}
	case .Invalid:
	}
	return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
}

checker_builtin_type_from_name :: proc(
	checker: ^Checker,
	name: string,
	is_ref := false,
	is_table := false,
) -> ^Type {
	if name == "" {
		return project_type_unknown(checker.project)
	}
	if entity, ok := checker_lookup_builtin_entity(checker, .Type, name); ok && entity.type != nil {
		base := entity.type
		if is_ref {
			base = project_type_ref(checker.project, base)
		}
		if is_table {
			base = project_type_table(checker.project, base, .Standard_Table)
		}
		return base
	}
	return project_type_unknown(checker.project)
}

checker_builtin_type_from_symbol :: proc(
	checker: ^Checker,
	type_name: string,
	type_field_name: string,
	is_ref := false,
	is_table := false,
) -> ^Type {
	base: ^Type
	if type_field_name != "" {
		if structure := checker_builtin_structure_by_name(checker, type_name); structure != nil {
			field_name := checker_intern_name(checker.project, type_field_name)
			if field, ok := scope_lookup_declaration(structure.scope, .Value, field_name); ok {
				base = field.type
			}
		}
	} else {
		base = checker_builtin_type_from_name(checker, type_name)
	}
	if base == nil {
		base = project_type_unknown(checker.project)
	}
	if is_ref {
		base = project_type_ref(checker.project, base)
	}
	if is_table {
		base = project_type_table(checker.project, base, .Standard_Table)
	}
	return base
}

checker_lookup_builtin_entity :: proc(
	checker: ^Checker,
	namespace: Namespace,
	name: string,
) -> (^Entity, bool) {
	assert(checker != nil && checker.info.builtin_scope != nil)
	interned := checker_intern_name(checker.project, name)
	if !string_interner.is_valid(interned) {
		return nil, false
	}
	return scope_lookup_declaration(checker.info.builtin_scope, namespace, interned)
}

checker_builtin_structure_by_name :: proc(checker: ^Checker, name: string) -> ^Structure {
	if name == "" {
		return nil
	}
	entity, ok := checker_lookup_builtin_entity(checker, .Type, name)
	if !ok {
		return nil
	}
	if payload, payload_ok := entity.payload.(^Entity_Type_Name_Payload); payload_ok && payload != nil {
		return payload.structure
	}
	return nil
}

checker_builtin_structure_field_description :: proc(structure_name, field_name: string) -> string {
	for &structure in BUILTIN_STRUCTURE_SPECS {
		if !strings.equal_fold(structure.name, structure_name) {
			continue
		}
		for &field in structure.fields {
			if strings.equal_fold(field.name, field_name) {
				return field.docs
			}
		}
	}
	return ""
}

checker_builtin_namespace_for_kind :: proc(kind: Entity_Kind) -> Namespace {
	#partial switch kind {
	case .Type_Def:
		return .Type
	case .Builtin:
		return .Routine
	case .Constant, .Variable, .Field:
		return .Value
	case:
		unreachable()
	}
	return .Value
}

checker_builtin_type_metadata :: proc(name: string) -> (Builtin_Type_Metadata, bool) {
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
		return {type_name = "c"}, true
	case "abap_cr_lf":
		return {type_name = "c"}, true
	case "abap_byte_order_mark":
		return {type_name = "x"}, true
	case "abap_byte_order_utf8":
		return {type_name = "x"}, true
	case "progname", "include":
		return {type_name = "c"}, true
	case "abap_editmask":
		return {type_name = "c"}, true
	case "abap_helpid":
		return {type_name = "c"}, true
	case "abap_typename", "abap_attrname", "abap_methname", "abap_evntname":
		return {type_name = "c"}, true
	case "abap_abstypename":
		return {type_name = "c"}, true
	case "abap_compname", "abap_parmname", "abap_excpname":
		return {type_name = "c"}, true
	case "abap_keyname":
		return {type_name = "c"}, true
	case "abap_keycompname":
		return {type_name = "abap_keyname"}, true
	case "abap_classname":
		return {type_name = "c"}, true
	case "abap_intfname":
		return {type_name = "c"}, true
	case "textpool_table":
		return {type_name = "textpool", is_table = true}, true
	case "abap_func_parmbind_tab":
		return {type_name = "abap_func_parmbind", is_table = true}, true
	case "abap_func_excpbind_tab":
		return {type_name = "abap_func_excpbind", is_table = true}, true
	case "abap_encoding":
		return {type_name = "abap_encod"}, true
	case "abap_endian":
		return {type_name = "abap_endia"}, true
	case "abap_trans_parmname",
	     "abap_trans_parmvalue",
	     "abap_trans_objname",
	     "abap_trans_srcname",
	     "abap_trans_resname":
		return {type_name = "string"}, true
	case "abap_trans_parmref":
		return {type_name = "data", is_ref = true}, true
	case "abap_trans_parmbind_tab":
		return {type_name = "abap_trans_parmbind", is_table = true}, true
	case "abap_trans_parm_obj_bind_tab":
		return {type_name = "abap_trans_parm_obj_bind", is_table = true}, true
	case "abap_trans_objbind_tab":
		return {type_name = "abap_trans_objbind", is_table = true}, true
	case "abap_trans_srcbind_tab_sorted":
		return {type_name = "abap_trans_srcbind", is_table = true}, true
	case "abap_trans_resbind_tab_sorted":
		return {type_name = "abap_trans_resbind", is_table = true}, true
	}
	return {}, false
}
