package abap_frontend_semantic

import "../adt"
import dep_store "../dependency_store"
import "../parser"
import "../tokenizer"
import frontend_runtime "../runtime"

import "core:os"
import filepath "core:path/filepath"
import "core:testing"

@(test)
symbol_kind_namespace_occupancy :: proc(t: ^testing.T) {
	testing.expect(t, symbol_kind_occupies(.Builtin_Type, .Type))
	testing.expect(t, !symbol_kind_occupies(.Builtin_Type, .Value))
	testing.expect(t, symbol_kind_occupies(.Builtin_Routine, .Routine))
	testing.expect(t, !symbol_kind_occupies(.Builtin_Routine, .Type))
	testing.expect(t, symbol_kind_occupies(.Variable, .Value))
	testing.expect(t, symbol_kind_occupies(.Report, .Value))
	testing.expect(t, symbol_kind_occupies(.Method, .Routine))
}

@(test)
creates_root_file_scope_and_builtins :: proc(t: ^testing.T) {
	unit := unit_analysis_make(
		Unit_Id(0),
		"mem://main.prog.abap",
		tokenizer.text_range(0, 10),
		context.allocator,
	)

	root := scope(&unit, unit.root_scope)
	testing.expect(t, root != nil)
	testing.expect_value(t, root.kind, Scope_Kind.File)
	testing.expect_value(t, root.range, tokenizer.text_range(0, 10))

	testing.expect(t, find_symbol(&unit, "i", .Builtin_Type) != nil)
	testing.expect(t, find_symbol(&unit, "abap_bool", .Builtin_Type) != nil)
	testing.expect(t, find_symbol(&unit, "abap_true", .Builtin_Constant) != nil)
	testing.expect(t, find_symbol(&unit, "sy", .Builtin_Variable) != nil)
	testing.expect(t, find_symbol(&unit, "syst", .Builtin_Type) != nil)
	testing.expect(t, builtin_routine_spec("strlen") != nil)
	testing.expect(t, find_symbol(&unit, "strlen", .Builtin_Routine) != nil)
}

@(test)
structure_field_lookup_for_syst_and_screen :: proc(t: ^testing.T) {
	unit := unit_analysis_make(
		Unit_Id(0),
		"mem://main.prog.abap",
		tokenizer.text_range(0, 0),
		context.allocator,
	)

	syst := find_structure(&unit, "syst")
	screen := find_structure(&unit, "screen")
	testing.expect(t, syst != nil)
	testing.expect(t, screen != nil)

	subrc, ok := structure_field_info(&unit, syst.id, "subrc")
	testing.expect(t, ok)
	testing.expect_value(t, subrc.name, "subrc")
	testing.expect(t, .Has_Type_Ref in subrc.flags)
	testing.expect_value(t, subrc.type_ref.base_name, "i")

	screen_name, ok2 := structure_field_info(&unit, screen.id, "name")
	testing.expect(t, ok2)
	testing.expect_value(t, screen_name.name, "name")
	testing.expect(t, .Has_Type_Ref in screen_name.flags)
	testing.expect_value(t, screen_name.type_ref.base_name, "c")
}

collect_test_unit :: proc(t: ^testing.T, uri, source: string) -> Unit_Analysis {
	parsed := parser.parse(source, uri, context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	return analyze_unit(Unit_Id(0), uri, source, parsed, context.allocator)
}

has_symbol :: proc(unit: ^Unit_Analysis, kind: Symbol_Kind, name: string) -> bool {
	for symbol in unit.symbols {
		if symbol.kind == kind && symbol.name == name {
			return true
		}
	}
	return false
}

has_scope_kind :: proc(unit: ^Unit_Analysis, kind: Scope_Kind) -> bool {
	for scope in unit.scopes {
		if scope.kind == kind {
			return true
		}
	}
	return false
}

has_diagnostic :: proc(unit: ^Unit_Analysis, kind: Diagnostic_Kind) -> bool {
	for diagnostic in unit.diagnostics {
		if diagnostic.kind == kind {
			return true
		}
	}
	return false
}

project_has_diagnostic :: proc(project: ^Project_Analysis, kind: Diagnostic_Kind) -> bool {
	for diagnostic in project.diagnostics {
		if diagnostic.kind == kind {
			return true
		}
	}
	return false
}

project_units_have_diagnostic :: proc(project: ^Project_Analysis, kind: Diagnostic_Kind) -> bool {
	for &unit in project.units {
		if has_diagnostic(&unit, kind) {
			return true
		}
	}
	return false
}

analyze_project_test :: proc(
	t: ^testing.T,
	worker_count: int,
	target: Source_Input,
	candidates: []Source_Input,
) -> Project_Analysis {
	pool: frontend_runtime.Pool
	options := frontend_runtime.Options {
		worker_count = worker_count,
		task_capacity = 128,
		queue_capacity = 32,
		deque_capacity = 32,
	}
	testing.expect_value(t, frontend_runtime.pool_init(&pool, options, context.allocator), frontend_runtime.Submit_Error.None)
	if pool.options.worker_count > 0 {
		testing.expect_value(t, frontend_runtime.pool_start(&pool), frontend_runtime.Submit_Error.None)
	}
	project := analyze_target(target, candidates, Analyze_Options{pool = &pool}, context.allocator)
	if pool.options.worker_count > 0 {
		frontend_runtime.pool_join(&pool)
	}
	frontend_runtime.pool_destroy(&pool)
	return project
}

analyze_path_test :: proc(t: ^testing.T, target_path: string) -> Manifest_Analysis_Result {
	return analyze_path_test_with_options(t, target_path, {})
}

analyze_path_test_with_options :: proc(
	t: ^testing.T,
	target_path: string,
	options: Analyze_Options,
) -> Manifest_Analysis_Result {
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	run_options := options
	run_options.pool = &pool
	result := analyze_path(target_path, nil, run_options, context.allocator)
	frontend_runtime.pool_destroy(&pool)
	return result
}

manifest_workspace_path :: proc(name: string) -> string {
	package_dir := filepath.dir(#file)
	root, _ := filepath.join(
		{package_dir, "..", "..", "bin", "test-data", "manifest", name},
		context.allocator,
	)
	os.remove_all(root)
	os.make_directory_all(root)
	return root
}

external_export_workspace_path :: proc(name: string) -> string {
	package_dir := filepath.dir(#file)
	root, _ := filepath.join(
		{package_dir, "..", "..", "bin", "test-data", "local-export", name},
		context.allocator,
	)
	os.remove_all(root)
	os.make_directory_all(root)
	return root
}

manifest_test_file :: proc(t: ^testing.T, root, relative, source: string) -> string {
	path, _ := filepath.join({root, relative}, context.allocator)
	dir := filepath.dir(path)
	testing.expect(t, os.make_directory_all(dir) == nil)
	testing.expect(t, os.write_entire_file(path, source) == nil)
	cleaned, ok := absolute_clean_path(path, context.allocator)
	testing.expect(t, ok)
	return cleaned
}

analyze_units_project_test :: proc(t: ^testing.T, sources: []Source_Input) -> Project_Analysis {
	units := make([dynamic]Unit_Analysis, 0, len(sources), context.allocator)
	for source, i in sources {
		parsed := parser.parse(source.source, source.uri, context.allocator)
		testing.expect_value(t, len(parsed.errors), 0)
		unit := collect_unit(Unit_Id(u32(i)), source.uri, source.source, parsed, context.allocator)
		resolve_unit_locally(&unit, context.allocator)
		append(&units, unit)
	}
	project := project_analysis_from_units(units, context.allocator)
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	finish_project_analysis(&project, &pool, context.allocator)
	frontend_runtime.pool_destroy(&pool)
	return project
}

include_target_uri :: proc(project: ^Project_Analysis, unit: ^Unit_Analysis, name: string) -> string {
	for edge in unit.include_edges {
		if edge.name != name || !edge.has_target {
			continue
		}
		target_index := unit_id_index(edge.target)
		if target_index >= 0 && target_index < len(project.units) {
			return project.units[target_index].uri
		}
	}
	return ""
}

class_member_named :: proc(
	unit: ^Unit_Analysis,
	class_symbol: Symbol_Id,
	name: string,
	kind: Class_Member_Kind,
) -> ^Class_Member_Data {
	for &member in unit.class_members {
		if member.class_symbol == class_symbol && member.kind == kind && member.name == name {
			return &member
		}
	}
	return nil
}

reference_count :: proc(
	unit: ^Unit_Analysis,
	name: string,
	namespace: Namespace,
	kind: Reference_Kind,
) -> int {
	count := 0
	for reference in unit.references {
		if reference.name == name && reference.namespace == namespace && reference.kind == kind {
			count += 1
		}
	}
	return count
}

has_reference :: proc(
	unit: ^Unit_Analysis,
	name: string,
	namespace: Namespace,
	kind: Reference_Kind,
) -> bool {
	return reference_count(unit, name, namespace, kind) > 0
}

reference_resolves_to_uri :: proc(
	project: ^Project_Analysis,
	unit: ^Unit_Analysis,
	name: string,
	namespace: Namespace,
	kind: Reference_Kind,
	uri: string,
) -> bool {
	for reference in unit.references {
		if reference.name != name ||
		   reference.namespace != namespace ||
		   reference.kind != kind ||
		   !reference.has_resolution ||
		   reference.resolution.kind != .Symbol {
			continue
		}
		unit_index := unit_id_index(reference.resolution.symbol.unit)
		if unit_index >= 0 && unit_index < len(project.units) && project.units[unit_index].uri == uri {
			return true
		}
	}
	return false
}

has_named_argument :: proc(
	unit: ^Unit_Analysis,
	name: string,
	section: Named_Argument_Section,
	target_kind: Named_Argument_Target_Kind,
) -> bool {
	for arg in unit.named_arguments {
		if arg.name == name &&
		   arg.has_section &&
		   arg.section == section &&
		   arg.target.kind == target_kind {
			return true
		}
	}
	return false
}

field_names_match :: proc(structure: ^Structure_Data, names: []string) -> bool {
	if structure == nil || len(structure.fields) != len(names) {
		return false
	}
	for i in 0 ..< len(names) {
		if structure.fields[i].name != names[i] {
			return false
		}
	}
	return true
}

provided_name_present :: proc(unit: ^Unit_Analysis, name: string) -> bool {
	for provided in unit.provided_names {
		if provided == name {
			return true
		}
	}
	return false
}

sql_source_present :: proc(
	unit: ^Unit_Analysis,
	name: string,
	resolution: Sql_Resolution,
) -> bool {
	for source in unit.sql_sources {
		if source.name == name && source.resolution == resolution {
			return true
		}
	}
	return false
}

sql_source_alias_present :: proc(
	unit: ^Unit_Analysis,
	name, alias: string,
	kind: Sql_Source_Kind,
) -> bool {
	for source in unit.sql_sources {
		if source.name == name && source.alias == alias && source.source_kind == kind {
			return true
		}
	}
	return false
}

sql_projection_present :: proc(
	unit: ^Unit_Analysis,
	name: string,
	kind: Sql_Projection_Kind,
) -> bool {
	for projection in unit.sql_projections {
		if projection.name == name && projection.kind == kind {
			return true
		}
	}
	return false
}

sql_projection_alias_present :: proc(
	unit: ^Unit_Analysis,
	alias: string,
	kind: Sql_Projection_Kind,
) -> bool {
	for projection in unit.sql_projections {
		if projection.alias == alias && projection.kind == kind {
			return true
		}
	}
	return false
}

sql_name_ref_present :: proc(unit: ^Unit_Analysis, name: string, kind: Sql_Name_Ref_Kind) -> bool {
	for reference in unit.sql_name_refs {
		if reference.name == name && reference.kind == kind {
			return true
		}
	}
	return false
}

sql_qualified_ref_present :: proc(
	unit: ^Unit_Analysis,
	qualifier, name: string,
	kind: Sql_Name_Ref_Kind,
) -> bool {
	for reference in unit.sql_name_refs {
		if reference.qualifier == qualifier && reference.name == name && reference.kind == kind {
			return true
		}
	}
	return false
}

sql_predicate_present :: proc(unit: ^Unit_Analysis, kind: Sql_Predicate_Kind) -> bool {
	for predicate in unit.sql_predicates {
		if predicate.kind == kind {
			return true
		}
	}
	return false
}

sql_dynamic_present :: proc(unit: ^Unit_Analysis, kind: Sql_Dynamic_Fragment_Kind) -> bool {
	for fragment in unit.sql_dynamic_fragments {
		if fragment.kind == kind {
			return true
		}
	}
	return false
}

sql_target_present :: proc(
	unit: ^Unit_Analysis,
	name: string,
	kind: Sql_Target_Kind,
	flags: Sql_Target_Flags,
) -> bool {
	for target in unit.sql_targets {
		if target.target_name != name || target.kind != kind {
			continue
		}
		ok := true
		for flag in flags {
			if !(flag in target.flags) {
				ok = false
			}
		}
		if ok {
			return true
		}
	}
	return false
}

string_list_matches :: proc(values: [dynamic]string, expected: []string) -> bool {
	if len(values) != len(expected) {
		return false
	}
	for i in 0 ..< len(expected) {
		if values[i] != expected[i] {
			return false
		}
	}
	return true
}

internal_table_order_present :: proc(
	unit: ^Unit_Analysis,
	table_name: string,
	fields: []string,
) -> bool {
	for order in unit.internal_table_orders {
		if order.table_name == table_name && string_list_matches(order.key_fields, fields) {
			return true
		}
	}
	return false
}

binary_search_present :: proc(unit: ^Unit_Analysis, table_name: string, fields: []string) -> bool {
	for read in unit.read_table_binary_searches {
		if read.table_name == table_name && string_list_matches(read.key_fields, fields) {
			return true
		}
	}
	return false
}

system_update_present :: proc(
	unit: ^Unit_Analysis,
	statement: System_Field_Statement_Kind,
	field_name: string,
) -> bool {
	for update in unit.system_field_updates {
		if update.statement == statement && update.field_name == field_name {
			return true
		}
	}
	return false
}

find_text :: proc(source, needle: string) -> int {
	if needle == "" || len(needle) > len(source) {
		return -1
	}
	for i in 0 ..= len(source) - len(needle) {
		if source[i:i + len(needle)] == needle {
			return i
		}
	}
	return -1
}

find_text_last :: proc(source, needle: string) -> int {
	if needle == "" || len(needle) > len(source) {
		return -1
	}
	found := -1
	for i in 0 ..= len(source) - len(needle) {
		if source[i:i + len(needle)] == needle {
			found = i
		}
	}
	return found
}

@(test)
collects_top_level_and_nested_definitions :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lv_local TYPE i.
ENDFORM.

CLASS lcl_demo IMPLEMENTATION.
  METHOD execute.
    DATA lv_inner TYPE i.
  ENDMETHOD.
ENDCLASS.

DATA gv_value TYPE i.
TYPES ty_name TYPE string.
CONSTANTS gc_limit TYPE i VALUE 1.
FIELD-SYMBOLS <fs_row> TYPE any.
`
	unit := collect_test_unit(t, "file:///defs.abap", source)

	testing.expect(t, has_symbol(&unit, .Form, "run"))
	testing.expect(t, has_symbol(&unit, .Class, "lcl_demo"))
	testing.expect(t, has_symbol(&unit, .Method, "execute"))
	testing.expect(t, has_symbol(&unit, .Variable, "gv_value"))
	testing.expect(t, has_symbol(&unit, .Type_Def, "ty_name"))
	testing.expect(t, has_symbol(&unit, .Constant, "gc_limit"))
	testing.expect(t, has_symbol(&unit, .Field_Symbol, "<fs_row>"))
	testing.expect(t, has_scope_kind(&unit, .Form))
	testing.expect(t, has_scope_kind(&unit, .Method))
}

@(test)
collects_single_include_edge :: proc(t: ^testing.T) {
	unit := collect_test_unit(t, "file:///zmain.abap", "INCLUDE zinc.")

	testing.expect_value(t, len(unit.include_edges), 1)
	testing.expect_value(t, unit.include_edges[0].name, "zinc")
	testing.expect(t, !unit.include_edges[0].has_target)
	testing.expect(t, has_symbol(&unit, .Include, "zinc"))
	testing.expect_value(t, len(unit.references), 1)
	testing.expect_value(t, unit.references[0].kind, Reference_Kind.Include)
	testing.expect_value(t, unit.references[0].name, "zinc")
	testing.expect(t, unit.references[0].has_resolution)
}

@(test)
collects_chained_include_edges :: proc(t: ^testing.T) {
	unit := collect_test_unit(t, "file:///zmain.abap", "INCLUDE: ztop, zf01.")

	testing.expect_value(t, len(unit.include_edges), 2)
	testing.expect_value(t, unit.include_edges[0].name, "ztop")
	testing.expect_value(t, unit.include_edges[1].name, "zf01")
	testing.expect(t, has_symbol(&unit, .Include, "ztop"))
	testing.expect(t, has_symbol(&unit, .Include, "zf01"))
}

@(test)
provided_name_includes_uri_stem :: proc(t: ^testing.T) {
	unit := collect_test_unit(t, "file:///workspace/ZMAIN.abap", "REPORT zother.")

	testing.expect(t, provided_name_present(&unit, "zother"))
	testing.expect(t, provided_name_present(&unit, "zmain"))
}

@(test)
collector_reports_duplicate_and_shadowed_declarations :: proc(t: ^testing.T) {
	source := `
DATA lv_value TYPE i.
DATA lv_value TYPE i.
FORM run.
  DATA lv_value TYPE i.
ENDFORM.
`
	unit := collect_test_unit(t, "file:///diagnostics.abap", source)

	testing.expect(t, has_diagnostic(&unit, .Duplicate_Declaration))
	testing.expect(t, has_diagnostic(&unit, .Shadowed_Symbol))
}

@(test)
collects_tables_work_area_declarations :: proc(t: ^testing.T) {
	unit := collect_test_unit(t, "file:///tables.abap", "TABLES: tbtco, v_op.")

	table_names := [?]string{"tbtco", "v_op"}
	for name in table_names {
		s := find_symbol(&unit, name, .Variable)
		testing.expect(t, s != nil)
		testing.expect(t, s.has_declared_type)
		testing.expect_value(t, s.declared_type.namespace, Namespace.Type)
		testing.expect_value(t, s.declared_type.base_name, name)
		testing.expect_value(t, s.type_clause_display, name)
	}
	testing.expect_value(t, len(unit.table_work_areas), 2)
	testing.expect_value(t, unit.table_work_areas[0].name, "tbtco")
	testing.expect_value(t, unit.table_work_areas[1].name, "v_op")
}

@(test)
legacy_occurs_header_line_keeps_declared_type_clean :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///occurs_header.abap",
		"DATA int_eket LIKE beket OCCURS 0 WITH HEADER LINE.",
	)

	s := find_symbol(&unit, "int_eket", .Variable)
	testing.expect(t, s != nil)
	testing.expect(t, s.has_declared_type)
	testing.expect_value(t, s.declared_type.namespace, Namespace.Value)
	testing.expect_value(t, s.declared_type.base_name, "beket")
	testing.expect_value(t, s.type_clause_display, "beket")
}

@(test)
common_part_delimiters_do_not_emit_bogus_symbols :: proc(t: ^testing.T) {
	source := `
DATA: BEGIN OF COMMON PART fm06lcbe.
DATA: BEGIN OF bet OCCURS 50.
        INCLUDE STRUCTURE ekbe.
DATA: END OF bet.
DATA: END OF COMMON PART.
`
	unit := collect_test_unit(t, "file:///common_part.abap", source)

	testing.expect(t, has_symbol(&unit, .Variable, "bet"))
	bogus := [?]string{"begin", "common", "end"}
	for name in bogus {
		testing.expect(t, !has_symbol(&unit, .Variable, name))
		testing.expect(t, !has_reference(&unit, name, .Value, .Identifier))
	}
}

@(test)
declarations_named_common_still_collect :: proc(t: ^testing.T) {
	source := `
DATA common TYPE i.
CLASS lcl_holder DEFINITION.
  PUBLIC SECTION.
    DATA common TYPE i.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///common_name.abap", source)

	testing.expect(t, has_symbol(&unit, .Variable, "common"))
	class := find_symbol(&unit, "lcl_holder", .Class)
	testing.expect(t, class != nil)
	testing.expect(t, class_member_named(&unit, class.id, "common", .Attribute) != nil)
}

@(test)
constant_structure_collects_numeric_prefixed_component_names :: proc(t: ^testing.T) {
	source := `
CONSTANTS: BEGIN OF gc_bapi_proc_mode,
             aip VALUE 'A',
             46c VALUE 'B',
           END OF gc_bapi_proc_mode.
`
	unit := collect_test_unit(t, "file:///constant_components.abap", source)

	s := find_symbol(&unit, "gc_bapi_proc_mode", .Constant)
	testing.expect(t, s != nil)
	testing.expect(t, s.structure != INVALID_STRUCTURE_ID)
	st := structure(&unit, s.structure)
	const_fields := [?]string{"aip", "46c"}
	testing.expect(t, field_names_match(st, const_fields[:]))
	testing.expect_value(t, st.fields[0].value_clause_display, "'A'")
	testing.expect_value(t, st.fields[1].value_clause_display, "'B'")
}

@(test)
structured_includes_expand_known_members :: proc(t: ^testing.T) {
	source := `
TYPES: BEGIN OF ty_base,
         a TYPE i,
       END OF ty_base.
TYPES: BEGIN OF ty_wrap,
         INCLUDE TYPE ty_base,
         b TYPE string,
       END OF ty_wrap.
`
	unit := collect_test_unit(t, "file:///structured_include.abap", source)

	wrap := find_symbol(&unit, "ty_wrap", .Type_Def)
	testing.expect(t, wrap != nil)
	st := structure(&unit, wrap.structure)
	wrap_fields := [?]string{"a", "b"}
	testing.expect(t, field_names_match(st, wrap_fields[:]))
	testing.expect_value(t, st.fields[0].type_ref.base_name, "i")
	testing.expect_value(t, st.fields[1].type_ref.base_name, "string")
}

@(test)
selection_ranges_collect_range_structure :: proc(t: ^testing.T) {
	source := `
TYPES zattp_gln TYPE string.
DATA lv_rogln TYPE zattp_gln.
RANGES r_rogln FOR lv_rogln.
SELECT-OPTIONS s_rogln FOR lv_rogln.
`
	unit := collect_test_unit(t, "file:///ranges.abap", source)

	range_names := [?]string{"r_rogln", "s_rogln"}
	for name in range_names {
		s := find_symbol(&unit, name, .Variable)
		testing.expect(t, s != nil)
		testing.expect(t, s.structure != INVALID_STRUCTURE_ID)
		st := structure(&unit, s.structure)
		range_fields := [?]string{"sign", "option", "low", "high"}
		testing.expect(t, field_names_match(st, range_fields[:]))
		testing.expect_value(t, st.fields[2].type_ref.namespace, Namespace.Value)
		testing.expect_value(t, st.fields[2].type_ref.base_name, "lv_rogln")
	}
}

@(test)
collects_class_members_inheritance_interfaces_and_method_signature :: proc(t: ^testing.T) {
	source := `
INTERFACE lif_demo.
  METHODS base.
ENDINTERFACE.

CLASS zcl_base DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS base IMPORTING iv_code TYPE string.
ENDCLASS.

CLASS zcl_child DEFINITION INHERITING FROM zcl_base.
  PUBLIC SECTION.
    INTERFACES lif_demo.
    ALIASES alias_base FOR lif_demo~base.
    DATA mv_flag TYPE i.
    CLASS-DATA gv_count TYPE i.
    EVENTS changed EXPORTING VALUE(ev_flag) TYPE i.
    METHODS run IMPORTING iv_value TYPE i RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
`
	unit := collect_test_unit(t, "file:///class_members.abap", source)

	base := find_symbol(&unit, "zcl_base", .Class)
	child := find_symbol(&unit, "zcl_child", .Class)
	testing.expect(t, base != nil)
	testing.expect(t, child != nil)
	testing.expect(t, len(unit.class_definitions) >= 2)
	testing.expect(t, unit.class_definitions[0].is_abstract)
	testing.expect_value(t, unit.class_inheritance[0].class_symbol, child.id)
	testing.expect_value(t, unit.class_inheritance[0].superclass_name, "zcl_base")
	testing.expect_value(t, unit.implemented_interfaces[0].interface_name, "lif_demo")
	testing.expect_value(t, unit.member_aliases[0].alias_name, "alias_base")
	testing.expect_value(t, unit.member_aliases[0].target_interface_name, "lif_demo")
	testing.expect_value(t, unit.member_aliases[0].target_member_name, "base")

	attr := class_member_named(&unit, child.id, "mv_flag", .Attribute)
	static_attr := class_member_named(&unit, child.id, "gv_count", .Attribute)
	event := class_member_named(&unit, child.id, "changed", .Event)
	method := class_member_named(&unit, child.id, "run", .Method)
	testing.expect(t, attr != nil)
	testing.expect(t, static_attr != nil)
	testing.expect(t, .Is_Static in static_attr.flags)
	testing.expect(t, event != nil)
	testing.expect_value(t, event.parameters[0].name, "ev_flag")
	testing.expect(t, method != nil)
	testing.expect(t, .Has_Implementation in method.flags)
	testing.expect_value(t, len(method.parameters), 2)
	testing.expect_value(t, method.parameters[0].section, Method_Parameter_Section.Importing)
	testing.expect_value(t, method.parameters[0].name, "iv_value")
	testing.expect_value(t, method.parameters[1].section, Method_Parameter_Section.Returning)
	testing.expect_value(t, method.parameters[1].name, "rv_text")
}

@(test)
collects_oop_section_visibility_from_ast :: proc(t: ^testing.T) {
	source := `CLASS lcl_vis DEFINITION.
  PUBLIC SECTION.
    METHODS pub.
  PROTECTED SECTION.
    METHODS prot.
  PRIVATE SECTION.
    METHODS priv.
ENDCLASS.
INTERFACE lif_vis.
  PUBLIC SECTION.
    METHODS if_pub.
ENDINTERFACE.`
	unit := collect_test_unit(t, "file:///oop_visibility.abap", source)

	class := find_symbol(&unit, "lcl_vis", .Class)
	iface := find_symbol(&unit, "lif_vis", .Interface)
	testing.expect(t, class != nil)
	testing.expect(t, iface != nil)

	pub := class_member_named(&unit, class.id, "pub", .Method)
	prot := class_member_named(&unit, class.id, "prot", .Method)
	priv := class_member_named(&unit, class.id, "priv", .Method)
	if_pub := class_member_named(&unit, iface.id, "if_pub", .Method)
	testing.expect(t, pub != nil)
	testing.expect(t, prot != nil)
	testing.expect(t, priv != nil)
	testing.expect(t, if_pub != nil)
	testing.expect_value(t, pub.visibility, Visibility.Public)
	testing.expect_value(t, prot.visibility, Visibility.Protected)
	testing.expect_value(t, priv.visibility, Visibility.Private)
	testing.expect_value(t, if_pub.visibility, Visibility.Public)
}

@(test)
collects_class_header_facts_from_ast :: proc(t: ^testing.T) {
	source := `CLASS zcl_abs DEFINITION ABSTRACT.
ENDCLASS.
CLASS zcl_super DEFINITION.
ENDCLASS.
CLASS zcl_child DEFINITION INHERITING FROM zcl_super.
ENDCLASS.
CLASS zcl_impl DEFINITION.
ENDCLASS.
CLASS zcl_impl IMPLEMENTATION.
ENDCLASS.
CLASS zcl_deferred DEFINITION DEFERRED.`
	unit := collect_test_unit(t, "file:///class_header_facts.abap", source)

	abs := find_symbol(&unit, "zcl_abs", .Class)
	child := find_symbol(&unit, "zcl_child", .Class)
	impl := find_symbol(&unit, "zcl_impl", .Class)
	deferred := find_symbol(&unit, "zcl_deferred", .Class)
	testing.expect(t, abs != nil)
	testing.expect(t, child != nil)
	testing.expect(t, impl != nil)
	testing.expect(t, deferred != nil)

	abs_is_abstract := false
	deferred_has_definition := false
	for definition in unit.class_definitions {
		if definition.class_symbol == abs.id {
			abs_is_abstract = definition.is_abstract
		}
		if definition.class_symbol == deferred.id {
			deferred_has_definition = true
		}
	}
	testing.expect(t, abs_is_abstract)
	testing.expect(t, !deferred_has_definition)

	child_inherits_super := false
	for inheritance in unit.class_inheritance {
		if inheritance.class_symbol == child.id && inheritance.superclass_name == "zcl_super" {
			child_inherits_super = true
		}
	}
	testing.expect(t, child_inherits_super)

	impl_symbol_count := 0
	for symbol in unit.symbols {
		if symbol.kind == .Class && symbol.name == "zcl_impl" {
			impl_symbol_count += 1
		}
	}
	testing.expect_value(t, impl_symbol_count, 1)
}

@(test)
collects_multiple_method_parameters_from_oop_ast :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_count TYPE i iv_text TYPE string
      RETURNING VALUE(rv_ok) TYPE abap_bool.
ENDCLASS.`
	unit := collect_test_unit(t, "file:///oop_params.abap", source)

	class := find_symbol(&unit, "lcl", .Class)
	testing.expect(t, class != nil)
	method := class_member_named(&unit, class.id, "run", .Method)
	testing.expect(t, method != nil)
	testing.expect_value(t, len(method.parameters), 3)
	testing.expect_value(t, method.parameters[0].name, "iv_count")
	testing.expect_value(t, method.parameters[1].name, "iv_text")
	testing.expect_value(t, method.parameters[2].section, Method_Parameter_Section.Returning)
	testing.expect_value(t, method.parameters[2].name, "rv_ok")
}

@(test)
collects_form_and_function_signatures :: proc(t: ^testing.T) {
	source := `
FORM run TABLES !ct_rows STRUCTURE mara USING VALUE(iv_text) TYPE string REFERENCE(iv_ref) LIKE sy-uname CHANGING cv_count TYPE i.
ENDFORM.

FUNCTION z_demo
  IMPORTING VALUE(iv_value) TYPE i OPTIONAL iv_text TYPE string DEFAULT 'x'
  EXPORTING ev_text LIKE sy-uname
  CHANGING REFERENCE(cv_any) TYPE REF TO object
  TABLES et_return STRUCTURE bapiret2
  EXCEPTIONS failed = 1 not_found.
ENDFUNCTION.
`
	unit := collect_test_unit(t, "file:///signatures.abap", source)

	testing.expect_value(t, len(unit.form_routines), 1)
	form := unit.form_routines[0]
	testing.expect_value(t, len(form.parameters), 4)
	ct_rows := unit.symbols[symbol_id_index(form.parameters[0].symbol)]
	iv_text := unit.symbols[symbol_id_index(form.parameters[1].symbol)]
	iv_ref := unit.symbols[symbol_id_index(form.parameters[2].symbol)]
	testing.expect_value(t, ct_rows.name, "ct_rows")
	testing.expect_value(t, ct_rows.declared_type.namespace, Namespace.Value)
	testing.expect_value(t, form.parameters[0].section, Form_Parameter_Section.Tables)
	testing.expect_value(t, form.parameters[1].section, Form_Parameter_Section.Using)
	testing.expect_value(t, form.parameters[1].passing, Form_Parameter_Passing_Kind.Value)
	testing.expect_value(t, iv_text.name, "iv_text")
	testing.expect_value(t, form.parameters[2].passing, Form_Parameter_Passing_Kind.Reference)
	testing.expect_value(t, iv_ref.declared_type.namespace, Namespace.Value)
	testing.expect_value(t, form.parameters[3].section, Form_Parameter_Section.Changing)

	testing.expect_value(t, len(unit.function_modules), 1)
	fm := unit.function_modules[0]
	testing.expect_value(t, len(fm.parameters), 5)
	testing.expect_value(t, fm.parameters[0].name, "iv_value")
	testing.expect(t, .Is_Optional in fm.parameters[0].flags)
	testing.expect_value(t, fm.parameters[1].name, "iv_text")
	testing.expect(t, .Has_Default_Value in fm.parameters[1].flags)
	testing.expect_value(t, fm.parameters[2].section, Function_Module_Parameter_Section.Exporting)
	testing.expect_value(t, fm.parameters[2].declared_type.namespace, Namespace.Value)
	testing.expect_value(t, fm.parameters[3].section, Function_Module_Parameter_Section.Changing)
	testing.expect_value(t, fm.parameters[3].declared_type.base_name, "object")
	testing.expect_value(t, fm.parameters[4].section, Function_Module_Parameter_Section.Tables)
	testing.expect_value(t, fm.parameters[4].declared_type.namespace, Namespace.Value)
	testing.expect_value(t, fm.parameters[4].declared_type.base_name, "bapiret2")
	testing.expect_value(t, len(fm.exceptions), 2)
	testing.expect_value(t, fm.exceptions[0].name, "failed")
	testing.expect_value(t, fm.exceptions[1].name, "not_found")
	testing.expect(t, has_reference(&unit, "string", .Type, .Type_Ref))
	testing.expect(t, has_reference(&unit, "sy", .Value, .Type_Ref))
	testing.expect(t, has_reference(&unit, "bapiret2", .Value, .Type_Ref))
}

@(test)
resolves_local_variable_references :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///local_refs.abap",
		`
FORM run.
  DATA lv_count TYPE i.
  lv_count = lv_count + 1.
ENDFORM.
`,
	)

	testing.expect_value(t, reference_count(&unit, "lv_count", .Value, .Identifier), 2)
	for reference in unit.references {
		if reference.name == "lv_count" && reference.kind == .Identifier {
			testing.expect(t, reference.has_resolution)
			testing.expect_value(t, reference.resolution.kind, Resolution_Kind.Symbol)
		}
	}
	testing.expect_value(t, len(unit.assignment_sites), 1)
}

@(test)
validates_unresolved_and_wrong_namespace_references :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///reference_diagnostics.abap",
		`
TYPES ty_value TYPE i.
ty_value = 1.
missing = 2.
`,
	)

	testing.expect(t, has_diagnostic(&unit, .Wrong_Namespace))
	testing.expect(t, has_diagnostic(&unit, .Unresolved_Reference))
}

@(test)
resolves_form_changing_parameter_in_body :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///form_params.abap",
		`
FORM some_form CHANGING cv_result TYPE string.
  DATA lv_text TYPE string.
  cv_result = lv_text.
ENDFORM.
`,
	)

	param := find_symbol(&unit, "cv_result", .Parameter)
	testing.expect(t, param != nil)
	testing.expect(t, param.has_declared_type)
	testing.expect_value(t, param.declared_type.namespace, Namespace.Type)
	testing.expect_value(t, param.declared_type.base_name, "string")
	for reference in unit.references {
		if reference.name == "cv_result" {
			testing.expect(t, reference.has_resolution)
			testing.expect_value(t, reference.resolution.symbol.symbol, param.id)
		}
	}
}

@(test)
resolves_class_type_ref_to :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///class_ref_to.abap",
		`
CLASS c1 DEFINITION.
ENDCLASS.

DATA lo_c1 TYPE REF TO c1.
`,
	)

	c1 := find_symbol(&unit, "c1", .Class)
	lo_c1 := find_symbol(&unit, "lo_c1", .Variable)
	testing.expect(t, c1 != nil)
	testing.expect(t, lo_c1 != nil)
	testing.expect(t, lo_c1.has_declared_type)
	testing.expect(t, lo_c1.declared_type.is_ref)
	testing.expect_value(t, lo_c1.declared_type.base_name, "c1")
	for reference in unit.references {
		if reference.name == "c1" && reference.kind == .Type_Ref {
			testing.expect(t, reference.has_resolution)
			testing.expect_value(t, reference.resolution.symbol.symbol, c1.id)
		}
	}
}

@(test)
resolves_class_qualified_local_type_refs :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///class_qualified_type.abap",
		`
CLASS lcl_archive_connector DEFINITION.
  PUBLIC SECTION.
    TYPES tr_retriable_errs TYPE RANGE OF string.
ENDCLASS.

DATA(lr_retriable_errs) = VALUE lcl_archive_connector=>tr_retriable_errs( ).
`,
	)

	class_symbol := find_symbol(&unit, "lcl_archive_connector", .Class)
	testing.expect(t, class_symbol != nil)
	testing.expect(t, has_symbol(&unit, .Type_Def, "tr_retriable_errs"))
	testing.expect(t, has_symbol(&unit, .Variable, "lr_retriable_errs"))
	found_type_ref := false
	for reference in unit.references {
		if reference.name == "lcl_archive_connector" && reference.kind == .Type_Ref {
			found_type_ref = true
			testing.expect(t, reference.has_resolution)
			testing.expect_value(t, reference.resolution.symbol.symbol, class_symbol.id)
		}
	}
	testing.expect(t, found_type_ref)
}

@(test)
resolves_builtin_screen_and_syst_fields :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///builtins_fields.abap",
		`
IF sy-subrc = 0.
ENDIF.

LOOP AT SCREEN.
  IF screen-name = 'P_FOO'.
  ENDIF.
ENDLOOP.
`,
	)

	names := [?]string{"sy", "screen"}
	for name in names {
		for reference in unit.references {
			if reference.name == name && reference.kind == .Identifier {
				testing.expect(t, reference.has_resolution)
				testing.expect_value(t, reference.resolution.kind, Resolution_Kind.Symbol)
			}
		}
	}
	syst := find_structure(&unit, "syst")
	screen := find_structure(&unit, "screen")
	testing.expect(t, syst != nil)
	testing.expect(t, screen != nil)
	subrc, subrc_ok := structure_field_info(&unit, syst.id, "subrc")
	screen_name, screen_ok := structure_field_info(&unit, screen.id, "name")
	testing.expect(t, subrc_ok)
	testing.expect(t, screen_ok)
	testing.expect_value(t, subrc.type_ref.base_name, "i")
	testing.expect_value(t, screen_name.type_ref.base_name, "c")
}

@(test)
semantic_queries_find_symbols_references_sql_and_facts :: proc(t: ^testing.T) {
	source := `DATA lv_value TYPE i.
DATA lv_copy TYPE i.
lv_copy = lv_value.
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).`
	unit := collect_test_unit(t, "file:///semantic_queries.abap", source)
	query := semantic(&unit)
	decl_query := decls(query)
	ref_query := refs(query)
	sql_query := sql(query)
	fact_query := facts(query)

	decl_offset := find_text(source, "lv_value")
	use_offset := find_text_last(source, "lv_value")
	testing.expect(t, decl_offset >= 0)
	testing.expect(t, use_offset > decl_offset)

	sym := decl_symbol_at_offset(decl_query, decl_offset)
	testing.expect(t, sym != nil)
	testing.expect_value(t, sym.name, "lv_value")

	by_range := decl_symbol_with_kind_and_decl_range(decl_query, .Variable, sym.decl_range)
	testing.expect(t, by_range != nil)
	testing.expect_value(t, by_range.id, sym.id)

	ref := ref_reference_at_offset(ref_query, use_offset)
	testing.expect(t, ref != nil)
	testing.expect_value(t, ref.name, "lv_value")
	testing.expect(t, ref.has_resolution)

	exact_ref := ref_reference_at_range(ref_query, ref.range)
	testing.expect(t, exact_ref != nil)
	testing.expect_value(t, exact_ref.id, ref.id)

	resolved := ref_resolving_to(
		ref_query,
		Symbol_Handle{unit = unit.unit_id, symbol = sym.id},
		context.allocator,
	)
	testing.expect_value(t, len(resolved), 1)

	source_offset := find_text(source, "scarr")
	sql_ref := sql_name_ref_at_offset(sql_query, source_offset)
	testing.expect(t, sql_ref != nil)
	testing.expect_value(t, sql_ref.kind, Sql_Name_Ref_Kind.Source)
	testing.expect(t, sql_has_source_named(sql_query, "SCARR"))
	sql_sources := sql_source_name_refs_named(sql_query, "scarr", context.allocator)
	testing.expect_value(t, len(sql_sources), 1)

	fact := fact_expression_fact_at_offset(fact_query, use_offset)
	testing.expect(t, fact != nil)
	testing.expect_value(t, fact.kind, Expression_Fact_Kind.Reference)
}

@(test)
semantic_queries_find_class_members_and_structure_fields :: proc(t: ^testing.T) {
	source := `
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

TYPES: BEGIN OF ty_demo,
         comp TYPE i,
       END OF ty_demo.
`
	unit := collect_test_unit(t, "file:///decl_queries.abap", source)
	query := semantic(&unit)
	decl_query := decls(query)

	method_offset := find_text(source, "run")
	member := decl_class_member_at_offset(decl_query, method_offset)
	testing.expect(t, member != nil)
	testing.expect_value(t, member.kind, Class_Member_Kind.Method)
	testing.expect_value(t, member.name, "run")

	class_symbol := find_symbol(&unit, "lcl_demo", .Class)
	testing.expect(t, class_symbol != nil)
	member_by_name := decl_class_member(decl_query, class_symbol.id, "RUN")
	testing.expect(t, member_by_name != nil)
	testing.expect_value(t, member_by_name.name, "run")

	field_offset := find_text(source, "comp")
	field, ok := decl_structure_field_at_offset(decl_query, field_offset)
	testing.expect(t, ok)
	testing.expect_value(t, field.name, "comp")
	direct, direct_ok := decl_structure_field_info(decl_query, field.owner, "COMP")
	testing.expect(t, direct_ok)
	testing.expect_value(t, direct.name, "comp")
}

@(test)
collects_do_times_header_references :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///do_times_header.abap",
		`
FORM run.
  DATA lv_max_len TYPE i.
  DO lv_max_len TIMES.
  ENDDO.
ENDFORM.
`,
	)

	testing.expect_value(t, reference_count(&unit, "lv_max_len", .Value, .Identifier), 1)
	testing.expect_value(t, len(unit.routine_control_regions), 1)
	testing.expect_value(t, unit.routine_control_regions[0].kind, Routine_Control_Region_Kind.Loop)
}

@(test)
collects_case_when_header_references :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///case_when_header.abap",
		`
FORM run.
  CONSTANTS lc_rs_agg_op TYPE string VALUE 'SUM'.
  DATA lv_kind TYPE string.
  CASE lv_kind.
    WHEN lc_rs_agg_op.
      WRITE lc_rs_agg_op.
  ENDCASE.
ENDFORM.
`,
	)

	testing.expect_value(t, reference_count(&unit, "lv_kind", .Value, .Identifier), 1)
	testing.expect_value(t, reference_count(&unit, "lc_rs_agg_op", .Value, .Identifier), 2)
	testing.expect_value(t, len(unit.routine_control_regions), 1)
	testing.expect_value(t, unit.routine_control_regions[0].kind, Routine_Control_Region_Kind.Case)
}

@(test)
catch_into_data_declares_inline_and_type_ref :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///catch_inline.abap",
		`
CLASS cx_demo DEFINITION.
ENDCLASS.

TRY.
  CATCH cx_demo INTO DATA(lo_error).
ENDTRY.
`,
	)

	testing.expect(t, has_symbol(&unit, .Variable, "lo_error"))
	testing.expect(t, has_reference(&unit, "cx_demo", .Type, .Type_Ref))
}

@(test)
collects_method_function_and_perform_argument_facts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///call_facts.abap",
		`
FORM process_data USING pv_mode TYPE string CHANGING cv_count TYPE i.
ENDFORM.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_value TYPE i EXPORTING ev_value TYPE i.
    METHODS exec.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD exec.
    DATA lv_value TYPE i.
    CALL METHOD run EXPORTING iv_value = lv_value IMPORTING ev_value = DATA(lv_out).
    CALL FUNCTION 'Z_DEMO' EXPORTING iv_value = lv_value.
    PERFORM process_data USING 'demo' CHANGING lv_value.
  ENDMETHOD.
ENDCLASS.
`,
	)

	testing.expect(t, has_named_argument(&unit, "iv_value", .Exporting, .Implicit_Method))
	testing.expect(t, has_named_argument(&unit, "ev_value", .Importing, .Implicit_Method))
	testing.expect(t, has_named_argument(&unit, "iv_value", .Exporting, .Function))
	testing.expect(t, has_symbol(&unit, .Variable, "lv_out"))
	testing.expect(t, len(unit.call_sites) >= 2)
	testing.expect_value(t, len(unit.perform_calls), 1)
	testing.expect_value(t, len(unit.perform_calls[0].arguments), 2)
	testing.expect_value(
		t,
		unit.perform_calls[0].arguments[1].section,
		Perform_Parameter_Section.Changing,
	)
}

@(test)
collects_message_default_and_message_use_facts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///message_facts.abap",
		`
REPORT zmain MESSAGE-ID zmsg.
DATA lv_text TYPE string.
MESSAGE i001 WITH lv_text.
`,
	)

	testing.expect(t, unit.has_message_default_class)
	testing.expect_value(t, unit.message_default_class.name, "zmsg")
	testing.expect(t, has_reference(&unit, "zmsg", .Value, .Message_Class))
	testing.expect_value(t, len(unit.message_uses), 1)
	testing.expect_value(t, unit.message_uses[0].class_name, "zmsg")
	testing.expect_value(t, len(unit.message_uses[0].with_arg_ranges), 1)
	testing.expect(t, has_reference(&unit, "lv_text", .Value, .Identifier))
}

@(test)
no_bogus_references_for_transaction_and_type_pools :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///no_bogus_refs.abap",
		`
FORM run.
  COMMIT WORK.
  ROLLBACK WORK.
  TYPE-POOLS abap.
ENDFORM.
`,
	)

	testing.expect(t, !has_reference(&unit, "work", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "abap", .Value, .Identifier))
	testing.expect(t, !has_reference(&unit, "abap", .Type, .Type_Ref))
}

@(test)
collects_open_sql_query_projection_source_predicate_and_inline_target :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql.abap",
		`
DATA iv_carrid TYPE c LENGTH 3.
SELECT carrid, carrname
  FROM scarr
  INTO TABLE @DATA(lt_scarr)
  WHERE carrid = @iv_carrid.
`,
	)

	testing.expect_value(t, len(unit.sql_queries), 1)
	query := unit.sql_queries[0]
	testing.expect(t, .Has_Projection_Clause in query.flags)
	testing.expect(t, .Has_From_Clause in query.flags)
	testing.expect(t, .Has_Into_Clause in query.flags)
	testing.expect(t, .Has_Where_Clause in query.flags)
	testing.expect(t, sql_source_present(&unit, "scarr", .External))
	testing.expect(t, sql_projection_present(&unit, "carrid", .Column))
	testing.expect(t, sql_projection_present(&unit, "carrname", .Column))
	testing.expect(t, sql_predicate_present(&unit, .Where))
	testing.expect(t, sql_target_present(&unit, "lt_scarr", .Into, {.Is_Table, .Is_Inline}))
	testing.expect(t, has_reference(&unit, "iv_carrid", .Value, .Identifier))

	target := find_symbol(&unit, "lt_scarr", .Variable)
	testing.expect(t, target != nil)
	testing.expect(t, target.structure != INVALID_STRUCTURE_ID)
	st := structure(&unit, target.structure)
	fields := [?]string{"carrid", "carrname"}
	testing.expect(t, field_names_match(st, fields[:]))
}

@(test)
collects_dynamic_open_sql_fragments :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///dynamic_sql.abap",
		`
DATA lv_fields TYPE string.
DATA lv_table TYPE string.
DATA lv_where TYPE string.

SELECT (lv_fields)
  FROM (lv_table)
  INTO TABLE @DATA(lt_rows)
  WHERE (lv_where).
`,
	)

	testing.expect_value(t, len(unit.sql_queries), 1)
	testing.expect(t, sql_dynamic_present(&unit, .Projection))
	testing.expect(t, sql_dynamic_present(&unit, .Source))
	testing.expect(t, sql_dynamic_present(&unit, .Where))
	testing.expect_value(t, len(unit.sql_sources), 0)
	testing.expect(t, !sql_name_ref_present(&unit, "lv_fields", .Column))
	dynamic_names := [?]string{"lv_fields", "lv_table", "lv_where"}
	for name in dynamic_names {
		testing.expect(t, has_reference(&unit, name, .Value, .Identifier))
	}
}

@(test)
sql_scan_query_tolerates_ranges_without_select_keyword :: proc(t: ^testing.T) {
	source := "carrid FROM scarr"
	c := Collector{source = source, allocator = context.allocator}
	scan := sql_scan_query(&c, tokenizer.text_range(0, len(source)))

	testing.expect(t, !range_valid(scan.projection_clause))
	testing.expect(t, range_valid(scan.from_clause))
}

@(test)
collects_common_table_expression_sql_facts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///cte_dossier.abap",
		`
DATA lv_carrid TYPE string.

WITH +filtered AS (
       SELECT carrid, connid
         FROM sflight
         WHERE carrid = @lv_carrid
     ),
     +joined AS (
       SELECT f~carrid
         FROM +filtered AS f
         INNER JOIN spfli AS p ON p~carrid = f~carrid
     )
SELECT carrid
  FROM +joined
  INTO TABLE @DATA(lt_flights).
`,
	)

	testing.expect_value(t, len(unit.sql_queries), 3)
	testing.expect(t, sql_source_present(&unit, "sflight", .External))
	testing.expect(t, sql_source_present(&unit, "spfli", .External))
	testing.expect(t, sql_source_present(&unit, "+filtered", .Local_Cte))
	testing.expect(t, sql_source_present(&unit, "+joined", .Local_Cte))
	testing.expect(t, sql_name_ref_present(&unit, "sflight", .Source))
	testing.expect(t, sql_name_ref_present(&unit, "spfli", .Source))
	testing.expect(t, !sql_name_ref_present(&unit, "+filtered", .Source))
	testing.expect(t, !sql_name_ref_present(&unit, "+joined", .Source))
	testing.expect(t, sql_target_present(&unit, "lt_flights", .Into, {.Is_Table, .Is_Inline}))
	testing.expect(t, has_reference(&unit, "lv_carrid", .Value, .Identifier))
}

@(test)
collects_join_alias_qualified_star_aggregate_and_set_operator_facts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql_join_set.abap",
		`
SELECT a~carrid AS carrier, COUNT( * ) AS total
  FROM scarr AS a
  INNER JOIN spfli AS b ON b~carrid = a~carrid
  INTO TABLE @DATA(lt_rows)
UNION ALL SELECT * FROM spfli INTO TABLE @lt_rows.
`,
	)

	testing.expect_value(t, len(unit.sql_queries), 1)
	testing.expect(t, .Has_Set_Operators in unit.sql_queries[0].flags)
	testing.expect(t, sql_source_alias_present(&unit, "scarr", "a", .From))
	testing.expect(t, sql_source_alias_present(&unit, "spfli", "b", .Join))
	testing.expect(t, sql_projection_alias_present(&unit, "carrier", .Column))
	testing.expect(t, sql_projection_alias_present(&unit, "total", .Aggregate))
	testing.expect(t, sql_name_ref_present(&unit, "count", .Aggregate))
	testing.expect(t, sql_qualified_ref_present(&unit, "a", "carrid", .Qualified_Column))
	testing.expect(t, sql_name_ref_present(&unit, "*", .Star))
	testing.expect(t, sql_predicate_present(&unit, .Join_On))
}

@(test)
collects_sql_for_all_entries_and_dynamic_where_predicates :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql_predicates.abap",
		`
DATA lt_keys TYPE string.
DATA lv_where TYPE string.

SELECT DISTINCT carrid
  FROM sflight
  INTO TABLE @DATA(lt_rows)
  FOR ALL ENTRIES IN lt_keys
  WHERE (lv_where).
`,
	)

	testing.expect_value(t, len(unit.sql_queries), 1)
	testing.expect(t, .Is_Distinct in unit.sql_queries[0].flags)
	testing.expect(t, .Has_Dynamic_Where in unit.sql_queries[0].flags)
	testing.expect(t, .Has_For_All_Entries in unit.sql_queries[0].flags)
	testing.expect(t, sql_predicate_present(&unit, .Dynamic_Where))
	testing.expect(t, sql_predicate_present(&unit, .For_All_Entries))
	testing.expect(t, sql_dynamic_present(&unit, .Where))
	testing.expect(t, has_reference(&unit, "lt_keys", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_where", .Value, .Identifier))
}

@(test)
validates_open_sql_source_and_field_diagnostics :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///sql_validation.abap",
		`
TYPES: BEGIN OF zflight,
         carrid TYPE string,
       END OF zflight.

SELECT connid FROM zflight INTO TABLE @DATA(lt_rows).
SELECT carrid FROM zmissing INTO TABLE @DATA(lt_missing).
`,
	)

	testing.expect(t, has_diagnostic(&unit, .Unknown_Field))
	testing.expect(t, has_diagnostic(&unit, .Unverified_Open_Sql_Source))
}

@(test)
collects_sort_order_and_read_table_binary_search_facts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///read_table_binary_search.abap",
		`
FORM run.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
           connid TYPE string,
         END OF ty_row.
  DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
  DATA ls_row TYPE ty_row.

  SORT lt_rows BY carrid connid.
  READ TABLE lt_rows INTO ls_row WITH KEY carrid = 'AA' connid = '001' BINARY SEARCH.
ENDFORM.
`,
	)
	keys := [?]string{"carrid", "connid"}
	testing.expect(t, internal_table_order_present(&unit, "lt_rows", keys[:]))
	testing.expect(t, binary_search_present(&unit, "lt_rows", keys[:]))
}

@(test)
collects_select_order_by_into_table_order_fact :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///select_order.abap",
		`
FORM run.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  SELECT carrid, connid
    FROM zflights
    INTO TABLE @lt_rows
    ORDER BY carrid, connid.
ENDFORM.
`,
	)
	keys := [?]string{"carrid", "connid"}
	testing.expect(t, .Has_Order_By_Clause in unit.sql_queries[0].flags)
	testing.expect(t, internal_table_order_present(&unit, "lt_rows", keys[:]))
}

@(test)
collects_db_table_statement_sql_sources_and_host_refs :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///db_table_sql.abap",
		`
FORM run.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA ls_row TYPE string.
  DATA lr_objid TYPE string.

  INSERT zinsert_tab FROM TABLE lt_rows.
  MODIFY zmodify_tab FROM ls_row.
	UPDATE zupdate_tab SET status = ls_row WHERE objid = lr_objid.
  DELETE FROM zdelete_tab WHERE objid = lr_objid.
ENDFORM.
`,
	)

	db_names := [?]string{"zinsert_tab", "zmodify_tab", "zupdate_tab", "zdelete_tab"}
	for name in db_names {
		testing.expect(t, sql_source_present(&unit, name, .External))
		testing.expect(t, sql_name_ref_present(&unit, name, .Source))
		testing.expect(t, !has_reference(&unit, name, .Value, .Identifier))
	}
	testing.expect(t, sql_name_ref_present(&unit, "status", .Column))
	testing.expect(t, sql_name_ref_present(&unit, "objid", .Column))
	testing.expect(t, sql_predicate_present(&unit, .Where))
	testing.expect(t, has_reference(&unit, "lt_rows", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "ls_row", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lr_objid", .Value, .Identifier))
	testing.expect(t, system_update_present(&unit, .Insert_Db_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Modify_Db_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Update_Db_Table, "subrc"))
	testing.expect(t, system_update_present(&unit, .Delete_Db_Table, "subrc"))
}

@(test)
collects_surface_source_maintenance_and_string_operation_facts :: proc(t: ^testing.T) {
	unit := collect_test_unit(
		t,
		"file:///surface_facts.abap",
		`
FORM run.
  DATA lt_source TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lt_report TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lt_pool TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lv_prog TYPE string.
  DATA lv_msg TYPE string.
  DATA lv_line TYPE i.
  DATA lv_word TYPE string.
  DATA lv_offset TYPE i.
  DATA lv_file TYPE string.
  DATA lv_text TYPE string.
  DATA lv_len TYPE i.
  DATA lv_pos TYPE i.
  DATA lv_attr TYPE string.
  DATA lv_result TYPE string.
  DATA lv_off TYPE i.
  DATA lv_match_len TYPE i.

  READ REPORT lv_prog INTO lt_report.
  INSERT REPORT lv_prog FROM lt_source.
  INSERT TEXTPOOL lv_prog FROM lt_pool LANGUAGE 'E'.
  GENERATE SUBROUTINE POOL lt_source NAME lv_prog MESSAGE lv_msg LINE lv_line WORD lv_word OFFSET lv_offset.
  OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_msg.
  READ DATASET lv_file INTO lv_text ACTUAL LENGTH lv_len.
  GET DATASET lv_file POSITION lv_pos ATTRIBUTES lv_attr.
  CONCATENATE LINES OF lt_source INTO lv_text IN BYTE MODE.
  FIND 'A' IN lv_text MATCH OFFSET lv_off MATCH LENGTH lv_match_len RESULTS lv_result.
ENDFORM.
`,
	)

	testing.expect(t, system_update_present(&unit, .Read_Report, "subrc"))
	testing.expect(t, system_update_present(&unit, .Insert_Report, "subrc"))
	testing.expect(t, system_update_present(&unit, .Insert_Textpool, "subrc"))
	testing.expect(t, len(unit.concatenate_lines_of_sites) == 1)
	testing.expect(t, unit.concatenate_lines_of_sites[0].byte_mode)
	testing.expect(t, len(unit.find_sites) == 1)
	testing.expect(t, len(unit.find_sites[0].write_targets) == 3)
	testing.expect(t, len(unit.assignment_sites) >= 12)
	testing.expect(t, has_reference(&unit, "lt_source", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lt_report", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lt_pool", .Value, .Identifier))
	testing.expect(t, has_reference(&unit, "lv_attr", .Value, .Identifier))
}

@(test)
manifest_decoder_accepts_rust_compatible_units :: proc(t: ^testing.T) {
	source := `
connection = "SAP-DEV"

[dependency_store]
product_version = "S4-2023"
default_package_version = "base"

[dependency_store.packages]
SAP_BASIS = "basis"

[local_export]
roots = ["../exports", "D:\\adt\\export"]

[dependencies]
source = "local-first"

[[unit]]
name = "ZMAIN"
kind = "program"
root_file = "src\\ZMAIN.abap"
members = [
  "src/ZMAIN_TOP.abap",
  { file = "./src/forms/ZMAIN_F01.abap", role = "include", object_name = "ZMAIN_F01" },
]

[[unit]]
name = "ZCL_DEP"
kind = "class"
root_file = "src/ZCL_DEP.abap"
dependency_of = ["src/ZMAIN.abap"]
`
	manifest, ok, err := parse_workspace_manifest_text("D:/workspace", "D:/workspace/abapls.toml", source, context.allocator)

	testing.expect(t, ok)
	testing.expect_value(t, err, "")
	testing.expect_value(t, manifest.connection, "sap-dev")
	testing.expect(t, manifest.has_dependency_store)
	testing.expect_value(t, manifest.dependency_store.product_version, "S4-2023")
	testing.expect_value(t, manifest.dependency_store.default_package_version, "base")
	testing.expect_value(t, len(manifest.dependency_store.packages), 1)
	if len(manifest.dependency_store.packages) == 1 {
		testing.expect_value(t, manifest.dependency_store.packages[0].package_name, "SAP_BASIS")
		testing.expect_value(t, manifest.dependency_store.packages[0].version, "basis")
	}
	testing.expect_value(t, len(manifest.local_export_roots), 2)
	if len(manifest.local_export_roots) == 2 {
		testing.expect_value(t, manifest.local_export_roots[0], "../exports")
		testing.expect_value(t, manifest.local_export_roots[1], "D:/adt/export")
	}
	testing.expect_value(t, manifest.dependency_source, "local-first")
	testing.expect_value(t, len(manifest.units), 2)
	if !ok || len(manifest.units) != 2 {
		return
	}
	testing.expect_value(t, manifest.units[0].name, "ZMAIN")
	testing.expect_value(t, manifest.units[0].kind, "program")
	testing.expect_value(t, manifest.units[0].root_file, "src/ZMAIN.abap")
	testing.expect_value(t, len(manifest.units[0].members), 2)
	testing.expect_value(t, manifest.units[0].members[0].file, "src/ZMAIN_TOP.abap")
	testing.expect_value(t, manifest.units[0].members[1].file, "src/forms/ZMAIN_F01.abap")
	testing.expect_value(t, manifest.units[0].members[1].role, "include")
	testing.expect_value(t, manifest.units[0].members[1].object_name, "ZMAIN_F01")
	testing.expect_value(t, manifest.units[1].root_file, "src/ZCL_DEP.abap")
	testing.expect_value(t, len(manifest.units[1].dependency_of), 1)
	testing.expect_value(t, manifest.units[1].dependency_of[0].file, "src/ZMAIN.abap")
}

@(test)
manifest_root_resolves_explicit_member_include_name :: proc(t: ^testing.T) {
	root := manifest_workspace_path("explicit-member")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
kind = "program"
root_file = "src/ZMAIN.abap"
members = [{ file = "src/includes/generated.abap", role = "include", object_name = "ZTOP" }]
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain. INCLUDE ztop. lv_top = 1.")
	member_file := manifest_test_file(t, root, "src/includes/generated.abap", "DATA lv_top TYPE i.")

	result := analyze_path_test(t, root_file)

	testing.expect(t, result.ok)
	testing.expect(t, result.used_manifest)
	testing.expect_value(t, len(result.project.units), 2)
	root_unit := project_unit_by_uri(&result.project, root_file)
	testing.expect(t, root_unit != nil)
	testing.expect_value(t, include_target_uri(&result.project, root_unit, "ztop"), member_file)
	testing.expect(t, reference_resolves_to_uri(&result.project, root_unit, "lv_top", .Value, .Identifier, member_file))
	testing.expect(t, !project_has_diagnostic(&result.project, .Unresolved_Include))
}

@(test)
manifest_listed_unreferenced_member_is_not_visible :: proc(t: ^testing.T) {
	root := manifest_workspace_path("unreferenced-member")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
root_file = "src/ZMAIN.abap"
members = ["src/ZUNUSED.abap"]
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain. DATA lo_unused TYPE REF TO zcl_unused.")
	unused_file := manifest_test_file(t, root, "src/ZUNUSED.abap", "CLASS zcl_unused DEFINITION. ENDCLASS.")

	result := analyze_path_test(t, root_file)
	root_unit := project_unit_by_uri(&result.project, root_file)

	testing.expect(t, result.ok)
	testing.expect(t, result.used_manifest)
	testing.expect_value(t, len(result.project.units), 1)
	testing.expect(t, project_unit_by_uri(&result.project, unused_file) == nil)
	testing.expect(t, root_unit != nil)
	testing.expect(t, has_diagnostic(root_unit, .Unresolved_Reference))
}

@(test)
manifest_dependency_store_drains_iteratively :: proc(t: ^testing.T) {
	root := manifest_workspace_path("dependency-store-drain")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	inputs := [?]dep_store.Stored_Artifact_Input {
		{
			package_name   = "ZPKG",
			object_kind    = "global-class",
			object_name    = "ZCL_OUTER",
			object_uri     = "/sap/bc/adt/oo/classes/ZCL_OUTER",
			object_type    = "CLAS/OC",
			description    = "Outer class",
			file_extension = "abap",
			source_text    = "CLASS zcl_outer DEFINITION. PUBLIC SECTION. DATA value TYPE zdep_type. ENDCLASS. CLASS zcl_outer IMPLEMENTATION. ENDCLASS.",
			fetched_at     = "2026-05-21T00:00:00Z",
		},
		{
			package_name   = "ZPKG",
			object_kind    = "ddic-data-element",
			object_name    = "ZDEP_TYPE",
			object_uri     = "/sap/bc/adt/ddic/dataelements/ZDEP_TYPE",
			object_type    = "DTEL/DE",
			description    = "Dependent type",
			file_extension = "abap",
			source_text    = "TYPES zdep_type TYPE string.",
			fetched_at     = "2026-05-21T00:00:00Z",
		},
	}
	_, err = dep_store.put_artifacts(&store, &profile, inputs[:], context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[dependency_store]
product_version = "S4-2023"
default_package_version = "base"

[[unit]]
name = "ZMAIN"
root_file = "src/ZMAIN.abap"
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain. DATA lo_outer TYPE REF TO zcl_outer.")

	result := analyze_path_test_with_options(t, root_file, Analyze_Options{dependency_store_path = store_path})

	testing.expect(t, result.ok)
	testing.expect(t, result.used_manifest)
	testing.expect_value(t, len(result.project.units), 3)
	testing.expect(t, !project_has_diagnostic(&result.project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&result.project, .Unresolved_Reference))
}

@(test)
standalone_file_drains_dependency_store :: proc(t: ^testing.T) {
	root := manifest_workspace_path("standalone-dependency-store-drain")
	store_path, _ := filepath.join({root, "cache.sqlite3"}, context.allocator)
	store, err := dep_store.dependency_store_from_override_path(store_path, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)
	profile := dep_store.Dependency_Profile {
		product_version         = "S4-2023",
		default_package_version = "base",
	}
	artifact := dep_store.Stored_Artifact_Input {
		package_name   = "ZPKG",
		object_kind    = "global-class",
		object_name    = "ZCL_STANDALONE_CACHE",
		object_uri     = "/sap/bc/adt/oo/classes/ZCL_STANDALONE_CACHE",
		object_type    = "CLAS/OC",
		description    = "Standalone cache class",
		file_extension = "abap",
		source_text    = "CLASS zcl_standalone_cache DEFINITION. ENDCLASS. CLASS zcl_standalone_cache IMPLEMENTATION. ENDCLASS.",
		fetched_at     = "2026-05-21T00:00:00Z",
	}
	_, err = dep_store.put_artifact(&store, &profile, &artifact, context.allocator)
	testing.expect_value(t, err, dep_store.Store_Error.None)

	root_file := manifest_test_file(
		t,
		root,
		"ZMAIN.abap",
		"REPORT zmain. DATA lo_dep TYPE REF TO zcl_standalone_cache.",
	)
	result := analyze_path_test_with_options(t, root_file, Analyze_Options{dependency_store_path = store_path})

	testing.expect(t, result.ok)
	testing.expect(t, !result.used_manifest)
	testing.expect_value(t, len(result.project.units), 2)
	testing.expect(t, !project_has_diagnostic(&result.project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&result.project, .Unresolved_Reference))
}

@(test)
manifest_local_export_fallback_resolves_remote_candidate :: proc(t: ^testing.T) {
	export_root := external_export_workspace_path("external-export-root")
	export_file := manifest_test_file(
		t,
		export_root,
		"source-code-library/classes/ZCL_LOCAL_EXPORT.abap",
		"CLASS zcl_local_export DEFINITION. ENDCLASS. CLASS zcl_local_export IMPLEMENTATION. ENDCLASS.",
	)
	manifest := Workspace_Manifest {
		root_path          = export_root,
		dependency_source  = "local-first",
		local_export_roots = make([dynamic]string, 0, 1, context.allocator),
	}
	append(&manifest.local_export_roots, export_root)
	target := Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lo_dep TYPE REF TO zcl_local_export.",
	}
	candidates := make([dynamic]Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]Source_Input, context.allocator)
	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	project := analyze_with_manifest_dependency_drain(
		&manifest,
		target,
		candidates,
		dependencies,
		Analyze_Options{pool = &pool},
		context.allocator,
	)
	frontend_runtime.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 2)
	testing.expect(t, project_unit_by_uri(&project, export_file) != nil)
	testing.expect(t, !project_has_diagnostic(&project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
manifest_project_dotenv_gates_adt_dependency_fetch :: proc(t: ^testing.T) {
	root := manifest_workspace_path("adt-dotenv-gate")
	manifest := Workspace_Manifest{root_path = root}
	testing.expect(t, !manifest_has_project_dotenv(&manifest, context.allocator))

	manifest_test_file(
		t,
		root,
		".env",
		`
ABAP_ADT_URL=http://127.0.0.1:1
ABAP_ADT_USER=demo
ABAP_ADT_PASSWORD=secret
`,
	)
	testing.expect(t, manifest_has_project_dotenv(&manifest, context.allocator))
}

@(test)
adt_fetched_dependency_input_resolves_remote_candidate :: proc(t: ^testing.T) {
	target := Source_Input {
		uri    = "mem://ZMAIN.abap",
		source = "REPORT zmain. DATA lo_dep TYPE REF TO zcl_adt_fetch.",
	}
	candidates := make([dynamic]Project_Candidate_Input, context.allocator)
	dependencies := make([dynamic]Source_Input, context.allocator)
	object_ref := adt.build_class_object_ref("ZCL_ADT_FETCH", "ZPKG", context.allocator)
	defer adt.object_ref_destroy(&object_ref, context.allocator)
	uri_keys := project_input_uri_keys(target.uri, dependencies[:], candidates[:], 1, context.allocator)

	added := add_adt_fetched_dependency_input(
		&candidates,
		&dependencies,
		Remote_Dependency_Candidate{name = "zcl_adt_fetch", kind = "type"},
		&object_ref,
		"CLASS zcl_adt_fetch DEFINITION. ENDCLASS. CLASS zcl_adt_fetch IMPLEMENTATION. ENDCLASS.",
		"abap",
		&uri_keys,
		context.allocator,
		context.allocator,
	)
	testing.expect(t, added)
	testing.expect_value(t, len(dependencies), 1)

	pool: frontend_runtime.Pool
	testing.expect_value(
		t,
		frontend_runtime.pool_init(&pool, frontend_runtime.Options{worker_count = 0, task_capacity = 128}, context.allocator),
		frontend_runtime.Submit_Error.None,
	)
	project := analyze_target_with_candidate_inputs(
		target,
		candidates[:],
		dependencies[:],
		Analyze_Options{pool = &pool},
		context.allocator,
	)
	frontend_runtime.pool_destroy(&pool)

	testing.expect_value(t, len(project.units), 2)
	testing.expect(t, !project_has_diagnostic(&project, .Unresolved_Reference))
	testing.expect(t, !project_units_have_diagnostic(&project, .Unresolved_Reference))
}

@(test)
manifest_unlisted_reachable_include_joins_root_and_selects_owner :: proc(t: ^testing.T) {
	root := manifest_workspace_path("unlisted-include")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
root_file = "src/ZMAIN.abap"
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain. INCLUDE zinc. lv_inc = 1.")
	include_file := manifest_test_file(t, root, "src/ZINC.abap", "DATA lv_inc TYPE i.")

	root_result := analyze_path_test(t, root_file)
	requested_include_result := analyze_path_test(t, include_file)

	testing.expect(t, root_result.ok)
	testing.expect(t, root_result.used_manifest)
	testing.expect(t, project_unit_by_uri(&root_result.project, include_file) != nil)
	testing.expect(t, requested_include_result.ok)
	testing.expect(t, requested_include_result.used_manifest)
	testing.expect(t, project_unit_by_uri(&requested_include_result.project, root_file) != nil)
	testing.expect(t, project_unit_by_uri(&requested_include_result.project, include_file) != nil)
	testing.expect_value(t, len(requested_include_result.project.units), 2)
}

@(test)
manifest_unclaimed_loose_file_analyzes_standalone :: proc(t: ^testing.T) {
	root := manifest_workspace_path("loose-file")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
root_file = "src/ZMAIN.abap"
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain.")
	loose_file := manifest_test_file(t, root, "src/ZLOOSE.abap", "DATA lv_loose TYPE i.")

	result := analyze_path_test(t, loose_file)

	testing.expect(t, result.ok)
	testing.expect(t, !result.used_manifest)
	testing.expect_value(t, len(result.project.units), 1)
	testing.expect_value(t, result.project.units[0].uri, loose_file)
	testing.expect(t, project_unit_by_uri(&result.project, root_file) == nil)
}

@(test)
manifest_dependency_of_activates_only_selected_root_dependencies :: proc(t: ^testing.T) {
	root := manifest_workspace_path("dependency-of")
	manifest_test_file(
		t,
		root,
		"abapls.toml",
		`
[[unit]]
name = "ZMAIN"
kind = "program"
root_file = "src/ZMAIN.abap"

[[unit]]
name = "ZCL_DEP"
kind = "class"
root_file = "src/ZCL_DEP.abap"
dependency_of = ["src/ZMAIN.abap"]

[[unit]]
name = "ZCL_OTHER"
kind = "class"
root_file = "src/ZCL_OTHER.abap"
dependency_of = ["src/ZOTHER.abap"]
`,
	)
	root_file := manifest_test_file(t, root, "src/ZMAIN.abap", "REPORT zmain. DATA lo_dep TYPE REF TO zcl_dep.")
	dependency_file := manifest_test_file(t, root, "src/ZCL_DEP.abap", "CLASS zcl_dep DEFINITION. ENDCLASS.")
	other_file := manifest_test_file(t, root, "src/ZCL_OTHER.abap", "CLASS zcl_other DEFINITION. ENDCLASS.")

	result := analyze_path_test(t, root_file)
	root_unit := project_unit_by_uri(&result.project, root_file)

	testing.expect(t, result.ok)
	testing.expect(t, result.used_manifest)
	testing.expect(t, root_unit != nil)
	testing.expect(t, project_unit_by_uri(&result.project, dependency_file) != nil)
	testing.expect(t, project_unit_by_uri(&result.project, other_file) == nil)
	testing.expect(t, reference_resolves_to_uri(&result.project, root_unit, "zcl_dep", .Type, .Type_Ref, dependency_file))
	testing.expect(t, !has_diagnostic(root_unit, .Unresolved_Reference))
}

@(test)
analyze_target_inline_pool_discovers_reachable_includes_only :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: ztop, zf01.",
	}
	candidates := [?]Source_Input {
		{uri = "file:///workspace/ztop.abap", source = "DATA gv_shared TYPE i."},
		{uri = "file:///workspace/zf01.abap", source = "FORM run. gv_shared = 1. ENDFORM."},
		{uri = "file:///workspace/zunused.abap", source = "DATA gv_unused TYPE i."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])

	testing.expect_value(t, len(project.units), 3)
	testing.expect_value(t, project.units[0].uri, target.uri)
	testing.expect_value(t, project.units[1].uri, candidates[0].uri)
	testing.expect_value(t, project.units[2].uri, candidates[1].uri)
	testing.expect(t, project_unit_by_uri(&project, candidates[2].uri) == nil)
	root := project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil)
	testing.expect_value(t, include_target_uri(&project, root, "ztop"), candidates[0].uri)
	testing.expect_value(t, include_target_uri(&project, root, "zf01"), candidates[1].uri)
	testing.expect(t, !project_has_diagnostic(&project, .Unresolved_Include))
}

@(test)
analyze_target_threaded_pool_prefers_includes_folder_candidate :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/src/ZREP/ZREP.abap",
		source = "REPORT zrep. INCLUDE zrep_top.",
	}
	includes_uri := "file:///workspace/src/ZREP/Includes/ZREP_TOP.abap"
	candidates := [?]Source_Input {
		{uri = "file:///workspace/src/includes/ZREP_TOP.abap", source = "DATA lv_global TYPE i."},
		{uri = includes_uri, source = "DATA lv_includes TYPE i."},
	}

	project := analyze_project_test(t, 2, target, candidates[:])
	root := project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil)
	testing.expect_value(t, include_target_uri(&project, root, "zrep_top"), includes_uri)
	testing.expect_value(t, len(project.units), 2)
}

@(test)
analyze_target_prefers_same_folder_before_includes_folder :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/src/ZREP/ZREP.abap",
		source = "REPORT zrep. INCLUDE zrep_top.",
	}
	same_folder_uri := "file:///workspace/src/ZREP/ZREP_TOP.abap"
	candidates := [?]Source_Input {
		{uri = "file:///workspace/src/ZREP/Includes/ZREP_TOP.abap", source = "DATA lv_includes TYPE i."},
		{uri = same_folder_uri, source = "DATA lv_same_folder TYPE i."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	root := project_unit_by_uri(&project, target.uri)
	testing.expect(t, root != nil)
	testing.expect_value(t, include_target_uri(&project, root, "zrep_top"), same_folder_uri)
	testing.expect_value(t, len(project.units), 2)
}

@(test)
analyze_target_ignores_sibling_candidate_without_include_edge :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. gr_demo = 1.",
	}
	candidates := [?]Source_Input {
		{uri = "file:///workspace/zmain_top.abap", source = "DATA gr_demo TYPE i."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])

	testing.expect_value(t, len(project.units), 1)
	testing.expect(t, project_unit_by_uri(&project, candidates[0].uri) == nil)
	testing.expect(t, len(project.units[0].include_edges) == 0)
}

@(test)
project_global_class_resolves_when_name_matches_unit_stem :: proc(t: ^testing.T) {
	sources := [?]Source_Input {
		{
			uri = "file:///workspace/zcl_parent.abap",
			source = "CLASS zcl_parent DEFINITION. ENDCLASS.",
		},
		{
			uri = "file:///workspace/zconsumer.abap",
			source = "DATA lo_parent TYPE REF TO zcl_parent.",
		},
	}

	project := analyze_units_project_test(t, sources[:])
	consumer := project_unit_by_uri(&project, sources[1].uri)

	testing.expect(t, consumer != nil)
	testing.expect(t, reference_resolves_to_uri(&project, consumer, "zcl_parent", .Type, .Type_Ref, sources[0].uri))
	testing.expect(t, !has_diagnostic(consumer, .Unresolved_Reference))
}

@(test)
project_program_local_class_without_prefix_stays_unit_local :: proc(t: ^testing.T) {
	sources := [?]Source_Input {
		{
			uri = "file:///workspace/zprogram_top.abap",
			source = "CLASS zcl_helper DEFINITION. ENDCLASS.",
		},
		{
			uri = "file:///workspace/zconsumer.abap",
			source = "DATA lo_helper TYPE REF TO zcl_helper.",
		},
	}

	project := analyze_units_project_test(t, sources[:])
	consumer := project_unit_by_uri(&project, sources[1].uri)

	testing.expect(t, consumer != nil)
	testing.expect(t, !reference_resolves_to_uri(&project, consumer, "zcl_helper", .Type, .Type_Ref, sources[0].uri))
	testing.expect(t, has_diagnostic(consumer, .Unresolved_Reference))
}

@(test)
analyze_target_reports_unresolved_include :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "INCLUDE zmissing.",
	}

	project := analyze_project_test(t, 0, target, nil)

	testing.expect_value(t, len(project.units), 1)
	testing.expect(t, project_has_diagnostic(&project, .Unresolved_Include))
	testing.expect(t, has_diagnostic(&project.units[0], .Unresolved_Include))
}

@(test)
analyze_target_reports_include_cycle :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "INCLUDE za.",
	}
	candidates := [?]Source_Input {
		{uri = "file:///workspace/za.abap", source = "INCLUDE zb."},
		{uri = "file:///workspace/zb.abap", source = "INCLUDE za."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])

	testing.expect_value(t, len(project.units), 3)
	testing.expect(t, project_has_diagnostic(&project, .Include_Cycle))
}

@(test)
analyze_target_resolves_symbols_from_included_units :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "INCLUDE zinc. lv_inc = 1.",
	}
	candidates := [?]Source_Input {
		{uri = "file:///workspace/zinc.abap", source = "DATA lv_inc TYPE i."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	root := project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect_value(t, include_target_uri(&project, root, "zinc"), candidates[0].uri)
	testing.expect(t, reference_resolves_to_uri(&project, root, "lv_inc", .Value, .Identifier, candidates[0].uri))
}

@(test)
analyze_target_closes_nested_explicit_includes :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "INCLUDE za. gv_leaf = 1.",
	}
	candidates := [?]Source_Input {
		{uri = "file:///workspace/za.abap", source = "INCLUDE zb."},
		{uri = "file:///workspace/zb.abap", source = "DATA gv_leaf TYPE i."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	root := project_unit_by_uri(&project, target.uri)
	za := project_unit_by_uri(&project, candidates[0].uri)

	testing.expect_value(t, len(project.units), 3)
	testing.expect(t, root != nil)
	testing.expect(t, za != nil)
	testing.expect_value(t, include_target_uri(&project, root, "za"), candidates[0].uri)
	testing.expect_value(t, include_target_uri(&project, za, "zb"), candidates[1].uri)
	testing.expect(t, reference_resolves_to_uri(&project, root, "gv_leaf", .Value, .Identifier, candidates[1].uri))
}

@(test)
analyze_target_included_units_share_compilation_context :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: ztop, zf01.",
	}
	candidates := [?]Source_Input {
		{uri = "file:///workspace/ztop.abap", source = "DATA gv_shared TYPE i."},
		{uri = "file:///workspace/zf01.abap", source = "FORM run. gv_shared = 1. ENDFORM."},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	form := project_unit_by_uri(&project, candidates[1].uri)

	testing.expect(t, form != nil)
	testing.expect(t, reference_resolves_to_uri(&project, form, "gv_shared", .Value, .Identifier, candidates[0].uri))
}

@(test)
analyze_target_reports_type_declared_in_later_include :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: zdata, ztypes.",
	}
	candidates := [?]Source_Input {
		{uri = "file:///workspace/zdata.abap", source = "DATA ls_object_src TYPE ts_obj_ids."},
		{uri = "file:///workspace/ztypes.abap", source = `
TYPES: BEGIN OF ts_obj_ids,
         owner TYPE char12,
       END OF ts_obj_ids.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	data := project_unit_by_uri(&project, candidates[0].uri)

	testing.expect(t, data != nil)
	testing.expect(t, has_diagnostic(data, .Unresolved_Reference))
}

@(test)
analyze_target_accepts_type_declared_in_prior_include :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE ztypes. DATA ls_object_src TYPE ts_obj_ids.",
	}
	candidates := [?]Source_Input {
		{uri = "file:///workspace/ztypes.abap", source = `
TYPES: BEGIN OF ts_obj_ids,
         owner TYPE char12,
       END OF ts_obj_ids.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	root := project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, !has_diagnostic(root, .Unresolved_Reference))
}

@(test)
analyze_target_links_class_definition_and_implementation_across_ordered_includes :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = `
REPORT zmain.
INCLUDE: ztop, zcls.
START-OF-SELECTION.
  CREATE OBJECT gr_demo.
  CALL METHOD gr_demo->get_data.
`,
	}
	candidates := [?]Source_Input {
		{uri = "file:///workspace/ztop.abap", source = `
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS get_data.
ENDCLASS.
`},
		{uri = "file:///workspace/zcls.abap", source = `
DATA gr_demo TYPE REF TO lcl_demo.
CLASS lcl_demo IMPLEMENTATION.
  METHOD get_data.
  ENDMETHOD.
ENDCLASS.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	top := project_unit_by_uri(&project, candidates[0].uri)
	cls := project_unit_by_uri(&project, candidates[1].uri)
	class_symbol: ^Symbol_Data
	member: ^Class_Member_Data
	if top != nil {
		class_symbol = find_symbol(top, "lcl_demo", .Class)
		if class_symbol != nil {
			member = class_member_named(top, class_symbol.id, "get_data", .Method)
		}
	}

	testing.expect(t, top != nil)
	testing.expect(t, cls != nil)
	testing.expect(t, class_symbol != nil)
	testing.expect(t, member != nil)
	testing.expect(t, .Has_Implementation in member.flags)
	testing.expect_value(t, member.implementation.unit, cls.unit_id)
	testing.expect(t, !has_diagnostic(top, .Missing_Method_Implementation))
}

@(test)
analyze_target_imports_structure_types_across_includes :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: ztop, zf01.",
	}
	candidates := [?]Source_Input {
		{uri = "file:///workspace/ztop.abap", source = `
TYPES: BEGIN OF ty_row,
         comp TYPE string,
       END OF ty_row.
DATA gs_row TYPE ty_row.
`},
		{uri = "file:///workspace/zf01.abap", source = `
FORM run.
  DATA lv_comp TYPE string.
  lv_comp = gs_row-comp.
ENDFORM.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	form := project_unit_by_uri(&project, candidates[1].uri)

	testing.expect(t, form != nil)
	testing.expect(t, reference_resolves_to_uri(&project, form, "gs_row", .Value, .Identifier, candidates[0].uri))
	testing.expect(t, !has_diagnostic(form, .Unknown_Field))
}

@(test)
analyze_target_reclassifies_open_sql_predicate_globals_from_prior_include :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = "REPORT zmain. INCLUDE: ztop, zf01.",
	}
	candidates := [?]Source_Input {
		{uri = "file:///workspace/ztop.abap", source = `
DATA p_lgnum TYPE string.
DATA p_lgtyp TYPE string.
DATA p_lgpla TYPE string.
`},
		{uri = "file:///workspace/zf01.abap", source = `
FORM run.
  DATA lw_lgpla TYPE string.
  DATA lw_skzsi TYPE string.
  SELECT SINGLE lgpla skzsi
    FROM lagp
    INTO (lw_lgpla, lw_skzsi)
    WHERE lgnum = p_lgnum
      AND lgtyp = p_lgtyp
      AND lgpla = p_lgpla.
ENDFORM.
`},
	}

	project := analyze_project_test(t, 0, target, candidates[:])
	form := project_unit_by_uri(&project, candidates[1].uri)

	testing.expect(t, form != nil)
	names := [?]string{"p_lgnum", "p_lgtyp", "p_lgpla"}
	for name in names {
		testing.expect(t, !sql_name_ref_present(form, name, .Column))
		testing.expect(t, reference_resolves_to_uri(&project, form, name, .Value, .Identifier, candidates[0].uri))
	}
}

@(test)
validates_project_object_type_method_implementation_and_inherited_visibility :: proc(t: ^testing.T) {
	target := Source_Input {
		uri = "file:///workspace/zmain.abap",
		source = `
CLASS lcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS run.
  PRIVATE SECTION.
    METHODS secret.
ENDCLASS.

CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
  PUBLIC SECTION.
    METHODS own.
ENDCLASS.

CLASS lcl_parent IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
  METHOD secret.
  ENDMETHOD.
ENDCLASS.

DATA lo_child TYPE REF TO lcl_child.
DATA lo_bad TYPE lcl_child.

START-OF-SELECTION.
  CALL METHOD lo_child->run.
  CALL METHOD lo_child->secret.
`,
	}

	project := analyze_project_test(t, 0, target, nil)
	root := project_unit_by_uri(&project, target.uri)

	testing.expect(t, root != nil)
	testing.expect(t, has_diagnostic(root, .Invalid_Object_Type_Reference))
	testing.expect(t, has_diagnostic(root, .Missing_Method_Implementation))
	testing.expect(t, has_diagnostic(root, .Unknown_Field))
}
