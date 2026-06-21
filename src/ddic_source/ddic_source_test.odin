package ddic_source

import "core:strings"
import "core:testing"

expect_contains_fold :: proc(t: ^testing.T, source, needle: string) {
	lower := strings.to_lower(source, context.allocator)
	defer delete(lower, context.allocator)
	testing.expect(t, strings.contains(lower, needle))
}

expect_not_contains_fold :: proc(t: ^testing.T, source, needle: string) {
	lower := strings.to_lower(source, context.allocator)
	defer delete(lower, context.allocator)
	testing.expect(t, !strings.contains(lower, needle))
}

@(test)
parse_define_type_members_from_tokens :: proc(t: ^testing.T) {
	parsed := parse(
		`@EndUserText.label : 'Structure'
define type dd03p {
  key tabname : tabname
    with foreign key [1..*,1] dd02l
      where tabname = dd03p.tabname;
  fieldname : fieldname;
}`,
		context.allocator,
	)

	testing.expect_value(t, parsed.definition.name, "dd03p")
	testing.expect_value(t, len(parsed.definition.annotations), 1)
	testing.expect_value(t, parsed.definition.annotations[0].name, "EndUserText.label")
	testing.expect_value(t, parsed.definition.annotations[0].value, "Structure")
	testing.expect_value(t, len(parsed.definition.members), 2)
	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, parsed.definition.members[0].kind, Member_Kind.Field)
	testing.expect_value(t, len(parsed.definition.members[0].clauses), 2)
	testing.expect_value(t, parsed.definition.members[0].clauses[0].kind, Clause_Kind.Foreign_Key)
	testing.expect_value(t, parsed.definition.members[0].clauses[1].kind, Clause_Kind.Where)
	testing.expect_value(t, parsed.definition.members[0].key, true)
	testing.expect_value(t, parsed.definition.members[0].name, "tabname")
	testing.expect_value(t, parsed.definition.members[0].type_ref.name, "tabname")
	testing.expect_value(t, parsed.definition.members[1].name, "fieldname")
}

@(test)
dependency_source_formats_structure_with_descriptions_and_key_comments :: proc(t: ^testing.T) {
	source := dependency_source(
		`@EndUserText.label : 'Change & Transport System: Header of Requests/Tasks'
define type e070 {
  @EndUserText.label : 'Request/Task'
  key trkorr : trkorr;
  @EndUserText.label : 'Function'
  trfunction : trfunction;
}`,
		context.allocator,
	)
	defer delete(source, context.allocator)

	testing.expect_value(
		t,
		source,
		`" Change & Transport System: Header of Requests/Tasks
TYPES:
  BEGIN OF e070, " Change & Transport System: Header of Requests/Tasks
    trkorr TYPE trkorr, " key field; Request/Task
    trfunction TYPE trfunction, " Function
  END OF e070.
`,
	)
}

@(test)
dependency_source_preserves_named_and_standalone_includes :: proc(t: ^testing.T) {
	source := dependency_source(
		`@EndUserText.label : 'Event Processing Structure: Transaction Event'
define type /sttp/s_proc_evtt {
  proc_evt           : include /sttp/s_proc_evt;
  parentobject       : /sttp/e_objcode;
  include /sttp/s_extra_evt;
  ext_xmlx           : abap.rawstring(0);
}`,
		context.allocator,
	)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "types:\n  begin of /sttp/s_proc_evtt")
	expect_contains_fold(t, source, "include type /sttp/s_proc_evt as proc_evt")
	expect_contains_fold(t, source, "parentobject type /sttp/e_objcode")
	expect_contains_fold(t, source, "include type /sttp/s_extra_evt")
	expect_contains_fold(t, source, "ext_xmlx type xstring")
}

@(test)
dependency_source_ignores_metadata_clauses_without_line_slicing :: proc(t: ^testing.T) {
	source := dependency_source(
		`define type dd03p {
  @AbapCatalog.foreignKey.keyType : #NON_KEY
  key tabname    : tabname
    with foreign key [1..*,1] dd02l
      where tabname = dd03p.tabname;
  key rollname   : rollname
    with value help dd_type_for_field
      where datatype = dd02d.datatype
        and fieldname = dd03p.fieldname;
  key fieldname  : fieldname;
}`,
		context.allocator,
	)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "tabname type tabname")
	expect_contains_fold(t, source, "rollname type rollname")
	expect_contains_fold(t, source, "fieldname type fieldname")
	expect_contains_fold(t, source, `tabname type tabname, " key field`)
	expect_contains_fold(t, source, `rollname type rollname, " key field`)
	expect_contains_fold(t, source, `fieldname type fieldname, " key field`)
	expect_not_contains_fold(t, source, "key tabname")
	expect_not_contains_fold(t, source, "key fields:")
	expect_not_contains_fold(t, source, "foreign key")
	expect_not_contains_fold(t, source, "value help")
}

@(test)
dependency_source_parses_nullability_references_and_include_extensions :: proc(t: ^testing.T) {
	source := dependency_source(
		`define type swd_snodes {
  include swd_rnodes
    extend evt_otype :
      remove foreign key;
  handle : include swt_handle not null;
  ref    : reference to zif_ref;
  value  : abap.dec(15,2) not null;
}`,
		context.allocator,
	)
	parsed := parse(
		`define type swd_snodes {
  include swd_rnodes
    extend evt_otype :
      remove foreign key;
  handle : include swt_handle not null;
  ref    : reference to zif_ref;
  value  : abap.dec(15,2) not null;
}`,
		context.allocator,
	)
	defer delete(source, context.allocator)

	testing.expect_value(t, len(parsed.definition.members[0].clauses), 2)
	testing.expect_value(t, parsed.definition.members[0].clauses[0].kind, Clause_Kind.Extend)
	testing.expect_value(t, parsed.definition.members[0].clauses[1].kind, Clause_Kind.Remove_Foreign_Key)
	testing.expect_value(t, parsed.definition.members[1].clauses[0].kind, Clause_Kind.Not_Null)
	expect_contains_fold(t, source, "include type swd_rnodes")
	expect_contains_fold(t, source, "include type swt_handle as handle")
	expect_contains_fold(t, source, "ref type ref to zif_ref")
	expect_contains_fold(t, source, "value type p")
	expect_not_contains_fold(t, source, "not null")
	expect_not_contains_fold(t, source, "remove foreign key")
}
