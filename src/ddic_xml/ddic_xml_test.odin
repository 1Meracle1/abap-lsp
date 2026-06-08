package ddic_xml

import "core:strings"
import "core:testing"

expect_contains_fold :: proc(t: ^testing.T, source, needle: string) {
	lower := strings.to_lower(source, context.allocator)
	defer delete(lower, context.allocator)
	testing.expect(t, strings.contains(lower, needle))
}

@(test)
table_dependency_source_uses_observed_element_info_fields :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="ztab" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="mandt">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">MANDT</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">clnt</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="counter">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">int4</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	source := dependency_source("ZTAB", "ddic-table", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "types: begin of ztab")
	expect_contains_fold(t, source, "mandt type mandt")
	expect_contains_fold(t, source, "counter type i")
}

@(test)
table_dependency_source_escapes_include_field_name :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="d010inc" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="master">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">master</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="include">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">include</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	source := dependency_source("D010INC", "ddic-table", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "!include type include")
}

@(test)
structure_dependency_source_uses_observed_element_info_fields :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name="zstr" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="id">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">ZDE_ID</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">char</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	source := dependency_source("ZSTR", "ddic-structure", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "types: begin of zstr")
	expect_contains_fold(t, source, "id type zde_id")
}

@(test)
structure_dependency_source_ignores_elementinfo_include_without_group :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name="/sttp/s_proc_evtt" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name=".include">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicIncludeName">/sttp/s_proc_evt</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="evttime">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">/sttp/e_timestamp_evt</abapsource:entry>
      <abapsource:entry abapsource:key="ddicIsPartOfInclude">/sttp/s_proc_evt</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name=".include">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicIsPartOfInclude">/sttp/s_proc_evt</abapsource:entry>
      <abapsource:entry abapsource:key="ddicIncludeName">/sttp/s_dm_evt_dat_code</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	source := dependency_source("/sttp/s_proc_evtt", "ddic-structure", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "evttime type /sttp/e_timestamp_evt")
	lower := strings.to_lower(source, context.allocator)
	defer delete(lower, context.allocator)
	testing.expect(t, !strings.contains(lower, "include type /sttp/s_proc_evt as proc_evt"))
	testing.expect(t, !strings.contains(lower, "include type /sttp/s_dm_evt_dat_code"))
}

@(test)
structure_dependency_source_uses_ddic_source_include_group :: proc(t: ^testing.T) {
	source := dependency_source(
		"/sttp/s_proc_evtt",
		"ddic-structure",
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

	expect_contains_fold(t, source, "include type /sttp/s_proc_evt as proc_evt")
	expect_contains_fold(t, source, "parentobject type /sttp/e_objcode")
	expect_contains_fold(t, source, "include type /sttp/s_extra_evt")
	expect_contains_fold(t, source, "ext_xmlx type xstring")
}

@(test)
structure_dependency_source_strips_ddic_source_not_null :: proc(t: ^testing.T) {
	source := dependency_source(
		"SWW_WIHEAD",
		"ddic-structure",
		`define type sww_wihead {
  wi_id  : sww_wiid not null;
  handle : include swt_handle not null;
  include swd_protcl not null;
}`,
		context.allocator,
	)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "wi_id type sww_wiid")
	expect_contains_fold(t, source, "include type swt_handle as handle")
	expect_contains_fold(t, source, "include type swd_protcl")
	lower := strings.to_lower(source, context.allocator)
	defer delete(lower, context.allocator)
	testing.expect(t, !strings.contains(lower, "not null"))
}

@(test)
structure_dependency_source_strips_ddic_source_key_modifier :: proc(t: ^testing.T) {
	source := dependency_source(
		"DD03P",
		"ddic-structure",
		`define type dd03p {
  key tabname    : tabname
    with foreign key [1..*,1] dd02l
      where tabname = dd03p.tabname;
  key fieldname  : fieldname;
}`,
		context.allocator,
	)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "tabname type tabname")
	expect_contains_fold(t, source, "fieldname type fieldname")
	lower := strings.to_lower(source, context.allocator)
	defer delete(lower, context.allocator)
	testing.expect(t, !strings.contains(lower, "key tabname"))
	testing.expect(t, !strings.contains(lower, "foreign key"))
}

@(test)
structure_dependency_source_ignores_ddic_source_include_extensions :: proc(t: ^testing.T) {
	source := dependency_source(
		"SWD_SNODES",
		"ddic-structure",
		`define type swd_snodes {
  include swd_rnodes
    extend evt_otype :
      remove foreign key;
  crl_elem : swc_elem;
}`,
		context.allocator,
	)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "types: begin of swd_snodes")
	expect_contains_fold(t, source, "include type swd_rnodes")
	expect_contains_fold(t, source, "crl_elem type swc_elem")
	lower := strings.to_lower(source, context.allocator)
	defer delete(lower, context.allocator)
	testing.expect(t, !strings.contains(lower, "extend evt_otype"))
}

@(test)
view_dependency_source_uses_observed_element_info_fields :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="VIEW/DV" adtcore:name="zview" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="text">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">string</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	source := dependency_source("ZVIEW", "ddic-view", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "types: begin of zview")
	expect_contains_fold(t, source, "text type string")
}

@(test)
data_element_dependency_source_uses_dtel_data_type :: proc(t: ^testing.T) {
	xml := `<blue:wbobj adtcore:name="zde_text" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>predefinedAbapType</dtel:typeKind>
    <dtel:typeName></dtel:typeName>
    <dtel:dataType>STRING</dtel:dataType>
    <dtel:dataTypeLength>0</dtel:dataTypeLength>
    <dtel:dataTypeDecimals>0</dtel:dataTypeDecimals>
  </dtel:dataElement>
</blue:wbobj>`
	source := dependency_source("ZDE_TEXT", "ddic-data-element", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "types zde_text type string")
}

@(test)
data_element_dependency_source_trims_formatted_decfloat_type :: proc(t: ^testing.T) {
	xml := `<blue:wbobj adtcore:name="zde_decfloat" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>
      predefinedAbapType
    </dtel:typeKind>
    <dtel:dataType>
      DF34_RAW
    </dtel:dataType>
  </dtel:dataElement>
</blue:wbobj>`
	source := dependency_source("ZDE_DECFLOAT", "ddic-data-element", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "types zde_decfloat type decfloat34")
}

@(test)
data_element_dependency_source_maps_long_char_type :: proc(t: ^testing.T) {
	xml := `<blue:wbobj adtcore:name="xuvals" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>
      domain
    </dtel:typeKind>
    <dtel:dataType>
      LCHR
    </dtel:dataType>
  </dtel:dataElement>
</blue:wbobj>`
	source := dependency_source("XUVALS", "ddic-data-element", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "types xuvals type c")
}

@(test)
data_element_dependency_source_maps_long_raw_type :: proc(t: ^testing.T) {
	xml := `<blue:wbobj adtcore:name="indx_clust" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>domain</dtel:typeKind>
    <dtel:dataType>LRAW</dtel:dataType>
  </dtel:dataElement>
</blue:wbobj>`
	source := dependency_source("INDX_CLUST", "ddic-data-element", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "types indx_clust type xstring")
}

@(test)
data_element_dependency_source_uses_clif_reference_type :: proc(t: ^testing.T) {
	xml := `<blue:wbobj adtcore:name="zde_ref" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>refToClifType</dtel:typeKind>
    <dtel:typeName>ZIF_REF</dtel:typeName>
    <dtel:dataType></dtel:dataType>
  </dtel:dataElement>
</blue:wbobj>`
	source := dependency_source("ZDE_REF", "ddic-data-element", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "types zde_ref type ref to zif_ref")
}

@(test)
data_element_dependency_source_uses_dictionary_reference_type :: proc(t: ^testing.T) {
	xml := `<blue:wbobj adtcore:name="sxsltdref" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>refToDictionaryType</dtel:typeKind>
    <dtel:typeName>DATA</dtel:typeName>
    <dtel:dataType/>
  </dtel:dataElement>
</blue:wbobj>`
	source := dependency_source("SXSLTDREF", "ddic-data-element", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "types sxsltdref type ref to data")
}

@(test)
table_type_dependency_source_uses_named_row_type :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TTYP/DA" adtcore:name="zrows" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:properties>
    <abapsource:entry abapsource:key="ddicAccessMode">Not Specified</abapsource:entry>
  </abapsource:properties>
  <abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name="ZROW">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicRowType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	source := dependency_source("ZROWS", "ddic-table-type", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "type standard table of zrow with default key")
}

@(test)
table_type_dependency_source_uses_named_reference_type :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TTYP/DA" adtcore:name="zrefs" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="INTF/OI" adtcore:name="ZIF_REF">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicReferenceType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	source := dependency_source("ZREFS", "ddic-table-type", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "type standard table of ref to zif_ref with default key")
}

@(test)
table_type_dependency_source_uses_string_for_anonymous_row_type :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TTYP/DA" adtcore:name="zstrings" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo>
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicRowType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	source := dependency_source("ZSTRINGS", "ddic-table-type", xml, context.allocator)
	defer delete(source, context.allocator)

	expect_contains_fold(t, source, "type standard table of string with default key")
}

@(test)
data_element_dependency_source_does_not_invent_empty_data_type :: proc(t: ^testing.T) {
	xml := `<blue:wbobj adtcore:name="zde_ref" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>refToDictionaryType</dtel:typeKind>
    <dtel:typeName></dtel:typeName>
    <dtel:dataType></dtel:dataType>
  </dtel:dataElement>
</blue:wbobj>`
	source := dependency_source("ZDE_REF", "ddic-data-element", xml, context.allocator)
	defer delete(source, context.allocator)

	testing.expect_value(t, source, "")
}
