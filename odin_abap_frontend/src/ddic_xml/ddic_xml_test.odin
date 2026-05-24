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

	expect_contains_fold(t, source, "type standard table of zif_ref with default key")
}

@(test)
table_type_dependency_source_does_not_invent_unnamed_row_type :: proc(t: ^testing.T) {
	xml := `<abapsource:elementInfo adtcore:type="TTYP/DA" adtcore:name="zstrings" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo>
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicRowType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>`
	source := dependency_source("ZSTRINGS", "ddic-table-type", xml, context.allocator)
	defer delete(source, context.allocator)

	testing.expect_value(t, source, "")
}

@(test)
data_element_dependency_source_does_not_invent_empty_data_type :: proc(t: ^testing.T) {
	xml := `<blue:wbobj adtcore:name="zde_ref" adtcore:type="DTEL/DE" xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel" xmlns:adtcore="http://www.sap.com/adt/core" xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
  <dtel:dataElement>
    <dtel:typeKind>refToDictionaryType</dtel:typeKind>
    <dtel:typeName>SYUNAME</dtel:typeName>
    <dtel:dataType></dtel:dataType>
  </dtel:dataElement>
</blue:wbobj>`
	source := dependency_source("ZDE_REF", "ddic-data-element", xml, context.allocator)
	defer delete(source, context.allocator)

	testing.expect_value(t, source, "")
}
