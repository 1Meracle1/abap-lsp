package ddic_xml

import xml_doc "core:encoding/xml"
import "core:mem"
import "core:strings"

Ddic_Xml_Kind :: enum {
	Unknown,
	Alias,
	Structure,
	Table_Type,
}

Ddic_Xml_Field :: struct {
	name:      string,
	type_name: string,
}

Ddic_Xml_Type_Ref :: struct {
	name:   string,
	is_ref: bool,
}

dependency_source :: proc(
	name, object_kind, source: string,
	allocator: mem.Allocator,
) -> string {
	doc, parsed := ddic_xml_parse(source, allocator)
	kind := ddic_xml_kind(object_kind, doc)
	out: string

	if kind == .Structure || kind == .Unknown {
		fields := ddic_xml_fields(doc, allocator)
		if len(fields) > 0 {
			out = ddic_xml_structure_source(name, fields[:], allocator)
		}
		delete(fields)
	}

	if len(out) == 0 {
		line := ddic_xml_table_line_type(doc)
		if len(line.name) != 0 && (kind == .Table_Type || kind == .Unknown) {
			out = ddic_xml_table_type_source(name, line, allocator)
		}
	}

	if len(out) == 0 && (kind == .Alias || kind == .Unknown) {
		base_type := ddic_xml_data_element_type(doc)
		if len(base_type.name) != 0 {
			out = ddic_xml_type_alias_source(name, base_type, allocator)
		}
	}

	if parsed {
		xml_doc.destroy(doc, allocator)
	}
	return out
}

ddic_xml_parse_error_ignore :: proc(pos: xml_doc.Pos, fmt: string, args: ..any) {}

ddic_xml_parse :: proc(source: string, allocator: mem.Allocator) -> (^xml_doc.Document, bool) {
	doc, err := xml_doc.parse(
		source,
		xml_doc.DEFAULT_OPTIONS,
		"",
		ddic_xml_parse_error_ignore,
		allocator,
	)
	if err != .None {
		xml_doc.destroy(doc, allocator)
		return nil, false
	}
	return doc, true
}

ddic_xml_kind :: proc(object_kind: string, doc: ^xml_doc.Document) -> Ddic_Xml_Kind {
	switch {
	case strings.equal_fold(object_kind, "ddic-table-type"):
		return .Table_Type
	case strings.equal_fold(object_kind, "ddic-structure") ||
	     strings.equal_fold(object_kind, "ddic-table") ||
	     strings.equal_fold(object_kind, "ddic-view"):
		return .Structure
	case strings.equal_fold(object_kind, "ddic-data-element") ||
	     strings.equal_fold(object_kind, "ddic-domain"):
		return .Alias
	}

	if doc == nil || len(doc.elements) == 0 {
		return .Unknown
	}
	root := doc.elements[0]
	root_type := ddic_xml_attr_value(doc, 0, "type")
	switch {
	case ddic_xml_name_equal(root.ident, "wbobj") && strings.equal_fold(root_type, "DTEL/DE"):
		return .Alias
	case ddic_xml_name_equal(root.ident, "elementInfo"):
		switch {
		case strings.equal_fold(root_type, "TTYP/DA"):
			return .Table_Type
		case strings.equal_fold(root_type, "TABL/DT") ||
		     strings.equal_fold(root_type, "TABL/DS") ||
		     strings.equal_fold(root_type, "VIEW/DV"):
			return .Structure
		}
	}
	return .Unknown
}

ddic_xml_type_alias_source :: proc(
	name: string,
	type_ref: Ddic_Xml_Type_Ref,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "TYPES ")
	write_canonical_abap_name(&out, name)
	strings.write_string(&out, " TYPE ")
	if type_ref.is_ref {
		strings.write_string(&out, "REF TO ")
	}
	write_canonical_abap_name(&out, type_ref.name)
	strings.write_string(&out, ".\n")
	return strings.to_string(out)
}

ddic_xml_table_type_source :: proc(
	name: string,
	line: Ddic_Xml_Type_Ref,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "TYPES ")
	write_canonical_abap_name(&out, name)
	strings.write_string(&out, " TYPE STANDARD TABLE OF ")
	if line.is_ref {
		strings.write_string(&out, "REF TO ")
	}
	write_canonical_abap_name(&out, line.name)
	strings.write_string(&out, " WITH DEFAULT KEY.\n")
	return strings.to_string(out)
}

ddic_xml_structure_source :: proc(
	name: string,
	fields: []Ddic_Xml_Field,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "TYPES: BEGIN OF ")
	write_canonical_abap_name(&out, name)
	strings.write_string(&out, ",\n")
	for field in fields {
		strings.write_string(&out, "         ")
		write_canonical_abap_name(&out, field.name)
		strings.write_string(&out, " TYPE ")
		write_canonical_abap_name(&out, field.type_name)
		strings.write_string(&out, ",\n")
	}
	strings.write_string(&out, "       END OF ")
	write_canonical_abap_name(&out, name)
	strings.write_string(&out, ".\n")
	return strings.to_string(out)
}

write_canonical_abap_name :: #force_inline proc(out: ^strings.Builder, name: string) {
	for r in name {
		strings.write_rune(out, r)
	}
}

ddic_xml_fields :: proc(
	doc: ^xml_doc.Document,
	allocator: mem.Allocator,
) -> [dynamic]Ddic_Xml_Field {
	fields := make([dynamic]Ddic_Xml_Field, 0, 4, allocator)
	if doc == nil ||
	   len(doc.elements) == 0 ||
	   !ddic_xml_name_equal(doc.elements[0].ident, "elementInfo") {
		return fields
	}
	for value in doc.elements[0].value {
		child_id, ok := value.(xml_doc.Element_ID)
		if !ok ||
		   doc.elements[child_id].kind != .Element ||
		   !ddic_xml_name_equal(doc.elements[child_id].ident, "elementInfo") ||
		   !strings.equal_fold(ddic_xml_attr_value(doc, child_id, "type"), "TABL/DTF") {
			continue
		}
		name := ddic_xml_attr_value(doc, child_id, "name")
		if name != "" {
			ddic_xml_append_field(&fields, name, ddic_xml_field_type(doc, child_id))
		}
	}
	return fields
}

ddic_xml_field_type :: proc(doc: ^xml_doc.Document, id: xml_doc.Element_ID) -> string {
	if data_element := ddic_xml_entry_text(doc, id, "ddicDataElement"); data_element != "" {
		return data_element
	}
	data_type := ddic_xml_entry_text(doc, id, "ddicDataType")
	if builtin := ddic_builtin_type(data_type); builtin != "" {
		return builtin
	}
	return data_type
}

ddic_xml_append_field :: proc(fields: ^[dynamic]Ddic_Xml_Field, name, type_name: string) {
	for field in fields^ {
		if strings.equal_fold(field.name, name) {
			return
		}
	}
	if type_name == "" {
		return
	}
	append(fields, Ddic_Xml_Field{name = name, type_name = type_name})
}

ddic_xml_table_line_type :: proc(doc: ^xml_doc.Document) -> Ddic_Xml_Type_Ref {
	if doc == nil ||
	   len(doc.elements) == 0 ||
	   !ddic_xml_name_equal(doc.elements[0].ident, "elementInfo") {
		return {}
	}
	child_id, ok := ddic_xml_direct_child(doc, 0, "elementInfo")
	if !ok {
		return {}
	}
	is_ref := strings.equal_fold(ddic_xml_entry_text(doc, child_id, "ddicReferenceType"), "X")
	if !is_ref && !strings.equal_fold(ddic_xml_entry_text(doc, child_id, "ddicRowType"), "X") {
		return {}
	}
	name := ddic_xml_attr_value(doc, child_id, "name")
	if name == "" && !is_ref {
		name = "string"
	}
	return Ddic_Xml_Type_Ref{name = name, is_ref = is_ref}
}

ddic_xml_data_element_type :: proc(doc: ^xml_doc.Document) -> Ddic_Xml_Type_Ref {
	if doc == nil ||
	   len(doc.elements) == 0 ||
	   !ddic_xml_name_equal(doc.elements[0].ident, "wbobj") ||
	   !strings.equal_fold(ddic_xml_attr_value(doc, 0, "type"), "DTEL/DE") {
		return {}
	}
	data_element_id, ok := ddic_xml_direct_child(doc, 0, "dataElement")
	if !ok {
		return {}
	}
	type_kind := ddic_xml_direct_child_text(doc, data_element_id, "typeKind")
	if strings.equal_fold(type_kind, "refToClifType") {
		return Ddic_Xml_Type_Ref {
			name = ddic_xml_direct_child_text(doc, data_element_id, "typeName"),
			is_ref = true,
		}
	}
	return Ddic_Xml_Type_Ref {
		name = ddic_builtin_type(ddic_xml_direct_child_text(doc, data_element_id, "dataType")),
	}
}

ddic_xml_entry_text :: proc(
	doc: ^xml_doc.Document,
	id: xml_doc.Element_ID,
	key: string,
) -> string {
	properties_id, ok := ddic_xml_direct_child(doc, id, "properties")
	if !ok {
		return ""
	}
	for value in doc.elements[properties_id].value {
		child_id, child_ok := value.(xml_doc.Element_ID)
		if !child_ok ||
		   doc.elements[child_id].kind != .Element ||
		   !ddic_xml_name_equal(doc.elements[child_id].ident, "entry") ||
		   !strings.equal_fold(ddic_xml_attr_value(doc, child_id, "key"), key) {
			continue
		}
		return ddic_xml_element_text(doc, child_id)
	}
	return ""
}

ddic_xml_direct_child_text :: proc(
	doc: ^xml_doc.Document,
	id: xml_doc.Element_ID,
	name: string,
) -> string {
	child_id, ok := ddic_xml_direct_child(doc, id, name)
	if !ok {
		return ""
	}
	return ddic_xml_element_text(doc, child_id)
}

ddic_xml_direct_child :: proc(
	doc: ^xml_doc.Document,
	id: xml_doc.Element_ID,
	name: string,
) -> (xml_doc.Element_ID, bool) {
	for value in doc.elements[id].value {
		child_id, ok := value.(xml_doc.Element_ID)
		if ok &&
		   doc.elements[child_id].kind == .Element &&
		   ddic_xml_name_equal(doc.elements[child_id].ident, name) {
			return child_id, true
		}
	}
	return 0, false
}

ddic_xml_element_text :: proc(doc: ^xml_doc.Document, id: xml_doc.Element_ID) -> string {
	for value in doc.elements[id].value {
		switch text in value {
		case string:
			trimmed := strings.trim_space(text)
			if trimmed != "" {
				return trimmed
			}
		case xml_doc.Element_ID:
		}
	}
	return ""
}

ddic_xml_attr_value :: proc(
	doc: ^xml_doc.Document,
	id: xml_doc.Element_ID,
	name: string,
) -> string {
	for attr in doc.elements[id].attribs {
		if ddic_xml_name_equal(attr.key, name) {
			return attr.val
		}
	}
	return ""
}

ddic_builtin_type :: proc(raw: string) -> string {
	switch {
	case strings.equal_fold(raw, "CHAR") ||
	     strings.equal_fold(raw, "CLNT") ||
	     strings.equal_fold(raw, "LANG") ||
	     strings.equal_fold(raw, "CUKY") ||
	     strings.equal_fold(raw, "UNIT") ||
	     strings.equal_fold(raw, "C"):
		return "c"
	case strings.equal_fold(raw, "NUMC") ||
	     strings.equal_fold(raw, "ACCP") ||
	     strings.equal_fold(raw, "N"):
		return "n"
	case strings.equal_fold(raw, "DATS") ||
	     strings.equal_fold(raw, "DATE") ||
	     strings.equal_fold(raw, "D"):
		return "d"
	case strings.equal_fold(raw, "TIMS") ||
	     strings.equal_fold(raw, "TIME") ||
	     strings.equal_fold(raw, "T"):
		return "t"
	case strings.equal_fold(raw, "INT1") || strings.equal_fold(raw, "B"):
		return "int1"
	case strings.equal_fold(raw, "INT2") || strings.equal_fold(raw, "S"):
		return "int2"
	case strings.equal_fold(raw, "INT4") ||
	     strings.equal_fold(raw, "INT") ||
	     strings.equal_fold(raw, "I"):
		return "i"
	case strings.equal_fold(raw, "INT8") || strings.equal_fold(raw, "8"):
		return "int8"
	case strings.equal_fold(raw, "DEC") ||
	     strings.equal_fold(raw, "CURR") ||
	     strings.equal_fold(raw, "QUAN") ||
	     strings.equal_fold(raw, "PREC") ||
	     strings.equal_fold(raw, "P"):
		return "p"
	case strings.equal_fold(raw, "FLTP") || strings.equal_fold(raw, "F"):
		return "f"
	case strings.equal_fold(raw, "RAW") || strings.equal_fold(raw, "X"):
		return "x"
	case strings.equal_fold(raw, "RAWSTRING") ||
	     strings.equal_fold(raw, "XSTRING") ||
	     strings.equal_fold(raw, "XSTR") ||
	     strings.equal_fold(raw, "Y"):
		return "xstring"
	case strings.equal_fold(raw, "STRING") ||
	     strings.equal_fold(raw, "SSTRING") ||
	     strings.equal_fold(raw, "STRG") ||
	     strings.equal_fold(raw, "G"):
		return "string"
	case strings.equal_fold(raw, "DF16_RAW") ||
	     strings.equal_fold(raw, "DF16_DEC") ||
	     strings.equal_fold(raw, "DECFLOAT16"):
		return "decfloat16"
	case strings.equal_fold(raw, "DF34_RAW") ||
	     strings.equal_fold(raw, "DF34_DEC") ||
	     strings.equal_fold(raw, "DECFLOAT34"):
		return "decfloat34"
	}
	return ""
}

ddic_xml_name_equal :: proc(name, candidate: string) -> bool {
	if strings.equal_fold(name, candidate) {
		return true
	}
	if i := strings.last_index_byte(name, ':');
	   i >= 0 && strings.equal_fold(name[i + 1:], candidate) {
		return true
	}
	return false
}
