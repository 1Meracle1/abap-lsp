package ddic_xml

import ddic "src:ddic"
import ddic_source "src:ddic_source"
import xml_doc "core:encoding/xml"
import "core:mem"
import "core:strconv"
import "core:strings"

Ddic_Xml_Kind :: enum {
	Unknown,
	Alias,
	Structure,
	Table_Type,
}

Ddic_Xml_Field :: struct {
	name:        string,
	type_name:   string,
	key:         bool,
	description: string,
}

Ddic_Xml_Type_Ref :: struct {
	name:   string,
	is_ref: bool,
	length: string,
}

dependency_source :: proc(
	name, object_kind, source: string,
	allocator: mem.Allocator,
) -> string {
	if out := ddic_source.dependency_source(source, allocator); out != "" {
		return out
	}

	doc, parsed := ddic_xml_parse(source, allocator)
	kind := ddic_xml_kind(object_kind, doc)
	out: string

	if kind == .Structure || kind == .Unknown {
		fields := ddic_xml_fields(doc, allocator)
		if len(fields) > 0 {
			out = ddic_xml_structure_source(name, ddic_xml_description(doc, 0), fields[:], allocator)
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
	ddic.write_abap_name(&out, name)
	strings.write_string(&out, " TYPE ")
	if type_ref.is_ref {
		strings.write_string(&out, "REF TO ")
	}
	ddic.write_abap_name(&out, type_ref.name)
	write_type_ref_length(&out, type_ref)
	strings.write_string(&out, ".\n")
	return strings.to_string(out)
}

write_type_ref_length :: proc(out: ^strings.Builder, type_ref: Ddic_Xml_Type_Ref) {
	if !ddic_xml_type_ref_supports_length(type_ref.name) {
		return
	}
	length, ok := strconv.parse_int(strings.trim_space(type_ref.length), 10)
	if !ok || length <= 0 {
		return
	}
	buf: [32]byte
	strings.write_string(out, " LENGTH ")
	strings.write_string(out, strconv.write_int(buf[:], i64(length), 10))
}

ddic_xml_type_ref_supports_length :: proc(name: string) -> bool {
	return strings.equal_fold(name, "c") ||
	       strings.equal_fold(name, "n") ||
	       strings.equal_fold(name, "x")
}

ddic_xml_table_type_source :: proc(
	name: string,
	line: Ddic_Xml_Type_Ref,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "TYPES ")
	ddic.write_abap_name(&out, name)
	strings.write_string(&out, " TYPE STANDARD TABLE OF ")
	if line.is_ref {
		strings.write_string(&out, "REF TO ")
	}
	ddic.write_abap_name(&out, line.name)
	strings.write_string(&out, " WITH DEFAULT KEY.\n")
	return strings.to_string(out)
}

ddic_xml_structure_source :: proc(
	name: string,
	description: string,
	fields: []Ddic_Xml_Field,
	allocator: mem.Allocator,
) -> string {
	definition := ddic_source.Type_Definition {
		name        = name,
		annotations = make([dynamic]ddic_source.Annotation, 0, 1, allocator),
		members     = make([dynamic]ddic_source.Member, 0, len(fields), allocator),
	}
	if description != "" {
		append(
			&definition.annotations,
			ddic_source.Annotation {
				name  = "EndUserText.label",
				value = description,
			},
		)
	}
	for field in fields {
		member := ddic_source.Member {
			kind     = .Field,
			name     = field.name,
			key      = field.key,
			type_ref = ddic_source.Type_Ref{kind = .Named, name = field.type_name},
			annotations = make([dynamic]ddic_source.Annotation, 0, 1, allocator),
		}
		if field.description != "" {
			append(
				&member.annotations,
				ddic_source.Annotation {
					name  = "EndUserText.label",
					value = field.description,
				},
			)
		}
		append(&definition.members, member)
	}
	return ddic_source.dependency_source_from_definition(&definition, allocator)
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
		   !ddic_xml_name_equal(doc.elements[child_id].ident, "elementInfo") {
			continue
		}
		child_type := ddic_xml_attr_value(doc, child_id, "type")
		name := ddic_xml_attr_value(doc, child_id, "name")
		if strings.equal_fold(child_type, "TABL/DTF") && name != "" {
			type_name := ddic_xml_field_type(doc, child_id)
			if type_name == "" {
				continue
			}
			duplicate := false
			for field in fields {
				if strings.equal_fold(field.name, name) {
					duplicate = true
					break
				}
			}
			if !duplicate {
				append(
					&fields,
					Ddic_Xml_Field {
						name        = name,
						type_name   = type_name,
						key         = ddic_xml_field_is_key(doc, child_id),
						description = ddic_xml_description(doc, child_id),
					},
				)
			}
		}
	}
	return fields
}

ddic_xml_field_type :: proc(doc: ^xml_doc.Document, id: xml_doc.Element_ID) -> string {
	if data_element := ddic_xml_entry_text(doc, id, "ddicDataElement"); data_element != "" {
		return data_element
	}
	data_type := ddic_xml_entry_text(doc, id, "ddicDataType")
	if builtin := ddic.builtin_type(data_type); builtin != "" {
		return builtin
	}
	return data_type
}

ddic_xml_field_is_key :: proc(doc: ^xml_doc.Document, id: xml_doc.Element_ID) -> bool {
	attrs := [?]string{"key", "isKey"}
	for attr in attrs {
		if ddic_xml_truthy(ddic_xml_attr_value(doc, id, attr)) {
			return true
		}
	}
	keys := [?]string {
		"key",
		"isKey",
		"ddicIsKey",
		"ddicKeyField",
		"ddicKeyFlag",
		"ddicIsKeyField",
	}
	for key in keys {
		if ddic_xml_truthy(ddic_xml_entry_text(doc, id, key)) {
			return true
		}
	}
	return false
}

ddic_xml_truthy :: proc(value: string) -> bool {
	trimmed := strings.trim_space(value)
	return trimmed == "1" ||
	       strings.equal_fold(trimmed, "x") ||
	       strings.equal_fold(trimmed, "true") ||
	       strings.equal_fold(trimmed, "yes") ||
	       strings.equal_fold(trimmed, "key")
}

ddic_xml_description :: proc(doc: ^xml_doc.Document, id: xml_doc.Element_ID) -> string {
	if doc == nil || int(id) < 0 || int(id) >= len(doc.elements) {
		return ""
	}
	if value := strings.trim_space(ddic_xml_attr_value(doc, id, "description")); value != "" {
		return value
	}
	keys := [?]string {
		"description",
		"shortDescription",
		"quickInfo",
		"ddicDescription",
		"ddicShortText",
		"ddicMediumText",
		"ddicLongText",
		"ddicHeading",
		"ddicFieldText",
		"ddicFieldLabel",
		"ddicScrtextS",
		"ddicScrtextM",
		"ddicScrtextL",
		"ddicReptext",
		"ddtext",
		"fieldtext",
		"reptext",
		"scrtext_s",
		"scrtext_m",
		"scrtext_l",
	}
	for key in keys {
		if value := strings.trim_space(ddic_xml_entry_text(doc, id, key)); value != "" {
			return value
		}
		if value := strings.trim_space(ddic_xml_direct_child_text(doc, id, key)); value != "" {
			return value
		}
	}
	return ""
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
	   !ddic_xml_name_equal(doc.elements[0].ident, "wbobj") {
		return {}
	}
	root_type := ddic_xml_attr_value(doc, 0, "type")
	alias_child_name := ""
	switch {
	case strings.equal_fold(root_type, "DTEL/DE"):
		alias_child_name = "dataElement"
	case ddic_xml_starts_with_ignore_case(root_type, "DOMA/"):
		alias_child_name = "domain"
	}
	if alias_child_name == "" {
		return {}
	}
	alias_id, ok := ddic_xml_direct_child(doc, 0, alias_child_name)
	if !ok {
		return {}
	}
	type_kind := ddic_xml_direct_child_text(doc, alias_id, "typeKind")
	if strings.equal_fold(type_kind, "refToClifType") {
		return Ddic_Xml_Type_Ref {
			name = ddic_xml_direct_child_text(doc, alias_id, "typeName"),
			is_ref = true,
		}
	}
	if strings.equal_fold(type_kind, "refToDictionaryType") {
		return Ddic_Xml_Type_Ref {
			name = ddic_xml_direct_child_text(doc, alias_id, "typeName"),
			is_ref = true,
		}
	}
	return Ddic_Xml_Type_Ref {
		name   = ddic.builtin_type(ddic_xml_direct_child_text(doc, alias_id, "dataType")),
		length = ddic_xml_alias_type_length(doc, alias_id),
	}
}

ddic_xml_alias_type_length :: proc(doc: ^xml_doc.Document, id: xml_doc.Element_ID) -> string {
	if length := ddic_xml_direct_child_text(doc, id, "dataTypeLength"); length != "" {
		return length
	}
	return ddic_xml_direct_child_text(doc, id, "length")
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

ddic_xml_starts_with_ignore_case :: proc(value, prefix: string) -> bool {
	return len(value) >= len(prefix) && strings.equal_fold(value[:len(prefix)], prefix)
}
