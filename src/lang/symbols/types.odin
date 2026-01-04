package lang_symbols

import "../ast"

import "core:fmt"

TypeKind :: enum {
	Unknown,
	Inferred,
	Integer,
	Float,
	String,
	StringTemplate, // String template expression result
	Char,
	Numeric,
	Date,
	Time,
	Hex,
	XString,
	Table,
	Structure,
	Reference,
	Named,
	LineOf, // LINE OF table_type
}

// Table type kinds
TableTypeKind :: enum {
	Standard,
	Sorted,
	Hashed,
	Any,
}

// Table key definition
TableKeyInfo :: struct {
	is_unique:  bool,
	is_default: bool,
	name:       string, // For named secondary keys
	components: [dynamic]string,
}

StructField :: struct {
	name:      string,
	type_info: ^Type,
	length:    int,
}

Type :: struct {
	kind:           TypeKind,
	name:           string,
	elem_type:      ^Type, // For Table: element type
	target_type:    ^Type, // For Reference: target type; For LineOf: table type
	fields:         [dynamic]StructField,
	length:         int,
	infer_source:   ^ast.Expr,
	ast_node:       ^ast.Expr,
	// Table-specific fields
	table_kind:     TableTypeKind,
	primary_key:    ^TableKeyInfo,
	secondary_keys: [dynamic]^TableKeyInfo,
}

format_type :: proc(t: ^Type) -> string {
	if t == nil {
		return "unknown"
	}

	switch t.kind {
	case .Unknown:
		return "unknown"
	case .Inferred:
		return "inferred"
	case .Integer:
		return "i"
	case .Float:
		return "f"
	case .String:
		return "string"
	case .StringTemplate:
		return "string"
	case .Char:
		if t.length > 0 {
			return fmt.tprintf("c LENGTH %d", t.length)
		}
		return "c"
	case .Numeric:
		if t.length > 0 {
			return fmt.tprintf("n LENGTH %d", t.length)
		}
		return "n"
	case .Date:
		return "d"
	case .Time:
		return "t"
	case .Hex:
		if t.length > 0 {
			return fmt.tprintf("x LENGTH %d", t.length)
		}
		return "x"
	case .XString:
		return "xstring"
	case .Table:
		elem_str := format_type(t.elem_type)
		table_kind_str := ""
		switch t.table_kind {
		case .Standard:
			table_kind_str = "STANDARD TABLE OF"
		case .Sorted:
			table_kind_str = "SORTED TABLE OF"
		case .Hashed:
			table_kind_str = "HASHED TABLE OF"
		case .Any:
			table_kind_str = "TABLE OF"
		}
		result := fmt.tprintf("%s %s", table_kind_str, elem_str)
		// Add key info if present
		if t.primary_key != nil && len(t.primary_key.components) > 0 {
			key_prefix := t.primary_key.is_unique ? "UNIQUE KEY" : "KEY"
			if t.primary_key.is_default {
				result = fmt.tprintf("%s WITH %s DEFAULT KEY", result, key_prefix)
			} else {
				// Build key components string
				key_comps := ""
				for i := 0; i < len(t.primary_key.components); i += 1 {
					if i > 0 {
						key_comps = fmt.tprintf("%s, %s", key_comps, t.primary_key.components[i])
					} else {
						key_comps = t.primary_key.components[i]
					}
				}
				result = fmt.tprintf("%s WITH %s %s", result, key_prefix, key_comps)
			}
		}
		return result
	case .LineOf:
		target_str := format_type(t.target_type)
		return fmt.tprintf("LINE OF %s", target_str)
	case .Structure:
		if t.name != "" {
			return t.name
		}
		return "structure"
	case .Reference:
		target_str := format_type(t.target_type)
		return fmt.tprintf("REF TO %s", target_str)
	case .Named:
		if t.name != "" {
			return t.name
		}
		return "named"
	}
	return "unknown"
}