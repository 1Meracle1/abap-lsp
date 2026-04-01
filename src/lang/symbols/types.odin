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
	Data, // Generic TYPE data / REF TO data (any data object)
	Table,
	Structure,
	Reference,
	Named,
	LineOf, // LINE OF table_type
	RangeOf, // RANGE OF elementary_or_domain
	Cursor, // Open SQL database cursor (built-in type)
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
	name:       string,
	type_info:  ^Type,
	length:     int,
	// From CONSTANTS … VALUE …; nil for DATA / TYPES fields.
	const_init: ^ast.Expr,
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
		if t.length == 15 && t.name == "timestamp" {
			return "p LENGTH 15 (UTC timestamp short form, YYYYMMDDhhmmss)"
		}
		return "f"
	case .String:
		return "string"
	case .StringTemplate:
		return "string"
	case .Char:
		if t.length > 0 {
			if t.length == 1 && t.name == "xfeld" {
				return "c LENGTH 1 (char1)"
			}
			if t.length == 50 && t.name == "symsgv" {
				return "c LENGTH 50 (SYMSGV)"
			}
			if t.length == 30 && t.name == "tabname" {
				return "c LENGTH 30 (TABNAME)"
			}
			if t.length == 15 && t.name == "cdobjectcl" {
				return "c LENGTH 15 (CDOBJECTCL)"
			}
			if t.length == 30 && t.name == "rs38l_fnam" {
				return "c LENGTH 30 (RS38L_FNAM)"
			}
			if t.length == 20 && t.name == "memoryid" {
				return "c LENGTH 20 (MEMORYID)"
			}
			if t.length == 8 && t.name == "sydatum" {
				return "c LENGTH 8 (YYYYMMDD)"
			}
			return fmt.tprintf("c LENGTH %d", t.length)
		}
		return "c"
	case .Numeric:
		if t.length > 0 {
			switch t.name {
			case "numc3":
				return "n LENGTH 3 (NUMC)"
			case "numc4":
				return "n LENGTH 4 (NUMC)"
			}
			return fmt.tprintf("n LENGTH %d", t.length)
		}
		return "n"
	case .Date:
		return "d"
	case .Time:
		return "t"
	case .Hex:
		if t.length > 0 {
			if t.length == 16 && t.name == "guid" {
				return "RAW LENGTH 16 (byte sequence)"
			}
			return fmt.tprintf("x LENGTH %d", t.length)
		}
		return "x"
	case .XString:
		return "xstring"
	case .Cursor:
		return "cursor (Open SQL database cursor)"
	case .Data:
		return "data"
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
	case .RangeOf:
		elem_str := format_type(t.elem_type)
		return fmt.tprintf("RANGE OF %s", elem_str)
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