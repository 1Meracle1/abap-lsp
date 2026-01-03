package lang_symbols

import "../ast"

import "core:fmt"

TypeKind :: enum {
	Unknown,
	Inferred,
	Integer,
	Float,
	String,
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
}

StructField :: struct {
	name:      string,
	type_info: ^Type,
	length:    int,
}

Type :: struct {
	kind:         TypeKind,
	name:         string,
	elem_type:    ^Type,
	target_type:  ^Type,
	fields:       [dynamic]StructField,
	length:       int,
	infer_source: ^ast.Expr,
	ast_node:     ^ast.Expr,
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
		return fmt.tprintf("TABLE OF %s", elem_str)
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