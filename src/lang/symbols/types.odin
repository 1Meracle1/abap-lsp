package lang_symbols

import "../ast"

import "core:fmt"

TypeKind :: enum {
	Unknown,     // Type not yet resolved or unresolvable
	Inferred,    // Type to be inferred from expression (DATA(x) = ...)
	// Basic ABAP types
	Integer,     // i
	Float,       // f, p (packed/decimal)
	String,      // string
	Char,        // c
	Numeric,     // n
	Date,        // d
	Time,        // t
	Hex,         // x
	XString,     // xstring
	// Complex types
	Table,       // TABLE OF ...
	Structure,   // structured type
	Reference,   // REF TO ...
	// Named/user-defined
	Named,       // reference to a named type (class, interface, etc.)
}

Type :: struct {
	kind:         TypeKind,
	// For Named types: the type name
	name:         string,
	// For Table types: element type
	elem_type:    ^Type,
	// For Reference types: target type
	target_type:  ^Type,
	// For Inferred types: the expression to infer from (kept for later resolution)
	infer_source: ^ast.Expr,
	// Original AST node that defined this type (for diagnostics/navigation)
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
		return "c"
	case .Numeric:
		return "n"
	case .Date:
		return "d"
	case .Time:
		return "t"
	case .Hex:
		return "x"
	case .XString:
		return "xstring"
	case .Table:
		elem_str := format_type(t.elem_type)
		return fmt.tprintf("TABLE OF %s", elem_str)
	case .Structure:
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