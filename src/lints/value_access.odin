package abap_frontend_lints

import "src:ast"
import "src:tokenizer"
import "src:utils"

import "core:mem"
import "core:strings"

Value_Access :: struct {
	base_name:  string,
	base_range: tokenizer.Range,
	fields:     [dynamic]string,
}

value_access_from_expr :: proc(expr: ^ast.Expr, allocator: mem.Allocator) -> (Value_Access, bool) {
	if expr == nil {
		return {}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		return value_access_from_expr(n.value, allocator)
	case ^ast.Paren_Expr:
		return value_access_from_expr(n.expr, allocator)
	case ^ast.Ident_Expr:
		if n.name == "" {
			return {}, false
		}
		return Value_Access {
			base_name = utils.to_lower_ascii(n.name, allocator),
			base_range = n.range,
			fields = make([dynamic]string, 0, 2, allocator),
		}, true
	case ^ast.Data_Inline_Name_Expr:
		if n.name.text == "" {
			return {}, false
		}
		return Value_Access {
			base_name = utils.to_lower_ascii(n.name.text, allocator),
			base_range = n.range,
			fields = make([dynamic]string, 0, 2, allocator),
		}, true
	case ^ast.Field_Symbol_Inline_Name_Expr:
		if n.name.text == "" {
			return {}, false
		}
		return Value_Access {
			base_name = utils.to_lower_ascii(n.name.text, allocator),
			base_range = n.range,
			fields = make([dynamic]string, 0, 2, allocator),
		}, true
	case ^ast.Type_Ref_Expr:
		if n.name.text == "" {
			if len(n.raw_refs) != 1 {
				return {}, false
			}
			ref := n.raw_refs[0]
			if ref.name.text == "" || ref.type_base || ref.call_like || ref.dynamic_path {
				return {}, false
			}
			fields := make([dynamic]string, 0, len(ref.path), allocator)
			for segment in ref.path {
				if segment.name.text == "" {
					return {}, false
				}
				append(&fields, utils.to_lower_ascii(segment.name.text, allocator))
			}
			return Value_Access {
				base_name = utils.to_lower_ascii(ref.name.text, allocator),
				base_range = ref.name.range,
				fields = fields,
			}, true
		}
		return Value_Access {
			base_name = utils.to_lower_ascii(n.name.text, allocator),
			base_range = n.range,
			fields = make([dynamic]string, 0, 2, allocator),
		}, true
	case ^ast.Selector_Expr:
		access, ok := value_access_from_expr(n.base, allocator)
		if !ok {
			return {}, false
		}
		if name, _, name_ok := expr_name(n.field); name_ok {
			append(&access.fields, utils.to_lower_ascii(name, allocator))
			return access, true
		}
	}
	return {}, false
}

last_field :: proc(access: Value_Access) -> string {
	if len(access.fields) == 0 {
		return ""
	}
	return access.fields[len(access.fields) - 1]
}

table_order_name_from_access :: proc(access: Value_Access, allocator: mem.Allocator) -> string {
	if len(access.fields) == 0 {
		return access.base_name
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, access.base_name)
	for field in access.fields {
		strings.write_byte(&out, '-')
		strings.write_string(&out, field)
	}
	return strings.to_string(out)
}

expr_name :: proc(expr: ^ast.Expr) -> (string, tokenizer.Range, bool) {
	if expr == nil {
		return "", tokenizer.Range{}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name, n.range, n.name != ""
	case ^ast.Type_Ref_Expr:
		return n.name.text, n.range, n.name.text != ""
	case ^ast.Literal_Expr:
		return n.value, n.range, n.value != ""
	}
	return "", tokenizer.Range{}, false
}

strip_quotes :: proc(value: string) -> string {
	if len(value) >= 2 && ((value[0] == '\'' && value[len(value) - 1] == '\'') || (value[0] == '"' && value[len(value) - 1] == '"')) {
		return value[1:len(value) - 1]
	}
	return value
}
