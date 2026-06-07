package ddic

import "core:strings"

builtin_type :: proc(raw: string) -> string {
	switch {
	case strings.equal_fold(raw, "CHAR") ||
	     strings.equal_fold(raw, "CLNT") ||
	     strings.equal_fold(raw, "LANG") ||
	     strings.equal_fold(raw, "CUKY") ||
	     strings.equal_fold(raw, "UNIT") ||
	     strings.equal_fold(raw, "LCHR") ||
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
	     strings.equal_fold(raw, "LRAW") ||
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

write_abap_name :: #force_inline proc(out: ^strings.Builder, name: string) {
	for r in name {
		strings.write_rune(out, r)
	}
}
