package abap_frontend_semantic2

import "core:strings"

Builtin_Proc_Id :: enum {
	Invalid,
	Boolc,
	Line_Exists,
	Abs,
	Sign,
	Ceil,
	Floor,
	Trunc,
	Frac,
	Ipow,
	Nmax,
	Nmin,
	Acos,
	Asin,
	Atan,
	Cos,
	Sin,
	Tan,
	Cosh,
	Sinh,
	Tanh,
	Exp,
	Log,
	Log10,
	Sqrt,
	Charlen,
	Dbmaxlen,
	Numofchar,
	Strlen,
	Substring,
	Substring_Before,
	Substring_After,
	Shift_Left,
	Condense,
	Replace,
	Matches,
	Find,
	Repeat,
	Escape,
	Reverse,
	Round,
	Rescale,
	To_Lower,
	To_Upper,
	To_Mixed,
	From_Mixed,
	Xstrlen,
	Lines,
	Concat_Lines_Of,
}

Builtin_Proc_Param :: struct {
	name:      string,
	type_name: string,
}

Builtin_Proc_Metadata :: struct {
	id:                    Builtin_Proc_Id,
	name:                  string,
	params:                []Builtin_Proc_Param,
	return_type:           string,
	docs:                  string,
	supports_named_args:   bool,
}

BUILTIN_PROC_NUMERIC_ARG_PARAMS :: []Builtin_Proc_Param{{"arg", "data"}}
BUILTIN_PROC_FLOAT_ARG_PARAMS :: []Builtin_Proc_Param{{"arg", "f"}}
BUILTIN_PROC_IPOW_PARAMS :: []Builtin_Proc_Param{{"base", "data"}, {"exp", "i"}}
BUILTIN_PROC_EXTREMUM_PARAMS :: []Builtin_Proc_Param {
	{"val1", "data"},
	{"val2", "data"},
	{"val3", "data"},
	{"val4", "data"},
	{"val5", "data"},
	{"val6", "data"},
	{"val7", "data"},
	{"val8", "data"},
	{"val9", "data"},
}
BUILTIN_PROC_DEC_FLOAT_ROUNDING_PARAMS :: []Builtin_Proc_Param {
	{"val", "decfloat34"},
	{"dec", "i"},
	{"prec", "i"},
	{"mode", "data"},
}
BUILTIN_PROC_SUBSTRING_MATCH_PARAMS :: []Builtin_Proc_Param {
	{"val", "string"},
	{"sub", "string"},
	{"regex", "string"},
	{"occ", "i"},
	{"case", "abap_bool"},
}

BUILTIN_PROCS :: []Builtin_Proc_Metadata {
	{
		id = .Boolc,
		name = "boolc",
		params = []Builtin_Proc_Param{{"log_exp", "abap_bool"}},
		return_type = "string",
		docs = "Returns 'X' as a string when the logical expression is true, otherwise a blank string.",
	},
	{
		id = .Line_Exists,
		name = "line_exists",
		params = []Builtin_Proc_Param{{"table_line", "data"}},
		return_type = "abap_bool",
		docs = "Predicate function: returns whether a row exists for the given internal table expression.",
	},
	{id = .Abs, name = "abs", params = BUILTIN_PROC_NUMERIC_ARG_PARAMS, return_type = "data", docs = "Absolute value of `arg`."},
	{id = .Sign, name = "sign", params = BUILTIN_PROC_NUMERIC_ARG_PARAMS, return_type = "data", docs = "Sign of `arg`: -1, 0, or 1."},
	{id = .Ceil, name = "ceil", params = BUILTIN_PROC_NUMERIC_ARG_PARAMS, return_type = "data", docs = "Smallest integer not less than `arg`."},
	{id = .Floor, name = "floor", params = BUILTIN_PROC_NUMERIC_ARG_PARAMS, return_type = "data", docs = "Largest integer not greater than `arg`."},
	{id = .Trunc, name = "trunc", params = BUILTIN_PROC_NUMERIC_ARG_PARAMS, return_type = "data", docs = "Integer part of `arg`."},
	{id = .Frac, name = "frac", params = BUILTIN_PROC_NUMERIC_ARG_PARAMS, return_type = "data", docs = "Decimal part of `arg`."},
	{id = .Ipow, name = "ipow", params = BUILTIN_PROC_IPOW_PARAMS, return_type = "data", docs = "Integer power: `base` raised to `exp`.", supports_named_args = true},
	{id = .Nmax, name = "nmax", params = BUILTIN_PROC_EXTREMUM_PARAMS, return_type = "data", docs = "Largest numeric argument.", supports_named_args = true},
	{id = .Nmin, name = "nmin", params = BUILTIN_PROC_EXTREMUM_PARAMS, return_type = "data", docs = "Smallest numeric argument.", supports_named_args = true},
	{id = .Acos, name = "acos", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Arccosine of `arg`."},
	{id = .Asin, name = "asin", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Arcsine of `arg`."},
	{id = .Atan, name = "atan", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Arctangent of `arg`."},
	{id = .Cos, name = "cos", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Cosine of `arg`."},
	{id = .Sin, name = "sin", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Sine of `arg`."},
	{id = .Tan, name = "tan", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Tangent of `arg`."},
	{id = .Cosh, name = "cosh", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Hyperbolic cosine of `arg`."},
	{id = .Sinh, name = "sinh", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Hyperbolic sine of `arg`."},
	{id = .Tanh, name = "tanh", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Hyperbolic tangent of `arg`."},
	{id = .Exp, name = "exp", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Exponential function for base e."},
	{id = .Log, name = "log", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Natural logarithm of `arg`."},
	{id = .Log10, name = "log10", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Logarithm of `arg` to base 10."},
	{id = .Sqrt, name = "sqrt", params = BUILTIN_PROC_FLOAT_ARG_PARAMS, return_type = "f", docs = "Square root of `arg`."},
	{id = .Charlen, name = "charlen", params = []Builtin_Proc_Param{{"arg", "string"}, {"text", "string"}}, return_type = "i", docs = "Length of the first character in the current code page."},
	{id = .Dbmaxlen, name = "dbmaxlen", params = []Builtin_Proc_Param{{"arg", "string"}, {"val", "string"}}, return_type = "i", docs = "Maximum ABAP Dictionary length for a string-like value."},
	{id = .Numofchar, name = "numofchar", params = []Builtin_Proc_Param{{"arg", "string"}, {"str", "string"}}, return_type = "i", docs = "Number of characters in a text value."},
	{id = .Strlen, name = "strlen", params = []Builtin_Proc_Param{{"arg", "string"}, {"val", "string"}}, return_type = "i", docs = "Number of characters in a text value."},
	{id = .Substring, name = "substring", params = []Builtin_Proc_Param{{"val", "string"}, {"off", "i"}, {"len", "i"}}, return_type = "string", docs = "Returns a substring of a text-like value.", supports_named_args = true},
	{id = .Substring_Before, name = "substring_before", params = BUILTIN_PROC_SUBSTRING_MATCH_PARAMS, return_type = "string", docs = "Returns the text before a substring or regular-expression match.", supports_named_args = true},
	{id = .Substring_After, name = "substring_after", params = BUILTIN_PROC_SUBSTRING_MATCH_PARAMS, return_type = "string", docs = "Returns the text after a substring or regular-expression match.", supports_named_args = true},
	{id = .Shift_Left, name = "shift_left", params = []Builtin_Proc_Param{{"val", "string"}, {"places", "i"}, {"circular", "abap_bool"}, {"sub", "string"}}, return_type = "string", docs = "Returns a text value shifted left.", supports_named_args = true},
	{id = .Condense, name = "condense", params = []Builtin_Proc_Param{{"val", "string"}, {"del", "string"}, {"from", "string"}, {"to", "string"}}, return_type = "string", docs = "Returns a condensed character string.", supports_named_args = true},
	{id = .Replace, name = "replace", params = []Builtin_Proc_Param{{"val", "string"}, {"sub", "string"}, {"regex", "string"}, {"with", "string"}, {"occ", "i"}, {"case", "abap_bool"}}, return_type = "string", docs = "Returns a character string with matching occurrences replaced.", supports_named_args = true},
	{id = .Matches, name = "matches", params = []Builtin_Proc_Param{{"val", "string"}, {"regex", "string"}, {"case", "abap_bool"}}, return_type = "abap_bool", docs = "Predicate function: returns whether a text value matches a regular expression.", supports_named_args = true},
	{id = .Find, name = "find", params = []Builtin_Proc_Param{{"val", "string"}, {"sub", "string"}, {"regex", "string"}, {"occ", "i"}, {"case", "abap_bool"}}, return_type = "i", docs = "Returns the offset of a substring or regular-expression match in a text value.", supports_named_args = true},
	{id = .Repeat, name = "repeat", params = []Builtin_Proc_Param{{"val", "string"}, {"occ", "i"}}, return_type = "string", docs = "Returns a string containing `val` repeated `occ` times.", supports_named_args = true},
	{id = .Escape, name = "escape", params = []Builtin_Proc_Param{{"val", "string"}, {"format", "data"}}, return_type = "string", docs = "Returns a character string escaped for the requested target format.", supports_named_args = true},
	{id = .Reverse, name = "reverse", params = []Builtin_Proc_Param{{"val", "string"}}, return_type = "string", docs = "Returns a character string with its characters in reverse order.", supports_named_args = true},
	{id = .Round, name = "round", params = BUILTIN_PROC_DEC_FLOAT_ROUNDING_PARAMS, return_type = "decfloat34", docs = "Rounds a decimal floating-point value.", supports_named_args = true},
	{id = .Rescale, name = "rescale", params = BUILTIN_PROC_DEC_FLOAT_ROUNDING_PARAMS, return_type = "decfloat34", docs = "Rescales a decimal floating-point value.", supports_named_args = true},
	{id = .To_Lower, name = "to_lower", params = []Builtin_Proc_Param{{"val", "string"}}, return_type = "string", docs = "Returns a text value converted to lowercase.", supports_named_args = true},
	{id = .To_Upper, name = "to_upper", params = []Builtin_Proc_Param{{"val", "string"}}, return_type = "string", docs = "Returns a text value converted to uppercase.", supports_named_args = true},
	{id = .To_Mixed, name = "to_mixed", params = []Builtin_Proc_Param{{"val", "string"}, {"sep", "string"}, {"case", "string"}, {"min", "i"}}, return_type = "string", docs = "Converts separator-delimited text to mixed case.", supports_named_args = true},
	{id = .From_Mixed, name = "from_mixed", params = []Builtin_Proc_Param{{"val", "string"}, {"sep", "string"}, {"case", "string"}, {"min", "i"}}, return_type = "string", docs = "Converts mixed-case text to separator-delimited text.", supports_named_args = true},
	{id = .Xstrlen, name = "xstrlen", params = []Builtin_Proc_Param{{"arg", "xstring"}, {"val", "xstring"}}, return_type = "i", docs = "Number of bytes in a byte string value."},
	{id = .Lines, name = "lines", params = []Builtin_Proc_Param{{"arg", "data"}, {"val", "data"}}, return_type = "i", docs = "Number of rows in an internal table value."},
	{id = .Concat_Lines_Of, name = "concat_lines_of", params = []Builtin_Proc_Param{{"table", "data"}, {"sep", "string"}}, return_type = "string", docs = "Concatenates the rows of an internal table into one character string.", supports_named_args = true},
}

checker_builtin_proc_metadata :: proc(id: Builtin_Proc_Id) -> (^Builtin_Proc_Metadata, bool) {
	for &metadata in BUILTIN_PROCS {
		if metadata.id == id {
			return &metadata, true
		}
	}
	return nil, false
}

checker_builtin_proc_metadata_by_name :: proc(name: string) -> (^Builtin_Proc_Metadata, bool) {
	for &metadata in BUILTIN_PROCS {
		if strings.equal_fold(metadata.name, name) {
			return &metadata, true
		}
	}
	return nil, false
}
