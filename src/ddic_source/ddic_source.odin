package ddic_source

import ddic "src:ddic"
import "src:tokenizer"

import "core:mem"
import "core:strings"

Token :: tokenizer.Token
Range :: tokenizer.Range

Type_Definition :: struct {
	name:        string,
	annotations: [dynamic]Annotation,
	members:     [dynamic]Member,
}

Annotation :: struct {
	name:  string,
	value: string,
	range: Range,
}

Member_Kind :: enum {
	Field,
	Include,
}

Type_Ref_Kind :: enum {
	Named,
	Include,
	Reference_To,
}

Type_Ref :: struct {
	kind: Type_Ref_Kind,
	name: string,
}

Member :: struct {
	kind:        Member_Kind,
	name:        string,
	key:         bool,
	type_ref:    Type_Ref,
	annotations: [dynamic]Annotation,
	clauses:     [dynamic]Clause,
	range:       Range,
}

Clause_Kind :: enum {
	Unknown,
	Not_Null,
	Foreign_Key,
	Value_Help,
	Where,
	Extend,
	Remove_Foreign_Key,
}

Clause :: struct {
	kind:  Clause_Kind,
	range: Range,
}

Parse_Error :: struct {
	message: string,
	range:   Range,
}

Parse_Result :: struct {
	definition: Type_Definition,
	errors:     []Parse_Error,
}

Parser :: struct {
	source:         string,
	tokens:         []Token,
	index:          int,
	previous_index: int,
	errors:         [dynamic]Parse_Error,
	allocator:      mem.Allocator,
}

parse :: proc(source: string, allocator: mem.Allocator) -> Parse_Result {
	lexed := tokenizer.tokenize(source, context.temp_allocator)
	parser := Parser {
		source = source,
		tokens = lexed.tokens,
		previous_index = -1,
		errors = make([dynamic]Parse_Error, 0, len(lexed.errors) + 4, allocator),
		allocator = allocator,
	}
	for e in lexed.errors {
		append(&parser.errors, Parse_Error{message = e.message, range = e.range})
	}
	definition := parse_type_definition(&parser)
	return Parse_Result{definition = definition, errors = parser.errors[:]}
}

dependency_source :: proc(source: string, allocator: mem.Allocator) -> string {
	parsed := parse(source, context.temp_allocator)
	if parsed.definition.name == "" {
		return ""
	}
	return dependency_source_from_definition(&parsed.definition, allocator)
}

dependency_source_from_definition :: proc(definition: ^Type_Definition, allocator: mem.Allocator) -> string {
	if definition == nil || definition.name == "" {
		return ""
	}
	out := strings.builder_make(allocator)
	description := annotation_description(definition.annotations[:])
	write_comment_line(&out, description)
	strings.write_string(&out, "TYPES:\n")
	strings.write_string(&out, "  BEGIN OF ")
	ddic.write_abap_name(&out, definition.name)
	strings.write_string(&out, ",")
	write_comment_suffix(&out, false, description)
	strings.write_byte(&out, '\n')
	for member in definition.members {
		strings.write_string(&out, "    ")
		switch member.kind {
		case .Include:
			strings.write_string(&out, "INCLUDE TYPE ")
			ddic.write_abap_name(&out, member.type_ref.name)
			strings.write_string(&out, ",")
			write_comment_suffix(&out, false, annotation_description(member.annotations[:]))
			strings.write_byte(&out, '\n')
		case .Field:
			if member.type_ref.kind == .Include {
				strings.write_string(&out, "INCLUDE TYPE ")
				ddic.write_abap_name(&out, member.type_ref.name)
				strings.write_string(&out, " AS ")
				ddic.write_abap_decl_name(&out, member.name)
				strings.write_string(&out, ",")
				write_comment_suffix(&out, member.key, annotation_description(member.annotations[:]))
				strings.write_byte(&out, '\n')
				continue
			}
			ddic.write_abap_decl_name(&out, member.name)
			strings.write_string(&out, " TYPE ")
			write_type_ref(&out, member.type_ref)
			strings.write_string(&out, ",")
			write_comment_suffix(&out, member.key, annotation_description(member.annotations[:]))
			strings.write_byte(&out, '\n')
		}
	}
	strings.write_string(&out, "  END OF ")
	ddic.write_abap_name(&out, definition.name)
	strings.write_string(&out, ".")
	strings.write_string(&out, "\n")
	return strings.to_string(out)
}

annotation_description :: proc(annotations: []Annotation) -> string {
	for annotation in annotations {
		value := strings.trim_space(annotation.value)
		if value == "" {
			continue
		}
		if strings.equal_fold(annotation.name, "EndUserText.label") ||
		   strings.equal_fold(annotation.name, "EndUserText.quickInfo") {
			return value
		}
	}
	return ""
}

write_comment_line :: proc(out: ^strings.Builder, description: string) {
	desc := strings.trim_space(description)
	if desc == "" {
		return
	}
	strings.write_string(out, `" `)
	write_comment_text(out, desc)
	strings.write_byte(out, '\n')
}

write_comment_suffix :: proc(out: ^strings.Builder, key: bool, description: string) {
	desc := strings.trim_space(description)
	if !key && desc == "" {
		return
	}
	strings.write_string(out, ` " `)
	if key {
		strings.write_string(out, "key field")
		if desc != "" {
			strings.write_string(out, "; ")
		}
	}
	write_comment_text(out, desc)
}

write_comment_text :: proc(out: ^strings.Builder, text: string) {
	space := false
	for r in strings.trim_space(text) {
		if r == '\r' || r == '\n' || r == '\t' {
			space = true
			continue
		}
		if space {
			strings.write_byte(out, ' ')
			space = false
		}
		strings.write_rune(out, r)
	}
}

write_type_ref :: proc(out: ^strings.Builder, type_ref: Type_Ref) {
	if type_ref.kind == .Reference_To {
		strings.write_string(out, "REF TO ")
	}
	if builtin := abap_builtin_type(type_ref.name); builtin != "" {
		strings.write_string(out, builtin)
		return
	}
	ddic.write_abap_name(out, type_ref.name)
}

abap_builtin_type :: proc(name: string) -> string {
	prefix :: "abap."
	if len(name) < len(prefix) || !strings.equal_fold(name[:len(prefix)], prefix) {
		return ""
	}
	raw := name[len(prefix):]
	return ddic.builtin_type(raw)
}

parse_type_definition :: proc(p: ^Parser) -> Type_Definition {
	definition := Type_Definition {
		annotations = make([dynamic]Annotation, 0, 2, p.allocator),
		members = make([dynamic]Member, 0, 16, p.allocator),
	}
	definition.annotations = parse_annotations(p)
	if !allow_keyword(p, "define") {
		return definition
	}
	if !expect_keyword(p, "type") {
		recover_to_definition_body(p)
	}
	definition.name = parse_name(p)
	if definition.name == "" {
		error_current(p, "expected DDIC type name")
		recover_to_definition_body(p)
	}
	if !allow_token(p, .LBrace) {
		error_current(p, "expected '{'")
		recover_to_definition_body(p)
	_ = allow_token(p, .LBrace)
	}
	for !at_eof(p) && !at_token(p, .RBrace) {
		annotations := parse_annotations(p)
		if at_eof(p) || at_token(p, .RBrace) {
			break
		}
		start := p.index
		if member, ok := parse_member(p); ok {
			member.annotations = annotations
			append(&definition.members, member)
		} else {
			recover_member(p)
		}
		ensure_forward_progress(p, start)
	}
	_ = allow_token(p, .RBrace)
	return definition
}

parse_member :: proc(p: ^Parser) -> (Member, bool) {
	start := current_token(p).range.start
	if allow_keyword(p, "include") {
		type_name := parse_name(p)
		if type_name == "" {
			error_current(p, "expected include type name")
			return {}, false
		}
		clauses := parse_member_suffix(p)
		return Member {
				kind = .Include,
				type_ref = Type_Ref{kind = .Named, name = type_name},
				clauses = clauses,
				range = tokenizer.text_range(start, previous_token(p).range.end),
			},
			true
	}

	key := allow_keyword(p, "key")
	field_name := parse_name(p)
	if field_name == "" {
		error_current(p, "expected DDIC field name")
		return {}, false
	}
	if !expect_token(p, .Colon, "expected ':'") {
		return {}, false
	}
	type_ref := parse_type_ref(p)
	if type_ref.name == "" {
		error_current(p, "expected DDIC field type")
		return {}, false
	}
	clauses := parse_member_suffix(p)
	return Member {
			kind = .Field,
			name = field_name,
			key = key,
			type_ref = type_ref,
			clauses = clauses,
			range = tokenizer.text_range(start, previous_token(p).range.end),
		},
		true
}

parse_type_ref :: proc(p: ^Parser) -> Type_Ref {
	if allow_keyword(p, "include") {
		name := parse_name(p)
		return Type_Ref{kind = .Include, name = name}
	}
	if allow_keyword(p, "reference") {
		_ = expect_keyword(p, "to")
		name := parse_name(p)
		parse_type_arguments(p)
		return Type_Ref{kind = .Reference_To, name = name}
	}
	name := parse_name(p)
	parse_type_arguments(p)
	return Type_Ref{kind = .Named, name = name}
}

parse_type_arguments :: proc(p: ^Parser) {
	if !allow_token(p, .LParen) {
		return
	}
	depth := 1
	for !at_eof(p) && depth > 0 {
		#partial switch current_token(p).kind {
		case .LParen:
			depth += 1
		case .RParen:
			depth -= 1
		case:
		}
		bump_token(p)
	}
}

parse_member_suffix :: proc(p: ^Parser) -> [dynamic]Clause {
	clauses := make([dynamic]Clause, 0, 2, p.allocator)
	for !at_eof(p) && !at_token(p, .RBrace) {
		if allow_semicolon(p) {
			return clauses
		}
		if clause, ok := parse_member_clause(p); ok {
			append(&clauses, clause)
			continue
		}
		parse_balanced_suffix_token(p)
	}
	return clauses
}

parse_member_clause :: proc(p: ^Parser) -> (Clause, bool) {
	switch {
	case at_keyword(p, "not"):
		start := current_token(p).range.start
		bump_token(p)
		_ = allow_keyword(p, "null")
		return Clause{kind = .Not_Null, range = tokenizer.text_range(start, previous_token(p).range.end)}, true
	case at_keyword(p, "with"):
		return parse_with_clause(p), true
	case at_keyword(p, "where"):
		return parse_keyword_clause(p, .Where), true
	case at_keyword(p, "extend"):
		return parse_keyword_clause(p, .Extend), true
	case at_keyword(p, "remove"):
		return parse_remove_clause(p), true
	}
	return {}, false
}

parse_with_clause :: proc(p: ^Parser) -> Clause {
	start := current_token(p).range.start
	_ = allow_keyword(p, "with")
	kind := Clause_Kind.Unknown
	if allow_keyword(p, "foreign") {
		_ = allow_keyword(p, "key")
		kind = .Foreign_Key
	} else if allow_keyword(p, "value") {
		_ = allow_keyword(p, "help")
		kind = .Value_Help
	}
	parse_clause_body(p)
	return Clause{kind = kind, range = tokenizer.text_range(start, previous_token(p).range.end)}
}

parse_keyword_clause :: proc(p: ^Parser, kind: Clause_Kind) -> Clause {
	start := current_token(p).range.start
	bump_token(p)
	parse_clause_body(p)
	return Clause{kind = kind, range = tokenizer.text_range(start, previous_token(p).range.end)}
}

parse_remove_clause :: proc(p: ^Parser) -> Clause {
	start := current_token(p).range.start
	_ = allow_keyword(p, "remove")
	kind := Clause_Kind.Unknown
	if allow_keyword(p, "foreign") {
		_ = allow_keyword(p, "key")
		kind = .Remove_Foreign_Key
	}
	parse_clause_body(p)
	return Clause{kind = kind, range = tokenizer.text_range(start, previous_token(p).range.end)}
}

parse_clause_body :: proc(p: ^Parser) {
	for !at_eof(p) && !at_token(p, .RBrace) && !at_semicolon(p) && !at_clause_boundary(p) {
		parse_balanced_suffix_token(p)
	}
}

at_clause_boundary :: proc(p: ^Parser) -> bool {
	return at_keyword(p, "with") ||
	       at_keyword(p, "where") ||
	       at_keyword(p, "extend") ||
	       at_keyword(p, "remove") ||
	       at_keyword(p, "not")
}

parse_balanced_suffix_token :: proc(p: ^Parser) {
	#partial switch current_token(p).kind {
	case .LParen:
		parse_balanced_tokens(p, .LParen, .RParen)
	case .LBracket:
		parse_balanced_tokens(p, .LBracket, .RBracket)
	case .LBrace:
		parse_balanced_tokens(p, .LBrace, .RBrace)
	case:
		bump_token(p)
	}
}

parse_balanced_tokens :: proc(p: ^Parser, open, close: tokenizer.Token_Kind) {
	if !allow_token(p, open) {
		return
	}
	depth := 1
	for !at_eof(p) && depth > 0 {
		if at_token(p, open) {
			depth += 1
		} else if at_token(p, close) {
			depth -= 1
		}
		bump_token(p)
	}
}

parse_name :: proc(p: ^Parser) -> string {
	if !at_name_token(p) {
		return ""
	}
	start := current_token(p).range.start
	end := current_token(p).range.end
	bump_token(p)
	for allow_token(p, .Period) {
		if !at_name_token(p) {
			break
		}
		end = current_token(p).range.end
		bump_token(p)
	}
	return strings.clone(p.source[start:end], p.allocator)
}

parse_annotations :: proc(p: ^Parser) -> [dynamic]Annotation {
	out := make([dynamic]Annotation, 0, 2, p.allocator)
	for at_token(p, .At) {
		append(&out, parse_annotation(p))
	}
	return out
}

parse_annotation :: proc(p: ^Parser) -> Annotation {
	start := current_token(p).range.start
	_ = allow_token(p, .At)
	name := parse_name(p)
	value := ""
	for !at_eof(p) && !at_token(p, .RBrace) {
		if current_has_newline_before(p) {
			break
		}
		if allow_semicolon(p) {
			break
		}
		if value == "" && current_token(p).kind == .String {
			value = annotation_string_value(p, current_token(p))
		}
		parse_balanced_suffix_token(p)
	}
	return Annotation{name = name, value = value, range = tokenizer.text_range(start, previous_token(p).range.end)}
}

annotation_string_value :: proc(p: ^Parser, token: Token) -> string {
	if token.range.start < len(p.source) {
		quote := p.source[token.range.start]
		if quote == '\'' || quote == '`' {
			return scan_annotation_text_literal(p.source, token.range.start, quote, p.allocator)
		}
	}
	raw := token_text(p, token)
	if len(raw) >= 2 {
		quote := raw[0]
		if (quote == '\'' || quote == '`') && raw[len(raw) - 1] == quote {
			return scan_annotation_text_literal(raw, 0, quote, p.allocator)
		}
	}
	return strings.clone(raw, p.allocator)
}

scan_annotation_text_literal :: proc(source: string, start: int, quote: u8, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	i := start + 1
	for i < len(source) {
		if source[i] == '\r' || source[i] == '\n' {
			break
		}
		if source[i] == quote {
			if i + 1 < len(source) && source[i + 1] == quote {
				strings.write_byte(&out, quote)
				i += 2
				continue
			}
			break
		}
		strings.write_byte(&out, source[i])
		i += 1
	}
	return strings.to_string(out)
}

recover_to_definition_body :: proc(p: ^Parser) {
	for !at_eof(p) && !at_token(p, .LBrace) {
		bump_token(p)
	}
}

recover_member :: proc(p: ^Parser) {
	for !at_eof(p) && !at_token(p, .RBrace) {
		if allow_semicolon(p) {
			return
		}
		bump_token(p)
	}
}

expect_keyword :: proc(p: ^Parser, keyword: string) -> bool {
	if allow_keyword(p, keyword) {
		return true
	}
	error_current(p, "expected keyword")
	return false
}

expect_token :: proc(p: ^Parser, kind: tokenizer.Token_Kind, message: string) -> bool {
	if allow_token(p, kind) {
		return true
	}
	error_current(p, message)
	return false
}

allow_keyword :: proc(p: ^Parser, keyword: string) -> bool {
	if at_keyword(p, keyword) {
		bump_token(p)
		return true
	}
	return false
}

at_keyword :: proc(p: ^Parser, keyword: string) -> bool {
	return current_token(p).kind == .Ident && strings.equal_fold(token_text(p, current_token(p)), keyword)
}

allow_token :: proc(p: ^Parser, kind: tokenizer.Token_Kind) -> bool {
	if at_token(p, kind) {
		bump_token(p)
		return true
	}
	return false
}

at_token :: proc(p: ^Parser, kind: tokenizer.Token_Kind) -> bool {
	return current_token(p).kind == kind
}

allow_semicolon :: proc(p: ^Parser) -> bool {
	if at_semicolon(p) {
		bump_token(p)
		return true
	}
	return false
}

at_semicolon :: proc(p: ^Parser) -> bool {
	token := current_token(p)
	return token.kind == .Other && token_text(p, token) == ";"
}

at_name_token :: proc(p: ^Parser) -> bool {
	token := current_token(p)
	return token.kind == .Ident || token.kind == .Number
}

current_has_newline_before :: proc(p: ^Parser) -> bool {
	return .Has_Newline_Before in current_token(p).flags
}

current_token :: proc(p: ^Parser) -> Token {
	if p.index < len(p.tokens) {
		return p.tokens[p.index]
	}
	return p.tokens[len(p.tokens) - 1]
}

previous_token :: proc(p: ^Parser) -> Token {
	if p.previous_index >= 0 && p.previous_index < len(p.tokens) {
		return p.tokens[p.previous_index]
	}
	return current_token(p)
}

bump_token :: proc(p: ^Parser) {
	if p.index < len(p.tokens) {
		p.previous_index = p.index
		p.index += 1
	}
}

at_eof :: proc(p: ^Parser) -> bool {
	return current_token(p).kind == .Eof
}

token_text :: #force_inline proc(p: ^Parser, token: Token) -> string {
	return tokenizer.token_lexeme(token, p.source)
}

error_current :: proc(p: ^Parser, message: string) {
	append(&p.errors, Parse_Error{message = message, range = current_token(p).range})
}

ensure_forward_progress :: proc(p: ^Parser, start_index: int) {
	if p.index == start_index && !at_eof(p) {
		bump_token(p)
	}
}
