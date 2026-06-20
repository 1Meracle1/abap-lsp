package abap_frontend_lints

import "src:ast"
import "src:semantic"

import "core:mem"
import "core:strings"

apply_ast_suppressions :: proc(
	out: ^Unit_Lints,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if out == nil || out.file == nil || out.file.root == nil || len(out.diagnostics) == 0 {
		return
	}
	entries := suppression_entries_from_ast(out.file.root, allocator)
	if len(entries) == 0 {
		return
	}
	report_suppressed := policy != nil && policy.report_suppressed
	filtered := make([dynamic]Diagnostic, 0, len(out.diagnostics), allocator)
	for diagnostic in out.diagnostics {
		item := diagnostic
		if !diagnostic.suppressed {
			if suppression, ok := suppression_for_diagnostic(entries[:], diagnostic, allocator); ok {
				if !report_suppressed {
					continue
				}
				mark_suppressed(&item, suppression)
			}
		}
		append(&filtered, item)
	}
	out.diagnostics = filtered
}

suppression_entries_from_ast :: proc(
	root: ^ast.File,
	allocator: mem.Allocator,
) -> [dynamic]Suppression_Entry {
	entries := make([dynamic]Suppression_Entry, 0, 4, allocator)
	if root == nil {
		return entries
	}
	for trivia in root.leading_trivia {
		if trivia.kind == .Comment {
			collect_file_allow_suppression(&entries, trivia, allocator)
		}
	}
	for record in root.detached_trivia {
		if record.trivia.kind == .Comment {
			collect_file_allow_suppression(&entries, record.trivia, allocator)
		}
	}
	suppression_collect_stmt_list(&entries, root.stmts[:], allocator)
	return entries
}

suppression_collect_stmt_list :: proc(
	entries: ^[dynamic]Suppression_Entry,
	stmts: []^ast.Stmt,
	allocator: mem.Allocator,
) {
	for stmt in stmts {
		suppression_collect_stmt(entries, stmt, allocator)
	}
}

suppression_collect_stmt :: proc(
	entries: ^[dynamic]Suppression_Entry,
	stmt: ^ast.Stmt,
	allocator: mem.Allocator,
) {
	if stmt == nil {
		return
	}
	statement_range := semantic.Range{start = stmt.range.start, end = stmt.range.end}
	for trivia in stmt.leading_trivia {
		if trivia.kind != .Comment {
			continue
		}
		if action, selectors, action_ok := abap_lsp_allow_action(trivia.text, allocator); action_ok {
			if action == .File {
				push_allow_suppression(entries, trivia, .File, {}, selectors, allocator)
			} else if action == .Next_Statement {
				push_allow_suppression(
					entries,
					trivia,
					.Statement,
					statement_range,
					selectors,
					allocator,
				)
			}
		}
	}
	for trivia in stmt.trailing_trivia {
		if trivia.kind == .Pragma {
			push_pragma_suppression(entries, trivia, statement_range, allocator)
		} else if trivia.kind == .Comment {
			push_pseudo_comment_suppressions(
				entries,
				trivia,
				statement_range,
				allocator,
			)
			if action, selectors, action_ok := abap_lsp_allow_action(trivia.text, allocator); action_ok {
				if action == .File {
					push_allow_suppression(entries, trivia, .File, {}, selectors, allocator)
				} else if action == .Current_Statement {
					push_allow_suppression(
						entries,
						trivia,
						.Statement,
						statement_range,
						selectors,
						allocator,
					)
				}
			}
		}
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.If_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
		for clause in n.elseif_clauses {
			suppression_collect_stmt_list(entries, clause.body[:], allocator)
		}
		if n.else_clause != nil {
			suppression_collect_stmt_list(entries, n.else_clause.body[:], allocator)
		}
	case ^ast.Case_Stmt:
		suppression_collect_stmt_list(entries, n.recovery[:], allocator)
		for clause in n.whens {
			suppression_collect_stmt_list(entries, clause.body[:], allocator)
		}
	case ^ast.While_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Do_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Loop_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.At_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Try_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
		for clause in n.catches {
			suppression_collect_stmt_list(entries, clause.body[:], allocator)
		}
		if n.cleanup != nil {
			suppression_collect_stmt_list(entries, n.cleanup.body[:], allocator)
		}
	case ^ast.Class_Decl:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Interface_Decl:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Method_Decl:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Form_Decl:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Function_Decl:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Module_Decl:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Event_Block_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Macro_Def_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Enhancement_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Enhancement_Section_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Test_Seam_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Test_Injection_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	case ^ast.Select_Stmt:
		suppression_collect_stmt_list(entries, n.body[:], allocator)
	}
}

collect_file_allow_suppression :: proc(
	entries: ^[dynamic]Suppression_Entry,
	trivia: ast.Ast_Trivia,
	allocator: mem.Allocator,
) {
	if action, selectors, action_ok := abap_lsp_allow_action(trivia.text, allocator); action_ok &&
	   action == .File {
		push_allow_suppression(entries, trivia, .File, {}, selectors, allocator)
	}
}

Allow_Action :: enum {
	Current_Statement,
	Next_Statement,
	File,
}

push_pragma_suppression :: proc(
	entries: ^[dynamic]Suppression_Entry,
	trivia: ast.Ast_Trivia,
	statement_range: semantic.Range,
	allocator: mem.Allocator,
) {
	token := strings.trim_space(trivia.text)
	if !strings.has_prefix(token, "##") || len(token) <= 2 {
		return
	}
	alias := normalized_alias(token[2:], allocator)
	if alias == "" || alias == "all" {
		return
	}
	selectors := make([dynamic]Suppression_Selector, 0, 1, allocator)
	append(&selectors, Suppression_Selector{kind = .Sap_Alias, value = alias})
	append(entries, Suppression_Entry {
		target = .Statement,
		range = statement_range,
		kind = .Pragma,
		token = strings.clone(token, allocator),
		selectors = selectors,
	})
}

push_pseudo_comment_suppressions :: proc(
	entries: ^[dynamic]Suppression_Entry,
	trivia: ast.Ast_Trivia,
	statement_range: semantic.Range,
	allocator: mem.Allocator,
) {
	comment := trivia.text
	pos, ok := find_ascii_case_insensitive(comment, "#EC")
	if !ok {
		return
	}
	tail := comment[pos + 3:]
	fields := space_fields(tail, allocator)
	for field in fields {
		alias := trim_selector_token(field)
		if alias == "" || alias == "*" || strings.equal_fold(alias, "all") {
			continue
		}
		if !selector_text_valid(alias) {
			continue
		}
		selectors := make([dynamic]Suppression_Selector, 0, 1, allocator)
		append(&selectors, Suppression_Selector {
			kind = .Sap_Alias,
			value = normalized_alias(alias, allocator),
		})
		append(entries, Suppression_Entry {
			target = .Statement,
			range = statement_range,
			kind = .Pseudo_Comment,
			token = strings.clone(alias, allocator),
			selectors = selectors,
		})
	}
}

push_allow_suppression :: proc(
	entries: ^[dynamic]Suppression_Entry,
	trivia: ast.Ast_Trivia,
	target: Suppression_Target_Kind,
	range: semantic.Range,
	selectors: [dynamic]Suppression_Selector,
	allocator: mem.Allocator,
) {
	if len(selectors) == 0 {
		return
	}
	append(entries, Suppression_Entry {
		target = target,
		range = range,
		kind = .Abap_Lsp_Allow,
		token = strings.clone(strings.trim_space(trivia.text), allocator),
		selectors = selectors,
	})
}

abap_lsp_allow_action :: proc(
	comment: string,
	allocator: mem.Allocator,
) -> (Allow_Action, [dynamic]Suppression_Selector, bool) {
	text := strings.trim_left(comment, " \t")
	if strings.has_prefix(text, "\"") || strings.has_prefix(text, "*") {
		text = strings.trim_left(text[1:], " \t")
	}
	marker, marker_ok := find_ascii_case_insensitive(text, "abap-lsp:")
	if !marker_ok {
		return {}, nil, false
	}
	rest := strings.trim_left(text[marker + len("abap-lsp:"):], " \t")
	action := Allow_Action.Current_Statement
	after, prefix_ok := strip_ascii_case_prefix(rest, "allow-next-line")
	if prefix_ok {
		action = .Next_Statement
		rest = strings.trim_left(after, " \t")
	} else {
		after, prefix_ok = strip_ascii_case_prefix(rest, "allow-file")
		if prefix_ok {
			action = .File
			rest = strings.trim_left(after, " \t")
		} else {
			after, prefix_ok = strip_ascii_case_prefix(rest, "allow")
			if prefix_ok {
				action = .Current_Statement
				rest = strings.trim_left(after, " \t")
			} else {
				return {}, nil, false
			}
		}
	}
	if len(rest) == 0 || rest[0] != '(' {
		return {}, nil, false
	}
	close := -1
	for i in 1 ..< len(rest) {
		if rest[i] == ')' {
			close = i
			break
		}
	}
	if close < 0 {
		return {}, nil, false
	}
	selectors := parse_allow_selectors(rest[1:close], allocator)
	return action, selectors, true
}

parse_allow_selectors :: proc(value: string, allocator: mem.Allocator) -> [dynamic]Suppression_Selector {
	out := make([dynamic]Suppression_Selector, 0, 2, allocator)
	fields := comma_fields(value, allocator)
	for field in fields {
		id := strings.trim_space(field)
		if id == "" || strings.contains(id, ":") || !selector_text_valid(id) {
			continue
		}
		normalized := normalized_id(id, allocator)
		if normalized == "" || normalized == "all" || suppression_selectors_contain(out[:], .Id, normalized) {
			continue
		}
		append(&out, Suppression_Selector{kind = .Id, value = normalized})
	}
	return out
}

suppression_selectors_contain :: proc(
	selectors: []Suppression_Selector,
	kind: Suppression_Selector_Kind,
	value: string,
) -> bool {
	for selector in selectors {
		if selector.kind == kind && selector.value == value {
			return true
		}
	}
	return false
}

suppression_for_diagnostic :: proc(
	entries: []Suppression_Entry,
	diagnostic: Diagnostic,
	allocator: mem.Allocator,
) -> (Suppression, bool) {
	id := normalized_id(diagnostic.id, context.temp_allocator)
	for entry in entries {
		if !suppression_entry_applies(entry, diagnostic.range.start) {
			continue
		}
		if !suppression_entry_matches(entry, id, diagnostic, allocator) {
			continue
		}
		return Suppression {
			kind = entry.kind,
			range = entry.range,
			token = entry.token,
		}, true
	}
	return {}, false
}

suppression_entry_applies :: proc(entry: Suppression_Entry, offset: int) -> bool {
	if entry.target == .File {
		return true
	}
	return entry.range.start <= offset && offset < entry.range.end
}

suppression_entry_matches :: proc(
	entry: Suppression_Entry,
	id: string,
	diagnostic: Diagnostic,
	allocator: mem.Allocator,
) -> bool {
	_ = allocator
	for selector in entry.selectors {
		if selector.kind == .Id && selector.value == id {
			return true
		}
		if selector.kind == .Sap_Alias {
			for i in 0 ..< diagnostic.sap_alias_count {
				if normalized_alias(diagnostic.sap_aliases[i], context.temp_allocator) == selector.value {
					return true
				}
			}
		}
	}
	return false
}

strip_ascii_case_prefix :: proc(value, prefix: string) -> (string, bool) {
	if len(value) < len(prefix) {
		return "", false
	}
	if strings.equal_fold(value[:len(prefix)], prefix) {
		return value[len(prefix):], true
	}
	return "", false
}

find_ascii_case_insensitive :: proc(haystack, needle: string) -> (int, bool) {
	if len(needle) == 0 || len(haystack) < len(needle) {
		return -1, false
	}
	for i := 0; i <= len(haystack) - len(needle); i += 1 {
		if strings.equal_fold(haystack[i:i + len(needle)], needle) {
			return i, true
		}
	}
	return -1, false
}

trim_selector_token :: proc(value: string) -> string {
	start := 0
	end := len(value)
	for start < end && !selector_char(value[start]) {
		start += 1
	}
	for end > start && !selector_char(value[end - 1]) {
		end -= 1
	}
	return value[start:end]
}

selector_text_valid :: proc(value: string) -> bool {
	if value == "" {
		return false
	}
	for i in 0 ..< len(value) {
		if !selector_char(value[i]) {
			return false
		}
	}
	return true
}

selector_char :: proc "contextless" (ch: u8) -> bool {
	return('a' <= ch && ch <= 'z') ||
	      ('A' <= ch && ch <= 'Z') ||
	      ('0' <= ch && ch <= '9') ||
	      ch == '_' ||
	      ch == '-' ||
	      ch == '.'
}

space_fields :: proc(value: string, allocator: mem.Allocator) -> [dynamic]string {
	out := make([dynamic]string, 0, 4, allocator)
	i := 0
	for i < len(value) {
		for i < len(value) && space(value[i]) {
			i += 1
		}
		start := i
		for i < len(value) && !space(value[i]) {
			i += 1
		}
		if start < i {
			append(&out, value[start:i])
		}
	}
	return out
}

comma_fields :: proc(value: string, allocator: mem.Allocator) -> [dynamic]string {
	out := make([dynamic]string, 0, 2, allocator)
	start := 0
	for i := 0; i <= len(value); i += 1 {
		if i == len(value) || value[i] == ',' {
			append(&out, value[start:i])
			start = i + 1
		}
	}
	return out
}

space :: proc "contextless" (ch: u8) -> bool {
	return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'
}
