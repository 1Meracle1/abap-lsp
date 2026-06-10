package abap_frontend_lsp

import json "core:encoding/json"
import "core:mem"
import "core:strings"

handle_folding_ranges :: proc(ctx: ^Request_Context, params: json.Value) {
	uri := uri_from_text_document_params(params)
	if uri == "" {
		send_success(ctx.output, ctx.id, []Folding_Range{}, ctx.state.allocator)
		return
	}
	if doc, ok := ctx.state.documents[uri]; ok {
		ranges := folding_ranges_for_source(doc.text, ctx.state.allocator)
		send_success(ctx.output, ctx.id, ranges, ctx.state.allocator)
		return
	}
	send_success(ctx.output, ctx.id, []Folding_Range{}, ctx.state.allocator)
}

folding_ranges_for_source :: proc(source: string, allocator: mem.Allocator) -> []Folding_Range {
	ranges := make([dynamic]Folding_Range, 0, 16, allocator)
	stack := make([dynamic]int, 0, 16, context.temp_allocator)
	line_start := 0
	line := 0
	for i := 0; i <= len(source); i += 1 {
		if i < len(source) && source[i] != '\n' {
			continue
		}
		line_text := source[line_start:i]
		if len(line_text) > 0 && line_text[len(line_text) - 1] == '\r' {
			line_text = line_text[:len(line_text) - 1]
		}
		keyword := leading_keyword(line_text, context.temp_allocator)
		if folding_start_keyword(keyword) {
			append(&stack, line)
		} else if folding_end_keyword(keyword) && len(stack) > 0 {
			start_line := pop(&stack)
			if line > start_line {
				append(
					&ranges,
					Folding_Range {
						start_line = start_line,
						start_character = 0,
						end_line = line,
						end_character = len(line_text),
					},
				)
			}
		}
		line += 1
		line_start = i + 1
	}
	return ranges[:]
}

leading_keyword :: proc(line: string, allocator: mem.Allocator) -> string {
	trimmed := strings.trim_space(line)
	if trimmed == "" || strings.has_prefix(trimmed, "*") || strings.has_prefix(trimmed, "\"") {
		return ""
	}
	end := 0
	for end < len(trimmed) {
		ch := trimmed[end]
		if !(('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '-') {
			break
		}
		end += 1
	}
	return strings.to_upper(trimmed[:end], allocator) if end > 0 else ""
}

folding_start_keyword :: proc "contextless" (keyword: string) -> bool {
	switch keyword {
	case "CLASS",
	     "INTERFACE",
	     "METHOD",
	     "FORM",
	     "FUNCTION",
	     "IF",
	     "CASE",
	     "DO",
	     "WHILE",
	     "LOOP",
	     "TRY",
	     "SELECT":
		return true
	}
	return false
}

folding_end_keyword :: proc "contextless" (keyword: string) -> bool {
	switch keyword {
	case "ENDCLASS",
	     "ENDINTERFACE",
	     "ENDMETHOD",
	     "ENDFORM",
	     "ENDFUNCTION",
	     "ENDIF",
	     "ENDCASE",
	     "ENDDO",
	     "ENDWHILE",
	     "ENDLOOP",
	     "ENDTRY",
	     "ENDSELECT":
		return true
	}
	return false
}
