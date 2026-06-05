package abap_frontend_semantic_analyze

import "src:tokenizer"

range_contains_offset :: #force_inline proc(range: tokenizer.Range, offset: int) -> bool {
	return range.start <= offset && offset < range.end
}
