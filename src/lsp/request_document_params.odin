package abap_frontend_lsp

import json "core:encoding/json"

uri_from_text_document_params :: proc(params: json.Value) -> string {
	object, ok := params.(json.Object)
	if !ok {
		return ""
	}
	text_document, doc_ok := object_object(object, "textDocument")
	if !doc_ok {
		return ""
	}
	uri, uri_ok := object_string(text_document, "uri")
	if !uri_ok {
		return ""
	}
	return normalize_lsp_uri(uri, context.allocator)
}
