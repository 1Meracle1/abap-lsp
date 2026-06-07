package abap_frontend_remote_dependencies

import "core:fmt"

trace_eprintf :: fmt.eprintf

trace_request_kind_text :: proc(kind: Remote_Dependency_Kind) -> string {
	switch kind {
	case .Include:
		return "include"
	case .Message_Class:
		return "message class"
	case .Report:
		return "report"
	case .Function:
		return "function module"
	case .Class:
		return "class"
	case .Interface:
		return "interface"
	case .Type:
		return "type"
	case .Symbol:
		return "symbol"
	}
	return "dependency"
}
