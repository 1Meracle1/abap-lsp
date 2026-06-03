package abap_frontend_semantic_analyze

Source_Mode :: enum {
	Full,
	Dependency_Interface,
}

Project_Analysis :: struct {
	units:       [dynamic]Unit_Analysis,
	diagnostics: [dynamic]Diagnostic,
}

diagnostic_is_warning :: proc(kind: Diagnostic_Kind) -> bool {
	#partial switch kind {
	case .Shadowed_Symbol,
	     .Unreachable_Code:
		return true
	}
	return false
}
