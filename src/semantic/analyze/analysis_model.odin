package abap_frontend_semantic_analyze

Source_Mode :: enum {
	Full,
	Dependency_Interface,
}

Project_Analysis :: struct {
	units:       [dynamic]Unit_Analysis,
	diagnostics: [dynamic]Diagnostic,
}
