package abap_frontend_semantic_dependencies

Remote_Dependency_Kind :: enum {
	Include,
	Message_Class,
	Report,
	Function,
	Static,
	Type,
	Symbol,
}

Remote_Dependency_Hint :: enum {
	None,
	Object_Type,
	Interface_Type,
}

Remote_Dependency_Candidate :: struct {
	name: string,
	kind: Remote_Dependency_Kind,
	hint: Remote_Dependency_Hint,
}

Remote_Dependency_Key :: struct {
	name: string,
	kind: Remote_Dependency_Kind,
	hint: Remote_Dependency_Hint,
}
